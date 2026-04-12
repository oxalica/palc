#![expect(improper_ctypes_definitions, reason = "used as a nounwind marker")]
use std::borrow::Cow;
use std::ffi::{OsStr, OsString};
use std::ops::ControlFlow;

use os_str_bytes::OsStrBytesExt;

use crate::Result;
use crate::error::ErrorKind;
use crate::refl::{RawArgsInfo, RawSubcommandInfo};
use crate::shared::ArgAttrs;
use crate::values::ValueParser;

//////// Random utility functions called by proc-macro ////////
// 1. `extern "C"` for nounwind hint.
// 2. `#[cold]` to properly streamline the happy path. Otherwise LLVM will pick
// the validation failure case to follow the main control flow, probably due to
// the assumption of "forward jump (`if`) is likely non-taken".

/// For various assertions.
#[cold]
pub extern "C" fn unreachable_nounwind() -> ! {
    unreachable!()
}

/// Commonly used: any non-`Option` non-`bool` argument will check this.
#[cold]
pub extern "C" fn error_missing_arg(info: &RawArgsInfo, idx: u8) -> Result<()> {
    Err(ErrorKind::MissingRequiredArgument.with_arg_idx(info, idx))
}

/// Commonly used: any non-`Option` subcommand will check this.
#[cold]
pub extern "C" fn error_missing_subcmd() -> Result<()> {
    Err(ErrorKind::MissingRequiredSubcommand.into())
}

/// Other relatively uncommon opt-in constraints: `exclusive`, `dependency`, `conflict`.
// FIXME: Too many arguments. Maybe flatten into a single template?
#[cold]
pub extern "C" fn validate_constraints(
    info: &RawArgsInfo,
    isset: &[bool],
    exclusive: &[u8],
    dependency_groups: &[&[u8]],
    conflict_groups: &[&[u8]],
) -> Result<()> {
    if let Some(&i) = exclusive.iter().find(|&&i| isset[usize::from(i)])
        && isset.iter().filter(|&&set| set).count() > 1
    {
        return Err(ErrorKind::ConstraintExclusive.with_arg_idx(info, i));
    }
    for &group in dependency_groups {
        if let Some((&lhs, others)) = group.split_first()
            && isset[usize::from(lhs)]
            && let Some(&_rhs) = others.iter().find(|&&rhs| !isset[usize::from(rhs)])
        {
            // TODO: Another argument?
            return Err(ErrorKind::ConstraintRequired.with_arg_idx(info, lhs));
        }
    }
    for &group in conflict_groups {
        if let Some((&lhs, others)) = group.split_first()
            && isset[usize::from(lhs)]
            && let Some(&_rhs) = others.iter().find(|&&rhs| isset[usize::from(rhs)])
        {
            // TODO: Another argument?
            return Err(ErrorKind::ConstraintConflict.with_arg_idx(info, lhs));
        }
    }
    Ok(())
}

//////// Major traits and types ////////

/// The implementation detail of [`crate::Parser`].
/// Not in public API.
#[doc(hidden)]
pub trait ParserInternal: Sized {
    #[expect(private_bounds, reason = "opaque to proc-macro")]
    type __Flavor: ParserFlavor<Self>;
}

pub(crate) trait ParserFlavor<T>: Sized {
    fn run_parser(p: &mut RawParser, program_name: &OsStr) -> Result<T>;
}

/// The fallback type for graceful failing from proc-macro.
pub struct FallbackParserFlavor;
impl<T> ParserFlavor<T> for FallbackParserFlavor {
    fn run_parser(_p: &mut RawParser, _program_name: &OsStr) -> Result<T> {
        // Never called at runtime.
        unreachable!()
    }
}

/// Top-level parser for a single struct entry.
///
/// It ignores the 0-th argument (aka. `argv[0]`) and parsing arguments after.
pub struct StructParserFlavor<A>(A);
impl<A: Args> ParserFlavor<A> for StructParserFlavor<A> {
    fn run_parser(p: &mut RawParser, program_name: &OsStr) -> Result<A> {
        let mut out = None;
        let mut frame = CommandFrame {
            #[cfg(feature = "help")]
            name: &program_name.to_string_lossy(),
            parent: &mut (),
        };
        #[cfg(not(feature = "help"))]
        {
            let _ = program_name;
        }
        A::__parse(&mut out, &mut frame, p)?;
        Ok(out.unwrap())
    }
}

/// Trait of argument structs, for composition.
///
/// This trait is in not public API. Only `derive(Args)` is.
#[diagnostic::on_unimplemented(
    message = "cannot flatten `{Self}` which is not a `palc::Args`",
    label = "this type is expected to have `derive(palc::Args)` but it is not"
)]
#[doc(hidden)]
pub trait Args: Sized + 'static {
    /// For proc-macro to reject flattening a subcommand.
    const __HAS_SUBCOMMAND: bool;
    const __INFO: &'static RawArgsInfo;

    /// Run the parser, validate arguments and store the result into `out`.
    ///
    /// This function must not unwind.
    extern "C" fn __parse(
        out: &mut Option<Self>,
        frame: &mut dyn Frame,
        parser: &mut dyn RunParser,
    ) -> Result<()>;
}

/// Trait of subcommand enums.
///
/// This trait is in not public API. Only `derive(Subcommand)` is.
#[diagnostic::on_unimplemented(
    message = "`{Self}` is not a `palc::Subcommand`",
    label = "this type is expected to have `derive(palc::Subcommand)` but it is not"
)]
#[doc(hidden)]
pub trait Subcommand: Sized + 'static {
    const __INFO: RawSubcommandInfo;

    // This function must not unwind.
    extern "C" fn __parse(
        out: &mut Option<Self>,
        idx: usize,
        frame: &mut dyn Frame,
        parser: &mut dyn RunParser,
    ) -> Result<()>;

    /// This function exists only for proc-macro to generate
    /// `<UserTy as Subcommand>::..` which can emit a trait unsatisfied error on
    /// the `UserTy` span.
    fn __erase(this: &mut Option<Self>) -> &mut dyn SubcommandPlace {
        this
    }
}

/// Same as `Subcommand` but implements on `Option<impl Subcommand>` for dyn-compatibility.
pub trait SubcommandPlace {
    // This function must not unwind.
    extern "C" fn __parse(
        &mut self,
        idx: usize,
        frame: &mut dyn Frame,
        parser: &mut dyn RunParser,
    ) -> Result<()>;
}
impl<S: Subcommand> SubcommandPlace for Option<S> {
    // This function must not unwind.
    extern "C" fn __parse(
        &mut self,
        idx: usize,
        frame: &mut dyn Frame,
        parser: &mut dyn RunParser,
    ) -> Result<()> {
        S::__parse(self, idx, frame, parser)
    }
}

/// Callback function to run the actual parser.
///
/// This is used instead of `FnMut` for:
/// - Simpler proc-macro codegen without the need to repeat the signature.
/// - Smaller vtable.
/// - TODO: "nounwind" marking.
pub trait RunParser {
    extern "C" fn run(&mut self, frame: &mut ArgsFrame) -> Result<()>;
}
impl dyn RunParser + '_ {
    /// Parse a unit struct.
    ///
    /// This is called on unit enum variants.
    pub extern "C" fn run_unit(
        &mut self,
        parent: &mut dyn Frame,
        info: &RawArgsInfo,
    ) -> Result<()> {
        self.run(&mut ArgsFrame { fields: &mut [], subcmd: None, info, parent, next_positional: 0 })
    }
}
impl<F: FnMut(&mut ArgsFrame) -> Result<()>> RunParser for F {
    extern "C" fn run(&mut self, frame: &mut ArgsFrame) -> Result<()> {
        self(frame)
    }
}

/// Dynamic information of the chain of entered `Args`.
///
/// Frames are chained into a singly-linked list, with decreasing parsing precedence.
/// Entering a subcommand or Args pushes one or more frames to the head.
/// Flattenned Args frame are pushed before main frame since they have lower precedences.
///
/// ```text
///           --- growing direction -->
/// Stack:              Field variables..   Field variables..    Field variables..
///                            ^                ^                      ^
///                            |                |                      |
/// ROOT <- CommandFrame <- ArgsFrame <- ArgsFrame <- CommandFrame <- ArgsFrame <- HEAD
/// `()`      "myapp"  <SubCli as Args> <Cli as Args>   "mysubcmd"  Subcmd::Variant
/// ```
pub trait Frame {
    fn search_named(
        &mut self,
        enc_name: &OsStr,
        global: bool,
    ) -> Option<(&mut dyn Parsable, &RawArgsInfo, ArgAttrs)>;

    #[cfg(feature = "help")]
    fn collect_command_prefix(&self, out: &mut String);
}

// NB: proc-macro constructs this.
pub struct ArgsFrame<'a, 'b> {
    pub fields: &'a mut [&'b mut dyn Parsable],
    pub subcmd: Option<&'a mut dyn SubcommandPlace>,
    pub info: &'a RawArgsInfo,
    pub parent: &'a mut dyn Frame,

    /// The next index to `info.positional_attrs` to parse.
    /// There cannot be more than `u8::MAX - 1` arguments so this will not
    /// overflow after parsing the last one.
    pub next_positional: u8,
}

impl Frame for ArgsFrame<'_, '_> {
    fn search_named(
        &mut self,
        enc_name: &OsStr,
        global: bool,
    ) -> Option<(&mut dyn Parsable, &RawArgsInfo, ArgAttrs)> {
        match self.info.get_named(enc_name) {
            Some(attrs) if !global || attrs.contains(ArgAttrs::GLOBAL) => {
                let place = &mut *self.fields[usize::from(attrs.get_index())];
                Some((place, self.info, attrs))
            }
            _ => self.parent.search_named(enc_name, global),
        }
    }

    #[cfg(feature = "help")]
    fn collect_command_prefix(&self, out: &mut String) {
        self.parent.collect_command_prefix(out);
    }
}

/// An auxiliary frame recording encountered (sub)command names for "Usage".
///
/// The top-level `Parser` will create one command frame to recording basename
/// of argv0 as well.
///
/// This frame is invisible to proc-macro generated code.
// TODO: Can we elide this frame if help is disabled? Currently this also serves
// as a global argument boundary.
struct CommandFrame<'a> {
    #[cfg(feature = "help")]
    name: &'a str,
    parent: &'a mut dyn Frame,
}
impl Frame for CommandFrame<'_> {
    fn search_named(
        &mut self,
        enc_name: &OsStr,
        _global: bool,
    ) -> Option<(&mut dyn Parsable, &RawArgsInfo, ArgAttrs)> {
        self.parent.search_named(enc_name, true)
    }

    #[cfg(feature = "help")]
    fn collect_command_prefix(&self, out: &mut String) {
        self.parent.collect_command_prefix(out);
        out.push(' ');
        out.push_str(self.name);
    }
}

/// End of the frame chain.
impl Frame for () {
    fn search_named(
        &mut self,
        _enc_name: &OsStr,
        _global: bool,
    ) -> Option<(&mut dyn Parsable, &RawArgsInfo, ArgAttrs)> {
        None
    }

    #[cfg(feature = "help")]
    fn collect_command_prefix(&self, _out: &mut String) {}
}

/// Type-erased objects that can be parsed into.
pub trait Parsable {
    type Value
    where
        Self: Sized;

    type Output
    where
        Self: Sized;

    /// `value` is extracted from either inlined or the next argument.
    /// It is empty for argument that accepts no value.
    fn parse_from(&mut self, attrs: ArgAttrs, value: &OsStr) -> Result<()>;

    fn is_set(&self) -> bool
    where
        Self: Sized;

    fn set_default(&mut self, f: impl FnOnce() -> Self::Output)
    where
        Self: Sized;

    fn set_default_parse(&mut self, default: &str)
    where
        Self: Sized,
    {
        #[inline(never)]
        fn set_from_default_dyn(p: &mut dyn Parsable, default: &OsStr) {
            // FIXME: Is `ArgAttrs::default()` correct here?
            p.parse_from(ArgAttrs::default(), default).expect("invalid default value");
        }

        if !self.is_set() {
            set_from_default_dyn(self, default.as_ref());
        }
    }
}

//////// Places ////////

/// A value-less named argument, so-called flag.
///
/// `--flag` set the state to `true`. Multiple occurrences are rejected.
pub struct FlagPlace(pub bool);

impl Parsable for FlagPlace {
    type Value = bool;
    type Output = bool;

    fn parse_from(&mut self, attrs: ArgAttrs, value: &OsStr) -> Result<()> {
        if attrs.contains(ArgAttrs::HAS_INLINE_VALUE) {
            return Err(ErrorKind::UnexpectedInlineValue.with_input(value.into()));
        }
        if self.0 {
            return Err(ErrorKind::DuplicatedNamedArgument.into());
        }
        self.0 = true;
        Ok(())
    }

    fn is_set(&self) -> bool {
        self.0
    }
    fn set_default(&mut self, f: impl FnOnce() -> Self::Output) {
        if !self.is_set() {
            self.0 = f();
        }
    }
}

/// A multi-occurring named argument or a variable length positional argument,
/// that collects all the values into a `Vec`.
/// Multiple occurrences can be interleaved by other arguments.
pub struct VecPlace<P: ValueParser>(pub Option<Vec<P::Output>>, pub P);
impl<P: ValueParser> Parsable for VecPlace<P> {
    type Value = P::Output;
    type Output = Vec<P::Output>;

    fn parse_from(&mut self, attrs: ArgAttrs, value: &OsStr) -> Result<()> {
        let v = self.0.get_or_insert_default();

        let visit: &mut dyn FnMut(&OsStr) -> Result<()> = if let Some(delim) = attrs.get_delimiter()
        {
            &mut move |value| {
                for frag in value.split(char::from(delim.get())) {
                    v.push(P::parse(frag)?);
                }
                Ok(())
            }
        } else {
            &mut |value| {
                v.push(P::parse(value)?);
                Ok(())
            }
        };

        visit(value)?;

        Ok(())
    }
    fn is_set(&self) -> bool {
        self.0.is_some()
    }
    fn set_default(&mut self, f: impl FnOnce() -> Self::Output) {
        if !self.is_set() {
            self.0 = Some(f());
        }
    }
}

/// A single-occurring single-value named or positional argument.
///
/// Multiple occurrences produce a duplicated argument error.
pub struct SetValuePlace<P: ValueParser>(pub Option<P::Output>, pub P);
impl<P: ValueParser> Parsable for SetValuePlace<P> {
    type Value = P::Output;
    type Output = P::Output;
    fn parse_from(&mut self, _: ArgAttrs, value: &OsStr) -> Result<()> {
        if self.0.is_some() {
            return Err(ErrorKind::DuplicatedNamedArgument.into());
        }
        self.0 = Some(P::parse(value)?);
        Ok(())
    }
    fn is_set(&self) -> bool {
        self.0.is_some()
    }
    fn set_default(&mut self, f: impl FnOnce() -> Self::Output) {
        if !self.is_set() {
            self.0 = Some(f());
        }
    }
}

/// A single-occurring zero-or-one value named argument.
///
/// Currently it must have `requires_eq` set, and accepts either:
/// - Missing argument => `None`
/// - `--foo` => `Some(None)`
/// - `--foo=value` => `Some(Some(..))`
pub struct SetOptionalValuePlace<P: ValueParser>(pub Option<Option<P::Output>>, pub P);
impl<P: ValueParser> Parsable for SetOptionalValuePlace<P> {
    type Value = P::Output;
    type Output = Option<P::Output>;
    fn parse_from(&mut self, attrs: ArgAttrs, value: &OsStr) -> Result<()> {
        if self.0.is_some() {
            return Err(ErrorKind::DuplicatedNamedArgument.into());
        }
        self.0 = Some(if attrs.contains(ArgAttrs::HAS_INLINE_VALUE) {
            Some(P::parse(value)?)
        } else {
            None
        });
        Ok(())
    }
    fn is_set(&self) -> bool {
        self.0.is_some()
    }
    fn set_default(&mut self, f: impl FnOnce() -> Self::Output) {
        self.0 = Some(f());
    }
}

//////// Parsing logic ////////

pub struct RawParser<'i> {
    iter: &'i mut dyn Iterator<Item = OsString>,
}

impl<'i> RawParser<'i> {
    pub(crate) fn new(iter: &'i mut dyn Iterator<Item = OsString>) -> Self {
        Self { iter }
    }
}

impl RunParser for RawParser<'_> {
    extern "C" fn run(&mut self, frame: &mut ArgsFrame) -> Result<()> {
        run_parser(self, frame)
    }
}

#[derive(Debug)]
enum RawInlineValue<'a> {
    /// No inline value given: `--long`, `-s`.
    None,
    /// Explicit inline value given: `--long=value`, `-s=value`.
    Eq(&'a OsStr),
    /// Ambiguous trailing chars that can be a inline value or bundled arguments:
    /// `-svalue`.
    Ambiguous(&'a OsStr),
}

fn run_parser(p: &mut RawParser, frame: &mut ArgsFrame) -> Result<()> {
    // Move out the subcommand since we will borrow it mutably together with `frame`.
    let mut subcmd = frame.subcmd.take();

    'next_arg: while let Some(arg) = p.iter.next() {
        if arg == "--" {
            // If there is a "last" argument, all further arguments goes into it.
            if let Some(last_attrs) = frame.info.positional_attrs.last()
                && last_attrs.contains(ArgAttrs::LAST)
            {
                frame.next_positional = frame.info.positional_len() as u8 - 1;
            }
            drop(arg);
            while let Some(arg) = p.iter.next() {
                visit_positional(p, frame, true, arg)?;
            }
            return Ok(());
        } else if arg.starts_with("--") {
            let (name, value) = match arg.split_once("=") {
                Some((name, value)) => (name, RawInlineValue::Eq(value)),
                None => (&*arg, RawInlineValue::None),
            };
            let _: ControlFlow<()> = visit_named(p, frame, false, name, value)?;
        } else if let Some(bundle) = arg.strip_prefix("-")
            // NB: A single "-" is treated as a normal positional argument.
            && !bundle.is_empty()
        {
            let chunk = bundle.as_encoded_bytes().utf8_chunks().next().unwrap();
            // WAIT: MSRV 1.93 for `char::MAX_LEN_UTF8`
            let mut buf = [0u8; 4];
            for (idx, ch) in chunk.valid().char_indices() {
                let enc_name = ch.encode_utf8(&mut buf);
                let after = bundle.index(idx + enc_name.len()..);
                let value = match after.strip_prefix("=") {
                    Some(value) => RawInlineValue::Eq(value),
                    None if !after.is_empty() => RawInlineValue::Ambiguous(after),
                    None => RawInlineValue::None,
                };
                if visit_named(p, frame, true, enc_name.as_ref(), value)?.is_break() {
                    continue 'next_arg;
                }
            }
            if !chunk.invalid().is_empty() {
                // Because we disallow non-UTF-8 named argument, this `visit_named` must fail
                // and produce an error rendering the rest of the input junk.
                let rest = bundle.index(chunk.valid().len()..);
                return Err(visit_named(p, frame, true, rest, RawInlineValue::None).unwrap_err());
            }
        } else {
            // Subcommand always has priority over positional arguments.
            // This aligns with clap, `subcommand_precedence_over_arg` only affect `num_args=1..` but not this.
            // See: <https://github.com/clap-rs/clap/issues/5513>
            if let Some(place) = &mut subcmd
                && let Some(subcmd_info) = &frame.info.subcmd_info
                && let Some((name, idx)) = subcmd_info.search(&arg)
            {
                let mut frame = CommandFrame {
                    #[cfg(feature = "help")]
                    name,
                    parent: frame,
                };
                #[cfg(not(feature = "help"))]
                {
                    let _ = name;
                }
                drop(arg);
                return (**place).__parse(idx, &mut frame, p);
            }

            visit_positional(p, frame, false, arg)?;
        }
    }
    Ok(())
}

fn visit_positional(
    p: &mut RawParser<'_>,
    frame: &mut ArgsFrame<'_, '_>,
    is_last: bool,
    arg: OsString,
) -> Result<()> {
    let attrs = if let Some(&attrs) =
        frame.info.positional_attrs.get(usize::from(frame.next_positional))
        && (is_last || !attrs.contains(ArgAttrs::LAST))
    {
        attrs
    } else if frame.info.subcmd_info.is_some() {
        return Err(ErrorKind::UnknownSubcommand.with_input(arg));
    } else {
        return Err(ErrorKind::ExtraPositionalArgument.with_input(arg));
    };

    // Advance only if this is not a va-arg.
    if !attrs.contains(ArgAttrs::VAR_ARG) {
        frame.next_positional += 1;
    }

    let place = &mut *frame.fields[usize::from(attrs.get_index())];
    place.parse_from(attrs, &arg)?;
    if attrs.contains(ArgAttrs::GREEDY) {
        for arg in &mut *p.iter {
            place.parse_from(attrs, &arg)?;
        }
        // Everything is consumed here. The outer loop should exit as well.
    }
    Ok(())
}

fn visit_named(
    p: &mut RawParser,
    frame: &mut ArgsFrame,
    is_short: bool,
    enc_name: &OsStr,
    inline_value: RawInlineValue<'_>,
) -> Result<ControlFlow<()>> {
    let Some((place, info, mut attrs)) = frame.search_named(enc_name, false) else {
        // TODO: Configurable help?
        #[cfg(feature = "help")]
        if enc_name == "h" || enc_name == "--help" {
            return Err(crate::Error::from(ErrorKind::Help).maybe_render_help(frame));
        }

        let mut input_arg = OsString::with_capacity(1 + enc_name.len());
        if is_short {
            input_arg.push("-");
        }
        input_arg.push(enc_name);
        return Err(ErrorKind::UnknownNamedArgument.with_input(input_arg));
    };

    let require_value = attrs.contains(ArgAttrs::REQUIRE_VALUE);
    let require_eq = attrs.contains(ArgAttrs::REQUIRE_EQ);
    let ret = match inline_value {
        RawInlineValue::Eq(v) => {
            attrs.set(ArgAttrs::HAS_INLINE_VALUE, true);
            Ok(Cow::Borrowed(v))
        }
        RawInlineValue::None => {
            // NB: On `Option<Option<_>>` case (`!require_value && require_eq`),
            // we should accept standalone long arguments `--foo` as missing value.
            // Thus `require_value` checks first.
            if !require_value {
                Ok(Cow::Borrowed("".as_ref()))
            } else if require_eq {
                Err(ErrorKind::MissingEq.into())
            } else {
                match p.iter.next().filter(|raw| {
                    let raw = raw.as_encoded_bytes();
                    if raw == b"-"
                        || !raw.starts_with(b"-")
                        || attrs.contains(ArgAttrs::ACCEPT_HYPHEN_ANY)
                    {
                        true
                    } else if attrs.contains(ArgAttrs::ACCEPT_HYPHEN_NUM) {
                        raw[1..].iter().all(|b| b.is_ascii_digit())
                    } else {
                        false
                    }
                }) {
                    Some(value) => Ok(Cow::Owned(value)),
                    None => Err(ErrorKind::MissingValue.into()),
                }
            }
        }
        RawInlineValue::Ambiguous(tail) => {
            // NB: On `Option<Option<_>>` case (`!require_value && require_eq`),
            // we should reject bundled arguments `-svalue` since `=` is required.
            // Thus `require_eq` checks first.
            if require_eq {
                Err(ErrorKind::MissingEq.into())
            } else if require_value {
                attrs.set(ArgAttrs::HAS_INLINE_VALUE, true);
                Ok(Cow::Borrowed(tail))
            } else {
                Ok(Cow::Borrowed("".as_ref()))
            }
        }
    }
    .and_then(|mut value| {
        if attrs.contains(ArgAttrs::MAKE_LOWERCASE) {
            value.to_mut().make_ascii_lowercase();
        }
        place.parse_from(attrs, &value)
    });

    match ret {
        Ok(()) => {
            Ok(if require_value { ControlFlow::Break(()) } else { ControlFlow::Continue(()) })
        }
        Err(err) => {
            let desc = info.get_description(attrs.get_index());
            Err(err.with_arg_desc(desc).maybe_render_help(frame))
        }
    }
}
