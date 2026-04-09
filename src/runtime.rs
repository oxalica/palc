use std::ffi::{OsStr, OsString};
use std::num::NonZero;

use os_str_bytes::OsStrBytesExt;
use ref_cast::RefCast;

use crate::Result;
use crate::error::ErrorKind;
use crate::refl::{RawArgsInfo, RawSubcommandInfo};
use crate::shared::ArgAttrs;
use crate::values::ValueParser;

use super::Error;

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
#[expect(improper_ctypes_definitions)]
pub extern "C" fn error_missing_arg(info: &RawArgsInfo, idx: u8) -> Result<()> {
    Err(ErrorKind::MissingRequiredArgument.with_arg_idx(info, idx))
}

/// Commonly used: any non-`Option` subcommand will check this.
#[cold]
#[expect(improper_ctypes_definitions)]
pub extern "C" fn error_missing_subcmd() -> Result<()> {
    Err(ErrorKind::MissingRequiredSubcommand.into())
}

/// Other relatively uncommon opt-in constraints: `exclusive`, `dependency`, `conflict`.
// FIXME: Too many arguments. Maybe flatten into a single template?
#[cold]
#[expect(improper_ctypes_definitions)]
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
    #[expect(improper_ctypes_definitions)]
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
    #[expect(improper_ctypes_definitions)]
    pub extern "C" fn run_unit(
        &mut self,
        parent: &mut dyn Frame,
        info: &RawArgsInfo,
    ) -> Result<()> {
        self.run(&mut ArgsFrame { fields: &mut [], subcmd: None, info, parent })
    }
}
impl<F: FnMut(&mut ArgsFrame) -> Result<()>> RunParser for F {
    #[expect(improper_ctypes_definitions)]
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
        enc_name: &str,
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
}

impl Frame for ArgsFrame<'_, '_> {
    fn search_named(
        &mut self,
        enc_name: &str,
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
        enc_name: &str,
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
        _enc_name: &str,
        _global: bool,
    ) -> Option<(&mut dyn Parsable, &RawArgsInfo, ArgAttrs)> {
        None
    }

    #[cfg(feature = "help")]
    fn collect_command_prefix(&self, _out: &mut String) {}
}

/// Type-erased objects that can be parsed into.
pub trait Parsable {
    /// `value` is extracted from either inlined or the next argument.
    /// It is empty for argument that accepts no value.
    fn parse_from(&mut self, attrs: ArgAttrs, value: &OsStr) -> Result<()>;
}

/// A state for parsing a field.
pub trait FieldState: Default {
    type Value;
    type Output;
    fn place<P: ValueParser<Output = Self::Value>>(&mut self, _: P) -> &mut dyn Parsable;
    fn finish(&mut self) -> Self::Output;
    fn finish_opt(&mut self) -> Option<Self::Output>;
    fn is_set(&self) -> bool;
    // NB. This is the default output value, i.e. the default type is `Vec<T>` for multi-args.
    fn set_default(&mut self, f: impl FnOnce() -> Self::Output);
    fn set_default_parse<P: ValueParser<Output = Self::Value>>(&mut self, default: &str, p: P) {
        #[inline(never)]
        fn set_from_default_dyn(p: &mut dyn Parsable, default: &OsStr) {
            p.parse_from(ArgAttrs::default(), default.as_ref()).expect("invalid default value");
        }

        if !self.is_set() {
            set_from_default_dyn(self.place(p), default.as_ref());
        }
    }
}

//////// Places ////////

/// A value-less named argument, so-called flag.
///
/// `--flag` set the state to `true`. Multiple occurrences are rejected.
#[derive(Default)]
pub struct FlagPlace(bool);

impl FieldState for FlagPlace {
    type Value = bool;
    type Output = bool;
    fn place<P: ValueParser<Output = bool>>(&mut self, _: P) -> &mut dyn Parsable {
        self
    }
    fn finish(&mut self) -> bool {
        self.0
    }
    fn finish_opt(&mut self) -> Option<Self::Output> {
        unreachable!()
    }
    fn is_set(&self) -> bool {
        self.0
    }
    fn set_default(&mut self, f: impl FnOnce() -> Self::Value) {
        if !self.0 {
            self.0 = f();
        }
    }
}
impl Parsable for FlagPlace {
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
}

/// A multi-occurring named argument or a variable length positional argument,
/// that collects all the values into a `Vec`.
/// Multiple occurrences can be interleaved by other arguments.
pub struct VecPlace<T>(Vec<T>);
impl<T> Default for VecPlace<T> {
    fn default() -> Self {
        Self(Vec::new())
    }
}
impl<T> FieldState for VecPlace<T> {
    type Value = T;
    type Output = Vec<T>;
    fn place<P: ValueParser<Output = Self::Value>>(&mut self, _: P) -> &mut dyn Parsable {
        <VecParser<P>>::ref_cast_mut(self)
    }
    fn finish(&mut self) -> Vec<T> {
        std::mem::take(&mut self.0)
    }
    fn finish_opt(&mut self) -> Option<Vec<T>> {
        if self.0.is_empty() { None } else { Some(self.finish()) }
    }
    fn is_set(&self) -> bool {
        !self.0.is_empty()
    }
    fn set_default(&mut self, f: impl FnOnce() -> Self::Output) {
        if self.0.is_empty() {
            self.0 = f();
        }
    }
}

#[derive(RefCast)]
#[repr(transparent)]
struct VecParser<P: ValueParser>(VecPlace<P::Output>);
impl<P: ValueParser> Parsable for VecParser<P> {
    fn parse_from(&mut self, attrs: ArgAttrs, value: &OsStr) -> Result<()> {
        let v = &mut self.0.0;

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
}

/// A single-occurring single-value named or positional argument.
///
/// Multiple occurrences produce a duplicated argument error.
pub struct SetValuePlace<T>(Option<T>);
impl<T> Default for SetValuePlace<T> {
    fn default() -> Self {
        Self(None)
    }
}
impl<T> FieldState for SetValuePlace<T> {
    type Value = T;
    type Output = T;
    fn place<P: ValueParser<Output = Self::Value>>(&mut self, _: P) -> &mut dyn Parsable {
        <SetValueParser<P>>::ref_cast_mut(self)
    }
    fn finish(&mut self) -> T {
        self.0.take().unwrap()
    }
    fn finish_opt(&mut self) -> Option<T> {
        self.0.take()
    }
    fn is_set(&self) -> bool {
        self.0.is_some()
    }
    fn set_default(&mut self, f: impl FnOnce() -> Self::Value) {
        self.0.get_or_insert_with(f);
    }
}

#[derive(RefCast)]
#[repr(transparent)]
struct SetValueParser<P: ValueParser>(SetValuePlace<P::Output>);
impl<P: ValueParser> Parsable for SetValueParser<P> {
    fn parse_from(&mut self, _: ArgAttrs, value: &OsStr) -> Result<()> {
        if self.0.0.is_some() {
            return Err(ErrorKind::DuplicatedNamedArgument.into());
        }
        self.0.0 = Some(P::parse(value)?);
        Ok(())
    }
}

/// A single-occurring zero-or-one value named argument.
///
/// Currently it must have `requires_eq` set, and accepts either:
/// - Missing argument => `None`
/// - `--foo` => `Some(None)`
/// - `--foo=value` => `Some(Some(..))`
pub struct SetOptionalValuePlace<T>(Option<Option<T>>);
impl<T> Default for SetOptionalValuePlace<T> {
    fn default() -> Self {
        Self(None)
    }
}
impl<T> FieldState for SetOptionalValuePlace<T> {
    type Value = T;
    type Output = Option<T>;
    fn place<P: ValueParser<Output = Self::Value>>(&mut self, _: P) -> &mut dyn Parsable {
        <SetOptionalValueParser<P>>::ref_cast_mut(self)
    }
    fn finish(&mut self) -> Option<T> {
        unreachable!()
    }
    fn finish_opt(&mut self) -> Option<Option<T>> {
        self.0.take()
    }
    fn is_set(&self) -> bool {
        self.0.is_some()
    }
    fn set_default(&mut self, f: impl FnOnce() -> Self::Output) {
        self.0.get_or_insert_with(f);
    }
}

#[derive(RefCast)]
#[repr(transparent)]
struct SetOptionalValueParser<P: ValueParser>(SetOptionalValuePlace<P::Output>);
impl<P: ValueParser> Parsable for SetOptionalValueParser<P> {
    fn parse_from(&mut self, attrs: ArgAttrs, value: &OsStr) -> Result<()> {
        if self.0.0.is_some() {
            return Err(ErrorKind::DuplicatedNamedArgument.into());
        }
        self.0.0 = Some(if attrs.contains(ArgAttrs::HAS_INLINE_VALUE) {
            Some(P::parse(value)?)
        } else {
            None
        });
        Ok(())
    }
}

//////// Parsing logic ////////

pub struct RawParser<'i> {
    iter: &'i mut dyn Iterator<Item = OsString>,
    /// If we are inside a short arguments bundle, the index of next short arg.
    next_short_idx: Option<NonZero<usize>>,
}

impl RunParser for RawParser<'_> {
    #[expect(improper_ctypes_definitions)]
    extern "C" fn run(&mut self, frame: &mut ArgsFrame) -> Result<()> {
        run_parser(self, frame)
    }
}

fn run_parser(p: &mut RawParser, frame: &mut ArgsFrame) -> Result<()> {
    // Move out the subcommand since we will borrow it mutably together with `frame`.
    let mut subcmd = frame.subcmd.take();

    let mut positional_attrs = frame.info.positional_attrs();
    let mut after_dash_dash = false;

    let mut buf = OsString::new();
    while let Some(arg) =
        if after_dash_dash { p.iter.next().map(RawArg::Positional) } else { p.next_arg(&mut buf)? }
    {
        match arg {
            RawArg::EncodedNamed(enc_name, inline_value) => {
                let Some((place, info, mut attrs)) = frame.search_named(enc_name, false) else {
                    // TODO: Configurable help?
                    #[cfg(feature = "help")]
                    if enc_name == "h" || enc_name == "help" {
                        return Err(Error::from(ErrorKind::Help).maybe_render_help(frame));
                    }
                    let mut dec_name = String::with_capacity(2 + enc_name.len());
                    // TODO: Dedup this code with `Error::fmt`.
                    if enc_name.chars().nth(1).is_none() {
                        dec_name.push('-');
                    } else if !enc_name.starts_with("--") {
                        dec_name.push_str("--");
                    }
                    dec_name.push_str(enc_name);
                    return Err(ErrorKind::UnknownNamedArgument.with_input(dec_name.into()));
                };

                let require_value = attrs.contains(ArgAttrs::REQUIRE_VALUE);
                let require_eq = attrs.contains(ArgAttrs::REQUIRE_EQ);
                // FIXME: Eliminate this clone.
                let value_ret = match inline_value {
                    RawInlineValue::Eq(v) => {
                        attrs.set(ArgAttrs::HAS_INLINE_VALUE, true);
                        Ok(v.to_owned())
                    }
                    RawInlineValue::None => {
                        // NB: On `Option<Option<_>>` case (`!require_value && require_eq`),
                        // we should accept standalone long arguments `--foo` as missing value.
                        // Thus `require_value` checks first.
                        if !require_value {
                            Ok(OsString::new())
                        } else if require_eq {
                            Err(ErrorKind::MissingEq.into())
                        } else {
                            p.next_arg_as_value(attrs)
                        }
                    }
                    RawInlineValue::Ambiguous(tail) => {
                        // NB: On `Option<Option<_>>` case (`!require_value && require_eq`),
                        // we should reject bundled arguments `-svalue` since `=` is required.
                        // Thus `require_eq` checks first.
                        if require_eq {
                            Err(ErrorKind::MissingEq.into())
                        } else if require_value {
                            p.discard_short_args();
                            attrs.set(ArgAttrs::HAS_INLINE_VALUE, true);
                            Ok(tail.to_owned())
                        } else {
                            Ok(OsString::new())
                        }
                    }
                };

                match value_ret.and_then(|mut value| {
                    if attrs.contains(ArgAttrs::MAKE_LOWERCASE) {
                        value.make_ascii_lowercase();
                    }
                    place.parse_from(attrs, &value)
                }) {
                    Ok(()) => {}
                    Err(err) => {
                        let desc = info.get_description(attrs.get_index());
                        return Err(err.with_arg_desc(desc).maybe_render_help(frame));
                    }
                }
            }
            RawArg::Positional(arg) => {
                // Subcommand has priority, but only before `--`.
                if !after_dash_dash
                    && let Some(place) = &mut subcmd
                    && let Some(subcmd_info) = &frame.info.subcmd_info
                    && let Some((name, idx)) = subcmd_info.search(&arg)
                {
                    drop(arg);
                    drop(buf);
                    let mut frame = CommandFrame {
                        #[cfg(feature = "help")]
                        name,
                        parent: frame,
                    };
                    #[cfg(not(feature = "help"))]
                    {
                        let _ = name;
                    }
                    return (**place).__parse(idx, &mut frame, p);
                }

                let attrs = if let Some((&attrs, rest)) = positional_attrs.split_first()
                    && (after_dash_dash || !attrs.contains(ArgAttrs::LAST))
                {
                    // Advance only if this is not a va-arg.
                    if !attrs.contains(ArgAttrs::VAR_ARG) {
                        positional_attrs = rest;
                    }
                    attrs
                } else if subcmd.is_some() {
                    return Err(ErrorKind::UnknownSubcommand.with_input(arg));
                } else {
                    return Err(ErrorKind::ExtraPositionalArgument.with_input(arg));
                };

                let place = &mut *frame.fields[usize::from(attrs.get_index())];
                place.parse_from(attrs, &arg)?;
                if attrs.contains(ArgAttrs::GREEDY) {
                    drop(arg);
                    drop(buf);
                    for arg in &mut *p.iter {
                        place.parse_from(attrs, &arg)?;
                    }
                    return Ok(());
                }
            }
            RawArg::DashDash => {
                after_dash_dash = true;

                // If there is a "last" argument, all further arguments goes into it.
                if let Some(last) = positional_attrs.last()
                    && last.contains(ArgAttrs::LAST)
                {
                    positional_attrs = std::slice::from_ref(last);
                }
            }
        }
    }
    Ok(())
}

#[derive(Debug)]
enum RawArg<'a> {
    /// "--"
    DashDash,
    /// Encoded arg name and an optional inline value.
    EncodedNamed(&'a str, RawInlineValue<'a>),
    /// A verbatim positional argument.
    Positional(OsString),
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

impl<'i> RawParser<'i> {
    pub(crate) fn new(iter: &'i mut dyn Iterator<Item = OsString>) -> Self {
        Self { iter, next_short_idx: None }
    }

    /// Iterate the next logical argument, possibly splitting short argument bundle.
    fn next_arg<'b>(&mut self, buf: &'b mut OsString) -> Result<Option<RawArg<'b>>> {
        #[cold]
        fn fail_on_next_short_arg(rest: &OsStr) -> Error {
            let bytes = rest.as_encoded_bytes();
            let mut dash_arg = OsString::new();
            // Add back the `-` prefix to signify this is parsed as a short named argument.
            dash_arg.push("-");

            // UTF-8 length of a char must be 1..=4, len==1 case is checked outside.
            for len in 2..=bytes.len().min(4) {
                if let Ok(s) = std::str::from_utf8(&bytes[..len]) {
                    dash_arg.push(s);
                    return ErrorKind::UnknownNamedArgument.with_input(dash_arg);
                }
            }

            // There is not a single valid UTF-8 char. Note that we cannot construct `OsStr` from `u8`.
            // Just dump all the invalid data back to the user.
            dash_arg.push(rest);
            ErrorKind::UnknownNamedArgument.with_input(dash_arg)
        }

        if let Some(pos) = self.next_short_idx.filter(|pos| pos.get() < buf.len()) {
            let argb = buf.as_encoded_bytes();
            let idx = pos.get();

            // By struct invariant, argb[..idx] must be UTF-8.
            let next_byte = std::str::from_utf8(&argb[idx..idx + 1]);
            // Assuming all valid short args are ASCII, if the next byte is not ASCII, it must fail.
            let short_arg = next_byte.map_err(|_| fail_on_next_short_arg(buf.index(idx..)))?;

            self.next_short_idx = pos.checked_add(1);
            let inline_value = match argb.get(idx + 1) {
                Some(&b'=') => {
                    // `=` must be consumed as a inlined value, not a short argument.
                    self.discard_short_args();
                    RawInlineValue::Eq(buf.index(idx + 2..))
                }
                Some(_) => RawInlineValue::Ambiguous(buf.index(idx + 1..)),
                None => {
                    // Reached the end of bundle.
                    self.discard_short_args();
                    RawInlineValue::None
                }
            };
            return Ok(Some(RawArg::EncodedNamed(short_arg, inline_value)));
        }
        self.next_short_idx = None;

        // Otherwise, fetch the next input argument.
        *buf = match self.iter.next() {
            Some(raw) => raw,
            None => return Ok(None),
        };

        if buf.starts_with("--") {
            if buf.len() == 2 {
                return Ok(Some(RawArg::DashDash));
            }
            // Using `strip_prefix` in if-condition requires polonius to make lifetime check.
            let rest = buf.index(2..);
            let (name, inline_value) = match rest.split_once('=') {
                Some((name, value)) => (name, RawInlineValue::Eq(value)),
                None => (rest, RawInlineValue::None),
            };
            // Include proceeding "--" only for single-char long arguments.
            let enc_name = if name.len() != 1 { name } else { buf.index(..3) };
            let enc_name = enc_name
                .to_str()
                .ok_or_else(|| ErrorKind::UnknownNamedArgument.with_input(enc_name.into()))?;
            Ok(Some(RawArg::EncodedNamed(enc_name, inline_value)))
        } else if buf.starts_with("-") && buf.len() != 1 {
            self.next_short_idx = Some(NonZero::new(1).unwrap());
            self.next_arg(buf)
        } else {
            Ok(Some(RawArg::Positional(std::mem::take(buf))))
        }
    }

    /// Discard the rest of short argument bundle, poll a new raw argument on next `next_arg`.
    fn discard_short_args(&mut self) {
        self.next_short_idx = None;
    }

    /// Consume the next argument as an outlined value for named argument.
    ///
    /// If the next raw argument starts with `-` and `attrs` disallows it, an
    /// error will be returned.
    fn next_arg_as_value(&mut self, attrs: ArgAttrs) -> Result<OsString> {
        assert!(self.next_short_idx.is_none());
        self.iter
            .next()
            .filter(|raw| {
                let raw = raw.as_encoded_bytes();
                if raw == b"-" || !raw.starts_with(b"-") {
                    return true;
                }
                if attrs.contains(ArgAttrs::ACCEPT_HYPHEN_ANY) {
                    true
                } else if attrs.contains(ArgAttrs::ACCEPT_HYPHEN_NUM) {
                    raw[1..].iter().all(|b| b.is_ascii_digit())
                } else {
                    false
                }
            })
            .ok_or_else(|| ErrorKind::MissingValue.into())
    }
}
