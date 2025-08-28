use std::convert::Infallible;
use std::ffi::{OsStr, OsString};
use std::marker::PhantomData;
use std::num::NonZero;
use std::ops::ControlFlow;

use os_str_bytes::OsStrBytesExt;
use ref_cast::RefCast;

use crate::Result;
use crate::error::ErrorKind;
use crate::refl::{RawArgsInfo, RawSubcommandInfo};
use crate::shared::{AcceptHyphen, ArgAttrs};
use crate::values::ValueParser;

use super::Error;

/// The implementation detail of [`crate::Parser`].
/// Not in public API.
#[doc(hidden)]
pub trait ParserInternal: Sized {
    fn __parse_toplevel(p: &mut RawParser, program_name: &OsStr) -> Result<Self>;
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
    #[doc(hidden)]
    type __State: ParserState<Output = Self>;
}

/// The fallback state type for graceful failing from proc-macro.
pub struct FallbackState<T>(Infallible, PhantomData<T>);

impl<T: 'static> ParserState for FallbackState<T> {
    type Output = T;

    const RAW_ARGS_INFO: &'static RawArgsInfo = RawArgsInfo::EMPTY_REF;

    const TOTAL_ARG_CNT: u8 = 0;
    const TOTAL_UNNAMED_ARG_CNT: u8 = 0;

    fn init() -> Self {
        unimplemented!()
    }
    fn finish(&mut self) -> Result<Self::Output> {
        match self.0 {}
    }
}
impl<T: 'static> ParserStateDyn for FallbackState<T> {}

// TODO: Invalid default strings are only caught at runtime, which is not ideal.
pub fn parse_default_str<P: ValueParser>(s: &str, _: P) -> Result<P::Output> {
    P::parse(s.as_ref())
}

pub fn assert_impl_display_for_help<T: std::fmt::Display>(x: T) -> T {
    x
}

// TODO: Check inlining behavior is expected.
pub fn unknown_subcommand<T>(name: &OsStr) -> Result<T> {
    Err(ErrorKind::UnknownSubcommand.with_input(name.into()))
}

pub fn missing_required_arg<S: ParserState, T>(idx: u8) -> Result<T> {
    Err(ErrorKind::MissingRequiredArgument.with_arg_idx::<S>(idx))
}

pub fn missing_required_subcmd<T>() -> Result<T> {
    Err(ErrorKind::MissingRequiredSubcommand.into())
}

pub fn constraint_required<S: ParserState, T>(idx: u8) -> Result<T> {
    Err(ErrorKind::ConstraintRequired.with_arg_idx::<S>(idx))
}

pub fn constraint_exclusive<S: ParserState, T>(idx: u8) -> Result<T> {
    Err(ErrorKind::ConstraintExclusive.with_arg_idx::<S>(idx))
}

pub fn constraint_conflict<S: ParserState, T>(idx: u8) -> Result<T> {
    Err(ErrorKind::ConstraintConflict.with_arg_idx::<S>(idx))
}

/// Type-erased objects that can be parsed into.
pub trait Parsable {
    /// `attrs` is the same value returned from `feed_*`.
    /// About `value`:
    /// - For named arguments accepting zero values, it should be ignored.
    /// - For named or unnamed arguments accepting one value, it is the extracted
    ///   string to be parsed, either from `--named=value`, `-ovalue` or `value`.
    ///   The callee should simply parse it.
    /// - For variable length unnamed arguments, it is the first argument
    ///   triggering the parsing. The callee should consume it and all the rest arguments.
    ///
    /// `cur_cmd_name` and `ancestors` are only used for subcommand, and should
    /// be ignored otherwise. They are not passed for named arguments.
    fn parse_from(
        &mut self,
        p: &mut RawParser,
        attrs: ArgAttrs,
        value: &OsStr,
        cur_cmd_name: &OsStr,
        ancestors: &mut dyn ParserChain,
    ) -> Result<()>;
}

#[inline(always)]
pub fn place_for_flag(place: &mut Option<bool>) -> &mut dyn Parsable {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place(Option<bool>);

    impl Parsable for Place {
        fn parse_from(
            &mut self,
            _: &mut RawParser,
            _: ArgAttrs,
            _: &OsStr,
            _: &OsStr,
            _: &mut dyn ParserChain,
        ) -> Result<()> {
            if self.0.is_some() {
                return Err(ErrorKind::DuplicatedNamedArgument.into());
            }
            self.0 = Some(true);
            Ok(())
        }
    }

    Place::ref_cast_mut(place)
}

#[inline(always)]
pub fn place_for_counter(place: &mut Option<u8>) -> &mut dyn Parsable {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place(Option<u8>);

    impl Parsable for Place {
        fn parse_from(
            &mut self,
            _: &mut RawParser,
            _: ArgAttrs,
            _: &OsStr,
            _: &OsStr,
            _: &mut dyn ParserChain,
        ) -> Result<()> {
            let v = self.0.get_or_insert_default();
            *v = v.saturating_add(1);
            Ok(())
        }
    }

    Place::ref_cast_mut(place)
}

pub fn place_for_vec<P: ValueParser>(
    place: &mut Option<Vec<P::Output>>,
    _: P,
) -> &mut dyn Parsable {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<P: ValueParser>(Option<Vec<P::Output>>);

    impl<P: ValueParser> Parsable for Place<P> {
        fn parse_from(
            &mut self,
            p: &mut RawParser,
            attrs: ArgAttrs,
            value: &OsStr,
            _: &OsStr,
            _: &mut dyn ParserChain,
        ) -> Result<()> {
            let v = self.0.get_or_insert_default();

            let feed: &mut dyn FnMut(&OsStr) -> Result<()> = if let Some(delim) = attrs.delimiter {
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

            feed(value)?;
            if attrs.greedy {
                for value in &mut p.iter {
                    feed(&value)?;
                }
            }
            Ok(())
        }
    }

    Place::<P>::ref_cast_mut(place)
}

pub fn place_for_set_value<P: ValueParser>(
    place: &mut Option<P::Output>,
    _: P,
) -> &'_ mut dyn Parsable {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<P: ValueParser>(Option<P::Output>);

    impl<P: ValueParser> Parsable for Place<P> {
        fn parse_from(
            &mut self,
            _: &mut RawParser,
            _: ArgAttrs,
            value: &OsStr,
            _: &OsStr,
            _: &mut dyn ParserChain,
        ) -> Result<()> {
            if self.0.is_some() {
                return Err(ErrorKind::DuplicatedNamedArgument.into());
            }
            self.0 = Some(P::parse(value)?);
            Ok(())
        }
    }

    Place::<P>::ref_cast_mut(place)
}

pub fn place_for_set_opt_value<P: ValueParser>(
    place: &mut Option<Option<P::Output>>,
    _: P,
) -> &'_ mut dyn Parsable {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<P: ValueParser>(Option<Option<P::Output>>);

    impl<P: ValueParser> Parsable for Place<P> {
        fn parse_from(
            &mut self,
            _: &mut RawParser,
            _: ArgAttrs,
            value: &OsStr,
            _: &OsStr,
            _: &mut dyn ParserChain,
        ) -> Result<()> {
            if self.0.is_some() {
                return Err(ErrorKind::DuplicatedNamedArgument.into());
            }
            self.0 = Some((!value.is_empty()).then(|| P::parse(value)).transpose()?);
            Ok(())
        }
    }

    Place::<P>::ref_cast_mut(place)
}

/// The singly linked list for states of ancestor subcommands. Deeper states come first.
/// `ancestors` are made into trait object for lifetime erasure, or it won't compile.
pub(crate) struct ParserChainNode<'a, 'b, 'c> {
    pub cmd_name: &'a OsStr,
    pub state: &'b mut dyn ParserStateDyn,
    pub ancestors: &'c mut dyn ParserChain,
}

pub trait ParserChain {
    #[expect(private_interfaces, reason = "not used by proc-macro")]
    fn out(&mut self) -> Option<ParserChainNode<'_, '_, '_>> {
        None
    }
}

impl dyn ParserChain + '_ {
    fn feed_global_named(
        &mut self,
        enc_name: &str,
    ) -> ControlFlow<(&mut dyn Parsable, ArgAttrs, &'static RawArgsInfo)> {
        let Some(node) = self.out() else { return ControlFlow::Continue(()) };
        let info = node.state.info();
        if let ControlFlow::Break((place, attrs)) = node.state.feed_named(enc_name) {
            if attrs.global {
                return ControlFlow::Break((place, attrs, info));
            }
        }
        node.ancestors.feed_global_named(enc_name)
    }
}

impl ParserChain for () {}

// TODO: De-virtualize this.
impl ParserChain for ParserChainNode<'_, '_, '_> {
    fn out(&mut self) -> Option<ParserChainNode<'_, '_, '_>> {
        // Reborrow fields.
        let ParserChainNode { cmd_name, state, ancestors } = self;
        Some(ParserChainNode { cmd_name, state: &mut **state, ancestors: &mut **ancestors })
    }
}

pub fn place_for_subcommand<G: GetSubcommand>(state: &mut G::State) -> FeedUnnamed<'_> {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<G: GetSubcommand>(G::State);

    impl<G: GetSubcommand> Parsable for Place<G> {
        fn parse_from(
            &mut self,
            p: &mut RawParser,
            _: ArgAttrs,
            value: &OsStr,
            cur_cmd_name: &OsStr,
            ancestors: &mut dyn ParserChain,
        ) -> Result<()> {
            // Recombine the state chain with the current state.
            let states =
                &mut ParserChainNode { cmd_name: cur_cmd_name, state: &mut self.0, ancestors };
            let subcmd = G::Subcommand::try_parse_with_name(p, value, states)?;
            *G::get(&mut self.0) = Some(subcmd);
            Ok(())
        }
    }

    ControlFlow::Break((Place::<G>::ref_cast_mut(state), /* unused */ ArgAttrs::new()))
}

/// `Break` on a resolved place. `Continue` on unknown names.
/// So we can `?` in generated code of `command(flatten)`.
pub type FeedNamed<'s> = ControlFlow<(&'s mut dyn Parsable, ArgAttrs)>;

pub type FeedUnnamed<'s> = FeedNamed<'s>;

pub trait ParserState: ParserStateDyn {
    type Output;

    const RAW_ARGS_INFO: &'static RawArgsInfo = RawArgsInfo::EMPTY_REF;
    /// For proc-macro to reject flattening an `impl Args` with subcommands.
    const HAS_SUBCOMMAND: bool = false;

    const TOTAL_ARG_CNT: u8 = 0;
    const TOTAL_UNNAMED_ARG_CNT: u8 = 0;

    fn init() -> Self;

    // Semantically this takes the ownership of `self`, but using `&mut self`
    // can eliminate partial drop codegen and call the default drop impl.
    // It gives a much better codegen.
    fn finish(&mut self) -> Result<Self::Output>;
}

/// The helper trait for `place_for_subcommand`.
///
/// It is possible to merge these methods into `ParserState`, but that would
/// expose `Subcommand` type at `UserParser::__State::Subcommand`, causing
/// various privacy issues if `UserParser` and its subcommand have different
/// privacy.
///
/// Here we define a separated (public) trait, but let proc-macro generate a
/// private witness type inside `feed_unnamed`, hiding the subcommand type from
/// public interface.
pub trait GetSubcommand: 'static {
    type State: ParserState;
    type Subcommand: Subcommand;
    fn get(state: &mut Self::State) -> &mut Option<Self::Subcommand>;
}

pub trait ParserStateDyn: 'static {
    /// Try to accept a named argument.
    ///
    /// If this parser accepts named argument `name`, return `Break(argument_place)`;
    /// otherwise, return `Continue(())`.
    ///
    /// `enc_name` is the encoded argument name to be matched on:
    /// - "-s" => "s"
    /// - "--long" => "long"
    /// - "--l" => "--l", to disambiguate from short arguments.
    fn feed_named(&mut self, _enc_name: &str) -> FeedNamed<'_> {
        ControlFlow::Continue(())
    }

    /// Try to accept an unnamed (positional) argument.
    ///
    /// `idx` is the index of logical arguments, counting each multi-value-argument as one.
    /// `is_last` indicates if a `--` has been encountered. It does not affect
    /// the increment of `idx`.
    fn feed_unnamed(&mut self, _arg: &OsStr, _idx: usize, _is_last: bool) -> FeedUnnamed<'_> {
        ControlFlow::Continue(())
    }

    /// Runtime reflection.
    fn info(&self) -> &'static RawArgsInfo {
        RawArgsInfo::EMPTY_REF
    }
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
    const RAW_INFO: &'static RawSubcommandInfo = RawSubcommandInfo::EMPTY_REF;

    fn feed_subcommand(_name: &OsStr) -> FeedSubcommand<Self> {
        None
    }

    fn try_parse_with_name(
        p: &mut RawParser,
        name: &OsStr,
        states: &mut dyn ParserChain,
    ) -> Result<Self> {
        let Some(f) = Self::feed_subcommand(name) else {
            return unknown_subcommand(name);
        };
        f(p, name, states)
    }
}

/// The fn signature of [`try_parse_state`], returned by [`Subcommand::feed_subcommand`].
pub type FeedSubcommand<T> =
    Option<fn(p: &mut RawParser, subcmd: &OsStr, states: &mut dyn ParserChain) -> Result<T>>;

#[cfg(test)]
fn _assert_feed_subcommand_ty<S: ParserState>() -> FeedSubcommand<S::Output> {
    Some(try_parse_state::<S>)
}

pub fn try_parse_state<S: ParserState>(
    p: &mut RawParser,
    cmd_name: &OsStr,
    ancestors: &mut dyn ParserChain,
) -> Result<S::Output> {
    let mut state = S::init();
    let node = &mut ParserChainNode { cmd_name, state: &mut state, ancestors };
    try_parse_state_dyn(p, node)?;
    state.finish()
}

/// The outlined main logic of parser.
#[inline(never)]
fn try_parse_state_dyn(p: &mut RawParser, chain: &mut ParserChainNode) -> Result<()> {
    let mut buf = OsString::new();

    let mut unnamed_idx = 0usize;

    while let Some(arg) = p.next_arg(&mut buf)? {
        match arg {
            Arg::EncodedNamed(enc_name, has_eq, value) => {
                let args_info = chain.state.info();
                let (place, attrs, args_info) = match chain.state.feed_named(enc_name) {
                    ControlFlow::Break((place, attrs)) => (place, attrs, args_info),
                    ControlFlow::Continue(()) => {
                        match chain.ancestors.feed_global_named(enc_name) {
                            ControlFlow::Break(ret) => ret,
                            ControlFlow::Continue(()) => {
                                // TODO: Configurable help?
                                #[cfg(feature = "help")]
                                if enc_name == "h" || enc_name == "help" {
                                    return Err(
                                        Error::from(ErrorKind::Help).maybe_render_help(chain)
                                    );
                                }
                                let mut dec_name = String::with_capacity(2 + enc_name.len());
                                // TODO: Dedup this code with `Error::fmt`.
                                if enc_name.chars().nth(1).is_none() {
                                    dec_name.push('-');
                                } else if !enc_name.starts_with("--") {
                                    dec_name.push_str("--");
                                }
                                dec_name.push_str(enc_name);
                                return Err(
                                    ErrorKind::UnknownNamedArgument.with_input(dec_name.into())
                                );
                            }
                        }
                    }
                };

                if attrs.no_value {
                    // Only fail on long arguments with inlined values `--long=value`.
                    if let Some(v) = value.filter(|_| enc_name.len() > 1) {
                        Err(ErrorKind::UnexpectedInlineValue.with_input(v.into()))
                    } else {
                        place.parse_from(p, attrs, "".as_ref(), "".as_ref(), &mut ())
                    }
                } else if attrs.require_eq && !has_eq {
                    Err(ErrorKind::MissingEq.into())
                } else if let Some(v) = value {
                    // Inlined value after `=`.
                    p.discard_short_args();
                    place.parse_from(p, attrs, v, "".as_ref(), &mut ())
                } else {
                    // Next argument as the value.
                    p.next_value(attrs.accept_hyphen)
                        .ok_or_else(|| ErrorKind::MissingValue.into())
                        .and_then(|mut v| {
                            if attrs.make_lowercase {
                                v.make_ascii_lowercase();
                            }
                            place.parse_from(p, attrs, &v, "".as_ref(), &mut ())
                        })
                }
                .map_err(
                    #[cold]
                    |err| {
                        let desc = args_info.get_description(attrs.index);
                        err.with_arg_desc(desc).maybe_render_help(chain)
                    },
                )?;
            }
            Arg::Unnamed(arg) => match chain.state.feed_unnamed(&arg, unnamed_idx, false) {
                ControlFlow::Break((place, attrs)) => {
                    unnamed_idx += 1;
                    place.parse_from(p, attrs, &arg, chain.cmd_name, chain.ancestors)?;
                }
                ControlFlow::Continue(()) => {
                    return Err(ErrorKind::ExtraUnnamedArgument.with_input(arg));
                }
            },
            Arg::DashDash => {
                drop(arg);
                drop(buf);
                while let Some(arg) = p.iter.next() {
                    match chain.state.feed_unnamed(&arg, unnamed_idx, true) {
                        ControlFlow::Break((place, attrs)) => {
                            unnamed_idx += 1;
                            place.parse_from(p, attrs, &arg, chain.cmd_name, chain.ancestors)?;
                        }
                        ControlFlow::Continue(()) => {
                            return Err(ErrorKind::ExtraUnnamedArgument.with_input(arg));
                        }
                    }
                }
                return Ok(());
            }
        }
    }
    Ok(())
}

pub struct RawParser<'i> {
    iter: &'i mut dyn Iterator<Item = OsString>,
    /// If we are inside a short arguments bundle, the index of next short arg.
    next_short_idx: Option<NonZero<usize>>,
}

#[derive(Debug)]
enum Arg<'a> {
    /// "--"
    DashDash,
    /// Encoded arg name, equal sign, and an inlined value (excluding `=`).
    ///
    /// - "--long" => ("long", None)
    /// - "--long=value" => ("long", Some("value"))
    /// - "-s" => ("s", None)
    /// - "-smore", "-s=more" => ("s", Some("more"))
    EncodedNamed(&'a str, bool, Option<&'a OsStr>),
    Unnamed(OsString),
}

impl<'i> RawParser<'i> {
    pub(crate) fn new(iter: &'i mut dyn Iterator<Item = OsString>) -> Self {
        Self { iter, next_short_idx: None }
    }

    /// Iterate the next logical argument, possibly splitting short argument bundle.
    fn next_arg<'b>(&mut self, buf: &'b mut OsString) -> Result<Option<Arg<'b>>> {
        #[cold]
        fn fail_on_next_short_arg(rest: &OsStr) -> Error {
            let bytes = rest.as_encoded_bytes();

            // UTF-8 length of a char must be 1..=4, len==1 case is checked outside.
            for len in 2..=bytes.len().min(4) {
                if let Ok(s) = std::str::from_utf8(&bytes[..len]) {
                    let mut dec_input = String::with_capacity(4);
                    dec_input.push('-');
                    dec_input.push_str(s);
                    return ErrorKind::UnknownNamedArgument.with_input(dec_input.into());
                }
            }
            ErrorKind::InvalidUtf8.with_input(rest.into())
        }

        if let Some(pos) = self.next_short_idx.filter(|pos| pos.get() < buf.len()) {
            let argb = buf.as_encoded_bytes();
            let idx = pos.get();

            // By struct invariant, argb[..idx] must be UTF-8.
            let next_byte = std::str::from_utf8(&argb[idx..idx + 1]);
            // Assuming all valid short args are ASCII, if the next byte is not ASCII, it must fail.
            let short_arg = next_byte.map_err(|_| fail_on_next_short_arg(buf.index(idx..)))?;

            self.next_short_idx = pos.checked_add(1);
            let (has_eq, value) = match argb.get(idx + 1) {
                Some(&b'=') => (true, Some(buf.index(idx + 2..))),
                Some(_) => (false, Some(buf.index(idx + 1..))),
                None => {
                    // Reached the end of bundle.
                    self.discard_short_args();
                    (false, None)
                }
            };
            return Ok(Some(Arg::EncodedNamed(short_arg, has_eq, value)));
        }
        self.next_short_idx = None;

        // Otherwise, fetch the next input argument.
        *buf = match self.iter.next() {
            Some(raw) => raw,
            None => return Ok(None),
        };

        if buf.starts_with("--") {
            if buf.len() == 2 {
                return Ok(Some(Arg::DashDash));
            }
            // Using `strip_prefix` in if-condition requires polonius to make lifetime check.
            let rest = buf.index(2..);
            let (name, has_eq, value) = match rest.split_once('=') {
                Some((name, value)) => (name, true, Some(value)),
                None => (rest, false, None),
            };
            // Include proceeding "--" only for single-char long arguments.
            let enc_name = if name.len() != 1 { name } else { buf.index(..3) };
            let enc_name = enc_name
                .to_str()
                .ok_or_else(|| ErrorKind::InvalidUtf8.with_input(enc_name.into()))?;
            Ok(Some(Arg::EncodedNamed(enc_name, has_eq, value)))
        } else if buf.starts_with("-") && buf.len() != 1 {
            self.next_short_idx = Some(NonZero::new(1).unwrap());
            self.next_arg(buf)
        } else {
            Ok(Some(Arg::Unnamed(std::mem::take(buf))))
        }
    }

    /// Discard the rest of short argument bundle, poll a new raw argument on next `next_arg`.
    fn discard_short_args(&mut self) {
        self.next_short_idx = None;
    }

    fn next_value(&mut self, hyphen: AcceptHyphen) -> Option<OsString> {
        assert!(self.next_short_idx.is_none());
        self.iter.next().filter(|raw| {
            let raw = raw.as_encoded_bytes();
            if raw == b"-" || !raw.starts_with(b"-") {
                return true;
            }
            match hyphen {
                AcceptHyphen::Yes => true,
                AcceptHyphen::NegativeNumber => raw[1..].iter().all(|b| b.is_ascii_digit()),
                AcceptHyphen::No => false,
            }
        })
    }
}
