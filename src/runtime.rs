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
use crate::values::ArgValueInfo;

use super::Error;

/// The implementation detail of [`crate::Parser`].
/// Not in public API.
#[doc(hidden)]
pub trait ParserInternal: Sized {
    fn __parse_toplevel(program: &OsStr, args: &mut ArgsIter<'_>) -> Result<Self>;
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

#[macro_export]
#[doc(hidden)]
macro_rules! __const_concat {
    // Fast path for default `__raw_meta`.
    ($($s:literal,)* $(env!($e:literal), $($s2:literal,)*)*) => {
        $crate::__private::concat!($($s,)* $(env!($e), $($s2,)*)*)
    };

    // Match exprs after literals to prevent invisible grouping.
    ($s:expr,) => {
        $s
    };
    ($($s:expr,)*) => {{
        const __STRS: &'static [&'static $crate::__private::str] = &[$($s),*];
        match $crate::__private::from_utf8(&const {
            $crate::__private::const_concat_impl::<{
                $crate::__private::const_concat_len(__STRS)
            }>(__STRS)
        }) {
            $crate::__private::Ok(__s) => __s,
            $crate::__private::Err(_) => $crate::__private::unreachable!(),
        }
    }};
}

#[cfg(feature = "help")]
#[macro_export]
#[doc(hidden)]
macro_rules! __gate_help {
    ($disabled:expr, $($enabled:tt)*) => {
        $($enabled)*
    };
}

#[cfg(not(feature = "help"))]
#[macro_export]
#[doc(hidden)]
macro_rules! __gate_help {
    ($disabled:expr, $($enabled:tt)*) => {
        $disabled
    };
}

pub const fn const_concat_len(strs: &[&str]) -> usize {
    let mut ret = 0;
    let mut i = 0;
    let str_cnt = strs.len();
    while i < str_cnt {
        ret += strs[i].len();
        i += 1;
    }
    ret
}

pub const fn const_concat_impl<const LEN: usize>(strs: &[&str]) -> [u8; LEN] {
    // Invalid UTF-8, to assert `LEN` is not too long.
    let mut buf = [0xFFu8; LEN];
    let mut o = 0;
    let mut i = 0;
    let str_cnt = strs.len();
    while i < str_cnt {
        let s = strs[i].as_bytes();
        let mut j = 0;
        let str_len = s.len();
        while j < str_len {
            buf[o] = s[j];
            j += 1;
            o += 1;
        }
        i += 1;
    }
    buf
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
pub fn parse_default_str<T, A: ArgValueInfo<T>>(s: &str, _: A) -> Result<T> {
    A::parse(s.as_ref())
}

pub fn parse_take_arg<T, A: ArgValueInfo<T>>(s: &mut OsString, _: A) -> Result<T> {
    A::parse(s)
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

/// A named argument with its place attached as `&mut self`.
pub trait ArgPlace {
    fn feed(&mut self, value: &OsStr, attrs: ArgAttrs) -> Result<(), Error>;
}

#[inline(always)]
pub fn place_for_flag(place: &mut Option<bool>) -> &mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place(Option<bool>);

    impl ArgPlace for Place {
        fn feed(&mut self, _: &OsStr, _: ArgAttrs) -> Result<(), Error> {
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
pub fn place_for_counter(place: &mut Option<u8>) -> &mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place(Option<u8>);

    impl ArgPlace for Place {
        fn feed(&mut self, _: &OsStr, _: ArgAttrs) -> Result<(), Error> {
            let v = self.0.get_or_insert_default();
            *v = v.saturating_add(1);
            Ok(())
        }
    }

    Place::ref_cast_mut(place)
}

pub fn place_for_vec<T, A: ArgValueInfo<T>>(place: &mut Option<Vec<T>>, _: A) -> &mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<T, A>(Option<Vec<T>>, PhantomData<A>);

    impl<T, A: ArgValueInfo<T>> ArgPlace for Place<T, A> {
        fn feed(&mut self, value: &OsStr, attrs: ArgAttrs) -> Result<(), Error> {
            let v = self.0.get_or_insert_default();
            if let Some(delim) = attrs.delimiter {
                for frag in value.split(char::from(delim.get())) {
                    v.push(A::parse(frag)?);
                }
            } else {
                v.push(A::parse(value)?);
            }
            Ok(())
        }
    }

    Place::<T, A>::ref_cast_mut(place)
}

pub fn place_for_set_value<T, A: ArgValueInfo<T>>(
    place: &mut Option<T>,
    _: A,
) -> &'_ mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<T, A>(Option<T>, PhantomData<A>);

    impl<T, A: ArgValueInfo<T>> ArgPlace for Place<T, A> {
        fn feed(&mut self, value: &OsStr, _: ArgAttrs) -> Result<(), Error> {
            if self.0.is_some() {
                return Err(ErrorKind::DuplicatedNamedArgument.into());
            }
            self.0 = Some(A::parse(value)?);
            Ok(())
        }
    }

    Place::<T, A>::ref_cast_mut(place)
}

pub fn place_for_set_opt_value<T, A: ArgValueInfo<T>>(
    place: &mut Option<Option<T>>,
    _: A,
) -> &'_ mut dyn ArgPlace {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<T, A>(Option<Option<T>>, PhantomData<A>);

    impl<T, A: ArgValueInfo<T>> ArgPlace for Place<T, A> {
        fn feed(&mut self, value: &OsStr, _: ArgAttrs) -> Result<(), Error> {
            if self.0.is_some() {
                return Err(ErrorKind::DuplicatedNamedArgument.into());
            }
            self.0 = Some((!value.is_empty()).then(|| A::parse(value)).transpose()?);
            Ok(())
        }
    }

    Place::<T, A>::ref_cast_mut(place)
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
    fn feed_named(
        &mut self,
        enc_name: &str,
    ) -> ControlFlow<(&mut dyn ArgPlace, ArgAttrs, &'static RawArgsInfo)> {
        let Some(node) = self.out() else { return ControlFlow::Continue(()) };
        let info = node.state.info();
        if let ControlFlow::Break((place, attrs)) = node.state.feed_named(enc_name) {
            return ControlFlow::Break((place, attrs, info));
        }
        node.ancestors.feed_named(enc_name)
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

/// A greedy unnamed argument with its place attached as `&mut self`.
///
/// It always consumes all the rest arguments.
pub trait GreedyArgsPlace {
    // TODO: Maybe avoid splitting out the first argument?
    fn feed_greedy(
        &mut self,
        arg: OsString,
        args: &mut ArgsIter<'_>,
        // NB. Since `self` borrows the state, we cannot pass the whole
        // `ParserChain` which overlaps with `self`'s lifetime.
        // Here we destruct and pass the rest fields of the outmost node.
        cur_cmd_name: &OsStr,
        ancestors: &mut dyn ParserChain,
    ) -> Result<()>;
}

pub fn place_for_trailing_var_arg<T, A: ArgValueInfo<T>>(
    place: &mut Option<Vec<T>>,
    _: A,
) -> FeedUnnamed<'_> {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<T, A>(Option<Vec<T>>, PhantomData<A>);

    impl<T, A: ArgValueInfo<T>> GreedyArgsPlace for Place<T, A> {
        fn feed_greedy(
            &mut self,
            mut arg: OsString,
            args: &mut ArgsIter<'_>,
            _cur_cmd_name: &OsStr,
            _ancestors: &mut dyn ParserChain,
        ) -> Result<()> {
            let v = self.0.get_or_insert_default();
            if let Some(high) = args.iter.size_hint().1 {
                v.reserve(1 + high);
            }
            loop {
                v.push(A::parse(&arg)?);
                arg = match args.iter.next() {
                    Some(arg) => arg,
                    None => return Ok(()),
                };
            }
        }
    }

    Ok(Some(Place::<T, A>::ref_cast_mut(place)))
}

pub fn place_for_subcommand<G: GetSubcommand>(state: &mut G::State) -> FeedUnnamed<'_> {
    #[derive(RefCast)]
    #[repr(transparent)]
    struct Place<G: GetSubcommand>(G::State);

    impl<G: GetSubcommand> GreedyArgsPlace for Place<G> {
        fn feed_greedy(
            &mut self,
            name: OsString,
            args: &mut ArgsIter<'_>,
            cur_cmd_name: &OsStr,
            ancestors: &mut dyn ParserChain,
        ) -> Result<()> {
            // Recombine the state chain with the current state.
            let states =
                &mut ParserChainNode { cmd_name: cur_cmd_name, state: &mut self.0, ancestors };
            let subcmd = G::Subcommand::try_parse_with_name(args, &name, states)?;
            *G::get(&mut self.0) = Some(subcmd);
            Ok(())
        }
    }

    Ok(Some(Place::<G>::ref_cast_mut(state)))
}

/// Break on a resolved place. Continue on unknown names.
/// So we can `?` in generated code of `command(flatten)`.
pub type FeedNamed<'s> = ControlFlow<(&'s mut dyn ArgPlace, ArgAttrs)>;

/// This should be an enum, but be this for `?` support, which is unstable to impl.
pub type FeedUnnamed<'s> = Result<Option<&'s mut dyn GreedyArgsPlace>, Option<Error>>;

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
    /// `is_last` indices if a `--` has been encountered. It does not effect `idx`.
    fn feed_unnamed(&mut self, arg: &mut OsString, idx: usize, is_last: bool) -> FeedUnnamed<'_> {
        let _ = (arg, idx, is_last);
        Err(None)
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
        args: &mut ArgsIter<'_>,
        name: &OsStr,
        states: &mut dyn ParserChain,
    ) -> Result<Self> {
        let Some(f) = Self::feed_subcommand(name) else {
            return unknown_subcommand(name);
        };
        f(args, name, states)
    }
}

/// The fn signature of [`try_parse_state`], returned by [`Subcommand::feed_subcommand`].
pub type FeedSubcommand<T> =
    Option<fn(args: &mut ArgsIter<'_>, subcmd: &OsStr, states: &mut dyn ParserChain) -> Result<T>>;

#[cfg(test)]
fn _assert_feed_subcommand_ty<S: ParserState>() -> FeedSubcommand<S::Output> {
    Some(try_parse_state::<S>)
}

pub fn try_parse_state<S: ParserState>(
    args: &mut ArgsIter<'_>,
    cmd_name: &OsStr,
    ancestors: &mut dyn ParserChain,
) -> Result<S::Output> {
    let mut state = S::init();
    let node = &mut ParserChainNode { cmd_name, state: &mut state, ancestors };
    try_parse_state_dyn(args, node)?;
    state.finish()
}

/// The outlined main logic of parser.
#[inline(never)]
fn try_parse_state_dyn(args: &mut ArgsIter<'_>, chain: &mut ParserChainNode) -> Result<()> {
    let mut buf = OsString::new();

    let mut unnamed_idx = 0usize;

    while let Some(arg) = args.next_arg(&mut buf)? {
        match arg {
            Arg::EncodedNamed(enc_name, has_eq, value) => {
                let (place, attrs, args_info) = match <dyn ParserChain>::feed_named(chain, enc_name)
                {
                    ControlFlow::Break(ret) => ret,
                    ControlFlow::Continue(()) => {
                        // TODO: Configurable help?
                        #[cfg(feature = "help")]
                        if enc_name == "h" || enc_name == "help" {
                            return Err(Error::from(ErrorKind::Help).maybe_render_help(chain));
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
                    }
                };

                if attrs.num_values == 0 {
                    // Only fail on long arguments with inlined values `--long=value`.
                    if let Some(v) = value.filter(|_| enc_name.len() > 1) {
                        Err(ErrorKind::UnexpectedInlineValue.with_input(v.into()))
                    } else {
                        place.feed("".as_ref(), attrs)
                    }
                } else {
                    debug_assert_eq!(attrs.num_values, 1);
                    if attrs.require_eq && !has_eq {
                        Err(ErrorKind::MissingEq.into())
                    } else if let Some(v) = value {
                        // Inlined value after `=`.
                        args.discard_short_args();
                        place.feed(v, attrs)
                    } else {
                        // Next argument as the value.
                        args.next_value(attrs.accept_hyphen)
                            .ok_or_else(|| ErrorKind::MissingValue.into())
                            .and_then(|mut v| {
                                if attrs.make_lowercase {
                                    v.make_ascii_lowercase();
                                }
                                place.feed(&v, attrs)
                            })
                    }
                }
                .map_err(
                    #[cold]
                    |err| {
                        let desc = args_info.get_description(attrs.index);
                        err.with_arg_desc(desc).maybe_render_help(chain)
                    },
                )?;
            }
            Arg::Unnamed(mut arg) => match chain.state.feed_unnamed(&mut arg, unnamed_idx, false) {
                Ok(None) => unnamed_idx += 1,
                Ok(Some(place)) => {
                    return place.feed_greedy(arg, args, chain.cmd_name, chain.ancestors);
                }
                Err(Some(err)) => return Err(err),
                Err(None) => return Err(ErrorKind::ExtraUnnamedArgument.with_input(arg)),
            },
            Arg::DashDash => {
                drop(arg);
                drop(buf);
                for mut arg in &mut args.iter {
                    match chain.state.feed_unnamed(&mut arg, unnamed_idx, true) {
                        Ok(None) => unnamed_idx += 1,
                        Ok(Some(place)) => {
                            return place.feed_greedy(arg, args, chain.cmd_name, chain.ancestors);
                        }
                        Err(Some(err)) => return Err(err),
                        Err(None) => return Err(ErrorKind::ExtraUnnamedArgument.with_input(arg)),
                    }
                }
                return Ok(());
            }
        }
    }
    Ok(())
}

pub struct ArgsIter<'a> {
    iter: &'a mut dyn Iterator<Item = OsString>,
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

impl<'a> ArgsIter<'a> {
    pub(crate) fn new(iter: &'a mut dyn Iterator<Item = OsString>) -> Self {
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
