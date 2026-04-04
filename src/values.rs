use std::ffi::OsStr;
use std::fmt;
use std::marker::PhantomData;
use std::str::FromStr;

use crate::error::DynStdError;
use crate::util::split_terminator;
use crate::{ErrorKind, Result};

mod sealed {
    pub trait Sealed {}
}

#[cold]
#[inline(always)]
fn cold_path() {}

/// Value types that are parsable from raw `&OsStr`.
///
/// It behaves like a `clap::ValueParser` but in type-level.
#[diagnostic::on_unimplemented(
    message = "`{Self}` cannot be parsed into a palc argument value",
    label = "unparsable value type",
    note = "this is an internal trait that should NOT be implemented directly",
    note = "types are parsable by `derive(palc::ValueEnum)`, or by implementing `TryFrom<&OsStr>`,
    `From<&OsStr>` or `FromStr`, with error type be either `&str`, `String` or \
    `impl std::error::Error + Send + Sync`"
)]
pub trait ValueParser: Sized + sealed::Sealed + 'static {
    type Output: Sized;

    /// Possible input strings terminated by NUL.
    const POSSIBLE_INPUTS_NUL: &'static str = "";

    fn parse(v: &OsStr) -> Result<Self::Output>;
}

/// This trait definition is not a public API, only the derive-macro is.
#[doc(hidden)]
pub trait ValueEnum: Sized {
    /// See [`ValueParser::POSSIBLE_INPUTS_NUL`].
    const POSSIBLE_INPUTS_NUL: &'static str = "";

    /// Whether there is no ASCII upper case letter in any variants.
    /// This is required for `arg(ignore_case)`.
    const NO_UPPER_CASE: bool = true;

    fn parse_value(s: &str) -> Option<Self>;
    fn display_value(&self) -> &'static str;
}

/// Deref-specialization for display impl of the default value:
/// prefer `ValueEnum`, then fallback to `Display`.
///
/// We'll generate:
/// `assert_impl_display_default_value(PhantomData::<$ty>.__palc_infer_display($expr))`
/// The typecheck should fail on the bounds on the outmost function call, and trigger
/// `on_unimplemented` on `DisplayDefaultValue`.
pub trait InferDisplayDefaultValue<T> {
    type Output;
    fn __palc_infer_display(self, v: T) -> Self::Output;
}
impl<T: ValueEnum> InferDisplayDefaultValue<T> for PhantomData<T> {
    type Output = &'static str;
    fn __palc_infer_display(self, v: T) -> Self::Output {
        v.display_value()
    }
}
impl<T> InferDisplayDefaultValue<T> for &PhantomData<T> {
    type Output = T;
    fn __palc_infer_display(self, v: T) -> Self::Output {
        v
    }
}

#[diagnostic::on_unimplemented(
    message = "the default value of `{Self}` cannot be formatted",
    label = "`{Self}` does not implement trait `std::fmt::Display` or `palc::ValueEnum`",
    note = "this argument has a default value via `arg(default_value_t)`, thus \
    a `std::fmt::Display` implementation is required in order to render it in
    the generated help message",
    note = "types defined with `derive(palc::ValueEnum)` will be automatically
    recognized and does not need `Display` implementation"
)]
trait DisplayDefaultValue: fmt::Display {}
impl<T: fmt::Display> DisplayDefaultValue for T {}

#[expect(private_bounds, reason = "opaque to proc-macro")]
pub fn assert_impl_display_default_value<T: DisplayDefaultValue>(x: T) -> T {
    x
}

/// Deref-specialization for parser impl selection:
/// prefer `ValueEnum`, then `TryFrom<&OsStr>`, finally `FromStr`.
///
/// We'll generate:
/// `assert_auto_infer_value_parser_ok((&&&PhantomData::<$ty>).__palc_infer_value_parser())`
pub trait InferValueParser<T> {
    type Output;
    fn __palc_infer_value_parser(self) -> Self::Output;
}

// This function is displayed in error message, thus describes itself in its name.
pub fn assert_auto_infer_value_parser_ok<P: ValueParser>(p: P) -> P {
    p
}

// Level 3

impl<T: ValueEnum + 'static> InferValueParser<T> for &&&PhantomData<T> {
    type Output = ValueEnumParser<T>;
    fn __palc_infer_value_parser(self) -> Self::Output {
        ValueEnumParser(PhantomData)
    }
}

pub struct ValueEnumParser<T>(PhantomData<T>);
impl<T> sealed::Sealed for ValueEnumParser<T> {}
#[diagnostic::do_not_recommend]
impl<T: ValueEnum + 'static> ValueParser for ValueEnumParser<T> {
    type Output = T;
    const POSSIBLE_INPUTS_NUL: &'static str = T::POSSIBLE_INPUTS_NUL;

    fn parse(v: &OsStr) -> Result<T> {
        // TODO: better diagnostics?
        let src_err = match <&str>::try_from(v) {
            Ok(s) => match T::parse_value(s) {
                Some(value) => return Ok(value),
                None => None,
            },
            Err(err) => Some(err.into()),
        };
        cold_path();
        let mut err = ErrorKind::InvalidValue
            .with_input(v.to_owned())
            .with_context(format_args!("{}", PossibleValues(T::POSSIBLE_INPUTS_NUL)));
        err = if let Some(src) = src_err { err.with_source(src) } else { err };
        Err(err)
    }
}

struct PossibleValues(&'static str);

impl fmt::Display for PossibleValues {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("[possible values: ")?;
        let mut first = true;
        for v in split_terminator(self.0, b'\0') {
            if first {
                first = false;
            } else {
                f.write_str(", ")?;
            }
            f.write_str(v)?;
        }
        f.write_str("]")
    }
}

// Level 2

impl<T> InferValueParser<T> for &&PhantomData<T>
where
    T: for<'a> TryFrom<&'a OsStr, Error: Into<DynStdError>> + 'static,
{
    type Output = TryFromOsStrParser<T>;
    fn __palc_infer_value_parser(self) -> Self::Output {
        TryFromOsStrParser(PhantomData)
    }
}

pub struct TryFromOsStrParser<T>(PhantomData<T>);
impl<T> sealed::Sealed for TryFromOsStrParser<T> {}
#[diagnostic::do_not_recommend]
impl<T> ValueParser for TryFromOsStrParser<T>
where
    T: for<'a> TryFrom<&'a OsStr, Error: Into<DynStdError>> + 'static,
{
    type Output = T;
    fn parse(v: &OsStr) -> Result<T> {
        match T::try_from(v) {
            Ok(value) => Ok(value),
            Err(err) => {
                cold_path();
                Err(ErrorKind::InvalidValue.with_input(v.into()).with_source(err.into()))
            }
        }
    }
}

// Level 1

impl<T> InferValueParser<T> for &PhantomData<T>
where
    T: FromStr<Err: Into<DynStdError>> + 'static,
{
    type Output = FromStrParser<T>;
    fn __palc_infer_value_parser(self) -> Self::Output {
        FromStrParser(PhantomData)
    }
}

pub struct FromStrParser<T>(PhantomData<T>);
impl<T> sealed::Sealed for FromStrParser<T> {}
#[diagnostic::do_not_recommend]
impl<T> ValueParser for FromStrParser<T>
where
    T: FromStr<Err: Into<DynStdError>> + 'static,
{
    type Output = T;
    fn parse(v: &OsStr) -> Result<T> {
        let err = match <&str>::try_from(v) {
            Ok(s) => match s.parse::<T>() {
                Ok(value) => return Ok(value),
                Err(err) => err.into(),
            },
            Err(err) => err.into(),
        };
        cold_path();
        Err(ErrorKind::InvalidValue.with_input(v.to_owned()).with_source(err))
    }
}

// Level 0

// For error reporting.
// Since `ValueParser` is sealed and all implementations are private, this user type is guaranteed
// to cause an unimplemented error on `ValueParser`.
impl<T> InferValueParser<T> for PhantomData<T> {
    type Output = T;
    fn __palc_infer_value_parser(self) -> Self::Output {
        const { unreachable!() }
    }
}

#[test]
fn infer_value_parser() {
    use std::ffi::OsString;
    use std::path::PathBuf;

    macro_rules! infer {
        ($ty:ty) => {
            (&&&PhantomData::<$ty>).__palc_infer_value_parser()
        };
    }

    fn has_parser<T>(_: impl ValueParser<Output = T>) {}

    enum MyValueEnum {}
    impl std::fmt::Display for MyValueEnum {
        fn fmt(&self, _: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            unreachable!()
        }
    }
    impl ValueEnum for MyValueEnum {
        fn parse_value(_s: &str) -> Option<Self> {
            None
        }
        fn display_value(&self) -> &'static str {
            unreachable!()
        }
    }

    has_parser::<MyValueEnum>(infer!(MyValueEnum));
    has_parser::<OsString>(infer!(OsString));
    has_parser::<PathBuf>(infer!(PathBuf));
    has_parser::<String>(infer!(String));
    has_parser::<usize>(infer!(usize));

    // `unreachable!()` at runtime.
    let _ = || {
        struct MyType;

        let () = infer!(());
        let _: MyType = infer!(MyType);
    };
}
