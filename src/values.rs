use std::ffi::OsStr;
use std::marker::PhantomData;
use std::ops::Deref;
use std::str::FromStr;

use crate::error::DynStdError;
use crate::{ErrorKind, Result};

mod sealed {
    pub trait Sealed {}
}

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
pub trait ValueEnum: std::fmt::Display + Sized {
    /// See [`ValueParser::POSSIBLE_INPUTS_NUL`].
    const POSSIBLE_INPUTS_NUL: &'static str = "";

    /// Whether there is no ASCII upper case letter in any variants.
    /// This is required for `arg(ignore_case)`.
    const NO_UPPER_CASE: bool = true;

    fn parse_value(_s: &str) -> Option<Self> {
        None
    }
}

pub struct InferValueParser<T, Fuel>(pub PhantomData<(T, Fuel)>);

impl<T, Fuel> Deref for InferValueParser<T, &Fuel> {
    type Target = InferValueParser<T, Fuel>;
    fn deref(&self) -> &Self::Target {
        &InferValueParser(PhantomData)
    }
}

// This function is displayed in error message, thus describes itself in its name.

pub fn assert_auto_infer_value_parser_ok<P: ValueParser>(p: P) -> P {
    p
}

// Level 3

impl<T: ValueEnum + 'static> InferValueParser<T, &&&()> {
    pub fn get(&self) -> impl ValueParser<Output = T> {
        ValueEnumParser(PhantomData)
    }
}
struct ValueEnumParser<T>(PhantomData<T>);
impl<T> sealed::Sealed for ValueEnumParser<T> {}
impl<T: ValueEnum + 'static> ValueParser for ValueEnumParser<T> {
    type Output = T;
    const POSSIBLE_INPUTS_NUL: &'static str = T::POSSIBLE_INPUTS_NUL;

    fn parse(v: &OsStr) -> Result<T> {
        // TODO: better diagnostics?
        v.to_str()
            .ok_or(ErrorKind::InvalidUtf8)
            .and_then(|s| T::parse_value(s).ok_or(ErrorKind::InvalidValue))
            .map_err(|err| {
                err.with_input(v.to_owned()).with_possible_values(T::POSSIBLE_INPUTS_NUL)
            })
    }
}

// Level 2

impl<T> InferValueParser<T, &&()>
where
    T: for<'a> TryFrom<&'a OsStr, Error: Into<DynStdError>> + 'static,
{
    pub fn get(&self) -> impl ValueParser<Output = T> {
        TryFromOsStrParser(PhantomData)
    }
}
struct TryFromOsStrParser<T>(PhantomData<T>);
impl<T> sealed::Sealed for TryFromOsStrParser<T> {}
impl<T> ValueParser for TryFromOsStrParser<T>
where
    T: for<'a> TryFrom<&'a OsStr, Error: Into<DynStdError>> + 'static,
{
    type Output = T;
    fn parse(v: &OsStr) -> Result<T> {
        T::try_from(v)
            .map_err(|err| ErrorKind::InvalidValue.with_input(v.into()).with_source(err.into()))
    }
}

// Level 1

impl<T> InferValueParser<T, &()>
where
    T: FromStr<Err: Into<DynStdError>> + 'static,
{
    pub fn get(&self) -> impl ValueParser<Output = T> {
        FromStrParser(PhantomData)
    }
}
struct FromStrParser<T>(PhantomData<T>);
impl<T> sealed::Sealed for FromStrParser<T> {}
impl<T> ValueParser for FromStrParser<T>
where
    T: FromStr<Err: Into<DynStdError>> + 'static,
{
    type Output = T;
    fn parse(v: &OsStr) -> Result<T> {
        let s = v.to_str().ok_or_else(|| ErrorKind::InvalidUtf8.with_input(v.into()))?;
        let t = s
            .parse::<T>()
            .map_err(|err| ErrorKind::InvalidValue.with_input(s.into()).with_source(err.into()))?;
        Ok(t)
    }
}

// Level 0

// For error reporting.
// Since `ValueParser` is sealed and all implementations are private, this user type is guaranteed
// to cause an unimplemented error on `ValueParser`.
impl<T> InferValueParser<T, ()> {
    pub fn get(&self) -> T {
        unreachable!()
    }
}

#[test]
fn infer_value_parser() {
    use std::ffi::OsString;
    use std::path::PathBuf;

    macro_rules! infer {
        ($ty:ty) => {
            InferValueParser::<$ty, &&&()>(PhantomData).get()
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
