//! *Prototype of a command line argument parser with several opposite design goals from [clap].*
//!
//! [clap]: https://github.com/clap-rs/clap
//!
//! > ⚠️ This project is in alpha stage and is not ready for production yet.
//! > The API is subject to change. Feedbacks are welcome.
//!
//! See [repository README](https://github.com/oxalica/palc?tab=readme-ov-file#palc) for details.
//!
//! TODO: Documentations.
#![forbid(unsafe_code)]
use std::{ffi::OsString, path::Path};

use error::ErrorKind;
use runtime::{ParserInternal, RawParser};

mod error;
mod refl;
mod runtime;
mod shared;
mod util;
mod values;

#[cfg(feature = "help")]
mod help;

pub use crate::error::Error;
pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Not public API. Only for proc-macro internal use.
// To scan all usages:
// ```sh
// rg --only-matching --no-filename '\b__rt::\w+' | LC_COLLATE=C sort --unique
// ```
#[doc(hidden)]
pub mod __private {
    pub use std::convert::Infallible;
    pub use std::ffi::{OsStr, OsString};
    pub use std::fmt;
    pub use std::marker::PhantomData;
    pub use std::num::NonZero;
    pub use std::ops::ControlFlow;
    pub use std::str::from_utf8;
    pub use {Default, Err, Fn, Iterator, None, Ok, Option, Some, Vec, bool, char, str, u8, usize};

    pub use crate::runtime::*;

    // Macros.
    pub use crate::util::{const_concat_impl, const_concat_len};
    pub use crate::{__const_concat, __gate_help};
    pub use std::{assert, concat, env, format_args, unimplemented, unreachable};

    pub use crate::values::{
        InferValueParser, ValueEnum, ValueParser, assert_auto_infer_value_parser_ok,
    };

    pub use crate::refl::{RawArgsInfo, RawArgsInfoRef, RawSubcommandInfo};
    pub use crate::shared::ArgAttrs;
    pub use crate::{Parser, Result};
}

/// Top-level command interface.
///
/// You should only get an implementation via [`derive(Parser)`](macro@Parser).
/// Do not manually implement this trait.
pub trait Parser: ParserInternal + Sized + 'static {
    fn parse() -> Self {
        match Self::try_parse_from(std::env::args_os()) {
            Ok(v) => v,
            Err(err) => {
                eprintln!("{err}");
                std::process::exit(1);
            }
        }
    }

    fn try_parse_from<I, T>(iter: I) -> Result<Self>
    where
        I: IntoIterator<Item = T>,
        T: Into<OsString> + Clone,
    {
        let mut iter = iter.into_iter().map(|s| s.into());
        let arg0 = iter.next().ok_or(ErrorKind::MissingArg0)?;
        let program_name = Path::new(&arg0).file_name().unwrap_or(arg0.as_ref());
        Self::__parse_toplevel(&mut RawParser::new(&mut iter), program_name)
    }

    #[cfg(feature = "help")]
    fn render_long_help(argv0: impl Into<String>) -> String {
        Self::try_parse_from([argv0.into().into(), OsString::from("--help")])
            .err()
            .unwrap()
            .try_into_help()
            .unwrap()
    }
}

/// Derive macro generating top-level [`Parser`][trait@Parser] implementation.
///
/// This macro currently only accepts non-generic `struct`s, and is
/// a superset of [`Args`][macro@Args].
/// `derive(Args)` must *NOT* be used if there is already `derive(Parser)`.
///
/// All attributes supported on the struct are identical to [`Args`][macro@Args].
pub use palc_derive::Parser;

/// Derive macro for a composable collection of CLI arguments.
///
/// *Note: Currently only this derive-macro is part of public API, but the
/// trait `Args` is not.*
///
/// This macro only accepts non-generic `struct`s. The type deriving this macro
/// can be used by other types via `#[command(flatten)]`.
///
/// # Container attributes
///
/// These subcommand-level attributes on `struct` are the description of the
/// subcommand itself. They are only meaningful on top-level [`Parser`], or a
/// struct used in [`Subcommand`] enum's tuple variant. If this struct is used
/// as `#[command(flatten)]`, these container attributes are useless and ignored.
///
/// - `#[command(name = "...")]`
///
///   TODO: TBD
///
/// - `#[command(version)]` or `#[command(version = EXPR)]`
///
///   TODO: TBD
///
/// - `#[command(long_about = CONST_EXPR)]` or `#[command(long_about)]`
///
///   Override the default "about" text shown in the generated help output.
///
///   `CONST_EXPR`, if present, must be valid in const-context and has type
///   `&'static str`.
///
///   If no explicit value (`= CONST_EXPR`) is set, package description from
///   `Cargo.toml` is used as its value.
///   If this attribute is not present, the doc-comment of the struct is used
///   as its value.
///
/// - `#[command(after_long_about = CONST_EXPR)]`
///
///   `CONST_EXPR`, if present, must be valid in const-context and has type
///   `&'static str`.
///
///   Additional text to be printed after the generated help output.
///
/// - `#[doc = "..."]` or `/// ...`
///
///   The default value for `long_about` if that attribute is not present.
///
/// # Field attributes for composition
///
/// - `#[command(flatten)]`
///
///   Flatten (inline) a collection of arguments into this struct.
///
///   The field type must derive [`Args`], must not contains a subcommand field,
///   and must not form a dependency cycle, otherwise a compile error is
///   emitted.
///
///   - Named arguments
///
///     Since named arguments are unordered, flattened arguments and
///     non-flattened ones can be parsed in any interleaving order.
///
///     In a struct, all named arguments, including flattened ones, should *NOT*
///     have collide names. Otherwise, the behavior is unspecified but
///     deterministic, and may change in the future.
///     The current behavior is: only one argument "wins" and consumes the value.
///
///   - Unnamed arguments
///
///     TODO: Not yet supported to be flattened.
///
/// - `#[command(subcommand)]`
///
///   Define a subcommand. At most one field can be marked as subcommand.
///   The field type must be either `SubcmdType` or `Option<SubcmdType>` where
///   `SubcmdType` derives [`Subcommand`].
///
/// # Field attributes for arguments
///
/// TODO
pub use palc_derive::Args;

/// Derive macro for a composable enum of CLI subcommands.
///
/// *Note: Currently only this derive-macro is part of public API, but the
/// trait `Subcommand` is not.*
///
/// This macro supports non-generic `enum`s.
///
/// # Subcommand names
///
/// Each variant corresponds to a subcommand with the name being the variant
/// identifier converted to "kebab-case".
/// Duplicated names after case conversion produce a compile error.
///
/// # Supported variants
///
/// - `UnitVariant`
///
///   A subcommand with no arguments on its own. Additional
///   arguments may still be accepted after it, if there is any from ancestor
///   subcommands.
///
///   [`#[command(..)]` container attributes][command_attrs] are allowed on this variant.
///
/// - `StructVariant { .. }`
///
///   An inlined subcommand. Variant fields are handled in
///   the same way as fields of [`derive(Args)`][Args]. See its docs for details.
///
///   [`#[command(..)]` container attributes][command_attrs] are allowed on this variant.
///
/// - `TupleVariant(AnotherType)`
///
///   An outlined subcommand where `AnotherType` must derive [`Args`].
///
///   [`#[command(..)]` container attributes][command_attrs] are *NOT* allowed on this variant.
///   Instead, those on `AnotherType` are used.
///
/// - Other kinds of variant are rejected.
///
/// [command_attrs]: derive.Args.html#container-attributes
pub use palc_derive::Subcommand;

/// Derive macro for enums parsable from argument values.
///
/// *Note: Currently only this derive-macro is part of public API, but the
/// trait `ValueEnum` is not.*
///
/// # Container attributes
///
/// - `#[value(rename_all = "...")]`
///
///   Override the variant name case conversion.
///   When not present, the default value is `"kebab-case"`.
///
///   All supported conversions:
///   - `"camelCase"`
///   - `"kebab-case"`
///   - `"PascalCase"`
///   - `"SCREAMING_SNAKE_CASE"`
///   - `"snake_case"`
///   - `"lower"`
///   - `"UPPER"`
///   - `"verbatim"`
///
/// # Variant attributes
///
/// - `#[value(name = "...")]`
///
///   Override the default case-converted name for this variant.
///   The string literal is taken verbatim for parsing, but it is still
///   checked for forbiddgen characters and conflicts. See the next section.
///
///   If not present, the variant name after case conversion (see previous section).
///
/// # Errors
///
/// - ASCII control characters are forbidden from subcommand names, due to
///   implementation limitations. A compile error is emitted if any is found.
///
/// - The macro will check all variant names for parsing (after case conversion if
///   necessary) do not collide. Otherwise a compile error is emitted.
pub use palc_derive::ValueEnum;
