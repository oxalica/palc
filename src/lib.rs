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
use runtime::{ArgsIter, ParserInternal};

mod error;
mod refl;
mod runtime;
mod shared;
mod values;

#[cfg(feature = "help")]
mod help;

pub use palc_derive::{Args, Parser, Subcommand, ValueEnum};

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
    pub use std::str::from_utf8;
    pub use {Default, Err, Fn, Iterator, None, Ok, Option, Some, Vec, bool, char, str, u8, usize};

    pub use crate::runtime::*;

    // Macros.
    pub use crate::{__const_concat, __gate_help, arg_value_info};
    pub use std::{assert, concat, env, format_args, unimplemented, unreachable};

    // Used by `__arg_value_info!`
    pub use crate::values::{ArgValueInfo, InferValueParser, ValueEnum};

    pub use crate::refl::{RawArgsInfo, RawArgsInfoRef, RawSubcommandInfo};
    pub use crate::shared::{AcceptHyphen, ArgAttrs};
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
        Self::__parse_toplevel(program_name, &mut ArgsIter::new(&mut iter))
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
