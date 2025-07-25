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
use std::ffi::OsString;
use std::path::PathBuf;

use error::ErrorKind;
use runtime::{ArgsIter, CommandInternal, ParserState};

mod error;
mod refl;
mod runtime;
mod shared;
mod values;

#[cfg(feature = "help")]
mod help;

#[cfg(feature = "derive")]
pub use palc_derive::{Args, Parser, Subcommand, ValueEnum};

pub use crate::error::Error;
use crate::runtime::{ParserChain, Sealed};
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
    pub use std::marker::PhantomData;
    pub use std::num::NonZero;
    pub use std::str::from_utf8;
    pub use {Default, Err, Fn, Iterator, None, Ok, Option, Some, Vec, bool, char, str, u8, usize};

    pub use crate::runtime::*;

    // Macros.
    pub use crate::{__const_concat, __gate_help, arg_value_info};
    pub use std::{assert, concat, env, unimplemented, unreachable};

    // Used by `__arg_value_info!`
    pub use crate::values::{ArgValueInfo, InferValueParser, ValueEnum};

    pub use crate::refl::RawArgsInfo;
    pub use crate::shared::{AcceptHyphen, ArgAttrs};
    pub use crate::{Args, Parser, Result, Subcommand};
}

/// Top-level command interface.
///
/// Users should only get an implementation via [`derive(Parser)`](macro@Parser).
pub trait Parser: Sized + CommandInternal + Sealed + 'static {
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
        try_parse_from_command(&mut iter)
    }

    #[cfg(feature = "help")]
    fn render_long_help(argv0: impl Into<String>) -> String {
        Self::try_parse_from([argv0.into().into(), OsString::from("--help")])
            .err()
            .unwrap()
            .into_help()
            .unwrap()
    }
}

fn try_parse_from_command<C: CommandInternal>(
    iter: &mut dyn Iterator<Item = OsString>,
) -> Result<C> {
    let arg0 = PathBuf::from(iter.next().ok_or(ErrorKind::MissingArg0)?);
    // A non-UTF8 program name does not matter in help. Multi-call commands will fail anyway.
    let program_name = arg0.file_name().unwrap_or(arg0.as_ref());
    let args = &mut ArgsIter::new(iter);
    let states: &mut dyn ParserChain = &mut ();
    CommandInternal::try_parse_with_name(args, program_name, states)
}

/// A group of arguments for composing larger interface.
///
/// Users should only get an implementation via [`derive(Args)`](macro@Args).
pub trait Args: Sized + Sealed + 'static {
    /// Not public API. Only for proc-macro internal use.
    #[doc(hidden)]
    type __State: ParserState<Output = Self>;
}

/// A subcommand enum.
///
/// Users should only get an implementation via [`derive(Subcommand)`](macro@Subcommand).
pub trait Subcommand: Sized + CommandInternal + Sealed + 'static {}
