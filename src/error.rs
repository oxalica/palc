#![cfg_attr(
    not(feature = "default"),
    allow(dead_code, reason = "help generation code can be unused")
)]
use std::ffi::OsString;
use std::fmt;

use crate::{
    runtime::{ParserChainNode, ParserState},
    util::split_terminator,
};

/// We use bound `UserErr: Into<DynStdError>` for conversing user errors.
/// This implies either `UserErr: std::error::Error` or it is string-like.
///
/// Note that `&str: !std::error::Error` so we cannot just use `UserErr: std::error::Err`.
pub(crate) type DynStdError = Box<dyn std::error::Error + Send + Sync + 'static>;

pub struct Error(Box<Inner>);

#[cfg(test)]
struct _AssertErrorIsSendSync
where
    Error: Send + Sync;

struct Inner {
    kind: ErrorKind,

    /// The target argument we are parsing into, when the error occurs.
    /// For unknown arguments or subcommand, this is `None`.
    arg_desc: Option<&'static str>,
    // The unexpected raw input we are parsing, when the error occurs.
    /// For finalization errors like constraint violation, this is `None`.
    input: Option<OsString>,
    /// The underlying source error, if there is any.
    source: Option<DynStdError>,
    /// Possible value strings, terminated by NUL.
    possible_inputs_nul: Option<&'static str>,

    /// Rendered help message.
    help: Option<String>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ErrorKind {
    // Input parsing errors.
    MissingArg0,
    UnknownNamedArgument,
    UnknownSubcommand,
    DuplicatedNamedArgument,
    ExtraPositionalArgument,
    UnexpectedInlineValue,
    MissingValue,
    InvalidValue,
    MissingEq,

    // Finalization errors.
    MissingRequiredArgument,
    MissingRequiredSubcommand,
    ConstraintRequired,
    ConstraintExclusive,
    ConstraintConflict,

    // Not really an error, but for bubbling out.
    Help,

    // User errors.
    Custom,
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.source.as_ref().map(|err| &**err as _)
    }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Error").field("message", &self.to_string()).finish_non_exhaustive()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let e = &*self.0;

        let input = e.input.as_deref().unwrap_or("<unknown>".as_ref());
        let arg_desc = e.arg_desc.unwrap_or("<unknown argument>");

        let fmt_args = match &e.kind {
            ErrorKind::MissingArg0 => format_args!("missing executable argument (argv[0])"),
            ErrorKind::UnknownNamedArgument => format_args!("unexpected argument {input:?}"),
            ErrorKind::UnknownSubcommand => format_args!("unrecognized subcommand {input:?}"),
            ErrorKind::DuplicatedNamedArgument => {
                format_args!("the argument '{arg_desc}' cannot be used multiple times")
            }
            ErrorKind::ExtraPositionalArgument => {
                format_args!("unexpected argument {input:?}")
            }
            ErrorKind::UnexpectedInlineValue => {
                format_args!("unexpected value {input:?} for '{arg_desc}'")
            }
            ErrorKind::MissingValue => {
                format_args!("a value is required for '{arg_desc}' but none was supplied")
            }
            ErrorKind::InvalidValue => {
                // This output contains conditionals, thus cannot use `format_args`...

                write!(f, "invalid value {input:?} for '{arg_desc}'")?;
                if let Some(src_err) = &e.source {
                    f.write_str(": ")?;
                    src_err.fmt(f)?;
                }

                if let Some(strs) = e.possible_inputs_nul {
                    f.write_str("\n  [possible values: ")?;
                    let mut first = true;
                    for s in split_terminator(strs, b'\0') {
                        if first {
                            first = false
                        } else {
                            f.write_str(", ")?
                        }
                        f.write_str(s)?;
                    }
                    f.write_str("]")?;
                }

                return Ok(());
            }
            ErrorKind::MissingEq => {
                format_args!("equal sign is needed when assigning values for '{arg_desc}'")
            }

            ErrorKind::MissingRequiredArgument => {
                format_args!("the argument '{arg_desc}' is required but not provided")
            }
            ErrorKind::MissingRequiredSubcommand => {
                format_args!("the subcommand is required but not provided")
                // TODO: Possible subcommands.
            }
            ErrorKind::ConstraintRequired => {
                format_args!("the argument '{arg_desc}' is required but not provided")
            }
            ErrorKind::ConstraintExclusive => {
                format_args!(
                    "the argument '{arg_desc}' cannot be used with one or more of the other specified arguments"
                )
            }
            ErrorKind::ConstraintConflict => {
                // TODO: Conflict with what?
                format_args!("the argument '{arg_desc}' cannot be used with some other arguments")
            }

            ErrorKind::Help => {
                return f.write_str(e.help.as_deref().unwrap_or("help is not available"));
            }

            ErrorKind::Custom => return self.0.source.as_ref().unwrap().fmt(f),
        };

        f.write_fmt(fmt_args)
    }
}

impl Error {
    fn new(kind: ErrorKind) -> Self {
        Self(Box::new(Inner {
            kind,
            arg_desc: None,
            input: None,
            source: None,
            possible_inputs_nul: None,
            help: None,
        }))
    }

    /// Create an custom error with given reason.
    pub fn custom(reason: impl Into<String>) -> Self {
        let source = reason.into().into();
        let mut e = Self::new(ErrorKind::Custom);
        e.0.source = Some(source);
        e
    }

    /// Render the help string if this error indicates a `--help` is encounered.
    ///
    /// # Errors
    ///
    /// If this error is not about help or feature "help" is disabled, `Err(self)` is returned.
    pub fn try_into_help(mut self) -> Result<String, Self> {
        if let Some(help) = self.0.help.take() { Ok(help) } else { Err(self) }
    }

    pub(crate) fn with_source(mut self, source: DynStdError) -> Self {
        self.0.source = Some(source);
        self
    }

    pub(crate) fn with_arg_desc(mut self, arg_desc: Option<&'static str>) -> Self {
        self.0.arg_desc = arg_desc;
        self
    }

    pub(crate) fn with_possible_values(mut self, possible_inputs_nul: &'static str) -> Self {
        self.0.possible_inputs_nul =
            (!possible_inputs_nul.is_empty()).then_some(possible_inputs_nul);
        self
    }

    #[cfg(not(feature = "help"))]
    pub(crate) fn maybe_render_help(self, _chain: &mut ParserChainNode) -> Self {
        self
    }

    #[cfg(feature = "help")]
    pub(crate) fn maybe_render_help(mut self, chain: &mut ParserChainNode) -> Self {
        if self.0.kind == ErrorKind::Help {
            let out = self.0.help.insert(String::new());
            crate::help::render_help_into(out, chain);
        }
        self
    }
}

impl From<ErrorKind> for Error {
    #[cold]
    fn from(kind: ErrorKind) -> Self {
        Self::new(kind)
    }
}

impl ErrorKind {
    #[cold]
    pub(crate) fn with_input(self, input: OsString) -> Error {
        let mut err = Error::new(self);
        err.0.input = Some(input);
        err
    }

    #[cold]
    pub(crate) fn with_arg_idx<S: ParserState>(self, arg_idx: u8) -> Error {
        let desc = S::RAW_ARGS_INFO.get_description(arg_idx);
        Error::new(self).with_arg_desc(desc)
    }
}
