#![cfg_attr(
    not(feature = "default"),
    allow(dead_code, reason = "help generation code can be unused")
)]
use std::fmt;
use std::{ffi::OsString, fmt::Write};

use crate::refl::RawArgsInfo;
use crate::runtime::ArgsFrame;

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

    /// The message to be displayed between source error message and help.
    ///
    /// Used for contextual help messages collected during error bubbling,
    /// eg. possible values, suggestions.
    contextual_help: String,

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
                format_args!("invalid value {input:?} for '{arg_desc}'")
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

            // Will be rendered at the end.
            ErrorKind::Help => format_args!(""),

            ErrorKind::Custom => return fmt::Display::fmt(e.source.as_deref().unwrap(), f),
        };

        f.write_fmt(fmt_args)?;
        if let Some(src_err) = e.source.as_deref() {
            f.write_str(": ")?;
            fmt::Display::fmt(src_err, f)?;
        }
        if !e.contextual_help.is_empty() {
            f.write_str("\n  ")?;
            f.write_str(&e.contextual_help)?;
        }
        if e.kind == ErrorKind::Help {
            f.write_str(e.help.as_deref().unwrap_or("help is not available"))?;
        }

        Ok(())
    }
}

impl Error {
    fn new(kind: ErrorKind) -> Self {
        Self(Box::new(Inner {
            kind,
            arg_desc: None,
            input: None,
            source: None,
            contextual_help: String::new(),
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

    pub(crate) fn with_context(mut self, f: fmt::Arguments<'_>) -> Self {
        // String write cannot fail.
        self.0.contextual_help.write_fmt(f).unwrap();
        self
    }

    pub(crate) fn with_arg_desc(mut self, arg_desc: Option<&'static str>) -> Self {
        self.0.arg_desc = arg_desc;
        self
    }

    #[cfg(not(feature = "help"))]
    pub(crate) fn maybe_render_help(self, _: &ArgsFrame) -> Self {
        self
    }

    #[cfg(feature = "help")]
    pub(crate) fn maybe_render_help(mut self, frame: &ArgsFrame) -> Self {
        if self.0.kind == ErrorKind::Help {
            let out = self.0.help.insert(String::new());
            crate::help::render_help_into(out, frame);
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
    pub(crate) fn with_arg_idx(self, info: &RawArgsInfo, idx: u8) -> Error {
        let mut err = Error::new(self);
        err.0.arg_desc = info.get_description(idx);
        err
    }
}
