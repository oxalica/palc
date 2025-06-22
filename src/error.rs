use std::ffi::OsString;
use std::fmt;

use crate::{
    refl::RawArgsInfo,
    runtime::{CommandInternal, ParserState},
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

    #[cfg(feature = "help")]
    subcommand_path: crate::help::SubcommandPath,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum ErrorKind {
    // Input parsing errors.
    MissingArg0,
    InvalidUtf8,
    UnknownNamedArgument,
    UnknownSubcommand,
    DuplicatedNamedArgument,
    ExtraUnnamedArgument,
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
    #[cfg(feature = "help")]
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
        let e = &*self.0;
        let mut s = f.debug_struct("Error");
        s.field("kind", &e.kind)
            .field("arg_desc", &e.arg_desc)
            .field("input", &e.input)
            .field("source", &e.source);
        #[cfg(feature = "help")]
        {
            let subcmds = e.subcommand_path.iter().rev().map(|(_, cmd)| cmd).collect::<Vec<_>>();
            s.field("subcommand_path", &subcmds);
        }
        s.finish_non_exhaustive()
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let e = &*self.0;

        let opt_input = |f: &mut fmt::Formatter<'_>| {
            if let Some(input) = &e.input {
                f.write_str(" '")?;
                f.write_str(&input.to_string_lossy())?;
                f.write_str("'")?;
            }
            Ok(())
        };
        let opt_arg = |f: &mut fmt::Formatter<'_>, with_for: bool| {
            if let Some(desc) = &e.arg_desc {
                f.write_str(if with_for { " for '" } else { " '" })?;
                f.write_str(desc)?;
                f.write_str("'")?;
            }
            Ok(())
        };
        let opt_for_arg = |f: &mut fmt::Formatter<'_>| opt_arg(f, true);

        match &e.kind {
            ErrorKind::MissingArg0 => f.write_str("missing executable argument (argv[0])"),
            ErrorKind::InvalidUtf8 => {
                f.write_str("invalid UTF-8")?;
                opt_input(f)?;
                opt_for_arg(f)
            }
            ErrorKind::UnknownNamedArgument => {
                f.write_str("unexpected argument")?;
                opt_input(f)
            }
            ErrorKind::UnknownSubcommand => {
                f.write_str("unrecognized subcommand")?;
                opt_input(f)
            }
            ErrorKind::DuplicatedNamedArgument => {
                f.write_str("the argument")?;
                opt_arg(f, false)?;
                f.write_str(" cannot be used multiple times")
            }
            ErrorKind::ExtraUnnamedArgument => {
                f.write_str("unexpected argument")?;
                opt_input(f)
            }
            ErrorKind::UnexpectedInlineValue => {
                f.write_str("unexpected value")?;
                opt_input(f)?;
                opt_for_arg(f)
            }
            ErrorKind::MissingValue => {
                f.write_str("a value is required")?;
                opt_for_arg(f)?;
                f.write_str(" but none was supplied")
            }
            ErrorKind::InvalidValue => {
                f.write_str("invalid value")?;
                opt_input(f)?;
                opt_for_arg(f)?;
                if let Some(strs) = e.possible_inputs_nul {
                    f.write_str("\n  [possible values: ")?;
                    let mut first = true;
                    for s in strs.split_terminator('\0') {
                        if first {
                            first = false
                        } else {
                            f.write_str(", ")?
                        }
                        f.write_str(s)?;
                    }
                    f.write_str("]")?;
                }
                Ok(())
            }
            ErrorKind::MissingEq => {
                f.write_str("equal sign is needed when assigning values")?;
                opt_for_arg(f)
            }

            ErrorKind::MissingRequiredArgument => {
                f.write_str("the argument")?;
                opt_arg(f, false)?;
                f.write_str(" is required but not provided")
            }
            ErrorKind::MissingRequiredSubcommand => {
                f.write_str("the subcommand is required but not provided")
                // TODO: Possible subcommands.
            }
            ErrorKind::ConstraintRequired => {
                f.write_str("the argument")?;
                opt_arg(f, false)?;
                f.write_str(" is required but not provided")
            }
            ErrorKind::ConstraintExclusive => {
                f.write_str("the argument")?;
                opt_arg(f, false)?;
                f.write_str(" cannot be used with one or more of the other specified arguments")
            }
            ErrorKind::ConstraintConflict => {
                f.write_str("the argument")?;
                opt_arg(f, false)?;
                // TODO: Conflict with what?
                f.write_str(" cannot be used with some other arguments")
            }

            #[cfg(feature = "help")]
            ErrorKind::Help => {
                let mut out = String::new();
                crate::help::render_help_into(&mut out, &e.subcommand_path);
                f.write_str(&out)
            }

            ErrorKind::Custom => self.0.source.as_ref().unwrap().fmt(f),
        }
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
            #[cfg(feature = "help")]
            subcommand_path: Vec::new(),
        }))
    }

    /// Create an custom error with given reason.
    pub fn custom(reason: impl Into<String>) -> Self {
        let source = reason.into().into();
        let mut e = Self::new(ErrorKind::Custom);
        e.0.source = Some(source);
        e
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

    #[cfg(feature = "help")]
    pub(crate) fn in_subcommand<C: CommandInternal>(mut self, subcmd: String) -> Self {
        self.0.subcommand_path.push((C::RAW_COMMAND_INFO, subcmd));
        self
    }

    #[cfg(not(feature = "help"))]
    pub(crate) fn in_subcommand<S: CommandInternal>(self, _subcmd: String) -> Self {
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
        Error::new(self).with_arg_desc(
            RawArgsInfo::arg_descriptions_of(S::RAW_ARGS_INFO.__raw_arg_descs).nth(arg_idx.into()),
        )
    }
}
