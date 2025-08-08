//! Runtime reflection of arguments, subcommands and help strings.
//! Required by precise error messages and help generations.
//!
//! Most of `&str` here can be changed to thin `&CStr`, which is blocked by extern types.
//! WAIT: <https://github.com/rust-lang/rust/issues/43467>
//!
//! TODO(low): Decide whether to expose these API.
#![cfg_attr(not(feature = "default"), allow(unused))]

use std::fmt;
#[cfg(not(feature = "help"))]
use std::marker::PhantomData;

/// Runtime information of a enum of subcommands.
#[derive(Debug)]
pub struct RawSubcommandInfo<A: ?Sized = [&'static str]> {
    /// Zero or more NUL-terminated subcommand names.
    #[cfg(feature = "help")]
    subcommands: &'static str,

    /// `RawArgsInfo::cmd_doc` of each subcommand.
    #[cfg(feature = "help")]
    cmd_docs: A,

    #[cfg(not(feature = "help"))]
    _marker: PhantomData<A>,
}

impl RawSubcommandInfo {
    pub(crate) const EMPTY_REF: &Self = &Self::new("", []);

    // Used by proc-macro.
    #[cfg(feature = "help")]
    pub const fn new<const N: usize>(
        subcommands: &'static str,
        cmd_docs: [&'static str; N],
    ) -> RawSubcommandInfo<[&'static str; N]> {
        RawSubcommandInfo { subcommands, cmd_docs }
    }

    #[cfg(not(feature = "help"))]
    pub const fn new<const N: usize>(
        _subcommands: &'static str,
        _cmd_docs: [&'static str; N],
    ) -> Self {
        Self { _marker: PhantomData }
    }
}

/// - `w` will always be a `&mut String` but type-erased to avoid aggressive
///   inlining (reserve, fail handling, inlined memcpy).
/// - `what` indicates what to format, see constants below.
type FmtWriter = fn(w: &mut dyn fmt::Write, what: u8);

const fn fmt_noop(_w: &mut dyn fmt::Write, _what: u8) {}

// This should be an enum but we use numbers to simplify proc-macro codegen.
pub(crate) const FMT_UNNAMED: u8 = 0;
pub(crate) const FMT_NAMED: u8 = 1;
pub(crate) const FMT_USAGE_UNNAMED: u8 = 2;
pub(crate) const FMT_USAGE_NAMED: u8 = 3;

#[derive(Debug)]
pub struct RawArgsInfo {
    /// Zero or more '\0'-terminated argument descriptions, either:
    /// `-s`, `--long`, `-s, --long=<VALUE>`, `<REQUIRED>`, or `[OPTIONAL]`.
    descriptions: &'static str,

    /// Is the child subcommand optional or required? Only useful if there are subcommands.
    #[cfg(feature = "help")]
    subcmd_optional: bool,

    /// If there is any optional named args, so that "[OPTIONS]" should be shown?
    #[cfg(feature = "help")]
    has_optional_named: bool,

    /// Child subcommands.
    #[cfg(feature = "help")]
    subcmd_info: Option<&'static RawSubcommandInfo>,

    /// The documentation about this command applet.
    ///
    /// This consists of '\0'-separated following elements:
    /// - long_about
    /// - after_long_help
    #[cfg(feature = "help")]
    cmd_doc: &'static str,

    /// Help string formatter.
    #[cfg(feature = "help")]
    fmt_help: FmtWriter,
}

impl RawArgsInfo {
    pub(crate) const EMPTY_REF: &'static Self = &Self::new(false, false, None, "", "", fmt_noop);

    // Used by proc-macro.
    pub const fn new(
        subcmd_optional: bool,
        has_optional_named: bool,
        subcmd_info: Option<&'static RawSubcommandInfo>,
        cmd_doc: &'static str,
        descriptions: &'static str,
        fmt_help: FmtWriter,
    ) -> Self {
        Self {
            descriptions,

            #[cfg(feature = "help")]
            subcmd_optional,
            #[cfg(feature = "help")]
            has_optional_named,
            #[cfg(feature = "help")]
            subcmd_info,
            #[cfg(feature = "help")]
            cmd_doc,
            #[cfg(feature = "help")]
            fmt_help,
        }
    }

    // Used by proc-macro for concatenation.
    pub const fn raw_descriptions(&self) -> &str {
        self.descriptions
    }

    // Used by proc-macro for composition.
    pub const fn has_optional_named(&self) -> bool {
        #[cfg(feature = "help")]
        {
            self.has_optional_named
        }
        #[cfg(not(feature = "help"))]
        {
            false
        }
    }

    // Used by proc-macro for composition.
    pub const fn fmt_help(&self) -> FmtWriter {
        #[cfg(feature = "help")]
        {
            self.fmt_help
        }
        #[cfg(not(feature = "help"))]
        {
            fmt_noop
        }
    }

    // Used by proc-macro for construction of `RawSubcommandInfo`.
    pub const fn raw_cmd_docs(&self) -> &str {
        #[cfg(feature = "help")]
        {
            self.cmd_doc
        }
        #[cfg(not(feature = "help"))]
        {
            ""
        }
    }

    #[cfg(feature = "help")]
    pub(crate) fn doc(&self) -> CommandDoc {
        let [long_about, after_long_help] = split_sep_many(self.cmd_doc, b'\0').unwrap_or([""; 2]);
        CommandDoc { long_about, after_long_help }
    }

    pub(crate) fn get_description(&self, idx: u8) -> Option<&'static str> {
        // See `RawArgsInfo`.
        split_terminator(self.descriptions, b'\0').nth(idx.into())
    }

    /// Iterate over subcommands and short descriptions.
    #[cfg(feature = "help")]
    pub(crate) fn subcommands(
        &self,
    ) -> Option<impl Iterator<Item = (&'static str, &'static str)> + Clone> {
        let subcmd = self.subcmd_info?;
        Some(split_terminator(subcmd.subcommands, b'\0').zip(
            subcmd.cmd_docs.iter().map(|raw_doc| split_once(raw_doc, b'\0').unwrap_or(("", "")).0),
        ))
    }

    #[cfg(feature = "help")]
    pub(crate) fn subcommand_optional(&self) -> bool {
        self.subcmd_optional
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CommandDoc {
    pub(crate) long_about: &'static str,
    pub(crate) after_long_help: &'static str,
}

#[inline(never)]
fn split_once(s: &str, b: u8) -> Option<(&str, &str)> {
    assert!(b.is_ascii());
    s.split_once(b as char)
}

fn split_sep_many<const N: usize>(mut s: &str, b: u8) -> Option<[&str; N]> {
    assert!(b.is_ascii());
    let mut arr = [""; N];
    let (last, init) = arr.split_last_mut().unwrap();
    for p in init {
        (*p, s) = split_once(s, b)?;
    }
    *last = s;
    Some(arr)
}

fn split_terminator(mut s: &str, b: u8) -> impl Iterator<Item = &str> + Clone {
    assert!(b.is_ascii());
    std::iter::from_fn(move || {
        let (fst, rest) = split_once(s, b)?;
        s = rest;
        Some(fst)
    })
}
