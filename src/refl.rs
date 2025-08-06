//! Runtime reflection of arguments, subcommands and help strings.
//! Required by precise error messages and help generations.
//!
//! Most of `&str` here can be changed to thin `&CStr`, which is blocked by extern types.
//! WAIT: <https://github.com/rust-lang/rust/issues/43467>
//!
//! TODO(low): Decide whether to expose these API.
#![cfg_attr(not(feature = "default"), allow(unused))]

/// Runtime information of a enum of subcommands.
#[cfg(feature = "help")]
#[derive(Debug)]
pub struct RawSubcommandInfo<A: ?Sized = [&'static str]> {
    /// Zero or more NUL-terminated subcommand names.
    subcommands: &'static str,

    /// `RawArgsInfo::cmd_doc` of each subcommand.
    cmd_docs: A,
}

#[cfg(feature = "help")]
impl RawSubcommandInfo<[&'static str; 0]> {
    pub(crate) const fn empty() -> Self {
        Self::new("", [])
    }
}

#[cfg(feature = "help")]
impl<const N: usize> RawSubcommandInfo<[&'static str; N]> {
    // Used by proc-macro.
    pub const fn new(subcommands: &'static str, cmd_docs: [&'static str; N]) -> Self {
        Self { subcommands, cmd_docs }
    }
}

#[cfg(not(feature = "help"))]
pub struct RawSubcommandInfo(());

#[cfg(not(feature = "help"))]
impl RawSubcommandInfo {
    pub(crate) const fn empty() -> Self {
        Self(())
    }

    pub const fn new<const N: usize>(_: &'static str, _: [&'static str; N]) -> Self {
        Self(())
    }
}

#[derive(Debug)]
pub struct RawArgsInfo {
    /// Zero or more '\0'-terminated argument descriptions, either:
    /// "-s", "--long", "-s, --long=<VALUE>", "<REQUIRED>", or "[OPTIONAL]".
    descriptions: &'static str,

    /// Is the child subcommand optional or required? Only useful if there are subcommands.
    #[cfg(feature = "help")]
    subcmd_optional: bool,

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

    /// Zero or more '\0'-terminated argument help strings, in the same order of `descriptions`.
    #[cfg(feature = "help")]
    helps: &'static str,
}

impl RawArgsInfo {
    pub(crate) const fn empty() -> Self {
        Self::new(false, None, "", "", "")
    }

    // Used by proc-macro.
    pub const fn new(
        subcmd_optional: bool,
        subcmd_info: Option<&'static RawSubcommandInfo>,
        cmd_doc: &'static str,
        descriptions: &'static str,
        helps: &'static str,
    ) -> Self {
        Self {
            descriptions,

            #[cfg(feature = "help")]
            subcmd_optional,
            #[cfg(feature = "help")]
            subcmd_info,
            #[cfg(feature = "help")]
            cmd_doc,
            #[cfg(feature = "help")]
            helps,
        }
    }

    // Used by proc-macro for concatenation.
    pub const fn raw_descriptions(&self) -> &str {
        self.descriptions
    }

    // Used by proc-macro for concatenation.
    pub const fn raw_helps(&self) -> &str {
        #[cfg(feature = "help")]
        {
            self.helps
        }
        #[cfg(not(feature = "help"))]
        {
            ""
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

    #[cfg(feature = "help")]
    fn args(&self) -> impl Iterator<Item = ArgInfo> {
        // See `RawArgsInfo`.
        split_terminator(self.descriptions, b'\0')
            .zip(split_terminator(self.helps, b'\0'))
            .filter_map(|(desc, raw_help)| ArgInfo::from_raw(desc, raw_help))
    }

    #[cfg(feature = "help")]
    pub(crate) fn named_args(&self) -> impl Iterator<Item = NamedArgInfo> {
        self.args().filter_map(ArgInfo::to_named)
    }

    #[cfg(feature = "help")]
    pub(crate) fn unnamed_args(&self) -> impl Iterator<Item = UnnamedArgInfo> {
        self.args().filter_map(ArgInfo::to_unnamed)
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

/// Description of an arguments.
#[derive(Debug, Clone, Copy)]
enum ArgInfo {
    Named(NamedArgInfo),
    Unnamed(UnnamedArgInfo),
}

impl ArgInfo {
    // See `RawArgsInfo`.
    fn from_raw(description: &'static str, raw_help: &'static str) -> Option<Self> {
        (|| {
            // For hidden arguments, this returns `None`.
            let (flags, long_help) = raw_help.split_at_checked(1)?;
            let required = flags.as_bytes()[0] == b'1';
            Some(if description.starts_with('-') {
                Self::Named(NamedArgInfo { description, required, long_help })
            } else {
                Self::Unnamed(UnnamedArgInfo { description, long_help })
            })
        })()
    }

    fn to_named(self) -> Option<NamedArgInfo> {
        if let Self::Named(v) = self { Some(v) } else { None }
    }

    fn to_unnamed(self) -> Option<UnnamedArgInfo> {
        if let Self::Unnamed(v) = self { Some(v) } else { None }
    }
}

/// Description of a named argument.
#[derive(Debug, Clone, Copy)]
pub struct NamedArgInfo {
    required: bool,
    description: &'static str,
    long_help: &'static str,
}

impl NamedArgInfo {
    pub fn description(&self) -> &'static str {
        self.description
    }

    pub fn required(&self) -> bool {
        self.required
    }

    pub fn long_help(&self) -> Option<&'static str> {
        opt(self.long_help)
    }
}

/// Description of an unnamed (positional) argument.
#[derive(Debug, Clone, Copy)]
pub struct UnnamedArgInfo {
    description: &'static str,
    long_help: &'static str,
}

impl UnnamedArgInfo {
    pub fn description(&self) -> &'static str {
        self.description
    }

    pub fn long_help(&self) -> Option<&'static str> {
        opt(self.long_help)
    }
}

fn opt(s: &str) -> Option<&str> {
    if s.is_empty() { None } else { Some(s) }
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
