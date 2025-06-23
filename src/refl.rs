#![allow(dead_code, reason = "TODO")]

/// Description of a collection of arguments.
///
/// NB. This struct is constructed by proc-macro.
#[doc(hidden)]
#[derive(Debug, Clone, Copy)]
pub struct RawArgsInfo {
    pub __total_arg_cnt: u8,
    pub __total_unnamed_arg_cnt: u8,

    /// Zero or more `\0` terminated subcommand names.
    pub __subcommands: &'static str,

    /// Zero or more '\0'-terminated arg descriptions.
    ///
    /// For named arguments, their descriptions always starts with "-".
    pub __arg_descs: &'static str,

    /// `__applet_doc` of each subcommands. Only used for help generation.
    pub __subcommand_docs: &'static [&'static str],

    /// Zero or more '\0'-terminated arg help texts. Only used for help generation.
    ///
    /// Each raw `ArgInfo` can either be empty for hidden arguments, or consists
    /// of following strings in order:
    /// - `required as u8`.
    /// - Help text.
    pub __arg_helps: &'static str,

    /// The first byte is '1' if there is an optional subcommand, otherwise '0'.
    ///
    /// If command doc is enabled, there are additionally NUL-separated elements
    /// in following order:
    /// - `name`
    /// - `version`
    /// - `about`
    /// - `long_about`
    /// - `long_help`
    /// - `after_long_help`
    pub __applet_doc: &'static str,
}

impl RawArgsInfo {
    // NB. Used by proc-macro.
    pub const fn empty() -> Self {
        Self {
            __total_arg_cnt: 0,
            __total_unnamed_arg_cnt: 0,
            __subcommands: "",
            __arg_descs: "",

            __subcommand_docs: &[],
            __arg_helps: "",
            __applet_doc: "",
        }
    }

    // This is an associated function, to eliminate dependency to the whole
    // struct if possible.
    pub(crate) fn arg_descriptions_of(
        arg_descs: &'static str,
    ) -> impl Iterator<Item = &'static str> {
        // See `RawArgsInfo`.
        split_terminator(arg_descs, b'\0')
    }

    fn args(&self) -> impl Iterator<Item = ArgInfo> {
        // See `RawArgsInfo`.
        split_terminator(self.__arg_descs, b'\0')
            .zip(split_terminator(self.__arg_helps, b'\0'))
            .filter_map(|(desc, raw_help)| ArgInfo::from_raw(desc, raw_help))
    }

    pub(crate) fn named_args(&self) -> impl Iterator<Item = NamedArgInfo> {
        self.args().filter_map(ArgInfo::to_named)
    }

    pub(crate) fn unnamed_args(&self) -> impl Iterator<Item = UnnamedArgInfo> {
        self.args().filter_map(ArgInfo::to_unnamed)
    }

    pub(crate) fn subcommands(&self) -> impl Iterator<Item = (&'static str, AppletDoc)> {
        split_terminator(self.__subcommands, b'\0')
            .zip(self.__subcommand_docs.iter().filter_map(|doc| AppletDoc::from_raw(doc)))
    }

    pub(crate) fn is_subcommand_optional(&self) -> bool {
        self.__applet_doc.as_bytes().first() == Some(&b'1')
    }

    pub(crate) fn doc(&self) -> Option<AppletDoc> {
        AppletDoc::from_raw(self.__applet_doc)
    }
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

/// Help and documentation of a command applet.
#[derive(Debug, Clone, Copy)]
pub struct AppletDoc {
    name: &'static str,
    version: &'static str,
    about: &'static str,
    long_about: &'static str,
    after_help: &'static str,
    after_long_help: &'static str,
}

impl AppletDoc {
    #[inline(never)]
    fn from_raw(raw: &'static str) -> Option<Self> {
        // See `RawArgsInfo`.
        let [name, version, about, long_about, after_help, after_long_help] =
            split_sep_many(raw.get(1..)?, b'\0')?;
        Some(AppletDoc { name, version, about, long_about, after_help, after_long_help })
    }

    pub fn name(&self) -> &str {
        self.name
    }

    pub fn version(&self) -> Option<&str> {
        opt(self.version)
    }

    pub fn about(&self) -> Option<&str> {
        opt(self.about)
    }

    pub fn long_about(&self) -> Option<&str> {
        opt(self.long_about)
    }

    pub fn after_help(&self) -> Option<&str> {
        opt(self.after_help)
    }

    pub fn after_long_help(&self) -> Option<&str> {
        opt(self.after_long_help)
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

fn split_terminator(mut s: &str, b: u8) -> impl Iterator<Item = &str> {
    assert!(b.is_ascii());
    std::iter::from_fn(move || {
        let (fst, rest) = split_once(s, b)?;
        s = rest;
        Some(fst)
    })
}
