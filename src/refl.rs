//! Runtime reflection of arguments, subcommands and help strings.
//! Required by precise error messages and help generations.
//!
//! Most of `&str` here can be changed to thin `&CStr`, which is blocked by extern types.
//! WAIT: <https://github.com/rust-lang/rust/issues/43467>
//!
//! TODO(low): Decide whether to expose these API.
#![cfg_attr(not(feature = "default"), allow(unused))]

#[cfg(not(feature = "help"))]
use std::marker::PhantomData;
use std::{ffi::OsStr, fmt};

use crate::{
    shared::ArgAttrs,
    util::{split_once, split_sep_many, split_terminator},
};

/// Runtime information of a enum of subcommands.
pub struct RawSubcommandInfo {
    /// Zero or more NUL-terminated subcommand names.
    names: &'static str,

    /// Mapping from subcommands to its index.
    // FIXME: Use index into `subcommands` instead of `&str`.
    map: &'static [(&'static str, usize)],

    /// Links to all variant infos in order to show descriptions of
    /// subcommands on the outer level.
    #[cfg(feature = "help")]
    variant_infos: &'static [&'static RawArgsInfo],
}

impl RawSubcommandInfo {
    // Used by proc-macro.
    pub const EMPTY: Self = Self::new("", &[], &[]);

    // NB: Used by proc-macro.
    pub const fn new(
        names: &'static str,
        map: &'static [(&'static str, usize)],
        variant_infos: &'static [&'static RawArgsInfo],
    ) -> Self {
        Self {
            names,
            map,
            #[cfg(feature = "help")]
            variant_infos,
        }
    }

    pub(crate) fn search(&self, cmd: &OsStr) -> Option<(&'static str, usize)> {
        let i = self
            .map
            .binary_search_by_key(&cmd.as_encoded_bytes(), |(key, _)| key.as_bytes())
            .ok()?;
        Some(self.map[i])
    }
}

pub struct RawArgsInfo {
    /// Zero or more '\0'-terminated argument descriptions, either:
    /// `-s`, `--long`, `-s, --long=<VALUE>`, `<REQUIRED>`, or `[OPTIONAL]`.
    descriptions: &'static str,

    /// Mapping named arguments into its attributes.
    /// They are sorted by names for binary searching.
    // FIXME: Use index into `descriptions` instead of `&str`.
    named_attrs: &'static [(&'static str, ArgAttrs)],

    /// Attributes of positional arguments.
    positional_attrs: &'static [ArgAttrs],

    /// Child subcommands.
    ///
    /// NB: This is used for parsing.
    pub(crate) subcmd_info: Option<RawSubcommandInfo>,

    /// Is the child subcommand optional or required? Only useful if there are subcommands.
    #[cfg(feature = "help")]
    subcmd_optional: bool,

    /// If there is any optional named args, so that "[OPTIONS]" should be shown?
    #[cfg(feature = "help")]
    has_optional_named: bool,

    /// The documentation about this command applet.
    ///
    /// This consists of '\0'-separated following elements:
    /// - long_about
    /// - after_long_help
    #[cfg(feature = "help")]
    cmd_doc: &'static str,

    /// Help string formatter.
    ///
    /// This is a mix of multiple part-formatters and is selected by precision flag.
    /// - `{:.0}`: Named argument usage, with leading space.
    /// - `{:.1}`: Positional argument usage, with leading space.
    /// - `{:.2}`: Named argument long help.
    /// - `{:.3}`: Positional argument long help.
    #[cfg(feature = "help")]
    help: &'static dyn fmt::Display,
}

impl RawArgsInfo {
    // Used by proc-macro.
    pub const EMPTY_REF: &'static Self = &Self::new("", &[], &[], None, false, false, "", &"", &[]);

    // Used by proc-macro.
    pub const fn new(
        descriptions: &'static str,
        named_attrs: &'static [(&'static str, ArgAttrs)],
        positional_attrs: &'static [ArgAttrs],
        subcmd_info: Option<RawSubcommandInfo>,

        subcmd_optional: bool,
        mut has_optional_named: bool,
        cmd_doc: &'static str,
        help: &'static dyn fmt::Display,
        flattened: &[&RawArgsInfo],
    ) -> RawArgsInfo {
        #[cfg(feature = "help")]
        {
            let len = flattened.len();
            let mut i = 0usize;
            while i < len {
                has_optional_named |= flattened[i].has_optional_named;
                i += 1;
            }
        }

        RawArgsInfo {
            descriptions,
            named_attrs,
            positional_attrs,
            subcmd_info,

            #[cfg(feature = "help")]
            subcmd_optional,
            #[cfg(feature = "help")]
            has_optional_named,
            #[cfg(feature = "help")]
            cmd_doc,
            #[cfg(feature = "help")]
            help,
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

    // FIXME: This is quite complex. Can we directly codegen byte offset of
    // description string, instead of using `idx`?
    pub(crate) fn get_description(&self, mut idx: u8) -> Option<&'static str> {
        // See `RawArgsInfo`.
        let mut desc = self.descriptions;
        while let Some((lhs, rhs)) = split_once(desc, b'\0') {
            if idx == 0 {
                return Some(lhs);
            }
            idx -= 1;
            desc = rhs;
        }
        None
    }

    #[cfg(feature = "help")]
    pub(crate) fn has_optional_named(&self) -> bool {
        self.has_optional_named
    }

    // Used by proc-macro for composing `help` that contains sub-args.
    pub fn help(&self) -> &'static dyn fmt::Display {
        #[cfg(feature = "help")]
        {
            self.help
        }
        #[cfg(not(feature = "help"))]
        {
            &""
        }
    }

    #[cfg(feature = "help")]
    pub(crate) fn doc(&self) -> CommandDoc {
        let [long_about, after_long_help] = split_sep_many(self.cmd_doc, b'\0').unwrap_or([""; 2]);
        CommandDoc { long_about, after_long_help }
    }

    /// Iterate over subcommands and short descriptions.
    #[cfg(feature = "help")]
    pub(crate) fn subcommands(
        &self,
    ) -> Option<impl Iterator<Item = (&'static str, &'static str)> + Clone> {
        let subcmd = self.subcmd_info.as_ref()?;
        Some(
            split_terminator(subcmd.names, b'\0').zip(
                self.subcmd_info
                    .as_ref()
                    .unwrap()
                    .variant_infos
                    .iter()
                    .map(|variant| variant.doc().long_about),
            ),
        )
    }

    #[cfg(feature = "help")]
    pub(crate) fn subcommand_optional(&self) -> bool {
        self.subcmd_optional
    }

    // FIXME: Used by proc-macro for assertion.
    pub const fn positional_len(&self) -> usize {
        self.positional_attrs.len()
    }

    pub(crate) fn get_named(&self, name: &str) -> Option<ArgAttrs> {
        let i = self.named_attrs.binary_search_by_key(&name, |(key, _)| *key).ok()?;
        Some(self.named_attrs[i].1)
    }

    pub(crate) fn positional_attrs(&self) -> &[ArgAttrs] {
        self.positional_attrs
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct CommandDoc {
    pub(crate) long_about: &'static str,
    pub(crate) after_long_help: &'static str,
}
