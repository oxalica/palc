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
    util::{split_once, split_terminator},
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

    /// Mapping "encoded" named arguments into its attributes.
    /// They are sorted by names for binary searching.
    ///
    /// - Long arguments are encoded as-is, eg. "--long".
    /// - Short arguments are encoded without dash prefix, eg. "F".
    // FIXME: Use index into `descriptions` instead of `&str`.
    named_attrs: &'static [(&'static str, ArgAttrs)],

    /// Attributes of positional arguments.
    pub(crate) positional_attrs: &'static [ArgAttrs],

    /// Child subcommands.
    ///
    /// NB: This is used for parsing.
    pub(crate) subcmd_info: Option<RawSubcommandInfo>,

    /// Is the child subcommand optional or required? Only useful if there are subcommands.
    #[cfg(feature = "help")]
    pub(crate) is_subcmd_optional: bool,

    /// If there is any optional named args, so that "[OPTIONS]" should be shown?
    #[cfg(feature = "help")]
    pub(crate) has_optional_named: bool,

    /// Help string formatter.
    ///
    /// This is a mix of multiple part-formatters and is selected by precision flag.
    /// - `{:.0}`: Named argument usage, with leading space.
    /// - `{:.1}`: Positional argument usage, with leading space.
    /// - `{:.2}`: Named argument long help.
    /// - `{:.3}`: Positional argument long help.
    /// - `{:.4}`: `about`
    /// - `{:.5}`: `after_long_help`
    #[cfg(feature = "help")]
    pub(crate) help: &'static dyn fmt::Display,
}

impl RawArgsInfo {
    // Used by proc-macro.
    pub const EMPTY_REF: &'static Self = &Self::new("", &[], &[], None, false, false, &"", &[]);

    // Used by proc-macro.
    pub const fn new(
        descriptions: &'static str,
        named_attrs: &'static [(&'static str, ArgAttrs)],
        positional_attrs: &'static [ArgAttrs],
        subcmd_info: Option<RawSubcommandInfo>,

        is_subcmd_optional: bool,
        mut has_optional_named: bool,
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
            is_subcmd_optional,
            #[cfg(feature = "help")]
            has_optional_named,
            #[cfg(feature = "help")]
            help,
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

    /// Iterate over subcommands with their aggregated helps.
    #[cfg(feature = "help")]
    pub(crate) fn subcommands_with_help(
        &self,
    ) -> Option<impl Iterator<Item = (&'static str, &'static dyn fmt::Display)> + Clone> {
        let subcmd = self.subcmd_info.as_ref()?;
        Some(
            split_terminator(subcmd.names, b'\0')
                .zip(subcmd.variant_infos.iter().map(|info| info.help)),
        )
    }

    // FIXME: Used by proc-macro for assertion.
    pub const fn positional_len(&self) -> usize {
        self.positional_attrs.len()
    }

    pub(crate) fn get_named(&self, name: &OsStr) -> Option<ArgAttrs> {
        let i = self
            .named_attrs
            .binary_search_by_key(&name.as_encoded_bytes(), |(key, _)| key.as_bytes())
            .ok()?;
        Some(self.named_attrs[i].1)
    }
}
