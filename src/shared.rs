//! NB. This file is shared between library and proc-macro crates.
use std::num::NonZero;

/// Bit-packed attribute of an logical argument.
/// Used to carry additional information into the runtime.
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ArgAttrs(pub u32);

impl ArgAttrs {
    /// New from the field index of the containing struct implementing `Args`.
    ///
    /// It is at bit 0:7.
    ///
    /// NB: The proc-macro codegen relies on this being at the lowest 8-bits, by
    /// adding index offset directly into the underlying `u32`.
    pub const fn index(i: u8) -> Self {
        Self(i as u32)
    }

    #[must_use]
    pub fn get_index(self) -> u8 {
        self.0 as u8
    }

    /// New from an ASCII value delimiter.
    ///
    /// It is at bit 8:15.
    pub const fn delimiter(ch: Option<NonZero<u8>>) -> Self {
        let ch = match ch {
            Some(ch) => ch.get(),
            None => 0,
        };
        assert!(ch.is_ascii());
        Self((ch as u32) << 8)
    }

    #[must_use]
    pub fn get_delimiter(self) -> Option<NonZero<u8>> {
        NonZero::new((self.0 >> 8) as u8)
    }

    /// Is this a named flag that accepts no value? Eg. `--verbose`.
    pub const NO_VALUE: Self = Self(1 << 16);
    /// Does this argument require an inlined value via `=`?
    pub const REQUIRE_EQ: Self = Self(1 << 17);
    /// Does this argument eat the next raw argument even if it starts with `-`?
    pub const ACCEPT_HYPHEN_ANY: Self = Self(1 << 18);
    /// Does this argument eat the next raw argument but only if it is a negative number?
    pub const ACCEPT_HYPHEN_NUM: Self = Self(1 << 19);
    /// Is this a global argument?
    pub const GLOBAL: Self = Self(1 << 20);
    /// Make the value lowercase before parsing it?
    pub const MAKE_LOWERCASE: Self = Self(1 << 21);
    /// Is this a greedy variable-length unnamed args that consumes everything after?
    pub const GREEDY: Self = Self(1 << 22);

    pub fn set(&mut self, other: Self, value: bool) {
        if value {
            self.0 |= other.0;
        }
        // No `else`. This function is a conditional set and never unset.
    }

    #[must_use]
    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    #[must_use]
    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }
}
