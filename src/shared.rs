//! NB. This file is shared between library and proc-macro crates.

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

    /// Does this argument require a value? This means it will consume the next
    /// argument as its value if no inlined value is provided.
    pub const REQUIRE_VALUE: Self = Self(1 << 16);
    /// Does this argument require an inlined value via `=`?
    pub const REQUIRE_EQ: Self = Self(1 << 17);
    /// Does this argument eat the next raw argument even if it starts with `-`?
    /// Only meaningful if [`Self::REQUIRE_VALUE`] is set.
    pub const ACCEPT_HYPHEN_ANY: Self = Self(1 << 18);
    /// Does this argument eat the next raw argument but only if it is a negative number?
    /// Only meaningful if [`Self::REQUIRE_VALUE`] is set.
    pub const ACCEPT_HYPHEN_NUM: Self = Self(1 << 19);
    /// Is this a global argument?
    pub const GLOBAL: Self = Self(1 << 20);
    /// Make the value lowercase before parsing it?
    pub const MAKE_LOWERCASE: Self = Self(1 << 21);
    /// Is this a variable-length positional argument that consumes indetermined
    /// number of arguments?
    /// NB: This differs from clap's `trailing_var_arg` which is similar to `GREEDY`.
    pub const VAR_ARG: Self = Self(1 << 22);
    /// `VAR_ARG` but in addition also bypasses subcommand check and `-`-prefix check.
    /// This always completely drains the whole input iterator once encountered.
    pub const GREEDY: Self = Self(1 << 23);
    /// Is this the positional argument that is only accessible through `--`?
    pub const LAST: Self = Self(1 << 24);

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
