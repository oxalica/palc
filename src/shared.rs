//! NB. This file is shared between library and proc-macro crates.

use std::num::NonZero;

// TODO: Is it better to encode this as an integer to reduce proc-macro codegen?
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ArgAttrs {
    /// The number of values this argument expects.
    /// Currently only 0 and 1 are implemented.
    pub num_values: u8,
    /// Does this argument require an inlined value via `=`?
    pub require_eq: bool,
    /// Does this argument eat the next raw argument even if it starts with `-`?
    pub accept_hyphen: AcceptHyphen,
    /// ASCII value delimiter.
    pub delimiter: Option<NonZero<u8>>,
    /// Is this a global argument?
    pub global: bool,
    /// Is this argument argument required?
    pub required: bool,
    // Make the value lowercase before parsing it?
    pub make_lowercase: bool,

    /// The field index in the containing struct implementing `Args`.
    pub index: u8,
}

impl ArgAttrs {
    #[allow(unused, reason = "used by runtime, but not used by proc-macro itself")]
    pub fn new() -> Self {
        ArgAttrs::default()
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum AcceptHyphen {
    #[default]
    No,
    NegativeNumber,
    Yes,
}
