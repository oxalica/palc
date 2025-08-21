//! Utilities used by but do not tied to runtime.

#[cfg(feature = "help")]
#[macro_export]
#[doc(hidden)]
macro_rules! __gate_help {
    ($disabled:expr, $($enabled:tt)*) => {
        $($enabled)*
    };
}

#[cfg(not(feature = "help"))]
#[macro_export]
#[doc(hidden)]
macro_rules! __gate_help {
    ($disabled:expr, $($enabled:tt)*) => {
        $disabled
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __const_concat {
    // Fast path for default `__raw_meta`.
    ($($s:literal,)* $(env!($e:literal), $($s2:literal,)*)*) => {
        $crate::__private::concat!($($s,)* $(env!($e), $($s2,)*)*)
    };

    // Match exprs after literals to prevent invisible grouping.
    ($s:expr,) => {
        $s
    };
    ($($s:expr,)*) => {{
        const __STRS: &'static [&'static $crate::__private::str] = &[$($s),*];
        match $crate::__private::from_utf8(&const {
            $crate::__private::const_concat_impl::<{
                $crate::__private::const_concat_len(__STRS)
            }>(__STRS)
        }) {
            $crate::__private::Ok(__s) => __s,
            $crate::__private::Err(_) => $crate::__private::unreachable!(),
        }
    }};
}

pub const fn const_concat_len(strs: &[&str]) -> usize {
    let mut ret = 0;
    let mut i = 0;
    let str_cnt = strs.len();
    while i < str_cnt {
        ret += strs[i].len();
        i += 1;
    }
    ret
}

pub const fn const_concat_impl<const LEN: usize>(strs: &[&str]) -> [u8; LEN] {
    // Invalid UTF-8, to assert `LEN` is not too long.
    let mut buf = [0xFFu8; LEN];
    let mut out: &mut [u8] = &mut buf;
    let mut i = 0;
    let str_cnt = strs.len();
    while i < str_cnt {
        let s = strs[i].as_bytes();
        let (lhs, rhs) = out.split_at_mut(s.len());
        lhs.copy_from_slice(s);
        out = rhs;
        i += 1;
    }
    buf
}

// String splitting specialized for ASCII.

#[inline(never)]
pub(crate) fn split_once(s: &str, b: u8) -> Option<(&str, &str)> {
    assert!(b.is_ascii());
    s.split_once(b as char)
}

pub(crate) fn split_sep_many<const N: usize>(mut s: &str, b: u8) -> Option<[&str; N]> {
    assert!(b.is_ascii());
    let mut arr = [""; N];
    let (last, init) = arr.split_last_mut().unwrap();
    for p in init {
        (*p, s) = split_once(s, b)?;
    }
    *last = s;
    Some(arr)
}

pub(crate) fn split_terminator(mut s: &str, b: u8) -> impl Iterator<Item = &str> + Clone {
    assert!(b.is_ascii());
    std::iter::from_fn(move || {
        let (fst, rest) = split_once(s, b)?;
        s = rest;
        Some(fst)
    })
}
