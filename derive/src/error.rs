//! A simplified version of proc-macro-error.
//!
//! Ref: <https://crates.io/crates/proc-macro-error>
use std::cell::Cell;

use proc_macro2::{Span, TokenStream};
use quote::quote_spanned;

thread_local! {
    static ERROR_TTS: Cell<Option<TokenStream>> = const { Cell::new(None) };
}

// For unrecoverable errors.
pub type Result<T, E = ()> = std::result::Result<T, E>;

macro_rules! emit_error {
    ($src:expr, $($tt:tt)+) => {
        crate::error::emit_error(syn::spanned::Spanned::span(&$src), &format!($($tt)+))
    };
}

macro_rules! abort {
    ($src:expr, $($tt:tt)+) => {{
        emit_error!($src, $($tt)+);
        return Err(());
    }};
}

pub fn try_syn<T>(ret: syn::Result<T>) -> Option<T> {
    match ret {
        Ok(v) => Some(v),
        Err(err) => {
            let mut tts = ERROR_TTS.take().unwrap_or_default();
            tts.extend(err.to_compile_error());
            ERROR_TTS.set(Some(tts));
            None
        }
    }
}

pub fn emit_error(span: Span, msg: &str) {
    let mut tts = ERROR_TTS.take().unwrap_or_default();
    tts.extend(quote_spanned! {span=> ::std::compile_error! { #msg } });
    ERROR_TTS.set(Some(tts));
}

pub fn catch_errors<T>(f: impl FnOnce() -> Result<T>) -> Result<T, TokenStream> {
    struct Guard(Option<TokenStream>);
    impl Drop for Guard {
        fn drop(&mut self) {
            ERROR_TTS.set(self.0.take());
        }
    }

    let _guard = Guard(ERROR_TTS.replace(None));
    let ret = f();
    let errors = ERROR_TTS.take();
    match (ret, errors) {
        (Ok(v), None) => Ok(v),
        (_, Some(err)) => {
            assert!(!err.is_empty());
            Err(err)
        }
        (Err(()), None) => unreachable!(),
    }
}
