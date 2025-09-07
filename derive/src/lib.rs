#![forbid(unsafe_code)]
use proc_macro::TokenStream;
use syn::DeriveInput;

#[macro_use]
mod error;

mod common;
mod derive_args;
mod derive_parser;
mod derive_subcommand;
mod derive_value_enum;

#[allow(unused, reason = "some functions are only used at runtime")]
mod shared;

#[proc_macro_derive(Parser, attributes(arg, command))]
pub fn derive_parser(tts: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(tts as DeriveInput);
    derive_parser::expand(&input).into()
}

#[proc_macro_derive(Args, attributes(arg, command))]
pub fn derive_args(tts: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(tts as DeriveInput);
    derive_args::expand(&input).into()
}

#[proc_macro_derive(Subcommand, attributes(arg, command))]
pub fn derive_subcommand(tts: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(tts as DeriveInput);
    derive_subcommand::expand(&input).into()
}

#[proc_macro_derive(ValueEnum, attributes(value))]
pub fn derive_value_enum(tts: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(tts as DeriveInput);
    derive_value_enum::expand(&input).into()
}
