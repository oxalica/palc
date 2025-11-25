use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use syn::{Data, DeriveInput, FieldsNamed};

use crate::{
    common::wrap_anon_item,
    error::{Result, catch_errors},
};

pub fn expand(input: &DeriveInput) -> TokenStream {
    assert_no_generics!(input);

    match catch_errors(|| match &input.data {
        Data::Struct(syn::DataStruct { fields: syn::Fields::Named(fields), .. }) => {
            try_expand_for_named_struct(input, fields)
        }
        _ => abort!(Span::call_site(), "only structs with named fields are supported yet"),
    }) {
        Ok(tts) => wrap_anon_item(tts),
        Err(mut tts) => {
            tts.extend(wrap_anon_item(fallback(&input.ident)));
            tts
        }
    }
}

fn fallback(ident: &Ident) -> TokenStream {
    quote! {
        #[automatically_derived]
        impl __rt::Parser for #ident {}

        #[automatically_derived]
        impl __rt::ParserInternal for #ident {
            type __Flavor = __rt::FallbackParserFlavor;
        }
    }
}

fn try_expand_for_named_struct(input: &DeriveInput, fields: &FieldsNamed) -> Result<TokenStream> {
    let mut tts = crate::derive_args::try_expand_for_named_struct(input, fields)?;
    let ident = &input.ident;
    tts.extend(quote! {
        #[automatically_derived]
        impl __rt::Parser for #ident {}

        #[automatically_derived]
        impl __rt::ParserInternal for #ident {
            type __Flavor = __rt::StructParserFlavor<Self>;
        }
    });
    Ok(tts)
}
