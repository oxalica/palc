use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use syn::{Data, DataEnum, DeriveInput, Generics, Ident};

use crate::{
    common::{ValueEnumMeta, ValueVariantMeta, wrap_anon_item},
    error::catch_errors,
};

pub(crate) fn expand(input: &DeriveInput) -> TokenStream {
    match catch_errors(|| match &input.data {
        Data::Enum(data) => Ok(expand_for_enum(input, data)),
        _ => abort!(Span::call_site(), "only enums are supported"),
    }) {
        Ok(tts) => wrap_anon_item(tts),
        Err(mut tts) => {
            tts.extend(wrap_anon_item(ValueEnumImpl {
                ident: &input.ident,
                generics: &input.generics,
                variants: Vec::new(),
            }));
            tts
        }
    }
}

fn expand_for_enum<'a>(input: &'a DeriveInput, data: &'a DataEnum) -> ValueEnumImpl<'a> {
    let enum_meta = ValueEnumMeta::parse_attrs(&input.attrs);

    let mut variants = data
        .variants
        .iter()
        .filter_map(|variant| {
            if !matches!(variant.fields, syn::Fields::Unit) {
                emit_error!(variant.ident, "only unit variant is supported");
                return None;
            }
            let variant_meta = ValueVariantMeta::parse_attrs(&variant.attrs);

            let parse_name = match variant_meta.name {
                Some(name) => name,
                None => enum_meta.rename_all.rename(variant.ident.to_string()),
            };

            if parse_name.contains(|c: char| c.is_ascii_control()) {
                emit_error!(
                    variant.ident,
                    "value names containing ASCII control characters are not supported",
                );
                return None;
            }

            Some(Variant { parse_name, ident: &variant.ident })
        })
        .collect::<Vec<_>>();

    variants.sort_by(|lhs, rhs| Ord::cmp(&lhs.parse_name, &rhs.parse_name));
    if let Some(w) = variants.windows(2).find(|w| w[0].parse_name == w[1].parse_name) {
        emit_error!(w[0].ident, "duplicated possible values {:?}", w[0].parse_name);
        emit_error!(w[1].ident.span(), "second variant here");
    }

    ValueEnumImpl { ident: &input.ident, generics: &input.generics, variants }
}

struct ValueEnumImpl<'i> {
    ident: &'i Ident,
    generics: &'i Generics,
    variants: Vec<Variant<'i>>,
}

struct Variant<'i> {
    parse_name: String,
    ident: &'i Ident,
}

impl ToTokens for ValueEnumImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = self.ident;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
        let variant_strs = self.variants.iter().map(|v| &v.parse_name);
        let variant_idents = self.variants.iter().map(|v| v.ident);
        let possible_inputs_nul =
            self.variants.iter().flat_map(|v| [&v.parse_name, "\0"]).collect::<String>();

        let no_upper_case =
            self.variants.iter().all(|v| v.parse_name.bytes().all(|b| !b.is_ascii_uppercase()));

        tokens.extend(quote! {
            #[automatically_derived]
            impl #impl_generics __rt::ValueEnum for #name #ty_generics #where_clause {
                const POSSIBLE_INPUTS_NUL: &'static __rt::str = #possible_inputs_nul;
                const NO_UPPER_CASE: __rt::bool = #no_upper_case;

                // If there is no variant.
                #[allow(unreachable_code)]
                fn parse_value(__v: &__rt::str) -> __rt::Option<Self> {
                    __rt::Some(match __v {
                        #(#variant_strs => Self:: #variant_idents,)*
                        _ => return __rt::None
                    })
                }
            }
        });
    }
}
