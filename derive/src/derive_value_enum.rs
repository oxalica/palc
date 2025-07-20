use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use syn::{DeriveInput, Generics, Ident};

use crate::common::{ValueEnumMeta, ValueVariantMeta, wrap_anon_item};

pub(crate) fn expand(input: &DeriveInput) -> TokenStream {
    let mut tts = match expand_impl(input) {
        Ok(tts) => return wrap_anon_item(tts),
        Err(err) => err.to_compile_error(),
    };

    tts.extend(wrap_anon_item(ValueEnumImpl {
        ident: &input.ident,
        generics: &input.generics,
        variants: Vec::new(),
    }));
    tts
}

fn expand_impl(def: &DeriveInput) -> syn::Result<ValueEnumImpl<'_>> {
    let syn::Data::Enum(enum_def) = &def.data else {
        return Err(syn::Error::new(
            Span::call_site(),
            "derive(ValueEnum) can only be used on enums",
        ));
    };

    let enum_meta = ValueEnumMeta::parse_attrs(&def.attrs)?;

    let mut variants = enum_def
        .variants
        .iter()
        .map(|variant| {
            if !matches!(variant.fields, syn::Fields::Unit) {
                return Err(syn::Error::new(
                    variant.ident.span(),
                    "only unit variant is supported",
                ));
            }
            let variant_meta = ValueVariantMeta::parse_attrs(&variant.attrs)?;

            let parse_name = match variant_meta.name {
                Some(name) => name,
                None => enum_meta.rename_all.rename(variant.ident.to_string()),
            };

            if parse_name.contains(|c: char| c.is_ascii_control()) {
                return Err(syn::Error::new(
                    variant.ident.span(),
                    "value names containing ASCII control characters are not supported",
                ));
            }

            Ok(Variant { parse_name, ident: &variant.ident })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    variants.sort_by(|lhs, rhs| Ord::cmp(&lhs.parse_name, &rhs.parse_name));
    if let Some(w) = variants.windows(2).find(|w| w[0].parse_name == w[1].parse_name) {
        let mut err = syn::Error::new(
            w[0].ident.span(),
            format!("duplicated possible values {:?}", w[0].parse_name),
        );
        err.combine(syn::Error::new(w[1].ident.span(), "second variant here"));
        return Err(err);
    }

    Ok(ValueEnumImpl { ident: &def.ident, generics: &def.generics, variants })
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

        tokens.extend(quote! {
            #[automatically_derived]
            impl #impl_generics __rt::ValueEnum for #name #ty_generics #where_clause {
                const POSSIBLE_INPUTS_NUL: &'static str = #possible_inputs_nul;

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
