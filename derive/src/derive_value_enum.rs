use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use syn::{DeriveInput, Generics, Ident};

use crate::common::wrap_anon_item;

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
            // TODO: Custom renaming.
            let value_str = heck::AsKebabCase(variant.ident.to_string()).to_string();
            // This is not possible for now, but will be if we support renaming `value(name = "..")`.
            if value_str.contains(|c: char| c.is_ascii_control()) {
                return Err(syn::Error::new(
                    variant.ident.span(),
                    "value names containing ASCII control characters are not supported",
                ));
            }

            Ok(Variant { value_str, ident: &variant.ident })
        })
        .collect::<syn::Result<Vec<_>>>()?;

    variants.sort_by(|lhs, rhs| Ord::cmp(&lhs.value_str, &rhs.value_str));
    if let Some(w) = variants.windows(2).find(|w| w[0].value_str == w[1].value_str) {
        let mut err = syn::Error::new(w[0].ident.span(), "duplicated variant names after renaming");
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
    value_str: String,
    ident: &'i Ident,
}

impl ToTokens for ValueEnumImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = self.ident;
        let (impl_generics, ty_generics, where_clause) = self.generics.split_for_impl();
        let variant_strs = self.variants.iter().map(|v| &v.value_str);
        let variant_idents = self.variants.iter().map(|v| v.ident);
        let possible_inputs_nul =
            self.variants.iter().flat_map(|v| [&v.value_str, "\0"]).collect::<String>();

        tokens.extend(quote! {
            #[automatically_derived]
            impl #impl_generics __rt::ValueEnum for #name #ty_generics #where_clause {
                const POSSIBLE_INPUTS_NUL: &'static str = #possible_inputs_nul;

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
