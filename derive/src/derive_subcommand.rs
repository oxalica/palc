use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::{DeriveInput, Ident, Type};

use crate::common::{CommandMeta, ErrorCollector, wrap_anon_item};
use crate::derive_args::ParserStateDefImpl;
use syn::spanned::Spanned;

pub(crate) fn expand(input: &DeriveInput) -> TokenStream {
    let mut tts = match expand_impl(input) {
        Ok(out) => return wrap_anon_item(out),
        Err(err) => err.into_compile_error(),
    };

    let name = &input.ident;
    tts.extend(wrap_anon_item(quote! {
        #[automatically_derived]
        impl __rt::Subcommand for #name {}

        #[automatically_derived]
        impl __rt::Sealed for #name {}

        #[automatically_derived]
        impl __rt::CommandInternal for #name {}
    }));
    tts
}

struct SubcommandImpl<'i> {
    enum_name: &'i Ident,
    state_defs: Vec<ParserStateDefImpl<'i>>,
    // (name, kind)
    variants: Vec<(String, VariantImpl<'i>)>,
}

enum VariantImpl<'i> {
    Unit { ident: &'i Ident },
    Tuple { ident: &'i Ident, ty: &'i Type },
    Struct { state_ident: Ident },
}

impl ToTokens for VariantImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            // Consume all rest args, possibly feed global arguments.
            VariantImpl::Unit { ident: variant_name } => quote! {
                |__args, __cmd_name, __ancestors| {
                    __rt::try_parse_state::<()>(__args, __cmd_name, __ancestors)?;
                    __rt::Ok(Self::#variant_name)
                }
            },
            VariantImpl::Tuple { ident: variant_name, ty } => quote_spanned! {ty.span()=>
                |__args, __cmd_name, __ancestors| {
                    __rt::Ok(Self::#variant_name(__rt::try_parse_state::<<#ty as __rt::Args>::__State>(
                        __args,
                        __cmd_name,
                        __ancestors,
                    )?))
                }
            },
            VariantImpl::Struct { state_ident: state_name } => quote! {
                __rt::try_parse_state::<#state_name>
            },
        });
    }
}

fn expand_impl(def: &DeriveInput) -> syn::Result<SubcommandImpl<'_>> {
    let syn::Data::Enum(data) = &def.data else {
        return Err(syn::Error::new(
            Span::call_site(),
            "derive(Subcommand) can only be used on enums",
        ));
    };

    if !def.generics.params.is_empty() || def.generics.where_clause.is_some() {
        return Err(syn::Error::new(def.ident.span(), "TODO: generics are not supported yet"));
    }

    let mut errs = ErrorCollector::default();
    let enum_name = &def.ident;
    let mut state_defs = Vec::with_capacity(data.variants.len());

    let variants = data
        .variants
        .iter()
        .filter_map(|variant| {
            let variant_name = &variant.ident;
            let arg_name = heck::AsKebabCase(variant_name.to_string()).to_string();
            // TODO: Should this also include enum attributes?
            let act = match &variant.fields {
                syn::Fields::Unit => {
                    // TODO: Handle `command()` here.
                    VariantImpl::Unit { ident: variant_name }
                }
                syn::Fields::Unnamed(fields) => {
                    if fields.unnamed.len() != 1 {
                        errs.push(syn::Error::new(
                            variant_name.span(),
                            "subcommand tuple variant must have a single element",
                        ));
                        return None;
                    }
                    // TODO: Handle or reject `command()` here.
                    VariantImpl::Tuple { ident: variant_name, ty: &fields.unnamed[0].ty }
                }
                syn::Fields::Named(fields) => {
                    let cmd_meta = errs.collect(CommandMeta::parse_attrs(&variant.attrs));
                    let state_name =
                        format_ident!("{enum_name}{variant_name}State", span = variant_name.span());
                    let mut state = errs.collect(crate::derive_args::expand_state_def_impl(
                        &def.vis,
                        cmd_meta,
                        state_name.clone(),
                        enum_name.to_token_stream(),
                        fields,
                    ))?;
                    state.output_ctor = Some(quote!(#enum_name :: #variant_name));
                    state_defs.push(state);
                    VariantImpl::Struct { state_ident: state_name }
                }
            };
            Some((arg_name, act))
        })
        .collect::<Vec<_>>();

    errs.finish_then(SubcommandImpl { enum_name, state_defs, variants })
}

impl ToTokens for SubcommandImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { enum_name, state_defs, variants } = self;
        let name_strs = variants.iter().map(|(name, _)| name);
        let cases = variants.iter().map(|(_, v)| v);

        let subcmds = name_strs.clone().flat_map(|name| [name, "\0"]).collect::<String>();
        let subcmd_docs = variants
            .iter()
            .map(|(_, v)| match v {
                // TODO
                VariantImpl::Unit { .. } => quote! { "" },
                VariantImpl::Tuple { ty, .. } => quote! {
                    <<#ty as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.__applet_doc
                },
                VariantImpl::Struct { state_ident } => quote! {
                    <#state_ident as __rt::ParserState>::RAW_ARGS_INFO.__applet_doc
                },
            })
            .collect::<Vec<TokenStream>>();

        tokens.extend(quote! {
            #(#state_defs)*

            #[automatically_derived]
            impl __rt::Sealed for #enum_name {}

            #[automatically_derived]
            impl __rt::Subcommand for #enum_name {}

            #[automatically_derived]
            impl __rt::CommandInternal for #enum_name {
                const SUBCOMMANDS: &'static __rt::str = #subcmds;
                const SUBCOMMAND_DOCS: &'static [&'static __rt::str] = &[#(#subcmd_docs),*];

                fn feed_subcommand(__name: &__rt::OsStr) -> __rt::FeedSubcommand<Self> {
                    __rt::Some(match __name.to_str() {
                        __rt::Some(__name) => match __name {
                            #(#name_strs => #cases,)*
                            _ => return __rt::None,
                        },
                        __rt::None => return __rt::None,
                    })
                }
            }
        });
    }
}
