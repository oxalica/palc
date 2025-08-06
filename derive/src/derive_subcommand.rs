use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::{Data, DataEnum, DeriveInput, Ident, Type};

use crate::common::{CommandMeta, wrap_anon_item};
use crate::derive_args::ParserStateDefImpl;
use crate::error::catch_errors;
use syn::spanned::Spanned;

pub fn expand(input: &DeriveInput) -> TokenStream {
    assert_no_generics!(input);

    match catch_errors(|| match &input.data {
        Data::Enum(data) => Ok(wrap_anon_item(expand_for_enum(input, data))),
        Data::Struct(_) => abort!(
            Span::call_site(),
            "structs are only supported by `derive(Args)`, not by `derive(Subcommand)`",
        ),
        Data::Union(_) => abort!(Span::call_site(), "only enums are supported"),
    }) {
        Ok(out) => out,
        Err(mut tts) => {
            tts.extend(wrap_anon_item(fallback(&input.ident)));
            tts
        }
    }
}

fn fallback(ident: &Ident) -> TokenStream {
    quote! {
        #[automatically_derived]
        impl __rt::Subcommand for #ident {}
    }
}

struct SubcommandImpl<'i> {
    enum_name: &'i Ident,
    state_defs: Vec<ParserStateDefImpl<'i>>,
    variants: Vec<VariantImpl<'i>>,
}

struct VariantImpl<'i> {
    arg_name: String,
    cmd_meta: Option<Box<CommandMeta>>,
    kind: VariantKind<'i>,
}

enum VariantKind<'i> {
    Unit { ident: &'i Ident },
    Tuple { ident: &'i Ident, ty: &'i Type },
    Struct { state_ident: Ident },
}

impl ToTokens for VariantKind<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match &self {
            // Consume all rest args, possibly feed global arguments.
            Self::Unit { ident: variant_name } => quote! {
                |__args, __cmd_name, __ancestors| {
                    __rt::try_parse_state::<()>(__args, __cmd_name, __ancestors)?;
                    __rt::Ok(Self::#variant_name)
                }
            },
            Self::Tuple { ident: variant_name, ty } => quote_spanned! {ty.span()=>
                |__args, __cmd_name, __ancestors| {
                    __rt::Ok(Self::#variant_name(__rt::try_parse_state::<<#ty as __rt::Args>::__State>(
                        __args,
                        __cmd_name,
                        __ancestors,
                    )?))
                }
            },
            Self::Struct { state_ident: state_name, .. } => quote! {
                __rt::try_parse_state::<#state_name>
            },
        });
    }
}

fn expand_for_enum<'a>(def: &'a DeriveInput, data: &'a DataEnum) -> SubcommandImpl<'a> {
    let enum_name = &def.ident;
    let mut state_defs = Vec::with_capacity(data.variants.len());

    let variants = data
        .variants
        .iter()
        .filter_map(|variant| {
            let variant_name = &variant.ident;
            let arg_name = heck::AsKebabCase(variant_name.to_string()).to_string();
            let cmd_meta = CommandMeta::parse_attrs_opt(&variant.attrs);
            let kind = match &variant.fields {
                syn::Fields::Unit => VariantKind::Unit { ident: variant_name },
                syn::Fields::Unnamed(fields) => {
                    if fields.unnamed.len() != 1 {
                        emit_error!(
                            variant_name,
                            "subcommand tuple variant must have a single element",
                        );
                        return None;
                    }
                    if cmd_meta.is_some() {
                        emit_error!(
                            variant_name,
                            "`#[command(..)]` or doc-comments are ignored for newtype variants. \
                            Attributes on the inner type of this variant will be used instead."
                        );
                    }
                    VariantKind::Tuple { ident: variant_name, ty: &fields.unnamed[0].ty }
                }
                syn::Fields::Named(fields) => {
                    let state_name =
                        format_ident!("{enum_name}{variant_name}State", span = variant_name.span());

                    let mut state = crate::derive_args::expand_state_def_impl(
                        &def.vis,
                        None,
                        state_name.clone(),
                        enum_name.to_token_stream(),
                        fields,
                    )
                    .ok()?;
                    state.output_ctor = Some(quote! { #enum_name :: #variant_name });
                    state_defs.push(state);
                    VariantKind::Struct { state_ident: state_name }
                }
            };

            Some(VariantImpl { arg_name, cmd_meta, kind })
        })
        .collect::<Vec<_>>();

    SubcommandImpl { enum_name, state_defs, variants }
}

impl ToTokens for SubcommandImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { enum_name, state_defs, variants } = self;
        let arg_strs = variants.iter().map(|v| &v.arg_name);
        let cases = variants.iter().map(|v| &v.kind);

        let subcmds = arg_strs.clone().flat_map(|name| [name, "\0"]).collect::<String>();
        let cmd_docs = variants
            .iter()
            .map(|v| match &v.kind {
                // Only forward to inner `#[command(..)]` on newtype variants.
                VariantKind::Tuple { ty, .. } => quote! {
                    <<#ty as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.raw_cmd_docs()
                },
                // Otherwise, use local doc-comment and attributes.
                _ => crate::derive_args::CommandDoc(v.cmd_meta.as_deref()).to_token_stream(),
            })
            .collect::<Vec<TokenStream>>();

        tokens.extend(quote! {
            #(#state_defs)*

            #[automatically_derived]
            impl __rt::Subcommand for #enum_name {
                const RAW_INFO: &'static __rt::RawSubcommandInfo = &__rt::RawSubcommandInfo::new(
                    #subcmds,
                    [#(#cmd_docs),*],
                );

                // If there is no variant.
                #[allow(unreachable_code)]
                fn feed_subcommand(__name: &__rt::OsStr) -> __rt::FeedSubcommand<Self> {
                    __rt::Some(match __name.to_str() {
                        __rt::Some(__name) => match __name {
                            #(#arg_strs => #cases,)*
                            _ => return __rt::None,
                        },
                        __rt::None => return __rt::None,
                    })
                }
            }
        });
    }
}
