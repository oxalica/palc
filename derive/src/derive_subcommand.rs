use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::{Data, DataEnum, DeriveInput, Ident, Type};

use crate::common::{CommandMeta, wrap_anon_item};
use crate::derive_args::CommandDoc;
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
    state_defs: TokenStream,
    variants: Vec<VariantImpl<'i>>,
}

struct VariantImpl<'i> {
    arg_name: String,
    kind: VariantKind<'i>,
}

enum VariantKind<'i> {
    Unit { state_name: Ident },
    Tuple { variant_name: &'i Ident, ty: &'i Type },
    Struct { state_name: Ident },
}

impl ToTokens for VariantKind<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match &self {
            Self::Tuple { variant_name, ty } => quote_spanned! {ty.span()=>
                |__args, __cmd_name, __ancestors| {
                    __rt::Ok(Self::#variant_name(__rt::try_parse_state::<<#ty as __rt::Args>::__State>(
                        __args,
                        __cmd_name,
                        __ancestors,
                    )?))
                }
            },
            Self::Unit { state_name, .. } | Self::Struct { state_name } => quote! {
                __rt::try_parse_state::<#state_name>
            },
        });
    }
}

fn expand_for_enum<'a>(def: &'a DeriveInput, data: &'a DataEnum) -> SubcommandImpl<'a> {
    let enum_name = &def.ident;
    let mut state_defs = TokenStream::new();

    let variants = data
        .variants
        .iter()
        .filter_map(|variant| {
            let variant_name = &variant.ident;
            let arg_name = heck::AsKebabCase(variant_name.to_string()).to_string();
            let cmd_meta = CommandMeta::parse_attrs_opt(&variant.attrs);

            let kind = match &variant.fields {
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
                    VariantKind::Tuple { variant_name, ty: &fields.unnamed[0].ty }
                }
                // FIXME: Unfortunately we need to generate state for each unit variant,
                // because each has a distinct `RAW_ARGS_INFO`, and it will be
                // used for about-text display.
                // Maybe we can pass it via an argument rather than a dyn method?
                syn::Fields::Unit => {
                    let state_name =
                        format_ident!("{enum_name}{variant_name}State", span = variant_name.span());

                    UnitVariantStateImpl {
                        state_name: &state_name,
                        enum_name,
                        variant_name,
                        cmd_meta: cmd_meta.as_deref(),
                    }
                    .to_tokens(&mut state_defs);

                    VariantKind::Unit { state_name }
                }
                syn::Fields::Named(fields) => {
                    let state_name =
                        format_ident!("{enum_name}{variant_name}State", span = variant_name.span());

                    let mut state = crate::derive_args::expand_state_def_impl(
                        &def.vis,
                        cmd_meta.as_deref(),
                        state_name.clone(),
                        enum_name.to_token_stream(),
                        fields,
                    )
                    .ok()?;
                    state.output_ctor = Some(quote! { #enum_name :: #variant_name });
                    state.to_tokens(&mut state_defs);
                    VariantKind::Struct { state_name }
                }
            };

            Some(VariantImpl { arg_name, kind })
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
                VariantKind::Tuple { ty, .. } => quote! {
                    <<#ty as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.raw_cmd_docs()
                },
                VariantKind::Unit { state_name,.. } |
                VariantKind::Struct { state_name } => quote! {
                    <#state_name as __rt::ParserState>::RAW_ARGS_INFO.raw_cmd_docs()
                }
            })
            .collect::<Vec<TokenStream>>();

        tokens.extend(quote! {
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

            #state_defs
        });
    }
}

struct UnitVariantStateImpl<'a> {
    state_name: &'a Ident,
    enum_name: &'a Ident,
    variant_name: &'a Ident,
    cmd_meta: Option<&'a CommandMeta>,
}

impl ToTokens for UnitVariantStateImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { state_name, enum_name, variant_name, cmd_meta } = self;
        let cmd_doc = CommandDoc(*cmd_meta);

        tokens.extend(quote! {
            struct #state_name;

            impl __rt::ParserState for #state_name {
                type Output = #enum_name;

                const RAW_ARGS_INFO: &'static __rt::RawArgsInfo = &__rt::RawArgsInfo::new(
                    false,
                    false,
                    __rt::None,
                    #cmd_doc,
                    "",
                    |_, _| {},
                    [],
                );

                fn init() -> Self {
                    Self
                }
                fn finish(&mut self) -> __rt::Result<Self::Output> {
                    __rt::Ok(#enum_name::#variant_name)
                }
            }

            impl __rt::ParserStateDyn for #state_name {
                fn info(&self) -> &'static __rt::RawArgsInfo {
                    <Self as __rt::ParserState>::RAW_ARGS_INFO
                }
            }
        });
    }
}
