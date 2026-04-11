use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote};
use syn::{Data, DataEnum, DeriveInput, Ident, Type};

use crate::common::{CommandMeta, wrap_anon_item};
use crate::derive_args::HelpDisplay;
use crate::error::catch_errors;

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
        impl __rt::Subcommand for #ident {
            const __INFO: __rt::RawSubcommandInfo = __rt::RawSubcommandInfo::EMPTY;
            extern "C" fn __parse(
                _: &mut __rt::Option<Self>,
                _: __rt::usize,
                _: &mut dyn __rt::Frame,
                _: &mut dyn __rt::RunParser,
            ) -> __rt::Result<()> {
                __rt::unimplemented!()
            }
        }
    }
}

struct SubcommandImpl<'i> {
    enum_name: &'i Ident,
    variants: Vec<Variant<'i>>,
    name_map: Vec<usize>,
}

struct Variant<'i> {
    ident: &'i Ident,
    arg_str: String,
    args_info: TokenStream,
    kind: VariantKind<'i>,
}

enum VariantKind<'i> {
    Unit,
    Newtype { ty: &'i Type },
    Struct { body: TokenStream },
}

fn expand_for_enum<'a>(def: &'a DeriveInput, data: &'a DataEnum) -> SubcommandImpl<'a> {
    let enum_name = &def.ident;
    let mut name_map = Vec::new();

    let variants = data
        .variants
        .iter()
        .enumerate()
        .filter_map(|(idx, variant)| {
            let variant_ident = &variant.ident;
            let cmd_meta = CommandMeta::parse_attrs_opt(&variant.attrs);

            let arg_str = heck::AsKebabCase(variant_ident.to_string()).to_string();
            name_map.push(idx);

            let (args_info, kind) = match &variant.fields {
                syn::Fields::Unit => {
                    let help = HelpDisplay { parse: None, cmd_meta: cmd_meta.as_deref() }
                        .to_token_stream();
                    let info = quote! {
                        &__rt::RawArgsInfo::new("", &[], &[], __rt::None, false, false, #help, &[])
                    };
                    (info, VariantKind::Unit)
                }
                syn::Fields::Unnamed(fields) => {
                    if fields.unnamed.len() != 1 {
                        emit_error!(
                            variant_ident,
                            "subcommand tuple variant must have a single element",
                        );
                        return None;
                    }
                    if cmd_meta.is_some() {
                        emit_error!(
                            variant_ident,
                            "`#[command(..)]` or doc-comments are ignored for newtype variants. \
                            Attributes on the inner type of this variant will be used instead."
                        );
                    }
                    let ty = &fields.unnamed[0].ty;
                    let args_info = quote! { <#ty as __rt::Args>::__INFO };
                    (args_info, VariantKind::Newtype { ty })
                }
                syn::Fields::Named(fields) => {
                    let parse = crate::derive_args::expand_args_parse(
                        cmd_meta.as_deref(),
                        quote! { Self::#variant_ident },
                        fields,
                    )
                    .ok()?;
                    let args_info = parse.args_info().to_token_stream();
                    let body = parse.to_token_stream();
                    (args_info, VariantKind::Struct { body })
                }
            };
            Some(Variant { ident: variant_ident, arg_str, args_info, kind })
        })
        .collect::<Vec<_>>();

    name_map
        .sort_unstable_by(|&lhs, &rhs| Ord::cmp(&variants[lhs].arg_str, &variants[rhs].arg_str));
    // FIXME: Duplication check.

    SubcommandImpl { enum_name, variants, name_map }
}

impl ToTokens for SubcommandImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { enum_name, variants, name_map } = self;

        let map_from = name_map.iter().map(|&i| &*variants[i].arg_str);
        let map_to = name_map.iter().copied();

        let subcmds = variants.iter().flat_map(|v| [&*v.arg_str, "\0"]).collect::<String>();

        let variant_infos = variants.iter().map(|v| &v.args_info);

        let idxs = 0..variants.len();
        let bodies =
            variants.iter().enumerate().map(|(idx, Variant { ident, kind, .. })| match &kind {
                VariantKind::Unit => {
                    quote! {
                        <dyn __rt::RunParser>::run_unit(__parser, __frame, __VARIANT_INFOS[#idx])?;
                        *__out = __rt::Some(Self::#ident);
                        __rt::Ok(())
                    }
                }
                VariantKind::Newtype { ty } => quote! {
                    let mut __tmp = __rt::None;
                    <#ty as __rt::Args>::__parse(&mut __tmp, __frame, __parser)?;
                    *__out = __rt::Some(Self::#ident(__tmp.unwrap()));
                    __rt::Ok(())
                },
                VariantKind::Struct { body } => {
                    quote! {
                        let __info = __VARIANT_INFOS[#idx];
                        #body
                    }
                }
            });

        tokens.extend(quote! {
            const __VARIANT_INFOS: &[&__rt::RawArgsInfo] = &[#(#variant_infos),*];

            #[automatically_derived]
            impl __rt::Subcommand for #enum_name {
                const __INFO: __rt::RawSubcommandInfo = __rt::RawSubcommandInfo::new(
                    #subcmds,
                    &[#((#map_from, #map_to)),*],
                    __VARIANT_INFOS,
                );

                // If there is no variant.
                #[allow(unreachable_code)]
                extern "C" fn __parse(
                    __out: &mut __rt::Option<Self>,
                    __idx: __rt::usize,
                    __frame: &mut dyn __rt::Frame,
                    __parser: &mut dyn __rt::RunParser,
                ) -> __rt::Result<()> {
                    match __idx {
                        #(#idxs => { #bodies })*
                        _ => __rt::unreachable_nounwind(),
                    }
                }
            }
        });
    }
}
