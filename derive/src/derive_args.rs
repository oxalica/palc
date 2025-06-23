use std::collections::HashMap;
use std::num::NonZero;

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{DeriveInput, Error, Ident, LitChar, LitStr, Visibility};

use crate::common::{
    ArgOrCommand, ArgTyKind, ArgsCommandMeta, CommandMeta, Doc, ErrorCollector, FieldPath,
    Override, TY_OPTION, parse_args_attrs, strip_ty_ctor, wrap_anon_item,
};
use crate::derive_subcommand::RawCommandInfo;
use crate::shared::{AcceptHyphen, ArgAttrs};

pub fn expand(input: &DeriveInput, is_parser: bool) -> TokenStream {
    let mut tts = match expand_args_impl(input, is_parser) {
        Ok(out) => return wrap_anon_item(out),
        Err(err) => err.into_compile_error(),
    };

    // Error fallback impl.
    let name = &input.ident;
    tts.extend(wrap_anon_item(quote! {
        #[automatically_derived]
        impl __rt::Parser for #name {}

        #[automatically_derived]
        impl __rt::Args for #name {
            type __State = __rt::FallbackState<#name>;
        }

        #[automatically_derived]
        impl __rt::CommandInternal for #name {}

        #[automatically_derived]
        impl __rt::Sealed for #name {}
    }));
    tts
}

/// For `derive({Args,Parser})`.
pub struct ArgsImpl<'i> {
    is_parser: bool,
    state: ParserStateDefImpl<'i>,
}

fn expand_args_impl(def: &DeriveInput, is_parser: bool) -> syn::Result<ArgsImpl<'_>> {
    let syn::Data::Struct(syn::DataStruct { fields: syn::Fields::Named(fields), .. }) = &def.data
    else {
        return Err(syn::Error::new(
            Span::call_site(),
            "derive(Args) and derive(Parser) can only be used on named structs",
        ));
    };

    if !def.generics.params.is_empty() || def.generics.where_clause.is_some() {
        return Err(Error::new(def.ident.span(), "TODO: generics are not supported yet"));
    }

    let mut errs = ErrorCollector::default();
    let cmd_meta = errs.collect(CommandMeta::parse_attrs(&def.attrs)).unwrap_or_default();

    if u8::try_from(fields.named.len()).is_err() {
        return Err(Error::new(def.ident.span(), "derive(Args) only supports up to 255 fields"));
    }

    let state_name = format_ident!("{}State", def.ident);
    let struct_name = def.ident.to_token_stream();
    let state = errs.collect(expand_state_def_impl(
        &def.vis,
        Some(cmd_meta),
        state_name,
        struct_name,
        fields,
    ));

    errs.finish()?;
    Ok(ArgsImpl { is_parser, state: state.unwrap() })
}

impl ToTokens for ArgsImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let struct_name = &self.state.output_ty;
        let state_name = &self.state.state_name;
        let state = &self.state;

        if self.is_parser {
            let raw_cmd_info = RawCommandInfo::Args { state_name: &self.state.state_name };

            tokens.extend(quote! {
                #[automatically_derived]
                impl __rt::Parser for #struct_name {}

                #[automatically_derived]
                impl __rt::CommandInternal for #struct_name {
                    const RAW_COMMAND_INFO: &'static __rt::RawCommandInfo = #raw_cmd_info;

                    fn feed_subcommand(_: &__rt::OsStr) -> __rt::FeedSubcommand<Self> {
                        __rt::Some(__rt::try_parse_state::<<Self as __rt::Args>::__State>)
                    }
                }
            });
        }

        tokens.extend(quote! {
            #[automatically_derived]
            impl __rt::Args for #struct_name {
                type __State = #state_name;
            }

            #[automatically_derived]
            impl __rt::Sealed for #struct_name {}

            #state
        });
    }
}

pub struct ParserStateDefImpl<'i> {
    pub vis: &'i Visibility,
    pub state_name: Ident,
    pub output_ty: TokenStream,
    pub output_ctor: Option<TokenStream>,

    /// All direct fields parsed by this impl.
    fields: Vec<FieldInfo<'i>>,
    /// Indirect fields that needs delegation.
    flatten_fields: Vec<FlattenFieldInfo<'i>>,
    /// Subcommand is special.
    subcommand: Option<SubcommandInfo<'i>>,

    // Classified direct fields.
    named_fields: Vec<usize>,
    unnamed_fields: Vec<usize>,
    catchall_field: Option<CatchallFieldInfo>,
    last_field: Option<usize>,

    cmd_meta: Option<Box<CommandMeta>>,
}

struct FieldInfo<'i> {
    // Basics //
    ident: &'i Ident,
    kind: FieldKind,
    /// The type used for parser inference, with `Option`/`Vec` stripped.
    effective_ty: &'i syn::Type,
    finish: FieldFinish,

    // Arg configurables //
    /// Encoded names for matching. Empty for unnamed arguments.
    enc_names: Vec<String>,
    attrs: ArgAttrs,

    // Validations //
    default_expr: Option<TokenStream>,
    exclusive: bool,
    dependencies: Vec<FieldPath>,
    conflicts: Vec<FieldPath>,

    // Docs //
    /// Example-like description, eg. `--key <VALUE>`, `-c, --color=<COLOR>`.
    description: String,
    doc: Doc,
    hide: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FieldKind {
    BoolSetTrue,
    Counter,
    Option,
    OptionOption,
    OptionVec,
}

#[derive(Clone, Copy)]
struct SubcommandInfo<'i> {
    ident: &'i Ident,
    effective_ty: &'i syn::Type,
    optional: bool,
}

#[derive(Clone, Copy)]
struct FlattenFieldInfo<'i> {
    ident: &'i Ident,
    effective_ty: &'i syn::Type,
}

#[derive(Clone, Copy)]
struct CatchallFieldInfo {
    field_idx: usize,
    greedy: bool,
}

fn value_info(ty: &syn::Type) -> TokenStream {
    quote_spanned! {ty.span()=> __rt::arg_value_info!(#ty) }
}

fn value_parsed(ty: &syn::Type) -> TokenStream {
    let value_info = value_info(ty);
    quote_spanned! {ty.span()=> __rt::parse_take_arg(__arg, #value_info)? }
}

#[derive(PartialEq)]
enum FieldFinish {
    Id,
    UnwrapDefault,
    UnwrapChecked,
}

impl ToTokens for FieldFinish {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Id => {}
            Self::UnwrapDefault => tokens.extend(quote! { .unwrap_or_default() }),
            Self::UnwrapChecked => tokens.extend(quote! { .unwrap() }),
        }
    }
}

fn encode_long_name(name: &LitStr, errs: &mut ErrorCollector) -> String {
    let s = name.value();
    if s.is_empty() {
        errs.push(syn::Error::new(name.span(), "arg(long) name must NOT be empty"));
    } else if s.starts_with('-') {
        errs.push(syn::Error::new(
                name.span(),
                "arg(long) name must NOT specify leading '-', they are always assumed to be '--'-prefixed",
            ));
    } else if s.contains(|c: char| c == '=' && c.is_ascii_control()) {
        errs.push(syn::Error::new(
            name.span(),
            "arg(long) name must NOT contain '=' or ASCII control characters",
        ));
    }
    if s.len() > 1 {
        s
    } else {
        // Disambiguate from short names.
        format!("--{s}")
    }
}

fn encode_short_name(name: &LitChar, errs: &mut ErrorCollector) -> String {
    let c = name.value();
    if c == '-' || c.is_ascii_control() {
        errs.push(syn::Error::new(
            name.span(),
            "arg(short) name must NOT be '-' or ASCII control characters",
        ));
    } else if !c.is_ascii() {
        // NB. It is assumed to be ASCII in `refl::NamedArgInfo::short_args()` and
        // `ArgsIter::next_arg()`.
        errs.push(syn::Error::new(
            name.span(),
            r#"Non-ASCII arg(short) name is reserved. Use `arg(long)` instead. \
                A unicode codepoint is not necessarity a "character" in human sense, thus \
                automatic splitting or argument de-bundling may give unexpected results. \
                If you do want this to be supported, convince us by opening an issue."#,
        ));
    }
    c.into()
}

pub fn expand_state_def_impl<'i>(
    vis: &'i Visibility,
    cmd_meta: Option<Box<CommandMeta>>,
    state_name: Ident,
    output_ty: TokenStream,
    input_fields: &'i syn::FieldsNamed,
) -> syn::Result<ParserStateDefImpl<'i>> {
    let field_attrs = parse_args_attrs(input_fields)?;
    let input_fields = &input_fields.named;

    let mut errs = ErrorCollector::default();

    let mut out = ParserStateDefImpl {
        vis,
        state_name,
        output_ty,
        output_ctor: None,
        fields: Vec::with_capacity(input_fields.len()),
        flatten_fields: Vec::new(),
        subcommand: None,

        named_fields: Vec::new(),
        unnamed_fields: Vec::new(),
        catchall_field: None,
        last_field: None,
        cmd_meta,
    };

    let mut variable_len_arg_span = None;
    let mut check_variable_len_arg = |errs: &mut ErrorCollector, span: Span| {
        if let Some(prev) = variable_len_arg_span.replace(span) {
            errs.push(Error::new(span, "duplicated variable-length arguments"));
            errs.push(Error::new(prev, "previously defined here"));
        }
    };

    let mut seen_enc_names = HashMap::new();
    let mut check_dup_name = |enc_name: String, span: Span, errs: &mut ErrorCollector| {
        if let Some(prev_span) = seen_enc_names.insert(enc_name, span) {
            errs.push(syn::Error::new(span, "duplicated argument names"));
            errs.push(syn::Error::new(prev_span, "previously defined here"));
        }
    };

    for (field, attrs) in input_fields.iter().zip(field_attrs) {
        let ident = field.ident.as_ref().expect("named struct");
        let ident_str = ident.to_string();

        let mut arg = match attrs {
            ArgOrCommand::Arg(arg) => arg,
            ArgOrCommand::Command(ArgsCommandMeta::Subcommand) => {
                check_variable_len_arg(&mut errs, ident.span());
                let (optional, effective_ty) = match strip_ty_ctor(&field.ty, TY_OPTION) {
                    Some(subty) => (true, subty),
                    None => (false, &field.ty),
                };
                out.subcommand = Some(SubcommandInfo { ident, effective_ty, optional });
                continue;
            }
            ArgOrCommand::Command(ArgsCommandMeta::Flatten) => {
                out.flatten_fields.push(FlattenFieldInfo { ident, effective_ty: &field.ty });
                continue;
            }
        };

        let ty_kind = ArgTyKind::of(&field.ty);
        #[rustfmt::skip]
        let (kind, effective_ty, finish) = match ty_kind {
            ArgTyKind::Bool => (FieldKind::BoolSetTrue, &field.ty, FieldFinish::UnwrapDefault),
            ArgTyKind::U8 => (FieldKind::Counter, &field.ty, FieldFinish::UnwrapDefault),
            ArgTyKind::Vec(subty) => (FieldKind::OptionVec, subty, FieldFinish::UnwrapDefault),
            ArgTyKind::OptionVec(subty) => (FieldKind::OptionVec, subty, FieldFinish::Id),
            ArgTyKind::Option(subty) => (FieldKind::Option, subty, FieldFinish::Id),
            ArgTyKind::OptionOption(subty) => (FieldKind::OptionOption, subty, FieldFinish::Id),
            ArgTyKind::Other => (FieldKind::Option, &field.ty, FieldFinish::UnwrapChecked),
        };

        let num_values = match kind {
            FieldKind::BoolSetTrue | FieldKind::Counter => 0,
            // NB. OptionOption expects exactly one value, not zero. Just the value can be empty.
            // clap also agrees with this behavior.
            FieldKind::Option | FieldKind::OptionOption | FieldKind::OptionVec => 1,
        };

        // Default values.
        let default_expr = match (arg.default_value_t.take(), arg.default_value.take()) {
            (Some(Override::Inherit), None) => Some(quote_spanned! {field.ty.span()=>
                __rt::Default::default()
            }),
            (Some(Override::Explicit(e)), None) => Some(e.into_token_stream()),
            (None, Some(default_str)) => {
                let value_info = value_info(effective_ty);
                Some(quote_spanned! {field.ty.span()=>
                    __rt::parse_default_str(#default_str, #value_info)?
                })
            }
            (None, None) => None,
            (Some(_), Some(tt)) => {
                errs.push(syn::Error::new(
                    tt.span(),
                    "arg(default_value) conflicts with arg(default_value_t)",
                ));
                None
            }
        };
        let (required, default_expr) = match default_expr {
            Some(e) => {
                if arg.required {
                    errs.push(Error::new(
                        ident.span(),
                        "arg(required) conflicts with arg(default_value{,_t})",
                    ));
                }
                (false, Some(e))
            }
            None => (arg.required || finish == FieldFinish::UnwrapChecked, None),
        };

        let value_delimiter = if let Some(ch) = &arg.value_delimiter {
            let ch = ch.value();
            if !matches!(kind, FieldKind::OptionVec) {
                errs.push(syn::Error::new(
                    ch.span(),
                    "arg(value_delimiter) must be used on Vec-like types",
                ));
                None
            } else if !ch.is_ascii() || ch.is_ascii_control() {
                errs.push(syn::Error::new(
                    ch.span(),
                    r#"arg(value_delimiter) must be non-control ASCII characters. \
                    A unicode codepoint is not necessarity a "character" in human sense, thus \
                    automatic splitting may give unexpected results. \
                    If you do want this to be supported, convince us by opening an issue."#,
                ));
                None
            } else if !arg.is_named() {
                errs.push(syn::Error::new(
                    ch.span(),
                    "TODO: arg(value_delimiter) is not yet supported on unnamed arguments",
                ));
                None
            } else {
                Some(NonZero::new(ch as u8).expect("not NUL"))
            }
        } else {
            None
        };

        let accept_hyphen = match (arg.allow_hyphen_values, arg.allow_negative_numbers) {
            (true, false) => AcceptHyphen::Yes,
            (false, true) => AcceptHyphen::NegativeNumber,
            (false, false) => AcceptHyphen::No,
            (true, true) => {
                // TODO: More accurate spans.
                errs.push(Error::new(
                    ident.span(),
                    "arg(allow_hyphen_values) and arg(allow_negative_numbers) \
                        conflict with each other",
                ));
                AcceptHyphen::No
            }
        };
        if accept_hyphen != AcceptHyphen::No
            && matches!(kind, FieldKind::BoolSetTrue | FieldKind::Counter)
        {
            errs.push(Error::new(
                ident.span(),
                "Only arguments that take values can allow hyphen values",
            ));
        }

        let value_name = match &arg.value_name {
            Some(s) => s.value(),
            None => heck::AsShoutySnekCase(&ident_str).to_string(),
        };
        if value_name.contains(|ch: char| ch.is_ascii_control()) {
            errs.push(syn::Error::new(
                value_name.span(),
                "arg(value_name) must NOT contain ASCII control characters",
            ));
        }

        if arg.is_named() {
            // Named arguments.

            if arg.last || arg.trailing_var_arg {
                errs.push(Error::new(
                    field.ty.span(),
                    "arg(last, trailing_var_arg) only support positional arguments",
                ));
                continue;
            }

            let mut enc_names = Vec::new();

            let primary_long_name = match arg.long {
                Some(Override::Inherit) => {
                    Some(LitStr::new(&heck::AsKebabCase(&ident_str).to_string(), ident.span()))
                }
                Some(Override::Explicit(name)) => Some(name.clone()),
                None => None,
            };
            for name in arg.alias.iter().chain(&primary_long_name) {
                let enc = encode_long_name(name, &mut errs);
                enc_names.push(enc.clone());
                check_dup_name(enc, name.span(), &mut errs);
            }

            let primary_short_name = match arg.short {
                Some(Override::Explicit(c)) => Some(c),
                Some(Override::Inherit) => Some(LitChar::new(
                    ident_str.chars().next().expect("must have ident"),
                    ident.span(),
                )),
                None => None,
            };
            for name in arg.short_alias.iter().chain(&primary_short_name) {
                let enc = encode_short_name(name, &mut errs);
                enc_names.push(enc.clone());
                check_dup_name(enc, name.span(), &mut errs);
            }

            assert!(!enc_names.is_empty());

            let description = {
                use std::fmt::Write;

                let mut buf = match (
                    primary_short_name.map(|s| s.value()),
                    primary_long_name.map(|s| s.value()),
                ) {
                    (Some(short), Some(long)) => format!("-{short}, --{long}"),
                    (_, Some(long)) => format!("--{long}"),
                    (Some(short), _) => format!("-{short}"),
                    (None, None) => unreachable!(),
                };
                if num_values > 0 {
                    write!(buf, "{}<{}>", if arg.require_equals { '=' } else { ' ' }, value_name)
                        .unwrap();
                }
                buf
            };

            let field_idx = out.fields.len();
            let attrs = ArgAttrs {
                num_values,
                require_eq: arg.require_equals,
                accept_hyphen,
                delimiter: value_delimiter,
                global: arg.global,
                required,
                index: field_idx as _,
            };

            out.named_fields.push(field_idx);
            out.fields.push(FieldInfo {
                ident,
                kind,
                effective_ty,
                finish,
                enc_names,
                attrs,
                default_expr,
                exclusive: arg.exclusive,
                dependencies: arg.requires,
                conflicts: arg.conflicts_with,
                description,
                doc: arg.doc,
                hide: arg.hide,
            });
        } else {
            // Unnamed arguments.

            if arg.require_equals || arg.global {
                errs.push(syn::Error::new(
                    ident.span(),
                    "arg(require_equals, global) only support named arguments",
                ));
                continue;
            }
            if arg.default_value_t.is_some() {
                errs.push(syn::Error::new(
                    ident.span(),
                    "TODO: arg(default_value_t) supports named arguments yet",
                ));
                continue;
            }

            let is_vec_like = matches!(kind, FieldKind::OptionVec);

            let mut description =
                if required { format!("<{value_name}>") } else { format!("[{value_name}]") };
            if is_vec_like {
                description.push_str("...");
            }

            let field_idx = out.fields.len();
            let attrs = ArgAttrs {
                num_values: 1,
                require_eq: false,
                accept_hyphen,
                delimiter: value_delimiter,
                global: false,
                required,
                index: field_idx as _,
            };
            out.fields.push(FieldInfo {
                ident,
                kind,
                effective_ty,
                finish,
                enc_names: Vec::new(),
                attrs,
                default_expr,
                exclusive: arg.exclusive,
                dependencies: arg.requires,
                conflicts: arg.conflicts_with,
                description,
                doc: arg.doc,
                hide: arg.hide,
            });

            let allow_accept_hyphen = if arg.last {
                // Last argument(s).
                if let Some(prev) = out.last_field.replace(field_idx) {
                    errs.push(Error::new(ident.span(), "duplicated arg(last)"));
                    errs.push(Error::new(out.fields[prev].ident.span(), "previously defined here"));
                }

                // `allow_hyphen_values` is allowed for last arguments, though it is already assumed.
                true
            } else if is_vec_like {
                // Variable length unnamed argument.

                let greedy = arg.trailing_var_arg;
                check_variable_len_arg(&mut errs, ident.span());
                out.catchall_field = Some(CatchallFieldInfo { field_idx, greedy });
                greedy
            } else {
                // Single unnamed argument.
                out.unnamed_fields.push(field_idx);
                false
            };

            if accept_hyphen != AcceptHyphen::No && !allow_accept_hyphen {
                errs.push(Error::new(
                    ident.span(),
                    "arg(allow_hyphen_values) can only be used on \
                    named arguments or arg(trailing_var_arg) yet",
                ));
            }
        }
    }

    if out.fields.iter().any(|f| f.exclusive) && !out.flatten_fields.is_empty() {
        errs.push(syn::Error::new(
            Span::call_site(),
            "TODO: arg(exclusive) is not supported on struct containing arg(flatten) yet",
        ));
    }

    errs.finish_then(out)
}

impl ToTokens for ParserStateDefImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { vis, state_name, output_ty, fields, .. } = self;

        // State initialization and finalization.
        //
        // T / Option<T>            => Option<T>
        // Vec<T> / Option<Vec<T>>  => Option<Vec<T>>
        // FlattenTy                => <FlattenTy as Args>::__State
        // SubcommandTy             => Option<SubcommandTy>
        let mut field_names = Vec::new();
        let mut field_tys = Vec::new();
        let mut field_inits = Vec::new();
        let mut field_finishes = Vec::new();
        for &idx in self.named_fields.iter().chain(&self.unnamed_fields).chain(&self.last_field) {
            let FieldInfo { ident, effective_ty, finish, kind, .. } = &fields[idx];
            field_names.push(*ident);
            match kind {
                FieldKind::Counter | FieldKind::Option | FieldKind::BoolSetTrue => {
                    field_tys.push(quote! { __rt::Option<#effective_ty> });
                    field_inits.push(quote! { __rt::None });
                }
                FieldKind::OptionOption => {
                    field_tys.push(quote! { __rt::Option<__rt::Option<#effective_ty>> });
                    field_inits.push(quote! { __rt::None });
                }
                FieldKind::OptionVec => {
                    field_tys.push(quote! { __rt::Option<__rt::Vec<#effective_ty>> });
                    field_inits.push(quote! { __rt::None });
                }
            }
            field_finishes.push(quote! { self.#ident.take() #finish });
        }
        if let Some(SubcommandInfo { ident, effective_ty, optional }) = self.subcommand {
            field_names.push(ident);
            field_tys.push(quote! { __rt::Option<#effective_ty> });
            field_inits.push(quote! { __rt::None });
            let tail = if optional {
                quote!()
            } else {
                quote! { .unwrap() }
            };
            field_finishes.push(quote! { self.#ident.take() #tail });
        }
        if let Some(CatchallFieldInfo { field_idx, .. }) = self.catchall_field {
            let FieldInfo { ident, effective_ty, finish, .. } = &fields[field_idx];
            field_names.push(*ident);
            field_tys.push(quote! { __rt::Option<__rt::Vec<#effective_ty>> });
            field_inits.push(quote! { __rt::None });
            field_finishes.push(quote! { self.#ident.take() #finish });
        }
        for &FlattenFieldInfo { ident, effective_ty } in &self.flatten_fields {
            field_names.push(ident);
            field_tys.push(quote! { <#effective_ty as __rt::Args>::__State });
            field_inits.push(quote! { __rt::ParserState::init() });
            field_finishes.push(quote! { __rt::ParserState::finish(&mut self.#ident)? });
        }

        let feed_named_func = FeedNamedImpl(self);
        let feed_unnamed_func = FeedUnnamedImpl(self);
        let validation = ValidationImpl(self);

        let output_ctor = self.output_ctor.as_ref().unwrap_or(&self.output_ty);

        let unnamed_arg_accept_hyphen = self
            .catchall_field
            .as_ref()
            .map_or(AcceptHyphen::No, |f| fields[f.field_idx].attrs.accept_hyphen);

        let raw_args_info = RawArgsInfo(self);

        tokens.extend(quote! {
            #vis struct #state_name {
                #(#field_names : #field_tys,)*
            }

            #[automatically_derived]
            impl __rt::ParserState for #state_name {
                type Output = #output_ty;

                const RAW_ARGS_INFO: __rt::RawArgsInfo = #raw_args_info;

                #[allow(clippy::unnecessary_lazy_evaluations)]
                fn init() -> Self {
                    Self {
                        #(#field_names : #field_inits,)*
                    }
                }

                fn finish(&mut self) -> __rt::Result<Self::Output> {
                    #validation
                    __rt::Ok(#output_ctor {
                        #(#field_names : #field_finishes,)*
                    })
                }
            }

            #[automatically_derived]
            impl __rt::ParserStateDyn for #state_name {
                #feed_named_func
                #feed_unnamed_func

                fn metadata(&self) -> (__rt::AcceptHyphen, __rt::RawArgsInfo) {
                    (#unnamed_arg_accept_hyphen, <Self as __rt::ParserState>::RAW_ARGS_INFO)
                }
            }
        });
    }
}

/// `fn feed_named` generator.
struct FeedNamedImpl<'i>(&'i ParserStateDefImpl<'i>);

impl ToTokens for FeedNamedImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let def = self.0;

        if def.named_fields.is_empty() && def.flatten_fields.is_empty() {
            return;
        }

        let arms = def
            .named_fields
            .iter()
            .map(|&idx| {
                let FieldInfo { ident, kind, effective_ty, enc_names, attrs, .. } =
                    &def.fields[idx];
                let value_info = value_info(effective_ty);
                let action = match kind {
                    FieldKind::BoolSetTrue => quote_spanned! {effective_ty.span()=>
                        __rt::place_for_flag(&mut self.#ident)
                    },
                    FieldKind::Counter => quote_spanned! {effective_ty.span()=>
                        __rt::place_for_counter(&mut self.#ident)
                    },
                    FieldKind::Option => quote_spanned! {effective_ty.span()=>
                        __rt::place_for_set_value(&mut self.#ident, #value_info)
                    },
                    FieldKind::OptionOption => quote_spanned! {effective_ty.span()=>
                        __rt::place_for_set_opt_value(&mut self.#ident, #value_info)
                    },
                    FieldKind::OptionVec => quote_spanned! {effective_ty.span()=>
                        __rt::place_for_vec(&mut self.#ident, #value_info)
                    },
                };
                quote! { #(#enc_names)|* => (#action, #attrs), }
            })
            .collect::<TokenStream>();

        let handle_else = {
            let self_field_cnt = def.fields.len() as u8;
            let flatten_names = def.flatten_fields.iter().map(|f| f.ident);
            let offsets = (0..def.flatten_fields.len())
                .map(|i| {
                    let prefix_tys = def.flatten_fields[..i].iter().map(|f| f.effective_ty);
                    quote! {
                        #self_field_cnt
                        #( + <<#prefix_tys as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.__total_arg_cnt)*
                    }
                });
            quote! {
                #(__rt::ParserStateDyn::feed_named(&mut self.#flatten_names, __name).map_break(|mut __ret| {
                    __ret.1.index += #offsets;
                    __ret
                })?;)*
            }
        };

        let body = if arms.is_empty() {
            quote! { #handle_else __rt::FeedNamed::Continue(()) }
        } else {
            quote! {
                __rt::FeedNamed::Break(match __name {
                    #arms
                    _ => { #handle_else return __rt::FeedNamed::Continue(()) }
                })
            }
        };

        tokens.extend(quote! {
            fn feed_named(&mut self, __name: &__rt::str) -> __rt::FeedNamed<'_> {
                #body
            }
        });
    }
}

// `fn feed_unnamed` generator.
struct FeedUnnamedImpl<'i>(&'i ParserStateDefImpl<'i>);

impl ToTokens for FeedUnnamedImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let def = self.0;

        if def.unnamed_fields.is_empty()
            && def.catchall_field.is_none()
            && def.subcommand.is_none()
            && def.last_field.is_none()
        {
            return;
        }

        let asserts = def
            .flatten_fields
            .iter()
            .map(|FlattenFieldInfo { effective_ty, .. }| quote_spanned! {effective_ty.span()=>
                const {
                    __rt::assert!(
                        <<#effective_ty as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.__total_unnamed_arg_cnt == 0,
                        "TODO: cannot arg(flatten) positional arguments yet",
                    );
                }
            })
            .collect::<TokenStream>();

        let handle_subcmd = if let Some(SubcommandInfo { ident, effective_ty, .. }) =
            &def.subcommand
        {
            let state_name = &def.state_name;
            quote! {
                struct __Subcommand;
                impl __rt::GetSubcommand for __Subcommand {
                    type State = #state_name;
                    type Subcommand = #effective_ty;
                    fn get(__this: &mut Self::State) -> &mut Option<Self::Subcommand> {
                        &mut __this.#ident
                    }
                }
                // TODO: We discard the parser fn here and reparse it in `place_for_subcommand`.
                // It seems impossible to somehow return it by partly erase the subcommand type.
                if !__is_last
                    && <#effective_ty as __rt::CommandInternal>::feed_subcommand(__arg.as_os_str()).is_some()
                {
                    return __rt::place_for_subcommand::<__Subcommand>(self);
                }
            }
        } else {
            TokenStream::new()
        };

        let mut arms = TokenStream::new();
        for (ord, &i) in def.unnamed_fields.iter().enumerate() {
            let FieldInfo { ident, effective_ty, .. } = def.fields[i];
            let parsed = value_parsed(effective_ty);
            arms.extend(quote! {
                #ord => self.#ident = __rt::Some(#parsed),
            });
        }

        let catchall = if let Some(CatchallFieldInfo { field_idx, greedy }) = def.catchall_field {
            let FieldInfo { ident, effective_ty, .. } = def.fields[field_idx];
            if greedy {
                let value_info = value_info(effective_ty);
                quote! { __rt::place_for_trailing_var_arg(&mut self.#ident, #value_info) }
            } else {
                let parsed = value_parsed(effective_ty);
                quote! {{ self.#ident.get_or_insert_default().push(#parsed); __rt::Ok(__rt::None) }}
            }
        } else if def.subcommand.is_some() {
            // Prefer to report "unknown subcommand" error for extra unnamed arguments.
            quote! { __rt::place_for_subcommand::<__Subcommand>(self) }
        } else {
            quote! { __rt::Err(__rt::None) }
        };

        let non_last = if arms.is_empty() {
            catchall
        } else {
            quote! {
                match __idx {
                    #arms
                    _ => return #catchall
                }
                __rt::Ok(__rt::None)
            }
        };

        let handle_last = def.last_field.map(|idx| {
            let FieldInfo { ident, effective_ty, .. } = &def.fields[idx];
            let value_info = value_info(effective_ty);
            quote! {
                if __is_last {
                    return __rt::place_for_trailing_var_arg(&mut self.#ident, #value_info);
                }
            }
        });

        tokens.extend(quote! {
            fn feed_unnamed(
                &mut self,
                __arg: &mut __rt::OsString,
                __idx: __rt::usize,
                __is_last: __rt::bool,
            ) -> __rt::FeedUnnamed {
                #asserts
                #handle_last
                #handle_subcmd
                #non_last
            }
        });
    }
}

struct ValidationImpl<'i>(&'i ParserStateDefImpl<'i>);

impl ToTokens for ValidationImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let def = self.0;

        if def.fields.iter().any(|f| f.exclusive) {
            let names = def.fields.iter().map(|f| f.ident).chain(def.subcommand.map(|s| s.ident));
            tokens.extend(quote! {
                let __argcnt = 0usize #(+ self.#names.is_some() as usize)*;
            });
        }

        for (f, idx) in def.fields.iter().zip(0u8..) {
            let ident = f.ident;
            if f.attrs.required && f.default_expr.is_none() {
                tokens.extend(quote! {
                    if self.#ident.is_none() {
                        return __rt::missing_required_arg::<Self, _>(#idx)
                    }
                });
            }

            let mut checks = TokenStream::new();
            if f.exclusive {
                checks.extend(quote! {
                    if __argcnt != 1 {
                        return __rt::constraint_exclusive::<Self, _>(#idx);
                    }
                });
            }
            if !f.dependencies.is_empty() {
                let paths = f.dependencies.iter();
                checks.extend(quote! {
                    if #(self #paths.is_none())||* {
                        return __rt::constraint_required::<Self, _>(#idx);
                    }
                });
            }
            if !f.conflicts.is_empty() {
                let paths = f.conflicts.iter();
                checks.extend(quote! {
                    if #(self #paths.is_some())||* {
                        return __rt::constraint_conflict::<Self, _>(#idx);
                    }
                });
            }
            if !checks.is_empty() {
                tokens.extend(quote! {
                    if self.#ident.is_some() {
                        #checks
                    }
                });
            }
        }

        if let Some(SubcommandInfo { ident, .. }) = def.subcommand.as_ref().filter(|s| !s.optional)
        {
            tokens.extend(quote! {
                if self.#ident.is_none() {
                    return __rt::missing_required_subcmd()
                }
            });
        }

        // Set default values after checks.
        for FieldInfo { ident, default_expr, .. } in &def.fields {
            if let Some(e) = default_expr {
                tokens.extend(quote! {
                    if self.#ident.is_none() {
                        self.#ident = __rt::Some(#e);
                    }
                });
            }
        }
    }
}

/// Generates the reflection constant for `const RAW_ARGS_INFO`.
struct RawArgsInfo<'a>(&'a ParserStateDefImpl<'a>);

impl ToTokens for RawArgsInfo<'_> {
    // See format in `RawArgInfo`.
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let raw_descs =
            self.0.fields.iter().flat_map(|f| [&f.description, "\0"]).collect::<String>();

        let mut raw_help_buf = String::new();
        for f in &self.0.fields {
            if !f.hide {
                raw_help_buf.push(if f.attrs.required { '1' } else { '0' });
                raw_help_buf.push_str(&f.doc.0);
            }
            raw_help_buf.push('\0');
        }

        let (raw_descs, raw_helps) = if self.0.flatten_fields.is_empty() {
            (quote!(#raw_descs), quote!(#raw_help_buf))
        } else {
            let tys1 = self.0.flatten_fields.iter().map(|f| f.effective_ty);
            let tys2 = tys1.clone();
            let mut asserts = TokenStream::new();
            for ty in tys1.clone() {
                asserts.extend(quote_spanned! {ty.span()=>
                    __rt::assert!(
                        <<#ty as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.__subcommand.is_none(),
                        "cannot flatten an Args with subcommand",
                    );
                });
            }
            // FIXME: This duplicates strings quadratically, especially when a large
            // `impl Args` is flattened in many places like in the `deno-palc` example.
            (
                quote! {{
                    #asserts
                    __rt::__const_concat!(
                        #raw_descs,
                        #(<<#tys1 as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.__raw_arg_descs,)*
                    )
                }},
                quote! {
                    __rt::__const_concat!(
                        #raw_help_buf,
                        #(<<#tys2 as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.__raw_arg_helps,)*
                    )
                },
            )
        };

        let (is_subcmd_optional, subcmd) = if let Some(s) = &self.0.subcommand {
            let ty = &s.effective_ty;
            (s.optional, quote! {__rt::Some(<#ty as __rt::CommandInternal>::RAW_COMMAND_INFO) })
        } else {
            (false, quote! { __rt::None })
        };
        let subcmd_optional = if is_subcmd_optional { "1" } else { "0" };

        let raw_meta = if let Some(m) = &self.0.cmd_meta {
            let name = match &m.name {
                Some(s) => quote! { #s },
                None => quote! { env!("CARGO_PKG_NAME") },
            };
            let version = match &m.version {
                Some(Override::Explicit(s)) => quote! { #s },
                Some(Override::Inherit) => quote! { env!("CARGO_PKG_VERSION") },
                None => quote! { "" },
            };
            let author = match &m.author {
                Some(Override::Explicit(s)) => quote! { #s },
                Some(Override::Inherit) => quote! { env!("CARGO_PKG_AUTHORS") },
                None => quote! { "" },
            };
            // TODO: Compress this if it is the first line of `long_about`.
            let about = match &m.about {
                Some(Override::Explicit(s)) => quote! { #s },
                Some(Override::Inherit) => quote! { env!("CARGO_PKG_DESCRIPTION") },
                None => m.doc.summary().to_token_stream(),
            };
            let long_about = match &m.long_about {
                Some(Override::Explicit(s)) => quote! { #s },
                Some(Override::Inherit) | None => m.doc.to_token_stream(),
            };
            let after_help = match &m.after_help {
                Some(e) => quote! { #e },
                None => quote! { "" },
            };
            let after_long_help = match &m.after_long_help {
                Some(e) => quote! { #e },
                None => quote! { "" },
            };
            quote! {
                __rt::__const_concat!(
                    #subcmd_optional,
                    #name, "\0",
                    #version, "\0",
                    #author, "\0",
                    #about, "\0",
                    #long_about, "\0",
                    #after_help, "\0",
                    #after_long_help,
                )
            }
        } else {
            quote! { "0" }
        };

        let self_arg_cnt = self.0.fields.len() as u8;
        let self_unnamed_arg_cnt = self.0.unnamed_fields.len() as u8;
        let flatten_tys1 = self.0.flatten_fields.iter().map(|f| f.effective_ty);
        let flatten_tys2 = flatten_tys1.clone();

        tokens.extend(quote! {
            __rt::RawArgsInfo {
                __total_arg_cnt: #self_arg_cnt
                    #(+ <<#flatten_tys1 as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.__total_arg_cnt)*,
                __total_unnamed_arg_cnt: #self_unnamed_arg_cnt
                    #(+ <<#flatten_tys2 as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.__total_unnamed_arg_cnt)*,
                __subcommand: #subcmd,
                __raw_arg_descs: #raw_descs,
                __raw_arg_helps: __rt::__gate_help_str!(#raw_helps),
                __raw_meta: __rt::__gate_help_str!(#raw_meta),
            }
        });
    }
}
