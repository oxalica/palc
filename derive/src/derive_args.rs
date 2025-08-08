use std::collections::HashMap;
use std::num::NonZero;

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Fields, FieldsNamed, Ident, LitChar, LitStr, Visibility};

use crate::common::{
    ArgOrCommand, ArgTyKind, ArgsCommandMeta, CommandMeta, Doc, FieldPath, Override, TY_OPTION,
    strip_ty_ctor, wrap_anon_item,
};
use crate::error::{Result, catch_errors};
use crate::shared::{AcceptHyphen, ArgAttrs};

pub fn expand(input: &DeriveInput) -> TokenStream {
    assert_no_generics!(input);

    match catch_errors(|| match &input.data {
        Data::Struct(syn::DataStruct { fields: Fields::Named(fields), .. }) => {
            try_expand_for_named_struct(input, fields)
        }
        _ => abort!(Span::call_site(), "only structs with named fields are supported"),
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
        impl __rt::Args for #ident {
            type __State = __rt::FallbackState<#ident>;
        }
    }
}

pub fn try_expand_for_named_struct(
    input: &DeriveInput,
    fields: &FieldsNamed,
) -> Result<TokenStream> {
    let ident = &input.ident;
    let cmd_meta = CommandMeta::parse_attrs_opt(&input.attrs);
    let state_name = format_ident!("{}State", input.ident);
    let struct_name = input.ident.to_token_stream();
    let state =
        expand_state_def_impl(&input.vis, cmd_meta.as_deref(), state_name, struct_name, fields)?;
    let state_name = &state.state_name;

    Ok(quote! {
        #state

        #[automatically_derived]
        impl __rt::Args for #ident {
            type __State = #state_name;
        }
    })
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

    cmd_meta: Option<&'i CommandMeta>,
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
    default_value: Option<DefaultValue>,
    exclusive: bool,
    dependencies: Vec<FieldPath>,
    conflicts: Vec<FieldPath>,

    // Docs //
    /// Example-like description, eg. `--key <VALUE>`, `-c, --color=<COLOR>`.
    description: String,
    doc: Doc,
    hide: bool,
}

enum DefaultValue {
    ParseStr(LitStr),
    ValueExpr(TokenStream),
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

fn encode_long_name(name: &LitStr) -> String {
    let s = name.value();
    if s.is_empty() {
        emit_error!(name, "arg(long) name must NOT be empty");
    } else if s.starts_with('-') {
        emit_error!(
            name,
            r#"arg(long) name is automatically prefixed by "--", and you should not add more "-" prefix"#,
        );
    } else if s.contains(|c: char| c == '=' || c.is_ascii_control()) {
        emit_error!(name, "arg(long) name must NOT contain '=' or ASCII control characters");
    }
    if s.len() > 1 {
        s
    } else {
        // Disambiguate from short names.
        format!("--{s}")
    }
}

fn encode_short_name(name: &LitChar) -> String {
    let c = name.value();
    if c == '-' || c.is_ascii_control() {
        emit_error!(name, "arg(short) name must NOT be '-' or ASCII control characters");
    } else if !c.is_ascii() {
        // NB. It is assumed to be ASCII in `refl::NamedArgInfo::short_args()` and
        // `ArgsIter::next_arg()`.
        emit_error!(
            name,
            "Non-ASCII arg(short) name is reserved. Use `arg(long)` instead. \
            A unicode codepoint is not necessarity a \"character\" in human sense, thus \
            automatic splitting or argument de-bundling may give unexpected results. \
            If you do want this to be supported, convince us by opening an issue.",
        );
    }
    c.into()
}

pub fn expand_state_def_impl<'i>(
    vis: &'i Visibility,
    cmd_meta: Option<&'i CommandMeta>,
    state_name: Ident,
    output_ty: TokenStream,
    input_fields: &'i syn::FieldsNamed,
) -> Result<ParserStateDefImpl<'i>> {
    let fields = &input_fields.named;

    if u8::try_from(fields.len()).is_err() {
        abort!(input_fields, "only up to 255 fields are supported");
    }

    let mut out = ParserStateDefImpl {
        vis,
        state_name,
        output_ty,
        output_ctor: None,
        fields: Vec::with_capacity(fields.len()),
        flatten_fields: Vec::new(),
        subcommand: None,

        named_fields: Vec::new(),
        unnamed_fields: Vec::new(),
        catchall_field: None,
        last_field: None,
        cmd_meta,
    };

    let mut variable_len_arg_span = None;
    let mut check_variable_len_arg = |span: Span| {
        if let Some(prev) = variable_len_arg_span.replace(span) {
            emit_error!(span, "duplicated variable-length arguments");
            emit_error!(prev, "previously defined here");
        }
    };

    let mut seen_enc_names = HashMap::new();
    let mut check_dup_name = |enc_name: String, span: Span| {
        if let Some(prev_span) = seen_enc_names.insert(enc_name, span) {
            emit_error!(span, "duplicated argument names");
            emit_error!(prev_span, "previously defined here");
        }
    };

    for field in fields {
        let attrs = ArgOrCommand::parse_attrs(&field.attrs);
        let ident = field.ident.as_ref().expect("named struct");
        let ident_str = ident.to_string();

        let mut arg = match attrs {
            ArgOrCommand::Arg(arg) => arg,
            ArgOrCommand::Command(ArgsCommandMeta::Subcommand) => {
                check_variable_len_arg(ident.span());
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
        let default_value = match (arg.default_value_t.take(), arg.default_value.take()) {
            (Some(Override::Inherit), None) => {
                Some(DefaultValue::ValueExpr(quote_spanned! {effective_ty.span()=>
                    <#effective_ty as __rt::Default>::default()
                }))
            }
            (Some(Override::Explicit(e)), None) => {
                Some(DefaultValue::ValueExpr(e.into_token_stream()))
            }
            (None, Some(default_str)) => Some(DefaultValue::ParseStr(default_str)),
            (None, None) => None,
            (Some(_), Some(tt)) => {
                emit_error!(tt, "arg(default_value) conflicts with arg(default_value_t)");
                None
            }
        };
        let required = if default_value.is_none() {
            arg.required || finish == FieldFinish::UnwrapChecked
        } else {
            if arg.required {
                emit_error!(
                    ident,
                    "`arg(required)` conflicts with `arg(default_value, default_value_t)`"
                );
            }
            false
        };

        let value_delimiter = if let Some(ch) = &arg.value_delimiter {
            let ch = ch.value();
            if !matches!(kind, FieldKind::OptionVec) {
                emit_error!(ch, "arg(value_delimiter) must be used on Vec-like types");
                None
            } else if !ch.is_ascii() || ch.is_ascii_control() {
                emit_error!(
                    ch,
                    r#"arg(value_delimiter) must be non-control ASCII characters. \
                    A unicode codepoint is not necessarity a "character" in human sense, thus \
                    automatic splitting may give unexpected results. \
                    If you do want this to be supported, convince us by opening an issue."#,
                );
                None
            } else if !arg.is_named() {
                emit_error!(
                    ch,
                    "TODO: arg(value_delimiter) is not yet supported on unnamed arguments",
                );
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
                emit_error!(
                    ident,
                    "arg(allow_hyphen_values) and arg(allow_negative_numbers) \
                        conflict with each other",
                );
                AcceptHyphen::No
            }
        };
        if accept_hyphen != AcceptHyphen::No
            && matches!(kind, FieldKind::BoolSetTrue | FieldKind::Counter)
        {
            emit_error!(ident, "Only arguments that take values can allow hyphen values");
        }

        let value_name = match &arg.value_name {
            Some(s) => s.value(),
            None => heck::AsShoutySnekCase(&ident_str).to_string(),
        };
        if value_name.contains(|ch: char| ch.is_ascii_control()) {
            emit_error!(value_name, "arg(value_name) must NOT contain ASCII control characters");
        }

        if arg.is_named() {
            // Named arguments.

            if arg.last || arg.trailing_var_arg {
                emit_error!(
                    field.ty,
                    "arg(last, trailing_var_arg) only support positional arguments",
                );
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
                let enc = encode_long_name(name);
                enc_names.push(enc.clone());
                check_dup_name(enc, name.span());
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
                let enc = encode_short_name(name);
                enc_names.push(enc.clone());
                check_dup_name(enc, name.span());
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
                make_lowercase: arg.ignore_case,
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
                default_value,
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
                emit_error!(ident, "arg(require_equals, global) only support named arguments");
                continue;
            }
            if arg.ignore_case {
                emit_error!(ident, "arg(ignore_case) only support named arguments");
            }
            if arg.default_value_t.is_some() {
                emit_error!(ident, "TODO: arg(default_value_t) supports named arguments yet");
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
                make_lowercase: false,
                index: field_idx as _,
            };
            out.fields.push(FieldInfo {
                ident,
                kind,
                effective_ty,
                finish,
                enc_names: Vec::new(),
                attrs,
                default_value,
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
                    emit_error!(ident, "duplicated arg(last)");
                    emit_error!(out.fields[prev].ident.span(), "previously defined here");
                }

                // `allow_hyphen_values` is allowed for last arguments, though it is already assumed.
                true
            } else if is_vec_like {
                // Variable length unnamed argument.

                let greedy = arg.trailing_var_arg;
                check_variable_len_arg(ident.span());
                out.catchall_field = Some(CatchallFieldInfo { field_idx, greedy });
                greedy
            } else {
                // Single unnamed argument.
                out.unnamed_fields.push(field_idx);
                false
            };

            if accept_hyphen != AcceptHyphen::No && !allow_accept_hyphen {
                emit_error!(
                    ident,
                    "arg(allow_hyphen_values) can only be used on \
                    named arguments or arg(trailing_var_arg) yet",
                );
            }
        }
    }

    if let Some(f) = out.fields.iter().find(|f| f.exclusive) {
        if !out.flatten_fields.is_empty() {
            emit_error!(
                f.ident,
                "TODO: arg(exclusive) is not supported on struct containing arg(flatten) yet",
            );
        }
    }

    Ok(out)
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
            field_tys.push(
                quote_spanned! {effective_ty.span()=> <#effective_ty as __rt::Args>::__State },
            );
            field_inits.push(quote! { __rt::ParserState::init() });
            field_finishes.push(quote! { __rt::ParserState::finish(&mut self.#ident)? });
        }

        // Assertions to be forced to evaluate.
        let mut asserts = TokenStream::new();
        for FlattenFieldInfo { effective_ty, .. } in &self.flatten_fields {
            asserts.extend(quote_spanned! {effective_ty.span()=>
                __rt::assert!(
                    <<#effective_ty as __rt::Args>::__State as __rt::ParserState>::TOTAL_UNNAMED_ARG_CNT == 0,
                    "TODO: cannot arg(flatten) positional arguments yet",
                );
                __rt::assert!(
                    !<<#effective_ty as __rt::Args>::__State as __rt::ParserState>::HAS_SUBCOMMAND,
                    "cannot flatten an Args with subcommand",
                );
            });
        }
        for &idx in &self.named_fields {
            let FieldInfo { effective_ty, attrs, .. } = &self.fields[idx];
            if attrs.make_lowercase {
                asserts.extend(quote_spanned! {effective_ty.span()=>
                    __rt::assert!(
                        <#effective_ty as __rt::ValueEnum>::NO_UPPER_CASE,
                        "`arg(ignore_case)` only supports `ValueEnum` that contains no UPPERCASE variants"
                    );
                });
            }
        }

        let feed_named_func = FeedNamedImpl(self);
        let feed_unnamed_func = FeedUnnamedImpl(self);
        let validation = ValidationImpl(self);

        let output_ctor = self.output_ctor.as_ref().unwrap_or(&self.output_ty);

        let raw_args_info = RawArgsInfo(self);
        let has_subcommand = self.subcommand.is_some();

        let self_arg_cnt = self.fields.len() as u8;
        let self_unnamed_arg_cnt = self.unnamed_fields.len() as u8;
        let flatten_tys1 = self.flatten_fields.iter().map(|f| f.effective_ty);
        let flatten_tys2 = flatten_tys1.clone();

        tokens.extend(quote! {
            #vis struct #state_name {
                #(#field_names : #field_tys,)*
            }

            #[automatically_derived]
            impl __rt::ParserState for #state_name {
                type Output = #output_ty;

                const RAW_ARGS_INFO: &'static __rt::RawArgsInfo = #raw_args_info;
                const HAS_SUBCOMMAND: __rt::bool = #has_subcommand;

                const TOTAL_ARG_CNT: __rt::u8 = #self_arg_cnt
                    #(+ <<#flatten_tys1 as __rt::Args>::__State as __rt::ParserState>::TOTAL_ARG_CNT)*;
                const TOTAL_UNNAMED_ARG_CNT: __rt::u8 = #self_unnamed_arg_cnt
                    #(+ <<#flatten_tys2 as __rt::Args>::__State as __rt::ParserState>::TOTAL_UNNAMED_ARG_CNT)*;

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

                fn info(&self) -> &'static __rt::RawArgsInfo {
                    &<Self as __rt::ParserState>::RAW_ARGS_INFO
                }
            }

            // The result is wrapped in a `const _: () = { .. }`, which forces evaluation.
            #asserts
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
                        #( + <<#prefix_tys as __rt::Args>::__State as __rt::ParserState>::TOTAL_ARG_CNT)*
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
                    && <#effective_ty as __rt::Subcommand>::feed_subcommand(__arg.as_os_str()).is_some()
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
            if f.attrs.required && f.default_value.is_none() {
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
        for FieldInfo { ident, default_value, effective_ty, .. } in &def.fields {
            if let Some(default) = default_value {
                let e = match default {
                    DefaultValue::ValueExpr(e) => e,
                    DefaultValue::ParseStr(s) => {
                        let value_info = value_info(effective_ty);
                        &quote_spanned! {effective_ty.span()=>
                            __rt::parse_default_str(#s, #value_info)?
                        }
                    }
                };
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
        // Generate help string formatter.
        let mut usage_named = FormatArgsBuilder::default();
        let mut usage_unnamed = FormatArgsBuilder::default();
        let mut help_named = FormatArgsBuilder::default();
        let mut help_unnamed = FormatArgsBuilder::default();
        for (idxs, help, usage) in [
            (&self.0.named_fields, &mut help_named, &mut usage_named),
            (&self.0.unnamed_fields, &mut help_unnamed, &mut usage_unnamed),
        ] {
            for &idx in idxs {
                let f = &self.0.fields[idx];
                if f.hide {
                    continue;
                }
                if f.attrs.required {
                    usage.maybe_push_usage_for(f);
                }
                help.maybe_push_help_for(f);
            }
        }
        // [FOO]...
        if let Some(c) = &self.0.catchall_field {
            let f = &self.0.fields[c.field_idx];
            if !f.hide {
                usage_unnamed.maybe_push_usage_for(f);
                help_unnamed.maybe_push_help_for(f);
            }
        }
        // -- [LAST]
        if let Some(idx) = self.0.last_field {
            let f = &self.0.fields[idx];
            if !f.hide {
                usage_unnamed.template.push_str(" --");
                usage_unnamed.maybe_push_usage_for(f);
                help_unnamed.maybe_push_help_for(f);
            }
        }

        let flatten_tys = self.0.flatten_fields.iter().map(|f| f.effective_ty);
        let flatten_tys2 = flatten_tys.clone();
        let fmt_fn = quote! {
            |__w, __what| {
                // WAIT: Rust 1.89 in order to join `format_args` results and `write_fmt` once.
                let _ = match __what {
                    0u8 => __rt::fmt::Write::write_fmt(__w, #help_unnamed),
                    1u8 => __rt::fmt::Write::write_fmt(__w, #help_named),
                    2u8 => __rt::fmt::Write::write_fmt(__w, #usage_unnamed),
                    _ => __rt::fmt::Write::write_fmt(__w, #usage_named),
                };
                #(<<#flatten_tys as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.fmt_help()(__w, __what);)*
            }
        };

        // Compose arg descriptions.

        let descs = self.0.fields.iter().flat_map(|f| [&f.description, "\0"]).collect::<String>();
        let descs = if self.0.flatten_fields.is_empty() {
            quote!(#descs)
        } else {
            let tys = self.0.flatten_fields.iter().map(|f| f.effective_ty);
            // FIXME: This duplicates strings quadratically, especially when a large
            // `impl Args` is flattened in many places like in the `deno-palc` example.
            quote! {
                __rt::__const_concat!(
                    #descs,
                    #(<<#tys as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.raw_descriptions(),)*
                )
            }
        };

        let (subcmd_opt, subcmd_info) = match &self.0.subcommand {
            Some(SubcommandInfo { effective_ty, optional, .. }) => (
                quote! { #optional },
                quote! { __rt::Some(<#effective_ty as __rt::Subcommand>::RAW_INFO) },
            ),
            None => (quote! { false }, quote! { __rt::None }),
        };

        let has_optional_named = if self.0.named_fields.iter().any(|&idx| {
            let f = &self.0.fields[idx];
            !f.attrs.required && !f.hide
        }) {
            quote! { true }
        } else {
            quote! {
                false
                #(|| <<#flatten_tys2 as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO.has_optional_named())*
            }
        };

        let cmd_doc = CommandDoc(self.0.cmd_meta);

        tokens.extend(quote! {
            &__rt::RawArgsInfo::new(
                #subcmd_opt,
                #has_optional_named,
                #subcmd_info,
                #cmd_doc,
                #descs,
                #fmt_fn,
            )
        });
    }
}

#[derive(Default)]
struct FormatArgsBuilder {
    template: String,
    args: TokenStream,
}

impl FormatArgsBuilder {
    fn push_arg(&mut self, tts: impl ToTokens) {
        self.template.push_str("{}");
        self.args.extend(quote! { , });
        tts.to_tokens(&mut self.args);
    }

    fn maybe_push_help_for(&mut self, f: &FieldInfo<'_>) {
        self.template.push_str("  ");
        if f.description.starts_with("--") {
            // Pad 4 spaces before "--key" to align with "-k, --key".
            self.template.push_str("    ");
        }
        self.push_arg(&f.description);
        self.template.push('\n');

        if !f.doc.0.is_empty() {
            self.template.push_str("          ");
            self.push_arg(&f.doc);
            self.template.push('\n');
        }

        if let Some(default) = &f.default_value {
            self.template.push_str("          [default: ");
            let arg: &dyn ToTokens = match default {
                DefaultValue::ParseStr(s) => s,
                DefaultValue::ValueExpr(e) => {
                    let effective_ty = f.effective_ty;
                    &quote_spanned! {e.span()=> __rt::assert_impl_display_for_help::<#effective_ty>(#e) }
                }
            };
            self.push_arg(arg);
            self.template.push_str("]\n");
        }

        self.template.push('\n');
    }

    fn maybe_push_usage_for(&mut self, f: &FieldInfo<'_>) {
        self.template.push(' ');
        // For "-k, --key <VALUE>", show its usage as "--key <VALUE>".
        let desc = if matches!(f.description.as_bytes(), [b'-', b, b',',  ..] if *b != b'-') {
            &f.description[4..]
        } else {
            &f.description
        };
        self.push_arg(desc);
    }
}

impl ToTokens for FormatArgsBuilder {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Self { template, args } = self;
        tokens.extend(quote! {
            __rt::format_args!(#template #args)
        });
    }
}

/// String value for `RawArgsInfo::cmd_doc`.
pub struct CommandDoc<'a>(pub Option<&'a CommandMeta>);

impl ToTokens for CommandDoc<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let Some(m) = self.0 else {
            "".to_tokens(tokens);
            return;
        };

        let long_about: &dyn ToTokens = match &m.long_about {
            Some(Override::Explicit(e)) => e,
            Some(Override::Inherit) => &quote! { env!("CARGO_PKG_DESCRIPTION") },
            None => &m.doc,
        };

        let after_long_help: &dyn ToTokens = match &m.after_long_help {
            Some(e) => e,
            None => &"",
        };

        tokens.extend(quote! {
            __rt::__const_concat!(
                #long_about,
                "\0",
                #after_long_help,
            )
        });
    }
}
