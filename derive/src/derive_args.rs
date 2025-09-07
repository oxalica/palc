use std::collections::HashMap;
use std::num::NonZero;

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Fields, FieldsNamed, Ident, LitChar, LitStr, Visibility};

use crate::common::{
    ArgOrCommand, ArgsCommandMeta, CommandMeta, Doc, FieldPath, Override, TY_BOOL, TY_OPTION,
    TY_U8, TY_VEC, strip_ty_ctor, wrap_anon_item,
};
use crate::error::{Result, catch_errors};
use crate::shared::ArgAttrs;

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

    named_fields: Vec<FieldInfo<'i>>,
    unnamed_fields: Vec<FieldInfo<'i>>,
    variable_num_unnamed: Option<FieldInfo<'i>>,
    /// The last argument that accepts only after `--`.  Since it's after `--`,
    /// it is always greedy and skips subcommand or named argument handling.
    last_unnamed: Option<FieldInfo<'i>>,

    /// The total number of direct fields, excluding flattened or subcommand fields.
    direct_field_cnt: u8,

    /// Indirect fields that needs delegation.
    flatten_fields: Vec<FlattenFieldInfo<'i>>,
    /// Subcommand is special.
    subcommand: Option<SubcommandInfo<'i>>,

    cmd_meta: Option<&'i CommandMeta>,
}

impl ParserStateDefImpl<'_> {
    /// All fields directly parsed, excluding flattened and subcommands fields.
    fn direct_fields(&self) -> impl Iterator<Item = &FieldInfo<'_>> {
        self.unnamed_fields
            .iter()
            .chain(&self.variable_num_unnamed)
            .chain(&self.last_unnamed)
            .chain(&self.named_fields)
    }

    fn assign_field_idx(&mut self) {
        for (f, i) in self
            .unnamed_fields
            .iter_mut()
            .chain(&mut self.variable_num_unnamed)
            .chain(&mut self.last_unnamed)
            .chain(&mut self.named_fields)
            .zip(0..)
        {
            // Not set yet.
            assert_eq!(f.attrs.get_index(), 0);
            f.attrs.set(ArgAttrs::index(i), true);
        }
    }

    fn flatten_tys(&self) -> impl Iterator<Item = &syn::Type> {
        self.flatten_fields.iter().map(|f| f.ty)
    }
}

struct FieldInfo<'i> {
    // Basics //
    ident: &'i Ident,
    kind: MagicKind,
    /// The type used for parser inference, with `Option`/`Vec` stripped.
    value_ty: &'i syn::Type,
    value_parser: ValueParser<'i>,

    // Arg configurables //
    /// Encoded names for matching. Empty for unnamed arguments.
    enc_names: Vec<String>,
    attrs: ArgAttrs,

    // Validations //
    required: bool,
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

struct ValueParser<'i>(&'i syn::Type);

impl ToTokens for ValueParser<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ty = self.0;
        tokens.extend(quote_spanned! {ty.span()=>
            __rt::assert_auto_infer_value_parser_ok(__rt::InferValueParser::<#ty, &&&()>(__rt::PhantomData).get())
        });
    }
}

enum DefaultValue {
    ParseStr(LitStr),
    ValueExpr(TokenStream),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MagicKind {
    Bool,
    U8,
    Value,
    Option,
    OptionOption,
    Vec,
    OptionVec,
}

impl MagicKind {
    fn classify(ty: &syn::Type) -> (Self, &syn::Type) {
        let syn::Type::Path(syn::TypePath { qself: None, path }) = ty else {
            return (MagicKind::Value, ty);
        };
        if path.is_ident(TY_BOOL) {
            (MagicKind::Bool, ty)
        } else if path.is_ident(TY_U8) {
            (MagicKind::U8, ty)
        } else if let Some(ty) = strip_ty_ctor(ty, TY_OPTION) {
            if let Some(ty) = strip_ty_ctor(ty, TY_OPTION) {
                (MagicKind::OptionOption, ty)
            } else if let Some(subty) = strip_ty_ctor(ty, TY_VEC) {
                (MagicKind::OptionVec, subty)
            } else {
                (MagicKind::Option, ty)
            }
        } else if let Some(ty) = strip_ty_ctor(ty, TY_VEC) {
            (MagicKind::Vec, ty)
        } else {
            (MagicKind::Value, ty)
        }
    }

    fn accepts_no_value(self) -> bool {
        matches!(self, Self::Bool | Self::U8)
    }

    fn accepts_multiple_values(self) -> bool {
        matches!(self, Self::Vec | Self::OptionVec)
    }

    fn place_ty(self, value_ty: &syn::Type) -> TokenStream {
        match self {
            MagicKind::Bool => quote! { __rt::FlagPlace },
            MagicKind::U8 => quote! { __rt::CounterPlace },
            MagicKind::Value | MagicKind::Option => quote! { __rt::SetValuePlace<#value_ty> },
            MagicKind::OptionOption => quote! { __rt::SetOptionalValuePlace<#value_ty> },
            MagicKind::Vec | MagicKind::OptionVec => quote! { __rt::VecPlace<#value_ty> },
        }
    }

    fn finalizer(self) -> TokenStream {
        match self {
            MagicKind::Bool | MagicKind::U8 | MagicKind::Vec | MagicKind::Value => {
                quote! { finish }
            }
            MagicKind::Option | MagicKind::OptionOption | MagicKind::OptionVec => {
                quote! { finish_opt }
            }
        }
    }
}

#[derive(Clone, Copy)]
struct SubcommandInfo<'i> {
    ident: &'i Ident,
    ty: &'i syn::Type,
    optional: bool,
}

#[derive(Clone, Copy)]
struct FlattenFieldInfo<'i> {
    ident: &'i Ident,
    ty: &'i syn::Type,
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
        // `RawParser::next_arg()`.
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

        named_fields: Vec::new(),
        unnamed_fields: Vec::new(),
        variable_num_unnamed: None,
        last_unnamed: None,
        direct_field_cnt: 0,

        flatten_fields: Vec::new(),
        subcommand: None,

        cmd_meta,
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
                let (optional, ty) = match strip_ty_ctor(&field.ty, TY_OPTION) {
                    Some(subty) => (true, subty),
                    None => (false, &field.ty),
                };
                if let Some(prev) = &out.subcommand {
                    emit_error!(ident, "duplicated subcommand");
                    emit_error!(prev.ident, "previously defined here");
                } else {
                    out.subcommand = Some(SubcommandInfo { ident, ty, optional });
                }
                continue;
            }
            ArgOrCommand::Command(ArgsCommandMeta::Flatten) => {
                out.flatten_fields.push(FlattenFieldInfo { ident, ty: &field.ty });
                continue;
            }
        };

        let (kind, value_ty) = MagicKind::classify(&field.ty);

        // Default values.
        let default_value = match (arg.default_value_t.take(), arg.default_value.take()) {
            (Some(Override::Inherit), None) => {
                Some(DefaultValue::ValueExpr(quote_spanned! {value_ty.span()=>
                    <#value_ty as __rt::Default>::default()
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
            arg.required || kind == MagicKind::Value
        } else {
            if arg.required {
                emit_error!(
                    ident,
                    "`arg(required)` conflicts with `arg(default_value, default_value_t)`"
                );
            }
            false
        };

        let value_delimiter = if let Some(lit_ch) = &arg.value_delimiter {
            let ch = lit_ch.value();
            if !kind.accepts_multiple_values() {
                emit_error!(lit_ch, "arg(value_delimiter) can only be used on Vec-like types");
                None
            } else if !ch.is_ascii() || ch.is_ascii_control() {
                emit_error!(
                    lit_ch,
                    r#"arg(value_delimiter) must be non-control ASCII characters. \
                    A unicode codepoint is not necessarity a "character" in human sense, thus \
                    automatic splitting may give unexpected results. \
                    If you do want this to be supported, convince us by opening an issue."#,
                );
                None
            } else if !arg.is_named() {
                emit_error!(
                    lit_ch,
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
            (true, false) => ArgAttrs::ACCEPT_HYPHEN_ANY,
            (false, true) => ArgAttrs::ACCEPT_HYPHEN_NUM,
            (false, false) => ArgAttrs::default(),
            (true, true) => {
                // TODO: More accurate spans.
                emit_error!(
                    ident,
                    "arg(allow_hyphen_values) and arg(allow_negative_numbers) \
                        conflict with each other",
                );
                ArgAttrs::default()
            }
        };
        if accept_hyphen != ArgAttrs::default() && kind.accepts_no_value() {
            emit_error!(
                ident,
                "arg(allow_{{hyphen,negative}}_value) is incompatible with bool or u8",
            );
        }

        let value_name = match &arg.value_name {
            Some(s) => s.value(),
            None => heck::AsShoutySnekCase(&ident_str).to_string(),
        };
        if value_name.contains(|ch: char| ch.is_ascii_control()) {
            emit_error!(value_name, "arg(value_name) must NOT contain ASCII control characters");
        }

        let mut attrs = ArgAttrs::delimiter(value_delimiter).union(accept_hyphen);
        attrs.set(ArgAttrs::NO_VALUE, kind.accepts_no_value());
        attrs.set(ArgAttrs::REQUIRE_EQ, arg.require_equals);
        attrs.set(ArgAttrs::GLOBAL, arg.global);
        attrs.set(ArgAttrs::MAKE_LOWERCASE, arg.ignore_case);

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
                if !kind.accepts_no_value() {
                    write!(buf, "{}<{}>", if arg.require_equals { '=' } else { ' ' }, value_name)
                        .unwrap();
                }
                buf
            };

            out.direct_field_cnt += 1;

            out.named_fields.push(FieldInfo {
                ident,
                kind,
                value_ty,
                value_parser: ValueParser(value_ty),
                enc_names,
                attrs,
                required,
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

            if arg.require_equals || arg.global || arg.ignore_case {
                emit_error!(
                    ident,
                    "arg(require_equals, global, ignore_case) only support named arguments"
                );
            }
            if arg.default_value_t.is_some() {
                emit_error!(ident, "TODO: arg(default_value_t) supports named arguments yet");
            }
            if accept_hyphen != ArgAttrs::default() {
                emit_error!(
                    ident,
                    "arg(allow_hyphen_values) can only be used on \
                    named arguments or arg(trailing_var_arg) yet",
                );
            }

            // Does this argument accept a variable number of input arguments?
            let is_variable_num = kind.accepts_multiple_values();

            let mut description =
                if required { format!("<{value_name}>") } else { format!("[{value_name}]") };
            if is_variable_num {
                description.push_str("...");
            }

            // arg(last) implies greedy.
            attrs.set(ArgAttrs::GREEDY, arg.trailing_var_arg | arg.last);

            out.direct_field_cnt += 1;
            let info = FieldInfo {
                ident,
                kind,
                value_ty,
                value_parser: ValueParser(value_ty),
                enc_names: Vec::new(),
                attrs,
                required,
                default_value,
                exclusive: arg.exclusive,
                dependencies: arg.requires,
                conflicts: arg.conflicts_with,
                description,
                doc: arg.doc,
                hide: arg.hide,
            };

            if arg.last {
                // Last argument(s).
                if !is_variable_num {
                    emit_error!(ident, "TODO: arg(last) only supports Vec-like types yet");
                }

                if let Some(prev) = &out.last_unnamed {
                    emit_error!(ident, "duplicated arg(last)");
                    emit_error!(prev.ident, "previously defined here");
                } else {
                    out.last_unnamed = Some(info);
                }
            } else if is_variable_num {
                // Variable length unnamed argument.
                if let Some(prev) = &out.variable_num_unnamed {
                    emit_error!(ident, "duplicated variable-length arguments");
                    emit_error!(prev.ident, "previously defined here");
                } else {
                    out.variable_num_unnamed = Some(info);
                }
            } else {
                // Single unnamed argument.

                if let Some(prev) = &out.variable_num_unnamed {
                    emit_error!(
                        ident,
                        "cannot have more unnamed arguments after a variable-length arguments"
                    );
                    emit_error!(prev.ident, "previous variable-length argument");
                }

                out.unnamed_fields.push(info);
            }
        }
    }

    out.assign_field_idx();

    if let Some(f) = out.direct_fields().find(|f| f.exclusive) {
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
        let Self { vis, state_name, output_ty, .. } = self;

        let (field_idents, place_tys, finalizers) = self
            .direct_fields()
            .map(|FieldInfo { ident, value_ty, kind, .. }| {
                let fin = kind.finalizer();
                (
                    *ident,
                    kind.place_ty(value_ty),
                    quote! { __rt::FieldState::#fin(&mut self.#ident) },
                )
            })
            .chain(self.flatten_fields.iter().map(|FlattenFieldInfo { ident, ty }| {
                (
                    *ident,
                    quote! { <#ty as __rt::Args>::__State },
                    quote! { __rt::ParserState::finish(&mut self.#ident)? },
                )
            }))
            .chain(self.subcommand.as_ref().map(|SubcommandInfo { ident, ty, optional }| {
                let unwrap = (!*optional).then(|| quote! { .unwrap() });
                (*ident, quote! { __rt::Option<#ty> }, quote! { self.#ident.take() #unwrap })
            }))
            .collect::<(Vec<_>, Vec<_>, Vec<_>)>();

        // Assertions to be forced to evaluate.
        let mut asserts = TokenStream::new();
        for ty in self.flatten_tys() {
            asserts.extend(quote_spanned! {ty.span()=>
                __rt::assert!(
                    <<#ty as __rt::Args>::__State as __rt::ParserState>::TOTAL_UNNAMED_ARG_CNT == 0,
                    "TODO: cannot arg(flatten) positional arguments yet",
                );
                __rt::assert!(
                    !<<#ty as __rt::Args>::__State as __rt::ParserState>::HAS_SUBCOMMAND,
                    "cannot flatten an Args with subcommand",
                );
            });
        }
        for FieldInfo { value_ty, attrs, .. } in &self.named_fields {
            if attrs.contains(ArgAttrs::MAKE_LOWERCASE) {
                asserts.extend(quote_spanned! {value_ty.span()=>
                    __rt::assert!(
                        <#value_ty as __rt::ValueEnum>::NO_UPPER_CASE,
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

        let direct_field_cnt = self.direct_field_cnt;
        let direct_unnamed_arg_cnt = self.unnamed_fields.len() as u8;
        let flatten_tys1 = self.flatten_tys();
        let flatten_tys2 = self.flatten_tys();

        tokens.extend(quote! {
            #[derive(::std::default::Default)]
            #vis struct #state_name {
                #(#field_idents: #place_tys,)*
            }

            #[automatically_derived]
            impl __rt::ParserState for #state_name {
                type Output = #output_ty;

                const RAW_ARGS_INFO: &'static __rt::RawArgsInfo = #raw_args_info;
                const HAS_SUBCOMMAND: __rt::bool = #has_subcommand;

                const TOTAL_ARG_CNT: __rt::u8 = #direct_field_cnt
                    #(+ <<#flatten_tys1 as __rt::Args>::__State as __rt::ParserState>::TOTAL_ARG_CNT)*;
                const TOTAL_UNNAMED_ARG_CNT: __rt::u8 = #direct_unnamed_arg_cnt
                    #(+ <<#flatten_tys2 as __rt::Args>::__State as __rt::ParserState>::TOTAL_UNNAMED_ARG_CNT)*;

                fn finish(&mut self) -> __rt::Result<Self::Output> {
                    #validation
                    __rt::Ok(#output_ctor {
                        #(#field_idents : #finalizers,)*
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
            .map(|FieldInfo { ident, value_parser, enc_names, attrs, .. }| {
                quote! { #(#enc_names)|* => (__rt::FieldState::place(&mut self.#ident, #value_parser), #attrs), }
            })
            .collect::<TokenStream>();

        // FIXME: This bloat generated code quadratically.
        let handle_else = {
            let direct_field_cnt = def.direct_field_cnt;
            let flatten_names = def.flatten_fields.iter().map(|f| f.ident);
            let offsets = (0..def.flatten_fields.len())
                .map(|i| {
                    let prefix_tys = def.flatten_fields[..i].iter().map(|f| f.ty);
                    quote! {
                        #direct_field_cnt
                        #( + <<#prefix_tys as __rt::Args>::__State as __rt::ParserState>::TOTAL_ARG_CNT)*
                    }
                });
            quote! {
                #(__rt::ParserStateDyn::feed_named(&mut self.#flatten_names, __name).map_break(|mut __ret| {
                    // FIXME: Add tests to ensure this overflow is caught at compile time.
                    // NB: This relies on that `ArgAttrs::index` is at the lowest 8-bits.
                    __ret.1.0 += const { #offsets } as ::std::primitive::u32;
                    __ret
                })?;)*
            }
        };

        let body = if arms.is_empty() {
            quote! { #handle_else __rt::ControlFlow::Continue(()) }
        } else {
            quote! {
                __rt::ControlFlow::Break(match __name {
                    #arms
                    _ => { #handle_else return __rt::ControlFlow::Continue(()) }
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
            && def.variable_num_unnamed.is_none()
            && def.last_unnamed.is_none()
            && def.subcommand.is_none()
        {
            return;
        }

        let handle_subcmd = def.subcommand.as_ref().map(|SubcommandInfo { ident, ty, .. }| {
            let state_name = &def.state_name;
            quote! {
                struct __Subcommand;
                impl __rt::GetSubcommand for __Subcommand {
                    type State = #state_name;
                    type Subcommand = #ty;
                    fn get(__this: &mut Self::State) -> &mut Option<Self::Subcommand> {
                        &mut __this.#ident
                    }
                }
                // TODO: We discard the parser fn here and reparse it in `place_for_subcommand`.
                // It seems impossible to somehow return it by partly erase the subcommand type.
                if !__is_last
                    && <#ty as __rt::Subcommand>::feed_subcommand(__arg).is_some()
                {
                    return __rt::place_for_subcommand::<__Subcommand>(self);
                }
            }
        });

        let mut arms = TokenStream::new();
        for (ord, FieldInfo { ident, value_parser, attrs, .. }) in
            def.unnamed_fields.iter().enumerate()
        {
            arms.extend(quote! {
                #ord => (__rt::FieldState::place(&mut self.#ident, #value_parser), #attrs),
            });
        }

        // Note: The catchall path is only entered if the subcommand does not match.
        let catchall =
            if let Some(FieldInfo { ident, value_parser, attrs, .. }) = &def.variable_num_unnamed {
                quote! { (__rt::FieldState::place(&mut self.#ident, #value_parser), #attrs) }
            } else if def.subcommand.is_some() {
                // Here we know the previous subcommand parse failed.
                // Just to report "unknown subcommand" error for it.
                quote! { return __rt::place_for_subcommand::<__Subcommand>(self) }
            } else {
                quote! { return __rt::ControlFlow::Continue(()) }
            };

        let handle_last = def.last_unnamed.as_ref().map(|FieldInfo { ident, value_parser, attrs, .. }| {
            quote! {
                if __is_last {
                    return __rt::ControlFlow::Break((__rt::FieldState::place(&mut self.#ident, #value_parser), #attrs));
                }
            }
        });

        tokens.extend(quote! {
            // If there is no match arm.
            #[allow(unreachable_code)]
            fn feed_unnamed(
                &mut self,
                __arg: &__rt::OsStr,
                __idx: __rt::usize,
                __is_last: __rt::bool,
            ) -> __rt::FeedUnnamed {
                #handle_last
                #handle_subcmd
                __rt::ControlFlow::Break(match __idx {
                    #arms
                    _ => #catchall
                })
            }
        });
    }
}

struct ValidationImpl<'i>(&'i ParserStateDefImpl<'i>);

impl ToTokens for ValidationImpl<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let def = self.0;

        let exclusive_idents =
            def.direct_fields().filter_map(|f| f.exclusive.then_some(f.ident)).collect::<Vec<_>>();
        if !exclusive_idents.is_empty() {
            let subcmd = def.subcommand.as_ref().map(|SubcommandInfo { ident, .. }| {
                quote! { self.#ident.is_some() as __rt::usize }
            });
            tokens.extend(quote! {
                if 0usize
                    #(+ __rt::FieldState::is_set(&self.#exclusive_idents) as __rt::usize)*
                    #subcmd
                    > 1usize
                {
                    __rt::todo!();
                }
            });
        }

        for f in def.direct_fields() {
            let ident = f.ident;
            let idx = f.attrs.get_index();
            if f.required {
                tokens.extend(quote! {
                    if !__rt::FieldState::is_set(&self.#ident) {
                        return __rt::missing_required_arg::<Self, _>(#idx)
                    }
                });
            }

            let mut checks = TokenStream::new();
            if !f.dependencies.is_empty() {
                let paths = &f.dependencies;
                checks.extend(quote! {
                    if #(!__rt::FieldState::is_set(&self #paths))||* {
                        return __rt::constraint_required::<Self, _>(#idx);
                    }
                });
            }
            if !f.conflicts.is_empty() {
                let paths = &f.conflicts;
                checks.extend(quote! {
                    if #(__rt::FieldState::is_set(&self #paths))||* {
                        return __rt::constraint_conflict::<Self, _>(#idx);
                    }
                });
            }
            if !checks.is_empty() {
                tokens.extend(quote! {
                    if __rt::FieldState::is_set(&self.#ident) {
                        #checks
                    }
                });
            }
        }

        if let Some(SubcommandInfo { ident, .. }) = def.subcommand.as_ref().filter(|s| !s.optional)
        {
            tokens.extend(quote! {
                if self.#ident.is_none() {
                    return __rt::missing_required_subcmd();
                }
            });
        }

        // Set default values after checks.
        for FieldInfo { ident, default_value, value_parser, .. } in def.direct_fields() {
            let Some(default_value) = default_value else { continue };
            tokens.extend(match default_value {
                DefaultValue::ValueExpr(e) => quote! {
                    __rt::FieldState::set_default(&mut self.#ident, || #e);
                },
                DefaultValue::ParseStr(s) => quote! {
                    __rt::FieldState::set_default_parse(&mut self.#ident, #s, #value_parser);
                },
            });
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
        for (fields, help, usage) in [
            (&self.0.named_fields[..], &mut help_named, &mut usage_named),
            (&self.0.unnamed_fields[..], &mut help_unnamed, &mut usage_unnamed),
        ] {
            for f in fields {
                if !f.hide {
                    if f.required {
                        usage.maybe_push_usage_for(f);
                    }
                    help.maybe_push_help_for(f);
                }
            }
        }
        if let Some(f) = &self.0.variable_num_unnamed {
            if !f.hide {
                // Variable unnamed fields are visible, no matter it's
                // optional (`[ARGS]...`) or required (`<ARGS>...`).
                usage_unnamed.maybe_push_usage_for(f);
                help_unnamed.maybe_push_help_for(f);
            }
        }
        // -- [LAST]
        if let Some(f) = &self.0.last_unnamed {
            if !f.hide {
                // FIXME: Optional last?
                usage_unnamed.template.push_str(" --");
                usage_unnamed.maybe_push_usage_for(f);
                help_unnamed.maybe_push_help_for(f);
            }
        }

        let fmt_fn = quote! {
            |__w, __what| {
                // WAIT: Rust 1.89 in order to join `format_args` results and `write_fmt` once.
                let _ = match __what {
                    0u8 => __rt::fmt::Write::write_fmt(__w, #help_unnamed),
                    1u8 => __rt::fmt::Write::write_fmt(__w, #help_named),
                    2u8 => __rt::fmt::Write::write_fmt(__w, #usage_unnamed),
                    _ => __rt::fmt::Write::write_fmt(__w, #usage_named),
                };
            }
        };

        let descs = self.0.direct_fields().flat_map(|f| [&f.description, "\0"]).collect::<String>();

        let (subcmd_opt, subcmd_info) = match &self.0.subcommand {
            Some(SubcommandInfo { ty, optional, .. }) => {
                (quote! { #optional }, quote! { __rt::Some(<#ty as __rt::Subcommand>::RAW_INFO) })
            }
            None => (quote! { false }, quote! { __rt::None }),
        };

        let has_optional_named = self.0.named_fields.iter().any(|f| !f.required && !f.hide);

        let cmd_doc = CommandDoc(self.0.cmd_meta);
        let flatten_tys = self.0.flatten_tys();

        tokens.extend(quote! {
            &__rt::RawArgsInfo::new(
                #subcmd_opt,
                #has_optional_named,
                #subcmd_info,
                #cmd_doc,
                #descs,
                #fmt_fn,
                [
                    #(__rt::RawArgsInfoRef(
                        <<#flatten_tys as __rt::Args>::__State as __rt::ParserState>::RAW_ARGS_INFO
                    )),*
                ],
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
                    let value_ty = f.value_ty;
                    &quote_spanned! {e.span()=> __rt::assert_impl_display_for_help::<#value_ty>(#e) }
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
