use std::collections::HashMap;
use std::num::NonZero;

use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, quote, quote_spanned};
use syn::spanned::Spanned;
use syn::{Data, DeriveInput, Fields, FieldsNamed, Ident, Lit, LitChar, LitStr};

use crate::common::{
    ArgOrCommand, ArgsCommandMeta, CommandMeta, Doc, FieldIdent, Override, TY_BOOL, TY_OPTION,
    TY_VEC, strip_ty_ctor, wrap_anon_item,
};
use crate::error::{Result, catch_errors};
use crate::shared::ArgAttrs;

/// This limitation exists because:
/// 1. We want to use a `u8` index inside `ArgAttrs`.
/// 2. Positional argument counter `ArgsFrame::next_positional` will end with
///    the number of positional arguments. We don't want to overflow it.
const MAX_FIELD_LEN: usize = (u8::MAX - 1) as usize;

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
            const __HAS_SUBCOMMAND: __rt::bool = false;
            const __INFO: &'static __rt::RawArgsInfo = __rt::RawArgsInfo::EMPTY_REF;
            extern "C" fn __parse(
                _: &mut __rt::Option<Self>,
                _: &mut dyn __rt::Frame,
                _: &mut dyn __rt::RunParser,
            ) -> __rt::Result<()> {
                __rt::unimplemented!()
            }
        }
    }
}

pub fn try_expand_for_named_struct(
    input: &DeriveInput,
    fields: &FieldsNamed,
) -> Result<TokenStream> {
    let ident = &input.ident;
    let cmd_meta = CommandMeta::parse_attrs_opt(&input.attrs);
    let struct_name = input.ident.to_token_stream();
    let parse = expand_args_parse(cmd_meta.as_deref(), struct_name, fields)?;
    let args_info = parse.args_info();
    let has_subcmd = parse.subcommand.is_some();

    Ok(quote! {
        #[automatically_derived]
        impl __rt::Args for #ident {
            const __HAS_SUBCOMMAND: __rt::bool = #has_subcmd;
            const __INFO: &'static __rt::RawArgsInfo = #args_info;
            extern "C" fn __parse(
                __out: &mut __rt::Option<Self>,
                __frame: &mut dyn __rt::Frame,
                __parser: &mut dyn __rt::RunParser,
            ) -> __rt::Result<()> {
                let __info = <Self as __rt::Args>::__INFO;
                #parse
            }
        }
    })
}

/// Generate the body of `Args::__parse`.
///
/// Referenced variables: `__out`, `__frame`, `__parser`, `__info`.
pub struct ArgsParse<'i> {
    pub output_ctor: TokenStream,

    /// This contains all "direct" argument fields, that is, everything
    /// excluding subcommand and flattened ones.
    /// They are sorted in this order:
    /// - Named arguments in definition order.
    /// - Positional arguments in definition order.
    /// - The variable argument, if any.
    /// - The tail argument, if any.
    direct_fields: Vec<FieldInfo<'i>>,

    named_field_cnt: usize,

    /// Indirect fields that needs delegation.
    flatten_fields: Vec<FlattenFieldInfo<'i>>,
    /// Subcommand is special.
    subcommand: Option<SubcommandInfo<'i>>,

    cmd_meta: Option<&'i CommandMeta>,
}

impl ArgsParse<'_> {
    pub fn args_info(&self) -> impl ToTokens {
        RawArgsInfo(self)
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

    // Arg configurable //
    /// Encoded names for matching. Empty for positional arguments.
    enc_names: Vec<String>,
    attrs: ArgAttrs,

    // Validations //
    required: bool,
    default_value: Option<DefaultValue>,
    exclusive: bool,
    dependencies: Vec<FieldIdent>,
    conflicts: Vec<FieldIdent>,

    /// Referenced field index. Will be set after assigning field index.
    dependencies_idx: Option<Vec<u8>>,
    conflicts_idx: Option<Vec<u8>>,

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
        // See also `palc::values::InferValueParser`.
        tokens.extend(quote_spanned! {ty.span()=>
            __rt::assert_auto_infer_value_parser_ok(
                (&&&__rt::PhantomData::<#ty>).__palc_infer_value_parser()
            )
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

    fn require_value(self) -> bool {
        match self {
            Self::Value | Self::Option | Self::Vec | Self::OptionVec => true,
            Self::Bool | Self::OptionOption => false,
        }
    }

    fn accepts_multiple_values(self) -> bool {
        match self {
            Self::Vec | Self::OptionVec => true,
            Self::Bool | Self::Value | Self::Option | Self::OptionOption => false,
        }
    }

    fn place_ty(self, value_ty: &syn::Type) -> TokenStream {
        match self {
            MagicKind::Bool => quote! { __rt::FlagPlace },
            MagicKind::Value | MagicKind::Option => quote! { __rt::SetValuePlace<#value_ty> },
            MagicKind::OptionOption => quote! { __rt::SetOptionalValuePlace<#value_ty> },
            MagicKind::Vec | MagicKind::OptionVec => quote! { __rt::VecPlace<#value_ty> },
        }
    }

    fn finalizer(self) -> TokenStream {
        match self {
            MagicKind::Bool | MagicKind::Vec | MagicKind::Value => {
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
    format!("--{s}")
}

fn encode_short_name(name: &LitChar) -> String {
    let c = name.value();
    if c == '-' || c == '=' || c.is_ascii_control() {
        emit_error!(name, "arg(short) name must NOT be '-', '=' or ASCII control characters");
    }
    c.into()
}

pub fn expand_args_parse<'i>(
    cmd_meta: Option<&'i CommandMeta>,
    output_ctor: TokenStream,
    input_fields: &'i syn::FieldsNamed,
) -> Result<ArgsParse<'i>> {
    let fields = &input_fields.named;

    if fields.len() > MAX_FIELD_LEN {
        abort!(input_fields, "only up to {MAX_FIELD_LEN} fields are supported");
    }

    let mut named_fields = Vec::with_capacity(fields.len());
    let mut positional_fields = Vec::with_capacity(fields.len());
    let mut var_arg_field = None::<FieldInfo>;
    let mut last_field = None::<FieldInfo>;
    let mut subcommand = None::<SubcommandInfo>;

    let mut flatten_fields = Vec::new();

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
                if let Some(prev) = &subcommand {
                    emit_error!(ident, "duplicated subcommand");
                    emit_error!(prev.ident, "previously defined here");
                } else {
                    subcommand = Some(SubcommandInfo { ident, ty, optional });
                }
                continue;
            }
            ArgOrCommand::Command(ArgsCommandMeta::Flatten) => {
                flatten_fields.push(FlattenFieldInfo { ident, ty: &field.ty });
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
                    "TODO: arg(value_delimiter) is not yet supported on positional arguments",
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
        if accept_hyphen != ArgAttrs::default() && (!kind.require_value() || arg.require_equals) {
            emit_error!(
                ident,
                "arg(allow_{{hyphen,negative}}_value) is incompatible with argument that \
                takes no value or has `requires_equals` set",
            );
        }

        let value_name = match &arg.value_name {
            Some(s) => s.value(),
            None => heck::AsShoutySnekCase(&ident_str).to_string(),
        };
        if value_name.contains(|ch: char| ch.is_ascii_control()) {
            emit_error!(ident, "arg(value_name) must NOT contain ASCII control characters");
        }

        if kind == MagicKind::OptionOption && !arg.require_equals {
            emit_error!(
                ident,
                "magic type `Option<Option<_>>` requires `requires_equals = true` to avoid ambiguity, \
                see <https://github.com/clap-rs/clap/issues/3030>",
            );
        }

        let mut attrs = ArgAttrs::delimiter(value_delimiter).union(accept_hyphen);
        attrs.set(ArgAttrs::REQUIRE_VALUE, kind.require_value());
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
                if kind == MagicKind::OptionOption {
                    write!(buf, "[=<{value_name}>]").unwrap();
                } else if kind.require_value() {
                    write!(buf, "{}<{}>", if arg.require_equals { '=' } else { ' ' }, value_name)
                        .unwrap();
                }
                buf
            };

            named_fields.push(FieldInfo {
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
                dependencies_idx: None,
                conflicts_idx: None,
                description,
                doc: arg.doc,
                hide: arg.hide,
            });
        } else {
            // Positional arguments.

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

            attrs.set(ArgAttrs::VAR_ARG, is_variable_num);
            attrs.set(ArgAttrs::LAST, arg.last);
            // arg(last) implies greedy.
            attrs.set(ArgAttrs::GREEDY, arg.trailing_var_arg | arg.last);

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
                dependencies_idx: None,
                conflicts_idx: None,
                description,
                doc: arg.doc,
                hide: arg.hide,
            };

            if arg.last {
                // Last argument(s).
                if !is_variable_num {
                    emit_error!(ident, "TODO: arg(last) only supports Vec-like types yet");
                }

                if let Some(prev) = last_field.replace(info) {
                    emit_error!(ident, "duplicated arg(last)");
                    emit_error!(prev.ident, "previously defined here");
                }
            } else if is_variable_num {
                // Variable length positional argument.
                if let Some(prev) = var_arg_field.replace(info) {
                    emit_error!(ident, "duplicated variable-length arguments");
                    emit_error!(prev.ident, "previously defined here");
                }
            } else {
                // Single positional argument.

                if let Some(prev) = &var_arg_field {
                    emit_error!(
                        ident,
                        "cannot have more positional arguments after a variable-length positional argument"
                    );
                    emit_error!(prev.ident, "previous variable-length argument");
                } else {
                    positional_fields.push(info);
                }
            }
        }
    }

    let named_field_cnt = named_fields.len();
    // The order matters! See `ArgsParse::direct_fields`.
    let mut direct_fields = named_fields
        .into_iter()
        .chain(positional_fields)
        .chain(var_arg_field)
        .chain(last_field)
        .collect::<Vec<_>>();

    // Assign fields indexes.
    for (f, idx) in direct_fields.iter_mut().zip(0u8..) {
        f.attrs.set(ArgAttrs::index(idx), true);
    }

    // Resolve field references for constraints.
    let ident_idx_map = direct_fields
        .iter()
        .map(|f| (f.ident.to_string(), f.attrs.get_index()))
        .collect::<HashMap<String, u8>>();
    let resolve_field_refs = |idents: &[FieldIdent]| {
        idents
            .iter()
            .filter_map(|ident| match ident_idx_map.get(&ident.0.to_string()) {
                Some(&idx) => Some(idx),
                _ => {
                    emit_error!(ident.0, "field not found");
                    None
                }
            })
            .collect::<Vec<_>>()
    };
    for f in &mut direct_fields {
        f.dependencies_idx = Some(resolve_field_refs(&f.dependencies));
        f.conflicts_idx = Some(resolve_field_refs(&f.conflicts));
    }

    if !flatten_fields.is_empty()
        && let Some(f) = direct_fields.iter().find(|f| f.exclusive)
    {
        emit_error!(
            f.ident,
            "TODO: arg(exclusive) is not supported on struct containing arg(flatten) yet",
        );
    }

    Ok(ArgsParse {
        output_ctor,
        direct_fields,
        named_field_cnt,
        flatten_fields,
        subcommand,
        cmd_meta,
    })
}

impl ToTokens for ArgsParse<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // Local variables.
        let (field_idents, place_tys, finalizers) = self
            .direct_fields
            .iter()
            .map(|FieldInfo { ident, value_ty, kind, .. }| {
                let fin = kind.finalizer();
                (*ident, kind.place_ty(value_ty), quote! { __rt::FieldState::#fin(&mut #ident) })
            })
            .chain(self.flatten_fields.iter().map(|FlattenFieldInfo { ident, ty }| {
                (*ident, quote! { __rt::Option<#ty> }, quote! { #ident.unwrap() })
            }))
            .chain(self.subcommand.as_ref().map(|SubcommandInfo { ident, ty, optional }| {
                let unwrap = (!*optional).then(|| quote! { .unwrap() });
                (*ident, quote! { __rt::Option<#ty> }, quote! { #ident #unwrap })
            }))
            .collect::<(Vec<_>, Vec<_>, Vec<_>)>();

        let field_array_iter =
            self.direct_fields.iter().map(|FieldInfo { ident, value_parser, .. }| {
                quote! { __rt::FieldState::place(&mut #ident, #value_parser) }
            });

        // Assertions to be forced to evaluate.
        let mut asserts = TokenStream::new();
        for ty in self.flatten_tys() {
            asserts.extend(quote_spanned! {ty.span()=>
                __rt::assert!(
                    <#ty as __rt::Args>::__INFO.positional_len() == 0,
                    "TODO: cannot arg(flatten) positional arguments yet",
                );
                __rt::assert!(
                    !<#ty as __rt::Args>::__HAS_SUBCOMMAND,
                    "cannot flatten an Args with subcommand",
                );
            });
        }
        for FieldInfo { value_ty, attrs, .. } in &self.direct_fields {
            if attrs.contains(ArgAttrs::MAKE_LOWERCASE) {
                asserts.extend(quote_spanned! {value_ty.span()=>
                    __rt::assert!(
                        <#value_ty as __rt::ValueEnum>::NO_UPPER_CASE,
                        "`arg(ignore_case)` only supports `ValueEnum` that contains no UPPERCASE variants"
                    );
                });
            }
        }
        if !asserts.is_empty() {
            // Force evaluation.
            asserts = quote! { const _: () = { #asserts }; };
        }

        let subcmd_param = match self.subcommand {
            Some(SubcommandInfo { ident, ty, .. }) => {
                quote! { __rt::Some(<#ty as __rt::Subcommand>::__erase(&mut #ident)) }
            }
            None => quote! { __rt::None },
        };
        let mut run_parser = quote! {
            __rt::RunParser::run(__parser, &mut __rt::ArgsFrame {
                fields: __fields,
                subcmd: #subcmd_param,
                info: __info,
                parent: __frame,
                next_positional: 0u8,
            })
        };
        // TODO: Add tests about the order of flattened Args.
        for FlattenFieldInfo { ident, ty } in &self.flatten_fields {
            run_parser = quote! {
                <#ty as __rt::Args>::__parse(&mut #ident, __frame, &mut |__frame: &mut __rt::ArgsFrame| {
                    #run_parser
                })
            };
        }

        let output_ctor = &self.output_ctor;
        let validate_finish = ValidateFinish(self);

        tokens.extend(quote! {
            #asserts

            #(let mut #field_idents = <#place_tys as __rt::Default>::default();)*
            let __fields: &mut [&mut dyn __rt::Parsable; _] = &mut [ #(#field_array_iter),* ];

            #run_parser?;

            #validate_finish

            *__out = __rt::Some(#output_ctor {
                #(#field_idents : #finalizers,)*
            });
            __rt::Ok(())
        });
    }
}

/// Validate non-empty-ness and constraints, and set default values.
///
/// This includes `conflicts_with`, `exclusive`, explicit and implicit `required`.
struct ValidateFinish<'i>(&'i ArgsParse<'i>);

impl ToTokens for ValidateFinish<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let def = self.0;

        // TODO: Can we compress this list?
        let isset_elems = def
            .direct_fields
            .iter()
            .map(|FieldInfo { ident, .. }| {
                quote! { __rt::FieldState::is_set(&#ident) }
            })
            // Include the subcommand for exclusiveness check.
            .chain(def.subcommand.as_ref().map(|SubcommandInfo { ident, .. }| {
                quote! { #ident.is_some() }
            }));
        let mut exclusive_idxs = def
            .direct_fields
            .iter()
            .filter_map(|f| f.exclusive.then_some(f.attrs.get_index()))
            .peekable();
        let mut dependency_groups = def
            .direct_fields
            .iter()
            .filter_map(|f| {
                let cur_idx = f.attrs.get_index();
                let idxs = f.dependencies_idx.as_ref().unwrap();
                (!idxs.is_empty()).then(|| quote! { &[#cur_idx, #(#idxs),*] })
            })
            .peekable();
        let mut conflict_groups = def
            .direct_fields
            .iter()
            .filter_map(|f| {
                let cur_idx = f.attrs.get_index();
                let idxs = f.conflicts_idx.as_ref().unwrap();
                (!idxs.is_empty()).then(|| quote! { &[#cur_idx, #(#idxs),*] })
            })
            .peekable();
        if exclusive_idxs.peek().is_some()
            || dependency_groups.peek().is_some()
            || conflict_groups.peek().is_some()
        {
            tokens.extend(quote! {
                __rt::validate_constraints(
                    __info,
                    &[#(#isset_elems),*],
                    &[#(#exclusive_idxs),*],
                    &[#(#dependency_groups),*],
                    &[#(#conflict_groups),*],
                )?;
            });
        }

        // Check required fields inline to hint the optimizer the `unwrap` below
        // will not fail.
        // This is necessary to eliminate the mass dropping codegen
        // which almost doubles or triples the function size.
        for f in &def.direct_fields {
            let ident = f.ident;
            let idx = f.attrs.get_index();
            if f.required {
                tokens.extend(quote! {
                    if !__rt::FieldState::is_set(&#ident) {
                        return __rt::error_missing_arg(__info, #idx);
                    }
                });
            }
        }

        // Missing subcommand produces a special error message.
        // This also serves as a hint to the optimizer.
        if let Some(SubcommandInfo { ident, .. }) = def.subcommand.as_ref().filter(|s| !s.optional)
        {
            tokens.extend(quote! {
                if #ident.is_none() {
                    return __rt::error_missing_subcmd();
                }
            });
        }

        // Set default values after checks.
        for FieldInfo { ident, default_value, value_parser, .. } in &def.direct_fields {
            let Some(default_value) = default_value else { continue };
            tokens.extend(match default_value {
                DefaultValue::ValueExpr(e) => quote! {
                    __rt::FieldState::set_default(&mut #ident, || #e);
                },
                DefaultValue::ParseStr(s) => quote! {
                    __rt::FieldState::set_default_parse(&mut #ident, #s, #value_parser);
                },
            });
        }

        // 1. Suppress bulk-drop on `__out` which is unnecessary.
        // 2. Assert all flattened arguments are already initialized.
        let flatten_vars = self.0.flatten_fields.iter().map(|f| f.ident);
        tokens.extend(quote! {
            if __out.is_some() #(|| #flatten_vars.is_none())* {
                __rt::unreachable_nounwind();
            }
        });
    }
}

/// Generates the reflection constant of type `RawArgsInfo`.
struct RawArgsInfo<'a>(&'a ArgsParse<'a>);

impl ToTokens for RawArgsInfo<'_> {
    // See format in `RawArgInfo`.
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let help_display = HelpDisplay { parse: Some(self.0), cmd_meta: self.0.cmd_meta };

        let descs =
            self.0.direct_fields.iter().flat_map(|f| [&f.description, "\0"]).collect::<String>();
        let (named_fields, positional_fields) =
            self.0.direct_fields.split_at(self.0.named_field_cnt);
        let positional_attrs = positional_fields.iter().map(|f| f.attrs);
        let mut named_map = named_fields
            .iter()
            .flat_map(|&FieldInfo { ref enc_names, attrs, .. }| {
                enc_names.iter().map(move |s| (&**s, attrs))
            })
            .collect::<Vec<_>>();
        named_map.sort_by_key(|(name, _)| *name);
        let named_map_from = named_map.iter().map(|(name, _)| *name);
        let named_map_to = named_map.iter().map(|(_, attr)| *attr);

        let (subcmd_opt, subcmd_info) = match &self.0.subcommand {
            Some(SubcommandInfo { ty, optional, .. }) => {
                (quote! { #optional }, quote! { __rt::Some(<#ty as __rt::Subcommand>::__INFO) })
            }
            None => (quote! { false }, quote! { __rt::None }),
        };

        let has_optional_named = named_fields.iter().any(|f| !f.required && !f.hide);

        let flatten_tys = self.0.flatten_tys();

        tokens.extend(quote! {
            &__rt::RawArgsInfo::new(
                #descs,
                &[#((#named_map_from, #named_map_to)),*],
                &[#(#positional_attrs),*],
                #subcmd_info,

                #subcmd_opt,
                #has_optional_named,
                #help_display,
                &[#(<#flatten_tys as __rt::Args>::__INFO),*],
            )
        });
    }
}

/// Generate a `&'static dyn Display` for help message related formatting.
pub struct HelpDisplay<'i> {
    pub parse: Option<&'i ArgsParse<'i>>,
    pub cmd_meta: Option<&'i CommandMeta>,
}

impl ToTokens for HelpDisplay<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let mut arms = TokenStream::new();
        let mut push_arm = |i: usize, tts: &dyn ToTokens| {
            arms.extend(quote! { #i => __f.write_fmt(#tts), });
        };

        if let Some(parse) = self.parse {
            // Generate help string formatter.
            let mut usage_named = FormatArgsBuilder::default();
            let mut usage_positional = FormatArgsBuilder::default();
            let mut help_named = FormatArgsBuilder::default();
            let mut help_positional = FormatArgsBuilder::default();

            for (i, f) in parse.direct_fields.iter().enumerate() {
                if f.hide {
                    continue;
                }
                let (help, usage) = if i < parse.named_field_cnt {
                    (&mut help_named, &mut usage_named)
                } else {
                    (&mut help_positional, &mut usage_positional)
                };

                let is_last = f.attrs.contains(ArgAttrs::LAST);
                if is_last {
                    usage.template.push_str(if f.required { " --" } else { " [--" });
                }

                // Variable positional fields are always visible, no matter if it is
                // optional (`[ARGS]...`) or required (`<ARGS>...`).
                if f.required || f.attrs.contains(ArgAttrs::VAR_ARG) || is_last {
                    usage.maybe_push_usage_for(f);
                }
                help.maybe_push_help_for(f);

                if is_last && !f.required {
                    usage.template.push(']');
                }
            }

            for ty in parse.flatten_tys() {
                // FIXME: Should we `let` bind these very long expressions?
                let help = quote! { <#ty as __rt::Args>::__INFO.help() };
                // Magic integers are documented at `palc::refl::RawArgsInfo::help`.
                usage_named.push_custom_arg("{:.0}", &help);
                usage_positional.push_custom_arg("{:.1}", &help);
                help_named.push_custom_arg("{:.2}", &help);
                help_positional.push_custom_arg("{:.3}", &help);
            }

            // The integer mapping is documented at `palc::refl::RawArgsInfo::help`.
            if !usage_named.is_empty() {
                push_arm(0, &usage_named);
            }
            if !usage_positional.is_empty() {
                push_arm(1, &usage_positional);
            }
            if !help_named.is_empty() {
                push_arm(2, &help_named);
            }
            if !help_positional.is_empty() {
                push_arm(3, &help_positional);
            }
        }

        if let Some(meta) = self.cmd_meta {
            let long_about: &dyn ToTokens = match &meta.long_about {
                Some(Override::Explicit(e)) => e,
                Some(Override::Inherit) => &quote! { env!("CARGO_PKG_DESCRIPTION") },
                None => &meta.doc,
            };
            push_arm(4, &quote! { __rt::format_args!("{}", #long_about) });

            if let Some(expr) = &meta.after_long_help {
                push_arm(5, &quote! { __rt::format_args!("{}", #expr) });
            }
        }

        if arms.is_empty() {
            tokens.extend(quote! { &"" });
            return;
        }

        tokens.extend(quote! {{
            struct Help;
            impl __rt::fmt::Display for Help {
                fn fmt(&self, __f: &mut __rt::fmt::Formatter<'_>) -> __rt::fmt::Result {
                    // There can be a lot of default values. Put this here rather than inlining below.
                    use __rt::InferDisplayDefaultValue as _;

                    // NB: Do not extract `write_fmt` call outside the match!
                    // There are several bugs that we may run into even after 1.89.
                    // Rustc bug: <https://github.com/rust-lang/rust/issues/145422>
                    // Clippy bug: <https://github.com/rust-lang/rust-clippy/issues/16736>
                    match __f.precision().unwrap_or(0) {
                        #arms
                        _ => __rt::Ok(())
                    }
                }
            }
            &Help
        }});
    }
}

#[derive(Default)]
struct FormatArgsBuilder {
    template: String,
    args: TokenStream,
}

impl FormatArgsBuilder {
    fn is_empty(&self) -> bool {
        self.template.is_empty()
    }

    fn push_arg(&mut self, tts: impl ToTokens) {
        self.push_custom_arg("{}", tts);
    }

    fn push_custom_arg(&mut self, template: &str, tts: impl ToTokens) {
        self.template.push_str(template);
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
            let value_ty = f.value_ty;
            let arg: &dyn ToTokens = match default {
                DefaultValue::ParseStr(s) => s,
                // See also: `palc::values::InferDisplayDefaultValue`
                DefaultValue::ValueExpr(e) => {
                    // Optimization: For literal defaults that are known
                    // to be `Display`, commonly integers, pass them directly
                    // without any wrappers. So they can be inlined by rustc
                    // `format_args` optimization.
                    // The default type checking will still happen inside
                    // `finish`, so skipping this check does not matter.
                    if let Ok(
                        Lit::Bool(_) | Lit::Byte(_) | Lit::Char(_) | Lit::Float(_) | Lit::Int(_),
                    ) = syn::parse2::<Lit>(e.clone())
                    {
                        e
                    } else {
                        &quote_spanned! {e.span()=>
                            __rt::assert_impl_display_default_value(
                                __rt::PhantomData::<#value_ty>.__palc_infer_display(#e)
                            )
                        }
                    }
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
