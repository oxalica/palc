use std::num::NonZero;
use std::ops;

use proc_macro2::{Ident, Span, TokenStream};
use quote::{ToTokens, quote};
use syn::meta::ParseNestedMeta;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{Attribute, GenericArgument, LitBool, LitChar, LitStr, PathArguments, Type};
use syn::{Token, bracketed, token};

use crate::shared::{AcceptHyphen, ArgAttrs};

pub const TY_BOOL: &str = "bool";
pub const TY_U8: &str = "u8";
pub const TY_OPTION: &str = "Option";
pub const TY_VEC: &str = "Vec";

#[derive(Default)]
pub(crate) struct ErrorCollector {
    err: Option<syn::Error>,
    defused: bool,
}

impl ErrorCollector {
    pub fn collect<T>(&mut self, ret: syn::Result<T>) -> Option<T> {
        match ret {
            Ok(v) => Some(v),
            Err(e) => {
                self.push(e);
                None
            }
        }
    }

    pub fn push(&mut self, e: syn::Error) {
        match &mut self.err {
            Some(prev) => prev.combine(e),
            p @ None => *p = Some(e),
        }
    }

    pub fn finish(mut self) -> syn::Result<()> {
        self.defused = true;
        match self.err.take() {
            None => Ok(()),
            Some(e) => Err(e),
        }
    }

    pub fn finish_then<T>(self, v: T) -> syn::Result<T> {
        self.finish().map(|()| v)
    }
}

impl Drop for ErrorCollector {
    fn drop(&mut self) {
        if !self.defused && !std::thread::panicking() {
            assert!(self.err.is_none(), "error not finished");
        }
    }
}

/// `TyCtor<ArgTy>` => `ArgTy`. `ty_ctor` must be a single-identifier path.
pub fn strip_ty_ctor<'i>(mut ty: &'i Type, ty_ctor: &str) -> Option<&'i Type> {
    let ty = loop {
        match ty {
            Type::Group(inner) => ty = &inner.elem,
            Type::Paren(inner) => ty = &inner.elem,
            Type::Path(inner) => break inner,
            _ => return None,
        }
    };
    if ty.path.leading_colon.is_none() && ty.path.segments.len() == 1 {
        let seg = &ty.path.segments[0];
        if seg.ident == ty_ctor {
            if let PathArguments::AngleBracketed(args) = &seg.arguments {
                if args.args.len() == 1 {
                    if let GenericArgument::Type(arg_ty) = &args.args[0] {
                        return Some(arg_ty);
                    }
                }
            }
        }
    }
    None
}

pub fn wrap_anon_item(tts: impl ToTokens) -> TokenStream {
    quote! {
        const _: () = {
            use ::palc::__private as __rt;
            #tts
        };
    }
}

pub enum ArgTyKind<'a> {
    Bool,
    U8,
    Option(&'a Type),
    OptionOption(&'a Type),
    Vec(&'a Type),
    OptionVec(&'a Type),
    // TODO: VecVec, OptionVecVec
    Other,
}

impl ArgTyKind<'_> {
    pub fn of(ty: &syn::Type) -> ArgTyKind<'_> {
        match ty {
            Type::Path(p) if p.qself.is_none() => {
                if p.path.is_ident(TY_BOOL) {
                    return ArgTyKind::Bool;
                }
                if p.path.is_ident(TY_U8) {
                    return ArgTyKind::U8;
                }
            }
            _ => {}
        }
        if let Some(subty) = strip_ty_ctor(ty, TY_OPTION) {
            if let Some(subty) = strip_ty_ctor(subty, TY_OPTION) {
                return ArgTyKind::OptionOption(subty);
            }
            if let Some(subty) = strip_ty_ctor(subty, TY_VEC) {
                return ArgTyKind::OptionVec(subty);
            }
            return ArgTyKind::Option(subty);
        }
        if let Some(subty) = strip_ty_ctor(ty, TY_VEC) {
            return ArgTyKind::Vec(subty);
        }
        ArgTyKind::Other
    }
}

pub enum ArgOrCommand {
    Arg(Box<ArgMeta>),
    Command(ArgsCommandMeta),
}

/// Parse `arg(..)` and `command(..)` in `derive(Args)`-like struct or variants.
pub fn parse_args_attrs(fields: &syn::FieldsNamed) -> syn::Result<Vec<ArgOrCommand>> {
    let mut errs = ErrorCollector::default();
    let attrs = fields
        .named
        .iter()
        .filter_map(|f| errs.collect(ArgOrCommand::parse_attrs(&f.attrs)))
        .collect();
    errs.finish_then(attrs)
}

impl ArgOrCommand {
    fn parse_attrs(attrs: &[Attribute]) -> syn::Result<ArgOrCommand> {
        let mut errs = ErrorCollector::default();
        let mut doc = Doc::default();
        let mut arg = None::<Box<ArgMeta>>;
        let mut command = None;
        for attr in attrs {
            let path = attr.path();
            doc.extend_from_attr(attr)?;
            if path.is_ident("arg") {
                let arg = arg.get_or_insert_default();
                errs.collect(attr.parse_nested_meta(|meta| arg.parse_update(&meta)));
            } else if path.is_ident("command") {
                if let Some(c) = errs.collect(attr.parse_args::<ArgsCommandMeta>()) {
                    if command.is_some() {
                        errs.push(syn::Error::new(path.span(), "duplicated command(..)"));
                    }
                    command = Some((path.span(), c));
                }
            }
        }
        doc.post_process();
        let ret = if let Some((span, c)) = command {
            if arg.is_some() {
                errs.push(syn::Error::new(span, "command(..) conflicts with arg(..)"));
            }
            Self::Command(c)
        } else {
            let mut arg = arg.unwrap_or_default();
            arg.doc = doc;
            Self::Arg(arg)
        };
        errs.finish_then(ret)
    }
}

#[derive(Default)]
pub struct ArgMeta {
    pub doc: Doc,

    // Names.
    pub long: Option<Override<LitStr>>,
    pub short: Option<Override<LitChar>>,
    pub alias: OneOrArray<LitStr>,
    pub short_alias: OneOrArray<LitChar>,
    pub value_name: Option<LitStr>,
    // TODO: {,visible_}{,short_}alias{,es}, value_names

    // Named argument behaviors.
    pub require_equals: bool,
    pub global: bool,
    pub allow_hyphen_values: bool,
    pub allow_negative_numbers: bool,

    // Unnamed argument behaviors.
    pub trailing_var_arg: bool,
    pub last: bool,
    // TODO: raw

    // Value behaviors.
    pub default_value: Option<syn::LitStr>,
    pub default_value_t: Option<Override<VerbatimExpr>>,
    pub value_delimiter: Option<syn::LitChar>,
    // TODO: num_args,
    // index, action, value_terminator, default_missing_value*, env

    // Help & completion.
    pub help: Option<LitStr>,
    pub long_help: Option<LitStr>,
    pub hide: bool,
    // TODO: add, hide_*, next_line_help, help_heading, display_order

    // Validation.
    pub required: bool,
    pub exclusive: bool,
    pub requires: Vec<FieldPath>,
    pub conflicts_with: Vec<FieldPath>,
    // TODO: default_value_if{,s}, required_unless_present*, required_if*,
    // conflicts_with*, overrides_with*
}

impl ArgMeta {
    pub fn is_named(&self) -> bool {
        self.long.is_some() || self.short.is_some()
    }

    fn parse_update(&mut self, meta: &ParseNestedMeta<'_>) -> syn::Result<()> {
        let path = &meta.path;
        let span = path.span();
        macro_rules! ensure {
            ($cond:expr, $msg:expr) => {
                if !$cond {
                    return Err(syn::Error::new(span, $msg));
                }
            };
        }
        macro_rules! check_dup {
            ($name:ident) => {
                ensure!(self.$name.is_none(), concat!("duplicated arg(", stringify!($name), ")"));
            };
        }
        macro_rules! check_true {
            () => {
                let v = meta.value()?.parse::<LitBool>()?;
                if !v.value {
                    return Err(syn::Error::new(v.span, "must be true"));
                }
            };
        }

        if path.is_ident("long") {
            check_dup!(long);
            self.long = Some(meta.input.parse()?);
        } else if path.is_ident("short") {
            check_dup!(short);
            self.short = Some(meta.input.parse()?);
        } else if path.is_ident("alias") || path.is_ident("aliases") {
            self.alias.extend(meta.value()?.parse::<OneOrArray<LitStr>>()?);
        } else if path.is_ident("short_alias") || path.is_ident("short_aliases") {
            self.short_alias.extend(meta.value()?.parse::<OneOrArray<LitChar>>()?);
        } else if path.is_ident("value_name") {
            check_dup!(value_name);
            self.value_name = Some(meta.value()?.parse::<LitStr>()?);
        } else if path.is_ident("require_equals") {
            check_true!();
            self.require_equals = true;
        } else if path.is_ident("global") {
            check_true!();
            self.global = true;
        } else if path.is_ident("allow_hyphen_values") {
            check_true!();
            self.allow_hyphen_values = true;
        } else if path.is_ident("allow_negative_numbers") {
            check_true!();
            self.allow_negative_numbers = true;
        } else if path.is_ident("trailing_var_arg") {
            check_true!();
            self.trailing_var_arg = true;
        } else if path.is_ident("last") {
            check_true!();
            self.last = true;
        } else if path.is_ident("default_value") {
            check_dup!(default_value);
            self.default_value = Some(meta.value()?.parse()?);
        } else if path.is_ident("default_value_t") || path.is_ident("default_values_t") {
            parse_unique_override(&mut self.default_value_t, meta)?;
        } else if path.is_ident("use_value_delimiter") {
            check_true!();
            check_dup!(value_delimiter);
            self.value_delimiter = Some(syn::LitChar::new(',', Span::call_site()));
        } else if path.is_ident("value_delimiter") {
            check_dup!(value_delimiter);
            self.value_delimiter = Some(meta.value()?.parse::<syn::LitChar>()?);
        } else if path.is_ident("value_enum") {
            // Ignored.
        } else if path.is_ident("help") {
            check_dup!(help);
            self.help = Some(meta.value()?.parse::<LitStr>()?);
        } else if path.is_ident("long_help") {
            check_dup!(long_help);
            self.help = Some(meta.value()?.parse::<LitStr>()?);
        } else if path.is_ident("hide") {
            check_true!();
            self.hide = true;
        } else if path.is_ident("required") {
            check_true!();
            self.required = true;
        } else if path.is_ident("exclusive") {
            check_true!();
            self.exclusive = true;
        } else if path.is_ident("requires") {
            self.requires.push(meta.value()?.parse()?);
        } else if path.is_ident("conflicts_with") {
            self.conflicts_with.push(meta.value()?.parse()?);
        } else if path.is_ident("conflicts_with_all") {
            self.conflicts_with.extend(meta.value()?.parse::<OneOrArray<FieldPath>>()?);
        } else {
            ensure!(false, "unknown attribute");
        }
        Ok(())
    }
}

/// The inner `command(..)` on fields of `derive(Parser, Args, Subcommand)`.
pub enum ArgsCommandMeta {
    Subcommand,
    Flatten,
}

impl Parse for ArgsCommandMeta {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<syn::Ident>()?;
        Ok(if ident == "subcommand" {
            Self::Subcommand
        } else if ident == "flatten" {
            Self::Flatten
        } else {
            return Err(syn::Error::new(ident.span(), "must be either 'subcommand' or 'flatten'"));
        })
    }
}

/// `command(..)` on the struct of `derive(Parser)` or enum variants of `derive(Subcommand)`.
#[derive(Default)]
pub struct CommandMeta {
    pub doc: Doc,

    pub name: Option<LitStr>,
    pub version: Option<Override<VerbatimExpr>>,
    pub author: Option<Override<VerbatimExpr>>,
    pub about: Option<Override<VerbatimExpr>>,
    pub long_about: Option<Override<VerbatimExpr>>,
    pub after_help: Option<VerbatimExpr>,
    pub after_long_help: Option<VerbatimExpr>,
    // TODO: verbatim_doc_comment, next_display_order, next_help_heading, rename_all{,_env}
}

impl CommandMeta {
    pub fn parse_attrs(attrs: &[Attribute]) -> syn::Result<Box<Self>> {
        let mut errs = ErrorCollector::default();
        let mut doc = Doc::default();
        let mut this = <Box<Self>>::default();
        for attr in attrs {
            doc.extend_from_attr(attr)?;
            if attr.path().is_ident("command") {
                errs.collect(attr.parse_nested_meta(|meta| this.parse_update(&meta)));
            } else if attr.path().is_ident("arg") {
                errs.push(syn::Error::new(
                    attr.span(),
                    "arg(..) is not supported here, do you mean command(..)?",
                ));
            }
        }
        doc.post_process();
        this.doc = doc;
        errs.finish_then(this)
    }

    fn parse_update(&mut self, meta: &ParseNestedMeta<'_>) -> syn::Result<()> {
        let path = &meta.path;
        let span = path.span();

        macro_rules! check_dup {
            ($name:ident) => {
                if self.$name.is_some() {
                    return Err(syn::Error::new(
                        span,
                        concat!("duplicated arg(", stringify!($name), ")"),
                    ));
                }
            };
        }

        if path.is_ident("name") {
            check_dup!(name);
            self.name = Some(meta.value()?.parse()?);
        } else if path.is_ident("version") {
            parse_unique_override(&mut self.version, meta)?;
        } else if path.is_ident("author") {
            parse_unique_override(&mut self.author, meta)?;
        } else if path.is_ident("about") {
            parse_unique_override(&mut self.about, meta)?;
        } else if path.is_ident("long_about") {
            parse_unique_override(&mut self.long_about, meta)?;
        } else if path.is_ident("after_help") {
            check_dup!(after_help);
            self.after_help = Some(meta.value()?.parse()?);
        } else if path.is_ident("after_long_help") {
            check_dup!(after_long_help);
            self.after_long_help = Some(meta.value()?.parse()?);
        } else if path.is_ident("term_width") || path.is_ident("max_term_width") {
            return Err(syn::Error::new(
                span,
                "command({max_,}term_width) are intentionally NOT supported, \
                because line-wrapping's drawback outweighs its benefits.",
            ));
        } else {
            return Err(syn::Error::new(path.span(), "unknown attribute"));
        }

        Ok(())
    }
}

/// Top-level `#[value]` for `derive(ValueEnum)` enum.
pub struct ValueEnumMeta {
    pub rename_all: Rename,
}

impl ValueEnumMeta {
    pub fn parse_attrs(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut errs = ErrorCollector::default();
        let mut rename_all = None;
        for attr in attrs {
            if !attr.path().is_ident("value") {
                continue;
            }
            let ret = attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("rename_all") {
                    let value = meta.value()?.parse::<Rename>()?;
                    if rename_all.replace(value).is_some() {
                        errs.push(syn::Error::new(
                            meta.path.span(),
                            "duplicated `value(rename_all)`",
                        ));
                    }
                } else {
                    errs.push(syn::Error::new(meta.path.span(), "unknown attribute"));
                }
                Ok(())
            });
            errs.collect(ret);
        }
        errs.finish_then(Self { rename_all: rename_all.unwrap_or(Rename::KebabCase) })
    }
}

/// Variant `#[value]` for `derive(ValueEnum)` enum.
#[derive(Default)]
pub struct ValueVariantMeta {
    pub name: Option<String>,
    // TODO: skip, help
}

impl ValueVariantMeta {
    pub fn parse_attrs(attrs: &[Attribute]) -> syn::Result<Self> {
        let mut errs = ErrorCollector::default();
        let mut this = Self::default();
        for attr in attrs {
            if !attr.path().is_ident("value") {
                continue;
            }
            let ret = attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("name") {
                    if this.name.is_some() {
                        errs.push(syn::Error::new(meta.path.span(), "duplicated attribute"));
                    }
                    this.name = Some(meta.value()?.parse::<LitStr>()?.value());
                } else {
                    errs.push(syn::Error::new(meta.path.span(), "unknown attribute"));
                }
                Ok(())
            });
            errs.collect(ret);
        }
        errs.finish_then(this)
    }
}

// Follows <https://docs.rs/clap/4.5.41/clap/_derive/index.html#valueenum-attributes>
#[derive(Clone, Copy)]
pub enum Rename {
    CamelCase,
    KebabCase,
    PascalCase,
    ScreamingSnakeCase,
    SnakeCase,
    Lower,
    Upper,
    Verbatim,
}

impl Parse for Rename {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let s = input.parse::<LitStr>()?;
        Ok(match &*s.value() {
            "camelCase" => Self::CamelCase,
            "kebab-case" => Self::KebabCase,
            "PascalCase" => Self::PascalCase,
            "SCREAMING_SNAKE_CASE" => Self::ScreamingSnakeCase,
            "snake_case" => Self::SnakeCase,
            "lower" => Self::Lower,
            "UPPER" => Self::Upper,
            "verbatim" => Self::Verbatim,
            _ => return Err(syn::Error::new(s.span(), "unknown case conversion")),
        })
    }
}

impl Rename {
    pub fn rename(self, s: String) -> String {
        #[allow(clippy::wildcard_imports)]
        use heck::*;

        match self {
            Self::CamelCase => s.to_lower_camel_case(),
            Self::KebabCase => s.to_kebab_case(),
            Self::PascalCase => s.to_pascal_case(),
            Self::ScreamingSnakeCase => s.to_shouty_snake_case(),
            Self::SnakeCase => s.to_snake_case(),
            Self::Lower => s.to_lowercase(),
            Self::Upper => s.to_uppercase(),
            Self::Verbatim => s,
        }
    }
}

pub struct OneOrArray<T>(pub Vec<T>);

impl<T> Default for OneOrArray<T> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<T> ops::Deref for OneOrArray<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> IntoIterator for OneOrArray<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<T> Extend<T> for OneOrArray<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        self.0.extend(iter);
    }
}

impl<T: Parse> Parse for OneOrArray<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(if input.peek(token::Bracket) {
            let inner;
            bracketed!(inner in input);
            inner.parse_terminated(T::parse, Token![,])?.into_iter().collect()
        } else {
            vec![input.parse::<T>()?]
        }))
    }
}

pub enum Override<T> {
    Inherit,
    Explicit(T),
}

impl<T: Parse> Parse for Override<T> {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            Self::Explicit(input.parse()?)
        } else {
            Self::Inherit
        })
    }
}

fn parse_unique_override(
    place: &mut Option<Override<impl Parse>>,
    meta: &ParseNestedMeta<'_>,
) -> syn::Result<()> {
    if place.is_some() {
        return Err(syn::Error::new(meta.path.span(), "duplicated attribute"));
    }
    *place = Some(meta.input.parse()?);
    Ok(())
}

/// `"IDENT"` or `("." IDENT)+`.
pub struct FieldPath(pub Vec<Ident>);

impl ops::Deref for FieldPath {
    type Target = [Ident];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Parse for FieldPath {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut path = Vec::new();
        if input.peek(LitStr) {
            path.push(input.parse::<LitStr>()?.parse::<Ident>()?);
        } else {
            while input.peek(Token![.]) {
                input.parse::<Token![.]>()?;
                path.push(input.parse::<Ident>()?);
            }
            if path.is_empty() {
                return Err(input.error(r#"expecting "field" or .field"#));
            }
        }
        Ok(Self(path))
    }
}

impl ToTokens for FieldPath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for ident in &self.0 {
            tokens.extend(quote! { . });
            ident.to_tokens(tokens);
        }
    }
}

pub struct VerbatimExpr(TokenStream);

impl Parse for VerbatimExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(input.parse::<syn::Expr>()?.to_token_stream()))
    }
}

impl From<VerbatimExpr> for TokenStream {
    fn from(e: VerbatimExpr) -> Self {
        e.0
    }
}

impl ToTokens for VerbatimExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }

    fn into_token_stream(self) -> TokenStream
    where
        Self: Sized,
    {
        self.0
    }
}

/// Collect doc-comments into a single string.
///
/// Paragraph (consecutive doc-comments without blank lines) are joined with space. In the result,
/// the first line is the summary, and each of rest lines corresponds to a paragraph.
///
/// See `src/refl.rs` for runtime usage that depends on this.
#[derive(Default, PartialEq)]
pub struct Doc(pub String);

impl Doc {
    fn post_process(&mut self) {
        let len = self.0.trim_ascii_end().len();
        self.0.truncate(len);
    }

    fn extend_from_attr(&mut self, attr: &Attribute) -> syn::Result<()> {
        if attr.path().is_ident("doc") {
            if let syn::Meta::NameValue(m) = &attr.meta {
                if let syn::Expr::Lit(syn::ExprLit { lit: syn::Lit::Str(s), .. }) = &m.value {
                    let s = s.value();
                    let s = s.trim_ascii();
                    if s.is_empty() {
                        if !self.0.ends_with('\n') {
                            self.0.push('\n');
                        }
                    } else {
                        if !self.0.is_empty() && !self.0.ends_with('\n') {
                            self.0.push(' ');
                        }
                        self.0.push_str(s);
                    }
                } else {
                    return Err(syn::Error::new(
                        m.value.span(),
                        "only literal doc comment is supported yet",
                    ));
                }
            }
        }
        Ok(())
    }

    pub fn summary(&self) -> &str {
        self.0.split_once('\n').unwrap_or((&self.0, "")).0
    }
}

impl ToTokens for Doc {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}

impl ToTokens for ArgAttrs {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if *self == Self::default() {
            tokens.extend(quote! { __rt::ArgAttrs::new() });
            return;
        }

        let Self { num_values, require_eq, accept_hyphen, delimiter, global, required, index } =
            self;
        let delimiter_u8 = delimiter.map_or(0, NonZero::get);
        tokens.extend(quote! {
            __rt::ArgAttrs {
                num_values: #num_values,
                require_eq: #require_eq,
                accept_hyphen: #accept_hyphen,
                delimiter: __rt::NonZero::new(#delimiter_u8),
                global: #global,
                required: #required,
                index: #index,
            }
        });
    }
}

impl ToTokens for AcceptHyphen {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        tokens.extend(match self {
            AcceptHyphen::No => quote! { __rt::AcceptHyphen::No },
            AcceptHyphen::NegativeNumber => quote! { __rt::AcceptHyphen::NegativeNumber },
            AcceptHyphen::Yes => quote! { __rt::AcceptHyphen::Yes },
        });
    }
}
