use std::ops;

use proc_macro2::{Ident, Span, TokenStream};
use quote::{ToTokens, quote};
use syn::meta::ParseNestedMeta;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{Attribute, GenericArgument, LitBool, LitChar, LitStr, PathArguments, Type};
use syn::{Token, bracketed, token};

use crate::error::try_syn;
use crate::shared::ArgAttrs;

pub const TY_BOOL: &str = "bool";
pub const TY_OPTION: &str = "Option";
pub const TY_VEC: &str = "Vec";

/// `TyCtor<ArgTy>` => `ArgTy`. `ty_ctor` must be a single-identifier path.
///
/// This is matched literally and does NOT try to be smart, i.e.
/// it does not recognize absolute paths or unwrap parenthesis.
pub fn strip_ty_ctor<'i>(ty: &'i Type, ty_ctor: &str) -> Option<&'i Type> {
    if let Type::Path(syn::TypePath { qself: None, path }) = ty {
        if path.leading_colon.is_none() && path.segments.len() == 1 {
            let seg = &path.segments[0];
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

// Utility macros for attribute parsers.

trait OptionExt<T> {
    fn set_once(&mut self, span: Span, v: T);
}
impl<T> OptionExt<T> for Option<T> {
    fn set_once(&mut self, span: Span, v: T) {
        if self.is_none() {
            *self = Some(v);
        } else {
            emit_error!(span, "duplicated attribute");
        }
    }
}

trait BoolExt {
    fn parse_true(&mut self, meta: &ParseNestedMeta<'_>) -> syn::Result<()>;
}
impl BoolExt for bool {
    fn parse_true(&mut self, meta: &ParseNestedMeta<'_>) -> syn::Result<()> {
        let lit = meta.value()?.parse::<LitBool>()?;
        if !lit.value {
            emit_error!(lit, "only `true` is supported here");
        } else if *self {
            emit_error!(lit, "duplicated attribute");
        } else {
            *self = true;
        }
        Ok(())
    }
}

pub enum ArgOrCommand {
    Arg(Box<ArgMeta>),
    Command(ArgsCommandMeta),
}

impl ArgOrCommand {
    pub fn parse_attrs(attrs: &[Attribute]) -> ArgOrCommand {
        let mut doc = Doc::default();
        let mut arg = None::<Box<ArgMeta>>;
        let mut command = None;
        for attr in attrs {
            let path = attr.path();
            doc.extend_from_attr(attr);
            if path.is_ident("arg") {
                let arg = arg.get_or_insert_default();
                try_syn(attr.parse_nested_meta(|meta| arg.parse_update(&meta)));
            } else if path.is_ident("command") {
                if let Some(c) = try_syn(attr.parse_args::<ArgsCommandMeta>()) {
                    if command.is_some() {
                        emit_error!(path, "duplicated command(..)");
                    }
                    command = Some((path.span(), c));
                }
            }
        }

        doc.post_process();

        if let Some((span, c)) = command {
            if arg.is_some() {
                emit_error!(span, "command(..) conflicts with arg(..)");
            }
            Self::Command(c)
        } else {
            let mut arg = arg.unwrap_or_default();
            arg.doc = doc;
            Self::Arg(arg)
        }
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
    pub ignore_case: bool,

    // Unnamed argument behaviors.
    pub trailing_var_arg: bool,
    pub last: bool,
    // TODO: raw

    // Value behaviors.
    pub default_value: Option<syn::LitStr>,
    pub default_value_t: Option<Override<VerbatimExpr>>,
    pub value_delimiter: Option<syn::LitChar>,
    pub value_enum: bool,
    // TODO: num_args, value_hint
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

        if path.is_ident("long") {
            self.long.set_once(span, meta.input.parse()?);
        } else if path.is_ident("short") {
            self.short.set_once(span, meta.input.parse()?);
        } else if path.is_ident("alias") || path.is_ident("aliases") {
            self.alias.extend(meta.value()?.parse::<OneOrArray<LitStr>>()?);
        } else if path.is_ident("short_alias") || path.is_ident("short_aliases") {
            self.short_alias.extend(meta.value()?.parse::<OneOrArray<LitChar>>()?);
        } else if path.is_ident("value_name") {
            self.value_name.set_once(span, meta.value()?.parse::<LitStr>()?);
        } else if path.is_ident("require_equals") {
            self.require_equals.parse_true(meta)?;
        } else if path.is_ident("global") {
            self.global.parse_true(meta)?;
        } else if path.is_ident("allow_hyphen_values") {
            self.allow_hyphen_values.parse_true(meta)?;
        } else if path.is_ident("allow_negative_numbers") {
            self.allow_negative_numbers.parse_true(meta)?;
        } else if path.is_ident("trailing_var_arg") {
            self.trailing_var_arg.parse_true(meta)?;
        } else if path.is_ident("last") {
            self.last.parse_true(meta)?;
        } else if path.is_ident("default_value") {
            self.default_value.set_once(span, meta.value()?.parse()?);
        } else if path.is_ident("default_value_t") || path.is_ident("default_values_t") {
            self.default_value_t.set_once(span, meta.input.parse()?);
        } else if path.is_ident("use_value_delimiter") {
            let lit = meta.value()?.parse::<LitBool>()?;
            if !lit.value {
                emit_error!(lit, "only `true` is supported here");
            }
            self.value_delimiter.set_once(span, syn::LitChar::new(',', Span::call_site()));
        } else if path.is_ident("value_delimiter") {
            self.value_delimiter.set_once(span, meta.value()?.parse::<syn::LitChar>()?);
        } else if path.is_ident("value_enum") {
            // NB. This attribute is standalone without `=`.
            if self.value_enum {
                emit_error!(path, "duplicated attribute");
            }
            self.value_enum = true;
        } else if path.is_ident("ignore_case") {
            self.ignore_case.parse_true(meta)?;
        } else if path.is_ident("help") {
            self.help.set_once(span, meta.value()?.parse::<LitStr>()?);
        } else if path.is_ident("long_help") {
            self.long_help.set_once(span, meta.value()?.parse::<LitStr>()?);
        } else if path.is_ident("hide") {
            self.hide.parse_true(meta)?;
        } else if path.is_ident("required") {
            self.required.parse_true(meta)?;
        } else if path.is_ident("exclusive") {
            self.exclusive.parse_true(meta)?;
        } else if path.is_ident("requires") {
            self.requires.push(meta.value()?.parse()?);
        } else if path.is_ident("conflicts_with") {
            self.conflicts_with.push(meta.value()?.parse()?);
        } else if path.is_ident("conflicts_with_all") {
            self.conflicts_with.extend(meta.value()?.parse::<OneOrArray<FieldPath>>()?);
        } else {
            emit_error!(path, "unknown attribute");
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
    pub long_about: Option<Override<VerbatimExpr>>,
    pub after_long_help: Option<VerbatimExpr>,
    // TODO: bin_name, verbatim_doc_comment, next_display_order, next_help_heading, rename_all{,_env}
}

impl CommandMeta {
    pub fn parse_attrs_opt(attrs: &[Attribute]) -> Option<Box<Self>> {
        let mut doc = Doc::default();
        let mut this: Option<Box<Self>> = None;
        for attr in attrs {
            doc.extend_from_attr(attr);
            if attr.path().is_ident("command") {
                let this = this.get_or_insert_default();
                try_syn(attr.parse_nested_meta(|meta| this.parse_update(&meta)));
            } else if attr.path().is_ident("arg") {
                emit_error!(attr, "only `command(..)` is allowed in this location");
            }
        }
        doc.post_process();
        if !doc.0.is_empty() {
            this.get_or_insert_default().doc = doc;
        }
        this
    }

    fn parse_update(&mut self, meta: &ParseNestedMeta<'_>) -> syn::Result<()> {
        let path = &meta.path;
        let span = path.span();

        if path.is_ident("name") {
            self.name.set_once(span, meta.value()?.parse()?);
        } else if path.is_ident("version") {
            self.version.set_once(span, meta.input.parse()?);
        } else if path.is_ident("long_about") {
            self.long_about.set_once(span, meta.input.parse()?);
        } else if path.is_ident("after_long_help") {
            self.after_long_help.set_once(span, meta.value()?.parse()?);
        } else if path.is_ident("author") {
            meta.input.parse::<Override<VerbatimExpr>>()?;
            emit_error!(
                span,
                "`command(author)` is NOT supported. \
                It is useless without custom help template anyway."
            );
        } else if path.is_ident("about") {
            meta.input.parse::<Override<VerbatimExpr>>()?;
            emit_error!(
                span,
                "custom short-about `command(about)` is NOT supported yet. \
                It always use the first line of `command(long_about)` (or doc-comments). \
                Please use `command(long_about)` (or doc-comments) instead.",
            );
        } else if path.is_ident("after_help") {
            meta.input.parse::<Override<VerbatimExpr>>()?;
            emit_error!(
                span,
                "custom after-short-help `command(after_help)` is NOT supported yet. \
                It always use the first line of `command(after_long_help)`. \
                Please use `command(after_long_help)`  instead.",
            );
        } else if path.is_ident("term_width") || path.is_ident("max_term_width") {
            meta.value()?.parse::<syn::Expr>()?;
            emit_error!(
                span,
                "`command(term_width, max_term_width)` are intentionally NOT supported, \
                because line-wrapping's drawback outweighs its benefits.",
            );
        } else {
            emit_error!(span, "unknown attribute");
        }

        Ok(())
    }
}

/// Top-level `#[value]` for `derive(ValueEnum)` enum.
pub struct ValueEnumMeta {
    pub rename_all: Rename,
}

impl ValueEnumMeta {
    pub fn parse_attrs(attrs: &[Attribute]) -> Self {
        let mut rename_all = None;
        for attr in attrs {
            if !attr.path().is_ident("value") {
                continue;
            }
            try_syn(attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("rename_all") {
                    rename_all.set_once(meta.path.span(), meta.value()?.parse::<Rename>()?);
                } else {
                    emit_error!(meta.path, "unknown attribute");
                }
                Ok(())
            }));
        }
        Self { rename_all: rename_all.unwrap_or(Rename::KebabCase) }
    }
}

/// Variant `#[value]` for `derive(ValueEnum)` enum.
#[derive(Default)]
pub struct ValueVariantMeta {
    pub name: Option<String>,
    // TODO: skip, help
}

impl ValueVariantMeta {
    pub fn parse_attrs(attrs: &[Attribute]) -> Self {
        let mut this = Self::default();
        for attr in attrs {
            if !attr.path().is_ident("value") {
                continue;
            }
            try_syn(attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("name") {
                    this.name.set_once(meta.path.span(), meta.value()?.parse::<LitStr>()?.value());
                } else {
                    emit_error!(meta.path, "unknown attribute");
                }
                Ok(())
            }));
        }
        this
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

    fn extend_from_attr(&mut self, attr: &Attribute) {
        if !attr.path().is_ident("doc") {
            return;
        }
        let syn::Meta::NameValue(m) = &attr.meta else { return };
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
            emit_error!(m.value, "only literal doc comment is supported yet");
        }
    }
}

impl ToTokens for Doc {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.to_tokens(tokens);
    }
}

impl ToTokens for ArgAttrs {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let val = self.0;
        tokens.extend(quote! { __rt::ArgAttrs(#val) });
    }
}
