use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Parser};
use syn::punctuated::Punctuated;
use syn::{Expr, FnArg, Ident, ItemFn, Pat, ReturnType, Signature, Token, Type};

#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) enum MacroKind {
    Default,
    Test,
    Compose,
}

pub(crate) fn expand(
    kind: MacroKind,
    attr: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> syn::Result<TokenStream> {
    let ProptestArg { config, params } =
        Parser::parse(|input: ParseStream| parse_attr(kind, input), attr)?;
    let ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = &syn::parse(item)?;
    check_signature(kind, sig)?;

    let Signature { ident, .. } = sig;
    if kind == MacroKind::Compose {
        let Signature {
            constness,
            asyncness,
            unsafety,
            abi,
            inputs,
            output,
            ..
        } = sig;
        let modifiers = quote!(#constness #asyncness #unsafety #abi);
        Ok(quote!(::proptest::prop_compose! {
            #(#attrs)*
            #vis [#modifiers] fn #ident(#inputs) (#(#params,)*) #output #block
        }))
    } else {
        let config_attr = config.as_ref().map(|v| quote!(#![proptest_config(#v)]));
        let test_attr = if kind == MacroKind::Test {
            Some(quote!(#[test]))
        } else {
            None
        };
        Ok(quote!(::proptest::proptest! {
            #config_attr #(#attrs)* #test_attr
            #vis fn #ident(#(#params,)*) #block
        }))
    }
}

fn parse_attr(kind: MacroKind, input: ParseStream) -> syn::Result<ProptestArg> {
    let mut config = None;
    let mut params = vec![];
    loop {
        if input.is_empty() {
            break;
        }
        if input.peek2(Token![=]) || input.peek2(syn::token::Paren) {
            // parse an argument with value
            let key = input.parse::<Ident>()?;
            if kind != MacroKind::Compose && key == "config" {
                if config.is_some() {
                    fail!(key.span(), "duplicate `config` argument");
                }
                config = Some(input.parse()?);
            } else {
                fail!(key.span(), "unknown argument: {}", key);
            }
        } else {
            // or a strategy parameter
            params.push(input.parse()?);
        }
        input.parse::<Option<Token![,]>>()?;
    }

    Ok(ProptestArg { config, params })
}

fn check_signature(kind: MacroKind, sig: &Signature) -> syn::Result<()> {
    let span = sig.ident.span();
    if sig.generics.lt_token.is_some()
        || sig.generics.where_clause.is_some()
        || sig.variadic.is_some()
    {
        fail!(span, "generics or variadic argument is not allowed");
    }

    match kind {
        MacroKind::Compose if matches!(sig.output, ReturnType::Default) => {
            for arg in sig.inputs.iter() {
                if let FnArg::Receiver(r) = arg {
                    fail!(r.self_token.span, "receiver is not allowed");
                }
            }
            fail!(span, "return type is required");
        },
        MacroKind::Default | MacroKind::Test
            if sig.abi.is_some()
                || sig.asyncness.is_some()
                || sig.constness.is_some()
                || sig.unsafety.is_some()
                || !sig.inputs.is_empty()
                || !matches!(sig.output, ReturnType::Default) =>
        {
            fail!(span, "modifiers, arguments or return type is not allowed");
        },
        _ => {},
    }

    Ok(())
}

struct ProptestArg {
    pub config: Option<ProptestConfig>,
    pub params: Vec<Param>,
}

#[allow(dead_code)]
enum ProptestConfig {
    Fields {
        paren_token: syn::token::Paren,
        fields: Punctuated<ConfigField, Token![,]>,
    },
    Expr {
        eq_token: Token![=],
        value: Expr,
    },
}

impl ToTokens for ProptestConfig {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Fields { fields, .. } => {
                let fields = fields.iter();
                quote!(proptest::test_runner::Config {
                    #(#fields,)*
                    ..::core::default::Default::default()
                })
                .to_tokens(tokens);
            },
            Self::Expr { value, .. } => value.to_tokens(tokens),
        }
    }
}

impl Parse for ProptestConfig {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Paren) {
            // syntax: `#[proptest(config(field1=value1, field2=value2))]`
            let content;
            Ok(ProptestConfig::Fields {
                paren_token: syn::parenthesized!(content in input),
                fields: content.parse_terminated(ConfigField::parse, Token![,])?,
            })
        } else if input.peek(Token![=]) {
            // compatible with old syntax
            Ok(ProptestConfig::Expr {
                eq_token: input.parse::<Token![=]>()?,
                value: input.parse()?,
            })
        } else {
            fail!(input.span(), "expected `(...)` or `= ...`");
        }
    }
}

struct ConfigField {
    pub ident: Ident,
    pub eq_token: Token![=],
    pub value: Expr,
}

impl Parse for ConfigField {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            ident: input.parse()?,
            eq_token: input.parse()?,
            value: input.parse()?,
        })
    }
}

impl ToTokens for ConfigField {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.ident.to_tokens(tokens);
        syn::token::Colon {
            spans: self.eq_token.spans,
        }
        .to_tokens(tokens);
        self.value.to_tokens(tokens);
    }
}

enum Param {
    Expr {
        pat: Pat,
        in_token: Token![in],
        strategy: Expr,
    },
    Any {
        pat: Pat,
        colon_token: Token![:],
        ty: Type,
    },
}

impl Parse for Param {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let pat = Pat::parse_single(input)?;
        if let Some(in_token) = input.parse::<Option<Token![in]>>()? {
            Ok(Self::Expr {
                pat,
                in_token,
                strategy: input.parse()?,
            })
        } else if let Some(colon_token) = input.parse::<Option<Token![:]>>()? {
            Ok(Self::Any {
                pat,
                colon_token,
                ty: input.parse()?,
            })
        } else {
            fail!(input.span(), "expected `in ...` or `: ...`");
        }
    }
}

impl ToTokens for Param {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Expr {
                pat,
                in_token,
                strategy,
            } => {
                pat.to_tokens(tokens);
                in_token.to_tokens(tokens);
                strategy.to_tokens(tokens);
            },
            Self::Any {
                pat,
                colon_token,
                ty,
            } => {
                pat.to_tokens(tokens);
                colon_token.to_tokens(tokens);
                ty.to_tokens(tokens);
            },
        }
    }
}
