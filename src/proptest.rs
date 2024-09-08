use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{
    Expr, FnArg, Ident, ItemFn, Pat, PatType, ReturnType, Signature, Token, Type, TypeMacro,
};

use crate::util::QuoteWith;

pub(crate) fn expand(
    attr: ProptestAttr,
    item: ItemFn,
    with_test_attr: bool,
) -> syn::Result<TokenStream> {
    let ProptestAttr { config, mut params } = attr;
    let ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = &item;
    let ident = &sig.ident;
    check_signature(sig)?;

    let mut param_types = vec![];
    for arg in sig.inputs.iter() {
        if let Some(param) = parse_magic_macro(arg)? {
            params.push(param);
        } else {
            param_types.push(arg);
        }
    }

    let config_attr = config
        .as_ref()
        .map(ProptestConfig::to_expr)
        .map(|v| QuoteWith(move |tokens| quote!(#![proptest_config(#v)]).to_tokens(tokens)));
    let test_attr = if with_test_attr {
        Some(quote!(#[test]))
    } else {
        None
    };
    Ok(quote!(::proptest::proptest! {
        #config_attr #(#attrs)* #test_attr
        #vis fn #ident(#(#params,)* #(#param_types,)*) #block
    }))
}

fn parse_magic_macro(arg: &FnArg) -> syn::Result<Option<Param>> {
    if let FnArg::Typed(PatType { pat, ty, .. }) = arg {
        if let Type::Macro(TypeMacro { mac }) = &**ty {
            if let Some(i) = mac.path.get_ident().filter(|&i| i == "of") {
                return Ok(Some(Param {
                    pat: Clone::clone(&*pat),
                    // tell Rust to treat `of` as an `in` keyword
                    in_token: syn::token::In { span: i.span() },
                    strategy: syn::parse2(mac.tokens.clone())?,
                }));
            }
        }
    }
    Ok(None)
}

fn check_signature(sig: &Signature) -> syn::Result<()> {
    if sig.constness.is_some()
        || sig.asyncness.is_some()
        || sig.unsafety.is_some()
        || sig.abi.is_some()
        || sig.generics.lt_token.is_some()
        || sig.generics.where_clause.is_some()
        || sig.variadic.is_some()
        || !matches!(sig.output, ReturnType::Default)
    {
        Err(syn_error!(
            sig.ident.span(),
            "`const`, `async`, `unsafe`, `extern`, generics, variadic argument or return type is \
             not allowed"
        ))
    } else {
        Ok(())
    }
}

pub(crate) struct ProptestAttr {
    pub config: Option<ProptestConfig>,
    pub params: Vec<Param>,
}

impl Parse for ProptestAttr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut config = None;
        let mut params = vec![];
        loop {
            if input.is_empty() {
                break;
            }
            if input.peek2(Token![in]) {
                // parse a strategy parameter
                params.push(input.parse()?);
            } else {
                // or an argument with value
                let key = input.parse::<Ident>()?;
                if key == "config" {
                    if config.is_some() {
                        return Err(syn_error!(key.span(), "duplicate `config` argument"));
                    }
                    let val = if input.peek(syn::token::Paren) {
                        // syntax: `#[proptest(config(field1=value1, field2=value2))]`
                        let content;
                        syn::parenthesized!(content in input);
                        content
                            .parse_terminated(ConfigField::parse, Token![,])
                            .map(ProptestConfig::Fields)?
                    } else if input.parse::<Option<Token![=]>>()?.is_some() {
                        // compatible with old syntax
                        input.parse::<Expr>().map(ProptestConfig::Expr)?
                    } else {
                        return Err(syn_error!(key.span(), "expected `= ...` or `(...)`"));
                    };
                    config = Some(val);
                } else {
                    return Err(syn_error!(key.span(), "unknown argument: {}", key));
                }
            }
            input.parse::<Option<Token![,]>>()?;
        }

        Ok(Self { config, params })
    }
}

pub(crate) enum ProptestConfig {
    Fields(Punctuated<ConfigField, Token![,]>),
    Expr(Expr),
}

impl ProptestConfig {
    pub fn to_expr(&self) -> impl '_ + ToTokens {
        QuoteWith(move |tokens| match self {
            Self::Fields(fields) => {
                let fields = fields.iter();
                quote!(proptest::test_runner::Config {
                    #(#fields,)*
                    ..::core::default::Default::default()
                })
                .to_tokens(tokens);
            }
            Self::Expr(e) => e.to_tokens(tokens),
        })
    }
}

pub(crate) struct ConfigField {
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
        new_token!(self.eq_token.span, :).to_tokens(tokens);
        self.value.to_tokens(tokens);
    }
}

pub(crate) struct Param {
    pub pat: Pat,
    pub in_token: Token![in],
    pub strategy: Expr,
}

impl Parse for Param {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            pat: Pat::parse_single(input)?,
            in_token: input.parse()?,
            strategy: input.parse()?,
        })
    }
}

impl ToTokens for Param {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.pat.to_tokens(tokens);
        self.in_token.to_tokens(tokens);
        self.strategy.to_tokens(tokens);
    }
}
