use proc_macro2::TokenStream;

pub(crate) struct QuoteWith<F>(pub F)
where
    F: Fn(&mut TokenStream);

impl<F> quote::ToTokens for QuoteWith<F>
where
    F: Fn(&mut TokenStream),
{
    fn to_tokens(&self, tokens: &mut TokenStream) {
        (self.0)(tokens)
    }
}

macro_rules! syn_error {
    ($msg:literal $(,$args:expr)* $(,)?) => {
        syn_error!(::proc_macro2::Span::call_site(), $msg $(,$args)*)
    };
    ($span:expr, $msg:literal $(,)?) => {
        ::syn::Error::new($span, $msg)
    };
    ($span:expr, $msg:literal $(,$args:expr)+ $(,)?) => {
        ::syn::Error::new($span, format!($msg $(,$args)*))
    };
}

macro_rules! new_token {
    ($span:expr, $keyword:ident) => {{
        type __Token = ::syn::Token![$keyword];
        __Token { span: $span }
    }};
    ($span:expr, $tt:tt) => {{
        type __Token = ::syn::Token![$tt];
        __Token { spans: [$span] }
    }};
    ($tt:tt) => {
        new_token!(::proc_macro2::Span::call_site(), $tt)
    };
}
