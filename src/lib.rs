#[macro_use]
mod util;
mod proptest;

use proc_macro::TokenStream;
use syn::parse_macro_input;

#[proc_macro_attribute]
pub fn test(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr = parse_macro_input!(attr as _);
    let item = parse_macro_input!(item as _);
    proptest::expand(attr, item, true)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_attribute]
pub fn property_test(attr: TokenStream, item: TokenStream) -> TokenStream {
    let attr = parse_macro_input!(attr as _);
    let item = parse_macro_input!(item as _);
    proptest::expand(attr, item, false)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}
