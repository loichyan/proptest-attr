#[macro_use]
mod util;
mod proptest;

use proc_macro::TokenStream;

use self::proptest::MacroKind;

fn exec_proptest(kind: MacroKind, attr: TokenStream, item: TokenStream) -> TokenStream {
    proptest::expand(kind, attr, item)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro_attribute]
pub fn property_test(attr: TokenStream, item: TokenStream) -> TokenStream {
    exec_proptest(MacroKind::Default, attr, item)
}

#[proc_macro_attribute]
pub fn test(attr: TokenStream, item: TokenStream) -> TokenStream {
    exec_proptest(MacroKind::Test, attr, item)
}

#[proc_macro_attribute]
pub fn compose(attr: TokenStream, item: TokenStream) -> TokenStream {
    exec_proptest(MacroKind::Compose, attr, item)
}
