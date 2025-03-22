#![feature(proc_macro_diagnostic)]
#![feature(proc_macro_span)]
mod derive;

use {
    proc_macro::TokenStream,
    proc_macro_error2::proc_macro_error,
    quote::{format_ident, quote},
    syn::{parse_macro_input, ItemFn},
};

/// Annotate the harness with a #[kanitool::<name>] with optional arguments.
macro_rules! kani_attribute {
    ($name:ident) => {
        #[proc_macro_attribute]
        pub fn $name(attr: TokenStream, item: TokenStream) -> TokenStream {
            let args = proc_macro2::TokenStream::from(attr);
            let fn_item = parse_macro_input!(item as ItemFn);
            let attribute = format_ident!("{}", stringify!($name));
            quote!(
                #[kanitool::#attribute(#args)]
                #fn_item
            ).into()
        }
    };
    ($name:ident, no_args) => {
        #[proc_macro_attribute]
        pub fn $name(attr: TokenStream, item: TokenStream) -> TokenStream {
            assert!(attr.is_empty(), "`#[kani::{}]` does not take any arguments currently", stringify!($name));
            let fn_item = parse_macro_input!(item as ItemFn);
            let attribute = format_ident!("{}", stringify!($name));
            quote!(
                #[kanitool::#attribute]
                #fn_item
            ).into()
        }
    };
}

#[proc_macro_error]
#[proc_macro_derive(Arbitrary, attributes(safety_constraint))]
pub fn derive_arbitrary(item: TokenStream) -> TokenStream {
    derive::expand_derive_arbitrary(item)
}

#[proc_macro_error]
#[proc_macro_derive(Invariant, attributes(safety_constraint))]
pub fn derive_invariant(item: TokenStream) -> TokenStream {
    derive::expand_derive_invariant(item)
}

#[proc_macro_error]
#[proc_macro_attribute]
pub fn proof(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let fn_item = parse_macro_input!(item as ItemFn);
    let attrs = fn_item.attrs;
    let vis = fn_item.vis;
    let sig = fn_item.sig;
    let body = fn_item.block;

    // Adds `#[kanitool::proof]` and other attributes
    quote!(
        #[allow(dead_code)]
        #[kanitool::proof]
        #(#attrs)*
        #vis #sig #body
    )
    .into()
}

kani_attribute!(should_panic, no_args);
kani_attribute!(recursion, no_args);
kani_attribute!(solver);
kani_attribute!(stub);
kani_attribute!(unstable);
kani_attribute!(unwind);
