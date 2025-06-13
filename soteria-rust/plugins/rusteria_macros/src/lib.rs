#![feature(proc_macro_diagnostic)]
#![feature(proc_macro_span)]

use {
    proc_macro::TokenStream,
    quote::{format_ident, quote},
    syn::{parse_macro_input, ItemFn},
};

macro_rules! rusteria_attribute {
    ($name:ident) => {
        #[proc_macro_attribute]
        pub fn $name(attr: TokenStream, item: TokenStream) -> TokenStream {
            let args = proc_macro2::TokenStream::from(attr);
            let fn_item = parse_macro_input!(item as ItemFn);
            let attribute = format_ident!("{}", stringify!($name));
            quote!(
                #[rusteriatool::#attribute(#args)]
                #fn_item
            ).into()
        }
    };
    ($name:ident, no_args) => {
        #[proc_macro_attribute]
        pub fn $name(attr: TokenStream, item: TokenStream) -> TokenStream {
            assert!(attr.is_empty(), "`#[rusteria::{}]` does not take any arguments currently", stringify!($name));
            let fn_item = parse_macro_input!(item as ItemFn);
            let attribute = format_ident!("{}", stringify!($name));
            quote!(
                #[rusteriatool::#attribute]
                #fn_item
            ).into()
        }
    };
}

rusteria_attribute!(step_fuel);
rusteria_attribute!(branch_fuel);
rusteria_attribute!(test, no_args);
