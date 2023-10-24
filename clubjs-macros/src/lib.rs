use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::quote;
use syn::{
    braced,
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    LitStr, Token,
};

struct PreIntern {
    symbols: Punctuated<Symbol, Token![,]>,
    keywords: Punctuated<Symbol, Token![,]>,
}

mod kw {
    syn::custom_keyword!(keywords);
}

impl Parse for PreIntern {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let keywords_content;
        let keywords = if input.parse::<kw::keywords>().is_ok() {
            braced!(keywords_content in input);
            keywords_content.parse_terminated(Symbol::parse, Token![,])?
        } else {
            Default::default()
        };
        let symbols = input.parse_terminated(Symbol::parse, Token![,])?;
        Ok(Self { symbols, keywords })
    }
}

struct Symbol(Ident, Option<LitStr>);

impl Parse for Symbol {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        let content = if input.parse::<Token![:]>().is_ok() {
            Some(input.parse()?)
        } else {
            None
        };
        Ok(Self(name, content))
    }
}

#[proc_macro]
pub fn preintern(input: TokenStream) -> TokenStream {
    let PreIntern { symbols, keywords } = parse_macro_input!(input);
    let mut symbol_decls = vec![];
    let mut strings = vec![];

    for (i, Symbol(name, value)) in keywords.iter().chain(symbols.iter()).enumerate() {
        let value = value
            .as_ref()
            .map_or_else(|| name.to_string(), |s| s.value());
        symbol_decls.push(quote! {
            pub const #name: Symbol = Symbol(#i);
        });
        strings.push(quote! { #value });
    }
    let len = symbol_decls.len();
    let n_keywords = keywords.len();
    quote! {
        #(#symbol_decls)*
        pub static SYMBOL_VALUES: [&str; #len] = [
            #(#strings),*
        ];
        // first n symbols are keywords
        pub static N_KEYWORDS: usize = #n_keywords;
    }
    .into()
}
