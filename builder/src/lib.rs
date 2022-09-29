use proc_macro::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use syn::DeriveInput;

use core::option::Option::{self, None, Some};
#[allow(unused)]
use core::result::Result::{self, Err, Ok};
#[allow(unused)]
use std::boxed::Box;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = syn::parse::<DeriveInput>(input).unwrap();
    // println!("{:#?}", ast);

    let name = &ast.ident;
    let bname = format!("{}Builder", name);
    let bident = syn::Ident::new(&bname, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!()
    };

    let optionized = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if builder_of(&f).is_some() || ty_inner_type("Option", &ty).is_some() {
            let expr = quote! {
                #name : #ty
            };
            return expr;
        }

        quote! {
            #name: std::option::Option<#ty>
        }
    });

    let methods = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;
        let set_method = if let Some(inner_ty) = ty_inner_type("Option", ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        } else if builder_of(&f).is_some() {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        };

        match extend_methods(&f) {
            Some((true, extend_method)) => extend_method.into(),
            Some((false, extend_method)) => quote! {
                #set_method
                #extend_method
            },
            None => set_method,
        }
    });

    let build_empty = fields.iter().map(|f| {
        let name = &f.ident;

        if builder_of(f).is_some() {
            quote! {
                #name: std::vec::Vec::new()
            }
        } else {
            quote! {
                #name: std::option::Option::None
            }
        }
    });

    let build_fields = fields.iter().map(|f| {
        let name = &f.ident;
        let ty = &f.ty;

        if ty_inner_type("Option", &ty).is_some() || builder_of(f).is_some() {
            let expr = quote! {
                 #name: self.#name.clone()
            };
            return expr;
        };
        quote! {
            #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
        }
    });

    // result
    let expanded = quote! {
        pub struct #bident {
            #(#optionized,)*
        }

        impl #bident {
            #(#methods)*

            pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
                    #(#build_fields,)*
                })
            }
        }

        impl #name {
            pub fn builder() -> #bident {
                #bident {
                    #(#build_empty,)*
                }
            }
        }
    };
    expanded.into()
}

fn ty_inner_type<'a>(wrapper: &'_ str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != wrapper {
            return None;
        }
        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }
            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref t) = inner_ty {
                return Some(t);
            }
        }
    }
    None
}

fn builder_of(f: &syn::Field) -> Option<&syn::Attribute> {
    for attr in &f.attrs {
        if attr.path.segments.len() == 1 && attr.path.segments[0].ident == "builder" {
            if let TokenTree::Group(_g) = attr.tokens.clone().into_iter().next().unwrap() {
                return Some(attr);
            }
        }
    }
    None
}

fn extend_methods(f: &syn::Field) -> Option<(bool, proc_macro2::TokenStream)> {
    let name = &f.ident;
    let g = builder_of(&f)?;
    
    fn mk_err<T: quote::ToTokens>(t: T) -> Option<(bool, proc_macro2::TokenStream)> {
        Some((
            false,
            syn::Error::new_spanned(t, "expected `builder(each = \"...\")`").to_compile_error(),
        ))
    }

    let meta = match g.parse_meta() {
        Ok(syn::Meta::List(mut nvs)) => {
            assert_eq!(nvs.path.get_ident().unwrap(), "builder");
            if nvs.nested.len() !=1 {
                return mk_err(nvs);
            }

            match nvs.nested.pop().unwrap().into_value() {
                syn::NestedMeta::Meta(syn::Meta::NameValue(nv)) => {
                    if nv.path.get_ident().unwrap() != "each"{
                        return mk_err(nvs);
                    }
                    nv
                },
                meta => {return mk_err(meta);}
            }
        }
        Ok(meta) => return mk_err(meta),
        Err(e) => {
            return Some((false, e.to_compile_error()));
        }
    };

    match meta.lit {
        syn::Lit::Str(s) => {
            let arg = syn::Ident::new(&s.value(), s.span());
            let inner_ty = ty_inner_type("Vec", &f.ty).unwrap();
            let method = {
                quote! {
                    pub fn #arg(&mut self, #arg: #inner_ty) -> &mut Self {
                        self.#name.push(#arg);
                        self
                    }
                }
            };
            Some((Some(&arg) == name.as_ref(), method))
        }
        lit => panic!("expected string, found {:#?}", lit),
    }
}
