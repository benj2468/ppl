use heck::{CamelCase, SnakeCase};
use proc_macro::{TokenStream, TokenTree};
use proc_macro2::Ident;
use quote::{quote, ToTokens};
use std::{
    collections::hash_map::RandomState,
    hash::{BuildHasher, Hash, Hasher},
};
use syn::{parse_quote, BinOp, ExprBinary};

#[proc_macro_attribute]
pub fn time_it(attr: TokenStream, input: TokenStream) -> TokenStream {
    let item = syn::parse(input).unwrap();

    if let syn::Item::Fn(syn::ItemFn {
        attrs,
        block,
        sig,
        vis,
    }) = item
    {
        let func_name = &sig.ident;
        let name_str = func_name.to_string();
        let start_time = Ident::new(
            &format!("{}_start_instant", func_name.to_string().to_snake_case()),
            func_name.span(),
        );

        let write = if let Some(TokenTree::Literal(lit)) = attr.into_iter().next() {
            let attribute = lit.to_string();
            let split = attribute.split('"').into_iter().collect::<Vec<_>>();
            let file_name = split.get(1).unwrap();

            quote! {
                use std::io::Write;
                let mut file = std::fs::OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&#file_name)
                    .expect(&format!("Could not open/create file: {}", #file_name));
                file.write_all(format!("[{}]: {:?}", #name_str, std::time::Instant::now().duration_since(#start_time)).as_bytes()).expect("Error writing duration log to file");
            }
            // quote! {}
        } else {
            quote! {
                println!("Duration [{}]: {:?}", #name_str, std::time::Instant::now().duration_since(#start_time));
            }
        };

        TokenStream::from(quote! {
            #(#attrs)*
            #vis #sig {
                let #start_time = std::time::Instant::now();

                let res = #block;

                #write

                res
            }
        })
    } else {
        unimplemented!("Can only be applied to functions")
    }
}

#[proc_macro_attribute]
pub fn cached(attr: TokenStream, input: TokenStream) -> TokenStream {
    let item = syn::parse(input.clone()).unwrap();

    let file_name = if let Some(TokenTree::Literal(lit)) = attr.into_iter().next() {
        Some(lit.to_string())
    } else {
        None
    };

    if let syn::Item::Fn(syn::ItemFn {
        attrs,
        block,
        sig,
        vis,
    }) = item
    {
        let s = RandomState::new();
        let mut hasher = s.build_hasher();

        input.to_string().hash(&mut hasher);

        let hash = hasher.finish();

        let func_name = &sig.ident;
        let func_str = func_name.to_string();
        let cacher_file =
            file_name.unwrap_or_else(|| format!("./.{}_cacher.json", &func_str.to_snake_case()));
        let cache_struct = Ident::new(
            &format!("{}Cacher", func_name.to_string().to_camel_case()),
            func_name.span(),
        );
        let entry_struct = Ident::new(
            &format!("{}Entry", func_name.to_string().to_camel_case()),
            func_name.span(),
        );

        let (inputs, input_args): (Vec<_>, Vec<_>) = sig
            .inputs
            .iter()
            .filter_map(|arg| {
                if let syn::FnArg::Typed(a) = arg {
                    Some((&a.ty, &a.pat))
                } else {
                    None
                }
            })
            .unzip();

        let output = &sig.output;
        if let syn::ReturnType::Type(_, return_ty) = output {
            TokenStream::from(quote! {
                #[derive(serde::Serialize, serde::Deserialize)]
                pub struct #entry_struct {
                    inputs: (#(#inputs,)*),
                    output: #return_ty
                }
                #[derive(serde::Serialize, serde::Deserialize)]
                pub struct #cache_struct {
                    hash: u64,
                    cache: Vec<#entry_struct>
                }

                impl std::default::Default for #cache_struct {
                    fn default() -> Self {
                        Self {
                            hash: #hash,
                            cache: vec![]
                        }
                    }
                }

                impl #cache_struct {
                    fn init() -> Self {
                        let s = std::fs::read(&#cacher_file)
                            .map(|data| serde_json::from_slice::<Self>(&data).unwrap())
                            .unwrap_or_default();

                        if s.hash != #hash {
                            Self::default()
                        } else {
                            s
                        }
                    }

                    fn write(&self) {
                        std::fs::write(&#cacher_file, serde_json::to_vec(self).unwrap()).unwrap();
                    }
                }

                #(#attrs)*
                #vis #sig {
                    let mut cacher = #cache_struct::init();
                    if let Some(res) = cacher.cache.iter().find(|e| e.inputs == (#(#input_args.clone(),)*)) {
                        println!("FETCHING FROM CACHE: {}", #cacher_file);
                        res.output.clone()
                    } else {
                        let entry = #entry_struct {
                            inputs: (#(#input_args.clone(),)*),
                            output: #block
                        };
                        let res = entry.output.clone();
                        cacher.cache.push(entry);
                        cacher.write();
                        res
                    }
                }
            })
        } else {
            input
        }
    } else {
        unimplemented!("Can only be applied to functions")
    }
}

fn fix_operation_generator(p: u32, expr: &syn::Expr) -> syn::Expr {
    let expr = expr.clone();
    match expr {
        syn::Expr::Binary(mut a) => match a.op {
            BinOp::Eq(_)
            | BinOp::Ge(_)
            | BinOp::Gt(_)
            | BinOp::Le(_)
            | BinOp::Lt(_)
            | BinOp::Ne(_)
            | BinOp::Or(_)
            | BinOp::Rem(_)
            | BinOp::RemEq(_)
            | BinOp::Shl(_)
            | BinOp::ShlEq(_)
            | BinOp::Shr(_)
            | BinOp::ShrEq(_) => {
                a.left = Box::new(fix_operation_generator(p, a.left.as_ref()));
                a.right = Box::new(fix_operation_generator(p, a.right.as_ref()));

                syn::Expr::Binary(a)
            }
            _ => parse_quote!((#a) % #p),
        },
        syn::Expr::Array(mut a) => {
            a.elems = a
                .elems
                .into_iter()
                .map(|e| fix_operation_generator(p, &e))
                .collect();

            syn::Expr::Array(a)
        }
        syn::Expr::Assign(mut a) => {
            a.right = Box::new(fix_operation_generator(p, &a.right));
            syn::Expr::Assign(a)
        }
        syn::Expr::Block(mut b) => {
            b.block.stmts = b
                .block
                .stmts
                .into_iter()
                .map(|stmt| fix_operation_generator_stmt(p, stmt))
                .collect();
            syn::Expr::Block(b)
        }
        syn::Expr::Box(mut b) => {
            b.expr = Box::new(fix_operation_generator(p, &b.expr));
            syn::Expr::Box(b)
        }
        syn::Expr::Call(mut c) => {
            c.args = c
                .args
                .into_iter()
                .map(|e| fix_operation_generator(p, &e))
                .collect();
            syn::Expr::Call(c)
        }
        syn::Expr::Cast(mut c) => {
            c.expr = Box::new(fix_operation_generator(p, &c.expr));
            syn::Expr::Cast(c)
        }
        syn::Expr::Closure(mut c) => {
            c.body = Box::new(fix_operation_generator(p, &c.body));
            syn::Expr::Closure(c)
        }
        syn::Expr::ForLoop(mut l) => {
            l.expr = Box::new(fix_operation_generator(p, &l.expr));
            l.body.stmts = l
                .body
                .stmts
                .into_iter()
                .map(|stmt| fix_operation_generator_stmt(p, stmt))
                .collect();
            syn::Expr::ForLoop(l)
        }
        syn::Expr::If(mut i) => {
            i.cond = Box::new(fix_operation_generator(p, &i.cond));
            i.then_branch.stmts = i
                .then_branch
                .stmts
                .into_iter()
                .map(|stmt| fix_operation_generator_stmt(p, stmt))
                .collect();
            i.else_branch = i
                .else_branch
                .map(|(a, e)| (a, Box::new(fix_operation_generator(p, &e))));

            syn::Expr::If(i)
        }
        syn::Expr::Index(mut i) => {
            i.index = Box::new(fix_operation_generator(p, &i.index));
            syn::Expr::Index(i)
        }
        syn::Expr::Let(mut l) => {
            l.expr = Box::new(fix_operation_generator(p, &l.expr));
            syn::Expr::Let(l)
        }
        syn::Expr::Loop(mut l) => {
            l.body.stmts = l
                .body
                .stmts
                .into_iter()
                .map(|stmt| fix_operation_generator_stmt(p, stmt))
                .collect();
            syn::Expr::Loop(l)
        }
        syn::Expr::Match(mut m) => {
            m.expr = Box::new(fix_operation_generator(p, &m.expr));
            m.arms = m
                .arms
                .into_iter()
                .map(|mut arm| {
                    arm.body = Box::new(fix_operation_generator(p, &arm.body));
                    arm
                })
                .collect();
            syn::Expr::Match(m)
        }
        syn::Expr::MethodCall(mut m) => {
            m.args = m
                .args
                .into_iter()
                .map(|e| fix_operation_generator(p, &e))
                .collect();
            m.receiver = Box::new(fix_operation_generator(p, &m.receiver));

            syn::Expr::MethodCall(m)
        }
        syn::Expr::Paren(mut e) => {
            e.expr = Box::new(fix_operation_generator(p, &e.expr));
            syn::Expr::Paren(e)
        }
        syn::Expr::Range(mut r) => {
            r.from = r.from.map(|e| Box::new(fix_operation_generator(p, &e)));
            r.to = r.to.map(|e| Box::new(fix_operation_generator(p, &e)));
            syn::Expr::Range(r)
        }
        syn::Expr::Try(mut t) => {
            t.expr = Box::new(fix_operation_generator(p, &t.expr));
            syn::Expr::Try(t)
        }
        syn::Expr::TryBlock(mut t) => {
            t.block.stmts = t
                .block
                .stmts
                .into_iter()
                .map(|stmt| fix_operation_generator_stmt(p, stmt))
                .collect();
            syn::Expr::TryBlock(t)
        }
        syn::Expr::Tuple(mut t) => {
            t.elems = t
                .elems
                .into_iter()
                .map(|e| fix_operation_generator(p, &e))
                .collect();

            syn::Expr::Tuple(t)
        }
        syn::Expr::Unary(u) => {
            parse_quote!((#u) % p)
        }
        syn::Expr::While(mut w) => {
            w.cond = Box::new(fix_operation_generator(p, &w.cond));
            w.body.stmts = w
                .body
                .stmts
                .into_iter()
                .map(|stmt| fix_operation_generator_stmt(p, stmt))
                .collect();
            syn::Expr::While(w)
        }
        syn::Expr::Yield(mut y) => {
            y.expr = y.expr.map(|e| Box::new(fix_operation_generator(p, &e)));
            syn::Expr::Yield(y)
        }
        r => r,
    }
}

fn fix_operation_generator_stmt(order: u32, stmt: syn::Stmt) -> syn::Stmt {
    match stmt {
        syn::Stmt::Local(mut l) => {
            let needs_mod = l.attrs.iter().any(|a| {
                a.path
                    .segments
                    .iter()
                    .last()
                    .map(|seg| seg.ident == "field")
                    .unwrap_or_default()
            });

            l.attrs = vec![];

            l.init = l.init.map(|(a, b)| {
                (
                    a,
                    Box::new({
                        let fixed = fix_operation_generator(order, b.as_ref());
                        if needs_mod {
                            parse_quote! {
                                (#fixed) % #order
                            }
                        } else {
                            fixed
                        }
                    }),
                )
            });
            syn::Stmt::Local(l)
        }
        syn::Stmt::Expr(e) => syn::Stmt::Expr(fix_operation_generator(order, &e)),
        syn::Stmt::Semi(e, s) => syn::Stmt::Semi(fix_operation_generator(order, &e), s),
        _ => unimplemented!("Item is not implemented"),
    }
}

#[proc_macro_attribute]
pub fn in_prime_field(attr: TokenStream, input: TokenStream) -> TokenStream {
    let item = syn::parse(input).unwrap();
    let attr = attr.into_iter().next().expect("Field Must be Specified");

    let order: u32 = if let TokenTree::Literal(lit) = attr {
        lit.to_string().parse().unwrap()
    } else {
        unimplemented!("Order must be an integer >= 0")
    };

    if !primes::is_prime(order as u64) {
        panic!(
            "Order MUST be prime, or else calculations are not in a finite field: {}",
            order
        );
    }

    if let syn::Item::Fn(syn::ItemFn {
        attrs,
        mut block,
        sig,
        vis,
    }) = item
    {
        block.stmts = block
            .clone()
            .stmts
            .into_iter()
            .map(|stmt| fix_operation_generator_stmt(order, stmt))
            .collect();

        TokenStream::from(quote! {
            #(#attrs)*
            #vis #sig #block
        })
    } else {
        unimplemented!("Can only be applied to functions")
    }
}
