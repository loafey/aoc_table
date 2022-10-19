use itertools::Itertools;
use proc_macro::TokenStream as TSOld;
use proc_macro2::{TokenStream, TokenTree};

#[proc_macro]
pub fn gen_ffi(item: TSOld) -> TSOld {
    let item: TokenStream = item.into();
    let mut org = "#".to_owned();
    let mut iters = item.into_iter();

    while let Some(n) = iters.next() {
        if let TokenTree::Punct(p) = n {
            if p.as_char() == ',' {
                break;
            }
        } else {
            let string = format!("{n}");

            org.push_str(&format!("{n} "));

            if string.ends_with(')') {
                org.push_str("-> *");
            }
        }
    }
    let mut types = Vec::new();
    while let Some(n) = iters.next() {
        if let TokenTree::Ident(i) = n {
            types.push(i.to_string());
        }
    }

    let combs = types
        .iter()
        .map(|c| org.replace("TYPE1", &c).replace("TYPE2", &c))
        .collect::<Vec<_>>()
        .join("\n");
    let mut done = types
        .into_iter()
        .permutations(2)
        .unique()
        .map(|mut t| {
            let t1 = t.remove(0);
            let t2 = t.remove(0);
            org.replace("TYPE1", &t1).replace("TYPE2", &t2)
        })
        .collect::<Vec<_>>()
        .join("\n");
    done.push_str(&combs);
    done.parse().unwrap()
    // panic!("{orgs:?}");
    // panic!("{types:?}");
    // while let Some(n) = iters.next() {
    //     if true {
    //     } else {
    //         orgs.push(n);
    //     }
    // }
}
