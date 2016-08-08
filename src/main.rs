pub mod ocalfer;

mod ast;
mod antast;
mod eval;

use antast::{VariantSpec, AExpr};
use std::io;

/*
read exprssions from a file
TODO: update parser for program: list of variants and expressions
create repl!
*/

fn main() {

    /*let okvnts = vec![
        ocalfer::parse_VariantSpec("type 'a option = Some of 'a | None of unit"),
        ocalfer::parse_VariantSpec("type 'a list = Nil of unit | Cons of ('a * 'a list)"),
        ocalfer::parse_VariantSpec("type 'a 'b aorb = A of 'a | B of 'b")
    ];

    let vnts = okvnts
        .iter()
        .fold(vec![], |mut acc, ref okvnt|
              if let &Ok(ref vnt) = *okvnt {
                  acc.push((*vnt).clone());
                  acc
              }
              else { acc });
    */
    //do_expr("let map = fun f -> fun l -> match l with | Nil () -> Nil () | Cons (hd, tl) -> Cons (f hd, map f tl).", &vnts);
    //do_expr("let map = fun f -> fun l -> match l with | Nil () -> Nil () | Cons (hd, tl) -> Cons (f hd, map f tl) in let l1 = Cons(1, Cons(2, Nil ())) in let fn = fun x -> (x + 1) in map fn l1", &vnts);
    //do_expr("let m = fun l -> match l with | Cons(hd, tl) -> 1 + m tl | Nil () -> 0 in let kay = Cons(4, Cons(5, Cons(6, Cons(7, Nil ())))) in m kay", &vnts);
    //do_expr("let make_list = fun n -> if n > 0 then Cons(n, make_list (n - 1)) else Nil () in make_list 10", &vnts);
    //do_expr("let sum_n = fun n -> if n > 0 then n + sum_n (n - 1) else 0 in sum_n 20", &vnts);
    //do_expr("let sum_n = fun n -> if n > 0 then n + sum_n (n - 1) else 0 in Cons(sum_n 10, Nil ())", &vnts);
    //do_expr("let x = Cons(1, Nil ()) in let y = 2 in Cons(y, x)", &vnts);
    //do_expr("Cons(1, Cons(\"hey\", Nil ()))", &vnts);
    repl();
}

fn repl() {

    let mut env: Vec<(String, Box<AExpr>)> = vec![];
    let mut vnts: Vec<VariantSpec> = vec![];
    let mut expr: String = String::new();
    let mut line: String = String::new();
    
    loop {
        line = "".to_string();
        io::stdin().read_line(&mut line);

        if line.trim() == "q" {
            break;
        }
        else if line.trim() == ";" {
            match ocalfer::parse_VariantSpec(&expr) {
                Ok(vnt) => {
                    println!("Vnt: {:?}", vnt.clone());
                    vnts.push(vnt);
                },
                _ => do_expr(&expr, &vnts, &mut env),
            }
            expr = "".to_string();
        }
        else {
            expr = expr + line.trim();
            println!("{:?}", expr.clone());
        }
    }
}

fn do_expr(s: &str, vnts: &Vec<VariantSpec>, env: &mut Vec<(String, Box<AExpr>)>) {
    match ocalfer::parse_Expr(s) {
        Ok(tree) => {
            //println!("{:?}", *tree);
            let mut next: &mut u8 = &mut b'a';
            let atree = antast::annotate(tree.clone(), next);
            //println!("{:?}", atree.clone());
            match antast::collect(atree.clone(), vnts, next, &env) {
                Ok(eqs) => {
                    //println!("Equations: {:?}", eqs.clone());
                    match antast::unify(eqs) {
                        Ok(substs) => {
                            //println!("Substitutions: {:?}", substs.clone());
                            let inferred_tree =
                                antast::infer_atree(atree, &substs);
                            let reduced_tree =
                                antast::reduce(inferred_tree);
                            //println!("Reduced tree: {:?}", reduced_tree.clone());
                            match eval::eval(reduced_tree, env) {
                                Ok(ev_tree) => {
                                    //println!("ENV: {:?}\n", env);
                                    println!("{:?}\n", ev_tree);
                                },
                                Err(e) => println!("{:?}", e),
                            }
                        },
                        Err(e) => println!("{:?}", e),
                    }
                },
                Err(e) => println!("{:?}", e),
            }
        },
        Err(e) => println!("{:?}", e),
    }
}
