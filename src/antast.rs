use std::char;
use std::fmt;

use std::collections::HashMap;
    
use ast::{Expr, Opcode};
use eval;

#[derive(Clone)]
pub enum AExpr {
    AUnit(Typ),
    AInt(Typ, i32),
    ABool(Typ, bool),
    AStr(Typ, String),
    AVar(Typ, String),
    AOp(Typ, Box<AExpr>, Opcode, Box<AExpr>),
    AIf(Typ, Box<AExpr>, Box<AExpr>, Box<AExpr>),
    ALet(Typ, (Typ, String), Box<AExpr>, Box<AExpr>),
    ABind(Typ, (Typ, String), Box<AExpr>),
    AFun(Typ, (Typ, String), Box<AExpr>),
    ATup(Typ, Box<AExpr>, Box<AExpr>),
    AVnt(Typ, String, Box<AExpr>),
    AApp(Typ, Box<AExpr>, Box<AExpr>),
    AMatch(Typ, Box<AExpr>, Vec<(Box<AExpr>, Box<AExpr>)>),
    APUnit(Typ),
    APInt(Typ, i32),
    APBool(Typ, bool),
    APStr(Typ, String),
    APVar(Typ, String),
    APVnt(Typ, String, Box<AExpr>),
    APTup(Typ, Box<AExpr>, Box<AExpr>),
}


impl fmt::Debug for AExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (*self).clone() {
            AExpr::AUnit(_) |
            AExpr::APUnit(_) => {
                write!(f, "()")
            },
            AExpr::AInt(_, i) |
            AExpr::APInt(_, i) => {
                write!(f, "{}", i)
            },
            AExpr::ABool(_, b) |
            AExpr::APBool(_, b) => {
                write!(f, "{}", b)
            },
            AExpr::AStr(_, s) |
            AExpr::APStr(_, s) => {
                write!(f, "{}", s)
            },
            AExpr::AVar(_, id) |
            AExpr::APVar(_, id) => {
                write!(f, "{}", id)
            },
            AExpr::AOp(_, ae1, op, ae2) => {
                write!(f, "({:?} {:?} {:?})", ae1, op, ae2)
            },
            AExpr::AIf(_, ae1, ae2, ae3) => {
                write!(f, "if {:?} then {:?} else {:?}",
                       ae1, ae2, ae3)
            },
            AExpr::ALet(_, (ann_typ_var, id_var), ae1, ae2) => {
                write!(f, "let {:?}[{:?}] =\n{:?}\nin {:?}",
                       id_var, ann_typ_var, ae1, ae2)
            },
            AExpr::ABind(_, (ann_typ_var, id_var), ae1) => {
                write!(f, "let {:?}[{:?}] =\n{:?}",
                       id_var, ann_typ_var, ae1)
            },
            AExpr::AFun(_, (ann_typ_var, id_var), ae1) => {
                write!(f, "fun {}: {:?} ->\n{:?}",
                       id_var, ann_typ_var, ae1)
            },
            AExpr::ATup(_, ae1, ae2) |
            AExpr::APTup(_, ae1, ae2) => {
                write!(f, "({:?}, {:?})", ae1, ae2)
            }
            AExpr::AApp(_, ae1, ae2) => {
                write!(f, "{:?} {:?}", ae1, ae2)
            },
            AExpr::AVnt(ann_typ, cnstr, ae1) |
            AExpr::APVnt(ann_typ, cnstr, ae1) => {
                write!(f, "{}[{:?}] of {:?}", cnstr, ann_typ, ae1)
            },
            AExpr::AMatch(_, ae1, matchblock) => {
                write!(f, "match {:?} with\n{:?}", ae1, matchblock)
            },
        }
    }
}


#[derive(Debug, Clone)]
pub struct VariantSpec {
    pub vars: Vec<Typ>,
    pub name: String,
    pub constructors: Vec<(String, Typ)>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Typ {
    TUnit,
    TInt,
    TBool,
    TStr,
    TAlpha(char),    
    TArrow(Box<Typ>, Box<Typ>),
    TStar(Box<Typ>, Box<Typ>),
    TVnt(Vec<Typ>, String),
}

impl fmt::Debug for Typ {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (*self).clone() {
            Typ::TUnit =>
                write!(f, "unit"),
            Typ::TInt =>
                write!(f, "int"),
            Typ::TBool =>
                write!(f, "bool"),
            Typ::TStr =>
                write!(f, "str"),
            Typ::TAlpha(c) =>
                write!(f, "{:?}", c),
            Typ::TArrow(t1, t2) => {
                write!(f, "({:?} -> {:?})", *t1, *t2)
            },
            Typ::TStar(t1, t2) => {
                write!(f, "({:?} * {:?})", *t1, *t2)
            },
            Typ::TVnt(alpha_list, name) => {
                write!(f, "{:?} {:?}", alpha_list, name)
            },
        }
    }
}

pub type Equation = (Typ, Typ);

fn new_var(next: &mut u8) -> char {
    let x = *next;
    *next = x + 1;
    x as char
}

pub fn annotate(expr: Box<Expr>, next: &mut u8) -> Box<AExpr> {
    
    fn ant(e: Box<Expr>, next: &mut u8) -> Box<AExpr> {
        let ex = *e;
        match ex {
            Expr::Unit =>
                Box::new(AExpr::AUnit(Typ::TAlpha(new_var(next)))),
            Expr::Int(i) =>
                Box::new(AExpr::AInt(Typ::TAlpha(new_var(next)), i)),
            Expr::Bool(b) =>
                Box::new(AExpr::ABool(Typ::TAlpha(new_var(next)), b)),
            Expr::Str(s) =>
                Box::new(AExpr::AStr(Typ::TAlpha(new_var(next)), s)),
            Expr::Var(id) =>
                Box::new(AExpr::AVar(Typ::TAlpha(new_var(next)), id)),
            Expr::Op(e1, op, e2) =>
                Box::new(AExpr::AOp(Typ::TAlpha(new_var(next)),
                                    ant(e1, next), op,
                                    ant(e2, next))),
            Expr::If(e1, e2, e3) =>
                Box::new(AExpr::AIf(Typ::TAlpha(new_var(next)),
                                    ant(e1, next),
                                    ant(e2, next),
                                    ant(e3, next))),
            Expr::Let(id, e1, e2) =>
                Box::new(AExpr::ALet(Typ::TAlpha(new_var(next)),
                                     (Typ::TAlpha(new_var(next)), id),
                                     ant(e1, next),
                                     ant(e2, next))),
            Expr::Bind(id, e1) =>
                Box::new(AExpr::ABind(Typ::TAlpha(new_var(next)),
                                     (Typ::TAlpha(new_var(next)), id),
                                     ant(e1, next))),
            Expr::Fun(id, e1) =>
                Box::new(AExpr::AFun(Typ::TAlpha(new_var(next)),
                                     (Typ::TAlpha(new_var(next)), id),
                                     ant(e1, next))),
            Expr::Tup(e1, e2) =>
                Box::new(AExpr::ATup(Typ::TAlpha(new_var(next)),
                                     ant(e1, next),
                                     ant(e2, next))),
            Expr::Vnt(c, e1) =>
                Box::new(AExpr::AVnt(Typ::TAlpha(new_var(next)), c,
                                     ant(e1, next))),
            Expr::App(e1, e2) =>
                Box::new(AExpr::AApp(Typ::TAlpha(new_var(next)),
                                     ant(e1, next),
                                     ant(e2, next))),
            Expr::Match(e1, ps) =>
                Box::new(AExpr::AMatch(Typ::TAlpha(new_var(next)),
                                       ant(e1, next),
                                       ps.iter()
                                       .map(|&(ref p, ref e)|
                                            (ant(p.clone(), next),
                                             ant(e.clone(), next)))
                                       .collect::<Vec<(Box<AExpr>,
                                                       Box<AExpr>)>>())),
            Expr::PUnit =>
                Box::new(AExpr::APUnit(Typ::TAlpha(new_var(next)))),
            Expr::PInt(i) =>
                Box::new(AExpr::APInt(Typ::TAlpha(new_var(next)), i)),
            Expr::PBool(b) =>
                Box::new(AExpr::APBool(Typ::TAlpha(new_var(next)), b)),
            Expr::PStr(s) =>
                Box::new(AExpr::APStr(Typ::TAlpha(new_var(next)), s)),
            Expr::PVar(id) =>
                Box::new(AExpr::APVar(Typ::TAlpha(new_var(next)), id)),
            Expr::PVnt(c, e1) =>
                Box::new(AExpr::APVnt(Typ::TAlpha(new_var(next)), c,
                                      ant(e1, next))),
            Expr::PTup(e1, e2) =>
                Box::new(AExpr::APTup(Typ::TAlpha(new_var(next)),
                                      ant(e1, next),
                                      ant(e2, next))),
        }
    }

    ant(expr, next)
}


fn collect_expr(aexpr: Box<AExpr>,
                specs: &Vec<VariantSpec>,
                var_typs: &mut Vec<(String, Typ)>,
                next: &mut u8,
                env: &Vec<(String, Box<AExpr>)>)
                -> Result<Vec<Equation>, String> {
    let aex = *aexpr;
    match aex {
        AExpr::AUnit(ann_typ) |
        AExpr::APUnit(ann_typ) => 
            Ok(vec![(ann_typ.clone(), Typ::TUnit)]),
        AExpr::AInt(ann_typ, _) |
        AExpr::APInt(ann_typ, _) =>
            Ok(vec![(ann_typ.clone(), Typ::TInt)]),
        AExpr::ABool(ann_typ, _) |
        AExpr::APBool(ann_typ, _) => 
            Ok(vec![(ann_typ.clone(), Typ::TBool)]),
        AExpr::AStr(ann_typ, _) |
        AExpr::APStr(ann_typ, _) => 
            Ok(vec![(ann_typ.clone(), Typ::TStr)]),
        AExpr::AVar(ann_typ, id) => {
            if let Some(seen_typ) = var_typ(&id, var_typs) {
                Ok(vec![(ann_typ.clone(), seen_typ)])
            }
            else {
                match eval::lookup_var(&id, env) {
                    Ok((aex, _)) => {
                        let t = typ_of(&Box::new(aex));
                        let mut alpha_map: HashMap<Typ, Typ> = HashMap::new();
                        let _t = try!(build_typ_inst(&t, next, &mut alpha_map));
                        Ok(vec![(ann_typ.clone(), _t)])
                    },
                    _ => Err("No typ found for var ".to_string() + &id),
                }
            }
        },
        AExpr::APVar(ann_typ, id) => {
            var_typs.push((id, ann_typ.clone()));
            Ok(vec![])
        }
        AExpr::AOp(ann_typ, ae1, opcode, ae2) => {
            let mut rules = vec![(typ_of(&ae1), typ_of(&ae2))];

            match opcode {
                Opcode::Add => {
                    rules.push((ann_typ, typ_of(&ae1)));
                },
                Opcode::Sub |
                Opcode::Mul |
                Opcode::Div => {
                    rules.push((ann_typ, Typ::TInt));
                },
                Opcode::Equal |
                Opcode::Less |
                Opcode::Greater => {
                    rules.push((ann_typ, Typ::TBool));
                },
            }

            let ae1_rules = try!(collect_expr(ae1, specs, var_typs, next, env));
            let ae2_rules = try!(collect_expr(ae2, specs, var_typs, next, env));
            rules.extend_from_slice(&ae1_rules[..]);
            rules.extend_from_slice(&ae2_rules[..]);

            Ok(rules)
        },
        AExpr::AIf(ann_typ, ae1, ae2, ae3) => {
            let mut rules = vec![(typ_of(&ae1), Typ::TBool)];
            rules.push((typ_of(&ae2), typ_of(&ae3)));
            rules.push((ann_typ.clone(), typ_of(&ae2)));
            rules.push((ann_typ.clone(), typ_of(&ae3)));
            let ae1_rules = try!(collect_expr(ae1, specs, var_typs, next, env));
            let ae2_rules = try!(collect_expr(ae2, specs, var_typs, next, env));
            let ae3_rules = try!(collect_expr(ae3, specs, var_typs, next, env));
            rules.extend_from_slice(&ae1_rules[..]);
            rules.extend_from_slice(&ae2_rules[..]);
            rules.extend_from_slice(&ae3_rules[..]);

            Ok(rules)
        },
        AExpr::ALet(ann_typ, (ann_typ_var, id_var), ae1, ae2) => {
            var_typs.push((id_var, ann_typ_var.clone()));
            
            let mut rules = vec![(ann_typ.clone(), typ_of(&ae2))];
            rules.push((ann_typ_var.clone(), typ_of(&ae1)));
            let ae1_rules = try!(collect_expr(ae1, specs, var_typs, next, env));
            let ae2_rules = try!(collect_expr(ae2, specs, var_typs, next, env));
            rules.extend_from_slice(&ae1_rules[..]);
            rules.extend_from_slice(&ae2_rules[..]);

            var_typs.pop();
            Ok(rules)
        },
        AExpr::ABind(ann_typ, (ann_typ_var, id_var), ae1) => {
            var_typs.push((id_var, ann_typ_var.clone()));
            
            let mut rules = vec![(ann_typ.clone(), Typ::TUnit)];
            rules.push((ann_typ_var.clone(), typ_of(&ae1)));
            let ae1_rules = try!(collect_expr(ae1, specs, var_typs, next, env));
            rules.extend_from_slice(&ae1_rules[..]);

            var_typs.pop();
            Ok(rules)
        },
        AExpr::AFun(ann_typ, (ann_typ_var, id_var), ae1) => {
            var_typs.push((id_var, ann_typ_var.clone()));
            let mut rules =
                vec![(ann_typ.clone(),
                      Typ::TArrow(Box::new(ann_typ_var), Box::new(typ_of(&ae1))))];
            let ae1_rules = try!(collect_expr(ae1, specs, var_typs, next, env));
            rules.extend_from_slice(&ae1_rules[..]);
            
            var_typs.pop();
            Ok(rules)
        },
        AExpr::ATup(ann_typ, ae1, ae2) |
        AExpr::APTup(ann_typ, ae1, ae2) => {
            let mut rules =
                vec![(ann_typ.clone(),
                      Typ::TStar(Box::new(typ_of(&ae1)), Box::new(typ_of(&ae2))))];

            let ae1_rules = try!(collect_expr(ae1, specs, var_typs, next, env));
            let ae2_rules = try!(collect_expr(ae2, specs, var_typs, next, env));
            rules.extend_from_slice(&ae1_rules[..]);
            rules.extend_from_slice(&ae2_rules[..]);

            Ok(rules)
        },
        AExpr::AApp(ann_typ, ae1, ae2) => {
            let mut rules =
                vec![(typ_of(&ae1),
                      Typ::TArrow(Box::new(typ_of(&ae2)),
                                  Box::new(ann_typ.clone())))];

            let ae1_rules = try!(collect_expr(ae1, specs, var_typs, next, env));
            let ae2_rules = try!(collect_expr(ae2, specs, var_typs, next, env));
            rules.extend_from_slice(&ae1_rules[..]);
            rules.extend_from_slice(&ae2_rules[..]);

            Ok(rules)
        },
        AExpr::AVnt(ann_typ, cnstr, ae1) |
        AExpr::APVnt(ann_typ, cnstr, ae1) => {
            let (vnt_typ, cnstr_typ) = try!(build_typ(&cnstr, specs, next));
            let mut rules = vec![(ann_typ, vnt_typ)];
            rules.push((typ_of(&ae1), cnstr_typ));
            let ae1_rules = try!(collect_expr(ae1, specs, var_typs, next, env));
            rules.extend_from_slice(&ae1_rules[..]);
            
            Ok(rules)
        },
        AExpr::AMatch(ann_typ, ae1, matchblock) => {
            let mut rules: Vec<(Typ, Typ)> = Vec::new();
            for (pat, aex) in matchblock {
                rules.push((ann_typ.clone(), typ_of(&aex)));
                rules.push((typ_of(&ae1), typ_of(&pat)));

                let num_vars: u8 = num_vars_in_pat(pat.clone(), var_typs);
                let pat_rules = try!(collect_expr(pat, specs, var_typs, next, env));
                let aex_rules = try!(collect_expr(aex, specs, var_typs, next, env));
                for i in 0..num_vars {
                    var_typs.pop();
                }
                rules.extend_from_slice(&pat_rules[..]);
                rules.extend_from_slice(&aex_rules[..]);
            }
            Ok(rules)
        },
    }
}

fn num_vars_in_pat(pat: Box<AExpr>, var_typs: &mut Vec<(String,Typ)>) -> u8 {
    let p = *pat;
    match p {
        AExpr::APVar(_,id) => {
            if let Some(_) = var_typ(&id, var_typs) { 0 }
            else { 1 }
        }
        AExpr::APTup(_, ap1, ap2) => {
            num_vars_in_pat(ap1, var_typs) +
                num_vars_in_pat(ap2, var_typs)
        },
        AExpr::APVnt(_,_,ap1) => num_vars_in_pat(ap1, var_typs),
        _ => 0,
    }
}

fn get_cnstr_typ(cnstr: &String, specs: &Vec<VariantSpec>)
                       -> Option<Typ> {
    for spec in specs {
        if let Some(&(_, ref t_cnstr)) = spec.constructors.iter()
            .find(|&&(ref c, _)| *cnstr == *c) {
                return Some(t_cnstr.clone());
            }
    }
    None
}

fn get_vnt_typ(cnstr: &String, specs: &Vec<VariantSpec>)
               -> Option<Typ> {
    for spec in specs {
        if let Some(&(_, _)) = spec.constructors.iter()
            .find(|&&(ref c, _)| *cnstr == *c) {
                let alphas = spec.vars.clone();
                let name = spec.name.clone();
                return Some(Typ::TVnt(alphas, name));
            }
    }
    None
}

fn build_typ(cnstr: &String, specs: &Vec<VariantSpec>, next: &mut u8)
             -> Result<(Typ, Typ), String> {

    if let (Some(t_v), Some(t_c)) = (get_vnt_typ(cnstr, specs),
                                     get_cnstr_typ(cnstr, specs)) {
        let mut alpha_map: HashMap<Typ, Typ> = HashMap::new();
        let _t_v = try!(build_typ_inst(&t_v, next, &mut alpha_map));
        let _t_c = try!(build_typ_inst(&t_c, next, &mut alpha_map));
        Ok((_t_v, _t_c))
    }
    else {
        return Err("Could not find variant for constructor ".to_string() + cnstr);
    }
}

fn swap(t: &Typ, next: &mut u8, alpha_map: &mut HashMap<Typ, Typ>) -> Typ {
    
    if let Some(ref _t) = alpha_map.get(t) {
        return (**_t).clone();
    }
    
    let _t = Typ::TAlpha(new_var(next));
    alpha_map.insert((*t).clone(), _t.clone());
    _t
}

fn build_typ_inst(t: &Typ, next: &mut u8, alpha_map: &mut HashMap<Typ, Typ>)
      -> Result<Typ, String> {

    match (*t).clone() {
        Typ::TUnit => Ok((*t).clone()),
        Typ::TInt => Ok((*t).clone()),
        Typ::TStr => Ok((*t).clone()),
        Typ::TBool => Ok((*t).clone()),
        Typ::TStar(t1, t2) => {
            let _t1 = Box::new(try!(build_typ_inst(&*t1, next, alpha_map)));
            let _t2 = Box::new(try!(build_typ_inst(&*t2, next, alpha_map)));
            Ok(Typ::TStar(_t1, _t2))
        },
        Typ::TAlpha(_) => Ok(swap(t, next, alpha_map)),
        Typ::TVnt(alphas, n) => 
            Ok(Typ::TVnt(alphas
                         .iter()
                         .map(|ref a| swap(a, next, alpha_map))
                         .collect::<Vec<Typ>>(), n)),
        Typ::TArrow(t1, t2) => {
            let _t1 = Box::new(try!(build_typ_inst(&*t1, next, alpha_map)));
            let _t2 = Box::new(try!(build_typ_inst(&*t2, next, alpha_map)));
            Ok(Typ::TArrow(_t1, _t2))
        },
    }
}

       
pub fn collect(aexpr: Box<AExpr>, specs: &Vec<VariantSpec>,
               next_var: &mut u8, env: &Vec<(String, Box<AExpr>)>)
               -> Result<Vec<Equation>, String> {
    let mut var_typs: Vec<(String, Typ)> = Vec::new();
    
    collect_expr(aexpr, specs, &mut var_typs, next_var, env)
}

fn occurs_in(t1: &Typ, t2: &Typ) -> bool {
    if *t1 == *t2 { return true; }
    match (*t2).clone() {
        Typ::TUnit |
        Typ::TInt |
        Typ::TBool |
        Typ::TStr => false,
        Typ::TAlpha(_) =>  *t1 == *t2,
        Typ::TArrow(a, b) |
        Typ::TStar(a, b) =>
            *t1 == *t2 ||
            occurs_in(t1, &*a) ||
            occurs_in(t1, &*b),
        _ => false,
    }
}

fn apply_subst(eq: &Equation, t: &Typ) -> Typ {
    let &(ref t1, ref t2) = eq;
    match (*t).clone() {
        Typ::TUnit |
        Typ::TInt |
        Typ::TBool |
        Typ::TStr => (*t).clone(),
        Typ::TAlpha(_) => if *t == *t1 { (*t2).clone() } else { (*t).clone() },
        Typ::TArrow(a1, a2) => if *t == *t1 { (*t2).clone() } else {
            Typ::TArrow(Box::new(apply_subst(eq, &*a1)),
                        Box::new(apply_subst(eq, &*a2)))
        },
        Typ::TStar(a1, a2) => if *t == *t1 { (*t2).clone() } else {
            Typ::TStar(Box::new(apply_subst(eq, &*a1)),
                       Box::new(apply_subst(eq, &*a2)))
        },
        Typ::TVnt(alphas, name) => {
            let new_alphas =
                alphas.iter()
                .map(|alpha|
                     if *alpha == *t1 { (*t2).clone() }
                     else { (*alpha).clone() })
                .collect::<Vec<Typ>>();
            Typ::TVnt(new_alphas, name)
        }
    }
}

pub fn unify(mut eqs: Vec<Equation>) -> Result<Vec<Equation>, String> {

    // a function to check if an equation is circular, and if it is not,
    // push it and return a new list of altered equations
    fn make_subst(eq: &Equation, eqs: Vec<Equation>, substs: &mut Vec<Equation>)
                  -> Result<Vec<Equation>, String> {
        //println!("eqs before: {:?}", eqs.clone());
        //println!("rule: {:?}", eq.clone());
        let &(ref t1, ref t2) = eq;

        if !occurs_in(t1, t2) {
            substs.push((*eq).clone());
            Ok(eqs.into_iter()
               .map(|(ref a, ref b)| (apply_subst(eq, a),
                                      apply_subst(eq, b)))
               .collect())
        }
        else { if *t1 == *t2 { Ok(eqs) }
               else { Err("Circular typs!".to_string()) } }
    }

    fn funify(mut eqs: Vec<Equation>, substs: &mut Vec<Equation>)
              -> Result<(), String> {
        if let Some(eq) = eqs.pop() {
            match eq {
                (Typ::TAlpha(x), t) |
                (t, Typ::TAlpha(x)) => {
                    match make_subst(&(Typ::TAlpha(x), t), eqs, substs) {
                        Ok(_eqs) => {//println!("eqs after: {:?}", _eqs.clone());
                                     funify(_eqs, substs)},
                        Err(s) => Err(s),
                    }
                },
                (Typ::TArrow(t1a, t2a), Typ::TArrow(t1b, t2b)) |
                (Typ::TStar(t1a, t2a), Typ::TStar(t1b, t2b)) => {
                    eqs.push((*t2a, *t2b));
                    eqs.push((*t1a, *t1b));
                    funify(eqs, substs)
                },
                (Typ::TVnt(t1_alphas, t1_name), Typ::TVnt(t2_alphas, t2_name)) => {
                    if t1_name == t2_name {
                        let mut iter = t1_alphas.iter().zip(t2_alphas.iter());

                        while let Some((ref alpha_1, ref alpha_2)) = iter.next() {
                            eqs.push(((**alpha_1).clone(), (**alpha_2).clone()));
                        }

                        funify(eqs, substs)
                    }
                    else {
                        Err("Cannot unify variant ".to_string() + &t1_name +
                            " with variant " + &t2_name)
                    }
                },
                (t1, t2) => {
                    if t1 != t2 { Err("Cannot unify type ".to_string() +
                                      &format!("{:?}", t1.clone()) + " with type " +
                                      &format!("{:?}", t2.clone())) }
                    else { funify(eqs, substs) }
                },
            }
        }
        else { Ok(()) }
    }

    eqs.reverse();
    //println!("{:?}", eqs);
    let mut substs: Vec<Equation> = Vec::new();
    match funify(eqs, &mut substs) {
        Ok(()) => Ok(substs),
        Err(e) => Err(e),
    }
}

fn var_typ(id: &String, vars: &mut Vec<(String, Typ)>)
           -> Option<Typ> {
    if let Some(&(_ ,ref t)) = vars.iter().find(|&&(ref s, _)| *s == *id) {
        Some(t.clone())
    }
    else { None }
}

pub fn typ_of(aexpr: &Box<AExpr>) -> Typ {
    let aex = (**aexpr).clone();
    match aex {
        AExpr::AUnit(t) => t,
        AExpr::AInt(t,_) => t,
        AExpr::ABool(t,_) => t,
        AExpr::AStr(t,_) => t,
        AExpr::AVar(t,_) => t,
        AExpr::AOp(t,_,_,_) => t,
        AExpr::AIf(t,_,_,_) => t,
        AExpr::ALet(t,_,_,_) => t,
        AExpr::ABind(t,_,_) => t,
        AExpr::AFun(t,_,_) => t,
        AExpr::ATup(t,_,_) => t,
        AExpr::AVnt(t,_,_,) => t,
        AExpr::AApp(t,_,_) => t,
        AExpr::AMatch(t,_,_) => t,
        AExpr::APUnit(t) => t,
        AExpr::APInt(t,_) => t,
        AExpr::APBool(t,_) => t,
        AExpr::APStr(t,_) => t,
        AExpr::APVar(t,_) => t,
        AExpr::APVnt(t,_,_) => t,
        AExpr::APTup(t,_,_) => t,
    }
}

pub fn infer_atree(aexpr: Box<AExpr>, substs: &Vec<Equation>) -> Box<AExpr> {
    let aex = *aexpr;
    match aex {
        AExpr::AUnit(ann_typ) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            Box::new(AExpr::AUnit(inferred_typ))
        },
        AExpr::APUnit(ann_typ) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            Box::new(AExpr::APUnit(inferred_typ))
        },
        AExpr::AInt(ann_typ, i) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            Box::new(AExpr::AInt(inferred_typ, i))
        },
        AExpr::APInt(ann_typ, i) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            Box::new(AExpr::APInt(inferred_typ, i))
        },
        AExpr::ABool(ann_typ, b) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            Box::new(AExpr::ABool(inferred_typ, b))
        },
        AExpr::APBool(ann_typ, b) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            Box::new(AExpr::APBool(inferred_typ, b))
        },
        AExpr::AStr(ann_typ, s) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            Box::new(AExpr::AStr(inferred_typ, s))
        },
        AExpr::APStr(ann_typ, s) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            Box::new(AExpr::APStr(inferred_typ, s))
        },
        AExpr::AVar(ann_typ, id) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            Box::new(AExpr::AVar(inferred_typ, id))
        },
        AExpr::APVar(ann_typ, id) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            Box::new(AExpr::APVar(inferred_typ, id))
        },
        AExpr::AOp(ann_typ, ae1, op, ae2) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            let inferred_ae1 = infer_atree(ae1, substs);
            let inferred_ae2 = infer_atree(ae2, substs);
            Box::new(AExpr::AOp(inferred_typ, inferred_ae1, op, inferred_ae2))
        },
        AExpr::AIf(ann_typ, ae1, ae2, ae3) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            let inferred_ae1 = infer_atree(ae1, substs);
            let inferred_ae2 = infer_atree(ae2, substs);
            let inferred_ae3 = infer_atree(ae3, substs);
            Box::new(AExpr::AIf(inferred_typ, inferred_ae1,
                                inferred_ae2, inferred_ae3))
        },
        AExpr::ALet(ann_typ, (ann_typ_var, id_var), ae1, ae2) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            let inferred_var_typ = infer_typ(&ann_typ_var, substs);
            let inferred_ae1 = infer_atree(ae1, substs);
            let inferred_ae2 = infer_atree(ae2, substs);
            Box::new(AExpr::ALet(inferred_typ, (inferred_var_typ, id_var),
                                 inferred_ae1, inferred_ae2))
        },
        AExpr::ABind(ann_typ, (ann_typ_var, id_var), ae1) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            let inferred_var_typ = infer_typ(&ann_typ_var, substs);
            let inferred_ae1 = infer_atree(ae1, substs);
            Box::new(AExpr::ABind(inferred_typ, (inferred_var_typ, id_var),
                                  inferred_ae1))
        },
        AExpr::AFun(ann_typ, (ann_typ_var, id_var), ae1) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            let inferred_var_typ = infer_typ(&ann_typ_var, substs);
            let inferred_ae1 = infer_atree(ae1, substs);
            Box::new(AExpr::AFun(inferred_typ, (inferred_var_typ, id_var),
                                 inferred_ae1))
        },
        AExpr::ATup(ann_typ, ae1, ae2) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            let inferred_ae1 = infer_atree(ae1, substs);
            let inferred_ae2 = infer_atree(ae2, substs);
            Box::new(AExpr::ATup(inferred_typ, inferred_ae1, inferred_ae2))
        }
        AExpr::APTup(ann_typ, ae1, ae2) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            let inferred_ae1 = infer_atree(ae1, substs);
            let inferred_ae2 = infer_atree(ae2, substs);
            Box::new(AExpr::APTup(inferred_typ, inferred_ae1, inferred_ae2))
        },
        AExpr::AApp(ann_typ, ae1, ae2) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            let inferred_ae1 = infer_atree(ae1, substs);
            let inferred_ae2 = infer_atree(ae2, substs);
            Box::new(AExpr::AApp(inferred_typ, inferred_ae1, inferred_ae2))
        },
        AExpr::AVnt(ann_typ, cnstr, ae1) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            let inferred_ae1 = infer_atree(ae1, substs);
            Box::new(AExpr::AVnt(inferred_typ, cnstr, inferred_ae1))
        },
        AExpr::APVnt(ann_typ, cnstr, ae1) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            let inferred_ae1 = infer_atree(ae1, substs);
            Box::new(AExpr::APVnt(inferred_typ, cnstr, inferred_ae1))
        },
        AExpr::AMatch(ann_typ, ae1, matchblock) => {
            let inferred_typ = infer_typ(&ann_typ, substs);
            let inferred_ae1 = infer_atree(ae1, substs);
            let inferred_matchblock =
                matchblock.iter()
                .map(|&(ref ap, ref ae)|
                     (infer_atree((*ap).clone(), substs),
                      infer_atree((*ae).clone(), substs)))
                .collect::<Vec<(Box<AExpr>, Box<AExpr>)>>();
            Box::new(AExpr::AMatch(inferred_typ, inferred_ae1, inferred_matchblock))
        },
    }
}

fn infer_typ(t: &Typ, substs: &Vec<Equation>) -> Typ {
    substs.iter().fold((*t).clone(), |acc, ref subst| apply_subst(subst, &acc))
}

pub fn reduce(aexpr: Box<AExpr>) -> Box<AExpr> {
    
    let mut alpha_map: HashMap<Typ, Typ> = HashMap::new();
    let mut next_alpha: u8 = b'a';

    fn helper(aexpr: Box<AExpr>, next_alpha: &mut u8,
              alpha_map: &mut HashMap<Typ, Typ>)
              -> Box<AExpr> {

        let red_typ = reduce_typ(&typ_of(&aexpr), next_alpha, alpha_map);
        let aex = *aexpr;
        match aex {
            AExpr::AUnit(_) => {
                Box::new(AExpr::AUnit(red_typ))
            },
            AExpr::APUnit(_) => {
                Box::new(AExpr::APUnit(red_typ))
            },
            AExpr::AInt(_, i) => {
                Box::new(AExpr::AInt(red_typ, i))
            },
            AExpr::APInt(_, i) => {
                Box::new(AExpr::APInt(red_typ, i))
            },
            AExpr::ABool(_, b) => {
                Box::new(AExpr::ABool(red_typ, b))
            },
            AExpr::APBool(_, b) => {
                Box::new(AExpr::APBool(red_typ, b))
            },
            AExpr::AStr(_, s) => {
                Box::new(AExpr::AStr(red_typ, s))
            },
            AExpr::APStr(_, s) => {
                Box::new(AExpr::APStr(red_typ, s))
            },
            AExpr::AVar(_, id) => {
                Box::new(AExpr::AVar(red_typ, id))
            },
            AExpr::APVar(_, id) => {
                Box::new(AExpr::APVar(red_typ, id))
            },
            AExpr::AOp(_, ae1, op, ae2) => {
                let red_ae1 = helper(ae1, next_alpha, alpha_map);
                let red_ae2 = helper(ae2, next_alpha, alpha_map);
                Box::new(AExpr::AOp(red_typ, red_ae1, op, red_ae2))
            },
            AExpr::AIf(_, ae1, ae2, ae3) => {
                let red_ae1 = helper(ae1, next_alpha, alpha_map);
                let red_ae2 = helper(ae2, next_alpha, alpha_map);
                let red_ae3 = helper(ae3, next_alpha, alpha_map);
                Box::new(AExpr::AIf(red_typ, red_ae1, red_ae2, red_ae3))
            },
            AExpr::ALet(_, (ann_typ_var, id_var), ae1, ae2) => {
                let red_typ_var = reduce_typ(&ann_typ_var, next_alpha, alpha_map);
                let red_ae1 = helper(ae1, next_alpha, alpha_map);
                let red_ae2 = helper(ae2, next_alpha, alpha_map);
                Box::new(AExpr::ALet(red_typ, (red_typ_var, id_var),
                                     red_ae1, red_ae2))
            },
            AExpr::ABind(_, (ann_typ_var, id_var), ae1) => {
                let red_typ_var = reduce_typ(&ann_typ_var, next_alpha, alpha_map);
                let red_ae1 = helper(ae1, next_alpha, alpha_map);
                Box::new(AExpr::ABind(red_typ, (red_typ_var, id_var), red_ae1))
            },
            AExpr::AFun(_, (ann_typ_var, id_var), ae1) => {
                let red_typ_var = reduce_typ(&ann_typ_var, next_alpha, alpha_map);
                let red_ae1 = helper(ae1, next_alpha, alpha_map);
                Box::new(AExpr::AFun(red_typ, (red_typ_var, id_var), red_ae1))
            },
            AExpr::ATup(_, ae1, ae2) => {
                let red_ae1 = helper(ae1, next_alpha, alpha_map);
                let red_ae2 = helper(ae2, next_alpha, alpha_map);
                Box::new(AExpr::ATup(red_typ, red_ae1, red_ae2))
            }
            AExpr::APTup(_, ae1, ae2) => {
                let red_ae1 = helper(ae1, next_alpha, alpha_map);
                let red_ae2 = helper(ae2, next_alpha, alpha_map);
                Box::new(AExpr::APTup(red_typ, red_ae1, red_ae2))
            },
            AExpr::AApp(_, ae1, ae2) => {
                let red_ae1 = helper(ae1, next_alpha, alpha_map);
                let red_ae2 = helper(ae2, next_alpha, alpha_map);
                Box::new(AExpr::AApp(red_typ, red_ae1, red_ae2))
            },
            AExpr::AVnt(_, cnstr, ae1) => {
                let red_ae1 = helper(ae1, next_alpha, alpha_map);
                Box::new(AExpr::AVnt(red_typ, cnstr, red_ae1))
            },
            AExpr::APVnt(_, cnstr, ae1) => {
                let red_ae1 = helper(ae1, next_alpha, alpha_map);
                Box::new(AExpr::APVnt(red_typ, cnstr, red_ae1))
            },
            AExpr::AMatch(_, ae1, matchblock) => {
                let red_ae1 = helper(ae1, next_alpha, alpha_map);
                let red_matchblock =
                    matchblock.iter()
                    .map(|&(ref inf_p, ref inf_e)|
                         (helper((*inf_p).clone(), next_alpha, alpha_map),
                          helper((*inf_e).clone(), next_alpha, alpha_map)))
                    .collect::<Vec<(Box<AExpr>, Box<AExpr>)>>();
                Box::new(AExpr::AMatch(red_typ, red_ae1, red_matchblock))
            },
        }
    }
    
    fn reduce_typ(t: &Typ, next_alpha: &mut u8, alpha_map: &mut HashMap<Typ, Typ>)
                  -> Typ {
        match (*t).clone() {
            Typ::TUnit |
            Typ::TInt |
            Typ::TBool |
            Typ::TStr => (*t).clone(),
            Typ::TArrow(t1, t2) =>
                Typ::TArrow(Box::new(reduce_typ(&(*t1), next_alpha, alpha_map)),
                            Box::new(reduce_typ(&(*t2), next_alpha, alpha_map))),
            Typ::TStar(t1, t2) =>
                Typ::TStar(Box::new(reduce_typ(&(*t1), next_alpha, alpha_map)),
                           Box::new(reduce_typ(&(*t2), next_alpha, alpha_map))),
            Typ::TVnt(typ_vec, name) => {
                let reduced_typ_vec =
                    typ_vec.iter()
                    .map(|ref alpha| reduce_typ(&alpha, next_alpha, alpha_map))
                    .collect::<Vec<Typ>>();
                Typ::TVnt(reduced_typ_vec, name)
            },
            _ => swap(t, next_alpha, alpha_map),
        }
    }

    helper(aexpr, &mut next_alpha, &mut alpha_map)
}
