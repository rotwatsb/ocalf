use ast::{Opcode};
use antast::{AExpr, Typ};

use std::collections::HashMap;

pub fn eval(aexpr: Box<AExpr>, env: &mut Vec<(String, Box<AExpr>)>)
            -> Result<AExpr, String> {
    
    fn helper(aexpr: Box<AExpr>, env: &mut Vec<(String, Box<AExpr>)>)
              -> Result<(AExpr, u8), String> {

        let aex = *aexpr;
        match aex {
            AExpr::AUnit(_) |
            AExpr::AInt(_,_) |
            AExpr::ABool(_,_) |
            AExpr::AStr(_,_) => Ok((aex, 0)),
            AExpr::AVar(_, id) => {
                let (aex, _) = try!(lookup_var(&id, env));
                helper(Box::new(aex), env)
            }
            AExpr::AOp(_, ae1, opcode, ae2) => {
                let (e1, _) = try!(helper(ae1, env));
                let (e2, _) = try!(helper(ae2, env));
                match opcode {
                    Opcode::Add =>
                        match (e1, e2) {
                            (AExpr::AInt(_, i1), AExpr::AInt(_, i2)) =>
                                Ok((AExpr::AInt(Typ::TInt, i1 + i2), 0)),
                            (AExpr::AStr(_, s1), AExpr::AStr(_, s2)) =>
                                Ok((AExpr::AStr(Typ::TStr, s1 + &s2), 0)),
                            _ => Err("Add error".to_string()),
                        },
                    Opcode::Sub =>
                        match (e1, e2) {
                            (AExpr::AInt(_, i1), AExpr::AInt(_, i2)) =>
                                Ok((AExpr::AInt(Typ::TInt, i1 - i2), 0)),
                            _ => Err("Sub error".to_string()),
                        },
                    Opcode::Mul =>
                        match (e1, e2) {
                            (AExpr::AInt(_, i1), AExpr::AInt(_, i2)) =>
                                Ok((AExpr::AInt(Typ::TInt, i1 * i2), 0)),
                            _ => Err("Mul error".to_string()),
                        },
                    Opcode::Div =>
                        match (e1, e2) {
                            (AExpr::AInt(_, i1), AExpr::AInt(_, i2)) =>
                                Ok((AExpr::AInt(Typ::TInt, i1 / i2), 0)),
                            _ => Err("Div error".to_string()),
                        },
                    Opcode::Equal =>
                        match (e1, e2) {
                            (AExpr::AInt(_, a), AExpr::AInt(_, b)) =>
                                Ok((AExpr::ABool(Typ::TBool, a == b), 0)),
                            (AExpr::ABool(_, a), AExpr::ABool(_, b)) =>
                                Ok((AExpr::ABool(Typ::TBool, a == b), 0)),
                            (AExpr::AStr(_, a), AExpr::AStr(_, b)) =>
                                Ok((AExpr::ABool(Typ::TBool, a == b), 0)),
                            _ => Err("Eq error".to_string()),
                        },
                    Opcode::Less =>
                        match (e1, e2) {
                            (AExpr::AInt(_, a), AExpr::AInt(_, b)) =>
                                Ok((AExpr::ABool(Typ::TBool, a < b), 0)),
                            _ => Err("Less error".to_string()),
                        },
                    Opcode::Greater =>
                        match (e1, e2) {
                            (AExpr::AInt(_, a), AExpr::AInt(_, b)) =>
                                Ok((AExpr::ABool(Typ::TBool, a > b), 0)),
                            _ => Err("Gtr error".to_string()),
                        },
                }
            },
            AExpr::AIf(_, ae1, ae2, ae3) => {
                let (e1, _) = try!(helper(ae1, env));
                match e1 {
                    AExpr::ABool(_, true) => helper(ae2, env),
                    AExpr::ABool(_, false) => helper(ae3, env),
                    _ => Err("If Error".to_string())
                }
            },
            AExpr::ALet(_, (_, id_var), ae1, ae2) => {
                env.push((id_var, ae1));
                let (e, _) = try!(helper(ae2, env));
                env.pop();
                Ok((e, 0))
            },
            AExpr::ABind(_, (_, id_var), ae1) => {
                env.push((id_var, ae1));
                Ok((AExpr::AUnit(Typ::TUnit), 0))
            },
            AExpr::AFun(_, (_, _), _) => Ok((aex, 0)),
            AExpr::ATup(red_typ, ae1, ae2) => {
                let (e1, _) = try!(helper(ae1, env));
                let (e2, _) = try!(helper(ae2, env));
                Ok((AExpr::ATup(red_typ, Box::new(e1), Box::new(e2)), 0))
            },
            AExpr::AVnt(t, name, ae1) => {
                let (e1, _) = try!(helper(ae1, env));
                Ok((AExpr::AVnt(t, name, Box::new(e1)), 0))
            }
            AExpr::AApp(_, ae1, ae2) => {
                let (e1, rem1) = try!(helper(ae1, env));
                let (e2, _) = try!(helper(ae2, env));
                
                match e1 {
                    AExpr::AFun(_, (_, id_var), aex) => {
                        env.push((id_var, Box::new(e2)));
                        let (e, _) = try!(helper(aex, env));
                        match e {
                            AExpr::AFun(_,(_,_,),_) => {
                                Ok((e, 1 + rem1))
                            },
                            _ => {
                                for i in 0..(rem1 + 1) {
                                    env.pop();
                                }
                                Ok((e, 0))
                            },
                        }
                    },
                    _ => Err("Cannot apply non-function to value".to_string()),
                }
            },
            AExpr::AMatch(_, ae1, matchblock) => {
                let (e1, _) = try!(helper(ae1, env));
                //println!("trying to match expr: {:?}", e1.clone());
                if let Some(&(ref pat, ref exp)) = 
                    matchblock.iter()
                    .find(|&&(ref p, _)|
                          {
                              //println!("with pat: {:?}", (*p).clone());
                              pattern_matches((*p).clone(), Box::new(e1.clone()))
                          }) {
                        
                        let n = make_matches((*pat).clone(),
                                             Box::new(e1.clone()), env);
                        let (r, _) = try!(helper((*exp).clone(), env));
                        for i in 0..n {
                            env.pop();
                        }
                        Ok((r, 0))
                    }
                else {
                    Err("Pattern matching failed.".to_string())
                }
            },
            _ => Err("Cannot evaluate patterns.".to_string()),
        }
    }

    fn make_matches(pat: Box<AExpr>, exp: Box<AExpr>,
                    env: &mut Vec<(String, Box<AExpr>)>) -> u32 {
        let p = (*pat).clone();
        let e = (*exp).clone();

        match e {
            AExpr::AUnit(_) |
            AExpr::AInt(_, _) |
            AExpr::AStr(_, _) |
            AExpr::ABool(_, _) => {
                match p {
                    AExpr::APVar(_, id) => {
                        env.push((id, exp));
                        1
                    },
                    _ => 0,
                }
            },
            AExpr::ATup(_, ae1, ae2) => {
                match p {
                    AExpr::APVar(_, id) => {
                        env.push((id, exp));
                        1
                    },
                    AExpr::APTup(_, pe1, pe2) =>
                        make_matches(pe1, ae1, env) +
                        make_matches(pe2, ae2, env),
                    _ => 0,
                }
            },
            AExpr::AVnt(_, _, ve) => {
                //println!("P: {:?}", (*pat).clone());
                //println!("E: {:?}", (*exp).clone());

                match p {
                    AExpr::APVar(_, id) => {
                        env.push((id, exp));
                        1
                    },
                    AExpr::APVnt(_, _, pe) =>
                        make_matches(pe, ve, env),
                    _ => 0,
                }
            },
            _ => 0,
        }
    }
    
    fn pattern_matches(pat: Box<AExpr>, exp: Box<AExpr>) -> bool {
        
        let p = *pat;
        let e = *exp;

        match e {
            AExpr::AUnit(_) => {
                match p {
                    AExpr::APVar(_,_) => true,
                    AExpr::APUnit(_) => true,
                    _ => false,
                }
            },
            AExpr::AInt(_, i) => {
                match p {
                    AExpr::APVar(_,_) => true,
                    AExpr::APInt(_, pi) => i == pi,
                    _ => false,
                }
            },
            AExpr::AStr(_, s) => {
                match p {
                    AExpr::APVar(_,_) => true,
                    AExpr::APStr(_, ps) => s == ps,
                    _ => false,
                }
            },
            AExpr::ABool(_, b) => {
                match p {
                    AExpr::APVar(_,_) => true,
                    AExpr::APBool(_, pb) => b == pb,
                    _ => false,
                }
            },
            AExpr::ATup(_, ae1, ae2) => {
                match p {
                    AExpr::APVar(_,_) => true,
                    AExpr::APTup(_, pe1, pe2) =>
                        pattern_matches(pe1, ae1) && pattern_matches(pe2, ae2),
                    _ => false,
                }
            },
            AExpr::AVnt(vt, vname, ve) => {
                match p {
                    AExpr::APVar(_,_) => true,
                    AExpr::APVnt(pt, pname, pe) =>
                        vname == pname && pattern_matches(pe, ve),
                    _ => false,
                }
            },
            _ => false,
        }
    }
    
    match helper(aexpr, env) {
        Ok((aex, _)) => {
            Ok(aex)
        },
        Err(s) => Err(s),
    }
}

/*fn variants_match(vt: &Typ, pt: &Typ) -> bool {

    fn helper(vt: &Typ, pt: &Typ, alpha_map: &mut HashMap<Typ, Typ>) -> bool {
        if *vt == *pt { true } else {
            match pt {
                TAlpha(_) => {
                    if let Some(ref _t) = alpha_map.get(pt) {
                    }
                }
            }
        }
    }

    let mut alpha_map: HashMap<Typ, Typ> = HashMap::new();
    helper(vt, pt, &mut alpha_map)
}*/

pub fn lookup_var(id: &String, env: &Vec<(String, Box<AExpr>)>)
                  -> Result<(AExpr, u8), String> {
    if let Some(&(_, ref box_aex)) =
        env.iter().rev().find(|&&(ref v, _)| *v == *id) {
            Ok(((**box_aex).clone(), 0))
        }
    else { Err("No binding for var ".to_string() +
               &id + " found") }
}

