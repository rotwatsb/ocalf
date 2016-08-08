use std::fmt;

#[derive(Debug, Clone)]
pub enum Expr {
    Unit,
    Int(i32),
    Bool(bool),
    Str(String),
    Var(String),
    Op(Box<Expr>, Opcode, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
    Bind(String, Box<Expr>),
    Fun(String, Box<Expr>),
    Tup(Box<Expr>, Box<Expr>),
    Vnt(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Box<Expr>, Box<Expr>)>),
    PUnit,
    PInt(i32),
    PBool(bool),
    PStr(String),
    PVar(String),
    PVnt(String, Box<Expr>),
    PTup(Box<Expr>, Box<Expr>),
}

#[derive(Clone)]
pub enum Opcode {
    Mul,
    Div,
    Add,
    Sub,
    Less,
    Greater,
    Equal,
}

impl fmt::Debug for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match (*self).clone() {
            Opcode::Add => write!(f, "+"),
            Opcode::Sub => write!(f, "-"),
            Opcode::Mul => write!(f, "*"),
            Opcode::Div => write!(f, "/"),
            Opcode::Less => write!(f, "<"),
            Opcode::Greater => write!(f, ">"),
            Opcode::Equal => write!(f, "="),
        }
    }
}
