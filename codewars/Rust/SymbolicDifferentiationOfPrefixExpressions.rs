//! https://www.codewars.com/kata/584daf7215ac503d5a0001ae

use std::rc::Rc;
use std::str;

#[derive(Debug, PartialEq)]
enum Expr {
    X,
    Num(f64),
    Add(Rc<Expr>, Rc<Expr>),
    Sub(Rc<Expr>, Rc<Expr>),
    Mul(Rc<Expr>, Rc<Expr>),
    Div(Rc<Expr>, Rc<Expr>),
    Pow(Rc<Expr>, Rc<Expr>),
    Cos(Rc<Expr>),
    Sin(Rc<Expr>),
    Tan(Rc<Expr>),
    Exp(Rc<Expr>),
    Ln(Rc<Expr>),
}
use Expr::*;


impl Expr {
    fn is_const(self: &Expr) -> bool {
        match self {
            Add(a, b) => a.is_const() && b.is_const(),
            Sub(a, b) => a.is_const() && b.is_const(),
            Mul(a, b) => a.is_const() && b.is_const(),
            Div(a, b) => a.is_const() && b.is_const(),
            Pow(a, b) => a.is_const() && b.is_const(),
            Sin(u) => u.is_const(),
            Cos(u) => u.is_const(),
            Tan(u) => u.is_const(),
            Exp(u) => u.is_const(),
            Ln(u) => u.is_const(),
            X => false,
            Num(_) => true,
        }
    }

    fn diff(self: &Rc<Expr>) -> Rc<Expr> {
        Rc::new(match &**self {
            X => Num(1.0),
            Num(_) => Num(0.0),
            Add(a, b) => Add(a.diff(), b.diff()),
            Sub(a, b) => Sub(a.diff(), b.diff()),
            Mul(a, b) => Add(
                Rc::new(Mul(a.diff(), b.clone())),
                Rc::new(Mul(a.clone(), b.diff()))
            ),
            Div(a, b) => Div(
                Rc::new(Sub(Rc::new(Mul(a.diff(), b.clone())),
                            Rc::new(Mul(a.clone(), b.diff())))),
                Rc::new(Pow(b.clone(), Rc::new(Num(2.0)))),
            ),
            Pow(a, b) => {
                if b.is_const() {
                    let t = Pow(a.clone(), Rc::new(Sub(b.clone(), Rc::new(Num(1.0)))));
                    Mul(Rc::new(Mul(b.clone(), Rc::new(t))), a.diff())
                } else {
                    let u = Mul(b.diff(), Rc::new(Ln(a.clone())));
                    let v = Div(b.clone(), a.clone());
                    let s = Add(Rc::new(u), Rc::new(v));
                    Mul(self.clone(), Rc::new(s))
                }
            },
            Cos(u) => Mul(Rc::new(Mul(Rc::new(Num(-1.0)), Rc::new(Sin(u.clone())))),
                          u.diff()),
            Sin(u) => Mul(Rc::new(Cos(u.clone())), u.diff()),
            Tan(u) => Mul(Rc::new(Add(Rc::new(Num(1.0)), Rc::new(Pow(self.clone(), Rc::new(Num(2.0)))))), u.diff()),
            Exp(u) => Mul(Rc::new(Exp(u.clone())), u.diff()),
            Ln(u) => Div(u.diff(), u.clone()),
        })
    }

    // Ok, I kinda hate Rust now...
    fn simplify_step(self: &Rc<Expr>) -> Rc<Expr> {
        match &**self {
            Add(a, b) => {
                match (&**a, &**b) {
                    (Num(a), Num(b)) => Rc::new(Num(a+b)),
                    (Num(0.0), _) => b.clone(),
                    (_, Num(0.0)) => a.clone(),
                    _ => self.clone(),
                }
            }
            Sub(a, b) => {
                match (&**a, &**b) {
                    (Num(a), Num(b)) => Rc::new(Num(a-b)),
                    (_, Num(0.0)) => a.clone(),
                    _ => self.clone(),
                }
            }
            Mul(a, b) => {
                match (&**a, &**b) {
                    (Num(a), Num(b)) => Rc::new(Num(a*b)),
                    (Num(0.0), _) => Rc::new(Num(0.0)),
                    (_, Num(0.0)) => Rc::new(Num(0.0)),
                    (Num(1.0), _) => b.clone(),
                    (_, Num(1.0)) => a.clone(),
                    (_, Num(_)) => Rc::new(Mul(b.clone(), a.clone())),
                    _ => self.clone(),
                }
            }
            Div(a, b) => {
                match (&**a, &**b) {
                    (Num(a), Num(b)) if *b != 0.0 => Rc::new(Num(a/b)),
                    (Num(0.0), b) => Rc::new(Num(0.0)),
                    _ => self.clone(),
                }
            }
            Pow(a, b) => {
                match (&**a, &**b) {
                    (Num(a), Num(b)) => Rc::new(Num(a.powf(*b))),
                    (Num(1.0), b) => Rc::new(Num(1.0)),
                    (Num(0.0), b) => Rc::new(Num(0.0)),
                    (_, Num(0.0)) => Rc::new(Num(0.0)),
                    (_, Num(1.0)) => a.clone(),
                    _ => self.clone(),
                }
            }
            _ => self.clone(),
        }
    }

    fn simplify(self: &Rc<Expr>) -> Rc<Expr> {
        match &**self {
            Add(a,b) => {
                Rc::new(Add(a.simplify(), b.simplify())).simplify_step()
            }
            Sub(a,b) => {
                Rc::new(Sub(a.simplify(), b.simplify())).simplify_step()
            }
            Mul(a,b) => {
                Rc::new(Mul(a.simplify(), b.simplify())).simplify_step()
            }
            Div(a,b) => {
                Rc::new(Div(a.simplify(), b.simplify())).simplify_step()
            }
            Pow(a,b) => {
                Rc::new(Pow(a.simplify(), b.simplify())).simplify_step()
            }
            Cos(u) => {
                Rc::new(Cos(u.simplify())).simplify_step()
            }
            Sin(u) => {
                Rc::new(Sin(u.simplify())).simplify_step()
            }
            Tan(u) => {
                Rc::new(Tan(u.simplify())).simplify_step()
            }
            Exp(u) => {
                Rc::new(Exp(u.simplify())).simplify_step()
            }
            Ln(u) => {
                Rc::new(Ln(u.simplify())).simplify_step()
            }
            _ => self.clone(),
        }
    }
}

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            X => format!("x"),
            Num(n) => format!("{}", *n),
            Add(a,b) => format!("(+ {} {})", a.to_string(), b.to_string()),
            Sub(a,b) => format!("(- {} {})", a.to_string(), b.to_string()),
            Mul(a,b) => format!("(* {} {})", a.to_string(), b.to_string()),
            Div(a,b) => format!("(/ {} {})", a.to_string(), b.to_string()),
            Pow(a,b) => format!("(^ {} {})", a.to_string(), b.to_string()),
            Exp(u) => format!("(exp {})", u.to_string()),
            Sin(u) => format!("(sin {})", u.to_string()),
            Cos(u) => format!("(cos {})", u.to_string()),
            Tan(u) => format!("(tan {})", u.to_string()),
            Ln(u) => format!("(ln {})", u.to_string()),
        }
    }
}

fn build_op<T>(s: &[u8], op: T, start: usize) -> (Option<Expr>, usize)
where T: Fn(Rc<Expr>, Rc<Expr>) -> Expr
{
    let mut i = 0;
    while s[i] == b' ' { i += 1; }
    let (maybe_a, len1) = (parse(&s[i..]));
    i += len1;
    while s[i] == b' ' { i += 1; }
    let (maybe_b, len2) = (parse(&s[i..]));
    i += len2;
    (maybe_a.and_then(
        |a| maybe_b.and_then(
            |b| Some(op(Rc::new(a), Rc::new(b))))),
     start+i+1)
}

fn build_unop<T>(s: &[u8], op: T, start: usize) -> (Option<Expr>, usize)
where T: Fn(Rc<Expr>) -> Expr
{
    let mut i = 0;
    while s[i] == b' ' { i += 1; }
    let (maybe_a, len1) = (parse(&s[i..]));
    i += len1;
    (maybe_a.and_then(|a| Some(op(Rc::new(a)))), start+i+1)
}

fn parse(s: &[u8]) -> (Option<Expr>, usize) {
    if s.len() == 0 {
        return (None, 0)
    }
    let mut j = 0;
    while s[j] == b' ' {
        j += 1;
    }
    match s[j] {
        b'(' => {
            match s[j+1] {
                b'+' => build_op(&s[j+2..], Add, j+2),
                b'-' => build_op(&s[j+2..], Sub, j+2),
                b'*' => build_op(&s[j+2..], Mul, j+2),
                b'/' => build_op(&s[j+2..], Div, j+2),
                b'^' => build_op(&s[j+2..], Pow, j+2),
                b's' => build_unop(&s[j+4..], Sin, j+4),
                b'c' => build_unop(&s[j+4..], Cos, j+4),
                b't' => build_unop(&s[j+4..], Tan, j+4),
                b'e' => build_unop(&s[j+4..], Exp, j+4),
                b'l' => build_unop(&s[j+3..], Ln , j+3),
                _ => unreachable!("parse op"),
            }
        }
        b'0'..=b'9' | b'.' => {
            let mut i = j;
            while i < s.len() && ((s[i] >= b'0' && s[i] <= b'9') || s[i] == b'.') {
                i += 1;
            }
            (Some(Num(str::from_utf8(&s[j..i]).unwrap().parse::<f64>().unwrap())), i)
        }
        b'-' => {
            let mut i = j+1;
            while i < s.len() && ((s[i] >= b'0' && s[i] <= b'9') || s[i] == b'.') {
                i += 1;
            }
            let num = str::from_utf8(&s[j..i]).unwrap().parse::<f64>().unwrap();
            (Some(Num(num)), i)
        }
        b'x' => {
            (Some(X), j+1)
        }
        _ => unreachable!("parse first char {} {}", j, s[j] as char),
    }
}

fn diff(expr: &str) -> String {
    let expr = Rc::new(parse(expr.as_bytes()).0.unwrap());
    println!("EXPR: {:?}", expr);
    expr.diff().simplify().to_string()
}

fn main() {
    let e = Rc::new(Mul(Rc::new(X), Rc::new(Num(2.0))));
    println!("{}", e.to_string());
    println!("{}", e.diff().to_string());
    println!("{}", diff("(+ x x)"));
    println!("{}", diff("(- x x)"));
    println!("{}", diff("(* x 2)"));
    println!("{}", diff("(/ x 2)"));
    println!("{}", diff("(sin (cos x))"));
    println!("{}", diff("(ln x)"));
    println!("{}", diff("(cos x)"));

    assert_eq!(diff("5"), "0");
    assert_eq!(diff("x"), "1");
    assert_eq!(diff("5"), "0");
    assert_eq!(diff("(+ x x)"), "2");
    assert_eq!(diff("(- x x)"), "0");
    assert_eq!(diff("(* x 2)"), "2");
    assert_eq!(diff("(/ x 2)"), "0.5");
    assert_eq!(diff("(^ x 2)"), "(* 2 x)");
    assert_eq!(diff("(cos x)"), "(* -1 (sin x))");
    assert_eq!(diff("(sin x)"), "(cos x)");
    assert_eq!(diff("(tan x)"), "(+ 1 (^ (tan x) 2))");
    assert_eq!(diff("(exp x)"), "(exp x)");
    assert_eq!(diff("(ln x)"), "(/ 1 x)");
    assert_eq!(diff("(+ x (+ x x))"), "3");
    assert_eq!(diff("(- (+ x x) x)"), "1");
    assert_eq!(diff("(* 2 (+ x 2))"), "2");
    assert_eq!(diff("(/ 2 (+ 1 x))"), "(/ -2 (^ (+ 1 x) 2))");
    assert_eq!(diff("(cos (+ x 1))"), "(* -1 (sin (+ x 1)))");

    let result = diff("(cos (* 2 x))");
    assert!(
        result == "(* 2 (* -1 (sin (* 2 x))))"
            || result == "(* -2 (sin (* 2 x)))"
            || result == "(* (* -1 (sin (* 2 x))) 2)"
    );

    assert_eq!(diff("(sin (+ x 1))"), "(cos (+ x 1))");
    assert_eq!(diff("(sin (* 2 x))"), "(* 2 (cos (* 2 x)))");
    assert_eq!(diff("(tan (* 2 x))"), "(* 2 (+ 1 (^ (tan (* 2 x)) 2)))");
    assert_eq!(diff("(exp (* 2 x))"), "(* 2 (exp (* 2 x)))");
    assert_eq!(diff(&diff("(sin x)")), "(* -1 (sin x))");
    assert_eq!(diff(&diff("(exp x)")), "(exp x)");
    let result = diff(&diff("(^ x 3)"));
    assert!(result == "(* 3 (* 2 x))" || result == "(* 6 x)");
}
