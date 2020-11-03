//! https://www.codewars.com/kata/simple-interactive-interpreter/

use std::collections::HashMap;
use std::iter::Peekable;

struct Interpreter {
    bind: HashMap<String, Bind>,
}

#[derive(Debug)]
enum Token {
    Ident(String),              // identifier
    Fn,                         // 'fn'
    Arrow,                      // =>
    Const(f32),                 // number
    Op(char),                   // + - * / = ( )
    LParen,
    RParen,
}

#[derive(Clone, Debug)]
enum Expr {
    Const(f32),
    BinOp(char, Box<Expr>, Box<Expr>),
    Assign(String, Box<Expr>),
    Call(String, Vec<Box<Expr>>), // Call a function
    Refer(usize),                 // Refer to an argument
    Var(String),                  // Refer to a global variable
}

#[derive(Clone, Debug)]
enum Top {
    Expr(Box<Expr>),
    Define(String, usize, Box<Expr>),
}

#[derive(Clone, Debug)]
enum Bind {
    Val(f32),
    Func(usize, Box<Expr>),
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            bind: HashMap::new()
        }
    }

    fn input(&mut self, input: &str) -> Result<Option<f32>, String> {
        match self.parse(input)? {
            None => Ok(None),
            Some(top) => self.run(&top),
        }
    }

    fn run(&mut self, ast: &Top) -> Result<Option<f32>, String> {
        match ast {
            Top::Expr(expr) => Ok(Some(self.eval(expr, &[])?)),
            Top::Define(func, arity, expr) => {
                if let Some(Bind::Val(_)) = self.bind.get(func) {
                    return Err(format!("{} was already defined as a variable", func));
                }
                self.bind.insert(func.clone(), Bind::Func(*arity, expr.clone()));
                Ok(None)
            }
        }
    }

    //
    // Evaluator
    //
    fn eval(&mut self, expr: &Expr, args: &[f32]) -> Result<f32, String> {
        match expr {
            Expr::Const(v) => Ok(*v),
            Expr::BinOp(op, aexpr, bexpr) => {
                let a = self.eval(aexpr.as_ref(), args)?;
                let b = self.eval(bexpr.as_ref(), args)?;
                let y = match op {
                    '+' => a + b,
                    '-' => a - b,
                    '*' => a * b,
                    '/' | '%' if b == 0.0 => return Err(String::from("divison by zero")),
                    '/' => a / b,
                    '%' => a % b,
                    _ => unreachable!()
                };
                Ok(y)
            }
            Expr::Refer(n) => {
                Ok(args[*n])
            }
            Expr::Call(fname, fargs) => {
                let body = match self.bind.get(fname) {
                    Some(Bind::Func(_, body)) => body.clone(),
                    _ => return Err(format!("Unknown function {}", fname)),
                };
                let mut fargvals = Vec::new();
                for farg in fargs {
                    fargvals.push(self.eval(farg, args)?);
                }
                self.eval(&body, &fargvals)
            }
            Expr::Var(var) => {
                match self.bind.get(var) {
                    Some(Bind::Val(v)) => Ok(*v),
                    _ => return Err(format!("Unknown variable {}", var))
                }
            }
            Expr::Assign(var, expr) => {
                match self.bind.get(var) {
                    Some(Bind::Func(_,_)) => return Err(format!("{} already defined as a function", var)),
                    _ => {
                        let val = self.eval(expr, args)?;
                        self.bind.insert(var.clone(), Bind::Val(val));
                        Ok(val)
                    }
                }
            }
        }
    }

    //
    // Parser (LL(1))
    // Context-sensitive
    //
    fn parse(&self, input: &str) -> Result<Option<Top>, String> {
        let tokens = Self::tokenize(input)?;
        if tokens.len() == 0 {
            return Ok(None)
        }
        Ok(Some(self.parse_top(&mut tokens.into_iter().peekable())?))
    }

    fn parse_top<I>(&self, iter: &mut Peekable<I>) -> Result<Top, String>
    where I: Iterator<Item=Token>
    {
        let res = match iter.peek() {
            Some(Token::Fn) => self.parse_func(iter),
            Some(_) => Ok(Top::Expr(Box::new(self.parse_expr(iter, None)?))),
            None => unreachable!()
        };
        if iter.peek().is_some() {
            return Err(format!("trailing input"));
        } else {
            res
        }
    }

    fn parse_func<I>(&self, iter: &mut Peekable<I>) -> Result<Top, String>
    where I: Iterator<Item=Token>
    {
        iter.next();            // Match fn

        // Function name
        let fname = match iter.next() {
            Some(Token::Ident(id)) => id,
            _ => return Err(format!("expected function name")),
        };

        // Function arguments
        let mut argnames = vec![];
        let mut arity = 0;
        let mut params = HashMap::new();
        while {
            match iter.peek() {
                Some(Token::Ident(_)) => true,
                _ => false
            }
        } {
            match iter.next() {
                Some(Token::Ident(id)) => {
                    argnames.push(id.clone());
                    if let Some(_) = params.get(&id) {
                        return Err(format!("{} is already used", id));
                    }
                    params.insert(id, arity);
                    arity += 1;
                }
                _ => unreachable!()
            }
        }

        // Match =>
        match iter.next() {
            Some(Token::Arrow) => (),
            _ => return Err(format!("expected =>")),
        }

        // Body
        let body = self.parse_expr(iter, Some(&params))?;

        Ok(Top::Define(fname, arity, Box::new(body)))
    }

    // expr ::= term | expr +|- term
    fn parse_expr<I>(&self, iter: &mut Peekable<I>, params: Option<&HashMap<String, usize>>) -> Result<Expr, String>
    where I: Iterator<Item=Token>
    {
        let mut term = self.parse_term(iter, params)?;
        loop {
            let op = match iter.peek() {
                Some(Token::Op('+')) => '+',
                Some(Token::Op('-')) => '-',
                _ => return Ok(term),
            };
            iter.next();
            let b = self.parse_term(iter, params)?;
            term = Expr::BinOp(op, Box::new(term), Box::new(b));
        }
    }

    // term ::= factor | term *|/|% factor
    fn parse_term<I>(&self, iter: &mut Peekable<I>, params: Option<&HashMap<String, usize>>) -> Result<Expr, String>
    where I: Iterator<Item=Token>
    {
        let mut factor = self.parse_factor(iter, params)?;
        loop {
            let op = match iter.peek() {
                Some(Token::Op('*')) => '*',
                Some(Token::Op('/')) => '/',
                Some(Token::Op('%')) => '%',
                _ => return Ok(factor),
            };
            iter.next();
            let b = self.parse_factor(iter, params)?;
            factor = Expr::BinOp(op, Box::new(factor), Box::new(b));
        }
    }

    // factor ::= const | (expr) | assign | var | call 
    fn parse_factor<I>(&self, iter: &mut Peekable<I>, params: Option<&HashMap<String, usize>>) -> Result<Expr, String>
    where I: Iterator<Item=Token>
    {
        match iter.next() {
            Some(Token::LParen) => {
                let expr = self.parse_expr(iter, params)?;
                match iter.next() {
                    Some(Token::RParen) => Ok(expr),
                    _ => return Err(format!("expected )")),
                    
                }
            }
            Some(Token::Const(v)) => Ok(Expr::Const(v)),
            Some(Token::Ident(id)) => {
                // If it's an assignment
                if let Some(Token::Op('=')) = iter.peek() {
                    iter.next();
                    let y = self.parse_expr(iter, params)?;
                    return Ok(Expr::Assign(id, Box::new(y)));
                }
                // If it's an argument
                if params.is_some() {
                    if let Some(n) = params.unwrap().get(&id) {
                        return Ok(Expr::Refer(*n));
                    }
                }
                // It's a global something
                match self.bind.get(&id) {
                    Some(Bind::Func(arity, _)) => {
                        let mut args = vec![];
                        for _ in 0..*arity {
                            args.push(Box::new(self.parse_expr(iter, params)?));
                        }
                        Ok(Expr::Call(id, args))
                    }
                    Some(Bind::Val(_))  => {
                        // NOTE: Forbid references to global variables in a function body
                        // Weird requirement but okay
                        if params.is_some() {
                            return Err(format!("Invalid identifier '{}' in function body.", id))
                        } else {
                            Ok(Expr::Var(id))
                        }
                    }
                    None => {
                        return Err(format!("Unknown identifier {}", id))
                    }
                }
            }
            _ => return Err(format!("expected factor"))
        }
    }
                    
                    
    //
    // Lexer
    //
    fn tokenize(input: &str) -> Result<Vec<Token>, String> {
        let mut tokens = vec![];
        let mut iter = input.chars().peekable();
        loop {
            match iter.peek() {
                None => break,
                Some(&c) => {
                    match c {
                        ' ' => { iter.next(); }
                        '0'..='9' | '.' => {
                            let mut res = String::new();
                            let mut no_point = true;
                            while {
                                match iter.peek() {
                                    Some('.') if no_point => {
                                        no_point = false;
                                        true
                                    }
                                    Some(c) => c.is_numeric(),
                                    _ => false
                                }
                            } {
                                res.push(iter.next().unwrap())
                            }
                            tokens.push(Token::Const(res.parse::<f32>().unwrap()));
                        }
                        'f' => {
                            iter.next();
                            match iter.peek() {
                                Some('n') => {
                                    iter.next();
                                    tokens.push(Token::Fn)
                                }
                                Some(_) => {
                                    let mut ident = String::from("f");
                                    while iter.peek().is_some() && iter.peek().unwrap().is_alphabetic() {
                                        ident.push(iter.next().unwrap());
                                    }
                                    tokens.push(Token::Ident(ident))
                                }
                                None => { tokens.push(Token::Ident(String::from("f"))) }
                            }
                        }
                        'a'..='z' | 'A'..='Z' | '_' => {
                            let mut res = String::new();
                            while {
                                match iter.peek() {
                                    Some('_') => true,
                                    Some('0'..='9') => true,
                                    Some(c) => c.is_alphabetic(),
                                    _ => false
                                }
                            } {
                                res.push(iter.next().unwrap())
                            }
                            tokens.push(Token::Ident(res));
                        }
                        '=' => {
                            iter.next();
                            match iter.peek() {
                                Some('>') => {
                                    iter.next();
                                    tokens.push(Token::Arrow);
                                }
                                _ => {
                                    tokens.push(Token::Op('='));
                                }
                            }
                        }
                        '(' => {
                            iter.next();
                            tokens.push(Token::LParen);
                        }
                        ')' => {
                            iter.next();
                            tokens.push(Token::RParen);
                        }
                        '+' | '-' | '*' | '/' | '%' => {
                            iter.next();
                            tokens.push(Token::Op(c));
                        }
                        _ => {
                            return Err(String::from("syntax error"))
                        }
                    }
                }
            }
        }
        Ok(tokens)
    }
}
