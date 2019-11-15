#[derive(Debug,Clone)]
enum Ast {
    BinOp(String, Box<Ast>, Box<Ast>),
    UnOp(String, i32),
}

impl Ast {
    fn new_bin(op: &str, a: &Ast, b: &Ast) -> Ast {
        Ast::BinOp(op.to_string(), Box::new(a.clone()), Box::new(b.clone()))
    }

    fn is_imm(&self) -> bool {
        match self {
            Ast::UnOp(tag, _) => tag == "imm",
            _ => false,
        }
    }

    fn is_arg(&self) -> bool {
        match self {
            Ast::UnOp(tag, _) => tag == "arg",
            _ => false,
        }
    }

    fn is_unop(&self) -> bool {
        self.is_imm() || self.is_arg()
    }

    fn imm(&self) -> i32 {
        match self {
            Ast::UnOp(_, val) => *val,
            _ => unreachable!(),
        }
    }
}

struct Compiler {
}

impl Compiler {
    fn new() -> Compiler {
        Compiler { }
    }

    /// Tokenize program (a string) into a sequence of tokens (a
    /// vector of strings).
    fn tokenize<'a>(&self, program : &'a str) -> Vec<String> {
        let mut tokens : Vec<String> = vec![];
        
        let mut iter = program.chars().peekable();
        loop {
            match iter.peek() {
                Some(&c) => match c {
                    'a'...'z'|'A'...'Z' => {
                        let mut tmp = String::new();
                        while iter.peek().is_some() && iter.peek().unwrap().is_alphabetic() {
                            tmp.push(iter.next().unwrap());
                        }
                        tokens.push(tmp);
                    },
                    '0'...'9' => {
                        let mut tmp = String::new();
                        while iter.peek().is_some() && iter.peek().unwrap().is_numeric() {
                            tmp.push(iter.next().unwrap());
                        }
                        tokens.push(tmp);
                    },
                    ' ' => { iter.next(); },
                    _ => {
                        tokens.push(iter.next().unwrap().to_string());
                    },
                },
                None => break
            }
        }
        
        tokens
    }

    fn compile(&mut self, program : &str) -> Vec<String> {
        let ast = self.pass1(program);
        let ast = self.pass2(&ast);
        self.pass3(&ast)
    }

    /// Pass 1: Translate tokens into AST.
    ///
    /// The parsing algorithm used here is a simple precedence-based
    /// bottom-up parser. It runs in linear time.
    fn pass1(&mut self, program : &str) -> Ast {
        let tokens = self.tokenize(program);
        let mut iter = tokens.iter();
        let mut args = vec![];
        let mut op_stack: Vec<char> = Vec::new();
        let mut ast_stack: Vec<Ast> = Vec::new();

        fn reduce(op: char, ast_stack: &mut Vec<Ast>) {
            let b = ast_stack.pop().unwrap();
            let a = ast_stack.pop().unwrap();
            ast_stack.push(Ast::BinOp(format!("{}", op), Box::new(a), Box::new(b)));
        }

        loop {
            match iter.next() {
                Some(token) => match token.as_ref() {
                    "[" => {
                        // Parse [ variable* ].
                        loop {
                            let arg = iter.next().expect("arg");
                            if arg == "]" {
                                break;
                            } else {
                                args.push(arg);
                            }
                        }
                    }
                    "(" => {
                        op_stack.push('(');
                    }
                    "+" | "-" => {
                        while let Some(op) = op_stack.pop() {
                            if op != '(' {
                                reduce(op, &mut ast_stack);
                            } else {
                                op_stack.push('(');
                                break;
                            }
                        }
                        op_stack.push(token.chars().nth(0).unwrap());
                    }
                    "*" | "/" => {
                        while let Some(op) = op_stack.pop() {
                            if op == '*' || op == '/' {
                                reduce(op, &mut ast_stack);
                            } else {
                                op_stack.push(op);
                                break;
                            }
                        }
                        op_stack.push(token.chars().nth(0).unwrap());
                    }
                    ")" => {
                        while let Some(op) = op_stack.pop() {
                            if op == '(' {
                                break;
                            } else {
                                reduce(op, &mut ast_stack);
                            }
                        }
                    }
                    value => {
                        let ast;
                        if value.chars().nth(0).expect("what").is_numeric() {
                            ast = Ast::UnOp("imm".to_string(), value.parse::<i32>().unwrap());
                        } else {
                            ast = Ast::UnOp("arg".to_string(), args.iter().position(|&x| x==value).unwrap() as i32);
                        }
                        ast_stack.push(ast);
                    }
                }
                None => {
                    break;
                }
            }
        }
        while let Some(op) = op_stack.pop() {
            reduce(op, &mut ast_stack);
        }
        ast_stack.pop().unwrap()
    }

    /// Pass 2: Constant folding.
    ///
    /// * Comm: 1+x = x+1
    /// * Assoc: (...+1)+1 = (...)+(1+1)
    /// * Assoc: (x+1)+x = (x+x)+1
    ///
    /// Note: / and * are tricky. 3*x/4 != 3/4*x = 0
    /// Note: Direction matters.
    fn pass2(&mut self, ast : &Ast) -> Ast {

        #[inline(always)]
        fn plus_like(op: &str) -> bool {
            if op == "+" || op == "-" { true } else { false }
        }

        /// Evaluate (a OP b).
        #[inline(always)]
        fn eval(op: &str, a: i32, b: i32) -> i32 {
            match op {
                "+" => a + b,
                "-" => a - b,
                "*" => a * b,
                "/" => a / b,
                _ => unreachable!(),
            }
        }

        /// Try fold constants.
        #[inline(always)]
        fn opt_fold(ast: &Ast) -> Option<Ast> {
            match ast {
                Ast::BinOp(op,a,b) if a.is_imm() && b.is_imm() => {
                    Some(Ast::UnOp("imm".to_string(), eval(op, a.imm(), b.imm())))
                }
                _ => None
            }
        }

        /// Try apply comm.
        #[inline(always)]
        fn opt_comm(ast: &Ast) -> Option<Ast> {
            match ast {
                Ast::BinOp(op,a,b) if b.is_arg() && a.is_imm() && (op=="+" || op=="*") => {
                    Some(Ast::BinOp(op.clone(), b.clone(), a.clone()))
                }
                Ast::BinOp(op,a,b) if b.is_arg() && a.is_imm() && (op=="-") => {
                    // a-b = (-b)+a
                    Some(Ast::new_bin("+", &Ast::new_bin("*", &b, &Ast::UnOp("imm".to_string(), -1)), &*a))
                }
                _ => None
            }
        }

        /// Try apply assoc. (?+1)+1 = ?+(1+1)
        #[inline(always)]
        fn opt_assoc(ast: &Ast) -> Option<Ast> {
            match ast {
                Ast::BinOp(op1, a1, b1) => {
                    match &**a1  {
                        Ast::BinOp(op2,a2,b2) if b1.is_imm() && b2.is_imm() &&
                            ((plus_like(op1) && plus_like(op2)) || (op1=="*" && op2=="*"))
                        => {
                            Some(Ast::BinOp(op2.clone(), a2.clone(), Box::new(Ast::BinOp(op1.clone(), b2.clone(), b1.clone()))))
                        }
                        _ => None,
                    }
                }
                _ => None
            }
        }

        /// Try apply assoc + comm to put x down. (1+x)+x = (x+x)+1.
        #[inline(always)]
        fn opt_assoc_comm(ast: &Ast) -> Option<Ast> {
            match ast {
                Ast::BinOp(op1, a1, b1) => {
                    match &**a1 {
                        Ast::BinOp(op2, a2, b2) if b1.is_arg() && b2.is_imm() &&
                            ((plus_like(op1) && plus_like(op2)) || (op1 == "*" && op2 == "*"))
                        => {
                            Some(Ast::BinOp(op2.clone(), Box::new(Ast::new_bin(op1, a2, b1)), b2.clone()))
                        }
                        _ => None,
                    }
                }
                _ => None
            }
        }

        /// Move x to the left side.
        #[inline(always)]
        fn opt_move(ast: &Ast) -> Option<Ast> {
            opt_assoc(ast).or(opt_assoc_comm(ast))
        }

        /// 0+x=0
        /// 1*x=1
        /// 0*x=0
        /// (-a)+b=b-a
        #[inline(always)]
        fn opt_algebraic(ast: &Ast) -> Option<Ast> {
            match ast {
                Ast::BinOp(op, a, b) if op == "+" && a.is_imm() && a.imm() == 0 => Some(*b.clone()),
                Ast::BinOp(op, a, b) if op == "+" && b.is_imm() && b.imm() == 0 => Some(*a.clone()),
                Ast::BinOp(op, a, b) if op == "*" && a.is_imm() && a.imm() == 1 => Some(*b.clone()),
                Ast::BinOp(op, a, b) if op == "*" && b.is_imm() && b.imm() == 1 => Some(*a.clone()),
                Ast::BinOp(op, a, _) if op == "*" && a.is_imm() && a.imm() == 0 => Some(*a.clone()),
                Ast::BinOp(op, _, b) if op == "*" && b.is_imm() && b.imm() == 0 => Some(*b.clone()),
                // This shouldn't be put here, because it undoes a-b=(-b)+a in `opt_comm'.
                //
                // Ast::BinOp(op1, a1, b1) if op1 == "+" => {
                //     // a2*(-1) + b1 = b1 - a2
                //     match &**a1 {
                //         Ast::BinOp(op2, a2, b2) if op2 == "*" && b2.is_imm() && b2.imm() == -1 => {
                //             Some(Ast::new_bin("-", &b1, &a2))
                //         }
                //         _ => None,
                //     }
                // }
                _ => None,
            }
        }

        /// Top-level optimizer.
        fn optimize(ast: &Ast) -> Ast {
            match ast {
                Ast::UnOp(_,_) => ast.clone(),
                Ast::BinOp(op,a,b) => {
                    let a = optimize(&*a);
                    let b = optimize(&*b);
                    let this = Ast::new_bin(op,&a,&b);
                    let this = opt_algebraic(&this).unwrap_or(this);
                    let this = opt_comm(&this).unwrap_or(this);
                    let this = match opt_move(&this) {
                        Some(ast) => match ast {
                            Ast::UnOp(_,_) => unreachable!(),
                            Ast::BinOp(op,a,b) => {
                                Ast::new_bin(&op, &*a, &opt_fold(&*b).unwrap_or(*b))
                            }
                        }
                        None => this
                    };
                    let this = opt_fold(&this).unwrap_or(this);
                    opt_algebraic(&this).unwrap_or(this)
                }
            }
        }

        fn normalize(ast: &Ast) -> Ast {
            let ast = match ast {
                Ast::UnOp(_,_) => return ast.clone(),
                Ast::BinOp(op,a,b) => {
                    Ast::new_bin(op, &normalize(a), &normalize(b))
                }
            };
            match &ast {
                Ast::BinOp(op,a,b) if a.is_arg() && b.is_imm() => {
                    Ast::new_bin(op,b,a)
                }
                Ast::BinOp(op1, a1, b1) if op1 == "+" => {
                    // a2*(-1) + b1 = b1 - a2
                    match &**a1 {
                        Ast::BinOp(op2, a2, b2) if op2 == "*" && b2.is_imm() && b2.imm() == -1 => {
                            Ast::new_bin("-", &b1, &a2)
                        }
                        _ => ast.clone()
                    }
                }
                _ => ast.clone()
            }
        }

        normalize(&optimize(ast))
    }

    /// Pass 3: Code generation.
    ///
    /// The stack is used, if and only if the second computation uses more than one register.
    /// UnOp meets this condition.
    ///
    /// This is a rather straightforward translation, and certainly
    /// more optimizations can be employed.
    ///
    /// It can't optimize, e.g., a+a+a (=3a), a*a*a*a (ar 0, sw, ar 0, mu, mu, mu), etc.
    fn pass3(&mut self, ast : &Ast) -> Vec<String> {
        fn opgen(op: &str) -> String {
            match op {
                "+" => "AD".to_string(),
                "-" => "SU".to_string(),
                "*" => "MU".to_string(),
                "/" => "DI".to_string(),
                _ => unreachable!(),
            }
        }
        let mut insts = vec![];
        match ast {
            Ast::UnOp(op, i) if op == "arg" => {
                insts.push(format!("AR {}", i));
            }
            Ast::UnOp(op, x) if op == "imm" => {
                insts.push(format!("IM {}", x));
            }
            Ast::BinOp(op, a, b) if a.is_unop() => {
                let proga = self.pass3(&*a);
                let progb = self.pass3(&*b);
                insts.extend(progb);          // R0 <- B.
                insts.push("SW".to_string()); // R1 <- B.
                insts.extend(proga);          // R0 <- A.
                insts.push(opgen(&op));
            }
            Ast::BinOp(op, a, b) if b.is_unop() => {
                let proga = self.pass3(&*a);
                let progb = self.pass3(&*b);
                insts.extend(proga);          // R0 <- A.
                insts.push("SW".to_string()); // R1 <- A.
                insts.extend(progb);          // R0 <- B.
                insts.push("SW".to_string()); // R0, R1 <- A, B
                insts.push(opgen(&op));
            }
            Ast::BinOp(op, a, b) => {
                let proga = self.pass3(&*a);
                let progb = self.pass3(&*b);
                insts.extend(proga);          // R0 <- A.
                insts.push("PU".to_string()); // PUSH[R0].
                insts.extend(progb);          // R0 <- B.
                insts.push("SW".to_string()); // R1 <- B.
                insts.push("PO".to_string()); // R0 <- A.
                insts.push(opgen(&op));
            }
            _ => unreachable!(),
        }
        insts
    }
}

// TODO: remove this function before submission.
fn main() {
    let mut compiler = Compiler::new();
    let ast1 = compiler.pass1("[ x y z ] (2*3*x)/(2+2)");
    let ast2 = compiler.pass2(&ast1);
    let code = compiler.pass3(&ast2);
    dbg!(&ast1);
    dbg!(&ast2);
    dbg!(&code);
    println!("{}", simulate(code.iter().map(|s| s.as_ref()).collect::<Vec<_>>(), vec![2, 3, 7]));
}


// Kata code. Don't touch.
fn simulate(assembly : Vec<&str>, argv : Vec<i32>) -> i32 {
    let mut r = (0, 0);
    let mut stack : Vec<i32> = vec![];

    for ins in assembly {
        let mut ws = ins.split_whitespace();
        match ws.next() {
            Some("IM") => r.0 = i32::from_str_radix(ws.next().unwrap(), 10).unwrap(),
            Some("AR") => r.0 = argv[i32::from_str_radix(ws.next().unwrap(), 10).unwrap() as usize],
            Some("SW") => r = (r.1,r.0),
            Some("PU") => stack.push(r.0),
            Some("PO") => r.0 = stack.pop().unwrap(),
            Some("AD") => r.0 += r.1,
            Some("SU") => r.0 -= r.1,
            Some("MU") => r.0 *= r.1,
            Some("DI") => r.0 /= r.1,
            _ => panic!("Invalid instruction encountered"),
        }
    }
    r.0
}
