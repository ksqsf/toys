enum S {
    Atom(char),
    Cons(char, Vec<S>),
}

impl fmt::Display for S {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            S::Atom(i) => write!(f, "{}", i),
            S::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token {
    Atom(char),
    Op(char),
    Eof,
}

struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    fn new(input: &str) -> Lexer {
        let mut tokens = input
            .chars()
            .filter(|it| !it.is_ascii_whitespace())
            .map(|c| match c {
                '0'..='9'
                | 'a'..='z' | 'A'..='Z' => Token::Atom(c),
                _ => Token::Op(c),
            })
            .collect::<Vec<_>>();
        tokens.reverse();
        Lexer { tokens }
    }

    fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }
    fn peek(&mut self) -> Token {
        self.tokens.last().copied().unwrap_or(Token::Eof)
    }
}

fn expr(input: &str) -> S {
    let mut lexer = Lexer::new(input);
    expr_bp(&mut lexer, 0)
}

fn prefix_binding_power(op: char) -> ((), u8) {
    match op {
        '+' | '-' => ((), 9),
        _ => panic!("bad op: {:?}", op),
    }
}

fn postfix_binding_power(op: char) -> Option<(u8, ())> {
    let res = match op {
        '!' => (11, ()),
        '[' => (11, ()),
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(op: char) -> Option<(u8, u8)> {
    let res = match op {
        '=' => (2, 1),
        '?' => (4, 3),
        '+' | '-' => (5, 6),
        '*' | '/' => (7, 8),
        '.' => (14, 13),
        _ => return None,
    };
    Some(res)
}

fn expr_bp(lexer: &mut Lexer, min_bp: u8) -> Option<S> {
    if min_bp == 100 {
        return None;
    }
    let mut lhs = match lexer.peek() {
        Token::Atom(it) => {
            lexer.next();
            Some(S::Atom(it))
        }
        Token::Op('(') => {
            lexer.next();
            let lhs = expr_bp(lexer, 0);
            assert_eq!(lexer.next(), Token::Op(')'));
            Some(lhs)
        }
        _ => None,
    };

    loop {
        let op = match lexer.peek() {
            Token::Eof => break,
            Token::Op(op) => op,
            t => panic!("bad token: {:?}", t),
        };

        if let Some((l_bp, r_bp)) = binding_power(op, lhs.is_none()) {
            if l_bp < min_bp {
                break;
            }
            lexer.next();

            let rhs = expr_bp(lexer, r_bp);
            let mut args = Vec::new();
            args.push(lhs);
            args.extend(rhs);
            lhs = S::Cons(op, args);
            continue;
        }

        break;
    }

    Some(lhs)
}

fn binding_power(op: char, prefix: bool) -> Option<(u8, u8)> {
    let res = match op {
        '=' => (2,1),
        '+' | '-' if prefix => (99, 9),
        '+' | '-' => (5,6),
        '*' | '/' => (7, 8),
        '!' => (11, 100),
        '.' => (14, 13),
        _ => return None,
    };
    Some(res)
}
