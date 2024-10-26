use crate::lexer::{Lexer, Token};

pub type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Clone)]
pub enum LiteralExpr {
    Number(f64),
    StringLiteral(String),
    TRUE,
    FALSE,
    NIL,
}

impl std::fmt::Display for LiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralExpr::Number(n) => write!(f, "{n}"),
            LiteralExpr::StringLiteral(s) => write!(f, "{s}"),
            LiteralExpr::TRUE => write!(f, "true"),
            LiteralExpr::FALSE => write!(f, "false"),
            LiteralExpr::NIL => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum Expr<'a> {
    Literal(LiteralExpr),
    Unary(Token<'a>, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Variable(String),
}

pub fn print_expr(expr: &Expr) -> String {
    match expr {
        Expr::Literal(l) => l.to_string(),
        Expr::Unary(op, right) => format!("({op} {})", print_expr(right.as_ref())),
        Expr::Binary(left, op, right) => {
            format!(
                "({} {} {})",
                op,
                print_expr(left.as_ref()),
                print_expr(right.as_ref())
            )
        }
        Expr::Grouping(expr) => format!("(group {})", print_expr(expr.as_ref())),
        Expr::Variable(name) => format!("(var {})", name),
    }
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    Print(Expr<'a>),
    Var(String, Option<Expr<'a>>),
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(file_contents: &'a str) -> Self {
        Self {
            lexer: Lexer::<'a>::new(file_contents),
        }
    }

    pub fn parse(&'a mut self) -> Result<Vec<Stmt>> {
        let mut stms = Vec::new();

        while let Some(token) = self.lexer.next() {
            match token.0 {
                _ => stms.push(self.statement(token.clone())?),
            }
        }

        Ok(stms)
    }

    fn statement(&mut self, token: (Token, usize)) -> Result<Stmt> {
        match token.0 {
            Token::ReservedKeyword("var") => {
                self.lexer.next();
                self.var_declaration()
            }
            Token::ReservedKeyword("print") => {
                self.lexer.next();
                self.print_statement()
            }
            _ => self.expression_statement(), // _ => Ok(Stmt::Expression(Expr::Literal(LiteralExpr::NIL))),
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = match self.lexer.next() {
            Some((Token::Identifier(i), _)) => i.to_string(),
            _ => return Err("expected identifier".to_string()),
        };

        let initializer = match self.lexer.peek() {
            Some((Token::Character('='), _)) => {
                self.lexer.next();
                Some(self.expression()?)
            }
            _ => None,
        };

        match self.lexer.next() {
            Some((Token::Character(';'), _)) => Ok(Stmt::Var(name, initializer)),
            _ => Err("expected semicolon".to_string()),
        }
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;

        match self.lexer.next() {
            Some((Token::Character(';'), _)) => Ok(Stmt::Print(expr)),
            _ => Err("expected semicolon".to_string()),
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;

        if let Some((Token::Character(';'), _)) = self.lexer.peek() {
            self.lexer.next();
            Ok(Stmt::Expression(expr))
        } else {
            Err("expected semicolon".to_string())
        }
    }

    fn expression(&mut self) -> Result<Expr> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr> {
        let mut result = self.comparisson()?;

        while let Some((t, _)) = self.lexer.peek() {
            match t {
                Token::Character('=') | Token::Character('!') => {
                    let op = self.lexer.next().unwrap().0;
                    let right = self.comparisson()?;
                    result = Expr::Binary(Box::new(result), op, Box::new(right))
                }
                _ => break,
            }
        }
        Ok(result)
    }

    fn comparisson(&mut self) -> Result<Expr> {
        let mut result = self.term()?;

        while let Some((t, _)) = self.lexer.peek() {
            match t {
                Token::Character(c) if matches!(c, '<' | '>') => {
                    let op = self.lexer.next().unwrap().0;
                    let right = self.term()?;
                    result = Expr::Binary(Box::new(result), op, Box::new(right))
                }
                _ => break,
            }
        }
        Ok(result)
    }

    fn term(&mut self) -> Result<Expr> {
        let mut result = self.factor()?;

        while let Some((t, _)) = self.lexer.peek() {
            match t {
                Token::Character(c) if matches!(c, '+' | '-') => {
                    let op = self.lexer.next().unwrap().0;
                    let right = self.factor()?;
                    result = Expr::Binary(Box::new(result), op, Box::new(right))
                }
                _ => break,
            }
        }
        Ok(result)
    }

    fn factor(&mut self) -> Result<Expr> {
        let mut result = self.unary()?;

        while let Some((t, _)) = self.lexer.peek() {
            match t {
                Token::Character(c) if matches!(c, '*' | '/') => {
                    let op = self.lexer.next().unwrap().0;
                    let right = self.unary()?;
                    result = Expr::Binary(Box::new(result), op, Box::new(right))
                }
                _ => break,
            }
        }
        Ok(result)
    }

    fn unary(&mut self) -> Result<Expr> {
        if let Some((t, _)) = self.lexer.peek() {
            match t {
                Token::Character(c) if matches!(c, '+' | '-') => {
                    let op = self.lexer.next().unwrap().0;
                    let right = self.unary()?;
                    Ok(Expr::Unary(op, Box::new(right)))
                }
                _ => self.primary(),
            }
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr> {
        if let Some((t, _)) = self.lexer.peek() {
            match t {
                Token::Number(n) => {
                    self.lexer.next();
                    Ok(Expr::Literal(LiteralExpr::Number(match n.0.parse() {
                        Ok(n) => n,
                        Err(_) => {
                            eprintln!("Error: Invalid number.");
                            panic!("Invalid number.");
                        }
                    })))
                }
                Token::StringLiteral(s) => {
                    self.lexer.next();
                    Ok(Expr::Literal(LiteralExpr::StringLiteral(s.to_string())))
                }
                Token::ReservedKeyword("true") => {
                    self.lexer.next();
                    Ok(Expr::Literal(LiteralExpr::TRUE))
                }
                Token::ReservedKeyword("false") => {
                    self.lexer.next();
                    Ok(Expr::Literal(LiteralExpr::FALSE))
                }
                Token::ReservedKeyword("nil") => {
                    self.lexer.next();
                    Ok(Expr::Literal(LiteralExpr::NIL))
                }
                Token::Character('(') => {
                    self.lexer.next();
                    let expr = self.expression()?;
                    if let Some((Token::Character(')'), _)) = self.lexer.peek() {
                        self.lexer.next();
                        Ok(expr)
                    } else {
                        eprintln!("Error: Unmatched parentheses.");
                        Err("Error: Unmatched parentheses.".to_string())
                    }
                }
                Token::Identifier(i) => Ok(Expr::Variable(i.to_string())),
                _ => Err("Error in primary: no match".to_string()),
            }
        } else {
            Err("Error in primary: no token found".to_string())
        }
    }
}
