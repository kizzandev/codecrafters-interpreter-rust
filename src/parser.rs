use crate::lexer::{Lexer, Token};

pub type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Clone, PartialEq)]
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

impl LiteralExpr {
    pub fn is_same_type(&self, other: &LiteralExpr) -> bool {
        matches!(
            (self, other),
            (LiteralExpr::Number(_), LiteralExpr::Number(_))
                | (LiteralExpr::StringLiteral(_), LiteralExpr::StringLiteral(_))
                | (LiteralExpr::TRUE, LiteralExpr::TRUE)
                | (LiteralExpr::FALSE, LiteralExpr::FALSE)
                | (LiteralExpr::TRUE, LiteralExpr::FALSE)
                | (LiteralExpr::FALSE, LiteralExpr::TRUE)
                | (LiteralExpr::NIL, LiteralExpr::NIL)
        )
    }

    pub fn is_same_type_weird(&self, other: &LiteralExpr) -> bool {
        // let first;
        // let second;
        if matches!(
            (self, other),
            (LiteralExpr::Number(_), LiteralExpr::StringLiteral(_))
                | (LiteralExpr::StringLiteral(_), LiteralExpr::Number(_))
        ) {
            // first = self.to_string();
            // second = other.to_string();
            // return first == second;
            return true;
        }

        matches!(
            (self, other),
            (LiteralExpr::Number(_), LiteralExpr::Number(_))
                | (LiteralExpr::StringLiteral(_), LiteralExpr::StringLiteral(_))
                | (LiteralExpr::TRUE, LiteralExpr::TRUE)
                | (LiteralExpr::FALSE, LiteralExpr::FALSE)
                | (LiteralExpr::TRUE, LiteralExpr::FALSE)
                | (LiteralExpr::FALSE, LiteralExpr::TRUE)
                | (LiteralExpr::NIL, LiteralExpr::NIL)
        )
    }
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Literal(LiteralExpr),
    Unary(Token<'a>, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Variable(String),
}

impl<'a> Expr<'a> {
    pub fn get_variable(self) -> String {
        match self {
            Expr::Variable(name) => name.to_string(),
            _ => "".to_string(),
        }
    }
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

    Err(String),
}

#[derive(Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(file_contents: &'a str) -> Self {
        Self {
            lexer: Lexer::<'a>::new(file_contents),
        }
    }

    pub fn eval_expr(&mut self) -> Result<Expr<'a>> {
        if self.lexer.peek().is_none() {
            return Ok(Expr::Literal(LiteralExpr::StringLiteral("".to_string())));
        };

        self.expression()
    }

    fn consume_token(&mut self) {
        // eprintln!("CONSUMED: {:?}", self.lexer.next());
        self.lexer.next();
    }

    // fn statement(&mut self, token: (Token, usize)) -> Result<Stmt<'a>> {
    fn statement(&mut self) -> Result<Stmt<'a>> {
        // eprintln!("STATEMENT CALLED: {:?}", self.lexer.peek().unwrap().0);
        match self.lexer.peek().unwrap().0 {
            Token::ReservedKeyword("var") => self.var_declaration(),
            Token::ReservedKeyword("print") => self.print_statement(),
            _ => self.expression_statement(),
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt<'a>> {
        // eprintln!("VAR TOKEN: {:?}", self.consume_token());
        // eprintln!("VAR DECL TOKEN: {:?}", self.lexer.next());
        self.lexer.next();

        let name = match self.lexer.next() {
            Some((Token::Identifier(i), _)) => i.to_string(),
            other => {
                return Err(format!(
                    "expected identifier at {} : {}\nGot {:#?}",
                    self.lexer.get_line(),
                    self.lexer.get_column(),
                    other,
                ));
            }
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
            other => Err(format!(
                "expected semicolon after declaration at {} : {}\nGot {:#?}",
                self.lexer.get_line(),
                self.lexer.get_column(),
                other,
            )),
        }
    }

    fn print_statement(&mut self) -> Result<Stmt<'a>> {
        // eprintln!("PRINT TOKEN: {:?}", self.consume_token());
        self.consume_token();

        let expr = self.expression()?;
        // eprintln!("PRINT EXPR: {:?}", expr);

        // eprintln!("PRINT: {:?}", self.lexer.peek());
        match self.lexer.next() {
            Some((Token::Character(';'), _)) => Ok(Stmt::Print(expr)),
            other => Err(format!(
                "expected semicolon after a print statement at {} : {}\nGot {:#?}",
                self.lexer.get_line(),
                self.lexer.get_column(),
                other,
            )),
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt<'a>> {
        let expr = self.expression()?;

        // eprintln!("EXPR STATAMENT: {:?}", expr);

        match self.lexer.peek() {
            Some((Token::Character(';'), _)) => {
                self.lexer.next();
                Ok(Stmt::Expression(expr))
            }
            other => Err(format!(
                "expected semicolon after an expression at {} : {}\nGot {:#?}",
                self.lexer.get_line(),
                self.lexer.get_column(),
                other,
            )),
        }
    }

    fn expression(&mut self) -> Result<Expr<'a>> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr<'a>> {
        let mut result = self.comparisson()?;
        // eprintln!("EQUALITY: {:?}", result);
        // eprintln!("EQUALITY PEEK: {:?}", self.lexer.peek());

        while let Some((t, _)) = self.lexer.peek() {
            match t {
                Token::Character('=') | Token::Character('!') => {
                    let op = self.lexer.next().unwrap().0;
                    let right = self.comparisson()?;
                    result = Expr::Binary(Box::new(result), op, Box::new(right))
                }
                Token::CharacterDouble(a, b)
                    if matches!(format!("{a}{b}").as_str(), "==" | "!=") =>
                {
                    let op = self.lexer.next().unwrap().0;
                    let right = self.comparisson()?;
                    result = Expr::Binary(Box::new(result), op, Box::new(right))
                }
                _ => break,
            }
        }
        // eprintln!("EQUALITY END: {:?}", result);
        Ok(result)
    }

    fn comparisson(&mut self) -> Result<Expr<'a>> {
        let mut result = self.term()?;

        while let Some((t, _)) = self.lexer.peek() {
            match t {
                Token::Character(c) if matches!(c, '<' | '>') => {
                    let op = self.lexer.next().unwrap().0;
                    let right = self.term()?;
                    result = Expr::Binary(Box::new(result), op, Box::new(right))
                }
                Token::CharacterDouble(a, b)
                    if matches!(format!("{a}{b}").as_str(), ">=" | "<=") =>
                {
                    let op = self.lexer.next().unwrap().0;
                    let right = self.term()?;
                    result = Expr::Binary(Box::new(result), op, Box::new(right))
                }
                _ => break,
            }
        }
        Ok(result)
    }

    fn term(&mut self) -> Result<Expr<'a>> {
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

    fn factor(&mut self) -> Result<Expr<'a>> {
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

    fn unary(&mut self) -> Result<Expr<'a>> {
        if let Some((t, _)) = self.lexer.peek() {
            match t {
                Token::Character(c) if matches!(c, '+' | '-') => {
                    let op = self.lexer.next().unwrap().0;
                    let right = self.unary()?;
                    Ok(Expr::Unary(op, Box::new(right)))
                }
                Token::Character(c) if matches!(c, '!') => {
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

    fn primary(&mut self) -> Result<Expr<'a>> {
        // eprintln!("PRIMARY: {:?}", self.lexer.peek());

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
                Token::Identifier(i) => {
                    self.lexer.next();
                    Ok(Expr::Variable(i.to_string()))
                }

                other => Err(format!(
                    "Error in primary: no match at {} : {}\nGot {:#?}",
                    self.lexer.get_line(),
                    self.lexer.get_column(),
                    other,
                )),
            }
        } else {
            Err("Error in primary: no token found".to_string())
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Stmt<'a>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.lexer.peek().is_none() {
            return None;
        };

        Some(self.statement())
    }
}
