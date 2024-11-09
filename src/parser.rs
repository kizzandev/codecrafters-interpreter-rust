use crate::lexer::{Lexer, Token};

pub type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOp {
    Or,
    And,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralExpr {
    Number(f64),
    StringLiteral(String),
    TRUE,
    FALSE,
    NIL,

    EOF,
}

impl std::fmt::Display for LiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralExpr::Number(n) => write!(f, "{}", parse_number(n)),
            LiteralExpr::StringLiteral(s) => write!(f, "{s}"),
            LiteralExpr::TRUE => write!(f, "true"),
            LiteralExpr::FALSE => write!(f, "false"),
            LiteralExpr::NIL => write!(f, "nil"),
            LiteralExpr::EOF => write!(f, "EOF"),
        }
    }
}

fn parse_number(n: &f64) -> String {
    let n_raw = n.to_string();
    if n_raw.contains('.') {
        let n_raw = n_raw.trim_end_matches('0').trim_end_matches('.');
        if n_raw.contains('.') {
            n_raw.to_string()
        } else {
            format!("{n_raw}.0")
        }
    } else {
        format!("{n_raw}.0")
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

    pub fn is_number_and_parsable(&self, other: &LiteralExpr) -> bool {
        if matches!(
            (self, other),
            (LiteralExpr::Number(_), LiteralExpr::StringLiteral(_))
        ) {
            return other.is_parsable();
        }

        if matches!(
            (self, other),
            (LiteralExpr::StringLiteral(_), LiteralExpr::Number(_))
        ) {
            return self.is_parsable();
        }

        false
    }

    fn is_parsable(&self) -> bool {
        match self.to_string().parse::<f64>() {
            Err(_) => false,
            Ok(_) => true,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<'a> {
    Literal(LiteralExpr),
    Unary(Token<'a>, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, Token<'a>, Box<Expr<'a>>),
    Grouping(Box<Expr<'a>>),
    Variable(String),
    Logical(LogicalOp),
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
        _ => format!(""),
    }
}

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    Print(Box<Stmt<'a>>),
    Var(String, Option<Box<Stmt<'a>>>),
    VarDecl(String, Option<Box<Stmt<'a>>>),
    Block(Vec<Stmt<'a>>),
    If(Box<Stmt<'a>>, Box<Stmt<'a>>, Option<Box<Stmt<'a>>>),
    BinaryStatement(Box<Stmt<'a>>, Token<'a>, Box<Stmt<'a>>),
    While(Box<Stmt<'a>>, Box<Stmt<'a>>),
    // for (var a = 1; a <= 10; a = a + 1) {} ???
    For(Box<Stmt<'a>>, Box<Stmt<'a>>, Box<Stmt<'a>>, Box<Stmt<'a>>),
}

impl<'a> Stmt<'a> {
    pub fn get_expression(&self) -> Option<Expr<'a>> {
        match self {
            Stmt::Expression(expr) => Some(expr.clone()),
            Stmt::Var(_, Some(expr)) => {
                let val = *expr.clone();
                Some(val.get_expression()?.clone())
            }
            _ => None,
        }
    }
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
            return Ok(Expr::Literal(LiteralExpr::EOF));
        };

        self.expression()
    }

    pub fn parse_code(&mut self) -> Result<Expr<'a>> {
        if self.lexer.peek().is_none() {
            return Ok(Expr::Literal(LiteralExpr::EOF));
        };

        self.expression()
    }

    fn syntax_error(&self, what: &str) -> Result<Stmt<'a>> {
        Err(format!("Syntax Error: {what}."))
    }

    fn consume_token(&mut self) {
        // eprintln!("CONSUMED: {:?}", self.lexer.next());
        self.lexer.next();
    }

    fn statement(&mut self) -> Result<Stmt<'a>> {
        // eprintln!("STATEMENT CALLED: {:?}", self.lexer.peek().unwrap().0);
        let res = match self.lexer.peek().unwrap().0 {
            Token::ReservedKeyword("var") => self.var_declaration(),
            Token::ReservedKeyword("print") => self.print_statement(),
            Token::ReservedKeyword("if") => self.if_statement(),
            Token::ReservedKeyword("while") => self.while_statement(),
            Token::ReservedKeyword("for") => self.for_statement(),
            _ => self.expression_statement(),
        };

        // eprintln!("STATEMENT RES: {:?}", res);
        match res {
            Ok(Stmt::Block(_)) => res,
            Ok(Stmt::If(_, _, _)) => res,
            Ok(Stmt::While(_, _)) => res,
            Ok(Stmt::For(_, _, _, _)) => res,

            Err(e) => Err(e),
            _ => match self.lexer.next() {
                Some((Token::Character(';'), _)) => res,
                other => Err(format!(
                    "expected semicolon at the end of an statement, at {} : {}\nGot {:#?}",
                    self.lexer.get_line(),
                    self.lexer.get_column(),
                    other,
                )),
            },
        }
    }

    fn while_statement(&mut self) -> Result<Stmt<'a>> {
        self.consume_token(); // while

        match self.lexer.peek() {
            Some((Token::Character('('), _)) => self.consume_token(), // (
            _ => {
                return Err(format!(
                    "Missing parethesis '(' at {} : {}",
                    self.lexer.get_line(),
                    self.lexer.get_column()
                ))
            }
        }

        let condition = self.generate_condition()?;
        let condition = Box::new(condition);

        match self.lexer.peek() {
            Some((Token::Character(')'), _)) => self.consume_token(), // )
            _ => {
                return Err(format!(
                    "Missing parethesis ')' at {} : {}",
                    self.lexer.get_line(),
                    self.lexer.get_column()
                ))
            }
        }

        let stmt = self.statement()?;

        // TEMP
        // Ok(Stmt::Expression(Expr::Literal(LiteralExpr::NIL)))

        // eprintln!("WHILE {:?}\nDO {:?}", *condition, stmt);
        Ok(Stmt::While(condition, Box::new(stmt)))
    }

    fn for_statement(&mut self) -> Result<Stmt<'a>> {
        self.consume_token(); // for

        match self.lexer.peek() {
            Some((Token::Character('('), _)) => self.consume_token(), // (
            _ => {
                return Err(format!(
                    "Missing parethesis '(' at {} : {}",
                    self.lexer.get_line(),
                    self.lexer.get_column()
                ))
            }
        }

        let mut init = Stmt::Expression(Expr::Literal(LiteralExpr::NIL));
        match self.lexer.peek() {
            Some((Token::Character(';'), _)) => self.consume_token(), // ;
            _ => {
                init = self.statement()?;
            }
        }

        let condition = self.generate_condition()?;

        match self.lexer.peek() {
            Some((Token::Character(';'), _)) => self.consume_token(), // ;
            _ => {
                return Err(format!(
                    "Missing semicolon ';' at {} : {}",
                    self.lexer.get_line(),
                    self.lexer.get_column()
                ))
            }
        }

        let mut increment = Stmt::Expression(Expr::Literal(LiteralExpr::NIL));

        match self.lexer.peek() {
            Some((Token::Character(')'), _)) => {
                self.consume_token(); // )
            }
            _ => {
                increment = self.expression_statement()?;
                // eprintln!("INC: {:?}", increment);
                match increment {
                    Stmt::Expression(_) | Stmt::Var(_, _) => {}
                    _ => return self.syntax_error("Expected an Expression"),
                };

                match self.lexer.peek() {
                    Some((Token::Character(')'), _)) => self.consume_token(), // )
                    _ => {
                        return Err(format!(
                            "Missing parethesis ')' at {} : {}",
                            self.lexer.get_line(),
                            self.lexer.get_column()
                        ))
                    }
                }
            }
        }

        let stmt = self.statement()?;

        Ok(Stmt::For(
            Box::new(init),
            Box::new(condition),
            Box::new(increment),
            Box::new(stmt),
        ))
    }

    fn generate_condition(&mut self) -> Result<Stmt<'a>> {
        let invert_truth = match self.lexer.peek() {
            Some((Token::Character('!'), _)) => {
                self.consume_token(); // !
                true
            }
            _ => false,
        };

        let condition = self.expression_statement()?;
        // eprintln!("CONDITION: {:?}", condition);

        let var_name = match &condition {
            Stmt::Var(name, _) => name.to_string(),
            _ => "".to_string(),
        };

        let mut condition: Expr<'a> = condition
            .get_expression()
            .unwrap_or(Expr::Literal(LiteralExpr::NIL));

        if condition == Expr::Literal(LiteralExpr::NIL) {
            return self.syntax_error("Invalid condition");
        }

        if invert_truth {
            condition = Expr::Unary(Token::Character('!'), Box::new(condition));
        }
        let mut condition = Stmt::Expression(condition);
        if !var_name.is_empty() {
            condition = Stmt::Var(var_name, Some(Box::new(condition)));
        }
        Ok(condition)
    }

    fn if_statement(&mut self) -> Result<Stmt<'a>> {
        self.consume_token(); // if

        match self.lexer.peek() {
            Some((Token::Character('('), _)) => self.consume_token(), // (
            _ => {
                return Err(format!(
                    "Missing parethesis '(' at {} : {}",
                    self.lexer.get_line(),
                    self.lexer.get_column()
                ))
            }
        }

        let condition = self.generate_condition()?;
        let condition = Box::new(condition);

        match self.lexer.peek() {
            Some((Token::Character(')'), _)) => self.consume_token(), // )
            _ => {
                return Err(format!(
                    "Missing parethesis ')' at {} : {}",
                    self.lexer.get_line(),
                    self.lexer.get_column()
                ))
            }
        }

        let stmt = self.statement()?;

        // ELSE BLOCK
        let has_else = match self.lexer.peek() {
            Some((Token::ReservedKeyword("else"), _)) => true,
            _ => false,
        };

        let mut else_block: Option<Box<Stmt<'a>>> = None;
        if has_else {
            self.consume_token(); // else
            let other = self.statement()?;
            else_block = Some(Box::new(other));
        }

        Ok(Stmt::If(condition, Box::new(stmt), else_block))
    }

    fn var_declaration(&mut self) -> Result<Stmt<'a>> {
        self.consume_token();

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
                Some(Box::new(self.expression_statement()?))
            }
            _ => None,
        };

        Ok(Stmt::VarDecl(name, initializer))
    }

    fn print_statement(&mut self) -> Result<Stmt<'a>> {
        self.consume_token();
        let expr_stmt = self.expression_statement()?;
        Ok(Stmt::Print(Box::new(expr_stmt)))
    }

    fn block_statement(&mut self) -> Result<Stmt<'a>> {
        let mut statements = Vec::new();
        self.consume_token(); // '{'

        while let Some((token, _)) = self.lexer.peek() {
            if matches!(token, Token::Character('}')) {
                break;
            }
            statements.push(self.statement()?);
        }

        if let Some((Token::Character('}'), _)) = self.lexer.next() {
            Ok(Stmt::Block(statements))
        } else {
            Err(format!(
                "expected closing braces '}}' for block at {} : {}",
                self.lexer.get_line(),
                self.lexer.get_column(),
            ))
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt<'a>> {
        match self.lexer.peek() {
            None => {
                return Err(format!(
                    "expected semicolon after an expression at {} : {}",
                    self.lexer.get_line(),
                    self.lexer.get_column()
                ));
            }
            Some((Token::Character('{'), _)) => self.block_statement(),
            other => {
                let mut has_stmt = false;
                if other.is_some() && other.unwrap().0 == Token::Character('(') {
                    let temp = self.lexer.clone();
                    while let Some((token, _)) = self.lexer.next() {
                        match token {
                            Token::Character(')') => {
                                self.lexer = temp;
                                break;
                            }
                            Token::Character('=') => {
                                self.lexer = temp;
                                has_stmt = true;
                                break;
                            }
                            _ => continue,
                        }
                    }
                };

                if has_stmt {
                    self.consume_token(); // (
                    let result = self.expression_statement()?;
                    match self.lexer.peek() {
                        Some((Token::Character(')'), _)) => self.consume_token(), // )
                        _ => return Err("Error: Unmatched parentheses.".to_string()),
                    }

                    // eprintln!("RESULT: {:?}", result);
                    // return Ok(result);

                    match self.lexer.peek() {
                        Some((Token::ReservedKeyword("or"), _)) => {
                            self.consume_token(); // or

                            let other = self.expression_statement()?; //
                                                                      // let other = other.get_expression().unwrap();

                            // eprintln!("LEFT: {:?}\nRIGHT: {:?}", result, other);

                            let res = Stmt::BinaryStatement(
                                Box::new(result),
                                Token::ReservedKeyword("or"),
                                Box::new(other),
                            );

                            // eprintln!("RESULT: {:?}", res);
                            return Ok(res);
                        }
                        Some((Token::ReservedKeyword("and"), _)) => {
                            self.consume_token(); // and

                            let other = self.expression_statement()?;

                            let res = Stmt::BinaryStatement(
                                Box::new(result),
                                Token::ReservedKeyword("and"),
                                Box::new(other),
                            );

                            return Ok(res);
                        }
                        Some((Token::Character(';'), _)) => return Ok(result),
                        _ => {}
                    }
                }

                let expr = self.expression()?;

                match self.lexer.peek() {
                    Some((Token::Character(';'), _)) => Ok(Stmt::Expression(expr)),
                    Some((Token::Character(')'), _)) => Ok(Stmt::Expression(expr)),
                    Some((Token::Character('='), _)) => {
                        self.consume_token();

                        let init_stmt = self.expression_statement()?;
                        let var_name: String = expr.get_variable();

                        if !var_name.is_empty() {
                            Ok(Stmt::Var(var_name, Some(Box::new(init_stmt.clone()))))
                        } else {
                            Err(format!(
                                "Invalid assignment target at {} : {}",
                                self.lexer.get_line(),
                                self.lexer.get_column(),
                            ))
                        }
                    }
                    Some((Token::ReservedKeyword("or"), _)) => {
                        self.consume_token(); // or

                        let other = self.expression_statement()?;
                        let other = other.get_expression().unwrap();

                        Ok(Stmt::Expression(Expr::Binary(
                            Box::new(expr),
                            Token::ReservedKeyword("or"),
                            Box::new(other),
                        )))
                    }
                    Some((Token::ReservedKeyword("and"), _)) => {
                        self.consume_token(); // and

                        let other = self.expression_statement()?;
                        let other = other.get_expression().unwrap();

                        Ok(Stmt::Expression(Expr::Binary(
                            Box::new(expr),
                            Token::ReservedKeyword("and"),
                            Box::new(other),
                        )))
                    }
                    other => Err(format!(
                        "expected semicolon after an expression at {} : {}\nGot {:#?}",
                        self.lexer.get_line(),
                        self.lexer.get_column(),
                        other,
                    )),
                }
            }
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
                Token::Character('!') => {
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
                    match self.lexer.peek() {
                        Some((Token::Character(')'), _)) => {
                            self.consume_token();
                            Ok(Expr::Grouping(Box::new(expr)))
                        }
                        _ => {
                            eprintln!("Error: Unmatched parentheses.");
                            Err("Error: Unmatched parentheses.".to_string())
                        }
                    }

                    /*if let Some((Token::Character(')'), _)) = self.lexer.peek() {
                        self.lexer.next();
                        Ok(Expr::Grouping(Box::new(expr)))
                    } else {
                        eprintln!("Error: Unmatched parentheses.");
                        Err("Error: Unmatched parentheses.".to_string())
                    }*/
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
