// use crate::ast::Expr;

use crate::lexer::Token;
use crate::parser::{Expr, LiteralExpr, Stmt};

use std::collections::HashMap;

type Result<T> = std::result::Result<T, String>;

/*pub enum Res {
    Number(f64),
    StringLiteral(String),
    Bool(String),
    RuntimeError(String),
}

impl Res {
    pub fn to_string(&self) -> String {
        match self {
            Res::Number(n) => n.to_string(),
            Res::StringLiteral(s) => s.to_string(),
            Res::Bool(s) => s.to_string(),
            Res::RuntimeError(s) => s.to_string(),
        }
    }
}

impl Res {
    fn is_number(&self) -> bool {
        match self {
            Res::Number(_) => true,
            _ => false,
        }
    }

    fn is_bool(&self) -> bool {
        match self {
            Res::Bool(_) => true,
            _ => false,
        }
    }

    fn is_string(&self) -> bool {
        match self {
            Res::StringLiteral(_) => true,
            _ => false,
        }
    }

    fn get_number(&self) -> f64 {
        match self {
            Res::Number(n) => *n,
            _ => panic!("Not a number"),
        }
    }

    fn is_same_type(&self, other: &Res) -> bool {
        (self.is_number() && other.is_number())
            || (self.is_string() && other.is_string())
            || (self.is_bool() && other.is_bool())
    }

    pub fn is_runtime_error(&self) -> bool {
        match self {
            Res::RuntimeError(_) => true,
            _ => false,
        }
    }
}

fn operand_must_be_number() -> Res {
    Res::RuntimeError("Operand must be a number.".to_string())
}

fn operands_must_be_numbers() -> Res {
    Res::RuntimeError("Operands must be numbers.".to_string())
}
*/

pub struct Interpreter {
    globals: HashMap<String, LiteralExpr>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
        }
    }

    // fn run(&mut self, stmts: &[Stmt]) -> Result<String> {
    pub fn run(&mut self, stmt: Stmt<'_>) -> Result<String> {
        let mut stdout = String::new();

        /*for stmt in stmts {
            match stmt {
                Stmt::Expression(expr) => {
                    self.eval_expr(expr)?;
                }

                Stmt::Print(expr) => {
                    let literal = self.eval_expr(expr)?;
                    let literal = print_literal(&literal);

                    println!("{}", literal);

                    stdout += literal.as_ref();

                    stdout += "\n";
                }

                Stmt::Var(name, initializer) => {
                    let value = match initializer {
                        Some(expr) => self.eval_expr(expr)?,

                        None => LiteralExpr::NIL,
                    };

                    self.globals.insert(name.clone(), value);
                }

                Stmt::Err(error) => {
                    eprintln!("Error: {error}");
                }
            }
        }*/

        // eprintln!("RUNNING: {:?}", stmt);
        match stmt {
            Stmt::Expression(expr) => {
                self.eval_expr(&expr)?;
            }

            Stmt::Print(expr) => {
                // eprintln!("EXPR: {:?}", expr);

                // eprintln!("GLOBALS: {:?}", self.globals);

                let literal = self
                    .globals
                    .get(&expr.get_variable())
                    .expect("Variable not found");

                // let literal = self.eval_expr(&expr)?;
                // let literal = print_literal(&literal);
                let literal = print_literal(&literal);

                println!("{}", literal);

                stdout += literal.as_ref();

                stdout += "\n";
            }

            Stmt::Var(name, initializer) => {
                let value = match initializer {
                    Some(expr) => self.eval_expr(&expr)?,

                    None => LiteralExpr::NIL,
                };

                self.globals.insert(name.clone(), value);
                // eprintln!("GLOBALS: {:?}", self.globals);
            }

            Stmt::Err(error) => {
                eprintln!("Error: {error}");
            }
        }

        Ok(stdout)
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<LiteralExpr> {
        match expr {
            Expr::Literal(literal_expr) => Ok((*literal_expr).clone()),

            Expr::Grouping(inner) => self.eval_expr(inner.as_ref()),

            Expr::Binary(left_expr, token_type, right_expr) => {
                let left_literal = self.eval_expr(left_expr.as_ref())?;

                let right_literal = self.eval_expr(right_expr.as_ref())?;

                match (left_literal, token_type, right_literal) {
                    // string concatenation
                    (
                        LiteralExpr::StringLiteral(l),
                        Token::Character('+'),
                        LiteralExpr::StringLiteral(r),
                    ) => Ok(LiteralExpr::StringLiteral(l + r.as_str())),

                    // math
                    (LiteralExpr::Number(l), Token::Character('+'), LiteralExpr::Number(r)) => {
                        Ok(LiteralExpr::Number(l + r))
                    }

                    (LiteralExpr::Number(l), Token::Character('-'), LiteralExpr::Number(r)) => {
                        Ok(LiteralExpr::Number(l - r))
                    }

                    (LiteralExpr::Number(l), Token::Character('/'), LiteralExpr::Number(r)) => {
                        Ok(LiteralExpr::Number(l / r))
                    }

                    (LiteralExpr::Number(l), Token::Character('*'), LiteralExpr::Number(r)) => {
                        Ok(LiteralExpr::Number(l * r))
                    }

                    // relation
                    (LiteralExpr::Number(l), Token::Character('<'), LiteralExpr::Number(r)) => {
                        if l < r {
                            Ok(LiteralExpr::TRUE)
                        } else {
                            Ok(LiteralExpr::FALSE)
                        }
                    }

                    (
                        LiteralExpr::Number(l),
                        Token::CharacterDouble('<', '='),
                        LiteralExpr::Number(r),
                    ) => {
                        if l <= r {
                            Ok(LiteralExpr::TRUE)
                        } else {
                            Ok(LiteralExpr::FALSE)
                        }
                    }

                    (LiteralExpr::Number(l), Token::Character('>'), LiteralExpr::Number(r)) => {
                        if l > r {
                            Ok(LiteralExpr::TRUE)
                        } else {
                            Ok(LiteralExpr::FALSE)
                        }
                    }

                    (
                        LiteralExpr::Number(l),
                        Token::CharacterDouble('>', '='),
                        LiteralExpr::Number(r),
                    ) => {
                        if l >= r {
                            Ok(LiteralExpr::TRUE)
                        } else {
                            Ok(LiteralExpr::FALSE)
                        }
                    }

                    // equality
                    (l, Token::CharacterDouble('=', '='), r) => {
                        if l.to_string() == r.to_string() {
                            Ok(LiteralExpr::TRUE)
                        } else {
                            Ok(LiteralExpr::FALSE)
                        }
                    }

                    (l, Token::CharacterDouble('!', '='), r) => {
                        if l.to_string() != r.to_string() {
                            Ok(LiteralExpr::TRUE)
                        } else {
                            Ok(LiteralExpr::FALSE)
                        }
                    }

                    (l, token_type, r) => Err(format!(
                        "binary expression not supported: {:?} {} {:?}",
                        l, token_type, r
                    )),
                }
            }

            Expr::Unary(token, right_expr) => match token {
                Token::Character('-') => {
                    let right_literal = self.eval_expr(right_expr)?;

                    match right_literal {
                        LiteralExpr::Number(v) => Ok(LiteralExpr::Number(-v)),

                        _ => Err(format!(
                            "{:?} is not possible in this context!",
                            right_literal
                        )),
                    }
                }

                Token::Character('!') => {
                    // "false" and "nil" are falsy, everything else is truthy

                    let right_literal = self.eval_expr(right_expr)?;

                    match right_literal {
                        LiteralExpr::FALSE | LiteralExpr::NIL => Ok(LiteralExpr::TRUE),

                        _ => Ok(LiteralExpr::FALSE),
                    }
                }

                _ => Err(format!("{} is not possible in this context!", token)),
            },

            Expr::Variable(name) => match self.globals.get(name) {
                Some(literal) => Ok(literal.clone()),

                None => Err(format!("Undefined variable: {}", name)),
            },
        }
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<LiteralExpr> {
        match stmt {
            Stmt::Expression(expr) => self.eval_expr(expr),
            _ => Err(format!("{:?} is not an expression!", stmt)),
        }
    }
}

pub enum Res {
    Ok(String),
    RuntimeError(String),
}

impl Res {
    pub fn is_runtime_error(&self) -> bool {
        match self {
            Res::RuntimeError(_) => true,
            _ => false,
        }
    }

    pub fn get_error(&self) -> String {
        match self {
            Res::RuntimeError(s) => s.to_string(),
            _ => "".to_string(),
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Res::Ok(value) => value.to_string(),
            Res::RuntimeError(s) => s.to_string(),
        }
    }
}

pub fn eval(stmt: &Stmt) -> Res {
    let res = Interpreter {
        globals: HashMap::new(),
    }
    .eval_stmt(stmt)
    .map(|literal_expr| print_literal(&literal_expr));

    match res {
        Ok(value) => Res::Ok(value),
        _ => Res::RuntimeError(format!("{:?} is not an expression!", stmt)),
    }

    // map(|literal_expr| print_literal(&literal_expr))
}

// pub fn run(stmt: Stmt) -> Result<String> {
//     Interpreter {
//         globals: HashMap::new(),
//     }
//     .run(stmt)
// }

fn print_literal(literal_expr: &LiteralExpr) -> String {
    match literal_expr {
        LiteralExpr::Number(v) => v.to_string(),
        LiteralExpr::StringLiteral(s) => s.to_string(),
        LiteralExpr::TRUE => "true".to_string(),
        LiteralExpr::FALSE => "false".to_string(),
        LiteralExpr::NIL => "nil".to_string(),
    }
}

/*pub fn evaluate(expr: &Expr) -> Res {
    match expr {
        Expr::EOF => Res::StringLiteral("EOF".to_string()),

        Expr::Number(n) => Res::Number(*n),
        Expr::StringLiteral(s) => Res::StringLiteral(s.to_string()),
        Expr::Identifier(i) => Res::StringLiteral(i.to_string()),
        Expr::Character(c) => Res::StringLiteral(c.to_string()),
        Expr::CharacterDouble(c1, c2) => Res::StringLiteral(format!("{c1}{c2}")),
        Expr::ReservedKeyword(k) => match k.to_string().as_str() {
            "true" => Res::Bool("true".to_string()),
            "false" => Res::Bool("false".to_string()),
            "nil" => Res::Bool("nil".to_string()),
            _ => Res::StringLiteral(k.to_string()),
        },
        Expr::Unary { op, right } => {
            let right = evaluate(right);
            match op {
                '-' => {
                    if right.is_number() {
                        Res::Number(-right.get_number())
                    } else {
                        operand_must_be_number()
                    }
                }
                '!' => {
                    if right.is_number() {
                        if right.get_number() != 0.0 {
                            Res::StringLiteral("false".to_string())
                        } else {
                            Res::StringLiteral("true".to_string())
                        }
                    } else if right
                        .to_string()
                        .eq(("true".to_string()).to_string().as_str())
                    {
                        Res::StringLiteral("false".to_string())
                    } else if right
                        .to_string()
                        .eq(("false".to_string()).to_string().as_str())
                    {
                        Res::StringLiteral("true".to_string())
                    } else if right
                        .to_string()
                        .eq(("nil".to_string()).to_string().as_str())
                    {
                        Res::StringLiteral("true".to_string())
                    } else {
                        panic!("Invalid unary operator: {op}")
                    }
                }
                _ => panic!("Invalid unary operator: {op}"),
            }
        }
        Expr::Binary { left, op, right } => {
            let left = evaluate(left);
            let right = evaluate(right);
            match op {
                '+' => {
                    if !left.is_same_type(&right) {
                        return operands_must_be_numbers();
                    }
                    if left.is_number() && right.is_number() {
                        Res::Number(left.get_number() + right.get_number())
                    } else {
                        Res::StringLiteral(format!("{}{}", left.to_string(), right.to_string()))
                    }
                }
                '-' => {
                    if !left.is_same_type(&right) {
                        return operands_must_be_numbers();
                    }
                    if left.is_number() && right.is_number() {
                        Res::Number(left.get_number() - right.get_number())
                    } else {
                        operands_must_be_numbers()
                    }
                }
                '*' => {
                    if !left.is_same_type(&right) {
                        return operands_must_be_numbers();
                    }
                    if left.is_number() && right.is_number() {
                        Res::Number(left.get_number() * right.get_number())
                    } else {
                        operands_must_be_numbers()
                    }
                }
                '/' => {
                    if !left.is_same_type(&right) {
                        return operands_must_be_numbers();
                    }
                    if left.is_number() && right.is_number() {
                        Res::Number(left.get_number() / right.get_number())
                    } else {
                        operands_must_be_numbers()
                    }
                }
                '<' => {
                    if !left.is_same_type(&right) {
                        return operands_must_be_numbers();
                    }
                    if left.is_number() && right.is_number() {
                        Res::StringLiteral((left.get_number() < right.get_number()).to_string())
                    } else {
                        panic!(
                            "Invalid binary operator: {} {op} {}",
                            left.to_string(),
                            right.to_string()
                        )
                    }
                }
                '>' => {
                    if !left.is_same_type(&right) {
                        return operands_must_be_numbers();
                    }
                    if left.is_number() && right.is_number() {
                        Res::StringLiteral((left.get_number() > right.get_number()).to_string())
                    } else {
                        panic!(
                            "Invalid binary operator: {} {op} {}",
                            left.to_string(),
                            right.to_string()
                        )
                    }
                }
                _ => panic!(
                    "Invalid binary operator: {} {op} {}",
                    left.to_string(),
                    right.to_string()
                ),
            }
        }
        Expr::Comparison { left, op, right } => {
            let left = evaluate(left);
            let right = evaluate(right);
            let op = match op {
                ('<', '=') => {
                    if left.is_number() && right.is_number() {
                        Res::StringLiteral((left.get_number() <= right.get_number()).to_string())
                    } else {
                        return operands_must_be_numbers();
                    }
                }
                ('>', '=') => {
                    if left.is_number() && right.is_number() {
                        Res::StringLiteral((left.get_number() >= right.get_number()).to_string())
                    } else {
                        return operands_must_be_numbers();
                    }
                }
                ('=', '=') => {
                    if !left.is_same_type(&right) {
                        return Res::StringLiteral("false".to_string());
                    }
                    Res::StringLiteral((left.to_string() == right.to_string()).to_string())
                }
                ('!', '=') => {
                    if !left.is_same_type(&right) {
                        return Res::StringLiteral("false".to_string());
                    }
                    Res::StringLiteral((left.to_string() != right.to_string()).to_string())
                }
                _ => panic!("Invalid comparison operator: {}{}", op.0, op.1),
            };
            Res::StringLiteral(op.to_string())
        }
        Expr::Grouping(expr) => evaluate(expr),
        Expr::VarAssign(name, right) => {
            let right = evaluate(right);
            Res::StringLiteral(format!("{} = {}", name, right.to_string()))
        }
    }
}
*/
