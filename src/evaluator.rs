use crate::lexer::Token;
use crate::parser::{Expr, LiteralExpr, Stmt};

use std::collections::HashMap;
use std::ops::Deref;

type Result<T> = std::result::Result<T, String>;

#[derive(Clone)]
pub struct Interpreter {
    super_interpreter: Option<Box<Interpreter>>,
    globals: HashMap<String, LiteralExpr>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            super_interpreter: None,
            globals: HashMap::new(),
        }
    }

    fn new_super(interpreter: Option<Box<Interpreter>>) -> Self {
        Self {
            super_interpreter: interpreter,
            globals: HashMap::new(),
        }
    }

    fn find_variable_in_scope(&self, var_name: &str) -> Option<LiteralExpr> {
        if let Some(lit) = self.globals.get(var_name) {
            return Some(lit.clone());
        }

        if let Some(ref super_interpreter) = self.super_interpreter {
            return super_interpreter.find_variable_in_scope(var_name);
        }

        None
    }

    pub fn run(&mut self, stmt: Stmt) -> Result<String> {
        let mut stdout = String::new();

        // eprintln!("INT RUNNING: {:?}", stmt);

        match stmt {
            Stmt::Expression(expr) => {
                let e = self.eval_expr(&expr);
                if e.is_err() {
                    return Err(e.err().unwrap());
                }
            }

            Stmt::Print(stmt_box) => {
                let stmt = stmt_box.deref();

                let literal: LiteralExpr = match stmt {
                    Stmt::Expression(Expr::Variable(_)) => {
                        let expr_op = stmt.get_expression();
                        /*let expr;
                        if expr_op.is_some() {
                            expr = expr_op.unwrap();
                        } else {
                            return Err("Only expressions can be printed.".to_string());
                        }*/
                        let expr = match expr_op {
                            Some(e) => e,
                            None => return Err("Only expressions can be printed.".to_string()),
                        };

                        if let Some(literal) =
                            self.find_variable_in_scope(&expr.clone().get_variable())
                        {
                            literal
                        } else {
                            return Err(format!("Undefined variable '{}'.", expr.get_variable()));
                        }

                        /*let mut var_opt = match self.globals.get(&expr.clone().get_variable()) {
                            Some(lit) => Some(lit.clone()),
                            None => None,
                        };

                        if var_opt.is_none() && self.super_interpreter.is_some() {
                            var_opt = match self
                                .clone()
                                .super_interpreter
                                .unwrap()
                                .globals
                                .get(&expr.clone().get_variable())
                            {
                                Some(lit) => Some(lit.clone()),
                                None => None,
                            }
                        }

                        if var_opt.is_some() {
                            var_opt.unwrap()
                        } else {
                            return Err(format!("Undefined variable '{}'.", expr.get_variable()));
                        }*/
                    }

                    Stmt::Var(name, initializer) => {
                        let value = match initializer {
                            Some(expr) => {
                                let val = (*expr).get_expression().unwrap();
                                self.eval_expr(&val)?
                            }

                            None => LiteralExpr::NIL,
                        };

                        self.globals.insert(name.clone(), value);

                        match self.globals.get(name) {
                            None => return Err(format!("Undefined variable '{}'.", name)),
                            Some(lit) => lit.clone(),
                        }
                    }

                    other => {
                        let mut e = self.clone();
                        let other_expr = other.get_expression();
                        let expr;
                        if other_expr.is_some() {
                            expr = other_expr.unwrap();
                        } else {
                            return Err("Only expressions can be printed.".to_string());
                        }
                        e.eval_expr(&expr)?
                    }
                };

                let literal_str = print_literal(&literal);

                println!("{}", literal_str);

                stdout += literal_str.as_ref();

                stdout += "\n";
            }

            Stmt::Var(name, initializer) => {
                let value = match initializer {
                    Some(expr) => {
                        let val = *expr;

                        match val {
                            Stmt::Var(ref name, ref value_) => {
                                let val = *(value_.clone().unwrap());
                                let val = val.get_expression().unwrap();
                                let expr = self.eval_expr(&val)?;
                                self.globals.insert(name.to_string(), expr);
                            }
                            _ => {}
                        }

                        let val = val.get_expression().unwrap();
                        self.eval_expr(&val)?
                    }

                    None => LiteralExpr::NIL,
                };

                self.globals.insert(name.clone(), value);
            }

            Stmt::Block(statements) => {
                let super_maker: Option<Box<Interpreter>> = Some(Box::new(self.clone()));
                let mut interpreter = Interpreter::new_super(super_maker);

                let mut iter_stmt = statements.iter();
                while let Some(stmt) = iter_stmt.next() {
                    let s = stmt.clone();
                    let res = interpreter.run(s);
                    if res.is_err() {
                        eprintln!("{:?}", res.err().unwrap());
                        return Err(format!("Runtime error inside a block statement."));
                    }
                }
            }
        }

        Ok(stdout)
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<LiteralExpr> {
        //  eprintln!("EVAL EXPR: {:?}", expr);
        match expr {
            Expr::Literal(literal_expr) => Ok((*literal_expr).clone()),

            Expr::Grouping(inner) => self.eval_expr(inner.as_ref()),

            Expr::Binary(left_expr, token_type, right_expr) => {
                let left_literal = self.eval_expr(left_expr.as_ref())?;

                let right_literal = self.eval_expr(right_expr.as_ref())?;

                // eprintln!("EXPRS ARE: {:?} AND {:?}", left_literal, right_literal);

                if left_literal.is_number_and_parsable(&right_literal) {
                    return Ok(LiteralExpr::FALSE);
                }

                if !left_literal.is_same_type(&right_literal) {
                    // eprintln!("Error: Not the same type");
                    return Err("Operands must be numbers.".to_string());
                }

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

pub fn eval(expr: &Expr) -> Res {
    let res = Interpreter {
        super_interpreter: None,
        globals: HashMap::new(),
    }
    .eval_expr(expr)
    .map(|literal_expr| print_literal(&literal_expr));

    match res {
        Ok(value) => Res::Ok(value),
        _ => Res::RuntimeError(format!("{:?} is not an expression!", expr)),
    }
}

fn print_literal(literal_expr: &LiteralExpr) -> String {
    match literal_expr {
        LiteralExpr::Number(v) => v.to_string(),
        LiteralExpr::StringLiteral(s) => s.to_string(),
        LiteralExpr::TRUE => "true".to_string(),
        LiteralExpr::FALSE => "false".to_string(),
        LiteralExpr::NIL => "nil".to_string(),
        LiteralExpr::EOF => "EOF".to_string(),
    }
}
