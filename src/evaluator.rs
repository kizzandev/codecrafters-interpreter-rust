use crate::lexer::Token;
use crate::parser::{Expr, LiteralExpr, Stmt};

use std::collections::HashMap;
use std::ops::Deref;

type Result<T> = std::result::Result<T, String>;

#[derive(Clone)]
pub struct Interpreter {
    enclosing: Option<Box<Interpreter>>,
    values: Vec<HashMap<String, LiteralExpr>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            enclosing: None,
            values: vec![HashMap::new()],
        }
    }

    /*fn new_super(interpreter: Option<Box<Interpreter>>) -> Self {
        Self {
            enclosing: interpreter,
            values: vec![],
        }
    }*/

    fn scope_enter(&mut self) {
        self.values.push(HashMap::new());
    }

    fn scope_exit(&mut self) {
        self.values.pop();
    }

    fn define_variable(&mut self, name: String, value: LiteralExpr) {
        if let Some(env) = self.values.last_mut() {
            env.insert(name.to_string(), value);
        }
    }

    fn assign_variable(&mut self, name: String, value: LiteralExpr) -> Result<()> {
        for env in self.values.iter_mut().rev() {
            if env.contains_key(&name) {
                env.insert(name.to_string(), value);
                return Ok(());
            }
        }

        // if let Some(ref mut enclosing) = self.enclosing {
        //     enclosing.assign_variable(name.to_string(), value)
        // } else {
        //     Err(format!("Undefined variable '{}'.", name))
        // }

        Err(format!("Undefined variable '{}'.", name))
    }

    fn find_variable(&self, name: String) -> Option<LiteralExpr> {
        for env in self.values.iter().rev() {
            if let Some(val) = env.get(&name) {
                return Some(val.clone());
            }
        }

        if let Some(ref enclosing) = self.enclosing {
            return enclosing.find_variable(name);
        }

        None
    }

    fn get_falsy_values(&self) -> Vec<LiteralExpr> {
        vec![
            LiteralExpr::FALSE,
            LiteralExpr::NIL,
            LiteralExpr::Number(0.0),
            // LiteralExpr::StringLiteral("".to_string()), // THEY CONSIDER THIS A TRUTHY VALUE I GUESS
        ]
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

                        let expr = match expr_op {
                            Some(e) => e,
                            None => return Err("Only expressions can be printed.".to_string()),
                        };

                        if let Some(literal) =
                            // self.find_variable_in_scope(&expr.clone().get_variable())
                            self.find_variable(expr.clone().get_variable())
                        {
                            literal
                        } else {
                            return Err(format!("Undefined variable '{}'.", expr.get_variable()));
                        }
                    }

                    Stmt::Var(name, initializer) => {
                        let value = match initializer {
                            Some(expr) => {
                                let val = (*expr).get_expression().unwrap();
                                self.eval_expr(&val)?
                            }

                            None => LiteralExpr::NIL,
                        };

                        // self.globals.insert(name.clone(), value);
                        let _ = self.assign_variable(name.to_string(), value);

                        // match self.globals.get(name) {
                        match self.find_variable(name.to_string()) {
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
                                let _ = self.assign_variable(name.to_string(), expr);
                            }
                            _ => {}
                        }

                        let val = val.get_expression().unwrap();
                        self.eval_expr(&val)?
                    }

                    None => LiteralExpr::NIL,
                };

                self.assign_variable(name.clone(), value)?;
                return Ok(name.clone());
            }

            Stmt::VarDecl(name, initializer) => {
                let value = match initializer {
                    Some(expr) => {
                        let val = *expr;

                        match val {
                            Stmt::Var(ref name, ref value_) => {
                                let val = *(value_.clone().unwrap());
                                let val = val.get_expression().unwrap();
                                let expr = self.eval_expr(&val)?;
                                let _ = self.define_variable(name.to_string(), expr);
                            }
                            _ => {}
                        }

                        let val = val.get_expression().unwrap();
                        self.eval_expr(&val)?
                    }

                    None => LiteralExpr::NIL,
                };

                self.define_variable(name.clone(), value);
            }

            Stmt::Block(statements) => {
                self.scope_enter();

                let mut iter_stmt = statements.iter();
                while let Some(stmt) = iter_stmt.next() {
                    let s = stmt.clone();
                    let _ = self.run(s);
                }

                self.scope_exit();
            }

            Stmt::BinaryStatement(left_stmt_box, token, right_stmt_box) => {
                // eprintln!("LEFT STMT: {:?}", left_stmt_box.deref());
                // eprintln!("TOKEN: {:?}", token);
                // eprintln!("RIGHT STMT: {:?}", right_stmt_box.deref());

                let falsy_values = self.get_falsy_values();

                let left_stmt = *left_stmt_box;
                let right_stmt = *right_stmt_box;

                let left_value = self.run(left_stmt)?;
                let left = self.find_variable(left_value).unwrap_or(LiteralExpr::NIL);

                match (left, token) {
                    (l, Token::ReservedKeyword("or")) => {
                        if !falsy_values.contains(&l) {
                            return Ok(l.to_string());
                        } else {
                            let right_value = self.run(right_stmt)?;
                            let right = self.find_variable(right_value).unwrap_or(LiteralExpr::NIL);

                            if !falsy_values.contains(&right) {
                                return Ok(right.to_string());
                            }
                        }
                    }

                    (l, Token::ReservedKeyword("and")) => {
                        let right_value = self.run(right_stmt)?;
                        let right = self.find_variable(right_value).unwrap_or(LiteralExpr::NIL);

                        if !falsy_values.contains(&l) && !falsy_values.contains(&right) {
                            return Ok(right.to_string());
                        }
                    }

                    (l, token_type) => {
                        let r = self.run(right_stmt)?;
                        return Err(format!(
                            "binary statement not supported: {:?} {} {:?}",
                            l,
                            token_type,
                            self.find_variable(r).unwrap_or(LiteralExpr::NIL)
                        ));
                    }
                }
            }

            Stmt::If(condition, block, else_block) => {
                // eprintln!("CONDITION TO EVAL: {:?}", *condition);

                let truth_value = match *condition {
                    Stmt::Expression(Expr::Literal(l)) => match l {
                        LiteralExpr::FALSE | LiteralExpr::NIL | LiteralExpr::Number(0.0) => false,
                        _ => true,
                    },
                    Stmt::Var(_, _) => {
                        let cond = *(condition.clone());
                        let _ = self.run(cond);
                        true
                    }
                    Stmt::Expression(Expr::Unary(_, var_box)) => {
                        let expr = *var_box;
                        match expr {
                            Expr::Literal(l) => match l {
                                LiteralExpr::FALSE
                                | LiteralExpr::NIL
                                | LiteralExpr::Number(0.0) => true,
                                _ => false,
                            },
                            _ => false,
                        }
                    }
                    expr => {
                        let res = self.eval_expr(&expr.get_expression().unwrap())?;
                        match res {
                            LiteralExpr::FALSE | LiteralExpr::NIL | LiteralExpr::Number(0.0) => {
                                false
                            }
                            _ => true,
                        }
                    }
                };

                if truth_value {
                    let block_stmt = *block;
                    // eprintln!("BLOCK STMT: {:?}", block_stmt);
                    let _ = self.run(block_stmt);
                } else if else_block.is_some() {
                    let block_stmt = *(else_block.unwrap());
                    let _ = self.run(block_stmt);
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
                // eprintln!("LEFT EXPR IS: {:?}", left_expr.as_ref());
                let is_conector = if token_type.eq(&Token::ReservedKeyword("or"))
                    || token_type.eq(&Token::ReservedKeyword("and"))
                {
                    true
                } else {
                    false
                };
                let falsy_values = self.get_falsy_values();

                let left_literal = self.eval_expr(left_expr.as_ref())?;
                let right_literal = self.eval_expr(right_expr.as_ref())?;

                // eprintln!("EXPRS ARE: {:?} AND {:?}", left_literal, right_literal);

                if left_literal.is_number_and_parsable(&right_literal) {
                    return Ok(LiteralExpr::FALSE);
                }

                if !is_conector && !left_literal.is_same_type(&right_literal) {
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

                    (l, Token::ReservedKeyword("or"), r) => {
                        if !falsy_values.contains(&l) {
                            Ok(l)
                        } else if !falsy_values.contains(&r) {
                            Ok(r)
                        } else {
                            Ok(LiteralExpr::FALSE)
                        }
                    }

                    (l, Token::ReservedKeyword("and"), r) => {
                        if !falsy_values.contains(&l) && !falsy_values.contains(&r) {
                            // Ok(LiteralExpr::TRUE)
                            Ok(r)
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

            // Expr::Variable(name) => match self.globals.get(name) {
            Expr::Variable(name) => match self.find_variable(name.to_string()) {
                Some(literal) => Ok(literal.clone()),

                None => Err(format!("Undefined variable: {}", name)),
            },

            other => Err(format!("Cannot evaluate: {:?}", other)),
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
        enclosing: None,
        values: vec![],
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
