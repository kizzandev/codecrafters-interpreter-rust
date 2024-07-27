use crate::ast::Expr;

pub enum Res {
    Number(f64),
    StringLiteral(String),
}

impl Res {
    pub fn to_string(&self) -> String {
        match self {
            Res::Number(n) => n.to_string(),
            Res::StringLiteral(s) => s.to_string(),
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

    fn get_number(&self) -> f64 {
        match self {
            Res::Number(n) => *n,
            _ => panic!("Not a number"),
        }
    }
}

pub fn evaluate(expr: &Expr) -> Res {
    match expr {
        Expr::Number(n) => Res::Number(*n),
        Expr::StringLiteral(s) => Res::StringLiteral(s.to_string()),
        Expr::Identifier(i) => Res::StringLiteral(i.to_string()),
        Expr::Character(c) => Res::StringLiteral(c.to_string()),
        Expr::CharacterDouble(c1, c2) => Res::StringLiteral(format!("{c1}{c2}")),
        Expr::ReservedKeyword(k) => Res::StringLiteral(k.to_string()),
        Expr::Unary { op, right } => {
            let right = evaluate(right);
            let op = match op {
                '-' => {
                    if right.is_number() {
                        (-(right.get_number())).to_string()
                    } else {
                        panic!("Invalid unary operator: {op}")
                    }
                }
                '!' => {
                    if right.is_number() {
                        if right.get_number() != 0.0 {
                            Expr::ReservedKeyword("false".to_string()).to_string()
                        } else {
                            Expr::ReservedKeyword("true".to_string()).to_string()
                        }
                    } else if right.to_string().eq(Expr::ReservedKeyword("true".to_string())
                        .to_string()
                        .as_str())
                    {
                        Expr::ReservedKeyword("false".to_string()).to_string()
                    } else if right.to_string().eq(Expr::ReservedKeyword("false".to_string())
                        .to_string()
                        .as_str())
                    {
                        Expr::ReservedKeyword("true".to_string()).to_string()
                    } else if right.to_string().eq(Expr::ReservedKeyword("nil".to_string())
                        .to_string()
                        .as_str())
                    {
                        Expr::ReservedKeyword("true".to_string()).to_string()
                    } else {
                        panic!("Invalid unary operator: {op}")
                    }
                }
                _ => panic!("Invalid unary operator: {op}"),
            };
            Res::StringLiteral(op)
        }
        Expr::Binary { left, op, right } => {
            let left = evaluate(left);
            let right = evaluate(right);
            let op = match op {
                '+' => {
                    if left.is_number() && right.is_number() {
                        (left.get_number() + right.get_number()).to_string()
                    } else {
                        format!("{}{}", left.to_string(), right.to_string())
                    }
                }
                '-' => {
                    if left.is_number() && right.is_number() {
                        (left.get_number() - right.get_number()).to_string()
                    } else {
                        panic!("Invalid binary operator: {op}")
                    }
                }
                '*' => {
                    if left.is_number() && right.is_number() {
                        (left.get_number() * right.get_number()).to_string()
                    } else {
                        panic!("Invalid binary operator: {op}")
                    }
                }
                '/' => {
                    if left.is_number() && right.is_number() {
                        (left.get_number() / right.get_number()).to_string()
                    } else {
                        panic!("Invalid binary operator: {op}")
                    }
                }
                '<' => {
                    if left.is_number() && right.is_number() {
                        (left.get_number() < right.get_number()).to_string()
                    } else {
                        panic!("Invalid binary operator: {op}")
                    }
                }
                '>' => {
                    if left.is_number() && right.is_number() {
                        (left.get_number() > right.get_number()).to_string()
                    } else {
                        panic!("Invalid binary operator: {op}")
                    }
                }
                _ => panic!("Invalid binary operator: {op}"),
            };
            Res::StringLiteral(op)
        }
        Expr::Comparison { left, op, right } => {
            let left = evaluate(left);
            let right = evaluate(right);
            let op = match op {
                ('<', '=') => {
                    if left.is_number() && right.is_number() {
                        Res::StringLiteral(
                            (left.get_number()
                                <= right.get_number())
                            .to_string(),
                        )
                    } else {
                        panic!("Invalid comparison operator: {}{}", op.0, op.1)
                    }
                }
                ('>', '=') => {
                    if left.is_number() && right.is_number() {
                        Res::StringLiteral(
                            (left.get_number()
                                >= right.get_number())
                            .to_string(),
                        )
                    } else {
                        panic!("Invalid comparison operator: {}{}", op.0, op.1)
                    }
                }
                ('=', '=') => {
                    Res::StringLiteral((left.to_string() == right.to_string()).to_string())
                }
                ('!', '=') => {
                    Res::StringLiteral((left.to_string() != right.to_string()).to_string())
                }
                _ => panic!("Invalid comparison operator: {}{}", op.0, op.1),
            };
            Res::StringLiteral(op.to_string())
        }
        Expr::Grouping(expr) => evaluate(expr),
    }
}
