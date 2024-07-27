use crate::ast::Expr;

pub fn evaluate(expr: &Expr) -> String {
    match expr {
        Expr::Number(n) => n.to_string(),
        Expr::StringLiteral(s) => s.to_string(),
        Expr::Identifier(i) => i.to_string(),
        Expr::Character(c) => c.to_string(),
        Expr::CharacterDouble(c1, c2) => format!("{c1}{c2}"),
        Expr::ReservedKeyword(k) => k.to_string(),
        Expr::Unary { op, right } => {
            let right = evaluate(right);
            let op = match op {
                '-' => {
                    if right.parse::<f64>().is_ok() {
                        (-(right.parse::<f64>().unwrap())).to_string()
                    } else {
                        panic!("Invalid unary operator: {op}")
                    }
                }
                '!' => {
                    if right.parse::<f64>().is_ok() {
                        if right.parse::<f64>().unwrap() != 0.0 {
                            Expr::ReservedKeyword("false".to_string()).to_string()
                        } else {
                            Expr::ReservedKeyword("true".to_string()).to_string()
                        }
                    } else if right.eq(Expr::ReservedKeyword("true".to_string())
                        .to_string()
                        .as_str())
                    {
                        Expr::ReservedKeyword("false".to_string()).to_string()
                    } else if right.eq(Expr::ReservedKeyword("false".to_string())
                        .to_string()
                        .as_str())
                    {
                        Expr::ReservedKeyword("true".to_string()).to_string()
                    } else if right.eq(Expr::ReservedKeyword("nil".to_string())
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
            op
        }
        Expr::Binary { left, op, right } => {
            let left = evaluate(left);
            let right = evaluate(right);
            let op = match op {
                '+' => {
                    if left.parse::<f64>().is_ok() && right.parse::<f64>().is_ok() {
                        (left.parse::<f64>().unwrap() + right.parse::<f64>().unwrap())
                            .to_string()
                    } else {
                        format!("{left}{right}")
                    }
                },
                '-' => {
                    if left.parse::<f64>().is_ok() && right.parse::<f64>().is_ok() {
                        (left.parse::<f64>().unwrap() - right.parse::<f64>().unwrap())
                            .to_string()
                    } else {
                        panic!("Invalid binary operator: {op}")
                    }
                }
                '*' => {
                    if left.parse::<f64>().is_ok() && right.parse::<f64>().is_ok() {
                        (left.parse::<f64>().unwrap() * right.parse::<f64>().unwrap())
                            .to_string()
                    } else {
                        panic!("Invalid binary operator: {op}")
                    }
                }
                '/' => {
                    if left.parse::<f64>().is_ok() && right.parse::<f64>().is_ok() {
                        (left.parse::<f64>().unwrap() / right.parse::<f64>().unwrap())
                            .to_string()
                    } else {
                        panic!("Invalid binary operator: {op}")
                    }
                }
                '<' => {
                    if left.parse::<f64>().is_ok() && right.parse::<f64>().is_ok() {
                        (left.parse::<f64>().unwrap() < right.parse::<f64>().unwrap())
                            .to_string()
                    } else {
                        panic!("Invalid binary operator: {op}")
                    }
                }
                '>' => {
                    if left.parse::<f64>().is_ok() && right.parse::<f64>().is_ok() {
                        (left.parse::<f64>().unwrap() > right.parse::<f64>().unwrap())
                            .to_string()
                    } else {
                        panic!("Invalid binary operator: {op}")
                    }
                }
                _ => panic!("Invalid binary operator: {op}"),
            };
            op
        }
        Expr::Comparison { left, op, right } => {
            let left = evaluate(left);
            let right = evaluate(right);
            let op = match op {
                ('<', '=') => left <= right,
                ('>', '=') => left >= right,
                ('=', '=') => left == right,
                ('!', '=') => left != right,
                _ => panic!("Invalid comparison operator: {}{}", op.0, op.1),
            };
            op.to_string()
        }
        Expr::Grouping(expr) => evaluate(expr),
    }
}
