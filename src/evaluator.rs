use crate::ast::Expr;

pub enum Res {
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

pub fn evaluate(expr: &Expr) -> Res {
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
    }
}
