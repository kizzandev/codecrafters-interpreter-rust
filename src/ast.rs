#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Expr {
    EOF,

    Number(f64),
    StringLiteral(String),
    Identifier(String),
    Character(char),
    CharacterDouble(char, char),
    ReservedKeyword(String),
    Unary {
        op: char,
        right: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        op: char,
        right: Box<Expr>,
    },
    Comparison {
        left: Box<Expr>,
        op: (char, char),
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),
    VarAssign(String, Box<Expr>),
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

impl Expr {
    pub fn to_string(&self) -> String {
        match self {
            Expr::EOF => "EOF".to_string(),
            Expr::Number(n) => parse_number(n),
            Expr::StringLiteral(s) => s.to_string(),
            Expr::Identifier(i) => i.to_string(),
            Expr::Character(c) => c.to_string(),
            Expr::CharacterDouble(c1, c2) => format!("{c1}{c2}"),
            Expr::ReservedKeyword(k) => k.to_string(),
            Expr::Unary { op, right } => format!("({op} {})", right.to_string()),
            Expr::Binary { left, op, right } => {
                format!("({} {} {})", op, left.to_string(), right.to_string())
            }
            Expr::Comparison { left, op, right } => {
                format!(
                    "({}{} {} {})",
                    op.0,
                    op.1,
                    left.to_string(),
                    right.to_string()
                )
            }
            Expr::Grouping(expr) => format!("(group {})", expr.to_string()),
            Expr::VarAssign(i, expr) => format!("({} = {})", i, expr.to_string()),
        }
    }

    pub fn get_type(&self) -> String {
        match self {
            Expr::EOF => "EOF".to_string(),
            Expr::Number(_) => "Number".to_string(),
            Expr::StringLiteral(_) => "StringLiteral".to_string(),
            Expr::Identifier(_) => "Identifier".to_string(),
            Expr::Character(_) => "Character".to_string(),
            Expr::CharacterDouble(_, _) => "CharacterDouble".to_string(),
            Expr::ReservedKeyword(_) => "ReservedKeyword".to_string(),
            Expr::Unary { .. } => "Unary".to_string(),
            Expr::Binary { .. } => "Binary".to_string(),
            Expr::Comparison { .. } => "Comparison".to_string(),
            Expr::Grouping(_) => "Grouping".to_string(),
            Expr::VarAssign(_, _) => "VarAssign".to_string(),
        }
    }

    pub fn change_value(&mut self, new_value: String) -> Expr {
        match self {
            Expr::Number(_) => Expr::Number(new_value.parse::<f64>().unwrap()),
            Expr::StringLiteral(_) => Expr::StringLiteral(new_value.to_string()),
            Expr::Identifier(_) => Expr::Identifier(new_value.to_string()),
            Expr::Character(_) => Expr::Character(new_value.chars().next().unwrap()),
            Expr::CharacterDouble(_, _) => Expr::CharacterDouble(
                new_value.chars().next().unwrap(),
                new_value.chars().nth(1).unwrap(),
            ),
            Expr::ReservedKeyword(_) => Expr::ReservedKeyword(new_value.to_string()),
            // Expr::Unary { op, right } => {
            //     // op.to_string(); // does nothing, prevents warning the lazy way
            //     // let op = new_value.chars().next().unwrap();
            //     // let value = new_value.clone().split_off(1);
            //     // eprintln!("new value: {}", value);

            //     let temp = Expr::Unary {
            //         op,
            //         right: Box::new(right.change_value(value)),
            //     };
            //     eprintln!("temp: {}", temp.to_string());
            //     temp

            //     // Expr::Unary {
            //     //     op: op,
            //     //     right: Box::new(self),
            //     // }
            //     //  }
            // }
            // Expr::Unary(op, right) => Expr::Unary(
            //     op: *op,
            //     right: Box::new(right.change_value(new_value))
            // )
            _ => {
                eprintln!("The.. {}", self.to_string());
                eprintln!("Type.. {}", self.get_type());
                panic!("Not implemented yet")
            }
        }

        // self
    }
}
