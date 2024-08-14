#[allow(dead_code)]
pub enum Expr {
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
        }
    }
}
