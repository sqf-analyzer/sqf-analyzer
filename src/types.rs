#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Boolean,
    String,
    Type,
    Nothing,
    Anything,
    Namespace,
    Number,
    Code,
    Array,
    HashMap,
    Object,
    Config,
    Script,
    Control,
    Group,
    Display,
    Side,
    Task,
    Location,
    NetObject,
    DiaryReport,
    TeamMember,
    ForType,
    IfType,
    SwitchType,
    WhileType,
    TryType,
    WithType,
}

pub enum Signature {
    Binary(Type, &'static str, Type, Type),
    Unary(&'static str, Type, Type),
    Nullary(&'static str, Type),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Span<T> {
    pub inner: T,
    pub span: (usize, usize),
}

impl<T> Span<T> {
    pub fn map<R, F: FnOnce(T) -> R>(self, f: F) -> Span<R> {
        Span::<R> {
            inner: f(self.inner),
            span: self.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(String),
    String(String),
    Array(Vec<Span<Expr>>),
    Boolean(String),
    Code(Vec<Span<Expr>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Define {
    pub name: String,
    pub arguments: Vec<String>,
    pub body: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Value(Value),
    Variable(Span<String>),
    Include(Span<String>),
    Macro(Span<String>, Span<String>),
    Define(Define),
    BinaryOp {
        lhs: Box<Span<Expr>>,
        op: Span<String>,
        rhs: Box<Span<Expr>>,
    },
    UnaryOp {
        op: Span<String>,
        rhs: Box<Span<Expr>>,
    },
    Assignment {
        is_private: bool,
        variable: Span<String>,
        expr: Box<Span<Expr>>,
    },
}
