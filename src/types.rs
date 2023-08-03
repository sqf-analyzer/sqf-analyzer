#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Boolean,
    String,
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

impl Type {
    #[inline]
    pub fn consistent(self, other: Self) -> bool {
        match (self, other) {
            (_, Type::Anything) | (Type::Anything, _) => true,
            (lhs, rhs) => lhs == rhs,
        }
    }
}
pub enum Signature {
    Binary(Type, &'static str, Type, Type),
    Unary(&'static str, Type, Type),
    Nullary(&'static str, Type),
}

pub type Span = (usize, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub inner: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn map<R, F: FnOnce(T) -> R>(self, f: F) -> Spanned<R> {
        Spanned::<R> {
            inner: f(self.inner),
            span: self.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(String),
    String(String),
    Array(Vec<Spanned<Expr>>),
    Boolean(String),
    Code(Vec<Spanned<Expr>>),
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
    Variable(Spanned<String>),
    Include(Spanned<String>),
    Macro(Spanned<String>, Spanned<String>),
    Define(Define),
    BinaryOp {
        lhs: Box<Spanned<Expr>>,
        op: Spanned<String>,
        rhs: Box<Spanned<Expr>>,
    },
    UnaryOp {
        op: Spanned<String>,
        rhs: Box<Spanned<Expr>>,
    },
    Assignment {
        is_private: bool,
        variable: Spanned<String>,
        expr: Box<Spanned<Expr>>,
    },
    Error,
}
