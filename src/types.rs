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
