#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Type {
    Boolean,
    String,
    Nothing,
    #[default]
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
    DiaryRecord,
    TeamMember,
    For,
    If,
    Switch,
    While,
    Exception,
    With,
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
    Binary(Type, &'static str, Type, Type, &'static str),
    Unary(&'static str, Type, Type, &'static str),
    Nullary(&'static str, Type, &'static str),
}
