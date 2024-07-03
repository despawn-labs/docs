#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Set(SetStatement),
    Call(CallStatement),
    Do(DoStatement),
    While(WhileStatement),
    Repeat(RepeatStatement),
    If(IfStatement),
    For(ForStatement),
    ForIn(ForInStatement),
    Function(FunctionStatement),
    LocalFunction(LocalFunctionStatement),
    LocalSet(LocalSetStatement),
    Last(LastStatement),
}

macro_rules! define_is {
    ($f: ident, $v: ident) => {
        pub fn $f(&self) -> bool {
            match self {
                Statement::$v(_) => true,
                _ => false,
            }
        }
    };
}

impl Statement {
    define_is!(is_set, Set);
    define_is!(is_call, Call);
    define_is!(is_do, Do);
    define_is!(is_while, While);
    define_is!(is_repeat, Repeat);
    define_is!(is_if, If);
    define_is!(is_for, For);
    define_is!(is_for_in, ForIn);
    define_is!(is_function, Function);
    define_is!(is_local_function, LocalFunction);
    define_is!(is_local_set, LocalSet);
    define_is!(is_last, Last);
}

#[derive(Debug, Clone, PartialEq)]
pub struct SetStatement {
    pub variables: Vec<PrefixExpression>,
    pub expressions: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallStatement {
    Default(DefaultCallStatement),
    This(ThisCallStatement),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DefaultCallStatement {
    pub prefix_expression: Box<PrefixExpression>,
    pub arguments: FunctionArguments,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ThisCallStatement {
    pub prefix: Box<PrefixExpression>,
    pub name: NameLiteral,
    pub arguments: FunctionArguments,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoStatement {
    pub block: Box<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStatement {
    pub expression: Box<Expression>,
    pub block: Box<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RepeatStatement {
    pub expression: Box<Expression>,
    pub block: Box<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub statement: ConditionalStatement,
    pub else_if_statements: Vec<ConditionalStatement>,
    pub else_statement: Option<ElseStatement>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalStatement {
    pub expression: Box<Expression>,
    pub block: Box<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElseStatement {
    pub block: Box<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStatement {
    pub name: NameLiteral,
    pub index: Box<Expression>,
    pub limit: Box<Expression>,
    pub step: Option<Box<Expression>>,
    pub block: Box<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForInStatement {
    pub names: Vec<NameLiteral>,
    pub expressions: Vec<Expression>,
    pub block: Box<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionStatement {
    pub name: FunctionName,
    pub body: FunctionBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalFunctionStatement {
    pub name: NameLiteral,
    pub body: FunctionBody,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionName {
    pub segments: Vec<NameLiteral>,
    pub ending_segment: Option<NameLiteral>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LocalSetStatement {
    pub names: Vec<NameLiteral>,
    pub expressions: Option<Vec<Expression>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LastStatement {
    Return(Vec<Expression>),
    Break,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionBody {
    pub parameters: Vec<NameLiteral>,
    pub varargs: bool,
    pub block: Box<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionArguments {
    List(Vec<Expression>),
    Table(Table),
    String(StringLiteral),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Table {
    pub fields: Vec<TableField>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TableField {
    Index {
        key: Box<Expression>,
        value: Box<Expression>,
    },
    Name {
        key: NameLiteral,
        value: Box<Expression>,
    },
    Expression {
        value: Box<Expression>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Variable {
    Name {
        name: NameLiteral,
    },
    Bracket {
        prefix: Box<PrefixExpression>,
        key: Box<Expression>,
    },
    Dot {
        prefix: Box<PrefixExpression>,
        key: NameLiteral,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Nil,
    False,
    True,
    Number(NumberLiteral),
    String(StringLiteral),
    Dots,
    Function(FunctionBody),
    Prefix(PrefixExpression),
    Table(Table),
    Binary(BinaryExpression),
    Unary(UnaryExpression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixExpression {
    Variable(Variable),
    Call(CallStatement),
    Paren(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryExpressionKind {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulus,
    Pow,
    Concat,
    NotEq,
    Eq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub kind: BinaryExpressionKind,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryExpressionKind {
    Not,
    Negate,
    Pound,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpression {
    pub kind: UnaryExpressionKind,
    pub inner: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct NameLiteral(pub String);

#[derive(Debug, Clone, PartialEq)]
pub struct NumberLiteral {
    pub value: f64,
    pub kind: NumberLiteralKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum NumberLiteralKind {
    Decimal,
    Hexadecimal,
    Binary,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub value: String,
    pub kind: StringLiteralKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringLiteralKind {
    Short,
    Long(u32),
}
