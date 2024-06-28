pub struct Block {
    pub statements: Vec<Statement>,
}

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

pub struct SetStatement {
    pub variables: Vec<Variable>,
    pub expressions: Vec<Expression>,
}

pub enum CallStatement {
    Default(DefaultCallStatement),
    This(ThisCallStatement),
}

pub struct DefaultCallStatement {
    pub prefix_expression: Box<PrefixExpression>,
    pub arguments: Arguments,
}

pub struct ThisCallStatement {
    pub prefix_expression: Box<PrefixExpression>,
    pub name: NameLiteral,
    pub arguments: Arguments,
}

pub struct DoStatement {
    pub block: Box<Block>,
}

pub struct WhileStatement {
    pub expression: Box<Expression>,
    pub block: Box<Block>,
}

pub struct RepeatStatement {
    pub expression: Box<Expression>,
    pub block: Box<Block>,
}

pub struct IfStatement {
    pub statement: ConditionalStatement,
    pub else_if_statements: Vec<ConditionalStatement>,
    pub else_statement: Option<ElseStatement>,
}

pub struct ConditionalStatement {
    pub expression: Box<Expression>,
    pub block: Box<Block>,
}

pub struct ElseStatement {
    pub block: Box<Block>,
}

pub struct ForStatement {
    pub name: NameLiteral,
    pub lower: Box<Expression>,
    pub upper: Box<Expression>,
    pub increment: Option<Box<Expression>>,
    pub block: Box<Block>,
}

pub struct ForInStatement {
    pub names: Vec<NameLiteral>,
    pub expressions: Vec<Expression>,
    pub block: Box<Block>,
}

pub struct FunctionStatement {
    pub name: NameLiteral,
    pub body: FunctionBody,
}

pub struct LocalFunctionStatement {
    pub name: NameLiteral,
    pub body: FunctionBody,
}

pub struct LocalSetStatement {
    pub names: Vec<NameLiteral>,
    pub expressions: Option<Vec<Expression>>,
}

pub enum LastStatement {
    Return(Vec<Expression>),
    Break,
}

pub struct FunctionBody {
    pub parameters: Vec<Expression>,
    pub varargs: bool,
    pub block: Box<Block>,
}

pub enum Arguments {
    List(Vec<Expression>),
    Table(Table),
    String(StringLiteral),
}

pub struct Table {}

pub enum Variable {
    Name(NameLiteral),
    Bracket(Box<PrefixExpression>, Box<Expression>),
    Dot(Box<PrefixExpression>, NameLiteral),
}

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
    Binary,
    Unary,
}

pub enum PrefixExpression {
    Variable(Variable),
    Call(CallStatement),
    Paren(Box<Expression>),
}

pub struct NameLiteral(String);

pub struct NumberLiteral {
    pub value: f64,
    pub kind: NumberLiteralKind,
}

pub enum NumberLiteralKind {
    Decimal,
    Hexadecimal,
    Binary,
}

pub struct StringLiteral {
    pub value: String,
    pub kind: StringLiteralKind,
}

pub enum StringLiteralKind {
    Short,
    Long(i32),
}
