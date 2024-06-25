#[derive(Debug, PartialEq, PartialOrd)]
pub enum Token {
    And,
    Break,
    Do,
    Else,
    ElseIf,
    End,
    False,
    For,
    Function,
    Goto,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,

    Concat, // ..

    Dot,  // .
    Dots, // ...

    GreaterEq, // >=
    Greater,   // >

    LessEq, // <=
    Less,   // <

    Eq,     // ==
    NotEq,  // ~=
    Assign, // =

    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /

    Modulus, // %
    Carrot,  // ^

    Pound, // #

    LSquare, // [
    RSquare, // ]

    LCurly, // {
    RCurly, // }

    LParen, // (
    RParen, // )

    SemiColon, // ;
    Colon,     // :

    Comma, // ,

    Label, // ::

    LongComment(i32, String), // --[=*[]=*]
    ShortComment(String),     // --

    Number(f64),
    String(String),
    Name(String),

    Eof,
}

impl Token {
    pub fn from_word(word: &str) -> Option<Token> {
        match word {
            "and" => Some(Token::And),
            "break" => Some(Token::Break),
            "do" => Some(Token::Do),
            "else" => Some(Token::Else),
            "elseif" => Some(Token::ElseIf),
            "end" => Some(Token::End),
            "false" => Some(Token::False),
            "for" => Some(Token::For),
            "function" => Some(Token::Function),
            "goto" => Some(Token::Goto),
            "if" => Some(Token::If),
            "in" => Some(Token::In),
            "local" => Some(Token::Local),
            "nil" => Some(Token::Nil),
            "not" => Some(Token::Not),
            "or" => Some(Token::Or),
            "repeat" => Some(Token::Repeat),
            "return" => Some(Token::Return),
            "then" => Some(Token::Then),
            "true" => Some(Token::True),
            "until" => Some(Token::Until),
            "while" => Some(Token::While),
            _ => None,
        }
    }

    pub fn from_char(char: u8) -> Option<Token> {
        match char {
            b'+' => Some(Token::Add),
            b'-' => Some(Token::Subtract),
            b'*' => Some(Token::Multiply),
            b'/' => Some(Token::Divide),
            b'%' => Some(Token::Modulus),
            b'^' => Some(Token::Carrot),
            b'#' => Some(Token::Pound),
            b'[' => Some(Token::LSquare),
            b']' => Some(Token::RSquare),
            b'{' => Some(Token::LCurly),
            b'}' => Some(Token::RCurly),
            b'(' => Some(Token::LParen),
            b')' => Some(Token::RParen),
            b';' => Some(Token::SemiColon),
            b':' => Some(Token::Colon),
            b',' => Some(Token::Comma),
            _ => None,
        }
    }
}
