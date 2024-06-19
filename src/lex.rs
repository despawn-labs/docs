//! Adapted from https://github.com/LuaJIT/LuaJIT/blob/v2.1/src/lj_lex.c.

use std::string::FromUtf8Error;

use crate::{char_is_digit, char_is_ident, token::Token};

#[derive(Debug, thiserror::Error)]
pub enum LexError {
    #[error("Failed to parse string: {from}")]
    Utf8 {
        #[from]
        from: FromUtf8Error,
    },
}

pub type LexResult<T> = Result<T, LexError>;

#[derive(Debug)]
pub struct Lex {
    input: Vec<u8>,
    buffer: Vec<u8>,

    char: u8,
    next_char_index: usize,

    token: Option<Token>,
    next_token: Option<Token>,
}

impl Lex {
    pub fn new(input: Vec<u8>) -> Self {
        Self {
            input,
            buffer: vec![],

            char: 0,
            next_char_index: 0,

            token: None,
            next_token: None,
        }
    }

    pub fn next(&mut self) -> LexResult<Token> {
        self.next_char();
        self.scan()
    }

    fn next_char(&mut self) {
        if self.next_char_index >= self.input.len() {
            self.char = 0;
        } else {
            self.char = self.input[self.next_char_index];
        }

        self.next_char_index += 1;
    }

    fn save_next_char(&mut self) {
        self.next_char();
        self.buffer.push(self.char);
    }

    fn get_buffer_string(&self) -> LexResult<String> {
        Ok(String::from_utf8(self.buffer.clone())?)
    }

    fn is_end_of_line(&self) -> bool {
        self.char == b'\n' || self.char == b'\r'
    }

    fn take_newline(&mut self) {
        let last = self.char;
        assert!(self.is_end_of_line());

        self.next_char();

        if self.is_end_of_line() && self.char != last {
            self.next_char();
        }
    }

    fn take_eq(&mut self) -> i32 {
        let ic = self.char;

        self.save_next_char();

        let mut i = 0;
        while self.char == b'=' {
            i += 1;

            self.save_next_char();
        }

        if ic == self.char {
            i
        } else {
            -i - 1
        }
    }

    fn take_long_string(&mut self, sep: i32) {
        self.save_next_char();

        loop {
            match self.char {
                0 => {
                    panic!("unexpected end of file")
                }
                b']' => {
                    if self.take_eq() == sep {
                        let n = self.buffer.len();
                        self.buffer.drain((n - 2 - sep as usize)..n);

                        return;
                    }
                }
                b'\n' | b'\r' => {
                    self.buffer.push(b'\n');
                    self.take_newline();
                }
                _ => {
                    self.save_next_char();
                }
            }
        }
    }

    fn scan(&mut self) -> LexResult<Token> {
        self.buffer.clear();

        loop {
            // Identifier or keyword.
            if char_is_ident!(self.char) {
                let mut buf = vec![];

                while char_is_ident!(self.char) {
                    buf.push(self.char);
                    self.next_char();
                }

                let s = String::from_utf8(buf)?;
                let ident = Token::from_word(s.as_str());

                return Ok(ident.unwrap_or(Token::Name(s)));
            }

            match self.char {
                b'\n' | b'\r' => {
                    self.take_newline();
                    continue;
                }
                b' ' | b'\t' => {
                    self.next_char();
                    continue;
                }
                b'-' => {
                    self.next_char();

                    // Minus.
                    if self.char != b'-' {
                        return Ok(Token::Subtract);
                    }

                    self.next_char();

                    // Long comment.
                    if self.char == b'[' {
                        let sep = self.take_eq();

                        if sep >= 0 {
                            self.buffer.clear();
                            self.take_long_string(sep);
                            return Ok(Token::LongComment(sep, self.get_buffer_string()?));
                        }
                    }

                    // Short comment.
                    while !self.is_end_of_line() && self.char != 0 {
                        self.buffer.push(self.char);
                        self.next_char();
                    }

                    return Ok(Token::ShortComment(self.get_buffer_string()?));
                }
                b'[' => {
                    let sep = self.take_eq();

                    if sep >= 0 {
                        self.buffer.clear();
                        self.take_long_string(sep);

                        return Ok(Token::String(self.get_buffer_string()?));
                    }

                    if sep == -1 {
                        return Ok(Token::LSquare);
                    }

                    panic!("invalid delim");
                }
                b'=' => {
                    self.next_char();

                    return Ok(if self.char != b'=' {
                        Token::Assign
                    } else {
                        Token::Eq
                    });
                }
                b'>' => {
                    self.next_char();

                    return Ok(if self.char != b'=' {
                        Token::Greater
                    } else {
                        Token::GreaterEq
                    });
                }
                b'<' => {
                    self.next_char();

                    return Ok(if self.char != b'=' {
                        Token::Less
                    } else {
                        Token::LessEq
                    });
                }
                b'~' => {
                    self.next_char();

                    if self.char == b'=' {
                        return Ok(Token::NotEq);
                    }
                }
                b':' => {
                    self.next_char();

                    return Ok(if self.char != b':' {
                        Token::Colon
                    } else {
                        Token::Label
                    });
                }
                b'.' => {
                    self.next_char();

                    if self.char == b'.' {
                        self.next_char();

                        if self.char == b'.' {
                            return Ok(Token::Dots);
                        }

                        return Ok(Token::Concat);
                    }

                    if !char_is_digit!(self.char) {
                        return Ok(Token::Dot);
                    }
                }
                0 => return Ok(Token::Eof),
                _ => panic!(""),
            }
        }
    }
}
