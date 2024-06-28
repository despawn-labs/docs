//! Adapted from https://github.com/LuaJIT/LuaJIT/blob/v2.1/src/lj_lex.c.

use std::string::FromUtf8Error;

use crate::{
    char_is, char_is_digit, char_is_ident,
    lang::{
        char::{to_digit, CHAR_DIGIT, CHAR_HEX_DIGIT},
        token::Token,
    },
};

#[derive(Debug, thiserror::Error)]
pub enum LexError {
    #[error("failed to parse string: '{from}'")]
    Utf8 {
        #[from]
        from: FromUtf8Error,
    },

    #[error("unexpected end of file")]
    Eof,

    #[error("unexpected character")]
    Char,

    #[error("failed to parse number")]
    Number,

    #[error("failed to parse string")]
    String,

    #[error("unexpected token")]
    Test,
}

pub type LexResult<T> = Result<T, LexError>;

#[derive(Debug)]
pub struct Lex {
    input: Vec<u8>,

    work: Vec<u8>,

    char: u8,
    next_char_index: usize,

    token: Token,
}

impl Lex {
    pub fn new(input: Vec<u8>) -> Self {
        let mut s = Self {
            input,

            work: vec![],

            char: 0,
            next_char_index: 1,

            token: Token::Eof,
        };

        s.char = s.input[0];

        s
    }

    pub fn next(&mut self) -> LexResult<Token> {
        self.token = self.scan()?;
        Ok(self.token)
    }

    pub fn test<F: Fn(&Token) -> bool>(&mut self, f: F) -> LexResult<()> {
        if !f(&self.token) {
            Err(LexError::Test)
        } else {
            Ok(())
        }
    }

    pub fn token(&self) -> Token {
        self.token
    }

    fn work_to_string(&self) -> LexResult<String> {
        Ok(String::from_utf8(self.work.clone())?)
    }

    fn is_end_of_line(&self) -> bool {
        self.char == b'\n' || self.char == b'\r'
    }

    fn next_char(&mut self) -> u8 {
        if self.next_char_index >= self.input.len() {
            self.char = 0;
        } else {
            self.char = self.input[self.next_char_index];
            self.next_char_index += 1;
        }

        self.char
    }

    fn take_char(&mut self) -> u8 {
        self.work.push(self.char);
        self.next_char()
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

        self.take_char();

        let mut i = 0;
        while self.char == b'=' {
            i += 1;

            self.take_char();
        }

        if ic == self.char {
            i
        } else {
            -i - 1
        }
    }

    fn take_long_string(&mut self, sep: i32) {
        self.next_char();

        loop {
            match self.char {
                0 => {
                    panic!("unexpected end of file")
                }
                b']' => {
                    if self.take_eq() == sep {
                        let n = self.work.len();
                        self.work.drain((n - 1 - sep as usize)..n);

                        return;
                    }
                }
                b'\n' | b'\r' => {
                    self.work.push(b'\n');
                    self.take_newline();
                }
                _ => {
                    self.take_char();
                }
            }
        }
    }

    fn take_string(&mut self) -> LexResult<String> {
        let delim = self.char;

        self.next_char();

        while self.char != delim {
            match self.char {
                0 | b'\n' | b'\r' => return Err(LexError::String),
                b'\\' => {
                    let mut c = self.next_char();

                    c = match c {
                        b'a' => b'\x07',
                        b'b' => b'\x08',
                        b'f' => b'\x0C',
                        b'n' => b'\n',
                        b'r' => b'\r',
                        b't' => b'\t',
                        b'v' => b'\x0B',
                        b'\\' => b'\\',
                        b'[' => b'[',
                        b']' => b']',
                        b'"' => b'"',
                        b'\'' => b'\'',
                        _ => return Err(LexError::String),
                    };

                    self.work.push(c);
                    self.next_char();
                }
                _ => {
                    self.take_char();
                }
            }
        }

        self.next_char();

        let result = self.work_to_string()?;
        Ok(result)
    }

    fn take_number(&mut self) -> LexResult<f64> {
        let mut pc = self.char;
        let mut xp = b'e';
        if self.char == b'0' && self.take_char().to_ascii_lowercase() == b'x' {
            pc = b'0';
            xp = b'p';
        }

        while char_is_ident!(self.char)
            || self.char == b'.'
            || ((self.char == b'-' || self.char == b'+') && pc.to_ascii_lowercase() == xp)
        {
            pc = self.char;
            self.take_char();
        }

        let mut base = 10;
        let mut mask = CHAR_DIGIT;

        let mut dec_start = 0usize;
        let mut dec_end = 0usize;

        let mut has_frac = false;
        let mut frac_start = 0usize;
        let mut frac_end = 0usize;

        let mut has_exp = false;
        let mut exp_start = 0usize;
        let mut exp_end = 0usize;

        if self.work.len() >= 2 {
            match (self.work[0], self.work[1].to_ascii_lowercase()) {
                (b'0', b'x') => {
                    base = 16;
                    mask = CHAR_HEX_DIGIT;
                    dec_start = 2;
                }
                (b'0', b'b') => {
                    base = 2;
                    dec_start = 2;
                }
                _ => {}
            };
        }

        let mut i = 0;
        while i < self.work.len() {
            let c = &self.work[i];

            // Provide proper end indices upon last char.
            if i == self.work.len() - 1 {
                if has_frac && !has_exp {
                    frac_end = i;
                } else if has_exp {
                    exp_end = i;
                } else if !has_frac && !has_exp {
                    dec_end = i;
                }
            }

            // Skip the "0{x|b}" in base 16/2 literals.
            if (base == 16 || base == 2) && i < 2 {
                i += 1;
                continue;
            }

            // Mark integer/fraction regions.
            if *c == b'.' && !has_frac && !has_exp {
                has_frac = true;

                dec_end = i - 1;
                frac_start = i + 1;

                i += 1;

                continue;
            }

            // Mark integer/fraction/exponential regions.
            if *c == xp && !has_exp {
                has_exp = true;

                if has_frac {
                    frac_end = i - 1;
                } else {
                    dec_end = i - 1;
                }

                let nc = self.work[i + 1];
                if nc == b'-' || nc == b'+' {
                    i += 1;
                }

                exp_start = i + 1;

                i += 1;

                continue;
            }

            // Check that the char is valid for the given base (10/16).
            if (base == 10 || base == 16) && char_is!(*c, mask) {
                i += 1;

                continue;
            }

            // Check that the char is valid if in base 2.
            if base == 2 && (*c == b'0' || *c == b'1') {
                i += 1;

                continue;
            }

            return Err(LexError::Number);
        }

        #[cfg(feature = "debug")]
        {
            println!(
                "dec - start={dec_start} end={dec_end} - {:?}",
                &self.work[dec_start..=dec_end]
            );

            if has_frac {
                println!(
                    "frac - start={frac_start} end={frac_end} - {:?}",
                    &self.work[frac_start..=frac_end]
                );
            }

            if has_exp {
                println!(
                    "exp - start={exp_start} end={exp_end} - {:?}",
                    &self.work[exp_start..=exp_end]
                );
            }
        }

        // Combine the parts of the number into the final value.
        let mut d = 0f64;

        let mut d_val = 0f64;
        let n_dec = dec_end - dec_start;
        for (i, c) in self.work[dec_start..=dec_end].iter().enumerate() {
            d_val += to_digit(base, *c) * (base as f64).powf((n_dec - i) as f64);
        }

        d += d_val;

        if has_frac {
            let mut f_val = 0f64;
            for (i, c) in self.work[frac_start..=frac_end].iter().enumerate() {
                f_val += to_digit(base, *c) * (base as f64).powf(-(i as f64) - 1f64);
            }

            d += f_val;
        }

        if has_exp {
            let mut e_val = 0f64;
            let n_exp = exp_end - exp_start;
            for (i, c) in self.work[exp_start..=exp_end].iter().enumerate() {
                e_val += to_digit(base, *c) * (base as f64).powf((n_exp - i) as f64);
            }

            d *= 10f64.powf(e_val);
        }

        Ok(d)
    }

    fn scan(&mut self) -> LexResult<Token> {
        self.work.clear();

        loop {
            // Identifier or keyword.
            if char_is_ident!(self.char) {
                if char_is_digit!(self.char) {
                    return Ok(Token::Number(self.take_number()?));
                }

                self.work.clear();

                while char_is_ident!(self.char) {
                    self.take_char();
                }

                let s = self.work_to_string()?;
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
                            self.work.clear();
                            self.take_long_string(sep);

                            return Ok(Token::LongComment(sep, self.work_to_string()?));
                        }
                    }

                    // Short comment.
                    while !self.is_end_of_line() && self.char != 0 {
                        self.take_char();
                    }

                    return Ok(Token::ShortComment(self.work_to_string()?));
                }
                b'[' => {
                    let sep = self.take_eq();

                    if sep >= 0 {
                        self.work.clear();
                        self.take_long_string(sep);

                        return Ok(Token::String(self.work_to_string()?));
                    }

                    if sep == -1 {
                        return Ok(Token::LSquare);
                    }

                    panic!("invalid delim");
                }
                b'=' => {
                    self.next_char();

                    return Ok(if self.char != b'=' {
                        self.next_char();
                        Token::Assign
                    } else {
                        self.next_char();
                        Token::Eq
                    });
                }
                b'>' => {
                    self.next_char();

                    return Ok(if self.char != b'=' {
                        self.next_char();
                        Token::Greater
                    } else {
                        self.next_char();
                        Token::GreaterEq
                    });
                }
                b'<' => {
                    self.next_char();

                    return Ok(if self.char != b'=' {
                        self.next_char();
                        Token::Less
                    } else {
                        self.next_char();
                        Token::LessEq
                    });
                }
                b'~' => {
                    self.next_char();

                    if self.char == b'=' {
                        self.next_char();
                        return Ok(Token::NotEq);
                    }
                }
                b':' => {
                    self.next_char();

                    return Ok(if self.char != b':' {
                        self.next_char();
                        Token::Colon
                    } else {
                        self.next_char();
                        Token::Label
                    });
                }
                b'.' => {
                    self.next_char();

                    if self.char == b'.' {
                        self.next_char();

                        if self.char == b'.' {
                            self.next_char();

                            return Ok(Token::Dots);
                        }

                        return Ok(Token::Concat);
                    }

                    if !char_is_digit!(self.char) {
                        return Ok(Token::Dot);
                    }

                    return Ok(Token::Number(self.take_number()?));
                }
                b'\'' | b'"' => return Ok(Token::String(self.take_string()?)),
                0 => return Ok(Token::Eof),
                c => {
                    self.take_char();
                    return Token::from_char(c).ok_or(LexError::Char);
                }
            }
        }
    }
}
