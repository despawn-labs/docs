// pub const CHAR_CONTROL: u8 = 0x01;
// pub const CHAR_SPACE: u8 = 0x02;
// pub const CHAR_PUNCTUATION: u8 = 0x04;
pub const CHAR_DIGIT: u8 = 0x08;
pub const CHAR_HEX_DIGIT: u8 = 0x10;
// pub const CHAR_UPPER: u8 = 0x20;
// pub const CHAR_LOWER: u8 = 0x40;
pub const CHAR_IDENT: u8 = 0x80;
// pub const CHAR_ALPHA: u8 = CHAR_LOWER | CHAR_UPPER;
// pub const CHAR_ALPHA_NUMERIC: u8 = CHAR_ALPHA | CHAR_DIGIT;
// pub const CHAR_GRAPH: u8 = CHAR_ALPHA_NUMERIC | CHAR_PUNCTUATION;

pub const CHAR_FLAGS: [u8; 257] = [
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 2, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 152, 152, 152, 152, 152, 152, 152, 152, 152,
    152, 4, 4, 4, 4, 4, 4, 4, 176, 176, 176, 176, 176, 176, 160, 160, 160, 160, 160, 160, 160, 160,
    160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 4, 4, 4, 4, 132, 4, 208, 208, 208,
    208, 208, 208, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192, 192,
    192, 192, 192, 192, 4, 4, 4, 4, 1, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
    128, 128,
];

#[macro_export]
macro_rules! char_is {
    ($c: expr, $t: expr) => {
        (((crate::lang::char::CHAR_FLAGS[1 + ($c as usize)]) & $t) != 0)
    };
}

#[macro_export]
macro_rules! char_is_ident {
    ($c: expr) => {
        crate::char_is!($c, crate::lang::char::CHAR_IDENT)
    };
}

#[macro_export]
macro_rules! char_is_digit {
    ($c: expr) => {
        crate::char_is!($c, crate::lang::char::CHAR_DIGIT)
    };
}

pub fn to_digit(base: u32, c: u8) -> f64 {
    match base {
        2 | 10 => (c - b'0') as f64,
        16 => match c.to_ascii_lowercase() {
            b'0'..=b'9' => (c - b'0') as f64,
            b'a'..=b'f' => 10f64 + (c.to_ascii_lowercase() - b'a') as f64,
            _ => panic!("invalid digit"),
        },
        _ => panic!("invalid base"),
    }
}
