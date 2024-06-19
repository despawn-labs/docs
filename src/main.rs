pub mod char;
pub mod lex;
pub mod token;

use std::fs;

use anyhow::Result;
use clap::Parser;
use lex::Lex;
use token::Token;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    #[arg(short, long)]
    input: String,
}

fn main() -> Result<()> {
    let args = Args::parse();

    let data = fs::read_to_string(args.input)?;
    let mut lex = Lex::new(data.into_bytes());

    let mut token = lex.next()?;
    while token != Token::Eof {
        println!("{token:?}");
        token = lex.next()?;
    }

    Ok(())
}
