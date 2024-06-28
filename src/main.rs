pub mod lang;

use std::fs;

use crate::lang::{Lex, Token};
use anyhow::Result;
use clap::Parser;

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
