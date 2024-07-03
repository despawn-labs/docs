pub mod lang;

use std::fs;

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
    let mut parser = lang::Parser::new(data.into_bytes())?;
    let chunk = parser.parse()?;
    println!("{:#?}", chunk);

    Ok(())
}
