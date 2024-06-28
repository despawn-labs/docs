use super::{ast, Lex, LexError, Token};

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("{from}")]
    Lex {
        #[from]
        from: LexError,
    },
}

pub type ParserResult<T> = Result<T, ParserError>;

pub struct Parser {
    lex: Lex,
}

impl Parser {
    pub fn new(input: Vec<u8>) -> Self {
        let mut lex = Lex::new(input);
        lex.next();

        Self { lex }
    }

    pub fn parse(&mut self) -> ParserResult<ast::Block> {
        self.parse_block()
    }

    fn parse_block(&mut self) -> ParserResult<ast::Block> {
        let mut statements = vec![];

        loop {
            if self.lex.token() == Token::End {
                break;
            }

            let statement = self.parse_statement()?;
            if statement.is_last() {
                break;
            }

            statements.push(statement);
        }

        Ok(ast::Block { statements })
    }

    fn parse_statement(&mut self) -> ParserResult<ast::Statement> {
        match self.lex.token() {
            Token::If => Ok(ast::Statement::If(self.parse_if_statement()?)),
            _ => panic!(""),
        }
    }

    fn parse_if_statement(&mut self) -> ParserResult<ast::IfStatement> {
        todo!();
    }

    fn parse_then(&mut self) -> ParserResult<ast::ConditionalStatement> {
        todo!();
    }

    fn parse_expr(&mut self) -> ParserResult<ast::Expression> {
        todo!();
    }
}
