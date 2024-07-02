use super::{
    ast,
    token::{self},
    Lex, LexError, Token,
};

#[inline]
fn precedence(token: &Token) -> (u8, u8) {
    match token {
        Token::Add | Token::Subtract => (6, 6),
        Token::Multiply | Token::Divide | Token::Modulus => (7, 7),
        Token::Carrot => (10, 9),
        Token::Concat => (5, 4),
        Token::Eq
        | Token::NotEq
        | Token::Less
        | Token::LessEq
        | Token::Greater
        | Token::GreaterEq => (3, 3),
        Token::And => (2, 2),
        Token::Or => (1, 1),
        Token::Dot => (12, 12),
        _ => (0, 0),
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("lexical analysis error: {from}")]
    Lex {
        #[from]
        from: LexError,
    },

    #[error("unexpected token: '{token:?}'")]
    UnexpectedToken { token: Token },

    #[error("missing closing '{end:?}' for '{start:?}' on line {line}")]
    Match { start: Token, end: Token, line: u32 },
}

pub type ParserResult<T> = Result<T, ParserError>;

pub struct Parser {
    lex: Lex,
}

impl Parser {
    pub fn new(input: Vec<u8>) -> ParserResult<Self> {
        let mut lex = Lex::new(input);
        lex.next()?;

        Ok(Self { lex })
    }

    pub fn parse(&mut self) -> ParserResult<ast::Block> {
        self.parse_block()
    }

    #[inline]
    fn check<F: Fn(&Token) -> bool>(&self, f: F) -> ParserResult<()> {
        let token = self.lex.token();
        if f(&token) {
            Ok(())
        } else {
            Err(ParserError::UnexpectedToken { token })
        }
    }

    #[inline]
    fn check_match(&self, start: Token, end: Token, line: u32) -> ParserResult<()> {
        if self.lex.token() == end {
            Ok(())
        } else {
            Err(ParserError::Match { start, end, line })
        }
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
        self.lex.next()?;

        let expr = self.parse_expr_binary(0)?;
        println!("{:#?}", expr);

        todo!();
    }

    fn parse_then(&mut self) -> ParserResult<ast::ConditionalStatement> {
        todo!();
    }

    fn parse_expr_binary(&mut self, min_prec: u8) -> ParserResult<ast::Expression> {
        let mut lhs = self.parse_expr_unary()?;

        let mut tk = self.lex.token();
        while tk.is_binary_operator() && precedence(&tk).0 > min_prec {
            self.lex.next()?;

            let rhs = self.parse_expr_binary(precedence(&tk).1)?;

            let kind = match tk {
                Token::Add => ast::BinaryExpressionKind::Add,
                Token::Subtract => ast::BinaryExpressionKind::Subtract,
                Token::Divide => ast::BinaryExpressionKind::Divide,
                Token::Multiply => ast::BinaryExpressionKind::Multiply,
                Token::Carrot => ast::BinaryExpressionKind::Pow,
                Token::Modulus => ast::BinaryExpressionKind::Modulus,
                Token::Concat => ast::BinaryExpressionKind::Concat,
                Token::NotEq => ast::BinaryExpressionKind::NotEq,
                Token::Eq => ast::BinaryExpressionKind::Eq,
                Token::Less => ast::BinaryExpressionKind::Less,
                Token::LessEq => ast::BinaryExpressionKind::LessEq,
                Token::Greater => ast::BinaryExpressionKind::Greater,
                Token::GreaterEq => ast::BinaryExpressionKind::GreaterEq,
                Token::And => ast::BinaryExpressionKind::And,
                Token::Or => ast::BinaryExpressionKind::Or,
                x => panic!("{x:?}"),
            };

            lhs = ast::Expression::Binary(ast::BinaryExpression {
                kind,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            });
            tk = self.lex.token();
        }

        Ok(lhs)
    }

    fn parse_expr_unary(&mut self) -> ParserResult<ast::Expression> {
        let kind = match self.lex.token() {
            Token::Not => ast::UnaryExpressionKind::Not,
            Token::Subtract => ast::UnaryExpressionKind::Negate,
            Token::Pound => ast::UnaryExpressionKind::Pound,
            _ => return self.parse_expr_simple(),
        };

        self.lex.next()?;

        Ok(ast::Expression::Unary(ast::UnaryExpression {
            kind,
            inner: Box::new(self.parse_expr_binary(8)?),
        }))
    }

    fn parse_expr_simple(&mut self) -> ParserResult<ast::Expression> {
        match self.lex.token() {
            Token::Number(value, kind) => {
                self.lex.next()?;
                return Ok(ast::Expression::Number(ast::NumberLiteral {
                    value,
                    kind: match kind {
                        token::NumberKind::Decimal => ast::NumberLiteralKind::Decimal,
                        token::NumberKind::Hexadecimal => ast::NumberLiteralKind::Hexadecimal,
                        token::NumberKind::Binary => ast::NumberLiteralKind::Binary,
                    },
                }));
            }
            Token::String(value, kind) => {
                self.lex.next()?;
                return Ok(ast::Expression::String(ast::StringLiteral {
                    value: value.clone(),
                    kind: match kind {
                        token::StringKind::Short => ast::StringLiteralKind::Short,
                        token::StringKind::Long(sep) => ast::StringLiteralKind::Long(sep),
                    },
                }));
            }
            Token::Nil => {
                self.lex.next()?;
                return Ok(ast::Expression::Nil);
            }
            Token::True => {
                self.lex.next()?;
                return Ok(ast::Expression::True);
            }
            Token::False => {
                self.lex.next()?;
                return Ok(ast::Expression::False);
            }
            Token::Dots => {
                self.lex.next()?;
                return Ok(ast::Expression::Dots);
            }
            Token::LCurly => return Ok(ast::Expression::Table(self.parse_table()?)),
            Token::Function => todo!(),
            _ => self.parse_expr_suffixed(),
        }
    }

    fn parse_expr_suffixed(&mut self) -> ParserResult<ast::Expression> {
        let mut expr = self.parse_expr_prefix()?;

        loop {
            match self.lex.token() {
                Token::Dot => {
                    self.lex.next()?;

                    let name = match self.lex.token() {
                        Token::Name(name) => ast::NameLiteral(name),
                        _ => {
                            return Err(ParserError::UnexpectedToken {
                                token: self.lex.token(),
                            })
                        }
                    };

                    self.lex.next()?;

                    expr = ast::PrefixExpression::Variable(ast::Variable::Dot {
                        prefix: Box::new(expr),
                        key: name,
                    });
                }
                Token::LSquare => {
                    let line = self.lex.line_number();
                    self.lex.next()?;

                    let index_expr = self.parse_expr_binary(0)?;

                    expr = ast::PrefixExpression::Variable(ast::Variable::Bracket {
                        prefix: Box::new(expr),
                        key: Box::new(index_expr),
                    });

                    match self.lex.token() {
                        Token::RSquare => {}
                        _ => {
                            return Err(ParserError::Match {
                                start: Token::LSquare,
                                end: Token::RSquare,
                                line,
                            })
                        }
                    }

                    self.lex.next()?;
                }
                Token::Colon => {
                    self.lex.next()?;

                    let name = match self.lex.token() {
                        Token::Name(name) => ast::NameLiteral(name),
                        _ => {
                            return Err(ParserError::UnexpectedToken {
                                token: self.lex.token(),
                            })
                        }
                    };

                    let arguments = self.parse_function_arguments()?;

                    expr = ast::PrefixExpression::Call(ast::CallStatement::This(
                        ast::ThisCallStatement {
                            prefix: Box::new(expr),
                            name,
                            arguments,
                        },
                    ));
                }
                Token::String(_, _) | Token::LCurly | Token::LParen => {
                    expr = ast::PrefixExpression::Call(ast::CallStatement::Default(
                        ast::DefaultCallStatement {
                            prefix_expression: Box::new(expr),
                            arguments: self.parse_function_arguments()?,
                        },
                    ))
                }
                _ => {
                    return Ok(ast::Expression::Prefix(expr));
                }
            }
        }
    }

    fn parse_expr_prefix(&mut self) -> ParserResult<ast::PrefixExpression> {
        match self.lex.token() {
            Token::LParen => {
                let line = self.lex.line_number();
                self.lex.next()?;

                let expr = self.parse_expr_binary(0)?;

                self.check_match(Token::LParen, Token::RParen, line)?;

                self.lex.next()?;

                return Ok(ast::PrefixExpression::Paren(Box::new(expr)));
            }
            Token::Name(name) => {
                self.lex.next()?;
                return Ok(ast::PrefixExpression::Variable(ast::Variable::Name {
                    name: ast::NameLiteral(name),
                }));
            }
            _ => {
                return Err(ParserError::UnexpectedToken {
                    token: self.lex.token(),
                })
            }
        }
    }

    fn parse_function_arguments(&mut self) -> ParserResult<ast::FunctionArguments> {
        match self.lex.token() {
            Token::LParen => {
                let line = self.lex.line_number();
                self.lex.next()?;

                let arguments = match self.lex.token() {
                    Token::RParen => vec![],
                    _ => self.parse_expression_list()?,
                };

                self.check_match(Token::LParen, Token::RParen, line)?;

                Ok(ast::FunctionArguments::List(arguments))
            }
            Token::LCurly => Ok(ast::FunctionArguments::Table(self.parse_table()?)),
            Token::String(value, kind) => {
                match kind {
                    token::StringKind::Short => {}
                    token::StringKind::Long(_) => {
                        return Err(ParserError::UnexpectedToken {
                            token: self.lex.token(),
                        })
                    }
                }

                Ok(ast::FunctionArguments::String(ast::StringLiteral {
                    value: value.clone(),
                    kind: ast::StringLiteralKind::Short,
                }))
            }
            // TODO: Add more specific errors.
            _ => Err(ParserError::UnexpectedToken {
                token: self.lex.token(),
            }),
        }
    }

    fn parse_expression_list(&mut self) -> ParserResult<Vec<ast::Expression>> {
        let mut exprs = vec![];

        loop {
            exprs.push(self.parse_expr_binary(0)?);

            match self.lex.token() {
                Token::Comma => {}
                _ => break,
            }

            self.lex.next()?;
        }

        Ok(exprs)
    }

    fn parse_table(&mut self) -> ParserResult<ast::Table> {
        self.check(|f| *f == Token::LCurly)?;
        self.lex.next()?;

        let mut fields = vec![];

        match self.lex.token() {
            Token::RCurly => return Ok(ast::Table { fields }),
            _ => {}
        }

        loop {
            fields.push(self.parse_table_field()?);

            match self.lex.token() {
                Token::RCurly => break,
                _ => {}
            }
        }

        self.lex.next()?;

        Ok(ast::Table { fields })
    }

    fn parse_table_field(&mut self) -> ParserResult<ast::TableField> {
        match self.lex.token() {
            Token::LSquare => {
                let line = self.lex.line_number();
                self.lex.next()?;

                let key = Box::new(self.parse_expr_binary(0)?);

                match self.lex.token() {
                    Token::RSquare => {}
                    _ => {
                        return Err(ParserError::Match {
                            start: Token::LSquare,
                            end: Token::RSquare,
                            line,
                        })
                    }
                }

                self.lex.next()?;

                match self.lex.token() {
                    Token::Assign => {}
                    _ => {
                        return Err(ParserError::UnexpectedToken {
                            token: self.lex.token(),
                        })
                    }
                }

                self.lex.next()?;

                let value = Box::new(self.parse_expr_binary(0)?);

                Ok(ast::TableField::Index { key, value })
            }
            Token::Name(name) => {
                self.lex.next()?;

                match self.lex.token() {
                    Token::Assign => {}
                    _ => {
                        return Err(ParserError::UnexpectedToken {
                            token: self.lex.token(),
                        })
                    }
                }

                self.lex.next()?;

                Ok(ast::TableField::Name {
                    key: ast::NameLiteral(name),
                    value: Box::new(self.parse_expr_binary(0)?),
                })
            }
            _ => Ok(ast::TableField::Expression {
                value: Box::new(self.parse_expr_binary(0)?),
            }),
        }
    }
}
