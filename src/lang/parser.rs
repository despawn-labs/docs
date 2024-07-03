use super::{
    ast::{self, NameLiteral, Statement, ThisCallStatement},
    token, Lex, LexError, Token,
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

    #[error("unexpected statement, block has been terminated")]
    UnexpectedStatement,

    #[error("unexpected expression, only calls are valid")]
    UnexpectedExpression,

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
        self.parse_block(&[])
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

    fn parse_block(&mut self, ending: &[Token]) -> ParserResult<ast::Block> {
        let mut statements = vec![];

        let mut hit_last = false;

        loop {
            match self.lex.token() {
                Token::Eof => break,
                token => {
                    if ending.iter().find(|tk| **tk == token).is_some() {
                        break;
                    }
                }
            }

            if hit_last {
                return Err(ParserError::UnexpectedStatement);
            }

            let statement = self.parse_statement()?;
            if statement.is_last() {
                hit_last = true;
            }

            statements.push(statement);
        }

        Ok(ast::Block { statements })
    }

    fn parse_statement(&mut self) -> ParserResult<ast::Statement> {
        match self.lex.token() {
            Token::Do => Ok(ast::Statement::Do(self.parse_do_statement()?)),
            Token::If => Ok(ast::Statement::If(self.parse_if_statement()?)),
            Token::For => Ok(self.parse_for_statement()?),
            Token::Repeat => Ok(ast::Statement::Repeat(self.parse_repeat_statement()?)),
            Token::While => Ok(ast::Statement::While(self.parse_while_statement()?)),
            Token::Function => Ok(ast::Statement::Function(self.parse_function_statement()?)),
            Token::Local => Ok(self.parse_local_statement()?),
            Token::Return | Token::Break => Ok(ast::Statement::Last(self.parse_last_statement()?)),
            _ => {
                let expr = self.parse_expr_suffixed()?;

                match self.lex.token() {
                    Token::Eq | Token::Comma => {
                        Ok(ast::Statement::Set(self.parse_set_statement(expr)?))
                    }
                    _ => Ok(ast::Statement::Call(self.parse_call_statement(expr)?)),
                }
            }
        }
    }

    fn parse_if_statement(&mut self) -> ParserResult<ast::IfStatement> {
        self.lex.next()?; // if

        let statement = self.parse_then()?;

        let mut else_if_statements = vec![];
        let mut else_statement = None;

        loop {
            match self.lex.token() {
                Token::ElseIf => {
                    self.lex.next()?;

                    else_if_statements.push(self.parse_then()?);
                }
                Token::Else => {
                    self.lex.next()?;

                    else_statement = Some(ast::ElseStatement {
                        block: Box::new(self.parse_block(&[
                            Token::Else,
                            Token::ElseIf,
                            Token::End,
                        ])?),
                    });
                }
                Token::End => {
                    self.lex.next()?;

                    break;
                }
                _ => {
                    return Err(ParserError::UnexpectedToken {
                        token: self.lex.token(),
                    })
                }
            }
        }

        Ok(ast::IfStatement {
            statement,
            else_if_statements,
            else_statement,
        })
    }

    fn parse_then(&mut self) -> ParserResult<ast::ConditionalStatement> {
        let expression = self.parse_expr_binary(0)?;

        self.lex.next()?; // then

        let block = self.parse_block(&[Token::ElseIf, Token::Else, Token::End])?;

        Ok(ast::ConditionalStatement {
            expression: Box::new(expression),
            block: Box::new(block),
        })
    }

    fn parse_do_statement(&mut self) -> ParserResult<ast::DoStatement> {
        self.lex.next()?; // do

        let block = self.parse_block(&[Token::End])?;

        self.lex.next()?; // end

        Ok(ast::DoStatement {
            block: Box::new(block),
        })
    }

    fn parse_for_statement(&mut self) -> ParserResult<ast::Statement> {
        let name_1 = self.lex.next()?; // for

        let name_1 = match name_1 {
            Token::Name(name) => ast::NameLiteral(name.clone()),
            token => return Err(ParserError::UnexpectedToken { token }),
        };

        self.lex.next()?; // name_1

        match self.lex.token() {
            Token::Assign => Ok(ast::Statement::For(
                self.parse_numeric_for_statement(name_1)?,
            )),
            Token::Comma | Token::In => Ok(ast::Statement::ForIn(
                self.parse_list_for_statement(name_1)?,
            )),
            token => Err(ParserError::UnexpectedToken { token }),
        }
    }

    fn parse_numeric_for_statement(
        &mut self,
        name: ast::NameLiteral,
    ) -> ParserResult<ast::ForStatement> {
        self.lex.next()?; // =

        let index = Box::new(self.parse_expr_binary(0)?);

        self.check(|tk| *tk == Token::Comma)?;
        self.lex.next()?;

        let limit = Box::new(self.parse_expr_binary(0)?);

        let step = match self.lex.token() {
            Token::Comma => {
                self.lex.next()?;
                Some(Box::new(self.parse_expr_binary(0)?))
            }
            _ => None,
        };

        self.check(|tk| *tk == Token::Do)?;
        self.lex.next()?; // do

        let block = Box::new(self.parse_block(&[Token::End])?);

        self.lex.next()?; // end

        Ok(ast::ForStatement {
            name,
            index,
            limit,
            step,
            block,
        })
    }

    fn parse_list_for_statement(
        &mut self,
        name: ast::NameLiteral,
    ) -> ParserResult<ast::ForInStatement> {
        let mut names = vec![name];

        match self.lex.token() {
            Token::In => {}
            Token::Comma => {
                loop {
                    self.lex.next()?; // ,

                    match self.lex.token() {
                        Token::Name(name) => names.push(ast::NameLiteral(name)),
                        token => return Err(ParserError::UnexpectedToken { token }),
                    };

                    self.lex.next()?; // name

                    match self.lex.token() {
                        Token::Comma => {}
                        Token::In => break,
                        token => return Err(ParserError::UnexpectedToken { token }),
                    }
                }
            }
            token => return Err(ParserError::UnexpectedToken { token }),
        }

        self.lex.next()?; // in

        let expressions = self.parse_expression_list()?;

        self.check(|tk| *tk == Token::Do)?;
        self.lex.next()?; // do

        let block = Box::new(self.parse_block(&[Token::End])?);

        self.lex.next()?; // end

        Ok(ast::ForInStatement {
            names,
            expressions,
            block,
        })
    }

    fn parse_repeat_statement(&mut self) -> ParserResult<ast::RepeatStatement> {
        self.lex.next()?; // repeat

        let block = Box::new(self.parse_block(&[Token::Until])?);

        self.lex.next()?; // until

        let expression = Box::new(self.parse_expr_binary(0)?);

        Ok(ast::RepeatStatement { block, expression })
    }

    fn parse_while_statement(&mut self) -> ParserResult<ast::WhileStatement> {
        self.lex.next()?; // while

        let expression = Box::new(self.parse_expr_binary(0)?);

        self.check(|tk| *tk == Token::Do)?;
        self.lex.next()?; // do

        let block = Box::new(self.parse_block(&[Token::End])?);

        self.lex.next()?; // end

        Ok(ast::WhileStatement { expression, block })
    }

    fn parse_function_statement(&mut self) -> ParserResult<ast::FunctionStatement> {
        self.lex.next()?; // function

        let name = self.parse_function_name()?;
        let body = self.parse_function_body()?;

        Ok(ast::FunctionStatement { name, body })
    }

    fn parse_function_name(&mut self) -> ParserResult<ast::FunctionName> {
        let mut segments = vec![];
        let mut ending_segment = None;

        loop {
            match self.lex.token() {
                Token::Name(name) => segments.push(ast::NameLiteral(name)),
                token => return Err(ParserError::UnexpectedToken { token }),
            }

            self.lex.next()?; // name

            match self.lex.token() {
                Token::Dot => {
                    self.lex.next()?;
                } // .
                _ => break,
            }
        }

        match self.lex.token() {
            Token::Colon => {
                self.lex.next()?; // :

                match self.lex.token() {
                    Token::Name(name) => ending_segment = Some(ast::NameLiteral(name)),
                    token => return Err(ParserError::UnexpectedToken { token }),
                }

                self.lex.next()?; // name
            }
            _ => {}
        }

        Ok(ast::FunctionName {
            segments,
            ending_segment,
        })
    }

    fn parse_function_body(&mut self) -> ParserResult<ast::FunctionBody> {
        let line = self.lex.line_number();
        self.check(|tk| *tk == Token::LParen)?;
        self.lex.next()?; // (

        let (parameters, varargs) = match self.lex.token() {
            Token::RParen => {
                self.lex.next()?; // )
                (vec![], false)
            }
            _ => {
                let parameters = self.parse_name_list(true)?;
                let varargs = match self.lex.token() {
                    Token::Dots => {
                        self.lex.next()?; // ...
                        true
                    }
                    _ => false,
                };

                self.check_match(Token::LParen, Token::RParen, line)?;
                self.lex.next()?; // )
                (parameters, varargs)
            }
        };

        let block = Box::new(self.parse_block(&[Token::End])?);

        self.lex.next()?; // end

        Ok(ast::FunctionBody {
            parameters,
            varargs,
            block,
        })
    }

    fn parse_local_statement(&mut self) -> ParserResult<ast::Statement> {
        self.lex.next()?; // local

        match self.lex.token() {
            Token::Function => Ok(ast::Statement::LocalFunction(
                self.parse_local_function_statement()?,
            )),
            _ => Ok(ast::Statement::LocalSet(self.parse_local_set_statement()?)),
        }
    }

    fn parse_local_function_statement(&mut self) -> ParserResult<ast::LocalFunctionStatement> {
        self.lex.next()?; // function

        let name = match self.lex.token() {
            Token::Name(name) => ast::NameLiteral(name),
            token => return Err(ParserError::UnexpectedToken { token }),
        };

        self.lex.next()?; // name

        let body = self.parse_function_body()?;

        Ok(ast::LocalFunctionStatement { name, body })
    }

    fn parse_local_set_statement(&mut self) -> ParserResult<ast::LocalSetStatement> {
        let names = self.parse_name_list(false)?;

        let expressions = match self.lex.token() {
            Token::Assign => {
                self.lex.next()?; // =
                Some(self.parse_expression_list()?)
            }
            _ => None,
        };

        Ok(ast::LocalSetStatement { names, expressions })
    }

    fn parse_set_statement(
        &mut self,
        initial_variable: ast::PrefixExpression,
    ) -> ParserResult<ast::SetStatement> {
        let mut variables = vec![initial_variable];

        loop {
            match self.lex.token() {
                Token::Comma => {
                    self.lex.next()?;
                }
                _ => break,
            }

            variables.push(self.parse_expr_suffixed()?);
        }

        self.check(|tk| *tk == Token::Assign)?;
        self.lex.next()?;

        let expressions = self.parse_expression_list()?;

        Ok(ast::SetStatement {
            variables,
            expressions,
        })
    }

    fn parse_call_statement(
        &mut self,
        prefix_expression: ast::PrefixExpression,
    ) -> ParserResult<ast::CallStatement> {
        match self.lex.token() {
            Token::Colon => {
                self.lex.next()?; // :

                let name = match self.lex.token() {
                    Token::Name(name) => ast::NameLiteral(name),
                    token => return Err(ParserError::UnexpectedToken { token }),
                };

                self.lex.next()?; // name

                let arguments = self.parse_function_arguments()?;

                Ok(ast::CallStatement::This(ast::ThisCallStatement {
                    prefix: Box::new(prefix_expression),
                    name,
                    arguments,
                }))
            }
            _ => Ok(match prefix_expression {
                ast::PrefixExpression::Call(inner) => inner,
                _ => return Err(ParserError::UnexpectedExpression),
            }),
        }
    }

    fn parse_last_statement(&mut self) -> ParserResult<ast::LastStatement> {
        match self.lex.token() {
            Token::Break => return Ok(ast::LastStatement::Break),
            _ => {}
        }

        let expressions = self.parse_expression_list()?;
        Ok(ast::LastStatement::Return(expressions))
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
            _ => Ok(ast::Expression::Prefix(self.parse_expr_suffixed()?)),
        }
    }

    fn parse_expr_suffixed(&mut self) -> ParserResult<ast::PrefixExpression> {
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
                    return Ok(expr);
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
                self.lex.next()?; // (

                let arguments = match self.lex.token() {
                    Token::RParen => vec![],
                    _ => self.parse_expression_list()?,
                };

                self.check_match(Token::LParen, Token::RParen, line)?;
                self.lex.next()?; // )

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

    fn parse_name_list(&mut self, function: bool) -> ParserResult<Vec<ast::NameLiteral>> {
        let mut names = vec![];

        loop {
            match self.lex.token() {
                Token::Name(name) => names.push(ast::NameLiteral(name)),
                Token::Dots => {
                    if function {
                        break;
                    }
                }
                token => return Err(ParserError::UnexpectedToken { token }),
            }

            self.lex.next()?; // name

            match self.lex.token() {
                Token::Comma => {}
                _ => break,
            }

            self.lex.next()?; // ,
        }

        Ok(names)
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
