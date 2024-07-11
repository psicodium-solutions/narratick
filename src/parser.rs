use std::{fmt::Display, iter::Peekable, ops::Range, str::Chars};

use crate::token::{self, Token, TokenData};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal<'a> {
    Bool(bool),
    Number(f64),
    String(&'a str),
}

impl<'a> From<&'a str> for Literal<'a> {
    fn from(value: &'a str) -> Self {
        Self::String(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value<'a> {
    Literal(Literal<'a>),
    Variable(&'a str),
}

impl<'a> From<&'a str> for Value<'a> {
    fn from(value: &'a str) -> Self {
        if let Ok(v) = value.parse::<bool>() {
            Self::Literal(Literal::Bool(v))
        } else if let Ok(v) = value.parse::<f64>() {
            Self::Literal(Literal::Number(v))
        } else {
            Self::Variable(value)
        }
    }
}

impl<'a> From<Literal<'a>> for Value<'a> {
    fn from(value: Literal<'a>) -> Self {
        Self::Literal(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Block<'a> {
    Title(Value<'a>),
    Author(Value<'a>),
    Anchor(Value<'a>),
    Jump(Value<'a>),
    Set {
        name: Value<'a>,
        value: Value<'a>,
    },
    Emit(Value<'a>),
    If {
        lhs: Value<'a>,
        op: Op,
        rhs: Value<'a>,
    },
    End,

    Text(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Op {
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
}

impl<'a> TryFrom<Token<'a>> for Op {
    type Error = crate::Error;

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        let Token::Text(op) = value else {
            return Err(crate::Error::UnexpectedToken {
                message: format!("expected an operator, got {value:?}"),
            });
        };

        match op {
            "==" => Ok(Self::Eq),
            "!=" => Ok(Self::NotEq),
            "<" => Ok(Self::Lt),
            "<=" => Ok(Self::LtEq),
            ">" => Ok(Self::Gt),
            ">=" => Ok(Self::GtEq),
            _ => Err(crate::Error::unrecognized_op(op)),
        }
    }
}

#[derive(Debug)]
pub struct Parser<'a> {
    input: &'a str,

    blocks: Vec<Block<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,

            blocks: vec![],
        }
    }

    pub fn parse(&mut self) -> Result<&Vec<Block<'a>>, crate::Error> {
        let mut tokens = token::tokenize(self.input)?.into_iter();

        let mut last_span: Range<usize> = Range { start: 0, end: 0 };
        let mut current_block: Option<Block> = None;

        macro_rules! clear_block {
            () => {{
                if let Some(block) = current_block.take() {
                    self.blocks.push(block);
                }
            }};
        }

        macro_rules! discard_space {
            ($token_name:expr) => {{
                let Some(TokenData {
                    token: Token::Space(_),
                    ..
                }) = tokens.next()
                else {
                    return Err(crate::Error::unexpected_token(format!(
                        "missing spaces for {}",
                        $token_name
                    )));
                };
            }};
        }

        macro_rules! value_from_token {
            ($token:expr) => {
                match $token {
                    Token::String(v) => Literal::String(v).into(),
                    Token::Number(v) | Token::Text(v) => Value::from(v),
                    v @ _ => {
                        return Err(crate::Error::unexpected_token(format!(
                            "unexpected token {v:?}"
                        )))
                    }
                }
            };
        }

        macro_rules! single_field_block {
            ($variant:ident, $token_name:expr) => {{
                clear_block!();

                discard_space!($token_name);

                let Some(next_token) = tokens.next() else {
                    return Err(crate::Error::insufficient_tokens(format!(
                        "insufficient tokens for {}",
                        $token_name
                    )));
                };

                let value = match next_token.token {
                    Token::String(v) => Literal::String(v).into(),
                    Token::Number(v) | Token::Text(v) => Value::from(v),
                    v @ _ => {
                        return Err(crate::Error::unexpected_token(format!(
                            "unexpected token {v:?}"
                        )))
                    }
                };

                last_span = next_token.span;
                self.blocks.push(Block::$variant(value));
            }};
        }

        while let Some(TokenData { span, token }) = tokens.next() {
            match token {
                Token::Space(v) | Token::String(v) | Token::Text(v) | Token::Number(v) => {
                    if let Some(block) = current_block.take() {
                        if let Block::Text(_) = block {
                            last_span.end = span.end;
                            current_block = Some(Block::Text(&self.input[last_span.clone()]));
                            continue;
                        } else {
                            self.blocks.push(block);
                        }
                    }

                    last_span = span;
                    current_block = Some(Block::Text(v));
                }
                // Token::Number(v) => buildable_block!(v, span),
                Token::Title => single_field_block!(Title, "title"),
                Token::Author => single_field_block!(Author, "author"),
                Token::Anchor => single_field_block!(Anchor, "anchor"),
                Token::Jump => single_field_block!(Jump, "jump"),
                Token::Set => {
                    clear_block!();

                    discard_space!("set");

                    let Some(name_block) = tokens.next() else {
                        return Err(crate::Error::insufficient_tokens("missing name for set"));
                    };

                    discard_space!("set");

                    let Some(value_block) = tokens.next() else {
                        return Err(crate::Error::insufficient_tokens("missing value for set"));
                    };

                    todo!()
                }
                Token::Emit => single_field_block!(Emit, "emit"),
                Token::If => {
                    clear_block!();

                    discard_space!("if");

                    let Some(TokenData {
                        token: lhs_token, ..
                    }) = tokens.next()
                    else {
                        return Err(crate::Error::insufficient_tokens("missing lhs for if"));
                    };
                    let lhs = value_from_token!(lhs_token);

                    discard_space!("if");

                    let Some(TokenData {
                        token: op_token, ..
                    }) = tokens.next()
                    else {
                        return Err(crate::Error::insufficient_tokens("missing op for if"));
                    };
                    let op = Op::try_from(op_token)?;

                    discard_space!("if");

                    let Some(TokenData {
                        token: rhs_token, ..
                    }) = tokens.next()
                    else {
                        return Err(crate::Error::insufficient_tokens("missing rhs for if"));
                    };
                    let rhs = value_from_token!(rhs_token);

                    self.blocks.push(Block::If { lhs, op, rhs });
                }
                Token::End => {
                    clear_block!();

                    self.blocks.push(Block::End);
                }
            }
        }

        if let Some(block) = current_block.take() {
            eprintln!("cleaning up last block");
            self.blocks.push(block);
        }

        Ok(&self.blocks)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let input = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/samples/simple.narratick"
        ));

        let mut parser = Parser::new(input);
        let mut blocks = parser.parse().unwrap().clone().into_iter();

        assert_eq!(
            blocks.next(),
            Some(Block::Title(Literal::String("Test Title").into()))
        );
        assert_eq!(
            blocks.next(),
            Some(Block::Author(Literal::String("It's me").into()))
        );
        assert_eq!(
            blocks.next(),
            Some(Block::Text("This is a test.   2.0 2.0."))
        );
        assert_eq!(blocks.next(), Some(Block::End));
    }
}
