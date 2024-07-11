use std::{fmt::Display, num::ParseFloatError, ops::Range};

use logos::{Lexer, Logos};

fn strip_double_quotes<'a>(lex: &mut Lexer<'a, Token<'a>>) -> &'a str {
    let string: &str = lex.slice();

    &string[1..string.len() - 1]
}

#[derive(Debug, Clone, Copy, PartialEq, Logos)]
#[logos(error = crate::Error)]
#[logos(skip r"[\t\r\n\f]+")]
pub enum Token<'a> {
    #[regex(r"[ ]+")]
    Space(&'a str),

    #[token("@title")]
    Title,
    #[token("@author")]
    Author,
    #[token("@anchor")]
    Anchor,
    #[token("@jump")]
    Jump,
    #[token("@set")]
    Set,
    #[token("@emit")]
    Emit,
    #[token("@if")]
    If,
    #[token("@end")]
    End,

    // https://github.com/maciejhirsz/logos/blob/master/examples/json_borrowed.rs
    #[regex(
        r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})+""#,
        priority = 4,
        callback = strip_double_quotes
    )]
    String(&'a str),
    #[regex(
        r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?",
        priority = 3,
        callback = |lex| lex.slice()
    )]
    Number(&'a str),
    #[regex(r#"[\S]+"#, priority = 2, callback = |lex| lex.slice())]
    Text(&'a str),
}

pub struct TokenData<'a> {
    pub span: Range<usize>,
    pub token: Token<'a>,
}

pub fn tokenize(input: &str) -> Result<Vec<TokenData>, crate::Error> {
    let mut r = vec![];
    let mut tokens = Token::lexer(input);
    while let Some(token) = tokens.next() {
        r.push(TokenData {
            span: tokens.span(),
            token: token?,
        });
    }

    Ok(r)
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
        let mut lex = Token::lexer(input);

        assert_eq!(lex.next(), Some(Ok(Token::Title)));
        assert_eq!(lex.next(), Some(Ok(Token::Space(" "))));
        assert_eq!(lex.next(), Some(Ok(Token::String(r#""Test Title""#))));

        assert_eq!(lex.next(), Some(Ok(Token::Author)));
        assert_eq!(lex.next(), Some(Ok(Token::Space(" "))));
        assert_eq!(lex.next(), Some(Ok(Token::String(r#""It's me""#))));

        assert_eq!(lex.next(), Some(Ok(Token::Text("This"))));
        assert_eq!(lex.next(), Some(Ok(Token::Space(" "))));
        assert_eq!(lex.next(), Some(Ok(Token::Text("is"))));
        assert_eq!(lex.next(), Some(Ok(Token::Space(" "))));
        assert_eq!(lex.next(), Some(Ok(Token::Text("a"))));
        assert_eq!(lex.next(), Some(Ok(Token::Space(" "))));
        assert_eq!(lex.next(), Some(Ok(Token::Text("test."))));
        assert_eq!(lex.next(), Some(Ok(Token::Space("   "))));
        assert_eq!(lex.next(), Some(Ok(Token::Number("2.0"))));
        assert_eq!(lex.next(), Some(Ok(Token::Space(" "))));
        assert_eq!(lex.next(), Some(Ok(Token::Text("2.0."))));

        assert_eq!(lex.next(), Some(Ok(Token::End)));

        assert_eq!(lex.next(), None);
    }
}
