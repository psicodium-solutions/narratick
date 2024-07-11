mod parser;
mod token;

use std::{fmt::Display, num::ParseFloatError, str::Utf8Error};

#[derive(Debug, Default, Clone, PartialEq)]
pub enum Error {
    #[default]
    Unmatched,

    UnexpectedToken {
        message: String,
    },
    InsufficientTokens {
        message: String,
    },
    UnrecognizedOp {
        message: String,
    },
}

impl Error {
    #[inline]
    pub fn unexpected_token(message: impl Display) -> Self {
        Self::UnexpectedToken {
            message: message.to_string(),
        }
    }

    #[inline]
    pub fn insufficient_tokens(message: impl Display) -> Self {
        Self::InsufficientTokens {
            message: message.to_string(),
        }
    }

    #[inline]
    pub fn unrecognized_op(message: impl Display) -> Self {
        Self::UnrecognizedOp {
            message: message.to_string(),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Unmatched => "Unmatched token".to_string(),

                Self::UnexpectedToken { message } => message.to_string(),
                Self::InsufficientTokens { message } => message.to_string(),
                Self::UnrecognizedOp { message } => message.to_string(),
            }
        )
    }
}

pub fn parse() {}
