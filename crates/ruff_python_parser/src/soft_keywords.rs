// TODO handle in parser: Always parse as soft keywords? Could this be causing issues in our lexer based lint rules?
use crate::lexer::LexResult;
use itertools::{Itertools, MultiPeek};

/// An [`Iterator`] that transforms a token stream to accommodate soft keywords (namely, `match`
/// and `case`).
///
/// [PEP 634](https://www.python.org/dev/peps/pep-0634/) introduced the `match` and `case` keywords
/// as soft keywords, meaning that they can be used as identifiers (e.g., variable names) in certain
/// contexts.
///
/// This function modifies a token stream to accommodate this change. In particular, it replaces
/// `match` and `case` tokens with `identifier` tokens if they are used as identifiers.
///
/// Handling soft keywords in this intermediary pass allows us to simplify both the lexer and
/// parser, as neither of them need to be aware of soft keywords.
pub struct SoftKeywordTransformer<I>
where
    I: Iterator<Item = LexResult>,
{
    underlying: MultiPeek<I>,
    start_of_line: bool,
}

impl<I> SoftKeywordTransformer<I>
where
    I: Iterator<Item = LexResult>,
{
    pub fn new(lexer: I, mode: Mode) -> Self {
        Self {
            underlying: lexer.multipeek(), // spell-checker:ignore multipeek
            start_of_line: matches!(mode, Mode::Interactive | Mode::Module),
        }
    }
}

impl<I> Iterator for SoftKeywordTransformer<I>
where
    I: Iterator<Item = LexResult>,
{
    type Item = LexResult;

    #[inline]
    fn next(&mut self) -> Option<LexResult> {
        let mut next = self.underlying.next();
        if let Some(Ok((tok, range))) = next.as_ref() {
            // If the token is a `match` or `case` token, check if it's used as an identifier.
            // We assume every `match` or `case` is an identifier unless both of the following
            // conditions are met:
            // 1. The token is at the start of a logical line.
            // 2. The logical line contains a top-level colon (that is, a colon that is not nested
            //    inside a parenthesized expression, list, or dictionary).
            // 3. The top-level colon is not the immediate sibling of a `match` or `case` token.
            //    (This is to avoid treating `match` and `case` as identifiers when annotated with
            //    type hints.)
            if matches!(tok, TokenKind::Match | TokenKind::Case) {
                if !self.start_of_line {
                    next = Some(Ok((*tok, *range)));
                } else {
                    let mut nesting = 0;
                    let mut first = true;
                    let mut seen_colon = false;
                    let mut seen_lambda = false;
                    while let Some(Ok((tok, _))) = self.underlying.peek() {
                        match tok {
                            TokenKind::Newline => break,
                            TokenKind::Lambda if nesting == 0 => seen_lambda = true,
                            TokenKind::Colon if nesting == 0 => {
                                if seen_lambda {
                                    seen_lambda = false;
                                } else if !first {
                                    seen_colon = true;
                                }
                            }
                            TokenKind::Lpar | TokenKind::Lsqb | TokenKind::Lbrace => nesting += 1,
                            TokenKind::Rpar | TokenKind::Rsqb | TokenKind::Rbrace => nesting -= 1,
                            _ => {}
                        }
                        first = false;
                    }
                    if !seen_colon {
                        next = Some(Ok((*tok, *range)));
                    }
                }
            }
        }

        self.start_of_line = next.as_ref().map_or(false, |lex_result| {
            lex_result.as_ref().map_or(false, |(tok, _)| {
                #[cfg(feature = "full-lexer")]
                if matches!(tok, Tok::NonLogicalNewline | Tok::Comment { .. }) {
                    return self.start_of_line;
                }

                matches!(tok, |TokenKind::Newline| TokenKind::Indent
                    | TokenKind::Dedent)
            })
        });

        next
    }
}
