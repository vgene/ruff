//! This module takes care of lexing Python source text.
//!
//! This means source code is scanned and translated into separate tokens. The rules
//! governing what is and is not a valid token are defined in the Python reference
//! guide section on [Lexical analysis].
//!
//! The primary function in this module is [`lex`], which takes a string slice
//! and returns an iterator over the tokens in the source code. The tokens are currently returned
//! as a `Result<Spanned, LexicalError>`, where [`Spanned`] is a tuple containing the
//! start and end [`TextSize`] and a [`TokenKind`] denoting the token.
//!
//! # Example
//!
//! ```
//! use rustpython_parser::{lexer::lex, Tok, Mode, StringKind};
//!
//! let source = "x = 'RustPython'";
//! let tokens = lex(source, Mode::Module)
//!     .map(|tok| tok.expect("Failed to lex"))
//!     .collect::<Vec<_>>();
//!
//! for (token, range) in tokens {
//!     println!(
//!         "{token:?}@{range:?}",
//!     );
//! }
//! ```
//!
//! [Lexical analysis]: https://docs.python.org/3/reference/lexical_analysis.html
use crate::Mode;
use ruff_text_size::{TextRange, TextSize};
use std::borrow::Cow;
use std::{char, cmp::Ordering};
use unic_emoji_char::is_emoji_presentation;
use unic_ucd_ident::{is_xid_continue, is_xid_start};

mod cursor;
mod indentation;
mod token;

use crate::lexer::cursor::Cursor;
use crate::lexer::indentation::{Character, Column, Indentation, Indentations};
pub use token::{Token, TokenFlags, TokenKind};

/// A lexer for Python source code.
pub struct Lexer<'source> {
    cursor: Cursor<'source>,

    source: &'source str,

    // Are we at the beginning of a line?
    at_begin_of_line: bool,

    // Indentation levels.
    indentations: Indentations,

    pending_indentation: Option<Indentation>,

    // TODO
    diagnostics: Vec<String>,
}

// generated in build.rs, in gen_phf()
/// A map of keywords to their tokens.
pub static KEYWORDS: phf::Map<&'static str, TokenKind> =
    include!(concat!(env!("OUT_DIR"), "/keywords.rs"));

/// The result of lexing a token.
pub type LexResult<'source> = Token<'source>;

/// Create a new lexer from a source string.
///
/// # Examples
///
/// ```
/// use rustpython_parser::{Mode, lexer::lex};
///
/// let source = "def hello(): return 'world'";
/// let lexer = lex(source, Mode::Module);
///
/// for token in lexer {
///    println!("{:?}", token);
/// }
/// ```
#[inline]
pub fn lex(source: &str, mode: Mode) -> Lexer<'_> {
    lex_starts_at(source, mode, TextSize::default())
}

/// Create a new lexer from a source string, starting at a given location.
/// You probably want to use [`lex`] instead.
pub fn lex_starts_at(source: &str, mode: Mode, start_offset: TextSize) -> Lexer<'_> {
    Lexer::new(source, start_offset)
}

impl<'source> Lexer<'source> {
    /// Create a new lexer from T and a starting location. You probably want to use
    /// [`lex`] instead.
    pub fn new(source: &'source str, start: TextSize) -> Self {
        let mut lexer = Lexer {
            at_begin_of_line: true,
            indentations: Indentations::default(),
            cursor: Cursor::new(source),
            diagnostics: Vec::new(),
            pending_indentation: None,
            source,
        };

        // TODO: Handle possible mismatch between BOM and explicit encoding declaration.
        // spell-checker:ignore feff
        lexer.cursor.eat_char('\u{feff}');

        lexer
    }

    pub fn finish(self) -> Vec<String> {
        self.diagnostics
    }

    pub fn next_token(&mut self) -> Token<'source> {
        if let Some(indentation) = self.pending_indentation.take() {
            match self.indentations.current().try_compare(&indentation) {
                Ok(Ordering::Greater) => {
                    self.pending_indentation = Some(indentation);
                    return Token::new(TokenKind::Dedent, TextSize::new(0));
                }
                Ok(Ordering::Equal) => {
                    if indentation.character() != Character::new(0) {
                        return Token::new(TokenKind::Whitespace, self.cursor.token_len());
                    }
                }
                _ => {
                    panic!("Invalid indentation stack. Parent indentation was smaller than this indentation.")
                }
            }
        }

        #[cfg(debug_assertions)]
        {
            if self.at_begin_of_line {
                dbg!(self.cursor.previous());
                debug_assert!(matches!(
                    self.cursor.previous(),
                    '\n' | '\r' | cursor::EOF_CHAR
                ));
            }
        }

        self.cursor.start_token();

        let Some(first) = self.cursor.bump() else {
            // TODO do we need to emit a trailing new line?
            return if let Some(dedent) = self.handle_indentation(Indentation::root()) {
dedent
            } else {
                Token::eof()
            }


        };

        if let Some(trivia) = self.eat_trivia(first) {
            return trivia;
        }

        self.lex_non_trivia(first)
    }

    fn lex_non_trivia(&mut self, first: char) -> Token<'source> {
        if first.is_ascii() {
            match first {
                'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier(first),
                '0'..='9' => todo!(),
                '#' => self.lex_comment(),
                '"' | '\'' => todo!(),
                '=' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::EqEqual, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::Equal, TextSize::new(1))
                    }
                }
                '+' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::PlusEqual, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::Plus, TextSize::new(1))
                    }
                }
                '*' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::StarEqual, TextSize::new(2))
                    } else if self.cursor.eat_char('*') {
                        if self.cursor.eat_char('=') {
                            Token::new(TokenKind::DoubleStarEqual, TextSize::new(3))
                        } else {
                            Token::new(TokenKind::DoubleStar, TextSize::new(2))
                        }
                    } else {
                        Token::new(TokenKind::Star, TextSize::new(1))
                    }
                }
                '/' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::SlashEqual, TextSize::new(2))
                    } else if self.cursor.eat_char('/') {
                        if self.cursor.eat_char('=') {
                            Token::new(TokenKind::DoubleSlashEqual, TextSize::new(3))
                        } else {
                            Token::new(TokenKind::DoubleSlash, TextSize::new(2))
                        }
                    } else {
                        Token::new(TokenKind::Slash, TextSize::new(1))
                    }
                }
                '%' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::PercentEqual, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::Percent, TextSize::new(1))
                    }
                }
                '|' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::VbarEqual, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::Vbar, TextSize::new(1))
                    }
                }
                '^' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::CircumflexEqual, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::CircumFlex, TextSize::new(1))
                    }
                }
                '&' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::AmperEqual, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::Amper, TextSize::new(1))
                    }
                }
                '-' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::MinusEqual, TextSize::new(2))
                    } else if self.cursor.eat_char('>') {
                        Token::new(TokenKind::Rarrow, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::Minus, TextSize::new(1))
                    }
                }
                '@' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::AtEqual, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::At, TextSize::new(1))
                    }
                }
                '!' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::NotEqual, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::Bogus, TextSize::new(1))
                    }
                }
                '~' => Token::new(TokenKind::Tilde, TextSize::new(1)),
                // TODO handle nesting inside of lexer?
                '(' => Token::new(TokenKind::Lpar, TextSize::new(1)),
                ')' => Token::new(TokenKind::Rpar, TextSize::new(1)),
                '[' => Token::new(TokenKind::Lsqb, TextSize::new(1)),
                ']' => Token::new(TokenKind::Rsqb, TextSize::new(1)),
                '{' => Token::new(TokenKind::Lbrace, TextSize::new(1)),
                '}' => Token::new(TokenKind::Rbrace, TextSize::new(1)),
                ':' => {
                    if self.cursor.eat_char('=') {
                        Token::new(TokenKind::ColonEqual, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::Colon, TextSize::new(1))
                    }
                }
                ';' => Token::new(TokenKind::Semi, TextSize::new(1)),
                '<' => {
                    if self.cursor.eat_char('<') {
                        if self.cursor.eat_char('=') {
                            Token::new(TokenKind::LeftShiftEqual, TextSize::new(3))
                        } else {
                            Token::new(TokenKind::LeftShift, TextSize::new(2))
                        }
                    } else if self.cursor.eat_char('=') {
                        Token::new(TokenKind::LessEqual, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::Less, TextSize::new(1))
                    }
                }
                '>' => {
                    if self.cursor.eat_char('>') {
                        if self.cursor.eat_char('=') {
                            Token::new(TokenKind::RightShiftEqual, TextSize::new(3))
                        } else {
                            Token::new(TokenKind::RightShift, TextSize::new(2))
                        }
                    } else if self.cursor.eat_char('=') {
                        Token::new(TokenKind::GreaterEqual, TextSize::new(2))
                    } else {
                        Token::new(TokenKind::Greater, TextSize::new(1))
                    }
                }
                ',' => Token::new(TokenKind::Comma, TextSize::new(1)),
                '.' => match self.cursor.first() {
                    '0'..='9' => todo!(),
                    '.' => Token::new(TokenKind::Ellipsis, TextSize::new(2)),
                    _ => Token::new(TokenKind::Dot, TextSize::new(1)),
                },
                // Line continuation. We should emit a token for the line continuation
                '\\' => {
                    todo!()
                }
                _ => Token::new(TokenKind::Bogus, TextSize::new(1)),
            }
        } else if is_non_ascii_identifier_start(first) {
            self.lex_identifier(first)
        } else if is_emoji_presentation(first) {
            Token::new(TokenKind::Identifier, self.cursor.token_len())
        } else {
            Token::new(TokenKind::Bogus, self.cursor.text_len())
        }
    }

    // TODO handle \x0C

    fn eat_trivia(&mut self, first: char) -> Option<Token<'source>> {
        let token = match first {
            prev @ (' ' | '\t') => {
                if self.at_begin_of_line {
                    let indentation = self.lex_indentation(prev);
                    self.at_begin_of_line = false;

                    // Indention of an all whitespace line or comment only line. Indention rules don't apply
                    if matches!(dbg!(self.cursor.first()), '\n' | '\r' | '#') {
                        Token::new(TokenKind::Whitespace, self.cursor.token_len())
                    } else {
                        return self.handle_indentation(indentation);
                    }
                } else {
                    // Skip over whitespace
                    self.cursor.eat_while(|c| matches!(c, ' ' | '\t'));
                    Token::new(TokenKind::Whitespace, self.cursor.token_len())
                }
            }

            '#' => {
                self.at_begin_of_line = false;
                self.lex_comment()
            }

            '\n' => {
                self.at_begin_of_line = true;
                Token::new(TokenKind::NonLogicalNewline, TextSize::new(1))
            }
            // `\r\n`
            '\r' if self.cursor.first() == '\n' => {
                self.at_begin_of_line = true;
                self.cursor.bump();
                Token::new(TokenKind::NonLogicalNewline, TextSize::new(2))
            }
            // `\r`
            '\r' => {
                self.at_begin_of_line = true;
                Token::new(TokenKind::NonLogicalNewline, TextSize::new(1))
            }

            _ => {
                return if self.at_begin_of_line {
                    self.at_begin_of_line = false;
                    self.handle_indentation(Indentation::root())
                } else {
                    None
                };
            }
        };

        Some(token)
    }

    fn lex_indentation(&mut self, first: char) -> Indentation {
        debug_assert!(self.at_begin_of_line);
        debug_assert!(matches!(first, ' ' | '\t'));

        let mut column = 0u32;
        let mut character = 0u32;

        if first == ' ' {
            column += 1;
        } else {
            column += 8;
        }

        loop {
            match self.cursor.first() {
                ' ' => {
                    column += 1;
                }
                '\t' => column = (column % 8) + column,
                _ => break,
            }

            self.cursor.bump();
            character += 1;
        }

        Indentation::new(Column::new(column), Character::new(character))
    }

    fn handle_indentation(&mut self, indentation: Indentation) -> Option<Token<'source>> {
        match self.indentations.current().try_compare(&indentation) {
            // Dedent
            Ok(Ordering::Greater) => {
                self.indentations.pop();
                self.pending_indentation = Some(indentation);

                Some(Token::new(TokenKind::Dedent, TextSize::new(0)))
            }

            Ok(Ordering::Equal) => {
                if indentation.character() != Character::new(0) {
                    Some(Token::new(TokenKind::Whitespace, self.cursor.token_len()))
                } else {
                    None
                }
            }

            // Indent
            Ok(Ordering::Less) => {
                self.indentations.push(indentation);
                Some(Token::new(TokenKind::Indent, self.cursor.token_len()))
            }
            Err(_) => {
                // TODO Emit as error token. Emit a diagnostic. Perform error recovery (guess the right indentation level by only comparing one dimension?).
                Some(Token::new(TokenKind::Bogus, self.cursor.token_len()));
                panic!("Unexpcted indentation");
            }
        }
    }

    #[inline]
    fn token_range(&self) -> TextRange {
        let end = TextSize::new(self.source.len() as u32) - self.cursor.text_len();
        let len = self.cursor.token_len();

        TextRange::at(end - len, len)
    }

    fn lex_comment(&mut self) -> Token<'source> {
        debug_assert_eq!(self.cursor.previous(), '#');

        self.cursor.eat_while(|c| !matches!(c, '\n' | '\r'));

        let range = self.token_range();
        let comment = Cow::Borrowed(&self.source[range]);

        Token::new(TokenKind::Comment, self.cursor.token_len()).with_value(comment)
    }

    /// Lex an identifier. Also used for keywords and string/bytes literals with a prefix.
    fn lex_identifier(&mut self, first: char) -> Token<'source> {
        // let one_lookahead = self.cursor.first();
        // let two_lookahead = self.cursor.second();

        // TODO
        // Detect potential string like rb'' b'' f'' u'' r''
        // if matches!(one_lookahead, '"' | '\'') {
        //     if let Ok(kind) = StringKind::try_from(first) {
        //         return self.lex_string(kind);
        //     }
        // } else if matches!(two_lookahead, '"', '\'') {
        //     if let Ok(kind) = StringKind::try_from([first, one_lookahead]) {
        //         return self.lex_string(kind);
        //     }
        // }

        self.cursor.eat_while(is_identifier_continuation);

        let range = self.token_range();
        let text = &self.source[range];

        if let Some(kind) = KEYWORDS.get(text) {
            Token::new(*kind, range.len())
        } else {
            Token::new(TokenKind::Identifier, range.len()).with_value(Cow::Borrowed(text))
        }
    }

    //
    // /// Numeric lexing. The feast can start!
    // fn lex_number(&mut self) -> LexResult {
    //     let start_pos = self.get_pos();
    //     match self.window[..2] {
    //         [Some('0'), Some('x' | 'X')] => {
    //             // Hex! (0xdeadbeef)
    //             self.next_char();
    //             self.next_char();
    //             self.lex_number_radix(start_pos, 16)
    //         }
    //         [Some('0'), Some('o' | 'O')] => {
    //             // Octal style! (0o377)
    //             self.next_char();
    //             self.next_char();
    //             self.lex_number_radix(start_pos, 8)
    //         }
    //         [Some('0'), Some('b' | 'B')] => {
    //             // Binary! (0b_1110_0101)
    //             self.next_char();
    //             self.next_char();
    //             self.lex_number_radix(start_pos, 2)
    //         }
    //         _ => self.lex_normal_number(),
    //     }
    // }
    //
    // /// Lex a hex/octal/decimal/binary number without a decimal point.
    // fn lex_number_radix(&mut self, start_pos: TextSize, radix: u32) -> LexResult {
    //     let value_text = self.radix_run(radix);
    //     let end_pos = self.get_pos();
    //     let value = BigInt::from_str_radix(&value_text, radix).map_err(|e| LexicalError {
    //         error: LexicalErrorType::OtherError(format!("{e:?}")),
    //         location: start_pos,
    //     })?;
    //     Ok((TokenKind::Int { value }, TextRange::new(start_pos, end_pos)))
    // }
    //
    // /// Lex a normal number, that is, no octal, hex or binary number.
    // fn lex_normal_number(&mut self) -> LexResult {
    //     let start_pos = self.get_pos();
    //     let start_is_zero = self.window[0] == Some('0');
    //     // Normal number:
    //     let mut value_text = self.radix_run(10);
    //
    //     // If float:
    //     if self.window[0] == Some('.') || self.at_exponent() {
    //         // Take '.':
    //         if self.window[0] == Some('.') {
    //             if self.window[1] == Some('_') {
    //                 return Err(LexicalError {
    //                     error: LexicalErrorType::OtherError("Invalid Syntax".to_owned()),
    //                     location: self.get_pos(),
    //                 });
    //             }
    //             value_text.push(self.next_char().unwrap());
    //             value_text.push_str(&self.radix_run(10));
    //         }
    //
    //         // 1e6 for example:
    //         if let Some('e' | 'E') = self.window[0] {
    //             if self.window[1] == Some('_') {
    //                 return Err(LexicalError {
    //                     error: LexicalErrorType::OtherError("Invalid Syntax".to_owned()),
    //                     location: self.get_pos(),
    //                 });
    //             }
    //             value_text.push(self.next_char().unwrap().to_ascii_lowercase());
    //             // Optional +/-
    //             if matches!(self.window[0], Some('-' | '+')) {
    //                 if self.window[1] == Some('_') {
    //                     return Err(LexicalError {
    //                         error: LexicalErrorType::OtherError("Invalid Syntax".to_owned()),
    //                         location: self.get_pos(),
    //                     });
    //                 }
    //                 value_text.push(self.next_char().unwrap());
    //             }
    //
    //             value_text.push_str(&self.radix_run(10));
    //         }
    //
    //         let value = f64::from_str(&value_text).map_err(|_| LexicalError {
    //             error: LexicalErrorType::OtherError("Invalid decimal literal".to_owned()),
    //             location: self.get_pos(),
    //         })?;
    //
    //         // Parse trailing 'j':
    //         if matches!(self.window[0], Some('j' | 'J')) {
    //             self.next_char();
    //             let end_pos = self.get_pos();
    //             Ok((
    //                 TokenKind::Complex {
    //                     real: 0.0,
    //                     imag: value,
    //                 },
    //                 TextRange::new(start_pos, end_pos),
    //             ))
    //         } else {
    //             let end_pos = self.get_pos();
    //             Ok((
    //                 TokenKind::Float { value },
    //                 TextRange::new(start_pos, end_pos),
    //             ))
    //         }
    //     } else {
    //         // Parse trailing 'j':
    //         if matches!(self.window[0], Some('j' | 'J')) {
    //             self.next_char();
    //             let end_pos = self.get_pos();
    //             let imag = f64::from_str(&value_text).unwrap();
    //             Ok((
    //                 TokenKind::Complex { real: 0.0, imag },
    //                 TextRange::new(start_pos, end_pos),
    //             ))
    //         } else {
    //             let end_pos = self.get_pos();
    //             let value = value_text.parse::<BigInt>().unwrap();
    //             if start_is_zero && !value.is_zero() {
    //                 // leading zeros in decimal integer literals are not permitted
    //                 return Err(LexicalError {
    //                     error: LexicalErrorType::OtherError("Invalid Token".to_owned()),
    //                     location: self.get_pos(),
    //                 });
    //             }
    //             Ok((TokenKind::Int { value }, TextRange::new(start_pos, end_pos)))
    //         }
    //     }
    // }
    //
    // /// Consume a sequence of numbers with the given radix,
    // /// the digits can be decorated with underscores
    // /// like this: '1_2_3_4' == '1234'
    // fn radix_run(&mut self, radix: u32) -> String {
    //     let mut value_text = String::new();
    //
    //     loop {
    //         if let Some(c) = self.take_number(radix) {
    //             value_text.push(c);
    //         } else if self.window[0] == Some('_')
    //             && Lexer::<T>::is_digit_of_radix(self.window[1], radix)
    //         {
    //             self.next_char();
    //         } else {
    //             break;
    //         }
    //     }
    //     value_text
    // }
    //
    // /// Consume a single character with the given radix.
    // fn take_number(&mut self, radix: u32) -> Option<char> {
    //     let take_char = Lexer::<T>::is_digit_of_radix(self.window[0], radix);
    //
    //     take_char.then(|| self.next_char().unwrap())
    // }
    //
    // /// Test if a digit is of a certain radix.
    // fn is_digit_of_radix(c: Option<char>, radix: u32) -> bool {
    //     match radix {
    //         2 => matches!(c, Some('0'..='1')),
    //         8 => matches!(c, Some('0'..='7')),
    //         10 => matches!(c, Some('0'..='9')),
    //         16 => matches!(c, Some('0'..='9') | Some('a'..='f') | Some('A'..='F')),
    //         other => unimplemented!("Radix not implemented: {}", other),
    //     }
    // }
    //
    // /// Test if we face '[eE][-+]?[0-9]+'
    // fn at_exponent(&self) -> bool {
    //     match self.window[..2] {
    //         [Some('e' | 'E'), Some('+' | '-')] => matches!(self.window[2], Some('0'..='9')),
    //         [Some('e' | 'E'), Some('0'..='9')] => true,
    //         _ => false,
    //     }
    // }
    //
    // /// Lex a string literal.
    // fn lex_string(&mut self, kind: StringKind) -> LexResult {
    //     let start_pos = self.get_pos();
    //     for _ in 0..u32::from(kind.prefix_len()) {
    //         self.next_char();
    //     }
    //     let quote_char = self.next_char().unwrap();
    //     let mut string_content = String::with_capacity(5);
    //
    //     // If the next two characters are also the quote character, then we have a triple-quoted
    //     // string; consume those two characters and ensure that we require a triple-quote to close
    //     let triple_quoted = if self.window[..2] == [Some(quote_char); 2] {
    //         self.next_char();
    //         self.next_char();
    //         true
    //     } else {
    //         false
    //     };
    //
    //     loop {
    //         match self.next_char() {
    //             Some(c) => {
    //                 if c == '\\' {
    //                     if let Some(next_c) = self.next_char() {
    //                         string_content.push('\\');
    //                         string_content.push(next_c);
    //                         continue;
    //                     }
    //                 }
    //                 if c == '\n' && !triple_quoted {
    //                     return Err(LexicalError {
    //                         error: LexicalErrorType::OtherError(
    //                             "EOL while scanning string literal".to_owned(),
    //                         ),
    //                         location: self.get_pos(),
    //                     });
    //                 }
    //
    //                 if c == quote_char {
    //                     if triple_quoted {
    //                         // Look ahead at the next two characters; if we have two more
    //                         // quote_chars, it's the end of the string; consume the remaining
    //                         // closing quotes and break the loop
    //                         if self.window[..2] == [Some(quote_char); 2] {
    //                             self.next_char();
    //                             self.next_char();
    //                             break;
    //                         }
    //                     } else {
    //                         break;
    //                     }
    //                 }
    //                 string_content.push(c);
    //             }
    //             None => {
    //                 return Err(LexicalError {
    //                     error: if triple_quoted {
    //                         LexicalErrorType::Eof
    //                     } else {
    //                         LexicalErrorType::StringError
    //                     },
    //                     location: self.get_pos(),
    //                 });
    //             }
    //         }
    //     }
    //     let end_pos = self.get_pos();
    //     let tok = TokenKind::String {
    //         value: string_content,
    //         kind,
    //         triple_quoted,
    //     };
    //     Ok((tok, TextRange::new(start_pos, end_pos)))
    // }
}

// Checks if the character c is a valid continuation character as described
// in https://docs.python.org/3/reference/lexical_analysis.html#identifiers
fn is_identifier_continuation(c: char) -> bool {
    if c.is_ascii() {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9')
    } else {
        is_xid_continue(c)
    }
}

fn is_non_ascii_identifier_start(c: char) -> bool {
    is_xid_start(c)
}

/// Represents an error that occur during lexing and are
/// returned by the `parse_*` functions in the iterator in the
/// [lexer] implementation.
///
/// [lexer]: crate::lexer
#[derive(Debug, PartialEq)]
pub struct LexicalError {
    /// The type of error that occurred.
    pub error: LexicalErrorType,
    /// The location of the error.
    pub location: TextSize,
}

impl LexicalError {
    /// Creates a new `LexicalError` with the given error type and location.
    pub fn new(error: LexicalErrorType, location: TextSize) -> Self {
        Self { error, location }
    }
}

/// Represents the different types of errors that can occur during lexing.
#[derive(Debug, PartialEq)]
pub enum LexicalErrorType {
    // TODO: Can probably be removed, the places it is used seem to be able
    // to use the `UnicodeError` variant instead.
    #[doc(hidden)]
    StringError,
    // TODO: Should take a start/end position to report.
    /// Decoding of a unicode escape sequence in a string literal failed.
    UnicodeError,
    /// The nesting of brackets/braces/parentheses is not balanced.
    NestingError,
    /// The indentation is not consistent.
    IndentationError,
    /// Inconsistent use of tabs and spaces.
    TabError,
    /// Encountered a tab after a space.
    TabsAfterSpaces,
    /// A non-default argument follows a default argument.
    DefaultArgumentError,
    /// A duplicate argument was found in a function definition.
    DuplicateArgumentError(String),
    /// A positional argument follows a keyword argument.
    PositionalArgumentError,
    /// An iterable argument unpacking `*args` follows keyword argument unpacking `**kwargs`.
    UnpackedArgumentError,
    /// A keyword argument was repeated.
    DuplicateKeywordArgumentError(String),
    /// An unrecognized token was encountered.
    UnrecognizedToken { tok: char },
    /// An unexpected character was encountered after a line continuation.
    LineContinuationError,
    /// An unexpected end of file was encountered.
    Eof,
    /// An unexpected error occurred.
    OtherError(String),
}

impl std::fmt::Display for LexicalErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexicalErrorType::StringError => write!(f, "Got unexpected string"),
            // LexicalErrorType::FStringError(error) => write!(f, "f-string: {error}"),
            LexicalErrorType::UnicodeError => write!(f, "Got unexpected unicode"),
            LexicalErrorType::NestingError => write!(f, "Got unexpected nesting"),
            LexicalErrorType::IndentationError => {
                write!(f, "unindent does not match any outer indentation level")
            }
            LexicalErrorType::TabError => {
                write!(f, "inconsistent use of tabs and spaces in indentation")
            }
            LexicalErrorType::TabsAfterSpaces => {
                write!(f, "Tabs not allowed as part of indentation after spaces")
            }
            LexicalErrorType::DefaultArgumentError => {
                write!(f, "non-default argument follows default argument")
            }
            LexicalErrorType::DuplicateArgumentError(arg_name) => {
                write!(f, "duplicate argument '{arg_name}' in function definition")
            }
            LexicalErrorType::DuplicateKeywordArgumentError(arg_name) => {
                write!(f, "keyword argument repeated: {arg_name}")
            }
            LexicalErrorType::PositionalArgumentError => {
                write!(f, "positional argument follows keyword argument")
            }
            LexicalErrorType::UnpackedArgumentError => {
                write!(
                    f,
                    "iterable argument unpacking follows keyword argument unpacking"
                )
            }
            LexicalErrorType::UnrecognizedToken { tok } => {
                write!(f, "Got unexpected token {tok}")
            }
            LexicalErrorType::LineContinuationError => {
                write!(f, "unexpected character after line continuation character")
            }
            LexicalErrorType::Eof => write!(f, "unexpected EOF while parsing"),
            LexicalErrorType::OtherError(msg) => write!(f, "{msg}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const WINDOWS_EOL: &str = "\r\n";
    const MAC_EOL: &str = "\r";
    const UNIX_EOL: &str = "\n";

    pub fn lex_source(source: &str) -> Vec<Token> {
        let mut lexer = lex(source, Mode::Module);
        let mut result = vec![];

        loop {
            let next = lexer.next_token();

            let is_eof = next.kind == TokenKind::EndOfFile;
            result.push(next);

            if is_eof {
                break;
            }
        }

        result
    }

    // fn str_tok(s: &str) -> TokenKind {
    //     TokenKind::String {
    //         value: s.to_owned(),
    //         kind: StringKind::String,
    //         triple_quoted: false,
    //     }
    // }
    //
    // fn raw_str_tok(s: &str) -> TokenKind {
    //     TokenKind::String {
    //         value: s.to_owned(),
    //         kind: StringKind::RawString,
    //         triple_quoted: false,
    //     }
    // }

    #[test]
    fn comment() {
        let source = r#"# Module comment

# New line comment

    # Indented comment
"#;

        let tokens = lex_source(source);

        assert_eq!(
            tokens,
            [
                Token::new(TokenKind::Comment, TextSize::new(16))
                    .with_value(Cow::Borrowed("# Module comment")),
                Token::new(TokenKind::NonLogicalNewline, TextSize::new(1)),
                Token::new(TokenKind::NonLogicalNewline, TextSize::new(1)),
                Token::new(TokenKind::Comment, TextSize::new(18))
                    .with_value(Cow::Borrowed("# New line comment")),
                Token::new(TokenKind::NonLogicalNewline, TextSize::new(1)),
                Token::new(TokenKind::NonLogicalNewline, TextSize::new(1)),
                Token::new(TokenKind::Whitespace, TextSize::new(4)),
                Token::new(TokenKind::Comment, TextSize::new(18))
                    .with_value(Cow::Borrowed("# Indented comment")),
                Token::new(TokenKind::NonLogicalNewline, TextSize::new(1)),
                Token::new(TokenKind::EndOfFile, TextSize::new(0))
            ]
        )
    }

    #[test]
    fn identifier() {
        let source = r#"x
nonlocal
"#;

        let tokens = lex_source(source);

        assert_eq!(
            tokens,
            [
                Token::new(TokenKind::Identifier, TextSize::new(1)).with_value(Cow::Borrowed("x")),
                Token::new(TokenKind::Newline, TextSize::new(1)),
                Token::new(TokenKind::Nonlocal, TextSize::new(8)),
                Token::new(TokenKind::Newline, TextSize::new(1)),
                Token::new(TokenKind::EndOfFile, TextSize::new(0))
            ]
        )
    }

    //     #[test]
    //     fn test_numbers() {
    //         let source = "0x2f 0o12 0b1101 0 123 123_45_67_890 0.2 1e+2 2.1e3 2j 2.2j";
    //         let tokens = lex_source(source);
    //         assert_eq!(
    //             tokens,
    //             vec![
    //                 TokenKind::Int {
    //                     value: BigInt::from(47),
    //                 },
    //                 TokenKind::Int {
    //                     value: BigInt::from(10)
    //                 },
    //                 TokenKind::Int {
    //                     value: BigInt::from(13),
    //                 },
    //                 TokenKind::Int {
    //                     value: BigInt::from(0),
    //                 },
    //                 TokenKind::Int {
    //                     value: BigInt::from(123),
    //                 },
    //                 TokenKind::Int {
    //                     value: BigInt::from(1234567890),
    //                 },
    //                 TokenKind::Float { value: 0.2 },
    //                 TokenKind::Float { value: 100.0 },
    //                 TokenKind::Float { value: 2100.0 },
    //                 TokenKind::Complex {
    //                     real: 0.0,
    //                     imag: 2.0,
    //                 },
    //                 TokenKind::Complex {
    //                     real: 0.0,
    //                     imag: 2.2,
    //                 },
    //                 TokenKind::Newline,
    //             ]
    //         );
    //     }
    //
    //     macro_rules! test_line_comment {
    //         ($($name:ident: $eol:expr,)*) => {
    //             $(
    //             #[test]
    //             fn $name() {
    //                 let source = format!(r"99232  # {}", $eol);
    //                 let tokens = lex_source(&source);
    //                 assert_eq!(tokens, vec![Tok::Int { value: BigInt::from(99232) }, Tok::Comment(format!("# {}", $eol)), Tok::Newline]);
    //             }
    //             )*
    //         }
    //     }
    //
    //     test_line_comment! {
    //         test_line_comment_long: " foo",
    //         test_line_comment_whitespace: "  ",
    //         test_line_comment_single_whitespace: " ",
    //         test_line_comment_empty: "",
    //     }
    //
    //     macro_rules! test_comment_until_eol {
    //         ($($name:ident: $eol:expr,)*) => {
    //             $(
    //             #[test]
    //             fn $name() {
    //                 let source = format!("123  # Foo{}456", $eol);
    //                 let tokens = lex_source(&source);
    //                 assert_eq!(
    //                     tokens,
    //                     vec![
    //                         Tok::Int { value: BigInt::from(123) },
    //                         Tok::Comment("# Foo".to_string()),
    //                         Tok::Newline,
    //                         Tok::Int { value: BigInt::from(456) },
    //                         Tok::Newline,
    //                     ]
    //                 )
    //             }
    //             )*
    //         }
    //     }
    //
    //     test_comment_until_eol! {
    //         test_comment_until_windows_eol: WINDOWS_EOL,
    //         test_comment_until_mac_eol: MAC_EOL,
    //         test_comment_until_unix_eol: UNIX_EOL,
    //     }
    //
    //     #[test]
    //     fn test_assignment() {
    //         let source = r"a_variable = 99 + 2-0";
    //         let tokens = lex_source(source);
    //         assert_eq!(
    //             tokens,
    //             vec![
    //                 TokenKind::Identifier {
    //                     name: String::from("a_variable"),
    //                 },
    //                 TokenKind::Equal,
    //                 TokenKind::Int {
    //                     value: BigInt::from(99)
    //                 },
    //                 TokenKind::Plus,
    //                 TokenKind::Int {
    //                     value: BigInt::from(2)
    //                 },
    //                 TokenKind::Minus,
    //                 TokenKind::Int {
    //                     value: BigInt::from(0)
    //                 },
    //                 TokenKind::Newline,
    //             ]
    //         );
    //     }
    //
    //     macro_rules! test_indentation_with_eol {
    //         ($($name:ident: $eol:expr,)*) => {
    //             $(
    //             #[test]
    //             fn $name() {
    //                 let source = format!("def foo():{}   return 99{}{}", $eol, $eol, $eol);
    //                 let tokens = lex_source(&source);
    //                 assert_eq!(
    //                     tokens,
    //                     vec![
    //                         Tok::Def,
    //                         Tok::Name {
    //                             name: String::from("foo"),
    //                         },
    //                         Tok::Lpar,
    //                         Tok::Rpar,
    //                         Tok::Colon,
    //                         Tok::Newline,
    //                         Tok::Indent,
    //                         Tok::Return,
    //                         Tok::Int { value: BigInt::from(99) },
    //                         Tok::Newline,
    //                         Tok::NonLogicalNewline,
    //                         Tok::Dedent,
    //                     ]
    //                 );
    //             }
    //             )*
    //         };
    //     }
    //
    //     test_indentation_with_eol! {
    //         test_indentation_windows_eol: WINDOWS_EOL,
    //         test_indentation_mac_eol: MAC_EOL,
    //         test_indentation_unix_eol: UNIX_EOL,
    //     }
    //
    //     macro_rules! test_double_dedent_with_eol {
    //         ($($name:ident: $eol:expr,)*) => {
    //         $(
    //             #[test]
    //             fn $name() {
    //                 let source = format!("def foo():{} if x:{}{}  return 99{}{}", $eol, $eol, $eol, $eol, $eol);
    //                 let tokens = lex_source(&source);
    //                 assert_eq!(
    //                     tokens,
    //                     vec![
    //                         Tok::Def,
    //                         Tok::Name {
    //                             name: String::from("foo"),
    //                         },
    //                         Tok::Lpar,
    //                         Tok::Rpar,
    //                         Tok::Colon,
    //                         Tok::Newline,
    //                         Tok::Indent,
    //                         Tok::If,
    //                         Tok::Name {
    //                             name: String::from("x"),
    //                         },
    //                         Tok::Colon,
    //                         Tok::Newline,
    //                         Tok::NonLogicalNewline,
    //                         Tok::Indent,
    //                         Tok::Return,
    //                         Tok::Int { value: BigInt::from(99) },
    //                         Tok::Newline,
    //                         Tok::NonLogicalNewline,
    //                         Tok::Dedent,
    //                         Tok::Dedent,
    //                     ]
    //                 );
    //             }
    //         )*
    //         }
    //     }
    //
    //     macro_rules! test_double_dedent_with_tabs {
    //         ($($name:ident: $eol:expr,)*) => {
    //         $(
    //             #[test]
    //             fn $name() {
    //                 let source = format!("def foo():{}\tif x:{}{}\t return 99{}{}", $eol, $eol, $eol, $eol, $eol);
    //                 let tokens = lex_source(&source);
    //                 assert_eq!(
    //                     tokens,
    //                     vec![
    //                         Tok::Def,
    //                         Tok::Name {
    //                             name: String::from("foo"),
    //                         },
    //                         Tok::Lpar,
    //                         Tok::Rpar,
    //                         Tok::Colon,
    //                         Tok::Newline,
    //                         Tok::Indent,
    //                         Tok::If,
    //                         Tok::Name {
    //                             name: String::from("x"),
    //                         },
    //                         Tok::Colon,
    //                         Tok::Newline,
    //                         Tok::NonLogicalNewline,
    //                         Tok::Indent,
    //                         Tok::Return,
    //                         Tok::Int { value: BigInt::from(99) },
    //                         Tok::Newline,
    //                         Tok::NonLogicalNewline,
    //                         Tok::Dedent,
    //                         Tok::Dedent,
    //                     ]
    //                 );
    //             }
    //         )*
    //         }
    //     }
    //
    //     test_double_dedent_with_eol! {
    //         test_double_dedent_windows_eol: WINDOWS_EOL,
    //         test_double_dedent_mac_eol: MAC_EOL,
    //         test_double_dedent_unix_eol: UNIX_EOL,
    //     }
    //
    //     test_double_dedent_with_tabs! {
    //         test_double_dedent_tabs_windows_eol: WINDOWS_EOL,
    //         test_double_dedent_tabs_mac_eol: MAC_EOL,
    //         test_double_dedent_tabs_unix_eol: UNIX_EOL,
    //     }
    //
    //     macro_rules! test_newline_in_brackets {
    //         ($($name:ident: $eol:expr,)*) => {
    //         $(
    //             #[test]
    //             fn $name() {
    //                 let source = r"x = [
    //
    //     1,2
    // ,(3,
    // 4,
    // ), {
    // 5,
    // 6,\
    // 7}]
    // ".replace("\n", $eol);
    //                 let tokens = lex_source(&source);
    //                 assert_eq!(
    //                     tokens,
    //                     vec![
    //                         Tok::Name {
    //                             name: String::from("x"),
    //                         },
    //                         Tok::Equal,
    //                         Tok::Lsqb,
    //                         Tok::NonLogicalNewline,
    //                         Tok::NonLogicalNewline,
    //                         Tok::Int { value: BigInt::from(1) },
    //                         Tok::Comma,
    //                         Tok::Int { value: BigInt::from(2) },
    //                         Tok::NonLogicalNewline,
    //                         Tok::Comma,
    //                         Tok::Lpar,
    //                         Tok::Int { value: BigInt::from(3) },
    //                         Tok::Comma,
    //                         Tok::NonLogicalNewline,
    //                         Tok::Int { value: BigInt::from(4) },
    //                         Tok::Comma,
    //                         Tok::NonLogicalNewline,
    //                         Tok::Rpar,
    //                         Tok::Comma,
    //                         Tok::Lbrace,
    //                         Tok::NonLogicalNewline,
    //                         Tok::Int { value: BigInt::from(5) },
    //                         Tok::Comma,
    //                         Tok::NonLogicalNewline,
    //                         Tok::Int { value: BigInt::from(6) },
    //                         Tok::Comma,
    //                         // Continuation here - no NonLogicalNewline.
    //                         Tok::Int { value: BigInt::from(7) },
    //                         Tok::Rbrace,
    //                         Tok::Rsqb,
    //                         Tok::Newline,
    //                     ]
    //                 );
    //             }
    //         )*
    //         };
    //     }
    //
    //     test_newline_in_brackets! {
    //         test_newline_in_brackets_windows_eol: WINDOWS_EOL,
    //         test_newline_in_brackets_mac_eol: MAC_EOL,
    //         test_newline_in_brackets_unix_eol: UNIX_EOL,
    //     }
    //
    //     #[test]
    //     fn test_non_logical_newline_in_string_continuation() {
    //         let source = r"(
    //     'a'
    //     'b'
    //
    //     'c' \
    //     'd'
    // )";
    //         let tokens = lex_source(source);
    //         assert_eq!(
    //             tokens,
    //             vec![
    //                 TokenKind::Lpar,
    //                 TokenKind::NonLogicalNewline,
    //                 str_tok("a"),
    //                 TokenKind::NonLogicalNewline,
    //                 str_tok("b"),
    //                 TokenKind::NonLogicalNewline,
    //                 TokenKind::NonLogicalNewline,
    //                 str_tok("c"),
    //                 str_tok("d"),
    //                 TokenKind::NonLogicalNewline,
    //                 TokenKind::Rpar,
    //                 TokenKind::Newline,
    //             ]
    //         );
    //     }
    //
    //     #[test]
    //     fn test_logical_newline_line_comment() {
    //         let source = "#Hello\n#World\n";
    //         let tokens = lex_source(source);
    //         assert_eq!(
    //             tokens,
    //             vec![
    //                 TokenKind::Comment("#Hello".to_owned()),
    //                 TokenKind::NonLogicalNewline,
    //                 TokenKind::Comment("#World".to_owned()),
    //                 TokenKind::NonLogicalNewline,
    //             ]
    //         );
    //     }
    //
    //     #[test]
    //     fn test_operators() {
    //         let source = "//////=/ /";
    //         let tokens = lex_source(source);
    //         assert_eq!(
    //             tokens,
    //             vec![
    //                 TokenKind::DoubleSlash,
    //                 TokenKind::DoubleSlash,
    //                 TokenKind::DoubleSlashEqual,
    //                 TokenKind::Slash,
    //                 TokenKind::Slash,
    //                 TokenKind::Newline,
    //             ]
    //         );
    //     }
    //
    //     #[test]
    //     fn test_string() {
    //         let source = r#""double" 'single' 'can\'t' "\\\"" '\t\r\n' '\g' r'raw\'' '\420' '\200\0a'"#;
    //         let tokens = lex_source(source);
    //         assert_eq!(
    //             tokens,
    //             vec![
    //                 str_tok("double"),
    //                 str_tok("single"),
    //                 str_tok(r"can\'t"),
    //                 str_tok(r#"\\\""#),
    //                 str_tok(r"\t\r\n"),
    //                 str_tok(r"\g"),
    //                 raw_str_tok(r"raw\'"),
    //                 str_tok(r"\420"),
    //                 str_tok(r"\200\0a"),
    //                 TokenKind::Newline,
    //             ]
    //         );
    //     }
    //
    //     macro_rules! test_string_continuation {
    //         ($($name:ident: $eol:expr,)*) => {
    //         $(
    //             #[test]
    //             fn $name() {
    //                 let source = format!("\"abc\\{}def\"", $eol);
    //                 let tokens = lex_source(&source);
    //                 assert_eq!(
    //                     tokens,
    //                     vec![
    //                         str_tok("abc\\\ndef"),
    //                         Tok::Newline,
    //                     ]
    //                 )
    //             }
    //         )*
    //         }
    //     }
    //
    //     test_string_continuation! {
    //         test_string_continuation_windows_eol: WINDOWS_EOL,
    //         test_string_continuation_mac_eol: MAC_EOL,
    //         test_string_continuation_unix_eol: UNIX_EOL,
    //     }
    //
    //     #[test]
    //     fn test_escape_unicode_name() {
    //         let source = r#""\N{EN SPACE}""#;
    //         let tokens = lex_source(source);
    //         assert_eq!(tokens, vec![str_tok(r"\N{EN SPACE}"), TokenKind::Newline])
    //     }
    //
    //     macro_rules! test_triple_quoted {
    //         ($($name:ident: $eol:expr,)*) => {
    //         $(
    //             #[test]
    //             fn $name() {
    //                 let source = format!("\"\"\"{0} test string{0} \"\"\"", $eol);
    //                 let tokens = lex_source(&source);
    //                 assert_eq!(
    //                     tokens,
    //                     vec![
    //                         Tok::String {
    //                             value: "\n test string\n ".to_owned(),
    //                             kind: StringKind::String,
    //                             triple_quoted: true,
    //                         },
    //                         Tok::Newline,
    //                     ]
    //                 )
    //             }
    //         )*
    //         }
    //     }
    //
    //     test_triple_quoted! {
    //         test_triple_quoted_windows_eol: WINDOWS_EOL,
    //         test_triple_quoted_mac_eol: MAC_EOL,
    //         test_triple_quoted_unix_eol: UNIX_EOL,
    //     }
}
