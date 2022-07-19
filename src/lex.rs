#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Function,
    If,
    Else,
    Elsif,
    While,
    For,
    Return,
    Break,
    Continue,
    Null,
    True,
    False,
    Global,
    LeftParen,
    RightParen,
    LeftCurly,
    RightCurly,
    Semicolon,
    Comma,
    LogicalAnd,
    LogicalOr,
    Assign,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Identifier(Ident),
    IntLiteral(i32),
    DoubleLiteral(f64),
    StringLiteral(String),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ident(String);

impl Ident {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.0
    }
    /// # Panics
    /// Panics if the input does not start with a character that matches `'a'..='z' | 'A'..='Z' | '_'`,
    /// or if the input contains something other than `'a'..='z' | 'A'..='Z' | '_' | '0'..='9'`.
    #[must_use]
    pub fn from(s: &str) -> Self {
        let mut iter = s.chars();
        assert!(matches!(iter.next(), Some('a'..='z' | 'A'..='Z' | '_')));
        for c in iter {
            assert!(matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'));
        }
        Self(s.to_owned())
    }

    #[must_use]
    const fn from_string_unchecked(s: String) -> Self {
        Self(s)
    }
}

enum LexerMode {
    Initial,
    Comment,
    StringLiteralState,
}

struct LexerState {
    mode: LexerMode,
    line_number: usize,
}

#[test]
fn test_get_token_raw() {
    assert_eq!(
        LexerState::new().get_token_raw("a(b)"),
        (Some(Token::Identifier(Ident::from("a"))), "(b)")
    );

    assert_eq!(
        LexerState::new().get_token_raw("  ==b)"),
        (Some(Token::Equal), "b)")
    );
    assert_eq!(
        LexerState::new().get_token_raw(
            r##"
            # comment
              ==b)"##
        ),
        (Some(Token::Equal), "b)")
    );
}

impl LexerState {
    #[must_use]
    const fn new() -> Self {
        Self {
            line_number: 1,
            mode: LexerMode::Initial,
        }
    }

    fn get_token_raw<'a>(&mut self, input: &'a str) -> (Option<Token>, &'a str) {
        match self.mode {
            LexerMode::Initial => {
                match input.chars().next() {
                    None => (None, input),
                    Some('a'..='z' | 'A'..='Z' | '_') => {
                        let ident: String = input
                            .chars()
                            .take_while(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
                            .collect();
                        let rest = input.trim_start_matches(
                            |c| matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'),
                        );
                        (
                            Some(Token::Identifier(Ident::from_string_unchecked(ident))),
                            rest,
                        )
                    }
                    Some('0'..='9') => {
                        let num: String = input
                            .chars()
                            .take_while(|c| matches!(c, '0'..='9' | '.'))
                            .collect();
                        let rest = input.trim_start_matches(|c| matches!(c, '0'..='9' | '.'));

                        if num.contains('.') {
                            // There must be one and only one period, and it must not come at the end
                            if num.chars().filter(|c| *c == '.').count() == 1 {
                                assert!(
                                    !num.ends_with('.'),
                                     "Invalid numeric literal `{}` found at line `{}`. A numeric literal cannot end with a period.",
                                        num, self.line_number);
                                let num = num.parse().unwrap();
                                (Some(Token::DoubleLiteral(num)), rest)
                            } else {
                                panic!(
                                    "Invalid numeric literal `{}` found at line `{}`. A numeric literal cannot contain two or more periods.",
                                    num, self.line_number
                                )
                            }
                        } else if num.starts_with('0') {
                            // The number must either be 0, or else it cannot start with a zero
                            if num == "0" {
                                (Some(Token::IntLiteral(0)), &input[1..])
                            } else {
                                panic!(
                                    "Invalid numeric literal `{}` found at line `{}`. A numeric literal cannot start with a zero unless it is zero itself.",
                                    num, self.line_number
                                )
                            }
                        } else {
                            let num = num.parse().unwrap();
                            (Some(Token::IntLiteral(num)), rest)
                        }
                    }
                    Some(c) => {
                        if let Some(rest) = input.strip_prefix('\n') {
                            self.line_number += 1;
                            return self.get_token_raw(rest);
                        }

                        if let Some(rest) = input.strip_prefix([' ', '\t', '\r']) {
                            return self.get_token_raw(rest);
                        }

                        if let Some(rest) = input.strip_prefix('#') {
                            self.mode = LexerMode::Comment;
                            return self.get_token_raw(rest);
                        }

                        if let Some(rest) = input.strip_prefix('"') {
                            self.mode = LexerMode::StringLiteralState;
                            return self.get_token_raw(rest);
                        }

                        for (prefix, token) in [
                            ("(", Token::LeftParen),
                            (")", Token::RightParen),
                            ("{", Token::LeftCurly),
                            ("}", Token::RightCurly),
                            (";", Token::Semicolon),
                            (",", Token::Comma),
                            ("&&", Token::LogicalAnd),
                            ("||", Token::LogicalOr),
                            // Must look for == earlier than we look for =
                            ("==", Token::Equal),
                            (">=", Token::GreaterThanOrEqual),
                            ("<=", Token::LessThanOrEqual),
                            ("!=", Token::NotEqual),
                            (">", Token::GreaterThan),
                            ("<", Token::LessThan),
                            ("=", Token::Assign),
                            (">", Token::GreaterThan),
                            ("+", Token::Add),
                            ("-", Token::Sub),
                            ("*", Token::Mul),
                            ("/", Token::Div),
                            ("%", Token::Mod),
                        ] {
                            if let Some(rest) = input.strip_prefix(prefix) {
                                return (Some(token), rest);
                            }
                        }
                        panic!(
                            "Invalid character `{}` found at line `{}`.",
                            c, self.line_number
                        );
                    }
                }
            }
            LexerMode::StringLiteralState => {
                let mut rest = input;
                let mut ans = String::new();
                loop {
                    if let Some(rest2) = rest.strip_prefix("\\\\") {
                        ans.push('\\');
                        rest = rest2;
                    } else if let Some(rest2) = rest.strip_prefix("\\n") {
                        ans.push('\n');
                        rest = rest2;
                    } else if let Some(rest2) = rest.strip_prefix("\\t") {
                        ans.push('\t');
                        rest = rest2;
                    } else if let Some(rest2) = rest.strip_prefix("\\\"") {
                        ans.push('"');
                        rest = rest2;
                    } else if let Some(rest2) = rest.strip_prefix('\n') {
                        self.line_number += 1;
                        ans.push('\n');
                        rest = rest2;
                    } else if let Some(rest2) = rest.strip_prefix('"') {
                        self.mode = LexerMode::Initial;
                        return (Some(Token::StringLiteral(ans)), rest2);
                    } else {
                        match rest.chars().next() {
                            None => panic!("Unterminated string literal."),
                            Some(c) => {
                                ans.push(c);
                                rest = &rest[c.len_utf8()..];
                            }
                        }
                    }
                }
            }
            LexerMode::Comment => match input.split_once('\n') {
                None => {
                    // All the remaining content is a comment. No more tokens to read
                    (None, "")
                }
                Some((_comment, rest)) => {
                    self.line_number += 1;
                    self.mode = LexerMode::Initial;
                    self.get_token_raw(rest)
                }
            },
        }
    }
}

#[must_use]
pub fn lex_with_linenumber(input: &str) -> Vec<(Token, usize)> {
    let mut ans = Vec::new();
    let mut state = LexerState::new();
    let mut rest = input;

    'outer: while let (Some(token), rest2) = state.get_token_raw(rest) {
        rest = rest2;
        for (reserved_str, reserved_token) in &[
            ("function", Token::Function),
            ("if", Token::If),
            ("else", Token::Else),
            ("elsif", Token::Elsif),
            ("while", Token::While),
            ("for", Token::For),
            ("return", Token::Return),
            ("break", Token::Break),
            ("continue", Token::Continue),
            ("null", Token::Null),
            ("true", Token::True),
            ("false", Token::False),
            ("global", Token::Global),
        ] {
            if token == Token::Identifier(Ident::from(reserved_str)) {
                ans.push((reserved_token.clone(), state.line_number));
                continue 'outer;
            }
        }
        ans.push((token, state.line_number));
    }
    ans
}

#[must_use]
pub fn lex(input: &str) -> Vec<Token> {
    lex_with_linenumber(input).into_iter().map(|(tok, _num)| tok).collect()
}
