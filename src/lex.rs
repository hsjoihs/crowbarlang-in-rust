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
    LeftBracket,
    RightBracket,
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
    Dot,
    Increment,
    Decrement,
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

/*#[test]
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
}*/

impl LexerState {
    #[must_use]
    const fn new() -> Self {
        Self {
            line_number: 1,
            mode: LexerMode::Initial,
        }
    }

    fn get_token_raw<'a>(&mut self, input: &'a [char]) -> (Option<Token>, &'a [char]) {
        match self.mode {
            LexerMode::Initial => {
                match input {
                    [] => (None, input),
                    [init @ ('a'..='z' | 'A'..='Z' | '_'), rest @ ..] => {
                        let mut rest = rest;
                        let mut ident = String::from(*init);
                        while let [c @ ('a'..='z' | 'A'..='Z' | '_' | '0'..='9'), rest2 @ ..] = rest
                        {
                            ident.push(*c);
                            rest = rest2;
                        }
                        (
                            Some(Token::Identifier(Ident::from_string_unchecked(ident))),
                            rest,
                        )
                    }
                    [init @ '0'..='9', rest @ ..] => {
                        let mut rest = rest;
                        let mut num = String::from(*init);
                        while let [c @ ('0'..='9' | '.'), rest2 @ ..] = rest {
                            num.push(*c);
                            rest = rest2;
                        }

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
                    ['\n', rest @ ..] => {
                        self.line_number += 1;
                        self.get_token_raw(rest)
                    }
                    [' ' | '\t' | '\r', rest @ ..] => self.get_token_raw(rest),
                    ['#', rest @ ..] => {
                        self.mode = LexerMode::Comment;
                        self.get_token_raw(rest)
                    }
                    ['"', rest @ ..] => {
                        self.mode = LexerMode::StringLiteralState;
                        self.get_token_raw(rest)
                    }
                    ['(', rest @ ..] => (Some(Token::LeftParen), rest),
                    [')', rest @ ..] => (Some(Token::RightParen), rest),
                    ['{', rest @ ..] => (Some(Token::LeftCurly), rest),
                    ['}', rest @ ..] => (Some(Token::RightCurly), rest),
                    ['[', rest @ ..] => (Some(Token::LeftBracket), rest),
                    [']', rest @ ..] => (Some(Token::RightBracket), rest),
                    [';', rest @ ..] => (Some(Token::Semicolon), rest),
                    [',', rest @ ..] => (Some(Token::Comma), rest),
                    ['&', '&', rest @ ..] => (Some(Token::LogicalAnd), rest),
                    ['|', '|', rest @ ..] => (Some(Token::LogicalOr), rest),

                    // Must look for == earlier than we look for =
                    ['=', '=', rest @ ..] => (Some(Token::Equal), rest),
                    ['>', '=', rest @ ..] => (Some(Token::GreaterThanOrEqual), rest),
                    ['<', '=', rest @ ..] => (Some(Token::LessThanOrEqual), rest),
                    ['!', '=', rest @ ..] => (Some(Token::NotEqual), rest),
                    ['>', rest @ ..] => (Some(Token::GreaterThan), rest),
                    ['<', rest @ ..] => (Some(Token::LessThan), rest),
                    ['=', rest @ ..] => (Some(Token::Assign), rest),
                    // Must look for ++ earlier than we look for +
                    ['+', '+', rest @ ..] => (Some(Token::Increment), rest),
                    ['-', '-', rest @ ..] => (Some(Token::Decrement), rest),
                    ['+', rest @ ..] => (Some(Token::Add), rest),
                    ['-', rest @ ..] => (Some(Token::Sub), rest),
                    ['*', rest @ ..] => (Some(Token::Mul), rest),
                    ['/', rest @ ..] => (Some(Token::Div), rest),
                    ['%', rest @ ..] => (Some(Token::Mod), rest),
                    ['.', rest @ ..] => (Some(Token::Dot), rest),
                    [c, ..] => panic!(
                        "Invalid character `{}` found at line `{}`.",
                        c, self.line_number
                    ),
                }
            }
            LexerMode::StringLiteralState => {
                let mut rest = input;
                let mut ans = String::new();
                loop {
                    match rest {
                        ['\\', '\\', rest2 @ ..] => {
                            ans.push('\\');
                            rest = rest2;
                        }
                        ['\\', 'n', rest2 @ ..] => {
                            ans.push('\n');
                            rest = rest2;
                        }
                        ['\\', 't', rest2 @ ..] => {
                            ans.push('\t');
                            rest = rest2;
                        }
                        ['\\', '\"', rest2 @ ..] => {
                            ans.push('"');
                            rest = rest2;
                        }
                        ['"', rest2 @ ..] => {
                            self.mode = LexerMode::Initial;
                            return (Some(Token::StringLiteral(ans)), rest2);
                        }
                        [] => panic!("Unterminated string literal."),
                        [c, rest2 @ ..] => {
                            ans.push(*c);
                            rest = rest2;
                        }
                    }
                }
            }
            LexerMode::Comment => {
                let mut rest = input;
                loop {
                    match rest {
                        ['\n', rest2 @ ..] => {
                            self.line_number += 1;
                            self.mode = LexerMode::Initial;
                            return self.get_token_raw(rest2);
                        }
                        [] => return (None, rest),
                        [_, rest2 @ ..] => {
                            rest = rest2;
                        }
                    }
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineNumber(pub usize);

#[must_use]
pub fn lex_with_linenumber(input: &str) -> Vec<(Token, LineNumber)> {
    let mut ans = Vec::new();
    let mut state = LexerState::new();
    let rest: Vec<char> = input.chars().collect();
    let mut rest: &[char] = &rest;

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
                ans.push((reserved_token.clone(), LineNumber(state.line_number)));
                continue 'outer;
            }
        }
        ans.push((token, LineNumber(state.line_number)));
    }
    ans
}

#[must_use]
pub fn lex(input: &str) -> Vec<Token> {
    lex_with_linenumber(input)
        .into_iter()
        .map(|(tok, _num)| tok)
        .collect()
}
