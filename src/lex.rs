#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Function,
    If,
    Else,
    Elsif,
    While,
    For,
    ReturnT,
    Break,
    Continue,
    NullT,
    TrueT,
    FalseT,
    GlobalT,
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
    Identifier(String),
    IntLiteral(i32),
    DoubleLiteral(f64),
    StringLiteral(String),
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

macro_rules! try_strip_and_return {
    ($input: expr, $prefix: expr, $token: expr) => {
        if let Some(rest) = $input.strip_prefix($prefix) {
            return (Some($token), rest);
        }
    };
}

#[test]
fn test_get_token_raw() {
    assert_eq!(
        LexerState::new().get_token_raw("a(b)"),
        (Some(Token::Identifier("a".to_string())), "(b)")
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
    fn new() -> LexerState {
        LexerState {
            line_number: 0,
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
                        (Some(Token::Identifier(ident)), rest)
                    }
                    Some('1'..='9') => {
                        let num: String = input
                            .chars()
                            .take_while(|c| matches!(c, '0'..='9'))
                            .collect();
                        let rest = input.trim_start_matches(|c| matches!(c, '0'..='9'));
                        let num = num.parse().unwrap();
                        (Some(Token::IntLiteral(num)), rest)
                    }
                    Some('0') => (Some(Token::IntLiteral(0)), &input[1..]),
                    Some(c) => {
                        if let Some(rest) = input.strip_prefix('\n') {
                            self.line_number += 1;
                            return self.get_token_raw(rest);
                        }

                        if let Some(rest) = input.strip_prefix([' ', '\t']) {
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

                        try_strip_and_return!(input, '(', Token::LeftParen);
                        try_strip_and_return!(input, ')', Token::RightParen);
                        try_strip_and_return!(input, '{', Token::LeftCurly);
                        try_strip_and_return!(input, '}', Token::RightCurly);
                        try_strip_and_return!(input, ';', Token::Semicolon);
                        try_strip_and_return!(input, ',', Token::Comma);
                        try_strip_and_return!(input, "&&", Token::LogicalAnd);
                        try_strip_and_return!(input, "||", Token::LogicalOr);
                        // Must look for == earlier than we look for =
                        try_strip_and_return!(input, "==", Token::Equal);
                        try_strip_and_return!(input, ">=", Token::GreaterThanOrEqual);
                        try_strip_and_return!(input, "<=", Token::LessThanOrEqual);
                        try_strip_and_return!(input, "!=", Token::NotEqual);
                        try_strip_and_return!(input, '>', Token::GreaterThan);
                        try_strip_and_return!(input, '<', Token::LessThan);
                        try_strip_and_return!(input, '=', Token::Assign);
                        try_strip_and_return!(input, '>', Token::GreaterThan);
                        try_strip_and_return!(input, '+', Token::Add);
                        try_strip_and_return!(input, '-', Token::Sub);
                        try_strip_and_return!(input, '*', Token::Mul);
                        try_strip_and_return!(input, '/', Token::Div);
                        try_strip_and_return!(input, '%', Token::Mod);

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
                                rest = &rest[c.len_utf8()..]
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

pub fn lex(input: &str) -> Vec<Token> {
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
            ("return", Token::ReturnT),
            ("break", Token::Break),
            ("continue", Token::Continue),
            ("null", Token::NullT),
            ("true", Token::TrueT),
            ("false", Token::FalseT),
            ("global", Token::GlobalT),
        ] {
            if token == Token::Identifier(reserved_str.to_string()) {
                ans.push(reserved_token.clone());
                continue 'outer;
            }
        }
        ans.push(token);
    }
    ans
}
