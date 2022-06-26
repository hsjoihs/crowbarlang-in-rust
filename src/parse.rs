use crate::lex::Token;

#[derive(PartialEq, Debug)]
pub enum Expr {
    Assign(String, Box<Expr>),
    LogicalOr(Box<Expr>, Box<Expr>),
    LogicalAnd(Box<Expr>, Box<Expr>),
    Equal(Box<Expr>, Box<Expr>),
    NotEqual(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanOrEqual(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    LessThanOrEqual(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    Negative(Box<Expr>),
    Null,
    True,
    False,
    StringLiteral(String),
    DoubleLiteral(f64),
    IntLiteral(i32),
    FunctionCall(String, Vec<Expr>),
    Identifier(String),
}

struct ParserState<'a> {
    tokvec: &'a [Token],
}

macro_rules! left_assoc_bin {
    ($this_parser_name: ident, $next_parser_name: ident, $delimiter_token: pat, $get_constructor_from_tok: expr) => {
        fn $this_parser_name(&mut self) -> Expr {
            let mut expr = self.$next_parser_name();
            loop {
                if let Some(a @ $delimiter_token) = self.tokvec.get(0) {
                    self.advance(1);
                    let expr2 = self.$next_parser_name();
                    expr = $get_constructor_from_tok(a)(Box::new(expr), Box::new(expr2));
                } else {
                    break;
                }
            }

            expr
        }
    };
}

macro_rules! expect_token_and_advance {
    ($self:expr, $token: pat, $expected: expr) => {
        match $self.tokvec.get(0) {
            Some(token @ $token) => {
                $self.advance(1);
                token
            }
            None => panic!("Unexpected end of file encountered; expected {}", $expected),
            Some(unexpected) => panic!(
                "Unexpected {:?} encountered; expected {}",
                unexpected, $expected
            ),
        }
    };
}

#[test]
fn test_parse_expression() {
    assert_eq!(
        parse_expression(&[
            Token::Identifier("i".to_string()),
            Token::Mod,
            Token::IntLiteral(15),
            Token::Equal,
            Token::IntLiteral(0),
        ]),
        Expr::Equal(
            Box::new(Expr::Mod(
                Box::new(Expr::Identifier("i".to_string())),
                Box::new(Expr::IntLiteral(15))
            )),
            Box::new(Expr::IntLiteral(0))
        )
    )
}

pub fn parse_expression(tokvec: &[Token]) -> Expr {
    let mut state = ParserState::new(tokvec);
    state.parse_expression()
}

impl<'a> ParserState<'a> {
    pub fn new(tokvec: &'a [Token]) -> Self {
        ParserState { tokvec }
    }
    fn advance(&mut self, i: usize) {
        self.tokvec = &self.tokvec[i..];
    }
    pub fn parse_expression(&mut self) -> Expr {
        if let Some(Token::Identifier(ident)) = self.tokvec.get(0) {
            if let Some(Token::Assign) = self.tokvec.get(1) {
                self.advance(2);
                let expr2 = self.parse_expression();
                return Expr::Assign(ident.to_owned(), Box::new(expr2));
            }
        }
        self.parse_logical_or_expression()
    }

    left_assoc_bin! {
        parse_logical_or_expression,
        parse_logical_and_expression,
        Token::LogicalOr,
        |_| Expr::LogicalOr
    }
    left_assoc_bin! {
        parse_logical_and_expression,
        parse_equality_expression,
        Token::LogicalAnd,
        |_| Expr::LogicalAnd
    }
    left_assoc_bin! {
        parse_equality_expression,
        parse_relational_expression,
        Token::Equal | Token::NotEqual,
        |t| if t == &Token::Equal {
            Expr::Equal
        } else {
            Expr::NotEqual
        }
    }
    left_assoc_bin! {
        parse_relational_expression,
        parse_additive_expression,
        Token::GreaterThan | Token::GreaterThanOrEqual | Token::LessThan | Token::LessThanOrEqual,
        |t| match t {
            &Token::GreaterThan => Expr::GreaterThan,
            &Token::GreaterThanOrEqual => Expr::GreaterThanOrEqual,
            &Token::LessThan => Expr::LessThan,
            _ => Expr::LessThanOrEqual,
        }
    }
    left_assoc_bin! {
        parse_additive_expression,
        parse_multiplicative_expression,
        Token::Add | Token::Sub,
        |t| if t == &Token::Add {
            Expr::Add
        } else {
            Expr::Sub
        }
    }
    left_assoc_bin! {
        parse_multiplicative_expression,
        parse_unary_expression,
        Token::Mul | Token::Div | Token::Mod,
        |t| match t {
            &Token::Mul => Expr::Mul,
            &Token::Div => Expr::Div,
            _ => Expr::Mod,
        }
    }

    fn parse_unary_expression(&mut self) -> Expr {
        if let Some(Token::Sub) = self.tokvec.get(0) {
            self.advance(1);
            let expr2 = self.parse_unary_expression();
            return Expr::Negative(Box::new(expr2));
        }
        self.parse_primary_expression()
    }

    fn parse_primary_expression(&mut self) -> Expr {
        match self.tokvec.get(0) {
            Some(Token::NullT) => {
                self.advance(1);
                Expr::Null
            }
            Some(Token::FalseT) => {
                self.advance(1);
                Expr::False
            }
            Some(Token::TrueT) => {
                self.advance(1);
                Expr::True
            }
            Some(Token::StringLiteral(s)) => {
                self.advance(1);
                Expr::StringLiteral(s.to_owned())
            }
            Some(Token::DoubleLiteral(d)) => {
                self.advance(1);
                Expr::DoubleLiteral(*d)
            }
            Some(Token::IntLiteral(i)) => {
                self.advance(1);
                Expr::IntLiteral(*i)
            }
            Some(Token::LeftParen) => {
                self.advance(1);
                let expr = self.parse_expression();
                expect_token_and_advance!(self, Token::RightParen, "right paren");
                expr
            }
            Some(Token::Identifier(ident)) => {
                self.advance(1);
                if let Some(Token::LeftParen) = self.tokvec.get(0) {
                    self.advance(1);
                    if let Some(Token::RightParen) = self.tokvec.get(1) {
                        self.advance(1);
                        Expr::FunctionCall(ident.to_owned(), vec![])
                    } else {
                        let exprs = self.parse_argument_list();
                        expect_token_and_advance!(
                            self,
                            Token::RightParen,
                            "right paren of a function call"
                        );
                        Expr::FunctionCall(ident.to_owned(), exprs)
                    }
                } else {
                    // ident
                    Expr::Identifier(ident.to_owned())
                }
            }
            None => {
                panic!("Unexpected end of file encountered while trying to parse an expression")
            }
            Some(unexpected) => panic!(
                "Unexpected {:?} encountered while trying to parse an expression",
                unexpected
            ),
        }
    }

    fn parse_argument_list(&mut self) -> Vec<Expr> {
        let mut ans = vec![self.parse_expression()];
        loop {
            if let Some(Token::Comma) = self.tokvec.get(0) {
                self.advance(1);
                ans.push(self.parse_expression());
            } else {
                break;
            }
        }
        ans
    }
}
