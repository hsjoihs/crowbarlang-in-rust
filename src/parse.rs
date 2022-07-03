use crate::lex::{Ident, Token};

#[derive(PartialEq, Debug)]
pub enum Expr {
    Assign(Ident, Box<Expr>),
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
    FunctionCall(Ident, Vec<Expr>),
    Identifier(Ident),
}

struct ParserState<'a> {
    tokvec: &'a [(Token, usize)],
}

macro_rules! left_assoc_bin {
    ($this_parser_name: ident, $next_parser_name: ident, $delimiter_token: pat, $get_constructor_from_tok: expr) => {
        fn $this_parser_name(&mut self) -> Expr {
            let mut expr = self.$next_parser_name();
            loop {
                if let Some((a @ $delimiter_token, _)) = self.tokvec.get(0) {
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

macro_rules! comma_list {
    ($this_parser_name: ident, $elem_parser_name: ident, $elem_type: ty) => {
        fn $this_parser_name(&mut self) -> Vec<$elem_type> {
            let mut ans = vec![self.$elem_parser_name()];
            loop {
                if matches!(self.tokvec.get(0), Some((Token::Comma, _))) {
                    self.advance(1);
                    ans.push(self.$elem_parser_name());
                } else {
                    break;
                }
            }
            ans
        }
    };
}

macro_rules! expect_token_and_advance {
    ($self:expr, $token: pat, $expected: expr) => {
        match $self.tokvec.get(0) {
            Some((token @ $token, _line_number)) => {
                $self.advance(1);
                token
            }
            None => panic!("Unexpected end of file encountered; expected {}", $expected),
            Some((unexpected, line_number)) => panic!(
                "Parse error at line {}: Unexpected {:?} encountered; expected {}",
                line_number, unexpected, $expected
            ),
        }
    };
}

#[test]
fn test_parse_expression() {
    use crate::lex::Ident;
    assert_eq!(
        crate::parse::expression(&[
            (Token::Identifier(Ident::from("i")),0),
            (Token::Mod,0),
            (Token::IntLiteral(15),0),
            (Token::Equal,0),
            (Token::IntLiteral(0),0),
        ]),
        Expr::Equal(
            Box::new(Expr::Mod(
                Box::new(Expr::Identifier(Ident::from("i"))),
                Box::new(Expr::IntLiteral(15))
            )),
            Box::new(Expr::IntLiteral(0))
        )
    );
}

#[test]
fn test_parse_expression2() {
    use crate::lex::Ident;
    let tokvec = vec![
        (Token::Identifier(Ident::from("print")), 0),
        (Token::LeftParen, 0),
        (Token::StringLiteral("FizzBuzz\n".to_string()), 0),
        (Token::RightParen, 0),
    ];
    let mut state = ParserState::new(&tokvec);
    let expr = state.parse_expression();
    assert_eq!(
        expr,
        Expr::FunctionCall(
            Ident::from("print"),
            vec![Expr::StringLiteral("FizzBuzz\n".to_string())]
        )
    );
    assert_eq!(state.tokvec, vec![]);
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Expression(Option<Expr>),
    Global(Vec<Ident>),
    If {
        if_expr: Expr,
        if_block: Block,
        elsif_list: Vec<(Expr, Block)>,
        else_block: Option<Block>,
    },
    While(Expr, Block),
    For(Option<Expr>, Option<Expr>, Option<Expr>, Block),
    Return(Option<Expr>),
    Break,
    Continue,
}

#[derive(Debug, PartialEq)]
pub struct Block(pub Vec<Statement>);

#[must_use]
pub fn expression(tokvec: &[(Token, usize)]) -> Expr {
    let mut state = ParserState::new(tokvec);
    state.parse_expression()
}

#[must_use]
pub fn statement(tokvec: &[(Token, usize)]) -> Statement {
    let mut state = ParserState::new(tokvec);
    state.parse_statement()
}

#[must_use]
pub fn translation_unit(tokvec: &[(Token, usize)]) -> Vec<DefinitionOrStatement> {
    let mut state = ParserState::new(tokvec);
    state.parse_translation_unit()
}

#[must_use]
pub fn statements(tokvec: &[(Token, usize)]) -> Vec<Statement> {
    let mut state = ParserState::new(tokvec);
    let mut stmts = vec![];
    loop {
        if state.tokvec.get(0).is_none() {
            return stmts;
        }
        stmts.push(state.parse_statement());
    }
}

macro_rules! parse_optional_expression_and_a_token {
    ($self: expr, $token: pat, $msg: expr) => {
        if matches!($self.tokvec.get(0), Some((Token::Semicolon, _))) {
            $self.advance(1);
            None
        } else {
            let expr = $self.parse_expression();
            expect_token_and_advance!($self, $token, $msg);
            Some(expr)
        }
    };
}
#[derive(PartialEq, Debug)]
pub struct CrowbarFuncDef {
    pub func_name: Ident,
    pub params: Vec<Ident>,
    pub block: Block,
}

#[derive(PartialEq, Debug)]
pub enum DefinitionOrStatement {
    Definition(CrowbarFuncDef),
    Statement(Statement),
}

impl<'a> ParserState<'a> {
    pub const fn new(tokvec: &'a [(Token, usize)]) -> Self {
        ParserState { tokvec }
    }
    fn advance(&mut self, i: usize) {
        self.tokvec = &self.tokvec[i..];
    }

    pub fn parse_translation_unit(&mut self) -> Vec<DefinitionOrStatement> {
        let mut things = vec![];
        loop {
            if self.tokvec.get(0).is_none() {
                return things;
            }
            things.push(self.parse_definition_or_statement());
        }
    }

    fn parse_definition_or_statement(&mut self) -> DefinitionOrStatement {
        if matches!(self.tokvec.get(0), Some((Token::Function,_))) {
            let func_def = self.parse_function_definition();
            DefinitionOrStatement::Definition(func_def)
        } else {
            let statement = self.parse_statement();
            DefinitionOrStatement::Statement(statement)
        }
    }

    fn parse_function_definition(&mut self) -> CrowbarFuncDef {
        if matches!(self.tokvec.get(0), Some((Token::Function,_))) {
            self.advance(1);
            if let Some((Token::Identifier(func_name), _)) = self.tokvec.get(0) {
                self.advance(1);
                expect_token_and_advance!(
                    self,
                    Token::LeftParen,
                    "an opening parenthesis after the name of the function"
                );
                if matches!(self.tokvec.get(0), Some((Token::RightParen,_))) {
                    self.advance(1);
                    let block = self.parse_block();
                    CrowbarFuncDef {
                        func_name: func_name.clone(),
                        params: vec![],
                        block,
                    }
                } else {
                    let params = self.parse_parameter_list();
                    expect_token_and_advance!(
                        self,
                        Token::RightParen,
                        "a closing parenthesis after the name of the function"
                    );
                    let block = self.parse_block();
                    CrowbarFuncDef {
                        func_name: func_name.clone(),
                        params,
                        block,
                    }
                }
            } else {
                panic!("Cannot find an identifier after the keyword `function`");
            }
        } else {
            panic!("This should not be handled by `parse_function_definition`")
        }
    }

    comma_list! {
        parse_parameter_list, parse_identifier, Ident
    }

    comma_list! {
        parse_identifier_list, parse_identifier, Ident
    }

    fn parse_identifier(&mut self) -> Ident {
        match self.tokvec.get(0) {
            Some((Token::Identifier(ident), _)) => {
                self.advance(1);
                ident.clone()
            }
            None => panic!("Unexpected end of file encountered; expected an identifier"),
            Some((unexpected, line_number)) => panic!(
                "Parse error at line {}: Unexpected {:?} encountered; expected an identifier. \nRemaining tokens: \n{:?}",
                line_number, unexpected, self.tokvec
            ),
        }
    }

    fn parse_statement(&mut self) -> Statement {
        match self.tokvec.get(0) {
            Some((Token::Global, _)) => {
                self.advance(1);
                let idents = self.parse_identifier_list();
                expect_token_and_advance!(
                    self,
                    Token::Semicolon,
                    "semicolon at the end of `global` statement"
                );
                Statement::Global(idents)
            }
            Some((Token::While, _)) => {
                self.advance(1);
                expect_token_and_advance!(
                    self,
                    Token::LeftParen,
                    "an opening parenthesis after `while`"
                );
                let expr = self.parse_expression();
                expect_token_and_advance!(
                    self,
                    Token::RightParen,
                    "a closing parenthesis of a `while` statement"
                );
                let block = self.parse_block();
                Statement::While(expr, block)
            }
            Some((Token::Continue, _)) => {
                self.advance(1);
                expect_token_and_advance!(
                    self,
                    Token::Semicolon,
                    "semicolon at the end of `continue` statement"
                );
                Statement::Continue
            }
            Some((Token::Break, _)) => {
                self.advance(1);
                expect_token_and_advance!(
                    self,
                    Token::Semicolon,
                    "semicolon at the end of `break` statement"
                );
                Statement::Break
            }
            Some((Token::Return, _)) => {
                self.advance(1);
                return Statement::Return(parse_optional_expression_and_a_token!(
                    self,
                    Token::Semicolon,
                    "semicolon at the end of `return` statement"
                ));
            }
            Some((Token::For, _)) => {
                self.advance(1);
                expect_token_and_advance!(
                    self,
                    Token::LeftParen,
                    "an opening parenthesis after `for`"
                );
                let expr1 = parse_optional_expression_and_a_token!(
                    self,
                    Token::Semicolon,
                    "semicolon after the first argument to `for`"
                );
                let expr2 = parse_optional_expression_and_a_token!(
                    self,
                    Token::Semicolon,
                    "semicolon after the second argument to `for`"
                );
                let expr3 = parse_optional_expression_and_a_token!(
                    self,
                    Token::RightParen,
                    "a closing parenthesis after the third argument to `for`"
                );
                let block = self.parse_block();
                Statement::For(expr1, expr2, expr3, block)
            }
            Some((Token::If, _)) => {
                self.advance(1);
                expect_token_and_advance!(
                    self,
                    Token::LeftParen,
                    "an opening parenthesis after `if`"
                );
                let if_expr = self.parse_expression();
                expect_token_and_advance!(
                    self,
                    Token::RightParen,
                    "a closing parenthesis of a `if` statement"
                );
                let if_block = self.parse_block();

                // Four candidates:
                // (none)
                // else {}
                // elsif_list
                // elsif_list else {}

                let mut elsif_list = vec![];
                loop {
                    if matches!(self.tokvec.get(0), Some((Token::Elsif,_))) {
                        self.advance(1);
                        expect_token_and_advance!(
                            self,
                            Token::LeftParen,
                            "an opening parenthesis after `elsif`"
                        );
                        let expr = self.parse_expression();
                        expect_token_and_advance!(
                            self,
                            Token::RightParen,
                            "a closing parenthesis for an `elsif`"
                        );
                        let block = self.parse_block();
                        elsif_list.push((expr, block));
                    } else {
                        break;
                    }
                }

                if matches!(self.tokvec.get(0), Some((Token::Else,_))) {
                    self.advance(1);
                    let else_block = self.parse_block();
                    return Statement::If {
                        if_expr,
                        if_block,
                        elsif_list,
                        else_block: Some(else_block),
                    };
                }

                Statement::If {
                    if_expr,
                    if_block,
                    elsif_list,
                    else_block: None,
                }
            }
            Some((Token::Function, _)) => panic!("This should not be handled by parse_statement"),
            _ => {
                let expr = parse_optional_expression_and_a_token!(
                    self,
                    Token::Semicolon,
                    "a semicolon after expression"
                );

                Statement::Expression(expr)
            }
        }
    }

    fn parse_block(&mut self) -> Block {
        expect_token_and_advance!(
            self,
            Token::LeftCurly,
            "an opening bracket starting a block"
        );
        if matches!(self.tokvec.get(0), Some((Token::RightCurly,_))) {
            self.advance(1);
            return Block(vec![]);
        }
        let mut ans = vec![];
        loop {
            if matches!(self.tokvec.get(0), Some((Token::RightCurly,_))) {
                self.advance(1);
                return Block(ans);
            }
            ans.push(self.parse_statement());
        }
    }

    pub fn parse_expression(&mut self) -> Expr {
        if let Some((Token::Identifier(ident),_)) = self.tokvec.get(0) {
            if matches!(self.tokvec.get(1), Some((Token::Assign, _))) {
                self.advance(2);
                let expr2 = self.parse_expression();
                return Expr::Assign(ident.clone(), Box::new(expr2));
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
        |t: &Token| match *t {
            Token::GreaterThan => Expr::GreaterThan,
            Token::GreaterThanOrEqual => Expr::GreaterThanOrEqual,
            Token::LessThan => Expr::LessThan,
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
        |t: &Token| match t {
            Token::Mul => Expr::Mul,
            Token::Div => Expr::Div,
            _ => Expr::Mod,
        }
    }

    fn parse_unary_expression(&mut self) -> Expr {
        if matches!(self.tokvec.get(0), Some((Token::Sub,_))) {
            self.advance(1);
            let expr2 = self.parse_unary_expression();
            return Expr::Negative(Box::new(expr2));
        }
        self.parse_primary_expression()
    }

    fn parse_primary_expression(&mut self) -> Expr {
        match self.tokvec.get(0) {
            Some((Token::Null, _)) => {
                self.advance(1);
                Expr::Null
            }
            Some((Token::False, _)) => {
                self.advance(1);
                Expr::False
            }
            Some((Token::True, _)) => {
                self.advance(1);
                Expr::True
            }
            Some((Token::StringLiteral(s), _)) => {
                self.advance(1);
                Expr::StringLiteral(s.clone())
            }
            Some((Token::DoubleLiteral(d), _)) => {
                self.advance(1);
                Expr::DoubleLiteral(*d)
            }
            Some((Token::IntLiteral(i), _)) => {
                self.advance(1);
                Expr::IntLiteral(*i)
            }
            Some((Token::LeftParen, _)) => {
                self.advance(1);
                let expr = self.parse_expression();
                expect_token_and_advance!(self, Token::RightParen, "right paren");
                expr
            }
            Some((Token::Identifier(ident), _)) => {
                self.advance(1);
                if matches!(self.tokvec.get(0), Some((Token::LeftParen,_))) {
                    self.advance(1);
                    if matches!(self.tokvec.get(0), Some((Token::RightParen,_))) {
                        self.advance(1);
                        Expr::FunctionCall(ident.clone(), vec![])
                    } else {
                        let exprs = self.parse_argument_list();
                        expect_token_and_advance!(
                            self,
                            Token::RightParen,
                            "right paren of a function call"
                        );
                        Expr::FunctionCall(ident.clone(), exprs)
                    }
                } else {
                    // ident
                    Expr::Identifier(ident.clone())
                }
            }
            None => {
                panic!("Unexpected end of file encountered while trying to parse an expression")
            }
            Some((unexpected, line_number)) => panic!(
                "Parse error at line {}: Unexpected {:?} encountered while trying to parse an expression",
                line_number, unexpected
            ),
        }
    }

    comma_list! {
        parse_argument_list, parse_expression, Expr
    }
}

#[test]
fn test3() {
    use crate::lex::Ident;
    use Expr::{Add, Assign, Identifier, IntLiteral, LessThanOrEqual};
    use Statement::For;
    let src = r#"for (i = 1; i <= 100; i = i + 1) {
}"#;
    let lexed = crate::lex::lex_with_linenumber(src);
    let parsed = crate::parse::statements(&lexed);

    assert_eq!(
        parsed,
        vec![For(
            Some(Assign(Ident::from("i"), Box::new(IntLiteral(1)))),
            Some(LessThanOrEqual(
                Box::new(Identifier(Ident::from("i"))),
                Box::new(IntLiteral(100))
            )),
            Some(Assign(
                Ident::from("i"),
                Box::new(Add(
                    Box::new(Identifier(Ident::from("i"))),
                    Box::new(IntLiteral(1))
                ))
            )),
            Block(vec![])
        )]
    );
}
#[test]
fn test4() {
    use crate::lex::Ident;
    use crate::parse::Expr::{
        Add, Assign, FunctionCall, Identifier, IntLiteral, LessThanOrEqual, StringLiteral,
    };
    use crate::parse::Statement::{Expression, For};
    let src = r#"for (i = 1; i <= 100; i = i + 1) {
        
		print("FizzBuzz\n");
	
}"#;
    let lexed = crate::lex::lex_with_linenumber(src);
    let parsed = crate::parse::statements(&lexed);
    assert_eq!(
        parsed,
        vec![For(
            Some(Assign(Ident::from("i"), Box::new(IntLiteral(1)))),
            Some(LessThanOrEqual(
                Box::new(Identifier(Ident::from("i"))),
                Box::new(IntLiteral(100))
            )),
            Some(Assign(
                Ident::from("i"),
                Box::new(Add(
                    Box::new(Identifier(Ident::from("i"))),
                    Box::new(IntLiteral(1))
                ))
            )),
            crate::parse::Block(vec![Expression(Some(FunctionCall(
                Ident::from("print"),
                vec![StringLiteral("FizzBuzz\n".to_string())]
            )))])
        )]
    );
}
