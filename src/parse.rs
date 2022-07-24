use crate::lex::{Ident, LineNumber, Token};

#[derive(PartialEq, Debug)]
pub enum Expr {
    Assign(Box<Expr>, Box<Expr>),
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
    IndexAccess {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    MethodCall {
        receiver: Box<Expr>,
        method_name: Ident,
        args: Vec<Expr>,
    },
    Increment(Box<Expr>),
    Decrement(Box<Expr>),
    ArrayLiteral(Vec<Expr>),
}

impl Expr {
    #[must_use]
    pub const fn get_operator_string(&self) -> Option<&'static str> {
        match &self {
            Expr::Assign(_, _) => Some("="),
            Expr::Add(_, _) => Some("+"),
            Expr::Sub(_, _) | Expr::Negative(_) => Some("-"),
            Expr::Mul(_, _) => Some("*"),
            Expr::Div(_, _) => Some("/"),
            Expr::Mod(_, _) => Some("%"),
            Expr::Equal(_, _) => Some("=="),
            Expr::NotEqual(_, _) => Some("!="),
            Expr::GreaterThan(_, _) => Some(">"),
            Expr::GreaterThanOrEqual(_, _) => Some(">="),
            Expr::LessThan(_, _) => Some("<"),
            Expr::LessThanOrEqual(_, _) => Some("<="),
            Expr::LogicalAnd(_, _) => Some("&&"),
            Expr::LogicalOr(_, _) => Some("||"),
            Expr::True
            | Expr::False
            | Expr::IntLiteral(_)
            | Expr::DoubleLiteral(_)
            | Expr::StringLiteral(_)
            | Expr::Identifier(_)
            | Expr::FunctionCall(_, _)
            | Expr::MethodCall { .. }
            | Expr::Null
            | Expr::ArrayLiteral(_)
            | Expr::IndexAccess { .. }
            | Expr::Increment(_)
            | Expr::Decrement(_) => None,
        }
    }
}

struct ParserState<'a> {
    tokvec: &'a [(Token, LineNumber)],
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
                line_number.0, unexpected, $expected
            ),
        }
    };
}

#[test]
fn test_parse_expression() {
    use crate::lex::Ident;
    assert_eq!(
        crate::parse::expression(&[
            (Token::Identifier(Ident::from("i")), LineNumber(0)),
            (Token::Mod, LineNumber(0)),
            (Token::IntLiteral(15), LineNumber(0)),
            (Token::Equal, LineNumber(0)),
            (Token::IntLiteral(0), LineNumber(0)),
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
        (Token::Identifier(Ident::from("print")), LineNumber(0)),
        (Token::LeftParen, LineNumber(0)),
        (
            Token::StringLiteral("FizzBuzz\n".to_string()),
            LineNumber(0),
        ),
        (Token::RightParen, LineNumber(0)),
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
pub fn expression(tokvec: &[(Token, LineNumber)]) -> Expr {
    let mut state = ParserState::new(tokvec);
    state.parse_expression()
}

#[must_use]
pub fn statement(tokvec: &[(Token, LineNumber)]) -> Statement {
    let mut state = ParserState::new(tokvec);
    state.parse_statement()
}

#[must_use]
pub fn translation_unit(tokvec: &[(Token, LineNumber)]) -> Vec<DefinitionOrStatement> {
    let mut state = ParserState::new(tokvec);
    state.parse_translation_unit()
}

#[must_use]
pub fn statements(tokvec: &[(Token, LineNumber)]) -> Vec<Statement> {
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
        if matches!($self.tokvec.get(0), Some(($token, _))) {
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
    pub const fn new(tokvec: &'a [(Token, LineNumber)]) -> Self {
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
        if matches!(self.tokvec.get(0), Some((Token::Function, _))) {
            let func_def = self.parse_function_definition();
            DefinitionOrStatement::Definition(func_def)
        } else {
            let statement = self.parse_statement();
            DefinitionOrStatement::Statement(statement)
        }
    }

    fn parse_function_definition(&mut self) -> CrowbarFuncDef {
        if matches!(self.tokvec.get(0), Some((Token::Function, _))) {
            self.advance(1);
            if let Some((Token::Identifier(func_name), _)) = self.tokvec.get(0) {
                self.advance(1);
                expect_token_and_advance!(
                    self,
                    Token::LeftParen,
                    "an opening parenthesis after the name of the function"
                );
                if matches!(self.tokvec.get(0), Some((Token::RightParen, _))) {
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
                line_number.0, unexpected, self.tokvec
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
                    if matches!(self.tokvec.get(0), Some((Token::Elsif, _))) {
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

                if matches!(self.tokvec.get(0), Some((Token::Else, _))) {
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
        if matches!(self.tokvec.get(0), Some((Token::RightCurly, _))) {
            self.advance(1);
            return Block(vec![]);
        }
        let mut ans = vec![];
        loop {
            if matches!(self.tokvec.get(0), Some((Token::RightCurly, _))) {
                self.advance(1);
                return Block(ans);
            }
            ans.push(self.parse_statement());
        }
    }

    pub fn parse_expression(&mut self) -> Expr {
        // expression
        //     : logical_or_expression
        //     | postfix_expression ASSIGN expression

        // First, try parsing a postfix expression and see if we get a Token::Assign
        let mut new_parser = ParserState {
            tokvec: self.tokvec,
        };
        let expr = new_parser.parse_postfix_expression();
        if matches!(new_parser.tokvec.get(0), Some((Token::Assign, _))) {
            new_parser.advance(1);
            let expr2 = new_parser.parse_expression();
            std::mem::swap(self, &mut new_parser);
            return Expr::Assign(Box::new(expr), Box::new(expr2));
        }

        // If we don't encounter a Token::Assign, we forget about the `new_parser` and continue parsing
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
        if matches!(self.tokvec.get(0), Some((Token::Sub, _))) {
            self.advance(1);
            let expr2 = self.parse_unary_expression();
            return Expr::Negative(Box::new(expr2));
        }
        self.parse_postfix_expression()
    }

    fn parse_postfix_expression(&mut self) -> Expr {
        let mut expr = self.parse_primary_expression();
        loop {
            match self.tokvec.get(0) {
                Some((Token::LeftBracket, _)) => {
                    self.advance(1);
                    let index = self.parse_expression();
                    expect_token_and_advance!(self, Token::RightBracket, "right bracket");
                    expr = Expr::IndexAccess {
                        array: Box::new(expr),
                        index: Box::new(index),
                    };
                }
                Some((Token::Dot, _)) => {
                    self.advance(1);
                    if let Some((Token::Identifier(method_name), _)) = self.tokvec.get(0) {
                        self.advance(1);
                        expect_token_and_advance!(
                            self,
                            Token::LeftParen,
                            "left parenthesis of method call"
                        );
                        if matches!(self.tokvec.get(0), Some((Token::RightParen, _))) {
                            self.advance(1);
                            expr = Expr::MethodCall {
                                receiver: Box::new(expr),
                                method_name: method_name.clone(),
                                args: vec![],
                            };
                        } else {
                            let args = self.parse_argument_list();
                            expect_token_and_advance!(
                                self,
                                Token::RightParen,
                                "right paren of a method call"
                            );
                            expr = Expr::MethodCall {
                                receiver: Box::new(expr),
                                method_name: method_name.clone(),
                                args,
                            };
                        }
                    } else {
                        panic!("Expected a method name after `.` but did not find one.")
                    }
                }
                Some((Token::Increment, _)) => {
                    self.advance(1);
                    expr = Expr::Increment(Box::new(expr));
                }
                Some((Token::Decrement, _)) => {
                    self.advance(1);
                    expr = Expr::Decrement(Box::new(expr));
                }
                _ => break,
            }
        }
        expr
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
            Some((Token::LeftCurly, line_number)) => {
                // Unlike function arguments, a single trailing comma is allowed.
                // The original implementation also allows comma at the very beginning,
                // but since it segfaults on such an input, I consider that to be a bug.
                self.advance(1);

                let mut ans = vec![];
                loop {
                    if matches!(self.tokvec.get(0), Some((Token::RightCurly, _))) {
                        self.advance(1);
                        break;
                    }
                    ans.push(self.parse_expression());

                    match self.tokvec.get(0) {
                        Some((Token::Comma, _)) => {
                            self.advance(1);
                            continue;
                        }
                        Some((Token::RightCurly, _)) => {
                            self.advance(1);
                            break;
                        }
                        None => panic!("Unexpected end of file encountered while trying to parse an array literal"),
                        _ => panic!("Parse error at line {}: Incorrect array literal", line_number.0)
                    }
                }
                Expr::ArrayLiteral(ans)
            }
            Some((unexpected, line_number)) => panic!(
                "Parse error at line {}: Unexpected {:?} encountered while trying to parse an expression",
                line_number.0, unexpected
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
            Some(Assign(
                Box::new(Identifier(Ident::from("i"))),
                Box::new(IntLiteral(1))
            )),
            Some(LessThanOrEqual(
                Box::new(Identifier(Ident::from("i"))),
                Box::new(IntLiteral(100))
            )),
            Some(Assign(
                Box::new(Identifier(Ident::from("i"))),
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
            Some(Assign(
                Box::new(Identifier(Ident::from("i"))),
                Box::new(IntLiteral(1))
            )),
            Some(LessThanOrEqual(
                Box::new(Identifier(Ident::from("i"))),
                Box::new(IntLiteral(100))
            )),
            Some(Assign(
                Box::new(Identifier(Ident::from("i"))),
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
