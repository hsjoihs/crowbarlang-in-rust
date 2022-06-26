fn main() {
    println!("Hello, world!");
}

#[test]
fn test1() {
    use lex::Ident;

    let src = r#"for (i = 1; i <= 100; i = i + 1) {
	if (i % 15 == 0) {
		print("FizzBuzz\n");
	} elsif (i % 3 == 0) {
		print("Fizz\n");
	} elsif (i % 5 == 0) {
		print("Buzz\n");
	} else {
		print("" + i + "\n");
	}
}"#;
    let lexed = lex::lex(src);
    {
        use lex::Token::*;
        assert_eq!(
            lexed,
            vec![
                For,
                LeftParen,
                Identifier(Ident::from("i")),
                Assign,
                IntLiteral(1),
                Semicolon,
                Identifier(Ident::from("i")),
                LessThanOrEqual,
                IntLiteral(100),
                Semicolon,
                Identifier(Ident::from("i")),
                Assign,
                Identifier(Ident::from("i")),
                Add,
                IntLiteral(1),
                RightParen,
                LeftCurly,
                If,
                LeftParen,
                Identifier(Ident::from("i")),
                Mod,
                IntLiteral(15),
                Equal,
                IntLiteral(0),
                RightParen,
                LeftCurly,
                Identifier(Ident::from("print")),
                LeftParen,
                StringLiteral("FizzBuzz\n".to_string()),
                RightParen,
                Semicolon,
                RightCurly,
                Elsif,
                LeftParen,
                Identifier(Ident::from("i")),
                Mod,
                IntLiteral(3),
                Equal,
                IntLiteral(0),
                RightParen,
                LeftCurly,
                Identifier(Ident::from("print")),
                LeftParen,
                StringLiteral("Fizz\n".to_string()),
                RightParen,
                Semicolon,
                RightCurly,
                Elsif,
                LeftParen,
                Identifier(Ident::from("i")),
                Mod,
                IntLiteral(5),
                Equal,
                IntLiteral(0),
                RightParen,
                LeftCurly,
                Identifier(Ident::from("print")),
                LeftParen,
                StringLiteral("Buzz\n".to_string()),
                RightParen,
                Semicolon,
                RightCurly,
                Else,
                LeftCurly,
                Identifier(Ident::from("print")),
                LeftParen,
                StringLiteral("".to_string()),
                Add,
                Identifier(Ident::from("i")),
                Add,
                StringLiteral("\n".to_string()),
                RightParen,
                Semicolon,
                RightCurly,
                RightCurly
            ]
        );
    }
    {
        use crate::lex::Ident;
        use crate::parse::Expr::*;
        use crate::parse::Statement::*;
        let parsed = crate::parse::parse_statements(&lexed);
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
                crate::parse::Block(vec![If {
                    if_expr: Equal(
                        Box::new(Mod(
                            Box::new(Identifier(Ident::from("i"))),
                            Box::new(IntLiteral(15))
                        )),
                        Box::new(IntLiteral(0))
                    ),
                    if_block: parse::Block(vec![Expression(Some(FunctionCall(
                        Ident::from("print"),
                        vec![StringLiteral("FizzBuzz\n".to_string())]
                    )))]),
                    elsif_list: vec![
                        (
                            Equal(
                                Box::new(Mod(
                                    Box::new(Identifier(Ident::from("i"))),
                                    Box::new(IntLiteral(3))
                                )),
                                Box::new(IntLiteral(0))
                            ),
                            parse::Block(vec![Expression(Some(FunctionCall(
                                Ident::from("print"),
                                vec![StringLiteral("Fizz\n".to_string())]
                            )))])
                        ),
                        (
                            Equal(
                                Box::new(Mod(
                                    Box::new(Identifier(Ident::from("i"))),
                                    Box::new(IntLiteral(5))
                                )),
                                Box::new(IntLiteral(0))
                            ),
                            parse::Block(vec![Expression(Some(FunctionCall(
                                Ident::from("print"),
                                vec![StringLiteral("Buzz\n".to_string())]
                            )))])
                        )
                    ],
                    else_block: Some(parse::Block(vec![Expression(Some(FunctionCall(
                        Ident::from("print"),
                        vec![Add(
                            Box::new(Add(
                                Box::new(StringLiteral("".to_string())),
                                Box::new(Identifier(Ident::from("i")))
                            )),
                            Box::new(StringLiteral("\n".to_string()))
                        )]
                    )))])),
                }])
            )]
        )
    }
}

#[test]
fn test2() {
    use lex::Ident;
    use lex::Token::*;
    let src = r##"
a = 10;

function func() {
	global a;
	a = 20;
}

function func2() {
	a = 30;
	print("a.." + a + "\n");
}

func();
func2();
print("a.." + a + "\n");

b = 3.2;
print("b.." + b + "\n");"##;
    assert_eq!(
        lex::lex(src),
        vec![
            Identifier(Ident::from("a")),
            Assign,
            IntLiteral(10),
            Semicolon,
            Function,
            Identifier(Ident::from("func")),
            LeftParen,
            RightParen,
            LeftCurly,
            GlobalT,
            Identifier(Ident::from("a")),
            Semicolon,
            Identifier(Ident::from("a")),
            Assign,
            IntLiteral(20),
            Semicolon,
            RightCurly,
            Function,
            Identifier(Ident::from("func2")),
            LeftParen,
            RightParen,
            LeftCurly,
            Identifier(Ident::from("a")),
            Assign,
            IntLiteral(30),
            Semicolon,
            Identifier(Ident::from("print")),
            LeftParen,
            StringLiteral("a..".to_string()),
            Add,
            Identifier(Ident::from("a")),
            Add,
            StringLiteral("\n".to_string()),
            RightParen,
            Semicolon,
            RightCurly,
            Identifier(Ident::from("func")),
            LeftParen,
            RightParen,
            Semicolon,
            Identifier(Ident::from("func2")),
            LeftParen,
            RightParen,
            Semicolon,
            Identifier(Ident::from("print")),
            LeftParen,
            StringLiteral("a..".to_string()),
            Add,
            Identifier(Ident::from("a")),
            Add,
            StringLiteral("\n".to_string()),
            RightParen,
            Semicolon,
            Identifier(Ident::from("b")),
            Assign,
            DoubleLiteral(3.2),
            Semicolon,
            Identifier(Ident::from("print")),
            LeftParen,
            StringLiteral("b..".to_string()),
            Add,
            Identifier(Ident::from("b")),
            Add,
            StringLiteral("\n".to_string()),
            RightParen,
            Semicolon
        ]
    );
}

pub mod lex;
pub mod parse;
