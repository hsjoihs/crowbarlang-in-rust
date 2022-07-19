#[test]
fn test1() {
    use crate::lex::Ident;

    let src = include_str!("../testcases/fizzbuzz.crb");
    let lexed = crate::lex::lex(src);
    {
        // lex
        use crate::lex::Token::{
            Add, Assign, Else, Elsif, Equal, For, Identifier, If, IntLiteral, LeftCurly, LeftParen,
            LessThanOrEqual, Mod, RightCurly, RightParen, Semicolon, StringLiteral,
        };
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
        // parse
        use crate::lex::Ident;
        use crate::parse::Expr::{
            Add, Assign, Equal, FunctionCall, Identifier, IntLiteral, LessThanOrEqual, Mod,
            StringLiteral,
        };
        use crate::parse::Statement::{Expression, For, If};
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
                crate::parse::Block(vec![If {
                    if_expr: Equal(
                        Box::new(Mod(
                            Box::new(Identifier(Ident::from("i"))),
                            Box::new(IntLiteral(15))
                        )),
                        Box::new(IntLiteral(0))
                    ),
                    if_block: crate::parse::Block(vec![Expression(Some(FunctionCall(
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
                            crate::parse::Block(vec![Expression(Some(FunctionCall(
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
                            crate::parse::Block(vec![Expression(Some(FunctionCall(
                                Ident::from("print"),
                                vec![StringLiteral("Buzz\n".to_string())]
                            )))])
                        )
                    ],
                    else_block: Some(crate::parse::Block(vec![Expression(Some(FunctionCall(
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
        );
    }
}

#[test]
fn test2() {
    use crate::lex::Ident;
    let src = include_str!("../testcases/global_versus_local.crb");
    let lexed = crate::lex::lex(src);
    {
        use crate::lex::Token::{
            Add, Assign, DoubleLiteral, Function, Global, Identifier, IntLiteral, LeftCurly,
            LeftParen, RightCurly, RightParen, Semicolon, StringLiteral,
        };
        assert_eq!(
            lexed,
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
                Global,
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
    let lexed = crate::lex::lex_with_linenumber(src);
    let parsed = crate::parse::translation_unit(&lexed);
    {
        use crate::lex::Ident;
        use crate::parse::CrowbarFuncDef;
        use crate::parse::DefinitionOrStatement::{Definition, Statement};
        use crate::parse::Expr::{
            Add, Assign, DoubleLiteral, FunctionCall, Identifier, IntLiteral, StringLiteral,
        };
        use crate::parse::Statement::{Expression, Global};
        assert_eq!(
            parsed,
            vec![
                Statement(Expression(Some(Assign(
                    Box::new(Identifier(Ident::from("a"))),
                    Box::new(IntLiteral(10))
                )))),
                Definition(CrowbarFuncDef {
                    func_name: Ident::from("func"),
                    params: vec![],
                    block: crate::parse::Block(vec![
                        Global(vec![Ident::from("a")]),
                        Expression(Some(Assign(
                            Box::new(Identifier(Ident::from("a"))),
                            Box::new(IntLiteral(20))
                        )))
                    ])
                }),
                Definition(CrowbarFuncDef {
                    func_name: Ident::from("func2"),
                    params: vec![],
                    block: crate::parse::Block(vec![
                        Expression(Some(Assign(
                            Box::new(Identifier(Ident::from("a"))),
                            Box::new(IntLiteral(30))
                        ))),
                        Expression(Some(FunctionCall(
                            Ident::from("print"),
                            vec![Add(
                                Box::new(Add(
                                    Box::new(StringLiteral("a..".to_string())),
                                    Box::new(Identifier(Ident::from("a")))
                                )),
                                Box::new(StringLiteral("\n".to_string()))
                            )]
                        )))
                    ])
                }),
                Statement(Expression(Some(FunctionCall(Ident::from("func"), vec![])))),
                Statement(Expression(Some(FunctionCall(Ident::from("func2"), vec![])))),
                Statement(Expression(Some(FunctionCall(
                    Ident::from("print"),
                    vec![Add(
                        Box::new(Add(
                            Box::new(StringLiteral("a..".to_string())),
                            Box::new(Identifier(Ident::from("a")))
                        )),
                        Box::new(StringLiteral("\n".to_string()))
                    )]
                )))),
                Statement(Expression(Some(Assign(
                    Box::new(Identifier(Ident::from("b"))),
                    Box::new(DoubleLiteral(3.2))
                )))),
                Statement(Expression(Some(FunctionCall(
                    Ident::from("print"),
                    vec![Add(
                        Box::new(Add(
                            Box::new(StringLiteral("b..".to_string())),
                            Box::new(Identifier(Ident::from("b")))
                        )),
                        Box::new(StringLiteral("\n".to_string()))
                    )]
                ))))
            ]
        );
    }
}

#[test]
fn test3() {
    use crate::lex::Ident;
    let src = include_str!("../testcases/arr0.crb");
    let lexed = crate::lex::lex(src);
    {
        use crate::lex::Token::{
            Add, Assign, Comma, Dot, For, Identifier, IntLiteral, LeftBracket, LeftCurly,
            LeftParen, LessThan, RightBracket, RightCurly, RightParen, Semicolon, StringLiteral,
        };
        assert_eq!(
            lexed,
            vec![
                Identifier(Ident::from("a")),
                Assign,
                LeftCurly,
                IntLiteral(1),
                Comma,
                IntLiteral(2),
                Comma,
                IntLiteral(3),
                Comma,
                IntLiteral(4),
                Comma,
                IntLiteral(5),
                Comma,
                IntLiteral(6),
                Comma,
                IntLiteral(7),
                Comma,
                IntLiteral(8),
                RightCurly,
                Semicolon,
                For,
                LeftParen,
                Identifier(Ident::from("i")),
                Assign,
                IntLiteral(0),
                Semicolon,
                Identifier(Ident::from("i")),
                LessThan,
                Identifier(Ident::from("a")),
                Dot,
                Identifier(Ident::from("size")),
                LeftParen,
                RightParen,
                Semicolon,
                Identifier(Ident::from("i")),
                Assign,
                Identifier(Ident::from("i")),
                Add,
                IntLiteral(1),
                RightParen,
                LeftCurly,
                Identifier(Ident::from("print")),
                LeftParen,
                StringLiteral("(".to_string()),
                Add,
                Identifier(Ident::from("a")),
                LeftBracket,
                Identifier(Ident::from("i")),
                RightBracket,
                Add,
                StringLiteral(")".to_string()),
                RightParen,
                Semicolon,
                RightCurly,
                Identifier(Ident::from("print")),
                LeftParen,
                Identifier(Ident::from("a")),
                RightParen,
                Semicolon,
                Identifier(Ident::from("print")),
                LeftParen,
                StringLiteral("\n".to_string()),
                RightParen,
                Semicolon,
                Identifier(Ident::from("print")),
                LeftParen,
                StringLiteral("len..".to_string()),
                Add,
                StringLiteral("abc".to_string()),
                Dot,
                Identifier(Ident::from("length")),
                LeftParen,
                RightParen,
                Add,
                StringLiteral("\n".to_string()),
                RightParen,
                Semicolon
            ]
        );
    }
    let lexed = crate::lex::lex_with_linenumber(src);
    let parsed = crate::parse::translation_unit(&lexed);
    {
        use crate::lex::Ident;
        use crate::parse::Block;
        use crate::parse::DefinitionOrStatement::Statement;
        use crate::parse::Expr::{
            Add, ArrayLiteral, Assign, FunctionCall, Identifier, IndexAccess, IntLiteral, LessThan,
            MethodCall, StringLiteral,
        };
        use crate::parse::Statement::{Expression, For};
        assert_eq!(
            parsed,
            vec![
                Statement(Expression(Some(Assign(
                    Box::new(Identifier(Ident::from("a"))),
                    Box::new(ArrayLiteral(vec![
                        IntLiteral(1),
                        IntLiteral(2),
                        IntLiteral(3),
                        IntLiteral(4),
                        IntLiteral(5),
                        IntLiteral(6),
                        IntLiteral(7),
                        IntLiteral(8)
                    ]))
                )))),
                Statement(For(
                    Some(Assign(
                        Box::new(Identifier(Ident::from("i"))),
                        Box::new(IntLiteral(0))
                    )),
                    Some(LessThan(
                        Box::new(Identifier(Ident::from("i"))),
                        Box::new(MethodCall {
                            receiver: Box::new(Identifier(Ident::from("a"))),
                            method_name: Ident::from("size"),
                            args: vec![]
                        })
                    )),
                    Some(Assign(
                        Box::new(Identifier(Ident::from("i"))),
                        Box::new(Add(
                            Box::new(Identifier(Ident::from("i"))),
                            Box::new(IntLiteral(1))
                        ))
                    )),
                    Block(vec![Expression(Some(FunctionCall(
                        Ident::from("print"),
                        vec![Add(
                            Box::new(Add(
                                Box::new(StringLiteral("(".to_string())),
                                Box::new(IndexAccess {
                                    array: Box::new(Identifier(Ident::from("a"))),
                                    index: Box::new(Identifier(Ident::from("i")))
                                })
                            )),
                            Box::new(StringLiteral(")".to_string()))
                        )]
                    )))])
                )),
                Statement(Expression(Some(FunctionCall(
                    Ident::from("print"),
                    vec![Identifier(Ident::from("a"))]
                )))),
                Statement(Expression(Some(FunctionCall(
                    Ident::from("print"),
                    vec![StringLiteral("\n".to_string())]
                )))),
                Statement(Expression(Some(FunctionCall(
                    Ident::from("print"),
                    vec![Add(
                        Box::new(Add(
                            Box::new(StringLiteral("len..".to_string())),
                            Box::new(MethodCall {
                                receiver: Box::new(StringLiteral("abc".to_string())),
                                method_name: Ident::from("length"),
                                args: vec![]
                            })
                        )),
                        Box::new(StringLiteral("\n".to_string()))
                    )]
                ))))
            ]
        );
    }
}
