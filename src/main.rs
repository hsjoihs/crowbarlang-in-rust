fn main() {
    println!("Hello, world!");
}

#[test]
fn test1() {
    use lex::Token::*;
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
    assert_eq!(
        lex::lex(src),
        vec![
            For,
            LeftParen,
            Identifier("i".to_string()),
            Assign,
            IntLiteral(1),
            Semicolon,
            Identifier("i".to_string()),
            LessThanOrEqual,
            IntLiteral(100),
            Semicolon,
            Identifier("i".to_string()),
            Assign,
            Identifier("i".to_string()),
            Add,
            IntLiteral(1),
            RightParen,
            LeftCurly,
            If,
            LeftParen,
            Identifier("i".to_string()),
            Mod,
            IntLiteral(15),
            Equal,
            IntLiteral(0),
            RightParen,
            LeftCurly,
            Identifier("print".to_string()),
            LeftParen,
            StringLiteral("FizzBuzz\n".to_string()),
            RightParen,
            Semicolon,
            RightCurly,
            Elsif,
            LeftParen,
            Identifier("i".to_string()),
            Mod,
            IntLiteral(3),
            Equal,
            IntLiteral(0),
            RightParen,
            LeftCurly,
            Identifier("print".to_string()),
            LeftParen,
            StringLiteral("Fizz\n".to_string()),
            RightParen,
            Semicolon,
            RightCurly,
            Elsif,
            LeftParen,
            Identifier("i".to_string()),
            Mod,
            IntLiteral(5),
            Equal,
            IntLiteral(0),
            RightParen,
            LeftCurly,
            Identifier("print".to_string()),
            LeftParen,
            StringLiteral("Buzz\n".to_string()),
            RightParen,
            Semicolon,
            RightCurly,
            Else,
            LeftCurly,
            Identifier("print".to_string()),
            LeftParen,
            StringLiteral("".to_string()),
            Add,
            Identifier("i".to_string()),
            Add,
            StringLiteral("\n".to_string()),
            RightParen,
            Semicolon,
            RightCurly,
            RightCurly
        ]
    );
}

pub mod lex;
