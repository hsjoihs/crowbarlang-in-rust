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

#[test]
fn test2() {
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
            Identifier("a".to_string()),
            Assign,
            IntLiteral(10),
            Semicolon,
            Function,
            Identifier("func".to_string()),
            LeftParen,
            RightParen,
            LeftCurly,
            GlobalT,
            Identifier("a".to_string()),
            Semicolon,
            Identifier("a".to_string()),
            Assign,
            IntLiteral(20),
            Semicolon,
            RightCurly,
            Function,
            Identifier("func2".to_string()),
            LeftParen,
            RightParen,
            LeftCurly,
            Identifier("a".to_string()),
            Assign,
            IntLiteral(30),
            Semicolon,
            Identifier("print".to_string()),
            LeftParen,
            StringLiteral("a..".to_string()),
            Add,
            Identifier("a".to_string()),
            Add,
            StringLiteral("\n".to_string()),
            RightParen,
            Semicolon,
            RightCurly,
            Identifier("func".to_string()),
            LeftParen,
            RightParen,
            Semicolon,
            Identifier("func2".to_string()),
            LeftParen,
            RightParen,
            Semicolon,
            Identifier("print".to_string()),
            LeftParen,
            StringLiteral("a..".to_string()),
            Add,
            Identifier("a".to_string()),
            Add,
            StringLiteral("\n".to_string()),
            RightParen,
            Semicolon,
            Identifier("b".to_string()),
            Assign,
            DoubleLiteral(3.2),
            Semicolon,
            Identifier("print".to_string()),
            LeftParen,
            StringLiteral("b..".to_string()),
            Add,
            Identifier("b".to_string()),
            Add,
            StringLiteral("\n".to_string()),
            RightParen,
            Semicolon
        ]
    );
}

pub mod lex;
pub mod parse;