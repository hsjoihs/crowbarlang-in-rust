use crowbarlang_in_rust::execute::Interpreter;
fn main() {
    let mut interpreter = Interpreter::compile_from_str(
        r#"for (i = 1; i <= 100; i = i + 1) {
	if (i % 15 == 0) {
		print("FizzBuzz\n");
	} elsif (i % 3 == 0) {
		print("Fizz\n");
	} elsif (i % 5 == 0) {
		print("Buzz\n");
	} else {
		print("" + i + "\n");
	}
}"#,
    );

    interpreter.interpret();
    println!("Hello, world!");
}
