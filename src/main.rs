use crowbarlang_in_rust::execute::Interpreter;
use std::env;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    let fizzbuzz = std::fs::File::open(if args.len() >= 2 {
        &args[1]
    } else {
        "testcases/fizzbuzz.crb"
    })?;
    let mut interpreter = Interpreter::compile(fizzbuzz)?;
    interpreter.interpret();
    Ok(())
}
