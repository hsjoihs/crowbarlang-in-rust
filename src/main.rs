use crowbarlang_in_rust::execute::Interpreter;
fn main() -> std::io::Result<()> {
    let fizzbuzz = std::fs::File::open("global_versus_local.crowbar")?;
    let mut interpreter = Interpreter::compile(fizzbuzz)?;
    interpreter.interpret();
    Ok(())
}
