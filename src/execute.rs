use crate::lex::Ident;
use crate::parse::Block;
use crate::parse::Expr;
use crate::parse::FuncDef;
use crate::parse::Statement;
#[derive(PartialEq, Debug)]
pub enum StatementResult {
    Normal,
    Return(Option<Value>),
    Break,
    Continue,
}
#[derive(PartialEq, Debug)]
pub enum Value {
    Boolean(bool),
    Int(i32),
    Double(f64),
    String(String),
    NativePointer(NativePointer),
    Null,
}
#[derive(PartialEq, Debug)]
pub struct NativePointer {}

struct Variable {
    name: String,
    value: Value,
}

pub struct InterpreterMutable {
    variables: Vec<Variable>,
    local_environment: Option<Vec<Variable>>,
}

pub struct InterpreterImmutable {
    function_list: Vec<FuncDef>,
    statement_list: Vec<Statement>,
}

pub struct Interpreter {
    mutable: InterpreterMutable,
    immutable: InterpreterImmutable,
}
impl Interpreter {
    pub fn compile(file: std::fs::File) -> std::io::Result<Self> {
        use std::io::prelude::*;
        use std::io::BufReader;
        let mut buf_reader = BufReader::new(file);
        let mut contents = String::new();
        buf_reader.read_to_string(&mut contents)?;
        Ok(Self::compile_from_str(&contents))
    }

    #[must_use]
    pub fn compile_from_str(src: &str) -> Self {
        let lexed = crate::lex::lex(src);
        let parsed = crate::parse::translation_unit(&lexed);
        let mut function_list = vec![];
        let mut statement_list = vec![];
        for funcdef_or_statement in parsed {
            match funcdef_or_statement {
                crate::parse::DefinitionOrStatement::Definition(def) => {
                    function_list.push(def);
                }
                crate::parse::DefinitionOrStatement::Statement(stmt) => {
                    statement_list.push(stmt);
                }
            }
        }

        Self {
            immutable: InterpreterImmutable {
                function_list,
                statement_list,
            },
            mutable: InterpreterMutable {
                variables: vec![],
                local_environment: None,
            },
        }
    }

    pub fn interpret(&mut self) {
        self.add_std_fp();
        self.mutable
            .execute_statement_list(&self.immutable.statement_list);
    }

    fn add_std_fp(&mut self) {
        todo!()
    }
}

impl InterpreterMutable {
    fn eval_expression(&mut self, expr: &Expr) -> Value {
        todo!()
    }

    fn eval_optional_expression(&mut self, expr: &Option<Expr>) -> Option<Value> {
        if let Some(expr) = expr {
            Some(self.eval_expression(expr))
        } else {
            None
        }
    }

    fn execute_statement(&mut self, statement: &Statement) -> StatementResult {
        let mut result = StatementResult::Normal;

        match statement {
            Statement::Expression(expr) => {
                self.eval_optional_expression(expr);
            }
            Statement::Global(idents) => {
                result = self.execute_global_statement(idents);
            }
            Statement::If {
                if_expr,
                if_block,
                elsif_list,
                else_block,
            } => {
                result = self.execute_if_statement(if_expr, if_block, elsif_list, else_block);
            }

            Statement::While(expr, block) => {
                result = self.execute_while_statement(expr, block);
            }
            Statement::Break => {
                result = StatementResult::Break;
            }
            Statement::Continue => {
                result = StatementResult::Continue;
            }
            Statement::For(expr1, expr2, expr3, block) => {
                result = self.execute_for_statement(expr1, expr2, expr3, block);
            }
            Statement::Return(expr) => {
                let value: Option<Value> = self.eval_optional_expression(expr);
                result = StatementResult::Return(value);
            }
        }

        result
    }

    fn execute_statement_list(&mut self, stmts: &[Statement]) -> StatementResult {
        let mut result = StatementResult::Normal;
        for stmt in stmts {
            result = self.execute_statement(stmt);
            if result != StatementResult::Normal {
                return result;
            }
        }
        result
    }

    fn execute_global_statement(&mut self, idents: &[Ident]) -> StatementResult {
        let mut result = StatementResult::Normal;
        match &self.local_environment {
            None => self.throw_runtime_error("`global` statement found in top-level"),
            Some(env) => {
                for ident in idents {
                    todo!()
                }
            }
        }

        result
    }

    fn execute_elsif(
        &mut self,
        elsif_list: &[(Expr, Block)],
        elsif_executed: &mut bool,
    ) -> StatementResult {
        *elsif_executed = false;
        let mut result = StatementResult::Normal;
        for elsif in elsif_list {
            let cond = self.eval_expression(&elsif.0);
            match cond {
                Value::Boolean(false) => {}
                Value::Boolean(true) => {
                    result = self.execute_statement_list(&elsif.1 .0);
                    *elsif_executed = true;
                    if result != StatementResult::Normal {
                        return result;
                    }
                }
                _ => self.throw_runtime_error(
                    "expected a boolean type for the condition of `while`, but it was not.",
                ),
            }
        }
        result
    }

    fn execute_if_statement(
        &mut self,
        if_expr: &Expr,
        if_block: &Block,
        elsif_list: &[(Expr, Block)],
        else_block: &Option<Block>,
    ) -> StatementResult {
        let mut result;
        let cond = self.eval_expression(if_expr);
        match cond {
            Value::Boolean(false) => {
                let mut elsif_executed: bool = false;
                result = self.execute_elsif(elsif_list, &mut elsif_executed);
                if result != StatementResult::Normal {
                    return result;
                }
                if !elsif_executed {
                    if let Some(block) = else_block {
                        result = self.execute_statement_list(&block.0);
                    }
                    return result;
                }
            }
            Value::Boolean(true) => {
                result = self.execute_statement_list(&if_block.0);
            }
            _ => self.throw_runtime_error(
                "expected a boolean type for the condition of `if`, but it was not.",
            ),
        }
        result
    }
    fn execute_while_statement(&mut self, expr: &Expr, block: &Block) -> StatementResult {
        let mut result;
        loop {
            let cond = self.eval_expression(expr);
            match cond {
                Value::Boolean(false) => {
                    result = StatementResult::Normal; // fixes a bug that exists in the original implementation
                    break;
                }
                Value::Boolean(true) => {
                    result = self.execute_statement_list(&block.0);
                    match result {
                        StatementResult::Return(_) => break,
                        StatementResult::Break => {
                            result = StatementResult::Normal;
                            break;
                        }
                        _ => {}
                    }
                }
                _ => self.throw_runtime_error(
                    "expected a boolean type for the condition of `while`, but it was not.",
                ),
            }
        }
        result
    }
    fn execute_for_statement(
        &mut self,
        expr1: &Option<Expr>,
        expr2: &Option<Expr>,
        expr3: &Option<Expr>,
        block: &Block,
    ) -> StatementResult {
        let mut result;
        if let Some(initial_expr) = expr1 {
            let _initial: Value = self.eval_expression(initial_expr);
        }
        loop {
            if let Some(cond_expr) = expr2 {
                let cond = self.eval_expression(cond_expr);
                match cond {
                    Value::Boolean(false) => {
                        result = StatementResult::Normal; // fixes a bug that exists in the original implementation
                        break;
                    }
                    Value::Boolean(true) => {}
                    _ => self.throw_runtime_error(
                        "expected a boolean type for the condition of `for`, but it was not.",
                    ),
                }
            }
            result = self.execute_statement_list(&block.0);

            match result {
                StatementResult::Return(_) => break,
                StatementResult::Break => {
                    result = StatementResult::Normal;
                    break;
                }
                _ => {}
            }

            if let Some(post_expr) = expr3 {
                let _3 = self.eval_expression(post_expr);
            }
        }
        result
    }

    fn throw_runtime_error(&self, msg: &str) -> ! {
        panic!("Runtime error: {}", msg)
    }
}
