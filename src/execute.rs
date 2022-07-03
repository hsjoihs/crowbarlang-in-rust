use crate::lex::Ident;
use crate::parse::Block;
use crate::parse::CrowbarFuncDef;
use crate::parse::Expr;
use crate::parse::Statement;
#[derive(PartialEq, Debug)]
pub enum StatementResult {
    Normal,
    Return(Option<Value>),
    Break,
    Continue,
}
#[derive(PartialEq, Debug, Clone)]
pub enum Value {
    Boolean(bool),
    Int(i32),
    Double(f64),
    String(String),
    NativePointer(NativePointer),
    Null,
}
#[derive(PartialEq, Debug, Clone)]
pub enum NativePointer {
    FilePointer(FilePointer),
}
#[derive(PartialEq, Debug, Clone)]
pub enum FilePointer {
    RawPointer(*mut libc::FILE),
    StdIn,
    StdOut,
    StdErr,
}

#[derive(Clone, Debug, PartialEq)]
struct Variable {
    name: Ident,
    value: Value,
}

pub struct MutableEnvironment {
    local_environment: Option<LocalEnvironment>,
    global_variables: Vec<Variable>,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct LocalEnvironment {
    local_variables: Vec<Variable>,
    global_variables_visible_from_local: Vec<Ident>,
}

pub struct InterpreterImmutable {
    function_list: Vec<FuncDef>,
    statement_list: Vec<Statement>,
}

type NativeFuncContent = Box<dyn Fn(&[Value]) -> Value>;

pub enum FuncDef {
    Crowbar(CrowbarFuncDef),
    Native(NativeFuncDef),
}

pub struct NativeFuncDef {
    pub func_name: Ident,
    pub content: NativeFuncContent,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Int(b) => write!(f, "{}", b),
            Value::Double(b) => write!(f, "{:.6}", b),
            Value::String(b) => write!(f, "{}", b),
            Value::NativePointer(b) => write!(f, "(NativePointer:{:?})", b),
            Value::Null => write!(f, "null"),
        }
    }
}

pub struct Interpreter {
    mutable: MutableEnvironment,
    src: InterpreterImmutable,
}
impl Interpreter {
    /// # Errors
    /// Gives out an IO error when it fails to read from the file
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
        let lexed = crate::lex::lex_with_linenumber(src);
        let parsed = crate::parse::translation_unit(&lexed);
        let mut function_list = vec![];
        let mut statement_list = vec![];
        for funcdef_or_statement in parsed {
            match funcdef_or_statement {
                crate::parse::DefinitionOrStatement::Definition(def) => {
                    function_list.push(FuncDef::Crowbar(def));
                }
                crate::parse::DefinitionOrStatement::Statement(stmt) => {
                    statement_list.push(stmt);
                }
            }
        }

        Self {
            src: InterpreterImmutable {
                function_list,
                statement_list,
            },
            mutable: MutableEnvironment {
                global_variables: vec![],
                local_environment: None,
            },
        }
    }

    pub fn interpret(&mut self) {
        self.add_std_file_pointers();
        self.add_std_native_functions();
        self.mutable
            .execute_statement_list(&self.src.function_list, &self.src.statement_list);
    }

    fn add_std_native_functions(&mut self) {
        self.add_native_function(
            "print",
            Box::new(|args| {
                match args {
                    [arg] => print!("{}", arg),
                    [] => panic!(
                        "Expected 1 argument to the native function `print`, but got 0 arguments.",
                    ),
                    [_, _, ..] => panic!(
                        "Expected 1 argument to the native function `print`, but got {} arguments.",
                        args.len()
                    ),
                };
                Value::Null
            }),
        );
        self.add_native_function(
            "fopen",
            Box::new(|args| match args {
                [Value::String(filename), Value::String(mode)] => {
                    use std::ffi::CString;
                    let filename = CString::new(filename as &str).unwrap();
                    let mode = CString::new(mode as &str).unwrap();
                    let fp = unsafe { libc::fopen(filename.as_ptr(), mode.as_ptr()) };
                    Value::NativePointer(NativePointer::FilePointer(FilePointer::RawPointer(fp)))
                }
                [_, Value::String(_)] => panic!("The first argument to fopen() is not a string"),
                [Value::String(_), _] => panic!("The second argument to fopen() is not a string"),
                [_, _] => panic!("The first and the second argument to fopen() are not strings"),
                _ => panic!(
                    "Expected 2 arguments to the native function `fopen`, but got {} arguments.",
                    args.len()
                ),
            }),
        );
        self.add_native_function(
            "fclose",
            Box::new(|args| match args {
                [Value::NativePointer(NativePointer::FilePointer(fp))] => {
                    match fp {
                        FilePointer::RawPointer(fp) => unsafe {
                            libc::fclose(*fp);
                        },

                        // It is an undefined behavior to use stdin, stdout or stderr
                        // after when they are closed.
                        // That means it's simply fine to ignore an attempt to call
                        // `fclose()` on them.
                        FilePointer::StdIn | FilePointer::StdOut | FilePointer::StdErr => {
                            /* do nothing */
                        }
                    }
                    Value::Null
                }
                [_] => panic!("The first argument to fclose() is not a file pointer"),
                _ => panic!(
                    "Expected 1 argument to the native function `fclose`, but got {} arguments.",
                    args.len()
                ),
            }),
        );

        self.add_native_function(
            "fgets",
            Box::new(|args| match args {
                [arg @ Value::NativePointer(NativePointer::FilePointer(fp))] => match fp {
                    FilePointer::RawPointer(fp) => {
                        let mut ret_buf: Vec<u8> = Vec::new();
                        let mut buf: [std::os::raw::c_char; 1024] = [0; 1024];

                        unsafe {
                            while !libc::fgets(std::ptr::addr_of_mut!(buf[0]), 1024, *fp).is_null()
                            {
                                let len = libc::strlen(std::ptr::addr_of_mut!(buf[0]));
                                for c in buf.iter().take(len) {
                                    #[allow(clippy::cast_sign_loss)]
                                    ret_buf.push(*c as u8);
                                }
                                if ret_buf.last() == Some(&b'\n') {
                                    break;
                                }
                            }
                        }

                        let str = std::ffi::CString::new(ret_buf)
                            .unwrap()
                            .to_string_lossy()
                            .to_string();

                        if str.is_empty() {
                            Value::Null
                        } else {
                            Value::String(str)
                        }
                    }

                    FilePointer::StdOut | FilePointer::StdErr => {
                        panic!("Cannot read from a write-only file pointer `{}`", arg);
                    }

                    FilePointer::StdIn => {
                        use std::io::BufRead;
                        let mut line = String::new();
                        let stdin = std::io::stdin();
                        let mut stdinlock = stdin.lock();
                        match stdinlock.read_line(&mut line).unwrap() {
                            0 => Value::Null,
                            _ => Value::String(line),
                        }
                    }
                },
                [_] => panic!("The first argument to fgets() is not a file pointer"),
                _ => panic!(
                    "Expected 1 argument to the native function `fgets`, but got {} arguments.",
                    args.len()
                ),
            }),
        );

        self.add_native_function(
            "fputs",
            Box::new(|args| match args {
                [Value::String(str), arg@ Value::NativePointer(NativePointer::FilePointer(fp))] => {
                    match fp {
                        FilePointer::StdIn => {
                             panic!("Cannot write to a read-only file pointer `{}`", arg);
                        }
                        FilePointer::RawPointer(fp) => {
                            let c_to_print = std::ffi::CString::new(str as &str).unwrap();
                            unsafe {
                                libc::fputs(c_to_print.as_ptr(), *fp);
                            }
                        },

                        FilePointer::StdOut => {
                            print!("{}", str);
                        }
                        FilePointer::StdErr => {
                            eprint!("{}", str);
                        }
                    }
                    Value::Null
                }
                [_] => panic!("The first argument to fclose() is not a file pointer"),
                _ => panic!(
                    "Expected 1 argument to the native function `fclose`, but got {} arguments.",
                    args.len()
                ),
            }),
        );
    }

    pub fn add_native_function(&mut self, name: &str, content: NativeFuncContent) {
        self.src.function_list.push(FuncDef::Native(NativeFuncDef {
            func_name: Ident::from(name),
            content,
        }));
    }

    fn add_std_file_pointers(&mut self) {
        self.mutable.global_variables.append(&mut vec![
            Variable {
                name: Ident::from("STDIN"),
                value: Value::NativePointer(NativePointer::FilePointer(FilePointer::StdIn)),
            },
            Variable {
                name: Ident::from("STDOUT"),
                value: Value::NativePointer(NativePointer::FilePointer(FilePointer::StdOut)),
            },
            Variable {
                name: Ident::from("STDERR"),
                value: Value::NativePointer(NativePointer::FilePointer(FilePointer::StdErr)),
            },
        ]);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NumOp {
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum EqOp {
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CmpOp {
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BinOp {
    Add,
    Numerical(NumOp),
    Eq(EqOp),
    Cmp(CmpOp),
}

#[test]
fn test_set_local_var() {
    let mut me = MutableEnvironment {
        local_environment: Some(LocalEnvironment {
            local_variables: vec![Variable {
                name: Ident::from("foo"),
                value: Value::Boolean(true),
            }],
            global_variables_visible_from_local: vec![],
        }),
        global_variables: vec![],
    };

    me.search_local_variable_and_set(&Ident::from("foo"), &Value::Boolean(false));
    assert_eq!(
        me.local_environment,
        Some(LocalEnvironment {
            local_variables: vec![Variable {
                name: Ident::from("foo"),
                value: Value::Boolean(false),
            }],
            global_variables_visible_from_local: vec![],
        })
    );
}

impl LocalEnvironment {
    fn add_local_variable_and_set(&mut self, ident: &Ident, v: &Value) {
        self.local_variables.push(Variable {
            name: ident.clone(),
            value: v.clone(),
        });
    }
}

impl MutableEnvironment {
    fn eval_binary_expression(
        &mut self,
        funcs: &[FuncDef],
        op: BinOp,
        left: &Expr,
        right: &Expr,
    ) -> Value {
        use BinOp::{Add, Cmp, Eq, Numerical};
        use Value::{Boolean, Double, Int, Null, String};
        let left_val = self.eval_expression(funcs, left);
        let right_val = self.eval_expression(funcs, right);
        match (left_val, right_val, op) {
            (Int(l), Int(r), Add) => Value::Int(l + r),
            (Int(l), Int(r), Numerical(o)) => eval_int_numerics(o, l, r),
            (Int(l), Int(r), Cmp(c)) => c.eval(&l, &r),
            (Int(l), Int(r), Eq(e)) => e.eval(&l, &r),
            (Double(l), Double(r), _) => eval_binary_double(op, l, r),
            (Int(l), Double(r), _) => eval_binary_double(op, f64::from(l), r),
            (Double(l), Int(r), _) => eval_binary_double(op, l, f64::from(r)),
            (Boolean(l), Boolean(r), Eq(e)) => e.eval(&l, &r),
            (String(l), r, BinOp::Add) => Value::String(format!("{}{}", l, r)),
            (String(l), String(r), BinOp::Cmp(c)) => c.eval(&l, &r),
            (String(l), String(r), BinOp::Eq(c)) => c.eval(&l, &r),
            (Null, Null, Eq(e)) => e.eval(&Null, &Null),
            (Null, _, Eq(EqOp::Equal)) | (_, Null, Eq(EqOp::Equal)) => Value::Boolean(false),
            (Null, _, Eq(EqOp::NotEqual)) | (_, Null, Eq(EqOp::NotEqual)) => Value::Boolean(true),
            _ => self.throw_runtime_error(&format!(
                "Invalid operation made using the operator `{:?}`",
                op
            )),
        }
    }

    fn eval_assign_expression(&mut self, funcs: &[FuncDef], ident: &Ident, right: &Expr) -> Value {
        let v = self.eval_expression(funcs, right);
        if self.search_local_variable_and_set(ident, &v).is_some()
            || self
                .search_global_variable_from_local_env_and_set(ident, &v)
                .is_some()
        {
        } else if let Some(env) = &mut self.local_environment {
            env.add_local_variable_and_set(ident, &v);
        } else {
            self.add_global_variable_and_set(ident, &v);
        }
        v
    }
    fn eval_expression(&mut self, funcs: &[FuncDef], expr: &Expr) -> Value {
        match expr {
            Expr::Assign(left, right) => self.eval_assign_expression(funcs, left, right),
            Expr::LogicalOr(left, right) => {
                let left_val = self.eval_expression(funcs, left);
                match left_val {
                    Value::Boolean(true) =>{ Value::Boolean(true) },
                    Value::Boolean(false) =>{
                        let right_val = self.eval_expression(funcs, right);
                        match right_val {
                            a @ Value::Boolean(_) => a,
                            _ => self.throw_runtime_error("expected a boolean type for the condition of the right side of `||`, but it was not."),
                        }
                    },
                    _ => self.throw_runtime_error(
                    "expected a boolean type for the condition of the left side of `||`, but it was not.",
                ),
                }
            }
            Expr::LogicalAnd(left, right) => {
                let left_val = self.eval_expression(funcs, left);
                match left_val {
                    Value::Boolean(false) =>{ Value::Boolean(false) },
                    Value::Boolean(true) =>{
                        let right_val = self.eval_expression(funcs, right);
                        match right_val {
                            a @ Value::Boolean(_) => a,
                            _ => self.throw_runtime_error("expected a boolean type for the condition of the right side of `&&`, but it was not."),
                        }
                    },
                    _ => self.throw_runtime_error(
                    "expected a boolean type for the condition of the left side of `&&`, but it was not.",
                ),
                }
            }
            Expr::Null => Value::Null,
            Expr::True => Value::Boolean(true),
            Expr::False => Value::Boolean(false),
            Expr::StringLiteral(s) => Value::String(s.clone()),
            Expr::DoubleLiteral(d) => Value::Double(*d),
            Expr::IntLiteral(i) => Value::Int(*i),
            Expr::Add(left, right) => self.eval_binary_expression(funcs, BinOp::Add, left, right),
            Expr::Sub(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Numerical(NumOp::Sub), left, right)
            }
            Expr::Mul(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Numerical(NumOp::Mul), left, right)
            }
            Expr::Div(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Numerical(NumOp::Div), left, right)
            }
            Expr::Mod(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Numerical(NumOp::Mod), left, right)
            }
            Expr::Equal(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Eq(EqOp::Equal), left, right)
            }
            Expr::NotEqual(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Eq(EqOp::NotEqual), left, right)
            }
            Expr::GreaterThan(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Cmp(CmpOp::GreaterThan), left, right)
            }
            Expr::GreaterThanOrEqual(left, right) => self.eval_binary_expression(
                funcs,
                BinOp::Cmp(CmpOp::GreaterThanOrEqual),
                left,
                right,
            ),
            Expr::LessThan(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Cmp(CmpOp::LessThan), left, right)
            }
            Expr::LessThanOrEqual(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Cmp(CmpOp::LessThanOrEqual), left, right)
            }

            Expr::Negative(e) => self.eval_negative_expression(funcs, e),
            Expr::FunctionCall(ident, args) => self.eval_funccall_expression(funcs, ident, args),
            Expr::Identifier(ident) => self.eval_ident_expression(ident),
        }
    }

    fn search_global_variable(&self, ident: &Ident) -> Option<Variable> {
        for gv in &self.global_variables {
            if &gv.name == ident {
                return Some(gv.clone());
            }
        }
        None
    }

    fn search_global_variable_from_local_env(&self, ident: &Ident) -> Option<Variable> {
        if let Some(env) = &self.local_environment {
            for gvname in &env.global_variables_visible_from_local {
                if gvname == ident {
                    return self.search_global_variable(ident);
                }
            }
            None
        } else {
            self.search_global_variable(ident)
        }
    }

    fn search_global_variable_from_local_env_and_set(
        &mut self,
        ident: &Ident,
        value: &Value,
    ) -> Option<()> {
        if let Some(env) = &mut self.local_environment {
            for gvname in &mut env.global_variables_visible_from_local {
                if gvname == ident {
                    return self.search_global_variable_and_set(ident, value);
                }
            }
            None
        } else {
            self.search_global_variable_and_set(ident, value)
        }
    }

    fn search_global_variable_and_set(&mut self, ident: &Ident, value: &Value) -> Option<()> {
        for gv in &mut self.global_variables {
            if &gv.name == ident {
                gv.value = value.clone();
                return Some(());
            }
        }
        None
    }

    fn eval_ident_expression(&mut self, ident: &Ident) -> Value {
        if let Some(var) = self.search_local_variable(ident) {
            return var.value;
        }
        if let Some(var) = self.search_global_variable_from_local_env(ident) {
            return var.value;
        }
        self.throw_runtime_error(&format!("Cannot find a variable named `{}`", ident.name()))
    }

    fn search_local_variable(&self, ident: &Ident) -> Option<Variable> {
        if let Some(env) = &self.local_environment {
            for lv in &env.local_variables {
                if &lv.name == ident {
                    return Some(lv.clone());
                }
            }
            None
        } else {
            None
        }
    }

    fn search_local_variable_and_set(&mut self, ident: &Ident, value: &Value) -> Option<()> {
        if let Some(env) = &mut self.local_environment {
            for lv in &mut env.local_variables {
                if &lv.name == ident {
                    lv.value = value.clone();
                }
            }
            None
        } else {
            None
        }
    }

    fn add_global_variable_and_set(&mut self, ident: &Ident, v: &Value) {
        self.global_variables.push(Variable {
            name: ident.clone(),
            value: v.clone(),
        });
    }

    fn eval_funccall_expression(
        &mut self,
        funcs: &[FuncDef],
        ident: &Ident,
        args: &[Expr],
    ) -> Value {
        for f in funcs {
            match f {
                FuncDef::Crowbar(f) => {
                    if &f.func_name == ident {
                        return self.call_crowbar_function(funcs, f, args);
                    }
                }
                FuncDef::Native(f) => {
                    if &f.func_name == ident {
                        return self.call_native_function(funcs, f, args);
                    }
                }
            }
        }
        self.throw_runtime_error(&format!("Cannot find a function named `{}`", ident.name()))
    }

    fn call_crowbar_function(
        &mut self,
        funcs: &[FuncDef],
        f: &CrowbarFuncDef,
        args: &[Expr],
    ) -> Value {
        let mut local_env = LocalEnvironment::default();
        if f.params.len() != args.len() {
            self.throw_runtime_error(&format!("Mismatch: the number of arguments passed is {} while the number of parameters is {}", args.len(), f.params.len()))
        }
        for (i, arg) in args.iter().enumerate() {
            let arg_val = self.eval_expression(funcs, arg);
            local_env.add_local_variable_and_set(&f.params[i], &arg_val);
        }

        let mut env = Some(local_env);

        // Store the current environment in env, so that we can restore it later
        std::mem::swap(&mut env, &mut self.local_environment);

        let result = self.execute_statement_list(funcs, &f.block.0);

        // Restore the environment
        std::mem::swap(&mut env, &mut self.local_environment);

        match result {
            StatementResult::Return(Some(value)) => value,
            _ => Value::Null,
        }
    }

    fn call_native_function(
        &mut self,
        funcs: &[FuncDef],
        f: &NativeFuncDef,
        args: &[Expr],
    ) -> Value {
        let mut arg_vals = vec![];
        for arg in args {
            arg_vals.push(self.eval_expression(funcs, arg));
        }

        (f.content)(&arg_vals)
    }

    fn eval_negative_expression(&mut self, funcs: &[FuncDef], expr: &Expr) -> Value {
        let operand_val = self.eval_expression(funcs, expr);
        match operand_val {
            Value::Int(i) => Value::Int(-i),
            Value::Double(d) => Value::Double(-d),
            _ => self.throw_runtime_error(
                "Invalid type found as the operand of the unary minus operator",
            ),
        }
    }
    fn eval_expression_optional(
        &mut self,
        funcs: &[FuncDef],
        expr: &Option<Expr>,
    ) -> Option<Value> {
        expr.as_ref().map(|expr| self.eval_expression(funcs, expr))
    }

    fn execute_statement(&mut self, funcs: &[FuncDef], statement: &Statement) -> StatementResult {
        let mut result = StatementResult::Normal;

        match statement {
            Statement::Expression(expr) => {
                self.eval_expression_optional(funcs, expr);
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
                result =
                    self.execute_if_statement(funcs, if_expr, if_block, elsif_list, else_block);
            }

            Statement::While(expr, block) => {
                result = self.execute_while_statement(funcs, expr, block);
            }
            Statement::Break => {
                result = StatementResult::Break;
            }
            Statement::Continue => {
                result = StatementResult::Continue;
            }
            Statement::For(expr1, expr2, expr3, block) => {
                result = self.execute_for_statement(funcs, expr1, expr2, expr3, block);
            }
            Statement::Return(expr) => {
                let value: Option<Value> = self.eval_expression_optional(funcs, expr);
                result = StatementResult::Return(value);
            }
        }

        result
    }

    fn execute_statement_list(
        &mut self,
        funcs: &[FuncDef],
        stmts: &[Statement],
    ) -> StatementResult {
        let mut result = StatementResult::Normal;
        for stmt in stmts {
            result = self.execute_statement(funcs, stmt);
            if result != StatementResult::Normal {
                return result;
            }
        }
        result
    }

    fn execute_global_statement(&mut self, idents: &[Ident]) -> StatementResult {
        let result = StatementResult::Normal;
        match &mut self.local_environment {
            None => self.throw_runtime_error("`global` statement found in top-level"),
            Some(env) => {
                for ident in idents {
                    env.global_variables_visible_from_local.push(ident.clone());
                }
            }
        }
        result
    }

    fn execute_elsif(
        &mut self,
        funcs: &[FuncDef],
        elsif_list: &[(Expr, Block)],
        elsif_executed: &mut bool,
    ) -> StatementResult {
        *elsif_executed = false;
        let mut result = StatementResult::Normal;
        for elsif in elsif_list {
            let cond = self.eval_expression(funcs, &elsif.0);
            match cond {
                Value::Boolean(false) => {}
                Value::Boolean(true) => {
                    result = self.execute_statement_list(funcs, &elsif.1 .0);
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
        funcs: &[FuncDef],
        if_expr: &Expr,
        if_block: &Block,
        elsif_list: &[(Expr, Block)],
        else_block: &Option<Block>,
    ) -> StatementResult {
        let mut result;
        let cond = self.eval_expression(funcs, if_expr);
        match cond {
            Value::Boolean(false) => {
                let mut elsif_executed: bool = false;
                result = self.execute_elsif(funcs, elsif_list, &mut elsif_executed);
                if result != StatementResult::Normal {
                    return result;
                }
                if !elsif_executed {
                    if let Some(block) = else_block {
                        result = self.execute_statement_list(funcs, &block.0);
                    }
                    return result;
                }
            }
            Value::Boolean(true) => {
                result = self.execute_statement_list(funcs, &if_block.0);
            }
            _ => self.throw_runtime_error(
                "expected a boolean type for the condition of `if`, but it was not.",
            ),
        }
        result
    }
    fn execute_while_statement(
        &mut self,
        funcs: &[FuncDef],
        expr: &Expr,
        block: &Block,
    ) -> StatementResult {
        let mut result;
        loop {
            let cond = self.eval_expression(funcs, expr);
            match cond {
                Value::Boolean(false) => {
                    result = StatementResult::Normal; // fixes a bug that exists in the original implementation
                    break;
                }
                Value::Boolean(true) => {
                    result = self.execute_statement_list(funcs, &block.0);
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
        funcs: &[FuncDef],
        expr1: &Option<Expr>,
        expr2: &Option<Expr>,
        expr3: &Option<Expr>,
        block: &Block,
    ) -> StatementResult {
        let mut result;
        if let Some(initial_expr) = expr1 {
            let _initial: Value = self.eval_expression(funcs, initial_expr);
        }
        loop {
            if let Some(cond_expr) = expr2 {
                let cond = self.eval_expression(funcs, cond_expr);
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
            result = self.execute_statement_list(funcs, &block.0);

            match result {
                StatementResult::Return(_) => break,
                StatementResult::Break => {
                    result = StatementResult::Normal;
                    break;
                }
                _ => {}
            }

            if let Some(post_expr) = expr3 {
                let _final = self.eval_expression(funcs, post_expr);
            }
        }
        result
    }

    fn throw_runtime_error(&self, msg: &str) -> ! {
        panic!("Runtime error: {}", msg)
    }
}

impl EqOp {
    fn eval<T: PartialEq>(self, l: &T, r: &T) -> Value {
        Value::Boolean(match self {
            EqOp::Equal => l == r,
            EqOp::NotEqual => l != r,
        })
    }
}

impl CmpOp {
    fn eval<T: PartialOrd>(self, l: &T, r: &T) -> Value {
        Value::Boolean(match self {
            CmpOp::GreaterThan => l > r,
            CmpOp::GreaterThanOrEqual => l >= r,
            CmpOp::LessThan => l < r,
            CmpOp::LessThanOrEqual => l <= r,
        })
    }
}

fn eval_binary_double(o: BinOp, l: f64, r: f64) -> Value {
    match o {
        BinOp::Add => Value::Double(l + r),
        BinOp::Numerical(o) => Value::Double(match o {
            NumOp::Sub => l - r,
            NumOp::Mul => l * r,
            NumOp::Div => l / r,
            NumOp::Mod => l % r,
        }),
        BinOp::Eq(e) => e.eval(&l, &r),
        BinOp::Cmp(c) => c.eval(&l, &r),
    }
}

const fn eval_int_numerics(o: NumOp, l: i32, r: i32) -> Value {
    Value::Int(match o {
        NumOp::Sub => l - r,
        NumOp::Mul => l * r,
        NumOp::Div => l / r,
        NumOp::Mod => l % r,
    })
}
