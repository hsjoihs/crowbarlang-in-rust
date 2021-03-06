use std::cell::RefCell;
use std::rc::Rc;

use crate::lex::Ident;
use crate::lex::LineNumber;
use crate::parse::Block;
use crate::parse::CrowbarFuncDef;
use crate::parse::Expr;
use crate::parse::Expr_;
use crate::parse::Statement;
use crate::parse::Statement_;
use crate::runtime_error::RuntimeError;
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
    Array(Rc<RefCell<Vec<Value>>>),
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

type NativeFuncContent = Box<dyn Fn(&[Value], LineNumber) -> Value>;

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
            Value::Array(arr) => {
                let arr = arr.borrow();
                let vec: Vec<String> = arr.iter().map(std::string::ToString::to_string).collect();
                // (1, 2, 3, 4, 5, 6, 7, 8)
                write!(f, "({})", vec.join(", "))
            }
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
            Box::new(|args, line_number| {
                match args {
                    [arg] => print!("{}", arg),
                    [] => RuntimeError::ArgumentTooFew.thrown_at(line_number),
                    [_, _, ..] => RuntimeError::ArgumentTooMany.thrown_at(line_number),
                };
                Value::Null
            }),
        );
        self.add_native_function(
            "fopen",
            Box::new(|args, line_number| match args {
                [Value::String(filename), Value::String(mode)] => {
                    use std::ffi::CString;
                    let filename = CString::new(filename as &str).unwrap();
                    let mode = CString::new(mode as &str).unwrap();
                    let fp = unsafe { libc::fopen(filename.as_ptr(), mode.as_ptr()) };
                    Value::NativePointer(NativePointer::FilePointer(FilePointer::RawPointer(fp)))
                }
                [_, _] => RuntimeError::FopenArgumentType.thrown_at(line_number),
                [] | [_] => RuntimeError::ArgumentTooFew.thrown_at(line_number),
                [_, _, _, ..] => RuntimeError::ArgumentTooMany.thrown_at(line_number),
            }),
        );
        self.add_native_function(
            "fclose",
            Box::new(|args, line_number| match args {
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
                [_] => RuntimeError::FcloseArgumentType.thrown_at(line_number),
                [] => RuntimeError::ArgumentTooFew.thrown_at(line_number),
                [_, _, ..] => RuntimeError::ArgumentTooMany.thrown_at(line_number),
            }),
        );

        self.add_native_function(
            "fgets",
            Box::new(|args, line_number| match args {
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
                        let c_str = std::ffi::CString::new(ret_buf).unwrap();
                        let str = c_str.to_str();

                        match str {
                            Ok(str) => {
                                if str.is_empty() {
                                    Value::Null
                                } else {
                                    Value::String(str.to_string())
                                }
                            }
                            Err(_) => RuntimeError::BadMultibyteCharacter.thrown_at(line_number),
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
                [_] => RuntimeError::FgetsArgumentType.thrown_at(line_number),
                [] => RuntimeError::ArgumentTooFew.thrown_at(line_number),
                [_, _, ..] => RuntimeError::ArgumentTooMany.thrown_at(line_number),
            }),
        );

        self.add_native_function(
            "fputs",
            Box::new(|args, line_number| match args {
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
                [_] => RuntimeError::FputsArgumentType.thrown_at(line_number),
                [] => RuntimeError::ArgumentTooFew.thrown_at(line_number),
                [_, _, ..] => RuntimeError::ArgumentTooMany.thrown_at(line_number),
            }),
        );

        self.add_native_function(
            "new_array",
            Box::new(|args, line_number| {
                if let [] = args {
                    RuntimeError::ArgumentTooFew.thrown_at(line_number)
                } else {
                    let dimensions = args
                        .iter()
                        .map(|arg| match arg {
                            Value::Int(len) => (*len)
                                .try_into()
                                .expect("The argument to new_array() is a negative integer"),
                            _ => RuntimeError::NewArrayArgumentType.thrown_at(line_number),
                        })
                        .collect::<Vec<usize>>();
                    new_array(&dimensions)
                }
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

fn new_array(dimensions: &[usize]) -> Value {
    match dimensions {
        [] => Value::Null,
        [len, rest @ ..] => {
            let mut vec = vec![];
            vec.resize_with(*len, || new_array(rest));
            Value::Array(Rc::new(RefCell::new(vec)))
        }
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

impl BinOp {
    pub const fn get_operator_string(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Numerical(NumOp::Sub) => "-",
            BinOp::Numerical(NumOp::Mul) => "*",
            BinOp::Numerical(NumOp::Div) => "/",
            BinOp::Numerical(NumOp::Mod) => "%",
            BinOp::Eq(EqOp::Equal) => "==",
            BinOp::Eq(EqOp::NotEqual) => "!=",
            BinOp::Cmp(CmpOp::GreaterThan) => ">",
            BinOp::Cmp(CmpOp::GreaterThanOrEqual) => ">=",
            BinOp::Cmp(CmpOp::LessThan) => "<",
            BinOp::Cmp(CmpOp::LessThanOrEqual) => "<=",
        }
    }
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

    me.search_local_variable_and_set(&Ident::from("foo"), Value::Boolean(false));
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
            (Boolean(_), Boolean(_), op) => RuntimeError::NotBooleanOperator {
                op: op.get_operator_string(),
            }
            .thrown_at(left.1),
            (String(l), r, BinOp::Add) => Value::String(format!("{}{}", l, r)),
            (String(l), String(r), BinOp::Cmp(c)) => c.eval(&l, &r),
            (String(l), String(r), BinOp::Eq(c)) => c.eval(&l, &r),
            (String(_), String(_), op) => RuntimeError::BadOperatorForString {
                op: op.get_operator_string(),
            }
            .thrown_at(left.1),
            (Null, Null, Eq(e)) => e.eval(&Null, &Null),
            (Null, Null, op) => RuntimeError::NotNullOperator {
                op: op.get_operator_string(),
            }
            .thrown_at(left.1),
            (Null, _, Eq(EqOp::Equal)) | (_, Null, Eq(EqOp::Equal)) => Value::Boolean(false),
            (Null, _, Eq(EqOp::NotEqual)) | (_, Null, Eq(EqOp::NotEqual)) => Value::Boolean(true),
            _ => RuntimeError::BadOperandType {
                op: op.get_operator_string(),
            }
            .thrown_at(left.1),
        }
    }

    fn eval_assign_expression(&mut self, funcs: &[FuncDef], left: &Expr, right: &Expr) -> Value {
        let v = self.eval_expression(funcs, right);
        self.assign(funcs, left, v.clone());
        v
    }

    fn eval_compound_add_expression(
        &mut self,
        funcs: &[FuncDef],
        expr: &Expr,
        delta: i32,
    ) -> Value {
        match &expr.0 {
            Expr_::IndexAccess { array, index } => {
                // must not evaluate the expression twice
                let array = self.eval_expression(funcs, array);
                let index = self.eval_expression(funcs, index);

                // all the remaining operations are implemented using `Value`
                if let Value::Int(i) = Self::get_array_element(&array, &index, expr.1) {
                    let new_val = Value::Int(i + delta);
                    Self::set_array_element(&array, &index, new_val.clone(), expr.1);
                    new_val
                } else {
                    RuntimeError::IncDecOperandType.thrown_at(expr.1)
                }
            }
            Expr_::Identifier(ident) => {
                // it's totally fine to evaluate an identifier twice
                if let Value::Int(i) = self.eval_ident_expression(ident, expr.1) {
                    let new_val = Value::Int(i + delta);
                    self.assign(funcs, expr, new_val.clone());
                    new_val
                } else {
                    RuntimeError::IncDecOperandType.thrown_at(expr.1)
                }
            }
            _ => RuntimeError::NotLvalue.thrown_at(expr.1),
        }
    }

    fn assign(&mut self, funcs: &[FuncDef], left: &Expr, value: Value) {
        // todo: many of the .clone() are unnecessary
        match &left.0 {
            Expr_::Identifier(ident) => {
                if self
                    .search_local_variable_and_set(ident, value.clone())
                    .is_some()
                    || self
                        .search_global_variable_from_local_env_and_set(ident, &value)
                        .is_some()
                {
                } else if let Some(env) = &mut self.local_environment {
                    env.add_local_variable_and_set(ident, &value);
                } else {
                    self.add_global_variable_and_set(ident, value);
                }
            }
            Expr_::IndexAccess { array, index } => {
                let array = self.eval_expression(funcs, array);
                let index = self.eval_expression(funcs, index);

                Self::set_array_element(&array, &index, value, left.1);
            }
            _ => RuntimeError::NotLvalue.thrown_at(left.1),
        }
    }

    fn set_array_element(array: &Value, index: &Value, v: Value, line_number: LineNumber) {
        match (array, index) {
            (Value::Array(array), Value::Int(index)) => {
                let mut array = array.borrow_mut();
                let len = array.len();
                if let Ok(index_) = TryInto::<usize>::try_into(*index) {
                    match (*array).get_mut(index_) {
                        Some(lvalue) => {
                            *lvalue = v;
                            return;
                        }
                        None => RuntimeError::ArrayIndexOutOfBounds {
                            size: len,
                            index: *index,
                        }
                        .thrown_at(line_number),
                    }
                }
                RuntimeError::ArrayIndexOutOfBounds {
                    size: len,
                    index: *index,
                }
                .thrown_at(line_number)
            }
            (Value::Array(_), _) => RuntimeError::IndexOperandNotInt.thrown_at(line_number),
            (_, _) => RuntimeError::IndexOperandNotArray.thrown_at(line_number),
        }
    }

    fn get_array_element(array: &Value, index: &Value, line_number: LineNumber) -> Value {
        match (array, index) {
            (Value::Array(array), Value::Int(index)) => {
                let array = array.borrow();
                let len = array.len();
                if let Ok(index_) = TryInto::<usize>::try_into(*index) {
                    match array.get(index_) {
                        Some(v) => {
                            return v.clone();
                        }
                        None => RuntimeError::ArrayIndexOutOfBounds {
                            size: len,
                            index: *index,
                        }
                        .thrown_at(line_number),
                    }
                }
                RuntimeError::ArrayIndexOutOfBounds {
                    size: len,
                    index: *index,
                }
                .thrown_at(line_number)
            }
            (Value::Array(_), _) => RuntimeError::IndexOperandNotInt.thrown_at(line_number),
            (_, _) => RuntimeError::IndexOperandNotArray.thrown_at(line_number),
        }
    }

    fn eval_array_literal_expression(&mut self, funcs: &[FuncDef], exprs: &[Expr]) -> Value {
        let mut ans = vec![];
        for expr in exprs {
            let val = self.eval_expression(funcs, expr);
            ans.push(val);
        }
        Value::Array(Rc::new(RefCell::new(ans)))
    }

    fn eval_array_index_access(
        &mut self,
        funcs: &[FuncDef],
        array: &Expr,
        index: &Expr,
        line_number: LineNumber,
    ) -> Value {
        let array = self.eval_expression(funcs, array);
        let index = self.eval_expression(funcs, index);

        Self::get_array_element(&array, &index, line_number)
    }

    fn eval_method_call_expression(
        &mut self,
        funcs: &[FuncDef],
        receiver: &Expr,
        method_name: &Ident,
        args: &[Expr],
    ) -> Value {
        let line_number = receiver.1;
        let receiver = self.eval_expression(funcs, receiver);
        match (receiver, method_name.name()) {
            (Value::Array(arr), "add") => match args {
                [expr] => {
                    let v = self.eval_expression(funcs, expr);
                    arr.borrow_mut().push(v);
                    Value::Null
                }
                [] => RuntimeError::ArgumentTooFew.thrown_at(line_number),
                _ => RuntimeError::ArgumentTooMany.thrown_at(line_number),
            },
            (Value::Array(arr), "size") => match args {
                [] => Value::Int(arr.borrow().len().try_into().unwrap()),
                _ => RuntimeError::ArgumentTooMany.thrown_at(line_number),
            },
            (Value::Array(arr), "resize") => match args {
                [expr] => {
                    let v = self.eval_expression(funcs, expr);
                    match v {
                        Value::Int(new_len) => {
                            let mut arr = arr.borrow_mut();
                            arr.resize(new_len.try_into().unwrap(), Value::Null);
                        }
                        _ => RuntimeError::ArrayResizeArgument.thrown_at(line_number),
                    }
                    Value::Null
                }
                [] => RuntimeError::ArgumentTooFew.thrown_at(line_number),
                _ => RuntimeError::ArgumentTooMany.thrown_at(line_number),
            },
            (Value::String(str), "length") => Value::Int(str.chars().count().try_into().unwrap()),
            _ => RuntimeError::NoSuchMethod(method_name.clone()).thrown_at(line_number),
        }
    }

    fn eval_expression(&mut self, funcs: &[FuncDef], expr: &Expr) -> Value {
        match &expr.0 {
            Expr_::IndexAccess { array, index } => {
                self.eval_array_index_access(funcs, array, index, expr.1)
            }
            Expr_::MethodCall {
                receiver,
                method_name,
                args,
            } => self.eval_method_call_expression(funcs, receiver, method_name, args),
            Expr_::Increment(expr) => self.eval_compound_add_expression(funcs, expr, 1),
            Expr_::Decrement(expr) => self.eval_compound_add_expression(funcs, expr, -1),
            Expr_::ArrayLiteral(exprs) => self.eval_array_literal_expression(funcs, exprs),
            Expr_::Assign(left, right) => self.eval_assign_expression(funcs, left, right),
            Expr_::LogicalOr(left, right) => {
                let left_val = self.eval_expression(funcs, left);
                match left_val {
                    Value::Boolean(true) => Value::Boolean(true),
                    Value::Boolean(false) => {
                        let right_val = self.eval_expression(funcs, right);
                        match right_val {
                            a @ Value::Boolean(_) => a,
                            _ => RuntimeError::BadOperandType { op: "||" }.thrown_at(left.1),
                        }
                    }
                    _ => RuntimeError::BadOperandType { op: "||" }.thrown_at(left.1),
                }
            }
            Expr_::LogicalAnd(left, right) => {
                let left_val = self.eval_expression(funcs, left);
                match left_val {
                    Value::Boolean(false) => Value::Boolean(false),
                    Value::Boolean(true) => {
                        let right_val = self.eval_expression(funcs, right);
                        match right_val {
                            a @ Value::Boolean(_) => a,
                            _ => RuntimeError::BadOperandType { op: "&&" }.thrown_at(left.1),
                        }
                    }
                    _ => RuntimeError::BadOperandType { op: "&&" }.thrown_at(left.1),
                }
            }
            Expr_::Null => Value::Null,
            Expr_::True => Value::Boolean(true),
            Expr_::False => Value::Boolean(false),
            Expr_::StringLiteral(s) => Value::String(s.clone()),
            Expr_::DoubleLiteral(d) => Value::Double(*d),
            Expr_::IntLiteral(i) => Value::Int(*i),
            Expr_::Add(left, right) => self.eval_binary_expression(funcs, BinOp::Add, left, right),
            Expr_::Sub(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Numerical(NumOp::Sub), left, right)
            }
            Expr_::Mul(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Numerical(NumOp::Mul), left, right)
            }
            Expr_::Div(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Numerical(NumOp::Div), left, right)
            }
            Expr_::Mod(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Numerical(NumOp::Mod), left, right)
            }
            Expr_::Equal(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Eq(EqOp::Equal), left, right)
            }
            Expr_::NotEqual(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Eq(EqOp::NotEqual), left, right)
            }
            Expr_::GreaterThan(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Cmp(CmpOp::GreaterThan), left, right)
            }
            Expr_::GreaterThanOrEqual(left, right) => self.eval_binary_expression(
                funcs,
                BinOp::Cmp(CmpOp::GreaterThanOrEqual),
                left,
                right,
            ),
            Expr_::LessThan(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Cmp(CmpOp::LessThan), left, right)
            }
            Expr_::LessThanOrEqual(left, right) => {
                self.eval_binary_expression(funcs, BinOp::Cmp(CmpOp::LessThanOrEqual), left, right)
            }

            Expr_::Negative(e) => self.eval_negative_expression(funcs, e),
            Expr_::FunctionCall(ident, args) => {
                self.eval_funccall_expression(funcs, ident, args, expr.1)
            }
            Expr_::Identifier(ident) => self.eval_ident_expression(ident, expr.1),
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

    fn eval_ident_expression(&mut self, ident: &Ident, line_number: LineNumber) -> Value {
        if let Some(var) = self.search_local_variable(ident) {
            return var.value;
        }
        if let Some(var) = self.search_global_variable_from_local_env(ident) {
            return var.value;
        }
        RuntimeError::VariableNotFound(ident.clone()).thrown_at(line_number)
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

    fn search_local_variable_and_set(&mut self, ident: &Ident, value: Value) -> Option<()> {
        if let Some(env) = &mut self.local_environment {
            for lv in &mut env.local_variables {
                if &lv.name == ident {
                    lv.value = value;
                    return Some(());
                }
            }
            None
        } else {
            None
        }
    }

    fn add_global_variable_and_set(&mut self, ident: &Ident, v: Value) {
        self.global_variables.push(Variable {
            name: ident.clone(),
            value: v,
        });
    }

    fn eval_funccall_expression(
        &mut self,
        funcs: &[FuncDef],
        ident: &Ident,
        args: &[Expr],
        line_number: LineNumber,
    ) -> Value {
        for f in funcs {
            match f {
                FuncDef::Crowbar(f) => {
                    if &f.func_name == ident {
                        return self.call_crowbar_function(funcs, f, args, line_number);
                    }
                }
                FuncDef::Native(f) => {
                    if &f.func_name == ident {
                        return self.call_native_function(funcs, f, args, line_number);
                    }
                }
            }
        }
        RuntimeError::FunctionNotFound(ident.clone()).thrown_at(line_number)
    }

    fn call_crowbar_function(
        &mut self,
        funcs: &[FuncDef],
        f: &CrowbarFuncDef,
        args: &[Expr],
        line_number: LineNumber,
    ) -> Value {
        let mut local_env = LocalEnvironment::default();
        match args.len().cmp(&f.params.len()) {
            std::cmp::Ordering::Less => RuntimeError::ArgumentTooFew.thrown_at(line_number),
            std::cmp::Ordering::Greater => RuntimeError::ArgumentTooMany.thrown_at(line_number),
            std::cmp::Ordering::Equal => {
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
        }
    }

    fn call_native_function(
        &mut self,
        funcs: &[FuncDef],
        f: &NativeFuncDef,
        args: &[Expr],
        line_number: LineNumber,
    ) -> Value {
        let mut arg_vals = vec![];
        for arg in args {
            arg_vals.push(self.eval_expression(funcs, arg));
        }

        (f.content)(&arg_vals, line_number)
    }

    fn eval_negative_expression(&mut self, funcs: &[FuncDef], expr: &Expr) -> Value {
        let operand_val = self.eval_expression(funcs, expr);
        match operand_val {
            Value::Int(i) => Value::Int(-i),
            Value::Double(d) => Value::Double(-d),
            _ => RuntimeError::MinusOperandType.thrown_at(expr.1),
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

        match &statement.0 {
            Statement_::Expression(expr) => {
                self.eval_expression_optional(funcs, expr);
            }
            Statement_::Global(idents) => {
                result = self.execute_global_statement(idents, statement.1);
            }
            Statement_::If {
                if_expr,
                if_block,
                elsif_list,
                else_block,
            } => {
                result =
                    self.execute_if_statement(funcs, if_expr, if_block, elsif_list, else_block);
            }

            Statement_::While(expr, block) => {
                result = self.execute_while_statement(funcs, expr, block);
            }
            Statement_::Break => {
                result = StatementResult::Break;
            }
            Statement_::Continue => {
                result = StatementResult::Continue;
            }
            Statement_::For(expr1, expr2, expr3, block) => {
                result = self.execute_for_statement(funcs, expr1, expr2, expr3, block);
            }
            Statement_::Return(expr) => {
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

    fn execute_global_statement(
        &mut self,
        idents: &[Ident],
        line_number: LineNumber,
    ) -> StatementResult {
        let result = StatementResult::Normal;
        let global_variables = &self.global_variables;
        match &mut self.local_environment {
            None => RuntimeError::GlobalStatementInToplevel.thrown_at(line_number),
            Some(env) => {
                for ident in idents {
                    if global_variables.iter().filter(|v| &v.name == ident).count() == 0 {
                        RuntimeError::GlobalVariableNotFound(ident.clone()).thrown_at(line_number)
                    }
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
                _ => RuntimeError::NotBooleanType.thrown_at(elsif.0 .1),
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
            _ => RuntimeError::NotBooleanType.thrown_at(if_expr.1),
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
                _ => RuntimeError::NotBooleanType.thrown_at(expr.1),
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
                    _ => RuntimeError::NotBooleanType.thrown_at(cond_expr.1),
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
