use crate::lex::Ident;
use std::fmt;

pub enum RuntimeError {
    VariableNotFound(Ident),
    FunctionNotFound(Ident),
    ArgumentTooMany,
    ArgumentTooFew,
    NotBooleanType,
    MinusOperandType,
    BadOperandType { op: &'static str },
    NotBooleanOperator { op: &'static str },
    FopenArgumentType,
    FcloseArgumentType,
    FgetsArgumentType,
    FputsArgumentType,
    NotNullOperator { op: &'static str },
    DivisionByZero,
    GlobalVariableNotFound(Ident),
    GlobalStatementInToplevel,
    BadOperatorForString { op: &'static str },
    NotLvalue,
    IndexOperandNotArray,
    IndexOperandNotInt,
    ArrayIndexOutOfBounds { size: usize, index: i32 },
    NoSuchMethod(Ident),
    NewArrayArgumentType,
    IncDecOperandType,
    ArrayResizeArgument,
    BadMultibyteCharacter,
    UnexpectedWideString,
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            RuntimeError::VariableNotFound(ident) => {
                write!(f, "変数が見つかりません({})。", ident.name())
            }
            RuntimeError::FunctionNotFound(ident) => {
                write!(f, "関数が見つかりません({})。", ident.name())
            }
            RuntimeError::ArgumentTooMany => {
                write!(f, "関数が要求する引数に対し、渡している引数が多すぎます。")
            }
            RuntimeError::ArgumentTooFew => {
                write!(
                    f,
                    "関数が要求する引数に対し、渡している引数が少なすぎます。"
                )
            }
            RuntimeError::NotBooleanType => {
                write!(f, "条件式はboolean型でなければなりません。")
            }
            RuntimeError::MinusOperandType => {
                write!(
                    f,
                    "マイナス演算子のオペランドは数値型でなければなりません。"
                )
            }
            RuntimeError::BadOperandType { op } => {
                write!(f, "2項演算子{}のオペランドの型が不正です。", op)
            }
            RuntimeError::NotBooleanOperator { op } => {
                write!(f, "{}演算子はboolean型には使えません。", op)
            }
            RuntimeError::FopenArgumentType => {
                write!(
                    f,
                    "fopen()関数にはファイルのパスとモード(どちらも文字列型)を渡してください。"
                )
            }
            RuntimeError::FcloseArgumentType => {
                write!(f, "fclose()関数にはファイルポインタを渡してください。")
            }
            RuntimeError::FgetsArgumentType => {
                write!(f, "fgets()関数にはファイルポインタを渡してください。")
            }
            RuntimeError::FputsArgumentType => {
                write!(
                    f,
                    "fputs()関数にはファイルポインタと文字列を渡してください。"
                )
            }
            RuntimeError::NotNullOperator { op } => {
                write!(
                    f,
                    "nullに対して適用できる演算は == と != だけです({}はできません)。",
                    op
                )
            }
            RuntimeError::DivisionByZero => write!(f, "ゼロで除算はできません。"),
            RuntimeError::GlobalVariableNotFound(ident) => {
                write!(f, "グローバル変数{}は存在しません。", ident.name())
            }
            RuntimeError::GlobalStatementInToplevel => {
                write!(f, "global文は関数外では使えません。")
            }
            RuntimeError::BadOperatorForString { op } => {
                write!(f, "文字列に対し演算子{}は適用できません。", op)
            }
            RuntimeError::NotLvalue => write!(f, "オペランドが左辺値ではありません。"),
            RuntimeError::IndexOperandNotArray => {
                write!(f, "添字演算子の左オペランドが配列ではありません。")
            }
            RuntimeError::IndexOperandNotInt => {
                write!(f, "添字演算子の中が整数ではありません。")
            }
            RuntimeError::ArrayIndexOutOfBounds { size, index } => {
                write!(
                    f,
                    "配列の範囲オーバーです。サイズ{}の配列の[{}]をアクセスしています。",
                    size, index
                )
            }
            RuntimeError::NoSuchMethod(method_name) => {
                write!(
                    f,
                    "対象のオブジェクトには{}というメソッドはありません。",
                    method_name.name()
                )
            }
            RuntimeError::NewArrayArgumentType => {
                write!(f, "new_array()関数には整数型(配列サイズ)を渡してください。")
            }
            RuntimeError::IncDecOperandType => {
                write!(f, "インクリメント/デクリメントの対象が整数ではありません。")
            }
            RuntimeError::ArrayResizeArgument => {
                write!(f, "配列のresize()の引数は整数型でなければなりません。")
            }
            RuntimeError::BadMultibyteCharacter => write!(f, "不正なマルチバイト文字です。"),
            RuntimeError::UnexpectedWideString => write!(f, "想定外のワイド文字列です。"),
        }
    }
}
