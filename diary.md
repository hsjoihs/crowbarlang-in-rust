# crowbarlang-in-rust 開発日記

## 2022年6月24日 (Day 0)

セキュリティ・キャンプ C コンパイラ班の受講生として [C コンパイラを書き始めて](https://github.com/hsjoihs/c-compiler/blob/master/misc/diary.md)からもう 4 年が過ぎようとしている。今では私が講師である。

指定教科書として指定した前橋和弥「新・標準プログラマーズライブラリ C言語 ポインタ完全制覇」も受講者の元に届き、事前学習を行っていく段取りが立っていった。ということで、4 人の受講生と 2 人の講師と 2 人のチューターを含めた Discord 通話を行い、それぞれがやっていきたいことについて話し合った。その中に、「いま C で C コンパイラを作っているけど、Rust で最初から書き直してみたいと思っている」という人がいた。

さて、講師というのは受講生を指導することが責務であり、別に私自身が何かを実装することは業務に含まれていない。とはいえ、皆さんが楽しそうに C コンパイラを書いているというのに、私が何もやらないというのもそれはそれで興ざめである。

そう思っていた最中、別のゼミ（暗号解読チャレンジゼミ）の講師と話したところ、前橋和弥「プログラミング言語を作る」の話になった。プロトタイプチェーンもスコープチェーンもクロージャもある言語を C で実装していくというこの本の面白さについて盛り上がった。

ということで、せっかくなので前橋和弥「プログラミング言語を作る」の crowbar を Rust で再実装してみようと考えた。実は「TypeScript で C コンパイラ書く」ってのも候補に入れていたのだけれど、「ゆーて C コンパイラは一回書いたしなぁ」という気持ちがあったのと、いわゆる動的インタプリタ言語の実装を全然やったことがない（中高生の頃から[コンパイラは書いてた](https://github.com/hsjoihs/camphorscript)けどインタプリタは書いてない）というのがあったので、せっかくならやってみよう。

## 2022年6月25日 (Day 1)

ということでやっていく。リポジトリは建てた。そして昨日の分の意気込みを日記に記載した。

まずは cargo init する。えーと yacc と lex を使うのよね。まあ再帰下降パーサーぐらいは手で書けばいいっしょ。ちょうど[京大マイコンクラブ (KMC) で開催した「Rust を知ろう」](https://hsjoihs.hatenablog.com/entry/2022/06/11/121152)で書いたんだし。

第 2 章は yacc とか lex とか再帰下降パーサーの話をしている。ということで第 3 章を観ていく。

```
for (i = 1; i <= 100; i = i + 1) {
	if (i % 15 == 0) {
		print("FizzBuzz\n");
	} elsif (i % 3 == 0) {
		print("Fizz\n");
	} elsif (i % 5 == 0) {
		print("Buzz\n");
	} else {
		print("" + i + "\n");
	}
}
```

これが実行できるようになるのが 3-1. の目標、とのことである。今回のバージョンでやるのは「boolean, int, double, 文字列, FFI 用のポインタ」。あーそういや libc との FFI があるんだったな。まあ Rust ならそういうのもなんとかなるっしょ。

「最初の代入が宣言を兼ねる」「代入が**実行された**時点で宣言される」

```
if (a == 10) {
	b = 10;
}
print("b.." + b);
```

は、「`a` が 10 のときだけ `b` が宣言され」「もし`a`が10でなければ、未定義変数エラー」とのこと。さすが動的言語。

「トップレベルで初めて代入された変数は、グローバル変数」「関数の中でそれを参照する場合、`global`文により宣言」了解。
「関数内で`global a;`のように宣言すると、以後、その関数内ではグローバル変数の`a`が参照可能になります」

```
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
```

を実行すると

```
a..30
a..20
```

と出力されるべし、とのこと。

制御構文は、`if` とかの後には波括弧を強制。それゆえ `elsif`と書きなさい、という仕様ね。

整数リテラルと実数リテラルと文字列リテラルがあり、演算子があり、実数に対する`%`はC言語の`fmod`と同一であるべし、とのこと。

組み込み関数は `print`, `fopen`, `fclose`, `fgets`, `fputs`。`fputs` は C と異なり改行を付与しない。なるほどね。

暗黙に宣言されるグローバル変数として `STDIN`, `STDOUT`, `STDERR` がある、と。

C との FFI は、C 側からインタプリタのコンストラクタを呼び、`CRB_compile` して `CRB_interpret` して `CRB_dispose_interpreter` すればいいのね。

えーと「モジュールと命名規則」は Rust 側の機構で解決可能。じゃあ 3-3. に行くか。

インタプリタはインタプリタ自体と同寿命のストレージを持ち、インタプリタ破棄のタイミングでそれが解放される、と。ここら辺は Rust に Drop があって本当によかったという気持ちになる。

よし、まあさっさと lexer を書いちゃいますか。

（2時間後）

変に idiomatic じゃない方法で lexer 書いたら 2 時間溶けた。`split_once` とほぼ同等だけどデリミタを消さないで残しておいてくれるやつ、つまり

```rust
    pub fn split_once<'a, P: Pattern<'a>>(&'a self, delimiter: P) -> Option<(&'a str, &'a str)> {
        let (start, end) = delimiter.into_searcher(self).next_match()?;
        // SAFETY: `Searcher` is known to return valid indices.
        unsafe { Some((self.get_unchecked(..start), self.get_unchecked(end..))) }
    }
```

の代わりに

```rust
    pub fn split_once_but_keep_the_delimiter<'a, P: Pattern<'a>>(&'a self, delimiter: P) -> Option<(&'a str, &'a str)> {
        let (start, _end) = delimiter.into_searcher(self).next_match()?;
        // SAFETY: `Searcher` is known to return valid indices.
        unsafe { Some((self.get_unchecked(..start), self.get_unchecked(start..))) }
    }
```

が欲しいという気持ちになった。これがないせいで、

```rust
match input.chars().next() {
	None => (None, input),
	Some('a'..='z' | 'A'..='Z' | '_') => {
		let ident: String = input
			.chars()
			.take_while(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))
			.collect();
		let rest = input.trim_start_matches(
			|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'),
		);
		(Some(Token::Identifier(ident)), rest)
	}
	...
}
```

とか書くはめになっている。いやまあ素直に nom を使えという話なんだよな。

## 2022年6月26日 (Day 2)
今日もやっていこう。えーととりあえず浮動小数点リテラルだよな。
crowbar の仕様上は、`([1-9][0-9]*)|"0"` が整数で、`[0-9]+\.[0-9]+` が小数とのことなので、`.5` とか `5.` とかは考えなくていいっぽいな。
とりあえず浮動小数点リテラルのためのパーサーも実装。そういやせっかくだしテストケースももうちょい増やしておくか。
浮動小数点リテラルを含むテストも書いた。

さて次は解析木の構築。再帰下降をバリバリ書いていきましょう。

バリバリ書いた。やっぱり Rust のマクロは便利ね。

文パーサーも書いていこう。

書いた。

`function` を実装してないけどとりあえずパースしてみよう。おや FizzBuzz は落ちた。for 文単体だと通る。了解。

えーと `print("FizzBuzz\n");` で落ちてる。

## 2022年6月27日 (Day 3)

### デバッグ

```rust
#[test]
fn test_parse_expression2() {
    use crate::lex::Ident;
    let tokvec = vec![
        Token::Identifier(Ident::from("print")),
        Token::LeftParen,
        Token::StringLiteral("FizzBuzz\n".to_string()),
        Token::RightParen,
    ];
    let mut state = ParserState::new(&tokvec);
    let expr = state.parse_expression();
    assert_eq!(state.tokvec, vec![])
}
```

こいつが 

```
assertion failed: `(left == right)`
  left: `[RightParen]`,
 right: `[]`
```

と言ってるので、右カッコを食い忘れたっぽいな。

あー理解。

```rust
if let Some(Token::LeftParen) = self.tokvec.get(0) {
	self.advance(1);
	if let Some(Token::RightParen) = self.tokvec.get(1) {
	...
	}
}
```
としてるのが誤りだな。もう advance してるから、次に問うべきは get(0) なのか。

よしテスト通ったわね。

FizzBuzz の構文木を手で書いて、ちゃんと期待するのと一致するか調べるか。

調べた。一致した。

関数にも対応し、構文木も処理できていそうであることを確認。これでパーサーは完了かな。

### リファクタリング

ちょっと疲れたので、一旦リファクタリングをするか。とりあえず `#![warn(clippy::pedantic, clippy::nursery)]` と書いて。

あ、 rust-analyzer 拡張の方だと clippy のが VSCode 上で見えないのか。じゃあ rust 拡張の方に戻して、適用していく。

あー、 lexer の cognitive complexity が高いと言われた。まあねぇ。

マクロにするのをやめて for 文にしたら普通に通った。

あと、 NullT とかの末尾についてる T は名前衝突回避のためらしい。 Rust ならそこんところちゃんとしてるので、この接尾辞は要らない。削除。

### 実行

インタプリタなので、実行部分も書かなきゃいけない。いやまあそんな大したことやってないのでわりとサクッと書けるでしょう。

というか、書籍の方の「プログラミング言語を作る」を見た後に原典の Web 連載の方を見ると、やっぱり「0.2 で構文衝突が起こるから for とか if には予めカッコを要求しよう」とかやってるんですね。まあやっぱり教材作成ってそういうところありますよね。

さて実行部分。

よし、global文以外は実装できたかな。とりあえずコミット。式の実行はまだだけど。

……ん？この reference 実装って continue はどう実装してるの？

## 2022年6月30日 (Day 4)

### continue

あー理解。

```c
StatementResult
crb_execute_statement_list(CRB_Interpreter *inter, LocalEnvironment *env,
                           StatementList *list)
{
    StatementList *pos;
    StatementResult result;

    result.type = NORMAL_STATEMENT_RESULT;
    for (pos = list; pos; pos = pos->next) {
        result = execute_statement(inter, env, pos->statement);
        if (result.type != NORMAL_STATEMENT_RESULT)
            goto FUNC_END;
    }

  FUNC_END:
    return result;
}
```

とあるから、`NORMAL_STATEMENT_RESULT` でないものが帰ってきた時点でブロックの実行がストップするからこれでいいのか。

えーと for 文の第三式はそもそも continue で飛ばされた際も実行されるのが正しいから、ここの部分も OK か。

ん？ `continue;` をした直後に for 文の条件が false になると `CONTINUE_STATEMENT_RESULT` がループ外に漏洩するのでは？

```js
for (j = 0; j < 2; j = j + 1) {
	for (i = 0; i < 2; i = i + 1) {
		print("i.." + i + "\n");
		continue;
		print("foo" + "\n");
	}
	print("bar" + "\n");
}
```

と書いて、`const print = console.log` とでもしてやって JavaScript で実行すると

```
i..0
i..1
bar
i..0
i..1
bar
```

と出る。さてこれを crowbar で実行するとどうなる？

### crowbar をビルドしよう

とりあえず crowbar を走らせないといかん。ということで make してみる。えーと -Werror がついているせいで 

```
crowbar.l:81:9: error: implicitly declaring library function 'isprint' with type 'int (int)'
      [-Werror,-Wimplicit-function-declaration]
```

で落ちてしまうな。とりあえず crowbar.l に `#include <ctype.h>` を足してみる。

さらに落ちるので、`CFLAGS` に ` -Wno-implicit` を加筆して、`$(CC)` としかなっていないところも `$(CC) $(CFLAGS)` に書き換えると、ビルドが通る。あれ？ crowbar が生成されない。

ああ、`$(TARGET)` の最終行のはリンカ呼び出しをしてるだけだから CFLAGS をつけちゃダメなのね。よしできた。

とりあえず FizzBuzz が走ることを確認したので、さっきのコードを試そう。

```
i..0
i..1
i..0
i..1
```

よっしゃ本家実装がバグった！！！！

## 2022年7月2日 (Day 5)

原作者に報告し、直していただけるとのこと。ということで式をexecuteするやつの実装をやっていこう。

### 式の execute

変数の名前解決が地味に面倒で時間がかかった。まだ二項演算子とか関数呼び出しとかが実装できてないけどとりあえずコミット。

二項演算子を実装。あとは関数とネイティブポインタですね。

関数が実装できたはず。ちょっと入り組んでるしこれはテスト書かんとな。

print も書けたと思う。さていきなり FizzBuzz やってみますか！まあ一発で動くとは思ってませんけどね。

れもん「いや〜どこでバグるか楽しみ」

あっ普通に FizzBuzz 動いた。

れもん「まあこれぐらいはバグらずにできてほしい」

問題は変数のスコープとかのやつなんだよな。

さて、動いたらリファクタリング。これ原則。

れもん「スコープチェーンが実装されたらスタックトレースとかほしいかもね」

そうそう、今は実行時エラーが panic! なんだよな。ここ直しておかないとね。たしかあとで try-catch が入るのでその時まで待ってもいいかもだが。

### テストを書く

stdout に print してるのでテストの書き方に困る。Write を素朴に使うと所有権の問題が発生する。さてどうするかな。

とりあえず現実逃避のために clippy する。

まあとりあえずは手動でテストするか。

### ファイル入出力関数

歯を食いしばって全部書いた。ファイルポインタを C と FFI するのがつらい。unsafe まみれ。

てかこれで todo は消えたっぽいな。

## 2022年7月3日 (Day 6)

テストはシェルスクリプトで書くことにした。てか本家実装にテストファイルあるのね。じゃあこれで実験してみるか。

よし、どちらも失敗！まあそうなるよね。

ftest の方は `Invalid operation made using the operator` と書いてある。あ、片方が `null` のときは `==` とか `!=` が許されるっぽい？

ほんまや、ここだけ 

```c
 } else if (left_val.type == CRB_NULL_VALUE
               || right_val.type == CRB_NULL_VALUE) {
        result.type = CRB_BOOLEAN_VALUE;
        result.u.boolean_value
            = eval_binary_null(inter, operator, &left_val, &right_val,
                               left->line_number);
```

と `||` になってる。ソースコードはよく読もうな。

直し、 fileio と stdout の両方を独立にテストするようにしたところ、ftest の方は通った。問題はパースエラーを吐いてる test.crb の方だ。

こういうのは二分探索するに限る。

てかやっぱ行番号が出ないとデバッグつらいな。

ということで、行番号をパーサのエラーに出す改修を入れる。

90行目で落ちてるらしい。見てみよう。

えーと、90行目の次である for 文で落ちているな。さては行番号を 1-indexed じゃなくて 0-indexed にしてるな。はい案の定。

あー、`parse_optional_expression_and_a_token!` マクロがセミコロン以外で正しく動かないバグがあった。なるほどね

えーとあと落ちるのは……はいはい、 double の表示方法の差ね。C はデフォルトで 6 digits の precision で出力する。

よし、テストケース通った！もうこれで ver 0.1 系は完了と言っても大丈夫だろう（フラグ）

## 2022年7月19日 (Day 7)


めちゃめちゃ日にちが空いた。さて、もう crowbar ver 0.2 を目指すとしますか。えーと、

* 配列の導入
* メソッドもどきの呼び出し
* インクリメント・デクリメント
* GC

か。GC が最大の改修で、それ以外はわりと楽そうだな。まずは文法の改修から。

### 文法

あー、そういや

```
expression
        : logical_or_expression
        | postfix_expression ASSIGN expression
```

ってやつがあったな。これやるには「一度 postfix_expression で読んで、その後に ASSIGN がなかったら、さっき読んだのを破棄して logical_or_expression で読み直す」が必要。しかし現状の ParserState でそんな器用なことをするにはどうするの。

えーと現状の実装は

```rust
struct ParserState<'a> {
    tokvec: &'a [(Token, usize)],
}
```

`usize` わかりにくいから `LineNumber` 型を作るか。えっと

```rust
struct ParserState<'a> {
    tokvec: &'a [(Token, LineNumber)],
}
```

だから、単に不変参照をもう一個生やせばいいだけか。

さて、文法の改修はかなりあっけなく完了してしまったな。あ、でも配列リテラル `{1, 2, 3}` とかがまだか。

（というか配列がいわゆる参照型なんだよな crowbar。だからそれもちゃんと実装してやらんとだし。）

まあとりあえずまずは文法。えーと、

```
array_literal
        : LC expression_list RC
        | LC expression_list COMMA RC
        ;

expression_list
        : /* empty */
        | expression
        | expression_list COMMA expression
        ;
```

えーっとつまり、 `{}` とか `{1}` とか `{1,}` とか `{1,2}` とか `{1,2,}` とか、あ、`{,2}` とか `{,2,}` とか `{,2,3,}` とかも合法ってことか。それぞれの意味論どうなるの？いわゆるケツカンマは無視ってことなんだろうけど、頭のカンマも無視なのかな。

とりあえず

```
a = {, 2, 3, 4, 5, 6, 7, };
for (i = 0; i < a.size(); i = i + 1) {
    print("(" + a[i] + ")");
}
print(a);
print("\n");
print("len.." + "abc".length() + "\n");
```

と書いて走らせると……セグフォ！！！

とりあえずケツカンマを実装する。

https://twitter.com/hsjoihs/status/1549175219696181248

を落とし込めばいいので、

```rust
let mut ans = vec![];
loop {
    if matches!(self.tokvec.get(0), Some((Token::RightCurly, _))) {
        self.advance(1);
        break;
    }
    ans.push(self.parse_expression());

    match self.tokvec.get(0) {
        Some((Token::Comma, _)) => {
            self.advance(1);
            continue;
        }
        Some((Token::RightCurly, _)) => {
            self.advance(1);
            break;
        }
        None => panic!("Unexpected end of file encountered while trying to parse an array literal"),
        _ => panic!("Parse error at line {}: Incorrect array literal", line_number.0)
    }
}
Expr::ArrayLiteral(ans)
```

とすればよさそうだな。

さて、文法は実装できてそうだし、とりあえず `execute.rs` 上での実体を全て `todo!()` にして、コミットだけしてしまおう。

あ、左辺が識別子のときの実装は保たないと既存のテストが通らないので、そこはちゃんと対処してコミット。

### 配列・参照・GC

「ver.0.2では、mark-sweep型のGCを実装します。」という書き方になっているということなので、mark-sweepがない限りそれは crowbar ver.0.2 と呼べないということだろう（そうかな？）。ということでやっていく必要がある。

……んー、でも現状ってそもそも明示的な参照カウンタすらないのよね。というのも、`Copy` な値または `String` しか扱ってないので。

しかしもちろん参照型を入れるとなると非自明な管理が必要。ということで、まず `Rc` で正しく実装することを目標にしよう。

（というか、 `Rc` とほぼ同等に使える `Gc` を提供するライブラリがあるっぽいのよね https://github.com/Manishearth/rust-gc ）

さて、`Rc`。

> Shared references in Rust disallow mutation by default, and `Rc` is no exception: you cannot generally obtain a mutable reference to something inside an `Rc`. If you need mutability, put a `Cell` or `RefCell` inside the `Rc`
> https://doc.rust-lang.org/stable/std/rc/index.html

ということで、 `Rc<RefCell<T>>` を使うことになる。

`Value` の定義に `Array(Rc<RefCell<Vec<Value>>>),` という可能性を増やしてやればいい感じかな。

えーと配列自体の pretty print は `(1, 2, 3, 4, 5, 6, 7, 8)` みたいにやるのね。

### Windows 対応

Windows で走らせたらコケた。 `"\r\n"` 周りですね。ということで、 `\r` を whitespace 扱いにすることでとりあえず解決。

### バグ修正

バグというわけではないけど、ループの中で return すべきなのに return せずに必ず None を返してる関数があったので、対処

### 配列・参照 (part 2)

とりあえず配列の読み書きは実装した。読み取りのテストを書いた。メソッドもどきはまだ。

参照型にちゃんとなってるかを試そうとしたら、return 抜けで書き込みに失敗してることが判明。修正。これもテストを追記し、解決。

メソッドもどきも実装した。

インクリメントとデクリメントも実装できたはず。さてテストを走らせよう。

……ん？実行が異様に遅い。これは無限ループとか踏んでないか？こういう無限ループはインクリメントのミスとかでしょ（Cコンパイラ自作時にまさに同様の経験をしたので）

ああはい、配列の方にだけインクリメントを実装してて、識別子へのアクセスのときに実装してなかった。それはそう。

それはそれとしてテストが落ちる。えーと、「Runtime error: Cannot find a function named `new_array`」

そうでした。すっかり忘れてた。とりあえず `new_array` を削ったテストケースが通ることを確認。

えーと、単一引数だけなら `Value::Array(Rc::new(RefCell::new(vec![Value::Null; len])))` でいいんだけど。

単純に `Value::Array(Rc::new(RefCell::new(vec![new_array(rest); *len])))` と再帰呼び出しすると `Rc<RefCell<T>>` の `.clone()` が呼ばれてしまって同じ配列を参照してしまうのでダメで（一敗）、

```rust
let mut vec = vec![];
vec.resize_with(*len, || new_array(rest));
Value::Array(Rc::new(RefCell::new(vec)))
```

と `.resize_with()` で処理してやれば問題ないはず。

よーしテスト通った。これにて ver.0.2 完了です。

## 2022年7月20日 (Day 8)

### ver.0.3

Rust で実装しており、全てを UTF-8 で動かしているという事情がある以上、Unicode 周りについてどうするかを考える必要がある。

改修量自体は小さくなるので、「エラーメッセージを改善して本家側に寄せる」というのも今回の目標に組み込もう。

本家実装においては、ver.0.3 は「length の挙動が変化してる」「変なマルチバイト文字に遭遇したらエラーを出す」ぐらいしかしてないので、こちらの改修も小さめでいいんだよな。あ、lexer を普通に char へのイテレータベースへと書き換える作業があったわね。

とりあえず lexer を改善。

## 2022年7月24日 (Day 9)

### ver.0.3 (part 2)

とりあえず `.length()` の実装をいじってしまおう。`.chars().count()` に変更。

そしてエラーメッセージ。`self.throw_runtime_error` のやつはとりあえず本家実装に合致させたけど、ネイティブ関数内についてはまだ対応できていない

## 2022年7月25日 (Day 10)

### ver.0.3 (part 2)

エラーメッセージを本家実装に寄せていっているが、`DIVISION_BY_ZERO_ERR` が定義されているにも関わらず呼ばれてないな。

`UNEXPECTED_WIDE_STRING_ERR` も同様。とりあえずこいつらは今は無視するか

さて行番号も出力すべきで、これについては本家実装は「行番号は、解析木中の『文』と『式』が保持しています」（書籍 p.100）と言っている。

えーと、 `crb_alloc_expression` の中で行番号を読んでるっぽい。

```
{}
-
3;
```

に対しては `  1:2項演算子-のオペランドの型が不正です。` となり、

```
{
    
}
-
3;
```

に対しては `  3:2項演算子-のオペランドの型が不正です。` と返ってくる。そして

```
3
-
{};
```

に対しては `  1:2項演算子-のオペランドの型が不正です。` と返ってくるのか。うーん。

行番号まで一致させるのは正直不毛なんだよな。まあとりあえずは行番号が出れば良しとするか。というか、そもそも「行番号」で出すのではなく「どこからどこまで」のspanとして持ったほうが応用が効くんだけど。たしか Rui さんもそんなこと言ってた気がする。

とりあえず行番号をつけてまわるが、次のトークンの先頭の行番号を読む仕組みだと末尾で落ちるんだよな。じゃあ `.advance()` を呼んだタイミングでメンバにセットするようにしておこう。

tests.rs は一旦消して、実行すると、はいちゃんとテストが全て通る。

ネイティブ関数内でもちゃんとエラーを投げるようにする。

しかし、実行時エラーを投げるときの関数が `MutableEnvironment::throw_runtime_error(line_number, RuntimeError::ほにゃらら)` なの気に入らないな。`RuntimeError::ほにゃらら.thrown_at(line_number)` にしたい。しよう。

変更した。
