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

