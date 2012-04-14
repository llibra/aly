Aly - A simple Lisp parser combinator library
=============================================

What is this?
-------------

Aly is a parser combinator library. It's inspired by the technical talk by
Shikano Keiichirou in Shibuya.lisp TT #6. The lazy stream implementation of aly
is based on the idea from PEG module of Gauche (an R5RS Scheme implementation).

パーサコンビネータライブラリです。Shibuya.lisp TT #6の鹿野さんの「多値で簡単パー
サーコンビネーター」に影響されて作りました。遅延ストリームのアイデアは Gauche の
PEGモジュールからいただきました。

Since aly is under development, it lacks some features and it's not optimized
yet. Probably slow.

まだ作りかけなので、欠けている機能が結構あります。最適化もまだしていません。おそ
らく速度も遅いと思います。

License
-------

It's licensed under the MIT license.

Requirements
------------

* [Alexandria](http://common-lisp.net/project/alexandria/)
* [Anaphora](http://common-lisp.net/project/anaphora/)
* [CL-PATTERN](https://github.com/arielnetworks/cl-pattern)

Install
-------

To obtain source code, run following commands in your shell:

ソースコードを入手するために、シェルで以下のコマンドを実行してください。

    % cd ~/quicklisp/local-projects
    % hg clone https://bitbucket.org/llibra/aly

Alternatively you can do it without Mercurial:

Mercurialを使わなくても手に入れることができます。

    % cd ~/quicklisp/local-projects
    % curl https://bitbucket.org/llibra/aly/get/default.tar.bz2 | bzcat | tar xf -

Then, run your Lisp implementation and evaluate a following form:

そうしたら、Lispの処理系を起動して以下の式を評価します。

    > (ql:quickload :aly)

It's all done.

完了です。

Usage
-----

At first, we need to load aly.

最初にalyを読み込みます。

    (asdf:load-system :aly) ; or (ql:quickload :aly)
    (defpackage :aly.demo (:use :cl :aly :aly.char))
    (in-package :aly.demo)

Next, define parsers. A parser takes a stream as only one argument, and returns
a result as multiple values.

パーサを定義します。パーサはストリームを唯一の引数として受け取り、結果を多値で返
します。

    (setf (symbol-function 'quoted)
      (many (choice (none-of #\")
                    (seqn (specific-char #\")
                          (specific-char #\")))))
    
    (setf (symbol-function 'field)
      (choice (mlet* ((_ (specific-char #\"))
                      (x #'quoted)
                      (_ (specific-char #\")))
                (unit (coerce x 'string)))
              (mlet1 x (many (none-of #\, #\newline))
                (unit (coerce x 'string)))))
    
    (setf (symbol-function 'record)
      (sep-by #'field (specific-char #\,)))
    
    (setf (symbol-function 'csv)
      (mlet* ((x #'record)
              (y (many (seqn (specific-char #\Newline) #'record))))
        (unit (cons x y))))

Let's run the parser.

パーサを実行します。

    (parse #'csv (format nil "a,b,c~%d,e,f~%g,h,i"))
    ;;=> (("a" "b" "c") ("d" "e" "f") ("g" "h" "i")), NIL

API
---

### Core

#### Function: unit x

Returns a parser that always suceeds with value `x` without consuming any input.

値`x`をともなって必ず成功するパーサを返します。このパーサは入力を消費しません。

#### Function: fail msg

Returns a parser that always fails with the `msg` error message without
consuming any input.

メッセージ`msg`をともなって必ず失敗するパーサを返します。 このパーサは入力を消費
しません。

#### Function: bind parser fn

Returns a parser for sequencing. The parser first applies `parser` and applies
`fn` to the result value of `parser`. Then it applies the parser returned from
`fn` and returns the result.

順番に実行するためのパーサを返します。パーサは最初に`parser`を適用し、`parser`の
結果の値に`fn`を適用します。次に、`fn`が返すパーサを適用し、その結果を返します。

#### Macro: mlet1 var form &body body

A syntax sugar of `bind`. Returns a parser that binds `var` to the result value
of `form` and executes `body`. If `var` is `_`, the returned value of `form` is
just discarded.

`bind`の構文糖です。 `form`の結果の値で`var`を束縛してから`body`を実行します。も
し`var`が`_`なら、`form`から返される値は単に捨てられます。

#### Macro: mlet* bindings &body body

A multiple and sequencial version of `mlet1`.

複数の束縛を順番に作る`mlet1`です。

### Function: seq &rest parsers

Returns a parser that sequentially applies each parser in `parsers` and returns
the list of the result values on success.

`parsers`のそれぞれのパーサを順番に適用し、 成功すると結果の値を返すパーサを返し
ます。

#### Function: seq1 parser1 &rest parsers

`seq1` is similar to `seq`, but requires at least one parser and the returned
parser returns the result of `parser1` on success.

`seq1`は`seq`に似ていますが、 少なくともひとつのパーサを必要とし、返されたパーサ
が成功すると`parser1`の結果を返します。

#### Function: seqn &rest parsers

`seqn` is similar to `seq`, but the returned parser returns the result of the
last parser in `parsers` on success.

`seqn`は`seq`に似ていますが、返されたパーサが成功すると`parsers`の最後のパーサの
結果を返します。

#### Macro: seq/bind &rest parsers

Haskell's `do` notation. Another syntax sugar of `bind`.

Haskellの`do`記法です。`bind`のもうひとつの構文糖です。

#### Function: choice &rest parsers

Returns a parser that sequentially applies each parser in `parsers` until one of
them succeeds. The parser does not backtrack implicitly. It returns the result
of the successful parser.

どれかが成功するまで、 `parsers`のそれぞれのパーサを順番に適用するパーサを返しま
す。パーサは暗黙のうちにバックトラックしたりはしません。成功したパーサの結果を返
します。

#### Function: try parser

Returns a parser that is equivalent to `parser` except the behavior on failure.
The parser consumes no input on failure.

失敗したときの動作以外は`parser`と等しいパーサを返します。パーサは失敗しても入力
を消費しません。

#### Function: expect parser x

Returns a parser that is equivalent to `parser` except the behavior on failure
without consuming any input. If the parser fails without consuming any input,
`x` is used as a part of the error message instead of the original one.

入力を消費しないで失敗したときの動作以外は`parser`と等しいパーサを返します。もし
パーサが入力を消費せずに失敗した場合、 元のものの代わりに`x`がエラーメッセージの
一部として使われます。

#### Function: many parser

Returns a parser that applies `parser` zero or more times. The parser returns a
list of the result values on success.

`parser`を0回以上適用するパーサを返します。 成功するとパーサは結果の値のリストを
返します。

#### Function: skip-many parser

Returns a parser that applies `parser` zero or more times. The result values is
just discarded. Thereby the parser always returns `nil`.

`parser`を0回以上適用するパーサを返します。 結果の値は単に捨てられ、パーサは常に
`nil`を返します。

#### Function: eoi stream

This is not a parser generator but a parser. Succeeds at the end of the input.

パーサジェネレータではなくてパーサです。入力が終端に達したときに成功します。

#### Function: parse parser input &key (parser-error-p t)

Runs `parser` with `input` and returns the result value. `input` is assumed as a
parser stream. If `parser-error-p` is true, signals an error on failure.
Otherwise returns `nil` on failure.

`input`に対して`parser`を実行して結果の値を返します。`input`はパーサストリームで
す。もしも`parser-error-p`が真なら、パーサが失敗したときにエラーを発生させます。
それ以外の場合、失敗すると`nil`を返します。

### Combinators

#### Function: sep-by parser sep

Returns a parser that expects zero or more sequences of `parser` separated by
`sep`. The parser returns a list of the result values of `parser` on success.
The result values of `sep` is discarded.

`sep`で区切られた`parser`の0回以上の繰り返しを期待するパーサを返します。成功する
とパーサは `parser`の結果の値のリストを返します。`sep`の結果の値は捨てられます。

#### Function: sep-by1 parser sep

Returns a parser that expects one or more sequences of `parser` separated by
`sep`. The parser returns a list of the result values of `parser` on success.
The result values of `sep` is discarded.

`sep`で区切られた`parser`の1回以上の繰り返しを期待するパーサを返します。成功する
とパーサは`parser`の結果の値のリストを返します。 `sep`の結果の値は捨てられます。

#### Function: many1 parser

Returns a parser that applies `parser` one or more times. The parser returns a
list of the result values on success.

`parser`を1回以上適用するパーサを返します。 成功するとパーサは結果の値のリストを
返します。

#### Function: skip-many1 parser

Returns a parser that applies `parser` one or more times. The result values is
just discarded. Thereby the parser always returns `nil` as the result value.

`parser`を1回以上適用するパーサを返します。 結果の値は単に捨てられ、パーサは結果
の値として常に`nil`を返します。

#### Function: end-by parser sep

Returns a parser that expects zero or more sequences of `parser` separated and
ended by `sep`. The parser returns a list of the result values of `parser` on
success. The result values of `sep` is discarded.

`sep`で区切られ、`sep`で終了する、 `parser`の0回以上の繰り返しを期待するパーサを
返します。成功するとパーサは`parser`の結果の値のリストを返します。 `sep`の結果の
値は捨てられます。

#### Function: end-by1 parser sep

Returns a parser that expects one or more sequences of `parser` separated and
ended by `sep`. The parser returns a list of the result values of `parser` on
success. The result values of `sep` is discarded.

`sep`で区切られ、`sep`で終了する、 `parser`の1回以上の繰り返しを期待するパーサを
返します。成功するとパーサは`parser`の結果の値のリストを返します。 `sep`の結果の
値は捨てられます。

#### Function: times parser n

Returns a parser that applies `parser` `n` times and returns a list of the
result values of `parser` on success.

`parser`を`n`回適用し、 成功すると`parser`の結果の値のリストを返すパーサを返しま
す。

#### Function: between open parser close

Returns a parser that expects a sequence of `open`, `parser` and `close`. The
parser returns the result of `parser` on success.

`open`と`parser`と`close`の並びを期待するパーサを返します。成功するとパーサは
`parser`の結果を返します。

#### Function: many-till parser end

Returns a parser that applies `parser` zero or more times until `end` succeeds.
The parser returns a list of the result values of `parser` on success.

`parser`を`end`が成功するまで0回以上適用するパーサを返します。成功するとパーサは
`parser`の結果の値のリストを返します。

#### Function: not-followed-by parser

Returns a parser that succeeds when `parser` fails. The parser does not consume
any input.

`parser`が失敗したときに成功するパーサを返します。パーサは入力を消費しません。

### Character parsers

#### Function: specific-char c

Expects a single character equals `c` and returns `c` on success.

`c`と等しい文字を期待します。成功すると`c`を返します。

#### Function: specific-string string

Expects a string equals `string` and returns `string` on success.

`string`と等しい文字列を期待します。成功すると`string`を返します。

#### Function: one-of &rest cs

Expects one of the characters in `cs` and returns the parsed character on
success.

`cs`の文字のひとつを期待します。成功すると解析された文字を返します。

#### Function: none-of &rest cs

Expects none of the characters in `cs` and returns the parsed character on
success.

`cs`に含まれない文字を期待します。成功すると解析された文字を返します。

#### Function: any-char stream

Succeeds for any character and returns the parsed character on success.

すべての文字に対して成功します。成功すると解析された文字を返します。

#### Function: upper stream

Expects an uppercase letter and returns the parsed character on success.

大文字を期待します。成功すると解析された文字を返します。

#### Function: lower stream

Expects a lower case letter and returns the parsed character on success.

小文字を期待します。成功すると解析された文字を返します。

#### Function: letter stream

Expects a letter and returns the parsed character on success.

文字を期待します。成功すると解析された文字を返します。

#### Function: alpha-num stream

Expects a letter or digit and returns the parsed character on success.

文字か数字を期待します。成功すると解析された文字を返します。

#### Function: digit &optional (radix 10)

Returns a parser that expects a digit and returns the parsed character on
success.

数字を期待し、成功すると解析された文字を返すパーサを返します。

#### Function: decimal-digit stream

Expects a decimal digit and returns the parsed character on success.

10進数の数字を期待します。成功すると解析された文字を返します。

#### Function: hex-digit stream

Expects a hex digit and returns the parsed character on success.

16進数の数字を期待します。成功すると解析された文字を返します。

#### Function: oct-digit stream

Expects a octal digit and returns the parsed character on success.

8進数の数字を期待します。成功すると解析された文字を返します。

#### Function: newline stream

Expects a newline (#\newline) and returns it on success.

改行（#\newline）を期待し、成功すると改行を返します。

#### Function: tab stream

Expects a tab (#\tab) and returns it on success.

タブ（#\tab）を期待し、成功するとタブを返します。

#### Function: whitespace stream

Expects a whitespace (#\space, #\page, #\tab and #\newline) and returns the
parsed character on success.

ホワイトスペース（#\space、#\page、#\tab、#\newline）を期待します。 成功すると解
析された文字を返します。

#### Function: whitespaces stream

Skips zero or more whitespace.

0個以上のホワイトスペースをスキップします。

Implementation strategy
-----------------------

* For performance, parsers return not structures or CLOS objects but
  multiple values. The implementations like SBCL and CCL, which use stack for
  handling of multiple values, probably compile it to more efficient code.

* 性能のために、パーサはCLOSオブジェクトや構造体でなく多値を返します。SBCLやCCL
  のように、スタックを使って多値を処理する処理系では、より効率的なコードにコンパ
  イルされるはずです。

* Parsers use simple lazy streams as input data. There are two reasons: a) to
  prevent converting large data at a time and b) to avoid a pain due to side
  effects and unnecessary copy of object. This approach has some disadvantages.
  The most important one is amount of memory consumption. On 32-bit systems,
  n * 8 bytes (n = the number of tokens) are required additionally. On 64-bit
  systems, it's n * 16 bytes.

* パーサは入力データとして単純な遅延ストリームを利用します。これにはふたつの理由
  があり、a)一度に大きなデータを変換するのを防ぐためと、b)副作用による面倒や不要
  なオブジェクトのコピーを避けるためです。このアプローチにはデメリットがあり、最
  も重大なものはメモリの消費量です。32ビットシステムではn * 8バイト（n＝トークン
  の数）多く必要になり、64ビットシステムではn * 16バイトです。

* The position of a token is not calculated until it is required. Thereby the
  calculation amount when a parsing error occurs increases.

* トークンの位置は必要になるまで計算されません。そのため、パースエラーが起きたと
  きの計算量が増えています。
