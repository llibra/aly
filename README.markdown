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

Usage
-----

At first, we need to load aly.

最初にalyを読み込みます。

    (asdf:load-system :aly)
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
      (choice (seq/bind (specific-char #\")
                        (x <- #'quoted)
                        (specific-char #\")
                        (unit (coerce x 'string)))
              (seq/bind (x <- (many (none-of #\, #\Newline)))
                        (unit (coerce x 'string)))))
    
    (setf (symbol-function 'record)
      (sep-by #'field (specific-char #\,)))
    
    (setf (symbol-function 'csv)
      (seq/bind (x <- #'record)
                (y <- (many (seqn (specific-char #\Newline) #'record)))
                (unit (cons x y))))

Let's run the parser.

パーサを実行します。

    (parse #'csv (format nil "a,b,c~%d,e,f~%g,h,i"))
    ;;=> (("a" "b" "c") ("d" "e" "f") ("g" "h" "i")), NIL

There is no documentation yet. The tests in t/ will help you know how to use.

まだドキュメントがないので、使い方は、t/以下にあるテストを見てください。

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
