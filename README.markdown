aly - Trivial Lisp parser combinator library
============================================

What is this?
-------------

Aly is a parser combinator library. It's inspired by the technical talk by
Shikano Keiichirou in Shibuya.lisp TT #6. Uncompleted and experimental. Lazy
stream is based on the idea from PEG module of Gauche.

パーサコンビネータライブラリです。Shibuya.lisp TT #6の鹿野さんの「多値で簡単パー
サーコンビネーター」に影響されて作りました。遅延ストリームのアイデアは Gauche の
PEGモジュールからいただきました。

Since aly is under development, it lacks some features. And without any
optimization. It's slow.

まだ作りかけなので、欠けている機能が多いです。最適化もしていないので、速度も遅い
です。

Usage
-----

At first, need to load aly.

最初にalyを読み込みます。

    (asdf:load-system :aly)
    (defpackage :aly.demo (:use :cl :aly :aly.char))
    (in-package :aly.demo)

Next, definition of parsers. A parser takes stream as argument, and returns
a result as multiple values.

パーサを定義します。パーサはストリームを引数に取り、結果を多値で返します。

    (setf (symbol-function 'quoted)
      (many (choice (none-of #\")
                    (seqn (specific-char #\")
                          (specific-char #\")))))
    
    (setf (symbol-function 'field)
      (choice (seq/bind (specific-char #\")
                        (x <- #'quoted)
                        (specific-char #\")
                        (pure (coerce x 'string)))
              (seq/bind (x <- (many (none-of #\, #\Newline)))
                        (pure (coerce x 'string)))))
    
    (setf (symbol-function 'record)
      (sep-by #'field (specific-char #\,)))
    
    (setf (symbol-function 'csv)
      (seq/bind (x <- #'record)
                (y <- (many (seqn (specific-char #\Newline) #'record)))
                (pure (cons x y))))

Let's run parser.

パーサを実行します。

    (parse #'csv (format nil "a,b,c~%d,e,f~%g,h,i"))
    ;;=> (("a" "b" "c") ("d" "e" "f") ("g" "h" "i")), NIL

No documentation yet. Please refer to tests in t/ for how to use each functions.

ドキュメントがないので、それぞれの関数の使い方については、t/以下にあるテストを見
てください。
