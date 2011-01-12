aly - Trivial Lisp parser combinator library
============================================

What is this?
-------------

Aly is parser combinator library. It's inspired by technical talk by Shikano
Keiichirou in Shibuya.lisp TT #6. Uncompleted and experimental. Lazy stream is
based on the idea from PEG module of Gauche.

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

Next, definition of parsers. A parser takes stream as argument, and returns
result and next stream as multiple values.

パーサを定義します。パーサはストリームを引数に取り、結果と処理後のストリームを多
値で返します。

    (defun additive ()
      (flet ((f1 (stream)
               (aly:parse stream
                 (x <- (multitive))
                 (aly:specific-char #\+)
                 (y <- (additive))
                 (aly:result (+ x y))))
             (f2 (stream)
               (aly:parse stream
                 (x <- (multitive))
                 (aly:specific-char #\-)
                 (y <- (additive))
                 (aly:result (- x y)))))
        (aly:choice (aly:try #'f1)
                    (aly:try #'f2)
                    (multitive))))
    
    (defun multitive ()
      (flet ((f1 (stream)
               (aly:parse stream
                 (x <- (primary))
                 (aly:specific-char #\*)
                 (y <- (multitive))
                 (aly:result (* x y))))
             (f2 (stream)
               (aly:parse stream
                 (x <- (primary))
                 (aly:specific-char #\/)
                 (y <- (multitive))
                 (aly:result (/ x y)))))
        (aly:choice (aly:try #'f1)
                    (aly:try #'f2)
                    (primary))))
    
    (defun primary ()
      (flet ((f (stream)
               (aly:parse stream
                 (aly:specific-char #\()
                 (r <- (additive))
                 (aly:specific-char #\))
                 (aly:result r))))
        (aly:choice (aly:try #'f) (decimal))))
    
    (defun decimal ()
      (lambda (stream)
        (aly:parse stream
          (c <- (aly:digit))
          (aly:result (read-from-string (princ-to-string c))))))

Let's run parser.

パーサを実行します。

    (aly:parse "1+2" (additive))
    ;; =>  3, NIL
    (aly:parse "4*2-((3+1)-(9/3))*2" (additive))
    ;; =>  6, NIL
    (aly:parse "4**2-3/5" (additive))
    ;; >>  Error

No documentation yet. Please refer to tests in t/ for how to use each functions.

ドキュメントがないので、それぞれの関数の使い方については、t/以下にあるテストを見
てください。
