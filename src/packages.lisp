(defpackage :aly
  (:use :cl :iterate)
  (:import-from :alexandria :curry :rcurry :with-gensyms :once-only :ensure-car)
  (:import-from :anaphora :aif :it)
  (:import-from :cl-pattern :match)
  (:export :make-parser-stream
           :parser-stream/string
           :parser-stream
           :parser-stream-car
           :parser-stream-cdr

           :parser-error

           :define-parser

           :parse
           :bind
           :seq
           :seq1
           :seqn
           :seq/bind
           :unit
           :choice
           :fail
           :fail/unexpected
           :try
           :expect
           :many
           :skip-many
           :eof

           :sep-by
           :sep-by1
           :many1
           :skip-many1

           :one-of
           :none-of
           :satisfy
           :specific-char
           :specific-string
           :any-char
           :upper
           :lower
           :letter
           :alpha-num
           :digit
           :hex-digit
           :oct-digit
           :newline
           :tab
           :space
           :spaces))
