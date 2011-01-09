(defpackage :aly
  (:use :cl)
  (:import-from :alexandria :curry :rcurry :with-gensyms)
  (:import-from :anaphora :aif :it)
  (:export :make-parser-stream
           :parser-stream/string
           :parser-stream
           :parser-stream-car
           :parser-stream-cdr

           :parser-error
           :simple-parser-error
           :unexpected-datum
           :end-of-stream

           :with-no-consumption/failure
           :with-context
           :with-expected-label
           
           :return-result
           :sequence
           :choice
           :fail
           :try
           :unexpected

           :many
           :many1

           :parse

           ;:one-of
           ;:none-of
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
