(defpackage :aly
  (:use :cl)
  (:import-from :alexandria :curry :rcurry :with-gensyms :once-only)
  (:import-from :anaphora :aif :it)
  (:import-from :fare-matcher :match)
  (:export :make-parser-stream
           :parser-stream/string
           :parser-stream
           :parser-stream-car
           :parser-stream-cdr

           :parser-error
           :parser-error-stream
           :failure
           :failure-datum
           :failure-position
           :simple-failure
           :failure-control
           :failure-arguments
           :failure/unexpected
           :failure/expected
           :failure-expected

           :parse
           :bind
           :result
           :choice
           :fail
           :fail/unexpected
           :try
           :expect
           :many
           :skip-many

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
