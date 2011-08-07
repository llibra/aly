(in-package :cl-user)

(defpackage :aly.util
  (:use :cl)
  (:import-from :alexandria :with-gensyms)
  (:import-from :cl-pattern :match)
  (:export :defalias :result-match :intersperse))

(defpackage :aly.stream
  (:use :cl)
  (:import-from :anaphora :aif :it)
  (:export :make-parser-stream :parser-stream :parser-stream-car
           :parser-stream-cdr))

(defpackage :aly.condition
  (:use :cl)
  (:export :parser-error))

(defpackage :aly.core
  (:use :cl :aly.util :aly.stream :aly.condition)
  (:import-from :alexandria :with-gensyms)
  (:import-from :cl-pattern :match)
  (:export :success :failure :satisfy :unit :fail :bind :seq :seq1 :seqn
           :seq/bind :choice :try :expect :many :skip-many :eof :parse))

(defpackage :aly.combinator
  (:use :cl :aly.core)
  (:export :sep-by :sep-by1 :many1 :skip-many1))

(defpackage :aly
  (:use :cl :aly.condition :aly.core :aly.combinator)
  (:export :parser-error

           :unit :fail :bind :seq :seq1 :seqn :seq/bind :choice :try :expect
           :many :skip-many :eof :parse

           :sep-by :sep-by1 :many1 :skip-many1))

(defpackage :aly.char
  (:use :cl :aly.util :aly.core :aly.combinator)
  (:import-from :alexandria :curry :rcurry)
  (:export :specific-char :specific-string :one-of :none-of :any-char :upper
           :lower :letter :alpha-num :digit :decimal-digit :hex-digit :oct-digit
           :newline :tab :space :spaces))
