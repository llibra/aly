(in-package :cl-user)

(defpackage :aly.util
  (:use :cl)
  (:import-from :alexandria :with-gensyms)
  (:export :defalias :intersperse))

(defpackage :aly.stream
  (:use :cl)
  (:import-from :anaphora :aif :it)
  (:export :make-parser-stream :parser-stream :parser-stream-car
           :parser-stream-cdr))

(defpackage :aly.condition
  (:use :cl :aly.util :aly.stream)
  (:export :parser-error :parser-error-stream :parser-error-position
           :parser-error-expected))

(defpackage :aly.core
  (:use :cl :aly.util :aly.stream :aly.condition)
  (:import-from :alexandria :with-gensyms :flatten)
  (:import-from :5pm :match :ematch :ematch-values :guard)
  (:export :success :failure :satisfy :unit :fail :bind :mlet1 :mlet* :seq :seq1
           :seqn :seq/bind :choice :try :expect :many :skip-many :eoi :parse))

(defpackage :aly.combinator
  (:use :cl :aly.core)
  (:export :sep-by :sep-by1 :many1 :skip-many1 :end-by :end-by1 :times :between
           :many-till :chainl1 :chainl :chainr1 :chainr :not-followed-by))

(defpackage :aly
  (:use :cl :aly.condition :aly.core :aly.combinator)
  (:export :parser-error

           :unit :fail :bind :mlet1 :mlet* :seq :seq1 :seqn :seq/bind :choice
           :try :expect :many :skip-many :eoi :parse

           :sep-by :sep-by1 :many1 :skip-many1 :end-by :end-by1 :times :between
           :many-till :chainl1 :chainl :chainr1 :chainr :not-followed-by))

(defpackage :aly.char
  (:use :cl :aly.util :aly.core :aly.combinator)
  (:import-from :alexandria :curry :rcurry)
  (:import-from :5pm :ematch-values)
  (:export :specific-char :specific-string :one-of :none-of :any-char :upper
           :lower :letter :alpha-num :digit :decimal-digit :hex-digit :oct-digit
           :newline :tab :whitespace :whitespaces))
