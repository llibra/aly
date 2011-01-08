(defpackage :parser
  (:use :cl)
  (:import-from :alexandria :curry :rcurry :with-gensyms)
  (:import-from :anaphora :aif :it)
  (:export :make-parser-stream
           :string->parser-stream
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

(in-package :parser)

;;;; Stream

;;; Lazy parser stream is based on the idea from PEG module of Gauche.
;;;
;;; <parser-stream> : nil | (<token> . <stream-or-generator>) 
;;; <stream-or-generator> : <parser-stream> | <generator>

(defun make-parser-stream (generator)
  (cons (funcall generator) generator))

(defun string->parser-stream (string)
  (let ((in (make-string-input-stream string)))
    (flet ((f ()
             (let ((c (read-char in nil)))
               (unless c (close in))
               c)))
      (make-parser-stream #'f))))

(declaim (inline parser-stream-car))

(defun parser-stream-car (stream)
  (car stream))

(defun parser-stream-cdr (stream)
  (cond ((functionp (cdr stream))
         (aif (funcall (cdr stream))
              (setf (cdr stream) (cons it (cdr stream)))
              (setf (cdr stream) nil)))
        (t
         (cdr stream))))

;;;; Conditions

(define-condition parser-error (error)
  ((stream :initarg :stream :accessor parser-error-stream)))

(define-condition simple-parser-error (parser-error)
  ((control :initarg :control :reader simple-parser-error-control)
   (arguments :initarg :arguments :reader simple-parser-error-arguments))
  (:report (lambda (c s)
             (apply #'format s
                    (simple-parser-error-control c)
                    (simple-parser-error-arguments c)))))

(define-condition unexpected-datum (parser-error)
  ((expected :initform nil
             :initarg :expected
             :accessor unexpected-datum-expected)
   (unexpected :initform nil
               :initarg :unexpected
               :accessor unexpected-datum-unexpected)
   (datum :initform nil
          :initarg :datum
          :reader unexpected-datum-datum))
  (:report (lambda (c s)
             (assert (not (and (unexpected-datum-expected c)
                               (unexpected-datum-unexpected c))))
             (if (unexpected-datum-expected c)
                 (format s "Parser is expecting ~S, but got ~S."
                         (unexpected-datum-expected c)
                         (unexpected-datum-datum c))
                 (format s "Parser is not expecting ~S, but got ~S."
                         (unexpected-datum-unexpected c)
                         (unexpected-datum-datum c))))))

(define-condition end-of-stream (parser-error)
  ()
  (:report (lambda (c s)
             (format s "Parser encountered end of stream on ~S."
                     (parser-error-stream c)))))

(defun parser-error (stream condition &rest arguments)
  (if (stringp condition)
      (error 'simple-parser-error
             :stream stream :control condition :arguments arguments)
      (apply #'error condition :stream stream arguments)))

;;;; Macros

(defmacro with-no-consumption/failure ((stream) &body body)
  (with-gensyms (condition)
    `(handler-case
         (progn ,@body)
       ((or unexpected-datum end-of-stream) (,condition)
         (setf (parser-error-stream ,condition) ,stream)
         (error ,condition)))))

(defmacro with-context ((var stream) (&rest bindings) &body body)
  (labels ((rec (rest)
             (if rest
                 `(multiple-value-bind (,(caar rest) ,var)
                      (funcall ,@(cdar rest) ,var)
                    (declare (ignorable ,var))
                    ,(rec (cdr rest)))
                 `(progn ,@body))))
    `(let ((,var ,stream))
       (declare (ignorable ,var))
       ,(rec bindings))))

(defmacro with-expected-label ((label) &body body)
  (with-gensyms (condition)
    `(handler-case (progn ,@body)
       (unexpected-datum (,condition)
         (setf (unexpected-datum-unexpected ,condition) nil)
         (setf (unexpected-datum-expected ,condition) ,label)
         (error ,condition)))))

;;;; Primitives

(defun return-result (x)
  (lambda (stream)
    (values x stream)))

(defun sequence (&rest parsers)
  (lambda (stream)
    (labels ((rec (rest stream)
               (if (cdr rest)
                   (multiple-value-bind (_ next-stream)
                       (funcall (car rest) stream)
                     (declare (ignore _))
                     (rec (cdr rest) next-stream))
                   (funcall (car rest) stream))))
      (if parsers
          (rec parsers stream)
          (funcall (fail) stream)))))

(defun choice (&rest parsers)
  (lambda (stream)
    (labels ((rec (rest stream)
               (if rest
                   (handler-case (funcall (car rest) stream)
                     (parser-error (c)
                       (if (eq stream (parser-error-stream c))
                           (rec (cdr rest) stream)
                           (error c))))
                   (funcall (fail) stream))))
      (rec parsers stream))))

(defun fail (&optional (ctrl "Parser failed.") &rest args)
  (lambda (stream)
    (apply #'parser-error stream ctrl args)))

(defun try (parser)
  (lambda (stream)
    (with-no-consumption/failure (stream)
      (funcall parser stream))))

(defun unexpected (x)
  (lambda (stream)
    (parser-error stream 'unexpected-datum :unexpected x)))

(defun many (parser)
  (lambda (stream)
    (labels ((rec (stream acc)
               (handler-case
                   (rec (parser-stream-cdr stream)
                        (cons (funcall parser stream) acc))
                 (parser-error (c)
                   (declare (ignore c))
                   acc))))
      (nreverse (rec stream nil)))))

(defun many1 (parser)
  (lambda (stream)
    (with-context (s stream)
        ((r parser))
      (cons r (funcall (many parser) s)))))

;;; TODO: fix ad hoc
(defun parse (parser stream)
  (let ((stream (string->parser-stream stream)))
    (values (funcall parser stream)
            stream)))

;;;; Character

(defun satisfy (pred)
  (lambda (stream)
    (unless stream
      (error 'end-of-stream :stream stream))
    (let ((datum (parser-stream-car stream)))
      (if (funcall pred datum)
          (values datum (parser-stream-cdr stream))
          (error 'unexpected-datum :stream stream :datum datum)))))

(defun specific-char (c)
  (satisfy (curry #'eql c)))

;;; TODO: think about stream position on failure
(defun specific-string (string)
  (lambda (stream)
    (with-no-consumption/error (stream)
      (values string
              (reduce (lambda (s x)
                        (funcall (specific-char x) s)
                        (parser-stream-cdr s))
                      string
                      :initial-value stream)))))

(defun any-char ()
  (satisfy (constantly t)))

(defun upper ()
  (satisfy #'upper-case-p))

(defun lower ()
  (satisfy #'lower-case-p))

(defun letter ()
  (satisfy #'alpha-char-p))

(defun alpha-num ()
  (satisfy #'alphanumericp))

(defun digit (&optional (radix 10))
  (satisfy (rcurry #'digit-char-p radix)))

(defun hex-digit ()
  (satisfy (rcurry #'digit-char-p 16)))

(defun oct-digit ()
  (satisfy (rcurry #'digit-char-p 8)))

(defun newline ()
  (specific-char #\Newline))

(defun tab ()
  (specific-char #\Tab))

(defun space ()
  (satisfy (lambda (c)
             (some (curry #'eql c)
                   '(#\Space #\Page #\Tab #\Newline)))))

(defun spaces ()
  'need-implementation)

;;;;

(defun empty (stream)
  (declare (ignore stream))
  nil)

(defun not-brace (stream)
  (funcall
   (make-parser
    (complement (lambda (x)
                  (or (eql x #\{)
                      (eql x #\})))))
   stream))

;; TODO: :>を廃止して、デフォルトでparser-seqにする
(defmacro parser (exp)
  (labels ((rec (exp)
             (if exp
                 (etypecase (car exp)
                   (cons (cons (expand (car exp)) (rec (cdr exp))))
                   (symbol (cons `#',(car exp) (rec (cdr exp)))))
                 nil))
           (expand (exp)
             (if exp
                 (ecase (car exp)
                   (:> (cons 'parser-seq (rec (cdr exp))))
                   (:/ (cons 'parser-or (rec (cdr exp))))
                   (:* (cons 'parser-many (rec (cdr exp))))
                   (:+ (cons 'parser-many1 (rec (cdr exp))))
                   (:f (cdr exp)))
                 nil)))
    `(parser-seq ,(expand exp))))

#+(or)
(defun f (stream)
  (funcall (parser (:/ (:> (:f char-parser #\{)
                           (:/ (:+ not-brace) {x})
                           (:f char-parser #\})
                           {x})
                       empty))
           stream))

(defpackage :parser.test (:use :parser :cl))
(in-package :parser.test)

(5am:def-suite parser)
(5am:in-suite parser)

(5am:test with-context
  (with-context (s (string->parser-stream "a"))
      ((c (any-char)))
    (5am:is (eql #\a c)))
  (with-context (s (string->parser-stream "b"))
      ((c (any-char)))
    (5am:is (eql #\b c)))
  (with-context (s (string->parser-stream "a1"))
      ((c (any-char))
       (d (digit)))
    (5am:is (equal '(#\a . #\1) (cons c d))))
  (with-context (s (string->parser-stream "abc"))
      ((c0 (any-char))
       (c1 (any-char)))
    (values :for-avoiding-warnings c0 c1)
    (5am:is (eql #\c (parser-stream-car s)))))

(test specific-char
  (is (eql #\a (parse (specific-char #\a) "a")))
  (is (eql #\b (parse (specific-char #\b) "b")))
  (signals unexpected-datum
    (parse (specific-char #\a) "b")))

(test specific-string
  (is (equal "a" (parse (specific-string "a") "a")))
  (is (equal "a" (parse (specific-string "a") "abc")))
  (let ((s "string"))
    (is (equal s (parse (specific-string s) s)))
    (signals unexpected-datum
      (parse (specific-string s) "another")))
  (let ((s (string->parser-stream "string")))
    (handler-case (funcall (specific-string "strong") s)
      (parser-error (c)
        (is (eq s (parser-error-stream c)))))))

(test any-char
  (is (eql #\a (parse (any-char) "a")))
  (is (eql #\Space (parse (any-char) " "))))

(test upper
  (is (eql #\A (parse (upper) "A")))
  (is (eql #\B (parse (upper) "B")))
  (signals unexpected-datum
    (parse (upper) "a")))

(test lower
  (is (eql #\a (parse (lower) "a")))
  (is (eql #\b (parse (lower) "b")))
  (signals unexpected-datum
    (parse (lower) "A")))

(test letter
  (is (eql #\a (parse (letter) "a")))
  (is (eql #\A (parse (letter) "A")))
  (signals unexpected-datum
    (parse (letter) "!")))

(test alpha-num
  (is (eql #\a (parse (alpha-num) "a")))
  (is (eql #\0 (parse (alpha-num) "0")))
  (signals unexpected-datum
    (parse (alpha-num) " ")))

(test digit
  (is (eql #\0 (parse (digit) "0")))
  (is (eql #\1 (parse (digit) "1")))
  (is (eql #\f (parse (digit 16) "f")))
  (signals unexpected-datum
    (parse (digit) "a"))
  (signals unexpected-datum
    (parse (digit 16) "g")))

(test hex-digit
  (is (eql #\0 (parse (hex-digit) "0")))
  (is (eql #\a (parse (hex-digit) "a")))
  (is (eql #\F (parse (hex-digit) "F")))
  (signals unexpected-datum
    (parse (hex-digit) "g")))

(test oct-digit
  (is (eql #\0 (parse (oct-digit) "0")))
  (is (eql #\7 (parse (oct-digit) "7")))
  (signals unexpected-datum
    (parse (oct-digit) "8")))

(test newline
  (is (eql #\Newline (parse (newline) "
")))
  (signals unexpected-datum
    (parse (newline) "a")))

(test tab
  (is (eql #\Tab (parse (tab) "	")))
  (signals unexpected-datum
    (parse (tab) "a")))

(test space
  (is (eql #\Space (parse (space) " ")))
  (is (eql #\Page (parse (space) "")))
  (is (eql #\Tab (parse (space) "	")))
  (is (eql #\Newline (parse (space) "
")))
  (signals unexpected-datum
    (parse (space) "a")))

(5am:test choice
  (5am:signals parser-error
    (parse (choice) "a"))
  (5am:signals parser-error
    (parse (choice (specific-char #\a)) "b"))
  ;(5am:signals unexpected-datum
  ;  (parse (choice (specific-string "aa")) "ab"))
  (5am:is (eql #\a (parse (choice (specific-char #\a)) "a")))
  (5am:is (eql #\a (parse (choice (specific-char #\a)
                                  (specific-char #\b))
                          "a")))
  (5am:is (eql #\b (parse (choice (specific-char #\a)
                                  (specific-char #\b))
                          "b")))
  (5am:signals parser-error
    (parse (choice (specific-char #\a)
                   (specific-char #\b))
           "c")))

(test parser-try
  (is (eql #\a (parse (parser-try (specific-char #\a)) "a")))
  (signals unexpected-datum
    (parse (parser-try (specific-char #\a)) "b")))

(test parser-cont
  (is (eq nil (parse (parser-cont) "any string is not read")))
  (is (equal '(#\a)
             (parse (parser-cont (char-parser #\a))
                    "a")))
  (signals not-expected
    (parse (parser-cont (char-parser #\a))
           "b"))
  (is (equal '(#\a #\a)
             (parse (parser-cont (char-parser #\a)
                                 (char-parser #\a))
                    "aa")))
  (is (equal '(#\a #\b)
             (parse (parser-cont (char-parser #\a)
                                 (char-parser #\b))
                    "ab")))
  (signals not-expected
    (parse (parser-cont (char-parser #\a)
                        (char-parser #\b))
