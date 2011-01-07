(defpackage :parser
  (:use :cl)
  (:import-from :alexandria :curry :rcurry :with-gensyms)
  (:import-from :anaphora :aif :it)
  (:export :make-parser-stream
           :string->parser-stream
           :parser-stream-car
           :parser-stream-cdr

           :parser-error
           :unexpected-datum
           :end-of-stream

           :parser-or
           :parser-try

           :parse

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

;; Lazy parser stream is based on the idea from PEG module of Gauche.
;;
;; <parser-stream> : nil | (<token> . <stream-or-generator>) 
;; <stream-or-generator> : <parser-stream> | <generator>

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

(define-condition unexpected-datum (parser-error)
  ((datum :initarg :datum :reader unexpected-datum-datum))
  (:report (lambda (c s)
             (format s "~S is unexpected datum."
                     (unexpected-datum-datum c)))))

(define-condition end-of-stream (parser-error)
  ()
  (:report (lambda (c s)
             (format s "Parser encountered end of stream on ~S."
                     (parser-error-stream c)))))

;;;; Macros

;;; TODO: once-only stream
(defmacro with-no-consumption/error ((stream) &body body)
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

;;;; Primitives

(defun make-parser (pred)
  (lambda (stream)
    (unless stream
      (error 'end-of-stream :stream stream))
    (let ((datum (parser-stream-car stream)))
      (if (funcall pred datum)
          (values datum (parser-stream-cdr stream))
          (error 'unexpected-datum :stream stream :datum datum)))))
    
;;; Combinators

(defun parser-return (x)
  (lambda (stream)
    (values x stream)))

;; TODO: think about (null parsers) case
(defmacro parser-or (&rest parsers)
  (with-gensyms (stream condition)
    (labels ((rec (rest)
               (if rest
                   `(handler-case (funcall ,(car rest) ,stream)
                      ((or unexpected-datum end-of-stream) (,condition)
                        (if (eq ,stream (parser-error-stream ,condition))
                            ,(rec (cdr rest))
                            (error ,condition))))
                   `(error ,condition))))
      (if parsers
          `(lambda (,stream) ,(rec parsers))
          `(lambda (,stream) (values nil ,stream))))))

(defun parser-try (parser)
  (lambda (stream)
    (with-no-consumption/error (stream)
      (funcall parser stream))))

(defun parser-with-error-message

(defmacro parser-seq (&rest parsers)
  (with-gensyms (stream result parser)
    `(lambda (,stream)
       (nreverse
        (reduce (lambda (,result ,parser)
                  (cons (funcall ,parser ,stream) ,result))
                (list ,@parsers)
                :initial-value nil)))))

(defun parser-many (parser)
  (lambda (stream)
    (labels ((rec (s acc)
               (handler-case
                   (rec (parser-stream-cdr s)
                        (cons (funcall parser s) acc))
                 ((or unexpected-datum end-of-stream) (c)
                   (declare (ignore c))
                   acc))))
      (nreverse (rec stream nil)))))

(defun parser-many (parser)
  (lambda (stream)
    (labels ((rec (result)
               (let ((previous (file-position stream)))
                 (handler-case
                     (let ((r (funcall parser stream)))
                       (rec (cons r result)))
                   (not-expected (c)
                     (declare (ignore c))
                     (file-position stream previous)
                     result)
                   (end-of-stream (c)
                     (declare (ignore c))
                     result)))))
      (nreverse (rec nil)))))

(defun parser-many1 (parser)
  (parser-seq parser (parser-many parser)))

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

(defun specific-string (string)
  (lambda (stream)
    (with-no-consumption/error (stream)
      (values string
              (reduce (lambda (s x)
                        (funcall (specific-char x) s)
                        (parser-stream-cdr s))
                      string
                      :initial-value stream)))))

#+(or)
(defun specific-string (string)
  (lambda (stream)
    (let ((length (length string)))
      (labels ((rec (stream n)
                 (cond ((>= n length)
                        (values string stream))
                       ((not stream)
                        (error 'end-of-stream :stream stream))
                       (t
                        (funcall (specific-char (aref string n)) stream)
                        (rec (parser-stream-cdr stream) (1+ n))))))
        (rec stream 0)))))

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

(defun f (stream)
  (funcall (parser (:/ (:> (:f char-parser #\{)
                           (:/ (:+ not-brace) {x})
                           (:f char-parser #\})
                           {x})
                       empty))
           stream))

(defun {x} (stream)
  (funcall
   (parser-or
    (parser-cont
     (char-parser #\{)
     (parser-or
      (parser-many1 #'not-brace)
      #'{x})
     (char-parser #\})
     #'{x})
    #'empty)
   stream))

(parse #'f "{{abc}{123}}rest...")

(defpackage :parser.test (:use :parser :cl :5am))
(in-package :parser.test)

(def-suite parser.test)
(in-suite parser.test)

(test charset-contains-p
  (is-false (charset-contains-p #\a '(#\b)))
  (is-true (charset-contains-p #\a '(#\a)))
  (is-true (charset-contains-p #\Newline '(#\Space #\Tab #\Newline)))
  (is-true (charset-contains-p #\m '((#\a . #\z))))
  (is-false (charset-contains-p #\m '((#\A . #\Z))))
  (is-true (charset-contains-p #\o '((#\a . #\z) #\Space #\Tab #\Newline)))
  (is-true (charset-contains-p #\Tab '((#\a . #\z) #\Space #\Tab #\Newline))))

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

(test parser-or
  (signals not-expected
    (parse (parser-or (char-parser #\a)) "b"))
  (is (eql #\a (parse (parser-or (char-parser #\a)) "a")))
  (is (eql #\a (parse (parser-or (char-parser #\b)
                                 (char-parser #\a))
                      "a")))
  (signals not-expected
    (parse (parser-or (char-parser #\a)
                      (char-parser #\b))
           "c")))

(test parser-try
  (is (eql #\a (parse (parser-try (specific-char #\a)) "a")))
  (signals unexpected-datum
    (parse (parser-try (specific-char #\a)) "b")))

(test with-context
  (with-context (s (string->parser-stream "a"))
      ((c (any-char)))
    (is (eql #\a c)))
  (with-context (s (string->parser-stream "b"))
      ((c (any-char)))
    (is (eql #\b c)))
  (with-context (s (string->parser-stream "a1"))
      ((c (any-char))
       (d (digit)))
    (is (equal '(#\a . #\1) (cons c d))))
  (with-context (s (string->parser-stream "abc"))
      ((c0 (any-char))
       (c1 (any-char)))
    (values :for-avoiding-warnings c0 c1)
    (is (eql #\c (parser-stream-car s)))))

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
