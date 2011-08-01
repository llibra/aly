(in-package :aly)

;;;; Stream

;;; Lazy parser stream is based on the idea from PEG module of Gauche.
;;;
;;; <parser-stream> : nil | (<token> . <stream-or-generator>) 
;;; <stream-or-generator> : <parser-stream> | <generator>
;;; <token> : (<datum> . <position>)

(defun make-parser-stream (generator)
  (aif (funcall generator)
       (cons (cons it 0) generator)
       nil))

(defmethod parser-stream ((x null)) nil)

(defmethod parser-stream ((x string))
  (let ((in (make-string-input-stream x)))
    (flet ((f ()
             (let ((c (read-char in nil)))
               (unless c (close in))
               c)))
      (make-parser-stream #'f))))

(declaim (inline parser-stream-car))

(defun parser-stream-car (stream)
  (car stream))

(declaim (inline parser-stream-cdr))

(defun parser-stream-cdr (stream)
  (if (functionp (cdr stream))
      (aif (funcall (cdr stream))
           (setf (cdr stream)
                 (cons (cons it (1+ (cdar stream))) (cdr stream)))
           (setf (cdr stream) nil))
      (cdr stream)))

;;;; Condition

(define-condition parser-error (error)
  ((stream :initarg :stream :accessor parser-error-stream)))

(define-condition failure (parser-error)
  ((datum :initarg :datum :reader failure-datum)
   (position :initarg :position :reader failure-position)))

(define-condition simple-failure (failure)
  ((control :initarg :control :reader failure-control)
   (arguments :initarg :arguments :reader failure-arguments))
  (:report (lambda (c s)
             (apply #'format s (failure-control c) (failure-arguments c)))))

(define-condition failure/unexpected (failure) ()
  (:report (lambda (c s)
             (if (failure-datum c)
                 (format s "Parser encounterd unexpected datum ~S at ~S."
                         (failure-datum c)
                         (failure-position c))
                 (format s "Parser encountered unexpected end of stream.")))))

(define-condition failure/expected (failure)
  ((expected :initarg :expected :accessor failure-expected))
  (:report (lambda (c s)
             (if (failure-datum c)
                 (format s "Parser is expecting ~A, but got ~S at ~S."
                         (failure-expected c)
                         (failure-datum c)
                         (failure-position c))
                 (format s "Parser is expecting ~A, but encountered unexpected end of stream."
                         (failure-expected c))))))

;;;; Macro

(defmacro result-match (form &body clauses)
  (with-gensyms (values)
    `(let ((,values (multiple-value-list ,form)))
       (declare (dynamic-extent ,values))
       (match ,values ,@clauses))))

(defmacro define-parser (name &body body)
  `(progn
     (setf (symbol-function ',name) (progn ,@ body))
     ',name))

;;;; Primitive

(declaim (inline success))
(defun success (value stream &optional pos msgs)
  (values t value stream pos msgs))

(declaim (inline failure))
(defun failure (&optional pos msgs)
  (values nil pos msgs))

;; TODO: improve error handling
(defun signal-parser-error (stream pos msgs)
  (declare (stream pos msgs))
  (values nil nil))

(defun parse (parser input &key (parser-error-p t))
  (let ((stream (parser-stream input)))
    (result-match (funcall parser stream)
      ((t value _ _ _)
       (values value t))
      ((nil pos msgs)
       (if parser-error-p
           (signal-parser-error stream pos msgs)
           (values nil nil))))))

(defun unit (x)
  #'(lambda (stream) (success x stream)))

(defun satisfy (pred)
  #'(lambda (stream)
      (if stream
          (let ((token (parser-stream-car stream)))
            (if (funcall pred (car token))
                (success (car token) (parser-stream-cdr stream) stream)
                (failure stream)))
          (failure))))

(defun bind (parser fn)
  #'(lambda (stream)
      (result-match (funcall parser stream)
        ((t value stream _ _)
         (funcall (funcall fn value) stream))
        ((nil pos msgs)
         (failure pos msgs)))))

(defun seq (&rest parsers)
  (flet ((mcons (mx my)
           (bind mx #'(lambda (x)
                        (bind my #'(lambda (y) (unit (cons x y))))))))
    (reduce #'mcons parsers
            :from-end t
            :initial-value (unit nil))))

(labels ((rec (rest stream)
           (match rest
             ((parser)
              (funcall parser stream))
             ((parser . rest)
              (result-match (funcall parser stream)
                ((t _ stream _ _)
                 (rec rest stream))
                ((nil pos msgs)
                 (failure pos msgs)))))))
  (defun seq1 (&rest parsers)
    (match parsers
      (() (unit nil))
      ((parser)
       #'(lambda (stream) (funcall parser stream)))
      ((parser . rest)
       #'(lambda (stream)
           (result-match (funcall parser stream)
             ((t value stream _ _)
              (result-match (rec rest stream)
                ((t _ stream pos msgs)
                 (success value stream pos msgs))
                ((nil pos msgs)
                 (failure pos msgs))))
             ((nil pos msgs)
              (failure pos msgs)))))))

  (defun seqn (&rest parsers)
    (match parsers
      (() (unit nil))
      (_ #'(lambda (stream) (rec parsers stream))))))

(defmacro seq/bind (&rest parsers)
  (with-gensyms (ignore)
    (match parsers
      (() '(unit nil))
      (((var <- parser))
       (if (string= <- "<-")
           parser
           `(,var ,<- ,parser)))
      ((parser) parser)
      (((var <- parser) . rest)
       (if (string= <- "<-")
           `(bind ,parser #'(lambda (,var) (seq/bind ,@rest)))
           `(bind (,var ,<- ,parser)
                  #'(lambda (,ignore)
                      (declare (ignore ,ignore))
                      (seq/bind ,@rest)))))
      ((parser . rest)
       `(bind ,parser
              #'(lambda (,ignore)
                  (declare (ignore ,ignore))
                  (seq/bind ,@rest)))))))

(defun choice2 (parser1 parser2)
  #'(lambda (stream)
      (result-match (funcall parser1 stream)
        ((t value stream1 pos msgs)
         (success value stream1 pos msgs))
        ((nil pos msgs)
         (if (eq pos stream)
             (funcall parser2 pos)
             (failure pos msgs))))))

(defun choice (&rest parsers)
  (match parsers
    (() (unit nil))
    (_ (reduce #'choice2 parsers))))

(defun fail (msg)
  #'(lambda (stream) (failure stream msg)))

(defun try (parser)
  #'(lambda (stream)
      (result-match (funcall parser stream)
        ((t value stream pos msgs)
         (success values stream pos msgs))
        ((nil pos msgs)
         (if (eq pos stream)
             (failure pos msgs)
             ;; TODO: treat the position of failure
             (failure stream msgs))))))

(defun expect (parser x)
  #'(lambda (stream)
      (result-match (funcall parser stream)
        ((t value stream pos msgs)
         (if (eq pos stream)
             (success value stream pos (list x))
             (success value stream pos msgs)))
        ((nil pos msgs)
         (if (eq pos stream)
             (failure pos (list x))
             (failure pos msgs))))))

;; TODO: mreduce
(defun many-common (accum-fn parser stream0)
  (labels ((rec (stream accum)
             (result-match (funcall parser stream)
               ((t value stream1 _ _)
                (rec stream1 (funcall accum-fn value accum)))
               ((nil _ _)
                (success accum stream)))))
    (rec stream0 nil)))

(defun many (parser)
  #'(lambda (stream)
      (result-match (many-common #'cons parser stream)
        ((t value stream pos msgs)
         (success (nreverse value) stream pos msgs)))))

(defun skip-many (parser)
  #'(lambda (stream)
      (many-common (constantly nil) parser stream)))

(define-parser eof
  #'(lambda (stream)
      (if stream
          (funcall (fail "Parser is expecting end of stream.") stream)
          (values nil stream))))

;;;; Combinator

(defun sep-by (parser sep)
  (choice (sep-by1 parser sep)
          (unit nil)))

(defun sep-by1 (parser sep)
  (seq/bind (x  <- parser)
            (xs <- (many (seqn sep parser)))
            (unit (cons x xs))))

(defun many1 (parser)
  (seq/bind (r  <- parser)
            (rs <- (many parser))
            (unit (cons r rs))))

(defun skip-many1 (parser)
  (seqn parser (skip-many parser)))

;;;; Character

(defun specific-char (c)
  (expect (satisfy (curry #'eql c)) c))

;; TODO: mreduce
(defun specific-string (string)
  #'(lambda (stream)
      (values string
              (reduce (lambda (s0 x)
                        (multiple-value-bind (_ s1)
                            (funcall (specific-char x) s0)
                          (declare (ignore _))
                          s1))
                      string
                      :initial-value stream))))

(defun one-of (&rest cs)
  (expect (satisfy (rcurry #'member cs))
          (format nil "one of ~{~A~}" (intersperse ", " cs " and "))))

(defun none-of (&rest cs)
  (expect (satisfy (complement (rcurry #'member cs)))
          (format nil "except any of ~{~A~}" (intersperse ", " cs " and "))))

(define-parser any-char
  (satisfy (constantly t)))

(define-parser upper
  (expect (satisfy #'upper-case-p) "a uppercase letter"))

(define-parser lower
  (expect (satisfy #'lower-case-p) "a lowercase letter"))

(define-parser letter
  (expect (satisfy #'alpha-char-p) "a letter"))

(define-parser alpha-num
  (expect (satisfy #'alphanumericp) "a letter or a digit"))

(defun digit (&optional (radix 10))
  (expect (satisfy (rcurry #'digit-char-p radix)) "a digit"))

(define-parser decimal-digit
  (expect (satisfy (rcurry #'digit-char-p 10)) "a decimal digit"))

(define-parser hex-digit
  (expect (satisfy (rcurry #'digit-char-p 16)) "a hexadecimal digit"))

(define-parser oct-digit
  (expect (satisfy (rcurry #'digit-char-p 8)) "a octal digit"))

(define-parser newline
  (expect (specific-char #\Newline) "a new line"))

(define-parser tab
  (expect (specific-char #\Tab) "a tab"))

(define-parser space
  (expect (satisfy #'(lambda (c)
                       (some (curry #'eql c)
                             '(#\Space #\Page #\Tab #\Newline))))
          "a space"))

(define-parser spaces
  (skip-many #'space))
