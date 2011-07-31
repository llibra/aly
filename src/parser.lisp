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

(defmacro with-result-bind (vars form &body body)
  `(multiple-value-bind ,vars ,form ,@body))

(defmacro if-result/bind (vars form then &optional else)
  (let ((type (if vars (car vars) (gensym))))
    `(with-result-bind ,vars ,form
       (declare (ignorable ,@vars))
       (if (eq ,type :success) ,then ,else))))

(defmacro define-parser (name &body body)
  `(progn
     (setf (symbol-function ',name) (progn ,@ body))
     ',name))

;;;; Primitive

(declaim (inline result))
(defun result (type value position expected stream)
  (values type value position expected stream))

(declaim (inline success))
(defun success (&key value expected stream (position stream))
  (result :success value position expected stream))

(declaim (inline failure))
(defun failure (&key expected stream (position stream))
  (result :failure nil position expected stream))

;; TODO: improve error handling
(defun signal-parser-error (type value position expected stream)
  (declare (ignore type position expected stream))
  (values value nil))

(defun parse (parser input &key (parser-error-p t))
  (if-result/bind (type value position expected stream)
                  (funcall parser (parser-stream input))
                  (values value t)
                  (if parser-error-p
                      (signal-parser-error type value position expected stream)
                      (values value nil))))

(defun unit (x)
  #'(lambda (stream)
      (success :value x :stream stream)))

(defun satisfy (pred)
  #'(lambda (stream)
      (if stream
          (let ((token (parser-stream-car stream)))
            (if (funcall pred (car token))
                (success :value (car token)
                         :position stream
                         :stream (parser-stream-cdr stream))
                (failure :stream stream)))
          (failure))))

(defun bind (parser fn)
  #'(lambda (stream)
      (if-result/bind (type value position expected stream)
                      (funcall parser stream)
                      (funcall (funcall fn value) stream)
                      (result type value position expected stream))))

(defun seq (&rest parsers)
  (labels ((rec (rest stream result)
             (if rest
                 (if-result/bind (type value position expected stream)
                                 (funcall (car rest) stream)
                                 (rec (cdr rest) stream (cons value result))
                                 (result type value position expected stream))
                 (success :value (nreverse result) :stream stream))))
    (if parsers
        #'(lambda (stream) (rec parsers stream nil))
        (unit nil))))

(labels ((rec (rest stream)
           (destructuring-bind (parser . rest) rest
             (if rest
                 (if-result/bind (type value position expected stream)
                                 (funcall parser stream)
                                 (rec rest stream)
                                 (result type value position expected stream))
                 (funcall parser stream)))))
  (defun seq1 (&rest parsers)
    (if parsers
        (destructuring-bind (parser . rest) parsers
          (if rest
              #'(lambda (stream)
                  (if-result/bind
                   (type value1 position expected stream)
                   (funcall parser stream)
                   (if-result/bind (type value position expected stream) 
                                   (rec rest stream)
                                   (result type value1 position expected stream)
                                   (result type value position expected stream))
                   (result type value1 position expected stream)))
              #'(lambda (stream) (funcall parser stream))))
        (unit nil)))

  (defun seqn (&rest parsers)
    (if parsers
        #'(lambda (stream) (rec parsers stream))
        (unit nil))))

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
           `(bind ,parser
                  #'(lambda (,var)
                      (seq/bind ,@rest)))
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
      (if-result/bind (type value position expected stream1)
                      (funcall parser1 stream)
                      (result type value position expected stream1)
                      (if (eq stream1 stream)
                          (funcall parser2 stream1)
                          (result type value position expected stream1)))))

(defun choice (&rest parsers)
  (if parsers
      (reduce #'choice2 parsers)
      (unit nil)))

(defun fail (&optional (ctrl "Parser failed.") &rest args)
  #'(lambda (stream)
      (error 'simple-failure :stream stream :control ctrl :arguments args)))

(defun fail/unexpected (x)
  #'(lambda (stream)
      (error 'failure/unexpected
             :stream stream
             :datum x
             :position (if stream
                           (cdr (parser-stream-car stream))
                           "end of stream"))))

(defun try (parser)
  #'(lambda (stream)
      (handler-case (funcall parser stream)
        (failure (c)
          (setf (parser-error-stream c) stream)
          (error c)))))

(defun expect (parser x)
  #'(lambda (stream)
      (handler-case (funcall parser stream)
        (failure/expected (c)
          (setf (failure-expected c) x)
          (error c))
        (failure (c)
          (error 'failure/expected
                 :stream (parser-error-stream c)
                 :datum (failure-datum c)
                 :position (failure-position c)
                 :expected x)))))

(defun many-common (accum parser stream0)
  (labels ((rec (stream result)
             (if-result/bind (type value position expected stream1)
                             (funcall parser stream)
                             (rec stream1 (funcall accum value result))
                             (success :value result
                                      :position stream0
                                      :stream stream))))
    (rec stream0 nil)))

(defun many (parser)
  #'(lambda (stream)
      (with-result-bind (type value position expected stream)
          (many-common #'cons parser stream)
        (result type (nreverse value) position expected stream))))

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
