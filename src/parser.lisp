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

(defmacro define-parser (name &body body)
  `(progn
     (setf (symbol-function ',name) (progn ,@ body))
     ',name))

;;;; Primitive

(defun parse (parser input)
  (funcall parser (parser-stream input)))

(defun pure (x)
  #'(lambda (stream)
      (values x stream)))

(defun seq (&rest parsers)
  (labels ((rec (rest stream value)
             (if rest
                 (multiple-value-bind (x stream) (funcall (car rest) stream)
                   (rec (cdr rest) stream (cons x value)))
                 (values (nreverse value) stream))))
    (if parsers
        #'(lambda (stream) (rec parsers stream nil))
        (pure nil))))

(labels ((rec (rest stream)
           (destructuring-bind (parser . rest) rest
             (if rest
                 (multiple-value-bind (_ stream) (funcall parser stream)
                   (declare (ignore _))
                   (rec rest stream))
                 (funcall parser stream)))))
  (defun seq1 (&rest parsers)
    (cond ((null parsers)
           (pure nil))
          ((cdr parsers)
           #'(lambda (stream)
               (multiple-value-bind (x stream) (funcall (car parsers) stream)
                 (multiple-value-bind (_ stream) (rec (cdr parsers) stream)
                   (declare (ignore _))
                   (values x stream)))))
          (t
           #'(lambda (stream) (funcall (car parsers) stream)))))

  (defun seqn (&rest parsers)
    (if parsers
        #'(lambda (stream) (rec parsers stream))
        (pure nil))))

(defmacro seq/bind (&rest parsers)
  (with-gensyms (stream ignore)
    (labels ((rec (rest)
               (match rest
                 ((and (list (list x <- y))
                       (when (equal (symbol-name <-) "<-")))
                  `(funcall ,y ,stream))
                 ((list x) `(funcall ,x ,stream))
                 ((and (cons (list x <- y) z)
                       (when (equal (symbol-name <-) "<-")))
                  `(multiple-value-bind (,x ,stream) (funcall ,y ,stream)
                     ,(rec z)))
                 ((cons x y)
                  `(multiple-value-bind (,ignore ,stream) (funcall ,x ,stream)
                     (declare (ignore ,ignore))
                     ,(rec y))))))
      `#'(lambda (,stream) ,(rec parsers)))))

(defun choice (&rest parsers)
  #'(lambda (stream)
      (labels ((rec (rest stream)
                 (if rest
                     (handler-case (funcall (car rest) stream)
                       (failure (c)
                         (if (eq stream (parser-error-stream c))
                             (rec (cdr rest) stream)
                             (error c))))
                     (funcall (fail) stream))))
        (rec parsers stream))))

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

(defun %many (accum parser stream)
  (labels ((rec (stream acc)
             (handler-case
                 (multiple-value-bind (r s) (funcall parser stream)
                   (rec s (funcall accum r acc)))
               (failure (c)
                 (declare (ignore c))
                 (values acc stream)))))
    (rec stream nil)))

(defun many (parser)
  #'(lambda (stream)
      (multiple-value-bind (r s)
          (%many #'cons parser stream)
        (values (nreverse r) s))))

(defun skip-many (parser)
  #'(lambda (stream)
      (%many (constantly nil) parser stream)))

(define-parser eof
  #'(lambda (stream)
      (if stream
          (funcall (fail "Parser is expecting end of stream.") stream)
          (values nil stream))))

;;;; Combinator

(defun sep-by (parser sep)
  (choice (sep-by1 parser sep)
          (pure nil)))

(defun sep-by1 (parser sep)
  (seq/bind (x  <- parser)
            (xs <- (many (seqn sep parser)))
            (pure (cons x xs))))

(defun many1 (parser)
  (seq/bind (r  <- parser)
            (rs <- (many parser))
            (pure (cons r rs))))

(defun skip-many1 (parser)
  (seqn parser (skip-many parser)))

;;;; Character

(defun satisfy (pred)
  #'(lambda (stream)
      (unless stream
        (error 'failure/unexpected
               :stream stream
               :datum nil
               :position "end of stream"))
      (let ((token (parser-stream-car stream)))
        (if (funcall pred (car token))
            (values (car token) (parser-stream-cdr stream))
            (error 'failure/unexpected
                   :stream stream
                   :datum (car token)
                   :position (cdr token))))))

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
