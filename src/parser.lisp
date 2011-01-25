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

(defun parser-stream/string (string)
  (let ((in (make-string-input-stream string)))
    (flet ((f ()
             (let ((c (read-char in nil)))
               (unless c (close in))
               c)))
      (make-parser-stream #'f))))

(defun parser-stream (x)
  (etypecase x
    (string (parser-stream/string x))
    (null nil)))

(defun parser-stream-p (x)
  (and (consp x) (consp (car x))))

(declaim (inline parser-stream-car))

(defun parser-stream-car (stream)
  (car stream))

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

;;;; Utility

(defmacro with-no-consumption/failure ((stream) &body body)
  (with-gensyms (condition)
    `(handler-case
         (progn ,@body)
       (failure (,condition)
         (setf (parser-error-stream ,condition) ,stream)
         (error ,condition)))))

;;;; Primitive

(defun parse (parser stream)
  (funcall parser
           (if (parser-stream-p stream)
               stream
               (parser-stream stream))))

(defmacro bind (&rest parsers)
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
      `(lambda (,stream) ,(rec parsers)))))

(defun result (x)
  (lambda (stream)
    (values x stream)))

(defun choice (&rest parsers)
  (lambda (stream)
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
  (lambda (stream)
    (error 'simple-failure :stream stream :control ctrl :arguments args)))

(defun fail/unexpected (x)
  (lambda (stream)
    (error 'failure/unexpected
           :stream stream
           :datum x
           :position (if stream
                         (cdr (parser-stream-car stream))
                         "end of stream"))))

(defun try (parser)
  (lambda (stream)
    (with-no-consumption/failure (stream)
      (funcall parser stream))))

(defun expect (parser x)
  (lambda (stream)
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
  (lambda (stream)
    (multiple-value-bind (r s)
        (%many #'cons parser stream)
      (values (nreverse r) s))))

(defun skip-many (parser)
  (lambda (stream)
    (%many (constantly nil) parser stream)))

;;;; Combinator

(defun sep-by (parser sep)
  (choice (sep-by1 parser sep)
          (result nil)))

(defun sep-by1 (parser sep)
  (bind (x  <- parser)
        (xs <- (many (bind sep parser)))
        (result (cons x xs))))

(defun many1 (parser)
  (bind (r  <- parser)
        (rs <- (many parser))
        (result (cons r rs))))

(defun skip-many1 (parser)
  (bind parser (skip-many parser)))

;;;; Character

(defun satisfy (pred)
  (lambda (stream)
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
  (lambda (stream)
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

(defun any-char ()
  (satisfy (constantly t)))

(defun upper ()
  (expect (satisfy #'upper-case-p) "a uppercase letter"))

(defun lower ()
  (expect (satisfy #'lower-case-p) "a lowercase letter"))

(defun letter ()
  (expect (satisfy #'alpha-char-p) "a letter"))

(defun alpha-num ()
  (expect (satisfy #'alphanumericp) "a letter or a digit"))

(defun digit (&optional (radix 10))
  (expect (satisfy (rcurry #'digit-char-p radix)) "a digit"))

(defun hex-digit ()
  (expect (satisfy (rcurry #'digit-char-p 16)) "a hexadecimal digit"))

(defun oct-digit ()
  (expect (satisfy (rcurry #'digit-char-p 8)) "a octal digit"))

(defun newline ()
  (expect (specific-char #\Newline) "a new line"))

(defun tab ()
  (expect (specific-char #\Tab) "a tab"))

(defun space ()
  (expect (satisfy (lambda (c)
                     (some (curry #'eql c)
                           '(#\Space #\Page #\Tab #\Newline))))
          "a space"))

(defun spaces ()
  (skip-many (space)))
