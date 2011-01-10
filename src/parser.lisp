(in-package :aly)

;;;; Stream

;;; Lazy parser stream is based on the idea from PEG module of Gauche.
;;;
;;; <parser-stream> : nil | (<token> . <stream-or-generator>) 
;;; <stream-or-generator> : <parser-stream> | <generator>

(defun make-parser-stream (generator)
  (cons (funcall generator) generator))

(defun parser-stream/string (string)
  (let ((in (make-string-input-stream string)))
    (flet ((f ()
             (let ((c (read-char in nil)))
               (unless c (close in))
               c)))
      (make-parser-stream #'f))))

(defun parser-stream (x)
  (etypecase x
    (string (parser-stream/string x))))

(declaim (inline parser-stream-car))

(defun parser-stream-car (stream)
  (car stream))

(defun parser-stream-cdr (stream)
  (if (functionp (cdr stream))
      (aif (funcall (cdr stream))
           (setf (cdr stream) (cons it (cdr stream)))
           (setf (cdr stream) nil))
      (cdr stream)))

;;;; Condition

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

(defmacro with-expected ((str) &body body)
  (with-gensyms (condition)
    `(handler-case (progn ,@body)
       (unexpected-datum (,condition)
         (setf (unexpected-datum-unexpected ,condition) nil)
         (setf (unexpected-datum-expected ,condition) ,str)
         (error ,condition)))))

;;;; Primitive

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

(defun %many (accum parser stream)
  (labels ((rec (stream acc)
             (handler-case
                 (rec (parser-stream-cdr stream)
                      (funcall accum (funcall parser stream) acc))
               (parser-error (c)
                 (declare (ignore c))
                 (values acc (parser-stream-cdr stream))))))
    (rec stream nil)))

(defun many (parser)
  (lambda (stream)
    (multiple-value-bind (r s)
        (%many #'cons parser stream)
      (values (nreverse r) s))))

(defun skip-many (parser)
  (lambda (stream)
    (%many (constantly nil) parser stream)))

(defun parse (parser data)
  (funcall parser (parser-stream data)))

;;;; Combinator

(defun many1 (parser)
  (lambda (stream)
    (with-context (s stream)
        ((r parser))
      (cons r (funcall (many parser) s)))))

(defun skip-many1 (parser)
  (sequence parser (skip-many parser)))

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
    (values string
            (reduce (lambda (s x)
                      (funcall (specific-char x) s)
                      (parser-stream-cdr s))
                    string
                    :initial-value stream))))

(defun one-of (cs)
  (fail "Not implemented."))

(defun none-of (cs)
  (fail "Not implemented."))

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
  (fail "Not implemented."))
