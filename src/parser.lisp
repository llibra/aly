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
                 (format s "Parser is not expecting ~S." (failure-datum c))
                 (format s "Parser encountered unexpected end of stream.")))))

(define-condition failure/expected (failure)
  ((expected :initarg :expected :accessor failure-expected))
  (:report (lambda (c s)
             (if (failure-datum c)
                 (format s "Parser is expecting ~A, but got ~S."
                         (failure-expected c)
                         (failure-datum c))
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

(defmacro parse (data &body parsers)
  (check-type parsers cons)
  (with-gensyms (stream ignore)
    (labels ((binding-p (x)
               (and (consp x)
                    (symbolp (cadr x))
                    (equal (symbol-name (cadr x)) "<-")))
             (rec (rest)
               (cond ((null (cdr rest))
                      `(funcall ,(car rest) ,stream))
                     ((binding-p (car rest))
                      `(multiple-value-bind (,(caar rest) ,stream)
                           (funcall ,@(cddar rest) ,stream)
                         ,(rec (cdr rest))))
                     (t
                      `(multiple-value-bind (,ignore ,stream)
                           (funcall ,(car rest) ,stream)
                         (declare (ignore ,ignore))
                         ,(rec (cdr rest)))))))
      (once-only (data)
        `(let ((,stream (if (parser-stream-p ,data)
                            ,data
                            (parser-stream ,data))))
           ,(rec parsers))))))

(defun result (x)
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
    (error 'failure/unexpected :stream stream :datum x)))

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

(defun many1 (parser)
  (lambda (stream)
    (parse stream
      (r  <- parser)
      (rs <- (many parser))
      (result (cons r rs)))))

(defun skip-many1 (parser)
  (sequence parser (skip-many parser)))

;;;; Character

(defun satisfy (pred)
  (lambda (stream)
    (unless stream
      (error 'failure/unexpected :stream stream :datum nil))
    (let ((token (parser-stream-car stream)))
      (if (funcall pred (car token))
          (values (car token) (parser-stream-cdr stream))
          (error 'failure/unexpected
                 :stream stream
                 :datum (car token))))))

(defun specific-char (c)
  (expect (satisfy (curry #'eql c)) c))

(defun specific-string (string)
  (expect (lambda (stream)
            (values string
                    (reduce (lambda (s x)
                              (funcall (specific-char x) s)
                              (parser-stream-cdr s))
                            string
                            :initial-value stream)))
          string))

(labels ((rec (item last-item rest acc)
           (cond ((null rest) (nreverse acc))
                 ((cdr rest)
                  (rec item last-item (cdr rest)
                       (cons (car rest) (cons item acc))))
                 (t
                  (rec item last-item (cdr rest)
                       (cons (car rest) (cons last-item acc))))))
         (intersperse (item list &optional (last-item item))
           (etypecase list
             (null nil)
             (cons (rec item last-item (cdr list) (list (car list)))))))
  (defun one-of (&rest cs)
    (expect (satisfy (rcurry #'member cs))
            (format nil "one of ~{~A~}" (intersperse ", " cs " and "))))

  (defun none-of (&rest cs)
    (expect (satisfy (complement (rcurry #'member cs)))
            (format nil "except any of ~{~A~}" (intersperse ", " cs " and ")))))

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
