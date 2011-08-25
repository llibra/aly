(in-package :aly.condition)

(defun numerical-position (pos stream)
  (labels ((rec (rest n)
             (if (eq rest pos)
                 n
                 (rec (cdr rest) (1+ n)))))
    (rec stream 1)))

(defun print-error-message (out stream pos expect)
  (format out "The parser encountered the unexpected ")
  (if pos
      (format out "input at ~a.~%" (numerical-position pos stream))
      (format out "end of input.~%"))
  (let ((*print-circle* nil)
        (unexpected (if pos (car (parser-stream-car pos)) "the end of input")))
    (if expect
        (format out "It expects ~{~a~}, but got ~a."
                (intersperse ", " expect " or ") unexpected)
        (format out "It doesn't expect ~a." unexpected))))

(define-condition parser-error (error)
  ((stream   :initarg :stream
             :reader parser-error-stream)
   (position :initarg :position
             :reader parser-error-position)
   (expected :initarg :expected
             :reader parser-error-expected))
  (:report (lambda (c s)
             (print-error-message
              s
              (parser-error-stream c)
              (parser-error-position c)
              (parser-error-expected c)))))
