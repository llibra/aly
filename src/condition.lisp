(in-package :aly.condition)

(define-condition parser-error (error)
  ((stream :initarg :stream)
   (position :initarg :position)))
