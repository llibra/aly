(in-package :aly.stream)

;;; Lazy parser stream is based on the idea from PEG module of Gauche.
;;;
;;; <parser-stream> : nil | (<token> . <stream-or-generator>) 
;;; <stream-or-generator> : <parser-stream> | <generator>

(defun make-parser-stream (generator)
  (aif (funcall generator)
       (cons it generator)
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
           (setf (cdr stream) (cons it (cdr stream)))
           (setf (cdr stream) nil))
      (cdr stream)))
