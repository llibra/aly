(in-package :aly)

(defun intersperse (item list &optional (last-item item))
  (labels ((rec (rest acc)
             (if rest
                 (destructuring-bind (x . y) rest
                   (rec y (cons x (cons (if y item last-item) acc))))
                 (nreverse acc))))
    (etypecase list
      (null nil)
      (cons (rec (cdr list) (list (car list)))))))
