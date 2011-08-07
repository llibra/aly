(in-package :aly)

(defmacro defalias (name function-designator)
  (with-gensyms (function designator)
    `(let* ((,designator ,function-designator)
            (,function (if (functionp ,designator)
                           ,designator
                           (symbol-function ,designator))))
       (setf (symbol-function ',name) ,function)
       ',name)))

(defun intersperse (item list &optional (last-item item))
  (labels ((rec (rest acc)
             (if rest
                 (destructuring-bind (x . y) rest
                   (rec y (cons x (cons (if y item last-item) acc))))
                 (nreverse acc))))
    (etypecase list
      (null nil)
      (cons (rec (cdr list) (list (car list)))))))
