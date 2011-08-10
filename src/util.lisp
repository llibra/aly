(in-package :aly.util)

(defmacro defalias (name function-designator)
  (with-gensyms (function designator)
    `(let* ((,designator ,function-designator)
            (,function (if (functionp ,designator)
                           ,designator
                           (symbol-function ,designator))))
       (setf (symbol-function ',name) ,function)
       ',name)))

;;; TODO: Refactoring for speed
(defmacro result-match (form &body clauses)
  (with-gensyms (values)
    `(let ((,values (multiple-value-list ,form)))
       ;; The list is allocated in the heap for tail call optimization.
       ;; But it will make this part slow.
       ;(declare (dynamic-extent ,values))
       (match ,values ,@clauses))))

(defun intersperse (item list &optional (last-item item))
  (labels ((rec (rest acc)
             (if rest
                 (destructuring-bind (x . y) rest
                   (rec y (cons x (cons (if y item last-item) acc))))
                 (nreverse acc))))
    (etypecase list
      (null nil)
      (cons (rec (cdr list) (list (car list)))))))
