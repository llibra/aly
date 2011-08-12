(in-package :aly.core)

(declaim (inline success))
(defun success (value stream &optional pos msgs)
  (values t value stream pos msgs))

(declaim (inline failure))
(defun failure (&optional pos msgs)
  (values nil pos msgs))

(defun satisfy (pred)
  #'(lambda (stream)
      (if stream
          (let ((token (parser-stream-car stream)))
            (if (funcall pred (car token))
                (success (car token) (parser-stream-cdr stream) stream)
                (failure stream)))
          (failure))))

(defun unit (x)
  #'(lambda (stream) (success x stream)))

(defun fail (msg)
  #'(lambda (stream) (failure stream msg)))

(defun bind (parser fn)
  #'(lambda (stream)
      (result-match (funcall parser stream)
        ((t value stream _ _)
         (funcall (funcall fn value) stream))
        ((nil pos msgs)
         (failure pos msgs)))))

(defmacro mlet1 (var form &body body)
  (if (string= var "_")
      (with-gensyms (_)
        `(bind ,form #'(lambda (,_) (declare (ignore ,_)) ,@body)))
      `(bind ,form #'(lambda (,var) ,@body))))

(defmacro mlet* (bindings &body body)
  (match bindings
    (() `(progn ,@body))
    (((var parser) . rest)
     `(mlet1 ,var ,parser (mlet* ,rest ,@body)))))

(defun seq (&rest parsers)
  (flet ((mcons (mx my)
           (mlet* ((x mx) (y my)) (unit (cons x y)))))
    (reduce #'mcons parsers
            :from-end t
            :initial-value (unit nil))))

(defun seqn2 (parser1 parser2)
  (mlet1 _ parser1 parser2))

(defun seqn (&rest parsers)
  (match parsers
    (() (unit nil))
    (_ (reduce #'seqn2 parsers))))

(defun seq1 (parser1 &rest parsers)
  (match parsers
    (() parser1)
    (_ (mlet* ((x parser1)
               (_ (reduce #'seqn2 parsers)))
         (unit x)))))

(defmacro seq/bind (&rest parsers)
  (match parsers
    (() '(unit nil))
    (((var <- parser))
     (if (string= <- "<-") parser `(,var ,<- ,parser)))
    ((parser) parser)
    (((var <- parser) . rest)
     (if (string= <- "<-")
         `(mlet1 ,var ,parser (seq/bind ,@rest))
         `(mlet1 _ (,var ,<- ,parser) (seq/bind ,@rest))))
    ((parser . rest)
     `(mlet1 _ ,parser (seq/bind ,@rest)))))

(defun choice2 (parser1 parser2)
  #'(lambda (stream)
      (result-match (funcall parser1 stream)
        ((t value stream1 pos msgs)
         (success value stream1 pos msgs))
        ((nil pos msgs)
         (if (eq pos stream)
             (funcall parser2 pos)
             (failure pos msgs))))))

(defun choice (&rest parsers)
  (match parsers
    (() (unit nil))
    (_ (reduce #'choice2 parsers))))

(defun try (parser)
  #'(lambda (stream)
      (result-match (funcall parser stream)
        ((t value stream pos msgs)
         (success value stream pos msgs))
        ((nil pos msgs)
         (if (eq pos stream)
             (failure pos msgs)
             ;; TODO: Treating the position of failure
             (failure stream msgs))))))

(defun expect (parser x)
  #'(lambda (stream)
      (result-match (funcall parser stream)
        ((t value stream pos msgs)
         (if (eq pos stream)
             (success value stream pos (list x))
             (success value stream pos msgs)))
        ((nil pos msgs)
         (if (eq pos stream)
             (failure pos (list x))
             (failure pos msgs))))))

;; TODO: Refactoring
(defun many-common (accum-fn parser stream0)
  (labels ((rec (stream accum)
             (result-match (funcall parser stream)
               ((t value stream1 _ _)
                (rec stream1 (funcall accum-fn value accum)))
               ((nil _ _)
                (success accum stream)))))
    (rec stream0 nil)))

(defun many (parser)
  #'(lambda (stream)
      (result-match (many-common #'cons parser stream)
        ((t value stream pos msgs)
         (success (nreverse value) stream pos msgs)))))

(defun skip-many (parser)
  #'(lambda (stream)
      (many-common (constantly nil) parser stream)))

(defun eoi (stream)
  (match stream
    (() (success nil stream))
    (_ (failure stream (list "end of input")))))

;; TODO: Improving error handling
(defun signal-parser-error (stream pos msgs)
  (declare (ignore msgs))
  (error 'parser-error :stream stream :position pos))

(defun parse (parser input &key (parser-error-p t))
  (let ((stream (parser-stream input)))
    (result-match (funcall parser stream)
      ((t value _ _ _)
       (values value t))
      ((nil pos msgs)
       (if parser-error-p
           (signal-parser-error stream pos msgs)
           (values nil nil))))))
