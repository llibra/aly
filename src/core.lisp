(in-package :aly.core)

(declaim (inline success))
(defun success (value stream &optional pos msgs)
  (values t value stream pos msgs))

(declaim (inline failure))
(defun failure (&optional pos msgs)
  (values nil pos msgs))

(defun unit (x)
  (lambda (stream) (success x stream)))

(defun fail (msg)
  (lambda (stream) (failure stream msg)))

(defun bind (parser fn)
  (lambda (stream)
    (ematch-values (funcall parser stream)
      ((t value1 stream1 _ msgs1)
       (ematch-values (funcall (funcall fn value1) stream1)
         ((t value2 stream2 pos2 msgs2)
          (let ((msgs (if (eq stream1 stream2) (cons msgs1 msgs2) msgs2)))
            (success value2 stream2 pos2 msgs)))
         ((nil pos2 msgs2)
          (let ((msgs (if (eq stream1 pos2) (cons msgs1 msgs2) msgs2)))
            (failure pos2 msgs)))))
      ((nil pos msgs)
       (failure pos msgs)))))

(defmacro mlet1 (var form &body body)
  (if (string= var "_")
      (with-gensyms (_)
        `(bind ,form (lambda (,_) (declare (ignore ,_)) ,@body)))
      `(bind ,form (lambda (,var) ,@body))))

(defmacro mlet* (bindings &body body)
  (ematch bindings
    (() `(progn ,@body))
    ((cons (list var parser) rest)
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
  (flet ((<-p (<-) (string= <- "<-")))
    (ematch parsers
      (() '(unit nil))
      ((guard (list (list _ <- parser)) (<-p <-)) parser)
      ((list parser) parser)
      ((guard (cons (list var <- parser) rest) (<-p <-))
       `(mlet1 ,var ,parser (seq/bind ,@rest)))
      ((cons parser rest)
       `(mlet1 _ ,parser (seq/bind ,@rest))))))

(defun satisfy (pred)
  (lambda (stream)
    (if stream
        (let ((token (parser-stream-car stream)))
          (if (funcall pred token)
              (success token (parser-stream-cdr stream) stream)
              (failure stream)))
        (failure))))

(defun choice2 (parser1 parser2)
  (lambda (stream)
    (ematch-values (funcall parser1 stream)
      ((t value stream pos msgs)
       (success value stream pos msgs))
      ((nil pos1 msgs1)
       (if (eq stream pos1)
           (ematch-values (funcall parser2 pos1)
             ((t value2 stream2 pos2 msgs2)
              (let ((msgs (if (eq pos1 stream2) (cons msgs1 msgs2) msgs2)))
                (success value2 stream2 pos2 msgs)))
             ((nil pos2 msgs2)
              (let ((msgs (if (eq pos1 pos2) (cons msgs1 msgs2) msgs2)))
                (failure pos2 msgs))))
           (failure pos1 msgs1))))))

(defun choice (&rest parsers)
  (match parsers
    (() (unit nil))
    (_ (reduce #'choice2 parsers))))

(defun try (parser)
  (lambda (stream)
    (ematch-values (funcall parser stream)
      ((t value stream pos msgs)
       (success value stream pos msgs))
      ((nil pos msgs)
       (if (eq stream pos)
           (failure pos msgs)
           (failure stream))))))

(defun expect (parser x)
  (lambda (stream0)
    (ematch-values (funcall parser stream0)
      ((t value stream pos msgs)
       (let ((msgs (if (eq stream0 stream) (list x) msgs)))
         (success value stream pos msgs)))
      ((nil pos msgs)
       (let ((msgs (if (eq stream0 pos) (list x) msgs)))
         (failure pos msgs))))))

;; TODO: Treating a parser that accepts an empty string properly
(defun many-common (accum-fn parser stream)
  (labels ((rec (stream0 accum)
             (ematch-values (funcall parser stream0)
               ((t value stream)
                (rec stream (funcall accum-fn value accum)))
               ((nil pos msgs)
                (if (eq stream0 pos)
                    (success accum stream0 pos msgs)
                    (failure pos msgs))))))
    (rec stream nil)))

(defun many (parser)
  (lambda (stream)
    (ematch-values (many-common #'cons parser stream)
      ((t value stream pos msgs)
       (success (nreverse value) stream pos msgs))
      ((nil pos msgs)
       (failure pos msgs)))))

(defun skip-many (parser)
  (lambda (stream)
    (many-common (constantly nil) parser stream)))

(defun eoi (stream)
  (match stream
    (() (success nil stream))
    (_ (failure stream (list "end of input")))))

;; TODO: Improving error handling
(defun signal-parser-error (stream pos msgs)
  (error 'parser-error
         :stream stream
         :position pos
         :expected (remove-duplicates (flatten msgs) :test #'equal)))

(defun parse (parser input &key (parser-error-p t))
  (let ((stream (parser-stream input)))
    (ematch-values (funcall parser stream)
      ((t value)
       (values value t))
      ((nil pos msgs)
       (if parser-error-p
           (signal-parser-error stream pos msgs)
           (values nil nil))))))
