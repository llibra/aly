(in-package :aly.combinator)

(defun sep-by (parser sep)
  (choice (sep-by1 parser sep)
          (unit nil)))

(defun sep-by1 (parser sep)
  (seq/bind (x  <- parser)
            (xs <- (many (seqn sep parser)))
            (unit (cons x xs))))

(defun many1 (parser)
  (seq/bind (r  <- parser)
            (rs <- (many parser))
            (unit (cons r rs))))

(defun skip-many1 (parser)
  (seqn parser (skip-many parser)))

(defun end-by (parser sep)
  (many (seq1 parser sep)))

(defun end-by1 (parser sep)
  (many1 (seq1 parser sep)))

(defun times (parser n)
  (apply #'seq (make-list n :initial-element parser)))

(defun between (open parser close)
  (mlet* ((_ open)
          (x parser)
          (_ close))
    (unit x)))

(defun many-till (parser end)
  (many (mlet1 _ (not-followed-by end) parser)))

(defun chainl1 (parser op)
  (labels ((rec (x)
             (choice (mlet* ((f op) (y parser))
                       (rec (funcall f x y)))
                     (unit x))))
    (mlet1 x parser (rec x))))

(defun chainl (parser op &optional x)
  (choice (chainl1 parser op)
          (unit x)))

(defun chainr1 (parser op)
  (labels ((rec ()
             (mlet1 x parser (rec-1 x)))
           (rec-1 (x)
             (choice (mlet* ((f op) (y (rec)))
                       (unit (funcall f x y)))
                     (unit x))))
    (rec)))

(defun chainr (parser op &optional x)
  (choice (chainr1 parser op)
          (unit x)))

(defun not-followed-by (parser)
  (try (choice (mlet1 _ parser (fail nil))
               (unit nil))))
