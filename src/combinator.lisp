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
