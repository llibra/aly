(in-package :aly.test)

(5am:def-suite all)
(5am:in-suite all)

(5am:def-suite stream :in all)
(5am:in-suite stream)

(5am:test parser-stream
  (5am:is (eq nil (parser-stream ""))))

(5am:def-suite primitive :in all)
(5am:in-suite primitive)

(5am:test parse
  (5am:is (eql #\a (parse (any-char) "a")))
  (5am:is (eql #\b (parse (any-char) "b"))))

(5am:test seq
  (5am:is (eq nil (parse (seq) "a")))
  (5am:is (equal '(#\a) (parse (seq (any-char)) "a")))
  (5am:is (equal '(#\b) (parse (seq (any-char)) "b")))
  (5am:is (equal '(#\a #\b) (parse (seq (any-char) (any-char)) "ab")))
  (5am:is (equal '(#\b #\a) (parse (seq (any-char) (any-char)) "ba"))))

(5am:test seq1
  (5am:is (eq nil (parse (seq1) "a")))
  (5am:is (eql #\a (parse (seq1 (any-char)) "a")))
  (5am:is (eql #\b (parse (seq1 (any-char)) "b")))
  (5am:is (eql #\a (parse (seq1 (any-char) (any-char)) "ab")))
  (5am:is (eql #\b (parse (seq1 (any-char) (any-char)) "ba"))))

(5am:test seqn
  (5am:is (eq nil (parse (seqn) "a")))
  (5am:is (eql #\a (parse (seqn (any-char)) "a")))
  (5am:is (eql #\b (parse (seqn (any-char)) "b")))
  (5am:is (eql #\b (parse (seqn (any-char) (any-char)) "ab"))))

(5am:test seq/bind
  (5am:is (eql #\a (parse (seq/bind (any-char)) "a")))
  (5am:is (eql #\b (parse (seq/bind (any-char)) "b")))
  (5am:signals failure
    (parse (seq/bind (specific-char #\a)) "b"))
  (5am:is (eql #\a (parse (seq/bind (c <- (any-char))) "a")))
  (5am:is (eql #\b (parse (seq/bind (c <- (any-char))) "b")))
  (5am:signals failure
    (parse (seq/bind (c <- (specific-char #\a))) "b"))
  (5am:is (eql #\a
               (parse (seq/bind (any-char)
                                (any-char))
                      "ba")))
  (5am:is (eql #\b
               (parse (seq/bind (any-char)
                                (any-char))
                      "ab")))
  (5am:signals failure
    (parse (seq/bind (specific-char #\a)
                     (specific-char #\b))
           "aa"))
  (5am:is (eql #\a
               (parse (seq/bind (x <- (any-char))
                                (y <- (any-char)))
                      "ba")))
  (5am:is (eql #\b
               (parse (seq/bind (x <- (any-char))
                                (y <- (any-char)))
                      "ab")))
  (5am:signals failure
    (parse (seq/bind (x <- (specific-char #\a))
                     (y <- (specific-char #\b)))
           "aa"))
  (multiple-value-bind (r s)
      (parse (seq/bind (any-char)) "a")
    (declare (ignore r))
    (5am:is (null s)))
  (multiple-value-bind (r s)
      (parse (seq/bind (any-char) (any-char)) "abc")
    (declare (ignore r))
    (5am:is (eql #\c (car (parser-stream-car s))))))

(5am:test result
  (5am:is (eql #\a (funcall (result #\a) nil))))

(5am:test choice
  (5am:signals failure
    (parse (choice) "a"))
  (5am:is (eql #\a (parse (choice (specific-char #\a)) "a")))
  (5am:is (eql #\b (parse (choice (specific-char #\b)) "b")))
  (5am:signals failure
    (parse (choice (specific-char #\a)) "b"))
  (5am:is (eql #\a
               (parse (choice (specific-char #\a)
                              (specific-char #\b))
                      "a")))
  (5am:is (eql #\b
               (parse (choice (specific-char #\a)
                              (specific-char #\b))
                      "b")))
  (5am:signals failure
    (parse (choice (specific-char #\a)
                   (specific-char #\b))
           "c")))

(5am:test fail
  (5am:signals failure (funcall (fail) nil)))

(5am:test try
  (5am:is (eql #\a (parse (try (specific-char #\a)) "a")))
  (5am:is (eql #\b (parse (try (specific-char #\b)) "b")))
  (5am:signals failure
    (parse (try (specific-char #\a)) "b")))

(5am:test many
  (5am:is (equal '(#\a) (parse (many (any-char)) "a")))
  (5am:is (equal '(#\b) (parse (many (any-char)) "b")))
  (let ((s (parser-stream "ab")))
    (multiple-value-bind (rv rs)
        (parse (many (specific-char #\b)) s)
      (declare (ignore rv))
      (5am:is (eq s rs)))
    (multiple-value-bind (rv rs)
        (parse (many (specific-char #\a)) s)
      (declare (ignore rv))
      (5am:is (eq (parser-stream-cdr s) rs)))))

(5am:def-suite character :in all)
(5am:in-suite character)

(5am:test specific-char
  (5am:is (eql #\a (parse (specific-char #\a) "a")))
  (5am:is (eql #\b (parse (specific-char #\b) "b")))
  (5am:signals failure
    (parse (specific-char #\a) "b")))

(5am:test specific-string
  (5am:is (equal "a" (parse (specific-string "a") "a")))
  (5am:is (equal "b" (parse (specific-string "b") "b")))
  (5am:signals failure
    (parse (specific-string "a") "b"))
  (5am:is (equal "a" (parse (specific-string "a") "abc")))
  (let ((s (parser-stream "string")))
    (handler-case (funcall (specific-string "strong") s)
      (parser-error (c)
        (5am:is (eq (parser-stream-cdr
                     (parser-stream-cdr
                      (parser-stream-cdr s)))
                    (parser-error-stream c)))))))

(5am:test one-of
  (5am:is (eql #\a (parse (one-of #\a) "a")))
  (5am:is (eql #\b (parse (one-of #\b) "b")))
  (5am:signals failure
    (parse (one-of #\a) "b"))
  (5am:is (eql #\a (parse (one-of #\a #\b) "a")))
  (5am:is (eql #\b (parse (one-of #\a #\b) "b")))
  (5am:signals failure
    (parse (one-of #\a #\b) "c")))

(5am:test none-of
  (5am:is (eql #\a (parse (none-of #\b) "a")))
  (5am:is (eql #\b (parse (none-of #\c) "b")))
  (5am:signals failure
    (parse (none-of #\a) "a"))
  (5am:is (eql #\a (parse (none-of #\b #\c) "a")))
  (5am:is (eql #\b (parse (none-of #\a #\c) "b")))
  (5am:signals failure
    (parse (none-of #\a #\b) "b")))

(5am:test any-char
  (5am:is (eql #\a (parse (any-char) "a")))
  (5am:is (eql #\Space (parse (any-char) " "))))

(5am:test upper
  (5am:is (eql #\A (parse (upper) "A")))
  (5am:is (eql #\B (parse (upper) "B")))
  (5am:signals failure
    (parse (upper) "a")))

(5am:test lower
  (5am:is (eql #\a (parse (lower) "a")))
  (5am:is (eql #\b (parse (lower) "b")))
  (5am:signals failure
    (parse (lower) "A")))

(5am:test letter
  (5am:is (eql #\a (parse (letter) "a")))
  (5am:is (eql #\A (parse (letter) "A")))
  (5am:signals failure
    (parse (letter) "!")))

(5am:test alpha-num
  (5am:is (eql #\a (parse (alpha-num) "a")))
  (5am:is (eql #\0 (parse (alpha-num) "0")))
  (5am:signals failure
    (parse (alpha-num) " ")))

(5am:test digit
  (5am:is (eql #\0 (parse (digit) "0")))
  (5am:is (eql #\1 (parse (digit) "1")))
  (5am:is (eql #\f (parse (digit 16) "f")))
  (5am:signals failure
    (parse (digit) "a"))
  (5am:signals failure
    (parse (digit 16) "g")))

(5am:test hex-digit
  (5am:is (eql #\0 (parse (hex-digit) "0")))
  (5am:is (eql #\a (parse (hex-digit) "a")))
  (5am:is (eql #\F (parse (hex-digit) "F")))
  (5am:signals failure
    (parse (hex-digit) "g")))

(5am:test oct-digit
  (5am:is (eql #\0 (parse (oct-digit) "0")))
  (5am:is (eql #\7 (parse (oct-digit) "7")))
  (5am:signals failure
    (parse (oct-digit) "8")))

(5am:test newline
  (5am:is (eql #\Newline (parse (newline) "
")))
  (5am:signals failure
    (parse (newline) "a")))

(5am:test tab
  (5am:is (eql #\Tab (parse (tab) "	")))
  (5am:signals failure
    (parse (tab) "a")))

(5am:test space
  (5am:is (eql #\Space (parse (space) " ")))
  (5am:is (eql #\Page (parse (space) "")))
  (5am:is (eql #\Tab (parse (space) "	")))
  (5am:is (eql #\Newline (parse (space) "
")))
  (5am:signals failure
    (parse (space) "a")))

(5am:test spaces
  (5am:is (eq nil (parse (spaces) "")))
  (let ((s (parser-stream " ")))
    (5am:is (eq (parser-stream-cdr s)
                (multiple-value-bind (rv rs)
                    (parse (spaces) s)
                  (declare (ignore rv))
                  rs))))
  (let ((s (parser-stream " a")))
    (5am:is (eq (parser-stream-cdr s)
                (multiple-value-bind (rv rs)
                    (parse (spaces) s)
                  (declare (ignore rv))
                  rs))))
  (let ((s (parser-stream "  a")))
    (5am:is (eq (parser-stream-cdr
                 (parser-stream-cdr s))
                (multiple-value-bind (rv rs)
                    (parse (spaces) s)
                  (declare (ignore rv))
                  rs)))))
