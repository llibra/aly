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
  (5am:is (eql #\a (parse "a" (any-char))))
  (5am:is (eql #\b (parse "b" (any-char))))
  (5am:is (equal '(#\a . #\1)
                 (parse "a1"
                   (c <- (any-char))
                   (d <- (digit))
                   (result (cons c d)))))
  (5am:is (eql #\c (multiple-value-bind (r s)
                       (parse "abc" (any-char) (any-char))
                     (declare (ignore r))
                     (car (parser-stream-car s))))))

(5am:test result
  (5am:is (eql #\a (funcall (result #\a) nil))))

(5am:test sequence
  (5am:signals parser-error
    (parse "a" (sequence)))
  (5am:is (eql #\a (parse "a" (sequence (specific-char #\a)))))
  (5am:signals parser-error
    (parse "b" (sequence (specific-char #\a))))
  (5am:is (eql #\a (parse "aa"
                     (sequence (specific-char #\a)
                               (specific-char #\a)))))
  (5am:is (eql #\b (parse "ab"
                     (sequence (specific-char #\a)
                               (specific-char #\b))))))

(5am:test choice
  (5am:signals parser-error
    (parse "a" (choice)))
  (5am:signals parser-error
    (parse "b" (choice (specific-char #\a))))
  (5am:is (eql #\a (parse "a" (choice (specific-char #\a)))))
  (5am:is (eql #\a (parse "a"
                     (choice (specific-char #\a)
                             (specific-char #\b)))))
  (5am:is (eql #\b (parse "b"
                     (choice (specific-char #\a)
                             (specific-char #\b)))))
  (5am:signals parser-error
    (parse "c" (choice (specific-char #\a)
                       (specific-char #\b)))))

(5am:test fail
  (5am:signals parser-error (funcall (fail) nil)))

(5am:test try
  (5am:is (eql #\a (parse "a" (try (specific-char #\a)))))
  (5am:signals unexpected-datum
    (parse "b" (try (specific-char #\a)))))

(5am:test many
  (5am:is (equal '(#\a) (parse "a" (many (any-char)))))
  (5am:is (equal '(#\b) (parse "b" (many (any-char)))))
  (let ((s (parser-stream "ab")))
    (multiple-value-bind (rv rs)
        (parse s (many (specific-char #\b)))
      (declare (ignore rv))
      (5am:is (eq s rs)))
    (multiple-value-bind (rv rs)
        (parse s (many (specific-char #\a)))
      (declare (ignore rv))
      (5am:is (eq (parser-stream-cdr s) rs)))))

(5am:def-suite character :in all)
(5am:in-suite character)

(5am:test specific-char
  (5am:is (eql #\a (parse "a" (specific-char #\a))))
  (5am:is (eql #\b (parse "b" (specific-char #\b))))
  (5am:signals unexpected-datum
    (parse "b" (specific-char #\a))))

(5am:test specific-string
  (5am:is (equal "a" (parse "a" (specific-string "a"))))
  (5am:is (equal "b" (parse "b" (specific-string "b"))))
  (5am:signals unexpected-datum
    (parse "b" (specific-string "a")))
  (5am:is (equal "a" (parse "abc" (specific-string "a"))))
  (let ((s (parser-stream "string")))
    (handler-case (funcall (specific-string "strong") s)
      (parser-error (c)
        (5am:is (eq (parser-stream-cdr
                     (parser-stream-cdr
                      (parser-stream-cdr s)))
                    (parser-error-stream c)))))))

(5am:test one-of
  (5am:is (eql #\a (parse "a" (one-of #\a))))
  (5am:is (eql #\b (parse "b" (one-of #\b))))
  (5am:signals unexpected-datum
    (parse "b" (one-of #\a)))
  (5am:is (eql #\a (parse "a" (one-of #\a #\b))))
  (5am:is (eql #\b (parse "b" (one-of #\a #\b))))
  (5am:signals unexpected-datum
    (parse "c" (one-of #\a #\b))))

(5am:test none-of
  (5am:is (eql #\a (parse "a" (none-of #\b))))
  (5am:is (eql #\b (parse "b" (none-of #\c))))
  (5am:signals unexpected-datum
    (parse "a" (none-of #\a)))
  (5am:is (eql #\a (parse "a" (none-of #\b #\c))))
  (5am:is (eql #\b (parse "b" (none-of #\a #\c))))
  (5am:signals unexpected-datum
    (parse "b" (none-of #\a #\b))))

(5am:test any-char
  (5am:is (eql #\a (parse "a" (any-char))))
  (5am:is (eql #\Space (parse " " (any-char)))))

(5am:test upper
  (5am:is (eql #\A (parse "A" (upper))))
  (5am:is (eql #\B (parse "B" (upper))))
  (5am:signals unexpected-datum
    (parse "a" (upper))))

(5am:test lower
  (5am:is (eql #\a (parse "a" (lower))))
  (5am:is (eql #\b (parse "b" (lower))))
  (5am:signals unexpected-datum
    (parse "A" (lower))))

(5am:test letter
  (5am:is (eql #\a (parse "a" (letter))))
  (5am:is (eql #\A (parse "A" (letter))))
  (5am:signals unexpected-datum
    (parse "!" (letter))))

(5am:test alpha-num
  (5am:is (eql #\a (parse "a" (alpha-num))))
  (5am:is (eql #\0 (parse "0" (alpha-num))))
  (5am:signals unexpected-datum
    (parse " " (alpha-num))))

(5am:test digit
  (5am:is (eql #\0 (parse "0" (digit))))
  (5am:is (eql #\1 (parse "1" (digit))))
  (5am:is (eql #\f (parse "f" (digit 16))))
  (5am:signals unexpected-datum
    (parse "a" (digit)))
  (5am:signals unexpected-datum
    (parse "g" (digit 16))))

(5am:test hex-digit
  (5am:is (eql #\0 (parse "0" (hex-digit))))
  (5am:is (eql #\a (parse "a" (hex-digit))))
  (5am:is (eql #\F (parse "F" (hex-digit))))
  (5am:signals unexpected-datum
    (parse "g" (hex-digit))))

(5am:test oct-digit
  (5am:is (eql #\0 (parse "0" (oct-digit))))
  (5am:is (eql #\7 (parse "7" (oct-digit))))
  (5am:signals unexpected-datum
    (parse "8" (oct-digit))))

(5am:test newline
  (5am:is (eql #\Newline (parse "
" (newline))))
  (5am:signals unexpected-datum
    (parse "a" (newline))))

(5am:test tab
  (5am:is (eql #\Tab (parse "	" (tab))))
  (5am:signals unexpected-datum
    (parse "a" (tab))))

(5am:test space
  (5am:is (eql #\Space (parse " " (space))))
  (5am:is (eql #\Page (parse "" (space))))
  (5am:is (eql #\Tab (parse "	" (space))))
  (5am:is (eql #\Newline (parse "
" (space))))
  (5am:signals unexpected-datum
    (parse "a" (space))))

(5am:test spaces
  (5am:is (eq nil (parse "" (spaces))))
  (let ((s (parser-stream " ")))
    (5am:is (eq (parser-stream-cdr s)
                (multiple-value-bind (rv rs)
                    (parse s (spaces))
                  (declare (ignore rv))
                  rs))))
  (let ((s (parser-stream " a")))
    (5am:is (eq (parser-stream-cdr s)
                (multiple-value-bind (rv rs)
                    (parse s (spaces))
                  (declare (ignore rv))
                  rs))))
  (let ((s (parser-stream "  a")))
    (5am:is (eq (parser-stream-cdr
                 (parser-stream-cdr s))
                (multiple-value-bind (rv rs)
                    (parse s (spaces))
                  (declare (ignore rv))
                  rs)))))
