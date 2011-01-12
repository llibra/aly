(in-package :aly.test)

(5am:def-suite parser)
(5am:in-suite parser)

(5am:test parse
  (5am:is (eql #\a (parse "a" (any-char))))
  (5am:is (eql #\b (parse "b" (any-char))))
  (5am:is (equal '(#\a . 1)
                 (parse "a1"
                   (c <- (any-char))
                   (d <- (digit))
                   (result (cons c d)))))
  (5am:is (eql #\c (multiple-value-bind (r s)
                       (parse "abc" (any-char) (any-char))
                     (car (parser-stream-car s))))))

(5am:test specific-char
  (5am:is (eql #\a (parse (specific-char #\a) "a")))
  (5am:is (eql #\b (parse (specific-char #\b) "b")))
  (5am:signals unexpected-datum
    (parse (specific-char #\a) "b")))

(5am:test specific-string
  (5am:is (equal "a" (parse (specific-string "a") "a")))
  (5am:is (equal "a" (parse (specific-string "a") "abc")))
  (let ((s "string"))
    (5am:is (equal s (parse (specific-string s) s)))
    (5am:signals unexpected-datum
      (parse (specific-string s) "another")))
  (let ((s (parser-stream "string")))
    (handler-case (funcall (specific-string "strong") s)
      (parser-error (c)
        (5am:is (eq s (parser-error-stream c)))))))

(5am:test any-char
  (5am:is (eql #\a (parse (any-char) "a")))
  (5am:is (eql #\Space (parse (any-char) " "))))

(5am:test upper
  (5am:is (eql #\A (parse (upper) "A")))
  (5am:is (eql #\B (parse (upper) "B")))
  (5am:signals unexpected-datum
    (parse (upper) "a")))

(5am:test lower
  (5am:is (eql #\a (parse (lower) "a")))
  (5am:is (eql #\b (parse (lower) "b")))
  (5am:signals unexpected-datum
    (parse (lower) "A")))

(5am:test letter
  (5am:is (eql #\a (parse (letter) "a")))
  (5am:is (eql #\A (parse (letter) "A")))
  (5am:signals unexpected-datum
    (parse (letter) "!")))

(5am:test alpha-num
  (5am:is (eql #\a (parse (alpha-num) "a")))
  (5am:is (eql #\0 (parse (alpha-num) "0")))
  (5am:signals unexpected-datum
    (parse (alpha-num) " ")))

(5am:test digit
  (5am:is (eql #\0 (parse (digit) "0")))
  (5am:is (eql #\1 (parse (digit) "1")))
  (5am:is (eql #\f (parse (digit 16) "f")))
  (5am:signals unexpected-datum
    (parse (digit) "a"))
  (5am:signals unexpected-datum
    (parse (digit 16) "g")))

(5am:test hex-digit
  (5am:is (eql #\0 (parse (hex-digit) "0")))
  (5am:is (eql #\a (parse (hex-digit) "a")))
  (5am:is (eql #\F (parse (hex-digit) "F")))
  (5am:signals unexpected-datum
    (parse (hex-digit) "g")))

(5am:test oct-digit
  (5am:is (eql #\0 (parse (oct-digit) "0")))
  (5am:is (eql #\7 (parse (oct-digit) "7")))
  (5am:signals unexpected-datum
    (parse (oct-digit) "8")))

(5am:test newline
  (5am:is (eql #\Newline (parse (newline) "
")))
  (5am:signals unexpected-datum
    (parse (newline) "a")))

(5am:test tab
  (5am:is (eql #\Tab (parse (tab) "	")))
  (5am:signals unexpected-datum
    (parse (tab) "a")))

(5am:test space
  (5am:is (eql #\Space (parse (space) " ")))
  (5am:is (eql #\Page (parse (space) "")))
  (5am:is (eql #\Tab (parse (space) "	")))
  (5am:is (eql #\Newline (parse (space) "
")))
  (5am:signals unexpected-datum
    (parse (space) "a")))

(5am:test return-result
  (5am:is (eql #\a (funcall (return-result #\a) nil))))

(5am:test sequence
  (5am:signals parser-error
    (parse (sequence) "a"))
  (5am:is (eql #\a (parse (sequence (specific-char #\a)) "a")))
  (5am:signals parser-error
    (parse (sequence (specific-char #\a)) "b"))
  (5am:is (eql #\a (parse (sequence (specific-char #\a)
                                    (specific-char #\a))
                          "aa")))
  (5am:is (eql #\b (parse (sequence (specific-char #\a)
                                    (specific-char #\b))
                          "ab"))))

(5am:test choice
  (5am:signals parser-error
    (parse (choice) "a"))
  (5am:signals parser-error
    (parse (choice (specific-char #\a)) "b"))
  ;(5am:signals unexpected-datum
  ;  (parse (choice (specific-string "aa")) "ab"))
  (5am:is (eql #\a (parse (choice (specific-char #\a)) "a")))
  (5am:is (eql #\a (parse (choice (specific-char #\a)
                                  (specific-char #\b))
                          "a")))
  (5am:is (eql #\b (parse (choice (specific-char #\a)
                                  (specific-char #\b))
                          "b")))
  (5am:signals parser-error
    (parse (choice (specific-char #\a)
                   (specific-char #\b))
           "c")))

(5am:test fail
  (5am:signals parser-error (funcall (fail) nil)))

(5am:test try
  (5am:is (eql #\a (parse (parser-try (specific-char #\a)) "a")))
  (5am:signals unexpected-datum
    (parse (parser-try (specific-char #\a)) "b")))
