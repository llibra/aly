(in-package :aly.char)

(defun specific-char (c)
  (expect (satisfy (curry #'eql c)) c))

;; TODO: Refactoring
(defun specific-string (string)
  (labels ((rec (stream1 index)
             (if (= index (length string))
                 (success nil stream1)
                 (ematch-values
                     (funcall (specific-char (aref string index)) stream1)
                   ((t _ stream2 _ _)
                    (rec stream2 (1+ index)))
                   ((nil pos msgs)
                    (failure pos msgs))))))
    (lambda (stream)
      (ematch-values (rec stream 0)
        ((t _ stream pos msgs)
         (success string stream pos msgs))
        ((nil pos msgs)
         (failure pos msgs))))))

(defun one-of (&rest cs)
  (expect (satisfy (rcurry #'member cs))
          (format nil "one of ~{~A~}" (intersperse ", " cs " and "))))

(defun none-of (&rest cs)
  (expect (satisfy (complement (rcurry #'member cs)))
          (format nil "except any of ~{~A~}" (intersperse ", " cs " and "))))

(defalias any-char
  (satisfy (constantly t)))

(defalias upper
  (expect (satisfy #'upper-case-p) "an uppercase letter"))

(defalias lower
  (expect (satisfy #'lower-case-p) "a lowercase letter"))

(defalias letter
  (expect (satisfy #'alpha-char-p) "a letter"))

(defalias alpha-num
  (expect (satisfy #'alphanumericp) "a letter or a digit"))

(defun digit (&optional (radix 10))
  (expect (satisfy (rcurry #'digit-char-p radix)) "a digit"))

(defalias decimal-digit
  (expect (satisfy (rcurry #'digit-char-p 10)) "a decimal digit"))

(defalias hex-digit
  (expect (satisfy (rcurry #'digit-char-p 16)) "a hexadecimal digit"))

(defalias oct-digit
  (expect (satisfy (rcurry #'digit-char-p 8)) "an octal digit"))

(defalias newline
  (expect (specific-char #\Newline) "a new line"))

(defalias tab
  (expect (specific-char #\Tab) "a tab"))

(defalias whitespace
  (expect (satisfy (lambda (c)
                     (some (curry #'eql c)
                           '(#\Space #\Page #\Tab #\Newline))))
          "a space"))

(defalias whitespaces
  (skip-many #'whitespace))
