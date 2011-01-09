(defpackage :aly-test.asd (:use :cl :asdf))
(in-package :aly-test.asd)

(defsystem :aly-test
  :depends-on (:aly :fiveam)
  :components ((:module "t"
                        :serial t
                        :components ((:file "packages")
                                     (:file "test")))))
