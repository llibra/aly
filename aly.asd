(defpackage :aly.asd (:use :cl :asdf))
(in-package :aly.asd)

(defsystem :aly
  :version "0.1"
  :author "Manabu Takayama <learn.libra@gmail.com>"
  :licence "MIT License"
  :depends-on (:alexandria :anaphora)
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "parser")))))
