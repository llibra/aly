(defpackage :aly.asdf (:use :cl :asdf))
(in-package :aly.asdf)

(defsystem :aly
  :version "0.1"
  :author "Manabu Takayama <learn.libra@gmail.com>"
  :licence "MIT License"
  :depends-on (:alexandria :anaphora :cl-pattern)
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "util")
                                     (:file "stream")
                                     (:file "condition")
                                     (:file "core")
                                     (:file "combinator")
                                     (:file "char")))))
