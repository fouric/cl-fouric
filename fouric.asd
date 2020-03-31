;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(asdf:defsystem fouric
  :name "fouric"
  :description "fouric's code library"
  :version "0.0.0"
  :maintainer "fouric <fouric@protonmail.com>"
  :author "fouric <fouric@protonmail.com>"
  :license "MIT"

  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "types")
               (:file "iteration")
               (:file "io")
               (:file "macro")
               (:file "object-access")
               (:file "cons")
               (:file "strings")
               (:file "misc")
               (:file "control-flow")
               (:file "constants")
               (:file "charms")
               (:file "narrow")
               (:file "sdl2")
               (:file "run-tests")
               )

  :depends-on (:sdl2 :sdl2-ttf :swank #+sbcl :sb-sprof :trivial-shell :alexandria :trivial-indent :cl-charms))
