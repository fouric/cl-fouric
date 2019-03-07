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
               (:file "iteration")
               (:file "io")
               (:file "macro")
               (:file "object-access")
               (:file "strings")
               (:file "misc")
               (:file "control-flow")
               (:file "constants")
               (:file "sdl2"))

  :depends-on (:sdl2 :sdl2-ttf :swank #+sbcl :sb-sprof :trivial-shell :alexandria))
