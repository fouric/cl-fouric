;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:fouriclib-asd
  (:use :cl :asdf))

(in-package :fouriclib-asd)

(defsystem fouriclib
  :name "fouriclib"
  :version "0.0.0"
  :maintainer "fouric"
  :author "fouric"
  :license "All rights reserved"
  :description "fouric's code library"

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
               (:file "sdl2"))

  :depends-on (:sdl2 :sdl2-ttf :swank))