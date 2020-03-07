(in-package #:fouric)

;; trying to figure out how to allow in-tree code to be used if the given package/system isn't available
#++(handler-bind
    ((sb-int:extension-failure
       #'(lambda (c)
           (format t "could not load package~%"))))
  (eval '(require 'foobar)))
