(in-package #:fouriclib)

(defun to-string (item)
  (coerce item 'string))

(defun to-list (item)
  (coerce item 'list))

(defun strcat (first &rest others)
  (apply #'concatenate 'string first others))

(defun symcat (first &rest others)
  (intern (string-upcase (apply 'strcat first others))))
