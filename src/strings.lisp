(in-package #:fouric)

(defun to-string (item)
  (coerce item 'string))

(defun to-list (item)
  (coerce item 'list))

(defun strcat (first &rest others)
  (apply #'concatenate 'string first others))

(defun symcat (first &rest others)
  (intern (string-upcase (apply 'strcat first others))))

(defun nonconsecutive-substring-match (term item)
  (labels ((rec (term item)
             (if (not term)
                 0
                 (multiple-value-bind (rest pos) (nmember (first term) item)
                   (when rest
                     (+ pos (rec (rest term) (rest item))))))))
    (rec (coerce term 'list) (coerce item 'list))))
