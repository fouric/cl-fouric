(in-package #:fouric)

(defun to-string (item)
  (coerce item 'string))

(defun to-list (item)
  (coerce item 'list))

(defun strcat (first &rest others)
  (apply #'concatenate 'string first others))

(defun symcat (first &rest others)
  (intern (string-upcase (apply 'strcat first others))))

(defun nonconsecutive-substring-match (needle haystack)
  (labels ((rec (needle haystack)
             (if (not needle)
                 0
                 (multiple-value-bind (rest pos) (nmember (first needle) haystack)
                   (when rest
                     (a:when-let ((rec (rec (rest needle) (rest rest))))
                       (+ pos rec)))))))
    (rec (coerce needle 'list) (coerce haystack'list))))

(defun genspaces (count)
  (if (zerop count)
      ""
      (concatenate 'string " " (genspaces (1- count)))))
