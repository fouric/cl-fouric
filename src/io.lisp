(in-package #:fouric)

(defun read-file (filename)
  (with-open-file (in filename
                      :direction :input
                      :if-exists :supersede)
    (with-standard-io-syntax
      (read in))))

(defun write-file (filename object)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print object out))))

;; just a shortcut for getting a thing from a place relative to a path
(defun resource (path system)
  (asdf:system-relative-pathname system path))
