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

(defun file-lines (filename &optional filter)
  "read in the named file into a list of strings, one per line, and optionall call FILTER on each line"
  (with-open-file (in filename
                      :direction :input
                      :if-exists :supersede)
    (loop for line = (read-line in nil) while line collect (if filter (funcall filter line) line))))

;; just a shortcut for getting a thing from a place relative to a path
(defun resource (path &optional system)
  (asdf:system-relative-pathname (if system system (intern (package-name *package*))) path))
