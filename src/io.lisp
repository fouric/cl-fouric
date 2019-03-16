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

;; from mfiano
(defun map-files (path function &key (test (constantly t)) (recursive-p t))
  "Map over all files located in the directory of `PATH`, applying `FUNCTION` to each file's path.
`TEST` is a function that takes a file path and decides if `FUNCTION` should be applied to it.
`RECURSIVE-P`, when non-NIL will descend into sub-directories of `PATH` recursively."
  (labels ((process-files (dir)
             (map nil
                  (lambda (x)
                    (when (funcall test x)
                      (funcall function x)))
                  (uiop/filesystem:directory-files dir))))
    (uiop/filesystem:collect-sub*directories
     (uiop/pathname:ensure-directory-pathname path) t recursive-p #'process-files)))
