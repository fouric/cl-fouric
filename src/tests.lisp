(in-package #:fouric)

;; TODO: ask whether to use symbol-plists or hash tables
;; actually, should almost certainly use a hash table in the fouric package, to prevent pollution...
;; or maybe just need to CLEAR-TESTS for everything before deploying binary
;; TODO: ask #lisp and lisp discord about this

#++(progn
  (setf *features* (remove :fouric/hashtable *features*))
  (push :fouric/plist *features*))

#+fouric/plist
(defun run-tests (name)
  ;;(format t "~&running tests for ~a::~s~%" (package-name (symbol-package name)) name)
  (a:doplist (test-name test-function (get name 'function-tests))
      ;;(format t "~&running test named ~s~%" test-name)
    (funcall test-function)))

#+fouric/plist
(defun add-test (function-name test-name &optional test)
  ;; (get function-name 'function-tests) returns the property list value for 'function-tests for the symbol given in FUNCTION-NAME
  ;; now that we have the 'function-tests property, we're going to store a plist in it
  ;; the keys of the plist are going to be symbols naming tests, and the values are going to be functions to be run
  (setf (getf (get function-name 'function-tests) test-name) (or test (symbol-function test-name))))

#+fouric/plist
(defun remove-test (function-name test-name)
  (a:remove-from-plistf (get function-name 'function-tests) test-name))

#+fouric/plist
(defun list-tests (function-name)
  (get function-name 'function-tests))

#+fouric/plist
(defun clear-tests (function-name)
  (setf (get function-name 'function-tests) nil))



(progn
  (setf *features* (remove :fouric/plist *features*))
  (push :fouric/hashtable *features*))

#+fouric/hashtable
(defparameter *tests* (make-hash-table))

#+fouric/hashtable
(defun run-all-tests (package)
  ;; every key in the hash table is a symbol naming a function
  ;; and every value is a plist of tests for that function
  (let ((package (if (packagep package)
                     package
                     (find-package package))))
    (maphash (lambda (function-name function-tests)
               (when (eq (symbol-package function-name) package)
                 (a:doplist (test-name test-function function-tests)
                     ;;(format t "~&running test named ~s~%" test-name)
                     (funcall test-function)))) *tests*)))

#+fouric/hashtable
(defun run-tests (name)
  ;;(format t "~&running tests for ~a::~s~%" (package-name (symbol-package name)) name)
  (a:doplist (test-name test-function (gethash name *tests*))
      ;;(format t "~&running test named ~s~%" test-name)
    (funcall test-function)))

#+fouric/hashtable
(defun add-test (function-name test-name &optional test)
  ;; (get function-name 'function-tests) returns the property list value for 'function-tests for the symbol given in FUNCTION-NAME
  ;; now that we have the 'function-tests property, we're going to store a plist in it
  ;; the keys of the plist are going to be symbols naming tests, and the values are going to be functions to be run
  (setf (getf (gethash function-name *tests*) test-name) (or test (symbol-function test-name))))

#+fouric/hashtable
(defun remove-test (function-name test-name)
  (a:remove-from-plistf (gethash function-name *tests*) test-name))

#+fouric/hashtable
(defun list-tests (function-name)
  (gethash function-name *tests*))

#+fouric/hashtable
(defun clear-tests (function-name)
  (setf (gethash function-name *tests*) nil))
