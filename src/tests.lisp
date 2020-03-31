(in-package #:fouric)

(defun run-tests (name)
  ;;(format t "~&running tests for ~a::~s~%" (package-name (symbol-package name)) name)
  (a:doplist (test-name test-function (get name 'function-tests))
      ;;(format t "~&running test named ~s~%" test-name)
    (funcall test-function)))

(defun add-test (function-name test-name &optional test)
  ;; (get function-name 'function-tests) returns the property list value for 'function-tests for the symbol given in FUNCTION-NAME
  ;; now that we have the 'function-tests property, we're going to store a plist in it
  ;; the keys of the plist are going to be symbols naming tests, and the values are going to be functions to be run
  (setf (getf (get function-name 'function-tests) test-name) (or test (symbol-function test-name))))

(defun remove-test (function-name test-name)
  (a:remove-from-plistf (get function-name 'function-tests) test-name))

(defun list-tests (function-name)
  (get function-name 'function-tests))

(defun clear-tests (function-name)
  (setf (get function-name 'function-tests) nil))
