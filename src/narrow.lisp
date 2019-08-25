(in-package #:fouric)

(defclass narrower ()
  ((candidates :accessor candidates :initarg :candidates)
   (criteria :accessor criteria :initarg :criteria)
   (results :accessor results :initarg :results)
   (hook-list :accessor hook-list :initarg :hook-list)
   (key-function :accessor key-function :initarg :key-function)
   (filter-function :accessor filter-function :initarg :filter-function)))

;; use like (narrow '("foo" "bar" "baz") "a" nil #'search)
(defun narrow (candidates criteria &optional (key (lambda (x) x)) (filter #'eq))
  (remove-if-not (lambda (item) (funcall filter criteria item)) candidates :key key))

(defmethod (setf candidates) :after (value (self narrower))
  (setf (results self) (narrow (candidates self) (criteria self) (key-function self) (filter-function self))))
(defmethod (setf criteria) :after (value (self narrower))
  (setf (results self) (narrow (candidates self) (criteria self) (key-function self) (filter-function self))))
(defmethod (setf results) :after (value (self narrower))
  (mapcar (lambda (hook) (funcall hook self)) (hook-list self)))

(defun make-narrower (key filter &optional hooks candidates criteria)
  (let ((obj (make-instance 'narrower
                            :candidates candidates
                            :criteria criteria
                            :key-function key
                            :filter-function filter
                            :hook-list hooks
                            :results (narrow candidates criteria key filter))))
    (mapcar #'funcall hooks)
    obj))
