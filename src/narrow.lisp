(in-package #:fouric)

;; only a text narrower for now!

(defclass narrower ()
  ((haystack :accessor haystack :initarg :haystack)
   (needle :accessor needle :initarg :needle)
   (results :accessor results :initarg :results)
   (result-hooks :accessor result-hooks :initarg :result-hooks)
   (key-function :accessor key-function :initarg :key-function)
   (spyglass :accessor spyglass :initarg :spyglass)
   (cache :accessor cache :initarg :cache)))

;; use like (narrow '("foo" "bar" "baz") "a" (lambda (x) x) #'nonconsecutive-substring-match)
(defun narrow (haystack needle key spyglass)
  ;; "haystack" is a LIST of the same type as STRAW, that will be searched through
  ;; "needle" is a single object, that is used to search through the haystack
  ;; "key" is a function object taking a single item of same type as STRAW and returning the actual data that will be used to filter (which has same type as NEEDLE)
  (mapcar #'cdr (sort (remove-if-not #'car (loop :for straw :in haystack :collect (cons (funcall spyglass needle (funcall key straw)) straw)))
                      (lambda (first second)
                        (< (car first)
                           (car second))))))

;; should probably just move logic into needle-append and needle-backspace
(defun narrow-mutate (narrower)
  (let* ((default (gensym))
         (cache (gethash (needle narrower) (cache narrower) default)))
    (setf (results narrower) (if (eq cache default)
                                 (let ((narrowed (narrow-object narrower)))
                                   (format t "generated new cache value for ~s~%" (needle narrower))
                                   (setf (gethash (needle narrower) (cache narrower)) narrowed)
                                   narrowed)
                                 (progn
                                   (format t "used cache value for ~s~%" (needle narrower))
                                   cache)))))
(defun narrow-object (narrower)
  (narrow (haystack narrower) (needle narrower) (key-function narrower) (spyglass narrower)))

(defmethod (setf haystack) :after (value (self narrower))
  (setf (cache self) nil)
  (narrow-mutate self))

(defmethod needle-append ((self narrower) character)
  (setf (needle self) (concatenate 'string (needle self) (coerce (list character) 'string)))
  (narrow-mutate self)
  (format t "results: ~s~%" (results self)))
(defmethod needle-backspace ((self narrower))
  (setf (needle self) (subseq (needle self) 0 (max 0 (1- (length (needle self))))))
  (narrow-mutate self)
  (format t "results: ~s~%" (results self)))
(defmethod needle-clear ((self narrower))
  (setf (needle self) "")
  (narrow-mutate self)
  (format t "results: ~s~%" (results self)))

(defmethod (setf results) :after (value (self narrower))
  (mapcar (lambda (hook) (funcall hook self)) (result-hooks self)))

(defun make-narrower (&key filter key hooks haystack needle)
  (let ((obj (make-instance 'narrower
                            :haystack haystack
                            :needle (or needle "")
                            :key-function (or key (lambda (x) x))
                            :spyglass (or filter #'nonconsecutive-substring-match)
                            :result-hooks hooks
                            :cache (make-hash-table :test 'equal))))
    (narrow-mutate obj)
    (dolist (hook hooks)
      (funcall hook obj))
    obj))
