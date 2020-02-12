(in-package #:fouric)

;; only a text narrower for now!
;; use tests to develop - or at least try

(defclass narrower ()
  ((haystack :accessor haystack :initarg :haystack)
   (needle :accessor needle :initarg :needle)
   (results :accessor results :initarg :results)
   (result-hooks :accessor result-hooks :initarg :result-hooks)
   (key-function :accessor key-function :initarg :key-function)
   (spyglass :accessor spyglass :initarg :spyglass)
   (cache :accessor cache :initarg :cache)))

;; use like (narrow '("foo" "bar" "baz") "a" #'identity #'nonconsecutive-substring-match)
(defun narrow (haystack needle key spyglass)
  ;; "haystack" is a LIST of the same type as STRAW, that will be searched through
  ;; "needle" is a single object, that is used to search through the haystack
  ;; "key" is a function object taking a single item of same type as STRAW and returning the actual data that will be used to filter (which has same type as NEEDLE)
  (mapcar #'cdr (sort (remove-if-not #'car (loop :for straw :in haystack :collect (cons (funcall spyglass needle (funcall key straw)) straw)))
                      (lambda (first second)
                        (< (car first)
                           (car second))))))

;; TODO: state for whether to reset on selection
;; TODO: logic to fire hooks
;; TODO: "reset" function?

;; should probably just move logic into needle-append and needle-backspace
(defun narrow-mutate (narrower)
  (let* ((default (gensym))
         (cache (gethash (needle narrower) (cache narrower) default)))
    (setf (results narrower) (if (eq cache default)
                                 (if (string= (needle narrower) "")
                                     (progn
                                       (format t "used haystack~%")
                                       (haystack narrower))
                                     (let ((narrowed (narrow-object narrower)))
                                       (format t "generated new cache value for ~s~%" (needle narrower))
                                       (setf (gethash (needle narrower) (cache narrower)) narrowed)
                                       narrowed))
                                 (progn
                                   (format t "used cache value for ~s~%" (needle narrower))
                                   cache)))
    (run-hooks narrower)))
(defun narrow-object (narrower)
  (narrow (haystack narrower) (needle narrower) (key-function narrower) (spyglass narrower)))

;; TODO: replace setf haystack with normal accessor function (?)
(defmethod (setf haystack) :after (value (self narrower))
  (let ((results (narrow-object self)))
    (setf (cache self) nil
          (results self) nil
          (gethash (needle self) (cache self)) results))
  (run-hooks self))

(defmethod needle-append ((narrower narrower) character)
  (with-accessors+ (cache key-function spyglass haystack) narrower
    (let* ((new-needle (concatenate 'string (needle narrower) (coerce (list character) 'string)))
           (cached-results (gethash new-needle cache 'uncached)))
      (setf (results narrower) (if (eq cached-results 'uncached)
                                   (let ((results (narrow haystack new-needle key-function spyglass)))
                                     (format t "generated new cache value for ~s~%" new-needle)
                                     (setf (gethash new-needle cache) results))
                                   (progn
                                     (format t "used cache value for ~s~%" new-needle)
                                     cached-results))
            (needle narrower) new-needle)))
  (run-hooks narrower)
  (format t "results: ~s~%" (results narrower)))
(defmethod needle-backspace ((narrower narrower))
  (with-accessors+ (cache key-function spyglass haystack) narrower
    (let* ((new-needle (subseq (needle narrower) 0 (max 0 (1- (length (needle narrower)))))))
      (setf (results narrower) (if (not (string= new-needle ""))
                                   (let ((cached-results (gethash new-needle cache 'uncached)))
                                     (if (eq cached-results 'uncached)
                                         (let ((results (narrow haystack new-needle key-function spyglass)))
                                           (format t "generated new cache value for ~s~%" new-needle)
                                           (setf (gethash new-needle cache) results))
                                         (progn
                                           (format t "used cache value for ~s~%" new-needle)
                                           cached-results)))
                                   (progn
                                     (format t "used haystack for results~%")
                                     haystack))
            (needle narrower) new-needle)))
  (run-hooks narrower)
  (format t "results: ~s~%" (results narrower)))
(defmethod needle-clear ((narrower narrower))
  (setf (needle narrower) ""
        (results narrower) (haystack narrower))
  (run-hooks narrower)
  (format t "results: ~s~%" (results narrower)))

(defun run-hooks (narrower)
  (dolist (hook (result-hooks narrower))
    (funcall hook narrower)))

(defun reset (narrower)
  (setf (cache narrower) (make-hash-table :test 'equal)
        (needle narrower) ""))

(defun make-narrower (&key (spyglass #'nonconsecutive-substring-match) (key #'identity) hooks haystack (needle ""))
  (let ((obj (make-instance 'narrower
                            :haystack haystack
                            :needle needle
                            :key-function key
                            :spyglass spyglass
                            :result-hooks hooks
                            :cache (make-hash-table :test 'equal)
                            :results (narrow haystack needle key spyglass))))
    (dolist (hook hooks)
      (funcall hook obj))
    obj))
