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

(defmethod needle-append ((self narrower) character)
  (with-accessors+ (cache key-function spyglass haystack) self
    (let* ((old-needle (needle self))
           (new-needle (concatenate 'string old-needle (coerce (list character) 'string)))
           (cached-results (gethash old-needle cache 'uncached)))
      (setf (results self) (if (eq cached-results 'uncached)
                               (let* ((last-level-cache (if (string= old-needle "")
                                                            haystack
                                                            (ensure-gethash old-needle cache (narrow haystack old-needle key-function spyglass))))
                                      (results (narrow last-level-cache new-needle key-function spyglass)))
                                 (format t "generated new cache value for ~s~%" new-needle)
                                 (setf (gethash (needle self) cache) results))
                               (progn
                                 (format t "used cache value for ~s~%" new-needle)
                                 cached-results))
            (needle self) new-needle)))
  (run-hooks self)
  (format t "results: ~s~%" (results self)))
(defmethod needle-backspace ((self narrower))
  (with-accessors+ (cache key-function spyglass haystack) self
    (let* ((old-needle (needle self))
           (new-needle (subseq (needle self) 0 (max 0 (1- (length (needle self))))))
           (cached-results (gethash old-needle cache 'uncached)))
      (setf (results self) (if (eq cached-results 'uncached)
                               (if (not (string= new-needle ""))
                                   (let ((results (ensure-gethash new-needle cache (narrow haystack new-needle key-function spyglass))))
                                     (format t "generated new cache value for ~s~%" new-needle)
                                     (setf (gethash (needle self) cache) results))
                                   (progn
                                     (format t "used haystack for results~%")
                                     haystack))
                               (progn
                                 (format t "used cache value for ~s~%" new-needle)
                                 cached-results))
            (needle self) new-needle)))
  (run-hooks self)
  (format t "results: ~s~%" (results self)))
(defmethod needle-clear ((self narrower))
  (setf (needle self) ""
        (results self) (haystack self))
  (run-hooks self)
  (format t "results: ~s~%" (results self)))

(defun run-hooks (self)
  (dolist (hook (result-hooks self))
    (funcall hook self)))

(defun make-narrower (&key filter key hooks haystack needle)
  (let ((obj (make-instance 'narrower
                            :haystack haystack
                            :needle (or needle "")
                            :key-function (or key #'identity)
                            :spyglass (or filter #'nonconsecutive-substring-match)
                            :result-hooks hooks
                            :cache (make-hash-table :test 'equal))))
    (setf (result obj) haystack)
    (dolist (hook hooks)
      (funcall hook obj))
    obj))
