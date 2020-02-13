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
  (setf (results narrower) (if (string= (needle narrower) "")
                               (haystack narrower)
                               (let* ((default (gensym))
                                      (cache (gethash (needle narrower) (cache narrower) default)))
                                 (if (eq cache default)
                                     (let ((narrowed (narrow-object narrower)))
                                       (setf (gethash (needle narrower) (cache narrower)) narrowed))
                                     cache))))
  (run-hooks narrower))
(defun narrow-object (narrower)
  (narrow (haystack narrower) (needle narrower) (key-function narrower) (spyglass narrower)))

;; TODO: replace setf haystack with normal accessor function (?)
(defmethod (setf haystack) :after (value (self narrower))
  (let ((results (narrow-object self)))
    (setf (cache self) nil
          (results self) nil
          (gethash (needle self) (cache self)) results))
  (run-hooks self))

;;; TODO: specialize needle-append to be more efficient by exploiting the fact that the results are a subset of those with the current/old needle
;;; TODO: POTENTIAL flexibility enhancement (don't use unless necessary): specialize to needle-append-char and needle-append-string
(defun needle-append (narrower character)
  (setf (needle narrower) (concatenate 'string (needle narrower) (coerce (list character) 'string)))
  (narrow-mutate narrower))
;;; potential flexibility enhancement: allow for &optional count argument that deletes that number of characters
(defun needle-backspace (narrower)
  (setf (needle narrower) (subseq (needle narrower) 0 (max 0 (1- (length (needle narrower))))))
  (narrow-mutate narrower))
(defun needle-clear (narrower)
  (setf (needle narrower) "")
  (narrow-mutate narrower))

(defun run-hooks (narrower)
  (dolist (hook (result-hooks narrower) narrower)
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
    (dolist (hook hooks obj)
      (funcall hook obj))))

(defun test-narrower ()
  ;; what if no haystack?
  (let* ((haystack '("foo" "foob" "bar" "baz" "zuul"))
         (n (make-narrower :haystack haystack)))
    (unless (equal (results n) haystack)
      (error "results not equal to haystack"))
    (needle-append n #\f)
    (unless (equal (results n) '("foo" "foob"))
      (error "did not narrow for f"))
    (needle-append n #\o)
    (unless (equal (results n) '("foo" "foob"))
      (error "did not narrow for o"))
    (needle-append n #\o)
    (unless (equal (results n) '("foo" "foob"))
      (error "did not narrow for o a second time"))
    (needle-backspace n)
    (unless (equal (results n) '("foo" "foob"))
      (error "did not backspace properly"))
    (needle-append n #\o)
    (needle-append n #\b)
    (unless (equal (results n) '("foob"))
      (error "did not narrow to foob"))
    (needle-backspace n)
    (needle-backspace n)
    (needle-backspace n)
    (needle-backspace n)
    (needle-append n #\o)
    (unless (equal (results n) '("foo" "foob"))
      (error "did not select using characters in middle of haystack entries"))
    (needle-backspace n)
    (needle-append n #\z)
    (unless (equal (results n) '("zuul"))
      (error "did not select zuul"))))
