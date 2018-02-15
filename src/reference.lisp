(defun main-function ()
  (ext:exit))

;; clisp
;;(ext:saveinitmem "filename.elf"
;;  :quiet t
;;  :init-function 'main-function
;;  :executable t
;;  :norc t)

;; sbcl
;; (sb-ext:save-lisp-and-die "filename.elf" :toplevel 'main-function :executable t)

(defparameter dx 0.0001)
(defun derive (f)
  (lambda (x)
    (/ (- (funcall f (+ x dx)) (funcall f x)) dx)))
(defun cube (x)
  (* x x x))
(funcall (derive (function cube)) 2)

define setf interactions on a thing
(defun (setf TYPE) (thing)
  (setf (field TYPE) thing))

(defparameter *var* 2)
(defparameter *thread* (bordeaux-threads:make-thread (lambda () (+ *var* 1))))
(bordeaux-threads:join-thread *thread*)
