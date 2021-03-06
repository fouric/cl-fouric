(in-package #:fouric)

;; create a tinylambda, closure-style
;; use like so: (mapcar (_ (+ 1 _)) '(3 5 6))
(defmacro _ (&body body)
  "an even more concise (albeit limited) lambda form"
  `(lambda (_) ,@body))

;; PROGN that collapses to just the provided form if only one is given - makes macro output cleaner
(defmacro iprogn (&body body)
  (if (< 1 (length body))
      `(progn
         ,@body)
      (first body)))

;;; TODO: use UNWIND-PROTECT for cleanup code and maybe ONCE-ONLY for gensyms

(defmacro make-alpha-with-macro (macro-name constructor destructor)
  "creates a macro that accepts an ARG and calls (CONSTRUCTOR ARG) and (DESTRUCTOR ARG)"
  ` (defmacro ,macro-name ((name args) &body body)
      `(let ((,name (,',constructor ,@args)))
         ,@body
         (,',destructor ,name))))

(defmacro make-with-macro (macro-name constructor destructor)
  `(defmacro ,macro-name ((name args) &body body)
     `(let ((,name (,',constructor ,@args)))
        ,@body
        (,',destructor ,name))))

(defmacro make-with-macros (macro-name constructor destructor)
  `(defmacro ,macro-name (shader-init-forms &body body)
     (let ((retval (gensym "RETVAL")))
       `(let ,(append (mapcar (lambda (initform) (cons (first initform) (list (cons ',constructor (first (rest initform)))))) shader-init-forms)
                      (list retval))
          ,@(butlast body)
          (setf ,retval ,(first (last body)))
          ,@(mapcar (lambda (initform) (list ',destructor (first initform))) shader-init-forms)
          ,retval))))

(defmacro make-with-macros-2 (macro-name constructor-form destructor-form)
  `(defmacro ,macro-name (shader-init-forms &body body)
     (let ((retval (gensym "RETVAL")))
       `(let ,(append (mapcar (lambda (initform) (cons (first initform) (list (cons ',constructor-form (first (rest initform)))))) shader-init-forms)
                      (list retval))
          ,@(butlast body)
          (setf ,retval ,(first (last body)))
          ,@(mapcar ,destructor-form shader-init-forms)
          ,retval))))

(defmacro with-accessors+ (slots instance &body body)
  `(with-accessors (,@(loop :for slot :in slots :collect (if (consp slot) slot (list slot slot)))) ,instance
     ,@body))

;; this is, ostensibly, useful for live-coding
(defmacro udefun (name lambda-list update-code &body body)
  "like normal defun, except runs UPDATE-CODE whenever you redefine the function and then call it again"
  (multiple-value-bind (body declarations docstring) (a:parse-body body :documentation t :whole 'udefun)
    `(defun ,name ,lambda-list
       ,@(list docstring)
       ,@declarations
       ,@(if update-code (list `(unless (eq (get ',name 'old-function-object) (if (fboundp ',name) (symbol-function ',name)))
                                  ,update-code)))
       (setf (get ',name 'old-function-object) (symbol-function ',name))
       ,@body)))

(defmacro cond? (&rest clauses)
  "like COND, but overrides T (default) clause to return T if any clause matches and NIL if they don't, regardless of what the original return values of the clauses are"
  (a:with-gensyms (default)
    `(let ((,default (gensym)))
       (not  (eq ,default (cond
                            ,@clauses
                            (t ,default)))))))

(defmacro out (&rest expressions)
  ;; FIXME: breaks when you try to wrap a FORMAT expression
  (if (< 1 (length expressions))
      `(progn ,@(loop :for exp :in expressions
                      :collect `(format t ,(concatenate 'string (format nil "~s" exp) ": ~s~%") ,exp)))
      (a:with-gensyms (e)
        `(let ((,e ,(first expressions)))
           (format t ,(concatenate 'string (format nil "~s" (first expressions)) ": ~s~%") ,e)
           ,e))))

;; TODO: extend to handle multiple bindings
(defmacro on-global-update ((binding) &body body)
  (a:with-gensyms (old-value)
    `(a:if-let ((,old-value (get ',binding 'old-value)))
       (unless (eq ,old-value ,binding)
         (setf (get ',binding 'old-value) ,binding)
         ,@body)
       (setf (get ',binding 'old-value) ,binding))))

(defmacro retry-once (form)
  (a:with-gensyms (try-it)
    `(block ,try-it
       (handler-bind
           ((error
              (lambda (c)
                (declare (ignore c))
                (handler-bind
                    ((error
                       #'error))
                  (return-from ,try-it ,form)))))
         ,form))))

#++(defmacro retry-once ((retry-if-different-error) form)
  (a:with-gensyms (try-it tried-once)
    `(block ,try-it
       (let (,tried-once)
         (handler-bind
             ((error
                (lambda (c1)
                  (setf ,tried-once c1)
                  (handler-bind
                      ((error
                         (lambda (c2)
                           (format t "already tried to recover once, dying~%")
                           (error c2))))
                    (return-from ,try-it ,form)))))
           ,form)))))

;; use to test RETRY-ONCE
#++(defparameter flag nil)
#++(defun error-alternate ()
  (format t "flag was ~s~%" flag)
  (setf flag (not flag))
  (format t "flag now ~s~%" flag)
  (when flag
    (error "flag set"))
  'error-alternate)

(defmacro let-ret1 (bindings &body body)
  `(let ,bindings
     ,@body
     ,(caar bindings)))

(defmacro with-accumulator (&body body)
  (alexandria:with-gensyms (list)
    `(let (,list)
       (flet ((clear ()
                (prog1
                    (nreverse ,list)
                  (setf ,list nil)))
              (accumulate (value)
                (push value ,list)))
         ,@body))))
