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

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
