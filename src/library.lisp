(in-package #:fouriclib)

(defun range (x y)
  (if (= x y)
      (cons y nil)
      (cons x (range (apply (if (> y x) #'+ #'-) `(,x 1)) y))))

(defun to-string (item)
  (coerce item 'string))

(defun to-list (item)
  (coerce item 'list))

(defun strcat (first &rest others)
  (apply #'concatenate 'string first others))

(defun symcat (first &rest others)
  (intern (string-upcase (apply 'strcat first others))))

                                        ; create a tinylambda, closure-style
                                        ; use like so: (mapcar (_ (+ 1 _)) '(3 5 6))
(defmacro _ (&body body)
  "an even more concise (albeit limited) lambda form"
  `(lambda (_) ,@body))

(defmacro with-plist-aliases (plist prefix vars &body body)
  `(let ,(mapcar (lambda (var)
                   (list
                    (symcat prefix (string var))
                    `(getf ,plist (quote ,var)))) vars)
     ,@body))

(defmacro while (test &body body)
  `(loop
     (unless ,test
       (return))
     ,@body))

(defmacro fn-case (keyform test &body clauses)
  (let ((kf (gensym))
        (tst (gensym)))
    `(let ((,kf ,keyform)
           (,tst ,test))
       (cond
         ,@ (mapcar (lambda (clause)
                      (if (eq (car clause) t)
                          `(t
                            ,@ (cdr clause))
                          (let ((key (car clause))
                                (code (cdr clause)))
                            `((funcall ,tst ,kf ,key)
                              ,@code)))) clauses)))))

(defmacro iprogn (&body body)
  (if (< 1 (length body))
      `(progn
         ,@body)
      (first body)))

(defmacro doitimes (count &body body)
  `(dotimes (,(gensym) ,(first count))
     ,@body))

(defmacro when-bind ((name test) &body body)
  "combines `when` and `let` so that the binding is automatically created if the test passes"
  `(let ((,name ,test))
     (when ,name
       ,@body)))

(defun read-file (filename)
  (with-open-file (in filename
                      :direction :input
                      :if-exists :supersede)
    (with-standard-io-syntax
      (read in))))

(defun write-file (filename object)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print object out))))

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

(defmacro print-call (call)
  `(format t "~s => ~s~%" ',call ,call))

#|
(defun pathcat (append-slash? &rest values)
  (apply #'concatenate (append (list 'string "") values (list (if append-slash? "/" "")))))
|#

#|
(with-shader (shader (:vertex-shader :filename vertex-shader-filename))
  ...)

(let ((vertex-shader (create-shader :vertex-shader :filename vertex-shader-filename)))
  ...
  (gl:delete-shader shader))


(make-with-macro with-shader gl:create-shader gl:delete-shader)

(defmacro with-shader ((name args) &body body)
  `(let ((,name (create-shader ,@args)))
     ,@body
     (gl:delete-shader ,name)))


(defmacro make-with-macro (macro-name constructor destructor)
  `(defmacro ,macro-name ((name args) &body body)
     `(let ((,name (,,constructor ,@args)))
        ,@body
        (,,destructor))))




(with-shaders ((shader1 (:vertex-shader vertex-shader-filename))
               (shader2 (:fragment-shader fragment-shader-filename)))
  ...)

(let ((shader1 (create-shader :vertex-shader vertex-shader-filename))
      (shader2 (create-shader :fragment-shader fragment-shader-filename)))
  ...
  (gl:delete-shader shader1)
  (gl:delete-shader shader2))

|#

#|
(defmacro with-shaders (shader-init-forms &body body)
  (let ((retval (gensym "RETVAL")))
    `(let ,(append (mapcar (lambda (initform) (cons (first initform) (list (cons 'make-shader-from-file (first (rest initform)))))) shader-init-forms)
                   (list retval))
       ,@(butlast body)
       (setf ,retval ,(first (last body)))
       ,@(mapcar (lambda (initform) (list 'gl:delete-shader (first initform))) shader-init-forms)
       ,retval)))
|#

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

;; just a shortcut for getting a thing from a place relative to a path
(defun resource (path system)
  (asdf:system-relative-pathname system path))

(defmacro with-dot-accessors (type-mappings &body body)
  (labels
      ;; is the given symbol a candidate for transformation? that is, does it have exactly one . (dot) in it?
      ((valid? (symbol)
         (let* ((name (string symbol))
                (position (search "." name)))
           (when position
             (let ((second-position (search "." name :start2 (1+ position))))
               (when (or (not second-position) (= position second-position))
                 position)))))

       ;; split the given symbol into two separate symbols around (not including) the index character
       (split (symbol index)
         (let ((name (string symbol)))
           (cons (intern (subseq name 0 index)) (intern (subseq name (1+ index))))))

       (split-if-valid (symbol)
         (let ((position? (valid? symbol)))
           (when position?
             (split symbol position?))))

       (get-type (symbol)
         (let (type)
           (dolist (type-form type-mappings)
             (when (member symbol (cadr type-form))
               (setf type (first type-form))
               (return)))
           type))

       (transform (symbol)
         (let ((split (split-if-valid symbol)))
           (if split
               (let* ((name (car split))
                      (field (cdr split))
                      (type (get-type name)))
                 (if type
                     (list (intern (string-upcase (concatenate 'string (string type) "-" (string field)))) name)
                                        ;(error "type ~S not specified in type-mapping alist ~S" name type-mappings)
                     symbol
                     ))
               symbol))))

    (defun process (form)
      (typecase form
        (symbol (transform form))
        (list (mapcar #'process form))
        (t form)))
    `(progn ,@ (process body))))

(defmacro profile (&rest packages)
  `(sb-profile:profile ,@(mapcar (lambda (x) (string-upcase (if (and (consp x) (eq (first x) 'quote)) (cadr x) x))) packages)))
