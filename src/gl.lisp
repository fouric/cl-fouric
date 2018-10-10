(in-package #:fouriclib)

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
