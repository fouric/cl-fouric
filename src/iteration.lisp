(in-package #:fouriclib)

(defun range (x y)
  (if (= x y)
      (cons y nil)
      (cons x (range (apply (if (> y x) #'+ #'-) `(,x 1)) y))))

(defmacro while (test &body body)
  `(loop
     (unless ,test
       (return))
     ,@body))

;; todo: make custom DOTIMES variant where binding is optional
(defmacro doitimes (count &body body)
  `(dotimes (,(gensym) ,(first count))
     ,@body))
