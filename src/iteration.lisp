(in-package #:fouric)

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

(defmacro intersperse (list binding always almost-always)
  (a:once-only (list)
    (a:with-gensyms (length i)
      `(let ((,length (length ,list)))
         (loop :for ,binding :in ,list :for ,i :from 0 :do
           (progn
             ,always
             (unless (= ,i (1- ,length))
               ,almost-always)))))))
