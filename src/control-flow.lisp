(in-package #:fouriclib)

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
