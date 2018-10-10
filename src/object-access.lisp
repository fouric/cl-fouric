(in-package #:fouric)

(defmacro with-plist-aliases (plist prefix vars &body body)
  `(let ,(mapcar (lambda (var)
                   (list
                    (symcat prefix (string var))
                    `(getf ,plist (quote ,var)))) vars)
     ,@body))

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
