(defclass foo ()
  ((once :accessor once :initform 1)
   (twice :accessor twice :initform 2)))

;; need to parse lambda list and handle declare forms using https://common-lisp.net/project/alexandria/draft/alexandria.html#Macro-Writing
;; goal: when you pass in a parameter called SELF that specializes on a class, automatically break those out into with-accessors

(mapcar #'slot-definition-name
        (class-direct-slots (class-of (make-instance 'shape))))
(defmacro defmethod+ (name &rest args)
  (let* ((qualifier? (symbolp (first args)))
         (method-qualifier (if qualifier? (list (first args))))
         (parameters (if qualifier? (second args) (first args)))
         (pair (first (member 'self parameters :test #'eql/package-relaxed :key (lambda (binding)
                                                                        (if (consp binding)
                                                                            (first binding))))))
         (class-name (second pair))
         (class (when class-name (class-of (make-instance class-name))))
         (slots (when class (mapcar #'sb-mop:slot-definition-name (sb-mop:class-direct-slots class))))
         (body (nthcdr (if qualifier? 2 1) args))
         ;; problem: not passing declare forms through correctly
         (declare? (and (consp (first body)) (eql/package-relaxed (first (first body)) 'declare)))
         (declare-form (when declare? (first body)))
         (body (if slots
                   `((f:with-accessors+ ,slots ,(first pair) ,@body))
                   body)))
    (fresh-line)
    (format t "args: ~s~%" args)
    (format t "name: ~s~%" name)
    (format t "qualifier?: ~s~%" qualifier?)
    (format t "method-qualifier: ~s~%" method-qualifier)
    (format t "parameters: ~s~%" parameters)
    (format t "body: ~s~%" body)
    (format t "declare: ~s~%" declare-form)
    (format t "slots: ~s~%" slots)
    `(defmethod ,@(cons name method-qualifier) ,parameters ,@body)
    ))

(defgeneric my-method (foo other-arg))
(defmethod+ my-method :after ((self foo) other-arg)
  (declare (ignore other-arg))
  (format t "hello, world!~%")
  (+ 1 2))
