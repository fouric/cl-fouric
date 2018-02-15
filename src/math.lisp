;; TODO: add macro that, if passed a list form that starts with a number, treat it as a list/vector instead and allow for skipping of the (QUOTE ...)
(defun ^ (base power)
  "just an alias for EXPT"
  (expt base power))
(defun square (x)
  "square the input argument"
  (^ x 2))
(defun inv (x)
  "1/x"
  (/ 1 x))
(defun x (a b)
  "vector cross product"
  (when (and (listp a) (= 3 (length a)) (listp b) (= 3 (length b)))
    (list (- (* (nth 1 a) (nth 2 b)) (* (nth 2 a) (nth 1 b)))
          (- (* (nth 2 a) (nth 0 b)) (* (nth 0 a) (nth 2 b)))
          (- (* (nth 0 a) (nth 1 b)) (* (nth 1 a) (nth 0 b))))))
(defun dot (a b)
  "vector dot product"
  (apply #'+ (mapcar #'* a b)))
(defun v+v (a b)
  "vector sum"
  (mapcar #'+ a b))
(defun v-v (a b)
  "vector difference"
  (mapcar #'- a b))
(defun v/s (v s)
  "divide each element of a vector by a scalar"
  (mapcar (lambda (x) (/ x s)) v))
(defun v*s (v s)
  "divide each element of a vector by a scalar"
  (mapcar (lambda (x) (* x s)) v))
(defun vabs (v)
  "length of a vector"
  (sqrt (apply #'+ (mapcar #'square v))))
(defun vectors-midpoint (a b)
  "get the midpoint (or vector) between two vectors"
  (v/s (v+v a b) 2))
(defun polygons-area (v1 v2 v3)
  "calculate the area of a polygon designated by the three given vectors"
  (* 0.5 (vabs (v-v v3 (vectors-midpoint v1 v2))) (vabs (v-v v1 v2))))
