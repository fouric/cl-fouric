;(defun main-function ()
;  (ext:exit))

;; clisp
;;(ext:saveinitmem "filename.elf"
;;  :quiet t
;;  :init-function 'main-function
;;  :executable t
;;  :norc t)

;; sbcl
;; (sb-ext:save-lisp-and-die "filename.elf" :toplevel 'main-function :executable t)

;(defparameter dx 0.0001)
;(defun derive (f)
;  (lambda (x)
;    (/ (- (funcall f (+ x dx)) (funcall f x)) dx)))
;(defun cube (x)
;  (* x x x))
;(funcall (derive (function cube)) 2)

; define setf interactions on a thing
;(defun (setf TYPE) (thing)
;  (setf (field TYPE) thing))

; (defparameter *var* 2)
; (defparameter *thread* (bordeaux-threads:make-thread (lambda () (+ *var* 1))))
; (bordeaux-threads:join-thread *thread*)


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

(defmacro make-with-macro (macro-name constructor destructor)
  `(defmacro ,macro-name ((name args) &body body)
     `(let ((,name (,',constructor ,@args)))
	,@body
	(,',destructor ,name))))

(defmacro print-call (call)
  `(format t "~s => ~s~%" ',call ,call))

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

#|
(defun find-dollar-signs (string)
  (labels ((find-next-break-char (pos)
	     (loop as i to (1- (length string)) do
		  (let ((char (schar string i)))
		    (when (or (char= char #\Space) (char= char #\~))
		      (return i)))))
	   (finder (positions)
	     (let* ((last-match (car positions))
		    (start-index (if last-match
				     (1+ last-match)
				     0))
		    (pos (if (= (length string) start-index)
			     nil
			     (position #\$ string
				       :start start-index
				       :end (find-next-break-char start-index)))))
	       (if pos
		   (finder (push pos positions))
		   positions))))
    (nreverse (finder nil))))

(defun extract-tokens (string positions)
  (remove-if (lambda (str)
	       (or (string= str "")
		   (string= str "$")))
	     (mapcar (lambda (position)
		       (let ((end (position #\Space string :start position)))
			 (subseq string position end))) positions)))

(defun $token->token (token)
  (intern (string-upcase (subseq token 1))))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun string-replace (string find replace)
  (let ((position (search find string))
	(length (length find)))
    (if position
	(concatenate 'string
		     (subseq string 0 position)
		     replace
		     (subseq string (+ position length)))
	string)))

(defun transform-string (string tokens)
  (let ((vars nil)
	(string (copy-seq string)))
    (setf string (replace-all string "~" "~~"))
    (dolist (token tokens)
      (let ((sym-token ($token->token token)))
	(setf string (replace-all string token "~A"))
	(push sym-token vars)))
    (values string (nreverse vars))))

;; warning: breaks (defun foo-$bar (...) ...)

(defun dollar-sign-reader (stream char)
  (declare (ignore char))
  (let ((string (read stream t nil t)))
    (if (stringp string)
	(multiple-value-bind (string vars)
	    (transform-string string (extract-tokens string (find-dollar-signs string)))
	  `(format nil ,string ,@vars))
	string)))

(set-macro-character #\$ #'dollar-sign-reader)

|#
