(make-package :libfouric :nicknames '(:f) :use '(:cl))
(in-package :libfouric)
;; (use-package :libfouric) to just use everything without explicitly naming the package (that is, import all exported/external libfouric symbols into the current package)

(defun read-integer ()
  (let ((val (handler-case (read-from-string (read-line))
               (sb-int:simple-reader-package-error (e)
                 e))))
    (etypecase val
      (integer
       val))))

(defun range (start end)
  "is inclusive of both start and end"
  ;; is not tail-recursive, no accumulator
  ;; could use a step argument
  ;; maybe make this, then implement DORANGE on top, or reimplement for efficiency?
  (if (= start end)
      (cons end nil)
      (cons start (range (if (> end start) (1+ start) (1- start)) end))))

;; also maybe make DORANGE macro
;; e.g. (dorange (VAR END &optional (START 0) (STEP 1) (RETURN nil)) ...)

(defun to-string (item)
  (coerce item 'string))

(defun to-list (item)
  (coerce item 'list))

(defun strcat (first &rest others)
  (apply #'concatenate 'string first others))

(defun symcat (first &rest others)
  (intern (string-upcase (apply 'strcat first others))))

;; create a tinylambda, closure-style
;; use like so: (mapcar (_ (+ 1 _)) '(3 5 6))
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

;; FN-CASE replaced with ALEXANDRIA:SWITCH

(defmacro iprogn (&body body)
  (if (< 1 (length body))
      `(progn
         ,@body)
      (first body)))

(defmacro doitimes (count &body body)
  `(dotimes (,(gensym) ,(first count))
     ,@body))

;; WHEN-BIND replaced with ALEXANDRIA:WHEN-LET (http://stevelosh.com/blog/2018/07/fun-with-macros-if-let/)

(defun read-file (filename)
  (with-open-file (in filename
                      :direction :input
                      :if-exists :supersede)
    (with-standard-io-syntax
      (read in))))

(defun get-file (filename)
  "get a file as lines"
  (with-open-file (stream filename
                          :direction :input
                          :if-exists :supersede)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defmacro make-with-macro (macro-name constructor destructor)
  `(defmacro ,macro-name ((name args) &body body)
     `(let ((,name (,',constructor ,@args)))
        ,@body
        (,',destructor ,name))))

(defmacro print-call (call)
  `(format t "~s => ~s~%" ',call ,call))

(defmacro llread ((name initial) &body forms)
  (if (null forms)
      initial
      `(llread (,name (let ((,name ,initial)) ,(first forms))) ,@(rest forms))))

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an
   error. Remember to hit C in slime or pick the restart so
   errors don't kill the app."
  `(restart-case
       (progn ,@body)
     (continue () :report "CEPL Continue")))

#+swank(defun update-swank ()
         "Called from within the main loop, this keep the lisp repl
   working while cepl runs"
         (continuable
          (let ((connection (or swank::*emacs-connection*
                                (swank::default-connection))))
            (when connection
              (swank::handle-requests connection t)))))

(let ((pack (find-package :libfouric)))
  (do-all-symbols (sym pack) (when (and (or (fboundp sym) (macro-function sym)) (eql (symbol-package sym) pack)) (export sym))))

;; list all subdirectories in a directory
;; (uiop:subdirectories (user-homedir-pathname))
;; (uiop:directory-files (user-homedir-pathname))

;; check to see if a file exists
;; (probe-file #P"/home/fouric/.bashrc")

;; catch a condition
#++(handler-case
    (the-code)
  (some-exception (se) (recover se)))

;; (llread (x (list 1 2 3)) (mapcar #'1+ x) (reduce #'+) (lambda (x) #'1 x))

#|
;; expansion looks like
(LET ((X
        (LET ((X
                (LET ((X FOO))
                  (FOO X))))
          (BAR X))))
  (ZUUL X))
;; fix to be (zuul (bar foo)) or whatever
|#

;; (llread (x value) (foo x) (bar x) (baz x))
;;   -> (llread (x (foo value)) (bar x) (baz x))
;;   -> (llread (x (bar (foo value))) (baz x))
;;   -> (llread (x (baz (bar (foo value)))))

;; (llread (x value) (foo x) ...)
;;   -> (llready (x (foo value)) ...)

;; potential problems: multiple evaluation of value if multiple x per form (can be programmatically detected and fixed), forms like lambda (impossible to do generally but can find the most common ones)
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

|#
