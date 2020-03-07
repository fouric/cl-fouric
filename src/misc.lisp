(in-package #:fouric)

(defmacro print-call (call)
  `(format t "~s => ~s~%" ',call ,call))

(defmacro prof (&rest packages)
  `(sb-profile:profile ,@(mapcar (lambda (x) (string-upcase (if (and (consp x) (eq (first x) 'quote)) (cadr x) x))) packages)))

;; takes about 12 microseconds per call, FYI
(defun update-swank ()
  (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
    (when connection
      (swank::handle-requests connection t))))

(defmacro e (form)
  (eval form))

(defmacro clampf (place min max)
  `(progn
     (if (< ,place ,min)
         (setf ,place ,min))
     (if (> ,place ,max)
         (setf ,place ,max))))

(defmacro inclampf (place delta min max)
  `(progn
     (incf ,place ,delta)
     (clampf ,place ,min ,max)))

(defun +-clamp (number delta min max)
  (incf number delta)
  (min max (max min number)))

(defmacro pushlast (obj place)
  `(push ,obj (cdr (last ,place))))

(defun eql/package-relaxed (obj1 obj2)
  (cond
    ((eql obj1 obj2) t)
    ((and (symbolp obj1) (symbolp obj2))
     (string= (symbol-name obj1)
              (symbol-name obj2)))))

(defun edit-definition (name)
  (let ((name (if (stringp name) name (concatenate 'string (package-name (symbol-package name)) "::" (string name)))))
    (trivial-shell:shell-command (format nil "emacsclient -e \"(slime-edit-definition \\\"~a\\\")\"" name))))

(defun emacs-jump-to-term (term &optional (path "."))
  (trivial-shell:shell-command (format nil "emacsclient -n +$(grep -HnR '~a' ~a/* | head -n 1 | cut -d : -f 2) $(grep -HnR '~a' ~a/* | head -n 1 | cut -d : -f 1)" term path term path)))

(defun convert-symbols (tree)
  (cond
    ((symbolp tree)
     (intern (string-upcase (string tree))))
    ((listp tree)
     (mapcar #'convert-symbols tree))
    (t
     tree)))

(defun emacs-eval (command &optional eval-in-emacs)
  "attempt to call out to emacs to evaluate code. EVAL-IN-EMACS requires slime-enable-evaluate-in-emacs to be set to t in your emacs. COMMAND can be a string or a list. strings might break for eval-in-emacs => nil, and lists might break for eval-in-emacs => t"
  ;; FIXME: maybe should just use swank-client (http://quickdocs.org/swank-client/) to talk to development environment...
  (if eval-in-emacs
      (let ((command (convert-symbols (if (listp command)
                                          command
                                          (read-from-string (string-upcase command))))))
        (swank:eval-in-emacs command))
      (let ((command (if (stringp command)
                         command
                         (string-downcase (format nil "~s" (convert-symbols command))))))
        (trivial-shell:shell-command (format nil "emacsclient --eval \"~a\"" command)))))

(defun command (control-string &rest format-arguments)
  (trivial-shell:shell-command (apply #'format nil control-string format-arguments)))

(defmacro ensure-gethash (key hash-table &body value-generation-body)
  (a:with-gensyms (default result)
    (a:once-only (key hash-table)
      `(let* ((,default (gensym))
              (,result (gethash ,key ,hash-table ,default)))
         (if (eq ,result ,default)
             (setf (gethash ,key ,hash-table) (progn
                                                ,@value-generation-body))
             ,result)))))

(defmacro defn (name (&key redefinition-hook) lambda-list &body body)
  (let ((handler (getf (symbol-plist name) 'redefinition-hook)))
    (if handler
        (progn
          (format t "~&handler found for symbol ~s: ~s~%" name handler)
          (funcall (eval redefinition-hook)))
        (progn
          (format t "~&no handler found for symbol ~s~%" name)
          (setf (getf (symbol-plist name) 'redefinition-hook) (eval redefinition-hook)))))
  `(defun ,name ,lambda-list
     ,@body))

#++(defun test-add-it ()
  (print (add-it 10 20)))

#++(defn add-it (:redefinition-hook #'test-add-it) (a b)
  (+ a b))
