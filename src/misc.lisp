(in-package #:fouric)

(defmacro print-call (call)
  `(format t "~s => ~s~%" ',call ,call))

(defmacro profile (&rest packages)
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
