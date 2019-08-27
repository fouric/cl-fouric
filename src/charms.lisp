(in-package #:fouric)

;; is this OK?
(eval-when (:compile-toplevel :load-toplevel)
  (trivial-indent:define-indentation defcolors (2)))

(defparameter *charms-win* nil)
(defparameter *screen-width* 1)
(defparameter *screen-height* 1)

(defun clamp-w (x)
  (a:clamp x 0 *screen-width*))
(defun clamp-h (y)
  (a:clamp y 0 *screen-height*))

(defmacro defcolors (&rest colors)
  `(progn
     ,@(loop :for n :from 0 :for blob :in colors :collect `(defparameter ,(first blob) ,n))
     ;; TODO: fix so that when you recompile defcolors, it automatically updates the thing
     (defun init-colors ()
       ;; each `blob` is a list (constant fg bg)
       ,@(loop :for blob :in colors :collect `(charms/ll:init-pair ,(first blob) ,(second blob) ,(third blob))))))

(defmacro with-color (color &body body)
  (alexandria:once-only (color)
    `(unwind-protect
          (progn
            (charms/ll:attron (charms/ll:color-pair ,color))
            ,@body)
       (charms/ll:attroff (charms/ll:color-pair ,color)))))

;; TODO: add special case for writing to the character in the lower-right-hand corner of the screen, or otherwise figure out what the heck is going on
(defun write-string-at (string x y &optional colors)
  (if (< y *screen-height*)
      (if colors
          (with-color colors
            (charms:write-string-at-point *charms-win* (subseq string 0 (- (clamp-w (+ (length string) x)) x)) (clamp-w x) (clamp-h y)))
          (charms:write-string-at-point *charms-win* (subseq string 0 (- (clamp-w (+ (length string) x)) x)) (clamp-w x) (clamp-h y))))
  (length string))

(defcolors
  ;; TODO: need some way of, when this is recompiled, patching it into running instance
  ;; https://linux.die.net/man/3/init_pair probably keep the assignments, diff versions, and call charms/ll:init-pair
  (+color-white-black+  charms/ll:COLOR_WHITE   charms/ll:COLOR_BLACK)
  (+color-black-white+  charms/ll:COLOR_BLACK charms/ll:COLOR_WHITE)

  (+color-blue-black+  charms/ll:COLOR_BLUE   charms/ll:COLOR_BLACK)
  (+color-blue-white+  charms/ll:COLOR_BLACK charms/ll:COLOR_BLUE)

  (+color-black-black+  charms/ll:COLOR_BLACK charms/ll:COLOR_BLACK))

(defun init-charms (timeout color raw-input interpret-control-characters)
  (force-output *terminal-io*)
  (charms:initialize)
  ;; timeout set in milliseconds
  (charms/ll:timeout timeout)
  (setf *charms-win* (charms:standard-window))
  (charms:disable-echoing)
  (charms/ll:curs-set 0) ;; invisible cursor
  (when color
    (charms/ll:start-color)
    (init-colors))
  (if raw-input
      (charms:enable-raw-input :interpret-control-characters interpret-control-characters)
      (charms:disable-raw-input)))

(defun clear-window (&optional force-repaint)
  (charms:clear-window *charms-win* :force-repaint force-repaint))

(defun refresh-window ()
  (charms:refresh-window *charms-win*))

(defun get-char ()
  (charms:get-char *charms-win* :ignore-error t))

(defun update-charms-dimensions ()
  (multiple-value-bind (width height) (charms:window-dimensions *charms-win*)
    (setf *screen-width* (1- width)
          ;; ok so this is monumentally stupid BUT you apparently can't write to the cell in the very bottom right-hand corner without causing an error in charms...
          *screen-height* height)))

(defmacro with-charms ((&key (timeout 100) (color nil) (raw-input t) (interpret-control-characters t)) &body body)
  ;; TODO: if we change parameters, live reload
  `(unwind-protect
        (progn
          (init-charms ,timeout ,color ,raw-input ,interpret-control-characters)
          (update-charms-dimensions)
          ,@body)
     (charms:finalize)))
