(in-package #:fouric)

;; is this OK?
(eval-when (:compile-toplevel :load-toplevel)
  (trivial-indent:define-indentation defcolors (2)))

(defclass charms-handle ()
  ((window :accessor window :initarg :window)
   (width :accessor width :initarg :width)
   (height :accessor height :initarg :height)))

(defun clamp-w (handle x)
  (a:clamp x 0 (width handle)))
(defun clamp-h (handle y)
  (a:clamp y 0 (height handle)))

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
(defun write-string-at (handle string x y &optional colors)
  (if (< y (height handle))
    (if colors
      (with-color colors
        (charms:write-string-at-point (window handle) (subseq string 0 (- (clamp-w handle (+ (length string) x)) x)) (clamp-w handle x) (clamp-h handle y)))
      (charms:write-string-at-point (window handle) (subseq string 0 (- (clamp-w handle (+ (length string) x)) x)) (clamp-w handle x) (clamp-h handle y))))
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
  ;; TODO: if recompiled while running, if raw-input or interpret-control-characters have changed, call enable-raw-input or disable-raw-input to sync state
  (force-output *terminal-io*)
  (charms:initialize)
  ;; timeout set in milliseconds
  (charms/ll:timeout timeout)
  (charms:disable-echoing)
  (charms/ll:curs-set 0) ;; invisible cursor
  (when color
    (charms/ll:start-color)
    (init-colors))
  (if raw-input
    (charms:enable-raw-input :interpret-control-characters interpret-control-characters)
    (charms:disable-raw-input))
  (multiple-value-bind (w h)
      (charms:window-dimensions (charms:standard-window))
    (make-instance 'charms-handle
                   :window (charms:standard-window)
                   :width w
                   :height h)))

(defun clear-window (handle &optional force-repaint)
  (charms:clear-window (window handle) :force-repaint force-repaint))

(defun write-spaces-window (handle)
  "write the space character to every cell on the screen to forcibly clear it if curses doesn't want to cooperate"
  (multiple-value-bind (width height) (window-dimensions handle)
    (with-color +color-black-black+
      (clear-window handle)
      (dotimes (y height)
        (charms:write-string-at-point (window handle) (make-string (if (= y (1- height)) (1- width) width) :initial-element #\space) 0 y))
      (refresh-window handle))))

(defun refresh-window (handle)
  (charms:refresh-window (window handle)))

(defun get-char (handle)
  (charms:get-char (window handle) :ignore-error t))

(defun update-charms-dimensions (handle)
  (multiple-value-bind (width height) (charms:window-dimensions (window handle))
    (setf (width handle) (1- width)
          ;; ok so this is monumentally stupid BUT you apparently can't write to the cell in the very bottom right-hand corner without causing an error in charms...
          (height handle) height)))

(defmacro with-charms ((handle-name &key (timeout 100) (color nil) (raw-input t) (interpret-control-characters t)) &body body)
  ;; TODO: if we change parameters, live reload
  `(unwind-protect
        (let ((,handle-name (init-charms ,timeout ,color ,raw-input ,interpret-control-characters)))
          (update-charms-dimensions ,handle-name)
          ,@body)
     (charms:finalize)))

(defun repeatchar (char count)
  (if (zerop count)
    ""
    (concatenate 'string (list char) (repeatchar char (1- count)))))

(defun charms-draw-box (handle x y w h)
  ;; usually takes no more than a few hundred microseconds per call, although the complexity does scale with the box size
  (let* ((w (min w (- (width handle) x)))
         (h (min h (- (height handle) y)))
         ;; make a string of entirely the horizontal line character, w units long
         (upper-left #\+)
         (upper-right #\+)
         (lower-left #\+)
         (lower-right #\+)
         (vertical "|")
         (horizontal (make-string w :initial-element #\-)))
    ;; set the first and last elements to be the upper left and right corners, respectively
    (setf (aref horizontal 0) upper-left
          (aref horizontal (1- w)) upper-right)
    ;; draw the top of the box
    (write-string-at handle horizontal x (+ y 0))
    ;; then set the first and last elements to be the bottom characters
    (setf (aref horizontal 0) lower-left
          (aref horizontal (1- w)) lower-right)
    ;; and draw
    (write-string-at handle horizontal x (+ y h -1))
    ;; we don't have a way to draw vertical lines, so we'll just loop
    (dotimes (i (- h 2))
      (write-string-at handle vertical (+ x 0) (+ y i 1))
      (write-string-at handle vertical (+ x w -1) (+ y i 1)))))

(defun window-dimensions (handle)
  (charms:window-dimensions (window handle)))
