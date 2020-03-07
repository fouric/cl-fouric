(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :sdl2))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "/home/grant/code/lisp/personal/library.lisp"))

(defmacro just-display-it (renderer-name keysym-name keysym-quit-trigger &rest draw-logic)
  "supply a renderer name, a keysym name, logic that handles the keysym and tells when to quit, and draw logic, and we'll do the rest"
  `(make-accelerated-window-renderer
    window
    ,renderer-name
    (sdl2:with-event-loop (:method :poll)
      (:keydown (:keysym ,keysym-name)
       (if ,keysym-quit-trigger
	   (sdl2:push-event :quit)))
      (:idle ()
	     (sdl2:set-render-draw-color ,renderer-name 0 0 0 255)
	     (sdl2:render-clear ,renderer-name)
	     ,@draw-logic
	     (sdl2:render-present ,renderer-name))
      (:quit () t))))

(defmacro clear-and-render (renderer r g b a &rest body)
  `(progn
     (sdl2:set-render-draw-color ,renderer ,r ,g ,b ,a)
     (sdl2:render-clear ,renderer)
     ,@body
     (sdl2:render-present ,renderer)))

;; tiny, specific wrapper around clear-and-render that assumes a renderer
;; called "renderer" and a black clear value
(defmacro black-render (&rest body)
  `(clear-and-render renderer 0 0 0 255 ,@body))

;; NOT thread-safe!
(let ((rect (sdl2:make-rect 0 0 0 0)))
  (defun render-rectangle (renderer xy wh rgb &optional (filled t))
    (plus-c:c-let ((rect sdl2-ffi:sdl-rect :from rect))
      (setf (rect :x) (car xy)
            (rect :y) (cdr xy)
            (rect :w) (car wh)
            (rect :h) (cdr wh)))
    (sdl2:set-render-draw-color renderer (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) 255)
    (if filled
        (sdl2:render-fill-rect renderer rect)
        (sdl2:render-draw-rect renderer rect))))

;; NOT thread-safe!
(let ((rect (sdl2:make-rect 0 0 0 0)))
  (defun render-texture (renderer texture x y)
    (plus-c:c-let ((rect sdl2-ffi:sdl-rect :from rect))
      (setf (rect :x) x
            (rect :y) y
            (rect :w) (sdl2:texture-width texture)
            (rect :h) (sdl2:texture-height texture)))
    (sdl2:render-copy renderer texture :source-rect (cffi:null-pointer) :dest-rect rect)))
