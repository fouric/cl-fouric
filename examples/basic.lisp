(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :sdl2)
  (ql:quickload :swank))

(defmacro event-loop (&body event-handlers)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (window :flags '(:shown))
       (sdl2:with-renderer (renderer window :flags '(:accelerated :presentvsync))
         (sdl2:with-event-loop (:method :poll)
           ,@event-handlers
           (:quit () t))))))

(defun basic ()
  (let ((box-x 300)
        (box-y 300))
    (event-loop
      (:keydown (:keysym keysym)
                ;; learning opportunity: factor out common sdl2:scancode-value value into LET
                (when (or (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                          (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q))
                  (sdl2:push-event :quit)))
      (:idle ()
             ;; learning opportunity: use INCF
             (when (sdl2:keyboard-state-p :scancode-w)
               (setf box-y (- box-y 5)))
             (when (sdl2:keyboard-state-p :scancode-s)
               (setf box-y (+ box-y 5)))
             (when (sdl2:keyboard-state-p :scancode-a)
               (setf box-x (- box-x 5)))
             (when (sdl2:keyboard-state-p :scancode-d)
               (setf box-x (+ box-x 5)))
             ;(format t "box-x: ~s~%" box-x)
             ;(format t "box-y: ~s~%" box-y)

             (render-code renderer box-x box-y)))))

(defun render-code (renderer x y)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)

  (sdl2:set-render-draw-color renderer 200 0 200 255)
  (sdl2:render-fill-rect renderer (sdl2:make-rect x y 50 50))

  (sdl2:render-present renderer))
