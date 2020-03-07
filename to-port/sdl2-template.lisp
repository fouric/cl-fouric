(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :sdl2))

(defun basic-test ()
  (let ((stdout *standard-output*))
    (sdl2:with-init (:everything)
      (sdl2:with-window (window :w 640 :h 480 :flags '())
        (sdl2:with-renderer (renderer window :flags '(:accelerated))
          (let ((*standard-output* stdout))
            (sdl2:with-event-loop (:method :poll)
              (:keydown (:keysym keysym)
                        (let ((scancode (sdl2:scancode-value keysym))
                              (sym (sdl2:sym-value keysym))
                              (mod-value (sdl2:mod-value keysym)))
                          (cond
                            ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
                            ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
                            ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
                          (format t "Key sym: ~a, code: ~a, mod: ~a~%" sym scancode mod-value)))

              (:keyup (:keysym keysym)
                      (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                        (sdl2:push-event :quit)))

              (:mousemotion (:x x :y y :xrel xrel :yrel yrel :state state)
                            (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%" x xrel y yrel state))

              (:idle ()
                     (sdl2:set-render-draw-color renderer 50 50 50 255)
                     (sdl2:render-clear renderer)

                     (sdl2:render-present renderer))

              (:quit () t))))))))
