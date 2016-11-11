(in-package #:fouriclib)

(defmacro with-ttf-init (&body body)
  `(progn
     (sdl2-ttf:init)
     ,@body
     (sdl2-ttf:quit)))

(defun font (name size)
  (sdl2-ttf:open-font (resource (concatenate 'string "fonts/" name)) size))

#|
(when surface (sdl2:free-surface surface))
(when texture (sdl2:destroy-texture texture))
(when rect (sdl2:free-rect rect))
|#

(defparameter *character-width* nil)
(defparameter *character-height* nil)

(defun col>pix (column-index)
  (* column-index *character-width*))
(defun row>pix (row-index)
  (* row-index *character-height*))

(defstruct cell
  (fg-color (list 255 255 255) :type list)
  (bg-color (list 0 0 0) :type list)
  (char #\Space :type character)
  (surface nil)
  (texture nil))

(defun draw-cell (renderer cell rect)
  (when (cell-texture cell)
    (let ((color (cell-bg-color cell)))
      (sdl2:set-render-draw-color renderer (nth 0 color) (nth 1 color) (nth 2 color) 255))
    (sdl2:render-draw-rect renderer rect)
    (sdl2:render-copy renderer (cell-texture cell) :source-rect (cffi:null-pointer) :dest-rect rect)))

(defun draw-grid (renderer grid rows cols)
  (let ((rect (sdl2:make-rect 0 0 *character-width* *character-height*)))
    (dotimes (y rows)
      (dotimes (x cols)
        (let ((cell (aref grid y x)))
          (when cell
            (plus-c:c-let ((rect sdl2-ffi:sdl-rect :from rect))
              (setf (rect :x) (* x *character-width*)
                    (rect :y) (* y *character-height*)))
            (draw-cell renderer cell rect)))))))

(defun set-cell (renderer cell-grid font x y &key (char #\Space) fg bg)
  (let* ((cell (aref cell-grid y x)))
    (when fg
      (setf (cell-fg-color cell) fg))
    (when bg
      (setf (cell-bg-color cell) bg))
    (let ((color (cell-fg-color cell)))
      (setf (cell-char cell) char)
      (setf (cell-surface cell) (sdl2-ttf:render-text-blended font (coerce (list (cell-char cell)) 'string) (nth 0 color) (nth 1 color) (nth 2 color) 255))
      (setf (cell-texture cell) (sdl2:create-texture-from-surface renderer (cell-surface cell))))))

(defun termlib-test ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (window)
      (sdl2:with-renderer (renderer window :flags '(:accelerated))
        (with-ttf-init
          (multiple-value-bind (window-width window-height)
              (sdl2:get-window-size window)
            (format t "~s~%" (cons window-width window-height))
            (let* ((font (font "UbuntuMono-R.ttf" 36))
                   (surface (sdl2-ttf:render-text-blended font " " 200 0 200 255))
                   (texture (sdl2:create-texture-from-surface renderer surface)))
              (setf *character-width* (sdl2:texture-width texture)
                    *character-height* (sdl2:texture-height texture))
              (let* ((num-rows (floor (/ window-height *character-height*)))
                     (num-cols (floor (/ window-width *character-width*)))
                     (cell-grid (make-array (list num-rows num-cols) :initial-element nil))
                                        ;(rect (sdl2:make-rect (col>pix 0) (row>pix 0) *character-width* *character-height*))
                     )

                (dotimes (y num-rows)
                  (dotimes (x num-cols)
                    (setf (aref cell-grid y x) (make-cell))))

                (set-cell renderer cell-grid font 0 0 :char #\+ :fg (list 255 255 255))
                (set-cell renderer cell-grid font 1 0 :char #\- :fg (list 255 255 255))
                (set-cell renderer cell-grid font 0 1 :char #\| :fg (list 255 255 255))
                (set-cell renderer cell-grid font 1 1 :char #\@ :fg (list 255 0 255))

                (sdl2:with-event-loop (:method :poll)
                  (:keyup (:keysym keysym)
                          (let ((scancode-value (sdl2:scancode-value keysym)))
                            (cond
                              ((sdl2:scancode= scancode-value :scancode-escape) (sdl2:push-event :quit)))))
                  (:idle ()
                         (sdl2:set-render-draw-color renderer 0 0 0 255)
                         (sdl2:render-clear renderer)

                                        ;(sdl2:set-render-draw-color renderer 255 255 255 255)
                                        ;(sdl2:render-draw-rect renderer rect)

                                        ;(sdl2:render-copy renderer texture :source-rect (cffi:null-pointer) :dest-rect rect)

                         (draw-grid renderer cell-grid num-rows num-cols)

                         (sdl2:render-present renderer))
                  (:quit () t))))))))))

(defmacro with-init-window-gl ((init-flags window-flags gl-flags) &body body)
  `(sdl2:with-init ,init-flags
     (sdl2:with-window ,window-flags
       (sdl2:with-gl-context ,gl-flags
         ,@body))))
