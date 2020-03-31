(in-package #:fouric)

(defmacro with-ttf-init (&body body)
  `(unwind-protect
        (progn
          (sdl2-ttf:init)
          ,@body)
     (sdl2-ttf:quit)))

(defun font (name size)
  (sdl2-ttf:open-font (resource (concatenate 'string "fonts/" name)) size))

#++(when surface (sdl2:free-surface surface))
#++(when texture (sdl2:destroy-texture texture))
#++(when rect (sdl2:free-rect rect))

(defmacro with-init-window-gl ((init-flags window-flags gl-flags) &body body)
  `(sdl2:with-init ,init-flags
     (sdl2:with-window ,window-flags
       (sdl2:with-gl-context ,gl-flags
         ,@body))))

;;(sdl2:gl-set-swap-interval 1)

(defmacro make-accelerated-window-renderer (window-name renderer-name &rest body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (,window-name :flags '(:shown :opengl))
       (sdl2:with-renderer (,renderer-name ,window-name :flags '(:accelerated))
         ,@body))))

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

(defmacro quick-event-loop-with-window-size (w h &rest body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (window :w ,w :h ,h :flags '(:shown :opengl))
       (sdl2:with-renderer (renderer window :flags '(:accelerated))
         (sdl2:with-event-loop (:method :poll)
           ,@body)))))

(defmacro clear-and-render ((renderer r g b a) &body body)
  `(progn
     (sdl2:set-render-draw-color ,renderer ,r ,g ,b ,a)
     (sdl2:render-clear ,renderer)
     ,@body
     (sdl2:render-present ,renderer)))

;; tiny, specific wrapper around clear-and-render that assumes a renderer
;; called "renderer" and a black clear value
(defmacro black-renderer (&rest body)
  `(clear-and-render renderer 0 0 0 255 ,@body))

;; convenience macro to make SDL2+TTF init & renderer and window creation easier
(defmacro sdl2-omni (((&rest sdl-init-flags)
                      (window-symbol &key title (x :centered) (y :centered) (w 800) (h 600) window-flags)
                      (renderer-symbol &key index renderer-flags)
                      (&rest sdl-with-event-loop-flags)
                      &optional extras)
                     &body body)
  (let ((raw-event-forms body)
        (initial-clear-forms `((sdl2:set-render-draw-color ,renderer-symbol 0 0 0 255)
                               (sdl2:render-fill-rect ,renderer-symbol (cffi:null-pointer))
                               (sdl2:render-present ,renderer-symbol))))
    `(sdl2:with-init (,@sdl-init-flags)
       (sdl2:with-window (,window-symbol :title ,title :x ,x :y ,y :w ,w :h ,h :flags ,window-flags)
         (sdl2:with-renderer (,renderer-symbol ,window-symbol :index ,index :flags ,renderer-flags)
           (,(if (and extras (member :ttf extras)) 'with-ttf-init 'progn)
            ,@(if (and extras (member :initial-clear extras)) initial-clear-forms)
            (sdl2:with-event-loop (,@sdl-with-event-loop-flags)
              ,@raw-event-forms)))))))

;; convenience macro to make it WAY easier for my most common case
(defmacro sdl2-quick ((window-symbol renderer-symbol &key title (x :centered) (y :centered) (w 800) (h 600) window-flags extras)
                      &body body)
  `(sdl2-omni ((:video)
               (,window-symbol :title ,title :x ,x :y ,y :w ,w :h ,h :window-flags ,window-flags)
               (,renderer-symbol :renderer-flags '(:accelerated :presentvsync))
               (:method :poll)
               ,extras)
     ,@body))

(defun render-rectangle (renderer xy wh rgb &optional (filled t))
  (let ((rect (sdl2:make-rect (car xy) (cdr xy) (car wh) (cdr wh))))
    (sdl2:set-render-draw-color renderer (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) 255)
    (if filled
        (sdl2:render-fill-rect renderer rect)
        (sdl2:render-draw-rect renderer rect))
    (sdl2:free-rect rect)))

(defun render-texture (renderer texture x y)
  (when texture
    (let ((rect (sdl2:make-rect x y (sdl2:texture-width texture) (sdl2:texture-height texture))))
      (sdl2:render-copy renderer texture :source-rect (cffi:null-pointer) :dest-rect rect)
      (sdl2:free-rect rect))))

(defun render-text (renderer font text r g b a)
  (let* ((surface (sdl2-ttf:render-utf8-solid font text r g b a))
         (texture (sdl2:create-texture-from-surface renderer surface)))
    ;; this is a bug, i think - causes memory corruption due to poorly-written finalizer if you call free-surface
    ;; yes, it's a bug. sdl2:free-surface calls sdl2-ffi.functions:sdl-free-surface, which is called again when the object goes out of scope. this should be a bug report. investigate (try (TRACE sdl2::sdl-free-surface)) and report
    ;;(sdl2:free-surface surface)
    texture))

(defmacro with-font ((font-sym path-to-font point-size) &body body)
  `(let ((,font-sym (sdl2-ttf:open-font ,path-to-font ,point-size)))
     (unwind-protect
          (progn
            ,@body)
       (sdl2-ttf:close-font ,font-sym))))

(defun get-window-width (win)
  (multiple-value-bind (w h) (sdl2:get-window-size win)
    (declare (ignore h))
    w))

(defun get-window-height (win)
  (multiple-value-bind (w h) (sdl2:get-window-size win)
    (declare (ignore w))
    h))

(defun make-text-texture (renderer path-to-font point-size text r g b a)
  (with-font (font path-to-font point-size)
    (render-text renderer font text r g b a)))

(defmacro replace-texture (place texture)
  "peek at PLACE. if there's a texture, destroy and replace it with TEXTURE. warning: multiple evaluation of PLACE"
  ;; FIXME: rename to ENSURE-TEXTURE?
  `(progn
     (when ,place
       (sdl2:destroy-texture ,place))
     (setf ,place ,texture)))

(defmacro sdl2-event-process ((event recursion-name quit-symbol (&rest args)) &body event-handlers)
  (alexandria:with-gensyms (event-type event-id)
    `(sdl2:with-sdl-event (,event)
       (let )
       (if (not (zerop (sdl2:next-event ,event :poll nil)))
           (let* ((,event-type (sdl2:get-event-type ,event))
                  (,event-id (and (sdl2::user-event-type-p ,event-type) (,event :user :code))))
             (case ,event-type
               (:lisp-message ()
                (sdl2::get-and-handle-messages))
               ,@(loop :for (type params . forms) :in event-handlers
                       :collect
                       (if (eq type :quit)
                           `(:quit ,@forms)
                           #++(sdl2::expand-quit-handler event forms quit)
                           (sdl2::expand-handler event type params forms))
                         :into results
                       :finally (return (remove nil results))))
             (when (and ,event-id (not (eq ,event-type :lisp-message)))
               (sdl2::free-user-data ,event-id))
             (,recursion-name ,quit-symbol ,@args))
           ,quit-symbol))))

(defmacro sdl2-event-recursion (() &body body)
  `(unless sdl2::*event-loop*
     (setf sdl2::*event-loop* t)
     (sdl2:in-main-thread (:background nil)
       (unwind-protect
            (progn
              ,@body)
         (setf sdl2::*event-loop* nil)))))

#++(defun gooey ()
     (sdl2:with-init (:everything)
       (sdl2:with-window (window :w 400 :h 400)
         (sdl2:with-renderer (renderer window)
           (sdl2-event-recursion ()
             (process-next-loop renderer))))))
#++(defun idle-function (renderer)
     (sdl2:set-render-draw-color renderer 255 255 255 255)
     (sdl2:render-clear renderer)
     (sdl2:render-present renderer))
#++(defun quit-function ()
     t)
#++(defun process-next-loop (renderer)
     (unless (process-next-event renderer nil) ;; blocks until we've processed all events, and returns non-nil if we want to quit
       (idle-function renderer)
       (process-next-loop renderer)))
#++(defun process-next-event (renderer quit?)
     (sdl2-event-process (event)
       (:keydown (:keysym keysym)
                 (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q)
                   (sdl2:push-event :quit)))
       (:idle ()
              (idle-function renderer))
       (:quit ()
              (setf quit? (quit-function)))))


;;; here's some stuff that's more hard-coded - a template, not a framework or library - but gives you a recursive event loop and a separate event-handling function

(defun sdl2-outer ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :w 400 :h 400)
      (sdl2:with-renderer (renderer window :flags '(:accelerated :presentvsync))
        (unless sdl2::*event-loop*
          (setf sdl2::*event-loop* t)
          (sdl2:in-main-thread (:background nil)
            (unwind-protect
                 (sdl2-inner window renderer `())
              (setf sdl2::*event-loop* nil))))))))

(defun sdl2-inner (window renderer data)
  (sdl2:with-sdl-event (event)
    (unless (eq :quit (if (not (zerop (sdl2:next-event event :poll nil)))
                          (let* ((event-type (sdl2:get-event-type event))
                                 (event-id (and (sdl2::user-event-type-p event-type) (event :user :code))))
                            (prog1
                                (handle-event window renderer event data event-type)
                              (when (and event-id (not (eq event-type :lisp-message)))
                                (sdl2::free-user-data event-id))))
                          (sdl2-handle-event window renderer nil data :idle)))
      (inner-inner window renderer data))))

(defun sdl2-handle-event (window renderer event data event-type)
  (declare (ignore window data))
  (case event-type
    (:lisp-message
     (sdl2::get-and-handle-messages))
    (:keydown
     (let ((keysym (plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym)))
       (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q)
         (sdl2:push-event :quit))))
    (:idle
     (sdl2:set-render-draw-color renderer 0 0 0 255)
     (sdl2:render-clear renderer)
     (sdl2:render-present renderer))
    (:quit
     (return-from handle-event :quit))))
