(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :sdl2)
  (ql:quickload :swank))

(defun update-swank ()
  (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
    (when connection
      (swank::handle-requests connection t))))

(defun recursive ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :w 400 :h 400)
      (sdl2:with-renderer (renderer window :flags '(:accelerated :presentvsync))
        ;; flag the event loop as running
        (unless sdl2::*event-loop*
          (setf sdl2::*event-loop* t)
          ;; not sure what in-main-thread does
          (sdl2:in-main-thread (:background nil)
            (unwind-protect
                 (inner window renderer `(:x 10)) ; that last argument is the initial user data
              ;; make sure that we clear the flag when we quit
              (setf sdl2::*event-loop* nil))))))))

(defun inner (window renderer data)
  (update-swank)
  ;; allocate and free C-space SDL struct
  (sdl2:with-sdl-event (event)
    ;; we use the retval of HANDLE-EVENT for basic feedback
    ;; right now, the only thing we do is check to see if it's :QUIT, and if it is, we quit
    (unless (eq :quit (if (not (zerop (sdl2:next-event event :poll nil))) ; NEXT-EVENT will fill the event struct with data
                          ;; if we got an event, pass it in to HANDLE-EVENT
                          (let* ((event-type (sdl2:get-event-type event))
                                 (event-id (and (sdl2::user-event-type-p event-type) (event :user :code)))
                                 retval)
                            (if (eq event-type :lisp-message)
                                (sdl2::get-and-handle-messages)
                                (progn
                                  (setf retval (handle-event window renderer event-type event data))
                                  (when event-id
                                    (sdl2::free-user-data event-id))))
                            retval)
                          ;; if we didn't get an event, then just pass in the :IDLE symbol
                          (handle-event window renderer :idle nil data)))
      ;; recursively call ourselves again, to enable hotpatching
      (inner window renderer data))))

;; the goal is to move as much boilerplate out of HANDLE-EVENT as possible
(defun handle-event (window renderer event-type event data)
  (declare (ignore window))

  ;; EVENT is either an sdl2 foreign event struct, or the :IDLE symbol
  (case event-type

    (:keydown
     (let ((keysym (plus-c:c-ref event sdl2-ffi:sdl-event :key :keysym)))
       (when (or (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-q)
                 (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape))
         (sdl2:push-event :quit))
       (cond
         ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-right)
          (incf (getf data :x) 5))
         ((sdl2:scancode= (sdl2:scancode-value keysym) :scancode-left)
          (decf (getf data :x) 5)))))

    (:idle
     (sdl2:set-render-draw-color renderer 0 0 0 255)
     (sdl2:render-clear renderer)
     (sdl2:set-render-draw-color renderer 25 25 200 255)
     (sdl2:render-fill-rect renderer (sdl2:make-rect (getf data :x) 50 25 25))

     #++(sdl2:set-render-draw-color renderer 255 0 255 255)
     #++(let ((x-position 100)
              (y-position 200)
              (width 500)
              (height 25))
          (let ((rectangle (sdl2:make-rect x-position y-position width height)))
            (sdl2:render-fill-rect renderer rectangle)))
     (sdl2:render-present renderer))

    (:quit
     (return-from handle-event :quit)))
  nil)
