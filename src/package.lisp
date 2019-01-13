(defpackage #:fouric
  (:use #:cl)
  (:export
   #:read-file
   #:write-file
   #:with-init-window-gl
   #:profile
   #:resource
   #:doitimes
   #:while
   #:fn-case
   #:sdl2-omni
   #:with-ttf-init
   #:render-rectangle
   #:render-texture
   #:with-font
   #:clear-and-render
   #:make-text-texture
   #:render-text
   #:clampf
   #:inclampf
   #:+-clamp
   #:pushlast
   #:with-accessors+
   #:replace-texture
   #:+whitespace-characters+
   ))
