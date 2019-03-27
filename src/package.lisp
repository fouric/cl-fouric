(defpackage #:fouric
  (:use #:cl)
  (:local-nicknames (:a :alexandria))
  (:export
   #:read-file
   #:file-lines
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
   #:*whitespace-characters*
   #:eql/package-relaxed
   #:edit-definition
   #:emacs-jump-to-term
   #:intersperse
   #:unix-to-universal-time
   #:update-swank
   #:udefun
   #:cond?
   #:out
   ))
