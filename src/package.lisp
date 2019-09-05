(defpackage #:fouric
  (:use #:cl)
  (:local-nicknames (:a :alexandria))
  (:export
   #:read-file
   #:file-lines
   #:write-lines
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
   #:on-global-update
   #:nmember
   #:nonconsecutive-substring-match
   #:emacs-eval
   #:genspaces

   ;; charms
   #:with-charms
   #:init-charms
   #:update-charms-dimensions
   #:clear-window
   #:refresh-window
   #:get-char
   #:clamp-w
   #:clamp-h
   #:with-color
   #:write-string-at
   #:init-charms
   #:with-charms
   #:update-charms-dimensions
   #:+color-white-black+
   #:+color-black-white+
   #:+color-blue-black+
   #:+color-black-blue+
   #:+color-black-black+
   #:*screen-width*
   #:*screen-height*
   #:charms-draw-box
   #:window-dimensions

   ;; narrower
   #:make-narrower
   #:criteria
   #:candidates
   #:results
   ))
