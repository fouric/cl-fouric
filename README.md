# cl-fouric

Common Lisp utilities I use a lot. Check out [Alexandria](https://common-lisp.net/project/alexandria/), [Serapeum](https://github.com/ruricolist/serapeum), and [cl-losh](https://github.com/sjl/cl-losh) *before* using anything from here.

# documented things:

* cond? : modified cond that overrides default/T clause to return T if any clauses matches and NIL if it doesn't. use: saves you a bit of time for when you don't want to explicitly control the return value of every clause to make sure that the last value in a clause doesn't accidentally return NIL or something. todo: try to find and patch default/T clause instead of overriding it.

* out : "debug printf" macro that prints out both all expressions given and their evaluations

* udefun : augmented defun that allows you to run code when the given function is called after getting recompiled - useful for interactive development

* on-global-update : stores the last value of a global variable in its symbol-plist, and when that variable changes, runs the provided code - similar to udefun for globals

# undocumented things:

* read-file
* file-lines
* write-file
* with-init-window-gl
* profile
* resource
* doitimes
* while
* fn-case
* sdl2-omni
* with-ttf-init
* render-rectangle
* render-texture
* with-font
* clear-and-render
* make-text-texture
* render-text
* clampf
* inclampf
* +-clamp
* pushlast
* with-accessors+
* replace-texture
* \*whitespace-characters\*
* eql/package-relaxed
* edit-definition
* emacs-jump-to-term
* intersperse
* update-swank
