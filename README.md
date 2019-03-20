# cl-fouric

Common Lisp utilities I use a lot. Check out [Alexandria](https://common-lisp.net/project/alexandria/), [Serapeum](https://github.com/ruricolist/serapeum), and [cl-losh](https://github.com/sjl/cl-losh) *before* using anything from here.

# documented things:

* cond? : modified cond that overrides default/T clause to return T if any clauses matches and NIL if it doesn't. use: saves you a bit of time for when you don't want to explicitly control the return value of every clause to make sure that the last value in a clause doesn't accidentally return NIL or something. todo: try to find and patch default/T clause instead of overriding it.

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
* udefun
* cond?
