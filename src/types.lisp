(in-package #:fouric)

(deftype pathname-designator ()
  '(or string
    #++file-associated-stream
    pathname))

(deftype function-designator ()
  '(or symbol
    function))

(deftype string-designator ()
  '(or character
    symbol
    string))
