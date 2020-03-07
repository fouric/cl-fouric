(in-package #:fouric)

(deftype pathname-designator ()
  '(or string
    pathname
    ;;file-associated-stream
    ))

(deftype function-designator ()
  '(or symbol
    function))

(deftype string-designator ()
  '(or character
    symbol
    string))
