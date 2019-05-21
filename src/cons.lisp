(in-package #:fouric)

(defun nmember (item list)
  (loop :for i :from 0
        :for cdr :on list
        :do (when (eq item (first cdr))
              (return-from nmember (values cdr i))))
  (values nil 0))
