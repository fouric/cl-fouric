(defun find-dollar-signs (string)
  (labels ((find-next-break-char (pos)
	     (loop as i to (1- (length string)) do
		  (let ((char (schar string i)))
		    (when (or (char= char #\Space) (char= char #\~))
		      (return i)))))
	   (finder (positions)
	     (let* ((last-match (car positions))
		    (start-index (if last-match
				     (1+ last-match)
				     0))
		    (pos (if (= (length string) start-index)
			     nil
			     (position #\$ string
				       :start start-index
				       :end (find-next-break-char start-index)))))
	       (if pos
		   (finder (push pos positions))
		   positions))))
    (nreverse (finder nil))))

(defun extract-tokens (string positions)
  (remove-if (lambda (str)
	       (or (string= str "")
		   (string= str "$")))
	     (mapcar (lambda (position)
		       (let ((end (position #\Space string :start position)))
			 (subseq string position end))) positions)))

(defun $token->token (token)
  (intern (string-upcase (subseq token 1))))

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun string-replace (string find replace)
  (let ((position (search find string))
	(length (length find)))
    (if position
	(concatenate 'string
		     (subseq string 0 position)
		     replace
		     (subseq string (+ position length)))
	string)))

(defun transform-string (string tokens)
  (let ((vars nil)
	(string (copy-seq string)))
    (setf string (replace-all string "~" "~~"))
    (dolist (token tokens)
      (let ((sym-token ($token->token token)))
	(setf string (replace-all string token "~A"))
	(push sym-token vars)))
    (values string (nreverse vars))))

;; warning: breaks (defun foo-$bar (...) ...)

(defun dollar-sign-reader (stream char)
  (declare (ignore char))
  (let ((string (read stream t nil t)))
    (if (stringp string)
	(multiple-value-bind (string vars)
	    (transform-string string (extract-tokens string (find-dollar-signs string)))
	  `(format nil ,string ,@vars))
	string)))

(set-macro-character #\$ #'dollar-sign-reader)
