(defun lispdoc ()
  "searches lispdoc.com for SYMBOL, which is by default the symbol
currently under the curser"
  (interactive)
  (let* ((word-at-point (word-at-point))
	 (symbol-at-point (symbol-at-point))
	 (default (symbol-name symbol-at-point))
	 (inp (read-from-minibuffer
	       (if (or word-at-point symbol-at-point)
		   (concat "Symbol (default " default "): ")
		   "Symbol (no default): "))))
    (if (and (string= inp "")
             (not word-at-point)
             (not symbol-at-point))
	     (message "you didn't enter a symbol!")
	 (let ((search-type
            (read-from-minibuffer
			  "full-text (f) or basic (b) search (default b)? ")))
	  (browse-url (concat "http://lispdoc.com?q="
			      (if (string= inp "")
				  default
				  inp)
			      "&search="
			      (if (string-equal search-type "f")
				  "full+text+search"
				  "basic+search")))))))