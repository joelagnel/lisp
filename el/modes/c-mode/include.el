;; include.el - quick hack
;; Tijs van Bakel, <smoke@wanadoo.nl>

(defun c-include-at-point ()
  "return the filename in the next #include statement."
  (save-excursion
    (beginning-of-line)
    (re-search-forward "[\"<]")
    (let ((start (point)))
      (re-search-forward "[\">]")
      (let ((end (point))
	    ;; It might be better to search a global and local include path list.
	    ;; This is a temporary hack.  Please send patches :-)
	    (prefix (if (string-equal (buffer-substring (1- start) start) "\"")
			"" ;; local include path
			"/usr/include/")))  ;; global include path
	  (concat prefix (format "%s" (buffer-substring start (1- end))))))))

(defun c-find-include ()
  "Visit the include file at point."
  (interactive)
  (find-file (c-include-at-point)))