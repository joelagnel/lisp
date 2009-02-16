;;;; a couple of things in my startup need this
;;; Time-stamp: <2005-08-12 14:46:42 jcgs>

(provide 'system-names)

;;;###autoload
(defun system-short-name ()
  "Return the first part of the name of the machine you are running on, as a string."
  (let* ((full-name (system-name))
         (dot (string-match "\\." full-name)))
    (if dot (substring full-name 0 dot) full-name)))

;;;###autoload
(defun system-domain-name ()
  "Return the domain of the machine you are running on, as a string."
  (let* ((full (system-name))
	 (short (system-short-name))
	 (short-len (length short)))
    (if (string= full short)
	"unknown"
      (if (string= short (substring full 0 short-len))
	  (substring full (1+ short-len))
	full))))

;;;; end of system-names.el
