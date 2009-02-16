;; Shut up.

(defun maybe-fbind (args)
  (while args
    (or (fboundp (car args))
	(defalias (car args) 'ignore))
    (setq args (cdr args))))

(defun maybe-bind (args)
  (mapcar (lambda (var) (unless (boundp var) (set var nil))) args))

(maybe-fbind '(Info-directory
	       Info-menu create-image display-graphic-p
	       display-time-event-handler find-coding-system find-image
	       image-size image-type-available-p insert-image
	       make-mode-line-mouse-map make-temp-file propertize put-image
	       replace-regexp-in-string rmail-msg-is-pruned
	       rmail-msg-restore-non-pruned-header sort-coding-systems
	       tool-bar-add-item tool-bar-add-item-from-menu
	       tool-bar-local-item-from-menu url-generic-parse-url
	       url-http-file-exists-p url-insert-file-contents
	       vcard-pretty-print w32-focus-frame w3m-detect-meta-charset
	       w3m-region x-focus-frame))
(maybe-bind '(filladapt-mode
	      rmail-insert-mime-forwarded-message-function
	      w3-meta-charset-content-type-regexp
	      w3-meta-content-type-charset-regexp))

(if (featurep 'xemacs)
    (progn
      (maybe-fbind '(delete-overlay
		     detect-coding-string event-click-count event-end
		     event-start find-coding-systems-for-charsets
		     find-coding-systems-region find-coding-systems-string
		     mail-abbrevs-setup mouse-minibuffer-check
		     mouse-movement-p mouse-scroll-subr overlay-lists
		     posn-point posn-window read-event set-buffer-multibyte
		     track-mouse window-edges))
      (maybe-bind '(adaptive-fill-first-line-regexp
		    buffer-display-table buffer-file-coding-system
		    current-language-environment
		    default-enable-multibyte-characters
		    enable-multibyte-characters gnus-agent-expire-current-dirs
		    language-info-alist mark-active mouse-selection-click-count
		    mouse-selection-click-count-buffer pgg-parse-crc24
		    temporary-file-directory transient-mark-mode)))
  (maybe-fbind '(bbdb-complete-name
		 delete-annotation delete-extent device-connection dfw-device
		 events-to-keys font-lock-set-defaults frame-device
		 get-char-table glyph-height glyph-width mail-aliases-setup
		 make-annotation make-event make-glyph make-network-process
		 map-extents message-xmas-redefine put-char-table
		 set-extent-property temp-directory
		 valid-image-instantiator-format-p
		 w3-coding-system-for-mime-charset w3-do-setup
		 w3-prepare-buffer w3-region window-pixel-height
		 window-pixel-width))
  (maybe-bind '(help-echo-owns-message mail-mode-hook mm-w3m-mode-map)))

(when (and (featurep 'xemacs)
	   (not (featurep 'mule)))
  (maybe-fbind '(ccl-execute-on-string
		 char-charset charsetp coding-system-get define-ccl-program
		 find-charset-region get-charset-property
		 pgg-parse-crc24-string))
  (unless (featurep 'file-coding)
    (maybe-fbind '(coding-system-base
		   coding-system-list coding-system-p decode-coding-region
		   decode-coding-string detect-coding-region
		   encode-coding-region encode-coding-string))))

(defun nnkiboze-score-file (a)
  )

(defun split-line (&optional arg)
  )

(provide 'lpath)

;;; arch-tag: d1ad864f-dca6-4d21-aa3f-be3248e66dba
