;;; mew-mime.el --- MIME launcher for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997

;;; Code:

(require 'mew)

(defvar mew-process-file-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Start and call process
;;;

(defun mew-mime-start-process (program options file)
  (let ((process-connection-type mew-connection-type1) pro)
    (message "Starting %s..." program)
    (setq pro (apply 'mew-start-process-disp
		     (format "*mew %s*" program)
		     nil
		     program
		     (append options (list file))))
    (set-process-sentinel pro 'mew-mime-start-process-sentinel)
    (message "Starting %s...done" program)
    (setq mew-process-file-alist (cons (cons pro file) mew-process-file-alist))
    t)) ;; to next part

(defun mew-mime-start-process-sentinel (process event)
  (let* ((al (assoc process mew-process-file-alist))
	 (file (cdr al)))
    ;; A launcher program may be executed.
    ;; The launcher program executes an application according to
    ;; file's suffix or something.
    ;; The time when the launcher is finished is not the time when
    ;; the application is finished. So, we can't delete the temporary
    ;; file here. Hoping that the file will be deleted when Mew is 
    ;; finished.
    (if mew-delete-temp-file (mew-delete-file file))
    (setq mew-process-file-alist (delq al mew-process-file-alist))))

(defun mew-mime-call-process (program options file)
  (message "Calling %s..." program)
  (apply 'call-process program file nil nil options)
  (mew-delete-file file)
  (message "Calling %s...done" program)
  t) ;; to next part

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Part handler
;;;

(defun mew-mime-part (cache fullpart nums)
  ;; called in Message buffer
  ;; if nums is nil, it means singlepart.
  (let* ((syntax  (mew-syntax-get-entry fullpart nums))
	 (begin   (mew-syntax-get-begin syntax))
	 (end     (mew-syntax-get-end   syntax))
	 (ctl     (mew-syntax-get-ct    syntax))
	 (cte     (mew-syntax-get-cte   syntax))
	 (ct      (mew-syntax-get-value ctl 'cap))
	 (cdpl    (mew-syntax-get-cdp syntax))
	 (fname   (mew-syntax-get-filename cdpl ctl))
	 (cd      (mew-syntax-get-cd syntax))
	 (params  (mew-syntax-get-params ctl))
	 (program (mew-ctdb-prog (mew-ctdb-by-ct ct))))
    (cond
     ((symbolp program)
      (when (fboundp program)
	(cond
	 ((eq program 'mew-mime-message/rfc822)
	  (funcall program cache syntax)) ;; for recursive MIME
	 ((eq program 'mew-mime-application/octet-stream)
	  (funcall program cache begin end params ct cte fname))
	 (t
	  (funcall program cache begin end params)))))
     ((and (listp program) (nth 0 program) (symbolp (nth 0 program)))
      (when (fboundp (nth 0 program))
	(if (mew-ct-imagep ct)
	    (funcall (nth 0 program) cache begin end params fname ct cte)
	  (if (eq (nth 0 program) 'mew-mime-application/octet-stream)
	      (funcall (nth 0 program) cache begin end params ct cte fname (nth 1 program))
	    (funcall (nth 0 program) cache begin end params)))))
     ((and (listp program) (nth 0 program) (stringp (nth 0 program)))
      (insert " ######  ######  #######  #####  ######     #    #     #\n"
	      " #     # #     # #     # #     # #     #   # #   ##   ##\n"
	      " #     # #     # #     # #       #     #  #   #  # # # #\n"
	      " ######  ######  #     # #  #### ######  #     # #  #  #\n"
	      " #       #   #   #     # #     # #   #   ####### #     #\n"
	      " #       #    #  #     # #     # #    #  #     # #     #\n"
	      " #       #     # #######  #####  #     # #     # #     #\n"
	      "\n\n")
      (mew-insert "Content-Type:\t%s\n" ct)
      (mew-insert "Encoding:\t%s\n" cte)
      (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
      (mew-insert "Filename:\t%s\n" fname)
      (mew-insert "Description:\t%s\n" cd)
      (mew-insert "Program:\t%s\n" (nth 0 program))
      (insert "\n")
      (mew-mime-part-messages t)))))

(defun mew-mime-part-messages (prog)
  (cond
   ((and prog (fboundp prog))
    (mew-insert-manual
     "To execute this function, type "
     "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n"
     "\n"))
   (prog
    (mew-insert-manual
     "To execute this external command, type "
     "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n"
     "\n")))
  (mew-insert-manual
   "To save this part, type "
   "'\\<mew-summary-mode-map>\\[mew-summary-save]'.\n"
   "To insert this part in Message mode, type "
   "'\\<mew-summary-mode-map>\\[mew-summary-display-asis]'.\n"
   "To execute a specified external command, type "
   "'\\<mew-summary-mode-map>\\[mew-summary-execute-command]'.\n"
   "To display this part as a specified Content-Type:, type "
   "'\\[universal-argument] \\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text/Plain
;;;

(defun mew-mime-text/plain (cache begin end &optional params)
  ;; called in Message buffer
  (when (> end begin)
    (save-excursion
      ;; We need to keep properties (e.g. citation color)
      ;; in a message cache.
      ;; This must be "insert-buffer-substring".
      (insert-buffer-substring cache begin end)
      (when mew-insert-final-newline
	(save-excursion
	  (goto-char (point-max))
	  (if (not (bolp)) (insert "\n"))))
      ;; Page breaks
      (when mew-break-pages
	(goto-char (point-min))
	(mew-message-narrow-to-page)))))

(defun mew-mime-text/plain-ext (cache begin end &optional params)
  "To call the Text/Plain function for a text attachment whose
Content-Type: is Application/Octet-Stream."
  ;; called in Message buffer
  (when (> end begin)
    (mew-erase-buffer)
    ;; We need to keep properties (e.g. citation color)
    ;; in a message cache.
    ;; This must be "insert-buffer-substring".
    (insert-buffer-substring cache begin end)
    (unless (and mew-inherit-ct (mew-ct-textp mew-inherit-ct)
		 (or (mew-cache-dinfo-get-decode-broken cache)
		     (mew-charset-to-cs
		      (mew-syntax-get-param params "charset"))))
      (mew-cs-decode-region (point-min) (point-max) mew-cs-autoconv))
    (when mew-insert-final-newline
      (save-excursion
	(goto-char (point-max))
	(if (not (bolp)) (insert "\n"))))
    ;; Page breaks
    (when mew-break-pages
      (goto-char (point-min))
      (mew-message-narrow-to-page))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text/Enriched
;;;

(defun mew-mime-text/enriched (cache begin end &optional params)
  ;; called in Message buffer
  (when (> end begin)
    (save-excursion
      (let ((start (point)))
	;; We need to keep composite properties of charset.
	;; This must be "insert-buffer-substring".
	(insert-buffer-substring cache begin end)
	;; Highlight
	(when mew-use-text/enriched
	  (condition-case nil
	      ;; format.el is buggy.
	      (format-decode-region start (point-max) 'text/enriched)
	    (args-out-of-range (message nil))) ;; just clear
	  (enriched-mode 0)))
      ;; Page breaks
      (when mew-break-pages
	(goto-char (point-min))
	(mew-message-narrow-to-page)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text/Html
;;;

(defun mew-mime-text/html (cache begin end &optional params)
  ;; called in Message buffer
  (if (and (symbolp mew-prog-text/html) (fboundp mew-prog-text/html))
      (save-excursion
	(let ((start (point))
	      folder)
	  ;; We need to keep composite properties of charset.
	  ;; This must be "insert-buffer-substring".
	  (insert-buffer-substring cache begin end)
	  (save-excursion
	    (set-buffer cache)
	    (setq folder (mew-cinfo-get-fld)))
	  (if (or mew-use-text/html
		  (and mew-use-text/html-list
		       (mew-folder-spec folder
					mew-use-text/html-list
					mew-use-text/html-string-type
					mew-use-text/html-list-type)))
	      (funcall mew-prog-text/html start (point-max))
	    (mew-message-for-summary "To parse HTML, type '\\[mew-summary-analyze-again]'"))))
    (insert " #     # ####### #     # #\n"
	    " #     #    #    ##   ## #\n"
	    " #     #    #    # # # # #\n"
	    " #######    #    #  #  # #\n"
	    " #     #    #    #     # #\n"
	    " #     #    #    #     # #\n"
	    " #     #    #    #     # #######\n"
	    "\n\n")
    (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
    (mew-insert "Browser:\t%s\n" (if (stringp mew-prog-text/html-ext)
				     mew-prog-text/html-ext
				   "none"))
    (insert "\n")
    (mew-mime-part-messages t)))

(defun mew-mime-text/html-ext (cache begin end &optional params)
  ;; called in Message buffer
  (cond
   ((and (symbolp mew-prog-text/html-ext) (fboundp mew-prog-text/html-ext))
    (message "Displaying HTML...")
    (save-excursion
      (mew-erase-buffer)
      ;; We need to keep composite properties of charset.
      ;; This must be "insert-buffer-substring".
      (insert-buffer-substring cache begin end)
      (unless (and mew-inherit-ct (mew-ct-textp mew-inherit-ct)
		   (or (mew-cache-dinfo-get-decode-broken cache)
		       (mew-charset-to-cs
			(mew-syntax-get-param params "charset"))))
	(mew-cs-decode-region (point-min) (point-max) mew-cs-autoconv))
      (funcall mew-prog-text/html-ext (point-min) (point-max)))
    (message "Displaying HTML...done"))
   ((stringp mew-prog-text/html-ext)
    (when (> end begin)
      (let ((file (format mew-format-html (mew-make-temp-name)))
	    (orig mew-prog-text/html-ext-arg)
	    arg wcs esqp)
	(save-excursion
	  (message "Displaying Text/Html...")
	  (set-buffer cache)
	  (while orig
	    (if (string-match "%s" (car orig))
		(progn
		  (setq arg (cons (format (car orig) file) arg))
		  (setq esqp t))
	      (setq arg (cons (car orig) arg)))
	    (setq orig (cdr orig)))
	  (unless esqp (setq arg (cons file arg)))
	  (setq arg (nreverse arg))
	  (setq wcs (mew-text/html-detect-cs begin end params))
	  (unless (mew-coding-system-p wcs)
	    (setq wcs (mew-charset-to-cs
		       (mew-syntax-get-param params "charset"))))
	  (mew-frwlet
	   mew-cs-dummy wcs
	   (write-region begin end file nil 'no-msg)
	   (apply 'mew-start-process-disp
		  mew-prog-text/html-ext nil mew-prog-text/html-ext arg))
	  (message "Displaying Text/Html...done")))))))

(defun mew-text/html-detect-cs (begin end params)
  (let ((cs (mew-charset-to-cs (mew-syntax-get-param params "charset")))
	(case-fold-search t))
    (save-excursion
      (goto-char begin)
      (cond
       ((or (re-search-forward
	     (concat "<meta[ \t]+http-equiv=\"?content-type\"?[ \t]"
		     "+content=\"[^;]+"
		     ";[ \t]*charset=\"?\\([^\"]+\\)\"?[ \t]*/?>")
	     (min end (+ begin 1024)) t)
	    (re-search-forward
	     (concat "<meta[ \t]+content=\"[^;]+"
		     ";[ \t]*charset=\"?\\([^\"]+\\)\"?"
		     "[ \t]+http-equiv=\"?content-type\"?[ \t]*/?>")
	     (min end (+ begin 1024)) t))
	(mew-charset-to-cs (mew-match-string 1)))
       (cs cs)
       (t  ;; charset eq "us-ascii", cs is nil.
	(if mew-decode-broken ;; xxx
	    (mew-charset-to-cs (mew-charset-guess-region begin end))
	  ;; this text is no decoded.
	  mew-cs-text-for-write))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Text/XML
;;;

(defun mew-mime-text/xml (cache begin end &optional params)
  ;; called in Message buffer
  (if (and (symbolp mew-prog-text/xml) (fboundp mew-prog-text/xml))
      (save-excursion
	(let ((start (point)))
	  ;; We need to keep composite properties of charset.
	  ;; This must be "insert-buffer-substring".
	  (insert-buffer-substring cache begin end)
	  (if mew-use-text/xml
	      (funcall mew-prog-text/xml start (point-max))
	    (mew-message-for-summary "To parse XML, type '\\[mew-summary-analyze-again]'"))))
    (insert " #     # #     # #\n"
	    "  #   #  ##   ## #\n"
	    "   # #   # # # # #\n"
	    "    #    #  #  # #\n"
	    "   # #   #     # #\n"
	    "  #   #  #     # #\n"
	    " #     # #     # #######\n"
	    "\n\n")
    (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
    (mew-insert "Browser:\t%s\n" (if (stringp mew-prog-text/xml-ext)
				     mew-prog-text/xml-ext
				   "none"))
    (insert "\n")
    (mew-mime-part-messages t)))

(defun mew-mime-text/xml-ext (cache begin end &optional params)
  ;; called in Message buffer
  (cond
   ((and (symbolp mew-prog-text/xml-ext) (fboundp mew-prog-text/xml-ext))
    (message "Displaying XML...")
    (save-excursion
      (mew-erase-buffer)
      ;; We need to keep composite properties of charset.
      ;; This must be "insert-buffer-substring".
      (insert-buffer-substring cache begin end)
      (unless (and mew-inherit-ct (mew-ct-textp mew-inherit-ct)
		   (or (mew-cache-dinfo-get-decode-broken cache)
		       (mew-charset-to-cs
			(mew-syntax-get-param params "charset"))))
	(mew-cs-decode-region (point-min) (point-max) mew-cs-autoconv))
      (funcall mew-prog-text/xml-ext (point-min) (point-max)))
    (message "Displaying XML...done"))
   ((stringp mew-prog-text/xml-ext)
    (when (> end begin)
      (let ((file (format mew-format-xml (mew-make-temp-name)))
	    (orig mew-prog-text/xml-ext-arg)
	    arg wcs esqp)
	(save-excursion
	  (message "Displaying Text/Xml...")
	  (set-buffer cache)
	  (while orig
	    (if (string-match "%s" (car orig))
		(progn
		  (setq arg (cons (format (car orig) file) arg))
		  (setq esqp t))
	      (setq arg (cons (car orig) arg)))
	    (setq orig (cdr orig)))
	  (unless esqp (setq arg (cons file arg)))
	  (setq arg (nreverse arg))
	  (setq wcs (mew-text/xml-detect-cs begin end params))
	  (unless (mew-coding-system-p wcs)
	    (setq wcs (mew-charset-to-cs
		       (mew-syntax-get-param params "charset"))))
	  (mew-frwlet
	   mew-cs-dummy wcs
	   (write-region begin end file nil 'no-msg)
	   (apply 'mew-start-process-disp
		  mew-prog-text/xml-ext nil mew-prog-text/xml-ext arg))
	  (message "Displaying Text/Xml...done")))))))

(defun mew-text/xml-detect-cs (begin end params)
  (let ((cs (mew-charset-to-cs (mew-syntax-get-param params "charset")))
	(case-fold-search t))
    (save-excursion
      (goto-char begin)
      (cond
       ((re-search-forward
	 "<\\?xml.*encoding=['\"]\\([^'\"]+\\)['\"] *\\?>"
	 (min end (+ begin 1024)) t)
	(mew-charset-to-cs (mew-match-string 1)))
       (cs cs)
       (t  ;; charset eq "us-ascii", cs is nil.
	(if mew-decode-broken
	    (mew-charset-to-cs (mew-charset-guess-region begin end))
	  ;; this text is no decoded.
	  mew-cs-text-for-write))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application/Xml
;;;

(defun mew-mime-application/xml (cache begin end &optional params)
  ;; called in Message buffer
  (if (and (symbolp mew-prog-application/xml)
	   (fboundp mew-prog-application/xml))
      (save-excursion
	(let ((start (point)))
	  ;; We need to keep composite properties of charset.
	  ;; This must be "insert-buffer-substring".
	  (insert-buffer-substring cache begin end)
	  (funcall mew-prog-application/xml start (point-max))))
    (insert " #     # #     # #\n"
	    "  #   #  ##   ## #\n"
	    "   # #   # # # # #\n"
	    "    #    #  #  # #\n"
	    "   # #   #     # #\n"
	    "  #   #  #     # #\n"
	    " #     # #     # #######\n"
	    "\n\n")
    (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
    (mew-insert "Browser:\t%s\n" (if (stringp mew-prog-application/xml-ext)
				     mew-prog-application/xml-ext
				   "none"))
    (insert "\n")
    (mew-mime-part-messages t)))

(defun mew-mime-application/xml-ext (cache begin end &optional params)
  ;; called in Message buffer
  (cond
   ((and (symbolp mew-prog-application/xml-ext) (fboundp mew-prog-application/xml-ext))
    (message "Displaying XML...")
    (save-excursion
      (mew-erase-buffer)
      ;; We need to keep composite properties of charset.
      ;; This must be "insert-buffer-substring".
      (insert-buffer-substring cache begin end)
      (funcall mew-prog-application/xml-ext (point-min) (point-max)))
    (message "Displaying XML...done"))
   ((stringp mew-prog-application/xml-ext)
    (when (> end begin)
      (let ((file (format "%s.xml" (mew-make-temp-name)))
	    (orig mew-prog-application/xml-ext-arg)
	    arg wcs esqp)
	(save-excursion
	  (message "Displaying Application/Xml...")
	  (set-buffer cache)
	  (while orig
	    (if (string-match "%s" (car orig))
		(progn
		  (setq arg (cons (format (car orig) file) arg))
		  (setq esqp t))
	      (setq arg (cons (car orig) arg)))
	    (setq orig (cdr orig)))
	  (unless esqp (setq arg (cons file arg)))
	  (setq arg (nreverse arg))
	  (setq wcs mew-cs-binary)
	  (mew-frwlet
	   mew-cs-dummy wcs
	   (write-region begin end file nil 'no-msg)
	   (apply 'mew-start-process-disp
		  mew-prog-application/xml-ext nil
		  mew-prog-application/xml-ext arg))
	  (message "Displaying Application/Xml...done")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Image/*
;;;

(defun mew-mime-image/* (cache begin end &optional params fname ct cte)
  (let* ((format (mew-mime-image-format-name ct)))
    (if (mew-image-inline-p format)
	(mew-mime-image cache begin end format)
      (insert "  #####  #     #    #     #####  #######\n"
	      "    #    ##   ##   # #   #     # #      \n"
	      "    #    # # # #  #   #  #       #      \n"
	      "    #    #  #  # #     # #  #### #######\n"
	      "    #    #     # ####### #     # #      \n"
	      "    #    #     # #     # #     # #      \n"
	      "  #####  #     # #     #  #####  #######\n"
	      "\n\n")
      (mew-insert "Content-Type:\t%s\n" ct)
      (mew-insert "Encoding:\t%s\n" cte)
      (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
      (mew-insert "Filename:\t%s\n" fname)
      (mew-insert "Program:\t%s\n" mew-prog-image/*-ext)
      (insert "\n")
      (mew-mime-part-messages t))))

(defun mew-mime-image/*-ext (cache begin end &optional params fname)
  (let ((file (mew-make-temp-name fname)))
    (save-excursion
      (set-buffer cache)
      (mew-flet
       (write-region begin end file nil 'no-msg)))
    (mew-mime-start-process
     mew-prog-image/*-ext mew-prog-image/*-ext-arg file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message/Rfc822
;;;

(defun mew-mime-text/rfc822-headers (cache begin end &optional params)
  ;; called in Message buffer
  (when (> end begin)
    (save-excursion
      ;; We need to keep properties of a header.
      ;; This must be "insert-buffer-substring".
      (insert-buffer-substring cache begin end)
      (mew-header-arrange (point-min) (point-max)))))

(defun mew-mime-message/rfc822 (cache part)
  ;; called in Message buffer
  (let* ((hbeg (mew-syntax-get-begin part))
	 (hend (mew-syntax-get-end   part))
	 (body (mew-syntax-get-part part)))
    ;; We need to keep properties of a header.
    ;; This must be "insert-buffer-substring".
    (insert-buffer-substring cache hbeg hend)
    (mew-header-arrange (point-min) (point-max))
    (cond
     ;; Displaying the text/plain body or the first part of 
     ;; top level multipart if it is text/plain.
     ;; see also mew-syntax-singlepart
     ((mew-syntax-singlepart-p body)
      (mew-mime-part cache body nil)) ;; nil is single
     ((mew-syntax-multipart-p body)
      (let* ((first (mew-syntax-get-part body))
	     (ct (mew-syntax-get-value (mew-syntax-get-ct first) 'cap)))
	(when (and (mew-xinfo-get-text-body) (mew-ct-textp ct))
	  (mew-mime-part cache first nil))))))) ;; nil is single

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application/Octet-Stream
;;;

(defun mew-mime-application/octet-stream (cache begin end &optional params ct cte fl func)
  (insert " ######    ###   #     #    #    ######  #     #\n"
	  " #     #    #    ##    #   # #   #     #  #   #\n"
	  " #     #    #    # #   #  #   #  #     #   # #\n"
	  " ######     #    #  #  # #     # ######     #\n"
	  " #     #    #    #   # # ####### #   #      #\n"
	  " #     #    #    #    ## #     # #    #     #\n"
	  " ######    ###   #     # #     # #     #    #\n"
	  "\n\n")
  (mew-insert "Content-Type:\t%s\n" ct)
  (mew-insert "Encoding:\t%s\n" cte)
  (when params
    (mew-insert
     "Parameters:\t%s\n"
     (mapconcat (lambda (x) (concat (nth 0 x) "=" (nth 1 x))) params ", ")))
  (mew-insert "Size:\t\t%d bytes\n" (mew-region-bytes begin end cache))
  (mew-insert "Filename:\t%s\n" fl)
  (when func (mew-insert "Function:\t%s\n" func))
  (insert "\n")
  (mew-mime-part-messages func))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application/Msoffice
;;;

(defun mew-mime-application/msword (cache begin end &optional parameter)
  (mew-mime-application/msoffice
   mew-prog-application/msword cache begin end parameter))

(defun mew-mime-application/msexcel (cache begin end &optional parameter)
  (mew-mime-application/msoffice
   mew-prog-application/msexcel cache begin end parameter))

(defun mew-mime-application/mspowerpoint (cache begin end &optional parameter)
  (mew-mime-application/msoffice
   mew-prog-application/mspowerpoint cache begin end parameter))

(defun mew-mime-application/rtf (cache begin end &optional parameter)
  (mew-mime-application/msoffice
   mew-prog-application/rtf cache begin end parameter))

(defun mew-mime-application/msword-ext (cache begin end &optional parameter)
  (save-excursion
    (mew-erase-buffer)
    (mew-mime-application/msword cache begin end parameter)))

(defun mew-mime-application/msexcel-ext (cache begin end &optional parameter)
  (save-excursion
    (mew-erase-buffer)
    (mew-mime-application/msexcel cache begin end parameter)))

(defun mew-mime-application/mspowerpoint-ext (cache begin end &optional parameter)
  (save-excursion
    (mew-erase-buffer)
    (mew-mime-application/mspowerpoint cache begin end parameter)))

(defun mew-mime-application/rtf-ext (cache begin end &optional parameter)
  (save-excursion
    (mew-erase-buffer)
    (mew-mime-application/rtf cache begin end parameter)))

(defun mew-mime-application/msoffice (prog cache begin end &optional parameter)
  (let ((doit t) file1 file2)
    (unless mew-internal-utf-8p
      (condition-case nil
	  (require 'un-define)
	(file-error
	 (setq doit nil)
	 (insert "To display this, install Mule-UCS for UTF-8.\n"))))
    (unless (mew-which-exec prog)
      (setq doit nil)
      (insert "To display this, install \"" prog "\".\n"))
    (condition-case nil
	(require 'mew-w3m)
      (file-error
       (setq doit nil)
       (insert "To display this, install \"w3m.el\".\n")))
    (if (not doit)
	(progn
	  (insert "\n")
	  (mew-mime-part-messages nil))
      (message "Displaying an MS document...")
      (setq file1 (mew-make-temp-name))
      (save-excursion
	(set-buffer cache)
	(mew-flet
	 (write-region begin end file1 nil 'no-msg)))
      (setq file2 (mew-make-temp-name))
      (if (eq prog mew-prog-application/msword)
 	  (mew-frwlet
 	   'utf-8 mew-cs-dummy
	   (if mew-use-old-wvhtml
	       (call-process prog nil nil nil file1 file2)
	     (call-process prog nil nil nil
			   "--charset=utf-8"
			   (concat "--targetdir=" (file-name-directory file2))
			   file1 
			   (file-name-nondirectory file2)))
	   (let ((buffer-file-coding-system)) ;; to prevent the side effect
	     (mew-insert-file-contents file2)))
	(if (eq prog mew-prog-application/rtf)
	    (mew-frwlet
	     'shift_jis mew-cs-dummy
	     (call-process prog nil (current-buffer) nil file1))
	  (mew-frwlet
	   'utf-8 mew-cs-dummy
	   (call-process prog nil (current-buffer) nil file1))))
      (mew-delete-file file1)
      (mew-delete-file file2)
      (save-excursion
	(w3m-region (point-min) (point-max)
		    (w3m-expand-file-name-as-url (file-name-directory file2))))
      (message "Displaying an MS document...done"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Application/Ms-Tnef
;;;

(defun mew-mime-application-ms-tnef (cache begin end &optional parameter)
  (insert " ####### #     # ####### ########\n"
	  "    #    ##    # #       #\n"
	  "    #    # #   # #       #\n"
	  "    #    #  #  # ####### ######\n"
	  "    #    #   # # #       #\n"
	  "    #    #    ## #       #\n"
	  "    #    #     # ####### #\n"
	  "\n\n")
  (mew-insert-manual
   "To extract files, type "
   "'\\<mew-summary-mode-map>\\[mew-summary-execute-external]'.\n"))

(defvar mew-prog-tnef "tnef")

(defun mew-mime-application-ms-tnef-ext (cache begin end &optional parameter)
  ;; called in Message buffer
  (let ((file (mew-make-temp-name))
	(dir (mew-input-directory-name mew-home)))
    (if (not (mew-which-exec mew-prog-tnef))
	(message "'%s' not found" mew-prog-tnef)
      (save-excursion
	(set-buffer cache)
	(mew-plet
	 (write-region begin end file nil 'no-msg)))
      (mew-erase-buffer)
      (insert "Extracted: \n\n")
      (call-process mew-prog-tnef file t nil "--verbose" "-C" dir)
      (mew-delete-file file))))

(provide 'mew-mime)

;;; Copyright Notice:

;; Copyright (C) 1997-2005 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-mime.el ends here
