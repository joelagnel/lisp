;; lm.el --- lousy mime file attachment renderer

;; Copyright (C) 2000 S.Namba <sn@asahi-net.email.ne.jp>
;; Thu Mar 30 20:40:38 2000 Started

;; Depends largely on /usr/local/bin/uuenview output, so if the command
;; is modified, this program may not make sense at all.
;; Assumed uuenview version: UUENVIEW 0.5pl13

(provide 'lm)

;;
;; Variables and Inline Routines
;;

(defconst lm-version "Lousy Mime 0.1")

(defvar lm-framework-cmd "uuenview -b -a"
  "Shell command to make mime framework.
Perhaps defaults \"uuenview -b -a\".

The version of the uuenview\(1\) used to develop `lm.el' is UUENVIEW 0.5pl13,
and it is assumed to have `-a' option.

Eval the expression below to see uuenview\(1\) manual.

\(manual-entry \"uuenview\(1\)\"\)")

(defconst lm-coding-system
  (cond
   ((coding-system-p '*junet*unix) '*junet*unix) ; mule-2
   ((coding-system-p 'junet-unix) 'junet-unix) ; mule-4
   (t (error "can't decide lm-coding-system."))))

;; I really *HATE* the program suite because I can't write right code:
;; set-buffer-file-coding-system: alias for `set-file-coding-system' in `poem-om'.

(defconst lm-set-coding-system-func 
  (cond ((fboundp 'set-file-coding-system) ; mule-2
	 'set-file-coding-system)
	((fboundp 'set-buffer-file-coding-system) ; mule-4
	 'set-buffer-file-coding-system)))

(defconst lkk-default-process-coding-system
  (cons lm-coding-system lm-coding-system))

(defsubst lm-set-coding-system ()
  (funcall lm-set-coding-system-func lm-coding-system))

(defsubst lm-insert-header ()
  (insert (format "User-Agent: %s on emacs-%s\n"
		  lm-version emacs-version)))

(defsubst lm-fake-boundary ()
  (while (lm-re-replace "==UUD_=_" "lousy_mime_-_"  'nomove)))

(defsubst lm-junet-enc-charset ()
  (lm-re-replace "Content-Transfer-Encoding: 8bit"
		 (concat "Content-Transfer-Encoding: 7bit"
			 "\; charset=ISO-2022-JP") nil))

(defsubst lm-re-replace (str0 str1 &optional nomove)
  "re-search STR0 and replace it with STR1.
Opt 3rd arg is NOMOVE, otherwise goto bob before search.
Return t if replaced, otherwise nil."
  (save-excursion
    (or nomove (goto-char (point-min)))
    (cond ((search-forward-regexp str0 (point-max) t)
	   (goto-char (match-beginning 0))
	   (delete-region (match-beginning 0) (match-end 0))
	   (insert str1)
	   t))))

(defsubst lm-get-fname (file)
  (cond ((or (string= default-directory (expand-file-name file))
	     ;; in case (current-buffer) showing a visited file:
	     (string= buffer-file-name (expand-file-name file))
	     (string= "" file))	; user erased the suggested part
	 "/dev/null")
	(t (expand-file-name file))) )

;;
;; Keyboard Interface
;;

(defun lm-framework-region (rb re afile &optional afilelist)
  "Convert the mail body in region RB RE into mime-multi-part framework.
In interactive use, asks a file name AFILE, which is a filename to be
inserted as mime attachment.  If the user-input for AFILE does not
make sense, /dev/null is used as the attachment file name.

The shell command used to build the mime framework should be stored in
`lm-framework-cmd'.  Returns t on success or nil on failure.

If optional 4th arg AFILELIST is supplied the filename list is handed to 
`lm-framework-cmd' with concatenated. This is mostly for non-interactive use."
  ;; This simple format raises error if the buffer is not marked.
  ;; (interactive "*r\nfAttachment: ")
  (interactive
   (let ((afile (read-file-name "Attachment: " nil nil t nil)))
     ;; If the buffer is not marked, create a region of length 0 on point.
     (or (mark t) (set-mark (point)))
     ;; Region points must be eval'ed at the end (it's okay in this fun, though)
     ;; See *info* node `Using Interactive'.
     (list (region-beginning) (region-end) afile)))

  (let ((default-process-coding-system lkk-default-process-coding-system))
    (save-excursion
      (cond
       ((= 0 (call-process-region
	      rb re shell-file-name t t nil "-c"
	      (concat
	       lm-framework-cmd " "
	       ;; Take care of afilelist
	       (cond
		(afilelist
		 (let (files)
		   (mapcar
		    (lambda (c)
		      (setq files (concat files " " c)))
		    afilelist)
		   files))
		(t (lm-get-fname afile)))
	       )))
	(goto-char (point-min))
	(lm-insert-header)
	(lm-fake-boundary)
	(lm-junet-enc-charset)
	(lm-set-coding-system)
	t))
      )))

(defun lm-framework-region-plain (rb re textfile)
  "Almost compatible to `lm-framework-region' except attachment is inserted \
as `plain/text' or `plain/html'.
Boundary header `plain/text' or `plain/html' is decided by the extension
of specified as 3rd arg TEXTFILE."
  ;; (interactive "*r\nfText attachment: ") ;; See lm-framework-region()
  (interactive
   (let ((textfile (read-file-name "Attachment: " nil nil t nil)))
     (or (mark t) (set-mark (point)))
     (list (region-beginning) (region-end) textfile)))

  (cond
   ((lm-framework-region rb re "")
    (save-excursion
      (goto-char (point-min))
      (lm-re-replace "Content-Type: Application/Octet-Stream"
		     (concat "Content-Type: "
			     (let ((case-fold-search t))
			       (if (string-match "htm" textfile)
				   "text/html"
				 "text/plain"))
			     "; charset=ISO-2022-JP") nil)
      (lm-re-replace "Content-Transfer-Encoding: Base64"
		     "Content-Transfer-Encoding: 7bit" 'nomove)
      (lm-re-replace "filename=\"null\""
		     (concat "filename=\""
			     (file-name-nondirectory textfile)
			     "\""))
      (search-forward "filename=\"" (point-max) t)
      (forward-line 2)
      ;; delete a empty line corresponding to the /dev/null
      (delete-char 1)
      (insert-file-contents (lm-get-fname textfile))
      (lm-set-coding-system))
    t)))

;;
;; `mail-send-hook' defined in sendmail.el
;; My sendmail-program => "/home/sn/bin/impost":
;;

(add-hook 'mail-send-hook 'lm-check-coding-system)
(defun lm-check-coding-system ()
  "Check if `default-process-coding-system' is `junet' sort of."
  (cond
   ((and (string= mode-name "Mail")
	 (or (string< "19" emacs-version)
	     ;; sendmail.el in emacs-20 has failsafe:
	     ;; (coding-system-for-write (select-message-coding-system))
	     ;; in sendmail-send-it().
	     (eq lm-coding-system (cdr default-process-coding-system))
	     (eq 'iso-2022-jp (cdr default-process-coding-system))
	     (eq '*junet* (cdr default-process-coding-system))))
    ;; It's allright
    t)
   (t (cond ((y-or-n-p
	      (format
	       "Is default-process-coding-system: %s correct ? "
	       default-process-coding-system))
	     t)
	    (t (error "%s aborted" this-command))))))
