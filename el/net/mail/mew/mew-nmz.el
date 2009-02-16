;; -*- Mode: Emacs-Lisp -*-
;;
;; mew-nmz.el --- Namazu interfaces for Mew
;;
;;   Author: Hideyuki SHIRAI <shirai@mew.org>
;;   Created: Dec 24, 2004

(require 'mew)

(eval-when-compile
  (condition-case nil
      (require 'w3m-namazu)
    (file-error nil)))

;; Variables
(defgroup mew-nmz nil
  "Namazu support with Mew."
  :group 'mew)

(defcustom mew-nmz-index-path "~/Namazu"
  "*Namazu index top directory."
  :group 'mew-nmz
  :type 'directory)

(defcustom mew-nmz-index-mail "Mail"
  "*Namazu index mail directory name."
  :group 'mew-nmz
  :type 'string)

(defcustom mew-nmz-setup-hook nil
  "*Hook called on mew-nmz-setup."
  :group 'mew-nmz
  :type 'hook)

(defcustom mew-nmz-prog "namazu"
  "*Namazu program name."
  :group 'mew-nmz
  :type 'string)

(defcustom mew-nmz-prog-args nil
  "*Namazu's arguments."
  :group 'mew-nmz
  :type '(repeat string))

(defcustom mew-nmz-prog-mknmz "mknmz"
  "*Namazu make index program."
  :group 'mew-nmz
  :type 'string)

(defcustom mew-nmz-prog-mknmz-args nil
  "*Mknmz's argument, in addition to \"--no-encode-uri\", \"--mailnews\"."
  :group 'mew-nmz
  :type '(repeat string))

(defcustom mew-nmz-prog-mknmz-include "~/Namazu/mknmz-inc.pl"
  "*Include file for mknmz."
  :group 'mew-nmz
  :type 'file)

(defcustom mew-nmz-mknmz-skip-folders-regexp
  `(,(regexp-quote mew-draft-folder)
    ,(regexp-quote mew-trash-folder)
    ,(regexp-quote mew-queue-folder)
    ,(regexp-quote mew-attach-folder)
    ,(regexp-quote mew-imap-queue-folder)
    ,(regexp-quote mew-imap-trash-folder)
    ,(regexp-quote mew-postq-folder))
  "*Folders regexp to skip the index creating."
  :group 'mew-nmz
  :type '(repeat regexp))

(defcustom mew-nmz-mknmz-use-mode-line t
  "*Display indicator of namazu in mode line."
  :group 'mew-nmz
  :type 'boolean)

(defcustom mew-nmz-line-id '("Mew(nmz): %7b")
  "*A value of mode-line-buffer-identification for Mew summary mode, when mknmzing."
  :group 'mew-nmz
  :type '(list string))

(defcustom mew-nmz-mknmz-timer-interval 0.1
  "*Seconds of interval to execute next mknmz."
  :group 'mew-nmz
  :type 'number)

(defcustom mew-nmz-pick-default-field nil
  "*Default prefix string to be appeared when inputting a namazu pick pattern.
A good example is \"+from:\"."
  :group 'mew-nmz
  :type '(choice string (const nil)))

(defcustom mew-nmz-pick-field-list
  '("+subject:" "+from:" "+to:" "+newsgroups:" "+date:"
    "+message-id:" "+cc:" "+in-reply-to:" "+references:")
  "*A list of key for namazu pick pattern."
  :group 'mew-nmz
  :type '(repeat string))

(defcustom mew-nmz-pick-gather-field-list
  `((,mew-from: address "+from:" "+to:" "+cc:")
    (,mew-to: address "+from:" "+to:" "+cc:")
    (,mew-cc: address "+from:" "+to:" "+cc:")
    (,mew-message-id: msgid "+message-id:" "+in-reply-to:" "+references:")
    (,mew-in-reply-to: msgid "+message-id:" "+in-reply-to:" "+references:")
    (,mew-references: msgid "+message-id:" "+in-reply-to:" "+references:"))
  "*A list of completion keyword from message."
  :group 'mew-nmz
  :type '(repeat (list string
		       (choice (const address) (const msgid))
		       string string string)))

(defcustom mew-nmz-search-parent-folder `(,mew-inbox-folder)
  "*Search folder for parent or child, "
  :group 'mew-nmz
  :type '(repeat string))

(defcustom mew-nmz-db-max 64
  "*Namazu max index.
This value MUST be less then equal `INDEX_MAX' of libnamazu.h."
  :group 'mew-nmz
  :type 'integer)

(defcustom mew-nmz-query-max-length 256
  "*Namazu query string max length.
 This value MUST be less then equal `QUERY_MAX' of libnamazu.h."
  :group 'mew-nmz
  :type 'integer)

(defcustom mew-nmz-mark-pick mew-mark-review
  "*Mark for Namazu pick."
  :group 'mew-nmz
  :type 'character)

(defcustom mew-nmz-mark-unindexed mew-mark-review
  "*Mark for type unindexed messages."
  :group 'mew-nmz
  :type 'character)

(defcustom mew-nmz-mknmz-index-file ".mewmknmz"
  "*File name of the input file of mewmknmz. Expand mew-conf-path when use it."
  :group 'mew-nmz
  :type 'string)

(defcustom mew-nmz-input-folders-asterisk t
  "*Add \"*\" at the end of input folder name."
  :group 'mew-nmz
  :type 'boolean)

(defcustom mew-nmz-prog-gcnmz "gcnmz"
  "*Program name for the garbage collection."
  :group 'mew-nmz
  :type 'string)

(defcustom mew-nmz-use-gcnmz-folders-regexp `(,(regexp-quote mew-inbox-folder))
  "*Folders regexp to execute gcnmz, 't' means all folders."
  :group 'mew-nmz
  :type '(repeat regexp))

(defcustom mew-nmz-gcnmz-line-id '("Mew(gcn): %7b")
  "*A value of mode-line-buffer-identification for Mew summary mode, when gcnmzing."
  :group 'mew-nmz
  :type '(list string))

(defcustom mew-nmz-prog-rfnmz "rfnmz"
  "*Program name for the re-index."
  :group 'mew-nmz
  :type 'string)

(defcustom mew-nmz-mknmz-index-file-coding-system
  (if (boundp 'default-file-name-coding-system)
      default-file-name-coding-system)
  "*Coding system of index-file."
  :group 'mew-nmz
  :type '(coding-system :size 0))

;; internal variable, don't modify.
(defvar mew-nmz-pick-pattern-hist nil)
(defvar mew-nmz-gather-header-list nil)
(defvar mew-nmz-indexed-folders nil)

(defvar mew-nmz-input-folder-hist nil)

(defvar mew-nmz-mknmz-process nil)
(defvar mew-nmz-mknmz-process-folder nil)
(make-variable-buffer-local 'mew-nmz-mknmz-process)
(make-variable-buffer-local 'mew-nmz-mknmz-process-folder)

(defconst mew-nmz-namazu-index-alias "_mew-namazu"
  "*Alias name for mew-nmz-namazu.")
(defconst mew-nmz-namazu-content-type "message/mew")
(defvar mew-nmz-namazu-pattern nil)
(defvar mew-nmz-namazu-miss-folders nil)

(defvar mew-nmz-setup-p nil)
(defvar mew-nmz-imap-case-alist nil)
(defvar mew-nmz-nntp-case-alist nil)
(defvar mew-nmz-pop-case-alist nil)
(defvar mew-nmz-fld-index-alist nil)
(defvar mew-nmz-url-fld-alist nil)
(defvar mew-nmz-mknmz-all-folders nil)
(defvar mew-nmz-mknmz-continue-timer nil)

(defvar mew-nmz-mknmz-lang-alist
  '(("Japanese" "ja")))

(defvar mew-nmz-cs-index (cond
			  ((mew-coding-system-p 'euc-japan-unix)
			   'euc-japan-unix)
			  ((mew-coding-system-p 'euc-jp-unix)
			   'euc-jp-unix)
			  (t mew-cs-text-for-write))
  "*Coding system to write 'NMZ.field.uri'.")

(defconst mew-nmz-result-regex
  (concat "^\\(.*\\)" (regexp-quote mew-path-separator) "\\([0-9]+\\)"))

(defconst mew-nmz-use-drive-letter
  (memq system-type '(OS/2 emx windows-nt)))

(defun mew-nmz-original-message (&optional arg)
  "Jump to physical folder and message from virtual folder.
If executed with '\\[universal-argument]', move to next virtual folder include same physical folder."
  (interactive "P")
  (let (fld msg)
    (if (and (mew-virtual-p) (not (mew-thread-p)) (not arg))
	;; jump to physical folder
	(mew-summary-msg-or-part
	 (mew-summary-goto-message)
	 (setq fld (mew-summary-folder-name))
	 (setq msg (mew-summary-message-number))
	 (message "Jump to %s/%s..." fld msg)
	 (mew-nmz-goto-folder-msg fld msg)
	 (message "Jump to %s/%s...done" fld msg))
      ;; jump to next virtual folder
      (cond
       ((mew-thread-p)
	(setq fld (mew-vinfo-get-parent-folder)))
       ((mew-virtual-p)
	(when (or (mew-summary-message-number) (mew-syntax-number))
	  (mew-summary-goto-message)
	  (setq fld (mew-summary-folder-name))))
       (t
	(setq fld (mew-summary-folder-name))))
      (if (not fld)
	  (message "Nothing to do")
	(let ((bufs (delq (current-buffer) (buffer-list))) ;; remove current folder
	      buf)
	  (save-excursion
	    (setq fld (catch 'detect
			(while (setq buf (car bufs))
			  (setq bufs (cdr bufs))
			  (set-buffer buf)
			  (when (and (mew-virtual-p)
				     (member fld (mew-vinfo-get-flds)))
			    (throw 'detect (buffer-name buf)))))))
	  (if (not fld)
	      (message "Nothing to do")
	    (mew-summary-switch-to-folder fld)
	    (when (mew-sinfo-get-disp-msg)
	      (mew-summary-display 'redisplay))
	    (message "Move to %s" fld)))))))

(defsubst mew-nmz-expand-folder (case:folder)
  "Convert case:folder to the directory name of namazu's index."
  (let* ((fld (mew-case:folder-folder case:folder))
	 (imapp (mew-folder-imapp fld))
	 (mew-mail-path (concat (file-name-as-directory mew-nmz-index-path)
				mew-nmz-index-mail))
	 (nmzdir (directory-file-name (mew-expand-folder case:folder))))
    (if (not imapp)
	nmzdir
      (while (string-match "&" nmzdir)
	(setq nmzdir (concat (substring nmzdir 0 (match-beginning 0))
			     "%%"
			     (substring nmzdir (match-end 0)))))
      nmzdir)))

(defun mew-nmz-mknmz-lang-arg ()
  (when (boundp 'current-language-environment)
    (let* ((env current-language-environment)
	   (alist mew-nmz-mknmz-lang-alist)
	   (lang (nth 1 (assoc env alist))))
      (if lang (format "--indexing-lang=%s" lang)))))

;;
;; "Make Index" functions.
(defun mew-nmz-mknmz (&optional fld all)
  "Make namazu index for mew-nmz.
If executed with '\\[universal-argument]', make indices without the check of timestamp files.
If executed with '\\[universal-argument] 0', remove indices before make index."
  (interactive
   (list (directory-file-name
	  (mew-input-folder (mew-sinfo-get-case)
			    (or (mew-sinfo-get-folder) mew-inbox-folder)))
	 nil))
  (save-excursion
    (let ((msgenb (interactive-p))
	  (force (and current-prefix-arg
		      (not (eq current-prefix-arg 0))))
	  (remove (eq current-prefix-arg 0))
	  (flddir (mew-expand-folder fld))
	  (nmzdir (mew-nmz-expand-folder fld))
	  (bufname (concat " *mew mknmz*" fld))
	  (args mew-nmz-prog-mknmz-args)
	  (procname (concat mew-nmz-prog-mknmz "-" fld))
	  (procname2 (concat mew-nmz-prog-gcnmz "-" fld))
	  (incfile (and mew-nmz-prog-mknmz-include
			(expand-file-name mew-nmz-prog-mknmz-include)))
	  continue)
      (unless fld
	(setq fld (directory-file-name
		   (mew-input-folder (mew-sinfo-get-case) (mew-sinfo-get-folder)))))
      (cond
       ((not (mew-which-exec mew-nmz-prog-mknmz))
	(message "Please install mknmz"))
       ((or (mew-folder-virtualp fld) (mew-nmz-skip-folder-p fld))
	(and msgenb (message "Cannot make namazu index in %s" fld))
	(setq continue t))
       ((or (get-process procname) (get-process procname2))
	(and msgenb (message "Detect running mknmz/gcnmz process in %s" fld))
	(setq continue t))
       ((mew-nmz-have-no-msgs flddir)
	(and msgenb (message "%s has no message" fld))
	(mew-nmz-index-delete nmzdir)
	(setq continue t))
       ((and (not remove) (not force) (mew-nmz-index-new-p fld))
	(and msgenb (message "%s has a newer namazu index" fld))
	(setq continue t))
       ((and (not remove) (file-exists-p (expand-file-name "NMZ.lock2" nmzdir)))
	(message "Something error in %s's index" fld)
	(setq continue t))
       ((and flddir nmzdir (file-directory-p flddir))
	(setq args (delq nil
			 (append args
				 (list "--no-encode-uri" "--mailnews"
				       (mew-nmz-mknmz-lang-arg)
				       (when (and incfile (file-exists-p incfile))
					 (format "--include=%s" incfile))
				       "--allow=[0-9]+"
				       (format "--exclude=%s"
					       (expand-file-name ".+/" flddir))
				       (format "--output-dir=%s" nmzdir)
				       flddir))))
	(unless (file-directory-p nmzdir)
	  (mew-make-directory nmzdir))
	(when remove
	  (mew-nmz-index-delete nmzdir))
	(set-buffer (get-buffer-create bufname))
	(erase-buffer)
	;; folder set
	(and msgenb (message "Mew mknmz (%s)..." fld))
	(mew-nmz-timestamp-new fld)
	(mew-piolet
	 mew-cs-autoconv mew-cs-text-for-write
	 (setq mew-nmz-mknmz-process
	       (apply (function start-process)
		      procname (current-buffer) mew-nmz-prog-mknmz args)))
	(setq mew-nmz-mknmz-process-folder fld)
	(set-process-sentinel mew-nmz-mknmz-process 'mew-nmz-mknmz-sentinel)
	(when (and mew-nmz-mknmz-use-mode-line
		   fld (get-buffer fld) (buffer-name (get-buffer fld)))
	  (save-excursion
	    (set-buffer (get-buffer fld))
	    (setq mode-line-buffer-identification mew-nmz-line-id)
	    (set-buffer-modified-p nil)))))
      (when (and continue all mew-nmz-mknmz-all-folders)
	(mew-nmz-mknmz-continue-with-timer)))))

(defun mew-nmz-mknmz-continue-with-timer ()
  (unless mew-nmz-mknmz-continue-timer
    (setq mew-nmz-mknmz-continue-timer
	  (run-at-time mew-nmz-mknmz-timer-interval nil 'mew-nmz-mknmz-continue))))

(defun mew-nmz-mknmz-continue ()
  (when mew-nmz-mknmz-continue-timer
    (cancel-timer mew-nmz-mknmz-continue-timer)
    (setq mew-nmz-mknmz-continue-timer nil))
  (setq mew-nmz-mknmz-all-folders (cdr mew-nmz-mknmz-all-folders))
  (if mew-nmz-mknmz-all-folders
      (mew-nmz-mknmz (car mew-nmz-mknmz-all-folders) 'all)
    (ding)
    (message "All mknmz done")))

(defun mew-nmz-mknmz-sentinel (process event)
  (save-excursion
    (set-buffer (process-buffer process))
    (let* ((fld mew-nmz-mknmz-process-folder)
	   (nmzdir (mew-nmz-expand-folder fld))
	   msg success)
      (goto-char (point-min))
      (if (search-forward-regexp "^ERROR:.*$" nil t)
	  (progn
	    (setq msg (format "Mew mknmz (%s)...%s" fld (match-string 0)))
	    (condition-case nil
		(progn
		  (mew-nmz-index-delete nmzdir 'tmpfiles)
		  (delete-file (expand-file-name "NMZ.lock2" nmzdir))
		  (delete-file (expand-file-name "NMZ.stamp.new" nmzdir)))
	      (error nil)))
	(setq success t)
	(mew-nmz-timestamp-rename fld)
	(setq msg (format "Mew mknmz (%s)...done" fld))
	(when mew-nmz-setup-p
	  (setq fld (mew-nmz-case-folder-normalize (directory-file-name fld)))
	  (unless (mew-nmz-folder-to-nmzdir fld)
	    (setq mew-nmz-fld-index-alist
		  (cons (cons fld nmzdir) mew-nmz-fld-index-alist))
	    (setq mew-nmz-url-fld-alist
		  (cons (cons (mew-expand-folder fld) fld)
			mew-nmz-url-fld-alist)))))
      (setq mew-nmz-mknmz-process nil)
      (setq mew-nmz-mknmz-process-folder nil)
      (when (and mew-nmz-mknmz-use-mode-line
		 fld (get-buffer fld) (buffer-name (get-buffer fld)))
	(save-excursion
	  (set-buffer (get-buffer fld))
	  (setq mode-line-buffer-identification mew-mode-line-id)
	  (set-buffer-modified-p nil)))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer))
      (message "%s" msg)
      (when (and success (mew-nmz-gcnmz-folder-p fld))
	(mew-nmz-gcnmz fld nmzdir))
      (when mew-nmz-mknmz-all-folders
	(mew-nmz-mknmz-continue-with-timer)))))

(defun mew-nmz-gcnmz (&optional fld nmzdir)
  "Garbage collection for mew-nmz."
  (interactive
   (list (directory-file-name
	  (mew-input-folder (mew-sinfo-get-case)
			    (or (mew-sinfo-get-folder) mew-inbox-folder)))
	 nil))
  (unless nmzdir
    (setq nmzdir (mew-nmz-expand-folder fld)))
  (if (and (mew-which-exec mew-nmz-prog-gcnmz)
	   (file-directory-p nmzdir)
	   (file-exists-p (expand-file-name "NMZ.i" nmzdir)))
      (let ((buf (get-buffer-create (concat " *mew gcnmz*" fld)))
	    (procname (concat mew-nmz-prog-gcnmz "-" fld))
	    (args `("--no-backup" ,nmzdir))
	    process)
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer)
	  (setq mew-nmz-mknmz-process-folder fld)
	  (message "Mew gcnmz (%s)..." mew-nmz-mknmz-process-folder)
	  (setq process
		(apply (function start-process)
		       procname (current-buffer) mew-nmz-prog-gcnmz args))
	  (set-process-sentinel process 'mew-nmz-gcnmz-sentinel)
	  (when (and mew-nmz-mknmz-use-mode-line
		     fld (get-buffer fld) (buffer-name (get-buffer fld)))
	    (save-excursion
	      (set-buffer (get-buffer fld))
	      (setq mode-line-buffer-identification mew-nmz-gcnmz-line-id)
	      (set-buffer-modified-p nil)))))
    (when (interactive-p)
      (message "gcnmz cannot run on %s" fld))))

(defun mew-nmz-gcnmz-sentinel (process event)
  (when (buffer-name (process-buffer process))
    (set-buffer (process-buffer process))
    (let ((fld mew-nmz-mknmz-process-folder))
      (if (and fld event (stringp event) (string= event "kill"))
	  (progn
	    (message "Mew gcnmz (%s)...kill from user" fld)
	    (condition-case nil
		(mew-nmz-index-delete (mew-nmz-expand-folder fld) 'tmpfiles)
	      (error nil)))
	(message "Mew gcnmz (%s)...done" fld))
      (when (and mew-nmz-mknmz-use-mode-line
		 fld (get-buffer fld) (buffer-name (get-buffer fld)))
	(save-excursion
	  (set-buffer (get-buffer fld))
	  (setq mode-line-buffer-identification mew-mode-line-id)
	  (set-buffer-modified-p nil)))
      (kill-buffer (current-buffer)))))

(defun mew-nmz-mknmz-kill-process ()
  "Kill the all processes of mknmz."
  (interactive)
  (when mew-nmz-mknmz-continue-timer
    (cancel-timer mew-nmz-mknmz-continue-timer)
    (setq mew-nmz-mknmz-continue-timer nil))
  (let ((proc-list (process-list))
	(regex1 (concat "^" mew-nmz-prog-mknmz "-"))
	(regex2 (concat "^" mew-nmz-prog-gcnmz "-"))
	process buf kill)
    (while (setq process (car proc-list))
      (setq proc-list (cdr proc-list))
      (cond
       ((string-match regex1 (process-name process))
	;; mknmz
	(setq buf (process-buffer process))
	(when (buffer-name buf)
	  (save-excursion
	    (set-buffer buf)
	    (set-process-sentinel process 'ignore)
	    (goto-char (point-max))
	    (insert "\nERROR: Kill from user.\n")
	    (kill-process process)
	    (mew-nmz-mknmz-sentinel process "kill")
	    (setq kill t))))
       ((string-match regex2 (process-name process))
	;; gcnmz
	(setq buf (process-buffer process))
	(when (buffer-name buf)
	  (set-process-sentinel process 'ignore)
	  (kill-process process)
	  (mew-nmz-gcnmz-sentinel process "kill")
	  (setq kill t)))))
    (setq mew-nmz-mknmz-all-folders nil)
    (when (interactive-p)
      (if kill
	  (message "All process of mknmz killed")
	(message "No process of mknmz")))))

(defun mew-nmz-mknmz-get-all-folders ()
  (let ((protos (delq mew-folder-virtual (copy-sequence mew-folder-prefixes)))
	(allcases (if (null mew-config-alist)
		      '("")
		    (mapcar (lambda (x) (car x)) mew-config-alist)))
	flist case proto cases donecases flds fld dir)
    (message "mew-nmz getting all folders...")
    (while (setq proto (car protos))
      (setq protos (cdr protos))
      (setq flds nil)
      (setq cases allcases)
      (setq donecases nil)
      (while (setq case (car cases))
	(setq cases (cdr cases))
	(if (or (string= case mew-case-default)
		(string= case ""))
	    (setq case (mew-nmz-case-normalize proto))
	  (setq case (mew-nmz-case-normalize
		      (concat case ":" proto))))
	(unless (member case donecases)
	  (setq donecases (cons case donecases))
	  (setq flds
		(cond
		 ((mew-folder-imapp proto)
		  (mapcar (lambda (x) (car x))
			  (mew-imap-folder-alist case)))
		 ((mew-folder-nntpp proto)
		  (mapcar (lambda (x) (car x))
			  (mew-nntp-folder-alist case)))
		 ((mew-folder-popp proto)
		  (mapcar (lambda (x) (car x))
			  (mew-pop-folder-alist)))
		 (t
		  (mapcar (lambda (x) (car x))
			  (mew-local-folder-alist)))))
	  (setq case (if (string= case "")
			 ""
		       (concat case ":")))
	  (while (setq fld (car flds))
	    (setq flds (cdr flds))
	    (setq fld (concat case fld))
	    (setq dir (mew-expand-folder fld))
	    (when (and dir
		       (file-exists-p dir)
		       (file-directory-p dir)
		       (file-exists-p (expand-file-name mew-summary-touch-file dir)))
	      (setq flist (cons (directory-file-name fld) flist)))))))
    (prog1
	(setq flist (nreverse flist))
      (with-temp-buffer
	(while (setq fld (car flist))
	  (unless (mew-nmz-skip-folder-p fld)
	    (insert (format "%s\t%s\t%s\n"
			    fld
			    (mew-expand-folder fld)
			    (mew-nmz-expand-folder fld))))
	  (setq flist (cdr flist)))
	(mew-frwlet
	 mew-cs-text-for-read mew-nmz-mknmz-index-file-coding-system
	 (write-region (point-min) (point-max)
		       (expand-file-name mew-nmz-mknmz-index-file mew-conf-path)
		       nil 'nomsg)))
      (message "mew-nmz getting all folders...done"))))

(defun mew-nmz-mknmz-all-folders (&optional args)
  "Make namazu index all folders."
  (interactive "P")
  (when (or (null mew-nmz-mknmz-all-folders)
	    (and mew-nmz-mknmz-all-folders
		 (prog1
		     (y-or-n-p "Another mew-nmz-mknmz-all-folders() detect. Kill it? ")
		   (mew-nmz-mknmz-kill-process))))
    (when (y-or-n-p (format "Make index in all %s? "
			    (if args "folders" "indexed folders")))
      (mew-nmz-setup)
      (let (alist flist)
	(if args
	    ;; all exist folder
	    (setq flist (mew-nmz-mknmz-get-all-folders))
	  ;; all indexed folder
	  (setq alist mew-nmz-fld-index-alist)
	  (while alist
	    (setq flist (cons (car (car alist)) flist))
	    (setq alist (cdr alist))))
	;; for mew-nmz-mknmz()
	(setq current-prefix-arg nil)
	(setq mew-nmz-mknmz-all-folders flist)
	(when flist
	  (mew-nmz-mknmz (car flist) 'all))))))

(defun mew-nmz-mknmz-save-mewmknmz ()
  "Save the information for mknmz."
  (interactive)
  ;; (mew-nmz-clean-up)
  (mew-nmz-setup)
  (mew-nmz-mknmz-get-all-folders))

(defun mew-nmz-mark-unindexed ()
  "Mark unindexed messages."
  (interactive)
  (mew-summary-only
   (if (mew-summary-exclusive-p)
       (save-excursion
	 (if (and (mew-summary-mark-collect
		   mew-nmz-mark-unindexed (point-min) (point-max))
		  (y-or-n-p (format "Unmark '%c'? " mew-nmz-mark-unindexed)))
	     (mew-mark-undo-mark mew-nmz-mark-unindexed 'nomsg))
	 (let* ((ufname
		 (expand-file-name "NMZ.field.uri"
				   (mew-nmz-expand-folder (buffer-name))))
		(mmsgs 0)
		(umsgs 0)
		msgnums)
	   (if (not (file-exists-p ufname))
	       (message "%s has no index file" (buffer-name))
	     (with-temp-buffer
	       (message "checking %s..." (file-name-nondirectory ufname))
	       (insert-file-contents ufname)
	       (while (re-search-forward "/\\([0-9]+\\)$" nil t)
		 (setq msgnums (cons (string-to-number (match-string 1)) msgnums))))
	     (message "checking %s..." (buffer-name))
	     (goto-char (point-min))
	     (while (not (eobp))
	       (if (and (mew-sumsyn-match mew-regex-sumsyn-short)
			(not (memq (string-to-number (mew-sumsyn-message-number)) msgnums))
			(not (mew-in-decode-syntax-p)))
		   (progn
		     (setq umsgs (1+ umsgs))
		     (when (mew-summary-markable)
		       (mew-summary-mark-as mew-nmz-mark-unindexed)
		       (setq mmsgs (1+ mmsgs)))))
	       (forward-line))
	     (cond
	      ((= umsgs 1)
	       (message "%d message does not have index, %d marked"
			umsgs mmsgs))
	      ((> umsgs 1)
	       (message "%d messages do not have index, %d marked"
			umsgs mmsgs))
	      (t
	       (message "all messages have index")))))))))

;;
;; "search Message-ID" functions.
(defun mew-nmz-search-parent (&optional child mid)
  "Search *parent* message and jump to that.
If executed with '\\[universal-argument]', search *child* message."
  (interactive "P")
  (when (memq major-mode '(mew-summary-mode mew-virtual-mode))
    (mew-summary-goto-message))
  (let ((fld (mew-summary-folder-name))
	(msg (mew-summary-message-number))
	(idh (list (list mew-in-reply-to: mew-references:)
		   (list mew-message-id:)))
	(message (if child "children" "parent"))
	(refilefld (copy-sequence mew-nmz-search-parent-folder))
	(proto (or (mew-proto-to-refile (or (mew-sinfo-get-folder)
					    (mew-minfo-get-summary)
					    "+"))
		   "+"))
	(case (mew-sinfo-get-case))
	refiledir mess ref pid rh pos killbuff)
    (if mid
	(setq pid (list mid) idh nil)
      (if (not (or msg (mew-syntax-number)))
	  (message "No message here")
	(save-excursion
	  (mew-nmz-setup)
	  (mew-summary-display nil)
	  (if (setq mess (mew-cache-hit fld msg))
	      (set-buffer mess)
	    (setq mess (generate-new-buffer mew-buffer-prefix))
	    (setq killbuff t)
	    (set-buffer mess)
	    (mew-erase-buffer)
	    (mew-insert-message
	     fld msg mew-cs-text-for-read mew-header-reasonable-size))
	  (let ((mew-inherit-refile-proto proto)
		(mew-inherit-refile-case case))
	    (setq refilefld (append (car (mew-refile-guess nil t)) refilefld)))
	  (if child
	      (setq idh (car (cdr idh)))
	    (setq idh (car idh)))
	  (while idh
	    (setq rh (car idh))
	    (setq ref (mew-header-get-value rh))
	    (while (and ref (string-match "<\\([^>]+\\)>" ref))
	      (setq pid (cons (concat "\"" (match-string 1 ref) "\"") pid))
	      (setq refilefld
		    (cons (nth 1 (assoc (car pid) mew-refile-msgid-alist)) refilefld))
	      (setq ref (substring ref (match-end 0))))
	    (setq idh (cdr idh)))
	  (setq refilefld (cons fld refilefld))
	  (setq refilefld (mew-uniq-list (delete nil refilefld)))
	  (setq refiledir
		(delete nil (mapcar
			     (lambda (x)
			       (mew-nmz-expand-folder x))
			     refilefld))))))
    (when killbuff (mew-kill-buffer mess))
    (if (null pid)
	(message "No required header")
      (if (mew-syntax-number)
	  (while (not (mew-summary-message-number))
	    (forward-line -1)))
      (set-marker mew-summary-inbox-position (point) (current-buffer))
      (let ((pattern1 "")
	    (pattern2 "")
	    (addpattern (if child "+in-reply-to:" "+message-id:"))
	    (range nil))
	(if (not child)
	    (setq pattern1 (concat addpattern (car pid)))
	  (setq pattern1 (concat addpattern (car pid)))
	  (setq addpattern "+references:")
	  (setq pattern1 (concat pattern1 " | " addpattern (car pid))))
	(setq pid (delete (car pid) pid))
	(while pid
	  (if (> (length (concat pattern2 addpattern (car pid)))
		 mew-nmz-query-max-length)
	      (setq pid nil)
	    (setq pattern2 (concat pattern2 addpattern (car pid)))
	    (setq addpattern (if child " | +references:" " | +message-id:"))
	    (setq pid (delete (car pid) pid))))
	(message "Searching %s..." message)
	(let ((pattern (list pattern1 pattern2)))
	  (while (and (null range) pattern)
	    (if mid
		()
	      (message "Searching %s...%s" message (mew-join ", " refilefld))
	      (setq range (mew-nmz-multi-pick refiledir (car pattern)))
	      (when range
		(catch 'detect
		  (while refilefld
		    (if (null (setq idh (assoc (car refilefld) range)))
			()
		      (setq fld (car idh))
		      (if child
			  (setq range (cdr idh))
			(setq range (nreverse (cdr idh))))
		      (throw 'detect t))
		    (setq refilefld (cdr refilefld)))
		  nil)))
	    (unless range
	      ;; all folder search
	      (message "Searching %s...all folders" message)
	      (setq range (mew-nmz-multi-pick
			   (mew-nmz-expand-folder-regexp "*:*")
			   (car pattern) 'catch))
	      (if (null range)
		  (setq pattern (cdr pattern))
		(setq fld (car (car range)))
		(setq range (cdr (car range)))
		(if (not child) (setq range (nreverse range)))
		))))
	(if (null range)
	    (message "No message found")
	  (when (mew-virtual-p)
	    (save-excursion
	      (goto-char (point-min))
	      (when (re-search-forward
		     (concat "\r \\(" (regexp-quote fld) "\\)? +" (car range) " ") nil t)
		(setq fld (buffer-name))
		(goto-char (match-beginning 0))
		(beginning-of-line)
		(setq pos (point)))))
	  (if (listp (car range))
	      (setq fld (car (car range))
		    mess (car (cdr (car range))))
	    (setq mess (car range)))
	  (mew-nmz-goto-folder-msg fld mess pos)
	  (message "Searching %s...%s/%s" message fld mess))))))

(defun mew-nmz-search-child (&optional arg)
  (interactive "P")
  (mew-nmz-search-parent (not arg)))

(defun mew-nmz-search-msgid-at-point ()
  (interactive)
  (let (start end)
    (if (and (re-search-backward "<" (save-excursion (beginning-of-line) (point)) t)
	     (setq start (point))
	     (re-search-forward ">" (save-excursion (end-of-line) (point)) t)
	     (setq end (point)))
	(mew-nmz-search-msgid (buffer-substring start end))
      (message "No Message-ID"))))

(defun mew-nmz-search-msgid-region (start end)
  (interactive "r")
  (mew-nmz-search-msgid (buffer-substring start end)))

(defun mew-nmz-search-msgid (mid)
  (interactive "sMessage-ID: ")
  (if (string-match "<\\([^>]+\\)>" mid)
      (let ((mew-use-full-window t)
	    (pattern (concat "\"" (mew-match-string 1 mid) "\"")))
	(when (eq major-mode 'mew-message-mode)
	  (mew-message-goto-summary))
	(mew-nmz-search-parent nil pattern))
    (message "No Message-ID")))

;;
;; "Search" functions.
(defun mew-nmz-search-mark ()
  "Namazu pick messages according to a pick pattern which you input,
then put the '*' mark onto them. "
  (interactive)
  (mew-pickable
   (let ((fld (mew-summary-folder-name))
	 (preline 0)
	 pattern msgs msgsback threadmsgs total linenum msgtotal)
     (cond
      ((eq (point-min) (point-max))
       (message "No messages in this buffer"))
      ((not (mew-nmz-indexed-folder-p fld))
       (message "No index in this folder"))
      (t
       (mew-nmz-setup)
       (mew-sinfo-set-find-key nil) ;; force to ask a user
       (setq pattern (mew-nmz-input-pick-pattern))
       (message "Namazu picking messages in %s..." fld)
       (save-excursion
	 (setq msgs (cdr (car (mew-nmz-multi-pick
			       (list (mew-nmz-expand-folder fld)) pattern))))
	 (message "Namazu picking messages in %s...done" fld)
	 (if (null msgs)
	     (message "No message to be marked")
	   (setq msgsback msgs)
	   (setq msgtotal (length msgs))
	   (if (= msgtotal 1)
	       (message "Marking 1 message...")
	     (message "Marking %d messages..." msgtotal))
	   (cond
	    ((mew-thread-p)
	     ;; Thread folder
	     (setq threadmsgs (mew-summary-thread-get-msglst
			       (mew-vinfo-get-top) 'separator))
	     (setq total (length threadmsgs))
	     (goto-char (point-min))
	     (while (and msgs (not (eobp)))
	       (setq linenum (member (car msgs) threadmsgs))
	       (if (null linenum)
		   (setq msgsback (delete (car msgs) msgsback))
		 (setq linenum (- total (length linenum)))
		 (forward-line (- linenum preline))
		 (setq preline linenum)
		 (mew-summary-mark-as mew-mark-review))
	       (setq msgs (cdr msgs)))
	     (set-buffer-modified-p nil))
	    ((mew-virtual-p)
	     ;; Virtual mode
	     (goto-char (point-min))
	     (setq msgsback nil)
	     (while (and msgs (not (eobp)))
	       (when (re-search-forward (mew-regex-sumsyn-msg (car msgs)) nil t)
		 (setq msgsback (cons (car msgs) msgsback))
		 (mew-summary-mark-as mew-mark-review)
		 (forward-line))
	       (setq msgs (cdr msgs)))
	     (setq msgsback (nreverse msgsback))
	     (set-buffer-modified-p nil)))
	   ;; Physical folder
	   (when (get-buffer fld)
	     (set-buffer fld)
	     (save-excursion
	       (goto-char (point-min))
	       (setq msgs msgsback)
	       (while (and msgs (not (eobp)))
		 (when (mew-nmz-re-search-message (car msgs))
		   (mew-summary-mark-as mew-mark-review)
		   (forward-line))
		 (setq msgs (cdr msgs)))
	       (set-buffer-modified-p nil)))
	   (if (= msgtotal 1)
	       (message "Marking 1 message...done")
	     (message "Marking %d messages...done" msgtotal)))))))))

;;
;; "Namazu virtual" function.
(defun mew-nmz-input-folders ()
  (mew-input-clear)
  (mew-input-folder-clean-up)
  (let ((map (copy-keymap mew-input-folder-map))
	(case:folder (concat (or (mew-summary-folder-name) "+")
			     (if mew-nmz-input-folders-asterisk "*" ""))))
    (define-key map "*" 'mew-input-folder-self-insert)
    (let* ((init (mew-canonicalize-case-folder case:folder))
	   (mew-input-complete-function 'mew-complete-folder)
	   (mew-circular-complete-function 'mew-circular-complete-case:)
	   (mew-input-folder-search-multi t)
	   ;; mew-inherit-case must be nil
	   (ret (read-from-minibuffer "Namazu folder name: "
				      init map nil
				      'mew-nmz-input-folder-hist)))
      (when (string= ret "")
	(setq ret init))
      (setq ret (mapcar 'mew-chop (mew-split ret ?,))))))

(defun mew-nmz-virtual ()
  "Another virtual mode with namazu."
  (interactive)
  (if (not (mew-summary-or-virtual-p))
      (message "This command cannot be used in this mode")
    (let* ((flds (mew-nmz-input-folders))
	   (pattern (mew-nmz-input-pick-pattern (mapconcat 'identity flds ",")))
	   (buf (generate-new-buffer mew-buffer-prefix))
	   (file (mew-make-temp-name))
	   vfld nmzdirs fldmsgs fld msgs scans func lra pickflds path opts)
      (mew-nmz-setup)
      (setq nmzdirs (mew-nmz-flds-to-indexs flds))
      (if (null nmzdirs)
	  (message "Please make namazu index")
	(setq fldmsgs (mew-nmz-multi-pick nmzdirs pattern nil))
	(if (null fldmsgs)
	    (message "No message pick")
	  (while fldmsgs
	    (setq pickflds (cons (car (car fldmsgs)) pickflds))
	    (setq scans (cons (car fldmsgs) scans))
	    (setq fldmsgs (cdr fldmsgs)))
	  (setq scans (nreverse scans))
	  (setq pickflds (nreverse pickflds))
	  ;;
	  (setq vfld (mew-summary-get-vfolder pickflds))
	  (unless (or (= (length pickflds) 1)
		      (< (length vfld) 25))
	    (setq vfld (concat
			(substring vfld 0 24)
			"...*")))
	  (mew-summary-switch-to-folder vfld)
	  (when (mew-summary-exclusive-p)
	    (mew-erase-buffer)
	    (mew-vinfo-set-flds pickflds)
	    (set-buffer buf)
	    (mew-erase-buffer)
	    (while (setq fld (car (car scans)))
	      (setq path (mew-path-to-folder (mew-expand-folder fld)))
	      (setq lra (cons (cons path fld) lra))
	      (insert "CD: " path "\n")
	      (setq msgs (cdr (car scans)))
	      (while msgs
		(insert (car msgs) "\n")
		(setq msgs (cdr msgs)))
	      (setq scans (cdr scans)))
	    (mew-frwlet
	     mew-cs-text-for-read mew-cs-text-for-write
	     (write-region (point-min) (point-max) file nil 'nomsg))
	    (setq scans t))
	  (mew-remove-buffer buf)
	  (mew-summary-switch-to-folder vfld)
	  (when scans
	    (setq func `(lambda () (mew-delete-file ,file)))
	    (setq opts (list "-i" file))
	    (mew-local-retrieve 'vir opts func lra t)))))))

;;
;; Use namazu-mode.
;;
;; (setq w3m-namazu-arguments
;;       '("-r" "-h" "-H" "-n" w3m-namazu-page-max "-w" whence))

(defun mew-nmz-namazu (&optional pattern indexs)
  "Execute w3m-namazu.
If executed with '\\[universal-argument]', search result indexes."
  (interactive)
  (if (not (or (featurep 'w3m-namazu)
	       (condition-case nil
		   (require 'w3m-namazu)
		 (file-error nil))))
      (message "Please install \"emacs-w3m\"")
    (mew-nmz-setup)
    (if current-prefix-arg
	;; rest indexes
	(let (current-prefix-arg)
	  (mew-nmz-namazu mew-nmz-namazu-pattern mew-nmz-namazu-miss-folders))
      (let ((i mew-nmz-db-max)
	    flds nmzdirs fldmsgs pickflds overmsg)
	(setq flds (or indexs (mew-nmz-input-folders)))
	(setq mew-nmz-namazu-pattern
	      (setq pattern
		    (or pattern
			(mew-nmz-input-pick-pattern (mapconcat 'identity flds ",")))))
	(setq mew-nmz-namazu-miss-folders nil)
	(setq nmzdirs (mew-nmz-flds-to-indexs flds))
	(when (> (length nmzdirs) mew-nmz-db-max)
	  (setq fldmsgs (mew-nmz-multi-pick nmzdirs pattern))
	  (when fldmsgs
	    (while fldmsgs
	      (setq pickflds (cons (car (car fldmsgs)) pickflds))
	      (setq fldmsgs (cdr fldmsgs)))
	    (when (> (length pickflds) mew-nmz-db-max)
	      (setq mew-nmz-namazu-miss-folders
		    (nthcdr mew-nmz-db-max (nreverse pickflds)))
	      (setq overmsg (format "Warning: %d indexes over"
				    (length mew-nmz-namazu-miss-folders))))
	    (setq nmzdirs nil)
	    (while (and pickflds (> i 0))
	      (setq nmzdirs (cons (mew-nmz-expand-folder (car pickflds))
				  nmzdirs))
	      (setq pickflds (cdr pickflds))
	      (setq i (1- i)))))
	;; message viewer set
	(unless (assoc mew-nmz-namazu-content-type w3m-content-type-alist)
	  (setq w3m-content-type-alist
		(cons `(,mew-nmz-namazu-content-type "/[0-9]+$" nil "text/plain")
		      w3m-content-type-alist)))
	(add-hook 'w3m-display-hook 'w3m-mew-message-view t)
	;; remove pre mew-nmz-namazu's alist
	(when (assoc mew-nmz-namazu-index-alias w3m-namazu-index-alist)
	  (setq w3m-namazu-index-alist
		(delete (assoc mew-nmz-namazu-index-alias w3m-namazu-index-alist)
			w3m-namazu-index-alist)))
	;; for next page
	(setq w3m-namazu-index-alist
	      (append (list (cons mew-nmz-namazu-index-alias nmzdirs))
		      w3m-namazu-index-alist))
	(w3m-namazu mew-nmz-namazu-index-alias pattern 'reload)
	(when overmsg (message overmsg))))))

(defun w3m-mew-message-view (url)
  (if (and (boundp 'mew-mail-path)
	   (w3m-url-local-p url)
	   (string-match (concat (file-name-as-directory
				  (expand-file-name mew-mail-path))
				 ".+/[0-9]+$")
			 (expand-file-name (w3m-url-to-file-name url))))
      (unless (get-text-property (point-min) 'w3m-mew-namazu)
	(mew-elet
	 (goto-char (point-min))
	 (condition-case nil
	     (let (pos)
	       (mew-decode-rfc822-header)
	       (mew-header-goto-end)
	       (mew-header-arrange (point-min) (point))
	       (mew-highlight-body-region (point) (point-max))
	       ;; for emacs-w3m
	       (remove-text-properties (point-min) (point-max)
				       '(read-only nil))
	       (setq pos (if (get-text-property (point-min) 'mew-visible)
			     (point-min)
			   (or (next-single-property-change (point-min) 'mew-visible)
			       (point-min))))
	       (set-window-start (selected-window) pos))
	   (error nil))
	 (put-text-property (point-min) (point-min) 'w3m-mew-namazu t)
	 (set-buffer-modified-p nil)))
    (remove-text-properties (point-min) (point-min) '(w3m-mew-namazu nil))))

;;
;; Input "Namazu pattern" functions.
(defun mew-nmz-input-pick-pattern (&optional fld-str)
  "Input mew-nmz pick pattern."
  (mew-input-clear)
  (let ((mew-nmz-gather-header-list (mew-nmz-pick-pattern-gather-header)))
    (setq mew-input-complete-function (function mew-nmz-pick-pattern))
    (let ((keymap (copy-keymap mew-input-map)) pattern)
      (define-key keymap " " nil)
      (setq pattern (read-from-minibuffer
		     (if fld-str
			 (format "Namazu pattern for '%s': " fld-str)
		       "Namazu pattern: ")
		     mew-nmz-pick-default-field
		     keymap
		     nil
		     'mew-nmz-pick-pattern-hist))
      (mew-decode-syntax-delete)
      ;; for M-n, M-p
      (setq mew-input-pick-pattern-hist
	    (cons pattern mew-input-pick-pattern-hist))
      pattern)))

(defun mew-nmz-pick-pattern-gather-header ()
  (when mew-nmz-pick-gather-field-list
    (save-excursion
      (let* ((fld (mew-summary-folder-name))
	     (msg (mew-summary-message-number))
	     (buf (mew-cache-hit fld msg))
	     (gathers mew-nmz-pick-gather-field-list)
	     killbuff retlst gather header duplchk mid addrs addr prefix)
	(when (and (not buf) fld msg)
	  (setq buf (generate-new-buffer mew-buffer-prefix))
	  (setq killbuff t)
	  (set-buffer buf)
	  (mew-erase-buffer)
	  (mew-insert-message
	   fld msg mew-cs-text-for-read mew-header-reasonable-size))
	(when (and buf (get-buffer buf) (buffer-name (get-buffer buf)))
	  (set-buffer buf)
	  (while gathers
	    (setq gather (car gathers))
	    (setq header (mew-header-get-value (car gather)))
	    (when (and header (car (cdr gather)))
	      (cond
	       ((eq (car (cdr gather)) 'msgid)
		(while (and header (string-match "<\\([^>]+\\)>" header))
		  (setq mid (match-string 1 header))
		  (setq header (substring header (match-end 0)))
		  (if (member mid duplchk)
		      ()
		    (setq prefix (nthcdr 2 gather))
		    (setq duplchk (cons mid duplchk))
		    (while prefix
		      (setq retlst (cons (concat (car prefix) mid) retlst))
		      (setq prefix (cdr prefix))))))
	       ((eq (car (cdr gather)) 'address)
		(setq addrs (mew-addrstr-parse-address-list header))
		(while (setq addr (car addrs))
		  (setq addr (downcase addr))
		  (if (not (member addr duplchk))
		      (let ((prefix (nthcdr 2 gather)))
			(setq duplchk (cons addr duplchk))
			(while prefix
			  (setq retlst (cons (concat (car prefix) addr) retlst))
			  (setq prefix (cdr prefix)))))
		  (setq addrs (cdr addrs))))))
	    (setq gathers (cdr gathers)))
	  (when killbuff (mew-kill-buffer buf))
	  (when retlst
	    (setq retlst (append
			  retlst
			  (list
			   (concat " " (make-string (- (window-width) 10) ?-))))))
	  (nreverse retlst))))))

(defun mew-nmz-pick-pattern ()
  (let* ((pat (mew-delete-pattern))
	 (clist (append mew-nmz-pick-field-list
			mew-nmz-gather-header-list)))
    (if (null pat)
	(mew-complete-window-show clist)
      (mew-complete pat
		    (mapcar (function list) clist)
		    "Namazu pick pattern "
		    nil))))

;;
;; "Namazu search engine" functions.
(defun mew-nmz-multi-pick (nmzdirs pattern &optional catch)
  "Get message numbers with many folders."
  (let ((tmpdirs nmzdirs)
	(defaultregex
	  (concat "^" (regexp-opt
		       (delq mew-folder-virtual (copy-sequence mew-folder-prefixes)))))
	nxt prog-args intmsgs retmsgs sortfld defmsgs casemsgs cell)
    (setq pattern (mew-cs-encode-arg pattern))
    (setq nmzdirs nil)
    (while tmpdirs
      (setq nxt (nthcdr mew-nmz-db-max tmpdirs))
      (if nxt (setcdr (nthcdr (1- mew-nmz-db-max) tmpdirs) nil))
      (setq nmzdirs (cons tmpdirs nmzdirs))
      (setq tmpdirs nxt))
    (setq nmzdirs (nreverse nmzdirs))
    (with-temp-buffer
      (while (and nmzdirs
		  (or (not catch)
		      (and catch (null intmsgs))))
	(setq prog-args (delq nil (append mew-nmz-prog-args
					  (list "--all" "--list" "--no-decode-uri")
					  (list pattern)
					  (car nmzdirs))))
	(erase-buffer)
	(mew-piolet
	 mew-cs-text-for-read mew-cs-text-for-write
	 (let ((file-name-coding-system nil))
	   (apply (function call-process)
		  mew-nmz-prog nil t nil prog-args)))
	(goto-char (point-min))
	(let (dir msgnum)
	  (while (not (eobp))
	    (when (looking-at mew-nmz-result-regex)
	      (setq dir (mew-buffer-substring (match-beginning 1) (match-end 1)))
	      (setq msgnum (string-to-int
			    (mew-buffer-substring (match-beginning 2) (match-end 2))))
	      (if (not (setq cell (assoc dir intmsgs)))
		  (setq intmsgs (cons (list dir (list msgnum)) intmsgs))
		(unless (memq msgnum (car (cdr cell)))
		  (nconc (car (cdr cell)) (list msgnum)))))
	    (forward-line))
	  (setq nmzdirs (cdr nmzdirs))))
      (when intmsgs
	(setq retmsgs intmsgs)
	(while retmsgs
	  (setq sortfld (cons (car (car retmsgs)) sortfld))
	  (setq retmsgs (cdr retmsgs)))
	;; no sort
	;; (setq sortfld (sort sortfld 'string<))
	(while sortfld
	  (setq cell (assoc (car sortfld) intmsgs))
	  (setq retmsgs
		(cons
		 (cons (mew-nmz-url-to-folder (car cell))
		       (mapcar 'int-to-string
			       (sort (car (cdr cell)) '<)))
		 retmsgs))
	  (setq sortfld (cdr sortfld)))
	;; '((folder msg ...) (folder msg ...) ...)
	(while retmsgs
	  (if (string-match defaultregex (car (car retmsgs)))
	      (setq defmsgs (cons (car retmsgs) defmsgs))
	    (setq casemsgs (cons (car retmsgs) casemsgs)))
	  (setq retmsgs (cdr retmsgs)))
	(append defmsgs casemsgs)))))

;;
;; miscellaneous functions
(defun mew-nmz-flds-to-indexs (flds)
  (let ((suffix (append
		 '("*")
		 (delq mew-folder-virtual (copy-sequence mew-folder-prefixes))))
	nmzdirs fld tmp)
    (setq suffix (concat (regexp-opt suffix) "$"))
    (while (setq fld (car flds))
      (setq fld (mew-nmz-case-folder-normalize (directory-file-name fld)))
      (cond
       ((or (string-match suffix fld)
	    (string-match "^\\*" fld))
	(setq nmzdirs (append nmzdirs
			      (mew-nmz-expand-folder-regexp fld))))
       ((setq tmp (mew-nmz-folder-to-nmzdir fld))
	(setq nmzdirs (cons tmp nmzdirs))))
      (setq flds (cdr flds)))
    (nreverse (mew-uniq-list nmzdirs))))

(defun mew-nmz-expand-folder-regexp (case:folder)
  (mew-nmz-setup)
  (let ((alist mew-nmz-fld-index-alist)
	case fld caseregex nmzdirs tmpfld nmzdir protos newcase)
    (if (string= case:folder "*:*")
	;; all case, all proto, all folder
	(while alist
	  (setq nmzdirs (cons (cdr (car alist)) nmzdirs))
	  (setq alist (cdr alist)))
      (if (string-match "^\\([^:]+\\):\\(.*\\)$" case:folder)
	  (setq case (match-string 1 case:folder)
		fld (match-string 2  case:folder))
	(setq case "")
	(setq fld case:folder))
      (when (string-match "^.*\\*$" fld)
	(setq fld (substring fld 0 -1))
	(setq fld (directory-file-name fld)))
      (cond
       ((string= case "*")
	;; all case
	(setq caseregex "^\\([^:]+:\\)?"))
       ((or (string= case "") (string= mew-case-default case))
	;; default case
	(setq caseregex "^"))
       ((string= fld "")
	(setq protos (delq mew-folder-virtual (copy-sequence mew-folder-prefixes)))
	(setq caseregex
	      (concat "^\\(\\("
		      (mapconcat
		       (lambda (x)
			 (setq newcase (mew-nmz-case-normalize
					(concat case ":" x)))
			 (if (string= newcase "")
			     (regexp-quote x)
			   (regexp-quote (concat case ":" x))))
		       protos "\\)\\|\\(")
		      "\\)\\)")))
       (t
	(setq case (mew-nmz-case-normalize (concat case ":" fld)))
	(if (string= case "")
	    (setq caseregex "^")
	  (setq caseregex (concat "^" (regexp-quote case) ":")))))
      (if (string= fld "")
	  (setq caseregex (concat caseregex "[^:]+$"))
	(setq caseregex (concat caseregex (regexp-quote fld))))
      (while alist
	(setq tmpfld (car (car alist)))
	(setq nmzdir (cdr (car alist)))
	(when (string-match caseregex tmpfld)
	  (setq nmzdirs (cons nmzdir nmzdirs)))
	(setq alist (cdr alist))))
    nmzdirs))

(defun mew-nmz-goto-folder-msg (fld msg &optional pos)
  (mew-summary-visit-folder fld)
  (mew-rendezvous mew-summary-buffer-process)
  (if pos
      (goto-char pos)
    (goto-char (point-min))
    (mew-nmz-re-search-message msg))
  (mew-thread-move-cursor)
  (mew-summary-display 'force))

(defun mew-nmz-index-new-p (fld)
  (let ((touchtime (mew-file-get-time
		    (expand-file-name
		     mew-summary-touch-file
		     (file-chase-links (mew-expand-folder fld)))))
	(stamptime (mew-file-get-time
		    (expand-file-name
		     "NMZ.stamp" (mew-nmz-expand-folder fld)))))
    (cond
     ((null touchtime) nil)
     ((null stamptime) nil)
     ((> (nth 0 stamptime) (nth 0 touchtime)) t)
     ((and (= (nth 0 stamptime) (nth 0 touchtime))
	   (> (nth 1 stamptime) (nth 1 touchtime))) t)
     (t nil))))

(defun mew-nmz-have-no-msgs (dir)
  (setq dir (file-chase-links dir))
  (let* ((mfile (expand-file-name mew-summary-touch-file dir))
	 (t1 (mew-file-get-time mfile))
	 (cache (expand-file-name mew-summary-cache-file dir))
	 (t2 (mew-file-get-time cache)))
    (if (and t1 t2 (or (eq t2 t1)
		       (mew-compare-times t2 t1))
	     (> (mew-file-get-size cache) 0))
	nil
      (null (mew-dir-messages dir)))))

(defun mew-nmz-index-delete (nmzdir &optional tmpfiles)
  "Delete namazu index files."
  (if (file-directory-p nmzdir)
      (let* ((regex (if tmpfiles "^NMZ\..*tmp$" "^NMZ\."))
	     (flist (directory-files nmzdir 'full regex 'nosort))
	     file)
	(while flist
	  (setq file (expand-file-name (car flist) nmzdir))
	  (and (file-exists-p file)
	       (file-writable-p file)
	       (delete-file file))
	  (setq flist (cdr flist))))))

(defun mew-nmz-skip-folder-p (fld)
  (let ((skips mew-nmz-mknmz-skip-folders-regexp))
    (catch 'match
      (setq fld (mew-nmz-case-folder-normalize fld))
      (while skips
	(when (string-match (car skips) fld)
	  (throw 'match t))
	(setq skips (cdr skips)))
      nil)))

(defun mew-nmz-gcnmz-folder-p (fld)
  (let ((regexps mew-nmz-use-gcnmz-folders-regexp))
    (catch 'match
      (setq fld (mew-nmz-case-folder-normalize fld))
      (while regexps
	(when (string-match (car regexps) fld)
	  (throw 'match t))
	(setq regexps (cdr regexps)))
      nil)))

(defun mew-nmz-timestamp-new (fld)
  (let ((file (expand-file-name "NMZ.stamp.new" (mew-nmz-expand-folder fld))))
    (if (file-writable-p file)
	(write-region "touched by Mew." nil file nil 'no-msg))))

(defun mew-nmz-timestamp-rename (fld)
  (let ((nfile (expand-file-name "NMZ.stamp.new" (mew-nmz-expand-folder fld)))
	(tfile (expand-file-name "NMZ.stamp" (mew-nmz-expand-folder fld))))
    (if (and (file-readable-p nfile) (file-writable-p tfile))
	(rename-file nfile tfile 'ok)
      (if (file-writable-p nfile)
	  (delete-file nfile)))))

(defun mew-nmz-re-search-message (msg)
  (let ((here (point)))
    (if (not (re-search-forward (concat "^.*\r +" msg "[^0-9]") nil t))
	(progn (goto-char here)
	       nil)
      (beginning-of-line)
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; index delete/rename
;;;

;; call from mew-summary-delete-folder()
;; case:folder MUST not branch
(defun mew-nmz-folder-index-delete (case:folder)
  (let ((nmzdir (mew-nmz-expand-folder case:folder))
	(fld (mew-nmz-case-folder-normalize (directory-file-name case:folder))))
    (when (and (file-exists-p nmzdir) (file-directory-p nmzdir))
      (mew-delete-directory-recursively nmzdir)
      (setq mew-nmz-fld-index-alist
	    (delete (assoc fld mew-nmz-fld-index-alist) mew-nmz-fld-index-alist))
      (setq mew-nmz-url-fld-alist
	    (delete (assoc (mew-expand-folder fld) mew-nmz-url-fld-alist)
		    mew-nmz-url-fld-alist)))))

;; call from mew-summary-rename-folder()
(defun mew-nmz-folder-index-rename (case:folder case:new-folder)
  (let ((nmzdir (mew-nmz-expand-folder case:folder))
	(new-nmzdir (mew-nmz-expand-folder case:new-folder))
	(dir (file-name-as-directory (mew-expand-folder case:folder)))
	(new-dir (file-name-as-directory (mew-expand-folder case:new-folder))))
    (when (and (file-exists-p nmzdir) (file-directory-p nmzdir)
	       (not (file-exists-p new-nmzdir)))
      (mew-nmz-clean-up)
      (rename-file nmzdir new-nmzdir)
      (when mew-nmz-use-drive-letter
	(when (string-match "^\\([a-zA-Z]\\):\\(/.*\\)" dir)
	  (setq dir (concat "/"
			    (substring dir (match-beginning 1) (match-end 1))
			    "|"
			    (substring dir (match-beginning 2) (match-end 2)))))
	(when (string-match "^\\([a-zA-Z]\\):\\(/.*\\)" new-dir)
	  (setq new-dir (concat "/"
				(substring new-dir (match-beginning 1) (match-end 1))
				"|"
				(substring new-dir (match-beginning 2) (match-end 2))))))
      (mew-nmz-folder-reindex-recursively new-nmzdir dir new-dir))))

(defun mew-nmz-folder-reindex-recursively (dir from to)
  (let ((files (directory-files dir 'full mew-regex-files 'no-sort))
	urifile)
    (when (member (setq urifile (expand-file-name "NMZ.field.uri" dir)) files)
      (mew-nmz-folder-index-reindex dir urifile from to))
    (while files
      (if (file-directory-p (car files))
	  (mew-nmz-folder-reindex-recursively (car files) from to))
      (setq files (cdr files)))))

(defun mew-nmz-folder-index-reindex (dir file from to)
  (setq from (concat "^" (regexp-quote from)))
  (when (and (file-readable-p file) (file-regular-p file))
    (message "mew-nmz: reindexing %s..." dir)
    (with-temp-buffer
      (mew-frwlet
       mew-cs-autoconv mew-nmz-cs-index
       (insert-file-contents file)
       (goto-char (point-min))
       (while (not (eobp))
	 (when (looking-at from)
	   (delete-region (match-beginning 0) (match-end 0))
	   (insert to))
	 (forward-line 1))
       (write-region (point-min) (point-max) file nil 'nomsg)))
    (when (mew-which-exec mew-nmz-prog-rfnmz)
      (call-process mew-nmz-prog-rfnmz nil nil nil dir))
    (message "mew-nmz: reindexing %s...done" dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; mew-nmz-setup
;;;
(defsubst mew-nmz-case-folder-normalize (case:folder)
  (let ((case (mew-case:folder-case case:folder))
	(fld (mew-case:folder-folder case:folder))
	(newcase ""))
    (cond
     ((or (string= fld "")
	  (string= fld "*")
	  (mew-folder-imapp fld))
      (setq newcase (or (cdr (assoc case mew-nmz-imap-case-alist)) "")))
     ((mew-folder-popp fld)
      (setq newcase (or (cdr (assoc case mew-nmz-pop-case-alist)) "")))
     ((mew-folder-nntpp fld)
      (setq newcase (or (cdr (assoc case mew-nmz-nntp-case-alist)) ""))))
    (if (string= newcase "")
	fld
      (concat newcase ":" fld))))

(defsubst mew-nmz-case-normalize (case:folder)
  (let ((case (mew-case:folder-case case:folder))
	(fld (mew-case:folder-folder case:folder))
	(newcase ""))
    (cond
     ((or (string= fld "")
	  (string= fld "*")
	  (mew-folder-imapp fld))
      (setq newcase (or (cdr (assoc case mew-nmz-imap-case-alist)) "")))
     ((mew-folder-popp fld)
      (setq newcase (or (cdr (assoc case mew-nmz-pop-case-alist)) "")))
     ((mew-folder-nntpp fld)
      (setq newcase (or (cdr (assoc case mew-nmz-nntp-case-alist)) ""))))
    newcase))

(defsubst mew-nmz-folder-to-nmzdir (folder)
  (mew-nmz-setup)
  (cdr (assoc (directory-file-name folder) mew-nmz-fld-index-alist)))

(defsubst mew-nmz-url-to-folder (url)
  (mew-nmz-setup)
  (when (and mew-nmz-use-drive-letter
	     (string-match "^/\\([a-zA-Z]\\)|\\(/.*\\)" url))
    (setq url (concat
	       (substring url (match-beginning 1) (match-end 1))
	       ":"
	       (substring url (match-beginning 2) (match-end 2)))))
  (cdr (assoc (expand-file-name url) mew-nmz-url-fld-alist)))

(defsubst mew-nmz-indexed-folder-p (fld)
  (let ((nmzdir (mew-nmz-expand-folder fld)))
    (and nmzdir
	 (file-directory-p nmzdir)
	 (file-exists-p (expand-file-name "NMZ.i" nmzdir)))))

(defun mew-nmz-gather-indexed-folder (case folders-alist &optional nntp)
  (let (fld nmzdir fld-index-alist url-fld-alist)
    (unless case (setq case ""))
    (while (setq fld (car (car folders-alist)))
      (setq fld (directory-file-name (if (string= case "")
					 fld
				       (concat case ":" fld))))
      (when (and (or (not nntp)
		     (file-directory-p (mew-expand-folder fld)))
		 (setq nmzdir (mew-nmz-expand-folder fld))
		 (file-directory-p nmzdir)
		 (file-exists-p (expand-file-name "NMZ.i" nmzdir)))
	(setq fld-index-alist
	      (cons (cons fld nmzdir) fld-index-alist))
	(setq url-fld-alist
	      (cons (cons (mew-expand-folder fld) fld) url-fld-alist)))
      (setq folders-alist (cdr folders-alist)))
    (setq mew-nmz-fld-index-alist (append mew-nmz-fld-index-alist
					  (nreverse fld-index-alist)))
    (setq mew-nmz-url-fld-alist (append mew-nmz-url-fld-alist
					(nreverse url-fld-alist)))))

(defun mew-nmz-setup ()
  (unless mew-nmz-setup-p
    (unless (and (mew-which-exec mew-nmz-prog-mknmz)
		 (mew-which-exec mew-nmz-prog))
      (error "Please install namazu"))
    (message "mew-nmz setup...")
    (mew-nmz-clean-up)
    (let ((prefixes (delq mew-folder-virtual (copy-sequence mew-folder-prefixes)))
	  nmzdir-case-alist
	  case-alist cases case ocase gcase nmzdir alst)
      (while prefixes
	(setq cases (mapcar (lambda (x)
			      (if (string= x mew-case-default) "" x))
			    mew-config-cases))
	(when (member "" cases)
	  (setq cases (cons "" (delete "" cases))))
	(setq nmzdir-case-alist nil)
	(setq case-alist nil)
	(setq gcase nil)
	(while (setq case (car cases))
	  (setq nmzdir (mew-nmz-expand-folder (if (string= case "")
						  (car prefixes)
						(concat case ":" (car prefixes)))))
	  (if (setq alst (assoc nmzdir nmzdir-case-alist))
	      (progn
		(setq ocase (car (assoc (cdr alst) case-alist)))
		(setq case-alist (cons (cons case ocase) case-alist)))
	    (setq nmzdir-case-alist
		  (cons (cons nmzdir case) nmzdir-case-alist))
	    (setq case-alist (cons (cons case case) case-alist))
	    (setq gcase (cons case gcase)))
	  (setq cases (cdr cases)))
	(setq gcase (nreverse gcase))
	(cond
	 ((mew-folder-imapp (car prefixes))
	  (setq mew-nmz-imap-case-alist (nreverse case-alist))
	  (while (setq case (car gcase))
	    (mew-nmz-gather-indexed-folder case (mew-imap-folder-alist case))
	    (setq gcase (cdr gcase))))
	 ((mew-folder-popp (car prefixes))
	  (setq mew-nmz-pop-case-alist (nreverse case-alist))
	  (while (setq case (car gcase))
	    (mew-nmz-gather-indexed-folder case `((,mew-pop-inbox-folder)))
	    (setq gcase (cdr gcase))))
	 ((mew-folder-nntpp (car prefixes))
	  (setq mew-nmz-nntp-case-alist (nreverse case-alist))
	  (while (setq case (car gcase))
	    (mew-nmz-gather-indexed-folder case (mew-nntp-folder-alist case) 'nntp)
	    (setq gcase (cdr gcase))))
	 (t ;; local
	  (mew-nmz-gather-indexed-folder nil (mew-local-folder-alist))))
	(setq prefixes (cdr prefixes)))
      (message "mew-nmz setup...done")
      (setq mew-nmz-setup-p t)
      (run-hooks 'mew-nmz-setup-hook))))

(defun mew-nmz-clean-up ()
  (mew-nmz-mknmz-kill-process)
  (setq mew-nmz-setup-p nil)
  (setq mew-nmz-imap-case-alist nil)
  (setq mew-nmz-nntp-case-alist nil)
  (setq mew-nmz-pop-case-alist nil)
  (setq mew-nmz-fld-index-alist nil)
  (setq mew-nmz-url-fld-alist nil))

(provide 'mew-nmz)

;;; Copyright Notice:

;; Copyright (C) 1999-2005 Hideyuki SHIRAI <shirai@mew.org>
;; Copyright (C) 1994-2005 Mew developing team.
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

;;; mew-nmz.el ends here
