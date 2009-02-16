;;; mew-summary.el --- Summary mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary info
;;;

(defvar mew-sinfo-list
  '(;; parameters to be saved
    "refile"  "mark-hist"
    ;; parameters used internally
    "scan-id" "find-key" "cursor-line" "direction" "start-point"
    "cache-time" "summary-form" "unread-mark" "refile-back"
    "disp-msg" "case" "folder" "proto" "mid-marker" "mid-line" "ttl-line"
    "mark-review"))

(mew-blinfo-defun 'mew-sinfo mew-sinfo-list)

(defvar mew-sinfo-list-save-length 2)
(defvar mew-sinfo-file ".mew-sinfo")

(defun mew-sinfo-save ()
  (when (mew-summary-p)
    (let* ((n mew-sinfo-list-save-length)
	   (data (make-vector n nil))
	   (bnm (mew-summary-folder-name 'ext))
	   (file (mew-expand-folder bnm mew-sinfo-file))
	   (i 0))
      (while (< i n)
	(aset data i (aref mew-sinfo i))
	(setq i (1+ i)))
      (mew-lisp-save file data nil 'unlimit))))

(defun mew-sinfo-load ()
  (when (mew-summary-p)
    (let* ((n mew-sinfo-list-save-length)
	   (bnm (mew-summary-folder-name 'ext))
	   (file (mew-expand-folder bnm mew-sinfo-file))
	   (i 0)
	   data)
      (setq data (mew-lisp-load file))
      (when data
	(while (< i n)
	  (aset mew-sinfo i (aref data i))
	  (setq i (1+ i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros
;;;

(defmacro mew-summary-msg-or-part (&rest body)
  "See if the cursor is on a message or a part."
  `(cond
    ((eobp) (message "No message"))
    ((not (or (mew-summary-message-number) (mew-syntax-number)))
     (message "No message"))
    (t ,@body)))

(defmacro mew-summary-msg (&rest body)
  "See if the cursor is on a message."
  `(cond
    ((eobp) (message "No message"))
    ((not (mew-summary-message-number))
     (message "Please use this command on a message, not a part"))
    (t ,@body)))

(defmacro mew-summary-part (&rest body)
  "See if the cursor is on a part."
  `(cond
    ((eobp) (message "No part"))
    ((not (mew-syntax-number))
     (message "Please use this command on a part, not a message"))
    (t ,@body)))

(defmacro mew-summary-multi-msgs (&rest body)
  "Collect messages marked with '*' and set their corresponding
files to FILES."
  `(let* ((FLD-MSGS (mew-summary-mark-collect2 mew-mark-review))
	  (FLD-MSG-LIST FLD-MSGS) ;; may be used in body
	  FILES ;; may be used in body
	  fld-msg)
     (cond
      ((null FLD-MSGS)
       (message "No %c marks" mew-mark-review))
      (t
       ;; a little bit complicated because of Virtual mode
       (while FLD-MSGS
	 (setq fld-msg (car FLD-MSGS))
	 (setq FLD-MSGS (cdr FLD-MSGS))
	 (setq FILES (cons (mew-expand-folder (car fld-msg) (cdr fld-msg))
			   FILES)))
       (setq FILES (nreverse FILES))
       ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Subfunctions
;;;

(defsubst mew-summary-local-p ()
  (mew-folder-localp (mew-summary-folder-name 'ext)))

(defsubst mew-summary-draft-p ()
  (mew-folder-draftp (mew-summary-folder-name 'ext)))

(defsubst mew-summary-queue-p ()
  (or (mew-folder-queuep (mew-summary-folder-name 'ext))
      (mew-folder-postqp (mew-summary-folder-name 'ext))))

(defsubst mew-summary-pop-p ()
  (mew-folder-popp (mew-summary-folder-name 'ext)))

(defsubst mew-summary-case1 ()
  (and (mew-summary-or-virtual-p) (not (mew-summary-draft-p))))

(defsubst mew-summary-case2 ()
  (and (mew-summary-p) (not (mew-summary-queue-p))))

(defsubst mew-summary-case3 ()
  (and (mew-summary-p) mew-summary-buffer-process))

(defmacro mew-summary-only (&rest body)
  "See if the mode of this buffer is Summary mode.
This macro is used to prohibit using a command in Virtual mode."
  `(cond
    ((not (mew-summary-p))
     (message "This command can be used in Summary mode only"))
    (t ,@body)))

(defmacro mew-virtual-only (&rest body)
  "See if the mode of this buffer is Virtual mode.
This macro is used to prohibit using a command in Summary mode."
  `(cond
    ((not (mew-virtual-p))
     (message "This command can be used in Virtual mode only"))
    (t ,@body)))

(defmacro mew-thread-only (&rest body)
  "See if this buffer is Thread folder.
This macro is used to prohibit using a command in Summary mode."
  `(cond
    ((not (mew-thread-p))
     (message "This command can be used in Thread folder only"))
    (t ,@body)))

(defmacro mew-pickable (&rest body)
  "See if pick can be used for this folder."
  `(cond
    ((not (mew-pickable-p))
     (message "This command cannot be used in this folder"))
    (t ,@body)))

(defmacro mew-summary-not-in-queue (&rest body)
  "See if this folder is not +queue."
  `(cond
    ((not (mew-summary-or-virtual-p))
     (message "This command cannot be used in this mode"))
    ((mew-summary-queue-p)
     (message "This command cannot be used in %s" (mew-summary-folder-name)))
    (t ,@body)))

(defmacro mew-summary-not-in-draft (&rest body)
  "See if this folder is not +draft."
  `(cond
    ((not (mew-summary-or-virtual-p))
     (message "This command cannot be used in this mode"))
    ((mew-summary-draft-p)
     (message "This command cannot be used in %s" (mew-summary-folder-name)))
    (t ,@body)))

(defmacro mew-summary-not-in-nntp (&rest body)
  "See if this folder is not NNTP."
  `(cond
    ((mew-folder-nntpp (mew-sinfo-get-folder))
     (message "This command cannot be used in %s" (mew-summary-folder-name)))
    (t ,@body)))

(defmacro mew-summary-local-or-imap (&rest body)
  "See if this folder is either local or IMAP."
  `(cond
    ((not (mew-summary-or-virtual-p))
     (message "This command cannot be used in this mode"))
    ((or (mew-folder-nntpp (mew-sinfo-get-folder))
	 (mew-folder-popp (mew-sinfo-get-folder)))
     (message "This command cannot be used in %s" (mew-summary-folder-name)))
    (t ,@body))) ;; local or IMAP

(defmacro mew-summary-local-only (&rest body)
  "See if this folder is local."
  `(cond
    ((not (mew-summary-or-virtual-p))
     (message "This command cannot be used in this mode"))
    ((not (mew-summary-local-p))
     (message "This command cannot be used in %s" (mew-summary-folder-name)))
    (t ,@body)))

(defmacro mew-summary-with-mewl (&rest body)
  `(cond
    ((not (mew-which-exec mew-prog-mewl))
     (message "'%s' not found!" mew-prog-mewl))
    (t ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Region
;;;

(defun mew-summary-get-region ()
  "Get a region according to 'mew-summary-region-include-cursor-line'
and return (beg . end)."
  (save-excursion
    (let ((beg (region-beginning))
	  (end (region-end)))
      (goto-char beg)
      (beginning-of-line)
      (setq beg (point))
      (goto-char end)
      (cond
       ((eq mew-summary-region-include-cursor-line t)
	(forward-line))
       ((eq mew-summary-region-include-cursor-line 'end)
	(if (eq (char-after) ?\n)
	    (forward-line)
	  (beginning-of-line)))
       (t
	(if (> (current-column) 0)
	    (forward-line)
	  (beginning-of-line))))
      (cons beg (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dialog
;;;

(defun mew-substitute-for-summary (msg)
  (substitute-command-keys (concat "\\<mew-summary-mode-map>" msg)))

(defun mew-message-for-summary (msg)
  (message "%s" (mew-substitute-for-summary msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modeline
;;;

(defvar mew-mode-line-target "%p")
(defvar mew-mode-line-format
  `("" mew-summary-buffer-left-msgs
    (mew-summary-buffer-raw "*")
    (mew-summary-buffer-secure-process ,mew-secure-format2)))

(defvar mew-mode-line-process
  '((mew-summary-buffer-secure-process mew-secure-format)
    (mew-summary-buffer-process mew-summary-buffer-process-status)))

(defun mew-summary-setup-mode-line ()
  (let ((tgt mew-mode-line-target)
	target prev pos)
    (if (boundp 'mode-line-position)
	(progn
	  (make-local-variable 'mode-line-position) ;; Emacs 21.3.50
	  (setq mode-line-position
		(copy-sequence (default-value 'mode-line-position)))
	  (setq prev mode-line-position))
      (setq mode-line-format (copy-sequence (default-value 'mode-line-format)))
      (setq prev mode-line-format))
    (setq target (or (rassoc (list tgt) prev) ;; Emacs 21.3.50
		     (rassoc tgt prev)
		     (car (member tgt prev))))
    (when target
      (setq pos (- (length prev) (length (member target prev))))
      (setcar (nthcdr pos prev) mew-mode-line-format))
    (when (boundp 'line-number-mode)
      (make-local-variable 'line-number-mode)
      (setq line-number-mode nil))
    (or (assq 'mew-summary-buffer-process mode-line-process)
	(setq mode-line-process
	      (append mew-mode-line-process mode-line-process)))))

(defun mew-summary-reset-mode-line ()
  (setq mew-summary-buffer-left-msgs nil))

(defun mew-summary-mode-name (name)
  (let ((in (if (mew-case-default-p mew-case-input)
		"" mew-case-input))
	(out (if (mew-case-default-p mew-case-output)
		 "" mew-case-output)))
    (if (and (string= in "") (string= out ""))
	(setq mode-name name)
      (if (or (and mew-case-synchronize (string= in out))
              mew-ask-flush-case)
	  (setq mode-name (format "%s %s" name in))
	(setq mode-name (format "%s %s:%s" name in out))))
    (force-mode-line-update)))

(defun mew-summary-mode-line ()
  (unless mew-summary-buffer-process
    (let ((pos (point))
	  (mid-point (marker-position (mew-sinfo-get-mid-marker)))
	  (mid-line (mew-sinfo-get-mid-line))
	  (ttl (mew-sinfo-get-ttl-line))
	  cur left)
      (if (< pos mid-point)
	  (if (< pos (/ mid-point 2))
	      (setq cur (mew-count-lines (point-min) pos))
	    (setq cur (- mid-line (mew-count-lines pos mid-point))))
	(if (< pos (+ mid-point (/ mid-point 2)))
	    (setq cur (+ mid-line (mew-count-lines mid-point pos)))
	  (setq cur (- ttl (mew-count-lines pos (point-max))))))
      (unless (and (mew-decode-syntax-p)
		   (equal (mew-decode-syntax-buffer) (current-buffer))
		   (>= pos (mew-decode-syntax-begin))
		   (<= pos (mew-decode-syntax-end)))
	(setq cur (1+ cur)))
      (setq left (- ttl cur))
      (setq mew-summary-buffer-left-msgs (format "[%d/%d{%d}]" cur ttl left)))
    (force-mode-line-update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Locking
;;;

(defun mew-summary-exclusive-p (&optional no-msg)
  (cond
   ((eq mew-summary-buffer-process t)
    (or no-msg (message "Try again later"))
    nil) ;; not exclusive
   ((processp mew-summary-buffer-process)
    (or no-msg
	(mew-message-for-summary
	 "Another process is running. Try later or type '\\[mew-summary-kill-subprocess]' to kill it"))
    nil) ;; not exclusive
   (t t))) ;; exclusive

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summary mode
;;;

(defun mew-summary-mode ()
  "\\<mew-summary-mode-map>
Mew Summary mode:: major mode to show a list of messages in a folder.

The keys that are defined for both Summary mode and Virtual mode are:

\\[mew-summary-display]	This command lets you read through messages.
	That is, display a message, scroll it, and move-then-display
	another message or part.

	See 'mew-summary-show-direction' to set the direction that the
	cursor moves. You can select a value out of 'up, 'down,
	'next(current direction) or 'stop.  Default is
	'down. 'mew-summary-show-direction' is valid in this case only
	because the cursor stays in the other cases.

	If called with '\\[universal-argument]', this command displays
	the current message or part again. This is a convenient way to
	get back to the beginning of the current message or part.

\\[mew-summary-prev-page]	Back-scroll this message. 
	Unnecessary header fields are hidden over the window. 
	Type '\\[mew-summary-prev-page]' to see them when a message is displayed.

\\[mew-summary-analyze-again]	1) If this command executed on a message, the cache of the
	message is removed and the message is analyzed then displayed.

	1a) If the size of the current message exceeds
	'mew-file-max-size', MIME analysis is skipped then the
	beginning of the raw message is displayed. In this
	situation, this command analyzes the current message
	without the limitation then displays it.

	1b) If the length of a header exceeds
	'mew-header-max-length', a broken message is
	displayed. In this situation, this command analyzes the
	current message without the limitation then displays it.

	1c) If the current message is displayed by '\\[mew-summary-analyze-again-alternative]', this command
	gets it back to the normal display.

	1d) If 'mew-use-text/html' is nil, its HTML body is
	displayed as is. In this situation, this command analyzes
	the HTML body and displays it.

	1e) If called with '\\[universal-argument]', analyze the
	message with 'mew-decode-broken' reversed.

	2) If this command is called on a part, the part is displayed again.

	2a) If 'mew-use-text/html' is nil, its HTML part is
	displayed as is. In this situation, this command analyzes
	the HTML part and displays it.

\\[mew-summary-analyze-again-alternative]	This command analyzes the message again with 
	'mew-use-alternative' and 'mew-use-text-body' reversed.

\\[mew-summary-display-asis]	Insert this message or part into Message mode in the raw format.

\\[mew-summary-trace-path]	Parse the Received: fields and display them in Message mode.

\\[mew-summary-find-file]	Open this message and makes it read-only.
	If called with '\\[universal-argument]', it stays writable.


\\[mew-summary-scroll-up]	Make this message scroll up with one line.
\\[mew-summary-scroll-down]	Make this message scroll down with one line.

\\[mew-summary-display-down]	Move to below then display. 
	Targets includes parts, messages marked with '*', and non-marked
	messages. When called with '\\[universal-argument]', parts are skipped.
\\[mew-summary-display-up]	Move to above then display. 
	Targets includes parts, messages marked with '*', and non-marked
	messages. When called with '\\[universal-argument]', parts are skipped.

\\[mew-summary-goto-line]	Jump to a line according to the number which you input.
\\[mew-summary-jump-top]	Go to the beginning of this Summary mode.
\\[mew-summary-jump-bottom]	Go to the end of this Summary mode.

\\[mew-summary-retrieve]	Retrieve messages to +inbox asynchronously
	 according to 'mew-mailbox-type'. 
	If 'mew-auto-flush-queue' is non-nil, +queue is flushed.
	If called with '\\[universal-argument]', +queue is not flushed.

\\[mew-summary-retrieve-message]	Retrieve the rest of a truncated('T') message.
	In a LOCAL folder: 'mew-input-case' is used for a case.  If
	'mew-pop-delete'/'mew-imap-delete' is non-nil, delete the message from
	the mailbox.  When executed with '\\[universal-argument]',
	'mew-pop-delete'/'mew-imap-delete' is considered reversed.
	In a REMOTE folder: case is determined by its folder.  The message in
	the server side is always retained.

\\[mew-summary-ls]	List this folder asynchronously.
	In a LOCAL folder: messages in the local folder are scanned according
	to the range which you specify.
	In a REMOTE folder: messages in the server's folder are cached
	according to the range which you specify. If
	'mew-pop-header-only'/'mew-imap-header-only'/'mew-nntp-header-only' is
	non-nil, only headers of messages are cached. If executed with
	'\\[universal-argument]', these variables are considered reversed.

\\[mew-summary-goto-folder]	Go to the folder which you specify.  If executed with
	'\\[universal-argument]', the cursor always goes to the bottom of Summary mode.

\\[mew-summary-write]	Write a message. A new draft is prepared in Draft mode.
	If called with '\\[universal-argument]', the From: address of
	the current message is copied to To: in a draft.

\\[mew-summary-reply]	Answer to this message. A new draft is prepared	in Draft mode.
	Mew automatically decides To: and Cc:.
	When executed with '\\[universal-argument]', answer only to the sender.
\\[mew-summary-reply-with-citation]	Answer to this message. A new draft is prepared in Draft mode.
	Mew automatically decides To:and Cc: and cites the body.
	When executed with '\\[universal-argument]', answer only to the sender.

\\[mew-summary-forward]	Forward this message to a third person. 
	A new draft is prepared in Draft mode and this message is
	automatically attached.
\\[mew-summary-multi-forward]	Forward messages marked with '*' to a third person.
	A new draft is prepared in Draft mode and 
	this message is automatically attached.

\\[mew-summary-reedit]	Edit this message again to retry sending. 
	Or edit this rfc822 part typically included MIME-encapsulated
	error message.  In the +draft folder, it just edits the	message. 
	Otherwise, copy the message to the +draft folder, then edit.

\\[mew-summary-edit-again]	Edit an old fashioned error message in
	which the original message is encapsulated after after strings
	defined in 'mew-summary-edit-again-regex'
	(e.g. \"----- Original message follows -----\").

\\[mew-summary-review]	Put the review mark '*' on this message.
	Use '\\[mew-summary-display-review-down]' or '\\[mew-summary-display-review-up]' to jump to a message marked with '*'.  
	The cursor stays always.
	See also '\\[mew-summary-mark-refile]',	'\\[mew-summary-mark-delete]', '\\[mew-summary-mark-regexp]', and '\\[mew-summary-mark-all]'.

\\[mew-summary-display-review-down]	Jump to the message marked with	'*' below.
\\[mew-summary-display-review-up]	Jump to the message marked with '*' above.

\\[mew-summary-escape]	Put the escape mark '$'.

\\[mew-summary-unshar]	Apply 'unshar' on messages marked with '*'.
\\[mew-summary-uudecode]	Apply 'uudecode' on messages marked with '*'.

\\[mew-summary-burst-multi]	De-capsulate messages embedded in the messages marked with '*'.
\\[mew-summary-join]	Concat Message/Partial fragments marked with '*'
	to an original message.

\\[mew-summary-undo]	Cancel the mark on this message.
\\[mew-summary-undo-all]	Cancel all marks according to what you input.

\\[mew-summary-mark-regexp]	Put the '*' mark onto all messages
	matched to a regular expression.

\\[mew-summary-mark-all]	Put the '*' mark onto all messages which are not marked.
\\[mew-summary-mark-review]	Change the '$' mark into the '*' mark.
\\[mew-summary-mark-escape]	Change the '*' mark into the '$' mark.
\\[mew-summary-mark-undo-all]	Unmark all message marked with 'o'/'D'/'X'.
\\[mew-summary-mark-swap]	Swap the '$' mark and the '*' mark.
\\[mew-summary-exchange-marks]	Exchange the first input mark to the second one.

\\[mew-summary-mark-retrieve-message]	Retrieve the rest of a truncated('T') message marked with '*'.
	In a LOCAL folder: 'mew-input-case' is used for a case.  
	If 'mew-pop-delete'/'mew-imap-delete' is non-nil, 
	delete the message from	the mailbox.
	When executed with '\\[universal-argument]',
	'mew-pop-delete'/'mew-imap-delete' is considered reversed.
	In a REMOTE folder: case is determined by its folder.  
	The message in the server side is always retained.

\\[mew-summary-delete]	Put the delete mark 'D' on this	message.  
	This can overlay other marks. When it overlays, the cursor stays
	on the message. If it marks newly, displays the next message. 

\\[mew-summary-clean-trash]	Remove all messages in +trash.
	When called with '\\[universal-argument]', you can specify a target folder to clean up.

\\[mew-summary-save]	Save any parts. If the target is a message, you
	are asked which you want to save, the entire message or its
	body. If the target is a non-message part, the part is saved
	(with line delimiter conversion if it is a text object).

\\[mew-summary-toggle-disp-msg]	Toggle 'Summary mode only' and	'Summary & Message mode'.
	If you choose 'Summary mode only', you can quickly put 
	the delete marks since the next message is not displayed.

\\[mew-summary-toggle-header-veil]	If 'mew-use-header-veil' is non-nil, field lines of To: and Cc:
	over 'mew-header-veil-count' are covered with invisible veils.
	This commands toggles visibility of these lines.

\\[mew-summary-jump-to-draft-buffer]	Jump to one of drafts if exists.

\\[mew-summary-cite]	Cite this message to one of drafts.

\\[mew-summary-execute-external] Execute an external command according to Content-Type:.
	If called with '\\[universal-argument]', Content-Type: is asked.

\\[mew-summary-execute-command] Execute an inputted command.

\\[mew-summary-recenter]	Make the current line to the center of Summary mode.

\\[mew-summary-burst]	De-capsulate embedded messages in MIME format.

\\[mew-status-update]	Read Addrbook and update its information. 
	If executed with '\\[universal-argument]', information about local folders is 
	also updated in addition to that of Addrbook. 
	If executed with '1', information about local folders is updated.
	If executed with '2', information about newsgroups is updated.
	If executed with '3', information about IMAP folders is updated. 

\\[mew-summary-set-case]	Set cases.

\\[mew-summary-suspend]	Suspend Mew then switch to another buffer. All
	buffers of Mew retain, so you can resume with buffer operations.
\\[mew-summary-quit]	Quit Mew. All buffers of Mew are erased.
\\[mew-summary-kill]	Kill this Summary mode.

\\[mew-summary-convert-local-cs]	Convert the character set of body to the one used locally.

\\[mew-summary-decode-old-pgp]	Decrypting/verifying old-fashioned PGP messages.

\\[mew-summary-x-face]	Display xface.

\\[mew-pgp-fetch-key]	Fetch the PGP public key whose key ID appears in the X-Mew: field.

\\[mew-pgp-select]	Select PGP version.

\\[mew-summary-addrbook-add]	Adding the value of From: in Message mode to Addrbook.
	When executed with '\\[universal-argument], it will add personal information.
	Otherwise, it will add an alias.

\\[mew-summary-alias-edit]	Editing the auto alias file which contains
	a list of alias-address pairs. 
	Remove unnecessary entries and save the file by	'\\[save-buffer]'. 
	After saving, the modification is automatically	reflected.

\\[mew-summary-kill-subprocess]	Kill a process in Summary mode.
	Sometime a process accidentally remains in Summary mode.
	In this situation, you cannot execute '\\[mew-summary-retrieve]', '\\[mew-summary-ls]', nor '\\[mew-summary-exec]'.
	Use this command to solve this problem.

\\[mew-summary-isearch-forward]	Incremental search forward in Message mode.

\\[mew-summary-isearch-backward]	Incremental search backward in Message mode.

\\[mew-summary-print]	Print the content of Message mode according to 'mew-print-function'.
	If called with '\\[universal-argument]', you can specify a printer name.

\\[mew-summary-pipe-message]	Send the content of Message buffer to a command via pipe.
	If called with '\\[universal-argument]', the body of the message
	(excluding its header) is sent.

\\[mew-summary-cmd-msg]	Executing an external command specifying this message as an
	argument.
\\[mew-summary-cmd-msgs]	Executing an external command specifying messages
	marked with '*' as arguments.

\\[mew-summary-info]	Display the folder name of this message and the message number.
	If this is a cache message, both the unique id and the size are 
	also shown.

\\[mew-summary-send-message]	If in +queue, send the messages in +queue.
	If in +postq, post the messages in +postq.
	If in %queue, process the jobs in %queue.
	Otherwise, flush the default queue.
	If executed with '\\[universal-argument]', you can set the sending case.

\\[mew-summary-toggle-8bit]	Toggle 8bit mode(i.e. 'mew-use-8bit').
\\[mew-summary-cache-clean-up]	Clean-up caches of analyzed messages.
\\[mew-summary-toggle-warning]	Toggle warning level.
	If 'mew-warning-field-level' is 2, set it to 1.
	If 'mew-warning-field-level' is 1, set it to 2.
\\[mew-summary-toggle-policy]	Toggle decode policy(i.e. 'mew-decode-broken').
\\[mew-summary-toggle-pgp]	Toggle PGP/MIME and old PGP (i.e. 'mew-use-old-pgp').
\\[mew-summary-toggle-debug]	Toggle 'mew-debug'.

The following commands are provided for Summary mode only, not for
Virtual mode.

\\[mew-summary-refile]	Put the refile mark 'o' on this message. 
	If already marked with 'o', it prints where this message
	will be refiled. This can overlay other marks.
	When it	overlays, the cursor stays on the message.
	If it marks newly, displays the next message.
	If executed with'\\[universal-argument]', it displays how the refile rules work
	in Message mode.

\\[mew-summary-copy]	Put the refile mark 'o' on this message with
	the current folder as a candidate in addition to guessed folders.

\\[mew-summary-refile-again]	Put a refile mark on this message according to
	the previous refile folder.

\\[mew-summary-exec]	Process marked messages. To cancel the '*' mark, use '\\[mew-summary-undo]' or '\\[mew-summary-undo-all]'.

\\[mew-summary-exec-one]	Process the current marked messages.

\\[mew-summary-exec-delete]	Process messages marked with 'D'.
\\[mew-summary-exec-refile]	Process messages marked with 'o'.
	If called with '\\[universal-argument]', only messages whose destination is 
	the same as that of the current message are processed.

\\[mew-summary-mark-refile]	Put the 'o' mark onto all messages marked with '*'. 
	This is very convenient to refile all messages picked by '\\[mew-summary-pick-mark]' and \"\\[mew-summary-grep-mark]\".

\\[mew-summary-mark-copy]	Put the refile mark onto all messages marked with '*', 
	with the current folder as a candidate in addition to guessed folders.
	This is very convenient to refile all messages picked by '\\[mew-summary-pick-mark]' and \"\\[mew-summary-grep-mark]\".

\\[mew-summary-mark-delete]	Put the 'D' mark onto all messages marked with '*'.
\\[mew-summary-mark-unlink]	Put the 'X' mark onto all messages marked with '*'.

\\[mew-summary-exec-offline]	Process marked messages *offline*. Messages to be refiled in 
	a remote folder are moved to the corresponding folder but they are
	marked invalid. Invalid messages are marked with '#'.
	Invalid messages will be overridden when the remote folder
	is scanned online. 
	A job to delete or refile messages, which is created by this command, 
	is queued in a queue folder (%queue for IMAP). To flush jobs in 
	the queue, type '\\[mew-summary-send-message]' in the queue online.
\\[mew-summary-local-copy]	Copy a cached message in a remote folder to a local folder.
\\[mew-summary-mark-local-copy]	Copy cached message marked with '*' in a remote folder
	to a local folder.

\\[mew-summary-pick-mark]	Pick messages according to a pick pattern which you input,
	then put the '*' mark onto them. 'mewl' is called as a picking
	command. If called with '\\[universal-argument]', the target is	the region.

\\[mew-summary-grep-mark]	Grep messages according to a grep pattern which you input,
	then put the '*' mark onto them. 'mew-prog-grep' is called as a
	grep command. If called with '\\[universal-argument]', the target is the region.

\\[mew-summary-cmd-mark]	Grep messages according to a grep pattern which you input,
	then put the '*' mark onto them. You can asked to specify a grep
	command.  If called with '\\[universal-argument]', the target is
	the region.

\\[mew-summary-find-keyword-down]	Display a message marked with '*'
	and find a keyword and highlight it in the forward direction.
	The keyword is stored in a buffer local variable in Summary mode.
	If no key word is set to the variable, this command first asks you
	a keyword. If you want to change the stored keyword, 
	execute this command with '\\[universal-argument]'.
\\[mew-summary-find-keyword-up]	Display a message marked with '*' and
	find a keyword and highlight it in the backward direction. The
	keyword is stored in a buffer local variable in Summary
	mode. If no key word is set to the variable, this command
	first asks you a keyword. If you want to change the stored
	keyword, execute this command with '\\[universal-argument]'.

\\[mew-summary-virtual]	Go to Virtual mode which gives a single view
	to picked messages from multiple folders. Enter a virtual
	folder name, comma-separated folders, and a pick pattern.

\\[mew-summary-mark-virtual]	Making Virtual mode for messages marked with '*'.
	If called with '\\[universal-argument]', you can specify a target mark.

\\[mew-summary-sort]	Sort messages and list them up again.
	If called with '\\[universal-argument]', sort the region.
	After sorting, the cursor moves onto the beginning of the buffer
	or the region. If this command is used in a remote folder,
	local cache messages are sorted.

\\[mew-summary-delete-folder]	Delete this folder.
\\[mew-summary-rename-folder]	Rename this folder.

\\[mew-summary-make-thread]	This command makes threads for the Summary mode as Virtual mode,
	then the cursor jump onto the current message in the Virtual mode. 
	If a corresponding Virtual mode exists,	this command just visits
	the Virtual mode.
\\[mew-summary-regexp-make-thread]	Make threads for messages matched to a specified regular expression.

\\[mew-summary-learn-spam]	Learn that this message is a spam.

\\[mew-summary-learn-ham]	Learn that this message is a ham.

Candidates of RANGE for local folders are as follows:
	all
	update
	<n1>-<n2>
	<n1>-
	-<n2>
	last:<n>

Candidates of RANGE for remote folders are as follows:
	all
	update
	sync
	last:<n>

- 'all' means all messages. Use 'all' to flush the summary buffer. 

- 'update' means the range between the last message included in
  Summary mode + 1 and the real last message on the folder.

- 'sync' means deleting messages which were removed in a remote folder.

<pick pattern> is as follows (in the strong order):
- key=value
	Match if the 'key' field contains the 'value' string
	(case-insensitive).
- key==value
	Match if the 'key' field contains the 'value' string
	(case-sensitive).
- key!=value
	Match if the 'key' field does not contain the 'value' string
	(case-insensitive).
- key!==value
	Match if the 'key' field does not contain the 'value' string
	(case-sensitive).
- ( <pattern> )
	Evaluate <pattern> first.
- ! <pattern>
	Match if not <pattern>.
- <pattern1> & <pattern2>
	Match if <pattern1> AND <pattern2>.
- <pattern1> | <pattern2>
	Match if <pattern1> OR <pattern2>.

Example:
	! key1=val1 | key2=val2 & key3=val3
is equivalent to
	 key1!=val1 | (key2=val2 & key3=val3)

"
  (interactive)
  (setq major-mode 'mew-summary-mode)
  (setq mode-line-buffer-identification mew-mode-line-id)
  (use-local-map mew-summary-mode-map)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  ;;
  (make-local-variable 'tab-width)
  (make-local-variable 'search-invisible)
  (setq search-invisible nil)
  (make-local-variable 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t)
  (make-local-variable 'search-invisible)
  (cond
   (mew-gemacs-p
    (jit-lock-register 'mew-summary-cook-region))
   (t
    (make-local-hook 'window-scroll-functions)
    (add-hook 'window-scroll-functions 'mew-summary-cook-window nil 'local)))
  (add-hook 'kill-buffer-hook 'mew-sinfo-save nil 'local)
  (mew-sinfo-set-disp-msg t)
  ;;
  (mew-summary-mode-name mew-mode-name-summary)
  (mew-summary-setup-mode-line)
  (mew-summary-setup-decoration)
  (mew-highlight-cursor-line)
  (run-hooks 'mew-summary-mode-hook))

(defun mew-summary-kill ()
  "Kill this Summary mode."
  (interactive)
  (mew-kill-buffer)
  (when (and mew-use-other-frame-for-summary (> (length (frame-list)) 1))
    (mew-remove-buffer (mew-buffer-message))
    (delete-frame)))

(defun mew-summary-reset ()
  (mew-unhighlight-cursor-line)
  (mew-window-configure 'summary)
  (mew-current-set nil nil nil)
  (mew-summary-reset-mode-line)
  (mew-decode-syntax-delete))

(provide 'mew-summary)

;;; Copyright Notice:

;; Copyright (C) 1996-2005 Mew developing team.
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

;;; mew-summary.el ends here
