;;; mew-draft.el --- Draft mode for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct  2, 1996

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draft info
;;;

(defvar mew-tinfo-list
  '("header-keymap" "attach-keymap" "case" "encrypted-p" "privacy-err"
    "encode-err" "privacy-type" "hdr-file" "field-del" "other-frame"))

(mew-blinfo-defun 'mew-tinfo mew-tinfo-list)


(defvar mew-draft-mode-syntax-table nil
  "*Syntax table used while in Draft mode.")

(unless mew-draft-mode-syntax-table
  (setq mew-draft-mode-syntax-table (make-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?% "." mew-draft-mode-syntax-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draft mode
;;;

(defun mew-draft-set-local-variables ()
  (auto-save-mode mew-draft-mode-auto-save)
  (make-local-variable 'completion-ignore-case)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat mew-eoh "\\|[ \t]*$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator mew-header-separator)
  (make-local-variable 'comment-start)
  (setq comment-start mew-comment-start)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip mew-comment-start-skip)
  (make-local-hook 'after-change-functions)
  (add-hook 'after-change-functions 'mew-draft-dynamic-highlight nil 'local)
  (if (boundp 'write-file-functions)
      (add-hook 'write-file-functions 'mew-encode-make-backup nil 'local)
    (add-hook 'local-write-file-hooks 'mew-encode-make-backup))
  (make-local-variable 'after-save-hook)
  (when mew-require-final-newline
    (make-local-variable 'require-final-newline)
    (setq require-final-newline t))
  (when (featurep 'x-dnd)
    (make-variable-buffer-local 'x-dnd-test-function)
    (make-variable-buffer-local 'x-dnd-protocol-alist)
    (setq x-dnd-test-function 'mew-draft-dnd-test-function)
    (setq x-dnd-protocol-alist
	  (append '(("^file:///" . mew-draft-dnd-handle-local-file)
		    ("^file://"  . mew-draft-dnd-handle-file)
		    ("^file:"    . mew-draft-dnd-handle-local-file))
		  x-dnd-protocol-alist))))

(defun mew-draft-mode (&optional encrypted)
  "\\<mew-draft-mode-map>
Mew Draft mode:: major mode for composing a MIME message.
Key actions are different in each region: Header, Body, and Attachment.

To send a draft, type \\[mew-draft-make-message] and \\[mew-draft-send-message].  To make multipart, type
\\[mew-draft-prepare-attachments], edit attachments, type \\[mew-draft-make-message] and \\[mew-draft-send-message].

*Whole buffer key assignment:

\\[mew-draft-make-message]	Compose a MIME message then put it into a queue folder.
\\[mew-draft-send-message]	Compose a MIME message then send it.

\\[mew-draft-prepare-attachments]	Prepare an attachment region in the bottom of the draft.
	To compose a multipart message, you should execute this 
	command first.

\\[mew-draft-set-case]	Guess case and set the case for output to it.
\\[mew-draft-rehighlight]  Highlight header and body again.

\\[mew-draft-kill]	Kill this draft.

\\[mew-pgp-sign-message]	Sign the entire draft with PGP. Input your passphrase.
\\[mew-pgp-encrypt-message]	Encrypt the entire draft with PGP.
\\[mew-pgp-sign-encrypt-message]	Sign then encrypt the entire draft with PGP.
	Input your passphrase.
\\[mew-pgp-encrypt-sign-message]	Encrypt then sign the entire draft with PGP.
	Input your passphrase.

\\[mew-draft-toggle-privacy-always]	Toggle whether or not all drafts are protected.
\\[mew-draft-toggle-privacy-encrypted]	Toggle whether or not drafts replying to encrypted messages 
		are protected.
\\[mew-draft-set-privacy-type]	Set privacy service which will be effective when \\[mew-draft-make-message].
\\<mew-draft-header-map>
*Header region key assignment:

\\[mew-draft-header-comp]	Complete field keys.
	Complete and expand an address short name.
	Complete folder names.
\\[mew-draft-circular-comp]	Complete your mail domain.
\\[mew-draft-expand]	Replace an address with 'NAME <address>'.

*Body region key assignment:

\\<mew-draft-body-map>\\[mew-draft-insert-signature]	Insert '~/.signature' on the cursor point.
\\<mew-draft-mode-map>\\[mew-draft-cite]	Copy and paste a part of message from Message mode with
	citation prefix and label.
	1. Roughly speaking, it copies the body in Message mode. 
	   For example, if text/plain is displayed, the entire Message 
	   mode is copied. If message/rfc822 is displayed, the body 
	   without the header is copied.
	2. If called with '\\[universal-argument]', the header is also copied if exists.
	3. If an Emacs mark exists, the target is the region between 
	   the mark and the cursor.
\\[mew-draft-yank]	Copy and paste a part of message from Message mode WITHOUT
	citation prefix and label.

*Attachments region Key assignment:
\\<mew-draft-attach-map>
\\[mew-attach-forward]	Go to the first subdirectory.
\\[mew-attach-backforward]	Go to the parent directory.
\\[mew-attach-next]	Go to the next file in the current directory.
\\[mew-attach-previous]	Go to the previous file in the current directory.

\\[mew-attach-copy]	Copy a file (via networks) on '.'.
	To copy a remote file, use the '/[user@]hostname:/filepath' syntax.
\\[mew-attach-link]	Link a file with a symbolic link on '.'.
\\[mew-attach-link-message]	Link the message displayed in Message mode with a symbolic link on '.'.
\\[mew-attach-delete]	Delete this file or this directory.
\\[mew-attach-multipart]	Create a subdirectory(i.e. multipart) on '.'.
\\[mew-attach-find-file]	Open this file into a buffer.
\\[mew-attach-find-new-file]	Open a new file into a buffer on '.'.
\\[mew-attach-external-body]	Input external-body on '.'.
\\[mew-attach-audio]	Sampling voice and insert as audio file on '.'.
\\[mew-attach-pgp-public-key]	Extract the PGP key for the inputted user on '.'.
\\[mew-attach-description]	Input a description(Content-Description:).
\\[mew-attach-disposition]	Change the file name(Content-Disposition:).
\\[mew-attach-type]	Change the data type(Content-Type:).
\\[mew-attach-charset]	Specify the charset parameter for a Text/* object.
\\[mew-attach-icharset]	Specify a input coding-system for a text file.

\\[mew-attach-base64]	Put the 'B' mark to encode with Base64.
\\[mew-attach-quoted-printable]	Put the 'Q' mark to encode with Quoted-Printable.
\\[mew-attach-gzip64]	Put the 'G' mark to encode with Gzip64. This is applicable 
	only to Text/Plain and Application/Postscript since compression 
	is not effective other objects. For example, JPEG is already 
	compressed.
\\[mew-attach-pgp-sign]	Put the 'PS' mark to sign with PGP.
\\[mew-attach-pgp-enc]	Put the 'PE' mark to encrypt with PGP. 
	Input decryptors' addresses.
\\[mew-attach-undo]	Unmark. The original mark appears.

* Fill blanks
\\<mew-draft-mode-map>
Prepare '~/.mew-fib' like;

	name:  Kazuhiko Yamamoto
	email: Kazu@Mew.org

If you receive a message like;

	Your name : |>name<|
	Your e-mail address: |>email<|

Type \\<mew-summary-mode-map>\\[mew-summary-reply] in Summary mode to enter Draft mode. 
Then type \\<mew-draft-mode-map>\\[mew-draft-yank], \\[mew-fib-fill-default], and \\[mew-fib-delete-frame] makes following
draft.

	Your name : Kazuhiko Yamamoto
	Your e-mail address: Kazu@Mew.org

In this way, mew-fil fills up items quoted like |> <| from '~/.mew-fib'.
The fill functions described below.

\\[mew-fib-fill-default]	Fill |>item<| from '~/.mew-fib'.
\\[mew-fib-delete-frame]	Delete all quotations, i.e. |> <|.
\\[mew-fib-next-item]	Jump to the next fib item.
\\[mew-fib-previous-item]	Jump to the previous fib item.
\\[mew-fib-flush-input]	Flush input from '~/.mew-fib'.

Moreover, '~/.mew-fib' supports aliases like;

	email: Kazu@Mew.org
	e-mail:

"
  (interactive)
  (setq major-mode 'mew-draft-mode)
  (setq mode-line-buffer-identification mew-mode-line-id)
  (mew-draft-mode-name)
  (mew-draft-set-local-variables)
  (use-local-map mew-draft-mode-map)
  (set-syntax-table mew-draft-mode-syntax-table)
  (cd (expand-file-name mew-home))
  (mew-draft-setup-decoration)
  (mew-ainfo-set-icon (file-name-nondirectory (buffer-file-name)))
  (mew-tinfo-set-encrypted-p encrypted)
  (mew-tinfo-set-privacy-err nil)
  (mew-tinfo-set-privacy-type nil)
  (run-hooks 'text-mode-hook 'mew-draft-mode-hook)
  ;; auto-fill-function is set by mew-draft-mode-hook
  (when auto-fill-function
    (make-local-variable 'auto-fill-function)
    (setq auto-fill-function 'mew-draft-auto-fill))
  (setq buffer-undo-list nil))

(defun mew-draft-mode-name (&optional header)
  (let (pcdb sub)
    (cond
     ((or (mew-tinfo-get-privacy-type) (mew-tinfo-get-privacy-err))
      ;; If privacy err, don't display mew-protect-privacy-always-type etc.
      (setq pcdb (mew-pcdb-by-service (mew-tinfo-get-privacy-type)))
      (setq sub (mew-pcdb-mark pcdb)))
     ((and (mew-tinfo-get-encrypted-p) mew-protect-privacy-encrypted)
      (setq pcdb (mew-pcdb-by-service mew-protect-privacy-encrypted-type))
      (setq sub (mew-pcdb-mark pcdb)))
     (mew-protect-privacy-always
      (setq pcdb (mew-pcdb-by-service mew-protect-privacy-always-type))
      (setq sub (mew-pcdb-mark pcdb))))
    (setq mode-name (if header mew-mode-name-header mew-mode-name-draft))
    (if sub (setq mode-name (concat mode-name " " sub)))
    (unless (mew-case-default-p (mew-tinfo-get-case))
      (setq mode-name (concat mode-name " " (mew-tinfo-get-case))))
    (force-mode-line-update)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draft subfunctions
;;;

(defun mew-draft-dynamic-highlight (beg end len)
  (when (mew-in-header-p)
    (save-match-data
      (mew-highlight-header)
      (when (mew-draft-p)
	(mew-draft-header-keymap)))))

(defun mew-draft-auto-fill ()
  (let ((ret1 (do-auto-fill)) ret2)
    (when (mew-in-header-p)
      (save-excursion
	(beginning-of-line)
	(while (not (or (looking-at "[^ \t\n]+:\\|[ \t]") (bobp)))
	  (setq ret2 t)
	  (insert "\t")
	  (forward-line -1)
	  (beginning-of-line))))
    (or ret1 ret2))) ;; if modifies, return t.

(defun mew-draft-find-and-switch (draft-path &optional switch-func)
  ;; switch-func = nil :: switch-to-buffer
  ;; switch-func = t   :: switch-to-buffer-other-window
  (let* ((special-display-buffer-names nil)
	 (special-display-regexps nil)
	 (same-window-buffer-names nil)
	 (same-window-regexps nil)
	 (draftname (mew-path-to-folder draft-path)))
    (when (get-buffer draftname)
      (save-excursion
	(set-buffer draftname)
	(clear-visited-file-modtime)
	(set-buffer-modified-p nil) ;; just in case
	(mew-delete-file buffer-auto-save-file-name)
	(mew-remove-buffer draftname)))
    (cond
     (mew-use-other-frame-for-draft
      (setq switch-func 'switch-to-buffer-other-frame))
     ((eq switch-func nil)
      (setq switch-func 'switch-to-buffer))
     ((eq switch-func t)
      (setq switch-func 'switch-to-buffer-other-window)))
    (mew-frwlet
     mew-cs-m17n mew-cs-dummy
     (funcall switch-func (mew-find-file-noselect draft-path)))
    ;; draft buffer
    (mew-set-buffer-cs mew-cs-m17n)
    ;; copy config, first
    (mew-tinfo-set-case mew-case-output)
    (when mew-use-other-frame-for-draft
      (mew-tinfo-set-other-frame t)
      ;; to ensure to cite a message from summary frame.
      (mew-remove-buffer (mew-buffer-message)))
    (rename-buffer draftname)))

(defun mew-draft-to-attach (draft)
  "Converting draft to attach. E.g. +draft/1 -> +attach/1"
  (mew-concat-folder mew-attach-folder (file-name-nondirectory draft)))

(defun mew-attachdir (&optional draft)
  (mew-expand-folder (mew-draft-to-attach (or draft (buffer-name)))))

(defun mew-draft-header-insert-alist (halist)
  "Insert field-body: and field-value. Return the value of
the Body: field."
  (let ((case-fold-search t)
	key val ret)
    (while halist
      (setq key (car (car halist)))
      (setq val (cdr (car halist)))
      (setq halist (cdr halist))
      (unless (string-match ":$" key)
	(setq key (concat key ":")))
      (if (string-match mew-body: key)
	  (setq ret val)
	(mew-draft-header-insert key val)))
    ret))

(defun mew-insert-address-list (field adrs del force-insert)
  (let ((cnt 0) (beg (point)) med adr)
    (while adrs
      (setq adr (car adrs) adrs (cdr adrs))
      (unless (mew-is-my-address del adr)
	(if (= cnt 0)
	    (insert adr)
	  (insert ", " adr))
	(setq del (cons (concat "^" (regexp-quote adr) "$") del))
	(setq cnt (1+ cnt))))
    (when (or force-insert (> cnt 0))
      (beginning-of-line)
      (insert field " ")
      (setq med (point))
      (end-of-line)
      (insert "\n")
      (mew-header-fold-region beg (point) med 'use-tab))
    del))

(defun mew-insert-address-list2 (field adrs)
  (when adrs
    (let ((beg (point)) med)
      (insert field " ")
      (setq med (point))
      (insert (car adrs))
      (setq adrs (cdr adrs))
      (while adrs
	(insert ", " (car adrs))
	(setq adrs (cdr adrs)))
      (insert "\n")
      (mew-header-fold-region beg (point) med 'use-tab))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Draft header
;;;

(defun mew-draft-header (&optional subject nl to cc newsgroups in-reply-to references other-headers fromme)
;; to -- string or list
;; cc -- string or list
;; nl -- one empty line under "----", which is necessary if
;;      attachment is prepared
  (let ((del (unless fromme mew-regex-my-address-list)) ;; deleting list
	case body)
    (goto-char (point-min))
    (if newsgroups
	(cond
	 ((stringp newsgroups)
	  (mew-draft-header-insert mew-newsgroups: newsgroups))
	 ((listp newsgroups)
	  (mew-insert-address-list2 mew-newsgroups: newsgroups)))
      ;; Insert To: first.
      ;; All addresses inserted on To: are appended to del.
      (cond
       ((null to) (mew-draft-header-insert mew-to: ""))
       ((stringp to) ;; To: inputted from the mini-buffer.
	;; do not check to is mine. Cc: is also string
	;; We believe that user never specifies the same address of To: to Cc:.
	(mew-draft-header-insert mew-to: to))
       ;; To: collected by reply
       ((listp to)
	(setq del (mew-insert-address-list mew-to: to del t))))
      (cond
       ((null cc) ()) ;; do nothing 
       ((stringp cc) ;; Cc: inputted from the mini-buffer.
	(mew-draft-header-insert mew-cc: cc))
       ((listp cc) ;; Cc: collected by reply.
	(mew-insert-address-list mew-cc: cc del nil))))
    (if mew-case-guess-when-prepared
	(mew-draft-set-case-by-guess))
    (setq case (mew-tinfo-get-case))
    (unless newsgroups
      (mew-draft-header-insert mew-cc: (mew-cc case)))
    (mew-draft-header-insert mew-subj: (or subject ""))
    (mew-draft-header-insert mew-from: (mew-from case))
    (mew-draft-header-insert mew-fcc: (mew-fcc case))
    (unless newsgroups
      (mew-draft-header-insert mew-dcc: (mew-dcc case)))
    (mew-draft-header-insert mew-reply-to: (mew-reply-to case))
    (unless newsgroups
      (mew-draft-header-insert mew-in-reply-to: in-reply-to))
    (mew-draft-header-insert mew-references: references)
    (if (and mew-x-face-file
	     (file-exists-p (expand-file-name mew-x-face-file)))
	(let (xface)
	  (with-temp-buffer
	    (mew-insert-file-contents (expand-file-name mew-x-face-file))
	    (setq xface (mew-buffer-substring (point-min)
					      (max (buffer-size) 1))))
	  (mew-draft-header-insert mew-x-face: xface)))
    (mew-draft-header-insert mew-organization: (mew-organization case))
    (setq body (mew-draft-header-insert-alist other-headers))
    ;; Deleting fields defined in mew-header-alist to replace them.
    (mew-header-delete-lines (mapcar 'car (mew-header-alist case)))
    (mew-header-goto-end)
    (mew-draft-header-insert-alist (mew-header-alist case))
    ;; X-Mailer: must be the last
    (mew-draft-header-insert mew-x-mailer: mew-x-mailer)
    ;; (mew-header-set "\n") is enough. But highlighting delayed.
    (mew-header-prepared)
    ;; on the body
    (if nl (insert "\n"))
    (if body (save-excursion (insert body)))
    ;; move the cursor after "To: "
    (goto-char (point-min))
    (search-forward ": " nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Citation
;;;

(defun mew-draft-auto-set-input-method ()
  (if (and (fboundp 'activate-input-method)
	   mew-charset-input-method-alist)
      (let* ((charset (mew-charset-guess-region
		       (mew-header-end) (or (mew-attach-begin) (point-max))))
	     (method (mew-charset-to-input-method charset)))
	(when (stringp method)
	  (activate-input-method method)
	  (message "Set input method to %s" method)))))

(defun mew-draft-yank (&optional arg force)
  "Copy and paste a part of message from Message mode WITHOUT
citation prefix and label.
1. Roughly speaking, it copies the body in Message mode. For example,
   if text/plain is displayed, the entire Message mode is copied.
   If message/rfc822 is displayed, the body without the header is copied.
2. If called with '\\[universal-argument]', the header is also copied if exists.
3. If an Emacs mark exists, the target is the region between the mark and 
   the cursor."
;; MUST take care of C-x C-x
;; MUST be able to cancel by C-x u
  (interactive "P")
  (if (and (not force) (or (mew-in-header-p) (mew-in-attach-p)))
      (message "Cannot cite a message here")
    (let (cite beg end)
      (save-excursion
	(cond
	 ((get-buffer (mew-buffer-message))
	  (set-buffer (mew-buffer-message)))
	 ((get-buffer mew-message-last-buffer)
	  (set-buffer mew-message-last-buffer)))
	(set-buffer (mew-buffer-message))
	(save-restriction
	  (widen)
	  (let ((mark-active t))
	    (cond
	     (arg 
	      (setq beg (point-min) end (point-max)))
	     ((and (not mew-cite-ignore-region)
		   (mew-mark)
		   (/= (point) (mew-mark))
		   (not (and mew-cite-ignore-mouse-region
			     (mew-mouse-region-p))))
	      (setq beg (region-beginning) end (region-end)))
	     ((mew-msghder-p)
	      ;; header exists in Message mode
	      (mew-header-goto-body)
	      (setq beg (point) end (point-max)))
	     (t
	      (setq beg (point-min) end (point-max)))))
	  (setq cite (mew-buffer-substring beg end))))
      (mew-push-mark)
      (insert cite)
      (mew-draft-auto-set-input-method))))

(defvar mew-message-citation-buffer nil
  "This value is used by mew-gnus.el to specify a buffer from where
you can cite.")

(defvar mew-message-citation-frame-id nil)

(defun mew-draft-cite (&optional arg force)
  "Copy and paste a part of message from Message mode with
citation prefix and label.
1. Roughly speaking, it copies the body in Message mode. For example,
   if text/plain is displayed, the entire Message mode is copied.
   If message/rfc822 is displayed, the body without the header is copied.
2. If called with '\\[universal-argument]', the header is also copied if exists.
3. If an Emacs mark exists, the target is the region between the mark and 
   the cursor."
;; MUST take care of C-x C-x
;; MUST be able to cancel by C-x u
  (interactive "P")
  (if (and (not force) (or (mew-in-header-p) (mew-in-attach-p)))
      (message "Cannot cite a message here")
    (let* ((nonmewbuf mew-message-citation-buffer) ;; may be buffer local
	   (fid (or mew-message-citation-frame-id (mew-frame-id))) 
	   (fld (mew-current-get-fld fid))
	   (msg (mew-current-get-msg fid))
	   (msg-buf (mew-buffer-message))
	   cite beg end tbuf irt-msgid)
      (unless (get-buffer msg-buf)
	(setq msg-buf mew-message-last-buffer))
      (save-excursion
	;;
	;; extract the body without header
	;;
	(setq tbuf (or nonmewbuf msg-buf))
	(if (get-buffer tbuf)
	    (set-buffer tbuf)
	  (error "No buffer to be cited"))
	(save-restriction
	  ;; first prepare "cite"
	  (widen)
	  (let ((mark-active t))
	    (cond
	     ;; arg will be effect in mew-cite-original
	     ((and (not mew-cite-ignore-region)
		   (mew-mark)
		   (/= (point) (mew-mark))
		   (not (and mew-cite-ignore-mouse-region
			     (mew-mouse-region-p))))
	      (setq beg (region-beginning) end (region-end)))
	     ((mew-msghder-p)
	      ;; header exists in Message mode. Skip the header
	      ;; because we will concatenate it to cite later.
	      (mew-header-goto-body)
	      (setq beg (point) end (point-max)))
	     (t
	      (setq beg (point-min) end (point-max)))))
	  (setq cite (mew-buffer-substring beg end)))
	;; concat the header
	;; see also mew-summary-reply
	(setq tbuf (or nonmewbuf
		       (save-excursion
			 (when (get-buffer msg-buf)
			   (if (mew-msghder-p) (current-buffer))))
		       ;; header exists only in cache if multipart
		       (mew-cache-hit fld msg)))
	(if (get-buffer tbuf)
	    (set-buffer tbuf)
	  (error "No buffer to be cited"))
	(save-restriction
	  (widen)
	  (mew-header-goto-end)
	  (setq cite (concat (mew-buffer-substring (point-min) (point)) 
			     "\n" cite))
          (setq irt-msgid (mew-idstr-get-first-id
			   (mew-header-get-value mew-message-id:)))))
      ;; 
      ;; Draft mode, insert the header and the body.
      ;;

      ;; Append message-id to In-Reply-To:
      (if (and irt-msgid (mew-msghder-p))
          (save-excursion
            (let* ((mew-references-max-count nil)
		   (irt (mew-header-get-value mew-in-reply-to:))
		   (irtl (mew-idstr-to-id-list irt 'rev))
		   irtstr)
	      (mew-addq irtl irt-msgid)
	      (setq irtl (nreverse irtl))
	      (setq irtstr (mew-id-list-to-idstr irtl))
	      (mew-header-delete-lines (list mew-in-reply-to:))
	      (unless irt (goto-char (mew-header-end)))
	      (mew-draft-header-insert mew-in-reply-to: irtstr))))
      (save-restriction
	;; this gets complicated due to supercite, please do not care
	(narrow-to-region (point)(point)) ;; for (goto-char (point-min))
	(insert cite)
	;; not for C-x C-x. Do not use mew-push-mark.
	(push-mark (point) t t)
	(goto-char (point-min)))
      (cond
       (mew-cite-hook
	(run-hooks 'mew-cite-hook))
       (t (mew-cite-original arg)))
      ;; (mark-marker) indicates the point after label.
      ;; Should we include the label too?
      (or force (mew-highlight-body-region (mark-marker) (point) 'draft))
      (mew-draft-auto-set-input-method))))

(defun mew-cite-original (&optional arg)
  (if (< (marker-position (mark-marker)) (point))
      (exchange-point-and-mark))
  (let ((beg (point)) (end (marker-position (mark-marker)))
        label prefix)
    (save-restriction
      (narrow-to-region beg end)
      (condition-case nil
          (setq label (funcall mew-cite-strings-function))
        (error
	 (error "Syntax of mew-cite-format was changed. Read explanation of mew-cite-fields")))
      (if (null mew-cite-prefix-function)
          (setq prefix mew-cite-prefix)
        (setq prefix (funcall mew-cite-prefix-function)))
      (if mew-cite-prefix-confirmp
          (let ((ask (read-string 
                      (format "Prefix (\"%s\"): " prefix) "")))
            (if (not (string= ask "")) (setq prefix ask))))
      ;; C-u C-c C-y cites body with header.
      (if (eq arg nil) 
	  ;; header has been already cited. So, delete it.
	  (delete-region beg (progn (mew-header-goto-body) (point))))
      (insert label)
      (mew-push-mark)
      (and (bolp) (insert prefix))
      (while (= 0 (forward-line))
	(or (= (point) (point-max))
	    (insert prefix))))))

(defun mew-cite-get-value (field)
  (let ((value (mew-header-get-value field))
	repl func)
    (when (and (string= mew-from: field) value
	       (setq func (mew-addrbook-func mew-addrbook-for-cite-label)))
      (setq repl (funcall func (mew-addrstr-parse-address value)))
      (if repl (setq value repl)))
    (or value "")))

(defun mew-cite-strings ()
  "A function to create cite labels according to 
'mew-cite-format' and 'mew-cite-fields'."
  (if (null mew-cite-fields)
      ""
    (let* ((vals (mapcar 'mew-cite-get-value mew-cite-fields))
	   (label (apply 'format mew-cite-format vals))
	   (ellipses (if (stringp mew-draft-cite-ellipses)
			 mew-draft-cite-ellipses ""))
	   beg eol)
      (if (not (or (eq mew-draft-cite-fill-mode 'truncate)
		   (eq mew-draft-cite-fill-mode 'wrap)))
	  label
	(with-temp-buffer
	  (let ((fill-column
		 (or mew-draft-cite-label-fill-column fill-column)))
	    (insert label)
	    (goto-char (point-min))
	    (while (not (eobp))
	      (cond
	       ((eq mew-draft-cite-fill-mode 'truncate)
		(end-of-line)            
		(if (>= fill-column (current-column))
		    ()
		  (setq eol (point))
		  (insert ellipses)
		  (goto-char eol)
		  (while (< fill-column (current-column))
		    (delete-backward-char 1))))
	       ((eq mew-draft-cite-fill-mode 'wrap)
		(setq beg (point))
		(end-of-line)
		(if (= (current-column) 0)
		    ()
		  (fill-region beg (point))
		  (if (= (current-column) 0) ;; for XEmacs
		      (delete-backward-char 1)))))
	      (forward-line)))
	  (buffer-string))))))

(defun mew-cite-prefix-username ()
  "A good candidate for mew-cite-prefix-function.
The citation style is 'from_address> ', e.g. 'kazu> '"
  (let* ((from (mew-header-parse-address mew-from:))
	 (user (mew-addrstr-extract-user from))
	 (func (mew-addrbook-func mew-addrbook-for-cite-prefix))
	 nickname prefix)
    (if func (setq nickname (funcall func from)))
    (setq prefix (or nickname user))
    (if mew-ask-cite-prefix
	(setq prefix (read-string "Citation prefix: " prefix)))
    (concat prefix mew-cite-prefix)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Misc
;;;

(defun mew-draft-save-buffer ()
  "Save this draft."
  (interactive)
  (let ((after-change-functions nil))
    (save-excursion
      (mew-header-clear 'keep-read-only)
      (insert-before-markers "\n") ;; for mew-summary-reply
      (save-buffer)
      (delete-region (1- (point)) (point))
      (mew-header-prepared)
      (set-buffer-modified-p nil))))

(defun mew-draft-kill ()
  "Kill this draft."
  (interactive)
  (if (not (y-or-n-p "Kill draft message? "))
      (message "Draft was not killed")
    (let* ((attachdir (mew-attachdir)) ;; attachdir must be here
	   (draft (buffer-file-name))
	   (buf (current-buffer))
	   (mdi (concat draft mew-draft-info-suffix)))
      (mew-elet
       (mew-overlay-delete-buffer))
      (save-buffer)
      (mew-delete-file draft)
      (mew-delete-file mdi)
      (if (and (mew-tinfo-get-other-frame) (> (length (frame-list)) 1))
	  (delete-frame)
	(mew-current-get-window-config))
      (mew-delete-directory-recursively attachdir)
      (mew-remove-buffer buf)
      (message "Draft was killed"))))

(defun mew-draft-insert-signature (&optional arg)
  "Insert the signature file specified by mew-signature-file.
If attachments exist and mew-signature-as-lastpart is *non-nil*,
the file is attached to the last part. Otherwise, the file is 
inserted into the body. If mew-signature-insert-last is *non-nil*,
the file is inserted to the end of the body. Otherwise, inserted
the cursor position. If executed with '\\[universal-argument]',
you can set the case."
  (interactive "P")
  (let (case sigfile)
    (cond
     ((stringp arg)
      (setq case arg))
     (arg
      (setq case (mew-input-case (mew-tinfo-get-case) "Signature")))
     (t
      (setq case (mew-tinfo-get-case))))
    (setq sigfile (expand-file-name (mew-signature-file case)))
    (if (not (file-exists-p sigfile))
	(message "No signature file %s" sigfile)
      (if (and (mew-attach-p) mew-signature-as-lastpart)
	  (progn
	    (goto-char (point-max))
	    (forward-line -2)
	    (mew-attach-forward)
	    (mew-attach-copy sigfile "Signature")
	    (let* ((nums (mew-syntax-nums))
		   (syntax (mew-syntax-get-entry mew-encode-syntax nums)))
	      (mew-syntax-set-cdp syntax nil)
	      (mew-syntax-set-cd  syntax mew-signature-description))
	    (mew-encode-syntax-print mew-encode-syntax))
	(when mew-signature-insert-last 
	  (if (null (mew-attach-p))
	      (goto-char (point-max))
	    (goto-char (1- (mew-attach-begin))))
	  (end-of-line)
	  (unless (bolp) (insert "\n")))
	(mew-insert-file-contents sigfile)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Re-highlight
;;; 

(defun mew-draft-rehighlight-body ()
  (save-excursion
    (let ((beg (progn (goto-char (mew-header-end)) (forward-line) (point)))
	  (end (or (mew-attach-begin) (point-max))))
      (mew-highlight-body-region beg end 'draft 'rehighlight))))

(defun mew-draft-rehighlight ()
  "Highlight header and body again."
  (interactive)
  (let ((mod (buffer-modified-p)))
    (mew-highlight-header)
    (mew-draft-header-keymap)
    (mew-draft-rehighlight-body)
    (set-buffer-modified-p mod)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Privacy
;;;

(defun mew-draft-toggle-privacy-always ()
  "Toggle whether or not all drafts are protected."
  (interactive)
  (setq mew-protect-privacy-always (not mew-protect-privacy-always))
  (message "Set mew-protect-privacy-always to %s"
	   mew-protect-privacy-always)
  (mew-draft-mode-name))

(defun mew-draft-toggle-privacy-encrypted ()
  "Toggle whether or not drafts replying to encrypted messages are 
protected."
  (interactive)
  (setq mew-protect-privacy-encrypted (not mew-protect-privacy-encrypted))
  (message "Set mew-protect-privacy-encrypted to %s"
	   mew-protect-privacy-encrypted)
  (mew-draft-mode-name))

(defun mew-draft-set-privacy-type ()
  "\\<mew-draft-mode-map>
Set privacy service which will be effective when \\[mew-draft-make-message]."
  (interactive)
  (let* ((services (mew-pcdb-services))
	 (alist (mapcar (lambda (x) (cons (symbol-name x) x)) services))
	 str)
    (setq str (completing-read "Input privacy services: " alist nil t))
    (when (stringp str)
      (mew-tinfo-set-privacy-type (cdr (assoc str alist)))
      (mew-tinfo-set-privacy-err nil)))
  (mew-draft-mode-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sending and Queuing
;;;

(defun mew-draft-make-message (&optional privacy signer)
  "Compose a MIME message then put it into a queue folder."
  (interactive)
  (if (and (interactive-p) ;; prevent the loop
	   mew-use-old-pgp
	   mew-protect-privacy-with-old-pgp-signature)
      (mew-pgp-sign-message)
    (mew-draft-process-message 'queue privacy signer)))

(defun mew-draft-send-message ()
  "Compose a MIME message then send it."
  (interactive)
  (if (and (interactive-p) ;; just in case
	   mew-use-old-pgp
	   mew-protect-privacy-with-old-pgp-signature)
      (mew-pgp-sign-message)
    (mew-draft-process-message 'send)))

(defun mew-draft-process-message (action &optional privacy signer)
  (run-hooks 'mew-make-message-hook)
  (let* ((case (or (mew-tinfo-get-case) mew-case-default))
	 (old-case case)
	 guessed-case)
    (when mew-case-guess-when-composed
      (setq guessed-case (mew-draft-get-case-by-guess))
      (when guessed-case
	(if mew-case-guess-addition
	    (setq case (mew-draft-add-case case guessed-case))
	  (setq case guessed-case))))
    (unless (string= old-case case)
      (mew-tinfo-set-case case)
      (mew-draft-mode-name (mew-tinfo-get-hdr-file))
      (mew-draft-replace-fields old-case)
      (when (eq action 'send)
	(mew-highlight-header)
	(unless (mew-tinfo-get-hdr-file) (mew-draft-header-keymap)))
      (save-buffer))
    (if (mew-header-existp mew-newsgroups:)
	(mew-draft-nntp-process-message case action privacy signer)
      (mew-draft-smtp-process-message case action privacy signer))))

(defun mew-draft-resent-p (end)
  (let ((case-fold-search t))
    (save-excursion
      (re-search-forward mew-resent-regex end t))))

(defun mew-draft-smtp-process-message (case action &optional privacy signer)
  (run-hooks 'mew-send-hook)
  (let* ((buf (current-buffer))
	 (pnm (mew-smtp-info-name case))
	 (queue (mew-queue-folder case))
	 resentp fcc sendit msg err)
    (if (get-process pnm)
	(message "Another message is being sent. Try later")
      (mew-draft-remove-invalid-fields)
      ;; Check resentp
      (save-excursion
	(goto-char (point-min))
	(setq resentp (mew-draft-resent-p (mew-header-end))))
      ;; Ask Subject: before the query of "Really send".
      ;; Typing C-g here gets back to the draft.
      (mew-encode-ask-subject)
      (setq fcc (mew-encode-ask-fcc resentp))
      (if (eq action 'queue)
	  (setq sendit t)
	(if mew-ask-send
	    (setq sendit (y-or-n-p "Really send this message? "))
	  (setq sendit t)))
      (when sendit
	;; password should be asked in Summary mode.
	(if (and (mew-tinfo-get-other-frame) (> (length (frame-list)) 1))
	    (delete-frame)
	  (mew-current-get-window-config)
	  (delete-windows-on buf)) ;; just in case
	(save-excursion
	  (save-window-excursion
	    (set-buffer buf)
	    (if (mew-smtp-encode pnm case resentp fcc privacy signer)
		(let ((mdi (concat (buffer-file-name) mew-draft-info-suffix)))
		  (mew-delete-file mdi)
		  (setq msg (mew-smtp-queue case "from Draft mode"))
		  (mew-remove-buffer buf)
		  (if (eq action 'send)
		      (mew-smtp-send-message case queue (list msg))))
	      (setq err t))))
	;; now +queue/1 exists
	(if err
	    (progn
	      (mew-current-set-window-config)
	      (switch-to-buffer buf)
	      (delete-other-windows))
	  (if (and (eq action 'queue) mew-visit-queue-after-sending)
	      (mew-summary-visit-folder queue))
	  (run-hooks 'mew-real-send-hook))))))

(defun mew-draft-nntp-process-message (case action &optional privacy signer)
  (run-hooks 'mew-post-hook)
  (let* ((buf (current-buffer))
	 (pnm (mew-nntp2-info-name case))
	 (postq (mew-postq-folder case))
	 fcc sendit msg err)
    (if (get-process pnm)
	(message "Another message is being posted. Try later")
      (mew-draft-remove-invalid-fields)
      ;; Ask Subject: before the query of "Really post".
      ;; Typing C-g here gets back to the draft.
      (mew-encode-ask-subject)
      (setq fcc (mew-encode-ask-fcc nil))
      (if (eq action 'queue)
	  (setq sendit t)
	(if mew-ask-post
	    (setq sendit (y-or-n-p "Really post this message? "))
	  (setq sendit t)))
      (when sendit
	;; password should be asked in Summary mode.
	(if (and (mew-tinfo-get-other-frame) (> (length (frame-list)) 1))
	    (delete-frame)
	  (mew-current-get-window-config)
	  (delete-windows-on buf)) ;; just in case
	(save-excursion
	  (save-window-excursion
	    (set-buffer buf)
	    (if (mew-nntp2-encode pnm case fcc privacy signer)
		(let ((mdi (concat (buffer-file-name) mew-draft-info-suffix)))
		  (mew-delete-file mdi)
		  (setq msg (mew-nntp2-queue case "from Draft mode"))
		  (mew-remove-buffer buf)
		  (if (eq action 'send)
		      (mew-nntp2-send-message case postq (list msg))))
	      (setq err t))))
	(if err
	    (progn
	      (mew-current-set-window-config)
	      (switch-to-buffer buf)
	      (delete-other-windows))
	  (if (and (eq action 'queue) mew-visit-queue-after-sending)
	      (mew-summary-visit-folder postq))
	  (run-hooks 'mew-real-post-hook))))))

(defun mew-draft-remove-invalid-fields ()
  (when (mew-header-end)
    (save-excursion
      (save-restriction
	(goto-char (mew-header-end))
	(if (not (bolp)) (insert "\n"))
	(narrow-to-region (point-min) (mew-header-end))
	(let (beg med str)
	  (mew-elet
	   ;; removing null lines
	   (goto-char (point-min))
	   (while (and (re-search-forward "^$" nil t)
		       (not (eobp)))
	     (delete-char 1))
	   ;; removing fields which do not have value.
	   (goto-char (point-min))
	   (while (not (eobp))
	     (if (not (looking-at mew-keyval))
		 (forward-line)
	       (setq beg (match-beginning 0))
	       (setq med (match-end 0))
	       (forward-line)
	       (mew-header-goto-next)
	       (setq str (mew-buffer-substring med (1- (point))))
	       ;; str may consists of multiple lines
	       ;; So, "$" does not work. We need to use "[^ ]".
	       (unless (string-match "[^ \t\n]" str)
		 (delete-region beg (point)))))))))))

;; backward-compatibility
(defalias 'mew-draft-send-letter 'mew-draft-send-message)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Privacy
;;;

(defvar mew-draft-privacy-method 'pgp)

(defun mew-draft-sign-message (&optional arg)
  "Sign the entire draft. Input your passphrase."
  (interactive "P")
  (cond
   ((eq mew-draft-privacy-method 'pgp)
    (mew-pgp-sign-message arg))
   ((eq mew-draft-privacy-method 'smime)
    (mew-smime-sign-message arg))
   (t
    (message "'%s' is not supported" mew-draft-privacy-method))))

(defun mew-draft-encrypt-message ()
  "Encrypt the entire draft with PGP."
  (interactive)
  (cond
   ((eq mew-draft-privacy-method 'pgp)
    (mew-pgp-encrypt-message))
;;   ((eq mew-draft-privacy-method 'smime)
;;    (mew-smime-encrypt-message))
   (t
    (message "'%s' is not supported" mew-draft-privacy-method))))

(defun mew-draft-sign-encrypt-message (&optional arg)
  "Sign then encrypt the entire draft. Input your passphrase."
  (interactive "P")
  (cond
   ((eq mew-draft-privacy-method 'pgp)
    (mew-pgp-sign-encrypt-message arg))
;;   ((eq mew-draft-privacy-method 'smime)
;;    (mew-smime-sign-encrypt-message arg))
   (t
    (message "'%s' is not supported" mew-draft-privacy-method))))


(defun mew-draft-encrypt-sign-message (&optional arg)
  "Encrypt then sign the entire draft. Input your passphrase."
  (interactive "P")
  (cond
   ((eq mew-draft-privacy-method 'pgp)
    (mew-pgp-encrypt-sign-message arg))
;;   ((eq mew-draft-privacy-method 'smime)
;;    (mew-smime-encrypt-sign-message arg))
   (t
    (message "'%s' is not supported" mew-draft-privacy-method))))

(provide 'mew-draft)

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

;;; mew-draft.el ends here
