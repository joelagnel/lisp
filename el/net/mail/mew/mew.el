;;; mew.el --- Messaging in the Emacs World

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1994
;; Revised: Feb  9, 2005

;;; Commentary:

;; The updated version is available from:
;;	ftp://ftp.Mew.org/pub/Mew/mew-current.tar.gz
;;	http://www.Mew.org/
;;
;; See info for configuring a site file/.emacs/.mew.el.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mew version
;;;

(defconst mew-version-number "4.2"
  "Version number for this version of Mew.")
(defconst mew-version (format "Mew version %s" mew-version-number)
  "Version string for this version of Mew.")
(provide 'mew)
(require 'mew-const)
(require 'mew-blvs)
(require 'mew-func)
(require 'mew-vars)
(require 'mew-vars2)
(cond
 ((eq system-type 'windows-nt)
  (require 'mew-win32))
 ((eq system-type 'darwin)
  (require 'mew-darwin))
 (t
  (require 'mew-unix)))

(eval-when-compile
  (unless (fboundp 'mew-scan-setup)
    (require 'mew-scan))
  (mew-scan-setup))

(defun mew-version-show ()
  "Show mew-version in minibuffer."
  (interactive)
  (message "%s" mew-version))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; For developers
;;;

(defvar mew-debug nil
  "'decode, 'encode, 'net, 'pgp, 'thread, 'sort, t for all.")
(defsubst mew-debug (category)
  (or (eq mew-debug t) (eq mew-debug category)))

(defvar mew-profile nil)
(defvar mew-profile-functions-list nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bootstrap
;;;

(defvar mew-init-p nil)

;;;###autoload
(defun mew (&optional arg)
  "Execute Mew first unless Mew is running. And retrieve arrived
messages or just visit to the default folder.

'proto' is determined by 'mew-proto' and 'mew-input-case'.  

If 'proto' is '+' (ie a local folder), a mailbox is determined
according to 'mew-mailbox-type'. Otherwise (ie a remote folder), an
appropriate protocol to retrieve messages is chosen according to
'proto'.

If 'mew-auto-get' is 't', arrived messages are asynchronously fetched
and listed up in Summary mode.

'mew-auto-get' is 'nil', just visit to the folder determined by
'proto'.

When executed with '\\[universal-argument]', 'mew-auto-get' is
considered reversed."
  (interactive "P")
  (mew-window-push)
  (unless mew-init-p (mew-init))
  (let* ((auto (if arg (not mew-auto-get) mew-auto-get))
	 (case mew-case-input)
	 (proto (mew-proto case))
	 inbox case:inbox)
    (if auto
	(mew-summary-scan-boot proto case)
      ;; see also mew-summary-retrieve
      (setq inbox (mew-proto-inbox-folder proto case))
      (setq case:inbox (mew-case-folder case inbox))
      (mew-summary-visit-folder case:inbox 'goend))
    (setq mew-inbox-window (current-window-configuration))))

;;;###autoload
(defun mew-send (&optional to cc subject)
  "Execute Mew then prepare a draft. This may be used as library
function."
  (interactive)
  (mew-current-set-window-config)
  (unless mew-init-p (mew-init))
  (mew-summary-send to cc subject))

;;;###autoload
(defun mew-user-agent-compose (&optional to subject other-headers continue
                                             switch-function yank-action
                                             send-actions)
  "Set up message composition draft with Mew.
This is 'mail-user-agent' entry point to Mew.

The optional arguments TO and SUBJECT specify recipients and the
initial Subject field, respectively.

OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

A Draft buffer is prepared according to SWITCH-FUNCTION.

CONTINUE, YANK-ACTION and SEND-ACTIONS are ignored."
  (unless mew-init-p (mew-init))
  (let* ((draft (mew-folder-new-message mew-draft-folder))
	 (attachdir (mew-attachdir draft)))
    (mew-current-set-window-config)
    (mew-window-configure 'draft)
    (mew-summary-prepare-draft
     (mew-draft-find-and-switch draft switch-function)
     (mew-delete-directory-recursively attachdir)
     (mew-draft-header subject nil to nil nil nil nil other-headers)
     (mew-draft-mode)
     (run-hooks 'mew-draft-mode-newdraft-hook))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for boot time
;;;

(defun mew-init ()
  (with-temp-buffer
    (run-hooks 'mew-env-hook)
    (load mew-rc-file 'no-err 'no-msg)
    (switch-to-buffer (current-buffer))
    (mew-window-configure '(1 0)) ;; Using mew-mode-name-message
    (mew-hello)
    (message "Setting up Mew world...")
    (mew-set-environment)
    (run-hooks 'mew-init-hook)
    (mew-status-update t)
    (mew-passwd-setup)
    (mew-highlight-timer-setup)
    (setq mew-init-p t)
    (message "Setting up Mew world...done")))

(defun mew-set-environment (&optional no-dir)
  (let (error-message)
    (condition-case nil
	(progn
	  ;; sanity check
	  (cond
	   ((string-match "^\\(18\\|19\\)" emacs-version)
	    (setq error-message "Not support Emacs 18/19 nor Mule 1\n")
	    (error "")))
	  ;; initializing
	  (or no-dir (mew-buffers-init))
	  (or no-dir (mew-temp-dir-init))
	  (mew-mark-init)
	  (mew-config-init)
	  (mew-rotate-log-files mew-smtp-log-file)
	  (mew-rotate-log-files mew-nntp-log-file)
	  (mew-rotate-log-files mew-refile-log-file))
      (error
       (set-buffer (generate-new-buffer mew-buffer-debug))
       (goto-char (point-max))
       (insert "\n\nMew errors:\n\n")
       (and error-message (insert error-message))
       (set-buffer-modified-p nil)
       (setq buffer-read-only t)
       ;; cause an error again
       (error "Mew found some errors above")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Status update
;;;

(defun mew-folder-init (folders)
  (let (folder dir)
    (while folders
      (setq folder (mew-canonicalize-folder (car folders)))
      (setq folders (cdr folders))
      (setq dir (mew-expand-folder folder))
      (unless (file-exists-p dir)
	(mew-make-directory dir)
	(mew-local-folder-insert folder)
	(message "%s was created" dir))
      (setq dir (file-chase-links dir))
      (if (file-exists-p dir)
	  (if (/= mew-folder-mode (mew-file-get-mode dir))
	      (set-file-modes dir mew-folder-mode))))))

(defun mew-status-update (arg)
  "Read Addrbook and update its information. 
If executed with '\\[universal-argument]', information about local folders is 
also updated in addition to that of Addrbook. 
If executed with '1', information about local folders is updated.
If executed with '2', information about newsgroups is updated.
If executed with '3', information about IMAP folders is updated."
  (interactive "P")
  (message "Updating status...")
  (cond
   ((eq arg 1)
    (mew-local-update (interactive-p)))
   ((eq arg 2)
    (mew-nntp-update))
   ((eq arg 3)
    (mew-imap-update))
   (t
    (if (interactive-p) (mew-set-environment 'no-dir))
    ;; These two must be before mew-local-update
    (mew-config-setup)
    (mew-regex-setup)
    (if arg (mew-local-update (interactive-p))) ;; arg == '(4)
    (mew-folder-init mew-basic-folders)
    (mew-folder-init mew-inbox-folders)
    (mew-folder-init mew-queue-folders)
    (mew-folder-init mew-postq-folders)
    (mew-refile-setup)
    (mew-addrbook-setup)
    (mew-scan-setup)
    (mew-pgp-setup)
    (mew-smime-setup)
    (mew-ssh-setup)
    (mew-ssl-setup)
    (mew-net-setup)
    (mew-thread-setup)
    (mew-theme-setup)
    (mew-biff-setup)
    (mew-ct-setup)
    (mew-nmz-clean-up)))
  (unless (or (eq arg 2) (eq arg 3))
    (message "Updating status...done")))

(defvar mew-mime-content-type-list nil
  "Candidate of Content-Type: when CT: is changed in draft buffer.")

(defun mew-ct-setup ()
  (let ((cts mew-mime-content-type) ct)
    (while cts
      (setq ct (car (car cts)))
      (setq cts (cdr cts))
      (if (and (stringp ct) (not (string-match "/$" ct)))
	  (setq mew-mime-content-type-list
		(cons (capitalize ct) mew-mime-content-type-list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Current status
;;;

(defsubst mew-frame-id (&optional frame)
  (if window-system
      (let ((fram (or frame (selected-frame))))
	(concat "mew-current-info-"
		(cdr (assq 'window-id (frame-parameters fram)))))
    "mew-current-info-no-window"))

(defvar mew-current-info-list '("fld" "msg" "part" "window"))

(mew-info-defun "mew-current-" mew-current-info-list)

(defun mew-current-set (fld msg part)
  (mew-current-set-fld (mew-frame-id) fld)
  (mew-current-set-msg (mew-frame-id) msg)
  (mew-current-set-part (mew-frame-id) part))

(defun mew-current-clean-up ()
  (if window-system
      (let ((frams (frame-list)))
	(while frams
	  (mew-info-clean-up
	   (concat "mew-current-info-" (mew-frame-id (car frams))))
	  (setq frams (cdr frams))))
    (mew-info-clean-up "mew-current-info-no-window")))

(defsubst mew-current-set-window-config ()
  (mew-current-set-window (mew-frame-id) (current-window-configuration)))

(defsubst mew-current-get-window-config ()
  (let ((win (mew-current-get-window (mew-frame-id))))
    (unless (window-configuration-p win)
      (setq win mew-inbox-window))
    (if win (set-window-configuration win))
    (mew-current-set-window (mew-frame-id) nil)
    (mew-summary-toolbar-update)
    (mew-redraw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Window configuration stack
;;;

(defvar mew-inbox-window nil)
(defvar mew-window-stack nil)

(defun mew-window-clean-up ()
  (setq mew-window-stack nil))

(defun mew-window-push ()
  (let ((frame (selected-frame))
	(config (current-window-configuration)))
    (setq mew-window-stack (cons (cons frame config) mew-window-stack))))

(defun mew-window-pop ()
  (let* ((frame (selected-frame))
	 (assoc (assoc frame mew-window-stack)))
    (if (and assoc (window-configuration-p (cdr assoc)))
	(set-window-configuration (cdr assoc))
      (switch-to-buffer (get-buffer-create mew-window-home-buffer)))
    (setq mew-window-stack (delq assoc mew-window-stack))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Window configuration
;;;
(defun mew-delete-other-window ()
  (unless (one-window-p)
    (let ((owin (selected-window))
	  wins ret)
      (walk-windows
       (lambda (win)
	 (set-buffer (window-buffer win))
	 (unless (or (eq owin win)
		     (string-match "^mew-" (symbol-name major-mode)))
	   (setq wins (cons win wins))))
       nil)
      (while wins
	(unless (one-window-p)
	  (setq ret t)
	  (delete-window (car wins)))
	(setq wins (cdr wins)))
      (select-window owin)
      ret)))

(defun mew-window-configure (action)
  "Configure windows according to ACTION.
ACTION should be symbol or a list of two numbers.
Predefined symbol is 'summary, 'message, and 'draft.
They are used as a key of 'mew-window-configuration to get
a list of two numbers.

Two numbers means the ratio of the upper window and 
the lower window (i.e. the window of Message buffer).

If Message buffer does not exist, it will be created. If the height of
the lower window is not zero, switch to the buffer."
  (let* ((windows
	  (if (listp action) 
	      action
	    (car (cdr (assq action mew-window-configuration)))))
	 (msgbuf  (mew-buffer-message))
	 (obufwin (get-buffer-window (current-buffer)))
	 (msgwin  (get-buffer-window msgbuf))
	 (height nil) (winsum nil) (sum-height 0) (msg-height 0))
    (setq height (+ (if obufwin (window-height obufwin) 0)
		    (if msgwin  (window-height msgwin)  0)))
    (cond
     ((<= height (* 2 window-min-height))
      ;; Delete other windows and use full emacs window.
      (delete-other-windows)
      (setq height (window-height (selected-window))))
     ((and mew-use-full-window (mew-delete-other-window))
      ;; delete windows all but Mew's one.
      (setq height (+ (if obufwin (window-height obufwin) 0)
		      (if msgwin  (window-height msgwin)  0)))))
    ;;
    (if (get-buffer msgbuf)
	(delete-windows-on msgbuf)
      (save-excursion
	(set-buffer (get-buffer-create msgbuf))
	;; "truncate?" is asked in Message mode.
	;; so set the same toolbar as Summary mode
	(mew-summary-toolbar-update)
	(mew-message-mode)))
    ;;
    (setq winsum (apply '+ windows))
    (unless (zerop (nth 0 windows))
      (setq sum-height (max window-min-height
			    (/ (* height (nth 0 windows)) winsum))))
    (if (and (eq action 'message) (= (% sum-height 2) 1)) 
	(setq sum-height (1+ sum-height)))
    (unless (zerop (nth 1 windows))
      (setq msg-height (max window-min-height (- height sum-height))))
    (setq height (+ sum-height msg-height))
    ;;
    (unless (zerop msg-height)
      (split-window nil sum-height)
      (other-window 1)
      (switch-to-buffer msgbuf 'norecord)
      (mew-set-buffer-cs mew-cs-m17n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Message buffer
;;;

(defun mew-window-number ()
  (let ((x (car (mew-window-edges (selected-window))))
	(i 1))
    (walk-windows
     (lambda (win)
       (when (save-excursion
	       (set-buffer (window-buffer win))
	       (mew-summary-or-virtual-p))
	 (if (< (car (mew-window-edges win)) x)
	     (setq i (1+ i)))))
     'nominibuf)
    i))

(defun mew-buffer-message ()
  (let* ((me (selected-frame))
	 (frames (frame-list))
	 (len (length frames))
	 (n (mew-window-number))
	 (i 0))
    (catch 'loop
      (while frames
	(if (equal me (car frames))
	    (throw 'loop i))
	(setq i (1+ i))
	(setq frames (cdr frames))))
    (setq i (- len i 1))
    (if (= n 1)
	(format "%s%d" mew-buffer-message i)
      (format "%s%d<%d>" mew-buffer-message i n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffers
;;;

(defvar mew-buffers nil)

(defun mew-buffers-init ()
  (setq mew-buffers nil))

(defun mew-buffers-setup (folder)
  (mew-addq mew-buffers folder))

(defun mew-buffers-bury ()
  (let ((buffers mew-buffers))
    (while buffers
      (if (get-buffer (car buffers))
	  (bury-buffer (car buffers)))
      (setq buffers (cdr buffers)))))

(defun mew-buffers-clean-up ()
  (while mew-buffers
    (mew-remove-buffer (car mew-buffers))
    (setq mew-buffers (cdr mew-buffers)))
  (mew-buffers-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Temporary directory
;;;

(defvar mew-temp-dir nil)  ;; the default is "/tmp/user_name_uniq"
(defvar mew-temp-file nil) ;; the default is "/tmp/user_name_uniq/mew"

(defun mew-temp-dir-init ()
  "Setting temporary directory for Mew.
mew-temp-file must be local and readable for the user only
for privacy/speed reasons."
  (setq mew-temp-dir (make-temp-name mew-temp-file-initial))
  (mew-make-directory mew-temp-dir)
  (set-file-modes mew-temp-dir mew-folder-mode)
  (setq mew-temp-file (expand-file-name "mew" mew-temp-dir))
  (add-hook 'kill-emacs-hook 'mew-temp-dir-clean-up))

(defun mew-temp-dir-clean-up ()
  "A function to remove Mew's temporary directory recursively. 
It is typically called by kill-emacs-hook."
  (remove-hook 'kill-emacs-hook 'mew-temp-dir-clean-up)
  (if (and mew-temp-dir (file-exists-p mew-temp-dir))
      (mew-delete-directory-recursively mew-temp-dir))
  (setq mew-temp-dir nil)
  (setq mew-temp-file nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Regular expressions
;;;

(defvar mew-regex-msg-show-list nil)
(defvar mew-regex-msg-show nil)
(defvar mew-regex-msg-or-part nil)
(defvar mew-regex-msg-review nil)
(defvar mew-regex-msg-delete nil)
(defvar mew-regex-attach-beg nil)
(defvar mew-regex-attach-end nil)
(defvar mew-regex-my-address-list nil)
(defvar mew-regex-ignore-folders nil)
(defvar mew-regex-thread-separator nil)

(defsubst mew-mark-regex (mark)
  (concat "^" (regexp-quote (char-to-string mark))))

(defsubst mew-mark-list-regex (mark-list)
  (concat "^" "[" (mapconcat 'char-to-string mark-list "") "]"))

(defun mew-regex-setup-msg (lst)
  (setq mew-regex-msg-show-list lst) 
  (setq mew-regex-msg-show (mew-mark-list-regex lst))
  (setq mew-regex-msg-or-part (concat mew-regex-msg-show "\\|" mew-regex-part)))

(defun mew-regex-setup ()
  (setq mew-eoh (format "^\\(%s\\|\\)$" (regexp-quote mew-header-separator)))
  (mew-regex-setup-msg mew-mark-show-list)
  (setq mew-regex-msg-review (mew-mark-regex mew-mark-review))
  (setq mew-regex-msg-delete (mew-mark-regex mew-mark-delete))
  (setq mew-regex-thread-separator
	(concat "^" (regexp-quote mew-thread-separator)))
  (setq mew-regex-attach-beg
	(concat "^" mew-draft-attach-boundary-beg "$"))
  (setq mew-regex-attach-end
	(concat "^" mew-draft-attach-boundary-end "$"))
  (setq mew-regex-my-address-list (mew-get-my-address-regex-list))
  (setq mew-regex-ignore-folders
	(mapconcat
	 'mew-folder-regex
	 (mew-uniq-list
	  (append
	   mew-basic-folders
	   mew-inbox-folders
	   mew-queue-folders
	   mew-postq-folders
	   (list
	    mew-friend-folder
	    mew-attach-folder)))
	 "\\|"))
  (unless mew-range-list
    (setq mew-range-list
	  `((,mew-queue-folders ,mew-range-str-all)
	    (, mew-postq-folders ,mew-range-str-all)
	    ((,mew-draft-folder) ,mew-range-str-all)
	    (t ,mew-range-str-update)))))

(defun mew-summary-toggle-mark-regex ()
  (interactive)
  (let (lst)
    (if (equal mew-regex-msg-show-list mew-mark-show-list)
	(setq lst mew-mark-show-list2)
      (setq lst mew-mark-show-list))
    (mew-regex-setup-msg lst)
    (message "Target marks are: %s" (mapconcat (lambda (x) (message "'%c'" x)) lst ", "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Theme
;;;

(defun mew-theme-setup ()
  (interactive)
  (if mew-theme-file (load mew-theme-file 'no-err 'no-msg))
  (when mew-gemacs-p
    (put-text-property 0 (length mew-end-of-message-string)
		       'face 'mew-face-eof-message
		       mew-end-of-message-string)
    (put-text-property 0 (length mew-end-of-part-string)
		       'face 'mew-face-eof-part
		       mew-end-of-part-string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tear down
;;;

(defun mew-bury-buffer (&optional buf)
  (bury-buffer buf)
  (delete-windows-on buf t))

(defun mew-kill-buffer (&optional buf)
  "Erase the current buffer."
  (interactive)
  (let* ((buf (or buf (current-buffer)))
	 (folder (if (bufferp buf) (buffer-name buf) buf))
	 obuf)
    (if (get-buffer buf)
	(save-excursion
	  (set-buffer buf)
	  (when (mew-summary-or-virtual-p)
	    (mew-summary-kill-subprocess)
	    (setq obuf (mew-local-buffer-name folder))
	    ;; xxx should kill pop's buffer...
	    (mew-remove-buffer obuf))
	  (mew-overlay-delete-buffer)))
    (mew-remove-buffer buf)))

(defun mew-buffer-clean-up (regex &optional func)
  (unless func (setq func 'mew-kill-buffer))
  (let ((bufs (mew-buffer-list regex)))
    (while bufs
      (funcall func (car bufs))
      (setq bufs (cdr bufs)))))

(defsubst mew-quit-toolbar-update ()
  (mew-redraw) ;; due to mouse-face bug
  (if (fboundp 'redraw-frame) ;; for BOW
      (redraw-frame (selected-frame)))) ;; update toolbar

(defun mew-summary-suspend ()
  "Suspend Mew then switch to another buffer. All buffers of 
Mew remain, so you can resume with buffer operations."
  (interactive)
  (mew-buffer-clean-up
   (concat "^" (regexp-quote mew-buffer-message))
   'mew-bury-buffer)
  (mew-buffers-bury)
  (mew-window-pop)
  (mew-quit-toolbar-update)
  (run-hooks 'mew-suspend-hook))

(defun mew-summary-quit ()
  "Quit Mew. All buffers of Mew are erased."
  (interactive)
  (when (y-or-n-p "Quit Mew? ")
    ;; killing buffers
    (mew-cache-clean-up)
    (mew-buffer-clean-up (concat "^" (regexp-quote mew-buffer-message)))
    (mew-buffer-clean-up (mew-folder-regex mew-draft-folder)) ;; +draft/*
    (mew-buffer-clean-up mew-buffer-regex) ;; other buffers
    ;;
    (mew-sinfo-clean-up)
    (mew-buffers-clean-up) ;; Summary mode and Virtual mode
    ;;
    (mew-temp-dir-clean-up)
    ;;
    (run-hooks 'mew-quit-hook)
    ;;
    ;; lastly, clean up variables
    ;;
    (mew-folder-clean-up)
    (mew-refile-clean-up)
    (mew-current-clean-up)
    (mew-addrbook-clean-up)
    (mew-passwd-clean-up)
    (mew-highlight-timer-clean-up)
    (mew-net-clean-up)
    (mew-biff-clean-up)
    (mew-nmz-clean-up)
    ;;
    (mew-window-pop)
    (mew-window-clean-up)
    (mew-quit-toolbar-update)
    ;;
    (setq mew-init-p nil)
    (message nil))) ;; just clear

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load Mew libraries
;;;

(require 'mew-local)
(require 'mew-addrbook)
(require 'mew-complete)
(require 'mew-minibuf)
(require 'mew-cache)
(require 'mew-encode)
(require 'mew-decode)
(require 'mew-edit)
(require 'mew-mime)
(require 'mew-mark)
(require 'mew-exec)
(require 'mew-header)
(require 'mew-pgp)
(require 'mew-smime)
(require 'mew-bq)
(require 'mew-syntax)
(require 'mew-scan)
(require 'mew-pick)
(require 'mew-summary)
(require 'mew-summary2)
(require 'mew-summary3)
(require 'mew-summary4)
(require 'mew-virtual)
(require 'mew-thread)
(require 'mew-message)
(require 'mew-draft)
(require 'mew-attach)
(require 'mew-demo)
(require 'mew-refile)
(require 'mew-ext)
(require 'mew-fib)
(require 'mew-sort)
(require 'mew-highlight)
(require 'mew-net)
(require 'mew-ssh)
(require 'mew-ssl)
(require 'mew-smtp)
(require 'mew-pop)
(require 'mew-nntp)
(require 'mew-nntp2)
(require 'mew-imap)
(require 'mew-imap2)
(require 'mew-config)
(require 'mew-auth)
(require 'mew-nmz)

;;; Copyright Notice:

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

;;; mew.el ends here
