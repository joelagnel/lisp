;; remote.el version 2.6
;;
;; Module to do remote editing via rcp.  Assume .rhosts files are
;; set up properly on both machines. 
;; Modeled after ftp.el by MLY.PREP.AI.MIT.EDU
;;
;; Nick Tran
;; University of Minnesota
;; Summer 87
;;
;;; Synched up with: Not in FSF.

;; Almost complete rewrite.  Added minor mode support, better
;; defaults, rewrote find-remote-file, wrote read-remote-file-name,
;; insert-remote-file, find-file, find-alternate-remote-file,
;; get-remote-file-or-buffer, get-remote-buffer, process-wait,
;; remote-rcp-error.  Also general clean up, error handling, etc.
;; Eric Raible Wednesday Sept 5, 1988
;;
;; Automatically set major mode, added prefix arg support for most
;; file operations to toggle sense of remote editing.
;; Eric Raible Thursday October 6, 1988
;;
;; Manipulate buffer name more appropriately
;; Eric Raible Friday October 7, 1988
;;
;; For write-remote-file, allow default of file part of remote name.
;; Eric Raible Tuesday October 11, 1988

(defvar default-remote-host "navier:"
  "The host to use for remote file operations when none other is appropriate.")

(defvar track-default-remote-host t
  "Controls whether  default-remote-host  is changed after reading a remote file name.
When non-nil,  default-remote-host  will have the value of the last remote host read.")

(make-variable-buffer-local 'buffer-remote-file-name)
(set-default 'buffer-remote-file-name "")
(make-variable-buffer-local 'remote-editing)

(defvar rcp (cond ((file-exists-p "/bin/rcp") "/bin/rcp")
		  ((file-exists-p "/usr/bsd/rcp") "/usr/bsd/rcp")
		  (t "rcp")))

(if (assoc 'remote-editing minor-mode-alist)
    ()
  (setq minor-mode-alist (cons '(remote-editing " Remote") minor-mode-alist)))

(defun remote-editing (arg)
  "Toggle remote-editing mode.
With arg, turn on remote editing mode iff arg is positive, otherwise just toggle it.

In remote editing mode, the normal bindings for find-file,
find-file-read-only, find-alternate-file, save-buffer, write-file,
and insert-file are changed to operate on a remote system by default.

When remote editing, a prefix arg allows local file operations.  When not
remote editing, a prefix arg allows remote file operations.

It is assumed that .rhosts files are set up properly on both machines."
  (interactive "P")
  (setq remote-editing
	(if (null arg) (not remote-editing)
	  (> (prefix-numeric-value arg) 0)))
  (set-buffer-modified-p (buffer-modified-p))) ;No-op, but updates mode line.

(global-set-key "\C-xr" 'remote-editing)

;;;
;;; Macro used as front-end to normal file operation key bindings to decide between
;;; local and remote modes.  Automatically constructs doc string and includes prefix arg
;;; to temporarily toggle sense of remote-editing.
;;;
(defmacro def-local-or-remote (binding name remote local)
  (let ((r (symbol-name (eval remote)))
	(l (symbol-name (eval local))))
    (list 'progn
	  (list 'global-set-key binding (list 'quote name))
	  (list 'defun name '(arg)
		(concat "Call either " r " or " l ".
If remote-editing (which see), call " r ", else call " l ".

See also the documentation for " r " and " l ".")
		'(interactive "P")
		(list 'call-interactively
		      (list 'if '(xor remote-editing arg)
			    remote
			    local))))))

(def-local-or-remote "\C-x\C-f" find-local-or-remote-file           'find-remote-file           'find-file)
(def-local-or-remote "\C-x\C-r" find-local-or-remote-file-read-only 'find-remote-file-read-only 'find-file-read-only)
(def-local-or-remote "\C-x\C-v" find-alternate-local-or-remote-file 'find-alternate-remote-file 'find-alternate-file)
(def-local-or-remote "\C-x\C-s" save-local-or-remote-buffer         'save-remote-buffer         'save-buffer)
(def-local-or-remote "\C-x\C-w" write-local-or-remote-file          'write-remote-file          'write-file)
(def-local-or-remote "\C-xi"    insert-local-or-remote-file         'insert-remote-file         'insert-file)

(defun find-remote-file (host file)
  "Edit remote file HOST:FILE (using rcp).
This command is similiar to find-file, but uses rcp to read the file from
a remote machine.  Also see remote-editing."
  (interactive (read-remote-file-name "Find remote file"))
  (let ((buffer-or-file (get-remote-file-or-buffer host file "retrieve"))
	local-file)
    (if buffer-or-file
	(if (bufferp buffer-or-file)
	    (switch-to-buffer buffer-or-file)
	  (setq local-file buffer-or-file)
	  (let ((buf (generate-new-buffer
		      (concat host (file-name-nondirectory file)))))
	    (switch-to-buffer buf)
	    (if (not (file-exists-p local-file))
		(message "(New remote file)")
	      (insert-file-contents local-file)
	      (set-buffer-modified-p nil)
	      (delete-file local-file))
	    ;; dynamic binding for normal-mode
	    (let ((buffer-file-name (concat host file)))
	      (normal-mode)
	      (remote-editing 1)
	      (setq buffer-remote-file-name buffer-file-name
		    buffer-offer-save t)))))))

(defun find-remote-file-read-only ()
  "Edit remote file FILENAME, but mark buffer as read-only.
Also see find-remote-file and remote-editing."
  (interactive)
  (call-interactively 'find-remote-file)
  (setq buffer-read-only t))

(defun find-alternate-remote-file ()
  "Find alternate file using rcp.
This command is similiar to find-alternate-file, but uses rcp to read the file from
a remote machine.  Also see remote-editing."
  (interactive)
  (and (buffer-modified-p)
       (not buffer-read-only)
       (not (yes-or-no-p (format "Buffer %s is modified; kill anyway? "
				 (buffer-name))))
       (error "Aborted"))
  (let ((obuf (current-buffer))
	(oname (buffer-name)))
    (rename-buffer " **lose**")
    (unwind-protect
	(apply 'find-remote-file
	       (read-remote-file-name "Find remote alternate file"))
      (if (eq obuf (current-buffer))
	  (rename-buffer oname)
	(kill-buffer obuf)))))

(defun save-remote-buffer ()
  "Save a file using rcp.
This command is similiar to save-buffer, but uses rcp to write the file back
to a remote machine.  Also see remote-editing."
  (interactive)
  (if (buffer-modified-p)
      (if (zerop (length buffer-remote-file-name))
	  (call-interactively 'write-remote-file)
	(do-write-remote-file buffer-remote-file-name))
    (message "(No changes need to be saved)")))

(defun write-remote-file (host file)
  "Write a file HOST:FILE using rcp.
This command is similiar to write-file, but uses rcp to write the file back
to a remote machine.  Also see remote-editing."
  (interactive (read-remote-file-name "Write remote file" 'no-file-ok))
  (do-write-remote-file (concat host file)))

(defun insert-remote-file (host file)
  "Insert a remote file HOST:FILE using rcp.
This command is similiar to insert-file, but uses rcp to read the file from
a remote machine.  Also see remote-editing."
  (interactive (read-remote-file-name "Insert remote file"))
  (let ((f-or-b (get-remote-file-or-buffer host file "insert")))
    (if f-or-b
	(if (bufferp f-or-b)
	    (insert-buffer f-or-b)
	  (insert-file f-or-b)
	  (delete-file f-or-b)))))

;;;
;;; Internal routines
;;;

(defun do-write-remote-file (file)
  (let* ((temp (concat "/tmp/" (buffer-name)))
	 (output (save-excursion
		   (prog1 (set-buffer (get-buffer-create "*Rcp Output*"))
		     (erase-buffer))))
	 (cursor-in-echo-area t)
	 time)
    ;; write-file doesn't quite do it.
    (save-restriction
      (widen)
      (write-region (point-min) (point-max) temp nil 'no-message))
    (message "Sending %s..." file)
    (if (setq time (process-wait (start-process "rcp" output rcp temp file)))
	(progn
	  (if remote-editing
	      (let ((new-name (concat (host-part-only file)
				      (file-name-nondirectory (file-part-only file)))))
		(or (get-buffer new-name) (rename-buffer new-name))
		(set-buffer-modified-p nil)))
	  (setq buffer-remote-file-name file)
	  (message "%d bytes in %d seconds" (buffer-size) time)
	  (delete-file temp))
      (remote-rcp-error output buffer-remote-file-name "update"))))

(defun get-remote-file-or-buffer (host file message)
  "Return a remote file as either a buffer or a file.
If the file HOST:FILE already has been read in, return the buffer
that contains it; otherwise try and rcp the file to the local machine.
If successful, return the local file name."
  (let ((remote (concat host file))
	(temp (concat "/tmp/" (file-name-nondirectory file)))
	time)
    (if (string= file (file-name-directory file))
	(progn
	  (message "Remote directory listing not yet implemented")
	  nil)
      (or (get-remote-buffer remote)	  ;; already exists
	  (let* ((output (save-excursion
			   (prog1 (set-buffer (get-buffer-create "*Rcp Output*"))
			     (erase-buffer))))
		 (cursor-in-echo-area t))
	    (message "Retrieving %s..." remote)
	    (if (setq time (process-wait (start-process "rcp" output rcp remote temp)))
		(progn
		  (message "%d bytes in %d seconds"
			   (nth 7 (file-attributes temp)) time)
		  temp)
	      (remote-rcp-error output remote message)))))))

(defun get-remote-buffer (name)
  (save-window-excursion
    (let ((buffers (buffer-list)) found)
      (while (and (not found) buffers)
	(set-buffer (car buffers))
	(if (string= name buffer-remote-file-name)
	    (setq found (car buffers)))
	(setq buffers (cdr buffers)))
      found)))

(defun read-remote-file-name (prompt &optional no-file-ok)
  "Read a remote file specification, and return list (host file).
Prompting with PROMPT, read a string of the form host:file.  The default
value is derived from the remote file name, or if there is none, then
from the global default (default-remote-host)."
  (let* ((host (or (host-part-only buffer-remote-file-name)
		   default-remote-host))
	 (result (concat host (file-name-directory
			       (or (file-part-only buffer-remote-file-name)
				   ""))))
	 (prompt (concat prompt " (host:file): "))
	 file)
    (setq result (read-no-blanks-input prompt result))
    (while (not (string-match (if no-file-ok
				  ".+:"
				".+:.+")
			      result))
      (setq result (read-no-blanks-input prompt result)))
    (setq host (host-part-only result)
	  file (file-part-only result))
    (and track-default-remote-host
	 (setq default-remote-host host))
    (list host
	  (if (or (null file) (string= file (file-name-directory file)))
	      (concat file (or (if (not (string= buffer-remote-file-name ""))
				   (file-name-nondirectory
				    (file-part-only buffer-remote-file-name)))
			       (file-part-only (buffer-name))
			       (buffer-name)))
	    file))))

(defun host-part-only (name)
  (if (string-match ".+:" name)
      (substring name 0 (match-end 0))))

(defun file-part-only (name)
  (if (string-match ".+:\\(.+\\)" name)
      (substring name (match-beginning 1) (match-end 1))))

(defun xor (a b)
  (eq (null a) (not (null b))))

(defun process-wait (proc)
  (let ((time 0))
    (while (eq (process-status proc) 'run)
      (setq time (1+ time))
      (sleep-for 1))
    (if (and (eq (process-status proc) 'exit)
	     (eq (process-exit-status proc) 0))
	time
      nil)))

(defun remote-rcp-error (buffer file-name message)
  (save-window-excursion
    (switch-to-buffer buffer)
    (delete-other-windows)
    (goto-char 1)
    (insert (format "Unable to %s %s\n\n" message file-name))
    (goto-char (point-max))
    (message "Hit any character to continue")
    (read-char)
    (bury-buffer buffer)))
