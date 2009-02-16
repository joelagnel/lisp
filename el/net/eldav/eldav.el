;;; eldav.el --- Yet Another WebDAV interface for Emacsen.

;; Copyright (C) 2002 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi  <teranisi@gohome.org>
;; Keywords: WebDAV

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commenty:
;;
;; The w3 package (url) already provides WebDAV interface for Emacsen.
;; But w3 is too big for non-w3 users such as emacs-w3m users.
;;
;; This program currently uses
;; `nd - Tiny little command line WebDAV interface' as an WebDAV
;; backend program. It is available at: http://www.gohome.org/nd/
;;
;;
;; Usage:
;; Add following line to your .emacs.
;;
;; (require 'eldav)
;;
;; If you want to use proxy server, following setting is also needed.
;;
;; (setq eldav-proxy "http://your.proxy.server:8080")
;;
;; Then you can access WebDAV files by specifying magic file name like:
;;
;; :http://your.webdav.server/path/to/file/name
;;
;; (Add preceding ':' to the WebDAV URL.)
;;
;; See http://www.gohome.org/eldav for more details.
;;

(eval-when-compile
  (require 'cl))

;; For make-temp-file
(eval-and-compile
  (or (fboundp 'make-temp-file)
      (require 'poe)))

;; For non-mule XEmacs.
(if (and (featurep 'xemacs)
	 (not (featurep 'mule)))
    (require 'poem))

(require 'timezone)

(defmacro eldav-static-if (cond then &rest else)
  "Like `if', except that it evaluates COND at compile-time."
  (if (eval cond) then (` (progn  (,@ else)))))
(put 'eldav-static-if 'lisp-indent-function 2)

(defgroup eldav nil
  "Accessing remote files and directories using WebDAV."
  :group 'files
  :prefix "eldav-")

(defcustom eldav-debug nil
  "If non-nil, process string is inserted to the debug buffer."
  :type 'boolean
  :group 'eldav)

(defcustom eldav-temporal-directory temporary-file-directory
  "Name of temporal directory."
  :group 'eldav
  :type 'directory)

(defcustom eldav-proxy nil
  "URL of the proxy server."
  :group 'eldav
  :type 'string)

(defcustom eldav-no-proxy-url-regexp nil
  "Regexp of URLs which you do not want to use proxy server."
  :group 'eldav
  :type 'regexp)

(defcustom eldav-lock-identifier (or (and (boundp 'user-mail-address)
					  user-mail-address
					  (> (length user-mail-address) 0)
					  (concat "mailto:"
						  user-mail-address))
				     (and (getenv "USER")
					  (concat "mailto:"
						  (getenv "USER")
						  "@" (system-name)))
				     (system-name))
  "URL used as contact information when creating locks in DAV.
This will be used as the contents of the DAV:owner/DAV:href tag to
identify the owner of a LOCK when requesting it.  This will be shown
to other users when the DAV:lockdiscovery property is requested, so
make sure you are comfortable with it leaking to the outside world."
  :group 'eldav
  :type 'string)

(defcustom eldav-url-coding-system (if (and (fboundp 'coding-system-p)
					    (coding-system-p 'utf-8))
				       'utf-8)
  "Coding system for encoding URL string."
  :group 'eldav
  :type 'coding-system)

(defcustom eldav-auto-save 0
  "If 1, allow DAV files to be auto-saved.
If 0, inhibit auto-saving of DAV files.
Don't use any other value."
  :group 'eldav
  :type '(choice (const :tag "Suppress" 0)
		 (const :tag "Allow" 1)))

(defcustom eldav-process-connection-type
  (not (and (featurep 'xemacs)
	    (string-match "solaris" system-configuration)))
  "Process connection type for command execution."
  :group 'eldav
  :type 'boolean)

(defcustom eldav-executable-property-namespace "http://apache.org/dav/props/"
  "The namespace for `executable' property."
  :group 'eldav
  :type 'string)

(defcustom eldav-use-vc nil
  "VC is used if non-nil."
  :group 'eldav
  :type 'boolean)

(defvar eldav-inode-hashtable (make-vector 97 0)
  "Hash table for storing file names and their \"inode numbers\".")
(defvar eldav-inode-max 0
  "Max of the inode number.")

(defvar eldav-inhibit-propfind-cache nil
  "A switch to inhibit propfind cache.")

(defvar eldav-last-propfind-result nil
  "A cache to save the last propfind result.")

(defvar eldav-running-process nil)
(make-variable-buffer-local 'eldav-running-process)
(defvar eldav-process-auth-required nil)
(make-variable-buffer-local 'eldav-process-auth-required)
(defvar eldav-process-url nil)
(make-variable-buffer-local 'eldav-process-url)
(defvar eldav-process-user nil)
(make-variable-buffer-local 'eldav-process-user)
(defvar eldav-process-realm nil)
(make-variable-buffer-local 'eldav-process-realm)
(defvar eldav-process-passwd nil)
(make-variable-buffer-local 'eldav-process-passwd)
(defvar eldav-process-proxy-user nil)
(make-variable-buffer-local 'eldav-process-proxy-user)
(defvar eldav-process-proxy-passwd nil)
(make-variable-buffer-local 'eldav-process-proxy-passwd)
(defvar eldav-process-proxy-realm nil)
(make-variable-buffer-local 'eldav-process-proxy-realm)

(defvar eldav-debug-buffer nil)
(defun eldav-debug (string)
  "Insert STRING to the debug buffer."
  (when eldav-debug
    (if (or (null eldav-debug-buffer)
	    (not (bufferp eldav-debug-buffer))
	    (not (buffer-live-p eldav-debug-buffer)))
	(setq eldav-debug-buffer (get-buffer-create "*Debug eldav*")))
    (with-current-buffer eldav-debug-buffer
      (goto-char (point-max))
      (insert string))))

(defun eldav-url-decode-string (str &optional coding)
  (let ((start 0)
	(buf))
    (while (string-match "+\\|%\\(0D%0A\\|\\([0-9a-fA-F][0-9a-fA-F]\\)\\)"
			 str start)
      (push (substring str start (match-beginning 0)) buf)
      (push (cond
	     ((match-beginning 2)
	      (string (string-to-number (match-string 2 str) 16)))
	     ((match-beginning 1) "\n")
	     (t " "))
	    buf)
      (setq start (match-end 0)))
    (decode-coding-string
     (apply 'concat (nreverse (cons (substring str start) buf)))
     (or coding
	 eldav-url-coding-system
	 'undecided))))

(defun eldav-url-encode-string (str &optional coding)
  (apply (function concat)
	 (mapcar
	  (lambda (ch)
	    (cond
	     ((eq ch ?\n)		; newline
	      "%0D%0A")
	     ((string-match "[-a-zA-Z0-9_:/.]" (char-to-string ch)) ; xxx?
	      (char-to-string ch))	; printable
	     ((char-equal ch ?\x20)	; space
	      "+")
	     (t
	      (format "%%%02x" ch))))	; escape
	  ;; Coerce a string to a list of chars.
	  (append (encode-coding-string (or str "")
					(or coding
					    eldav-url-coding-system
					    'undecided))
		  nil))))

(defun eldav-get-hash-entry (key tbl)
  "Return the value associated with KEY in HASHTABLE."
  (let ((sym (intern-soft key tbl)))
    (and sym (get sym 'val))))

(defun eldav-put-hash-entry (key val tbl)
  "Record an association between KEY and VALUE in HASHTABLE."
  (let ((sym (intern key tbl)))
    (put sym 'val val)
    (put sym 'key key)))

(defun eldav-file-inode-number (name)
  (let ((inode (eldav-get-hash-entry name eldav-inode-hashtable)))
    (or inode
	(progn (eldav-put-hash-entry name (incf eldav-inode-max)
				     eldav-inode-hashtable)
	       eldav-inode-max))))

(defun eldav-url-server-root (url)
  "Extract a server root from URL."
  (if (string-match "\\`[^:/?#]+://\\([^/?#]+\\)" url)
      (downcase (match-string 1 url))
    url))

(defun eldav-file-name-server-root (name)
  (substring name
	     0
	     (- (length name)
		(length (eldav-file-name-server-path name)))))

(defun eldav-file-name-server-path (name)
  (let ((url (eldav-file-name-url name))
	server-path)
    (when (string-match (concat "^" (regexp-quote
				     (concat "http://"
					     (eldav-url-server-root url))))
			url)
      (setq server-path (eldav-url-decode-string
			 (substring url (match-end 0))))
      (if (zerop (length server-path)) "/" server-path))))

(defun eldav-lockinfo-list (url)
  "Lock information list of URL."
  (let ((eldav-inhibit-propfind-cache t))
    (if (string-match "^.*/$" url)
	(setq url (substring url 0 (- (length url) 1))))
    (cdr (assq 'lock-list (cdr (car (ignore-errors (eldav-propfind url))))))))

(defun eldav-lockinfo-token (lockinfo)
  "Return token of LOCKINFO."
  (cadr (assq 'token lockinfo)))

(defun eldav-lockinfo-owner (lockinfo)
  "Return token of LOCKINFO."
  (cadr (assq 'owner-href lockinfo)))

(defun eldav-lockinfo-timeout (lockinfo)
  "Return timeout of LOCKINFO."
  (let ((timeout (cadr (assq 'timeout lockinfo))))
    (cond
     ((string= timeout "Infinite")
      'infinite)
     ((string-match "Second-\\([0-9]+\\)" timeout)
      (string-to-number (match-string 1 timeout)))
     (t timeout))))

(defun eldav-lockinfo-scope (lockinfo)
  "Return scope of LOCKINFO."
  (cadr (assq 'scope lockinfo)))

(defun eldav-lockinfo-list-search (name value list)
  "Return the matched lockinfo list which matches its NAME and VALUE.
LIST is the lockinfo list."
  (let (match)
    (while list
      (when (equal (apply (intern (concat "eldav-lockinfo-"
					  (symbol-name name)))
			  (list (car list)))
		   value)
	(setq match (cons (car list) match)))
      (setq list (cdr list)))
    match))

;;; file-name prefix definition.
(defconst eldav-file-name-prefix ":http://")
(defconst eldav-file-name-regexp (concat "^"
					 (regexp-quote eldav-file-name-prefix)
					 "\\([^/]+\\)"))
(add-to-list 'file-name-handler-alist
	     (cons eldav-file-name-regexp 'eldav-handler))

(defun eldav-file-name-url (name)
  (eldav-url-encode-string
   (substring name 1)))

;;; WebDAV backend using `nd'.
;;
(defvar eldav-program-name "nd")
(defvar eldav-program-arguments '("-S"))
(defvar eldav-process-authinfo-alist nil)
(defvar eldav-url-realm-alist nil "An alist of URL and its REALM.")

;; eldav-process-authinfo-alist has an association list as below format.
;; (("root1" ("realm11" ("user11" . "pass11")
;;                      ("user12" . "pass12"))
;;           ("realm12" ("user13" . "pass13")))
;;  ("root2" ("realm21" ("user21" . "pass21"))))
(defun eldav-process-set-authinfo (url realm username password)
  "Add an authinfo cache entry which consists of URL, REALM, USERNAME, PASSWORD.
If PASSWORD is nil, remove authinfo entry if exists."
  (let (x y z (root (eldav-url-server-root url)))
    (if (setq x (assoc root eldav-process-authinfo-alist))
	(if (setq y (assoc realm x))
	    (if (setq z (assoc username (cdr y)))
		(if (null password)
		    ;; Delete entry.
		    (setcdr y (delq z (cdr y)))
		  ;; Change a password only.
		  (setcdr z password))
	      ;; Add a pair of a username and a password.
	      (setcdr y (cons (cons username password) (cdr y))))
	  ;; Add a 3-tuple of a realm, a username and a password.
	  (setcdr x (cons (cons realm (list (cons username password)))
			  (cdr x))))
      ;; Add a 4-tuple of a server root, a realm, a username and a password.
      (push (cons root (list (cons realm (list (cons username password)))))
	    eldav-process-authinfo-alist))))

(defun eldav-process-read-passwd (url realm username)
  (let ((root (eldav-url-server-root url))
	(enable-recursive-minibuffers t)
	passwd)
    (or (cdr (assoc username
		    (cdr (assoc realm
				(assoc root
				       eldav-process-authinfo-alist)))))
	(progn
	  (eldav-process-set-authinfo
	   url realm username
	   (setq passwd (read-passwd (format "Password: "
					     realm username root))))
	  passwd))))

(defun eldav-process-read-user (url realm)
  "Read a user name for URL and REALM."
  (let* ((root (when (stringp url) (eldav-url-server-root url)))
	 (ident (or realm root))
	 (enable-recursive-minibuffers t)
	 (alist))
    (condition-case nil
	(if (setq alist
		  (cdr (assoc realm
			      (cdr (assoc root
					  eldav-process-authinfo-alist)))))
	    (if (= 1 (length alist))
		(caar alist)
	      (completing-read (if ident
				   (format "Select username for %s: " ident)
				 "Select username: ")
			       (mapcar (lambda (x) (cons (car x) (car x)))
				       alist)
			       nil t))
	  (read-from-minibuffer (if ident
				    (format "Username for %s: " ident)
				  "Username: ")))
      (quit (delete-process eldav-running-process)))))

(defun eldav-process-clear-authinfo (url realm username)
  (eldav-process-set-authinfo url realm username nil))

(defun eldav-set-url-realm (url realm)
  "Register URL's REALM to the `eldav-url-realm-alist'."
  (or (eldav-url-realm url)
      (setq eldav-url-realm-alist (cons (cons (file-name-directory url)
					      realm)
					eldav-url-realm-alist))))

(defun eldav-url-realm (url)
  "Get URL's REALM from the `eldav-url-realm-alist'."
  (catch 'found
    (dolist (elem eldav-url-realm-alist)
      (when (string-match (concat "^" (regexp-quote (car elem))) url)
	(throw 'found (cdr elem))))))

(eval-when-compile
  (defvar system-time-locale)
  (defvar message-log-max)
  (defvar coding-system-used)
  (defvar default-process-coding-system))

(defun eldav-process-sentinel (process event)
  ;; nothing to do.
  )

(defun eldav-process-filter (process string)
  (eldav-debug string)
  (with-current-buffer (process-buffer process)
    (let ((buffer-read-only nil)
	  (case-fold-search nil)
	  (enable-recursive-minibuffers t))
      (goto-char (process-mark process))
      (insert string)
      (unless eldav-process-auth-required
	(let (message-log-max)
	  (message "Reading %d bytes" (buffer-size))))
      (set-marker (process-mark process) (point))
      (unless (string= "" string)
	(goto-char (point-min))
	(cond
	 ((and eldav-process-auth-required
	       (looking-at "\n?OK\n?"))
	  (delete-region (point-min) (match-end 0))
	  (if (and eldav-process-user eldav-process-passwd)
	      (eldav-set-url-realm eldav-process-url eldav-process-realm))
	  (setq eldav-process-auth-required nil))
	 ((and eldav-process-auth-required
	       (looking-at "Proxy Username for \\(.*\\): ")
	       (= (match-end 0) (point-max)))
	  (unless eldav-process-proxy-user
	    (setq eldav-process-proxy-realm (match-string 1))
	    (when (eq (string-to-char eldav-process-proxy-realm) ?\")
	      (setq eldav-process-proxy-realm
		    (read eldav-process-proxy-realm)))
	    (setq eldav-process-proxy-user
		  (read-from-minibuffer (match-string 0))))
	  (ignore-errors
	    (erase-buffer)
	    (process-send-string process (concat eldav-process-proxy-user
						 "\n"))))
	 ((and eldav-process-auth-required
	       (looking-at "Proxy Password: ")
	       (= (match-end 0) (point-max)))
	  (unless eldav-process-proxy-passwd
	    (setq eldav-process-proxy-passwd
		  (read-passwd (match-string 0))))
	  (ignore-errors
	    (erase-buffer)
	    (process-send-string process (concat eldav-process-proxy-passwd
						 "\n"))))
	 ((and eldav-process-auth-required
	       (looking-at "Password: ")
	       (= (match-end 0) (point-max)))
	  (setq eldav-process-passwd
		(eldav-process-read-passwd eldav-process-url
					   eldav-process-realm
					   eldav-process-user))
	  (ignore-errors
	    (erase-buffer)
	    (process-send-string process (concat eldav-process-passwd "\n"))))
	 ((and eldav-process-auth-required
	       (looking-at "\n?Username for \\(.*\\): ")
	       (= (match-end 0) (point-max)))
	  (setq eldav-process-realm (match-string 1))
	  (when (eq (string-to-char eldav-process-realm) ?\")
	    (setq eldav-process-realm
		  (read eldav-process-realm)))
	  (when eldav-process-user ; Second time, which means wrong auth.
	    (eldav-process-clear-authinfo eldav-process-url
					  eldav-process-realm
					  eldav-process-user))
	  (setq eldav-process-user
		(eldav-process-read-user eldav-process-url
					 eldav-process-realm))
	  (ignore-errors
	    (erase-buffer)
	    (process-send-string process (concat eldav-process-user
						 "\n")))))))))

(defun eldav-exec (url &rest args)
  "Exec nd command for URL with ARGS."
  (let ((process-environment process-environment)
	(coding-system-for-write 'binary)
	(coding-system-for-read 'binary)
	(default-process-coding-system (cons 'binary 'binary))
	(process-connection-type eldav-process-connection-type)
	(enable-recursive-minibuffers t)
	(realm (eldav-url-realm url))
	proc)
    (with-current-buffer (get-buffer-create " *eldav*")
      (eldav-static-if (featurep 'xemacs) nil (set-buffer-multibyte nil))
      (if (and eldav-proxy
	       (or (null eldav-no-proxy-url-regexp)
		   (not (string-match eldav-no-proxy-url-regexp url))))
	  (setenv "http_proxy" eldav-proxy)
	(setenv "http_proxy" nil))
      (when eldav-running-process
	(error "DAV process is running."))
      (setq eldav-process-auth-required t)
      (erase-buffer)
      ;; URL is in the authenticated realm
      (if realm (setq args (append args (list "-a" realm))))
      (if eldav-process-proxy-realm
	  (setq args (append args (list "-A"
					eldav-process-proxy-realm))))
      ;;      (eldav-debug (concat "EXEC: "
      ;;			   eldav-program-name
      ;;			   " "
      ;;			   (mapconcat
      ;;			    'identity
      ;;			    (append eldav-program-arguments args
      ;;				    (list url))
      ;;			    " ")))
      (setq proc (apply 'start-process
			"eldav-exec"
			(current-buffer)
			eldav-program-name
			(append eldav-program-arguments args
				(list url))))
      (setq eldav-running-process proc
	    eldav-process-user nil
	    eldav-process-realm nil
	    eldav-process-passwd nil
	    eldav-process-url url)
      (set-process-filter proc 'eldav-process-filter)
      (set-process-sentinel proc 'eldav-process-sentinel)
      (process-kill-without-query proc)
      (condition-case nil
	  (while (not (or (eq (process-status proc) 'exit)
			  (eq (process-status proc) 'signal)))
	    (accept-process-output proc 0 200))
	(quit (delete-process proc)
	      (setq proc nil))
	(error (delete-process proc)
	       (setq proc nil)))
      (when proc
	(accept-process-output proc 0 200))
      (message "")
      (setq eldav-running-process nil)
      (if proc
	  (if (and (numberp (process-exit-status proc))
		   (zerop (process-exit-status proc)))
	      (buffer-string)
	    (if (numberp (process-exit-status proc))
		(let ((err (ignore-errors (read (buffer-string)))))
		  (if (and (consp err)
			   (eq (car err) 'error))
		      (eval err)
		    (error "DAV process is stopped")))
	      (error "Error on DAV process `%s'"
		     (process-exit-status proc))))
	(error "DAV process was stopped.")))))

(defun eldav-propfind (url)
  "Propfind URL."
  (if (and (not eldav-inhibit-propfind-cache)
	   (or (string= url (car eldav-last-propfind-result))
	       (string= (file-name-as-directory url)
			(car eldav-last-propfind-result))))
      (cdr eldav-last-propfind-result)
    (let ((props (read (condition-case err
			   (apply 'eldav-exec url (list "-v"))
			 (error
			  (setq eldav-last-propfind-result nil)
			  (signal (car err) (cdr err)))))))
      (setq props (mapcar (lambda (prop)
			    (setcar prop
				    (eldav-url-decode-string (car prop)))
			    prop)
			  props))
      (setq eldav-last-propfind-result (cons url props))
      props)))

(defun eldav-proppatch (url prop value &optional namespace)
  "Apply PROPPATCH to the URL.
PROP is the name of the property.
VALUE is the value for the property.
Optional NAMESPACE is the namespace for the property."
  (apply 'eldav-exec url
	 (append (list "-e" (concat prop "=" value))
		 (if namespace (list "-N" namespace)))))

(defun eldav-get (url)
  "Get URL."
  (eldav-exec url))

(defun eldav-mkcol (url &optional token)
  "MKCOL URL."
  (apply 'eldav-exec url
	 (append (list "-k")
		 (if token (list "-t" token)))))

(defun eldav-lock (url &optional owner scope timeout)
  "Lock URL with OWNER, SCOPE and TIMEOUT."
  (read (apply 'eldav-exec url
	       (append (list "-l")
		       (list "-o" (or owner
				      eldav-lock-identifier))
		       (if scope (list "-s" (symbol-name scope)))
		       (if (and timeout
				(not (eq timeout 'infinite))
				(numberp timeout))
			   (list
			    "-i"
			    (format "Second-%d" timeout)))))))

(defun eldav-unlock (url token)
  "Unlock URL with lock TOKEN."
  (when (and url token)
    (apply 'eldav-exec url (list "-u" "-t" token)))
  t)

(defun eldav-put-buffer (url &optional buffer token)
  "PUT buffer content to URL.
If BUFFER is specified its content is written.
Default is current buffer.
Use lock TOKEN if specified."
  (let ((temp-file (make-temp-file
		    (eldav-real-expand-file-name
		     "eldav-el"
		     eldav-temporal-directory)))
	(coding-system-for-write 'binary)
	status)
    ;; for Emacs 21.1-21.3
    (set-file-modes temp-file 384)
    (let (default-directory)
      (eldav-real-write-region (point-min) (point-max) temp-file nil 'no-msg)
      (unwind-protect
	  (setq status
		(read (apply 'eldav-exec url (append
					      (list "-p" temp-file)
					      (if token (list "-t" token))))))
	(delete-file temp-file))
      (eldav-check-multistat "PUT" url status))))

(defun eldav-check-multistat (method url status)
  (dolist (stat status)
    (let (code)
      (when (string-match (concat (regexp-quote (car stat))
				  "$") url)
	(setq code (cadr (assq 'status (cdr stat))))
	(if (> code 300)
	    (cond
	     ((eq code 422)
	      (error "%s failed, `Unprocessable Entity'" method))
	     ((eq code 423)
	      (error "%s failed, `Locked'" method))
	     ((eq code 424)
	      (error "%s failed, `Failed Dependency'" method))
	     (t (error "%s failed, code=`%d'" method code))))))))

(defun eldav-put-file (url file &optional token)
  "PUT file content to URL. Use lock TOKEN if specified."
  (eldav-check-multistat "PUT"
			 url
			 (read (apply 'eldav-exec url
				      (append (list "-p" file)
					      (if token (list "-t" token)))))))

(defun eldav-delete (url &optional token)
  (apply 'eldav-exec url (append (list "-d")
				 (if token (list "-t" token))))
  t)

(defun eldav-copy (src-url dest-url &optional overwrite token)
  (apply 'eldav-exec src-url
	 (append (list "-c" dest-url)
		 (if overwrite (list "-f"))
		 (if token (list "-t" token))))
  t)

(defun eldav-move (src-url dest-url &optional overwrite token)
  (apply 'eldav-exec src-url
	 (append (list "-m" dest-url)
		 (if overwrite (list "-f"))
		 (if token (list "-t" token))))
  t)

;;; eldav file handlers.
;;
(defun eldav-add-name-to-file (file newname &optional ok-if-already-exists)
  (eldav-copy (eldav-file-name-url file) (eldav-file-name-url newname)
	      ok-if-already-exists))

(defun eldav-copy-file (file newname &optional ok-if-already-exists keep-date)
  (interactive "fCopy file: \nFCopy %s to file: \np")
  (if (and (eq (find-file-name-handler newname 'copy-file) 'eldav-handler)
	   (eq (find-file-name-handler file 'copy-file) 'eldav-handler))
      (eldav-copy (eldav-file-name-url file)
		  (eldav-file-name-url newname) ok-if-already-exists)
    (with-temp-buffer
      (eldav-static-if (featurep 'xemacs) nil (set-buffer-multibyte nil))
      (let ((coding-system-for-read 'binary))
	(insert-file-contents file))
      (if (and (not ok-if-already-exists) (file-exists-p newname))
	  (signal 'file-already-exists (list file)))
      (let ((coding-system-for-write 'binary))
	(write-region (point-min) (point-max) newname nil 'no-msg)))))

(defun eldav-delete-directory (dir)
  (if (eldav-file-directory-p dir)
      (eldav-delete (eldav-file-name-url dir))))

(defun eldav-delete-file (file)
  (unless (eldav-file-directory-p file)
    (let ((eldav-inhibit-propfind-cache t))
      (if (file-exists-p file)
	  (eldav-delete (eldav-file-name-url file))
	(signal 'file-error (list "Removing old name" file))))))

(defun eldav-directory-files (dir &optional full match nosort)
  (let ((server-path (eldav-file-name-server-path dir))
	node-file files)
    (dolist (node (eldav-propfind (eldav-file-name-url dir)))
      (setq node-file (car node))
      (unless (string= server-path node-file)
	(when (string-match "\\(.*\\)/$" node-file)
	  (setq node-file (match-string 1 node-file)))
	(setq node-file (file-name-nondirectory node-file))
	(if (and match (not (string-match match node-file)))
	    nil
	  (push node-file files))))
    (if full
	(setq files (mapcar (lambda (file)
			      (expand-file-name file dir)) files)))
    (if nosort
	files
      (sort (copy-sequence files) 'string-lessp))))

(defun eldav-file-accessible-directory-p (dir)
  (eldav-file-directory-p dir))

(defun eldav-parse-date (string)
  "Parse the time-string STRING and return its time as Emacs style."
  (ignore-errors
    (let ((x (timezone-fix-time string nil nil)))
      (encode-time (aref x 5) (aref x 4) (aref x 3)
		   (aref x 2) (aref x 1) (aref x 0)
		   (aref x 6)))))

(defun eldav-node-property-value (node name &optional namespace)
  "Get optional property value from NODE.
NAME is the property name string.
If optional NAMESPACE is specified, treat it as a namespace for the PROP."
  (catch 'value
    (dolist (prop (cdr node))
      (when (and (eq (car prop) 'property)
		 (string= (cadr (assq 'name prop)) name)
		 (or (null namespace) (string= (cadr (assq 'ns prop))
					       namespace)))
	(throw 'value (cadr (assq 'value prop)))))))

(defun eldav-node-attributes (node)
  (let (last-modified isdir executable)
    (when (and node
	       (> (length (delq (assq 'lock-list (cdr node)) (cdr node))) 0))
      (if (or (string-match ".*/$" (car node))
	      (string= (car node) ".."))
	  (setq isdir t))
      (setq executable (if (string=
			    (eldav-node-property-value node "executable")
			    "T")
			   "x" "-"))
      (list isdir			;0 file type
	    1				;1 link count
	    -1				;2 uid
	    -1				;3 gid
	    '(0 0)			;4 atime
	    (setq last-modified
		  (eldav-parse-date
		   (cadr (assq 'last-modified (cdr node))))) ;5 mtime
	    last-modified		;6 ctime
	    (cadr (assq 'size (cdr node))) ; 7 size
	    (if isdir "drwxrwxrwx"
	      (format "-rw%srw-rw-" executable)) ;8 mode
	    nil				;9 gid weird
	    (eldav-file-inode-number (car node)) ;10 "inode number".
	    -1				;11 device number.
	    ))))

(defun eldav-file-attributes (file)
  (if (string-match eldav-file-name-regexp file)
      (progn
	(setq file (expand-file-name file))
	(eldav-node-attributes
	 (car (ignore-errors (eldav-propfind (eldav-file-name-url file))))))
    (eldav-real-file-attributes file)))

(defun eldav-file-props-directory-p (name props)
  (string= (cadr (assq 'resourcetype (cdr (assoc
					   (eldav-file-name-server-path
					    (file-name-as-directory name))
					   props)))) "collection"))

(defun eldav-file-directory-p (name)
  (eldav-file-props-directory-p
   name
   (ignore-errors (eldav-propfind
		   (eldav-file-name-url name)))))

(defun eldav-file-executable-p (file)
  (string=
   (eldav-node-property-value
    (car (ignore-errors (eldav-propfind (eldav-file-name-url file))))
    "executable")
   "T"))

(defun eldav-file-exists-p (name)
  (condition-case nil
      (and (eldav-propfind (eldav-file-name-url name))
	   t)
    (error)))

(defun eldav-file-local-copy (file)
  (let ((temp-file (make-temp-file
		    (expand-file-name "eldav-el"
				      eldav-temporal-directory))))
    ;; for Emacs 21.1-21.3
    (set-file-modes temp-file 384)
    (with-temp-file temp-file
      (insert-file-contents file))
    temp-file))

(defun eldav-file-modes (file)
  (if (eldav-file-directory-p file)
      511
    (if (eldav-file-executable-p file)
	502
      438)))

(defun eldav-file-name-all-completions (file dir)
  (let ((server-path (eldav-file-name-server-path dir))
	node-file matches isdir)
    (catch 'done
      (dolist (node (eldav-propfind (eldav-file-name-url dir)))
	(setq node-file (car node)
	      isdir nil)
	(unless (string= server-path node-file)
	  (when (string-match "\\(.*\\)/$" node-file)
	    (setq isdir t)
	    (setq node-file (match-string 1 node-file)))
	  (setq node-file (file-name-nondirectory node-file))
	  (when (string-match (concat "^" file) node-file)
	    (setq matches (cons (concat node-file (if isdir "/"))
				matches)))))
      matches)))

(defun eldav-file-name-completion (file dir)
  (let (isdir)
    (try-completion
     file
     (mapcar (lambda (node)
	       (let ((node-file (car node)))
		 (setq isdir nil)
		 (when (string-match "\\(.*\\)/$" node-file)
		   (setq isdir t)
		   (setq node-file (match-string 1 node-file)))
		 (list (concat (file-name-nondirectory node-file)
			       (if isdir "/")))))
	     (eldav-propfind (eldav-file-name-url dir))))))

(defsubst eldav-time-less-p (t1 t2)
  "Say whether time T1 is less than time T2."
  (unless (consp (cdr t1))
    (setq t1 (list (car t1)(cdr t1))))
  (unless (consp (cdr t2))
    (setq t2 (list (car t2)(cdr t2))))
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (< (nth 1 t1) (nth 1 t2)))))

(defun eldav-file-newer-than-file-p (f1 f2)
  (let ((attr1 (eldav-file-attributes f1))
	(attr2 (eldav-file-attributes f2)))
    (and attr1 attr2
	 (not (eldav-time-less-p (nth 5 attr1)
				 (nth 5 attr2))))))

(defun eldav-file-readable-p (file)
  t)

;; `file-truename' is identity.

(defun eldav-own-lock-p (file)
  (let ((locks (eldav-lockinfo-list (eldav-file-name-url file))))
    (member eldav-lock-identifier
	    (mapcar 'eldav-lockinfo-owner locks))))

(defun eldav-file-writable-p (file)
  ;; Read-only unless my lock is contained.
  (if eldav-use-vc
      (eldav-own-lock-p file)
    (file-exists-p (file-name-directory (directory-file-name file)))))

(defun eldav-insert-file-attribute-as-ls-line (name attr switches)
  (insert (nth 8 attr) " " (format "%3d" (or (nth 1 attr) 0))
	  " "
	  "WebDAV" " "
	  "WebDAV" " " (format "%8d" (or (nth 7 attr) 0))
	  " "
	  (let ((system-time-locale "C"))
	    (format-time-string
	     (if (string= (format-time-string "%Y" (current-time))
			  (format-time-string "%Y" (nth 5 attr)))
		 "%b %e %R"
	       "%b %e %Y ")
	     (nth 5 attr)))
	  " " name "\n"))

(defun eldav-insert-directory (file switches &optional wildcard full)
  (unless (string-match eldav-file-name-regexp file)
    (setq file (expand-file-name file default-directory)))
  ;; remove '/' in the tail of the filename.
  (when (string-match "\\(.*\\)/$" file)
    (setq file (match-string 1 file)))
  (let ((server-path
	 (file-name-as-directory (eldav-file-name-server-path file)))
	(as-file (catch 'done
		   (dolist (option (split-string switches))
		     (unless (string= option "--dired")
		       (when (string-match "d" option)
			 (throw 'done t))))))
	node-file parent attr)
    (let ((parentdir (file-name-directory (directory-file-name file))))
      (setq parent (ignore-errors
		     (eldav-propfind
		      (eldav-file-name-url parentdir))))
      (when parent
	(setq parent (copy-sequence parent))
	(setcar parent "..")))
    (when (and (file-directory-p file)
	       (ignore-errors (eldav-propfind
			       (eldav-file-name-url
				(file-name-as-directory file)))))
      (setq file (file-name-as-directory file)))
    (dolist (node
	     (sort (copy-sequence
		    (append (if (and (not as-file) parent) (list parent))
			    (let ((eldav-inhibit-propfind-cache t))
			      (eldav-propfind
			       (eldav-file-name-url
				(if as-file
				    (setq file (directory-file-name file))
				  file))))))
		   (lambda (n1 n2)
		     (string-lessp
		      (if (string= server-path (car n1))
			  "."
			(car n1))
		      (if (string= server-path (car n2))
			  "."
			(car n2))))))
      (setq node-file (car node))
      (cond
       ((string= node-file ".."))
       (t
	(if (and (not as-file) (string= server-path node-file))
	    (setq node-file "."))
	(when (string-match "\\(.*\\)/$" node-file)
	  (setq node-file (match-string 1 node-file)))
	(setq node-file (file-name-nondirectory node-file))))
      (when (setq attr (eldav-node-attributes node))
	(eldav-insert-file-attribute-as-ls-line node-file
						attr
						switches)))))

(defun eldav-insert-file-contents (filename &optional visit beg end replace)
  (barf-if-buffer-read-only)
  (setq filename (expand-file-name filename))
  (if visit (setq buffer-file-name filename))
  (let ((content (condition-case nil
		     (eldav-get (eldav-file-name-url filename))
		   (error (signal 'file-error (list "Cannot open" filename)))))
	coding)
    (if replace (erase-buffer))
    (if beg (setq content (substring content beg)))
    (if end (setq content (substring content 0 (- end (or beg 0)))))
    (save-excursion
      (insert
       (decode-coding-string
	content
	(cond
	 ((not (boundp 'coding-system-for-read))
	  nil)
	 (coding-system-for-read
	  coding-system-for-read)
	 (t
	  (eldav-static-if (fboundp 'find-operation-coding-system)
	      (setq coding (find-operation-coding-system
			    'insert-file-contents
			    filename visit beg end replace))
	    (catch 'done
	      (dolist (elem file-coding-system-alist)
		(when (string-match (car elem) filename)
		  (setq coding (cdr elem))
		  (if (and (symbolp (cdr elem))
			   (fboundp (cdr elem)))
		      (setq coding (funcall coding)))
		  (throw 'done nil)))))
	  (cond
	   ((coding-system-p coding)
	    coding)
	   ((consp coding)
	    (car coding))
	   (t 'binary)))))))
    (eldav-static-if (boundp 'last-coding-system-used)
	(setq coding-system-used last-coding-system-used))
    (list filename (length content))))

(defun eldav-load (file &optional noerror nomessage nosuffix)
  (let ((tryfiles (if nosuffix
		      (list file)
		    (list (concat file ".elc") (concat file ".el") file)))
	;; make sure there are no references to temp files
	(load-force-doc-strings t)
	copy)
    (while (and tryfiles (not copy))
      (ignore-errors
	(setq copy (eldav-file-local-copy (car tryfiles))))
      (setq tryfiles (cdr tryfiles)))
    (if copy
	(unwind-protect
	    (funcall 'load copy noerror nomessage nosuffix)
	  (delete-file copy))
      (or noerror
	  (signal 'file-error (list "Cannot open load file" file)))
      nil)))

(defun eldav-make-directory (dir &optional parents)
  (interactive (list (expand-file-name (read-file-name "Make directory: "))))
  (if parents
      (let ((parent (file-name-directory (directory-file-name dir))))
	(or (file-exists-p parent)
	    (make-directory parent parents))))
  (if (file-exists-p dir)
      (error "Cannot make directory %s: file already exists" dir)
    (eldav-mkcol (eldav-file-name-url dir))))

(defun eldav-rename-file (filename newname &optional ok-if-already-exists)
  (eldav-move (eldav-file-name-url filename)
	      (eldav-file-name-url newname) ok-if-already-exists))

(defun eldav-unhandled-file-name-directory (filename)
  (file-name-directory eldav-temporal-directory))

(defun eldav-file-name-directory (filename)
  (let ((name (eldav-real-file-name-directory filename)))
    (if (string= eldav-file-name-prefix name)
	(concat filename "/")
      name)))

(defun eldav-real-file-name-directory (&rest args)
  (eldav-run-real-handler 'file-name-directory args))

(defun eldav-real-write-region (&rest args)
  (eldav-run-real-handler 'write-region args))

(defun eldav-real-expand-file-name (&rest args)
  (eldav-run-real-handler 'expand-file-name args))

(defun eldav-real-file-attributes (&rest args)
  (eldav-run-real-handler 'file-attributes args))

(defun eldav-expand-file-name (name &optional base)
  "Convert path string NAME to the canonicalized one."
  (cond
   ((string-match "/\\.\\.$" name)
    (setq name (file-name-directory
		(directory-file-name (file-name-directory name)))))
   ((string-match "/\\.$" name)
    (setq name (file-name-directory name))))
  (if (string-match eldav-file-name-regexp name)
      (concat
       (substring name 0 (match-end 0))
       (substring (expand-file-name (concat
				     "/"
				     (substring name (match-end 0)))) 1))
    ;; base name matches eldav path.
    (cond
     ((string= name ".")
      base)
     ((eq (string-to-char name) ?~)
      (eldav-real-expand-file-name name))
     ((eq (string-to-char name) ?/)
      (eldav-real-expand-file-name name))
     ((string= name "..")
      (file-name-directory (directory-file-name base)))
     (t
      (concat (directory-file-name (file-name-as-directory base))
	      (expand-file-name name "/"))))))

(defun eldav-file-modtime (file)
  (let ((eldav-inhibit-propfind-cache t))
    (or (nth 5 (eldav-file-attributes file)) '(0 0))))

(defun eldav-set-buffer-mode ()
  "Set correct modes for the current buffer if visiting a remote file."
  (if (and (stringp buffer-file-name)
	   (string-match eldav-file-name-regexp buffer-file-name))
      (auto-save-mode eldav-auto-save)))

(defun eldav-verify-visited-file-modtime (buf)
  (let ((name (buffer-file-name buf)))
    (let ((file-mdtm (eldav-file-modtime name))
	  (buf-mdtm (with-current-buffer buf (visited-file-modtime))))
      (or (zerop (car file-mdtm))
	  (not (eldav-time-less-p buf-mdtm file-mdtm))))))

(eldav-static-if (fboundp 'select-safe-coding-system)
    (defun eldav-select-safe-coding-system (from to default-coding-system)
      (funcall (symbol-function 'select-safe-coding-system)
	       from to default-coding-system))
  (defun eldav-select-safe-coding-system  (from to default-coding-system)
    (if (boundp 'default-coding-system)
	default-coding-system)))

(defun eldav-write-region (start end filename &optional append visit
				 &rest args)
  (if append (signal 'file-error (list "Append write not supported")))
  (let* ((origin (current-buffer))
	 (coding (if (boundp 'buffer-file-coding-system)
		     buffer-file-coding-system))
	 (url (eldav-file-name-url filename))
	 (lockinfo (car (eldav-lockinfo-list-search
			 'owner
			 eldav-lock-identifier
			 (eldav-lockinfo-list url)))))
    (setq filename (expand-file-name filename))
    (save-restriction
      (narrow-to-region start end)
      (with-temp-buffer
	(insert-buffer origin)
	(encode-coding-region (point-min) (point-max)
			      (eldav-select-safe-coding-system
			       (point-min)
			       (point-max) coding))
	(eldav-put-buffer url
			  (current-buffer)
			  (if lockinfo
			      (eldav-lockinfo-token lockinfo))))
      (cond
       ((stringp visit))		; second file name?
       ((eq visit t)
	(set-visited-file-modtime (eldav-file-modtime filename))
	(eldav-set-buffer-mode)
	(setq buffer-file-name filename)
	(set-buffer-modified-p nil)
	(message "Wrote %s" filename))
       ((null visit)
	(message "Wrote %s" filename))))
    (eldav-static-if (boundp 'last-coding-system-used)
	(setq last-coding-system-used coding))
    nil))

;;;###autoload
(defun eldav-handler (operation &rest args)
  (let ((fn (get operation 'eldav)))
    (if fn
	(apply fn args)
      (eldav-run-real-handler operation args))))

(defun eldav-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
	 (cons 'eldav-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(defun eldav-vc-registered (file)
  (when eldav-use-vc
    (when (string-match eldav-file-name-regexp file)
      (funcall (symbol-function 'vc-file-setprop) file 'vc-backend 'eldav)
      t)))

;;; Utils
(defun eldav-set-file-executable-p (file value)
  (let ((props (cdar (read (eldav-proppatch
			    (eldav-file-name-url file)
			    "executable" (if value "T" "F")
			    eldav-executable-property-namespace)))))
    (if (>= (cadr (assq 'status props)) 300)
	(error "PROPPATCH Failed"))))

;;;
;; An example setting for dired:
;; (add-hook 'dired-mode-hook
;;	  (lambda ()
;;	    (define-key
;;	      dired-mode-map "\C-ce" 'dired-eldav-toggle-file-executable)))
;;;###autoload
(defun dired-eldav-toggle-file-executable (&optional arg)
  "Set the executable property of marked (or next ARG) files."
  (interactive "P")
  (let* ((files (dired-get-marked-files t arg)))
    (dolist (file files)
      (setq file (expand-file-name file (dired-current-directory)))
      (when (string-match eldav-file-name-regexp file)
	(eldav-set-file-executable-p file (not (file-executable-p file)))))
    (dired-do-redisplay arg)))

(eldav-static-if (boundp 'find-file-hook)
    (or (memq 'eldav-set-buffer-mode find-file-hook)
	(add-hook 'find-file-hook
		  'eldav-set-buffer-mode))
  (or (memq 'eldav-set-buffer-mode find-file-hooks)
      (setq find-file-hooks
	    (cons 'eldav-set-buffer-mode find-file-hooks))))

(put 'add-name-to-file 'eldav 'eldav-add-name-to-file)
(put 'copy-file 'eldav 'eldav-copy-file)
(put 'delete-directory 'eldav 'eldav-delete-directory)
(put 'delete-file 'eldav 'eldav-delete-file)
(put 'directory-files 'eldav 'eldav-directory-files)
(put 'file-accessible-directory-p 'eldav 'eldav-file-accessible-directory-p)
(put 'file-attributes 'eldav 'eldav-file-attributes)
(put 'file-directory-p 'eldav 'eldav-file-directory-p)
(put 'file-executable-p 'eldav 'eldav-file-executable-p)
(put 'file-exists-p 'eldav 'eldav-file-exists-p)
(put 'file-local-copy 'eldav 'eldav-file-local-copy)
(put 'file-modes 'eldav 'eldav-file-modes)
(put 'file-name-all-completions 'eldav 'eldav-file-name-all-completions)
(put 'file-name-completion 'eldav 'eldav-file-name-completion)
(put 'file-newer-than-file-p 'eldav 'eldav-file-newer-than-file-p)
(put 'file-readable-p 'eldav 'eldav-file-readable-p)
(put 'file-symlink-p 'eldav 'null)
(put 'file-writable-p 'eldav 'eldav-file-writable-p)
(put 'insert-directory 'eldav 'eldav-insert-directory)
(put 'insert-file-contents 'eldav 'eldav-insert-file-contents)
(put 'load 'eldav 'eldav-load)
(put 'make-directory 'eldav 'eldav-make-directory)
(put 'make-symbolic-link 'eldav 'null)
(put 'rename-file 'eldav 'eldav-rename-file)
(put 'substitute-in-file-name 'eldav 'identity)
(put 'unhandled-file-name-directory 'eldav
     'eldav-unhandled-file-name-directory)
(put 'file-name-directory 'eldav 'eldav-file-name-directory)
(put 'vc-registered 'eldav 'eldav-vc-registered)
(put 'verify-visited-file-modtime 'eldav
     'eldav-verify-visited-file-modtime)
(put 'expand-file-name 'eldav 'eldav-expand-file-name)
(put 'write-region 'eldav 'eldav-write-region)
(put 'file-truename 'eldav 'identity)

(provide 'eldav)

;;; eldav.el ends here
