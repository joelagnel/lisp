;;; mew-browse.el --- Handling URI with browse-url.el

;; Author: Hideyuki SHIRAI <shirai@mew.org>
;; Modify: Shuichi Kitaguchi <kit@Mew.org>
;; Created: May 19, 1999
;; Revised: Nov 08, 2004

;;;
;;; ~/.emacs settings.
;;;
;;; ... anything browse-url setting ...
;; (require 'mew-browse)

;;; SHIFT + (Middle|Right)-Click = browse-url or mew-user-agent-compose
;;; for Emacs
;; (define-key global-map [S-mouse-2] 'browse-url-at-mouse)
;;; for XEmacs
;; (define-key global-map [(shift button2)] 'browse-url-at-mouse)
;;

;;; Appending URI to specified file.
;;
;;   mew-browse-noask                 ... ask or not when browse
;;   mew-browse-append-file           ... URL collection file name
;;   mew-browse-append-always-file    ... always, append URL to file (for dial-up)
;;   mew-browse-append-always-mailto  ... always, URL is mailto: (for emacs19.28)
;;   mew-browse-append-file-sort      ... always, sort URL file
;;
;;; example:
;;   (setq mew-browse-noask                nil)
;;   (setq mew-browse-append-file          "~/.browse")
;;   (setq mew-browse-append-always-file   nil)
;;   (setq mew-browse-append-always-mailto nil)
;;   (setq mew-browse-append-file-sort nil)
;;

;;; Use mew-url-mailto instead of url-mailto in W3, add followings in your ~/.emacs file.
;;
;; (cond
;;  ((locate-library "url-mail")
;;   (eval-after-load "url-mail"
;;     '(fset 'url-mailto (symbol-function 'mew-url-mailto))))
;;  ((locate-library "url")
;;   (eval-after-load "url"
;;     '(fset 'url-mailto (symbol-function 'mew-url-mailto)))))
;;

;;; Use from emacs-w3m, add followings in your ~/.emacs file.
;;  (setq w3m-mailto-url-function 'mew-url-mailto)
;;

;;; Use from MS-Windows application, set followings in your registry.
;;   registry key: '\\HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\mailto\\shell\\open\\command'
;;   registry value: 'drive:\\path\\gnudoitw.exe \"(mew-url-mailto \"%%1\")\"'"
;;

;;; Use from Mozilla & Firefox with MozEx, set followings on Mailer form of MozEx.
;; "/path/gnudoit (mew-url-mailto-mozex "%r")"
;; or
;; "/path/emacseval (mew-url-mailto-mozex "%r")"
;; 
;; `emacseval' is a shell script like this,
;; ----cut here----
;; #!/bin/sh
;; /usr/local/bin/emacsclient --eval "`echo $*`"
;; ----cut here----"
;;

(eval-when-compile (require 'mew))

(if (string-match "XEmacs" emacs-version)
    (defvar mew-browse-button [(button2)] "*Mouse button in message mode.")
  (defvar mew-browse-button [mouse-2] "*Mouse button in message mode."))

(defvar mew-ext-prog-url mew-prog-text/html-ext)
(defvar mew-ext-prog-url-args nil)

(setq browse-url-browser-function 'mew-browse-url)

(add-hook 'mew-init-hook
	  (lambda ()
	    (progn 
	      (define-key mew-message-mode-map mew-browse-button 'browse-url-at-mouse)
	      )))

(defvar mew-browse-url-mailto-switch-func nil
  "*Which do you like, nil, 'switch-to-buffer-other-window or 'switch-to-buffer-other-frame ?")

(setq browse-url-regexp "\\(\\b\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=?#$@~`%&*+|\\/.,]*[-a-zA-Z0-9_=#$@~`%&*+|\\/]+\\)\\|\\(\\([^-A-Za-z0-9!_.%]\\|^\\)[-A-Za-z0-9._!%]+@[A-Za-z0-9][-A-Za-z0-9._!]+[A-Za-z0-9]\\)")

(defvar mew-browse-noask                t   "*Ask or not when browse.")
(defvar mew-browse-append-file          nil "*URL collection file.")
(defvar mew-browse-append-always-file   nil "*For dialup user.")
(defvar mew-browse-append-always-mailto nil "*For emacs19.28.")
(defvar mew-browse-append-file-sort     nil "*Sort URL file.")

(defun mew-browse-url (url &optional args)
  "Exec browse URL or mew-user-agent-compose with parsing RFC2368."
  (interactive
   (list (read-from-minibuffer "Mew URL: ")))
  (when (or (not (boundp 'mew-init-p)) (null mew-init-p))
    (save-excursion
      (require 'mew)
      (mew-init)))
  (let* ((append-buffer (and mew-browse-append-file
			     (string= buffer-file-name
				      (expand-file-name mew-browse-append-file))))
	 (append-nil (or append-buffer (not mew-browse-append-file)))
	 (append-all (and (not append-nil) mew-browse-append-always-file))
	 (append-ask (and (not append-nil) (not mew-browse-append-always-file)))
	 (browse-all (or append-buffer mew-browse-noask))
	 (browse-ask (and (not append-buffer) (not mew-browse-noask))))
    (string-match "\\([a-zA-Z0-9][-a-zA-Z0-9!_=?#$@~`%&*+|\\/.,:]+\\)" url)
    (setq url (substring url (match-beginning 0) (match-end 0)))
    (unless (string-match ":" url)    ;; emacs19.28 only
      (if (and (not mew-browse-append-always-mailto)
	       (not (y-or-n-p (format "mailto:%s(y) or ftp://%s(n)? " url url))))
	  (setq url (concat "ftp://" url))
	(setq url (concat "mailto:" url))))
    (cond
     ((and append-all browse-all)
      (mew-browse-url-append url)
      (mew-browse-url-start url))
     ((and append-ask browse-all)
      (when (y-or-n-p (format "Append %s? " url))
	(mew-browse-url-append url))
      (mew-browse-url-start url))
     ((and append-nil browse-all)
      (mew-browse-url-start url))
     ((and append-all browse-ask)
      (mew-browse-url-append url)
      (when (y-or-n-p (format "Browse %s? " url))
	(mew-browse-url-start url)))
     ((and append-nil browse-ask)
      (when (y-or-n-p (format "Browse %s? " url))
	(mew-browse-url-start url)))
     (t ;; (and append-ask browse-ask)
      (if (y-or-n-p (format "Browse %s(y) or Append(n)? " url))
	  (mew-browse-url-start url)
	(mew-browse-url-append url)))
     )))

(defun mew-browse-url-append (url)
  (let ((file (expand-file-name mew-browse-append-file))
	(beg))
    (save-excursion
      (find-file file)
      (set-buffer (current-buffer))
      (goto-char (point-min))
      (while (search-forward url nil t)
	(progn
	  (beginning-of-line)
	  (setq beg (point))
	  (forward-line)
	  (delete-region beg (point))))
      (goto-char (point-max))
      (insert url "\n")
      (when mew-browse-append-file-sort
	(sort-lines nil (point-min) (point-max)))
      (write-file file)
      (kill-buffer (current-buffer))
      (message "Append %s to %s done" url file)
      )))

(defun mew-browse-url-start (url)
  (message "Browse %s" url)
  (cond
   ((string-match "^mailto:" url)
    (mew-browse-url-mailto url))
   ((and (symbolp mew-ext-prog-url) (fboundp mew-ext-prog-url))
    (funcall mew-ext-prog-url url))
   ((equal mew-ext-prog-url "w3")
    (require 'w3)
    (w3-fetch-other-frame url))
   (t
    (apply (function start-process)
	   (format "*mew %s*" mew-ext-prog-url)
	   nil mew-ext-prog-url 
	   (append mew-ext-prog-url-args (list url))))))

(defun mew-url-mailto (url)
  "Execute mew-user-agent-compose with parsing RFC2368.

If use from emacs-w3m, add followings in your ~/.emacs file.
  (setq w3m-mailto-url-function 'mew-url-mailto)

If use from MS-Windows application, set followings in your registry.
  registry key: '\\HKEY_LOCAL_MACHINE\\SOFTWARE\\Classes\\mailto\\shell\\open\\command'
  registry value: 'drive:\\path\\gnudoitw.exe \"(mew-url-mailto \"%%1\")\"'"
  (interactive
   (list (read-from-minibuffer "Mew mailto: ")))
  (when (or (not (boundp 'mew-init-p)) (null mew-init-p))
    (save-excursion
      (require 'mew)
      (mew-init)))
  (let ((lst (mew-browse-url-mailto-analysis url)))
    (mew-user-agent-compose (nth 0 lst)	;; to
			    (nth 1 lst)	;; subject
			    (nth 2 lst) ;; other
			    nil mew-browse-url-mailto-switch-func)))

(defun mew-url-mailto-mozex (url)
  "Execute mew-user-agent-compose from Mozilla and Firefox with MozEx.

If use this command, set followings on Mailer form of MozEx.
\"/path/gnudoit (mew-url-mailto-mozex \"%r\")\"
or
\"/path/emacseval (mew-url-mailto-mozex \"%r\")\"

`emacseval' is a shell script like this,
----cut here----
#!/bin/sh
/usr/local/bin/emacsclient --eval \"`echo $*`\"
----cut here----"
  (interactive)
  (when (or (not (boundp 'mew-init-p)) (null mew-init-p))
    (save-excursion
      (require 'mew)
      (mew-init)))
  (let ((lst (mew-browse-url-mailto-analysis url 'mozex)))
    (mew-user-agent-compose (nth 0 lst)	;; to
			    (nth 1 lst)	;; subject
			    (nth 2 lst) ;; other
			    nil mew-browse-url-mailto-switch-func)))

;; return '(to subject other)"
(defun mew-browse-url-mailto-analysis (url &optional mozex)
  (let (to subject other)
    (while (string-match "[ \t]+" url)
      (setq url (concat (substring url 0 (match-beginning 0))
			(substring url (match-end 0)))))
    (if (string-match "^mailto:" url)
	(setq url (mew-browse-url-mailto-decamp (substring url (match-end 0))))
      (setq url (mew-browse-url-mailto-decamp url)))
    (when (string-match "^\\([^?]+\\)" url)
      (setq to (mew-browse-url-mailto-hex-to-string
		(substring url (match-beginning 1) (match-end 1))))
      (setq url (substring url (match-end 0))))
    (while (string-match "^[?&]\\([^=]+\\)=\\([^&]*\\)" url)
      (let ((hname (substring url (match-beginning 1) (match-end 1)))
	    (hvalue (mew-browse-url-mailto-hex-to-string
		     (substring url (match-beginning 2) (match-end 2)))))
	(setq url (substring url (match-end 0)))
	(when mozex
	  (setq hname (mew-browse-url-mailto-hex-to-string hname))
	  (while (string-match "[ \t\n]+" hname)
	    (setq hname (replace-match "" nil nil hname)))
	  (when (string-match "[ \t\n]+$" hvalue)
	    (setq hvalue (replace-match "" nil nil hvalue))))
	(cond
	 ((string-match "^to$" hname)
	  (if to
	      (setq to (concat to ", " hvalue))
	    (setq to hvalue)))
	 ((string-match "^subject$" hname)
	  (setq subject hvalue))
	 (t
	  (setq other (cons (cons (capitalize hname) hvalue) other))))))
    `(,to ,subject ,other)))

(defun mew-browse-url-mailto-decamp (str)
  (save-match-data
    (while (string-match "&amp;" str)
      (setq str (concat (substring str 0 (match-beginning 0))
			"&"
			(substring str (match-end 0)))))
    str))

(defun mew-browse-url-mailto-hex-to-string (str)
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (let ((doit t) char cs)
	(while (re-search-forward "%\\([0-9a-fA-F][0-9a-fA-F]\\)" nil t)
	  (setq char (mew-browse-url-mailto-2hexs-to-int
		      (buffer-substring (match-beginning 1) (match-end 1))))
	  (delete-region (match-beginning 0) (match-end 0))
	  (insert char))
	(setq cs (mew-charset-to-cs
		  (mew-charset-guess-region (point-min) (point-max))))
	(when (or (not cs)
		  (eq cs mew-cs-unknown))
	  (setq cs mew-cs-autoconv))
	(when (and (eq cs 'utf-8)
		   (or (not mew-internal-utf-8p)
		       (not (featurep 'un-define))))
	  (condition-case nil
	      (require 'un-define)
	    (file-error
	     (setq doit nil)
	     (delete-region (point-min) (point-max))
	     (insert "Install Mule-UCS for UTF-8.\n"))))
	(when doit
	  (mew-set-buffer-multibyte nil)
	  (decode-coding-region (point-min) (point-max) cs)
	  (mew-set-buffer-multibyte t))
	(buffer-string)))))

(defun mew-browse-url-mailto-2hexs-to-int (hex)
  (+ (* 16 (mew-hexchar-to-int (aref hex 0)))
     (mew-hexchar-to-int (aref hex 1))))

(provide 'mew-browse)

;;; Copyright Notice:

;; Copyright (C) 1999-2004 Mew developing team.
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

;;; mew-browse.el ends here
