;;; advogato.el -- elisp mode to post diary entries to advogato.org.

;; Copyright (C) 2002 Ramakrishnan M <rkrishnan@debian.org>.
;; Based on code Copyright (C) 2002 by Mark A. Hershberger's blogger.el.

;; Author: Ramakrishnan M <rkrishnan@debian.org>
;; Version: 1.0

;; This file is not yet part of GNU Emacs.

;; advogato.el free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; advogato.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; advogato.el implements the XML-RPC interface to advogato.org
;; Needs xml-rpc.el from http://elisp.info/
;;
;; For ease of use:
;; I use the following commands in my .emacs file:
;;
;; (require 'advogato) ;; will autoload advogato.el 
;; (global-set-key "\C-cbs" 'advogato-start-post)
;;
;; C-c b s will switch to a new buffer where you can compose a
;; post.
;;
;; C-x C-s    -- post-and-publish current buffer to the blog.
;;               Calling blogger-save-post with an prefix argument
;;               (i.e. C-u C-c C-c) will prompt for which weblog
;;               to use.
;;
;; C-c C-c    -- identical to C-x C-s, but will also bury the buffer.
;;
;;

;;; Changelog
;; v1.0 -- (date?) -  elimentary mode with lots of ugly code 
;;
;; v1.1 -- 11 Feb 2002 - Removed `url-insert-entities-in-string' as this is now
;;                       being handled at xml-rpc layer. Removes the bug which
;;                       escapes HTML markups.
;; v1.2 -- 11 Feb 2002 - Improved cookie support. Now can read the advogato
;;                       cookies from Mozilla/galeon/url cookies file.
;; v1.3 -- 11 Feb 2004 - Allow the user to set the mozilla profile name
;;                       (Thanks - Muli Ben-Yehuda <mulix@mulix.org>)
;; v1.4 -- 13 Feb 2004 - provide 'advogato' feature (Muli) 
;; Please note once again that advogato.el needs the v1.6.1 of xml-rpc.el
;; from http://elisp.info

(require 'xml-rpc)
(require 'url)

(defvar advogato-url "http://www.advogato.org/XMLRPC"
  "Server you want to Use.")

(defvar advogato-username nil
  "Your username")

(defvar advogato-password nil
  "Your password")

(defvar advogato-id nil
  "Cookie used for authentication")

(defvar *advogato* nil
  "The advogato buffer where we compose posts")

(defvar advogato-mode-hook nil
  "Hook to run after starting up advogato mode.")

(defvar advogato-mode-map nil
  "Keymap for advogato-mode.")

(defvar advogato-mozilla-profile-name nil
  "Name of the mozilla profile to look at for advogato cookies.")

(unless advogato-mode-map
  (setq advogato-mode-map (copy-keymap text-mode-map))
  (define-key advogato-mode-map "\C-c\C-c" 'advogato-send-post)
  (define-key advogato-mode-map "\C-x\C-s" 'advogato-save-post))

(defun advogato-mode ()
  "Major mode for editing text for Advogato.  Based on text-mode."
  (interactive)
  (text-mode)
  (use-local-map advogato-mode-map)
  (setq mode-name "advogato")
  (setq major-mode 'advogato-mode)
;  (setq blogger-post-ring (make-ring blogger-max-entries))
  (run-hooks 'advogato-mode-hook))

(defun advogato-start-post()
  "*Start creating a advogato post in the *advogato* buffer"
  (interactive)
  (setq *advogato* (switch-to-buffer "*advogato*"))
  (advogato-mode)
;  (setq blogger-ring-index nil)
  (erase-buffer))

(defun advogato-send-post (&optional arg)
  "Publish the current message.  With optional argument prompts
for blog to use."
  (interactive)
  (advogato-save-post arg)
  (message "Diary Posted")
  (bury-buffer))

(defun advogato-save-post (&optional arg)
  "Publish the current message.  With optional argument prompts
for blog to use."
  (interactive)
  (if (not (equal (current-buffer) *advogato*))
      (message 
       "You are not in the *advogato* buffer."))
  (if (buffer-modified-p)
      (advogato-new-post (buffer-string))))

(defun advogato-username (&optional prompt)
  "Get the username.  If you've not yet logged in then prompt for
it"
  (setq advogato-username
	(if (or prompt (not advogato-username))
	      (read-from-minibuffer "Username: " advogato-username)
	    advogato-username)))

(defun advogato-password (&optional prompt)
  "Get the password.  If you've not yet logged in then prompt for
it"
  (setq advogato-password
	(if (or prompt (not advogato-password))
	    (if advogato-password
		(read-passwd "Password for advogato: " nil advogato-password)
	      (read-passwd "Password for advogato: " nil))
	  advogato-password)))

(defun advogato-cookie ()
  "Get the advogato cookie."
  (setq advogato-id (advogato-get-cookie))
  (if (not advogato-id)
      (setq advogato-id
	    (xml-rpc-method-call
	     advogato-url
	     'authenticate
	     (advogato-username)
	     (advogato-password)))))
 
(defun advogato-new-post (message)
  "Post a new message into your Diary."
  (let ((msg-id))
    (if (not advogato-id)
	(advogato-cookie))
    (setq msg-id (xml-rpc-method-call
		  advogato-url
		  'diary.set
		  advogato-id
		  -1
		  message))))
;  (run-hooks 'advogato-new-post-hook)))


(defun advogato-edit-message (msg)
  "Edit a Message.  MSG specifies which message to edit."
  (set-buffer *advogato*)
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert (cdr (assoc 'content msg)))
  (set-buffer-modified-p nil)
;  (or (eq (blogger-get-userid) (cdr (assoc 'userid msg)))
;      (setq buffer-read-only t))
  (pop-to-buffer *advogato*))


;;; Netscape style cookie handling
;; Lifted jwz-lj.el. Hoping to use the url-cookie.el procedures soon (from 
;; W3/url package)

;;; Try to read the cookie from netscape/mozilla/galeon/url cookie.txt
(defun advogato-get-cookie (&optional hostname)
  "Looks in the Mozilla/Galeon/url cookie files to find Advogato data.
Returns an alist of matching cookies if found, else returns nil"

  (or hostname (setq hostname "www.advogato.org"))

  (let ((files '())
        (cookies '()))

    ;; FTSO Mozilla!
    (if (file-directory-p "~/.mozilla")
        (let ((dir
	       (let (d)
		 (cond ((and 
			 advogato-mozilla-profile-name
			 (file-directory-p 
			  (setq d 
				(concat "~/.mozilla/" advogato-mozilla-profile-name))))
			d)
		       ((file-directory-p (setq d "~/.mozilla/default"))
			d)
		       ((file-directory-p
			 (setq d (concat "~/.mozilla/" (user-login-name))))
			d)
                       (t (error "can't figure out your .mozilla profile"))))))
          (setq dir (car (directory-files dir "\\.slt$" nil 'dirs)))
          (or dir (error "couldn't figure mozilla salt directory"))
          (setq files (cons (concat dir "/cookies.txt") files))))

    ;; Galeon
    (if (file-directory-p "~/.galeon/mozilla/galeon")
        (setq files (cons "~/.galeon/mozilla/galeon/cookies.txt"
                          files)))
    ;; Emacs/W3 URL
    (if (file-directory-p "~/.url")
	(if (file-exists-p "~/.url/cookies")
	    (setq files (cons "~/.url/cookies"
                          files))))

    (while files
      (save-excursion
        (let (b)
          (unwind-protect
              (progn
                (setq b (get-buffer-create " *lj-cookie-tmp*"))
                (set-buffer b)
                (insert-file-contents (car files) nil nil nil t)
                (goto-char (point-min))
                (while (not (eobp))
                  (if (looking-at (concat "^\\([^\t]+\\)\t" ; 1 host
                                          "\\([^\t]+\\)\t" ; 2 bool
                                          "\\([^\t]+\\)\t" ; 3 path
                                          "\\([^\t]+\\)\t" ; 4 bool
                                          "\\([^\t]+\\)\t" ; 5 time_t
                                          "\\([^\t]+\\)\t" ; 6 key
                                          "\\([^\t]+\\)$")) ; 7 val
                      (let ((host (match-string 1))
                            (key (match-string 6))
                            (val (match-string 7)))
                        (if (string-match "^\\." host)
                            (setq host (concat "^.*"
                                               (regexp-quote host) "$"))
                          (setq host (concat "^" (regexp-quote host) "$")))
                        (if (and (string-match host hostname)
                                 (not (assoc key cookies)))
                            (setq cookies (cons (cons key val) cookies)))
                        ))
                  (forward-line 1))
                )
            ;; unwind-protected
            (if b (kill-buffer b)))))
      (if cookies
	  (progn
	    (setq files nil)
	    (setq advogato-id (cdr (assoc "id" cookies)))
	    cookies)
        (setq files (cdr files)))))
  advogato-id)

;;     (while files
;;       (setq url-cookie-file (car files))
;;       (url-cookie-parse-file url-cookie-file)
;;       (print url-cookie-file)
;;       (setq cookie (url-cookie-retrieve "www.advogato.org" "/"))
;;       (setq files (cdr files)))
;;   ;; if cookie for advogato.org is present, return (id . value), else nil
;;     (if cookie
;; 	;; cookie name is "Id" and value is the authentication string
;; 	(progn
;; 	  (setq files nil)
;; 	  (setq advogato-id (url-cookie-value cookie))
;; 	  (cons (url-cookie-name cookie) (url-cookie-value cookie)))
;;       cookie)))

(provide 'advogato)
