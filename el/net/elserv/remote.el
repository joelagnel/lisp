;;; remote.el -- Emacs remote controller.

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: Remote Controll, WWW

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:

;; This program requires Elserv -- Yet another HTTP server on Emacsen
;; which is available at: http://www.gohome.org/elserv/
;; Please install it first.

;;; History:
;;

(require 'elserv)

;;; Code:

(defvar remote-controller-command-page-body
  "<H2>Emacs リモコン</H2><form action=\"/\" method=\"POST\">
コマンド: <input type=\"text\" name=\"command\">
<input type=\"submit\" value=\"おくる\">
<input type=\"reset\" value=\"クリア\"><br>"
  "Page body of /command.")

(defvar remote-controller-key-page-body
  "<H2>Emacs リモコン</H2><form action=\"/key\" method=\"POST\">
キー入力: <input type=\"text\" name=\"key\">
<input type=\"submit\" value=\"おくる\">
<input type=\"reset\" value=\"クリア\"><br>"
  "Page body of /key.")

(defvar remote-controller-eval-page-body
  "<H2>Emacs リモコン</H2><form action=\"/eval\" method=\"POST\">
eval:<br> <textarea name=\"eval\" rows=\"10\" cols=\"60\"></textarea><br>
<input type=\"submit\" value=\"おくる\">
<input type=\"reset\" value=\"クリア\"><br>"
  "Page body of /eval.")

(defvar remote-controller-default-port 8001)

(defun remote-controller-function (result path ppath request)
  "Elserv publish function.
RESULT, PATH, PPATH and REQUEST are given arguments."
  (let ((body (elserv-url-decode (plist-get request 'body)))
	data message)
    (when body
      (with-current-buffer (window-buffer (selected-window))
	(cond
	 ((and (setq data (cdr (assoc "command" body)))
	       (> (length data) 0))
	  (call-interactively (intern data)))
	 ((and (setq data (cdr (assoc "key" body)))
	       (> (length data) 0))
	  (let ((binding (or (lookup-key (current-local-map) data t)
			     (lookup-key global-map data t))))
	    (if (and binding
		     (not (numberp binding)))
		(if (eq binding 'self-insert-command)
		    (insert data)
		  (call-interactively binding))
	      (setq message (format "key `%s' is not bound." data)))))
	 ((and (setq data (cdr (assoc "eval" body)))
	       (> (length data) 0))
	  (if (setq message (eval (read data)))
	      (setq message (prin1-to-string message)))))))
    (elserv-set-result-header
     result
     '(content-type "text/html; charset=\"iso-2022-jp\""))
    (elserv-set-result-body
     result
     (encode-coding-string
      (concat
       "<html><head><title>Remote Controller</title></head><body>"
       (symbol-value (intern (format "remote-controller-%s-page-body"
				     (substring (if (string= ppath "/")
						    "/command" ppath) 1))))
       "<a href=\"/\">コマンド</a> <a href=\"key\">キー入力</a> <a href=\"eval\">eval</a> <a href=\"/buffers\">バッファ一覧</a><hr>"
       message
       "</body></html>") 'iso-2022-jp))))

;; Original is elserv-demo-buffers
(defun remote-controller-buffers (result path ppath request)
  "Elserv publish function.
RESULT, PATH, PPATH and REQUEST are given arguments."
  (let ((buf (and (not (string= path ""))
		  (get-buffer (substring path 1)))))
    (if buf
	(progn
	  (elserv-set-result-header result '(content-type "text/plain"))
	  (elserv-set-result-body
	   result
	   (with-current-buffer buf
	     (encode-coding-string (buffer-string) 'iso-2022-jp))))
      (elserv-set-result-header result '(content-type "text/html; charset=iso-2022-jp"))
      (elserv-set-result-body
       result
       (concat
	(encode-coding-string
	 "<H2>Emacs リモコン</H2>バッファ一覧<br><ul>"
	 'iso-2022-jp)
	(mapconcat
	 (function
	  (lambda (buf)
	    (let ((name (buffer-name buf)))
	      (unless (string= (substring name 0 1) " ")
		(concat "<li><a href=\""
			ppath
			"/"
			(mapconcat 'identity (split-string
					      name " ") "+")
			"\">"
			name "</a>\n")))))
	 (buffer-list)
	 "")
	(encode-coding-string
	 "</ul><br><a href=\"/\">コマンド</a> <a href=\"key\">キー入力</a> <a href=\"eval\">eval</a> <a href=\"/buffers\">バッファ一覧</a>"
	 'iso-2022-jp
	 ))))))

(defun remote-controller (&optional port)
  "Start remote controller.
Optional argument PORT is used as port number for the remote controller."
  (interactive (if current-prefix-arg
		   (list (string-to-number (read-from-minibuffer "Port: ")))))
  (elserv-start (or port remote-controller-default-port))
  (elserv-publish (elserv-find-process)
		  "/"
		  :function 'remote-controller-function)
  (elserv-publish (elserv-find-process)
		  "/key"
		  :function 'remote-controller-function)
  (elserv-publish (elserv-find-process)
		  "/eval"
		  :function 'remote-controller-function)
  (elserv-publish (elserv-find-process)
		  "/buffers"
		  :function 'remote-controller-buffers)
  (message "Access 'http://%s:%d/' to controll this Emacs!"
	   (system-name) (or port remote-controller-default-port)))

(provide 'remote)

;;; remote.el ends here
