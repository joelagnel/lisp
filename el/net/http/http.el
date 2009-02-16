;;; http.el --- Get http pages from the net

;; Copyright (C) 1998-2001 Sean MacLennan
;; Revision:   1.6
;; XEmacs/Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(defvar http-proxy-server nil)
(defvar http-proxy-port   0)
(defvar http-wget-program "wget")
(defvar http-wget-options '("-sqO" "-"))
(defvar http-save-headers nil)
(defvar http-1.0 nil
  "*Non-nil if HTTP/1.0 required.")
(defvar http-zcat-program "zcat")

;; SAM TODO how to handle error output from wget

;; SAM Internal version does not handle chunked data properly

;;;###autoload
(defun http-get-page (url &optional bufname sentinel)
  "Send an http request for page URL.
Uses wget if possible."
  (interactive "sURL: ")
  (if (string-match "^gopher:" url)
      (gopher-get-page url bufname sentinel)
    ;; default everything
    (unless bufname  (setq bufname "*http get page*"))
    (let ((buff (get-buffer-create bufname)))
      (empty-buffer buff)
      ;; Use wget if it exists
      (if (or (exec-installed-p http-wget-program)
	      (file-exists-p    http-wget-program))
	  (let ((http (apply 'start-process "wget" buff
			     http-wget-program
			     (append http-wget-options (list url)))))
	    (unless sentinel (setq sentinel 'http-get-page-sentinel))
	    (set-process-sentinel http sentinel)
	    http)
	;; Do it internally
	(save-excursion
	  (set-buffer buff)
	  (http-get-page-internal url bufname sentinel))))))

;;;###autoload
(defun gopher-get-page (url &optional bufname sentinel)
  "Send a gopher request for page URL."
  (interactive "sURL: ")
  ;; default everything
  (unless bufname  (setq bufname "*gopher get page*"))
  (unless sentinel (setq sentinel 'gopher-loaded-sentinel))
  ;; Validate the URL and split into host/path
  (if (string-match "^gopher://" url)
      (setq url (replace-match "" nil nil url)))
  (unless (string-match "\\([^/]+\\)\\(.*\\)" url)
    (error "Invalid http url: %s" url))
  (let ((host (match-string 1 url))
	(path (match-string 2 url))
	(port 70)
	;; Create buffer and open network stream
	(buff (get-buffer-create bufname))
	http
	request)
    (when (string-match ":\\([0-9]+\\)" host)
      (setq port (match-string 1 host))
      (setq host (replace-match "" nil nil host)))
    (setq http (open-network-stream
		bufname buff
		(if http-proxy-server http-proxy-server host)
		port))
    (empty-buffer buff)
    ;; Get the page
    (set-process-sentinel http sentinel)
    (setq request (concat path "\r\n"))
    (process-send-string http request)
    http))

(defun gopher-loaded-sentinel (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (message "Page loaded. %s" str)))

;;;###autoload
(defun http-get-page-with-auth (url username password &optional bufname sentinel)
  "Send an http request for page URL with authorization.
Does not call wget."
  (interactive "sURL: \nsUsername: \ni")
  (unless password
    (setq password (comint-read-noecho "Password: " t)))

  (let ((auth (base64-encode-string (concat username ":" password))))
    (http-get-page-internal url bufname sentinel nil auth)))

;;;###autoload
(defun http-head-page (url &optional bufname sentinel)
  "Send an http HEAD request for page URL."
  (interactive "sURL: ")
  (http-get-page-internal url bufname sentinel t))

(defun http-get-page-internal (url &optional bufname sentinel head auth)
  "Send an http request for page URL.
This function does not use wget."
  ;; default everything
  (unless bufname  (setq bufname "*http get page*"))
  (unless sentinel (setq sentinel 'http-loaded-sentinel))
  ;; Validate the URL and split into host/path
  (if (string-match "^http://" url)
      (setq url (replace-match "" nil nil url)))
  (unless (string-match "\\([^/]+\\)\\(.*\\)" url)
    (error "Invalid http url: %s" url))
  (let ((host (match-string 1 url))
	(path (match-string 2 url))
	(port 80)
	;; Create buffer and open network stream
	(buff (get-buffer-create bufname))
	http
	request)
    (when (string-match ":\\([0-9]+\\)" host)
      (setq port (match-string 1 host))
      (setq host (replace-match "" nil nil host)))
    (setq http (open-network-stream
		bufname buff
		(if http-proxy-server http-proxy-server host)
		port))
    (empty-buffer buff)
    ;; Get the page
    (set-process-sentinel http sentinel)
    (setq request
	  (concat (if head "HEAD " "GET ")
		  (if http-proxy-server (concat "http://" host))
		  path
		  (if http-1.0
		      " HTTP/1.0\r\n"
		    (concat " HTTP/1.1\r\nHost: " host "\r\n"))
		  (if auth (concat "Authorization: Basic " auth "\r\n"))
		  "\r\n"))
    (process-send-string http request)
    http))

(defun http-loaded-sentinel (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (point-min))
    (unless (looking-at "HTTP/[1-9]\\.[0-9]+ 200")
      ;; http error
      (error "http request failed: %s"
	     (buffer-substring (point-min) (progn (end-of-line) (point)))))
    (message "Page loaded.")))

;;;; ------------------------------------------------------------------
;;;###autoload
(defun http-save-page (url filename &optional bufname sentinel)
  "Send a http request for page URL and save in file FILENAME.
This command will use wget if it exists."
  (interactive "sURL: \nsFile: ")
  (unless (string= filename "-")
    (setq filename (expand-file-name filename)))
  ;; if interactive, check if file exists
  (and (interactive-p)
       (file-exists-p filename)
       (not (y-or-n-p "File exists. Overwrite? "))
       (keyboard-quit))

  (unless bufname (setq bufname "*http-save-page*"))

  (let ((buff (get-buffer-create bufname)))
    (empty-buffer buff)
    ;; Use wget if it exists
    (if (or (exec-installed-p http-wget-program)
	    (file-exists-p    http-wget-program))
	(let ((http (start-process "wget" buff
				   http-wget-program
				   http-wget-options "-O" filename url)))
	  (unless sentinel (setq sentinel 'http-error-message))
	  (set-process-sentinel http sentinel)
	  http)
      ;; Do it internally
      (save-excursion
	(set-buffer buff)
	(set-visited-file-name filename t)
	(http-get-page-internal url bufname 'http-save-page-sentinel sentinel)
	))))

;;;; ------------------------------------------------------------------
;; SAM Cleanup
(defun http-get-page-sentinel (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (point-min))
    (if (looking-at "HTTP/.\\.. 200")
	(progn
	  (when (not http-save-headers)
	    ;; Remove the header - look for first blank line
	    (if (search-forward-regexp "^[ \t\r]*\n" nil t)
		(delete-region (point-min) (point))))
	  (message "Loaded."))
      ;; Failure
      (error "http request failed: %s"
	     (buffer-substring (point-min) (progn (end-of-line) (point)))))))

(defun http-save-page-sentinel (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (point-min))
    (if (looking-at "HTTP/.\\.. 200")
	(progn ;; Success
	  ;; Remove the header - look for first blank line
	  (if (search-forward-regexp "^[ \t]*[\r]?\n" nil t)
	      (delete-region (point-min) (point)))
	  ;; Save the file
	  (save-buffer))
      ;; Failure
      (error "http request failed: %s"
	     (buffer-substring (point-min) (progn (end-of-line) (point)))))))

;;;; ------------------------------------------------------------------
;;;###autoload
(defun http-error-message (proc str)
  "Print an error message based on the buffer output from wget."
  (unless (string= str "finished\n")
    (save-excursion
      (set-buffer (process-buffer proc))
      (goto-char (point-min))
      (end-of-line 3)
      (forward-char 1)
      (message "http request failed: %s"
	       (buffer-substring (point) (point-max))))))

(defun empty-buffer (buff)
  "Erase a specified buffer.
Emacs erase-buffer does not take a buffer arg."
  (let ((save (current-buffer)))
    (set-buffer buff)
    (erase-buffer)
    (set-buffer save)))

;;;; ------------------------------------------------------------------
;; Helper function to handle gzipped content
(defun http-gunzip ()
  (save-excursion
    (goto-char (point-min))
    (unless (search-forward "\n\n" nil t) (error "Could not find start"))
    (call-process-region (point) (point-max) http-zcat-program t t)
    ))

(provide 'http)
