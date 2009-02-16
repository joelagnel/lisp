;; Copyright (C) 2001 Sami Salkosuo
;; Author: Sami Salkosuo @roguemail.net="">
;; Version: 0.1 Fri Oct 12 16:22:27 2001

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Commentary:
;;
;; Get HTTP headers from specified URL.
;;
;; Installation:
;;
;; Add http-headers.el to your load path and add
;; (require 'http-headers)
;; to .emacs
;;
;; If using http-headers from behind proxy
;; (setq http-headers-proxy-host )
;; (setq http-headers-proxy-port )
;;
;; Usage:
;;
;; M-x http-headers and type url.

(defvar http-headers-proxy-host nil
  "HTTP proxy host")

(defvar http-headers-proxy-port nil
  "HTTP proxy port")

(defun http-headers (url)
  "Fetches HTTP headers from specified url"
  (interactive "sURL: http://")
  ;;create new buffer
  (let
      ((tcp-connection)       
       (temp-buf)
       (buf)
       (host)
       (port)
       (file)
       (content-text)
       (header-end)
       (request)
       (tmp)
       )

    (setq temp-buf (get-buffer-create "*HTTP headers*"))
    (set-buffer temp-buf)
    (erase-buffer)
    (goto-char 0)
    ;;get host and port from url
    ;;(setq port (string-to-number (read-string "Port: " "80" )))
    ;;(setq file (read-string "Path: " "/"))

    ;;set proxy if needed
    (if http-headers-proxy-host
	(progn
	  ;;(setq file (concat "http://" host ":" (number-to-string port) file))
	  (setq file (concat "http://" url))
	  (setq host http-headers-proxy-host)
	  (setq port http-headers-proxy-port)
	  )
      (progn
	(setq host (substring url 0 (string-match "/" url)))
	(if (string-match ":" host)
	    (setq port (string-to-number (substring host (string-match ":" host))))
	  (setq port 80)
	  )
	(setq host (substring url 0 (string-match ":" url)))
	(if (string-match "/" url)
	    (setq file (substring url  (string-match "/" url)))
	  (setq file "/")
	  )
	)
      )

    (setq tcp-connection
	  (open-network-stream
	   "GET process-name"
	   temp-buf
	   host
	   port
	   ))
    (set-marker (process-mark tcp-connection) (point-min))
    (set-process-sentinel tcp-connection 'http-headers-sentinel)
    (setq request (concat "GET " file " HTTP/1.0\n\n"))
    (process-send-string tcp-connection request)
    (if http-headers-proxy-host
	(setq tmp file)
      (setq tmp (concat "http://" url ":" (number-to-string port) file))
      )
    (http-headers-parse tmp tcp-connection)
    (kill-buffer temp-buf)
    (delete-process tcp-connection)  
    )
  )

(defun http-headers-parse (url process)
  "Parse header"
  (let (
	(buffer)
	(headers)
	(header-end)
	)
    (while (eq (process-status process) 'open)
      (sit-for 0 200)
      )
    (setq buffer (get-buffer-create "*HTTP headers*"))
    (set-buffer buffer)
    (goto-char 0)
    (setq header-end (re-search-forward "\n\n" nil t))
    (delete-region header-end (point-max))
    (setq headers (buffer-string))
    (with-electric-help
     '(lambda () (insert "Headers from " url "\n\n" headers) (goto-char 0) "*HTTP-headers*")
     )
    
    )
  )

(defun http-headers-sentinel (process string)
  "Process the results from the efine network connection.
process - The process object that is being notified.
string - The string that describes the notification."

  )

(provide 'http-headers)
;; Copyright (C) 2001 Sami Salkosuo
;; Author: Sami Salkosuo @roguemail.net="">
;; Version: 0.1 Fri Oct 12 16:22:27 2001

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Commentary:
;;
;; Get HTTP headers from specified URL.
;;
;; Installation:
;;
;; Add http-headers.el to your load path and add
;; (require 'http-headers)
;; to .emacs
;;
;; If using http-headers from behind proxy
;; (setq http-headers-proxy-host )
;; (setq http-headers-proxy-port )
;;
;; Usage:
;;
;; M-x http-headers and type url.

(defvar http-headers-proxy-host nil
  "HTTP proxy host")

(defvar http-headers-proxy-port nil
  "HTTP proxy port")

(defun http-headers (url)
  "Fetches HTTP headers from specified url"
  (interactive "sURL: http://")
  ;;create new buffer
  (let
      ((tcp-connection)       
       (temp-buf)
       (buf)
       (host)
       (port)
       (file)
       (content-text)
       (header-end)
       (request)
       (tmp)
       )

    (setq temp-buf (get-buffer-create "*HTTP headers*"))
    (set-buffer temp-buf)
    (erase-buffer)
    (goto-char 0)
    ;;get host and port from url
    ;;(setq port (string-to-number (read-string "Port: " "80" )))
    ;;(setq file (read-string "Path: " "/"))

    ;;set proxy if needed
    (if http-headers-proxy-host
	(progn
	  ;;(setq file (concat "http://" host ":" (number-to-string port) file))
	  (setq file (concat "http://" url))
	  (setq host http-headers-proxy-host)
	  (setq port http-headers-proxy-port)
	  )
      (progn
	(setq host (substring url 0 (string-match "/" url)))
	(if (string-match ":" host)
	    (setq port (string-to-number (substring host (string-match ":" host))))
	  (setq port 80)
	  )
	(setq host (substring url 0 (string-match ":" url)))
	(if (string-match "/" url)
	    (setq file (substring url  (string-match "/" url)))
	  (setq file "/")
	  )
	)
      )

    (setq tcp-connection
	  (open-network-stream
	   "GET process-name"
	   temp-buf
	   host
	   port
	   ))
    (set-marker (process-mark tcp-connection) (point-min))
    (set-process-sentinel tcp-connection 'http-headers-sentinel)
    (setq request (concat "GET " file " HTTP/1.0\n\n"))
    (process-send-string tcp-connection request)
    (if http-headers-proxy-host
	(setq tmp file)
      (setq tmp (concat "http://" url ":" (number-to-string port) file))
      )
    (http-headers-parse tmp tcp-connection)
    (kill-buffer temp-buf)
    (delete-process tcp-connection)  
    )
  )

(defun http-headers-parse (url process)
  "Parse header"
  (let (
	(buffer)
	(headers)
	(header-end)
	)
    (while (eq (process-status process) 'open)
      (sit-for 0 200)
      )
    (setq buffer (get-buffer-create "*HTTP headers*"))
    (set-buffer buffer)
    (goto-char 0)
    (setq header-end (re-search-forward "\n\n" nil t))
    (delete-region header-end (point-max))
    (setq headers (buffer-string))
    (with-electric-help
     '(lambda () (insert "Headers from " url "\n\n" headers) (goto-char 0) "*HTTP-headers*")
     )
    
    )
  )

(defun http-headers-sentinel (process string)
  "Process the results from the efine network connection.
process - The process object that is being notified.
string - The string that describes the notification."

  )

(provide 'http-headers)
