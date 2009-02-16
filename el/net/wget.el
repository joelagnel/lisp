;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: wget
;; Version: 1.1.0

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; History:
;;
;; - Fri Nov 02 2001 01:43 AM (burton@openprivacy.org): it now uses 100%
;; elisp.. IE no external wget binary needed.
;;
;; - Tue Oct 30 2001 08:22 PM (burton@openprivacy.org): enable html-mode if we
;; are viewing an html file.
;;
;; - Mon Oct 29 2001 07:04 PM (burton@openprivacy.org): modernized for Emacs 21.

;; Version 1.0.1:
;;
;; - Correctly paying attention to stderr.  Invalid URLs will not be detected
;;   and the user notified.
;;
;; - Create a new *wget* buffer for each new URL and dont' reuse the same
;;   buffer.
;;
;; - supports --save-headers from wget
;;
;; - http:// is now specified as the init for input
;;
;; - have wget-mode that highlights the firsts
;;   lines of  the buffer until the regexp ^$

;; Version 1.0:
;; init.

;;; Commentary:

;; This is an interface for wget.  Basically it allows you to pull down URLs and
;; then view the output in a buffer.  This is a too to help develop/debug web
;; applications.

;;; TODO:

;; - This is NOT synchronous AKA if we have a server that is remove and times
;; out Emacs will allow the user to type things and the metaphor becomes VERY
;; confusing!  This needs to be fixed!
;; 

;; - FIXME: why are we getting ^M chars in the output?
;;
;;    - I think I need to set my character conversion

;; - include a mechanism to support posting content to a webserver.  This would
;; prompt for the URL, method (get|post), and any specific HTTP headers to send.
;; It should save this URL and all its params in memory for future use. The the
;; user can repeat this again.

(require 'cl)

;;; Code:
(defface wget-headers-face '((t (:foreground "GoldenRod" :bold t)))
  "Face used to highlight the HTTP headers in the buffer.")

(defvar wget-current-url nil "Current URL for local buffer.")
(make-variable-buffer-local 'wget-current-url)

(defun wget(url)
  "Fetch the source for the given URL using wget and view it in a buffer."
  (interactive
   (list
    (read-string "URL: " "")))

  (let((inhibit-read-only t)
       (buffer-name nil)
       (hostname nil)
       (protocol nil)
       (port nil)
       (stream nil))
    (save-excursion 
      
      (setq buffer-name (concat "*wget " url "*"))
      
      (if (get-buffer buffer-name)
          (kill-buffer (get-buffer buffer-name)))
      
      (set-buffer (get-buffer-create buffer-name))

      (message "Fetching %s..." url)

      (assert (string-match "^\\(.*\\)://\\([^/:]+\\)\\(:\\([0-9]+\\)\\)?" url)
              nil "Unable to determine protocol and host.")

      (setq protocol (match-string 1 url))

      (when (null protocol)
          (error "Unable to determine protocol"))
      
      (setq host (match-string 2 url))

      (setq port (match-string 4 url))
      
      (if (string-equal protocol "http")
          (progn

            ;;provide a default port if we need it.
            (if (null port)
                (setq port 80)))
            
        (error (format "Unknown or unsupported protocol '%s'" protocol)))

      (setq stream (open-network-stream "wget" buffer-name host port))

      (setq wget-current-url url)

      ;;do the HTTP get now...  we need a better way to plugin protocols here.

      (set-process-sentinel stream 'wget-process-sentinel)
      
      (wget--protocol-http-do url stream))))

(defun wget--protocol-http-do(url stream)
  "Handle http protocol interaction."

  (save-excursion

    (set-buffer (process-buffer stream))

    (process-send-string stream (format "GET %s HTTP/1.0\n\n" url))

    (accept-process-output stream 30)))

(defun wget-process-sentinel(process event)
  "Run after wget to update misc items."

  (if (and (string-equal (process-name process) "wget")
           (or (string-match "^exited" event)
               (string-match "^finished" event)))

      (progn 
      
        (set-buffer (process-buffer process))

        (beginning-of-buffer)

        ;;(wget-highlight-buffer buffer-name)

        (message "Fetching %s...done" wget-current-url)

        ;;(wget-highlight-buffer (current-buffer))
        
        (wget-html-enable)

        (display-buffer (process-buffer process)))))
  
(defun wget-highlight-buffer(buffer-name)
  "Highlight misc information in the wget buffer."
  
  (save-excursion

    (set-buffer (get-buffer buffer-name))
    
    (beginning-of-buffer)

    (insert "--------------------------------------------------------------------------------\n")

    (assert (re-search-forward "^\n" nil t)
            nil "Could not find end of HTTP section.")

    (insert "--------------------------------------------------------------------------------\n")
    (end-of-line)

    (setq current-overlay (make-overlay 1 (point)))
      
    (overlay-put current-overlay 'face 'wget-headers-face)
    
    (overlay-put current-overlay 'window (get-buffer-window (current-buffer)))))

(defun wget-html-enable()
  "If the current buffer is html content.  Turn on `html-mode'."

  (if (re-search-forward "^Content-Type: text/html" nil t)
      (html-mode)))

(provide 'wget)

;;; wget.el ends here
