;;; mew-ssl.el

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Jul 25, 2002

;;; Code:

(require 'mew)

(defvar mew-prog-ssl "stunnel")
(defvar mew-ssl-cert-directory "~/.certs"
  "The directory where certificates of root servers are stored.
A file name of a certificate should be 'cert-hash.0'.
'cert-hash' can be extracted by 'openssl x509 -hash -noout -in cert.pem'.")

(defvar mew-ssl-verify-level 1
  "Verification level of server's certificate.
0 - no verification.
1 - verify server's certificate if present. If verification failed, an
    SSL/TLS connection is not created. If not present, an SSL/TLS connection
    is created.
2 - verify server's certificate. If verification failed, an SSL/TLS
    connection is not created. If not present, an SSL/TLS connection is
    not created.
3 - verify server's certificate which locally installed (not one from
    the server).")

(defvar mew-prog-ssl-arg nil) ;; xxx what about v4?

(defvar mew-ssl-ver nil)

(defconst mew-ssl-process-exec-cnt 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Magic words
;;;

(defconst mew-tls-smtp "smtp")
(defconst mew-tls-pop  "pop3")
(defconst mew-tls-nntp "nntp")
(defconst mew-tls-imap "imap") ;; xxx stunnel does not support this.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SSL/TLS info
;;;

(defvar mew-ssl-info-list '("status" "try" "file"))

(mew-info-defun "mew-ssl-" mew-ssl-info-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process name
;;;

(defconst mew-ssl-info-prefix "mew-ssl-info-")

(defsubst mew-ssl-info-name (server remoteport localport)
  (format "%s:%s:%s:%d" mew-ssl-info-prefix server remoteport localport))

(defsubst mew-ssl-info-name-regex (server remoteport)
  (format "^%s:%s:%s" mew-ssl-info-prefix server remoteport))

(defsubst mew-ssl-pnm-to-lport (pnm)
  (if (string-match ":\\([0-9]+\\)$" pnm) (match-string 1 pnm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Executing SSL/TLS
;;;

(defun mew-ssl-options (case server remoteport localport tls)
  (if (= mew-ssl-ver 3)
      (let (args)
	(setq args
	      `("-c" "-f"
		"-a" ,(expand-file-name (mew-ssl-cert-directory case))
		"-d" ,(format "localhost:%d" localport)
		"-v" ,(int-to-string (mew-ssl-verify-level case))
		"-D" "debug"
		"-P" "none"
		"-r" ,(format "%s:%s" server remoteport)
		,@mew-prog-ssl-arg))
	(if tls (setq args (cons "-n" (cons tls args))))
	args)
    (let ((file (mew-make-temp-name)))
      (with-temp-buffer
	(insert "client=yes\n")
	(insert "pid=\n")
	(insert (format "verify=%d\n" (mew-ssl-verify-level case)))
	(insert "foreground=yes\n")
	(insert "debug=debug\n")
	(insert "CApath=" (expand-file-name (mew-ssl-cert-directory case)) "\n")
	(insert (format "[%d]\n" localport))
	(insert (format "accept=localhost:%d\n" localport))
	(insert (format "connect=%s:%s\n" server remoteport))
	(if tls (insert (format "protocol=%s\n" tls)))
	(mew-frwlet
	 mew-cs-dummy mew-cs-text-for-write
	 ;; NEVER use call-process-region for privacy reasons
	 (write-region (point-min) (point-max) file nil 'no-msg))
	(list file)))))

(defun mew-open-ssl-stream (case server serv tls)
  "Open an SSL/TLS stream for SERVER's SERV.
This function returns a process when an SSL/TLS connection is created
successfully. 
If TLS is nil, an SSL connection is created.
If TLS is a magic word for 'stunnel', a TLS connection is created.
A local port number can be obtained the process name after ':'. "
  (cond
   ((not (and (stringp server) (stringp serv)))
    nil)
   ((or (null mew-ssl-ver) (not (mew-which-exec mew-prog-ssl)))
    (message "'%s' is not found" mew-prog-ssl)
    nil)
   (t
    (let* ((remoteport (mew-serv-to-port serv))
	   (localport (+ 8000 (% (mew-random) 4000)))
	   (process-connection-type mew-connection-type2)
	   (i 0) (N mew-ssl-process-exec-cnt)
	   (pros (process-list))
	   (regex (mew-ssl-info-name-regex server remoteport))
	   buf name pnm pro dummy bound opts)
      (catch 'find
	(while pros
	  (when (string-match regex (process-name (car pros)))
	    (if (memq (process-status (car pros)) '(run))
		(setq pro (car pros))
	      (delete-process (car pros)))
	    (throw 'find nil))
	  (setq pros (cdr pros))))
      (if pro
	  pro
	(message "Creating an SSL/TLS connection...")
	(setq pro nil)
	(setq buf (generate-new-buffer (concat mew-buffer-prefix "ssl")))
	(catch 'loop
	  (while (< i N)
	    (setq name (mew-ssl-info-name server remoteport localport))
	    (setq opts (mew-ssl-options case server remoteport localport tls))
	    (setq pro (apply 'start-process name buf mew-prog-ssl opts))
	    ;; An error would occur. So, let's exit in the case.
	    (cond
	     ((not (processp pro))
	      (message "Creating an SSL/TLS connection...FAILED")
	      (throw 'loop nil))
	     ((not (memq (process-status pro) '(run)))
	      (delete-process pro)
	      (message "Creating an SSL/TLS connection...FAILED")
	      (throw 'loop nil)))
	    ;; stunnel is now running.
	    (process-kill-without-query pro)
	    (setq pnm (process-name pro))
	    (mew-info-clean-up pnm)
	    (mew-ssl-set-try pnm 0)
	    (if (= mew-ssl-ver 4) (mew-ssl-set-file pnm (car opts)))
	    (mew-set-process-cs pro mew-cs-text-for-read mew-cs-text-for-write)
	    (set-process-filter pro 'mew-ssl-filter1)
	    (set-process-sentinel pro 'mew-ssl-sentinel)
	    (mew-rendezvous (null (mew-ssl-get-status pnm)))
	    (if (eq (mew-ssl-get-status pnm) t)
		(throw 'loop (setq bound t)))
	    ;; bind-failure
	    (setq localport (1+ localport))
	    (setq i (1+ i))))
	(mew-ssl-set-status pnm nil)
	(if (not bound)
	    (progn
	      (message "Creating an SSL/TLS connection...FAILED")
	      nil)
	  ;; "stunnel" does not gain access to the remote port
	  ;; until a tunneled connection is created.
	  ;; So, we need to check the SSL/TLS tunnel with a dummy
	  ;; tunneled connection here.
	  (set-process-filter pro 'mew-ssl-filter2)
	  (setq dummy (open-network-stream " *Mew dummy*" nil "localhost" localport))
	  (mew-rendezvous (null (mew-ssl-get-status pnm)))
	  (if (processp dummy) (delete-process dummy))
	  (if (eq (mew-ssl-get-status pnm) t)
	      (progn
		(message "Creating an SSL/TLS connection...done")
		(set-process-filter pro 'mew-ssl-filter3)
		pro)
	    ;; verify-failure
	    (delete-process pro)
	    (message "Creating an SSL/TLS connection...FAILED (cert verify failure)")
	    nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun mew-ssl-debug (label string)
  (when (mew-debug 'net)
    (save-excursion
      (set-buffer (get-buffer-create mew-buffer-debug))
      (goto-char (point-max))
      (insert (format "\n<%s>\n%s\n" label string)))))

(defun mew-ssl-filter1 (process string)
  (mew-ssl-debug "SSL/TLS: " string)
  (let ((pnm (process-name process)))
    (cond
     ((string-match "bound to" string)
      (mew-ssl-set-status pnm t))
     ((string-match "gethostbyname: Valid name, no data record of requested type" string)
      (mew-ssl-set-status pnm 'gethostbyname-failure))
     ((string-match "gethostbyname: Host not found" string)
      (mew-ssl-set-status pnm 'gethostbyname-failure))
     ((string-match "Local: bind: Address already in use" string)
      (mew-ssl-set-status pnm 'bind-failure)))))

(defun mew-ssl-filter2 (process string)
  (mew-ssl-debug "SSL/TLS: " string)
  (let ((pnm (process-name process)))
    (cond
     ((string-match "Negotiated ciphers\\|opened with SSL" string)
      (mew-ssl-set-status pnm t))
     ((string-match "Failed to initialize" string)
      (mew-ssl-set-status pnm t)) ;; xxx
     ((string-match "verify failed" string)
      (mew-ssl-set-status pnm 'verify-failure)))))

(defun mew-ssl-filter3 (process string)
  (mew-ssl-debug "SSL/TLS: " string))

(defun mew-ssl-sentinel (process event)
  (let* ((pnm (process-name process))
	 (file (mew-ssl-get-file pnm)))
    (mew-delete-file file)
    (mew-remove-buffer (process-buffer process))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; stunnel version check 
;;;

(defun mew-ssl-setup ()
  (if (not (mew-which-exec mew-prog-ssl))
      (setq mew-ssl-ver nil)
    (let ((status (call-process mew-prog-ssl nil nil nil "-V")))
      (if (= status 0)
	  (setq mew-ssl-ver 3)
	(setq mew-ssl-ver 4)))))

(provide 'mew-ssl)

;;; Copyright Notice:

;; Copyright (C) 2002-2005 Mew developing team.
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

;;; mew-ssl.el ends here
