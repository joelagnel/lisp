;;; mew-smime.el --- S/MIME for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Nov 12, 2004

;;; Code:

;;; for gpgsm only

(defvar mew-prog-smime "gpgsm")

(defvar mew-smime-ver nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; S/MIME check
;;;

(defun mew-smime-setup ()
  (if (mew-which-exec mew-prog-smime)
      (setq mew-smime-ver t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; S/MIME verification
;;;

(defun mew-smime-verify-check ()
  (goto-char (point-min))
  (if (search-forward "Good signature" nil t)
      (if (re-search-forward "aka \"\\(.*\\)\"" nil t)
	  (format "S/MIME Good signature <%s>" (mew-match-string 1))
	"S/MIME Good signature")
    (goto-char (point-min))
    (if (search-forward "invalid signature" nil t)
	"S/MIME Bad signature"
      (if (re-search-forward "invalid certification chain: \\(.*\\)" nil t)
	  (format "S/MIME signature %s" (mew-match-string 1))
	"S/MIME NEED TO HACK"))))

(defun mew-smime-verify (file1 file2)
  (message "S/MIME verifying...")
  (let (args ret)
    (with-temp-buffer
      (setq args (list "--verify" file2 file1))
      (apply 'mew-call-process-lang mew-prog-smime nil t nil args)
      (setq ret (mew-smime-verify-check)))
    (message "S/MIME verifying...done")
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; S/MIME xxx
;;;

(defun mew-smime-sign-message (&optional arg)
  "Sign the entire draft with S/MIME. Input your passphrase."
  (interactive "P")
  (mew-smime-encode-message 'smime-signature arg))

(defun mew-smime-encode-message (type &optional ask-singer)
  (if (null mew-smime-ver)
      (message "%s does not exist" mew-prog-smime)
    (if (and ask-singer (string-match "signature" (symbol-name type)))
	(mew-draft-make-message type (car (mew-input-address "Who's key?: ")))
      (mew-draft-make-message type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; S/MIME signing
;;;

(defvar mew-smime-signature-suffix ".p7s")

(defvar mew-smime-hash-alist ;; xxx
  '(("1" . "md5")
    ("2" . "sha1")
    ("3" . "sha256")
    ("4" . "sha384")
    ("5" . "sha512")))

;; xxx the --output option has not been implemented yet.

(defun mew-smime-sign (file1)
  (message "S/MIME signing...")
  (let ((process-connection-type mew-connection-type2)
	(loption "--local-user")
	(soptions '("--detach-sign" "--include-certs" "3")) ;; xxx
	(prog mew-prog-smime)
	file2 file3)
    (setq file2 (concat (mew-make-temp-name) mew-smime-signature-suffix))
    (setq file3 (mew-make-temp-name))
    ;; not perfectly unique but OK
    (with-temp-buffer
      (mew-piolet
       mew-cs-binary mew-cs-binary
       (apply 'mew-call-process-lang
	      prog
	      nil (list (current-buffer) file3) nil
	      (append soptions (list loption mew-inherit-encode-signer file1)))
       (write-region (point-min) (point-max) file2 nil 'no-msg)))
    (mew-delete-file file3)
    (message "S/MIME signing...done")
    (list file2 mew-b64 "sha1" nil)))

(provide 'mew-smime)

;;; Copyright Notice:

;; Copyright (C) 2004 Mew developing team.
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

;;; mew-smime.el ends here
