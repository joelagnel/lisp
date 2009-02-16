;;; esmtpmail.el --- simple SMTP protocol (RFC 821) for sending mail

;; Copyright (C) 1995, 1996 Free Software Foundation, Inc.

;; Author: Tomoji Kagatani <kagatani@rbc.ncl.omron.co.jp>
;; Maintainer: Brian D. Carlstrom <bdc@ai.mit.edu>
;; ESMTP support: Simon Leinen <simon@switch.ch>
;; Keywords: mail

;; AUTH=LOGIN, multiple SMTP servers based on From header, etc. support
;; Author:      Robert Fenk
;; Status:      Tested with XEmacs 21.1.10 & GNU Emacs 20.7 & VM 6.82
;; Keywords:    extensions, vm
;; X-URL:       http://www.robf.de/Hacking/elisp
;; X-RCS:       $Id: esmtpmail.el,v 1.12 2004/06/11 08:25:20 fenk Exp $

;; This file is NOT part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: FSF 20.7

;;; Commentary:

;; Send Mail to smtp host from esmtpmail temp buffer.

;; Please add these lines in your .emacs(_emacs) or use customize.
;;
;;(setq send-mail-function 'esmtpmail-send-it) ; if you use `mail'
;;(setq message-send-mail-function 'esmtpmail-send-it) ; if you use `message'
;;(setq esmtpmail-default-smtp-server "YOUR SMTP HOST")
;;(setq esmtpmail-local-domain "YOUR DOMAIN NAME")
;;(setq esmtpmail-debug-info t) ; only to debug problems
;;(load-library "esmtpmail")
;;(setq esmtpmail-code-conv-from nil)
;;(setq user-full-name "YOUR NAME HERE")

;; To queue mail, set esmtpmail-queue-mail to t and use 
;; esmtpmail-send-queued-mail to send.

;;; Code:


(require 'mail-utils)
(require 'sendmail)
(require 'time-stamp)

(eval-and-compile
  (let ((feature-list '(base64 vm-version vm-macro vm-vars vm-misc vm-pop
                               vm-minibuf)))
    (while feature-list
      (condition-case nil
          (require (car feature-list))
        (error
         (if (load (format "%s" (car feature-list)) t)
             nil
           (message "Could not load feature %S.  Related functions may not work correctly!" (car feature-list))
           (beep 1))
	 nil))
      (setq feature-list (cdr feature-list)))))

;; Some aliases for GNU Emacs
(eval-and-compile
  (if (not (functionp 'warn))
      (defun warn (&rest args)
        (beep 1)
        (sit-for 2)
        (apply 'message args)))
  (if (not (functionp 'read-passwd))
      (defalias 'read-string 'read-passwd))
  (if (not (functionp 'user-mail-address))
      (defun user-mail-address () user-mail-address)))
;;;
(defgroup esmtpmail nil
  "SMTP protocol for sending mail."
  :tag "Enhanced smtpmail.el"
  :group 'mail)

(defcustom esmtpmail-default-smtp-server nil
  "*Specify default SMTP server."
  :group 'esmtpmail
  :type '(choice (const nil) string))

(defcustom esmtpmail-smtp-server 
  (or (getenv "SMTPSERVER") esmtpmail-default-smtp-server)
  "*The name of the host running SMTP server."
  :group 'esmtpmail
  :type '(choice (const nil) string))

(defcustom esmtpmail-smtp-service 25
  "*SMTP service port number. smtp or 25 ."
  :group 'esmtpmail
  :type '(choice (integer :tag "Port") (string :tag "Service")))

(defcustom esmtpmail-local-domain nil
  "*Local domain name without a host name.
If the function (system-name) returns the full internet address,
don't define this value."
  :group 'esmtpmail
  :type '(choice
          (const :tag "None" nil)
          (string :tag "Name")))

(defcustom esmtpmail-debug-info nil
  "*esmtpmail debug info printout. messages and process buffer."
  :group 'esmtpmail
  :type 'boolean)

(defcustom esmtpmail-code-conv-from nil ;; *junet*
  "*esmtpmail code convert from this code to *internal*..for tiny-mime.."
  :group 'esmtpmail
  :type 'boolean)

(defcustom esmtpmail-queue-mail nil 
  "*Specify if mail is queued (if t) or sent immediately (if nil).
If queued, it is stored in the directory `esmtpmail-queue-dir'
and sent with `esmtpmail-send-queued-mail'."
  :group 'esmtpmail
  :type 'boolean)

(defcustom esmtpmail-queue-dir "~/Mail/queued-mail/"
  "*Directory where `esmtpmail.el' stores queued mail."
  :group 'esmtpmail
  :type 'directory)

(defvar esmtpmail-queue-index-file "index"
  "File name of queued mail index,
This is relative to `esmtpmail-queue-dir'.")

(defvar esmtpmail-text-buffer)
(defvar esmtpmail-address-buffer)
(defvar esmtpmail-recipient-address-list)

;; Buffer-local variable.
(defvar esmtpmail-read-point)

(defvar esmtpmail-queue-index (concat esmtpmail-queue-dir
                                     esmtpmail-queue-index-file))

(defvar vm-spool-files nil)

(define-widget 'vm-spool-file 'choice
  "An association list."
   :key-type '(string :tag "Key")
   :value-type '(sexp :tag "Value")
   :convert-widget 'widget-vm-spool-file-convert-widget
   :tag "vm-spool-file")

(defun widget-vm-spool-file-convert-widget (widget)
  (let ((args (append '((string :tag "Spoolfile name"))
                      (delete nil
                              (mapcar
                               (lambda (s)
                                 (setq s (cadr s))
                                 (if (stringp s)
                                     (list 'const s)))
                               vm-spool-files)))))
    (widget-put widget :args args)
    widget))

(defcustom esmtpmail-send-it-by-alist
  nil
  "*alist describing which SMTP host and authentication method should be used
for a specified From header or condition.  This list will be checked in order
to know weather the current message should not be send by the default SMPT
host and port.

The format of an element is as follows: 
    (FROM-CONDITION SMTP-HOST (AUTHENTICATION-METHOD [METHOD-DATA]))

FROM-CONDITION has to be a regexp matching the author of the message
or a sexp evaluating to t or nil.

SMTP-HOST is the host name or a list of host name and port.

AUTHENTICATION-METHOD and METHOD-DATE have to be one of
    helo
    ehlo
    login USER [PASSWORD]
    vm-pop-login VM-SPOOLNAME
    vm-after-pop VM-SPOOLNAME
    vm-after-pop-helo VM-SPOOLNAME

Where VM-SPOOLNAME is one used in `vm-spool-files'!

Examples:  (\"me@yahoo.com\" \"mail.yahoo.com\"
            (login \"me\"))
           (\"me@yahoo.com\" \"mail.yahoo.com\"
            (vm-pop-login \"pop.yahoo.com:110:pass:me:*\"))
           (t \"local.smtphost.com\") ;; default SMTP host"
  :group 'esmtpmail
  :type
  '(choice
    :tag "Authentication type" :value nil
    (const :tag "No (multiple) SMTP hosts." nil)
    (repeat :tag "Rule set"
            (list
             :tag "Rule"
             (choice
              :tag "Condition" :value t
              (const  :tag "always" t)
              (regexp :tag "if From matches regexp" "YOUR-EMAIL"))
             (string :tag "sent messages to hostname" "YOUR.SMTP.SERVER")
             (choice
             :tag "and use" :value nil
             (const :tag "Default Protocol" nil)
             (const :tag "HELO" helo)
             (const :tag "EHLO" ehlo)
             (list :tag "login"
                   (const :tag "with account" login)
                   (string :tag "user" "YOUR-LOGIN")
                   (choice :tag "and password"
                           (const :tag "None" nil)
                           (string "YOUR-PASSWD")))
             (list :tag "VM"
                   (choice
                    (const :tag "login is same as for POP"
                           vm-pop-login)
                    (const :tag "POP before sending + EHLO"
                           vm-after-pop)
                    (const :tag "POP before sending + HELO "
                           vm-after-pop-helo))
                   (vm-spool-file :tag "vm-spool-name")))))))


(defvar esmtpmail-smtp-login-and-password nil
  "alist storing ((host login) elogin password) elements for authentication.")

(defcustom esmtpmail-timeout 20
  "*Number of seconds before a timeout occurs."
  :group 'esmtpmail
  :type 'integer)

;;;
;;;
;;;

;;;###autoload
(defun esmtpmail-send-it ()
  (require 'mail-utils)
  (let ((errbuf (if mail-interactive
                    (generate-new-buffer " esmtpmail errors")
                  0))
        (tembuf (generate-new-buffer " esmtpmail temp"))
        (case-fold-search nil)
        delimline
        (mailbuf (current-buffer)))
    (unwind-protect
        (save-excursion
          (set-buffer tembuf)
          (erase-buffer)
          (insert-buffer-substring mailbuf)
          (goto-char (point-max))
          ;; require one newline at the end.
          (or (= (preceding-char) ?\n)
              (insert ?\n))
          ;; Change header-delimiter to be what sendmail expects.
          (goto-char (point-min))
          (re-search-forward
            (concat "^" (regexp-quote mail-header-separator) "\n"))
          (replace-match "\n")
          (backward-char 1)
          (setq delimline (point-marker))
;;        (sendmail-synch-aliases)
          (if (and mail-aliases (fboundp 'expand-mail-aliases)) ; XEmacs
              (expand-mail-aliases (point-min) delimline))
          (goto-char (point-min))
          ;; ignore any blank lines in the header
          (while (and (re-search-forward "\n\n\n*" delimline t)
                      (< (point) delimline))
            (replace-match "\n"))
          (let ((case-fold-search t))
            ;; We used to process Resent-... headers here,
            ;; but it was not done properly, and the job
            ;; is done correctly in esmtpmail-deduce-address-list.
            ;; Don't send out a blank subject line
            (goto-char (point-min))
            (if (re-search-forward "^Subject:\\([ \t]*\n\\)+\\b" delimline t)
                (replace-match "")
              ;; This one matches a Subject just before the header delimiter.
              (if (and (re-search-forward "^Subject:\\([ \t]*\n\\)+"
                                          delimline t)
                       (= (match-end 0) delimline))
                  (replace-match "")))
            ;; Put the "From:" field in unless for some odd reason
            ;; they put one in themselves.
            (goto-char (point-min))
            (if (not (re-search-forward "^From:" delimline t))
                (let* ((login (user-mail-address))
                       (fullname (user-full-name)))
                  (cond ((eq mail-from-style 'angles)
                         (insert "From: " fullname)
                         (let ((fullname-start (+ (point-min) 6))
                               (fullname-end (point-marker)))
                           (goto-char fullname-start)
                           ;; Look for a character that cannot appear unquoted
                           ;; according to RFC 822.
                           (when (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
                                                    fullname-end 1)
                             ;; Quote fullname, escaping specials.
                             (goto-char fullname-start)
                             (insert "\"")
                             (while (re-search-forward "[\"\\]"
                                                       fullname-end 1)
                               (replace-match "\\\\\\&" t))
                             (insert "\"")))
                         (insert " <" login ">\n"))
                        ((eq mail-from-style 'parens)
                         (insert "From: " login " (")
                         (let ((fullname-start (point)))
                           (insert fullname)
                           (let ((fullname-end (point-marker)))
                             (goto-char fullname-start)
                             ;; RFC 822 says \ and nonmatching parentheses
                             ;; must be escaped in comments.
                             ;; Escape every instance of ()\ ...
                             (while (re-search-forward "[()\\]" fullname-end 1)
                               (replace-match "\\\\\\&" t))
                             ;; ... then undo escaping of matching parentheses,
                             ;; including matching nested parentheses.
                             (goto-char fullname-start)
                             (while (re-search-forward 
                                     "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
                                     fullname-end 1)
                               (replace-match "\\1(\\3)" t)
                               (goto-char fullname-start))))
                         (insert ")\n"))
                        ((null mail-from-style)
                         (insert "From: " login "\n")))))

            ;; Insert an extra newline if we need it to work around
            ;; Sun's bug that swallows newlines.
            (goto-char (1+ delimline))
            (if (eval mail-mailer-swallows-blank-line)
                (newline))
            ;; Find and handle any FCC fields.
            (goto-char (point-min))
            (if (re-search-forward "^FCC:" delimline t)
                (mail-do-fcc delimline))
            (if mail-interactive
                (save-excursion
                  (set-buffer errbuf)
                  (erase-buffer))))

          (setq esmtpmail-address-buffer (generate-new-buffer "*smtp-mail*"))
          (setq esmtpmail-recipient-address-list
                (esmtpmail-deduce-address-list tembuf (point-min) delimline))
          (kill-buffer esmtpmail-address-buffer)

          (esmtpmail-do-bcc delimline)

          ;; Send or queue
          (if (not esmtpmail-queue-mail)
              (if (not (null esmtpmail-recipient-address-list))
                  (if (not (esmtpmail-via-smtp  
                            esmtpmail-recipient-address-list tembuf))
                      (error "Sending failed ; SMTP protocol error"))
                (error "Sending failed; no recipients"))
            (let* ((file-data (concat 
                               esmtpmail-queue-dir
                               (concat (time-stamp-yyyy-mm-dd)
                                       "_" (time-stamp-hh:mm:ss))))
                      (file-data (convert-standard-filename file-data))
                      (file-elisp (concat file-data ".el"))
                   (buffer-data (create-file-buffer file-data))
                   (buffer-elisp (create-file-buffer file-elisp))
                   (buffer-scratch "*queue-mail*"))
              (save-excursion
                (set-buffer buffer-data)
                (erase-buffer)
                (insert-buffer tembuf)
                (write-file file-data)
                (set-buffer buffer-elisp)
                (erase-buffer)
                (insert (concat
                         "(setq esmtpmail-recipient-address-list '"
                         (prin1-to-string esmtpmail-recipient-address-list)
                         ")\n"))                    
                (write-file file-elisp)
                (set-buffer (generate-new-buffer buffer-scratch))
                (insert (concat file-data "\n"))
                (append-to-file (point-min) 
                                (point-max) 
                                esmtpmail-queue-index)
                )
              (kill-buffer buffer-scratch)
              (kill-buffer buffer-data)
              (kill-buffer buffer-elisp))))
      (kill-buffer tembuf)
      (if (bufferp errbuf)
          (kill-buffer errbuf)))))

(defun esmtpmail-send-queued-mail ()
  "Send mail that was queued as a result of setting `esmtpmail-queue-mail'."
  (interactive)
  ;;; Get index, get first mail, send it, get second mail, etc...
  (let ((buffer-index (find-file-noselect esmtpmail-queue-index))
        (file-msg "")
        (tembuf nil))
    (save-excursion
      (set-buffer buffer-index)
      (beginning-of-buffer)
      (while (not (eobp))
        (setq file-msg (buffer-substring (point) (save-excursion
                                                   (end-of-line)
                                                   (point))))
        (load file-msg)
        (setq tembuf (find-file-noselect file-msg))
        (if (not (null esmtpmail-recipient-address-list))
            (if (not (esmtpmail-via-smtp esmtpmail-recipient-address-list 
                                        tembuf))
                (error "Sending failed  ; SMTP protocol error"))
          (error "Sending failed; no recipients"))  
        (delete-file file-msg)
        (delete-file (concat file-msg ".el"))
        (kill-buffer tembuf)
        (kill-line 1))      
      (set-buffer buffer-index)
      (save-buffer esmtpmail-queue-index)
      (kill-buffer buffer-index)
      )))

(defun esmtpmail-fqdn ()
  (if esmtpmail-local-domain
      (concat (system-name) "." esmtpmail-local-domain)
    (system-name)))

(defvar esmtpmail-pipelining-responses 0)

(defun esmtpmail-check-for-error (process &optional no-throw)
  (if esmtpmail-pipelining
      (progn
        (setq esmtpmail-pipelining-responses
              (1+ esmtpmail-pipelining-responses))
        nil)
    (let* ((response (esmtpmail-read-response process))
           (error (or (null (car response))
                      (not (integerp (car response)))
                      (>= (car response) 400))))
      (if (and (not no-throw) error)
          (throw 'done nil)
        (list error response)))))

(defvar esmtpmail-pipelining nil)

(defun esmtpmail-via-smtp-server (&optional from)
  "determine SMTP server to which the current message will be sent."
  (interactive)
  (if (not from)
      (setq from (save-excursion
                   ;; Get the content of the From header
                   (goto-char (point-min))
                   (if (re-search-forward "^From:\\s-*\\([^ \t\n].*[^ \t\n]\\)"
                                          (point-max) t)
                       (mail-strip-quoted-names (match-string 1))))))
  ;; find matching server
  (let ((case-fold-search t)
        (by-alist esmtpmail-send-it-by-alist)
        (host (or esmtpmail-smtp-server
                  (error "`esmtpmail-smtp-server' not defined")))
        (port esmtpmail-smtp-service)
        (auth-fun 'esmtpmail-authentication-by-ehlo) (auth-data nil)
        by)
    (while by-alist
      (setq by (car by-alist))
      (if (if (stringp (car by))
              (and from (string-match (car by) from))
            (eval (car by)))
          (let* ((method (car (nth 2 by)))
                 (fun (intern (format "esmtpmail-authentication-by-%s"
                                      method))))
            (if method
                (if (functionp auth-fun)
                    (setq auth-fun fun)
                  (error "Authentication method %s not supported!" method)))
            (setq auth-data (cdar (cddr by))
                  host (nth 1 by)
                  by-alist nil)
            (if (listp host) (setq port (cadr host) host (car host)))))
      (setq by-alist (cdr by-alist)))

    (list host port auth-fun auth-data)))

(defun esmtpmail-via-smtp (recipient esmtpmail-text-buffer)
  (let ((process nil)
        (host (or esmtpmail-smtp-server
                  (error "`esmtpmail-smtp-server' not defined")))
        (auth-fun 'esmtpmail-authentication-by-ehlo) (auth-data nil)
        (esmtpmail-pipelining nil)
        (esmtpmail-pipelining-responses 0)
        (port esmtpmail-smtp-service)
        (from (save-excursion
                (set-buffer esmtpmail-text-buffer)
                ;; Get the content of the From header
                (goto-char (point-min))
                (if (re-search-forward "^From:\\s-*\\([^ \t\n].*[^ \t\n]\\)"
                                       (point-max) t)
                    (mail-strip-quoted-names (match-string 1)))))
        process-buffer)

    (let ((by (esmtpmail-via-smtp-server from)))
      (if (nth 0 by) (setq host (nth 0 by)))
      (if (nth 1 by) (setq port (nth 1 by)))
      (if (nth 2 by) (setq auth-fun (nth 2 by)))
      (if (nth 3 by) (setq auth-data (nth 3 by))))

    (if esmtpmail-debug-info
        (message "ESMTP: from <%s> to %s%s" from host
                 (if auth-data
                     (format " with auth <%S>" auth-data)
                   "")))
        
    (unwind-protect
        (catch 'done
          ;; get or create the trace buffer
          (setq process-buffer
                (get-buffer-create (format "*trace of SMTP session to %s*"
                                           host)))

          ;; clear the trace buffer of old output
          (save-excursion
            (set-buffer process-buffer)
            (erase-buffer))

          ;; open the connection to the server
          (setq process (open-network-stream "SMTP" process-buffer host port))
          (and (null process) (throw 'done nil))

          ;; set the send-filter
          (set-process-filter process 'esmtpmail-process-filter)

          (save-excursion
            (set-buffer process-buffer)
            (if (not (string-match "XEmacs" emacs-version))
                (set-buffer-process-coding-system
                 'raw-text-unix 'raw-text-unix))
            (make-local-variable 'esmtpmail-read-point)
            (setq esmtpmail-read-point (point-min))

            ;; check greeting
            (esmtpmail-check-for-error process)

            ;; Perform authentication
            (apply auth-fun process from host auth-data)
            
            ;; RCPT TO: <recipient>
            (while recipient
              (esmtpmail-send-command process (format "RCPT TO: <%s>"
                                                     (car recipient)))
              (setq recipient (cdr recipient))
              (esmtpmail-check-for-error process))
            
            ;; DATA
            (esmtpmail-send-command process "DATA")
            (esmtpmail-check-for-error process)

            ;; Handle pipelining responses
            (when esmtpmail-pipelining
              (setq esmtpmail-pipelining nil
                    esmtpmail-pipelining-responses
                    (1- esmtpmail-pipelining-responses))
              (let ((response 1)) 
                (while (<= response esmtpmail-pipelining-responses)
                  (if esmtpmail-debug-info
                      (message "ESMTP: Checking response %d/%d"
                               response
                               esmtpmail-pipelining-responses))
                  (esmtpmail-check-for-error process)
                  (setq response (1+ response)))
                (setq esmtpmail-pipelining-responses 0)))

            ;; Mail contents
            (esmtpmail-send-data process esmtpmail-text-buffer)

            ;; DATA end "."
            (esmtpmail-send-command process ".")
            (esmtpmail-check-for-error process)

            ) ;; end of save-excursion
          ;; ignore any errors 
          t)
      
      (if process
          (save-excursion
            (set-buffer (process-buffer process))
            (if (equal (process-status process) 'running)
                (esmtpmail-send-command process "QUIT"))
            (if esmtpmail-debug-info
                (message "ESMTP: Process deleted %S" process))
            (delete-process process))))))

(defun esmtpmail-process-filter (process output)
  (save-excursion
    (set-buffer (process-buffer process))
    (goto-char (point-max))
    (insert output)
    (when (string-match "^[45][0-9][0-9].*$" output)
      (warn output)
      (sit-for 1)
      (if (string-match "^535" output)
          (throw 'done 'auth-failure)))
    ))

(defun esmtpmail-read-response (process)
  (let ((case-fold-search nil)
        (response-strings nil)
        (response-continue t)
        (return-value '(nil ()))
        match-end)

    (while response-continue
      (goto-char esmtpmail-read-point)
      
      (while (not (search-forward "\r\n" nil t))
        (when (not (accept-process-output process esmtpmail-timeout))
          (warn "SMTP: timeout, server is not responding.")
          (throw 'done nil))
      (goto-char esmtpmail-read-point))

      (setq match-end (point))
      (setq response-strings
            (cons (buffer-substring esmtpmail-read-point (- match-end 2))
                  response-strings))
        
      (goto-char esmtpmail-read-point)
      (if (looking-at "[0-9]+ ")
          (let ((begin (match-beginning 0))
                (end (match-end 0)))
            (if esmtpmail-debug-info
                (message "%s" (car response-strings)))

            (setq esmtpmail-read-point match-end)

            ;; ignore lines that start with "0"
            (if (looking-at "0[0-9]+ ")
                nil
              (setq response-continue nil)
              (setq return-value
                    (cons (string-to-int 
                           (buffer-substring begin end)) 
                          (nreverse response-strings)))))
        
        (if (looking-at "[0-9]+-")
            (progn (if esmtpmail-debug-info
                     (message "%s" (car response-strings)))
                   (setq esmtpmail-read-point match-end)
                   (setq response-continue t))
          (progn
            (setq esmtpmail-read-point match-end)
            (setq response-continue nil)
            (setq return-value 
                  (cons nil (nreverse response-strings)))
            ))))
    
    (setq esmtpmail-read-point match-end)
    return-value))


(defun esmtpmail-send-command (process command)
  (goto-char (point-max))
  (if (and (> 0 (length command)) (= (aref command 0) ?P))
      (insert "PASS <omitted>\r\n")
    (insert command "\r\n"))
  (setq esmtpmail-read-point (point))
  (process-send-string process command)
  (process-send-string process "\r\n"))

;; this is much faster than the original function smtpmail-send-data whcih was
;; splitting up into two functions ...

(defun esmtpmail-send-data (process buffer)
  (save-excursion 
    (set-buffer buffer)
    (goto-char (point-min))
    
    (let ((line-start nil)
          (point-max (point-max))
          (data nil)
          (linecount 0)
          (coding (and (featurep 'file-coding) esmtpmail-code-conv-from)))
      
      (setq line-start (point))
      
      (while (not (eobp))
        (forward-line)
        (setq data (buffer-substring line-start (1- (point)))
              line-start (point)
              linecount (1+ linecount))
        (if coding
            (setq data (encode-coding-string data esmtpmail-code-conv-from)))
        (if (eq (string-to-char data) ?.)
            (process-send-string process "."))
        (process-send-string process (concat data "\r\n"))

        (when (= linecount 500)
          (setq linecount 0)
          (if (functionp 'display-message)
              (display-message 'progress
                (format "ESMTP %3.2f%% of message transfered!"
                        (/ (* 100.0 line-start) point-max)))
            (message "ESMTP %3.2f%% of message transfered!"
                     (/ (* 100.0 line-start) point-max))))))
    
      ;; finish with an empty line 
      (beginning-of-line)
      (if (not (looking-at "^$"))
          (process-send-string process "\r\n")))
  
  (if esmtpmail-debug-info
      (message "ESMTP: message transfered!")))


(defun esmtpmail-deduce-address-list (esmtpmail-text-buffer
                                      header-start header-end)
  "Get address list suitable for smtp RCPT TO: <address>."
  (require 'mail-utils)  ;; pick up mail-strip-quoted-names
    
  (unwind-protect
      (save-excursion
        (set-buffer esmtpmail-address-buffer) (erase-buffer)
        (let
            ((case-fold-search t)
             (simple-address-list "")
             this-line
             this-line-end
             addr-regexp)
          (insert-buffer-substring esmtpmail-text-buffer
                                   header-start header-end)
          (goto-char (point-min))
          ;; RESENT-* fields should stop processing of regular fields.
          (save-excursion
            (if (re-search-forward "^Resent-\\(to\\|cc\\|bcc\\):" header-end t)
                (setq addr-regexp "^Resent-\\(to\\|cc\\|bcc\\):")
              (setq addr-regexp  "^\\(To:\\|Cc:\\|Bcc:\\)")))

          (while (re-search-forward addr-regexp header-end t)
            (replace-match "")
            (setq this-line (match-beginning 0))
            (forward-line 1)
            ;; get any continuation lines
            (while (and (looking-at "^[ \t]+") (< (point) header-end))
              (forward-line 1))
            (setq this-line-end (point-marker))
            (setq simple-address-list
                  (concat simple-address-list " "
                          (mail-strip-quoted-names
                           (buffer-substring this-line this-line-end)))))
          
          (erase-buffer)
          (insert-string " ")
          (insert-string simple-address-list)
          (insert-string "\n")
          ;; newline --> blank
          (subst-char-in-region (point-min) (point-max) 10 ?  t)
          ;; comma   --> blank
          (subst-char-in-region (point-min) (point-max) ?, ?  t)
          ;; tab     --> blank
          (subst-char-in-region (point-min) (point-max)  9 ?  t)

          (goto-char (point-min))
          ;; tidyness in case hook is not robust when it looks at this
          (while (re-search-forward "[ \t]+" header-end t) (replace-match " "))

          (goto-char (point-min))
          (let (recipient-address-list)
            (while (re-search-forward " \\([^ ]+\\) " (point-max) t)
              (backward-char 1)
              (setq recipient-address-list
                    (cons (buffer-substring (match-beginning 1) (match-end 1))
                          recipient-address-list)))
            (setq esmtpmail-recipient-address-list recipient-address-list))
          ))))


(defun esmtpmail-do-bcc (header-end)
  "Delete [Resent-]BCC: and their continuation lines from the header area.
There may be multiple BCC: lines, and each may have arbitrarily
many continuation lines."
  (let ((case-fold-search t))
    (save-excursion
      (goto-char (point-min))
      ;; iterate over all BCC: lines
      (while (re-search-forward "^\\(RESENT-\\)?BCC:" header-end t)
        (delete-region (match-beginning 0)
                       (progn (forward-line 1) (point)))
        ;; get rid of any continuation lines
        (while (and (looking-at "^[ \t].*\n") (< (point) header-end))
          (replace-match ""))))))

;;;############################################################################

(defun esmtpmail-authentication-by-helo
  (process from &optional host method)
  "Perform standard HELO and MAIL FROM."
  (esmtpmail-send-command process (format "HELO %s" (esmtpmail-fqdn)))
  (esmtpmail-check-for-error process)
  (esmtpmail-send-command process (format "MAIL FROM: <%s>" from))
  (esmtpmail-check-for-error process))

(defun esmtpmail-ehlo-get-extensions (response-code)
  "Return the list of supported extensions according to response-code."
  (let ((supported-extensions '())
        (extension-lines (cdr (cdr response-code))))
    (while extension-lines
      (let* ((name (substring (car extension-lines) 4))
             (name (intern (downcase (car (split-string name  "[ =]"))))))
        (cond ((memq name '(;; list of known & supported extensions  
                            verb
                            xvrb
                            8bitmime
                            onex
                            xone
                            size 
                            xusr
                            pipelining
                            auth
                            ;; list of known but ignored extensions
                            help
                            expn
                            dsn
                            etrn
                            enhancedstatuscodes
                            deliverby
                            starttls
                            ))
               (setq supported-extensions
                     (cons name supported-extensions)))
              (esmtpmail-debug-info
               (message "Unknown ESMTP extension `%s'!" name))))
      (setq extension-lines (cdr extension-lines)))
    (if (memq 'starttls supported-extensions)
        (message "Use \"stunnel\" to utilize starttls (SMTP over SSL)!"))
    supported-extensions))

(defun esmtpmail-ehlo-handle-extensions (process from response-code)
  "Handle supported extensions and return a \"MAIL FROM\" line."
  (let ((supported-extensions (esmtpmail-ehlo-get-extensions response-code)))
    (when (member 'pipelining supported-extensions)
      (if esmtpmail-debug-info
          (message "ESMTP: pipelining enabled"))
      (setq esmtpmail-pipelining t))

    (when (or (member 'onex supported-extensions)
            (member 'xone supported-extensions))
      (esmtpmail-send-command process (format "ONEX"))
      (esmtpmail-check-for-error process))
  
    (when (and esmtpmail-debug-info
               (or (member 'verb supported-extensions)
                   (member 'xvrb supported-extensions)))
      (esmtpmail-send-command process (format "VERB"))
      (esmtpmail-check-for-error process))
  
    (when (member 'xusr supported-extensions)
      (esmtpmail-send-command process (format "XUSR"))
      (esmtpmail-check-for-error process))
  
    ;; MAIL FROM: <sender>
    (let ((size-part
           (if (member 'size supported-extensions)
               (format " SIZE=%d"
                       (save-excursion
                         (set-buffer esmtpmail-text-buffer)
                         ;; size estimate:
                         (+ (- (point-max) (point-min))
                            ;; Add one byte for each change-of-line
                            ;; because or CR-LF representation:
                            (count-lines (point-min) (point-max))
                            ;; For some reason, an empty line is
                            ;; added to the message.  Maybe this
                            ;; is a bug, but it can't hurt to add
                            ;; those two bytes anyway:
                            2)))
             ""))
          (body-part
           (if (member '8bitmime supported-extensions)
               ;; FIXME:
               ;; Code should be added here that transforms
               ;; the contents of the message buffer into
               ;; something the receiving SMTP can handle.
               ;; For a receiver that supports 8BITMIME, this
               ;; may mean converting BINARY to BASE64, or
               ;; adding Content-Transfer-Encoding and the
               ;; other MIME headers.  The code should also
               ;; return an indication of what encoding the
               ;; message buffer is now, i.e. ASCII or
               ;; 8BITMIME.
               (if nil
                   " BODY=8BITMIME"
                 "")
             "")))
      (format "MAIL FROM: <%s>%s%s" from size-part body-part))))

(defun esmtpmail-authentication-by-ehlo
  (process from host)
  "Perform authentication by EHLO."
  (let (response-code mail-from)
    (esmtpmail-send-command process (format "EHLO %s" (esmtpmail-fqdn)))
    (setq response-code (esmtpmail-check-for-error process t))
    
    (if (car response-code)
        ;; switch back to HELO
        (esmtpmail-authentication-by-helo process from)
      ;; o.k. it was EHLO
      (setq response-code (car (cdr response-code))
            mail-from (esmtpmail-ehlo-handle-extensions
                       process from response-code))
      (esmtpmail-send-command process mail-from)
      (esmtpmail-check-for-error process))))

(defun esmtpmail-store-login-and-password (host-login login password)
  (add-to-list 'esmtpmail-smtp-login-and-password
               (list host-login login password)))

(defun esmtpmail-get-login-and-password (host login password encoder)
  (let (cache elogin)
    (cond ((and host login
                (listp esmtpmail-smtp-login-and-password)
                (setq cache (assoc (list host login)
                                   esmtpmail-smtp-login-and-password)))
           (cdr cache))
          (t
           (setq password (or password
                              (read-passwd (format "Password for %s@%s: "
                                                   login host))))
           (if encoder 
               (setq elogin (apply encoder (list login))
                     password (apply encoder (list password))))
           (esmtpmail-store-login-and-password (list host login)
                                              elogin password)
           (list elogin password)))))

(defun esmtpmail-forget-login-and-password (host login)
  (setq esmtpmail-smtp-login-and-password
        (delete (assoc (list host login) esmtpmail-smtp-login-and-password)
                esmtpmail-smtp-login-and-password)))

(defun esmtpmail-authentication-by-login
  (process from host login &optional password)
  "Perform authentication by SMTP login."
  (let ((login-and-pw (esmtpmail-get-login-and-password host login password
                                                       'base64-encode-string))
        elogin response-code mail-from)
    (setq elogin (car login-and-pw)
          password (cadr login-and-pw))
    (esmtpmail-send-command process (format "EHLO %s" (esmtpmail-fqdn)))
    (setq response-code (esmtpmail-check-for-error process t)
          mail-from (esmtpmail-ehlo-handle-extensions
                     process from response-code))
    (esmtpmail-send-command process "AUTH LOGIN")
    (esmtpmail-check-for-error process)
    (let ((code (catch 'done
                  (esmtpmail-send-command process elogin)
                  (esmtpmail-check-for-error process)
                  (esmtpmail-send-command process password)
                  (esmtpmail-check-for-error process)
                  t)))
      (when (eq code 'auth-failure)
        (esmtpmail-forget-login-and-password host login)
        (warn "Wrong password for %s@%s has been removed!" login host)
        (sit-for 5)
        (throw 'done 'auth-failure)))
    (esmtpmail-send-command process (concat mail-from " AUTH=<>"))))

(defun esmtpmail-authentication-by-vm-pop-login
  (process from host spoolname)
  "Perform authentication by SMTP login using VM pop account info."
  (require 'vm-misc)
  (require 'vm-pop)
  (let* ((source-list (vm-parse spoolname "\\([^:]+\\):?"))
         (source-nopwd (vm-popdrop-sans-password spoolname))
         (login (nth 3 source-list))
         (password (if (assoc source-nopwd vm-pop-passwords)
                       (cadr (assoc source-nopwd vm-pop-passwords))
                     (if (not (string= "*" (nth 4 source-list)))
                         (nth 4 source-list)))))
    
    (if (null password)
        (setq password (vm-read-password (format "POP password for %s: "
                                                 (nth 0 source-list)))
              vm-pop-passwords (cons (list source-nopwd password)
                                     vm-pop-passwords)))
    
    (if (eq 'auth-failure
            (catch 'done
              (esmtpmail-authentication-by-login
               process from host login password)
              t))
      (setq vm-pop-passwords
            (delq (assoc source-nopwd vm-pop-passwords) vm-pop-passwords)))))

(defun esmtpmail-authentication-by-vm-after-pop
  (process from host spoolname)
  "Perform authentication by checking a POP account of a VM spoolfile."
  (require 'vm-misc)
  (require 'vm-pop)
  (let ((vm-pop-ok-to-ask t))
    (vm-pop-check-mail spoolname))
  (esmtpmail-authentication-by-ehlo process from host))

(defun esmtpmail-authentication-by-vm-after-pop-helo
  (process from cond host method spoolname)
  "Perform authentication by checking a POP account of a VM spoolfile."
  (require 'vm-misc)
  (require 'vm-pop)
  (let ((vm-pop-ok-to-ask t))
    (vm-pop-check-mail spoolname))
    (esmtpmail-authentication-by-helo process from cond host))


(defun feedmail-buffer-to-esmtpmail (prepped errors-to addr-listoid)
  "Function which actually calls smtpmail-via-smtp to send buffer as e-mail."
  ;; I'm not sure smtpmail.el is careful about the following
  ;; return value, but it also uses it internally, so I will fear
  ;; no evil.
  (feedmail-say-debug ">in-> feedmail-buffer-to-esmtpmail %s" addr-listoid)

  (when (not (esmtpmail-via-smtp addr-listoid prepped))
    (set-buffer errors-to)
    (insert "Send via smtpmail failed.  Probable SMTP protocol error.\n")
    (insert "Look for details below or in the *Messages* buffer.\n\n")
    (let ((case-fold-search t)
          ;; don't be overconfident about the name of the trace buffer
          (tracer (concat "trace.*smtp.*"
                          (regexp-quote esmtpmail-smtp-server))))
      (mapcar
       '(lambda (buffy)
          (if (string-match tracer (buffer-name buffy))
              (progn
                (insert "SMTP Trace from " (buffer-name buffy)
                        "\n---------------")
                (insert-buffer buffy)
                (insert "\n\n"))))
       (buffer-list)))))


(provide 'esmtpmail)

;;; esmtpmail.el ends here
