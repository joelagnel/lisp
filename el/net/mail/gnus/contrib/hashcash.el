;;; hashcash.el --- Add hashcash payments to email

;; Copyright (C) 2002, 2003, 2005 Free Software Foundation
;; Copyright (C) 1997--2002 Paul E. Foley

;; Maintainer: Paul Foley <mycroft@actrix.gen.nz>
;; Keywords: mail, hashcash

;; Released under the GNU General Public License
;;   (http://www.gnu.org/licenses/gpl.html)

;;; Commentary:

;; The hashcash binary is at http://www.cypherspace.org/hashcash/
;;
;; Call mail-add-payment to add a hashcash payment to a mail message
;; in the current buffer.
;;
;; To automatically add payments to all outgoing mail:
;;    (add-hook 'message-send-hook 'mail-add-payment)

;;; Code:

(eval-and-compile
 (autoload 'executable-find "executable"))

(defgroup hashcash nil
  "Hashcash configuration."
  :group 'mail)

(defcustom hashcash-default-payment 0
  "*The default number of bits to pay to unknown users.
If this is zero, no payment header will be generated.
See `hashcash-payment-alist'."
  :type 'integer
  :group 'hashcash)

(defcustom hashcash-payment-alist '()
  "*An association list mapping email addresses to payment amounts.
Elements may consist of (ADDR AMOUNT) or (ADDR STRING AMOUNT), where
ADDR is the email address of the intended recipient and AMOUNT is
the value of hashcash payment to be made to that user.  STRING, if
present, is the string to be hashed; if not present ADDR will be used."
  :group 'hashcash)

(defcustom hashcash-default-accept-payment 10
  "*The default minimum number of bits to accept on incoming payments."
  :type 'integer
  :group 'hashcash)

(defcustom hashcash-accept-resources `((,user-mail-address nil))
  "*An association list mapping hashcash resources to payment amounts.
Resources named here are to be accepted in incoming payments.  If the
corresponding AMOUNT is NIL, the value of `hashcash-default-accept-payment'
is used instead."
  :group 'hashcash)

(defcustom hashcash-path (executable-find "hashcash")
  "*The path to the hashcash binary."
  :group 'hashcash)

(defcustom hashcash-double-spend-database "hashcash.db"
  "*The path to the double-spending database."
  :group 'hashcash)

(defcustom hashcash-in-news nil
  "*Specifies whether or not hashcash payments should be made to newsgroups."
  :type 'boolean
  :group 'hashcash)

(require 'mail-utils)

(eval-and-compile
 (if (fboundp 'point-at-bol)
     (defalias 'hashcash-point-at-bol 'point-at-bol)
   (defalias 'hashcash-point-at-bol 'line-beginning-position))

 (if (fboundp 'point-at-eol)
     (defalias 'hashcash-point-at-eol 'point-at-eol)
   (defalias 'hashcash-point-at-eol 'line-end-position)))

(defun hashcash-strip-quoted-names (addr)
  (setq addr (mail-strip-quoted-names addr))
  (if (and addr (string-match "\\`\\([^+@]+\\)\\+[^@]*\\(@.+\\)" addr))
      (concat (match-string 1 addr) (match-string 2 addr))
    addr))

(defun hashcash-payment-required (addr)
  "Return the hashcash payment value required for the given address."
  (let ((val (assoc addr hashcash-payment-alist)))
    (or (nth 2 val) (nth 1 val) hashcash-default-payment)))

(defun hashcash-payment-to (addr)
  "Return the string with which hashcash payments should collide."
  (let ((val (assoc addr hashcash-payment-alist)))
    (or (nth 1 val) (nth 0 val) addr)))

(defun hashcash-generate-payment (str val)
  "Generate a hashcash payment by finding a VAL-bit collison on STR."
  (if (> val 0)
      (save-excursion
	(set-buffer (get-buffer-create " *hashcash*"))
	(erase-buffer)
	(call-process hashcash-path nil t nil
		      "-m" "-q" "-b" (number-to-string val) str)
	(goto-char (point-min))
	(buffer-substring (hashcash-point-at-bol) (hashcash-point-at-eol)))
    nil))

(defun hashcash-check-payment (token str val)
  "Check the validity of a hashcash payment."
  (zerop (call-process hashcash-path nil nil nil "-c"
		       "-d" "-f" hashcash-double-spend-database
		       "-b" (number-to-string val)
		       "-r" str
		       token)))

(defun hashcash-version (token)
  "Find the format version of a hashcash token."
  ;; Version 1.2 looks like n:yymmdd:rrrrr:xxxxxxxxxxxxxxxx
  ;;   This carries its own version number embedded in the token,
  ;;   so no further format number changes should be necessary
  ;;   in the X-Payment header.
  ;;
  ;; Version 1.1 looks like yymmdd:rrrrr:xxxxxxxxxxxxxxxx
  ;;   You need to upgrade your hashcash binary.
  ;;
  ;; Version 1.0 looked like nnnnnrrrrrxxxxxxxxxxxxxxxx
  ;;   This is no longer supported.
  (cond ((equal (aref token 1) ?:) 1.2)
	((equal (aref token 6) ?:) 1.1)
	(t (error "Unknown hashcash format version"))))

;;;###autoload
(defun hashcash-insert-payment (arg)
  "Insert X-Payment and X-Hashcash headers with a payment for ARG"
  (interactive "sPay to: ")
  (let ((pay (hashcash-generate-payment (hashcash-payment-to arg)
					(hashcash-payment-required arg))))
    (when pay
;      (insert-before-markers "X-Payment: hashcash "
;			     (number-to-string (hashcash-version pay)) " "
;			     pay "\n")
      (insert-before-markers "X-Hashcash: " pay "\n"))))

;;;###autoload
(defun hashcash-verify-payment (token &optional resource amount)
  "Verify a hashcash payment"
  (let ((key (if (< (hashcash-version token) 1.2)
		 (nth 1 (split-string token ":"))
		 (nth 2 (split-string token ":")))))
    (cond ((null resource)
	   (let ((elt (assoc key hashcash-accept-resources)))
	     (and elt (hashcash-check-payment token (car elt)
			(or (cadr elt) hashcash-default-accept-payment)))))
	  ((equal token key)
	   (hashcash-check-payment token resource
				(or amount hashcash-default-accept-payment)))
	  (t nil))))

;;;###autoload
(defun mail-add-payment (&optional arg)
  "Add X-Payment: and X-Hashcash: headers with a hashcash payment
for each recipient address.  Prefix arg sets default payment temporarily."
  (interactive "P")
  (let ((hashcash-default-payment (if arg (prefix-numeric-value arg)
				    hashcash-default-payment))
	(addrlist nil))
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(search-forward mail-header-separator)
	(beginning-of-line)
	(narrow-to-region (point-min) (point))
	(let ((to (hashcash-strip-quoted-names (mail-fetch-field "To" nil t)))
	      (cc (hashcash-strip-quoted-names (mail-fetch-field "Cc" nil t)))
	      (ng (hashcash-strip-quoted-names (mail-fetch-field "Newsgroups"
								 nil t))))
	  (when to
	    (setq addrlist (split-string to ",[ \t\n]*")))
	  (when cc
	    (setq addrlist (nconc addrlist (split-string cc ",[ \t\n]*"))))
	  (when (and hashcash-in-news ng)
	    (setq addrlist (nconc addrlist (split-string ng ",[ \t\n]*")))))
	(when addrlist
	  (mapcar #'hashcash-insert-payment addrlist))))) ; mapc
  t)

;;;###autoload
(defun mail-check-payment (&optional arg)
  "Look for a valid X-Payment: or X-Hashcash: header.
Prefix arg sets default accept amount temporarily."
  (interactive "P")
  (let ((hashcash-default-accept-payment (if arg (prefix-numeric-value arg)
					   hashcash-default-accept-payment))
	(version (hashcash-version (hashcash-generate-payment "x" 1))))
    (save-excursion
      (goto-char (point-min))
      (search-forward "\n\n")
      (beginning-of-line)
      (let ((end (point))
	    (ok nil))
	(goto-char (point-min))
	(while (and (not ok) (search-forward "X-Payment: hashcash " end t))
	  (let ((value (split-string
			  (buffer-substring (point) (hashcash-point-at-eol))
			  " ")))
	    (when (equal (car value) (number-to-string version))
	      (setq ok (hashcash-verify-payment (cadr value))))))
	(goto-char (point-min))
	(while (and (not ok) (search-forward "X-Hashcash: " end t))
	  (setq ok (hashcash-verify-payment
		    (buffer-substring (point) (hashcash-point-at-eol)))))
	(when ok
	  (message "Payment valid"))
	ok))))

(provide 'hashcash)

;;; arch-tag: 0e7fe983-a124-4392-9788-0dbcbd2c4d62
