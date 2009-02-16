;;; -*- emacs-lisp -*-
;;; scrlttr2.el -- AUC TeX style for scrlttr2.cls

;; Copyright (C) 2002 Mark Trettin
;; License: GPL, see the file COPYING in the base directory of AUC TeX

;; Author: Mark Trettin <Mark.Trettin@gmx.de>
;; Created: 2002-10-26
;; Version: $Id: scrlttr2.el,v 1.1 2002/10/26 20:29:31 dakas Exp $
;; Keywords: tex

;;; Commentary: 

;; This file adds support for `scrlttr2.cls'.

;; Since I just recently switched from `g-brief.cls' to the
;; KOMA-Script letter class *and* I don't really write many
;; snailmails, there are probably some superflous macros included and
;; important ones left out. Comments appreciated.

;; I left out any length and positioning macros since those should be
;; set in a personal `*.lco'-File. IMHO.

;; This file is intended to be used with the AUC TeX-Package by Per
;; Abrahamsen. Put this File into your TeX-style-path. You may also
;; byte-compile this file.

;;; Code
(TeX-add-style-hook "scrlttr2"
  (lambda ()
    (TeX-add-symbols
     ;; letter commands
     '("opening" "Opening")
     '("cc" t)
     '("encl" t)
     '("ps")
     '("closing" "Closing Phrase")
     ;; Layout stuff
     '("firsthead" t)
     '("firstfoot" t)
     '("nexthead" t)
     '("nextfoot" t)
     ;; Inserting Options
     '("KOMAoptions" t)
     ;; KOMA-vars
     '("newkomavar" [ "Description" ] "Name")
     '("newkomavar*" [ "Description" ] "Name")
     '("setkomavar" TeX-arg-KOMA-scrlttr-vars [ "Description" ] t)
     '("setkomavar*" TeX-arg-KOMA-scrlttr-vars "Description")
     '("usekomavar" [ "Command" ] TeX-arg-KOMA-scrlttr-vars)
     '("usekomavar*" [ "Command" ] TeX-arg-KOMA-scrlttr-vars)
     '("addtoreffields" TeX-arg-KOMA-scrlttr-vars)
     ;; Checking for existing variables
     '("ifkomavarempty" TeX-arg-KOMA-scrlttr-vars 2)
     '("ifkomavarempty*" TeX-arg-KOMA-scrlttr-vars 2)
     ;; Fonts
     '("addtokomafont" TeX-arg-KOMA-scrlttr-fontelements t)
     '("setkomafont" TeX-arg-KOMA-scrlttr-fontelements t)
     '("usekomafont" TeX-arg-KOMA-scrlttr-fontelements)
     ;; Additional clearpage commands
     '("cleardoublestandardpage")
     '("cleardoubleplainpage")
     '("cleardoubleemptypage"))
    (LaTeX-add-environments
     '("letter" (lambda (env &rest ignore)
		  (LaTeX-insert-environment
		   env
		   (let ((options (read-string "Optional options: "))
			 (recip (read-string "Recipient: ")))
		     (concat
		      (if (not (zerop (length options)))
			  (format "[%s]" options))
		      (format "{%s}" recip)))))))))

(defun TeX-arg-KOMA-scrlttr-vars (optional &optional prompt)
  "Prompt for KOMA-Script's scrlttr2 predefined variables with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Variable")
    '(("")
      ("backaddress") ("backaddressseparator")
      ("ccseparator") ("customer")
      ("date")
      ("emailseparator") ("enclseparator")
      ("faxseparator") ("frombank") ("fromaddress") ("fromemail")
      ("fromfax") ("fromlogo") ("fromname") ("fromphone") ("fromurl")
      ("invoice")
      ("location")
      ("myref")
      ("place") ("placeseparator") ("phoneseparator")
      ("signature") ("specialmail") ("subject") ("subjectseparator")
      ("title") ("toname") ("toaddress")
      ("yourmail") ("yourref"))
    nil nil)
   optional))

(defun TeX-arg-KOMA-scrlttr-fontelements (optional &optional prompt)
  "Prompt for KOMA-Script's scrlttr2 fontelements with completion."
  (TeX-argument-insert
   (completing-read
    (TeX-argument-prompt optional prompt "Element")
    '(("")
      ("backaddress")
      ("descriptionlabel")
      ("fromaddress") ("fromname")
      ("pagefoot") ("pagehead") ("pagenumber")
      ("subject")
      ("title"))
    nil t)
   optional))
;;; scrlttr2.el ends here
