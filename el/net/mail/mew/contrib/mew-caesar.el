;; -*- emacs-lisp -*-
;;
;; mew-caesar.el --- Caesar encrypt/decrypt assistant package for Mew.
;;
;;   Author: Hideyuki SHIRAI <shirai@mew.org>
;;   Created: <02/07/1998>
;;
;; To use mew-caesar.el, install (tm|SEMI) package or "nkf"
;;  , and put the following codes in your .emacs.
;;
;; (add-hook 'mew-init-hook
;;  	  (lambda ()
;;  	    (require 'mew-caesar)))
;;

(eval-when-compile
  (require 'mew))

(defconst mew-caesar-version "mew-caesar.el 0.21")

(defvar mew-caesar-ext-prog
  (let (extprog)
    (cond
     ((or (memq system-type '(OS/2 emx))
	  (eq system-type 'windows-nt))
      (cond
       ((setq extprog (mew-which "nkf.exe" exec-path))
	extprog)
       ((setq extprog (mew-which "nkf32.exe" exec-path))
	extprog)
       (t nil)))
     (t (setq extprog (mew-which "nkf" exec-path))
	extprog)))
  "mew-caesar external program.
 Usually, auto searched \"nkf\", \"nkf.exe\" or \"nkf32.exe\"."
  )

(defvar mew-caesar-ext-prog-arg '("-r"))

(defvar mew-caesar-function
  (cond
   ((or (featurep 'mule-caesar)
	(locate-library "mule-caesar"))
    (require 'mule-caesar)
    'semi)
   ((or (featurep 'tm-def)
	(locate-library "tm-def"))
    (require 'tm-def)
    'tm)
   (mew-caesar-ext-prog
    'ext)
   (t
    (message "mew-caesar: program is not found")
    nil))
  "mew-caesar function select.
 Usually auto selected, which
 'semi(mule-caesar), 'tm(tm:caesar-region) or 'ext(mew-caesar-ext-prog)."
  )

(defvar mew-caesar-prog-xrot '(mew-caesar-mime-text/x-rot () nil))
(defconst mew-caesar-ct-rot13 "Text/X-Rot13-47-48")
(defconst mew-caesar-rot13-suffix ".rot")

(define-key mew-summary-mode-map "\C-cr" 'mew-caesar-summary-insert-xrot)
(define-key mew-draft-attach-map "R" 'mew-caesar-attach-find-new-xrot)

(setq mew-mime-content-type
      (append
       '(("text/x-rot13-47-48" "\\.rot$" nil mew-caesar-prog-xrot mew-icon-text)
	 ("text/x-rot13.*" "\\.rot$" nil mew-caesar-prog-xrot mew-icon-text))
       mew-mime-content-type))

(defun mew-caesar-mime-text/x-rot (cache beg end &optional params execute)
  (if (> end beg)
      (save-excursion
	(set-buffer (mew-buffer-message))
	(let ((buffer-read-only nil))
	  (insert " #     #         ######  ####### #######    #     #####\n"
		  "  #   #          #     # #     #    #      ##    #     #\n"
		  "   # #           #     # #     #    #     # #          #\n"
		  "    #     #####  ######  #     #    #       #     #####\n"
		  "   # #           #   #   #     #    #       #          #\n"
		  "  #   #          #    #  #     #    #       #    #     #\n"
		  " #     #         #     # #######    #     #####   #####\n"
		  "\n")
	  (insert "To save this part, type "
		  (substitute-command-keys
		   "\\<mew-summary-mode-map>\\[mew-summary-save].")
		  "\nTo display this part in Message mode, type "
		  (substitute-command-keys
		   "\\<mew-summary-mode-map>\\[mew-caesar-summary-insert-xrot]."))
	  (insert "\n\n-------------------- Original \"X-ROT13\" follows --------------------\n")
	  (insert-buffer-substring cache beg end)
	  ))))

(defun mew-caesar-summary-insert-xrot ()
  (interactive)
  (mew-summary-part
   (let* ((fld (mew-current-get-fld (mew-frame-id)))
	  (msg (mew-current-get-msg (mew-frame-id)))
	  (part (mew-syntax-nums))
	  (cache (mew-cache-hit fld msg 'must-hit))
	  (syntax (mew-cache-decode-syntax cache))
	  (stx (mew-syntax-get-entry syntax part))
	  (beg (mew-syntax-get-begin stx))
	  (end (mew-syntax-get-end stx))
	  (win (selected-window)))
     (unwind-protect
	 (progn
	   (mew-summary-toggle-disp-msg 'on)
	   (mew-window-configure 'message)
	   (set-buffer (mew-buffer-message))
	   (mew-elet
	    (mew-summary-display-preamble)
	    (insert-buffer-substring cache beg end)
	    (mew-caesar-whole-buffer)
	    ;; (goto-char (point-min))
	    (mew-summary-display-postscript)))
       (select-window win)))))

(defun mew-caesar-attach-find-new-xrot ()
  "Open a new Caesar encrypt file into a buffer on \".\" in attachments."
  (interactive)
  (if (not (mew-attach-not-line012-1))
      (message "Cannot find a new file here")
    (let* ((nums (mew-syntax-nums))
	   (subdir (mew-attach-expand-path mew-encode-syntax nums))
	   (attachdir (mew-attachdir))
	   file filepath)
      ;; attachdir / {subdir/} dir
      (if (not (equal subdir ""))
	  (setq attachdir (expand-file-name subdir attachdir)))
      ;; attachdir / file
      (setq filepath (mew-random-filename attachdir 1 nil mew-caesar-rot13-suffix))
      (if (null filepath)
	  (message "Could not make a text file, sorry")
	(setq file (file-name-nondirectory filepath))
	(setq mew-encode-syntax
	      (mew-syntax-insert-entry
	       mew-encode-syntax
	       nums
	       (mew-encode-syntax-single file (list mew-caesar-ct-rot13))))
	(mew-encode-syntax-print mew-encode-syntax)
	;;
	(find-file filepath)
	;; buffer switched
	(setq mode-name "X-Rot13")
	(setq mode-line-buffer-identification mew-mode-line-id)
	(local-set-key "\C-c\C-q" 'mew-kill-buffer)
	(local-set-key "\C-cr" 'mew-caesar-whole-buffer)
	(local-set-key "\C-c\C-s" 'mew-caesar-save-exit)
	(insert " #     #         ######  ####### #######    #     #####\n"
		"  #   #          #     # #     #    #      ##    #     #\n"
		"   # #           #     # #     #    #     # #          #\n"
		"    #     #####  ######  #     #    #       #     #####\n"
		"   # #           #   #   #     #    #       #          #\n"
		"  #   #          #    #  #     #    #       #    #     #\n"
		" #     #         #     # #######    #     #####   #####\n")
	(insert "\n define-key \"\\C-cr\"    -> mew-caesar-whole-buffer.")
	(insert "\n define-key \"\\C-c\\C-s\" -> mew-caesar-save-exit.")
	(insert "\n\n Press any key to start editting.")
	(read-char-exclusive)
	(delete-region (point-min) (point-max))
	(run-hooks 'mew-caesar-xrot-mode-hook)
	))))

(defun mew-caesar-save-exit ()
  "Caesar encrypt/decrypt at whole buffer, save and exit."
  (interactive)
  (mew-caesar-whole-buffer)
  (if (y-or-n-p (format "Save & Exit ?"))
      (progn
	(save-buffer)
	(kill-buffer (current-buffer)))
    (mew-caesar-whole-buffer)))

(defun mew-caesar-whole-buffer ()
  "Caesar encrypt/decrypt at whole buffer."
  (interactive)
  (mew-caesar-region (point-min) (point-max)))

(defun mew-caesar-region (min max)
  "Caesar encrypt/decrypt in region."
  (interactive "r")
  (save-excursion
    (cond
     ((eq mew-caesar-function 'semi)
      (mule-caesar-region min max))
     ((eq mew-caesar-function 'tm)
      (progn
	(goto-char min)
	(push-mark (point) nil t)
	(goto-char max)
	(tm:caesar-region)))
     ((and (eq mew-caesar-function 'ext)
	   mew-caesar-ext-prog mew-caesar-ext-prog-arg)
      (save-excursion
	(mew-piolet
	 mew-cs-autoconv mew-cs-m17n
	 (apply (function call-process-region)
		min max
		mew-caesar-ext-prog 
		t t nil
		mew-caesar-ext-prog-arg))))
     (t
      (message "mew-caesar: program is not found")))
    ))

(provide 'mew-caesar)

;;; Copyright Notice:

;; Copyright (C) 1998-2000 Hideyuki SHIRAI <shirai@mew.org>
;; Copyright (C) 1994-2000 Mew developing team.
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

;;; mew-caesar.el ends here
