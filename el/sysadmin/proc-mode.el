;; proc-mode.el -- Copyright (C) 1999 Pat Thoyts <pat@zsplat.freeserve.co.uk>
;;
;; Major mode for working with Proc files in EMACS.
;;
;; Author: Pat Thoyts <pat@zsplat.freeserve.co.uk>
;; Version: 1.10   (27 Aug 1999)
;; Maintainer: Pat Thoyts <pat@zsplat.freeserve.co.uk>
;; Keywords: languages
;;
;; ----------------------------------------------------------------------
;;
;; Copyright (C) 1999 Pat Thoyts <pat@zsplat.freeserve.co.uk>
;;
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This program  is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; see the file `Copying'.  If not, write to the Free 
;; Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; ----------------------------------------------------------------------
;;
;;; COMMENTARY:
;;  -----------
;;  This is an emacs major mode to provide some help to programmers who
;;  must edit Pick-style Proc language programs. It provides keyword
;;  colourisation and not much else.
;;
;;  Tested for XEmacs 19.15 and GNU Emacs 20.3
;;
;;
;;; INSTALLATION
;;  ------------
;;  Put 
;;
;;    (autoload 'proc-mode "proc-mode" "Mode for Pick PROC procedures." t)
;;
;;  into your .emacs file and type M-x proc-mode in any buffer you think
;;  is suitable.
;;
;;  You can also add
;;
;;    (autoload 'proc-insert-emacs-tag "proc-mode" nil t)
;;
;;  if you want to be able to easilly tag a buffer as proc mode for emacs.
;;  I recommend binding these functions to keys, possibly
;;
;;   C-c p   to proc-mode
;;   C-c C-p to proc-insert-emacs-tag
;;
;;  This can be done with something like:
;;
;;    (defun Ctl-C-prefix () Ctl-C-keymap)
;;    (defvar Ctl-C-keymap (make-keymap) "Keymap for C-c prefix.")
;;    (global-set-key "\C-c" (Ctl-C-prefix))
;;    (define-key Ctl-C-keymap "p"    'proc-mode)
;;    (define-key Ctl-C-keymap "\C-p" 'proc-insert-emacs-tag)
;;
;;
;;; SEE ALSO
;;  --------
;;  unibasic-mode - a major mode for editing Unibasic/Databasic buffers.
;;
;;
;;; BUGS
;;  ----
;;  Only that PROCs are so hideous.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst proc-rcs-version
  "@(#)$Id: proc-mode.el,v 1.10 1999/08/27 13:07:52 pat Exp $"
  "RCS version info for `proc-mode'.")

(defconst proc-mode-version
  (if (string-match "\\<[0-9]+\\.[0-9]+\\>" proc-rcs-version)
      (substring proc-rcs-version (match-beginning 0) (match-end 0))
    "0.0")
  "The current version of `proc-mode' in use.")

(defconst proc-mode-help-address "unibasic@zsplat.freeserve.co.uk"
  "Address accepting submission of unibasic-mode and proc-mode bug reports.")

;; We need to compile our lists of words into regular expressions.
;; In order to do this we need either regexp-opt which is provided with
;; recent emacsen, or we must provide make-regexp.el. Once the library
;; has been byte compiled it will be a lot quicker to load as we won't
;; need to run this for the compiled code.
(eval-when-compile
  (if (condition-case () (require 'regexp-opt) (error nil))
      (defun make-regexp (strings &optional paren lax) nil
        (regexp-opt strings paren))
    (if (not (condition-case () (fboundp 'make-regexp) (error nil)))
        (if (not (load-library "make-regexp"))
            (error "Failed to load make-regexp.el")))))

(cond   ;;; handle versions of emacs without custom.
 ((fboundp 'defgroup)
  (defgroup proc nil
    "Major mode for editing proc source in Emacs."
    :prefix "proc-"
    :group 'languages)
  (defcustom proc-mode-hook ()
    "*User hook for `proc-mode' called after the mode starts up."
    :type 'hook
    :group 'proc))
 (t
  (defvar proc-mode-hook ()
    "*User hook for `proc-mode' called after the mode starts up.")
  ))

(defvar proc-mode-map ()
  "Keymap used in `proc-mode' buffers.")
(if proc-mode-map
    ()
  (setq proc-mode-map (make-sparse-keymap))
  (define-key proc-mode-map "\t"        'proc-electric-tab)
  (define-key proc-mode-map "\M-q"      'indent-region)
  (define-key proc-mode-map "\C-c\C-p"  'proc-insert-emacs-tag)
  (define-key proc-mode-map "\C-c\C-b"  'proc-submit-bug-report))

(defconst proc-mode-key-words
  '( "PQ" "SP" "SS" "STON" "STOFF"
     "BO" "RO" "IT" "PX" "PH" "PP" "PW"
     "G" "GO" "IF")
  "List of keywords used in Pick proc language procedures.")

(defvar proc-mode-key-regexp
  (eval-when-compile
    (concat "\\b\\(" (make-regexp proc-mode-key-words) "\\)\\b")))

(defconst proc-mode-operator-words
  '( "AND" "OR" "EQ" "GT" "GE" "LT" "LE" "NE" )
  "List of operator works used in Pick proc language procedures.")

(defvar proc-mode-operator-regexp
  (eval-when-compile
    (concat "\\b\\(" (make-regexp proc-mode-operator-words) "\\)\\b")))


(defvar proc-mode-font-lock-keywords
  (list
   (cons "^[0-9 \t]*\\(C.*\\)"     '(1 font-lock-comment-face))
   (cons "^[0-9 \t]*\\(T.*\\)"     '(1 font-lock-reference-face))
   (cons "^[0-9 \t]*\\(O.*\\)"     '(1 font-lock-type-face))
   (cons proc-mode-key-regexp      'font-lock-keyword-face)
   (cons "^[0-9 \t]*\\(S[0-9]+\\)" '(1 font-lock-variable-name-face))
   (cons "A.?[0-9]+,?[0-9]?"       'font-lock-variable-name-face)
   (cons "^[0-9 \t]*\\(D[0-9]?,?[0-9]?\\+?\\)" 
         '(1 font-lock-variable-name-face))
   (cons "^[0-9 \t]*\\(\\(RI\\|IH\\)[0-9]*\\)" '(1 font-lock-keyword-face))
   (cons "^[0-9 \t]*\\(\\(IS\\|IP\\).?\\)"     '(1 font-lock-keyword-face))
   (cons "^[0-9 \t]*\\([BDHPXASTFG]\\)"   '(1 font-lock-function-name-face))
   (cons proc-mode-operator-regexp        'font-lock-keyword-face))
  "Proc mode regular expressions for font-lock-mode.")

;;;###autoload
(defun proc-mode ()
  "Major mode for display and editing of Pick proc files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map proc-mode-map)
  (setq major-mode 'proc-mode
	mode-name "proc")
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'proc-indent-line)
  (setq indent-tabs-mode nil                   ;; make all tabs spaces
        indent-line-function 'proc-indent-line ;; for indent region
	comment-start "C"                      ;; string used in comment-region
	comment-end   "" )
  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(proc-mode-font-lock-keywords))
  ;; Allow users to add functions to be run for this mode.
  ;;;(unibasic-find-magic-labels)
  (run-hooks 'proc-mode-hook))

;;;###autoload
(defun proc-insert-emacs-tag ()
  "Insert emacs notification at the end of the buffer to set the file to
proc mode whenever it is loaded.
Bound to \\[proc-insert-emacs-tag]"
  (interactive)
  (goto-char (point-max))
  (if (not (save-excursion
             (re-search-backward "End:[ \t]*$" 
                                 (save-excursion 
                                   (beginning-of-line -5) (point))
                                 t)))
      (insert "\nC\nC Local variables:\nC mode: proc\nC End:\n")))

(defsubst proc-within-string ()
  (save-excursion 
    (nth 3 
         (parse-partial-sexp
          (save-excursion (beginning-of-line) (point))
          (point))) ))

(defun proc-electric-tab (&optional count)
  "Function called in `proc-mode' when a TAB is entered."
  (interactive "p")
  ;; Don't do anything special if in a string.
  (if (proc-within-string)
      (insert-char (string-to-char "\t") count)
    (save-excursion
      (proc-indent-line))))

(defun proc-indent-line ()
  "Indent lines of code for `proc-mode'. They always go to column 0."
  (interactive)
  (beginning-of-line)
  (delete-horizontal-space))
  
(defun proc-submit-bug-report ()
  "Submit bug report via mail for proc-mode to the address specified in
`proc-mode-help-address'. Bound to \\[proc-submit-bug-report]."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you want to submit a report about proc-mode? ")
   (reporter-submit-bug-report
    proc-mode-help-address
    (concat "proc-mode " proc-rcs-version)
    (list
     ;; report only the vars that affect indentation
     ))))

(provide 'proc-mode)

;; Local variables:
;;   mode: emacs-lisp
;;   indent-tabs-mode: nil
;; End:

