;;; html-font.el --- Adds font-lock support to HTML helper mode.
;; Copyright (C) 1995 Ulrik Dickow.

;; Author: Ulrik Dickow <dickow@nbi.dk> (http://www.nbi.dk/~dickow)
;; Created: 18-Jan-1995
;; Maintainer: Ulrik Dickow <dickow@nbi.dk> (http://www.nbi.dk/~dickow)
;; Version: @(#) html-font.el version 4.01 19-Sep-1995 U.Dickow
;; Keywords: languages HTML font-lock faces
;; See also: fort-font.el kumac-font.el html-helper-mode.el html-mode.el
;; See also URL: http://www.nbi.dk/~dickow/emacs/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; LCD Archive Entry:
;; html-font|Ulrik Dickow|dickow@nbi.dk|
;; Adds Font Lock support to HTML helper mode.|
;; 19-Sep-1995|4.01|~/misc/html-font.el.Z|

;;; Commentary:

;; Sets up the current HTML helper mode for full support of font-lock-mode.
;; Required: html-helper-mode.el (LCD or http://www.santafe.edu/~nelson/tools/)
;; However, this file can easily be adapted to e.g. M.Andreessen's html-mode.
;; Known to work with Emacs 19.27-29 and XEmacs 19.12-13.
;; Probably works with XEmacs 19.11 too.

;; The code in this file was included in beta release 2.15 of html-helper-mode.
;; When it exits the beta stage and replaces the 1994 version,
;; this file will presumably become obsolete.

;; Installation, currently:
;;   1) Put html-font.el in your load-path and `M-x byte-compile-file' it.
;;   2) Add this to your ~/.emacs (without the ";;" in front):
;;        (add-hook 'html-helper-load-hook '(lambda () (require 'html-font)))
;; Customize font-lock as you would do for any other major mode.
;;   For XEmacs and Emacs 19.29, default colours are set up automatically.
;;   For earlier Emacsen, you have to write a lot yourself (or get it from
;;   somewhere else, e.g. Simon Marshall's face-lock.el in the LCD; see also
;;   http://www.nbi.dk/~dickow/emacs/), for example:
;;   3) To have a colourful display with font lock in any supported major mode,
;;      you can add something like this to your ~/.emacs:
;;        (copy-face             'bold 'font-lock-keyword-face) ; or italic/...
;;        (set-face-foreground         'font-lock-keyword-face "ForestGreen")
;;        (setq font-lock-keyword-face 'font-lock-keyword-face)
;;      and similary for `font-lock-string-face' etc. (see font-lock.el).
;;   4) To have font-lock turned on every time html-helper-mode is entered do
;;        (add-hook 'html-helper-mode-hook '(lambda () (font-lock-mode 1)))

;;; Code:

(require 'html-helper-mode) ; Even though we no longer modify the syntax table.
(require 'font-lock) ; New in 3.0, for better backwards compatibility (<19.29).

;; Originally aimed at Emacs 19.29.  Later on disabled syntactic fontification
;; and reordered regexps completely, to be compatible with XEmacs (it doesn't
;; understand OVERRIDE=`keep').
;;
;; We make an effort on handling nested tags intelligently.

;; font-lock compatibility with XEmacs/Lucid and older Emacsen (<19.29).
;;
(if (string-match "XEmacs\\|Lucid" (emacs-version))
    ;; XEmacs/Lucid
    ;; Make needed faces if the user hasn't already done so.
    ;; Respect X resources (`make-face' uses them when they exist).
    (let ((change-it
	   (function (lambda (face)
		       (or (if (fboundp 'facep)
			       (facep face)
			     (memq face (face-list)))
			   (make-face face))
		       (not (face-differs-from-default-p face))))))
      (if (funcall change-it 'html-helper-bold-face)
	  (copy-face 'bold 'html-helper-bold-face))
      (if (funcall change-it 'html-helper-italic-face)
	  (copy-face 'italic 'html-helper-italic-face))
      (if (funcall change-it 'html-helper-underline-face)
	  (set-face-underline-p 'html-helper-underline-face t))
      (if (funcall change-it 'font-lock-variable-name-face)
	  (set-face-foreground 'font-lock-variable-name-face "salmon"))
      (if (funcall change-it 'font-lock-reference-face)
	  (set-face-foreground 'font-lock-reference-face "violet")))
  ;; Emacs (any version)
  ;;
  ;; Note that Emacs evaluates the face entries in `font-lock-keywords',
  ;; while XEmacs doesn't.  So XEmacs doesn't use the following *variables*,
  ;; but instead the faces with the same names as the variables.
  (defvar html-helper-bold-face 'bold
    "Face used as bold.  Typically `bold'.")
  (defvar html-helper-italic-face 'italic
    "Face used as italic.  Typically `italic'.")
  (defvar html-helper-underline-face 'underline
    "Face used as underline.  Typically `underline'.")
  ;;
  (if (string-lessp "19.28.89" emacs-version)
      () ; Emacs 19.29 and later
    ;; Emacs 19.28 and older
    ;; Define face variables that don't exist until Emacs 19.29.
    (defvar font-lock-variable-name-face 'font-lock-doc-string-face
      "Face to use for variable names -- and some HTML keywords.")
    (defvar font-lock-reference-face 'underline ; Ugly at line breaks
      "Face to use for references -- including HTML hyperlink texts.")))

(defvar html-helper-font-lock-keywords
  (let ((tword "\\(h1\\|title\\)")          ; Titles, like function defs
	(bword "\\(b\\|h[2-4]\\|strong\\)") ; Names of tags to boldify
	(iword "\\(address\\|cite\\|em\\|i\\|var\\)") ; ... to italify
	;; Regexp to match shortest sequence that surely isn't a bold end.
	;; We simplify a bit by extending "</strong>" to "</str.*".
	;; Do similarly for non-italic and non-title ends.
	(not-bend (concat "\\([^<]\\|<\\([^/]\\|/\\([^bhs]\\|"
			  "b[^>]\\|"
			  "h\\([^2-4]\\|[2-4][^>]\\)\\|"
			  "s\\([^t]\\|t[^r]\\)\\)\\)\\)"))
	(not-iend (concat "\\([^<]\\|<\\([^/]\\|/\\([^aceiv]\\|"
			  "a\\([^d]\\|d[^d]\\)\\|"
			  "c\\([^i]\\|i[^t]\\)\\|"
			  "e\\([^m]\\|m[^>]\\)\\|"
			  "i[^>]\\|"
			  "v\\([^a]\\|a[^r]\\)\\)\\)\\)"))
	(not-tend (concat "\\([^<]\\|<\\([^/]\\|/\\([^ht]\\|"
			  "h[^1]\\|t\\([^i]\\|i[^t]\\)\\)\\)\\)")))
    (list ; Avoid use of `keep', since XEmacs will treat it the same as `t'.
     ;; First fontify the text of a HREF anchor.  It may be overridden later.
     ;; Anchors in headings will be made bold, for instance.
     '("<a\\s-+href[^>]*>\\([^>]+\\)</a>"
       1 font-lock-reference-face t)
     ;; Tag pairs like <b>...</b> etc.
     ;; Cunning repeated fontification to handle common cases of overlap.
     ;; Bold complex --- possibly with arbitrary other non-bold stuff inside.
     (list (concat "<" bword ">\\(" not-bend "*\\)</\\1>")
	   2 'html-helper-bold-face t)
     ;; Italic complex --- possibly with arbitrary non-italic kept inside.
     (list (concat "<" iword ">\\(" not-iend "*\\)</\\1>")
	   2 'html-helper-italic-face t)
     ;; Bold simple --- first fontify bold regions with no tags inside.
     (list (concat "<" bword ">\\("  "[^<]"  "*\\)</\\1>")
	   2 'html-helper-bold-face t)
     ;; Any tag, general rule, just after bold/italic stuff.
     '("\\(<[^>]*>\\)" 1 font-lock-type-face t)
     ;; Titles and level 1 headings (anchors do sometimes appear in h1's)
     (list (concat "<" tword ">\\(" not-tend "*\\)</\\1>")
	   2 'font-lock-function-name-face t)
     ;; Underline is rarely used. Only handle it when no tags inside.
     '("<u>\\([^<]*\\)</u>" 1 html-helper-underline-face t)
     ;; Forms, anchors & images (also fontify strings inside)
     '("\\(<\\(form\\|i\\(mg\\|nput\\)\\)\\>[^>]*>\\)"
       1 font-lock-variable-name-face t)
     '("</a>" 0 font-lock-keyword-face t)
     '("\\(<a\\b[^>]*>\\)" 1 font-lock-keyword-face t)
     '("=[ \t\n]*\\(\"[^\"]+\"\\)" 1 font-lock-string-face t)
     ;; Large-scale structure keywords (like "program" in Fortran).
     ;;   "<html>" "</html>" "<body>" "</body>" "<head>" "</head>" "</form>"
     '("</?\\(body\\|form\\|h\\(ead\\|tml\\)\\)>"
       0 font-lock-variable-name-face t)
     ;; HTML special characters
     '("&[^;\n]*;" 0 font-lock-string-face t)
     ;; SGML things like <!DOCTYPE ...> with possible <!ENTITY...> inside.
     '("\\(<![a-z]+\\>[^<>]*\\(<[^>]*>[^<>]*\\)*>\\)"
       1 font-lock-comment-face t)
     ;; Comments: <!-- ... -->. They traditionally override anything else.
     ;; It's complicated 'cause we won't allow "-->" inside a comment, and
     ;; font-lock colours the *longest* possible match of the regexp.
     '("\\(<!--\\([^-]\\|-[^-]\\|--[^>]\\)*-->\\)"
       1 font-lock-comment-face t)))
    "Additional expressions to highlight in HTML helper mode.")

(defun html-font-setup ()
  "Font-lock setup for `html-helper-mode' or `html-helper-mode-hook'."
   ;; font-lock setup for various emacsen: XEmacs, Emacs 19.29+, Emacs <19.29.
  ;; By Ulrik Dickow <dickow@nbi.dk>.  (Last update: 05-Sep-1995).
  (cond	((string-match "XEmacs\\|Lucid" (emacs-version)) ; XEmacs/Lucid
	 (put major-mode 'font-lock-keywords-case-fold-search t))
	;; XEmacs (19.13, at least) guesses the rest correctly.
	;; If any older XEmacsen don't, then tell me.
	;;
	((string-lessp "19.28.89" emacs-version) ; Emacs 19.29 and later
	 (make-local-variable 'font-lock-defaults)
	 (setq font-lock-defaults '(html-helper-font-lock-keywords t t)))
	;;
	(t ; Emacs 19.28 and older
	 (make-local-variable 'font-lock-keywords-case-fold-search)
	 (make-local-variable 'font-lock-keywords)
	 (make-local-variable 'font-lock-no-comments)
	 (setq font-lock-keywords-case-fold-search t)
	 (setq font-lock-keywords html-helper-font-lock-keywords)
	 (setq font-lock-no-comments t))))

(add-hook 'html-helper-mode-hook (function html-font-setup))

(provide 'html-font)

;;; html-font.el ends here
