;;; auc-old.el - Compatibility with AUC TeX 6.*
;;
;; Maintainer: Per Abrahamsen <auc-tex@sunsite.dk>
;; Version: 11.14
;;
;; Copyright (C) 1991, 2001 Kresten Krab Thorup 
;; Copyright (C) 1993 Per Abrahamsen 
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This file contains an alternative keymapping, compatible with
;; older versions of AUC TeX.  You are strongly suggested to try the
;; new keyboard layout, as we would like this file to go away
;; eventually. 

;;; Code:

(require 'latex)

;;; Keymaps

(defun TeX-define-key (key value)
  "OBSOLETE: Define KEY to VALUE in TeX and LaTeX mode."
  (define-key plain-TeX-mode-map key value)
  (define-key LaTeX-mode-map key value))
		
(TeX-define-key "\n"       'TeX-terminate-paragraph)
(TeX-define-key "\e}"     'up-list)
(TeX-define-key "\e{"     'TeX-insert-braces)
(TeX-define-key "\C-c\C-b" 'TeX-bold)
(TeX-define-key "\C-c\C-i" 'TeX-italic)
(TeX-define-key "\C-c\C-s" 'TeX-slanted)
(TeX-define-key "\C-c\C-r" 'TeX-roman)
(TeX-define-key "\C-c\C-e" 'TeX-emphasize)
(TeX-define-key "\C-c\C-t" 'TeX-typewriter)
(TeX-define-key "\C-c\C-y" 'TeX-small-caps)
(TeX-define-key "\C-c\C-d" 'TeX-region)
(TeX-define-key "\C-c\C-a" 'TeX-buffer)
(TeX-define-key "\C-c\C-p" 'TeX-preview)
(TeX-define-key "\C-c\C-n" 'TeX-next-error)
(TeX-define-key "\C-c!"    'TeX-print)
(TeX-define-key "\e\t"    'TeX-complete-symbol)
(TeX-define-key "\C-c$"    'TeX-run-lacheck)

(define-key LaTeX-mode-map "\C-c\n"   'TeX-terminate-paragraph)
(define-key LaTeX-mode-map "\C-c\C-x" 'LaTeX-section)
(define-key LaTeX-mode-map "\C-c\C-c" 'LaTeX-environment)
(define-key LaTeX-mode-map "\C-c@"    'LaTeX-bibtex)
(define-key LaTeX-mode-map "\C-c#"    'LaTeX-makeindex)
(define-key LaTeX-mode-map "\em"     'LaTeX-math-mode)
(define-key LaTeX-mode-map "\es"     'LaTeX-fill-section)
(define-key LaTeX-mode-map "\e\C-e"  'LaTeX-mark-environment)
(define-key LaTeX-mode-map "\e\C-x"  'LaTeX-mark-section) 
(define-key LaTeX-mode-map "\e\C-q"  'LaTeX-fill-environment)

;;; Buffer

(defun TeX-region (begin end)
  "OBSOLETE: Run TeX-command-default on current region."
  (interactive "r")
  (require 'tex-buf)
  (setq TeX-current-process-region-p t)
  (if (nth 4 (assoc TeX-command-default TeX-command-list))
      (TeX-region-create (TeX-region-file "tex")
			 (buffer-substring begin end)
			 (file-name-nondirectory (buffer-file-name))
			 (TeX-current-offset begin)))
  (TeX-command TeX-command-default 'TeX-region-file))

(defun TeX-buffer ()
  "OBSOLETE: Run TeX-command-default on the current document."
  (interactive)
  (save-some-buffers) ; added for compatibility reasons
  (require 'tex-buf)
  (setq TeX-current-process-region-p nil)
  (TeX-command TeX-command-default 'TeX-master-file))

(defun TeX-old-command (name)
  "OBSOLETE: Run command NAME on either the current document or region."
  (require 'tex-buf)
  (if TeX-current-process-region-p
      (TeX-command name 'TeX-region-file)
    (TeX-command name 'TeX-master-file)))

(defun TeX-preview ()
  "OBSOLETE: Run View command on either the current document or region."
  (interactive)
  (TeX-old-command "View"))

(defun TeX-print ()
  "OBSOLETE: Run Print command on either the current document or region."
  (interactive)
  (TeX-old-command "Print"))

(defun TeX-run-lacheck()
  "OBSOLETE: Run lacheck command on either the current document or region."
  (interactive)
  (TeX-old-command "Check"))

(defun LaTeX-bibtex ()
  "OBSOLETE: Run BibTeX command on either the current document or region."
  (interactive)
  (TeX-old-command TeX-command-BibTeX))

(defun LaTeX-makeindex ()
  "OBSOLETE: Run Index command on either the current document or region."
  (interactive)
  (TeX-old-command "Index"))

;;; Fonts

(defun TeX-bold ()
  (interactive "*")
  (insert TeX-grop TeX-esc "bf " TeX-grcl)
  (backward-char 1))

(defun TeX-italic ()
  (interactive "*")
  (insert TeX-grop TeX-esc "it " TeX-esc "/" TeX-grcl)
  (backward-char 3))

(defun TeX-slanted ()
  (interactive "*")
  (insert TeX-grop TeX-esc "sl " TeX-esc "/" TeX-grcl)
  (backward-char 3))

(defun TeX-roman ()
  (interactive "*")
  (insert TeX-grop TeX-esc "rm " TeX-grcl)
  (backward-char 1))

(defun TeX-emphasize ()
  (interactive "*")
  (insert TeX-grop TeX-esc "em " TeX-esc "/" TeX-grcl)
  (backward-char 3))

(defun TeX-typewriter ()
  (interactive "*")
  (insert TeX-grop TeX-esc "tt " TeX-grcl)
  (backward-char 1))

(defun TeX-small-caps ()
  (interactive "*")
  (insert TeX-grop TeX-esc "sc " TeX-grcl)
  (backward-char 1))

;;; AUC (La)TeX Mode
;;
;; Added by marsj@ida.liu.se Thu Mar  5 17:52:38 1992 to support
;; automatic mode change after using insert-mode-line hook. Also
;; modified regexp to choose tex mode to be more aware of latex
;; (documentstyle is uniq, isn'it)

(defun insert-mode-line ()
    "This little macro inserts `% -*- mode-name -*-' if not present.
You should insert this in your TeX-mode-hook!"
    (interactive "*")
    (save-excursion
      (goto-char (point-min))
      (if (not (re-search-forward "-\\*-.*-\\*-" 100 t))
	  (insert "% -*- " (substring (symbol-name major-mode) 0 -5)
		  " -*-\n"))))

(defun auc-tex-mode ()
  "Called when we have a mode line specification in first line."
  (interactive)
  (plain-tex-mode))

(defun auc-latex-mode ()
  "Called when we have a mode line specification in first line."
  (interactive)
  (latex-mode))

;;; Validation

(defun TeX-validate-buffer ()
  "Check current buffer for paragraphs containing mismatched $'s.
As each such paragraph is found, a mark is pushed at its beginning,
and the location is displayed for a few seconds."
  (interactive)
  (let ((opoint (point)))
    (goto-char (point-max))
    ;; Does not use save-excursion
    ;; because we do not want to save the mark.
    (unwind-protect
	(while (and (not (input-pending-p)) (not (bobp)))
	  (let ((end (point)))
	    (search-backward "\n\n" nil 'move)
	    (or (TeX-validate-paragraph (point) end)
		(progn
		  (push-mark (point))
		  (message "Mismatch found in pararaph starting here")
		  (sit-for 4)))))
      (goto-char opoint))))

(defun TeX-validate-paragraph (start end)
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (forward-sexp (- end start))
	  t))
    (error nil)))

(defun TeX-terminate-paragraph (inhibit-validation)
  "Insert two newlines, breaking a paragraph for TeX.
Check for mismatched braces/$'s in paragraph being terminated.
A prefix arg inhibits the checking."
  (interactive "*P")
  (or inhibit-validation
      (TeX-validate-paragraph
       (save-excursion
	 (search-backward "\n\n" nil 'move)
	 (point))
       (point))
      (message "Paragraph being closed appears to contain a mismatch"))
  (reindent-then-newline-and-indent)
  (newline-and-indent))

;;; Miscellaneous

(defun TeX-cmd-on-region (begin end command)
  "Reads a (La)TeX-command. Makes current region a TeX-group.
Inserts command at the start of the group."
  (interactive "*r\ns(La)TeX-command on region: ")
  (save-excursion
    (goto-char end)   (insert TeX-grcl)
    (goto-char begin) (insert TeX-grop TeX-esc command " ")))

(provide 'auc-old)
(provide 'auc-tex)

;;; auc-old.el ends here
