;;; latex.el --- Support for LaTeX documents.
;; 
;; Maintainer: Per Abrahamsen <auc-tex@sunsite.dk>
;; Version: 11.14
;; Keywords: wp
;; X-URL: http://www.nongnu.org/auctex/

;; Copyright 1991 Kresten Krab Thorup
;; Copyright 1993, 1994, 1995, 1996, 1997, 1999, 2000 Per Abrahamsen
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

;;; Code:

(require 'tex)

;;; Syntax

(defvar LaTeX-optop "["
  "The LaTeX optional argument opening character.")

(defvar LaTeX-optcl "]"
  "The LaTeX optional argument closeing character.")

;;; Style

(defcustom LaTeX-default-style "article"
  "*Default when creating new documents."
  :group 'LaTeX-environment
  :type 'string)

(defcustom LaTeX-default-options nil
  "Default options to documentstyle.
A list of strings."
  :group 'LaTeX-environment
  :type '(repeat (string :format "%v")))

 (make-variable-buffer-local 'LaTeX-default-options)

;;; Syntax Table

(defvar LaTeX-mode-syntax-table (copy-syntax-table TeX-mode-syntax-table)
  "Syntax table used in LaTeX mode.")

(progn ; set [] to match for LaTeX.
  (modify-syntax-entry (string-to-char LaTeX-optop)
		       (concat "(" LaTeX-optcl)
		       LaTeX-mode-syntax-table)
  (modify-syntax-entry (string-to-char LaTeX-optcl)
		       (concat ")" LaTeX-optop)
		       LaTeX-mode-syntax-table))

;;; Sections

(defun LaTeX-section (arg)
  "Insert a template for a LaTeX section.
Determinate the type of section to be inserted, by the argument ARG.

If ARG is nil or missing, use the current level.
If ARG is a list (selected by C-u), go downward one level.
If ARG is negative, go up that many levels.
If ARG is positive or zero, use absolute level:

  0 : part
  1 : chapter
  2 : section
  3 : subsection
  4 : subsubsection
  5 : paragraph
  6 : subparagraph

The following variables can be set to customize:

LaTeX-section-hook	Hooks to run when inserting a section.
LaTeX-section-label	Prefix to all section labels."
  
  (interactive "*P")
  (let* ((val (prefix-numeric-value arg))
	 (level (cond ((null arg)
		       (LaTeX-current-section))
		      ((listp arg)
		       (LaTeX-down-section))
		      ((< val 0)
		       (LaTeX-up-section (- val)))
		      (t val)))
	 (name (LaTeX-section-name level))
	 (toc nil)
	 (title "")
	 (done-mark (make-marker)))
    (newline)
    (run-hooks 'LaTeX-section-hook)
    (newline)
    (if (marker-position done-mark)
	(goto-char (marker-position done-mark)))
    (set-marker done-mark nil)))

(defun LaTeX-current-section ()
  "Return the level of the section that contain point.
See also `LaTeX-section' for description of levels."
  (save-excursion
    (max (LaTeX-largest-level)
	 (if (re-search-backward (LaTeX-outline-regexp) nil t)
	     (- (LaTeX-outline-level) (LaTeX-outline-offset))
	   (LaTeX-largest-level)))))

(defun LaTeX-down-section ()
  "Return the value of a section one level under the current.
Tries to find what kind of section that have been used earlier in the
text, if this fail, it will just return one less than the current
section."
  (save-excursion
    (let ((current (LaTeX-current-section))
	  (next nil)
	  (regexp (LaTeX-outline-regexp)))
      (if (not (re-search-backward regexp nil t))
	  (1+ current)
	(while (not next)
	  (cond
	   ((eq (LaTeX-current-section) current)
	    (if (re-search-forward regexp nil t)
		(if (<= (setq next (LaTeX-current-section)) current) ;Wow!
		    (setq next (1+ current)))
	      (setq next (1+ current))))
	   ((not (re-search-backward regexp nil t))
	    (setq next (1+ current)))))
	next))))

(defun LaTeX-up-section (arg)
  "Return the value of the section ARG levels above this one."
  (save-excursion
    (if (zerop arg)
	(LaTeX-current-section)
      (let ((current (LaTeX-current-section)))
	(while (and (>= (LaTeX-current-section) current)
		    (re-search-backward (LaTeX-outline-regexp)
					nil t)))
	(LaTeX-up-section (1- arg))))))

(defvar LaTeX-section-list '(("part" 0)
			     ("chapter" 1)
			     ("section" 2)
			     ("subsection" 3)
			     ("subsubsection" 4)
			     ("paragraph" 5)
			     ("subparagraph" 6))
  "List which elements is the names of the sections used by LaTeX.")

(defun LaTeX-section-name (level)
  "Return the name of the section corresponding to LEVEL."
  (let ((entry (TeX-member level LaTeX-section-list
			   (function (lambda (a b) (equal a (nth 1 b)))))))
    (if entry
	(nth 0 entry)
      nil)))

(defun LaTeX-section-level (name)
  "Return the level of the section NAME."
  (let ((entry (TeX-member name LaTeX-section-list
			   (function (lambda (a b) (equal a (nth 0 b)))))))

    (if entry
	(nth 1 entry)
      nil)))

(defcustom TeX-outline-extra nil
  "List of extra TeX outline levels.

Each element is a list with two entries.  The first entry is the
regular expression matching a header, and the second is the level of
the header.  See `LaTeX-section-list' for existing header levels."
  :group 'LaTeX
  :type '(repeat (group (regexp :tag "Match")
			(integer :tag "Level"))))

(defun LaTeX-outline-regexp (&optional anywhere)
  "Return regexp for LaTeX sections.

If optional argument ANYWHERE is not nil, do not require that the
header is at the start of a line."
  (concat (if anywhere "" "^")
	  "[ \t]*"
	  (regexp-quote TeX-esc)
	  "\\(appendix\\|documentstyle\\|documentclass\\|"
	  (mapconcat 'car LaTeX-section-list "\\|")
	  "\\)\\b"
	  (if TeX-outline-extra
	      "\\|"
	    "")
	  (mapconcat 'car TeX-outline-extra "\\|")
	  "\\|" TeX-header-end
	  "\\|" TeX-trailer-start))

(defvar LaTeX-largest-level nil
  "Largest sectioning level with current document style.")

(make-variable-buffer-local 'LaTeX-largest-level)

(defun LaTeX-largest-level ()
  (TeX-update-style)
  LaTeX-largest-level)

(defun LaTeX-outline-offset ()
  "Offset to add to `LaTeX-section-list' levels to get outline level."
  (- 2 (LaTeX-largest-level)))

(defun TeX-look-at (list)
  "Check if we are looking at the first element of a member of LIST.
If so, return the second element, otherwise return nil."
  (while (and list
	      (not (looking-at (nth 0 (car list)))))
    (setq list (cdr list)))
  (if list
      (nth 1 (car list))
    nil))

(defun LaTeX-outline-level ()
  "Find the level of current outline heading in an LaTeX document."
  (cond ((looking-at LaTeX-header-end) 1)
	((looking-at LaTeX-trailer-start) 1)
	((TeX-look-at TeX-outline-extra)
	 (max 1 (+ (TeX-look-at TeX-outline-extra)
		   (LaTeX-outline-offset))))
	(t
	 (save-excursion
	  (skip-chars-forward " \t")
	  (forward-char 1)
	  (cond ((looking-at "appendix") 1)
		((looking-at "documentstyle") 1)
		((looking-at "documentclass") 1)
		((TeX-look-at LaTeX-section-list)
		 (max 1 (+ (TeX-look-at LaTeX-section-list)
			   (LaTeX-outline-offset))))
		(t
		 (error "Unrecognized header")))))))

(defun LaTeX-outline-name ()
  "Guess a name for the current header line."
  (save-excursion
    (if (re-search-forward "{\\([^\}]*\\)}" (+ (point) fill-column 10) t)
	(match-string 1)
      (buffer-substring (point) (+ 20 (point))))))

(add-hook 'TeX-remove-style-hook
	  (function (lambda () (setq LaTeX-largest-level nil))))

(defcustom LaTeX-section-hook
  '(LaTeX-section-heading
    LaTeX-section-title
;; LaTeX-section-toc		; Most people won't want this
    LaTeX-section-section
    LaTeX-section-label)
  "List of hooks to run when a new section is inserted.

The following variables are set before the hooks are run

level - numeric section level, see the documentation of `LaTeX-section'.
name - name of the sectioning command, derived from `level'.
title - The title of the section, default to an empty string.
toc - Entry for the table of contents list, default nil.
done-mark - Position of point afterwards, default nil (meaning end).

The following standard hook exist -

LaTeX-section-heading: Query the user about the name of the
sectioning command.  Modifies `level' and `name'.

LaTeX-section-title: Query the user about the title of the
section. Modifies `title'.

LaTeX-section-toc: Query the user for the toc entry.  Modifies
`toc'.

LaTeX-section-section: Insert LaTeX section command according to
`name', `title', and `toc'.  If `toc' is nil, no toc entry is
enserted.  If `toc' or `title' are empty strings, `done-mark' will be
placed at the point they should be inserted.

LaTeX-section-label: Insert a label after the section command.
Controled by the variable `LaTeX-section-label'.

To get a full featured `LaTeX-section' command, insert

 (setq LaTeX-section-hook
       '(LaTeX-section-heading
	 LaTeX-section-title
	 LaTeX-section-toc
	 LaTeX-section-section
	 LaTeX-section-label))

in your .emacs file."
  :type 'hook
  :options '(LaTeX-section-heading
	     LaTeX-section-title
	     LaTeX-section-toc
	     LaTeX-section-section
	     LaTeX-section-label))


(defcustom LaTeX-section-label
  '(("chapter" . "cha:")
    ("section" . "sec:")
    ("subsection" . "sec:"))
  "Default prefix when asking for a label.

If it is a string, it it used unchanged for all kinds of sections.
If it is nil, no label is inserted.
If it is a list, the list is searched for a member whose car is equal
to the name of the sectioning command being inserted.  The cdr is then
used as the prefix.  If the name is not found, or if the cdr is nil,
no label is inserted."
  :group 'LaTeX-label
  :type '(choice (const :tag "none" nil)
		 (string :format "%v" :tag "Common")
		 (repeat :menu-tag "Level specific"
			 :format "\n%v%i"
			 (cons :format "%v"
			       (string :tag "Type")
			       (choice :tag "Prefix"
				       (const :tag "none" nil)
				       (string  :format "%v"))))))

;;; Section Hooks.

(defun LaTeX-section-heading ()
  "Hook to prompt for LaTeX section name.
Insert this hook into `LaTeX-section-hook' to allow the user to change
the name of the sectioning command inserted with `\\[LaTeX-section]'."
  (let ((string (completing-read
		 (concat "Select level: (default " name ") ")
		 LaTeX-section-list
		 nil nil nil)))
    ; Update name
    (if (not (zerop (length string)))
	(setq name string))
    ; Update level
    (setq level (LaTeX-section-level name))))

(defun LaTeX-section-title ()
  "Hook to prompt for LaTeX section title.
Insert this hook into `LaTeX-section-hook' to allow the user to change
the title of the section inserted with `\\[LaTeX-section]."
  (setq title (read-string "What title: ")))

(defun LaTeX-section-toc ()
  "Hook to prompt for the LaTeX section entry in the table of content .
Insert this hook into `LaTeX-section-hook' to allow the user to insert
a different entry for the section in the table of content."
  (setq toc (read-string "Toc Entry: "))
  (if (zerop (length toc))
      (setq toc nil)))

(defun LaTeX-section-section ()
  "Hook to insert LaTeX section command into the file.
Insert this hook into `LaTeX-section-hook' after those hooks which sets
the `name', `title', and `toc' variables, but before those hooks which
assumes the section already is inserted."
    (insert TeX-esc name)
    (cond ((null toc))
	  ((zerop (length toc))
	   (insert LaTeX-optop)
	   (set-marker done-mark (point))
	   (insert LaTeX-optcl))
	  (t
	   (insert LaTeX-optop toc LaTeX-optcl)))
    (insert TeX-grop)
    (if (zerop (length title))
	(set-marker done-mark (point)))
    (insert title TeX-grcl)
    (newline)
    ;; If RefTeX is available, tell it that we've just made a new section
    (and (fboundp 'reftex-notice-new-section)
	 (reftex-notice-new-section)))

(defun LaTeX-section-label ()
  "Hook to insert a label after the sectioning command.
Insert this hook into `LaTeX-section-hook' to prompt for a label to be
inserted after the sectioning command.

The behaviour of this hook is controlled by `LaTeX-section-label'."
  (and (LaTeX-label name)
       (newline)))

;;; Environments

(defgroup LaTeX-environment nil
  "Environments in AUC TeX."
  :group 'LaTeX-macro)

(defcustom LaTeX-default-environment "itemize"
  "*The default environment when creating new ones with `LaTeX-environment'."
  :group 'LaTeX-environment
  :type 'string)
 (make-variable-buffer-local 'LaTeX-default-environment)

(defvar LaTeX-environment-history nil)

(defun LaTeX-environment (arg)
  "Make LaTeX environment (\\begin{...}-\\end{...} pair).
With optional ARG, modify current environment.
 
It may be customized with the following variables:
 
`LaTeX-default-environment'       Your favorite environment.
`LaTeX-default-style'             Your favorite document style.
`LaTeX-default-options'           Your favorite document style options.
`LaTeX-float'                     Where you want figures and tables to float.
`LaTeX-table-label'               Your prefix to labels in tables.
`LaTeX-figure-label'              Your prefix to labels in figures.
`LaTeX-default-format'            Format for array and tabular.
`LaTeX-default-position'          Position for array and tabular."
 
  (interactive "*P")
  (let ((environment (completing-read (concat "Environment type: (default "
                                               (if (TeX-near-bobp)
                                                   "document"
                                                 LaTeX-default-environment)
                                               ") ")
				      (LaTeX-environment-list)
				      nil nil nil
				      'LaTeX-environment-history)))
    ;; Get default
    (cond ((and (zerop (length environment))
                (TeX-near-bobp))
           (setq environment "document"))
          ((zerop (length environment))
           (setq environment LaTeX-default-environment))
          (t
           (setq LaTeX-default-environment environment)))
 
    (let ((entry (assoc environment (LaTeX-environment-list))))
      (if (null entry)
          (LaTeX-add-environments (list environment)))
 
      (if arg
	  (LaTeX-modify-environment environment)
	(LaTeX-environment-menu environment)))))

(defun LaTeX-environment-menu (environment)
  "Insert ENVIRONMENT around point or region."
  (let ((entry (assoc environment (LaTeX-environment-list))))
    (cond ((not (and entry (nth 1 entry)))
	   (LaTeX-insert-environment environment))
	  ((numberp (nth 1 entry))
	   (let ((count (nth 1 entry))
		 (args ""))
	     (while (> count 0)
	       (setq args (concat args TeX-grop TeX-grcl))
	       (setq count (- count 1)))
	     (LaTeX-insert-environment environment args)))
	  ((stringp (nth 1 entry))
	   (let ((prompts (cdr entry))
		 (args ""))
	     (while prompts
	       (setq args (concat args
				  TeX-grop
				  (read-from-minibuffer
				   (concat (car prompts) ": "))
				  TeX-grcl))
	       (setq prompts (cdr prompts)))
	     (LaTeX-insert-environment environment args)))
	  (t
	   (apply (nth 1 entry) environment (nthcdr 2 entry))))))

(defun LaTeX-close-environment ()
  "Create an \\end{...} to match the current environment."
  (interactive "*")
  (if (> (point)
	 (save-excursion
	   (beginning-of-line)
	   (skip-chars-forward " \t")
	   (point)))
      (insert "\n"))
  (insert "\\end{" (LaTeX-current-environment 1) "}")
  (indent-according-to-mode)
  (if (not (looking-at "[ \t]*$"))
      (insert "\n")
    (let ((next-line-add-newlines t))
      (next-line 1)
      (beginning-of-line)))
  (indent-according-to-mode))

(autoload 'outline-flag-region "outline")

(defun LaTeX-hide-environment ()
  "Hide current LaTeX environment using selective display."
  (interactive)
  (outline-flag-region (save-excursion (LaTeX-find-matching-begin) (point))
		       (save-excursion (LaTeX-find-matching-end) (point))
		       (if (featurep 'noutline) t ?\r)))

(unless (widget-plist-member (symbol-plist 'LaTeX-hide-environment) 'disabled)
  (put 'LaTeX-hide-environment 'disabled t))

(defun LaTeX-show-environment ()
  "Show current LaTeX environment."
  (interactive)
  (outline-flag-region (save-excursion (LaTeX-find-matching-begin) (point))
		       (save-excursion (LaTeX-find-matching-end) (point))
		       (if (featurep 'noutline) nil ?\n)))

(defun LaTeX-insert-environment (environment &optional extra)
  "Insert ENVIRONMENT of type ENV, with optional argument EXTRA."
  (if (and (TeX-active-mark)
	   (not (eq (mark) (point))))
      (progn
	(if (< (mark) (point))
	    (exchange-point-and-mark))
	(or (TeX-looking-at-backward "^[ \t]*")
	    (newline))
	(insert TeX-esc "begin" TeX-grop environment TeX-grcl)
	(indent-according-to-mode)
	(if extra (insert extra))
	(newline)
	(goto-char (mark))
	(or (TeX-looking-at-backward "^[ \t]*")
	    (newline))
	(insert TeX-esc "end" TeX-grop environment TeX-grcl)
	(or (looking-at "[ \t]*$")
	    (save-excursion (newline-and-indent)))
	(indent-according-to-mode)
	(end-of-line 0)
	(or (assoc environment LaTeX-indent-environment-list)
	    (LaTeX-fill-environment nil)))
    (or (TeX-looking-at-backward "^[ \t]*")
	(newline))
    (insert TeX-esc "begin" TeX-grop environment TeX-grcl)
    (indent-according-to-mode)
    (if extra (insert extra))
    (newline-and-indent)
    (newline)
    (insert TeX-esc "end" TeX-grop environment TeX-grcl)
    (or (looking-at "[ \t]*$")
	(save-excursion (newline-and-indent)))
    (indent-according-to-mode)
    (end-of-line 0)))

(defun LaTeX-modify-environment (environment)
  "Modify current environment."
  (save-excursion
    (LaTeX-find-matching-end)
    (re-search-backward (concat (regexp-quote TeX-esc)
				"end"
				(regexp-quote TeX-grop)
				" *\\([a-zA-Z*]*\\)"
				(regexp-quote TeX-grcl))
			(save-excursion (beginning-of-line 1) (point)))
    (replace-match (concat TeX-esc "end" TeX-grop environment TeX-grcl) t t)
    (beginning-of-line 1)
    (LaTeX-find-matching-begin)
    (re-search-forward (concat (regexp-quote TeX-esc)
			       "begin"
			       (regexp-quote TeX-grop)
			       " *\\([a-zA-Z*]*\\)"
			       (regexp-quote TeX-grcl))
		       (save-excursion (end-of-line 1) (point)))
    (replace-match (concat TeX-esc "begin" TeX-grop environment TeX-grcl) t t)))

(defun LaTeX-current-environment (&optional arg)
  "Return the name (a string) of the enclosing LaTeX environment.
With optional ARG>=1, find that outer level."
  (setq arg (if arg (if (< arg 1) 1 arg) 1))
  (save-excursion
    (while (and
	    (/= arg 0)
	    (re-search-backward
	     (concat (regexp-quote TeX-esc) "begin" (regexp-quote TeX-grop)
		     "\\|"
		     (regexp-quote TeX-esc) "end" (regexp-quote TeX-grop))
	     nil t 1))
      (cond ((TeX-in-comment)
	     (beginning-of-line 1))
	    ((looking-at (concat (regexp-quote TeX-esc)
				 "end" (regexp-quote TeX-grop)))
	     (setq arg (1+ arg)))
	    (t
	    (setq arg (1- arg)))))
    (if (/= arg 0)
	"document"
      (search-forward TeX-grop)
      (let ((beg (point)))
	(search-forward TeX-grcl)
	(backward-char 1)
	(buffer-substring beg (point))))))

(defun TeX-near-bobp ()
  "Return t iff there's nothing but whitespace between (bob) and (point)."
  (save-excursion
    (skip-chars-backward " \t\n")
    (bobp)))

;;; Environment Hooks

(defvar LaTeX-document-style-hook nil
  "List of hooks to run when inserting a document style environment.

To insert a hook here, you must insert it in the appropiate style file.")

(defun LaTeX-env-document (&optional ignore)
  "Create new LaTeX document."

  (TeX-insert-macro (if (string-equal LaTeX-version "2")
			"documentstyle"
		      "documentclass"))

  (newline 3)
  (end-of-line 0)
  (LaTeX-insert-environment "document")
  (run-hooks 'LaTeX-document-style-hook)
  (setq LaTeX-document-style-hook nil))

(defcustom LaTeX-float "htbp"
  "*Default float when creating figure and table environments.
Set to nil if you don't want any float."
  :group 'LaTeX-environment
  :type '(choice (const :tag "none" nil)
		 (string :format "%v")))
 (make-variable-buffer-local 'LaTeX-float)

(defcustom LaTeX-top-caption-list nil
  "*List of float environments with top caption."
  :group 'LaTeX-environment
  :type '(repeat (string :format "%v")))

(defgroup LaTeX-label nil
  "Adding labels for LaTeX commands in AUC TeX."
  :group 'LaTeX)

(defcustom LaTeX-label-function nil
  "*A function inserting a label at point.
Sole argument of the function is the environment.  The function has to return
the label inserted, or nil if no label was inserted."
  :group 'LaTeX-label
  :type 'function)

(defcustom LaTeX-figure-label "fig:"
  "*Default prefix to figure labels."
  :group 'LaTeX-label
  :group 'LaTeX-environment
  :type 'string)

(defcustom LaTeX-table-label "tab:"
  "*Default prefix to table labels."
  :group 'LaTeX-label
  :group 'LaTeX-environment
  :type 'string)

(defcustom LaTeX-default-format ""
  "Specifies the default format string for array and tabular environments."
  :group 'LaTeX-environment
  :type 'string)
 (make-variable-buffer-local 'LaTeX-default-format)

(defcustom LaTeX-default-position ""
  "Specifies the default position string for array and tabular environments."
  :group 'LaTeX-environment
  :type 'string)
 (make-variable-buffer-local 'LaTeX-default-position)

(defcustom LaTeX-equation-label "eq:"
  "*Default prefix to equation labels."
  :group 'LaTeX-label
  :type 'string)

(defcustom LaTeX-eqnarray-label LaTeX-equation-label
  "*Default prefix to eqnarray labels."
  :group 'LaTeX-label
  :type 'string)

(defun LaTeX-env-item (environment)
  "Insert ENVIRONMENT and the first item."
  (LaTeX-insert-environment environment)
  (if (TeX-active-mark)
      (progn
	(LaTeX-find-matching-begin)
	(end-of-line 1))
    (end-of-line 0))
  (delete-char 1)
  (delete-horizontal-space)
  (LaTeX-insert-item))

(defcustom LaTeX-label-alist
  '(("figure" . LaTeX-figure-label)
    ("table" . LaTeX-table-label)
    ("figure*" . LaTeX-figure-label)
    ("table*" . LaTeX-table-label)
    ("equation" . LaTeX-equation-label)
    ("eqnarray" . LaTeX-eqnarray-label))
  "Lookup prefixes for labels.
An alist where the CAR is the environment name, and the CDR
either the prefix or a symbol referring to one."
  :group 'LaTeX-label
  :type '(repeat (cons (string :tag "Environment")
		       (choice (string :tag "Label prefix")
			       (symbol :tag "Label prefix symbol")))))

(make-variable-buffer-local 'LaTeX-label-alist)

(defun LaTeX-label (environment)
  "Insert a label for ENVIRONMENT at point.
If `LaTeX-label-function' is a valid function, LaTeX label will transfer the
job to this function."
  (let (label)
    (if (and (boundp 'LaTeX-label-function)
	     LaTeX-label-function
	     (fboundp LaTeX-label-function))

	(setq label (funcall LaTeX-label-function environment))
      (let ((prefix
	     (or (cdr (assoc environment LaTeX-label-alist))
		 (if (assoc environment LaTeX-section-list)
		     (if (stringp LaTeX-section-label)
			 LaTeX-section-label
		       (and (listp LaTeX-section-label)
			    (cdr (assoc environment LaTeX-section-label))))
		   ""))))
	(when prefix
	  (when (symbolp prefix)
	    (setq prefix (symbol-value prefix)))
	  ;; Use completing-read as we do with `C-c C-m \label RET'
	  (setq label (completing-read
		       (TeX-argument-prompt t nil "What label")
		       (LaTeX-label-list) nil nil prefix))
	  ;; No label or empty string entered?
	  (if (or (string= prefix label)
		  (string= "" label))
	      (setq label nil)
	    (insert TeX-esc "label" TeX-grop label TeX-grcl))))
      (if label
	  (progn
	    (LaTeX-add-labels label)
	    label)
	nil))))

(defun LaTeX-env-figure (environment)
  "Create ENVIRONMENT with \\label and \\caption commands."
  (let ((float (read-string "Float to: " LaTeX-float))
	(caption (read-string "Caption: "))
        (center (y-or-n-p "Center? ")))

    (setq LaTeX-float (if (zerop (length float))
			  LaTeX-float
			float))

    (LaTeX-insert-environment environment
			      (and LaTeX-float
				   (concat LaTeX-optop
					   LaTeX-float
					   LaTeX-optcl)))

    (if center
	(progn
	  (insert TeX-esc "centering")
	  (indent-according-to-mode)
	  (newline)))

    (if (member environment LaTeX-top-caption-list)
	(progn
	  (if (zerop (length caption))
	      ()
	    ;; NOTE: Caption is _inside_ center because that looks best typeset.
	    (indent-according-to-mode)
	    (insert TeX-esc "caption" TeX-grop caption TeX-grcl)
	    (indent-according-to-mode))

	  (newline-and-indent)
	  (LaTeX-label environment)
	  (indent-according-to-mode)
	  (end-of-line 0)
	  (newline-and-indent))
      ;; not top caption but bottom caption (default).
      (newline-and-indent)
      (LaTeX-label environment)
      (end-of-line 0)
      (indent-according-to-mode)
 
      (if (zerop (length caption))
	  ()
	;; NOTE: Caption is _inside_ center because that looks best typeset.
	(newline-and-indent)
	(insert TeX-esc "caption" TeX-grop caption TeX-grcl)
	(end-of-line 0)
	(indent-according-to-mode)))
  
  (if (member environment '("table" "table*"))
      (LaTeX-env-array "tabular"))))

(defun LaTeX-env-array (environment)
  "Insert ENVIRONMENT with position and column specifications.
Just like array and tabular."
  (let ((pos (read-string "Position: "))
	(fmt (read-string "Format: " LaTeX-default-format)))
    (setq LaTeX-default-position pos)
      (setq LaTeX-default-format fmt)
    (LaTeX-insert-environment environment
			      (concat
			        (if (not (zerop (length pos)))
				    (format "[%s]" pos))
				(format "{%s}" fmt)))
    (end-of-line 0)
    (next-line 1)
    (delete-horizontal-space)))

(defun LaTeX-env-label (environment)
  "Insert ENVIRONMENT and prompt for label."
  (LaTeX-insert-environment environment)
  (and (LaTeX-label environment)
       (newline-and-indent)))

(defun LaTeX-env-list (environment)
  "Insert ENVIRONMENT and the first item."
  (let ((label (read-string "Default Label: ")))
    (LaTeX-insert-environment environment
			      (format "{%s}{}" label))
    (end-of-line 0)
    (delete-char 1)
    (delete-horizontal-space))
  (LaTeX-insert-item))

(defun LaTeX-env-minipage (environment)
  "Create new LaTeX minipage."
  (let ((pos (read-string "Position: " LaTeX-default-position))
	(width (read-string "Width: ")))
    (setq LaTeX-default-position pos)
    (if (zerop (length width))
	(setq width "4cm"))
    (LaTeX-insert-environment environment
			      (concat (if (not (zerop (length pos)))
					  (format "[%s]" pos))
				      (format "{%s}" width)))
    (end-of-line 0)
    (next-line 1)
    (delete-horizontal-space)))

(defun LaTeX-env-tabular* (environment)
  "Insert ENVIRONMENT with width, position and column specifications."
  (let ((width (read-string "Width: "))
	(pos (read-string "Position: " LaTeX-default-position))
	(fmt (read-string "Format: " LaTeX-default-format)))
    (setq LaTeX-default-position pos)
    (setq LaTeX-default-format fmt)
    (LaTeX-insert-environment environment
			      (concat
			        (if (not (zerop (length width)))
				    (format "{%s}" width))
			        (if (not (zerop (length pos)))
					  (format "[%s]" pos))
				(format "{%s}" fmt)))
    (end-of-line 0)
    (next-line 1)
    (delete-horizontal-space)))

(defun LaTeX-env-picture (environment)
  "Insert ENVIRONMENT with width, height specifications."
  (let ((width (read-string "Width: "))
	(height (read-string "Height: "))
	(x-offset (read-string "X Offset: "))
	(y-offset (read-string "Y Offset: ")))
    (if (zerop (length x-offset))
	(setq x-offset "0"))
    (if (zerop (length y-offset))
	(setq y-offset "0"))
    (LaTeX-insert-environment environment
			      (concat (format "(%s,%s)" width height)
				      (if (not (and (string= x-offset "0")
						    (string= y-offset "0")))
					  (format "(%s,%s)" x-offset y-offset))))
				      
    (end-of-line 0)
    (next-line 1)
    (delete-horizontal-space)))

(defun LaTeX-env-bib (environment)
  "Insert ENVIRONMENT with label for bibitem."
  (LaTeX-insert-environment environment
			    (concat TeX-grop
				    (read-string "Label for BibItem: " "99")
				    TeX-grcl))
  (end-of-line 0)
  (delete-char 1)
  (delete-horizontal-space)
  (LaTeX-insert-item))

(defun LaTeX-env-contents (environment)
  "Insert ENVIRONMENT with filename for contents."
  (save-excursion
    (when (re-search-backward "^\\\\documentclass.*{" nil t)
      (error "Put %s environment before \\documentclass" environment)))
  (LaTeX-insert-environment environment
			    (concat TeX-grop
				    (read-string "File: ")
				    TeX-grcl))
  (delete-horizontal-space))

;;; Item hooks

(defvar LaTeX-item-list nil
  "An list of environments where items have a special syntax.
The cdr is the name of the function, used to insert this kind of items.")

(defun LaTeX-insert-item ()
  "Insert a new item in an environment.
You may use `LaTeX-item-list' to change the routines used to insert the item."
  (interactive "*")
  (let ((environment (LaTeX-current-environment)))
    (newline)
    (if (assoc environment LaTeX-item-list)
	(funcall (cdr (assoc environment LaTeX-item-list)))
      (TeX-insert-macro "item"))
    (indent-according-to-mode)))

(defun LaTeX-item-argument ()
  "Insert a new item with an optional argument."
  (let ((TeX-arg-item-label-p t))
    (TeX-insert-macro "item")))

(defun LaTeX-item-bib ()
  "Insert a new bibitem."
  (TeX-insert-macro "bibitem"))

;;; Parser

(defvar LaTeX-auto-minimal-regexp-list
  '(("\\\\document\\(style\\|class\\)\
\\(\\[\\(\\([^#\\\\\\.%]\\|%[^\n\r]*[\n\r]\\)+\\)\\]\\)?\
{\\([^#\\\\\\.\n\r]+\\)}"
     (3 5 1) LaTeX-auto-style)
    ("\\\\use\\(package\\)\\(\\[\\([^\]\\\\]*\\)\\]\\)?\
{\\(\\([^#}\\\\\\.%]\\|%[^\n\r]*[\n\r]\\)+\\)}"
     (3 4 1) LaTeX-auto-style))
  "Minimal list of regular expressions matching LaTeX macro definitions.")

(defvar LaTeX-auto-label-regexp-list
  '(("\\\\label{\\([^\n\r%\\{}]+\\)}" 1 LaTeX-auto-label))
  "List of regular expression matching LaTeX labels only.")

(defvar LaTeX-auto-index-regexp-list
   '(("\\\\\\(index\\|glossary\\){\\([^}{]*\\({[^}{]*\\({[^}{]*\\({[^}{]*}[^}{]*\\)*}[^}{]*\\)*}[^}{]*\\)*\\)}"
 	2 LaTeX-auto-index-entry))
   "List of regular expression matching LaTeX index/glossary entries only.
Regexp allows for up to 3 levels of parenthesis inside the index argument.
This is necessary since index entries may contain commands and stuff.")

(defvar LaTeX-auto-regexp-list
  (append
   '(("\\\\\\(new\\|provide\\)command\\*?{?\\\\\\([a-zA-Z]+\\)}?\\[\\([0-9]+\\)\\]\\[\\([^\n\r]*\\)\\]"
      (2 3 4) LaTeX-auto-optional)
     ("\\\\\\(new\\|provide\\)command\\*?{?\\\\\\([a-zA-Z]+\\)}?\\[\\([0-9]+\\)\\]"
      (2 3) LaTeX-auto-arguments)
     ("\\\\\\(new\\|provide\\)command\\*?{?\\\\\\([a-zA-Z]+\\)}?" 2 TeX-auto-symbol)
     ("\\\\newenvironment\\*?{?\\([a-zA-Z]+\\)}?\\[\\([0-9]+\\)\\]\\["
      1 LaTeX-auto-environment)
     ("\\\\newenvironment\\*?{?\\([a-zA-Z]+\\)}?\\[\\([0-9]+\\)\\]"
      (1 2) LaTeX-auto-env-args)
     ("\\\\newenvironment\\*?{?\\([a-zA-Z]+\\)}?" 1 LaTeX-auto-environment)
     ("\\\\newtheorem{\\([a-zA-Z]+\\)}" 1 LaTeX-auto-environment)
     ("\\\\input{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\include{\\(\\.*[^#}%\\\\\\.\n\r]+\\)\\(\\.[^#}%\\\\\\.\n\r]+\\)?}"
      1 TeX-auto-file)
     ("\\\\bibitem{\\([a-zA-Z][^, \n\r\t%\"#'()={}]*\\)}" 1 LaTeX-auto-bibitem)
     ("\\\\bibitem\\[[^][\n\r]+\\]{\\([a-zA-Z][^, \n\r\t%\"#'()={}]*\\)}"
      1 LaTeX-auto-bibitem)
     ("\\\\bibliography{\\([^#}\\\\\n\r]+\\)}" 1 LaTeX-auto-bibliography))
   LaTeX-auto-label-regexp-list
   LaTeX-auto-index-regexp-list
   LaTeX-auto-minimal-regexp-list)
  "List of regular expression matching common LaTeX macro definitions.")

(defun LaTeX-auto-prepare ()
  "Prepare for LaTeX parsing."
  (setq LaTeX-auto-arguments nil
	LaTeX-auto-optional nil
	LaTeX-auto-env-args nil
	LaTeX-auto-style nil
	LaTeX-auto-end-symbol nil))

(add-hook 'TeX-auto-prepare-hook 'LaTeX-auto-prepare)
  
(defun LaTeX-auto-cleanup ()
  "Cleanup after LaTeX parsing."

  ;; Cleanup BibTeX files
  (setq LaTeX-auto-bibliography
	(apply 'append (mapcar (function (lambda (arg)
					   (TeX-split-string "," arg)))
			       LaTeX-auto-bibliography)))
    
  ;; Cleanup document styles and packages
  (if (null LaTeX-auto-style)
      ()
    (while LaTeX-auto-style
      (let* ((entry (car LaTeX-auto-style))
	     (options (nth 0 entry))
	     (style (nth 1 entry))
	     (class (nth 2 entry)))

	;; Next document style.
	(setq LaTeX-auto-style (cdr LaTeX-auto-style))

	;; Get the options.
	(setq options (TeX-split-string
		       "\\([ \t\r\n]\\|%[^\n\r]*[\n\r]\\|,\\)+"
		       options))

	;; Strip empty options.
	(if (string-equal (car options) "")
	    (setq options (cdr options)))
	(let ((index options))
	  (while (cdr-safe index)
	    (if (string-equal (car (cdr index)) "")
		(setcdr index (cdr (cdr index)))
	      (setq index (cdr index)))))

	;; Add them, to the style list.
	(setq TeX-auto-file (append options TeX-auto-file))

	;; Treat documentclass/documentstyle specially.
	(if (string-equal "package" class)
	    (setq TeX-auto-file
		  (append (TeX-split-string
			   "\\([ \t\r\n]\\|%[^\n\r]*[\n\r]\\|,\\)+" style)
			  TeX-auto-file))
	  ;; And a special "art10" style file combining style and size.
	  (setq TeX-auto-file (cons style TeX-auto-file))
	  (setq TeX-auto-file
		(cons (concat
		       (cond ((string-equal "article" style)
			      "art")
			     ((string-equal "book" style)
			      "bk")
			     ((string-equal "report" style)
			      "rep")
			     ((string-equal "jarticle" style)
			      "jart")
			     ((string-equal "jbook" style)
			      "jbk")
			     ((string-equal "jreport" style)
			      "jrep")
			     ((string-equal "j-article" style)
			      "j-art")
			     ((string-equal "j-book" style)
			      "j-bk")
			     ((string-equal "j-report" style )
			      "j-rep")
			     (t style))
		       (cond ((member "11pt" options)
			      "11")
			     ((member "12pt" options)
			      "12")
			     (t
			      "10")))
		      TeX-auto-file)))

	;; The third argument if "class" indicates LaTeX2e features.
	(cond ((equal class "class")
	       (setq TeX-auto-file (cons "latex2e" TeX-auto-file)))
	      ((equal class "style")
	       (setq TeX-auto-file (cons "latex2" TeX-auto-file)))))))
    
  ;; Cleanup optional arguments
  (mapcar (function (lambda (entry)
		      (setq TeX-auto-symbol
			    (cons (list (nth 0 entry)
					(string-to-int (nth 1 entry)))
				  TeX-auto-symbol))))
	  LaTeX-auto-arguments)

  ;; Cleanup default optional arguments
  (mapcar (function (lambda (entry)
		      (setq TeX-auto-symbol
			    (cons (list (nth 0 entry)
					(vector "argument")
					(1- (string-to-int (nth 1 entry))))
				  TeX-auto-symbol))))
	  LaTeX-auto-optional)

  ;; Cleanup environments arguments
  (mapcar (function (lambda (entry)
		      (setq LaTeX-auto-environment
			    (cons (list (nth 0 entry)
					(string-to-int (nth 1 entry)))
				  LaTeX-auto-environment))))
	  LaTeX-auto-env-args)
    
  ;; Cleanup use of def to add environments
  ;; NOTE: This uses an O(N^2) algorithm, while an O(N log N)
  ;; algorithm is possible.
  (mapcar (function (lambda (symbol)
		      (if (not (TeX-member symbol TeX-auto-symbol 'equal))
			  ;; No matching symbol, insert in list
			  (setq TeX-auto-symbol
				(cons (concat "end" symbol) TeX-auto-symbol))
			;; Matching symbol found, remove from list
			(if (equal (car TeX-auto-symbol) symbol)
			    ;; Is it the first symbol?
			    (setq TeX-auto-symbol (cdr TeX-auto-symbol))
			  ;; Nope!  Travel the list
			  (let ((list TeX-auto-symbol))
			    (while (consp (cdr list))
			      ;; Until we find it.
			      (if (equal (car (cdr list)) symbol)
				  ;; Then remove it.
				  (setcdr list (cdr (cdr list))))
			      (setq list (cdr list)))))
			;; and add the symbol as an environment.
			(setq LaTeX-auto-environment
			      (cons symbol LaTeX-auto-environment)))))
	  LaTeX-auto-end-symbol))

(add-hook 'TeX-auto-cleanup-hook 'LaTeX-auto-cleanup)

(TeX-auto-add-type "label" "LaTeX")
(TeX-auto-add-type "bibitem" "LaTeX")
(TeX-auto-add-type "environment" "LaTeX")
(TeX-auto-add-type "bibliography" "LaTeX" "bibliographies")
(TeX-auto-add-type "index-entry" "LaTeX" "index-entries")

(fset 'LaTeX-add-bibliographies-auto
      (symbol-function 'LaTeX-add-bibliographies))
(defun LaTeX-add-bibliographies (&rest bibliographies)
  "Add BIBLIOGRAPHIES to the list of known bibliographies and style files."
  (apply 'LaTeX-add-bibliographies-auto bibliographies)
  (apply 'TeX-run-style-hooks bibliographies))

(fset 'LaTeX-add-environments-auto
      (symbol-function 'LaTeX-add-environments))
(defun LaTeX-add-environments (&rest environments)
  "Add ENVIRONMENTS to the list of known environments."
  (apply 'LaTeX-add-environments-auto environments)
  (setq LaTeX-menu-changed t))

;;; BibTeX

;;;###autoload
(defun BibTeX-auto-store ()
  "This function should be called from `bibtex-mode-hook'.
It will setup BibTeX to store keys in an auto file."
  ;; We want this to be early in the list, so we do not
  ;; add it before we enter BibTeX mode the first time.
  (if (boundp 'local-write-file-hooks)
      (add-hook 'local-write-file-hooks 'TeX-safe-auto-write)
    (add-hook 'write-file-hooks 'TeX-safe-auto-write))
  (make-local-variable 'TeX-auto-update)
  (setq TeX-auto-update 'BibTeX)
  (make-local-variable 'TeX-auto-untabify)
  (setq TeX-auto-untabify nil)
  (make-local-variable 'TeX-auto-parse-length)
  (setq TeX-auto-parse-length 999999)
  (make-local-variable 'TeX-auto-regexp-list)
  (setq TeX-auto-regexp-list BibTeX-auto-regexp-list)
  (make-local-variable 'TeX-master)
  (setq TeX-master t))

(defvar BibTeX-auto-regexp-list
  '(("@[Ss][Tt][Rr][Ii][Nn][Gg]" 1 ignore)
    ("@[a-zA-Z]+[{(][ \t]*\\([a-zA-Z][^, \n\r\t%\"#'()={}]*\\)"
     1 LaTeX-auto-bibitem))
  "List of regexp-list expressions matching BibTeX items.")

;;; Macro Argument Hooks

(defun TeX-arg-conditional (optional expr then else)
  "Implement if EXPR THEN ELSE.

If EXPR evaluate to true, parse THEN as an argument list, else parse
ELSE as an argument list."
  (TeX-parse-arguments (if (eval expr) then else)))

(defun TeX-arg-free (optional &optional &rest args)
  "Parse its arguments but use no braces when they are inserted."
  (let ((< "")
	(> ""))
    (if (equal (length args) 1)
	(TeX-parse-argument optional (car args))
      (TeX-parse-argument optional args))))

(defun TeX-arg-literal (optional &optional &rest args)
  "Insert its arguments into the buffer.
Used for specifying extra syntax for a macro."
  (apply 'insert args))

(defun TeX-arg-eval (optional &rest args)
  "Evaluate args and insert value in buffer."
  (TeX-argument-insert (eval args) optional))

(defun TeX-arg-label (optional &optional prompt definition)
  "Prompt for a label completing with known labels."
  (let ((label (completing-read (TeX-argument-prompt optional prompt "Key")
				(LaTeX-label-list))))
    (if (and definition (not (string-equal "" label)))
	(LaTeX-add-labels label))
    (TeX-argument-insert label optional optional)))

(defalias 'TeX-arg-ref 'TeX-arg-label)

(defun TeX-arg-index-tag (optional &optional prompt &rest args)
  "Prompt for an index tag.  This is the name of an index, not the entry."
  (let (tag)
    (setq prompt (concat (if optional "(Optional) " "")
			 (if prompt prompt "Index tag")
			 ": (default none) "))
    (setq tag (read-string prompt))
    (TeX-argument-insert tag optional)))

(defun TeX-arg-index (optional &optional prompt &rest args)
  "Prompt for an index entry completing with known entries."
  (let ((entry (completing-read (TeX-argument-prompt optional prompt "Key")
				(LaTeX-index-entry-list))))
    (if (and (not (string-equal "" entry))
	     (not (member (list entry) (LaTeX-index-entry-list))))
	(LaTeX-add-index-entries entry))
    (TeX-argument-insert entry optional optional)))

(defalias 'TeX-arg-define-index 'TeX-arg-index)

(defun TeX-arg-macro (optional &optional prompt definition)
  "Prompt for a TeX macro with completion."
  (let ((macro (completing-read (TeX-argument-prompt optional prompt
						     (concat "Macro: "
							     TeX-esc)
						     t)
				(TeX-symbol-list))))
    (if (and definition (not (string-equal "" macro)))
	(TeX-add-symbols macro))
    (TeX-argument-insert macro optional TeX-esc)))

(defun TeX-arg-environment (optional &optional prompt definition)
  "Prompt for a LaTeX environment with completion."
  (let ((environment (completing-read (TeX-argument-prompt optional prompt
							   "Environment")
				      (TeX-symbol-list))))
    (if (and definition (not (string-equal "" environment)))
	(LaTeX-add-environments environment))

    (TeX-argument-insert environment optional)))

(defun TeX-arg-cite (optional &optional prompt definition)
  "Prompt for a BibTeX citation with completion."
  (setq prompt (concat (if optional "(Optional) " "")
		       (if prompt prompt "Add key")
		       ": (default none) "))
  (let ((items (multi-prompt "," t prompt (LaTeX-bibitem-list))))
    (apply 'LaTeX-add-bibitems items)
    (TeX-argument-insert (mapconcat 'identity items ",") optional optional)))

(defun TeX-arg-counter (optional &optional prompt definition)
  "Prompt for a LaTeX counter."
  ;; Completion not implemented yet.
  (TeX-argument-insert
   (read-string (TeX-argument-prompt optional prompt "Counter"))
   optional))

(defun TeX-arg-savebox (optional &optional prompt definition)
  "Prompt for a LaTeX savebox."
  ;; Completion not implemented yet.
  (TeX-argument-insert
   (read-string (TeX-argument-prompt optional prompt
				     (concat "Savebox: " TeX-esc)
				     t))
   optional TeX-esc))

(defun TeX-arg-file (optional &optional prompt)
  "Prompt for a filename in the current directory."
  (TeX-argument-insert (read-file-name (TeX-argument-prompt optional
							    prompt "File")
				       "" "" nil)
		       optional))

(defun TeX-arg-define-label (optional &optional prompt)
  "Prompt for a label completing with known labels."
  (TeX-arg-label optional prompt t))

(defun TeX-arg-define-macro (optional &optional prompt)
  "Prompt for a TeX macro with completion."
  (TeX-arg-macro optional prompt t))

(defun TeX-arg-define-environment (optional &optional prompt)
  "Prompt for a LaTeX environment with completion."
  (TeX-arg-environment optional prompt t))

(defun TeX-arg-define-cite (optional &optional prompt)
  "Prompt for a BibTeX citation."
  (TeX-arg-cite optional prompt t))

(defun TeX-arg-define-counter (optional &optional prompt)
  "Prompt for a LaTeX counter."
  (TeX-arg-counter optional prompt t))

(defun TeX-arg-define-savebox (optional &optional prompt)
  "Prompt for a LaTeX savebox."
  (TeX-arg-savebox optional prompt t))

(defcustom LaTeX-style-list '(("amsart")
			      ("amsbook")
			      ("article")
			      ("book")
			      ("dinbrief")
			      ("foils")
			      ("letter")
			      ("minimal")
			      ("prosper")
			      ("report")
			      ("scrartcl")
			      ("scrbook")
			      ("scrlttr2")
			      ("scrreprt")
			      ("slides"))
  "List of document styles."
  :group 'LaTeX-environment
  :type '(repeat (group (string :format "%v"))))

(defun TeX-arg-document (optional &optional ignore)
  "Insert arguments to documentstyle and documentclass."
  (let ((style (completing-read
		(concat "Document style: (default " LaTeX-default-style ") ")
		LaTeX-style-list))
	(options (read-string "Options: "
			      (if (stringp LaTeX-default-options)
				  LaTeX-default-options
				(mapconcat 'identity
					   LaTeX-default-options
					   ",")))))
    (if (zerop (length style))
	(setq style LaTeX-default-style))
    (if (not (zerop (length options)))
	(insert LaTeX-optop options LaTeX-optcl))
    (insert TeX-grop style TeX-grcl))

  ;; remove old information
  (TeX-remove-style)

  ;; defined in individual style hooks
  (TeX-update-style))

(defvar TeX-global-input-files nil
  "List of the non-local TeX input files.

Initialized once at the first time you prompt for an input file.
May be reset with `C-u \\[TeX-normal-mode]'.")

(defun TeX-arg-input-file (optionel &optional prompt local)
  "Prompt for a tex or sty file.

First optional argument is the promt, the second is a flag.
If the flag is set, only complete with local files."
  (if (or TeX-global-input-files local)
      ()
    (message "Searching for files...")
    (setq TeX-global-input-files
	  (mapcar 'list (TeX-search-files (append TeX-macro-private
						  TeX-macro-global)
					  TeX-file-extensions t t))))
  (let ((file (if TeX-check-path
		  (completing-read
		   (TeX-argument-prompt optionel prompt "File")
		   (append (mapcar 'list
				   (TeX-search-files '("./")
						     TeX-file-extensions
						     t t))
			   (if local
			       nil
			     TeX-global-input-files)))
		(read-file-name
		 (TeX-argument-prompt optionel prompt "File")))))
    (if (null file)
	(setq file ""))
    (if (not (string-equal "" file))
	(TeX-run-style-hooks file))
    (TeX-argument-insert file optionel)))

(defvar BibTeX-global-style-files nil
  "Association list of BibTeX style files.

Initialized once at the first time you prompt for an input file.
May be reset with `C-u \\[TeX-normal-mode]'.")

(defun TeX-arg-bibstyle (optional &optional prompt)
  "Prompt for a BibTeX style file."
  (message "Searching for BibTeX styles...")
  (or BibTeX-global-style-files
      (setq BibTeX-global-style-files
	    (mapcar 'list
		    (TeX-search-files (append TeX-macro-private
					      TeX-macro-global)
				      BibTeX-style-extensions t t))))

  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "BibTeX style")
		    (append (mapcar 'list
				    (TeX-search-files '("./")
						      BibTeX-style-extensions
						      t t))
			    BibTeX-global-style-files))
   optional))

(defvar BibTeX-global-files nil
  "Association list of BibTeX files.

Initialized once at the first time you prompt for an BibTeX file.
May be reset with `C-u \\[TeX-normal-mode]'.")

(defun TeX-arg-bibliography (optional &optional prompt)
  "Prompt for a BibTeX database file."
  (message "Searching for BibTeX files...")
  (or BibTeX-global-files
      (setq BibTeX-global-files
	    (mapcar 'list (TeX-search-files nil BibTeX-file-extensions t t))))
  
  (let ((styles (multi-prompt
		 "," t
		 (TeX-argument-prompt optional prompt "BibTeX files")
		 (append (mapcar 'list
				 (TeX-search-files '("./")
						   BibTeX-file-extensions
						   t t))
			 BibTeX-global-files))))
    (apply 'LaTeX-add-bibliographies styles)
    (TeX-argument-insert (mapconcat 'identity styles ",") optional)))

(defun TeX-arg-corner (optional &optional prompt)
  "Prompt for a LaTeX side or corner position with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Position")
		    '(("") ("l") ("r") ("t") ("b") ("tl") ("tr") ("bl") ("br"))
		    nil t)
   optional))

(defun TeX-arg-lr (optional &optional prompt)
  "Prompt for a LaTeX side with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Position")
		    '(("") ("l") ("r"))
		    nil t)
   optional))

(defun TeX-arg-tb (optional &optional prompt)
  "Prompt for a LaTeX side with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Position")
		    '(("") ("t") ("b"))
		    nil t)
   optional))

(defun TeX-arg-pagestyle (optional &optional prompt)
  "Prompt for a LaTeX pagestyle with completion."
  (TeX-argument-insert
   (completing-read (TeX-argument-prompt optional prompt "Pagestyle")
		    '(("plain") ("empty") ("headings") ("myheadings")))
   optional))

(defun TeX-arg-verb (optional &optional ignore)
  "Prompt for delimiter and text."
  (let ((del (read-quoted-char "Delimiter: "))
	(text (read-from-minibuffer "Text: ")))
    (insert del text del)))

(defun TeX-arg-pair (optional first second)
  "Insert a pair of number, prompted by FIRST and SECOND.

The numbers are surounded by parenthesizes and separated with a
comma."
  (insert "(" (read-string (concat first  ": ")) ","
	      (read-string (concat second ": ")) ")"))

(defun TeX-arg-size (optional)
  "Insert width and height as a pair."
  (TeX-arg-pair optional "Width" "Height"))

(defun TeX-arg-coordinate (optional)
  "Insert x and y coordinate as a pair."
 (TeX-arg-pair optional "X position" "Y position"))

(defconst TeX-braces-default-association
  '(("[" . "]")
    ("\\{" . "\\}")
    ("(" . ")")
    ("|" . "|")
    ("\\|" . "\\|")
    ("/" . "/")
    ("\\backslash" . "\\backslash")
    ("\\lfloor" . "\\rfloor")
    ("\\lceil" . "\\rceil")
    ("\\langle" . "\\rangle")))

(defcustom TeX-braces-user-association nil
  "A list of your personal association of brace symbols.
These are used for \\left and \\right.

The car of each entry is the brace used with \\left,
the cdr is the brace used with \\right."
  :group 'LaTeX-macro
  :group 'LaTeX-math
  :type '(repeat (cons :format "%v"
		       (string :tag "Left")
		       (string :tag "Right"))))

(defvar TeX-braces-association
  (append TeX-braces-user-association
          TeX-braces-default-association)
    "A list of association of brace symbols for \\left and \\right.
The car of each entry is the brace used with \\left,
the cdr is the brace used with \\right.")

(defvar TeX-left-right-braces
  '(("[") ("]") ("\\{") ("\\}") ("(") (")") ("|") ("\\|")
    ("/") ("\\backslash") ("\\lfloor") ("\\rfloor")
    ("\\lceil") ("\\rceil") ("\\langle") ("\\rangle")
    ("\\uparrow") ("\\Uparrow") ("\\downarrow") ("\\Downarrow")
    ("\\updownarrow") ("\\Updownarrow") ("."))
  "List of symbols which can follow the \\left or \\right command.")

(defun TeX-arg-insert-braces (optional &optional prompt)
  (save-excursion
    (backward-word 1)
    (backward-char)
    (newline-and-indent)
    (beginning-of-line 0)
    (if (looking-at "^[ \t]*$")
	(progn (delete-horizontal-space)
	       (delete-char 1))))
  (let ((left-brace (completing-read
                     (TeX-argument-prompt optional prompt "Which brace")
                     TeX-left-right-braces)))
    (insert left-brace)
    (newline-and-indent)
    (save-excursion
      (let ((right-brace (cdr (assoc left-brace
                                     TeX-braces-association))))
	(newline)
        (insert TeX-esc "right")
        (if (and TeX-arg-right-insert-p
                 right-brace)
            (insert right-brace)
          (insert (completing-read
                   (TeX-argument-prompt optional prompt "Which brace")
                   TeX-left-right-braces)))
	(indent-according-to-mode)))))

;;; Indentation

(defgroup LaTeX-indentation nil
  "Indentation of LaTeX code in AUC TeX"
  :group 'LaTeX
  :group 'TeX-indentation)

(defcustom LaTeX-indent-level 2
  "*Indentation of begin-end blocks in LaTeX."
  :group 'LaTeX-indentation
  :type 'integer)

(defcustom LaTeX-item-indent (- LaTeX-indent-level)
  "*Extra indentation for lines beginning with an item."
  :group 'LaTeX-indentation
  :type 'integer)

(defcustom LaTeX-item-regexp "\\(bib\\)?item\\b"
  "*Regular expression matching macros considered items."
  :group 'LaTeX-indentation
  :type 'regexp)

(defun LaTeX-indent-line ()
  "Indent the line containing point, as LaTeX source.
Add `LaTeX-indent-level' indentation in each \\begin{ - \\end{ block.
Lines starting with an item is given an extra indentation of
`LaTeX-item-indent'."
  (interactive)
  (let* ((case-fold-search nil)
	 (indent (LaTeX-indent-calculate)))
    (save-excursion
      (if (/= (current-indentation) indent)
	  (let ((beg (progn
		       (beginning-of-line)
		       (point))))
	    (back-to-indentation)
	    (delete-region beg (point))
	    (indent-to indent))))
    (if (< (current-column) indent)
	(back-to-indentation))))

(defun LaTeX-fill-region-as-paragraph (from to &optional justify-flag)
  "Fill region as one paragraph.
Break lines to fit `fill-column', but leave all lines ending with \\\\
\(plus its optional argument) alone.  Prefix arg means justify too.
From program, pass args FROM, TO and JUSTIFY-FLAG."
  (interactive "*r\nP")
  (or (assoc (LaTeX-current-environment) LaTeX-indent-environment-list)
      (save-restriction
	(narrow-to-region from to)
	(goto-char from)
	(while (not (eobp))
	  (indent-according-to-mode)
	  (forward-line))
	(goto-char from)
	(while (not (eobp))
	  (if
	      (re-search-forward (concat "^.*"
					 (regexp-quote TeX-esc)
					 (regexp-quote TeX-esc)
					 "\\(\\s-*\\*\\)?"
					 "\\(\\s-*\\[[^]]*\\]\\)?\\s-*$")
				 nil t)
	      (progn
		(goto-char (match-end 0))
		(delete-horizontal-space)
		;; I doubt very much if we want justify -
		;; this is a line with \\
		;; if you think otherwise - uncomment the next line
		;; (and justify-flag (justify-current-line))
		(forward-char)
		;; keep our position in a buffer
		(save-excursion
		  (LaTeX-fill-region-as-para-do
		   from (match-beginning 0) justify-flag))
		(setq from (point)))
	    ;; ELSE part follows - loop termination relies on a fact
	    ;; that (LaTeX-fill-region-as-para-do) moves point past
	    ;; the filled region
	    (LaTeX-fill-region-as-para-do from to justify-flag)))
	;; the following four lines are clearly optional, but I like my
	;; LaTeX code that way
	(goto-char (point-min))
	(while (search-forward "$$ " nil t)
	  (replace-match "$$\n" t t)
	  (indent-according-to-mode)))))

(defun LaTeX-fill-region-as-para-do (from to justify-flag)
  "Fill region as one paragraph: break lines to fit `fill-column'."
  (if (< from to)
      (progn
	;; (save-restriction) here is likely not needed because
	;; it was done by a caller, but I am not sure - mj
	(save-restriction
	  (goto-char from)
	  (skip-chars-forward " \n")
	  (indent-according-to-mode)
	  (beginning-of-line)
	  (narrow-to-region (point) to)
	  (setq from (point))

	  ;; from is now before the text to fill,
	  ;; but after any fill prefix on the first line.

	  ;; Make sure sentences ending at end of line get an extra space.
	  (if (or (not (boundp 'sentence-end-double-space))
		  sentence-end-double-space)
	      (progn
		(goto-char from)
		(while (re-search-forward "[.?!][]})\"']*$" nil t)
		  (insert ? ))))
	  ;; The change all newlines to spaces.
    (subst-char-in-region from (point-max) ?\n ?\ )
    ;; Flush excess spaces, except in the paragraph indentation.
    (goto-char from)
    (skip-chars-forward " \t")
    (while (re-search-forward "   *" nil t)
      (delete-region
       (+ (match-beginning 0)
	  (if (save-excursion
		(skip-chars-backward " ]})\"'")
		(memq (preceding-char) '(?. ?? ?!)))
	      2 1))
       (match-end 0)))
    (goto-char (point-max))
    (delete-horizontal-space)
    (insert "  ")
    (goto-char (point-min))
    (let ((prefixcol 0))
      (while (not (eobp))
	(move-to-column (1+ fill-column))
	(if (eobp)
	    nil
	  (skip-chars-backward "^ \n")
	  (if (if (zerop prefixcol)
		  (bolp)
		(>= prefixcol (current-column)))
	      (skip-chars-forward "^ \n")
	    (forward-char -1)))
	(delete-horizontal-space)
	(if (equal (preceding-char) ?\\)
	    (insert ? ))
	(insert ?\n)
	(indent-according-to-mode)
	(setq prefixcol (current-column))
	(and justify-flag (not (eobp))
	     (progn
	       (forward-line -1)
	       (justify-current-line)
	       (forward-line 1)))
	)
      (goto-char (point-max))
      (delete-horizontal-space))))))

(defun LaTeX-fill-paragraph (prefix)
  "Fill and indent paragraph at or after point.
Prefix arg means justify as well."
  (interactive "*P")
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*%]")
	(re-search-forward "^[ \t]*[^% \t\n]"))
    (forward-paragraph)
    (or (bolp) (newline 1))
    (and (eobp) (open-line 1))
    (let ((end (point-marker))
	  (start (progn
		   (backward-paragraph)
		   (point))))
      (LaTeX-fill-region-as-paragraph start end prefix)))
  ;; Done.
  t)

(defun LaTeX-fill-region (from to &optional justify what)
  "Fill and indent each of the paragraphs in the region as LaTeX text.
Prefix arg (non-nil third arg, if called from program)
means justify as well. Fourth arg WHAT is a word to be displayed when
formatting."
  (interactive "*r\nP")
  (save-restriction
    (save-excursion
      (let ((length (- to from))
	    (to (set-marker (make-marker) to)))
	(goto-char from)
	(beginning-of-line)
	(while (< (point) to)
	  (message "Formatting%s ... %d%%"
		   (if (not what)
		       ""
		     what)
		   (/ (* 100 (- (point) from)) length))
	  (save-excursion (LaTeX-fill-paragraph justify))
	  (forward-paragraph 2)
	  (if (not (eobp))
	      (backward-paragraph)))
	(set-marker to nil)))
    (message "Finished")))

(defun LaTeX-find-matching-end ()
  "Move point to the \\end of the current environment."
  (interactive)
  (let ((regexp (concat (regexp-quote TeX-esc) "\\(begin\\|end\\)\\b"))
	(level 1))
    (save-excursion
      (skip-chars-backward "a-zA-Z \t{")
      (if (bolp)
	  nil
	(backward-char 1)
	(and (looking-at regexp)
	     (char-equal (char-after (1+ (match-beginning 0))) ?e)
	     (setq level 0))))
    (while (and (> level 0) (re-search-forward regexp nil t))
      (if (= (char-after (1+ (match-beginning 0))) ?b);;begin
	  (setq level (1+ level))
	(setq level (1- level))))
    (if (= level 0)
	(search-forward "}")
      (error "Can't locate end of current environment"))))

(defun LaTeX-find-matching-begin ()
  "Move point to the \\begin of the current environment."
  (interactive)
  (let ((regexp (concat (regexp-quote TeX-esc) "\\(begin\\|end\\)\\b"))
	(level 1))
    (skip-chars-backward "a-zA-Z \t{")
    (if (bolp)
	nil
      (backward-char 1)
      (and (looking-at regexp)
	   (char-equal (char-after (1+ (match-beginning 0))) ?b)
	   (setq level 0)))
    (while (and (> level 0) (re-search-backward regexp nil t))
      (if (= (char-after (1+ (match-beginning 0))) ?e);;end
	  (setq level (1+ level))
	(setq level (1- level))))
    (or (= level 0)
	(error "Can't locate beginning of current environment"))))

(defun LaTeX-mark-environment ()
  "Set mark to end of current environment and point to the matching begin
will not work properly if there are unbalanced begin-end pairs in
comments and verbatim environments"
  (interactive)
  (let ((cur (point)))
    (LaTeX-find-matching-end)
    (beginning-of-line 2)
    (set-mark (point))
    (goto-char cur)
    (LaTeX-find-matching-begin)
    (TeX-activate-region)))

(defun LaTeX-fill-environment (justify)
  "Fill and indent current environment as LaTeX text."
  (interactive "*P")
  (save-excursion
    (LaTeX-mark-environment)
    (re-search-forward "{\\([^}]+\\)}")
    (LaTeX-fill-region
     (region-beginning)
     (region-end)
     justify
     (concat " environment " (TeX-match-buffer 1)))))

(defun LaTeX-fill-section (justify)
  "Fill and indent current logical section as LaTeX text."
  (interactive "*P")
  (save-excursion
    (LaTeX-mark-section)
    (re-search-forward "{\\([^}]+\\)}")
    (LaTeX-fill-region
     (region-beginning)
     (region-end)
     justify
     (concat " section " (TeX-match-buffer 1)))))

(defun LaTeX-mark-section ()
  "Set mark at end of current logical section, and point at top."
  (interactive)
  (re-search-forward (concat  "\\(" (LaTeX-outline-regexp)
			      "\\|\\'\\)"))
  (re-search-backward "^")
  (set-mark (point))
  (re-search-backward (concat "\\(" (LaTeX-outline-regexp)
			      "\\|\\`\\)"))
  (TeX-activate-region))

(defun LaTeX-fill-buffer (justify)
  "Fill and indent current buffer as LaTeX text."
  (interactive "*P")
  (save-excursion
    (LaTeX-fill-region
     (point-min)
     (point-max)
     justify
     (concat " buffer " (buffer-name)))))

(defcustom LaTeX-indent-environment-list
  '(("verbatim" current-indentation)
    ("verbatim*" current-indentation)
    ;; The following should have there own, smart indentation function.
    ;; Some other day.
    ("alltt")
    ("array")
    ("displaymath")
    ("eqnarray")
    ("eqnarray*")
    ("equation")
    ("equation*")
    ("picture")
    ("tabbing")
    ("table")
    ("table*")
    ("tabular")
    ("tabular*"))
    "Alist of environments with special indentation.
The second element in each entry is the function to calculate the
indentation level in columns."
    :group 'LaTeX-indentation
    :type '(repeat (list (string :tag "Environment")
			 (option function))))

(defcustom LaTeX-indent-environment-check t
  "*If non-nil, check for any special environments."
  :group 'LaTeX-indentation
  :type 'boolean)

(defcustom LaTeX-left-comment-regexp "%%%"
  "*Regexp matching comments that should be placed on the left margin."
  :group 'LaTeX-indentation
  :type 'regexp)

(defcustom LaTeX-right-comment-regexp "%[^%]"
  "*Regexp matching comments that should be placed to the right margin."
  :group 'LaTeX-indentation
  :type 'regexp)

(defcustom LaTeX-ignore-comment-regexp nil
  "*Regexp matching comments whose indentation should not be touched."
  :group 'LaTeX-indentation
  :type '(choice (const :tag "none" nil)
		 (regexp :format "%v")))

(defcustom LaTeX-document-regexp "document"
  "Regexp matching environments in which the indentation starts at col 0."
  :group 'LaTeX-indentation
  :type 'regexp)

(defcustom LaTeX-verbatim-regexp "verbatim\\*?"
  "*Regexp matching environments with indentation at col 0 for begin/end."
  :group 'LaTeX-indentation
  :type 'regexp)

(defcustom LaTeX-begin-regexp "begin\\b"
  "*Regexp matching macros considered begins."
  :group 'LaTeX-indentation
  :type 'regexp)

(defcustom LaTeX-end-regexp "end\\b"
  "*Regexp matching macros considered ends."
  :group 'LaTeX-indentation
  :type 'regexp)

(defun LaTeX-indent-calculate ()
  "Return the correct indentation of line of LaTeX source. (I hope...)"
  (save-excursion
    (back-to-indentation)
    (cond ((looking-at (concat (regexp-quote TeX-esc)
			       "\\(begin\\|end\\){\\("
			       LaTeX-verbatim-regexp
			       "\\)}"))
	   ;; \end{verbatim} must be flush left, otherwise an unwanted
	   ;; empty line appears in LaTeX's output.
	   0)
	  ((and LaTeX-left-comment-regexp
		(looking-at LaTeX-left-comment-regexp))
	   ;; Comments to the left margin.
	   0)
	  ((and LaTeX-right-comment-regexp
                (looking-at LaTeX-right-comment-regexp))
           ;; Comments to the right margin.
	   comment-column)
	  ((and LaTeX-ignore-comment-regexp
                (looking-at LaTeX-ignore-comment-regexp))
           ;; Comments best left alone.
	   (current-indentation))
	  ((and LaTeX-indent-environment-check
		;; Special environments.
		(let ((entry (assoc (LaTeX-current-environment)
				    LaTeX-indent-environment-list)))
		  (and entry
		       (nth 1 entry)
		       (funcall (nth 1 entry))))))
	  ((looking-at (concat (regexp-quote TeX-esc)
			       "\\("
			       LaTeX-end-regexp
			       "\\)"))
	   ;; Backindent at \end.
	   (- (LaTeX-indent-calculate-last) LaTeX-indent-level))
	  ((looking-at (concat (regexp-quote TeX-esc) "right\\b"))
	   ;; Backindent at \right.
	   (- (LaTeX-indent-calculate-last) LaTeX-left-right-indent-level))
	  ((looking-at (concat (regexp-quote TeX-esc)
			       "\\("
			       LaTeX-item-regexp
			       "\\)"))
	   ;; Items.
	   (+ (LaTeX-indent-calculate-last) LaTeX-item-indent))
	  ((looking-at "}")
	   ;; End brace in the start of the line.
	   (- (LaTeX-indent-calculate-last) TeX-brace-indent-level))
	  (t (LaTeX-indent-calculate-last)))))

(defcustom LaTeX-left-right-indent-level LaTeX-indent-level
  "*The level of indentation produced by a \\left macro."
  :group 'LaTeX-indentation
  :type 'integer)

(defcustom LaTeX-indent-comment-start-regexp "%"
  "*Regexp matching comments ending the indent level count.
This means, we just count the LaTeX tokens \\left, \\right, \\begin,
and \\end up to the first occurence of text matching this regexp.
Thus, the default \"%\" stops counting the tokens at a comment.  A
value of \"%[^>]\" would allow you to alter the indentation with
comments, e.g. with comment `%> \\begin'.
Lines which start with `%' are not considered at all, regardless if this
value."
  :group 'LaTeX-indentation
  :type 'regexp)

(defun LaTeX-indent-level-count ()
  "Count indentation change caused by all \\left, \\right, \\begin, and
\\end commands in the current line."
  (save-excursion
    (save-restriction
      (let ((count 0))
	(narrow-to-region (point)
			  (save-excursion
			    (re-search-forward
			     (concat "[^"
				     (regexp-quote TeX-esc)
				     "]\\("
				     LaTeX-indent-comment-start-regexp
				     "\\)\\|\n\\|\\'"))
			    (backward-char)
			    (point)))
	(while (search-forward TeX-esc nil t)
	  (cond
	   ((looking-at "left\\b")
	    (setq count (+ count LaTeX-left-right-indent-level)))
	   ((looking-at "right\\b")
	    (setq count (- count LaTeX-left-right-indent-level)))
	   ((looking-at LaTeX-begin-regexp)
	    (setq count (+ count LaTeX-indent-level)))
	   ((looking-at LaTeX-end-regexp)
	    (setq count (- count LaTeX-indent-level)))
	   ((looking-at (regexp-quote TeX-esc))
	    (forward-char 1))))
	count))))

(defun LaTeX-indent-calculate-last ()
  "Return the correct indentation of a normal line of text.
The point is supposed to be at the beginning of the current line."
  (save-restriction
    (widen)
    (skip-chars-backward "\n\t ")
    (move-to-column (current-indentation))

    ;; Ignore comments.
    (while (and (looking-at (regexp-quote comment-start)) (not (bobp)))
      (skip-chars-backward "\n\t ")
      (if (not (bobp))
	  (move-to-column (current-indentation))))

    (cond ((bobp) 0)
	  ((looking-at (concat (regexp-quote TeX-esc)
			       "begin *{\\("
			       LaTeX-document-regexp
			       "\\)}"))
	   ;; I dislike having all of the document indented...
	   (current-indentation))
	  ((looking-at (concat (regexp-quote TeX-esc)
			       "begin *{\\("
			       LaTeX-verbatim-regexp
			       "\\)}"))
	   0)
	  ((looking-at (concat (regexp-quote TeX-esc)
			       "end *{\\("
			       LaTeX-verbatim-regexp
			       "\\)}"))
	   ;; If I see an \end{verbatim} in the previous line I skip
	   ;; back to the preceding \begin{verbatim}.
	   (save-excursion
	     (if (re-search-backward (concat (regexp-quote TeX-esc)
					     "begin *{\\("
					     LaTeX-verbatim-regexp
					     "\\)}") 0 t)
		 (LaTeX-indent-calculate-last)
	       0)))
	  (t (+ (current-indentation)
		(TeX-brace-count-line)
		(LaTeX-indent-level-count)
		(cond ((looking-at (concat (regexp-quote TeX-esc)
					   "\\("
					   LaTeX-end-regexp
					   "\\)"))
		       LaTeX-indent-level)
		      ((looking-at (concat (regexp-quote TeX-esc) "right\\b"))
		       LaTeX-left-right-indent-level)
		      ((looking-at (concat (regexp-quote TeX-esc)
					   "\\("
					   LaTeX-item-regexp
					   "\\)"))
		       (- LaTeX-item-indent))
		      ((looking-at "}")
		       TeX-brace-indent-level)
		      (t 0)))))))

;;; Math Minor Mode

(defgroup LaTeX-math nil
  "Mathematics in AUC TeX."
  :group 'LaTeX-macro)

(defcustom LaTeX-math-list nil
  "AList of your personal LaTeX math symbols.

Each entry should be a list with three elements, KEY, VALUE, and MENU.
KEY is the key to be redefined (under `LaTeX-math-abbrev-prefix' in
math minor mode, VALUE can be a string with the name of the macro to
be inserted, or a function to be called.  The optional third element is
the name of the submenu where the command should be added.

See also `LaTeX-math-menu'."
  :group 'LaTeX-math
  :type '(repeat (group (choice :tag "Key"
				(const :tag "none")
				(character :format "%v\n"))
			(string :tag "Value")
			(choice :tag "Menu"
				(string :tag "Name" :format "%v")
				(repeat :tag "Path"
					(string :format "%v"))))))

(defconst LaTeX-math-default
  '((?a "alpha" "greek")
    (?b "beta" "greek")
    (?c LaTeX-math-cal "Cal-whatever")
    (?d "delta" "greek")
    (?e "epsilon" "greek")
    (?f "phi" "greek")
    (?g "gamma" "greek")
    (?h  "eta" "greek")
    (?k "kappa" "greek")
    (?l "lambda" "greek")
    (?m "mu" "greek")
    (?N "nabla" "greek")
    (?n "nu" "greek")
    (?o "omega" "greek")
    (?p "pi" "greek")
    (?q "theta" "greek")
    (?r "rho" "greek")
    (?s "sigma" "greek")
    (?t "tau" "greek")
    (?u "upsilon" "greek")
    (?x "chi" "greek")
    (?y "psi" "greek")
    (?z "zeta" "greek")
    (?D "Delta" "Greek")
    (?F "Phi" "Greek")
    (?G "Gamma" "Greek")
    (?Q "Theta" "Greek")
    (?L "Lambda" "Greek")
    (?Y "Psi" "Greek")
    (?P "Pi" "Greek")
    (?S "Sigma" "Greek")
    (?U "Upsilon" "Greek")
    (?O "Omega" "Greek")
    (nil "pm" "Binary Op")
    (nil "mp" "Binary Op")
    (?* "times" "Binary Op")
    (nil "div" "Binary Op")
    (nil "ast" "Binary Op")
    (nil "star" "Binary Op")
    (nil "circ" "Binary Op")
    (nil "bullet" "Binary Op")
    (?. "cdot" "Binary Op")
    (?- "cap" "Binary Op")
    (?+ "cup" "Binary Op")
    (nil "uplus" "Binary Op")
    (nil "sqcap" "Binary Op")
    (?| "vee" "Binary Op")
    (?& "wedge" "Binary Op")
    (?\\ "setminus" "Binary Op")
    (nil "wr" "Binary Op")
    (nil "diamond" "Binary Op")
    (nil "bigtriangleup" "Binary Op")
    (nil "bigtriangledown" "Binary Op")
    (nil "triangleleft" "Binary Op")
    (nil "triangleright" "Binary Op")
    (nil "lhd" "Binary Op")
    (nil "rhd" "Binary Op")
    (nil "unlhd" "Binary Op")
    (nil "unrhd" "Binary Op")
    (nil "oplus" "Binary Op")
    (nil "ominus" "Binary Op")
    (nil "otimes" "Binary Op")
    (nil "oslash" "Binary Op")
    (nil "odot" "Binary Op")
    (nil "bigcirc" "Binary Op")
    (nil "dagger" "Binary Op")
    (nil "ddagger" "Binary Op")
    (nil "amalg" "Binary Op")
    (?< "leq" "Relational")
    (?> "geq" "Relational")
    (nil "qed" "Relational")
    (nil "equiv" "Relational")
    (nil "models" "Relational")
    (nil "prec" "Relational")
    (nil "succ" "Relational")
    (nil "sim" "Relational")
    (nil "perp" "Relational")
    (nil "preceq" "Relational")
    (nil "succeq" "Relational")
    (nil "simeq" "Relational")
    (nil "mid" "Relational")
    (nil "ll" "Relational")
    (nil "gg" "Relational")
    (nil "asymp" "Relational")
    (nil "parallel" "Relational")
    (?{ "subset" "Relational")
    (?} "supset" "Relational")
    (nil "approx" "Relational")
    (nil "bowtie" "Relational")
    (?\[ "subseteq" "Relational")
    (?\] "supseteq" "Relational")
    (nil "cong" "Relational")
    (nil "Join" "Relational")
    (nil "sqsubset" "Relational")
    (nil "sqsupset" "Relational")
    (nil "neq" "Relational")
    (nil "smile" "Relational")
    (nil "sqsubseteq" "Relational")
    (nil "sqsupseteq" "Relational")
    (nil "doteq" "Relational")
    (nil "frown" "Relational")
    (?i "in" "Relational")
    (nil "ni" "Relational")
    (nil "propto" "Relational")
    (nil "vdash" "Relational")
    (nil "dashv" "Relational")
    (?\C-b "leftarrow" "Arrows")
    (nil "Leftarrow" "Arrows")
    (?\C-f "rightarrow" "Arrows")
    (nil "Rightarrow" "Arrows")
    (nil "leftrightarrow" "Arrows")
    (nil "Leftrightarrow" "Arrows")
    (nil "mapsto" "Arrows")
    (nil "hookleftarrow" "Arrows")
    (nil "leftharpoonup" "Arrows")
    (nil "leftharpoondown" "Arrows")
    (nil "longleftarrow" "Arrows")
    (nil "Longleftarrow" "Arrows")
    (nil "longrightarrow" "Arrows")
    (nil "Longrightarrow" "Arrows")
    (nil "longleftrightarrow" "Arrows")
    (nil "Longleftrightarrow" "Arrows")
    (nil "longmapsto" "Arrows")
    (nil "hookrightarrow" "Arrows")
    (nil "rightharpoonup" "Arrows")
    (nil "rightharpoondown" "Arrows")
    (?\C-p "uparrow" "Arrows")
    (nil "Uparrow" "Arrows")
    (?\C-n "downarrow" "Arrows")
    (nil "Downarrow" "Arrows")
    (nil "updownarrow" "Arrows")
    (nil "Updownarrow" "Arrows")
    (nil "nearrow" "Arrows")
    (nil "searrow" "Arrows")
    (nil "swarrow" "Arrows")
    (nil "nwarrow" "Arrows")
    (nil "ldots" "Misc Symbol")
    (nil "cdots" "Misc Symbol")
    (nil "vdots" "Misc Symbol")
    (nil "ddots" "Misc Symbol")
    (nil "aleph" "Misc Symbol")
    (nil "prime" "Misc Symbol")
    (?A "forall" "Misc Symbol")
    (?I "infty" "Misc Symbol")
    (nil "hbar" "Misc Symbol")
    (?0 "emptyset" "Misc Symbol")
    (?E "exists" "Misc Symbol")
    (nil "nabla" "Misc Symbol")
    (nil "surd" "Misc Symbol")
    (nil "Box" "Misc Symbol")
    (nil "triangle" "Misc Symbol")
    (nil "Diamond" "Misc Symbol")
    (nil "imath" "Misc Symbol")
    (nil "jmath" "Misc Symbol")
    (nil "ell" "Misc Symbol")
    (nil "neg" "Misc Symbol")
    (?/ "not" "Misc Symbol")
    (nil "top" "Misc Symbol")
    (nil "flat" "Misc Symbol")
    (nil "natural" "Misc Symbol")
    (nil "sharp" "Misc Symbol")
    (nil "wp" "Misc Symbol")
    (nil "bot" "Misc Symbol")
    (nil "clubsuit" "Misc Symbol")
    (nil "diamondsuit" "Misc Symbol")
    (nil "heartsuit" "Misc Symbol")
    (nil "spadesuit" "Misc Symbol")
    (nil "mho" "Misc Symbol")
    (nil "Re" "Misc Symbol")
    (nil "Im" "Misc Symbol")
    (nil "angle" "Misc Symbol")
    (nil "partial" "Misc Symbol")
    (nil "sum" "Var Symbol")
    (nil "prod" "Var Symbol")
    (nil "coprod" "Var Symbol")
    (nil "int" "Var Symbol")
    (nil "oint" "Var Symbol")
    (nil "bigcap" "Var Symbol")
    (nil "bigcup" "Var Symbol")
    (nil "bigsqcup" "Var Symbol")
    (nil "bigvee" "Var Symbol")
    (nil "bigwedge" "Var Symbol")
    (nil "bigodot" "Var Symbol")
    (nil "bigotimes" "Var Symbol")
    (nil "bigoplus" "Var Symbol")
    (nil "biguplus" "Var Symbol")
    (nil "arccos" "Log-like")
    (nil "arcsin" "Log-like")
    (nil "arctan" "Log-like")
    (nil "arg" "Log-like")
    (?\C-c "cos" "Log-like")
    (nil "cosh" "Log-like")
    (nil "cot" "Log-like")
    (nil "coth" "Log-like")
    (nil "csc" "Log-like")
    (nil "deg" "Log-like")
    (?\C-d "det" "Log-like")
    (nil "dim" "Log-like")
    (?\C-e "exp" "Log-like")
    (nil "gcd" "Log-like")
    (nil "hom" "Log-like")
    (?\C-_ "inf" "Log-like")
    (nil "ker" "Log-like")
    (nil "lg" "Log-like")
    (?\C-l "lim" "Log-like")
    (nil "liminf" "Log-like")
    (nil "limsup" "Log-like")
    (nil "ln" "Log-like")
    (nil "log" "Log-like")
    (nil "max" "Log-like")
    (nil "min" "Log-like")
    (nil "Pr" "Log-like")
    (nil "sec" "Log-like")
    (?\C-s "sin" "Log-like")
    (nil "sinh" "Log-like")
    (?\C-^ "sup" "Log-like")
    (?\C-t "tan" "Log-like")
    (nil "tanh" "Log-like")
    (nil "uparrow" "delimiters")
    (nil "Uparrow" "delimiters")
    (nil "downarrow" "delimiters")
    (nil "Downarrow" "delimiters")
    (nil "{" "delimiters")
    (nil "}" "delimiters")
    (nil "updownarrow" "delimiters")
    (nil "Updownarrow" "delimiters")
    (nil "lfloor" "delimiters")
    (nil "rfloor" "delimiters")
    (nil "lceil" "delimiters")
    (nil "rceil" "delimiters")
    (?\( "langle" "delimiters")
    (?\) "rangle" "delimiters")
    (nil "backslash" "delimiters")
    (nil "|" "delimiters")
    (nil "rmoustache" "Delimiters")
    (nil "lmoustache" "Delimiters")
    (nil "rgroup" "Delimiters")
    (nil "lgroup" "Delimiters")
    (nil "arrowvert" "Delimiters")
    (nil "Arrowvert" "Delimiters")
    (nil "bracevert" "Delimiters")
    (nil "widetilde" "Constructs")
    (nil "widehat" "Constructs")
    (nil "overleftarrow" "Constructs")
    (nil "overrightarrow" "Constructs")
    (nil "overline" "Constructs")
    (nil "underline" "Constructs")
    (nil "overbrace" "Constructs")
    (nil "underbrace" "Constructs")
    (nil "sqrt" "Constructs")
    (nil "frac" "Constructs")
    (?^ "hat" "Accents")
    (nil "acute" "Accents")
    (nil "bar" "Accents")
    (nil "dot" "Accents")
    (nil "breve" "Accents")
    (nil "check" "Accents")
    (nil "grave" "Accents")
    (nil "vec" "Accents")
    (nil "ddot" "Accents")
    (?~ "tilde" "Accents")
    (nil "digamma" ("AMS" "Hebrew"))
    (nil "varkappa" ("AMS" "Hebrew"))
    (nil "beth" ("AMS" "Hebrew"))
    (nil "daleth" ("AMS" "Hebrew"))
    (nil "gimel" ("AMS" "Hebrew"))
    (nil "dashrightarrow" ("AMS" "Arrows"))
    (nil "dashleftarrow" ("AMS" "Arrows"))
    (nil "leftleftarrows" ("AMS" "Arrows"))
    (nil "leftrightarrows" ("AMS" "Arrows"))
    (nil "Lleftarrow" ("AMS" "Arrows"))
    (nil "twoheadleftarrow" ("AMS" "Arrows"))
    (nil "leftarrowtail" ("AMS" "Arrows"))
    (nil "looparrowleft" ("AMS" "Arrows"))
    (nil "leftrightharpoons" ("AMS" "Arrows"))
    (nil "curvearrowleft" ("AMS" "Arrows"))
    (nil "circlearrowleft" ("AMS" "Arrows"))
    (nil "Lsh" ("AMS" "Arrows"))
    (nil "upuparrows" ("AMS" "Arrows"))
    (nil "upharpoonleft" ("AMS" "Arrows"))
    (nil "downharpoonleft" ("AMS" "Arrows"))
    (nil "multimap" ("AMS" "Arrows"))
    (nil "leftrightsquigarrow" ("AMS" "Arrows"))
    (nil "looparrowright" ("AMS" "Arrows"))
    (nil "rightleftharpoons" ("AMS" "Arrows"))
    (nil "curvearrowright" ("AMS" "Arrows"))
    (nil "circlearrowright" ("AMS" "Arrows"))
    (nil "Rsh" ("AMS" "Arrows"))
    (nil "downdownarrows" ("AMS" "Arrows"))
    (nil "upharpoonright" ("AMS" "Arrows"))
    (nil "downharpoonright" ("AMS" "Arrows"))
    (nil "rightsquigarrow" ("AMS" "Arrows"))
    (nil "nleftarrow" ("AMS" "Neg Arrows"))
    (nil "nrightarrow" ("AMS" "Neg Arrows"))
    (nil "nLeftarrow" ("AMS" "Neg Arrows"))
    (nil "nRightarrow" ("AMS" "Neg Arrows"))
    (nil "nleftrightarrow" ("AMS" "Neg Arrows"))
    (nil "nLeftrightarrow" ("AMS" "Neg Arrows"))
    (nil "leqq" ("AMS" "Relational I"))
    (nil "leqslant" ("AMS" "Relational I"))
    (nil "eqslantless" ("AMS" "Relational I"))
    (nil "lesssim" ("AMS" "Relational I"))
    (nil "lessapprox" ("AMS" "Relational I"))
    (nil "approxeq" ("AMS" "Relational I"))
    (nil "lessdot" ("AMS" "Relational I"))
    (nil "lll" ("AMS" "Relational I"))
    (nil "lessgtr" ("AMS" "Relational I"))
    (nil "lesseqgtr" ("AMS" "Relational I"))
    (nil "lesseqqgtr" ("AMS" "Relational I"))
    (nil "doteqdot" ("AMS" "Relational I"))
    (nil "risingdotseq" ("AMS" "Relational I"))
    (nil "fallingdotseq" ("AMS" "Relational I"))
    (nil "backsim" ("AMS" "Relational I"))
    (nil "backsimeq" ("AMS" "Relational I"))
    (nil "subseteqq" ("AMS" "Relational I"))
    (nil "Subset" ("AMS" "Relational I"))
    (nil "sqsubset" ("AMS" "Relational I"))
    (nil "preccurlyeq" ("AMS" "Relational I"))
    (nil "curlyeqprec" ("AMS" "Relational I"))
    (nil "precsim" ("AMS" "Relational I"))
    (nil "precapprox" ("AMS" "Relational I"))
    (nil "vartriangleleft" ("AMS" "Relational I"))
    (nil "trianglelefteq" ("AMS" "Relational I"))
    (nil "vDash" ("AMS" "Relational I"))
    (nil "Vvdash" ("AMS" "Relational I"))
    (nil "smallsmile" ("AMS" "Relational I"))
    (nil "smallfrown" ("AMS" "Relational I"))
    (nil "bumpeq" ("AMS" "Relational I"))
    (nil "Bumpeq" ("AMS" "Relational I"))
    (nil "geqq" ("AMS" "Relational II"))
    (nil "geqslant" ("AMS" "Relational II"))
    (nil "eqslantgtr" ("AMS" "Relational II"))
    (nil "gtrsim" ("AMS" "Relational II"))
    (nil "gtrapprox" ("AMS" "Relational II"))
    (nil "gtrdot" ("AMS" "Relational II"))
    (nil "ggg" ("AMS" "Relational II"))
    (nil "gtrless" ("AMS" "Relational II"))
    (nil "gtreqless" ("AMS" "Relational II"))
    (nil "gtreqqless" ("AMS" "Relational II"))
    (nil "eqcirc" ("AMS" "Relational II"))
    (nil "circeq" ("AMS" "Relational II"))
    (nil "triangleq" ("AMS" "Relational II"))
    (nil "thicksim" ("AMS" "Relational II"))
    (nil "thickapprox" ("AMS" "Relational II"))
    (nil "supseteqq" ("AMS" "Relational II"))
    (nil "Supset" ("AMS" "Relational II"))
    (nil "sqsupset" ("AMS" "Relational II"))
    (nil "succcurlyeq" ("AMS" "Relational II"))
    (nil "curlyeqsucc" ("AMS" "Relational II"))
    (nil "succsim" ("AMS" "Relational II"))
    (nil "succapprox" ("AMS" "Relational II"))
    (nil "vartriangleright" ("AMS" "Relational II"))
    (nil "trianglerighteq" ("AMS" "Relational II"))
    (nil "Vdash" ("AMS" "Relational II"))
    (nil "shortmid" ("AMS" "Relational II"))
    (nil "shortparallel" ("AMS" "Relational II"))
    (nil "between" ("AMS" "Relational II"))
    (nil "pitchfork" ("AMS" "Relational II"))
    (nil "varpropto" ("AMS" "Relational II"))
    (nil "blacktriangleleft" ("AMS" "Relational II"))
    (nil "therefore" ("AMS" "Relational II"))
    (nil "backepsilon" ("AMS" "Relational II"))
    (nil "blacktriangleright" ("AMS" "Relational II"))
    (nil "because" ("AMS" "Relational II"))
    (nil "nless" ("AMS" "Neg Rel I"))
    (nil "nleq" ("AMS" "Neg Rel I"))
    (nil "nleqslant" ("AMS" "Neg Rel I"))
    (nil "nleqq" ("AMS" "Neg Rel I"))
    (nil "lneq" ("AMS" "Neg Rel I"))
    (nil "lneqq" ("AMS" "Neg Rel I"))
    (nil "lvertneqq" ("AMS" "Neg Rel I"))
    (nil "lnsim" ("AMS" "Neg Rel I"))
    (nil "lnapprox" ("AMS" "Neg Rel I"))
    (nil "nprec" ("AMS" "Neg Rel I"))
    (nil "npreceq" ("AMS" "Neg Rel I"))
    (nil "precnsim" ("AMS" "Neg Rel I"))
    (nil "precnapprox" ("AMS" "Neg Rel I"))
    (nil "nsim" ("AMS" "Neg Rel I"))
    (nil "nshortmid" ("AMS" "Neg Rel I"))
    (nil "nmid" ("AMS" "Neg Rel I"))
    (nil "nvdash" ("AMS" "Neg Rel I"))
    (nil "nvDash" ("AMS" "Neg Rel I"))
    (nil "ntriangleleft" ("AMS" "Neg Rel I"))
    (nil "ntrianglelefteq" ("AMS" "Neg Rel I"))
    (nil "nsubseteq" ("AMS" "Neg Rel I"))
    (nil "subsetneq" ("AMS" "Neg Rel I"))
    (nil "varsubsetneq" ("AMS" "Neg Rel I"))
    (nil "subsetneqq" ("AMS" "Neg Rel I"))
    (nil "varsubsetneqq" ("AMS" "Neg Rel I"))
    (nil "ngtr" ("AMS" "Neg Rel II"))
    (nil "ngeq" ("AMS" "Neg Rel II"))
    (nil "ngeqslant" ("AMS" "Neg Rel II"))
    (nil "ngeqq" ("AMS" "Neg Rel II"))
    (nil "gneq" ("AMS" "Neg Rel II"))
    (nil "gneqq" ("AMS" "Neg Rel II"))
    (nil "gvertneqq" ("AMS" "Neg Rel II"))
    (nil "gnsim" ("AMS" "Neg Rel II"))
    (nil "gnapprox" ("AMS" "Neg Rel II"))
    (nil "nsucc" ("AMS" "Neg Rel II"))
    (nil "nsucceq" ("AMS" "Neg Rel II"))
    (nil "succnsim" ("AMS" "Neg Rel II"))
    (nil "succnapprox" ("AMS" "Neg Rel II"))
    (nil "ncong" ("AMS" "Neg Rel II"))
    (nil "nshortparallel" ("AMS" "Neg Rel II"))
    (nil "nparallel" ("AMS" "Neg Rel II"))
    (nil "nvDash" ("AMS" "Neg Rel II"))
    (nil "nVDash" ("AMS" "Neg Rel II"))
    (nil "ntriangleright" ("AMS" "Neg Rel II"))
    (nil "ntrianglerighteq" ("AMS" "Neg Rel II"))
    (nil "nsupseteq" ("AMS" "Neg Rel II"))
    (nil "nsupseteqq" ("AMS" "Neg Rel II"))
    (nil "supsetneq" ("AMS" "Neg Rel II"))
    (nil "varsupsetneq" ("AMS" "Neg Rel II"))
    (nil "supsetneqq" ("AMS" "Neg Rel II"))
    (nil "varsupsetneqq" ("AMS" "Neg Rel II"))
    (nil "dotplus" ("AMS" "Binary Op"))
    (nil "smallsetminus" ("AMS" "Binary Op"))
    (nil "Cap" ("AMS" "Binary Op"))
    (nil "Cup" ("AMS" "Binary Op"))
    (nil "barwedge" ("AMS" "Binary Op"))
    (nil "veebar" ("AMS" "Binary Op"))
    (nil "doublebarwedge" ("AMS" "Binary Op"))
    (nil "boxminus" ("AMS" "Binary Op"))
    (nil "boxtimes" ("AMS" "Binary Op"))
    (nil "boxdot" ("AMS" "Binary Op"))
    (nil "boxplus" ("AMS" "Binary Op"))
    (nil "divideontimes" ("AMS" "Binary Op"))
    (nil "ltimes" ("AMS" "Binary Op"))
    (nil "rtimes" ("AMS" "Binary Op"))
    (nil "leftthreetimes" ("AMS" "Binary Op"))
    (nil "rightthreetimes" ("AMS" "Binary Op"))
    (nil "curlywedge" ("AMS" "Binary Op"))
    (nil "curlyvee" ("AMS" "Binary Op"))
    (nil "circleddash" ("AMS" "Binary Op"))
    (nil "circledast" ("AMS" "Binary Op"))
    (nil "circledcirc" ("AMS" "Binary Op"))
    (nil "centerdot" ("AMS" "Binary Op"))
    (nil "intercal" ("AMS" "Binary Op"))
    (nil "hbar" ("AMS" "Misc"))
    (nil "hslash" ("AMS" "Misc"))
    (nil "vartriangle" ("AMS" "Misc"))
    (nil "triangledown" ("AMS" "Misc"))
    (nil "square" ("AMS" "Misc"))
    (nil "lozenge" ("AMS" "Misc"))
    (nil "circledS" ("AMS" "Misc"))
    (nil "angle" ("AMS" "Misc"))
    (nil "measuredangle" ("AMS" "Misc"))
    (nil "nexists" ("AMS" "Misc"))
    (nil "mho" ("AMS" "Misc"))
    (nil "Finv" ("AMS" "Misc"))
    (nil "Game" ("AMS" "Misc"))
    (nil "Bbbk" ("AMS" "Misc"))
    (nil "backprime" ("AMS" "Misc"))
    (nil "varnothing" ("AMS" "Misc"))
    (nil "blacktriangle" ("AMS" "Misc"))
    (nil "blacktriangledown" ("AMS" "Misc"))
    (nil "blacksquare" ("AMS" "Misc"))
    (nil "blacklozenge" ("AMS" "Misc"))
    (nil "bigstar" ("AMS" "Misc"))
    (nil "sphericalangle" ("AMS" "Misc"))
    (nil "complement" ("AMS" "Misc"))
    (nil "eth" ("AMS" "Misc"))
    (nil "diagup" ("AMS" "Misc"))
    (nil "diagdown" ("AMS" "Misc"))
    (nil "Hat" ("AMS" "Accents"))
    (nil "Check" ("AMS" "Accents"))
    (nil "Tilde" ("AMS" "Accents"))
    (nil "Acute" ("AMS" "Accents"))
    (nil "Grave" ("AMS" "Accents"))
    (nil "Dot" ("AMS" "Accents"))
    (nil "Ddot" ("AMS" "Accents"))
    (nil "Breve" ("AMS" "Accents"))
    (nil "Bar" ("AMS" "Accents"))
    (nil "Vec" ("AMS" "Accents"))
    (nil "dddot" ("AMS" "Accents"))
    (nil "ddddot" ("AMS" "Accents"))
    (nil "bigl" ("AMS" "Delimiters"))
    (nil "bigr" ("AMS" "Delimiters"))
    (nil "Bigl" ("AMS" "Delimiters"))
    (nil "Bigr" ("AMS" "Delimiters"))
    (nil "biggl" ("AMS" "Delimiters"))
    (nil "biggr" ("AMS" "Delimiters"))
    (nil "Biggl" ("AMS" "Delimiters"))
    (nil "Biggr" ("AMS" "Delimiters"))
    (nil "lvert" ("AMS" "Delimiters"))
    (nil "rvert" ("AMS" "Delimiters"))
    (nil "lVert" ("AMS" "Delimiters"))
    (nil "rVert" ("AMS" "Delimiters"))
    (nil "ulcorner" ("AMS" "Delimiters"))
    (nil "urcorner" ("AMS" "Delimiters"))
    (nil "llcorner" ("AMS" "Delimiters"))
    (nil "lrcorner" ("AMS" "Delimiters"))
    (nil "nobreakdash" ("AMS" "Special"))
    (nil "leftroot" ("AMS" "Special"))
    (nil "uproot" ("AMS" "Special"))
    (nil "accentedsymbol" ("AMS" "Special"))
    (nil "xleftarrow" ("AMS" "Special"))
    (nil "xrightarrow" ("AMS" "Special"))
    (nil "overset" ("AMS" "Special"))
    (nil "underset" ("AMS" "Special"))
    (nil "dfrac" ("AMS" "Special"))
    (nil "genfrac" ("AMS" "Special"))
    (nil "tfrac" ("AMS" "Special"))
    (nil "binom" ("AMS" "Special"))
    (nil "dbinom" ("AMS" "Special"))
    (nil "tbinom" ("AMS" "Special"))
    (nil "smash" ("AMS" "Special"))
    (nil "eucal" ("AMS" "Special"))
    (nil "boldsymbol" ("AMS" "Special"))
    (nil "text" ("AMS" "Special"))
    (nil "intertext" ("AMS" "Special"))
    (nil "substack" ("AMS" "Special"))
    (nil "subarray" ("AMS" "Special"))
    (nil "sideset" ("AMS" "Special"))))

(defcustom LaTeX-math-abbrev-prefix "`"
  "Prefix key for use in `LaTeX-math-mode'."
  :group 'LaTeX-math
  :type 'sexp)

(defvar LaTeX-math-keymap (make-sparse-keymap)
  "Keymap used for `LaTeX-math-mode' commands.")

(defvar LaTeX-math-menu
  '("Math"
    ("Greek") ("greek") ("Binary Op") ("Relational") ("Arrows")
    ("Misc Symbol") ("Var Symbol") ("Log-like") ("delimiters")
    ("Delimiters") ("Constructs") ("Accents") ("AMS"))
  "Menu containing LaTeX math commands.
The menu entries will be generated dynamically, but you can specify
the sequence by initializing this variable.")

(define-key LaTeX-math-keymap
  (apply 'vector (append LaTeX-math-abbrev-prefix
			 LaTeX-math-abbrev-prefix nil))
  'LaTeX-math-insert-prefix)

(let ((math (reverse (append LaTeX-math-list LaTeX-math-default)))
      (map (lookup-key LaTeX-math-keymap LaTeX-math-abbrev-prefix)))
  (while math
    (let* ((entry (car math))
	   (key (nth 0 entry))
	   value menu name)
      (setq math (cdr math))
      (if (listp (cdr entry))
	  (setq value (nth 1 entry)
		menu (nth 2 entry))
	(setq value (cdr entry)
	      menu nil))
      (if (stringp value)
	  (progn
	   (setq name (intern (concat "LaTeX-math-" value)))
	   (fset name (list 'lambda (list 'arg) (list 'interactive "*P")
			    (list 'LaTeX-math-insert value 'arg))))
	(setq name value))
      (if key
	  (progn
	    (setq key (if (numberp key) (char-to-string key) (vector key)))
	    (define-key map key name)))
      (if menu
	  (let ((parent LaTeX-math-menu))
	    (if (listp menu)
		(progn
		  (while (cdr menu)
		    (let ((sub (assoc (car menu) LaTeX-math-menu)))
		      (if sub
			  (setq parent sub)
			(setcdr parent (cons (list (car menu)) (cdr parent))))
		      (setq menu (cdr menu))))
		  (setq menu (car menu))))
	    (let ((sub (assoc menu parent)))
	      (if sub
		  (if (stringp value)
		      (setcdr sub (cons (vector value name t) (cdr sub)))
		    (error "Cannot have multiple special math menu items"))
		(setcdr parent
			(cons (if (stringp value)
				  (list menu (vector value name t))
				(vector menu name t))
			      (cdr parent))))))))))

(easy-menu-define LaTeX-math-mode-menu
    LaTeX-math-keymap
    "Menu used in math minor mode."
  LaTeX-math-menu)

(defvar LaTeX-math-mode nil
  "Is `LaTeX-math-mode' on or off?  Non nil means on.")

 (make-variable-buffer-local 'LaTeX-math-mode)

(or (assoc 'LaTeX-math-mode minor-mode-alist)
    (setq minor-mode-alist (cons '(LaTeX-math-mode " Math") minor-mode-alist)))

(or (assoc 'LaTeX-math-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
	  (cons (cons 'LaTeX-math-mode LaTeX-math-keymap)
		minor-mode-map-alist)))

(defun LaTeX-math-mode (&optional arg)
  "A minor mode with easy acces to TeX math macros.

Easy insertion of LaTeX math symbols.  If you give a prefix argument,
the symbols will be surrounded by dollar signs.  The following
commands are defined:

\\{LaTeX-math-keymap}"
  (interactive "P")
  (setq LaTeX-math-mode
	(not (or (and (null arg) LaTeX-math-mode)
		 (<= (prefix-numeric-value arg) 0))))
  (if LaTeX-math-mode
      (easy-menu-add LaTeX-math-mode-menu LaTeX-math-keymap)
    (easy-menu-remove LaTeX-math-mode-menu))
  (set-buffer-modified-p (buffer-modified-p)))

(fset 'latex-math-mode 'LaTeX-math-mode)

(defun LaTeX-math-insert-prefix ()
  "Insert the value of `LaTeX-math-abbrev-prefix'."
  (interactive "*")
  (let (LaTeX-math-mode)
    (if (key-binding LaTeX-math-abbrev-prefix)
	(call-interactively (key-binding LaTeX-math-abbrev-prefix))
      (error "%S has no default binding" LaTeX-math-abbrev-prefix))))

(defcustom LaTeX-math-insert-function 'TeX-insert-macro
  "Function called with argument STRING to insert \\STRING."
  :group 'LaTeX-math
  :type 'function)

(defun LaTeX-math-insert (string dollar)
  "Insert \\STRING{}.  If DOLLAR is non-nil, put $'s around it."
  (if dollar (insert "$"))
  (funcall LaTeX-math-insert-function string)
  (if dollar (insert "$")))

(defun LaTeX-math-cal (char dollar)
  "Insert a {\\cal CHAR}.  If DOLLAR is non-nil, put $'s around it."
  (interactive "*c\nP")
  (if dollar (insert "$"))
  (if (member "latex2e" (TeX-style-list))
      (insert "\\mathcal{" (char-to-string char) "}")
    (insert "{\\cal " (char-to-string char) "}"))
  (if dollar (insert "$")))

(provide 'latex)

;;; Keymap

(defvar LaTeX-mode-map
  (let ((map (copy-keymap TeX-mode-map)))
    
    ;; Standard
    (define-key map "\n"      'reindent-then-newline-and-indent)
    
    ;; From latex.el
    ;; We now set `fill-paragraph-function' instead.
    ;; (define-key map "\eq"     'LaTeX-fill-paragraph) ;*** Alias
    ;; This key is now used by Emacs for face settings.
    ;; (define-key map "\eg"     'LaTeX-fill-region) ;*** Alias
    (define-key map "\e\C-e"  'LaTeX-find-matching-end)
    (define-key map "\e\C-a"  'LaTeX-find-matching-begin)
    
    (define-key map "\C-c\C-q\C-p" 'LaTeX-fill-paragraph)
    (define-key map "\C-c\C-q\C-r" 'LaTeX-fill-region)
    (define-key map "\C-c\C-q\C-s" 'LaTeX-fill-section)
    (define-key map "\C-c\C-q\C-e" 'LaTeX-fill-environment)
    
    (define-key map "\C-c."    'LaTeX-mark-environment) ;*** Dubious
    (define-key map "\C-c*"    'LaTeX-mark-section) ;*** Dubious

    (define-key map "\C-c\C-e" 'LaTeX-environment)
    (define-key map "\C-c\n"   'LaTeX-insert-item)
    (or (key-binding "\e\r")
	(define-key map "\e\r"    'LaTeX-insert-item)) ;*** Alias
    (define-key map "\C-c]" 'LaTeX-close-environment)
    (define-key map "\C-c\C-s" 'LaTeX-section)

    ;; Outline commands...
    ;; We want to use the right prefix, if possible.
    (let ((outline (cond ((not (boundp 'outline-minor-mode-prefix))
			  (lookup-key map "\C-c"))
			 ((keymapp (lookup-key map outline-minor-mode-prefix))
			  (lookup-key map outline-minor-mode-prefix))
			 (t
			  (define-key map
			    outline-minor-mode-prefix (make-sparse-keymap))
			  (lookup-key map outline-minor-mode-prefix)))))
      (define-key outline "\C-z" 'LaTeX-hide-environment)
      (define-key outline "\C-x" 'LaTeX-show-environment))

    (define-key map "\C-c~"    'LaTeX-math-mode) ;*** Dubious
    
    map)
  "Keymap used in `LaTeX-mode'.")

(defvar LaTeX-environment-menu-name "Insert Environment  (C-c C-e)")

(defun LaTeX-environment-menu-entry (entry)
  "Create an entry for the environment menu."
  (vector (car entry) (list 'LaTeX-environment-menu (car entry)) t))

(defvar LaTeX-environment-modify-menu-name "Change Environment  (C-u C-c C-e)")

(defun LaTeX-environment-modify-menu-entry (entry)
  "Create an entry for the change environment menu."
  (vector (car entry) (list 'LaTeX-modify-environment (car entry)) t))

(defun LaTeX-section-enable-symbol (LEVEL)
  "Symbol used to enable section LEVEL in the menu bar."
  (intern (concat "LaTeX-section-" (int-to-string (nth 1 entry)) "-enable")))

(defun LaTeX-section-enable (entry)
  "Enable or disable section ENTRY from `LaTeX-section-list'."
  (let ((level (nth 1 entry)))
    (set (LaTeX-section-enable-symbol level)
	 (>= level LaTeX-largest-level))))

(defun LaTeX-section-menu (level)
  "Insert section from menu."
  (let ((LaTeX-section-hook (delq 'LaTeX-section-heading
				  (copy-sequence LaTeX-section-hook))))
    (LaTeX-section level)))

(defun LaTeX-section-menu-entry (entry)
  "Create an ENTRY for the section menu."
  (let ((enable (LaTeX-section-enable-symbol (nth 1 entry))))
    (set enable t)
    (vector (car entry) (list 'LaTeX-section-menu (nth 1 entry)) enable)))

(defun LaTeX-section-menu-create ()
  "Create a menu over LaTeX sections."
  (append '("Section  (C-c C-s)")
	  (mapcar 'LaTeX-section-menu-entry LaTeX-section-list)))

(defvar LaTeX-menu-changed nil)
;; Need to update LaTeX menu.
(make-variable-buffer-local 'LaTeX-menu-changed)

(defcustom LaTeX-menu-max-items 25
  "*Maximum number of items in the menu for LaTeX environments.
If number of entries in a menu is larger than this value, split menu
into submenus of nearly equal length.  If nil, never split menu into
submenus."
  :group 'LaTeX-environment
  :type '(choice (const :tag "no submenus" nil)
		 (integer)))

(defcustom LaTeX-submenu-name-format "%-12.12s ... %.12s"
  "*Format specification of the submenu name.
Used by `LaTeX-split-long-menu' if the number of entries in a menu is
larger than `LaTeX-menu-max-items'.
This string should contain one %s for the name of the first entry and
one %s for the name of the last entry in the submenu.
If the value is a function, it should return the submenu name.  The
function is called with two arguments, the names of the first and
the last entry in the menu."
  :group 'LaTeX-environment
  :type '(choice (string :tag "Format string")
		 (function)))

(defun LaTeX-split-long-menu (menu)
  "Split MENU according to `LaTeX-menu-max-items'."
  (let ((len (length menu)))
    (if (or (null LaTeX-menu-max-items)
	    (null (featurep 'lisp-float-type))
	    (<= len LaTeX-menu-max-items))
	menu
      ;; Submenu is max 2 entries longer than menu, never shorter, number of
      ;; entries in submenus differ by at most one (with longer submenus first)
      (let* ((outer (floor (sqrt len)))
	     (inner (/ len outer))
	     (rest (% len outer))
	     (result nil))
	(setq menu (reverse menu))
	(while menu
	  (let ((in inner)
		(sub nil)
		(to (car menu)))
	    (while (> in 0)
	      (setq in   (1- in)
		    sub  (cons (car menu) sub)
		    menu (cdr menu)))
	    (setq result
		  (cons (cons (if (stringp LaTeX-submenu-name-format)
				  (format LaTeX-submenu-name-format
					  (aref (car sub) 0) (aref to 0))
				(funcall LaTeX-submenu-name-format
					 (aref (car sub) 0) (aref to 0)))
			      sub)
			result)
		  rest  (1+ rest))
	    (if (= rest outer) (setq inner (1+ inner)))))
	result))))

(defun LaTeX-menu-update ()
  "Update entries on AUC TeX menu."
  (or (not (eq major-mode 'latex-mode))
      (null LaTeX-menu-changed)
      (not (fboundp 'easy-menu-change))
      (progn
	(TeX-update-style)
	(setq LaTeX-menu-changed nil)
	(message "Updating section menu...")
	(mapcar 'LaTeX-section-enable LaTeX-section-list)
	(message "Updating environment menu...")
	(easy-menu-change '("LaTeX") LaTeX-environment-menu-name
			  (LaTeX-split-long-menu
			   (mapcar 'LaTeX-environment-menu-entry
				   (LaTeX-environment-list))))
	(message "Updating modify environment menu...")
	(easy-menu-change '("LaTeX") LaTeX-environment-modify-menu-name
			  (LaTeX-split-long-menu
			   (mapcar 'LaTeX-environment-modify-menu-entry
				   (LaTeX-environment-list))))
	(message "Updating...done"))))

(add-hook 'activate-menubar-hook 'LaTeX-menu-update)

(easy-menu-define LaTeX-mode-menu
    LaTeX-mode-map
    "Menu used in LaTeX mode."
  (list "LaTeX"
	(list LaTeX-environment-menu-name "Bug.")
	(list LaTeX-environment-modify-menu-name "Bug.")
	(LaTeX-section-menu-create)
	["Macro..." TeX-insert-macro t]
	["Complete" TeX-complete-symbol t]
	["Item" LaTeX-insert-item t]
	(list "Insert Font"
	      ["Emphasize"  (TeX-font nil ?\C-e) :keys "C-c C-f C-e"]
	      ["Bold"       (TeX-font nil ?\C-b) :keys "C-c C-f C-b"]
	      ["Typewriter" (TeX-font nil ?\C-t) :keys "C-c C-f C-t"]
	      ["Small Caps" (TeX-font nil ?\C-c) :keys "C-c C-f C-c"]
	      ["Sans Serif" (TeX-font nil ?\C-f) :keys "C-c C-f C-f"]
	      ["Italic"     (TeX-font nil ?\C-i) :keys "C-c C-f C-i"]
	      ["Slanted"    (TeX-font nil ?\C-s) :keys "C-c C-f C-s"]
	      ["Roman"      (TeX-font nil ?\C-r) :keys "C-c C-f C-r"]
	      ["Calligraphic" (TeX-font nil ?\C-a) :keys "C-c C-f C-a"])
	(list "Change Font"
	      ["Emphasize"  (TeX-font t ?\C-e) :keys "C-u C-c C-f C-e"]
	      ["Bold"       (TeX-font t ?\C-b) :keys "C-u C-c C-f C-b"]
	      ["Typewriter" (TeX-font t ?\C-t) :keys "C-u C-c C-f C-t"]
	      ["Small Caps" (TeX-font t ?\C-c) :keys "C-u C-c C-f C-c"]
	      ["Sans Serif" (TeX-font t ?\C-f) :keys "C-u C-c C-f C-f"]
	      ["Italic"     (TeX-font t ?\C-i) :keys "C-u C-c C-f C-i"]
	      ["Slanted"    (TeX-font t ?\C-s) :keys "C-u C-c C-f C-s"]
	      ["Roman"      (TeX-font t ?\C-r) :keys "C-u C-c C-f C-r"]
	      ["Calligraphic" (TeX-font t ?\C-a) :keys "C-u C-c C-f C-a"])
	["Delete Font" (TeX-font t ?\C-d) :keys "C-c C-f C-d"]
	"-"
	["Next Error" TeX-next-error t]
	(list "TeX Output"
	      ["Kill Job" TeX-kill-job t]
	      ["Debug Bad Boxes" TeX-toggle-debug-boxes
	       :style toggle :selected TeX-debug-bad-boxes ]
	      ["Switch to Original File" TeX-home-buffer t]
	      ["Recenter Output Buffer" TeX-recenter-output-buffer t])
	(list "Formatting and Marking"
	      ["Format Environment" LaTeX-fill-environment t]
	      ["Format Paragraph" LaTeX-fill-paragraph t]
	      ["Format Region" LaTeX-fill-region t]
	      ["Format Section" LaTeX-fill-section t]
	      ["Mark Environment" LaTeX-mark-environment t]
	      ["Mark Section" LaTeX-mark-section t]
	      ["Beginning of Environment" LaTeX-find-matching-begin t]
	      ["End of Environment" LaTeX-find-matching-end t]
	      ["Hide Environment" LaTeX-hide-environment t]
	      ["Show Environment" LaTeX-show-environment t])
	(list "Miscellaneous"
	      ["Uncomment Region" TeX-un-comment-region t]
	      ["Comment Region" TeX-comment-region t]
	      ["Switch to Master file" TeX-home-buffer t]
	      ["Save Document" TeX-save-document t]
	      ["Math Mode" LaTeX-math-mode
	       :style toggle :selected LaTeX-math-mode ]
	      ["Documentation" TeX-goto-info-page t]
	      ["Submit bug report" TeX-submit-bug-report t]
	      [ "Convert 209 to 2e" LaTeX-209-to-2e
		:active (member "latex2" (TeX-style-list)) ]
	      ["Reset Buffer" TeX-normal-mode t]
	      ["Reset AUC TeX" (TeX-normal-mode t) :keys "C-u C-c C-n"])))

(defcustom LaTeX-font-list
  '((?\C-a ""              ""  "\\mathcal{"    "}")
    (?\C-b "\\textbf{"     "}" "\\mathbf{"     "}")
    (?\C-c "\\textsc{"     "}")
    (?\C-e "\\emph{"       "}")
    (?\C-f "\\textsf{"     "}" "\\mathsf{"     "}")
    (?\C-i "\\textit{"     "}" "\\mathit{"     "}")
    (?\C-m "\\textmd{"     "}")
    (?\C-n "\\textnormal{" "}" "\\mathnormal{" "}")
    (?\C-r "\\textrm{"     "}" "\\mathrm{"     "}")
    (?\C-s "\\textsl{"     "}")
    (?\C-t "\\texttt{"     "}" "\\mathtt{"     "}")
    (?\C-u "\\textup{"     "}")
    (?\C-d "" "" t))
  "Font commands used with LaTeX2e.  See `TeX-font-list'."
  :group 'LaTeX-macro
  :type '(repeat
	   (group
	    :value (?\C-a "" "")
	    (character :tag "Key")
	    (string :tag "Prefix")
	    (string :tag "Suffix")
	    (option (group
		     :inline t
		     (string :tag "Math Prefix")
		     (string :tag "Math Suffix")))
	    (option (sexp :format "Replace\n" :value t)))))
;;; Mode

(defgroup LaTeX-macro nil
  "Special support for LaTeX macros in AUC TeX."
  :prefix "TeX-"
  :group 'LaTeX
  :group 'TeX-macro)

(defcustom TeX-arg-cite-note-p nil
  "*If non-nil, ask for optional note in citations."
  :type 'boolean
  :group 'LaTeX-macro)

(defcustom TeX-arg-footnote-number-p nil
  "*If non-nil, ask for optional number in footnotes."
  :type 'boolean
  :group 'LaTeX-macro)

(defcustom TeX-arg-item-label-p nil
  "*If non-nil, always ask for optional label in items.
Otherwise, only ask in description environments."
  :type 'boolean
  :group 'LaTeX-macro)

(defcustom TeX-arg-right-insert-p t
  "*If non-nil, always insert automatically the corresponding \\right.
This happens when \\left is inserted."
  :type 'boolean
  :group 'LaTeX-macro)

(defvar LaTeX-paragraph-commands
  (concat "\\[\\|\\]\\|"  ; display math delimitors
	  "begin\\b\\|end\\b\\|part\\b\\|chapter\\b\\|label\\b\\|"
	  "caption\\b\\|section\\b\\|subsection\\b\\|subsubsection\\b\\|"
	  "par\\b\\|noindent\\b\\|paragraph\\b\\|include\\b\\|"
	  "includeonly\\b\\|tableofcontents\\b\\|appendix\\b")
  "Regexp matching names of LaTeX macros that should have their own line.")

;;; Do not ;;;###autoload because of conflict with standard tex-mode.el.
(defun latex-mode ()
  "Major mode for editing files of input for LaTeX.
See info under AUC TeX for full documentation.

Special commands:
\\{LaTeX-mode-map}

Entering LaTeX mode calls the value of `text-mode-hook',
then the value of `TeX-mode-hook', and then the value
of `LaTeX-mode-hook'."
  (interactive)
  (LaTeX-common-initialization)
  (setq mode-name "LaTeX")
  (setq major-mode 'latex-mode)
  (setq TeX-command-default "LaTeX")
  (run-hooks 'text-mode-hook 'TeX-mode-hook 'LaTeX-mode-hook)

  ;; Defeat filladapt if auto-fill-mode is set in text-mode-hook.
  (and (boundp 'filladapt-function-table)
       (boundp 'auto-fill-function)
       (eq auto-fill-function 'do-auto-fill)
       (setq auto-fill-function
	     (cdr (assoc 'do-auto-fill filladapt-function-table)))))

(defvar LaTeX-header-end
  (concat (regexp-quote TeX-esc) "begin *" TeX-grop "document" TeX-grcl)
  "Default end of header marker for LaTeX documents.")

(defvar LaTeX-trailer-start
  (concat (regexp-quote TeX-esc) "end *" TeX-grop "document" TeX-grcl)
  "Default start of trailer marker for LaTeX documents.")

(defun LaTeX-common-initialization ()
  "Common initialization for LaTeX derived modes."
  (VirTeX-common-initialization)
  (set-syntax-table LaTeX-mode-syntax-table)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'LaTeX-indent-line)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'LaTeX-fill-paragraph)
  (use-local-map LaTeX-mode-map)
  (easy-menu-add TeX-mode-menu LaTeX-mode-map)
  (easy-menu-add LaTeX-mode-menu LaTeX-mode-map)

  (or LaTeX-largest-level
      (setq LaTeX-largest-level (LaTeX-section-level "section")))
  
  (setq TeX-header-end LaTeX-header-end
	TeX-trailer-start LaTeX-trailer-start)

  (require 'outline)
  (make-local-variable 'outline-level)
  (setq outline-level 'LaTeX-outline-level)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp (LaTeX-outline-regexp t))
  
  (make-local-variable 'TeX-auto-full-regexp-list)
  (setq TeX-auto-full-regexp-list
	(append LaTeX-auto-regexp-list plain-TeX-auto-regexp-list))

  (setq paragraph-start
	(concat
	 "\\("
	 "^.*[^" TeX-esc "\n]%.*$\\|"
	 "^%.*$\\|"
	 "^[ \t]*$\\|"
	 "^[ \t]*"
	 (regexp-quote TeX-esc)
	 "\\("
	 LaTeX-paragraph-commands
	 "\\|\\(bib\\)?item\\b"
	 "\\)"
	 "\\|"
	 "^[ \t]*\\$\\$" ; display math delimitor
	 "\\)" ))
  (setq paragraph-separate
	(concat
	 "\\("
	 "^.*[^" TeX-esc "\n]%.*$\\|"
	 "^%.*$\\|"
	 "^[ \t]*$\\|"
	 "^[ \t]*"
	 (regexp-quote TeX-esc)
	 "\\("
	 LaTeX-paragraph-commands
	 "\\)"
	 "\\)"))
  (setq selective-display t)

  (make-local-variable 'LaTeX-item-list)
  (setq LaTeX-item-list '(("description" . LaTeX-item-argument)
			  ("thebibliography" . LaTeX-item-bib)))

  (setq TeX-complete-list
	(append '(("\\\\cite\\[[^]\n\r\\%]*\\]{\\([^{}\n\r\\%,]*\\)"
		   1 LaTeX-bibitem-list "}")
		  ("\\\\cite{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}")
		  ("\\\\cite{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)"
		   2 LaTeX-bibitem-list)
		  ("\\\\nocite{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-bibitem-list "}")
		  ("\\\\nocite{\\([^{}\n\r\\%]*,\\)\\([^{}\n\r\\%,]*\\)"
		   2 LaTeX-bibitem-list)
		  ("\\\\ref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
		  ("\\\\eqref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
		  ("\\\\pageref{\\([^{}\n\r\\%,]*\\)" 1 LaTeX-label-list "}")
 		  ("\\\\\\(index\\|glossary\\){\\([^{}\n\r\\%]*\\)"
 		   2 LaTeX-index-entry-list "}")
		  ("\\\\begin{\\([A-Za-z]*\\)" 1 LaTeX-environment-list "}")
		  ("\\\\end{\\([A-Za-z]*\\)" 1 LaTeX-environment-list "}")
		  ("\\\\renewcommand\\*?{\\\\\\([A-Za-z]*\\)"
		   1 LaTeX-symbol-list "}")
		  ("\\\\renewenvironment\\*?{\\([A-Za-z]*\\)"
		   1 LaTeX-environment-list "}"))
		TeX-complete-list))

  (LaTeX-add-environments
   '("document" LaTeX-env-document)
   '("enumerate" LaTeX-env-item)
   '("itemize" LaTeX-env-item)
   '("list" LaTeX-env-list)
   '("trivlist" LaTeX-env-item)
   '("picture" LaTeX-env-picture)
   '("tabular" LaTeX-env-array)
   '("tabular*" LaTeX-env-array)
   '("array" LaTeX-env-array)
   '("eqnarray" LaTeX-env-label)
   '("equation" LaTeX-env-label)
   '("minipage" LaTeX-env-minipage)

   ;; The following have no special support, but are included in
   ;; case the auto files are missing.

   "sloppypar" "picture" "tabbing" "verbatim" "verbatim*"
   "flushright" "flushleft" "displaymath" "math" "quote" "quotation"
   "abstract" "center" "titlepage" "verse" "eqnarray*"

   ;; The following are not defined in latex.el, but in a number of
   ;; other style files.  I'm to lazy to copy them to all the
   ;; corresponding .el files right now.

   ;; This means that AUC TeX will complete e.g.
   ;; ``thebibliography'' in a letter, but I guess we can live with
   ;; that.

   '("description" LaTeX-env-item)
   '("figure" LaTeX-env-figure)
   '("figure*" LaTeX-env-figure)
   '("table" LaTeX-env-figure)
   '("table*" LaTeX-env-figure)
   '("thebibliography" LaTeX-env-bib)
   '("theindex" LaTeX-env-item))

  (TeX-add-symbols
   '("addtocounter" TeX-arg-counter "Value")
   '("alph" TeX-arg-counter)
   '("arabic" TeX-arg-counter)
   '("fnsymbol" TeX-arg-define-counter)
   '("newcounter" TeX-arg-define-counter
     [ TeX-arg-counter "Within counter" ])
   '("roman" TeX-arg-counter)
   '("setcounter" TeX-arg-counter "Value")
   '("usecounter" TeX-arg-counter)
   '("value" TeX-arg-counter)
   '("stepcounter" TeX-arg-counter)
   '("refstepcounter" TeX-arg-counter)
   '("label" TeX-arg-define-label)
   '("pageref" TeX-arg-ref)
   '("ref" TeX-arg-ref)
   '("newcommand" TeX-arg-define-macro [ "Number of arguments" ] t)
   '("renewcommand" TeX-arg-macro [ "Number of arguments" ] t)
   '("newenvironment" TeX-arg-define-environment
     [ "Number of arguments"] t t)
   '("renewenvironment" TeX-arg-environment
     [ "Number of arguments"] t t)
   '("providecommand" TeX-arg-define-macro [ "Number of arguments" ] t)
   '("providecommand*" TeX-arg-define-macro [ "Number of arguments" ] t)
   '("newcommand*" TeX-arg-define-macro [ "Number of arguments" ] t)
   '("renewcommand*" TeX-arg-macro [ "Number of arguments" ] t)
   '("newenvironment*" TeX-arg-define-environment
     [ "Number of arguments"] t t)
   '("renewenvironment*" TeX-arg-environment
     [ "Number of arguments"] t t)
   '("newtheorem" TeX-arg-define-environment
     [ TeX-arg-environment "Numbered like" ]
     t [ (TeX-arg-eval progn (if (eq (save-excursion
				       (backward-char 2)
				       (preceding-char)) ?\])
				 ()
			       (TeX-arg-counter t "Within counter"))
		       "") ])
   '("newfont" TeX-arg-define-macro t)
   '("circle" "Diameter")
   '("circle*" "Diameter")
   '("dashbox" "Dash Length" TeX-arg-size
     [ TeX-arg-corner ] t)
   '("frame" t)
   '("framebox" (TeX-arg-conditional
		 (string-equal (LaTeX-current-environment) "picture")
		 (TeX-arg-size [ TeX-arg-corner ] t)
		 ([ "Length" ] [ TeX-arg-lr ] t)))
   '("line" (TeX-arg-pair "X slope" "Y slope") "Length")
   '("linethickness" "Dimension")
   '("makebox" (TeX-arg-conditional
		(string-equal (LaTeX-current-environment) "picture")
		(TeX-arg-size [ TeX-arg-corner ] t)
		([ "Length" ] [ TeX-arg-lr ] t)))
   '("multiput"
     TeX-arg-coordinate
     (TeX-arg-pair "X delta" "Y delta")
     "Number of copies"
     t)
   '("oval" TeX-arg-size [ TeX-arg-corner "Portion" ])
   '("put" TeX-arg-coordinate t)
   '("savebox" TeX-arg-define-savebox
     (TeX-arg-conditional
      (string-equal (LaTeX-current-environment) "picture")
      (TeX-arg-size [ TeX-arg-corner ] t)
      ([ "Length" ] [ TeX-arg-lr ] t)))
   '("shortstack" [ TeX-arg-lr ] t)
   '("vector" (TeX-arg-pair "X slope" "Y slope") "Length")
   '("cline" "Span `i-j'")
   '("multicolumn" "Columns" "Position" t)
   '("item"
     (TeX-arg-conditional (or TeX-arg-item-label-p
			      (string-equal (LaTeX-current-environment)
					    "description"))
			  ([ "Item label" ])
			  ())
     (TeX-arg-literal " "))
   '("bibitem" [ "Bibitem label" ] TeX-arg-define-cite)
   '("cite"
     (TeX-arg-conditional TeX-arg-cite-note-p ([ "Note" ]) ())
     TeX-arg-cite)
   '("nocite" TeX-arg-cite)
   '("bibliographystyle" TeX-arg-bibstyle)
   '("bibliography" TeX-arg-bibliography)
   '("footnote"
     (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil)
     t)
   '("footnotetext"
     (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil)
     t)
   '("footnotemark"
     (TeX-arg-conditional TeX-arg-footnote-number-p ([ "Number" ]) nil))
   '("newlength" TeX-arg-define-macro)
   '("setlength" TeX-arg-macro "Length")
   '("addtolength" TeX-arg-macro "Length")
   '("settowidth" TeX-arg-macro t)
   '("\\" [ "Space" ])
   '("\\*" [ "Space" ])
   '("hyphenation" t)
   '("linebreak" [ "How much [0 - 4]" ])
   '("nolinebreak" [ "How much [0 - 4]" ])
   '("nopagebreak" [ "How much [0 - 4]" ])
   '("pagebreak" [ "How much [0 - 4]" ])
   '("stackrel" t nil)
   '("frac" t nil)
   '("lefteqn" t)
   '("overbrace" t)
   '("overline" t)
   '("sqrt" [ "Root" ] t)
   '("underbrace" t)
   '("underline" t)
   '("author" t)
   '("date" t)
   '("thanks" t)
   '("title" t)
   '("pagenumbering" (TeX-arg-eval
		      completing-read "Numbering style: "
		      '(("arabic") ("roman") ("Roman") ("alph") ("Alph"))))
   '("pagestyle" TeX-arg-pagestyle)
   '("markboth" t nil)
   '("markright" t)
   '("thispagestyle" TeX-arg-pagestyle)
   '("addvspace" "Length")
   '("fbox" t)
   '("hspace*" "Length")
   '("hspace" "Length")
   '("mbox" t)
   '("newsavebox" TeX-arg-define-savebox)
   '("parbox" [ TeX-arg-tb ] "Width" t)
   '("raisebox" "Raise" [ "Height above" ] [ "Depth below" ] t)
   '("rule" [ "Raise" ] "Width" "Thickness")
   '("sbox" TeX-arg-define-savebox t)
   '("usebox" TeX-arg-savebox)
   '("vspace*" "Length")
   '("vspace" "Length")
   '("documentstyle" TeX-arg-document)
   '("include" (TeX-arg-input-file "File" t))
   '("includeonly" t)
   '("input" TeX-arg-input-file)
   '("addcontentsline" TeX-arg-file
     (TeX-arg-eval
      completing-read "Numbering style: " LaTeX-section-list)
     t)
   '("addtocontents" TeX-arg-file t)
   '("typeout" t)
   '("typein" [ TeX-arg-define-macro ] t)
   '("verb" TeX-arg-verb)
   '("verb*" TeX-arg-verb)
   '("extracolsep" t)
   '("index" TeX-arg-index)
   '("glossary" TeX-arg-index)
   '("numberline" "Section number" "Heading")
   '("caption" t)
   '("marginpar" [ "Left margin text" ] "Text")
   '("left" TeX-arg-insert-braces)

   ;; These have no special support, but are included in case the
   ;; auto files are missing.

   "LaTeX" "SLiTeX" "samepage" "newline" "smallskip" "medskip"
   "bigskip" "stretch" "nonumber" "centering" "raggedright"
   "raggedleft" "kill" "pushtabs" "poptabs" "protect" "arraystretch"
   "hline" "vline" "cline" "thinlines" "thicklines" "and" "makeindex"
   "makeglossary" "reversemarginpar" "normalmarginpar"
   "raggedbottom" "flushbottom" "sloppy" "fussy" "newpage"
   "clearpage" "cleardoublepage" "twocolumn" "onecolumn"

   ;; Added 24/10/2002
   "TeX" "maketitle" "tableofcontents" "listoffigures" "listoftables"
   "tiny" "scriptsize" "footnotesize" "small"
   "normalsize" "large" "Large" "LARGE" "huge" "Huge"
   "pounds" "copyright"
   "hfil" "hfill" "vfil" "vfill" "hrulefill" "dotfill"
   "indent" "noindent" "today")

  (when (string-equal LaTeX-version "2e")
    (LaTeX-add-environments
     '("filecontents" LaTeX-env-contents)
     '("filecontents*" LaTeX-env-contents))

    (TeX-add-symbols
     '("enlargethispage" "Length")
     '("enlargethispage*" "Length")
     '("tabularnewline" [ "Length" ])
     '("suppressfloats" [ TeX-arg-tb "Suppress floats position" ])
     '("ensuremath" "Math commands")
     '("textsuperscript" "Text")
     '("textcircled" "Text")

     "LaTeXe"
     "listfiles" "frontmatter" "mainmatter" "backmatter"
     "textcompwordmark" "textvisiblespace" "textemdash" "textendash"
     "textexclamdown" "textquestiondown" "textquotedblleft"
     "textquotedblright" "textquoteleft" "textquoteright"
     "textbullet" "textperiodcentered"
     "textbackslash" "textbar" "textless" "textgreater"
     "textasciicircum" "textasciitilde"
     "textregistered" "texttrademark"
     "rmfamily" "sffamily" "ttfamily" "mdseries" "bfseries"
     "itshape" "slshape" "upshape" "scshape"))

  (TeX-run-style-hooks "LATEX")

  (make-local-variable 'TeX-font-list)
  (make-local-variable 'TeX-font-replace-function)
  (if (string-equal LaTeX-version "2")
      ()
    (setq TeX-font-list LaTeX-font-list)
    (setq TeX-font-replace-function 'TeX-font-replace-macro)
    (TeX-add-symbols
     '("newcommand" TeX-arg-define-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("renewcommand" TeX-arg-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("providecommand" TeX-arg-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("providecommand*" TeX-arg-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("newcommand*" TeX-arg-define-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("renewcommand*" TeX-arg-macro
       [ "Number of arguments" ] [ "Default value for first argument" ] t)
     '("usepackage" [ "Options" ] (TeX-arg-input-file "Package"))
     '("documentclass" TeX-arg-document)))

  (TeX-add-style-hook "latex2e"
   ;; Use new fonts for `\documentclass' documents.
   (function (lambda ()
     (setq TeX-font-list LaTeX-font-list)
     (setq TeX-font-replace-function 'TeX-font-replace-macro)
     (if (equal LaTeX-version "2")
	 (setq TeX-command-default "LaTeX2e"))
     (run-hooks 'LaTeX2e-hook))))
  
  (TeX-add-style-hook "latex2"
   ;; Use old fonts for `\documentstyle' documents.
   (function (lambda ()
     (setq TeX-font-list (default-value 'TeX-font-list))
     (setq TeX-font-replace-function
	   (default-value 'TeX-font-replace-function))
     (run-hooks 'LaTeX2-hook))))

  (set (make-local-variable 'imenu-create-index-function)
       'LaTeX-imenu-create-index-function))

(defun LaTeX-imenu-create-index-function ()
  "Imenu support function for LaTeX."
  (TeX-update-style)
  (let (entries level
	(regexp (LaTeX-outline-regexp)))
    (goto-char (point-max))
    (while (re-search-backward regexp nil t)
      (let* ((name (LaTeX-outline-name))
	     (level (make-string (1- (LaTeX-outline-level)) ?\ ))
	     (label (concat level level name))
	     (mark (make-marker)))
	(set-marker mark (point))
	(set-text-properties 0 (length label) nil label)
	(setq entries (cons (cons label mark) entries))))
    entries))

(defvar LaTeX-builtin-opts
  '("12pt" "11pt" "10pt" "twocolumn" "twoside" "draft")
  "Built in options for LaTeX standard styles.")

(defun LaTeX-209-to-2e ()
  "Make a stab at changing 2.09 doc header to 2e style."
  (interactive)
  (TeX-home-buffer)
  (let (optstr optlist 2eoptlist 2epackages docline docstyle)
    (goto-char (point-min))
    (if
	(search-forward-regexp
	 "\\documentstyle\\[\\([^]]*\\)\\]{\\([^}]*\\)}"
	 (point-max) t)
	(setq optstr (buffer-substring-no-properties (match-beginning 1) (match-end 1))
	      docstyle (buffer-substring-no-properties (match-beginning 2)
	      (match-end 2))
	      optlist (TeX-split-string "," optstr))
      (if (search-forward-regexp
	   "\\documentstyle{\\([^}]*\\)}"
	   (point-max) t)
	  (setq docstyle (buffer-substring-no-properties (match-beginning 1)
	  (match-end 1)))
	(error "No documentstyle defined")))
    (beginning-of-line 1)
    (setq docline (point))
    (insert "%%%")
    (while optlist
      (if (member (car optlist) LaTeX-builtin-opts)
	  (setq 2eoptlist (cons (car optlist) 2eoptlist))
	(setq 2epackages (cons (car optlist) 2epackages)))
      (setq optlist (cdr optlist)))
    ;;(message (format "%S %S" 2eoptlist 2epackages))
    (goto-char docline)
    (next-line 1)
    (insert "\\documentclass")
    (if 2eoptlist
	(insert "["
		(mapconcat (function (lambda (x) x))
			   (nreverse 2eoptlist) ",") "]"))
    (insert "{" docstyle "}\n")
    (if 2epackages
	(insert "\\usepackage{"
		(mapconcat (function (lambda (x) x))
			   (nreverse 2epackages) "}\n\\usepackage{") "}\n"))
    (if (equal docstyle "slides")
      (progn
	(goto-char (point-min))
	(while (re-search-forward "\\\\blackandwhite{" nil t)
      (replace-match "\\\\input{" nil nil)))))
  (TeX-normal-mode nil))

;;; latex.el ends here
