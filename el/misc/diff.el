;;; diff.el --- major mode for editing and updating diff files

;; Copyright (c) 2001 Michele Bini

;; Author: Michele Bini <mibin@libero.it>
;; Created: 13 Nov 2001
;; Version: 0.2
;; Keywords: languages

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Documentation:

;; This package is used to edit files produced by diff -u or
;; equivalent CVS diff.  You can use it to review your patch after you
;; have created it, to remove unwanted portions, to regenerate other
;; portions, reverse them, etc.  Only `unified' format patches are
;; supported.

;; To install this package, add the following to your .emacs:

;; (setq auto-mode-alist (append '(("\\.diff$" . diff-mode))
;;   auto-mode-alist))

;; and make sure to load this source file.  To have it auto-loaded any
;; time diff-mode is requested:

;; (autoload 'diff-mode "~/path/to/diff.el" nil t)

;; To use this package more conveniently you may need to set some
;; key bindings in your .emacs:

;; (defun my-diff-mode-patch ()
;;   (local-set-key [(super g)] 'diff-update-current-file)
;;   (local-set-key [(super f)] 'diff-find-new-file)
;;   (local-set-key [(super h)] 'diff-hide-body)
;;   (local-set-key [(super s)] 'diff-show-body)
;;   (local-set-key [(super n)] 'diff-forward-file)
;;   (local-set-key [(super p)] 'diff-backward-file)
;;   (local-set-key [(super r)] 'diff-reverse-current-file))
;; (add-hook 'diff-mode-hook 'my-diff-mode-patch)

;; I have made this package to help me to clean up, check my
;; patches, and to update them incrementally as i review my changes or
;; introduce new ones.
;; This package has no direct relation with the `ediff' library.

;; In the documentation, the word 'file' is used ambiguously,
;; referring sometimes to the actual patch file or to the individual
;; file-relative portions in the patch.

;;; History:
;; * 3 Dec 2001: minor documentation clean-ups.
;; * 2 Dec 2001: added documentation, first public release (on the Emacs
;; Wiki).


;;; Code:

(defvar diff-mode-map nil
  "Diff mode specific keymap.")
(unless diff-mode-map
  (setq diff-mode-map (make-sparse-keymap)))

(defvar diff-font-lock-keywords
  '(("^\\(\\+\\+\\+\\|\\-\\-\\-\\) [^\n]+" . font-lock-string-face)
    ("^@@ [^\n]+ @@" . font-lock-string-face)
    ("^+[^\n]*" . font-lock-variable-name-face)
    ("^-[^\n]*" . font-lock-function-name-face))
  "Default expressions to highlight diff mode files.")

(defconst diff-body-regexp "^[-+ ]")
(defconst diff-non-body-regexp "^[^-+ ]")
(defconst diff-non-body-non-hunk-regexp "^[^-+ @]")
(defconst diff-hunk-regexp "^@@ [^\n]+ @@")
(defconst diff-first-file-regexp "^\\-\\-\\- ")
(defvar diff-base-dir nil
  "Default value for the base directory.
Used for updating portions of the patch to new file contents.

To set this variable interactively (making this variable buffer
local) using `diff-set-base-dir'.
See also `diff-update-current-file'.")
(defvar diff-default-options nil
  "Default options to pass to the \"diff \" command.

See also `diff-update-current-file'.")

(defun diff-mode ()
  "Major mode for editing diff text files.

Turning on `diff-mode' runs the hook `diff-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map diff-mode-map)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	(list 'diff-font-lock-keywords t nil nil))
  (setq mode-name "Diff")
  (setq major-mode 'diff-mode)
  (run-hooks 'diff-mode-hook))

;;;; show/hide body
(defun diff-hide-body ()
  "Hide all the bodies of the patches, showing just headers.

This can be useful to remove large portions of the current patch."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^@@" nil t)
      (put-text-property
       (save-excursion (end-of-line) (point))
       (save-excursion
	 (if (re-search-forward diff-non-body-regexp nil t)
	     (- (match-beginning 0) 1) (- (point-max) 1)))
       'invisible
       'diff)))
  (add-to-invisibility-spec '(diff . t))
  (make-variable-buffer-local 'line-move-ignore-invisible)
  (setq line-move-ignore-invisible t))

(defun diff-show-body ()
  "Show back all the bodies of the patch.

See also `diff-hide-body'."
  (interactive)
  (remove-from-invisibility-spec '(diff . t)))

;;;; walking the buffer
(defun diff-forward-hunk (&optional no-error)
  "Walk one hunk forward.

When the optional NO-ERROR argument is non-nil, do not signal an error
if it is found impossible to find a subsequent hunk, and just return
nil instead."
  (interactive)
  (end-of-line)
  (let ((r (re-search-forward diff-hunk-regexp nil no-error)))
    (when r (beginning-of-line))
    r))

(defun diff-backward-hunk ()
  "Walk one hunk backward."
  (interactive)
  (re-search-backward diff-hunk-regexp))

(defun diff-forward-file ()
  "Walk one file forward in the current patch."
  (interactive)
  (end-of-line)
  (re-search-forward diff-first-file-regexp)
  (beginning-of-line))

(defun diff-backward-file ()
  "Walk one file backward in the current patch."
  (interactive)
  (re-search-backward diff-first-file-regexp))

;;;; basic editing
(defun diff-perform-current-file (command)
  "Call COMMAND with the extent of the current file."
  (funcall
   command
   (save-excursion
     (beginning-of-line 2)
     (diff-backward-file)
     (point))
   (save-excursion
     (beginning-of-line)
     (when (looking-at diff-non-body-regexp)
       (re-search-forward diff-body-regexp nil t))
     (cond
      ((re-search-forward diff-non-body-non-hunk-regexp nil t)
       (beginning-of-line)
       (point))
      (t (point-max))))))

(defun diff-kill-current-file ()
  "Kill the current file."
  (interactive)
  (diff-perform-current-file 'kill-region))

(defun diff-narrow-to-current-file ()
  "Narrow to the current file."
  (interactive)
  (diff-perform-current-file 'narrow-to-region))

(defun diff-perform-current-hunk (command)
  "Call COMMAND with the extent of the current hunk."
  (funcall
   command
   (save-excursion
     (beginning-of-line 2)
     (diff-backward-hunk)
     (point))
   (save-excursion
     (beginning-of-line)
     (when (looking-at diff-non-body-regexp)
       (re-search-forward diff-body-regexp nil t))
     (cond
      ((re-search-forward diff-non-body-regexp nil t)
       (- (point) 1))
      (t (point-max))))))

(defun diff-kill-current-hunk ()
  "Kill the current hunk."
  (interactive)
  (diff-perform-current-hunk 'kill-region))

(defun diff-narrow-to-current-hunk ()
  "Narrow to the current hunk."
  (interactive)
  (diff-perform-current-hunk 'narrow-to-region))

;;;; miscellaneous
(defun diff-current-file ()
  "Return a pair with the old and new file names."
  (save-excursion
    (and
     (or (looking-at diff-first-file-regexp)
	 (re-search-backward diff-first-file-regexp nil t))
     (looking-at "^--- \\([^ \t\n]+\\)")
     (let ((from-file (match-string 1)))
       (and
	(re-search-forward "^\\+\\+\\+ \\([^ \t\n]+\\)" nil t)
	(cons from-file (match-string 1)))))))

(defun diff-canonicalize-hunk ()
  "Canonicalize a unified format hunk.

This means that in consecutive line modification indicators, added
line indicators will be moved after removed lines indicators.  This
command can be useful after hand editing of hunks."
  (interactive)
  (save-excursion
    (while (re-search-forward "^[-+]" nil t)
      (let ((start (match-beginning 0))
	    (end (save-excursion
		   (or (and (re-search-forward "^[^-+]" nil t)
			    (match-beginning 0))
		       (point-max)))))
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (let ((old (list))
		(new (list)))
	    (while (re-search-forward "^-[^\n]*\n" nil t)
	      (setq old (cons (match-string 0) old)))
	    (goto-char start)
	    (while (re-search-forward "^+[^\n]*\n" nil t)
	      (setq new (cons (match-string 0) new)))
	    (kill-region (point-min) (point-max))
	    (apply 'insert (reverse old))
	    (apply 'insert (reverse new))))))))

(defun diff-reverse-current-file (&optional no-restore)
  "Reverse the current file patch.

After the patch is recreated the point will tentatively be restored to
a position contextually similar the point position before the update,
unless the optional NO-RESTORE argument is non-nil."

  (interactive)
  (save-restriction
    (diff-narrow-to-current-file)
    (let ((pos (or no-restore (diff-save-contextual-position))))
      ;; exchange --- and +++
      (goto-char (point-min))
      (unless
	  (looking-at
	   "^--- \\([^\n]+\\)\n\\+\\+\\+ \\([^\n]+\\)$")
	(error "---/+++ file heading not found"))
      (let ((a (match-string 1))
	    (b (match-string 2)))
	(kill-region (match-beginning 0) (match-end 0))
	(insert "--- " b "\n+++ " a))
      (goto-char (point-min))
      (while (diff-forward-hunk t)
	(save-restriction
	  (diff-narrow-to-current-hunk)
	  (goto-char (point-min))
	  (unless (looking-at
		   "^@@ +\\([^ \n]+\\) +\\([^ \n]+\\) @@$")
	    (error "Malformed hunk"))
	  (let ((a (match-string 1))
		(b (match-string 2)))
	    (kill-region (match-beginning 0) (match-end 0))
	    (insert "@@ " b " " a " @@"))
	  (goto-char (point-min))
	  ;; exchange + and -
	  (while (re-search-forward "^[-+]" nil t)
	    (replace-match
	     (if (equal (match-string 0) "+") "-" "+") t t))
	  (goto-char (point-min))
	  (diff-canonicalize-hunk)))
      (or no-restore
	  (diff-restore-contextual-position pos)))))

(defun diff-set-base-dir (base-dir)
  "Set the base directory for subsequent diff updates to BASE-DIR.

See also `diff-update-current-file'."
  (interactive "sBase directory for diff files: ")
  (make-variable-buffer-local 'base-dir)
  (setq diff-base-dir base-dir))

(defun diff-save-contextual-position (&optional context)
  "Save the current contextual position.

The optional argument argument CONTEXT is used to specify a different
number of contextual character instead of the default one (80)."
  (or context (setq context 80))
  (cons
   (buffer-substring (max (point-min) (- (point) context)) (point))
   (buffer-substring (point) (min (point-max) (+ (point) context)))))

(defun diff-restore-contextual-position (pos)
  "Restore the contextual position saved in POS."
  (let* ((a (car pos))
	 (b (cdr pos))
	 (la (length a))
	 (lb (length b))
	 (c (cond ((= la lb) (concat a b))
		  ((< la lb) (concat a (substring b 0 la)))
		  (t (concat (substring a (- la lb)) b))))
	 (m nil)
	 (n nil)
	 (p nil))
    (goto-char (point-min))
    (while (not (save-excursion (search-forward a nil t)))
      (setq a (let ((len (length a)))
		(substring a (- len (* (/ len 3) 2))))))
    (setq m (match-end 0))
    (goto-char (point-min))
    (while (not (save-excursion (search-forward c nil t)))
      (setq c (let* ((len (length c))
		     (d (* (/ len 3) 2))
		     (a (/ (- len d) 2)))
		(substring c a d))))
    (setq p (+ (match-beginning 0)
	       (/ (- (match-end 0) (match-beginning 0)) 2)))
    (goto-char (point-max))
    (while (not (save-excursion (search-backward b nil t)))
      (setq b (let ((len (length b)))
		(substring b 0 (* (/ len 3) 2)))))
    (setq n (match-beginning 0))
    (setq a (length a))
    (setq b (length b))
    (setq c (length c))
    (let ((r (max a b c)))
      (cond
       ((eq r c) (goto-char p) c)
       ((eq r a) (goto-char m) a)
       (t (goto-char n) b)))))

(defun diff-update-current-file (&optional diff-options base-dir
					   no-restore)
  "Update the current file patch.

The optional argument DIFF-OPTIONS is used to specify the options to
pass to diff to recreate the patch.  These options default to the one
specified in the line beginning with \"diff \" in the patch file,
when present, the ones in the variable `diff-default-options', or
\"-u\".

The base directory from which the diff file names are calculated is
specified by the optional argument BASE-DIR, or defaults to the value
of the variable `diff-base-dir' or current directory the patch file
resides in.  The interactive command `diff-set-base-dir' can be used
to set the default setting.

After the patch is recreated the point will tentatively be restored to
a position contextually similar the point position before the update,
unless the optional NO-RESTORE argument is non-nil."
  (interactive)
  (let ((pair (diff-current-file)))
    (unless pair (error "Cannot get the names of the files to compare"))
    (when (and (not base-dir) diff-base-dir)
      (setq base-dir diff-base-dir))
    (unless base-dir (setq base-dir default-directory))
    (let ((from (expand-file-name (car pair) base-dir))
	  (to (expand-file-name (cdr pair) base-dir)))
      ;;(unless (file-exists-p from) (error "%s does not exists" from))
      ;;(unless (file-exists-p to) (error "%s does not exists" to))
      (let ((options (or diff-options diff-default-options "-u")))
	(save-excursion
	  (beginning-of-line 2)
	  (diff-backward-file)
	  (beginning-of-line 0)
	  (and (looking-at
		"^diff +\\(.*\\) +\\([^ ]+\\) +\\([^ ]+\\) *$")
	       (let ((potential-options (match-string 1))
		     (file-name1 (match-string 2))
		     (file-name2 (match-string 3)))
		 (and
		  (string-match (concat (regexp-quote (car pair)) "$")
				file-name1)
		  (string-match (concat (regexp-quote (cdr pair)) "$")
				file-name2)
		  (setq options potential-options)))))
	(save-restriction
	  (diff-narrow-to-current-file)
	  (let ((pos (or no-restore (diff-save-contextual-position)))
		(default-directory base-dir))
	    (kill-region (point-min) (point-max))
	    (call-process "diff" nil (current-buffer) nil
			  options (car pair) (cdr pair))
	    (or no-restore
		(diff-restore-contextual-position pos))))))))

;;(defun diff-about-current-file ()
;;  (interactive)
;;  (message "%s" (cdr (diff-current-file))))

(defun diff-find-new-file ()
  "Edit the 'new file'."
  (interactive)
  (let ((pos (diff-save-contextual-position)))
    (find-file (expand-file-name (cdr (diff-current-file))
				 (or diff-base-dir
				     default-directory)))
    (diff-restore-contextual-position pos)))

(defun diff-find-old-file ()
  "Edit the 'old file'."
  (interactive)
  (let ((pos (diff-save-contextual-position)))
    (find-file (expand-file-name (car (diff-current-file))
				 (or diff-base-dir
				     default-directory)))
    (diff-restore-contextual-position pos)))

(provide 'diff)

;;; diff.el ends here
