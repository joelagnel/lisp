;; c-comment-edit.el --- edit C comments
;; Copyright (C) 1987, 1988, 1989, 1998 Kyle E. Jones

;; Author: Kyle Jones <kyle_jones@wonderworks.com>
;; Maintainer: Kyle Jones <kyle_jones@wonderworks.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to kyle_jones@wonderworks.com)
;; or from the Free Software Foundation, Inc., 675 Mass Ave, Cambridge,
;; MA 02139, USA.

;;; Synched up with: Not in FSF.

;;; Commentary:

;; Send bug reports to kyle_jones@wonderworks.com

;; The command c-comment-edit is the entry point for this library.

(provide 'c-comment-edit)

(defvar c-comment-edit-version "1.02"
  "Version number for the C Comment Edit package.")

(defvar c-comment-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\e" 'c-comment-edit-end)
    (define-key map "\C-c\C-c" 'c-comment-edit-end)
    (define-key map "\C-c\C-]" 'c-comment-edit-abort)
    map )
  "Keymap for c-comment-edit buffers")

(defvar c-comment-leader " *"
  "*Leader used when rebuilding edited C comments.  The value of this variable
should be a two-character string.  Values of \"  \", \" *\" and \"**\" produce
the comment styles:
	/*	/*	/*
	  ...	 * ...	** ...
	  ...	 * ...	** ...
	*/	 */	*/
respectively.")

(defconst c-comment-leader-regexp "^[ 	]*\\(\\*\\*\\|\\*\\)?[ ]?"
  "Regexp used to match C comment leaders.")

(defvar c-comment-edit-mode 'text-mode
  "*Major mode used by `c-comment-edit' when editing C comments.")

(defvar c-comment-edit-hook nil
  "*Function to call whenever `c-comment-edit' is used.
The function is called just before the `c-comment-edit' function allows you to
begin editing the comment.")

(defvar c-comment-edit-buffer-alist nil
  "Assoc list of C buffers and their associated comment buffers.
Elements are of the form (C-BUFFER COMMENT-BUFFER COMMENT-START COMMENT-END)
COMMENT-START and COMMENT-END are markers in the C-BUFFER.")

(defmacro save-point (&rest body)
  "Save value of point, evalutes FORMS and restore value of point.
If the saved value of point is no longer valid go to (point-max).
The variable `save-point' is lambda-bound to the value of point for
the duration of this call."
  (list 'let '((save-point (point)))
	(list 'unwind-protect
	      (cons 'progn body)
	      '(goto-char (min (point-max) save-point)))))

(defmacro marker (pos &optional buffer)
  (list 'set-marker '(make-marker) pos buffer))

;;;###autoload
(defun c-comment-edit (search-prefix)
  "Edit multi-line C comments.
This command allows the easy editing of a multi-line C comment like this:
   /*
    * ...
    * ...
    */
The comment may be indented or flush with the left margin.

If point is within a comment, that comment is used.  Otherwise the
comment to be edited is found by searching forward from point.

With one \\[universal-argument] searching starts after moving back one
  paragraph.
With two \\[universal-argument]'s searching starts at the beginning of the
  current or proceeding C function.
With three \\[universal-argument]'s searching starts at the beginning of the
  current page.
With four \\[universal-argument]'s searching starts at the beginning of the
  current buffer (clipping restrictions apply).

Once located, the comment is copied into a temporary buffer, the comment
leaders and delimiters are stripped away and the resulting buffer is
selected for editing.  The major mode of this buffer is controlled by
the variable `c-comment-edit-mode'.\\<c-comment-edit-map>

Use \\[c-comment-edit-end] when you have finished editing the comment.  The
comment will be inserted into the original buffer with the appropriate
delimiters and indention, replacing the old version of the comment.  If
you don't want your edited version of the comment to replace the
original, use \\[c-comment-edit-abort]." 
  (interactive "*P")
  (let ((c-buffer (current-buffer))
	marker tem c-comment-fill-column c-comment-buffer
	c-comment-start c-comment-end
	(inhibit-quit t))
    ;; honor search-prefix
    (cond ((equal search-prefix '(4))
	   (backward-paragraph))
	  ((equal search-prefix '(16))
	   (end-of-defun)
	   (beginning-of-defun)
	   (backward-paragraph))
	  ((equal search-prefix '(64))
	   (backward-page))
	  ((equal search-prefix '(256))
	   (goto-char (point-min))))
    (if (and (null search-prefix) (setq tem (within-c-comment-p)))
	(setq c-comment-start (marker (car tem))
	      c-comment-end (marker (cdr tem)))
      (let (start end)
	(condition-case error-data
	    (save-point
	      (search-forward "/*")
	      (setq start (- (point) 2))
	      (search-forward "*/")
	      (setq end (point)))
	  (search-failed (error "No C comment found.")))
	(setq c-comment-start (marker start))
	(setq c-comment-end (marker end))))
    ;; calculate the correct fill-column for the comment
    (setq c-comment-fill-column (- fill-column 3
				   (save-excursion
				     (goto-char c-comment-start)
				     (current-column))))
    ;; create the comment buffer
    (setq c-comment-buffer
	  (generate-new-buffer (concat (buffer-name) " *C Comment Edit*")))
    ;; link into the c-comment-edit-buffer-alist
    (setq c-comment-edit-buffer-alist
	  (cons (list (current-buffer) c-comment-buffer
		      c-comment-start c-comment-end)
		c-comment-edit-buffer-alist))
    ;; copy to the comment to the comment-edit buffer
    (copy-to-buffer c-comment-buffer (+ c-comment-start 2) (- c-comment-end 2))
    ;; mark the position of point, relative to the beginning of the
    ;; comment, in the comment buffer.  (iff point is within a comment.)
    (or search-prefix (< (point) c-comment-start)
	(setq marker (marker (+ (- (point) c-comment-start 2) 1)
			     c-comment-buffer)))
    ;; select the comment buffer for editing
    (switch-to-buffer c-comment-buffer)
    ;; remove the comment leaders and delimiters
    (goto-char (point-min))
    (while (not (eobp))
      (and (re-search-forward c-comment-leader-regexp nil t)
	   (replace-match "" nil t))
      (forward-line))
    ;; run appropriate major mode
    (funcall (or c-comment-edit-mode 'fundamental-mode))
    ;; override user's default fill-column here since it will lose if
    ;; the comment is indented in the C buffer.
    (setq fill-column c-comment-fill-column)
    ;; delete one leading whitespace char
    (goto-char (point-min))
    (if (looking-at "[ \n\t]")
	(delete-char 1))
    ;; restore cursor if possible
    (goto-char (or marker (point-min)))
    (if (fboundp 'set-keymap-parents)
	(set-keymap-parents c-comment-edit-map (list (current-local-map))))
    (use-local-map c-comment-edit-map)
    (set-buffer-modified-p nil))
  ;; run user hook, if present.
  (if c-comment-edit-hook
      (funcall c-comment-edit-hook))
  ;; final admonition
  (message
   (substitute-command-keys
    "Type \\[c-comment-edit-end] to end edit, \\[c-comment-edit-abort] to abort with no change.")))

(defun c-comment-edit-end ()
  "End c-comment-edit.
C comment is replaced by its edited counterpart in the appropriate C buffer.
Indentation will be the same as the original."
  (interactive)
  (let ((tuple (find-c-comment-buffer)))
    (if (null tuple)
	(error "Not a c-comment-edit buffer."))
    (let ((inhibit-quit t)
	  (c-comment-c-buffer (car tuple))
	  (c-comment-buffer (nth 1 tuple))
	  (c-comment-start (nth 2 tuple))
	  (c-comment-end (nth 3 tuple)))
      (cond
       ((buffer-modified-p)
	;; rebuild the comment
	(goto-char (point-min))
	(insert "/*\n")
	(if (string= c-comment-leader "  ")
	    (while (not (eobp))
	      (if (not (eolp))
		  (insert c-comment-leader " "))
	      (forward-line))
	  (while (not (eobp))
	    (insert c-comment-leader (if (eolp) "" " "))
	    (forward-line)))
	(if (not (char-equal (preceding-char) ?\n))
	    (insert "\n"))
	(insert (if (string= c-comment-leader " *") " */" "*/"))
	;; indent if necessary
	(let ((indention
	       (save-excursion
		 (set-buffer c-comment-c-buffer)
		 (goto-char c-comment-start)
		 (current-column))))
	  (goto-char (point-min))
	  (cond ((not (zerop indention))
		 ;; first line is already indented
		 ;; in the C buffer
		 (forward-line)
		 (while (not (eobp))
		   (indent-to indention)
		   (forward-line)))))
	;; replace the old comment with the new
	(save-excursion
	  (set-buffer c-comment-c-buffer)
	  (save-point
	    (save-excursion
	      (delete-region c-comment-start c-comment-end)
	      (goto-char c-comment-start)
	      (set-buffer c-comment-buffer)
	      (append-to-buffer c-comment-c-buffer
				(point-min) (point-max))))))
       (t (message "No change.")))
      ;; switch to the C buffer
      (if (get-buffer-window c-comment-c-buffer)
	  (select-window (get-buffer-window c-comment-c-buffer))
	(switch-to-buffer c-comment-c-buffer))
      ;; delete the window viewing the comment buffer
      (and (get-buffer-window c-comment-buffer)
	   (delete-window (get-buffer-window c-comment-buffer)))
      ;; unlink the tuple from c-comment-edit-buffer-alist
      (setq c-comment-edit-buffer-alist
	    (delq tuple c-comment-edit-buffer-alist))
      ;; let Emacs reclaim various resources
      (save-excursion
	(set-buffer c-comment-buffer)
	(set-buffer-modified-p nil)
	(kill-buffer c-comment-buffer))
      (set-marker c-comment-start nil)
      (set-marker c-comment-end nil))))

(defun c-comment-edit-abort ()
  "Abort a c-comment-edit with no change."
  (interactive)
  (let* ((tuple (find-c-comment-buffer))
	 (c-comment-c-buffer (car tuple))
	 (c-comment-buffer (nth 1 tuple))
	 (c-comment-start (nth 2 tuple))
	 (c-comment-end (nth 3 tuple)))
    (if (null tuple)
	(error "Not a c-comment-edit buffer."))
    ;; switch to the C buffer
    (if (get-buffer-window c-comment-c-buffer)
	(select-window (get-buffer-window c-comment-c-buffer))
      (switch-to-buffer c-comment-c-buffer))
    (let ((inhibit-quit t))
      (save-excursion
	(set-buffer c-comment-buffer)
	(set-buffer-modified-p nil)
	(kill-buffer c-comment-buffer))
      ;; unlink the tuple from c-comment-edit-buffer-alist
      (setq c-comment-edit-buffer-alist
	    (delq tuple c-comment-edit-buffer-alist))
      (set-marker c-comment-start nil)
      (set-marker c-comment-end nil)
      (message "Aborted with no change."))))

;; this loses on /* /* */ but doing it right would be grim.
(defun within-c-comment-p ()
  (condition-case error-data
      (let (start end)
	(save-point
	  (search-backward "/*")
	  (setq start (point))
	  (search-forward "*/")
	  (setq end (point)))
	(if (< (point) end) (cons start end) nil))
    (search-failed nil)))

(defun find-c-comment-buffer (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (let ((list c-comment-edit-buffer-alist))
    (catch 'return-value
      (while list
	(if (eq (nth 1 (car list)) buffer)
	    (throw 'return-value (car list))
	  (setq list (cdr list)))))))
	    
(defun find-c-comment-c-buffer (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (let ((list c-comment-edit-buffer-alist))
    (catch 'return-value
      (while list
	(if (eq (car (car list)) buffer)
	    (throw 'return-value (car list))
	  (setq list (cdr list)))))))

;; c-comment.el ends here
