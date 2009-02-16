;;; locdict.el --- Using a local Webster dictionary
;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <lmi@gnus.org>
;; Keywords: language

;; This file is not part of GNU Emacs.

;; locdict.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; locdict.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Usage:
;;
;; Put the following in your .emacs file
;;
;; (autoload 'locdict "locdict" nil t)
;;
;; and then use `M-x locdict RET word RET' to look up "word".
;;
;; You need to get the dictionary files from somewhere else --
;; I certainly wouldn't know a free source.

;;; Code:

(require 'widget)
(require 'wid-edit)

(defvar locdict-directory "/local/lib/webster/"
  "The directory where the dictionary resides.")

(defun locdict-create-alist ()
  (message "Building dictionary tables...")
  (let (alist)
    (save-excursion
      (dolist (file (directory-files locdict-directory t "\\.i$"))
	(with-temp-buffer
	  (insert-file-contents file)
	  (goto-char (point-min))
	  (while (eq (following-char) ?\t)
	    (forward-line 1))
	  (while (not (eobp))
	    (push
	     (cons (buffer-substring (point)
				     (progn (skip-chars-forward "^\t")
					    (point)))
		   (progn (read (current-buffer))))
	     alist)
	    (forward-line 1)))))
    (message "Building dictionary tables...done")
    alist))
	
(defvar locdict-alist nil)

(defun locdict (word &optional regexpp)
  "Look up WORD in the dictionary.
If REGEXPP (the prefix), look up all words matching a regexp."
  (interactive "sWord: \nP")
  (unless locdict-alist
    (setq locdict-alist (locdict-create-alist)))
  (let ((positions (locdict-positions word regexpp))
	initial lines)
    (unless positions
      (setq word (read-string "No such word.  Word: " word))
      (setq positions (locdict-positions word regexpp))
      (unless positions
	(error "No such word: %s" word)))
    (pop-to-buffer "*Dictionary*")
    (locdict-mode)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (dolist (pos positions)
	(setq initial (car pos))
	(let ((step 2048)
	      found)
	  (while (not found)
	    (with-temp-buffer
	      (insert-file-contents (expand-file-name initial locdict-directory)
				    nil (cdr pos) (+ (cdr pos) step))
	      (goto-char (point-min))
	      (if (not (re-search-forward "^F;" nil t))
		  (incf step 1024)
		(beginning-of-line)
		(setq found t)
		(narrow-to-region (point-min) (point))
		(goto-char (point-min))
		(insert "F;")
		(goto-char (point-min))
		(while (search-forward "#\n" nil t)
		  (replace-match " "))
		(goto-char (point-min))
		(while (not (eobp))
		  (push (split-string
			 (buffer-substring (point) (progn (end-of-line)
							  (point)))
			 ";")
			lines)
		  (forward-line 1)))))))
      (locdict-format (nreverse lines)))))

(defun locdict-positions (word rp)
  (if (not rp)
      (let ((pos (memq (assoc word locdict-alist) locdict-alist))
	    out initial)
	(string-match "[a-z]" word)
	(setq initial (match-string 0 word))
	(while (equal word (caar pos))
	  (push (cons initial (cdr (pop pos))) out))
	out)
    (let (out initial)
      (dolist (elem locdict-alist)
	(when (string-match word (car elem))
	  (string-match "[a-z]" (car elem))
	  (setq initial (match-string 0 (car elem)))
	  (push (cons initial (cdr elem)) out)))
      out)))

(defun locdict-format (lines)
  (let (last fill type)
    (erase-buffer)
    (dolist (line lines)
      (setq type (car line))
      (cond
       ((equal type "F")
	(unless (bobp)
	  (insert "\n"))
	(insert (nth 1 line))
	(beginning-of-line)
	(mapcar (lambda (c)
		  (forward-char (- (char-int c) (char-int ?0)))
		  (insert ".")) (nth 4 line))
	(end-of-line)
	(insert (format " (%s)\n" (nth 5 line))))
       ((equal type "P")
	(insert (format "(%s)\n" (nth 1 line))))
       ((equal type "E")
	(save-restriction
	  (narrow-to-region
	   (point)
	   (progn
	     (insert "Etymology: " (mapconcat 'identity (cdr line) ";"))
	     (point)))
	  (locdict-fontify)
	  (locdict-fill 3)))
       ((equal type "D")
	(save-restriction
	  (narrow-to-region
	   (point)
	   (progn
	     (if (or (equal last (nth 1 line))
		     (equal "0" (nth 1 line)))
		 (insert "   ")
	       (insert (nth 1 line) ") "))
	     (setq last (nth 1 line))
	     (setq fill 3)
	     (unless (equal (nth 2 line) "")
	       (insert (nth 2 line) ") ")
	       (setq fill 6))
	     (insert (nth 4 line) ", ")
	     (insert (nth 5 line) "\n")
	     (point)))
	  (locdict-fontify)
	  (locdict-fill fill)))
       ((equal type "R")
	(insert (nth 1 line))
	(beginning-of-line)
	(mapcar (lambda (c)
		  (forward-char (- (char-int c) (char-int ?0)))
		  (insert ".")) (nth 2 line))
	(beginning-of-line)
	(insert " -- ")
	(end-of-line)
	(insert (format ", %s\n" (nth 3 line)))))))
  (goto-char (point-min)))

(defun locdict-fontify ()
  (goto-char (point-min))
  (while (re-search-forward "\\[\\([^ ]+\\) \\([^ ]+\\)\\]" nil t)
    (save-restriction
      (narrow-to-region (match-beginning 0) (match-end 0))
      (let ((type (match-string 1))
	    (word (match-string 2)))
	(delete-region (point-min) (point-max))
	(insert word)
	(put-text-property (point-min) (point-max) 'face 
			   (cond
			    ((equal type "italic")
			     'italic)
			    ((equal type "mini")
			     (downcase-region (point-min) (point-max))
			     (widget-convert-button
			      'link (point-min) (point-max)
			      :action 'locdict-press-button
			      :help-echo 
			      (lambda (widget)
				"Look up the word"))
			     'bold)
			    ((equal type "sup")
			     'bold)
			    ((equal type "sub")
			     'bold)
			    (t
			     (error "Unknown type %s" type))))))))

(defun locdict-fill (length)
  (let ((fill-prefix (make-string length ? )))
    (fill-region (point-min) (point-max))))

;;;
;;; Locdict mode
;;;

(defvar locdict-mode-map nil)
(unless locdict-mode-map
  (setq locdict-mode-map (make-sparse-keymap))
  (define-key locdict-mode-map "\r" 'locdict-fetch))
(set-keymap-parent locdict-mode-map widget-keymap)
  
(defun locdict-make-menu-bar ()
  (unless (boundp 'locdict-menu)
    (easy-menu-define
     locdict-menu locdict-mode-map ""
     '("Locdict"
       ["Fetch" locdict-fetch (locdict-filename)]))))

(defun locdict-mode ()
  "Major mode for displaying dictionary definitions.

\\{locdict-mode-map}"
  (interactive)
  (buffer-disable-undo (current-buffer))
  (locdict-make-menu-bar)
  (kill-all-local-variables)
  (setq major-mode 'locdict-mode)
  (setq mode-name "Dictionary")
  (setq mode-line-process nil)
  (use-local-map locdict-mode-map)
  (setq buffer-read-only t)
  (run-hooks 'locdict-mode-hook))

(defun locdict-fetch (word)
  "Look up the word under point."
  (interactive (list (locdict-filename)))
  (if word
      (locdict word)
    (error "No word under point")))

(defun locdict-push (e)
  "Look up the word under the mouse pointer."
  (interactive "e")
  (mouse-set-point e)
  (locdict-fetch (locdict-filename)))
 
(defun locdict-filename ()
  "Return the name of the file under point."
  (save-excursion
    (buffer-substring
     (progn
       (skip-chars-backward "[a-z]")
       (point))
     (progn
       (skip-chars-forward "[a-z]")
       (point)))))

(defun locdict-press-button (elems el)
  (goto-char (widget-get elems :from))
  (locdict-fetch (locdict-filename)))

;;; locdict.el ends here
