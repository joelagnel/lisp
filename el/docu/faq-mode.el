;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; faq-mode.el --- 
;; 
;; Filename: faq-mode.el
;; Description: Minor mode for editing faq-like page.
;; Author: Yu Li
;; Maintainer: 
;; Created: Fri Dec 31 10:19:24 2004
;; Version: $Id$
;; Last-Updated: Mon Jan 03 10:13:25 2005
;;           By: Yu Li
;; Compatibility: Emacs21
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.
;;
;; ----------
;; Motivation
;; ----------
;;
;; Maintaining a faq page is not a easy work, especially when this faq
;; page is growing. You have to corret mistakes, re-order the questions
;; and so on. When the page growing, a index in the head of page is
;; necessary, which is just the source of all maintainning problems.
;;
;; Edit the faq page by using EmacsWiki is a good idea. EmacsWiki make you
;; feel like writing but not programming(that was what I think of editing
;; HTML page). In other ways, faq page is something predicable and what
;; EmacsWiki do not have is some key-bindings and indexing facilities. I
;; write this minor mode to solve that problem and to share my experience
;; in editing and maintaining a faq pages.
;; 
;; -----------------
;; How does it work?
;; -----------------
;; 
;; faq-mode work on entry templates. It assume that each entry in faq page
;; follow a prior template(can be configured in `faq-entry-template'). The
;; template consisit of there part:anchor, title and body. By search according
;; to the regexp of anchor or title, all entries can be easily found. Thus
;; we can apply some replacing and formating magics on it.
;;  
;; -----
;; Usage
;; -----
;; 
;; faq-mode is provided as minor mode of EmacsWiki. Place
;;
;;     (require 'faq-mode)
;;
;; in your .emacs file to load it. M-x faq-mode to toggle it.
;; 
;; faq-mode need not very special configuration in your .emacs. Default it
;; have these key bindings
;;
;;     C-c C-q C-i    Insert a new faq entry
;;     C-c C-q n      Move cursor to next faq entry
;;     C-c C-q p      Move cursor to previous faq entry
;;     C-c C-q C-a    Automatic index all faq entries
;;     C-c C-q C-m    Automatic index all faq entries and generate a link list
;;
;; try these keys in emacs-wiki page:)
;;
;; NOTE: Defaultly C-c C-q C-m will generate a link list at point or between
;;
;;     <!-- faq index begin -->
;;                             |<- Index will be generated here.
;;     <!-- faq index end -->
;;
;; so place above 2 comment lines to right place as your wish.
;;
;; Faq entry template and other variables can also be configurated in wiki
;; page. Here is a example
;;
;;     <!-- faq-mode configuration in page
;;     faq-entry-template:#faq
;;     faq-entry-template:**Q:** 
;;     faq-entry-template:
;;     faq-entry-template:<blockquote>
;;     faq-entry-template:**A:** 
;;     faq-entry-template:</blockquote>
;;     faq-entry-template:<hr style="clear:left;">
;;     //faq-mode configuration in page -->
;;
;; place above HTML comment fragment in your wiki page's foot and when you
;; open that file faq-mode will discover it. After toggle faq minor mode on,
;; it will read variable's configuration and configurate it. Variable
;; configuration is in form
;;
;;     NAME:VALUE
;;
;; and all page configurable variables are
;;
;;     `faq-entry-template' `faq-anchor-regexp' `faq-title-regexp'
;;     `faq-auto-index-format' `faq-make-index-format'
;;
;; NOTE: Configurating `faq-entry-template' is something different, in which
;; each line configuration represent one line and all configuration will be 
;; `concat' together.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; RCS $Log$
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(defvar faq-entry-template "#faq
**Q:** 

<blockquote>
**A:** 
</blockquote>
"
  "This is the entry template and it will be inserted in `faq-insert-entry'.
It consist of three parts: anchor, title and body.")

(defvar faq-anchor-regexp "\\(^#faq.*\\)"
  "Used in function `faq-find-entry' and refer to it for detail. It should
match with `faq-auto-index-format', so also see description of it.")

(defvar faq-title-regexp "^\\*\\*Q:\\*\\* \\(.*\\)"
  "Used in function `faq-find-entry' and refer to it for detail.")

(defvar faq-auto-index-format "#faq%d"
  "This is the format string used in `faq-auto-index'. %d will be filled
with faq index number. You can configure it to be what you prefer, but make
sure the string generated according it match `faq-anchor-regexp'.")

(defvar faq-make-index-format "- [[EmacsWikiZhFAQ#faq%d][%s]]\n"
  "This is the format string used in `faq-make-index'. %d will be filled
with faq index number and %s will be filled with title text. You can
configure it to be what you prefer.")

(defvar faq-var-config-map
  (list
   ["faq-entry-template"
    (lambda (value)
      (setq faq-entry-template (concat faq-entry-template "\n" value)))]

   ["faq-anchor-regexp"
    (lambda (value)
      (setq faq-anchor-regexp value))]

   ["faq-title-regexp"
    (lambda (value)
      (setq faq-title-regexp value))]

   ["faq-auto-index-format"
    (lambda (value)
      (setq faq-auto-index-format value))]

   ["faq-make-index-format"
    (lambda (value)
      (setq faq-make-index-format value))]
   ))

(defun faq-configurate-var (name value)
  (dolist (entry faq-var-config-map)
    (if (string= (elt entry 0) name)
	(funcall (elt entry 1) value))))

(defun faq-find-entry ()
  "faq-find-entry find all entries in current buffer. Entry is detected by
matching with `faq-anchor-regexp' and `faq-title-regexp'. It will return a
list with each element of the form

   ((LINE-NUMBER . ANCHOR-TEXT) . (LINE-NUMBER . TITLE-TEXT))

ANCHOR-TEXT is the matched string of `faq-anchor-regexp' and LINE-NUMBER is
the corresponding number get by `line-number-at-pos'. So as LINE-NUMBER and 
TITLE-TEXT."
  (let* ((entry-alist nil))
    (save-excursion
      (with-current-buffer (current-buffer)
	(goto-char (point-min))
	(let ((p (search-forward-regexp faq-anchor-regexp (point-max) t))
	      (anchor nil)
	      (title nil))
	  (while p
	    (setq anchor (cons (line-number-at-pos (match-beginning 1))
			       (buffer-substring-no-properties
				(match-beginning 1) (match-end 1))))
	    (search-forward-regexp faq-title-regexp (point-max) t)
	    (setq title (cons (line-number-at-pos (match-beginning 1))
			      (buffer-substring-no-properties
			       (match-beginning 1) (match-end 1))))
	    (setq entry-alist
		  (cons (cons anchor (cons title nil)) entry-alist))
	    (setq p (search-forward-regexp faq-anchor-regexp (point-max) t))))))
    (nreverse entry-alist)))

(defun faq-auto-index ()
  "Find all entries and update their anchors with automatic generated
index number."
  (interactive)
  (let ((entry-alist (faq-find-entry))
	(count 1))
    (save-excursion
      (with-current-buffer (current-buffer)
	(dolist (entry entry-alist)
	    (goto-line (caar entry))
	    (delete-region (line-beginning-position) (line-end-position))
	    (insert (format faq-auto-index-format count))
	    (setq count (+ count 1)))))
    ))

(defun faq-make-index ()
  "Find all entries and automatic generate a index. Default it will insert
index between '<!-- faq index begin -->' and '<!-- faq index end -->'. That
is

<!-- faq index begin -->
                        |<- Index will be generated here.
<!-- faq index end -->

. If search for comment failed, it will generate index at current position.:)
"
  (interactive)
  (faq-auto-index)
  (let* ((region nil))
    (save-excursion
      (with-current-buffer (current-buffer)
	(goto-char (point-min))
	(let ((comment-begin nil)
	      (comment-end nil))
	  (if (search-forward "<!-- faq index begin -->" (point-max) t)
	      (progn (setq comment-begin (line-beginning-position))
		     (if (search-forward "<!-- faq index end -->" (point-max) t)
			 (progn (setq comment-end (line-end-position))
				(setq region
				      (cons comment-begin
					    (cons comment-end nil))))))))))

    (let ((entry-alist (faq-find-entry))
	  (count 1))
      (with-current-buffer (current-buffer)
	(if region
	    (progn (goto-char (car region))
		   (delete-region (car region) (cadr region))
		   (insert "<!-- faq index begin -->\n")))
	(dolist (entry entry-alist)
	  (insert (format faq-make-index-format count (cdr (cadr entry))))
	  (setq count (+ count 1)))
	(if region
	    (insert "<!-- faq index end -->\n")))
	)))

(defun faq-insert-entry ()
  "Insert a entry according to template at current point."
  (interactive)
  (insert faq-entry-template))

(defun faq-next-entry ()
  "Move cursor to next entry."
  (interactive)
  (if (not (search-forward-regexp faq-title-regexp (point-max) t))
      (message "Search failed. This may be the last entry")))

(defun faq-previous-entry ()
  "Move cursor to previous entry."
  (interactive)
  (if (not (search-backward-regexp faq-title-regexp (point-min) t))
      (message "Search failed. This may be the first entry")))

(defun faq-read-page-configuration ()
  "Check configuration in page and enable faq-mode automatically."
  (save-excursion
    (let* ((config-begin nil)
	   (config-end nil)
	   (config nil)
	   (config-line nil)
	   (template-set nil))
      (goto-char (point-max))
      (if (search-backward "//faq-mode configuration in page -->" (point-min) t)
	  (progn (setq config-end (line-number-at-pos))
		 (if (search-backward "<!-- faq-mode configuration in page"
				      (point-min) t)
		     (progn (setq config-begin (+ 1 (line-number-at-pos)))
			    (setq config t)))))
      (if (and config (string= (symbol-name major-mode) "emacs-wiki-mode"))
	  (progn (faq-mode t)
		 (while (< config-begin config-end)
		   (goto-line config-begin)
		   (setq config-line
			 (buffer-substring-no-properties
			  (line-beginning-position)
			  (line-end-position)))
		   (let ((index nil))
		     (setq index (string-match ":" config-line))
		     (if (and (string= (substring config-line 0 index)
				       "faq-entry-template")
			      (not template-set))
			 (progn (setq faq-entry-template "")
				(setq template-set t)))
		     (if index
			 (faq-configurate-var
			  (substring config-line 0 index)
			  (substring config-line (+ index 1)))))
		   (setq config-begin (+ config-begin 1))))
	))))

(add-hook 'emacs-wiki-mode-hook 'faq-read-page-configuration)

(define-minor-mode faq-mode
  "Toggle Faq mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Faq mode is enabled, you can easily manipulate
faqs in a wiki page. See comment in faq-mode.el for
detail."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Faq"
  ;; The minor mode bindings.
  '(("\C-c\C-qn" . faq-next-entry)
    ("\C-c\C-qp" . faq-previous-entry)
    ("\C-c\C-q\C-i" . faq-insert-entry)
    ("\C-c\C-q\C-a" . faq-auto-index)
    ("\C-c\C-q\C-m" . faq-make-index))
  :group 'faq-mode)

(provide 'faq-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; faq-mode.el ends here
