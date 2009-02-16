;;; html-toc.el creates a table-of-contents on a html-document

;; Copyright (c) 2001 Rolf Rander Næss

;; Author:   Rolf Rander Næss <rolfn@pvv.org>
;; Created:  17-Mar-2001
;; Version:  0.3
;; Keywords: html
;; X-URL:    http://www.pvv.org/~rolfn/html-toc.el

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.  This is distributed in the hope that it will be
;; useful, but without any warranty; without even the implied warranty
;; of merchantability or fitness for a particular purpose.  See the
;; GNU General Public License for more details.  You should have
;; received a copy of the GNU General Public License along with GNU
;; Emacs; see the file `COPYING'.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
;; 02111-1307, USA.

;; Brief users guide:
;;
;;   This package will create a table-of-contents in a HTML-document
;;   based on <H[123]> tags.  The toc will be placed between the
;;   strings defined by *toc-open* and *toc-close*.  If these doesn't
;;   exist, it will be placed right after <body>.  If no <body>-tag
;;   exists, it will be put right before the first header.

;; Known bugs:
;;
;; - strange things will happen if your headers aren't valid (lack a
;;   close-tag), but in this case your page will also look strange in a
;;   browser...

(defvar html-toc-head-open  "<[ \t\n]*[Hh]\\([1-3]\\)[^>]*>")
(defvar html-toc-head-close "<[ \t\n]*/[ \t\n]*[Hh][1-3][ \t\n]*>")
(defvar html-toc-a-name     "<A NAME=\"\\([^\"]*\\)\"></A>")
(defvar html-toc-open       "<!-- table of contents start -->")
(defvar html-toc-close      "<!-- table of contents end -->")
(defvar html-toc-body-open  "<[ \t\n]*[Bb][Oo][Dd][Yy][ \t\n]*>")
(defvar html-toc-list-open  "<UL>")
(defvar html-toc-list-close "</UL>")
(defvar html-toc-list-item  "<LI>")
(defvar html-toc-name-pre   "tocref")
(defvar html-toc-title      "Table of Contents")
(defvar html-toc-tocref     (concat "<A NAME=\"" html-toc-name-pre
				    "\\([0-9]*\\)\">"))

(defun html-toc-find-position ()
  (goto-char (point-min))
  (if (search-forward html-toc-open nil t)
      (let ((toc-start (point)))
	(if (search-forward html-toc-close nil t)
	    (delete-region toc-start (match-beginning 0))
	  (insert html-toc-close))
	(goto-char toc-start)
	(insert "\n"))
    (progn (goto-char (point-min))
	   (if (not (search-forward-regexp html-toc-body-open nil t))
	       (progn (goto-char (point-min))
		      (search-forward-regexp html-toc-head-open nil t)
		      (goto-char (match-beginning 0))))
	   (insert html-toc-open "\n")
	   (let ((p (point)))
	     (insert html-toc-close "\n\n")
	     (goto-char p)))))


(defun html-toc-find-max ()
  (goto-char (point-min))
  (let ((max-toc 0))
    (while (search-forward-regexp html-toc-tocref nil t)
      (if (> (string-to-int (match-string 1)) max-toc)
	  (setq max-toc (string-to-int (match-string 1)))))
    (1+ max-toc)))

(defun html-toc-build ()
  (let ((toc '())
	(toc-cnt (html-toc-find-max)))
    (goto-char (point-min))
    (while (search-forward-regexp html-toc-head-open nil t)
      (let* ((level (string-to-int (match-string 1)))
	     (name (cond ((looking-at html-toc-a-name)
			  (goto-char (match-end 0))
			  (match-string 1))
			 (t (let ((n (concat html-toc-name-pre
					     (int-to-string toc-cnt))))
			      (insert "<A NAME=\"" n "\"></A>")
			      (setq toc-cnt (1+ toc-cnt))
			      n))))
	     (head-start (point)))
	(search-forward-regexp html-toc-head-close nil t)
	(setq toc (cons (list level
			      name 
			      (buffer-substring head-start
						(match-beginning 0)))
			toc))))
    (nreverse toc)))

(defun html-toc-aref (name text)
  (concat "<A HREF=\"#" name "\">" text "</A>"))

(defun html-toc-write-level (toc cur-level)
  (if toc
      (let* ((entry (car toc))
	     (level (car entry))
	     (name (cadr entry))
	     (text (cadr (cdr entry)))
	     (rest (cdr toc)))
	(cond ((> level cur-level)
	       (insert html-toc-list-open "\n")
	       (setq rest (html-toc-write-level toc (1+ cur-level)))
	       (insert html-toc-list-close "\n")
	       (html-toc-write-level rest cur-level))
	      ((= level cur-level)
	       (insert html-toc-list-item (html-toc-aref name text) "\n")
	       (html-toc-write-level rest cur-level))
	      ((< level cur-level)
	       toc)))))

(defun html-toc-write (toc)
  (insert "<H1><A NAME=\"toc\"></A>" html-toc-title "</H1>\n")
  (html-toc-write-level toc 0))

(defun html-toc ()
  (interactive)
  (save-excursion
    (html-toc-find-position)
    (let* ((toc-pos (point))
	   (toc (html-toc-build)))
      (goto-char toc-pos)
      (html-toc-write toc))))

(provide 'html-toc)
