;;; *0 peano.el --- Giuseppe Peano-style document structure\                              

;;  _ __   ___  __ _ _ __   ___  
;; | '_ \ / _ \/ _` | '_ \ / _ \ 
;; | |_) |  __/ (_| | | | | (_) |
;; | .__/ \___|\__,_|_| |_|\___/ 
;; |_|                           

;; Copyright (C) 2007  Dave O'Toole

;; Author: Dave O'Toole <dto@gnu.org>
;; Package-Version: 0.01
;; Version: $Id: peano.el,v 1.1 2007/09/20 20:35:01 dto Exp dto $
;; Time-stamp: <2007-09-22 00:01:36 dto>
;; Keywords: outlines, convenience, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; *1 Preface

;; Giuseppe Peano (1858-1932) was a mathematician and philosopher.
;; The subject of this essay is not really him, but rather one in
;; particular of his many notational ideas. 

;; If you want to learn more about Peano himself, I suggest starting
;; with the wikipedia page about him:

;; http://en.wikipedia.org/wiki/Giuseppe_Peano

;;; *1.1 Status

;; Pre-alpha release.

;;; *2 Overview

;; Peano was a prolific writer of mathematical papers and books. In
;; order to keep everything organized, he invented a universal scheme
;; for numbering and indexing the sections, paragraphs, formulae,
;; figures, and other components of an arbitrarily long and complex
;; document. His system was later adopted by Bertrand Russell and
;; Alfred North Whitehead for their "Principia Mathematica", and also
;; by Wittgenstein for his 1921 "Tractatus Logico-Philisophicus".

;;; *2.1 

;; This file implements an Emacs minor mode that can automatically
;; number, cross-reference, and browse any kind of plain-text document
;; in an extended version of the Principia Mathematica addressing
;; system. Optional font-locking is provided for the notation.

;;; *2.2

;; This file is (as you may already suspect) organized according to
;; the system it implements. To try it out, evaluate this file or put
;; it in your load-path and do

;; (require 'peano)

;; Then do M-x peano-mode RET to view the source code.

;;; *3 Document Structure

;; A document is a plain text file divided into short "blocks". A
;; block consists of a formatted header line, followed by zero or more
;; lines of any text whatsoever. Blank lines in the file are ignored.

;;; *3.1 Block Numbers

;; Every block is assigned a unique decimal number such as 3.1 or
;; 1.256. Unlike the typical LaTeX numbering system, in which creating
;; a new section can cause subsequent section numbers to change, here
;; we use decimals so that it is always possible to insert new blocks
;; between others without changing their numbers. 

;;; *3.11

;; This last step is important; we can annotate any block by inserting
;; a new block immediately following the first, and annotate the
;; annotations ad infinitum. Furthermore we can use the number as a
;; unnique and permanent identifier for the block. This simplifies the
;; cross-referencing of documents and imposes a consistent structure.

;;; *3.2 Header Formatting

;; A header line consists of a prefix string, followed by a block
;; number, and then finally an optional label. All three are
;; separated from one another by single spaces. The entire header line
;; is part of the block. 

;;; *3.21 Prefix String

;; The prefix string may be any string matching the local
;; `outline-regexp'; this makes it easy to use `orgstruct-mode' to
;; obtain folded overviews, navigation, and structure editing.

;;; *3.22 Block Number Formatting

;; Block numbers are always written in base-10, prefixed with an
;; asterisk, like this: *7.2 The decimal point may be ommitted in the
;; case of whole numbers.

;;; *3.23 Labels

;; The label extends to the end of the header line. Labels are
;; subject to change, and therefore do not identify blocks---only the
;; numbers do.

;;; *3.3 Chapters

;; The only other unit of document organization provided by this
;; system is the "chapter". A chapter is a series of blocks whose
;; numbers are in increasing order. The integer part of a block
;; number identifies the chapter it belongs to. We write *1 to refer
;; to the first chapter, *2 for the second, and so on.

;;; *3.4 Notes

;; Notes and other optional readings are written in-line, and are
;; marked with two asterisks before the number instead of one. There
;; are no footnotes or endnotes of any kind.

;;; *3.5 Appendices

;; Numbers of appendices are prefixed with *A instead of * or **. All
;; the *A numbers must follow all the * and ** numbers; this ensures
;; that a document's body can grow without limit using all available
;; numbers, while still keeping all the appendices at the end of the
;; document.

;;; *3.6 Preamble

;; Any text before the first block in a file is ignored. 

;;; *3.7 Postamble

;; After the appendices, the optional last section in a document is
;; called *Z. A header line whose number is *Z begins a block
;; containing the rest of the file. 

;;; *3.71

;; So *0 and *Z are the like the covers of a book, and are also useful
;; when you need to put some text at the beginning or at the end of a
;; file for technical reasons.

;;; **3.99 Additional Conventions

;; These are optional.

;;; **3.991 

;; Chapter *0 should contain authorship and copyright information, and
;; any needed tags (such as those used in the Emacs Lisp coding
;; standards.) The label should be of the form:

;;     filename.extension --- short description of contents

;;; **3.992

;; Chapter *1 should introduce the work to the reader and provide an
;; overview of its contents. Within *1, The following block names are
;; standardized:
;;
;;  Overview
;;  Conventions
;;  History
;;  Status
;;  Installation
;;  Roadmap
;;  
;; Other names are ignored.

;;; **3.993

;; Chapter *Z may contain notes, local variables for Emacs, or other
;; text.

;;; **3.994 

;; In a living document there are always things to do. The string
;; "TODO " makes the rest of the line into the text of a todo-list
;; item. Other such "annotations" may be added in the future.


;;; *4 Requirements

(require 'cl)
(require 'rx)
(require 'org)
(require 'font-lock)

;;; *5 Headers

;;; *5.1 Syntax

(make-variable-buffer-local
 (defvar peano-header-regexp "" "Regular expression matching headers."))

(defun peano-set-header-regexp ()
  "Set `peano-header-regexp' to its canonical value." 
  (setf peano-header-regexp
	(rx (sequence space "*" 
		      ;; 1. type symbol
		      (group (zero-or-one (any "ZA*")))
		      ;; 2. block number
		      (group 
		       ;; 3. chapter
		       (group (zero-or-more digit))
		       (zero-or-one "."
				    ;; 4. verse
				    (group 
				     (zero-or-more digit)))
		       (zero-or-one space
			;; 5. label
			(group (zero-or-more not-newline)))
			eol)))))

(peano-set-header-regexp)

;;; *5.1 Parsing Headers

(defun* peano-read-next-header (&optional &key bound backward)
  "Return a property list with data from the next header.
Return nil if there is no next header. Point is left where
the search leaves it."
  (let ((search-function (if backward 
			     #'re-search-backward
			   #'re-search-forward)))
    (when (funcall search-function peano-header-regexp bound :noerror)
      (labels ((group (n)
		      (match-string-no-properties n))
	       (ngroup (n)
		       (let ((s (group n)))
			 (if (stringp s)
			     (string-to-number s)
			   0))))
	(goto-char (point-at-eol))
	(list :type-symbol (group 1)
	      :block-number (ngroup 2)
	      :chapter (ngroup 3)
	      :verse (ngroup 4)
	      :label (group 5))))))
	  
;;; *5.2 Formatting Headers

;; TODO Finish this

(make-variable-buffer-local
 (defvar peano-header-prefix nil "Prefix to use when inserting headers"))

(defun* peano-insert-header (&key (block-number 
				   (string-to-number 
				    (format "%d.%d"
					    chapter verse)))
				  type-symbol
				  label
				  chapter
				  verse)
  (when (not (stringp peano-header-prefix))
    (error "You must set `peano-header-prefix' before inserting headers."))
  (insert peano-header-prefix " *")
  (when type-symbol 
    (insert type-symbol))
  (when chapter
    (insert (number-to-string chapter))
    (when verse
      (insert (concat "." 
		      (number-to-string verse)))))
  (when label
    (insert (concat " " label))))

;;; *5.3 Interactive Commands for Headers

;; (defun peano-chapter-and-verse (chapter verse type) 
;;   (let* ((chapter-string (number-to-string chapter))       
;; 	 (verse-string (number-to-string verse))
;; 	 (new-verse-maybe (+ 1 verse))
;; 	 (new-verse (if (> (length (number-to-string new-verse-maybe))
;; 			   (length verse-string))
;; 			(+ 1 (* 10 verse))
;; 		      new-verse-maybe)))
;;   (case type
;;     (:sibling (list chapter (+ 1 verse)))
;;     (:child (list chapter

;; TODO add 1 to verse if it's a sibling,
;; TODO append "1" to verse-string if it's a child

(defun peano-insert-sibling ()
  "Insert a sibling header following the current block."
  (interactive)
  (let ((block-info 
	 (peano-read-next-header :backward t)))
    (when (null block-info)
      (error "Cannot insert sibling; no previous header."))
    (destructuring-bind (&key chapter verse &allow-other-keys)
	block-info
      (peano-read-next-header)
      (insert "\n")
      (peano-insert-header :chapter chapter :verse (+ 1 verse)))
    (insert "\n")))


	
;;; *5.5

;(defun peano-insert-child


(defun org-cycle-global ()
  (interactive)
  (org-cycle t))

(defun org-cycle-local ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (org-cycle)))

;; TODO standardized keybindings for minor mode: M-_

;; (global-set-key (kbd "M-[") 'org-cycle-global)
;; (global-set-key (kbd "M-]") 'org-cycle-local)

;; (add-hook 'emacs-lisp-mode-hook #'orgstruct-mode)

;;;; Fontifying todo items outside of org-mode

(defface todo-comment-face '((t (:background "red" :foreground "yellow" :weight bold :bold t))) "Face for TODO in code buffers.")
(defvar todo-comment-face 'todo-comment-face)
(defun fontify-todo () 
  (font-lock-add-keywords nil '(("\\<\\(TODO\\)\\>" 
				 (1 todo-comment-face t)))))

(add-hook 'emacs-lisp-mode-hook #'fontify-todo)


(defface headline-face '((t (:foreground "white" :underline "white" :background "navyblue"))) "Face for headlines.")
(defvar headline-face 'headline-face)

(defun fontify-headline () 
  (font-lock-add-keywords nil '(("^;;;;* \\(.*\\)\\>" 
				 (1 headline-face t)))))


(add-hook 'emacs-lisp-mode-hook #'fontify-headline)





;;; *Z


;;; **10.1 Glyphs

(defface eon-form-feed-face 
  '((t (:foreground "white" :background "blue" :bold
		    t :weight bold :underline "red")))
  "Face for form-feed characters.")

(defvar eon-form-feed-face 'eon-form-feed-face)

;; TODO fix this stuff

(defun eon-do-glyphs ()
  "Highlight certain characters."
  (interactive)
  (let ((form-feed (make-glyph-code ? 'eon-form-feed-face)))
    (aset standard-display-table ?\^L (vector form-feed))))


;; Local Variables:
;; peano-header-prefix: ";;;"
;; End:

(provide 'peano)
;;; peano.el ends here
