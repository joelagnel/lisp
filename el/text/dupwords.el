;;; dupwords.el --- find duplicate words in sentences

;; Copyright (C) 1998 Stephen Eglen

;; Author: Stephen Eglen stephen@anc.ed.ac.uk
;; Maintainer: Stephen Eglen stephen@anc.ed.ac.uk
;; Created: 27 Jul 1998
;; Version: 1.0
;; Keywords: wp
;; Location: http://www.anc.ed.ac.uk/~stephen/emacs
;; RCS: $Id: dupwords.el,v 1.6 2004/04/11 15:16:58 stephen Exp $
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This program will find duplicate words in sentences.  For example,
;; in the sentence `The the cat sat on the mat', it will notice that
;; you have written `the the' and highlight the two identical words.
;; To check a region, use `M-x dw-check-region', or use `M-x
;; dw-check-to-end' to check from the current point to the end of the
;; buffer.  When you find a duplicate word, if you press `n' to exit,
;; point is left at the beginning of the first word, and mark at the
;; start of the second.

;; Some solutions to this have already been sent to the Emacs newsgroups e.g. 
;; http://x8.dejanews.com/getdoc.xp?AN=118205039&fmt=raw
;; http://x8.dejanews.com/getdoc.xp?AN=118375212&fmt=raw

;; The advantage of this approach is that it also tells you when you
;; have used the same word further on in the same sentence.  Set
;; `dw-forward-words' to 1 if you wish to find only duplicate words
;; that are immediately next to each other.  Or, if you want to check
;; for duplicate words within three words of each other, set it to 3.
;; If you want to check for duplicate words anywhere within a
;; sentence, set dw-forward-words to a negative value.  Duplicate
;; occurences of common words within a sentence but not adjacent can
;; be ignored by including the word in `dw-ignore-list'.

;;; How it works.

;; For each sentence, a list of the words in it is created.  The
;; position of each word in the buffer is stored using the text
;; property `dw-pos'.  When we find duplicate words in the sentence,
;; the text property is used to highlight the duplicate words in the
;; buffer.

;;; Developed on Emacs 20.2, but tested also on Emacs 19.34 and XEmacs
;;; 19.15.  Should work in Emacs 21.


;;; Bugs / To do:

;; Works only in text-based modes, such as Text, LaTeX, since it
;; relies on defuns such as `forward-sentence'.

;; Doesn't finish cleanly if the final sentence is not complete.

;; This code is new, so I'm sure there are lots of bugs!

;;; Some suggestions from Drew Adams:
;; Create a single command, say dw-check, to do both dw-check-to-end and
;; dw-check-region: If the region is active - for example (and
;; transient-mark-mode mark-active) - then dw-check-region; else
;; dw-check-to-end. (In any case, dw-check-region does nothing if the region is
;; not active, so the command might as well check till the end.)

;; Cancel a dw command cleanly with C-g, in addition to `n'.  Perhaps
;; also give SPACE the same binding as `y' (continue), for convenience.

;;  Supply a value as prefix arg for use instead of dw-forward-words,
;; and have it default to dw-forward-words (whose defvar default value
;; could be 1). For example, M-x dw-check would check for adjacent
;; duplicates (assuming dw-forward-words = 1), C-u 3 would check for
;; duplicates within 3 words, M-- would check for duplicates within sentence.
;; That would be more convenient than users changing dw-forward-words. In other
;; words, dw-forward-words would act as a default value, which could be
;; overridden with a prefix arg.

;; When checking for adjacent duplicates, perhaps temporarily bind a
;; convenient key (e.g. `.' or `k' or `d') to the binding used by C-w
;; (usually kill-region, but not necessarily), to make it easier to remove the
;; duplicate.


;;; Code:

(defvar dw-forward-words -1
  "*Number of words to check forward in rest of sentence for repeated word.
A negative value means check all the way to the end of the sentence.")

(defvar dw-ignore-list '("the" "a" "on" "in" "of" "in" "and" "to")
  "*List of words to be ignored when checking for repeated words. 
Will still check though if these words are adjacent.")

(defvar dw-sentence-start nil
  "Position of the start of the current sentence.")

(defvar dw-highlight-overlays [nil nil])

(defun dw-check-sentence (s)
  "Check the current sentence for repeated words.
S is a list of symbols representing the words in the sentence."
  (let (word rest)
    (while s
      (setq word (car s)
	    s (cdr s))
      (dw-check-next dw-forward-words word s)
      )))
	

(defun dw-ignore-word-p (word)
  "Return non-nil if WORD is in `dw-ignore-list'."
  ;; in Emacs 20 we can use just (member word dw-ignore-list),
  ;; but this doesn't work in Emacs 19.
  (let ((l dw-ignore-list)
	(searching t))
    (while (and searching (car l)
      (if (string= word (car l))
	  (setq searching nil)
	(setq l (cdr l)))))
    l))

(defun dw-check-next (n word rest)
  "Check to see if WORD occurs in next N elements of the REST of sentence."
  (if (dw-ignore-word-p word)
      ;; check only immediately following words.
      (setq n 1))
  (while (and (not (= n 0)) rest)
    (if (string= word (car rest))
	;; found a repeated word.
	(progn
	  (dw-highlight-words word (car rest))
	  (if (not (y-or-n-p (format "`%s' is repeated -- continue? " word)))
	      (dw-abort))))
    ;; move on to other words.
    (setq rest (cdr rest)
	  n (1- n))))
	   

(defun dw-abort ()
  "Abort checking words.
Point is left at start of first duplicate word, and mark at the start 
of the second."
  ;; Move point to start of first repeated word.
  (goto-char (overlay-start (aref dw-highlight-overlays 0)))
  (push-mark (overlay-start (aref dw-highlight-overlays 1)))
  (dw-unhighlight 0)
  (dw-unhighlight 1)
  (error "Abort duplicate word checking."))


(defun dw-finish ()
  "Finish checking words."
  (dw-unhighlight 0)
  (dw-unhighlight 1)
  (message "Finished duplicate word checking."))
  
(defun dw-highlight-words (word1 word2)
  "Highlight WORD1 and WORD2 (instances of the same word)."
  (let (
	(beg nil)
	(end nil)
	)
    (setq beg (get-text-property 0 'dw-pos word1)
	  end (+ beg (length word1)))
    (dw-highlight 0 beg end)

    (setq beg (get-text-property 0 'dw-pos word2)
	  end (+ beg (length word2)))
    (dw-highlight 1 beg end)
    ))


(defun dw-check-to-end ()
  "Check to the end of the document."
  (interactive)
  (dw-check-region (point) (point-max)))

;; (global-set-key (quote [f5]) 'dw-check-to-end)
;; (global-set-key (quote [f5]) 'dw-check-region)


(defun dw-check-region (beg end)
  "Check the current region for duplicate words."
  (interactive "r")
  (let (sentence end-sen beg-word end-word word-string word)
    (goto-char beg)
    (while (< (point) end)
      ;; get next sentence.
      ;; assume point is at beginning of sentence.
      (setq sentence nil)
      (setq dw-sentence-start (point))
      (forward-sentence 1)
      (setq end-sen (point))
      
      (goto-char dw-sentence-start)
      (while (< (point) end-sen)
	(re-search-forward "\\b\\w")
	(forward-char -1)
	(setq beg-word (point))
	;;(forward-word 1)
	(re-search-forward "\\W")
	(setq end-word (1- (point)))
	(cond ( (< end-word end-sen)
		;; get the current word as a string
		(setq word-string (buffer-substring beg-word end-word))
		;; convert it to a symbol
		(setq word (downcase word-string))
		(put-text-property 0 1 'dw-pos beg-word word)
		(setq sentence (cons word sentence))) ))
      (setq sentence (reverse sentence))
      ;;(setq ts sentence) ;todo -- delete.
      (goto-char end-sen)
      ;; Once the current sentence has been converted into a list of symbols,
      ;; we can find out if there are any double words.
      (dw-check-sentence sentence)
      ) ; try next sentence.
    (dw-finish)
    ))



;;; Highlighting (copied from reftex.el -- cheers Carsten!)

;; Highlighting uses overlays.  If this is for XEmacs, we need to load
;; the overlay library, available in version 19.15
(and (not (fboundp 'make-overlay))
     (condition-case nil
         (require 'overlay)
       ('error 
        (error "Fm needs overlay emulation (available in XEmacs 19.15)"))))

;; Initialize the overlays
(aset dw-highlight-overlays 0 (make-overlay 1 1))
(overlay-put (aref dw-highlight-overlays 0) 'face 'highlight)
(aset dw-highlight-overlays 1 (make-overlay 1 1))
(overlay-put (aref dw-highlight-overlays 1) 'face 'highlight)

;; Two functions for activating and deactivation highlight overlays
(defun dw-highlight (index begin end &optional buffer)
  "Highlight a region with overlay INDEX."
  (move-overlay (aref dw-highlight-overlays index)
                begin end (or buffer (current-buffer))))

(defun dw-unhighlight (index)
  "Detatch overlay INDEX."
  (delete-overlay (aref dw-highlight-overlays index)))

;;; dupwords.el ends here