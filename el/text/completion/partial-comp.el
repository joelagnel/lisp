;From utkcs2!emory!samsung!uunet!decwrl!elroy.jpl.nasa.gov!cit-vax!daveg Mon Jul  2 13:27:39 EDT 1990
;Article 3105 of gnu.emacs:
;Xref: utkcs2 gnu.emacs:3105 comp.emacs:4547
;Path: utkcs2!emory!samsung!uunet!decwrl!elroy.jpl.nasa.gov!cit-vax!daveg
;>From: daveg@cit-vax.Caltech.Edu (David Gillespie)
;Newsgroups: gnu.emacs,comp.emacs
;Subject: Partial completion update, version 1.04
;Message-ID: <15465@cit-vax.Caltech.Edu>
;Date: 30 Jun 90 00:35:08 GMT
;Reply-To: daveg@csvax.caltech.edu (David Gillespie)
;Organization: California Institute of Technology
;Lines: 426
;
;Here's (yet) another version of my partial completion system.  Known bugs
;have been fixed; behavior is even more compatible with standard Emacs
;completion (let's hope RMS doesn't sue me for "look and feel". :-)
;Handling of word-completion and file name completion is improved.
;
;								-- Dave


;; Partial completion mechanism for GNU Emacs.  Version 1.04.
;; Copyright (C) 1990 Dave Gillespie, daveg@csvax.caltech.edu.
;; Special thanks to Hallvard Furuseth for his many ideas and contributions.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;; Extended completion for the Emacs minibuffer.
;;
;; The basic idea is that the command name or other completable text is
;; divided into words and each word is completed separately, so that
;; "M-x p-b" expands to "M-x print-buffer".  If the entry is ambiguous
;; each word is completed as much as possible and then the cursor is
;; left at the first position where typing another letter will resolve
;; the ambiguity.
;;
;; Word separators for this purpose are hyphen, space, and period.
;; These would most likely occur in command names, Info menu items,
;; and file names, respectively.  But all word separators are treated
;; alike at all times.
;;
;; This completion package installs itself on Meta- key sequences by
;; default, but many people prefer to replace the old-style completer
;; outright.  You can do this by setting PC-meta-flag as described below.


;; Usage:  Load this file.  Now, during completable minibuffer entry,
;;
;;     M-TAB    means to do a partial completion;
;;     M-SPC    means to do a partial complete-word;
;;     M-RET    means to do a partial complete-and-exit;
;;     M-?      means to do a partial completion-help.
;;
;; If you set PC-meta-flag non-nil, then TAB, SPC, RET, and ? perform
;; these functions, and M-TAB etc. perform original Emacs completion.
;; To do this, put the command,
;;
;;       (setq PC-meta-flag t)
;;
;; in your .emacs file.  To load partial completion automatically, put
;;
;;       (load "complete")
;;
;; in your .emacs file, too.  Things will be faster if you byte-compile
;; this file when you install it.
;;
;; As an extra feature, in cases where RET would not normally
;; complete (such as `C-x b'), the M-RET key will always do a partial
;; complete-and-exit.  Thus `C-x b f.c RET' will select or create a
;; buffer called "f.c", but `C-x b f.c M-RET' will select the existing
;; buffer whose name matches that pattern (perhaps "filing.c").
;; (PC-meta-flag does not affect this behavior; M-RET used to be
;; undefined in this situation.)


(defvar PC-meta-flag nil
  "*If nil, TAB does normal Emacs completion and M-TAB does Partial Completion.
If t, TAB does Partial Completion and M-TAB does normal completion.")


(defvar PC-default-bindings t
  "Set this to nil to suppress the default partial completion key bindings.")

(if PC-default-bindings (progn
(define-key minibuffer-local-completion-map "\t" 'PC-complete)
(define-key minibuffer-local-completion-map " "  'PC-complete-word)
(define-key minibuffer-local-completion-map "?"  'PC-completion-help)

(define-key minibuffer-local-completion-map "\e\t" 'PC-complete)
(define-key minibuffer-local-completion-map "\e "  'PC-complete-word)
(define-key minibuffer-local-completion-map "\e\r" 'PC-force-complete-and-exit)
(define-key minibuffer-local-completion-map "\e\n" 'PC-force-complete-and-exit)
(define-key minibuffer-local-completion-map "\e?"  'PC-completion-help)

(define-key minibuffer-local-must-match-map "\t" 'PC-complete)
(define-key minibuffer-local-must-match-map " "  'PC-complete-word)
(define-key minibuffer-local-must-match-map "\r" 'PC-complete-and-exit)
(define-key minibuffer-local-must-match-map "\n" 'PC-complete-and-exit)
(define-key minibuffer-local-must-match-map "?"  'PC-completion-help)

(define-key minibuffer-local-must-match-map "\e\t" 'PC-complete)
(define-key minibuffer-local-must-match-map "\e "  'PC-complete-word)
(define-key minibuffer-local-must-match-map "\e\r" 'PC-complete-and-exit)
(define-key minibuffer-local-must-match-map "\e\n" 'PC-complete-and-exit)
(define-key minibuffer-local-must-match-map "\e?"  'PC-completion-help)
))


(defun PC-complete ()
  "Like minibuffer-complete, but allows \"b--di\"-style abbreviations.
For example, \"M-x b--di\" would match \"byte-recompile-directory\", or any
name which consists of three or more words, the first beginning with \"b\"
and the third beginning with \"di\".

The pattern \"b--d\" is ambiguous for \"byte-recompile-directory\" and
\"beginning-of-defun\", so this would produce a list of completions
just like when normal Emacs completions are ambiguous.

Word-delimiters for the purposes of Partial Completion are \"-\", \".\", and SPC."
  (interactive)
  (if (eq (or (> (length (this-command-keys)) 1)
	      (> (aref (this-command-keys) 0) 128)) PC-meta-flag)
      (minibuffer-complete)
    (PC-do-completion nil))
)


(defun PC-complete-word ()
  "Like minibuffer-complete-word, but allows \"b--di\"-style abbreviations.
See PC-complete for details."
  (interactive)
  (if (eq (or (> (length (this-command-keys)) 1)
	      (> (aref (this-command-keys) 0) 128)) PC-meta-flag)
      (if (= last-command-char ? )
	  (minibuffer-complete-word)
	(self-insert-command 1))
    (self-insert-command 1)
    (if (eobp)
	(PC-do-completion 'word)))
)


(defun PC-complete-and-exit ()
  "Like minibuffer-complete-and-exit, but allows \"b--di\"-style abbreviations.
See PC-complete for details."
  (interactive)
  (if (eq (or (> (length (this-command-keys)) 1)
	      (> (aref (this-command-keys) 0) 128)) PC-meta-flag)
      (minibuffer-complete-and-exit)
    (PC-do-complete-and-exit))
)

(defun PC-force-complete-and-exit ()
  "Like minibuffer-complete-and-exit, but allows \"b--di\"-style abbreviations.
See PC-complete for details."
  (interactive)
  (let ((minibuffer-completion-confirm nil))
    (PC-do-complete-and-exit))
)

(defun PC-do-complete-and-exit ()
  (if (= (buffer-size) 0)  ; Duplicate the "bug" that Info-menu relies on...
      (exit-minibuffer)
    (let ((flag (PC-do-completion 'exit)))
      (and flag
	   (if (or (eq flag 'complete)
		   (not minibuffer-completion-confirm))
	       (exit-minibuffer)
	     (temp-minibuffer-message " (Confirm)")))))
)


(defun PC-completion-help ()
  "Like minibuffer-completion-help, but allows \"b--di\"-style abbreviations.
See PC-complete for details."
  (interactive)
  (if (eq (or (> (length (this-command-keys)) 1)
	      (> (aref (this-command-keys) 0) 128)) PC-meta-flag)
      (minibuffer-completion-help)
    (PC-do-completion 'help))
)


(defun PC-do-completion (&optional mode)
  (let* ((table minibuffer-completion-table)
	 (pred minibuffer-completion-predicate)
	 (filename (eq table 'read-file-name-internal))
	 (dirname nil)
	 (str (buffer-string))
	 basestr
	 regex
	 (p 0)
	 (poss nil)
	 helpposs
	 (case-fold-search completion-ignore-case))

    ;; Check if buffer contents can already be considered complete
    (if (and (eq mode 'exit)
	     (PC-is-complete-p str table pred))
	'complete

      ;; Strip directory name if appropriate
      (if filename
	  (setq basestr (file-name-nondirectory str)
		dirname (file-name-directory str))
	(setq basestr str))

      ;; Convert search pattern to a standard regular expression
      (setq regex (regexp-quote basestr))
      (while (setq p (string-match "[-. ]" regex p))
	(if (eq (aref regex p) ? )
	    (setq regex (concat (substring regex 0 p)
				"[^-. ]*[-. ]"
				(substring regex (1+ p)))
		  p (+ p 12))
	  (let ((bump (if (eq (aref regex p) ?-) 0 -1)))
	    (setq regex (concat (substring regex 0 (+ p bump))
				"[^-. ]*"
				(substring regex (+ p bump)))
		  p (+ p 8)))))
      (setq regex (concat "\\`" regex))

      ;; Find an initial list of possible completions
      (if (not (setq p (string-match "[-. ]" str (length dirname))))

	  ;; Minibuffer contains no hyphens -- simple case!
	  (setq poss (all-completions str
				      table
				      pred))

	;; Use all-completions to do an initial cull.  This is a big win,
	;; since all-completions is written in C!
	(let ((compl (all-completions (substring str 0 p)
				      table
				      pred)))
	  (setq p compl)
	  (while p
	    (and (string-match regex (car p))
		 (setq poss (cons (car p) poss)))
	    (setq p (cdr p)))))

      ;; Now we have a list of possible completions
      (cond

       ;; No valid completions found
       ((null poss)
	(if (and (eq mode 'word)
		 (not PC-word-failed-flag))
	    (let ((PC-word-failed-flag t))
	      (delete-backward-char 1)
	      (PC-do-completion 'word))
	  (beep)
	  (temp-minibuffer-message (if (eq mode 'help)
				       " (No completions)"
				     " (No match)"))
	  nil))

       ;; More than one valid completion found
       ((or (cdr (setq helpposs poss))
	    (memq mode '(help word)))

	;; Handle completion-ignored-extensions
	(and filename
	     (not (eq mode 'help))
	     (let ((p2 poss))

	       ;; Build a regular expression representing the extensions list
	       (or (equal completion-ignored-extensions PC-ignored-extensions)
		   (setq PC-ignored-regexp
			 (concat "\\("
				 (mapconcat
				  'regexp-quote
				  (setq PC-ignored-extensions
					completion-ignored-extensions)
				  "\\|")
				 "\\)\\'")))

	       ;; Check if there are any without an ignored extension
	       (setq p nil)
	       (while p2
		 (or (string-match PC-ignored-regexp (car p2))
		     (setq p (cons (car p2) p)))
		 (setq p2 (cdr p2)))

	       ;; If there are "good" names, use them
	       (and p (setq poss p))))

	;; Is the actual string one of the possible completions?
	(setq p (and (not (eq mode 'help)) poss))
	(while (and p
		    (not (equal (car p) basestr)))
	  (setq p (cdr p)))
	(if p

	    (progn
	      (if (null mode)
		  (temp-minibuffer-message " (Complete, but not unique)"))
	      t)

	  ;; If ambiguous, try for a partial completion
	  (let ((improved nil)
		prefix
		(pt nil)
		(skip "\\`"))

	    ;; Check if next few letters are the same in all cases
	    (if (and (not (eq mode 'help))
		     (setq prefix (try-completion "" (mapcar 'list poss))))
		(let (i)
		  (if (eq mode 'word)
		      (setq prefix (PC-chop-word prefix basestr)))
		  (goto-char (+ (point-min) (length dirname)))
		  (while (and (progn
				(setq i 0)
				(while (< i (length prefix))
				  (if (eq (aref prefix i) (following-char))
				      (forward-char 1)
				    (if (or (and (looking-at " ")
						 (memq (aref prefix i)
						       '(?- ?. ? )))
					    (eq (downcase (aref prefix i))
						(downcase (following-char))))
					(delete-char 1)
				      (setq improved t))
				    (insert (substring prefix i (1+ i))))
				  (setq i (1+ i)))
				(or pt (setq pt (point)))
				(looking-at "[-. ]"))
			      (not (eq mode 'word))
			      (setq skip (concat skip
						 (regexp-quote prefix)
						 "[^-. ]*")
				    prefix (try-completion
					    ""
					    (mapcar
					     (function
					      (lambda (x)
						(list
						 (and (string-match skip x)
						      (substring
						       x
						       (match-end 0))))))
					     poss)))
			      (or (> i 0) (> (length prefix) 0))))
		  (goto-char (if (eq mode 'word) (point-max) pt))))

	    (if (and (eq mode 'word)
		     (not PC-word-failed-flag))

		(if improved

		    ;; We changed it... would it be complete without the space?
		    (if (PC-is-complete-p (buffer-substring 1 (1- (point-max)))
					  table pred)
			(delete-region (1- (point-max)) (point-max))))

	      (if improved

		  ;; We changed it... enough to be complete?
		  (and (eq mode 'exit)
		       (PC-is-complete-p (buffer-string) table pred))

		;; If totally ambiguous, display a list of completions
		(if (or completion-auto-help
			(eq mode 'help))
		    (with-output-to-temp-buffer " *Completions*"
		      (display-completion-list (sort helpposs 'string-lessp)))
		  (temp-minibuffer-message " (Next char not unique)"))
		nil)))))

       ;; Only one possible completion
       (t
	(if (equal basestr (car poss))
	    (if (null mode)
		(temp-minibuffer-message " (Sole completion)"))
	  (erase-buffer)
	  (insert (if filename
		      (substitute-in-file-name (concat dirname (car poss)))
		    (car poss))))
	t))))
)
(setq PC-ignored-extensions 'empty-cache)
(setq PC-word-failed-flag nil)


(defun PC-is-complete-p (str table pred)
  (let ((res (if (listp table)
		 (assoc str table)
	       (if (vectorp table)
		   (or (equal str "nil")   ; heh, heh, heh
		       (intern-soft str table))
		 (funcall table str pred 'lambda)))))
    (and (or (not pred)
	     (and (not (listp table)) (not (vectorp table)))
	     (funcall pred res))
	 res))
)

(defun PC-chop-word (new old)
  (let ((i -1)
	(j -1))
    (while (and (setq i (string-match "[-. ]" old (1+ i)))
		(setq j (string-match "[-. ]" new (1+ j)))))
    (if (and j
	     (or (not PC-word-failed-flag)
		 (setq j (string-match "[-. ]" new (1+ j)))))
	(substring new 0 (1+ j))
      new))
)

(defun temp-minibuffer-message (m)
  "A Lisp version of temp_minibuffer_message from minibuf.c."
  (let ((savemax (point-max)))
    (save-excursion
      (goto-char (point-max))
      (insert m))
    (let ((inhibit-quit t))
      (sit-for 2)
      (delete-region savemax (point-max))
      (if quit-flag
	  (setq quit-flag nil
		unread-command-char 7))))
)


