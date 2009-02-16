;;; Mapping replace commands for GNU Emacs
;;; Copyright (C) 1987, 1988, 1989 Kyle E. Jones

;; Mapping replace commands allow the replacement of instances or
;; groups of instances of a string or regexp match to be taken
;; sequentially from a series of replacements.
;;
;; For example, suppose you had this line
;;   (define-key vm-mode-map "0" 'digit-argument)
;; and you wanted to duplicate this 9 times except that the 0 should be
;; replaced with the digits 1-9.  The duplication is easy, just kill the
;; line and yank it back 10 times.  But getting the digits right requires
;; manual editing, unless you have mapping replace commands.  With the
;; command mapreplace-string you could put the cursor before the first
;; yanked line, and say
;;   M-x mapreplace-string RET
;;   0 RET
;;   0 SPC 1 SPC 2 SPC 3 SPC 4 SPC 5 SPC 6 SPC 7 SPC 8 SPC 9 RET
;; and the job is done.
;;
;; If you were setting up two keymaps and had ten copies of these two lines
;;   (define-key vm-mode-map "0" 'digit-argument)
;;   (define-key vm-summary-mode-map "0" 'digit-argument)
;; you would do the same as in the first example except you'd type
;;   C-u 2
;; before it all to give a prefix argument of two.  This gets every two
;; 0's found replaced by a 0, 1, 2, 3, etc.
;; 
;; This package is autoloadable.  Use
;;    (autoload 'mapreplace-string "mapreplace" nil t)
;;    (autoload 'mapreplace-regexp "mapreplace" nil t)
;;    (autoload 'query-mapreplace "mapreplace" nil t)
;;    (autoload 'query-mapreplace-regexp "mapreplace" nil t)
;; in your .emacs file.

(provide 'mapreplace)

(defvar mapreplace-version "1.00")

(defvar query-mapreplace-help
  "Type Space or `y' to replace one match, Delete or `n' to skip to next,
ESC or `q' to exit, Period to replace one match and exit,
Comma to replace but not move point immediately,
C-r to enter recursive edit (\\[exit-recursive-edit] to get out again),
C-w to delete match and recursive edit,
C-l to clear the screen, redisplay, and offer same replacement again,
! to replace all remaining matches with no more questions,
^ to move point back to previous match."
  "Help message for the query mapreplace commands.")

(defun mapreplace (expression replacements &optional instances literal query)
  (or instances (setq instances 1))
  (let ((search-function (if literal 'search-forward 're-search-forward))
	(fixedcase (not (and case-fold-search case-replace)))
	(previous-match-marker 0)
	count
	(help-form
	 (if query
	     '(format "Query mapreplacing \"%s\" with \"%s\".\n\n%s"
		      expression (car replacements)
		      (substitute-command-keys query-mapreplace-help))))
	(echo-keystrokes 0))
    (while replacements
      (setq count 1)
      (while (<= count instances)
	(funcall search-function expression)
	(if query
	    (let ((decision-pending t)
		  match-data char)
	      (while decision-pending
		(setq decision-pending nil)
		(message
		 (format "Query mapreplacing \"%s\" with \"%s\":"
			 expression (car replacements)))
		(setq match-data (match-data))
		(setq char (downcase (read-char)))
		(store-match-data match-data)
		(cond
		 ((memq char '(?\ ?y))
		  (if (not (= (point) previous-match-marker))
		      (replace-match (car replacements) fixedcase literal)
		    (goto-char (match-end 0))))
		 ((memq char '(?\C-? ?n)) nil)
		 ((memq char '(?\e ?q))
		  (setq replacements nil count instances))
		 ((eq char ?.)
		  (replace-match (car replacements) fixedcase literal)
		  (setq replacements nil count instances))
		 ((eq char ?,)
		  (replace-match (car replacements) fixedcase literal)
		  (setq decision-pending t))
		 ((eq char ??)
		  (setq unread-command-char help-char)
		  (setq decision-pending t))
		 ((eq char ?\C-r)
		  (save-excursion (recursive-edit))
		  (store-match-data match-data)
		  (goto-char (match-end 0))
		  (setq decision-pending t))
		 ((eq char ?\C-w)
		  (delete-region (match-beginning 0) (match-end 0))
		  (save-excursion (recursive-edit))
		  (store-match-data match-data)
		  (goto-char (match-end 0))
		  (setq decision-pending t))
		 ((eq char ?\C-l)
		   (recenter)
		   (setq decision-pending t))			 
		 ((eq char ?!)
		  (replace-match (car replacements) fixedcase literal)
		  (setq query nil))
		 ((eq char ?^)
		  (if previous-match-marker
		      (goto-char previous-match-marker))
		  (setq decision-pending t))
		 (t (setq replacements nil count instances)))
		(setq previous-match-marker (car (cdr (match-data))))))
	  (replace-match (car replacements) fixedcase literal))
	(setq count (1+ count)))
      (undo-boundary)
      (setq replacements (cdr replacements)))))

(defun mapreplace-string (string strings &optional instances)
  "Sequentially replace instances of STRING with the replacement STRINGS.
Optional third arg INSTANCES non-nil is a repeat count; that many
  matches of STRING are replaced before moving to the next replacement
  string.  This defaults to the numeric prefix argument if called
  interactively."
 (interactive
  (let (string)
    (barf-if-buffer-read-only)
    (setq string (read-string "Mapreplace string: "))
    (list
     string
     (read-string-list (format "Mapreplace string \"%s\" with: " string))
     (prefix-numeric-value current-prefix-arg))))
 (or instances (setq instances 1))
 (push-mark)
 (mapreplace string strings instances t)
 (message "Done"))

(defun mapreplace-regexp (regexp replacements &optional instances)
  "Sequentially replace instances of REGEXP with the REPLACEMENTS.
The REPLACEMENTS may contain the usual \\ escapes to copy \\(..\\) matches
  from the REGEXP.
Optional third arg INSTANCES non-nil is a repeat count; that many
  matches of REGEXP are replaced before moving to the next REPLACEMENT.
  This defaults to the numeric prefix argument if called interactively."
  (interactive
   (let (regexp)
     (barf-if-buffer-read-only)
     (setq regexp (read-string "Mapreplace regexp: "))
     (list
      regexp
      (read-string-list (format "Mapreplace regexp \"%s\" with: " regexp))
      (prefix-numeric-value current-prefix-arg))))
  (or instances (setq instances 1))
  (push-mark)
  (mapreplace regexp replacements instances)
  (message "Done"))

(defun query-mapreplace (string strings &optional instances)
  "Sequentially replace instances of STRING with the replacement STRINGS,
querying the user before each replacement.
Optional third arg INSTANCES non-nil is a repeat count; query about replacing
  that many matches of STRING before moving to the next replacement string.
  This defaults to the numeric prefix argument if called interactively."
  (interactive
   (let (string)
     (barf-if-buffer-read-only)
     (setq string (read-string "Query mapreplace string: "))
     (list
      string
      (read-string-list
       (format "Query mapreplace string \"%s\" with: " string))
      (prefix-numeric-value current-prefix-arg))))
  (or instances (setq instances 1))
  (push-mark)
  (mapreplace string strings instances t t)
  (message "Done"))

(defun query-mapreplace-regexp (regexp replacements &optional instances)
  "Sequentially replace instances of REGEXP with the REPLACEMENTS,
querying the user before each replacement.
The REPLACEMENTS may contain the usual \\ escapes to copy \\(..\\) matches
  from the REGEXP.
Optional third arg INSTANCES non-nil is a repeat count; query about replacing
  that many matches of REGEXP before moving to the next REPLACEMENT.
  This defaults to the numeric prefix argument if called interactively."
  (interactive
   (let (regexp)
     (barf-if-buffer-read-only)
     (setq regexp (read-string "Query mapreplace regexp: "))
     (list
      regexp
      (read-string-list
       (format "Query mapreplace regexp \"%s\" with: " regexp))
      (prefix-numeric-value current-prefix-arg))))
  (or instances (setq instances 1))
  (push-mark)
  (mapreplace regexp replacements instances nil t)
  (message "Done"))

(defun read-string-list (prompt)
  "Read a string using the minibuffer, prompting with PROMPT and return a
list of strings obtained by breaking the string the user entered at the
space boundaries.  (e.g. \"a b c\" becomes (\"a\" \"b\" \"c\"))."
  (let ((string (read-string prompt)) list)
    (store-match-data nil)
    (while (string-match " *\\([^ ]+\\) *" string (match-end 0))
      (setq list
	    (cons (substring string (match-beginning 1) (match-end 1)) list)))
    (nreverse list)))
