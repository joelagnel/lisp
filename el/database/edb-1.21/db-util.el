;;; db-util.el --- part of EDB, the Emacs database

;; See database.el for copyright notice, distribution conditions, etc.

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB

;;; Commentary:

;; Lisp utilities.
;; This file is largely cannibalized from util-mde.el and util-mdecl.el,
;; which are available from theory.lcs.mit.edu:/pub/emacs/.

;; I have made an effort to prefix the function and variable names by db-,
;; to avoid clashes with other packages.

;;; Code:


(provide 'db-util)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching, matching, and replacing
;;;


(defun db-match-string (n &optional source)
  "Return the string matched by parentheses number N.  If there is a
SOURCE string, return the substring of that string; else, return
substring of the current buffer."
  (cond
   ((stringp source)
    (substring source (match-beginning n) (match-end n)))
   (t (buffer-substring (match-beginning n) (match-end n)))))

(defun db-match-string-maybe (n &optional source)
  "Like db-match-string, but return nil if there was no match for parenthesis N."
  (and (match-beginning n)
       (db-match-string n source)))


(defun db-unused-char-in-buffer ()
  "Return a character not used in the current buffer, or nil.
This function attempts to return a character that can be displayed in a single
screen column."
  (save-excursion
    (let ((candidate ?\ )
	  (result t))
      (while (eq result t)
	(goto-char (point-min))
	(if (not (search-forward (char-to-string candidate) nil t))
	    (setq result candidate)
	  (progn
	    (setq candidate (% (1+ candidate) 256))
	    (if (eq candidate ?\ )
		(setq result nil)))))
      result)))

(defun db-unused-char-in-string (string)
  "Return a character not used in STRING, or nil.
This function attempts to return a character that can be displayed in a single
screen column."
  (save-excursion
    (set-buffer (get-buffer-create " *Temporary*"))
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (insert string)
    (db-unused-char-in-buffer)))

;;; Skipping

;; Is this more efficient than regexp-quote and db-skip-regexp-forward
;; (which equals looking-at-string and goto-char)?
(defsubst db-skip-string-forward (string)
  "If point is at STRING, move past it and return non-nil;
otherwise return nil."
  (if (equal "" string)
      t
    (if (search-forward string (+ (point) (length string)) t)
	(goto-char (match-end 0)))))

(defsubst db-skip-string-backward (string)
  "If point is after STRING, move back past it and return t;
otherwise return nil."
  (if (equal "" string)
      t
    (search-backward string (- (point) (length string)) t)))

(defsubst db-skip-regexp-forward (regexp &optional match-no)
  "If point is at REGEXP, move past it and return point;
otherwise return nil.
Point is left at the end of match MATCH-NO if it is specified."
  (if (looking-at regexp)
      (goto-char (match-end (or match-no 0)))))

(defsubst db-skip-regexp-backward (regexp &optional match-no)
  "If point is after REGEXP, move past it and return point;
otherwise return nil."
  (let ((here (point)))
    (if (re-search-backward regexp nil t)
	(if (= here (match-end 0))
	    t
	  (progn
	    (goto-char here)
	    nil)))))

;; From Robert Potter <potter@silver.lcs.mit.edu>
(defun db-looking-back-at (PAT)
  "t when text before point matches regular expression PAT."
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (point))
      (re-search-backward (concat "\\(" PAT "\\)\\'") (point-min) t))))


;;; String substitution

(defsubst db-string-substitute (newchar oldchar string)
  "Substitute NEWCHAR for instances of OLDCHAR in STRING.
NEWCHAR and OLDCHAR are characters."
  (db-string-substitute-opt newchar
			    (regexp-quote (char-to-string oldchar))
			    string))

;; Optimized version.  oldchar-regexp should only match one-character strings.
(defun db-string-substitute-opt (newchar oldchar-regexp string)
  (let ((i -1)
	(case-fold-search nil))
    (while (setq i (string-match oldchar-regexp string (1+ i)))
      (aset string i newchar))))


(defun db-string-substitute-substring-general-case (new old-regexp string)
  "Call `string-replace-regexp-2'.  Beware special meaning of \\!."
  (string-replace-regexp-2 string old-regexp new))

;; If much replacement is going to happen, this is more efficient.
;; Original version from gaynor@brushfire.rutgers.edu (Silver).
(defun string-replace-regexp-2 (string regexp replacement)
  "Return the string resulting by replacing all of STRING's instances of REGEXP
with REPLACEMENT."
  (save-excursion
    (set-buffer (get-buffer-create " *Temporary*"))
    (buffer-disable-undo (current-buffer))
    (erase-buffer)
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match replacement))
    (buffer-string)
    ))

;; Instead of using match-beginning, I could compute the length of the
;; string and use (backward-char (1- string-length)).
(defun db-how-many-string-overlapping (string)
  "Return number of matches for STRING following point, including overlapping ones."
  (let ((count 0))
    (save-excursion
     (while (search-forward string nil t)
       (goto-char (1+ (match-beginning 0)))
       (setq count (1+ count))))
    count))

(defun db-how-many-substring-overlapping (substring target)
  "Return number of matches for SUBSTRING in TARGET, including overlapping ones."
  (let ((ss-regexp (regexp-quote substring))
	(count 0)
	(start -1))
    (while (setq start (string-match ss-regexp target (1+ start)))
      (setq count (1+ count)))
    count))

;;; db-find-char

(defun db-find-char (char string &optional count)
  "Look for CHAR in STRING; return first index in STRING whose element is CHAR.
If optional arg COUNT is specified, return the COUNTth occurrance."
  (if (not count)
      (setq count 1))
  (let ((index 0)
	(string-length (length string))
	(result nil))
    (while (and (< index string-length) (not result))
      (if (char-equal char (aref string index))
	  (if (= count 1)
	      (setq result index)
	    (setq count (1- count))))
      (setq index (1+ index)))
    result))

(defun db-find-char-from-end (char string &optional count)
  "Look for CHAR in STRING; return last index in STRING whose element is CHAR.
If optional arg COUNT is specified, return the COUNTth occurrance from the end."
  (if (not count)
      (setq count 1))
  (let ((index (1- (length string)))
	(string-length )
	(result nil))
    (while (and (> index -1) (not result))
      (if (char-equal char (aref string index))
	  (if (= count 1)
	      (setq result index)
	    (setq count (1- count))))
      (setq index (1- index)))
    result))

(defsubst db-string-trim-whitespace (string)
  "Return a substring of STRING with whitespace removed from beginning and end."
  (if (string-match "\\s *\\(.*[^ \t\n]\\)\\s *" string)
      (db-match-string 1 string)
    ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

;; Ought to have a way to set the default value, too.
(defmacro deflocalvar (&rest args)
  "Like defvar, but defines a buffer-local variable."
  (` (progn
       (defvar (,@ args))
       (make-variable-buffer-local (quote (, (car args)))))))
(put 'deflocalvar 'edebug-form-spec '(&rest form))

(defun symbol-append (&rest symbols)
  (intern (apply (function concat)
		 (mapcar (function symbol-name)
			 symbols))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keys
;;;

;; Semi-transparent way to meta-ify a key.  This still isn't quite what we
;; want.  The problem is that some patches to permit 8-bit character sets
;; to be displayed change meta sequences to escape sequences, even without
;; changing meta-flag to nil.

;; Another problem is that Lucid Emacs expects calls like
;; (define-key database-view-mode-map '(meta tab) 'db-last-field)
;; (define-key database-view-mode-map '(meta v) 'db-scroll-down)

(defmacro db-meta-prefix-ify (keys)
  "Prepend `meta-prefix-char' to KEYS, a string."
  (` (concat (list meta-prefix-char) (, keys))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions
;;;

;; Lifted from edebug 2.7 with very minor modifications.
(defsubst db-functionp (object)
  "Return t if OBJECT is a function (is funcallable), nil otherwise."
  (while (and (symbolp object) (fboundp object))
    (setq object (symbol-function object)))
  (or (subrp object)
      (and (listp object)
	   (eq (car object) 'lambda)
	   (listp (car (cdr object))))))

(defun db-funcall-maybe (fun &rest args)
  "If FUN is non-nil, apply it to ARGS.  Otherwise return second argument,
which would have been the first argument to which FUN was applied.
FUN should be a funcallable object or nil."
;; Compare to `db-maybe-funcall'.
  (if fun
      (apply fun args)
    (if args
	(car args))))
(put 'db-funcall-maybe 'edebug-form-spec '(function &rest form))

;; ;; Perhaps I should change the order of arguments.
;; (defun db-funcall-maybe-default (default fun &rest args)
;;   "If FUN is non-nil, apply it to ARGS.  Otherwise return DEFAULT.
;; FUN should be a funcallable object or nil."
;;   (if fun
;;       (apply fun args)
;;     default))
;; (put 'db-funcall-maybe-default 'edebug-form-spec '(form function &rest form))
;; 
;; (defmacro db-maybe-funcall (fun &rest args)
;;   "If FUN is non-nil, apply it to ARGS.  Otherwise return nil.
;; FUN should be a funcallable object or nil.  Compare to `db-funcall-maybe'."
;;   (` (db-funcall-maybe-default nil (, fun) (,@ args))))
;; (put 'db-maybe-funcall 'edebug-form-spec '(function &rest form))


;; Obviously this could be (easily) generalized to take a list of integers
;; and to try all of those numbers of arguments; but why would I want that?
(defmacro db-vararg-call (func noargs1 noargs2 &rest args)
  "Apply FUNC to NOARGS1 (an integer), then (if that fails), to NOARGS2
of the ARGS.  -1 means all arguments.  This macro lets you deal with functions
expecting different numbers of arguments in a uniform way.  Since this is a
macro, don't supply something of the form (function foo) as its first argument;
just supply foo itself."
  (let ((noargs (length args))
	nocommon-args
	common-vars
	common-bindings
	thisvar
	(thisargno 0))

    (if (< noargs1 0) (setq noargs1 (- noargs)))
    (if (< noargs2 0) (setq noargs2 (- noargs)))
    (if (not (= (max noargs1 noargs2) noargs))
	(progn
	  (byte-compile-warn "`%s' was db-vararg-called with a maximum of %d arguments, but you supplied %d."
			   func (max noargs1 noargs2) noargs)
	  (setq args (firstn (max noargs1 noargs2) args))))
    (setq nocommon-args (min noargs1 noargs2))
    (if (= noargs1 noargs2)
	;; aka (` ((, func) (,@ args)))
	(cons func args)
      (while (< thisargno nocommon-args)
	(setq thisargno (1+ thisargno)
	      thisvar (make-symbol (concat "vararg-common-"
					   (int-to-string thisargno)))
	      common-vars (cons thisvar common-vars)
	      common-bindings (cons (list thisvar (car args)) common-bindings)
	      args (cdr args)))
      (setq common-vars (nreverse common-vars)
	    common-bindings (nreverse common-bindings))
      (` (let (, common-bindings)
	   (condition-case err
	       ;; Try calling it with first number of arguments.
	       ((, func) (,@ common-vars)
		(,@ (if (< nocommon-args noargs1) args)))
	     (wrong-number-of-arguments
	      ;; Call it with second number of arguments.
	      ((, func) (,@ common-vars)
	       (,@ (if (< nocommon-args noargs2) args))))
	     (error
	      ;; Otherwise resignal; "while t" makes this work under the
	      ;; debugger (see, eg, the code for the "error" function).
	      (while t
		(signal (car err) (cdr err))))))))))

;; Test cases:
;; (macroexpand '(db-vararg-call foo 3 1 bar baz bum))
;; (macroexpand '(db-vararg-call foo 3 5 bar baz bum quux quux2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
;;;

(defvar db-filename-extension-regexp "[^.]\\(\\.\\([-a-z]*\\)\\)$"
  "Submatch 1 is the extension with leading period; submatch 2 is without.")

;; (defun db-filename-extension (filename &optional period)
;;   "Return the file extension of FILENAME.
;; Return nil for extensionless files such as \"foo\".
;; Return the empty string for files such as \"foo.\".
;; The leading period is included only if optional arguemnt PERIOD is specified,
;; in which case nil is never returned."
;;   (if (string-match db-filename-extension-regexp filename)
;;       (substring filename (match-beginning (if period 1 2)))
;;     (if period
;; 	"")))

(defun db-filename-sans-extension (filename)
  (if (string-match db-filename-extension-regexp filename)
      (substring filename 0 (match-beginning 1))
    filename))

(defsubst db-if-file-readable-p (filename)
  "Return FILENAME if the file is readable, nil otherwise."
  (if (file-readable-p filename)
      filename))

(defun db-locate-file-with-extensions (filename extensions)
  "Return the name of a readable file starting with FILENAME
or FILENAME's basename and ending with a string in EXTENSIONS, which is a list.
EXTENSIONS may be nil, in which case FILENAME is searched for as is."
  (if extensions
      (let (result)
	(while (and extensions (not result))
	  (setq result (or (db-if-file-readable-p (concat filename (car extensions)))
			   (db-if-file-readable-p (concat (db-filename-sans-extension
							filename)
						       (car extensions))))
		extensions (cdr extensions)))
	result)
    (db-if-file-readable-p filename)))

(defun db-locate-file-with-extensions-on-path (filename extensions path)
  "Return the name of a readable file starting with FILENAME
or FILENAME's basename and ending with a string in EXTENSIONS, which is a list.
PATH is a list of strings representing directories to be searched in
order after the current one; they may be relative directories.
Nil means the current directory."
  (or (db-locate-file-with-extensions filename extensions)
      (let ((filename-directory (file-name-directory filename))
	    (filename-nondirectory (file-name-nondirectory filename))
	    result candidate-directory)
	(while (and path (not result))
	  (setq candidate-directory (if (car path)
					(file-name-as-directory (car path))
				      default-directory)
		path (cdr path)
		result (db-locate-file-with-extensions
			;; This check is so we return something reasonable,
			;; not because the code requires the simpler form.
			(if (file-name-absolute-p candidate-directory)
			    (concat candidate-directory filename-nondirectory)
			  ;; This probably only works on Unix.
			  (concat filename-directory candidate-directory
				  filename-nondirectory))
			extensions)))
	result)))

(defun db-locate-file-on-path (filename path)
  "Return the full path of a file named FILENAME located
in the current directory or on PATH, which is a list of directories (strings)
or nil for the current directory."
  (db-locate-file-with-extensions-on-path filename nil path))


(defun db-file-resolve-symlink (file)
  "Return the non-link FILE eventually points to, or FILE if it's not a symbolic link.
This gets in an infinite loop if FILE points into a circular list of symlinks."
  (while (file-symlink-p file)
    (setq file (expand-file-name (car (file-attributes file))
				 (file-name-directory file))))
  file)

(defun db-same-file-p (file1 file2)
  "Return t if FILE1 and FILE2 are names for the same file."
  (setq file1 (db-file-resolve-symlink file1)
	file2 (db-file-resolve-symlink file2))
  (or (equal file1 file2)
      (equal file1 (file-name-nondirectory file2))
      (equal file2 (file-name-nondirectory file1))
      ;; Works for hard links.  If neither file exists, attributes are nil
      ;; and so trivially equal.
      (and (file-exists-p file1) (file-exists-p file2)
	   (equal (file-attributes file1)
		  (file-attributes file2)))))

(defun db-insert-file-contents (filename &optional visit beg end)
  (if (fboundp 'crypt-insert-file-contents)
      ;; crypt++.el defines this to take only a single argument.
      (crypt-insert-file-contents filename)
   (insert-file-contents filename visit beg end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conversion
;;;

;; string-to-int is unacceptable because it returns 0 for unparseable values.

(defun string->integer-default (string &optional default)
  "If STRING represents an integer, return it; otherwise return DEFAULT."
  (let ((result (condition-case nil
		    (car (read-from-string string))
		  (error nil))))
    (if (integerp result)
	result
      default)))
(fset 'string->number-default (symbol-function 'string->integer-default))

(defun string->integer-or-nil (string)
  (string->integer-default string nil))
(fset 'string->number-or-nil (symbol-function 'string->integer-or-nil))

(defun string->integer (string)
  "Return the integer represented by STRING, or err.
See also `string->integer-default'."
  (or (string->integer-or-nil string)
      (error "string->integer:  `%s' doesn't look like an integer." string)))
(fset 'string->number (symbol-function 'string->integer))

(defun number-or-nil->string (number)
  (if (numberp number)
      (number-to-string number)
    ""))
(make-obsolete 'integer-or-nil->string 'number-or-nil->string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffers
;;;

;;; This macro duplicates BODY.  This should be fixed.
;; This version, which works when body moves point in a buffer displayed in
;; a window other than the selected window, is from Joe Wells
;; <jbw@cs.bu.edu>.  (If Lisp code moves point in a buffer displayed in a
;; window other than the selected window, Emacs kindly restores point in
;; the buffer to its window's version of point.)
(defmacro in-buffer (buffer &rest body)
  "Executes, in BUFFER, forms of BODY."
  ;; Need get-buffer-create because BUFFER might be a string.
  (` (let ((target-buffer (get-buffer-create (, buffer)))
	   (this-buffer (current-buffer)))
       (if (eq target-buffer this-buffer)
	   (progn
	     (,@ body))
	 ;; Can't use save-excursion here because we only want to save the
	 ;; current buffer, not its value for point.
	 (unwind-protect
	     (progn
	       (set-buffer target-buffer)
	       (let* ((target-window (get-buffer-window target-buffer))
		      (track-window-point-p
		       (and (not (eq target-window (selected-window)))
			    (eq (window-point target-window) (point)))))
		 (prog1
		     (progn
		       (,@ body))
		   (if (and track-window-point-p
			    ;; *** Do I need this check?
			    (eq (current-buffer) target-buffer)
			    (eq target-window (get-buffer-window target-buffer))
			    (not (eq target-window (selected-window))))
		       (set-window-point target-window (point))))))
	   (if (and (bufferp this-buffer)
		    (buffer-name this-buffer))
	       (set-buffer this-buffer)))))))
(put 'in-buffer 'lisp-indent-hook 1)
(put 'in-buffer 'edebug-form-spec '(&rest form))

;; Why not just use save-excursion for this?
(defmacro in-buffer-simple (buffer &rest body)
  "Execute, in BUFFER, forms of BODY.
BODY shouldn't move point in a buffer displayed in a non-selected window."
  (` (save-excursion
       (set-buffer (, buffer))
       (,@ body))))
(put 'in-buffer-simple 'lisp-indent-hook 1)

(defmacro in-window (window &rest body)
  "Executes, in WINDOW, forms of BODY.
This is more useful than `in-buffer' for window manipulation, as by `scroll-up'."
  (` (let ((this-window (selected-window)))
       (unwind-protect
	   (progn
	     (select-window (, window))
	     (,@ body))
	 (select-window this-window)))))
(put 'in-window 'lisp-indent-hook 1)
(put 'in-window 'edebug-form-spec '(&rest form))


;; Similar tricks can be done with syntax-table and current-local-map.
;; Adapted from code by Joe Wells.
(defun db-copy-buffer-local-variables (buffer)
  "Copy the values of all of BUFFER's local variables into the current buffer."
  (let ((blv (in-buffer-simple buffer (buffer-local-variables)))
	pair symbol)
    (while (consp blv)
      (setq pair (car blv)
	    symbol (car pair)
	    blv (cdr blv))
      ;; nil and 0 can be bogus local variables; never copy buffer-undo-list
      (if (not (memq symbol '(0 nil buffer-undo-list)))
	  (progn
	    (if (not (symbolp symbol))
		(error "\"%s\" should be a symbol with value \"%s\""
		       symbol (cdr pair)))
	    (make-local-variable symbol)
	    (set symbol (cdr pair)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Windows
;;;

;; I'm not sure whether this works if the last line is wrapped.
;; Likewise for bob-visible-p and wrapped first line (is that possible?).
(defun eob-visible-p ()
  (save-excursion
    (let ((ht (window-height (selected-window))))
      (move-to-window-line (- ht 2))
      (end-of-line)
      (eobp))))

(defun bob-visible-p ()
  (save-excursion
    (move-to-window-line 0)
    (beginning-of-line)
    (bobp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
;;;

;; It would be nice to make the delimiter an optional argument to these.

;; A syntax table implementation would be too complicated, so hardcode space
;; and tab.
(defun db-string-split-last-word (splitee &optional exceptions delimiter)
  "Return list of two strings (all-but-last-word last-word).
If there is only one word, return (SPLITEE \"\").
The result strings can be concatenated to return the original string,
with the exception of some number (at least one) of spaces and tabs,
and possibly a comma immediately preceding them.
Optional arg EXCEPTIONS, if non-nil, is a regexp (containing spaces or tabs)
which, if found at the end of SPLITEE, should be considered a single word.
Optional arg DELIMITER, if non-nil, is used instead of the default word
delimiter.  It should be a regexp."
  (if (not delimiter) (setq delimiter ",?[ \t]+"))
  (if (or (and exceptions
	       (string-match (concat delimiter "\\(" exceptions "\\)$") splitee))
	  (string-match (concat delimiter "\\([a-zA-Z0-9'-]+\\)$") splitee))
      (list (substring splitee 0 (match-beginning 0))
	    (substring splitee (match-beginning 1)))
    (list splitee "")))

;; maybe what I really want is string-to-word-list
(defun db-string-split-first-word (splitee &optional delimiter)
  "Return list of strings (first-word remaining-words).
Argument SPLITEE is split at the first sequence of spaces and tabs.
Optional arg DELIMITER, if non-nil, is used instead of the default word
delimiter.  It should be a regexp."
  (if (string-match (or delimiter "[ \t]+") splitee)
      (list (substring splitee 0 (match-beginning 0))
	    (substring splitee (match-end 0)))
    (list splitee "")))

(defun count-array (item array)
  "Return the number of times that ITEM appears in ARRAY; test with `equal'."
  (let ((limit (length array))
	(result 0)
	(index 0))
    (while (< index limit)
      (if (equal item (aref array index))
	  (setq result (1+ result)))
      (setq index (1+ index)))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists
;;;

;; Which should come first, N or LIST?
(defun firstn (n list)
  "Return a copy of the first N elements of LIST."
  (let ((result '()))
    (while (and list (> n 0))
      (setq result (cons (car list) result)
	    n (1- n)
	    list (cdr list)))
    (nreverse result)))

;;; Emacs provides rassq, but rassoc is nice to have too.
(defun db-rassoc (elt list)
  "Return non-nil if ELT is the cdr of an element of LIST.  Comparison done with  `equal'.
The value is actually the element of LIST whose cdr is ELT."
  (let (result)
    (while list
      (if (equal elt (cdr (car list)))
	  (setq result (car list)
		list nil)
	(setq list (cdr list))))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Messages
;;;

(defvar use-electric-help-p nil
  "*Non-nil if Emacs programs should use electric help where possible.
Don't set this to a non-nil value unless the ehelp package is available.")

;; Regrettably, this produces big code by including its argument twice.
(defmacro with-electric-help-maybe (&rest body)
  "Similar to `with-electric-help' if `use-electric-help-p' is non-nil;
otherwise like `with-output-to-temp-buffer' with the \"*Help*\" buffer.
Ehelp is loaded if necessary.
BODY is not a thunk (a function of no arguments), as with `with-electric-help',
but simply a set of forms."
  (` (if use-electric-help-p
	 (progn
	   (require 'ehelp)
	   (with-electric-help
	    (function (lambda ()
			(,@ body)))))
       (with-output-to-temp-buffer "*Help*"
	 (,@ body)))))

;; Originally by Joe Wells <jbw@cs.bu.edu>
(defun db-best-fit-message (text &optional buffer)
  "Show TEXT in echo area if it fits or in optional BUFFER (default *Message*)."
  (or buffer (setq buffer "*Message*"))
  (save-excursion
    (set-buffer (get-buffer-create " temp printing buffer"))
    (erase-buffer)
    (buffer-disable-undo (current-buffer))
    (insert text)
    (delete-region (point)
		   (progn
		     (skip-chars-backward " \t\n")
		     (point)))
    (cond ((and (< (current-column) (frame-width))
		(progn
		  (beginning-of-line 1)
		  (bobp)))
	   ;; This can't be just buffer, even though that's non-nil,
	   ;; because it might not be an existing buffer.
	   (delete-windows-on (get-buffer-create buffer))
	   (message "%s" (buffer-substring (point-min) (point-max))))
	  (t
	   (with-electric-help-maybe
	    (princ text))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cursor movement
;;;

(defun db-forward-line-wrapping (arg)
  "Like forward-line, but wrap around to the beginning of the buffer if
it encounters the end."
  (interactive "p")
  (let ((to-go (forward-line arg)))
    (cond ((or (> to-go 0) (not (bolp)))
	   (goto-char (point-min))
	   (db-forward-line-wrapping to-go))
	  ((< to-go 0)
	   (goto-char (point-max))
	   (db-forward-line-wrapping (1+ to-go))))))

(defun db-current-line ()
  "Return the line number of the line containing point."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffer substitution
;;;

;;   "Make replacements in the current buffer according to SUBSTITUTIONS.
;;   SUBSTITUTIONS is list of pairs of strings; the cdr of each pair will be
;; substituted for the car, in order, unless optional argument BACKWARD is
;; non-nil, in which case the car is substituted for the cdr and the
;; substitutions are done in reverse order.
;;   If optional third argument CHECK is non-nil, the user is warned if any of
;; the substituted-in strings already appears in the buffer; such a situation
;; would make substitution, then unsubstitution, not yield a result identical
;; to the original buffer, since all instances of the substituted-in string
;; will be assumed on the reverse substitution to have been the result of
;; replacing a substituted-for string.
;;   Return nil if CHECK is nil or there were no ambiguities; otherwise
;; return a list of replacements creating ambiguity."
(defun buffer-substitute (substitutions backward check)
  (if backward
      (setq substitutions (mapcar (function (lambda (sub-cons)
				     (cons (cdr sub-cons) (car sub-cons))))
				  (reverse substitutions))))
  ;; (message "buffer-substitute:  substitutions = %s" substitutions)

  ;; Should do all checking before any substitutions are done.
  ;; Bad:
  ;;  * any to-string appears in text, unless it's an earlier from-string.
  ;;  * any to-string appears in previous to-string without intervening
  ;;    from-string.  (but then it's just stupidly inefficient)

  ;; Perhaps be able to override checks of the substitutions pairs.  Such
  ;; checks will be hairy anyway because we may create an ambiguity by
  ;; replacing part of a match such that the other part is still in the
  ;; buffer unchanged.  With one-character stuff this is obviously much
  ;; easier.
  ;; Perhaps do the checks by character...?

  ;; Don't want to do checks as we do the substitutions because that leaves
  ;; us in a bad state:  the work is partially done.  We want to let the
  ;; guy know before we start.

  ;; If, in the case of an ambiguity, we're just going to give up anyway,
  ;; then perhaps it isn't so bad to do the checks after part of the work
  ;; is done (except that the work already done would have been a waste of
  ;; time).  So maybe make the check of the pairs a preliminary one and do
  ;; the real check as we go.  But in some cases such checks won't be
  ;; necessary.

  ;; Perhaps if we want checks on the substitution strings themselves, then
  ;; do that separately beforehand and call this with check = nil.

  ;; And hey, searching for one instance of a string is pretty cheap, after
  ;; all.  And I don't expect to be calling this with a truly enormous list
  ;; of substitutions anyway.

  ;; I think I'm being too paranoid here.  In many cases I'm not even going
  ;; to call this with check = t.

  (let (from-string to-string ambiguity ambiguities)
    (while substitutions
      (setq from-string (car (car substitutions))
	    to-string (cdr (car substitutions)))
      ;; (message "Substituting %s for %s." to-string from-string)
      (goto-char (point-min))

      (if (and check (search-forward to-string nil t))
	  (progn
	    (setq ambiguity (car substitutions))
	    (goto-char (point-min))))

      (replace-string from-string to-string)

      ;; Don't complain if we didn't actually do any substitution.
      (if ambiguity
	  (progn
	    (if (not (= (point) (point-min)))
		(setq ambiguities (cons ambiguity ambiguities)))
	    (setq ambiguity nil)))

      (setq substitutions (cdr substitutions)))
    ambiguities))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File local variables
;;;

(defun db-operate-on-local-variables (region-function)
  "Apply REGION-FUNCTION to the local-variables region of the buffer.
Return t if a local-variables region was found; REGION-FUNCTION should act
by side effect."
  (goto-char (point-max))
  (search-backward "\n\^L"
		   (max (- (point-max) 3000) (point-min)) 'move)
  (if (search-forward "Local Variables:" nil t)
      (progn
	(beginning-of-line 1)
	(funcall region-function (point) (point-max))
	t)))

(defun db-really-hack-local-variables ()
  "Call `hack-local-variables', ignoring variables that limit it."
  (let ((enable-local-eval t)
	(enable-local-variables t))
    (hack-local-variables)))


;; This page feed is to defeat local variables processing.

;;; db-util.el ends here
