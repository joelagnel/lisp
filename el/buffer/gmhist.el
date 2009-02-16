;;;; gmhist.el - Provide generic minibuffer history for commands
(defconst gmhist-version
  "$Id: gmhist.el,v 4.19 1991/09/20 13:20:58 sk RelBeta $")

;; Copyright (C) 1990 by Sebastian Kremer <sk@thp.uni-koeln.de>

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
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LISPDIR ENTRY for the Elisp Archive ===============================
;;    LCD Archive Entry:
;;    gmhist|Sebastian Kremer|sk@thp.uni-koeln.de
;;    |Generic minibuffer history package.
;;    |$Date: 1991/09/20 13:20:58 $|$Revision: 4.19 $|

;; INSTALLATION ======================================================
;; 
;; Put this file into your load-path and the following in your
;; ~/.emacs:
;; 
;;   (autoload 'read-with-history-in "gmhist")
;;   (autoload 'read-file-name-with-history-in "gmhist")
;;   (autoload 'completing-read-with-history-in "gmhist")
;;   (autoload 'gmhist-make-magic "gmhist")

;; USAGE =============================================================
;; 
;;   - as an Elisp programmer: use functions read-with-history-in,
;;     completing-read-with-history-in, read-file-name-with-history-in or
;;     gmhist-interactive inside the interactive clause of your functions
;;     instead of a string specification.  See the examples at the end of
;;     the file.
;;
;;   - as an Emacs user: To provide `simple' functions with history,
;;     just type M-x gmhist-make-magic and enter the name of the
;;     function, e.g., `eval-expression'.  This function's arguments
;;     are then remembered across calls and are available by typing
;;     M-p to the minibuffer prompt of the function.  More history
;;     commands are mentioned in the documentation of variable
;;     gmhist-map.
;;
;;     Type M-x gmhist-remove-magic to restore the function's old
;;     interactive behaviour.
;;
;;     `Simple' functions are those that prompt for strings, file
;;     names or lisp objects and perhaps use prefix args and the
;;     region.  See the file gmhist-app.el for examples with simple
;;     and other functions.

;; I'd like to thank Jamie Zawinski, Piet van Oostrum and Mike
;; Williams for very helpful feedback and ideas.


(provide 'gmhist)

;; Emacs 19 has s-expr interactive's on some functions (sometimes to
;; emulate functionality gmhist would give).  So we sometimes have to
;; test this to avoid letting gmhist-make-magic bombing on non-string
;; interactive specifications:
(defvar gmhist-emacs-19-p (equal (substring emacs-version 0 2) "19"))

(defvar gmhist-default-format "[%s] "	; saves screen space, too
  "Format used by gmhist to indicate the presence of a default value.
Set this to \"(default %s) \" to get the standard format.")

(defvar gmhist-search-history nil "History of history searches.")

(defun read-with-history-in (GMHIST-SYMBOL rwhi-prompt &optional
		GMHIST-INITIAL GMHIST-READ)
  ;; We have to be careful about dynamical scoping here so as not to
  ;; shadow other lisp code that depends on fluid vars like `prompt
  ;; (notorious in minibuffer code, e.g. electric-replace).
  ;; That's why our own fluid vars have upper-case names starting with
  ;; GMHIST- and why `rwhi-prompt' instead of `prompt' is used as
  ;; formal argument.  Similar below.
  "\
Read a string, maintaining minibuffer history across calls in GMHIST-SYMBOL,
  prompting with PROMPT, with optional GMHIST-INITIAL as initial contents.
If optional fourth arg GMHIST-READ is non-nil, then interpret the
  result as a lisp object and return that object.
See variable gmhist-map for history commands available during edit.
Example:
    (defun foo-command (cmd)
      (interactive (list (read-with-history-in 'foo-history \"Foo: \" )))
      (message \"Fooing %s...\" cmd))

See function gmhist-make-magic on how to give an existing function
history.

These properties (see function put) of GMHIST-SYMBOL are supported:

cursor-end    Put cursor at end of a newly retrieved history line.
cursor-pos    A regexp to put the cursor on.
keep-dups     If t, duplicate commands are remembered, too.
initial-hist  Initial value of the history list.
hist-ignore   Regexp of commands that are not to be added to the history.
backup	      If t, backup in the history list (as if user had typed
	      M-p as first thing).  Can also be an integer to backup
	      more than one history item.
default       An empty string as input will default to the last
	      command (whether the last command was added to the
	      history or not).  The default is stored in this
	      property, thus its initial value is the first default.
dangerous     Commands matching this regexp will never be the default.
no-default    If you don't want defaults at all, set this to t.

Use the following only if you know what you are doing:

hist-function Name of a function to call instead of doing normal
              history processing.  read-with-history-in becomes
              effectively an alias for this function.

These will be flushed soon (use let-binding minibuffer-completion-table
etc. instead):

hist-map      Minibuffer key map to use instead of gmhist-map.
completion-table
completion-predicate
	      Used in completion on history strings, when the hist-map
	      property has gmhist-completion-map as value.
	      The special value `t' for the table means to use the
	      current history list.
	      Thus, to get completion on history items just do:
		(put 'foo-history 'hist-map gmhist-completion-map)
		(put 'foo-history 'completion-table t)

Hooks:
  gmhist-after-insert-hook is run after a history item is
    inserted into the minibuffer.
  gmhist-load-hook is run after this package is loaded.
  gmhist-hook is run as first thing inside read-with-history-in.
  gmhist-before-move-hook is run before history motion takes place.
    Function gmhist-remember-zero is a candidate for that hook.
"
  ;; We don't use property names prefixed with 'ghmist-' because the
  ;;   caller has freedom to use anything for GMHIST-SYMBOL.
  ;; The history list is never truncated, but I don't think this will
  ;;   cause problems.  All histories together have at most a few k.
  ;;   On the other hand, some people run an Emacs session for weeks.
  ;;   Could use gmhist-hook to truncate the current history list.
  ;; You can use 'initial-hist to save (part of) the history in a file
  ;;   and provide it at next startup.  [Is there an exit-emacs-hook?]
  ;; You can use 'hist-function to implement a completely different
  ;;   history mechanism, e.g., a ring instead of a list, without having
  ;;   to modify existing gmhist applications.
  (run-hooks 'gmhist-hook)
  (let ((hist-function (get GMHIST-SYMBOL 'hist-function)))
    (if (fboundp hist-function)		; hist-function must be a symbol
	(funcall hist-function		;  not lambda
		 GMHIST-SYMBOL rwhi-prompt GMHIST-INITIAL GMHIST-READ)
      (or (boundp GMHIST-SYMBOL) ; history list defaults to nil
	  (set GMHIST-SYMBOL (get GMHIST-SYMBOL 'initial-hist)))
      ;; else do the usual history processing simply using lists:
      (let* ((history (symbol-value GMHIST-SYMBOL))
	     (minibuffer-completion-table (let ((table
						 (get GMHIST-SYMBOL
						      'completion-table))) 
					    (if (eq t table)
						(mapcar (function list)
							history)
					      table)))
	     (minibuffer-completion-predicate (get GMHIST-SYMBOL
						   'completion-predicate))
	     (minibuffer-history-symbol GMHIST-SYMBOL))
	(gmhist-new-read-from-minibuffer rwhi-prompt
				     GMHIST-INITIAL
				     (or (get GMHIST-SYMBOL 'hist-map)
					 gmhist-map)
				     GMHIST-READ)))))

(defun completing-read-with-history-in (crwhi-hist-sym &rest args)
  "Like completing-read, but with additional first arg HISTORY-SYMBOL."
  (let ((minibuffer-history-symbol crwhi-hist-sym))
    (apply 'gmhist-completing-read args)))

(defun gmhist-completing-read (crwhi-prompt table
					    &optional predicate
					    mustmatch initial)
  "Like completing-read, but see minibuffer-history-symbol."
  (let ((minibuffer-completion-confirm (if (eq mustmatch t) nil t))
	(minibuffer-completion-table table)
	(minibuffer-completion-predicate predicate))
    (gmhist-new-read-from-minibuffer crwhi-prompt
				     initial
				     (gmhist-lookup-keymap
				      (if mustmatch
					  gmhist-must-match-map
					gmhist-completion-map)))))


(defun read-file-name-with-history-in (crwhi-hist-sym &rest args)
  "Like read-file-name, but with additional first arg HISTORY-SYMBOL."
  (let ((file-history-symbol crwhi-hist-sym))
    (apply 'gmhist-read-file-name args)))

(defvar file-history-symbol 'file-history
  "*If non-nil, it is the name (a symbol) of a variable on which to cons
filenames entered in the minibuffer.
You may let-bind this to another symbol around calls to read-file-name.")

(defun gmhist-read-file-name
  (grfn-prompt &optional dir default mustmatch initial)
  "Args: PROMPT &optional DIR DEFAULT MUSTMATCH INITIAL.
Read file name, maintaining history in file-history-symbol, prompting
  with PROMPT, with optional INITIAL input and completing in directory DIR. 
Value is not expanded!  You must call expand-file-name yourself.
Default name to arg DEFAULT if user enters a null string (or, if
  INITIAL was given, leaves it unchanged).
MUSTMATCH non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
DIR defaults to current buffer's default-directory.

This function differs from read-file-name in providing a history of
filenames bound to file-history-symbol and (for pre-Emacs 19) in
providing an argument INITIAL not present in Emacs 18's read-file-name."
  (setq dir (or dir default-directory)
	default (or default buffer-file-name))
  (if file-history-symbol
      (progn (put file-history-symbol 'cursor-end t)
	     (put file-history-symbol 'no-default t)))
  (if initial
      (setq initial (gmhist-unexpand-home initial))
    (if insert-default-directory
	(setq initial (gmhist-unexpand-home dir))))
  (let* ((minibuffer-completion-confirm (if (eq mustmatch t) nil t))
	 (minibuffer-completion-table 'read-file-name-internal)
	 (minibuffer-completion-predicate dir)
	 (minibuffer-history-symbol file-history-symbol)
	 (val (gmhist-new-read-from-minibuffer
	       grfn-prompt initial (gmhist-lookup-keymap
				    (if mustmatch
					gmhist-filename-must-match-map
				      gmhist-filename-completion-map)))))

    (or (and (or (and (stringp initial)
		      (string= initial val))
		 (and (null initial)
		      (zerop (length val))))
	     default)
	(substitute-in-file-name val))))

(defun gmhist-unexpand-home (file)
  ;; Make prompt look nicer by un-expanding home dir.
  ;; read-file-name does this, too.
  ;; Avoid clobbering match-data with string-match.
  (let* ((home (expand-file-name "~/"))
	 (home-len (length home))
	 (file-len (length file)))
    (if (and home
	     (stringp file)
	     (>= file-len home-len)
	     (string= home (substring file 0 home-len))
	     (eq ?/ (aref file (1- home-len))))
	(concat "~/" (substring file home-len))
      file)))

(defun read-buffer-with-history-in (rbwhi-hist-sym &rest args)
  "Like read-buffer, but with additional first arg HISTORY-SYMBOL."
  (let ((buffer-history-symbol rbwhi-hist-sym))
    (apply 'gmhist-read-buffer args)))

(defvar buffer-history-symbol 'buffer-history
  "*If non-nil, it is the name (a symbol) of a variable on which to cons
buffer names entered in the minibuffer.")

(defun gmhist-read-buffer (grb-prompt &optional default existing)
  "Read a buffer name, maintaining history in buffer-history-symbol and return as string.
Args PROMPT &optional DEFAULT EXISTING.
Optional arg EXISTING means an existing buffer must be entered."
  (if (bufferp default);; want string in prompt, not buffer object
      (setq default (buffer-name default)))
  (if buffer-history-symbol
      (put buffer-history-symbol 'default default))	; also if nil
  (let* ((minibuffer-history-symbol buffer-history-symbol)
	 (name (gmhist-completing-read
		grb-prompt
		;;(function (lambda (buf) (list (buffer-name buf))))
		;; convert to alist:
		(mapcar 'list (mapcar 'buffer-name (buffer-list)))
		(function (lambda (elt) (get-buffer (car elt))))
		existing)))
    (if (equal "" name)
	default
      name)))

(defvar minibuffer-history-symbol 'minibuffer-history
  "*If non-nil, it is the name (a symbol) of a variable on which to cons
the string entered in the minibuffer.
Input is stored as string, even for e.g. `read-buffer'.")

(defvar minibuffer-history nil
  "List of strings entered using the minibuffer, most recent first.")

(put 'minibuffer-history 'no-default t)

(defvar minibuffer-history-read-only nil
  "If non-nil, nothing will be stored on `minibuffer-history-symbol'.
History motions commands are still available in the minibuffer.")

;; Save the subr, we need it inside the redefined version:
(or (fboundp 'gmhist-old-read-from-minibuffer)
    (fset 'gmhist-old-read-from-minibuffer
	  (symbol-function 'read-from-minibuffer)))

(defun gmhist-new-read-from-minibuffer
  (gnrfm-prompt &optional initial-contents keymap read position)
  "Read a string from the minibuffer, prompting with string PROMPT.
If optional second arg INITIAL-CONTENTS is non-nil, it is a string
  to be inserted into the minibuffer before reading input.
Third arg KEYMAP is a keymap to use whilst reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg POSITION, if non-nil, is where to put point
  in the minibuffer after inserting INITIAL-CONTENTS.

The ambient value of `minibuffer-history-symbol' (q.v.) is used and set.

*** This is the gmhist version.***"
  (if (null minibuffer-history-symbol)
      (if gmhist-emacs-19-p
	  (gmhist-old-read-from-minibuffer
	   gnrfm-prompt initial-contents keymap read position)
	(gmhist-old-read-from-minibuffer gnrfm-prompt initial-contents
					 keymap read))
    (gmhist-read-from-minibuffer
     gnrfm-prompt initial-contents keymap read position)))

(defun gmhist-read-from-minibuffer (grfm-prompt
				    &optional
				    initial-contents keymap read position)
  (or keymap (setq keymap minibuffer-local-map))
  (or minibuffer-history-read-only
      (boundp minibuffer-history-symbol) ; history list defaults to nil
      ;; create history list if not already done
      (set minibuffer-history-symbol
	   (get minibuffer-history-symbol 'initial-hist)))
  (let* ((minibuffer-history-position 0) ; fluid var for motion commands
	 (minibuffer-initial-contents initial-contents)	; ditto
	 (history (symbol-value minibuffer-history-symbol))
	 ;; Command is an s-exp when read->t.  In this case,
	 ;; cannot have empty input:
	 (no-default (or read
			 (get minibuffer-history-symbol 'no-default)))
	 (dangerous (if no-default
			nil
		      (get minibuffer-history-symbol 'dangerous)))
	 ;; Idea for 'backup feature by Mike Williams
	 (backup (get minibuffer-history-symbol 'backup))
	 (default (if no-default
		      nil
		    (get minibuffer-history-symbol 'default)))
	 (the-prompt (if default
			 (concat grfm-prompt (format gmhist-default-format
						     default))
		       grfm-prompt))
	 (the-initial (if (or minibuffer-initial-contents
			      (not backup))
			  minibuffer-initial-contents
			;; else we must backup in the history list
			(setq backup (min (max 0 (or (and (integerp backup)
							  backup)
						     1))
					  (length history)))
			(if (zerop (setq minibuffer-history-position backup))
			    nil
			  ;; else backup is at least 1
			  (let ((backup-input (nth (1- backup) history)))
			    (if read
				(prin1-to-string backup-input)
			      backup-input)))))
	 command)
    ;; Read the command from minibuffer, providing history motion
    ;; key map and minibuffer completion
    (setq command
	  (if position
	      ;; avoid passing POSITION arg unless given (presumably
	      ;; we are in Emacs 19 then)
	      (gmhist-old-read-from-minibuffer the-prompt the-initial keymap
					       position)
	    (gmhist-old-read-from-minibuffer the-prompt the-initial keymap)))
    ;; Care about default values unless forbidden:
    (or no-default
	(setq command (gmhist-handle-default command default dangerous)))
    (if minibuffer-history-read-only
	nil
      (let (ignore)
	;; Add to history if first command, or not a dup, or not to be ignored
	(or (and history
		 (or (if (get minibuffer-history-symbol 'keep-dups)
			 nil
		       (equal command (car history)))
		     (if (stringp (setq ignore (get minibuffer-history-symbol
						    'hist-ignore)))
			 (string-match ignore
				       (gmhist-stringify (car history))))))
	    (set minibuffer-history-symbol (cons command history)))))
    ;; Return command's value to caller:
    (if read
	(car (read-from-string command))
      command)))

(defun gmhist-handle-default (command default dangerous)
  (if (string= "" command)
      (if default (setq command default)))
  ;; Set default value unless it is dangerous.
  (or (and (stringp dangerous)
	   ;; Should actually save match-data as we call string-match
	   (string-match dangerous (gmhist-stringify command)))
      (put minibuffer-history-symbol 'default command))
  ;; Return the prefrobnicated command:
  command)


;; Minibuffer key maps to implement history

(or (fboundp 'gmhist-define-keys)
    (defun gmhist-define-keys (map)
      "Bind the standard history commands in MAP, a key map.

When gmhist is loaded, this function is only defined if you have not
already defined it, so that you can customize it without worrying
about load order."
      (define-key map "\M-p" 'gmhist-previous)
      (define-key map "\M-n" 'gmhist-next)
      (define-key map "\M-r" 'gmhist-search-backward)
      (define-key map "\M-s" 'gmhist-search-forward)
      ;;(define-key map "\M-<" 'gmhist-beginning)
      ;;(define-key map "\M-<" 'gmhist-beginning)
      ;; Last two for bash/readline compatibility. Better M-a and M-e ?
      ;; In  query-replace, multi-line text together with next-line's
      ;; misfeature of adding blank lines really lets you lose without M-<
      ;; and M->.
      ;;(define-key map "\M-a" 'gmhist-beginning)
      ;;(define-key map "\M-e" 'gmhist-end)
      ;; M-a is already used in electric replace
      ;; Try this as general purpose mover:
      (define-key map "\M-g" 'gmhist-toggle)
      (define-key map "\M-G" 'gmhist-switch-history)
      (define-key map "\M-?" 'gmhist-show)))

(defun gmhist-lookup-keymap (map)
  (if (keymapp map)
      map
    (gmhist-lookup-keymap (symbol-value map))))

(defvar gmhist-map nil
  "Key map for generic minibuffer history.
\\<gmhist-map>\\[gmhist-previous], \\[gmhist-next], \
\\[gmhist-beginning], \\[gmhist-end] move through, \
\\[gmhist-search-backward] and \\[gmhist-search-forward] search,
\\[gmhist-show] displays the history:
\\{gmhist-map}")

(if gmhist-map
    nil
  (setq gmhist-map (copy-keymap minibuffer-local-map))
  (gmhist-define-keys gmhist-map))

(defvar gmhist-completion-map nil
  "Key map for generic minibuffer history with completion, see gmhist-map.")

(if gmhist-completion-map
    nil
  ;; If you have loaded D. Gillespie's complete.el or Christopher
  ;; McConnell's completer.el *before* gmhist, you get it in gmhist,
  ;; too:
  (setq gmhist-completion-map (copy-keymap minibuffer-local-completion-map))
  (gmhist-define-keys gmhist-completion-map))

(defvar gmhist-must-match-map nil
  "Key map for generic minibuffer history with completion that must match,
see gmhist-map.")

(if gmhist-must-match-map
    nil
  (setq gmhist-must-match-map (copy-keymap minibuffer-local-must-match-map))
  (gmhist-define-keys gmhist-must-match-map))

(defvar gmhist-filename-completion-map 'gmhist-completion-map
  "A keymap (or a symbol pointing to one) to use in filename
completion that need not match.  Defaults to 'gmhist-completion-map.")

(defvar gmhist-filename-must-match-map 'gmhist-must-match-map

  "A keymap (or a symbol pointing to one) to use in filename
completion that must match.  Defaults to 'gmhist-must-match-map.") 


;; Minibuffer commands to implement history
;; They run inside read-with-history-in and heavily depend on fluid
;; vars from there.

(defun gmhist-goto (n)
  ;; Go to history position N, 1 <= N <= length of history
  ;; N<0 means the future and inserts an empty string
  ;; N=0 means minibuffer-initial-contents (fluid var from
  ;;     gmhist-new-read-from-minibuffer)
  (run-hooks 'gmhist-before-move-hook)
  (erase-buffer)
  (setq minibuffer-history-position n)
  (if (< n 0)
      nil
    (setq elt (if (= n 0)
		  (or minibuffer-initial-contents "")
		(nth (1- n) (symbol-value minibuffer-history-symbol))))
    (insert (gmhist-stringify elt))
    (run-hooks 'gmhist-after-insert-hook)
    ;; next two actually would be a good application for this hook
    (goto-char (if (get minibuffer-history-symbol 'cursor-end)
		   (point-max)
		 (point-min)))
    (let ((pos (get minibuffer-history-symbol 'cursor-pos)))
      (if (stringp pos)
	  (if (eobp)
	      (re-search-backward pos nil t)
	    (re-search-forward pos nil t))))))

(defun gmhist-beginning ()
  "Go to the oldest command in the history."
  (interactive)
  (gmhist-goto (length (symbol-value minibuffer-history-symbol))))

(defun gmhist-end ()
  "Position before the most recent command in the history."
  (interactive)
  (gmhist-goto 0))

(defun gmhist-toggle (&optional n)
  "If at end of history, move to beginning, else move to end.
Prefix arg is history position to go to."
  (interactive "P")
  (if n
      (gmhist-goto (prefix-numeric-value n))
    (if (= 0 minibuffer-history-position)
	(gmhist-beginning)
      (gmhist-end))))

(defun gmhist-switch-history (new-history)
  "Switch to a different history."
  (interactive
   (let ((enable-recursive-minibuffers t))
     (list (read-from-minibuffer "Switch to history: " nil nil t))))
  (setq minibuffer-history-symbol new-history
	minibuffer-history-position 0))

(defun gmhist-next (n)
  "Go to next history position."
  ;; fluid vars: minibuffer-history-symbol minibuffer-history-position
  ;; Inserts the next element of minibuffer-history-symbol's value
  ;; into the minibuffer.
  ;; minibuffer-history-position is the current history position.
  (interactive "p")
  ;; clip the new history position to the valid range:
  (let ((narg (min (max 0 (- minibuffer-history-position n))
		   (length (symbol-value minibuffer-history-symbol)))))
    (if (= minibuffer-history-position narg)
	(error "No %s item in %s"
	       (if (= 0 minibuffer-history-position) "following" "preceding")
	       minibuffer-history-symbol)
      (gmhist-goto narg))))

(defun gmhist-previous (n)
  "Go to previous history position."
  (interactive "p")
  (gmhist-next (- n)))

;; Searching the history

(defun gmhist-search-backward (regexp &optional forward)
  "Search backward in the history list for REGEXP."
  (interactive
   (let ((enable-recursive-minibuffers t))
     (list (read-with-history-in 'gmhist-search-history
				 "History search (regexp): "))))
  (let* (found
	 (direction (if forward -1 1))
	 (pos (+ minibuffer-history-position direction)) ; find _next_ match!
	 (history (symbol-value minibuffer-history-symbol))
	 (len (length history)))
    (while (and (if forward (> pos 0) (<= pos len))
		(not (setq found
			   (string-match
			    regexp
			    (gmhist-stringify (nth (1- pos) history))))))
      (setq pos (+ pos direction)))
    (or found (error "%s not found in %s" regexp minibuffer-history-symbol))
    (gmhist-goto pos)))

(defun gmhist-search-forward (regexp)
  "Search forward in the history list for REGEXP."
  (interactive
   (let ((enable-recursive-minibuffers t))
     (list (read-with-history-in 'gmhist-search-history
				 "History search forward (regexp): "))))
  (gmhist-search-backward regexp t))

;; Misc.

(defun gmhist-stringify (elt)
  ;; If ELT is not a string, convert it to one.
  (if (stringp elt) elt (prin1-to-string elt)))

(defun gmhist-show ()
  "Show the history list in another buffer.
Use \\[scroll-other-window] to scroll, with negative arg to scroll back."
  (interactive)
  (let ((count 0))
  (with-output-to-temp-buffer (concat "*" (symbol-name minibuffer-history-symbol) "*")
    (mapcar
     (function
      (lambda (x)
	(princ (format "%2s%2d: %s\n"
		       (if (eq (setq count (1+ count))
			       minibuffer-history-position)
			   "> "
			 "  ")
		       count x))))
     (symbol-value minibuffer-history-symbol)))))

(defun gmhist-remember-zero ()
  "Put this function on gmhist-before-move-hook to make gmhist
remember the initial value even after you edited it:

    (setq gmhist-before-move-hook 'gmhist-remember-zero)"
  (if (zerop minibuffer-history-position)
      (setq minibuffer-initial-contents (buffer-string))))

;; Hack up interactive specifications of existing functions

(defun gmhist-copy-function (fun)
  ;; copy-sequence does not copy recursively.
  ;; Iteration is faster than recursion, and we need just two levels
  ;; to be able to use setcdr to mung the interactive spec.
  (let (old new elt)
    (setq old (symbol-function fun))
    (cond
     ((consp old)  ; interpreted, or v18 compiled
      (while old
	(setq elt (car old)
	      old (cdr old)
	      new (cons (if (sequencep elt)
			    (copy-sequence elt)
			  elt)
			new)))
      (nreverse new))
     (t  ; v19 compiled
      (setq new (append old nil))
      (setcar (nthcdr 5 new) (copy-sequence (aref old 5)))
      (apply 'make-byte-code new)))))

(defun gmhist-check-autoload (fun)
  "If FUN is an autoload, load its definition."
  (let ((lis (symbol-function fun)))
    (if (and (listp lis)		; FUN could also be a subr
	     (eq 'autoload (car lis)))
	(load (nth 1 lis)))))

(defun gmhist-replace-spec (fun new-spec &optional copy-first)
  "Replace the interactive specification of FUN with NEW-SPEC.
FUN must be a symbol with a function definition.
Autoload functions are taken care of by loading the appropriate file first.
If FUN is a pure storage function (one dumped into Emacs) it is first
  copied onto itself, because pure storage cannot be modified.
  Optional non-nil third arg COPY-FIRST is used internally for this.
The old spec is put on FUN's gmhist-old-interactive-spec property.  
  That property is never overwritten by this function.  It is used by
  function gmhist-remove-magic."
  (gmhist-check-autoload fun)
  (if copy-first			; copy (from pure storage)
      (fset fun (gmhist-copy-function fun)))
  (let* ((flambda (gmhist-symbol-function fun))
	 (fint (and (consp flambda)
		    (if (eq 'interactive (car-safe (nth 2 flambda)))
			(nth 2 flambda)
		      (if (eq 'interactive (car-safe (nth 3 flambda)))
			  (nth 3 flambda)
			(error "%s is not interactive" fun)))))
	 (old-spec (if fint
		       (nth 1 fint)
		     (gmhist-spec fun))))
    ;; Save old interactive spec as property of FUN:
    (or (get fun 'gmhist-old-interactive-spec)
	(put fun 'gmhist-old-interactive-spec old-spec))
    ;; Replace '(interactive OLD-SPEC) with '(interactive NEW-SPEC)
    (if copy-first
	;; This should not fail - if it does, we must abort.
	(if (consp flambda)
	    (setcdr fint (list new-spec))
	  ;; can't "aset" a #<byte-code> object, though aref works...
	  (setq flambda (append flambda nil))
	  (setcar (nthcdr 5 flambda) new-spec)
	  (setq flambda (apply 'make-byte-code flambda))
	  (fset fun flambda))
      ;; else prepare for a second try
      (condition-case err
	  (setcdr fint (list new-spec))
	;; Setcdr bombs on preloaded functions:
	;;     (error "Attempt to modify read-only object")
	;; There seems to be no simple way to test whether an object
	;; resides in pure storage, so we let it bomb and try again.
	(error (gmhist-replace-spec fun new-spec t))))))

(defun gmhist-spec (fun)
  "Get the current interactive specification for FUN (a symbol).
Signal an error if FUN is not interactive."
  (let ((flambda (gmhist-symbol-function fun))
	fint)
    (cond ((consp flambda)  ; interpreted, or v18 compiled
	   ;; do it exactly like call-interactively, even if this
	   ;; means (interactive...) can come arbitrary late in FUN's body
	   (setq fint (assq 'interactive (cdr (cdr flambda))))
	   (or fint
	       (error "Cannot get spec of a non-interactive command: %s!" fun))
	   (nth 1 fint))
	  (t  ; otherwise it's a v19 compiled-code object
	   (aref flambda 5)))))

(defun gmhist-symbol-function (fun)
  ;; Return FUN's ultimate definition.
  ;; Recurse if FUN is fset to another function's name.
  (let ((flambda (symbol-function fun)))
    (if (symbolp flambda)
	;; Prefer recursion over while because infinite loop is caught
	;; by max-lisp-eval-depth.
	(gmhist-symbol-function flambda)
      flambda)))

;; Automagic gmhistification

;; There should be a builtin split function - inverse to mapconcat.
(defun gmhist-split (pat str &optional limit)
  "Splitting on regexp PAT, turn string STR into a list of substrings.
Optional third arg LIMIT (>= 1) is a limit to the length of the
resulting list.
Thus, if SEP is a regexp that only matches itself,

   (mapconcat 'identity (gmhist-split SEP STRING) SEP)

is always equal to STRING."
  (let* ((start (string-match pat str))
	 (result (list (substring str 0 start)))
	 (count 1)
	 (end (if start (match-end 0))))
    (if end				; else nothing left
	(while (and (or (not (integerp limit))
			(< count limit))
		    (string-match pat str end))
	  (setq start (match-beginning 0)
		count (1+ count)
		result (cons (substring str end start) result)
		end (match-end 0)
		start end)
	  ))
    (if (and (or (not (integerp limit))
		 (< count limit))
	     end)			; else nothing left
	(setq result
	      (cons (substring str end) result)))
    (nreverse result)))

(defun gmhist-interactive (spec hist)
  "Interpret SPEC, an interactive string, like call-interactively
would, only with minibuffer history in HIST (a symbol).

If the value of HIST is another symbol (which can never happen if
history lists are already stored on it), this symbol is taken instead
to facilitate dynamic indirections.

Currently recognized key letters are:

    a b B c C d D k m N n s S x X f F r p P v

and initial `*'.

Use it inside interactive like this

    \(interactive \(gmhist-interactive \"sPrompt: \\nP\" 'foo-history\)\)

or even like this:

    \(interactive
     \(gmhist-interactive \"sReplace: \\nsReplace %s with: \" 'replace-history\)\)
"
  (or (stringp spec)
      (error "gmhist-interactive: not a string %s" spec))
  (if (and (> (length spec) 0) (eq ?\* (aref spec 0)))
      (progn
	(barf-if-buffer-read-only)
	(setq spec (substring spec 1))))
  (if (and (boundp hist)
	   (symbolp (symbol-value hist))
	   (not (null (symbol-value hist))))
      (setq hist (symbol-value hist)))
  (let ((spec-list (mapcar '(lambda (x)
			      ;; forgive empty entries like
			      ;; call-interactively does:
			      (if (equal "" x)
				  nil
				(cons (aref x 0) (substring x 1))))
			   (gmhist-split "\n" spec)))
	cur-arg args-so-far
	elt char prompt xprompt)
    (setq spec-list (delq nil spec-list))
    (while spec-list
      (setq elt (car spec-list)
	    spec-list (cdr spec-list)
	    special nil			; special handling of args-so-far
	    char (car elt)
	    prompt (cdr elt)
	    xprompt (apply (function format) prompt (reverse args-so-far)))
      (cond ((eq char ?a)		; Symbol defined as a function
	     (setq cur-arg (intern
			    (completing-read-with-history-in
			     hist xprompt obarray 'fboundp t nil))))
	    ((eq char ?b)		; Name of existing buffer
	     (setq cur-arg (read-buffer-with-history-in
			    hist xprompt (other-buffer) t)))
	    ((eq char ?B)		; Name of possibly non-existing buffer
	     (setq cur-arg (read-buffer-with-history-in
			    hist xprompt (other-buffer) nil)))
	    ((eq char ?c)		; Character
	     (message xprompt)		; history doesn't make sense for this
	     (setq cur-arg (read-char)))
	    ((eq char ?C)		; Command
	     (setq cur-arg (intern
			    (completing-read-with-history-in
			     hist xprompt obarray 'commandp t nil))))
	    ((eq char ?d)		; Value of point.  Does not do I/O.
	     (setq cur-arg (point)))
	    ((eq char ?D)		; directory name
	     ;; This does not check file-directory-p, but neither does
	     ;; call-interactively.
	     (setq cur-arg (read-file-name-with-history-in
			    hist
			    xprompt
			    nil
			    default-directory
			    'confirm)))
	    ((eq char ?f)		; existing file name
	     (setq cur-arg (read-file-name-with-history-in
			    hist
			    xprompt
			    nil nil 'confirm)))
	    ((eq char ?F)		; possibly nonexistent file name
	     (setq cur-arg (read-file-name-with-history-in
			    hist
			    xprompt)))
	    ((eq char ?k)		; Key sequence (string)
	     (setq cur-arg (read-key-sequence (if (equal xprompt "")
						 nil xprompt))))
	    ((eq char ?m)		; Value of mark.  Does not do I/O.
	     (setq cur-arg (or (mark) (error "The mark is not set now"))))
	    ((eq char ?N)		; Prefix arg, else number from minibuf
	     (if current-prefix-arg
		 (setq cur-arg (prefix-numeric-value current-prefix-arg))
	       (while (not (integerp
			  (setq cur-arg
				(read-with-history-in hist xprompt nil t)))))))
	    ((eq char ?n)		; Read number from minibuffer
	     (while (not (integerp
			  (setq cur-arg
				(read-with-history-in hist xprompt nil t))))))
	    ((eq char ?p)		; cooked prefix arg
	     (setq cur-arg (prefix-numeric-value current-prefix-arg)))
	    ((eq char ?P)		; raw prefix arg
	     (setq cur-arg current-prefix-arg))
	    ((eq char ?r)		; region
	     (let (region-min region-max)
	       ;; take some pains to behave exactly like interactive "r"
	       (setq region-min (min (or (mark)
					 (error "The mark is not set now"))
				     (point))
		     region-max (max (or (mark)
					 (error "The mark is not set now"))
				     (point)))
	       (setq args-so-far
		     (append (list region-max region-min) args-so-far)
		     special t)))
	    ((eq char '?s)		; string
	     (setq cur-arg (read-with-history-in hist xprompt)))
	    ((eq char ?S)		; any symbol
	     (setq cur-arg (read-with-history-in hist xprompt nil t)))
	    ((eq char ?v)		; Variable name
	     (setq cur-arg (completing-read-with-history-in
			    hist xprompt obarray 'user-variable-p t nil)))
	    ((memq char '(?x ?X))	;  lisp expression
	     (setq cur-arg (read-with-history-in
			    hist
			    xprompt
			    nil
			    ;; have to tell gmhist to read s-exps
			    ;; instead of strings:
			    t))
	     (if (eq char ?X)		; lisp expression, evaluated
		 (setq cur-arg (eval cur-arg))))

	    (t
	     (error "Invalid control letter `%c' in gmhist-interactive" char)))
      (or special
	  (setq args-so-far (cons cur-arg args-so-far))))
    (reverse args-so-far)))

(defun gmhist-new-spec (fun &optional hist no-error)
  "Return a new interactive specification for FUN, suitable for use
with setcdr in function gmhist-replace-spec.
Use symbol HIST to store the history.  HIST defaults to `FUN-history'.
The returned spec does the same as the old one, only with history in HIST.
 
If FUN is an autoload object, its file is loaded first.

See function gmhist-interactive for a list of recognized interactive
keys letters.

Unless optional third arg NO-ERROR is given, signals an error if FUN's
interactive string contains unknown key letters or has no interactive string.
With NO-ERROR, it returns nil."
  (or hist (setq hist (intern (concat (symbol-name fun) "-history"))))
  (gmhist-check-autoload fun)
  (let ((spec (gmhist-spec fun)))
    (if (stringp spec)
	(list 'gmhist-interactive spec (list 'quote hist))
      (if no-error
	  nil
	(error "Can't gmhistify %s's spec: %s" fun spec)))))

(defun gmhist-make-magic (fun &optional hist)
  "Make FUN magically maintain minibuffer history in symbol HIST.
HIST defaults to `FUN-history'.
This works by modifying the interactive specification.  See also
  function gmhist-replace-spec.
The magic goes away when you call gmhist-remove-magic on FUN."
  (interactive "CPut gmhist magic on command: ")
  (let ((new-spec (gmhist-new-spec fun hist t)))
    (if new-spec
	(gmhist-replace-spec fun new-spec)
      ;; else there was some error.  Try to find out if this is a retry.
      (message "Another attempt to put magic on %s..." fun)
      (gmhist-remove-magic fun)		; will abort if not a retry
      ;; This time we don't catch errors - magic or blow!
      (gmhist-replace-spec fun (gmhist-new-spec fun hist))
      (message "Another attempt to put magic on %s...done." fun))))

(defun gmhist-remove-magic (fun)
  "Remove the magic that gmhist-make-magic put on FUN,
restoring the old interactive spec." 
  (interactive "CRemove gmhist magic from command: ")
  (gmhist-replace-spec
   fun
   (or (get fun 'gmhist-old-interactive-spec)
       (error "Can't find %s's old interactive spec!" fun))))

;; Now make yourself magic
(gmhist-make-magic 'gmhist-make-magic 'gmhist-make-magic-history)
(gmhist-make-magic 'gmhist-remove-magic 'gmhist-make-magic-history)


;; Examples, pedagogic and serious ones.  More in gmhist-app.el.

;;(defun foo-command (cmd)
;;  (interactive (list
;;	       (read-with-history-in 'foo-history "Foo: ")))
;;  (message "Foo %s" cmd))
;;
;; ;; The interactive clause could also have been the simpler
;; ;; (interactive (gmhist-interactive "sFoo: " 'foo-history))
;;
;;
;;;(put 'foo-history 'hist-map minibuffer-local-map) ; disable motion ...
;;;(put 'foo-history 'hist-function 'gmhist-read-nohistory) ; and history
;;
;;(put 'foo-history 'hist-function nil) ; enable history ...
;;(put 'foo-history 'hist-map nil) ; and motion again
;;
;;(defun gmhist-read-nohistory (symbol prompt initial-input read)
;;  "An example function to put on the hist-function property."
;;  (message "read-nohistory...")
;;  (sit-for 2)
;;  (read-string prompt initial-input))
;;
;; Example for reading file names:
;;(defun bar-command (cmd)
;;  (interactive
;;   (list
;;    (read-file-name-with-history-in
;;     ;; HIST-SYM  PROMPT  DIR DFLT MUSTMATCH
;;     'bar-history "Bar: " nil nil 'confirm)))
;;  (message "Bar %s" cmd))
;;
;; Example function to apply gmhist-make-magic to.
;; Compare the missing initial input in bar to the magic version of zod.
;;(defun zod-command (cmd)
;;  (interactive "fZod: ")
;;  (message "Zod %s" cmd))

;; Finally run the load-hook

(run-hooks 'gmhist-load-hook)

;; End of file gmhist.el
