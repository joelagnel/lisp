;;; -*-Emacs-Lisp-*- General command interpreter in a window stuff
;;; Copyright Olin Shivers (1988).
;;; Please imagine a long, tedious, legalistic 5-page gnu-style copyright
;;; notice appearing here to the effect that you may use this code any
;;; way you like, as long as you don't charge money for it, remove this
;;; notice, or hold me liable for its results.

;;; This hopefully generalises shell mode, lisp mode, tea mode, soar mode,...
;;; Hacked from tea.el and shell.el by Olin Shivers (shivers@cs.cmu.edu). 8/88

;;; This file defines a general command-interpreter-in-a-buffer package
;;; (comint mode). The idea is that you can build specific process-in-a-buffer
;;; modes on top of comint mode -- e.g., lisp, shell, scheme, T, soar, ....
;;; This way, all these specific packages share a common base functionality, 
;;; and a common set of bindings, which makes them easier to use (and
;;; saves code, implementation time, etc., etc.).
;;; 
;;; Several packages are already defined using comint mode:
;;; - The file cmushell.el defines cmushell and cmulisp mode.
;;; Cmushell and cmulisp mode are similar to, and intended to replace,
;;; their counterparts in the standard gnu emacs release (in shell.el). 
;;; These replacements are more featureful, robust, and uniform than the 
;;; released versions. The key bindings in lisp mode are also more compatible
;;; with the bindings of Hemlock and Zwei (the Lisp Machine emacs).
;;;
;;; - The file cmuscheme.el defines inferior-scheme mode.
;;; - The file tea.el tunes scheme and inferior-scheme modes for T.
;;; - The file soar.el tunes lisp and inferior-lisp modes for Soar.

;;; For documentation on the functionality provided by comint mode, and
;;; the hooks available for customising it, see the comments below.
;;; For further information on the standard derived modes (shell, 
;;; inferior-lisp, inferior-scheme, ...), see the relevant source files.

;;; Please send me bug reports, bug fixes, and extensions, so that I can
;;; merge them into the master source.

;;; For hints on converting existing process modes (e.g., tex-mode,
;;; background, dbx, gdb, kermit, prolog, telnet) to use comint-mode
;;; instead of shell-mode, see the notes at the end of this file.

(provide 'comint)


;;; Brief Command Documentation:
;;;============================================================================
;;; Comint Mode Commands: (common to all derived modes, like cmushell & cmulisp
;;; mode)
;;;
;;; m-p	    comint-previous-input    	    Cycle backwards in input history
;;; m-n	    comint-next-input  	    	    Cycle forwards
;;; c-c r   comint-previous-input-matching  Search backwards in input history
;;; return  comint-send-input
;;; c-a     comint-bol                      Beginning of line; skip prompt.
;;; c-d	    comint-delchar-or-maybe-eof     Delete char unless at end of buff.
;;; c-c c-u comint-kill-input	    	    ^u
;;; c-c c-w backward-kill-word    	    ^w
;;; c-c c-c comint-interrupt-subjob 	    ^c
;;; c-c c-z comint-stop-subjob	    	    ^z
;;; c-c c-\ comint-quit-subjob	    	    ^\
;;; c-c c-o comint-kill-output		    Delete last batch of process output
;;; c-c c-r comint-show-output		    Show last batch of process output
;;;
;;; Not bound by default in comint-mode
;;; send-invisible			Read a line w/o echo, and send to proc
;;; (These are bound in shell-mode)
;;; comint-dynamic-complete		Complete filename at point.
;;; comint-dynamic-list-completions	List completions in help buffer.
;;; comint-replace-by-expanded-filename	Expand and complete filename at point;
;;;					replace with expanded/completed name.
;;; comint-kill-subjob			No mercy.
;;; comint-continue-subjob		Send CONT signal to buffer's process
;;;					group. Useful if you accidentally
;;;					suspend your process (with C-c C-z).
;;;
;;; Bound for RMS -- I prefer the input history stuff, but you might like 'em.
;;; m-P	   comint-msearch-input		Search backwards for prompt
;;; m-N    comint-psearch-input		Search forwards for prompt
;;; C-cR   comint-msearch-input-matching Search backwards for prompt & string

;;; comint-mode-hook is the comint mode hook. Basically for your keybindings.
;;; comint-load-hook is run after loading in this package.


;;; Buffer Local Variables:
;;;============================================================================
;;; Comint mode buffer local variables:
;;;     comint-prompt-regexp    - string       comint-bol uses to match prompt.
;;;     comint-last-input-end   - marker       For comint-kill-output command
;;;     input-ring-size         - integer      For the input history
;;;     input-ring              - ring             mechanism
;;;     input-ring-index        - marker           ...
;;;     comint-last-input-match - string           ...
;;;     comint-get-old-input    - function     Hooks for specific 
;;;     comint-input-sentinel   - function         process-in-a-buffer
;;;     comint-input-filter     - function         modes.
(defvar comint-prompt-regexp "^"
  "Regexp to recognise prompts in the inferior process.
Defaults to \"^\", the null string at BOL.

Good choices:
  Canonical Lisp: \"^[^> ]*>+:? *\" (Lucid, franz, kcl, T, cscheme, oaklisp)
  Lucid Common Lisp: \"^\\(>\\|\\(->\\)+\\) *\"
  franz: \"^\\(->\\|<[0-9]*>:\\) *\"
  kcl: \"^>+ *\"
  shell: \"^[^#$%>]*[#$%>] *\"
  T: \"^>+ *\"

This is a good thing to set in mode hooks.")

(defvar input-ring-size 30
  "Size of input history ring.")

;;; Here are the per-interpreter hooks.
(defvar comint-get-old-input (function comint-get-old-input-default)
  "Function that submits old text in comint mode.
This function is called when return is typed while the point is in old text.
It returns the text to be submitted as process input.  The default is
comint-get-old-input-default, which grabs the current line, and strips off
leading text matching comint-prompt-regexp")

(defvar comint-input-sentinel (function ignore)
  "Called on each input submitted to comint mode process by comint-send-input.
Thus it can, for instance, track cd/pushd/popd commands issued to the csh.")

(defvar comint-input-filter
  (function (lambda (str) (not (string-match "\\`\\s *\\'" str))))
  "Predicate for filtering additions to input history.
Only inputs answering true to this function are saved on the input
history list. Default is to save anything that isn't all whitespace")

(defvar comint-mode-hook '()
  "Called upon entry into comint-mode")

(defvar comint-mode-map nil)

;; Added for ILISP
(defvar comint-input-chunk-size 512)



(defun comint-mode ()
  "Major mode for interacting with an inferior interpreter.
Interpreter name is same as buffer name, sans the asterisks.
Return at end of buffer sends line as input.
Return not at end copies rest of line to end and sends it.

This mode is typically customised to create inferior-lisp-mode,
shell-mode, etc.. This can be done by setting the hooks
comint-input-sentinel, comint-input-filter, and comint-get-old-input to
appropriate functions, and the variable comint-prompt-regexp to
the appropriate regular expression.

An input history is maintained of size input-ring-size, and
can be accessed with the commands comint-next-input [\\[comint-next-input]] and 
comint-previous-input [\\[comint-previous-input]]. Commands not keybound by
default are send-invisible, comint-dynamic-complete, and 
comint-list-dynamic-completions.
\\{comint-mode-map}
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

Entry to this mode runs the hooks on comint-mode-hook"
  (interactive)
  (let ((old-ring (and (assq 'input-ring (buffer-local-variables))
		       (boundp 'input-ring)
		       input-ring)))
    (kill-all-local-variables)
    (setq major-mode 'comint-mode)
    (setq mode-name "Comint")
    (setq mode-line-process '(": %s"))
    (use-local-map comint-mode-map)
    (make-local-variable 'comint-last-input-end)
    (setq comint-last-input-end (make-marker))
    (make-local-variable 'comint-last-input-match)
    (setq comint-last-input-match "")
    (make-variable-buffer-local 'comint-prompt-regexp) ; Don't set; default
    (make-variable-buffer-local 'input-ring-size)      ; ...to global val.
    (make-local-variable 'input-ring)
    (make-local-variable 'input-ring-index)
    (setq input-ring-index 0)
    (make-variable-buffer-local 'comint-get-old-input)
    (make-variable-buffer-local 'comint-input-sentinel)
    (make-variable-buffer-local 'comint-input-filter)  
    (run-hooks 'comint-mode-hook)
    ;Do this after the hook so the user can mung INPUT-RING-SIZE w/his hook.
    ;The test is so we don't lose history if we run comint-mode twice in
    ;a buffer.
    (setq input-ring (if (ring-p old-ring) old-ring
			 (make-ring input-ring-size)))))

(if comint-mode-map
    nil
  (setq comint-mode-map (make-sparse-keymap))
  (define-key comint-mode-map "\ep" 'comint-previous-input)
  (define-key comint-mode-map "\en" 'comint-next-input)
  (define-key comint-mode-map "\C-m" 'comint-send-input)
  (define-key comint-mode-map "\C-d" 'comint-delchar-or-maybe-eof)
  (define-key comint-mode-map "\C-a" 'comint-bol)
  (define-key comint-mode-map "\C-c\C-u" 'comint-kill-input)
  (define-key comint-mode-map "\C-c\C-w" 'backward-kill-word)
  (define-key comint-mode-map "\C-c\C-c" 'comint-interrupt-subjob)
  (define-key comint-mode-map "\C-c\C-z" 'comint-stop-subjob)
  (define-key comint-mode-map "\C-c\C-\\" 'comint-quit-subjob)
  (define-key comint-mode-map "\C-c\C-o" 'comint-kill-output)
  (define-key comint-mode-map "\C-cr"    'comint-previous-input-matching)
  (define-key comint-mode-map "\C-c\C-r" 'comint-show-output)
  ;;; Here's the prompt-search stuff I installed for RMS to try...
  (define-key comint-mode-map "\eP" 'comint-msearch-input)
  (define-key comint-mode-map "\eN" 'comint-psearch-input)
  (define-key comint-mode-map "\C-cR" 'comint-msearch-input-matching))


;;; This function is used to make a full copy of the comint mode map,
;;; so that client modes won't interfere with each other. This function
;;; isn't necessary in emacs 18.5x, but we keep it around for 18.4x versions.
(defun full-copy-sparse-keymap (km)
  "Recursively copy the sparse keymap KM"
  (cond ((consp km)
	 (cons (full-copy-sparse-keymap (car km))
	       (full-copy-sparse-keymap (cdr km))))
	(t km)))

(defun comint-check-proc (buffer-name)
  "True if there is a process associated w/buffer BUFFER-NAME, and
it is alive (status RUN or STOP)."
  (let ((proc (get-buffer-process buffer-name)))
    (and proc (memq (process-status proc) '(run stop)))))

;;; Note that this guy, unlike shell.el's make-shell, barfs if you pass it ()
;;; for the second argument (program).
(defun make-comint (name program &optional startfile &rest switches)
  (let* ((buffer (get-buffer-create (concat "*" name "*")))
	 (proc (get-buffer-process buffer)))
    ;; If no process, or nuked process, crank up a new one and put buffer in
    ;; comint mode. Otherwise, leave buffer and existing process alone.
    (cond ((or (not proc) (not (memq (process-status proc) '(run stop))))
	   (comint-exec buffer name program startfile switches)
	   (save-excursion
	     (set-buffer buffer)
	     (comint-mode)))) ; Install local vars, mode, keymap, ...
    buffer))

(defun comint-exec (buffer name command startfile switches)
  "Fires up a process in buffer for comint modes.
Blasts any old process running in the buffer. Doesn't set the buffer mode.
You can use this to cheaply run a series of processes in the same comint
buffer."
  (save-excursion
    (set-buffer buffer)
    (let ((proc (get-buffer-process buffer)))	; Blast any old process.
      (if proc (delete-process proc)))
    ;; Crank up a new process
    (let ((proc (apply 'start-process name buffer (concat exec-directory "env")
		       (format "TERMCAP=emacs:co#%d:tc=unknown:"
			       (screen-width))
		       "TERM=emacs" "EMACS=t" "-" command switches)))
      ;; Feed it the startfile.
      (cond (startfile
	     ;;This is guaranteed to wait long enough
	     ;;but has bad results if the comint does not prompt at all
	     ;;	     (while (= size (buffer-size))
	     ;;	       (sleep-for 1))
	     ;;I hope 1 second is enough!
	     (sleep-for 1)
	     (goto-char (point-max))
	     (insert-file-contents startfile)
	     (setq startfile (buffer-substring (point) (point-max)))
	     (delete-region (point) (point-max))
	     (process-send-string proc startfile)))
      ;; Jump to the end, and set the process mark.
      (goto-char (point-max))
      (set-marker (process-mark proc) (point)))
    buffer))
      


;;; Ring Code
;;;============================================================================
;;; This code defines a ring data structure. A ring is a 
;;;     (hd-index tl-index . vector) 
;;; list. You can insert to, remove from, and rotate a ring. When the ring
;;; fills up, insertions cause the oldest elts to be quietly dropped.
;;;
;;; HEAD = index of the newest item on the ring.
;;; TAIL = index of the oldest item on the ring.
;;;
;;; These functions are used by the input history mechanism, but they can
;;; be used for other purposes as well.

(defun ring-p (x) 
  "T if X is a ring; NIL otherwise."
  (and (consp x) (integerp (car x))
       (consp (cdr x)) (integerp (car (cdr x)))
       (vectorp (cdr (cdr x)))))

(defun make-ring (size)
  "Make a ring that can contain SIZE elts"
  (cons 1 (cons 0 (make-vector (+ size 1) nil))))

(defun ring-plus1 (index veclen)
  "INDEX+1, with wraparound"
  (let ((new-index (+ index 1)))
    (if (= new-index veclen) 0 new-index)))

(defun ring-minus1 (index veclen)
  "INDEX-1, with wraparound"
  (- (if (= 0 index) veclen index) 1))

(defun ring-length (ring)
  (let ((hd (car ring)) (tl (car (cdr ring)))  (siz (length (cdr (cdr ring)))))
    (let ((len (if (<= hd tl) (+ 1 (- tl hd)) (+ 1 tl (- siz hd)))))
      (if (= len siz) 0 len))))

(defun ring-empty-p (ring)
  (= 0 (ring-length ring)))

(defun ring-insert (ring item)
  "Insert a new item onto the ring. If the ring is full, dump the oldest
item to make room."       
  (let* ((vec (cdr (cdr ring)))  (len (length vec))
	 (new-hd (ring-minus1 (car ring) len)))
      (setcar ring new-hd)
      (aset vec new-hd item)
      (if (ring-empty-p ring) ;overflow -- dump one off the tail.
	  (setcar (cdr ring) (ring-minus1 (car (cdr ring)) len)))))

(defun ring-remove (ring)
  "Remove the oldest item retained on the ring."
  (if (ring-empty-p ring) (error "Ring empty")
      (let ((tl (car (cdr ring)))  (vec (cdr (cdr ring))))
	(set-car (cdr ring) (ring-minus1 tl (length vec)))
	(aref vec tl))))

;;; This isn't actually used in this package. I just threw it in in case
;;; someone else wanted it. If you want rotating-ring behavior on your history
;;; retrieval (analagous to kill ring behavior), this function is what you
;;; need. I should write the yank-input and yank-pop-input-or-kill to go with
;;; this, and not bind it to a key by default, so it would be available to
;;; people who want to bind it to a key. But who would want it? Blech.
(defun ring-rotate (ring n)
  (if (not (= n 0))
      (if (ring-empty-p ring) ;Is this the right error check?
	  (error "ring empty")
	  (let ((hd (car ring))  (tl (car (cdr ring)))  (vec (cdr (cdr ring))))
	    (let ((len (length vec)))
	      (while (> n 0)
		(setq tl (ring-plus1 tl len))
		(aset ring tl (aref ring hd))
		(setq hd (ring-plus1 hd len))
		(setq n (- n 1)))
	      (while (< n 0)
		(setq hd (ring-minus1 hd len))
		(aset vec hd (aref vec tl))
		(setq tl (ring-minus1 tl len))
		(setq n (- n 1))))
	    (set-car ring hd)
	    (set-car (cdr ring) tl)))))

(defun comint-mod (n m)
  "Returns N mod M. M is positive. Answer is guaranteed to be non-negative, 
and less than m."
  (let ((n (% n m)))
    (if (>= n 0) n
	(+ n
	   (if (>= m 0) m (- m)))))) ; (abs m)

(defun ring-ref (ring index)
  (let ((numelts (ring-length ring)))
    (if (= numelts 0) (error "indexed empty ring")
	(let* ((hd (car ring))  (tl (car (cdr ring)))  (vec (cdr (cdr ring)))
	       (index (comint-mod index numelts))
	       (vec-index (comint-mod (+ index hd) 
				      (length vec))))
	  (aref vec vec-index)))))


;;; Input history retrieval commands
;;; M-p -- previous input    M-n -- next input
;;; C-c r -- previous input matching
;;; ===========================================================================

(defun comint-previous-input (arg)
  "Cycle backwards through input history."
  (interactive "*p")
  (let ((len (ring-length input-ring)))
    (cond ((<= len 0)
	   (message "Empty input ring")
	   (ding))
	  ((not (comint-after-pmark-p))
	   (message "Not after process mark")
	   (ding))
	  (t
	   (cond ((eq last-command 'comint-previous-input)
		  (delete-region (mark) (point))
		  (set-mark (point)))
		 (t                          
		  (setq input-ring-index
			(if (> arg 0) -1
			    (if (< arg 0) 1 0)))
		  (push-mark (point))))
	   (setq input-ring-index (comint-mod (+ input-ring-index arg) len))
	   (message "%d" (1+ input-ring-index))
	   (insert (ring-ref input-ring input-ring-index))
	   (setq this-command 'comint-previous-input))
	  (t (ding)))))
	 
(defun comint-next-input (arg)
  "Cycle forwards through input history."
  (interactive "*p")
  (comint-previous-input (- arg)))

(defvar comint-last-input-match ""
  "Last string searched for by comint input history search, for defaulting.
Buffer local variable.") 

(defun comint-previous-input-matching (str)
  "Searches backwards through input history for substring match"
  (interactive (let ((s (read-from-minibuffer 
			 (format "Command substring (default %s): "
				 comint-last-input-match))))
		 (list (if (string= s "") comint-last-input-match s))))
; (interactive "sCommand substring: ")
  (setq comint-last-input-match str) ; update default
  (let ((str (regexp-quote str))
        (len (ring-length input-ring))
	(n 0))
    (while (and (<= n len) (not (string-match str (ring-ref input-ring n))))
      (setq n (+ n 1)))
    (cond ((<= n len) (comint-previous-input (+ n 1)))
	  (t (error "Not found.")))))

;;; These next three commands are alternatives to the input history commands --
;;; comint-next-input, comint-previous-input and 
;;; comint-previous-input-matching. They search through the process buffer
;;; text looking for occurrences of the prompt. RMS likes them better;
;;; I don't. Bound to M-P, M-N, and C-c R (uppercase P, N, and R) for
;;; now. Try'em out. Go with what you like...

;;; comint-msearch-input-matching prompts for a string, not a regexp.
;;; This could be considered to be the wrong thing. I decided to keep it
;;; simple, and not make the user worry about regexps. This, of course,
;;; limits functionality.

(defun comint-psearch-input ()
  "Search forwards for next occurrence of prompt and skip to end of line.
\(prompt is anything matching regexp comint-prompt-regexp)"
  (interactive)
  (if (re-search-forward comint-prompt-regexp (point-max) t)
      (end-of-line)
      (error "No occurrence of prompt found")))

(defun comint-msearch-input ()
  "Search backwards for previous occurrence of prompt and skip to end of line.
Search starts from beginning of current line."
  (interactive)
  (let ((p (save-excursion
	     (beginning-of-line)
	     (cond ((re-search-backward comint-prompt-regexp (point-min) t)
		    (end-of-line)
		    (point))
		   (t nil)))))
    (if p (goto-char p)
	(error "No occurrence of prompt found"))))

(defun comint-msearch-input-matching (str)
  "Search backwards for occurrence of prompt followed by STRING.
STRING is prompted for, and is NOT a regular expression."
  (interactive (let ((s (read-from-minibuffer 
			 (format "Command (default %s): "
				 comint-last-input-match))))
		 (list (if (string= s "") comint-last-input-match s))))
; (interactive "sCommand: ")
  (setq comint-last-input-match str) ; update default
  (let* ((r (concat comint-prompt-regexp (regexp-quote str)))
	 (p (save-excursion
	      (beginning-of-line)
	      (cond ((re-search-backward r (point-min) t)
		     (end-of-line)
		     (point))
		    (t nil)))))
    (if p (goto-char p)
	(error "No match"))))

(defun comint-send-input () 
  "Send input to process.  After the process output mark, sends all text
from the process mark to point as input to the process.  Before the
process output mark, calls value of variable comint-get-old-input to retrieve
old input, copies it to the end of the buffer, and sends it.  A terminal
newline is also inserted into the buffer and sent to the process.  In either
case, value of variable comint-input-sentinel is called on the input before
sending it.  The input is entered into the input history ring, if value of
variable comint-input-filter returns T when called on the input.

comint-get-old-input, comint-input-sentinel, and comint-input-filter are chosen
according to the command interpreter running in the buffer. E.g.,
If the interpreter is the csh,
    comint-get-old-input is the default: take the current line, discard any
        initial string matching regexp comint-prompt-regexp.
    comint-input-sentinel monitors input for \"cd\", \"pushd\", and \"popd\" 
        commands. When it sees one, it cd's the buffer.
    comint-input-filter is the default: returns T if the input isn't all white
	space.

If the comint is Lucid Common Lisp, 
    comint-get-old-input snarfs the sexp ending at point.
    comint-input-sentinel does nothing.
    comint-input-filter returns NIL if the input matches input-filter-regexp,
        which matches (1) all whitespace (2) :a, :c, etc.

Similarly for Soar, Scheme, etc.."
  (interactive)
  ;; Note that the input string does not include its terminal newline.
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
	(let* ((pmark (process-mark proc))
	       (pmark-val (marker-position pmark))
	       (input (if (>= (point) pmark-val)
			  (buffer-substring pmark (point))
			  (let ((copy (funcall comint-get-old-input)))
			    (goto-char pmark)
			    (insert copy)
			    copy))))
	  (insert ?\n)
	  (if (funcall comint-input-filter input) (ring-insert input-ring input))
	  (funcall comint-input-sentinel input)
	  (process-send-string proc input)
	  (process-send-string proc "\n")
	  (set-marker (process-mark proc) (point))
	  (set-marker comint-last-input-end (point))))))

(defun comint-get-old-input-default ()
  "Default for comint-get-old-input: take the current line, and discard
any initial text matching comint-prompt-regexp."
  (save-excursion
    (beginning-of-line)
    (comint-skip-prompt)
    (let ((beg (point)))
      (end-of-line)
      (buffer-substring beg (point)))))

(defun comint-skip-prompt ()
  "Skip past the text matching regexp comint-prompt-regexp. 
If this takes us past the end of the current line, don't skip at all."
  (let ((eol (save-excursion (end-of-line) (point))))
    (if (and (looking-at comint-prompt-regexp)
	     (<= (match-end 0) eol))
	(goto-char (match-end 0)))))


(defun comint-after-pmark-p ()
  "Is point after the process output marker?"
  ;; Since output could come into the buffer after we looked at the point
  ;; but before we looked at the process marker's value, we explicitly 
  ;; serialise. This is just because I don't know whether or not emacs
  ;; services input during execution of lisp commands.
  (let ((proc-pos (marker-position
		   (process-mark (get-buffer-process (current-buffer))))))
    (<= proc-pos (point))))

(defun comint-bol (arg)
  "Goes to the beginning of line, then skips past the prompt, if any.
If a prefix argument is given (\\[universal-argument]), then no prompt skip 
-- go straight to column 0.

The prompt skip is done by skipping text matching the regular expression
comint-prompt-regexp, a buffer local variable.

If you don't like this command, reset c-a to beginning-of-line 
in your hook, comint-mode-hook."
  (interactive "P")
  (beginning-of-line)
  (if (null arg) (comint-skip-prompt)))

;;; These two functions are for entering text you don't want echoed or
;;; saved -- typically passwords to ftp, telnet, or somesuch.
;;; Just enter m-x send-invisible and type in your line.

(defun comint-read-noecho (prompt)
  "Prompt the user with argument PROMPT. Read a single line of text
without echoing, and return it. Note that the keystrokes comprising
the text can still be recovered (temporarily) with \\[view-lossage]. This
may be a security bug for some applications."
  (let ((echo-keystrokes 0)
	(answ "")
	tem)
    (if (and (stringp prompt) (not (string= (message prompt) "")))
	(message prompt))
    (while (not(or  (= (setq tem (read-char)) ?\^m)
		    (= tem ?\n)))
      (setq answ (concat answ (char-to-string tem))))
    (message "")
    answ))

(defun send-invisible (str)
  "Read a string without echoing, and send it to the process running
in the current buffer. A new-line is additionally sent. String is not 
saved on comint input history list.
Security bug: your string can still be temporarily recovered with
\\[view-lossage]."
; (interactive (list (comint-read-noecho "Enter non-echoed text")))
  (interactive "P") ; Defeat snooping via C-x esc
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
	(process-send-string proc
			     (if (stringp str) str
				 (comint-read-noecho "Enter non-echoed text")))
	(process-send-string proc "\n"))))

;;; Random input hackage

(defun comint-kill-output ()
  "Kill all output from interpreter since last input."
  (interactive)
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (kill-region comint-last-input-end pmark)
    (goto-char pmark)    
    (insert "*** output flushed ***\n")
    (set-marker pmark (point))))

(defun comint-show-output ()
  "Display start of this batch of interpreter output at top of window.
Also put cursor there."
  (interactive)
  (goto-char comint-last-input-end)
  (backward-char)
  (beginning-of-line)
  (set-window-start (selected-window) (point))
  (end-of-line))

(defun comint-interrupt-subjob ()
  "Interrupt the current subjob."
  (interactive)
  (interrupt-process nil t))

(defun comint-kill-subjob ()
  "Send kill signal to the current subjob."
  (interactive)
  (kill-process nil t))

(defun comint-quit-subjob ()
  "Send quit signal to the current subjob."
  (interactive)
  (quit-process nil t))

(defun comint-stop-subjob ()
  "Stop the current subjob.
WARNING: if there is no current subjob, you can end up suspending
the top-level process running in the buffer. If you accidentally do
this, use \\[comint-continue-subjob] to resume the process. (This
is not a problem with most shells, since they ignore this signal.)"
  (interactive)
  (stop-process nil t))

(defun comint-continue-subjob ()
  "Send CONT signal to process buffer's process group.
Useful if you accidentally suspend the top-level process."
  (interactive)
  (continue-process nil t))

(defun comint-kill-input ()
  "Kill all text from last stuff output by interpreter to point."
  (interactive)
  (let* ((pmark (process-mark (get-buffer-process (current-buffer))))
	 (p-pos (marker-position pmark)))
    (if (> (point) p-pos)
	(kill-region pmark (point)))))

(defun comint-delchar-or-maybe-eof (arg)
  "Delete ARG characters forward, or send an EOF to lisp if at end of buffer."
  (interactive "p")
  (if (eobp)
      (process-send-eof)
      (delete-char arg)))




;;; Support for source-file processing commands.
;;;============================================================================
;;; Many command-interpreters (e.g., Lisp, Scheme, Soar) have
;;; commands that process files of source text (e.g. loading or compiling
;;; files). So the corresponding process-in-a-buffer modes have commands
;;; for doing this (e.g., lisp-load-file). The functions below are useful
;;; for defining these commands.
;;;
;;; Alas, these guys don't do exactly the right thing for Lisp, Scheme
;;; and Soar, in that they don't know anything about file extensions.
;;; So the compile/load interface gets the wrong default occasionally.
;;; The load-file/compile-file default mechanism could be smarter -- it
;;; doesn't know about the relationship between filename extensions and
;;; whether the file is source or executable. If you compile foo.lisp
;;; with compile-file, then the next load-file should use foo.bin for
;;; the default, not foo.lisp. This is tricky to do right, particularly
;;; because the extension for executable files varies so much (.o, .bin,
;;; .lbin, .mo, .vo, .ao, ...).


;;; COMINT-SOURCE-DEFAULT -- determines defaults for source-file processing
;;; commands.
;;;
;;; COMINT-CHECK-SOURCE -- if FNAME is in a modified buffer, asks you if you
;;; want to save the buffer before issuing any process requests to the command
;;; interpreter.
;;;
;;; COMINT-GET-SOURCE -- used by the source-file processing commands to prompt
;;; for the file to process.

;;; (COMINT-SOURCE-DEFAULT previous-dir/file source-modes)
;;;============================================================================
;;; This function computes the defaults for the load-file and compile-file
;;; commands for tea, soar, cmulisp, and cmuscheme modes. 
;;; 
;;; - PREVIOUS-DIR/FILE is a pair (directory . filename) from the last 
;;; source-file processing command. NIL if there hasn't been one yet.
;;; - SOURCE-MODES is a list used to determine what buffers contain source
;;; files: if the major mode of the buffer is in SOURCE-MODES, it's source.
;;; Typically, (lisp-mode) or (scheme-mode).
;;; 
;;; If the command is given in a file buffer whose major modes is in
;;; SOURCE-MODES, then the the filename is the default file, and the
;;; file's directory is the default directory.
;;; 
;;; If the buffer isn't a source file buffer (e.g., it's the process buffer),
;;; then the default directory & file are what was used in the last source-file
;;; processing command (i.e., PREVIOUS-DIR/FILE).  If this is the first time
;;; the command has been run (PREVIOUS-DIR/FILE is nil), the default directory
;;; is the cwd, with no default file. (\"no default file\" = nil)
;;; 
;;; SOURCE-REGEXP is typically going to be something like (tea-mode)
;;; for T programs, (lisp-mode) for Lisp programs, (soar-mode lisp-mode)
;;; for Soar programs, etc.
;;; 
;;; The function returns a pair: (default-directory . default-file).

(defun comint-source-default (previous-dir/file source-modes)
  (cond ((and buffer-file-name (memq major-mode source-modes))
	 (cons (file-name-directory    buffer-file-name)
	       (file-name-nondirectory buffer-file-name)))
	(previous-dir/file)
	(t
	 (cons default-directory nil))))


;;; (COMINT-CHECK-SOURCE fname)
;;;============================================================================
;;; Prior to loading or compiling (or otherwise processing) a file (in the CMU
;;; process-in-a-buffer modes), this function can be called on the filename.
;;; If the file is loaded into a buffer, and the buffer is modified, the user
;;; is queried to see if he wants to save the buffer before proceeding with
;;; the load or compile.

(defun comint-check-source (fname)
  (let ((buff (get-file-buffer fname)))
    (if (and buff
	     (buffer-modified-p buff)
	     (y-or-n-p (format "Save buffer %s first? "
			       (buffer-name buff))))
	;; save BUFF.
	(let ((old-buffer (current-buffer)))
	  (set-buffer buff)
	  (save-buffer)
	  (set-buffer old-buffer)))))


;;; (COMINT-GET-SOURCE prompt prev-dir/file source-modes mustmatch-p)
;;;============================================================================
;;; COMINT-GET-SOURCE is used to prompt for filenames in command-interpreter
;;; commands that process source files (like loading or compiling a file).
;;; It prompts for the filename, provides a default, if there is one,
;;; and returns the result filename.
;;; 
;;; See COMINT-SOURCE-DEFAULT for more on determining defaults.
;;; 
;;; PROMPT is the prompt string. PREV-DIR/FILE is the (directory . file) pair
;;; from the last source processing command.  SOURCE-MODES is a list of major
;;; modes used to determine what file buffers contain source files.  (These
;;; two arguments are used for determining defaults). If MUSTMATCH-P is true,
;;; then the filename reader will only accept a file that exists.
;;; 
;;; A typical use:
;;; (interactive (comint-get-source "Compile file: " prev-lisp-dir/file
;;;                                 "\\.lisp\\'" t))

(defun comint-get-source (prompt prev-dir/file source-regexp mustmatch-p)
  (let* ((def (comint-source-default prev-dir/file source-regexp))
	 (defdir  (car def))
	 (deffile (cdr def))
	 (ans (read-file-name (if deffile (format "%s(default %s) "
						  prompt    deffile)
				  prompt)
			      defdir
			      (concat defdir deffile)
			      mustmatch-p)))
    (list (expand-file-name (substitute-in-file-name ans)))))


;;; Filename completion in a buffer
;;; ===========================================================================
;;; Useful completion functions, courtesy of the Ergo group.
;;; M-<Tab> will complete the filename at the cursor as much as possible
;;; M-? will display a list of completions in the help buffer.

;;; Three commands:
;;; comint-dynamic-complete		Complete filename at point.
;;; comint-dynamic-list-completions	List completions in help buffer.
;;; comint-replace-by-expanded-filename	Expand and complete filename at point;
;;;					replace with expanded/completed name.

;;; These are not installed in the comint-mode keymap. But they are
;;; available for people who want them. Shell-mode installs them:
;;; (define-key cmushell-mode-map "\M-\t" 'comint-dynamic-complete)
;;; (define-key cmushell-mode-map "\M-?"  'comint-dynamic-list-completions)))
;;;
;;; Commands like this are fine things to put in load hooks if you
;;; want them present in specific modes. Example:
;;; (setq cmushell-load-hook
;;;       '((lambda () (define-key lisp-mode-map "\M-\t"
;;;				   'comint-replace-by-expanded-filename))))
;;;          


(defun comint-match-partial-pathname ()
  "Returns the string of an existing filename or causes and error."
  (if (save-excursion (backward-char 1) (looking-at "\\s ")) ""
      (save-excursion
	(re-search-backward "[^~/A-Za-z0-9---_.$#,]+")
	(re-search-forward "[~/A-Za-z0-9---_.$#,]+")
	(substitute-in-file-name 
	  (buffer-substring (match-beginning 0) (match-end 0))))))

(defun comint-replace-by-expanded-filename ()
  "Replace filename at point with expanded, completed name"
  (interactive)
  (let* ((pathname (comint-match-partial-pathname))
	 (pathdir (file-name-directory pathname))
	 (pathnondir (file-name-nondirectory pathname))
	 (completion (file-name-completion  pathnondir
					   (or pathdir default-directory))))
    (cond ((null completion)
	   (message "No completions of %s." pathname)
	   (ding))
	  ((eql completion t)
	   (message "Unique completion."))
	  (t				; this means a string was returned.
	   (delete-region (match-beginning 0) (match-end 0))
	   (insert pathdir completion)))))

(defun comint-dynamic-complete ()
  "Dynamically complete the filename at point."
  (interactive)
  (let* ((pathname (comint-match-partial-pathname))
	 (pathdir (file-name-directory pathname))
	 (pathnondir (file-name-nondirectory pathname))
	 (completion (file-name-completion  pathnondir
					   (or pathdir default-directory))))
    (cond ((null completion)
	   (message "No completions of %s." pathname)
	   (ding))
	  ((eql completion t)
	   (message "Unique completion."))
	  (t				; this means a string was returned.
	   (insert (substring completion (length pathnondir)))))))

(defun comint-dynamic-list-completions ()
  "List in help buffer all possible completions of the filename at point."
  (interactive)
  (let* ((pathname (comint-match-partial-pathname))
	 (pathdir (file-name-directory pathname))
	 (pathnondir (file-name-nondirectory pathname))
	 (completions
	  (file-name-all-completions pathnondir
				     (or pathdir default-directory))))
    (cond ((null completions)
	   (message "No completions of %s." pathname)
	   (ding))
	  (t (with-output-to-temp-buffer "*Help*"
	       (display-completion-list completions))))))

; Ergo bindings
; (global-set-key "\M-\t" 'comint-replace-by-expanded-filename)
; (global-set-key "\M-?" 'comint-dynamic-list-completions)
; (define-key shell-mode-map "\M-\t" 'comint-dynamic-complete)

;;; Log the user, so I know who's using the package during the beta test 
;;; period. This just inserts the user's name and current time into a 
;;; central file.
(defun comint-log-user ()
  (interactive)
  (if (file-writable-p "/afs/cs/user/shivers/lib/emacs/logdir/comint.log")
      (let ((u (getenv "USER"))
	    (old-buff (current-buffer)))
	(message "logging user in beta test database...")
	(find-file "/afs/cs/user/shivers/lib/emacs/logdir/comint.log")
	(cond ((search-forward u nil 'to-end)
	       (search-forward "| ")
	       (kill-line 1))
	      (t (insert (format "%s\t%s\t| " u (current-time-string)))))
	(insert (format "%s\n" (current-time-string)))
	(let ((make-backup-files nil)) (save-buffer))
	(kill-buffer (current-buffer))
	(set-buffer old-buff))))
(comint-log-user)


;;; Converting process modes to use comint mode
;;; ===========================================================================
;;; Several gnu packages (tex-mode, background, dbx, gdb, kermit, prolog, 
;;; telnet are some) use the shell package as clients. Most of them would
;;; be better off using the comint package, but they predate it. 
;;;
;;; Altering these packages to use comint mode should greatly
;;; improve their functionality, and is fairly easy.
;;; 
;;; Renaming variables
;;; Most of the work is renaming variables and functions. These are the common
;;; ones:
;;; Local variables:
;;; 	last-input-end		comint-last-input-end
;;;	last-input-start	<unnecessary>
;;;	shell-prompt-pattern	comint-prompt-regexp
;;;     shell-set-directory-error-hook <no equivalent>
;;; Miscellaneous:
;;;	shell-set-directory	<unnecessary>
;;; 	shell-mode-map		comint-mode-map
;;; Commands:
;;;	shell-send-input	comint-send-input
;;;	shell-send-eof		comint-delchar-or-maybe-eof
;;; 	kill-shell-input	comint-kill-input
;;;	interrupt-shell-subjob	comint-interrupt-subjob
;;;	stop-shell-subjob	comint-stop-subjob
;;;	quit-shell-subjob	comint-quit-subjob
;;;	kill-shell-subjob	comint-kill-subjob
;;;	kill-output-from-shell	comint-kill-output
;;;	show-output-from-shell	comint-show-output
;;;	copy-last-shell-input	Use comint-previous-input/comint-next-input
;;;
;;; LAST-INPUT-START is no longer necessary because inputs are stored on the
;;; input history ring. SHELL-SET-DIRECTORY is gone, its functionality taken
;;; over by SHELL-DIRECTORY-TRACKER, the shell mode's comint-input-sentinel.
;;; Comint mode does not provide functionality equivalent to 
;;; shell-set-directory-error-hook; it is gone.
;;; 
;;; If you are implementing some process-in-a-buffer mode, called foo-mode, do
;;; *not* create the comint-mode local variables in your foo-mode function.
;;; This is not modular.  Instead, call comint-mode, and let *it* create the
;;; necessary comint-specific local variables. Then create the
;;; foo-mode-specific local variables in foo-mode.  Set the buffer's keymap to
;;; be foo-mode-map, and it's mode to be foo-mode.  Set the comint-mode hooks
;;; (comint-prompt-regexp, comint-input-filter, comint-input-sentinel,
;;; comint-get-old-input) that need to be different from the defaults.  Call
;;; foo-mode-hook, and you're done. Don't run the comint-mode hook yourself;
;;; comint-mode will take care of it. The following example, from cmushell.el,
;;; is typical:
;;; 
;;; (defun shell-mode ()
;;;   (interactive)
;;;   (comint-mode)
;;;   (setq comint-prompt-regexp shell-prompt-pattern)
;;;   (setq major-mode 'shell-mode)
;;;   (setq mode-name "Shell")
;;;   (cond ((not shell-mode-map)
;;; 	     (setq shell-mode-map (full-copy-sparse-keymap comint-mode-map))
;;; 	     (define-key shell-mode-map "\M-\t" 'comint-dynamic-complete)
;;; 	     (define-key shell-mode-map "\M-?"
;;;                      'comint-dynamic-list-completions)))
;;;   (use-local-map shell-mode-map)
;;;   (make-local-variable 'shell-directory-stack)
;;;   (setq shell-directory-stack nil)
;;;   (setq comint-input-sentinel 'shell-directory-tracker)
;;;   (run-hooks 'shell-mode-hook))
;;;
;;;
;;; Note that make-comint is different from make-shell in that it
;;; doesn't have a default program argument. If you give make-shell
;;; a program name of NIL, it cleverly chooses one of explicit-shell-name,
;;; $ESHELL, $SHELL, or /bin/sh. If you give make-comint a program argument
;;; of NIL, it barfs. Adjust your code accordingly...
;;;

;;; Do the user's customisation...

(defvar comint-load-hook nil
  "This hook is run when comint is loaded in.
This is a good place to put keybindings.")
	
(run-hooks 'comint-load-hook)
