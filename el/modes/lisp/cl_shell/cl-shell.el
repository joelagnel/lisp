;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE:          cl-shell.el
;;; DESCRIPTION:   Extensions to the code in shell.el for running a Common
;;;                Lisp sub-process in a GnuEmacs buffer.
;;; AUTHOR:        Eero Simoncelli, 
;;;                Vision Science Group, 
;;;                MIT Media Laboratory.
;;; CREATED:       December, 1989
;;; MODIFIED:      See the file "cl-ChangeLog"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY.  No author or distributor accepts
;; responsibility to anyone for the consequences of using it or for
;; whether it serves any particular purpose or works at all, unless he
;; says so in writing.  Refer to the GNU Emacs General Public License
;; for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs
;; General Public License.  A copy of this license is supposed to have
;; been given to you along with GNU Emacs so you can know your rights
;; and responsibilities.  It should be in a file named COPYING.  Among
;; other things, the copyright notice and this notice must be
;; preserved on all copies.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The code in this file has been influenced by three other
;;; interfaces between Gnu Emacs and a Common Lisp (CL) subprocess.
;;; The first was written by Leigh Stoller and Robert Kessler of the
;;; University of Utah for running HPCL or PCLS on HP Bobcats.  It is
;;; HP-specific and somewhat limited in functionality.  The second
;;; comes with Franz's Allegro CL.  This one is a fairly hairy set of
;;; low-level extensions which set up multiple communication streams
;;; (Unix TCP sockets) between Emacs and Allegro CL.  The third was
;;; developed at Thinking Machines Corporation for use with Lucid
;;; Common Lisp on SUN machines, and is designed to simulate the
;;; Symbolics lispm environment.  It is large and is quite
;;; Lucid-dependent.

;;; This code provides a simple portable interface between Emacs and
;;; CL which allows CL to send messages to Emacs without the use of
;;; (non-Common-Lisp) multiple processes or special stream
;;; connections.  We view the standard output stream as a
;;; concatenation of non-nested special-purpose streams.  Emacs
;;; separates out the various substreams and treats them accordingly.
;;; In this sense, it achieves most of the functionality of the Franz
;;; code, without low-level modifications to Emacs.  We provide a
;;; mechanism for CL to send commands to Emacs, to display strings in
;;; the minibuffer or pop-up help buffers, or just to insert strings
;;; at the point.

;;; We also provide a set of functions which improve interaction with
;;; the lisp process when editing the *lisp* or lisp-mode buffers.
;;; These include direct (ie not through a temp file) evaluation and
;;; in-package compilation of forms from lisp-mode buffers with
;;; optional echo into the *lisp* buffer, type-ahead with multi-line
;;; editing and a history mechanism for the *lisp* buffer, and pop-up
;;; help facilities for the CL functions documentation, macroexpand
;;; and describe.  There is an additional file of extensions for Lucid
;;; Common Lisp which provide pop-up arglists and source file editing,
;;; including a sort of buffer menu to let the user choose from
;;; multiple definitions.  There are also extensions to do source
;;; files correctly (ie, let the user choose which method to edit) for
;;; FLAVORS, CLOS, or PCL.

;;; This code should be compatible with any implementation of Common
;;; Lisp -- the extensions for Lucid, FLAVORS, CLOS and PCL are only
;;; loaded (automatically) if these features are present in your CL
;;; environment -- and it requires no special code to be loaded into
;;; the CL environment.  CL can tell that this code is loaded by
;;; looking at the value of the global variable user::*emacs-cl-shell*
;;; which is set by run-cl.  See comments below (in the CL output
;;; filter section) on how to talk to Emacs from CL.  You should be
;;; aware that when you start up lisp, Emacs tells the CL process to
;;; define the macro user::compile-def for use in compiling top-level
;;; forms.

;;; To use this code, you should either copy this file and the files
;;; shell-history.el, cl-lucid.el, cl-clos.el, cl-pcl.el and
;;; cl-flavors.el into your main emacs/lisp directory, or add the
;;; directory containing these files to the Emacs variable load-path.
;;; Put the following lines in your .emacs file:

;;;   (setq load-path (cons "<directory-containing-this-file>" load-path))
;;;   (setq *cl-program* <pathname-of-lisp-executable>)  ;default is "lisp"
;;;   (autoload 'run-cl "<pathname-of-this-file>" "" t) 

;;; You may also need the following (defaults are correct for Lucid's ">" prompt):

;;;   (setq *cl-prompt* <emacs-regexp-for-prompt>)
;;;   (setq *cl-error-prompt* <emacs-regexp-for-error-prompt)

;;; To run lisp, type "M-x run-cl" in emacs.  Lisp will start up in a
;;; buffer called *lisp*, and after it comes up, a set of blank
;;; prompts will appear.  This is normal and occurs because emacs is
;;; sending initialization commands to the lisp process.  Lisp images
;;; other than the default one bound to the variable *cl-program* may
;;; be specified interactively by using a prefix arg.  You can get
;;; help on key bindings and a brief page of documentation by doing
;;; "C-h f cl-shell-mode". The files cl-lucid.el, cl-clos.el
;;; cl-pcl.el, and/or cl-flavors.el will be loaded automatically if
;;; the corresponding features are present in your lisp environment,
;;; but these files are not necessary for the code in this file to
;;; work.  If you want to add more key bindings, define a function
;;; called cl-shell-hook to do this.  Only bindings that everyone
;;; agrees on should be put in this file!

;;; NOTE: Known bugs or questionable behaviors are described in the
;;; file "cl-buglist", and are marked in this file and in the
;;; accompanying files with the string "***".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'shell)
(require 'tags)

(provide 'cl-shell)

;;;; -------------------- General stuff -------------------

;;; This is similar to the code in shell.el which defines inferior-lisp-mode.
;;; We introduce a cl-shell-mode so as not to clobber inferior-lisp-mode.
;;; Some of the defvars and defuns may need to be altered for different lisps.

(defvar *cl-pop-up* t
  "*If t, the *lisp* buffer pops up whenever it recieves output from the
Common Lisp process, and whenever the user sends an echoed command to
CL.  If 'output, only pop up when there is output from lisp.  If
'error, only pop up when Common Lisp encounters an error.  If the user
variable pop-up-windows is non-nil, then the window will be split if
necessary."
)

(defvar *cl-echo-commands* t
  "*If non-nil, commands being sent to Common Lisp are echoed into the
CL-shell output buffer.")

(defvar *cl-confirm-input* t
  "*If nil, carriage return in middle of the *lisp* buffer cause the
previous S-expression to be copied to the end of the *lisp* buffer and
executed.  Otherwise, the expression is just copied.")

(defvar *cl-replacement-prompt* nil
  "*If stringp, this is used to replace the top-level prompts in the 
Common Lisp output buffer.  Can be the empty string.  If nil, then
leave them as they are.  This variable is used to set the value of the
local variable cl-replacement-prompt when a CL shell buffer is created.")

(defvar *cl-program* "lisp"
  "*Program name for invoking a Common Lisp subshell with run-cl.")

;;; This regexp is used to recognize and parse the CL process prompts.
;;; This particular string is correct for Lucid CL.  Be careful that
;;; this does not conflict with the special-purpose stream tags
;;; defined below.
(defvar *cl-prompt* "^> "
  "Regular expression describing the Common Lisp prompt.")

;;; This is correct for Lucid CL.
(defvar *cl-error-prompt* "^\\(->\\)+ "
  "Regular expression describing the Common Lisp error prompt.")

;;; String printed before pathnames for your lisp dialect.  THis MUST
;;; END IN A DOUBLE QUOTE.  We assume pathnames are printed with this
;;; string followed by the actual pathname, and a closing quote.  This
;;; is used by cl-read-from-string to read CL pathnames.  This one is
;;; correct for Lucid CL
(defvar *cl-pathname-prefix-string* "#P\"")

;;; Quick debugger commands.  These are correct for Lucid CL.
(defun cl-abort () (interactive) (cl-send-string ":a\n"))

(defun cl-backtrace () (interactive) (cl-send-string ":b\n"))

;;; Replace the load command defined in shell.el with one for Common Lisp
;;; which is a little quieter.  For use in cl-compile-form.
(setq inferior-lisp-load-command "(load \"%s\" :verbose nil)\n")

;;; This is used by the cl-load-file command.
(setq cl-load-command "(load \"%s\")\n")

(defvar cl-fast-compile-regexp "(def\\(un\\|macro\\)[ \t\n]+"
  "Regular expression which determines whether to compile a top-level
s-expression by sending directly to the lisp process inside of a call
to user::compile-def, or by saving to a file and compiling the file.")

;;; This string is sent to CL at startup.  It defines the
;;; user::compile-def macro which is used by cl-compile-form to
;;; compile forms which match the cl-fast-compile-regexp.  Since
;;; compile-def will only be called for macros and defuns (see
;;; cl-fast-compile-regexp), we can just call compile on thing.
(defvar *cl-compile-def-definition*
  "(or (fboundp 'user::compile-def)	;don't bash it if it's already defined.
       (defmacro user::compile-def (thing)
         `(compile ,thing)))\n")

(defvar cl-compile-command "(compile-file \"%s\")\n"
  "CL compile-file command for use in compiling expressions that do
not match cl-fast-compile-regexp.")

(defvar *cl-default-buffer-package* t
  "*Default buffer package used by cl-set-buffer-package when 
 (in-package ...) cannot be found in the buffer.  Should typically 
be \"'user\" or t.  If t, then the buffer package will be the 
contents of the CL variable *package*.  Use cl-set-buffer-package 
to change the package of a buffer, and cl-get-buffer-package to get 
(and possibly set to the default) the package of a buffer.")

;;; This is used in lisps that have a source-file recording
;;; capability.  See cl-lucid.el for an example.
(defvar *cl-definition-regexp-alist* nil
  "An alist which keys off of definition types to give a regular
expression which Emacs uses to search for the definition.  The cdr
of each pair is either 1) a string suitable for an arg to a format
statement which inserts the symbol, or 2) a function which will be
called with the symbol and type-spec and should return a regexp.")

;;; *** Is this unnecessarily hairy? 
(defvar *cl-required-el-code-list*
  '(
    "#+:LUCID %s"   "(load \\\"cl-lucid\\\")"
    "#+:PCL %s"     "(load \\\"cl-pcl\\\")"
    "#+(and :LCL4.0 :CLOS) %s"   "(load \\\"cl-clos\\\")"
    "#+:FLAVORS %s" "(load \\\"cl-flavors\\\")"
    "%s"  "(progn (set-buffer (cl-buffer))
              (run-hooks 'shell-mode-hook 'lisp-mode-hook 'cl-shell-mode-hook))"
    )
  "A plist containing predicate strings which are to be evaluated in the
CL environment and command strings which will be executed in emacs-lisp 
if the predicates are non-nil.  The command will be inserted into the
predicate with a call to format.  These are executed (by run-cl) when emacs
receives the first prompt from the CL process.")

;;; This is called at startup time, after Lisp comes back with a prompt!
(defun cl-send-startup-commands ()
  (interactive)
  (cl-send-string " \n (progn\n")
  (cl-send-string
   "(format *standard-output* \"~%;;; Receiving startup commands from Emacs ...~%\")")
  ;; Define with-package and compile-def macros
  (cl-send-string *cl-compile-def-definition*)
  ;; Tell CL that Emacs is there.
  (cl-send-string "(setq user::*emacs-cl-shell* t)\n")
  (cl-send-string "(values))\n")	;end of progn
  ;; Load other emacs-lisp files according to features in CL
  (cl-load-required-el-code *cl-required-el-code-list*)
  (setq cl-shell-initialized-p t))

(defun cl-load-required-el-code (plist)
  (let (cl-command)
    (while plist
      (setq cl-command (concat "(format *standard-output* \""
			       (car (cdr plist))
			       "\")\n"))
      (cl-send-string
       (format (car plist)
	       (cl-concatenate-stream-id cl-command cl-eval-stream-id t)))
      (setq plist (cdr (cdr plist))))))

;;; Like copy-alist, but does it recursively so there are no shared structures.
(defun copy-tree (tree)
  (cond ((consp tree) (cons (copy-tree (car tree)) (copy-tree (cdr tree))))
	((sequencep tree) (copy-sequence tree))	;string or vector, but not list
	(t tree)))			;non-sequence atoms

(defvar cl-shell-mode-map 
  (let ((the-map (make-sparse-keymap))) ;(copy-tree inferior-lisp-mode-map)
    (lisp-mode-commands the-map)

    (define-key the-map "\C-cb" 'cl-backtrace)
    (define-key the-map "\C-ca" 'cl-abort)

    (define-key the-map "\C-c\C-c" 'cl-interrupt-process)
    (define-key the-map "\C-c\C-p" 'cl-set-buffer-package)

    ;; In addition, arglists are defined for Lucid in the file cl-lucid.el
    (define-key the-map "\C-c\C-d" 'cl-documentation)
    (define-key the-map "\C-c\C-m" 'cl-macroexpand-1)
    (define-key the-map "\C-c\C-i" 'cl-describe) ;info
    (define-key the-map "\C-c\C-h" 'cl-apropos)	 ;help

    (define-key the-map "\C-c\C-q" 'cl-bury-help)

    (define-key the-map "\C-a" 'cl-beginning-of-line)
    (define-key the-map "\C-m" 'cl-shell-send-input)  ;Carriage Return

    (define-key the-map "\M-\C-y" 'shell-yank-history)
    (define-key the-map "\M-\C-z" 'shell-yank-history-forward)

    (define-key the-map "\M-p" 'shell-yank-matching-history)
    (define-key the-map "\M-n" 'shell-yank-matching-history-forward)

    the-map))

(defun cl-add-lisp-mode-key-bindings ()
  "Add key bindings to lisp-mode-map for use with cl-shell-mode."
  (let ((the-map lisp-mode-map))
    (define-key the-map "\C-ce" 'cl-eval-form)
    (define-key the-map "\M-\C-x" 'cl-eval-form) ;get rid of old binding
    (define-key the-map "\C-cr" 'cl-eval-region)
    (define-key the-map "\C-cc" 'cl-compile-form)

    (define-key the-map "\C-c\C-p" 'cl-set-buffer-package)
    (define-key the-map "\C-cl" 'cl-load-file)
    (define-key the-map "\C-cx" 'cl-compile-file)

    (define-key the-map "\C-c\C-d" 'cl-documentation)
    (define-key the-map "\C-c\C-m" 'cl-macroexpand-1)
    (define-key the-map "\C-c\C-i" 'cl-describe) ;info
    (define-key the-map "\C-c\C-h" 'cl-apropos)	 ;help

    (define-key the-map "\C-c\C-q" 'cl-bury-help)

    (define-key the-map "\C-ca" 'cl-abort)
    (define-key the-map "\C-cb" 'cl-backtrace)
    the-map))

;;;; -------------------- cl-shell-mode stuff -------------------

;;; *** Should define the markers last-input-start and last-input-end
;;; (see shell-mode) since some inferior-lisp-mode-map bindings rely
;;; on them.  

;;; We define a new mode instead of bashing the standard Emacs lisp
;;; interaction mode (inferior-lisp-mode).
(defun cl-shell-mode (&optional dont-run-hooks)
  "Major mode for interacting with a Common Lisp process.  This mode
should be entered using the command run-cl, which runs a Lisp 
interpreter as a subprocess of Emacs, with Lisp I/O through a buffer
named *lisp*.  Run-cl takes an optional argument which defaults to the
value of the variable *cl-program*.  
Variable *cl-pop-up* determines whether the *lisp* buffer pops up on
output from the CL process.
Variable *cl-echo-commands* determines whether commands being sent to
Common Lisp are echoed in the *lisp* buffer. 
Variable *cl-confirm-input* determines whether carriage returns in middle
of *lisp* buffer send the preceding S-expression directly to CL or
require a second carriage-return.
Variable *cl-replacement-prompt* determines whether the CL prompts
should be replaced. 
Variables *cl-prompt* and *cl-error-prompt* should be customized for
the specific implementation of Common Lisp.  The defaults are correct
for Lucid.

Commands:
Return at end of the *lisp* buffer sends line as input.
Return not at end sends the top-level sexp starting at or before the
cursor.  
C-M-a goes to previous top-level form (as in lisp-mode buffers),
and C-a respects the listener prompt. 
The following commands are provided from within the *lisp* buffer:
In general, the C-c C-<char> commands do not change the state of the 
CL environment, and the C-c <char> do.
\\{cl-shell-mode-map}
Most of the C-c commands are also provided in lisp-mode buffers.  In
addition, the following bindings are added to lisp-mode buffers for
sending things to the CL process:

C-c e           cl-eval-form
C-c r           cl-eval-region
C-c c           cl-compile-form
C-c l           cl-load-file
C-c x           cl-compile-file

NOTE: Compilations are performed in the package of the lisp-mode buffer!

Entry to cl-shell-mode calls the functions shell-mode-hook, lisp-mode-hook
and cl-shell-mode-hook with no arguments, if any of these are non-nil."
  (interactive "P")
  (kill-all-local-variables)		;clean up previously existing mode.
  (setq major-mode 'cl-shell-mode)
  (setq mode-name "cl-shell")
  (lisp-mode-variables t)
  (setq mode-line-process '(": %s"))
  (make-local-variable 'cl-replacement-prompt)
  (setq cl-replacement-prompt *cl-replacement-prompt*)
  (make-local-variable 'cl-filter-state)
  (setq cl-filter-state nil)
  (make-local-variable 'cl-filter-string)
  (setq cl-filter-string "")
  (make-local-variable 'cl-shell-initialized-p)
  (setq cl-shell-initialized-p nil)
  (use-local-map cl-shell-mode-map)
  (cl-add-lisp-mode-key-bindings)	;add key bindings to lisp-mode-map
  (if (null dont-run-hooks)
      (run-hooks 'shell-mode-hook 'lisp-mode-hook 'cl-shell-mode-hook)))

;;; Use the same name as standard lisp-mode.   Currently, we assume only
;;; one lisp process.
(defconst *cl-process-name* "lisp")

;;; Returns the current CL subshell process.  We abstract this out for
;;; future extensions which may allow multiple lisp processes, in
;;; which this would return the "current" cl process.
(defun cl-process () (get-process *cl-process-name*))

(defun cl-buffer () 
  (let ((proc (cl-process)))
    (if proc 
	(process-buffer proc)
	nil)))

;;; Useful top-level function.  I usually bind it globally to C-M-l.
(defun cl-goto-lisp-buffer ()
  "Makes the *lisp* buffer the current buffer, running lisp if necessary"
  (interactive)
  (let ((buf (cl-buffer)))
    (if buf (pop-to-buffer buf) (run-cl))))

;;; Replacement for the standard Emacs run-lisp.  We assume there is
;;; only one lisp process running and that its name is
;;; *cl-process-name*.  This is the simplest behavior to deal with
;;; when evaluating commands from lisp-mode buffers.  It should,
;;; however, be easy to extend the code to work with multiple lisps.
;;; The initialization command cl-send-startup-commands is run when
;;; the CL process comes to top-level (ie when Emacs sees the prompt).
;;; This runs the mode hooks.
(defun run-cl (&optional cl-program)
  "Run Common Lisp as a subshell process.  With a prefix argument, prompts
for a pathname for the Lisp program to run.  Otherwise, uses the default
pathname specified by the global variable *cl-program*."
  (interactive "P")			;takes a prefix arg
  (cond ((null cl-program)		;use default program
	 (setq cl-program *cl-program*))
	((numberp cl-program)		;user called with prefix argument
	 (setq cl-program (read-file-name "Common Lisp Program: "
					  *cl-program* *cl-program* t))))
  (cond ((cl-buffer)
	 (let ((buf (current-buffer)))
	   (pop-to-buffer (cl-buffer))
	   (cl-shell-mode)		;set up mode, running hooks
	   (set-process-filter (cl-process) 'cl-output-filter)
	   (set-buffer buf))
	 (error "Process %s already exists." (process-name (cl-process))))
	(t
	 ;; This will split the window if global var pop-up-windows is non-nil:
	 (pop-to-buffer 
	  (let ((process-connection-type nil)) ;pipes have better buffering
	    (make-shell *cl-process-name* cl-program)))
	 (erase-buffer)
	 ;; set up local variables, keymap, modeline, etc.
	 (cl-shell-mode 'dont-run-hooks) ;set up mode, don't run hooks
	 ;; Filter parses special purpose output streams from CL.
	 (set-process-filter (cl-process) 'cl-output-filter))))

;;; The usual one for the shell only interrupts the current subjob.
(defun cl-interrupt-process ()
  "Send an interrupt to the lisp process created by run-cl."
  (interactive)
  (interrupt-process (cl-process)))

;;; We replace the standard kill-buffer function with one that is more
;;; careful about killing the CL process.
(defvar standard-kill-buffer (symbol-function 'kill-buffer))

;;; Ask user if they are sure, and then make sure lisp dies.  This is
;;; especially important for lisps that can run multiple processes.
(defun kill-buffer (the-buffer)
  (interactive "bKill buffer: ")	;prompt for a buffer, default to current
  (let ((the-process (cl-process)))
    (if (and the-process		;if lisp is running in this buffer
	     ;; *** Should also check if process is active.
	     (eq the-process (get-buffer-process the-buffer)))
	(if (yes-or-no-p "Are you sure you want to kill the Common Lisp process? ")
	    (progn
	      ;; get rid of filter, in case it is screwing up
	      (set-process-filter (cl-process) nil)
	      (interrupt-process the-process t)
	      (message "Killing Common Lisp process ...")
	      (sleep-for 1)	;wait a few seconds for death
	      (condition-case ()
		  (delete-process the-process)
		(error nil))
	      (funcall standard-kill-buffer the-buffer)
	      (message "Killing Common Lisp process ... done.")))
	(funcall standard-kill-buffer the-buffer))))	;otherwise, do the usual
	

;;;; ----------------- cl-shell-mode buffer commands ------------------

;;; Similar to shell-send-input in shell.el, this function is called
;;; when the user types a newline in the interactive buffer.  The
;;; process-mark points to the place where CL inserts its output, or
;;; the end of the last input that was sent to CL (if output has not
;;; yet been received).  It is pushed forward when output is received
;;; from CL (see cl-filter).  Expressions sent to CL using
;;; cl-send-string-with-echo are echoed at this point.  If
;;; cl-shell-send-input is called with the point beyond the
;;; process-mark, it sends everything between process-mark and
;;; point-max (end of buffer) to the Lisp process, as long as there is
;;; at least one s-expression in that region and all sexps are
;;; complete.  [*** Might be better to send all complete sexp's and
;;; set process mark after the last one].  If any sexps are not not
;;; complete, then nothing is sent, but a newline is inserted at the
;;; point.  If cl-shell-send-input is called with the point before the
;;; process-mark, we copy the previous top-level sexp to the end of
;;; the buffer, and send it to CL.  If the sexp is not complete, it
;;; just beeps [*** Would be nice if return in middle of buffer
;;; inserted a newline too].
(defun cl-shell-send-input ()
  "Send input from the *lisp* buffer to the Common Lisp subshell."
  (interactive)
  (let ((original-point (point))
	(complete-p nil)
	(process-mark (process-mark (cl-process))))
    (cond ((>= original-point process-mark) ;at end of buffer, so try to send sexps.
	   (goto-char process-mark)
	   (setq complete-p		;check if all sexps typed are complete.
		 (condition-case ()	;catch eof errors
		     (and (scan-sexps (point) 1) ;at least one sexp
			  (progn (while (scan-sexps (point) 1) 
				   (goto-char (scan-sexps (point) 1)))
				 t))
		   (error		;incomplete sexp or extra parens
		    (if (looking-at "[ \t\n]*)") ;extra parens
			(progn (setq original-point (point))
			       (beep)
			       (message "Unbalanced parentheses!")))
		    nil)))
	   ;; Point is now at end of last sexp
	   (cond (complete-p
		  (delete-region (point) (point-max)) ;get rid of extra spaces
		  (shell-add-history (buffer-substring process-mark (point-max)))
		  (insert ?\n)		;leaving a single newline
		  (cl-send-region process-mark (point-max)) ;send it.
		  (set-marker process-mark (point-max)))
		 (t (goto-char original-point) ;insert a newline if not complete-p
		    (lisp-newline))))
	  (t (end-of-line)		;if right on defun, don't go backwards
	     (beginning-of-defun)	;goto start of top-level sexp
	     (let ((str (buffer-substring
			 (point) (progn (forward-sexp 1) (point)))))
	       (goto-char process-mark)
	       (if *cl-confirm-input*
		   (insert str)
		   (cl-send-string-with-echo str)))))))

;;; This function calls whatever function <carriage-return> is bound
;;; to in lisp-mode.  We do this so that users can redefine newline to
;;; auto-indent things and get the same behavior in the *lisp*
;;; buffer...
(defun lisp-newline () 
  (interactive)
  (funcall (or (lookup-key lisp-mode-map "\C-m")  'newline)))

;;; Modified beginning-of-line that ignores prompts.
(defun cl-beginning-of-line (&optional arg)
  "Move point to beginning of current line of cl-shell buffer, 
ignoring prompts.  With argument ARG not nil or 1, move forward
ARG - 1 lines first.  If scan reaches end of buffer, stop there 
without error."
  (interactive "p")
  (let ((the-regexp
	     (cond ((null cl-replacement-prompt)
		    (format "\\(%s\\|%s\\)" *cl-prompt* *cl-error-prompt*))
		   ((string= cl-replacement-prompt "")
		    *cl-error-prompt*)
		   (t 
		    (format "\\(^%s\\|%s\\)" cl-replacement-prompt
			    *cl-error-prompt*)))))
	(beginning-of-line arg)
	(if (looking-at the-regexp) (goto-char (match-end 0)))))

;;; We replace the usual definition for beginning-of-defun.  This is much
;;; easier than having to rewrite end-of-defun (see lisp.el).
(defvar standard-beginning-of-defun (symbol-function 'beginning-of-defun))

;;; Tries to find a prompt followed by left paren or a word (symbol).
;;; *** Doesn't do the right thing with comments.
(defun beginning-of-defun (&optional arg)
  "Move backward to next beginning-of-defun, ignoring prompts if
in a cl-shell buffer.  With argument, do this that many times.
Returns t unless search stops due to end of buffer."
  (interactive "p")
  (if (eq major-mode 'cl-shell-mode)
      (let* ((cl-prompt (or (and cl-replacement-prompt
				 (concat "^" cl-replacement-prompt))
			    *cl-prompt*))
	     (the-regexp 
	      (cond ((string= cl-prompt "")
		     (format "\\(^\\|%s[ \t\n]*\\)[^ \t\n]" *cl-error-prompt*))
		    (t
		     (format "\\(%s\\|%s\\)[ \t\n]*[^ \t\n]"
			     cl-prompt *cl-error-prompt*)))))
	(and arg (< arg 0) (forward-char 1))
	(and (re-search-backward the-regexp nil 'move (or arg 1))
	     (progn (goto-char (1- (match-end 0))) t)))
      (funcall standard-beginning-of-defun arg)))

;;;; ----------------- Buffer packages -----------------------

;;; *** Should we only take in-package at or near top of file?
(defun cl-set-buffer-package (&optional pkg)
  "Set the package of a lisp buffer.  PKG argument is optional:
it can be nil, t, or a string.  If nil, package is found by
searching for in-package command in buffer, using the value of
*cl-default-buffer-package* if it is not found.  If t, the current
package will be used when compiling things from this buffer.  If
a string, it will be used as a package name. If prefix arg is given
with command, user will be prompted for an argument, which should
be string which can be used as a CL package name."
  (interactive "sCL package: ")		;prompt for string
  (if (string= pkg "") (setq pkg nil))	;interactive, with no arg passed.
  (if (or (string= pkg "t") (string= pkg "T")) ;interactive t typed
      (setq pkg t))
  (if (stringp pkg) (setq pkg (cl-add-quote pkg))) ;make sure quoted
  (let ((the-buf (current-buffer)))
    (if (null (assq 'buffer-package (buffer-local-variables the-buf)))
	(progn
	  (make-local-variable 'buffer-package)
	  (make-local-variable 'mode-line-buffer-identification)))
    (if pkg				;t or string.
	(setq buffer-package pkg)
	(if (eq (current-buffer) (cl-buffer))	;if *lisp* buffer
	    (setq buffer-package t)
	    (save-excursion
	      (beginning-of-buffer)
	      (if (re-search-forward "^(in-package[ \t\n]+" nil t)
		  (let ((start (point)))
		    (forward-sexp)
		    (setq buffer-package (buffer-substring start (point))))
		  (setq buffer-package (or *cl-default-buffer-package* t))
		  (beep)
		  (message "Warning: can't find in-package command in buffer.")))))
    (setq mode-line-buffer-identification
	  (list "" (default-value 'mode-line-buffer-identification)
		(format " (Pkg: %s)"
			(if (stringp buffer-package)
			    buffer-package
			    "*package*"))))
    (cl-get-buffer-package)))

;;; Use this function to get the package of a buffer.  This must
;;; return a string that can be sent inside an in-package command to
;;; the CL process.
(defun cl-get-buffer-package ()
  (cond ((null buffer-package) (cl-set-buffer-package))	;hasn't been computed
	((stringp buffer-package) buffer-package)
	(t "(package-name *package*)")))      ;if t, use current package.
	    
;;; A buffer variable containing a string which is the Common Lisp
;;; package of the file.  If t the file has no package and evaluation
;;; uses the package *package*.
(setq-default buffer-package nil)

;;; Take a string that is meant to be sent to CL for execution, and
;;; wrap stuff around it so that it will be read in the given package.
;;; A bit hairy, but I don't know how else to do it...  This is used
;;; for compiling, documentation, macroexpansion, arglists, etc.
(defun cl-with-package (package body-string)
  (concat "(let ((pkg lisp:*package*)
                 val)
             (in-package "  package  ")
             (unwind-protect
	          (setq val (eval (read-from-string
                    " (cl-make-readable-string body-string) ")))
		(in-package (package-name pkg))
                val))\n"))
 
;;;; ----------------- lisp-mode buffer commands -----------------

;;; This is called to evaluate top-level expressions in lisp-mode
;;; buffers.  If *cl-echo-commands* is non-nil, the expression is
;;; echoed into the listener (*lisp*) buffer.  Unlike lisp-send-defun
;;; (defined in shell.el), we send the form directly to the CL
;;; process. We do not do this in the package of the current buffer -
;;; it is as if the user typed the form to the listener.  The
;;; elaborate args to buffer-substring are to ensure that a newline is
;;; not included with the string.
;;; *** Wasteful: conses a string here.
(defun cl-eval-form ()
  "Send the current top-level sexp to the CL process created by
M-x run-cl, moving to end of sexp.  If *cl-echo-commands* is non-nil,
echo the sexp into cl-shell buffer."
  (interactive)
  (end-of-defun)			;move to end of defun
  (let ((the-string 
	 (save-excursion		;leave point at end of defun
	   (buffer-substring (progn (beginning-of-defun) (point))
			     (progn (forward-sexp 1) (point))))))
    (if *cl-echo-commands*
	(cl-send-string-with-echo the-string)
	(cl-send-string (concat the-string "\n")))))

;;; Send the marked region to CL.  This is usually used to send large
;;; numbers of forms at once (otherwise, you could use cl-eval-form)
;;; and so does not echo into the listener.
(defun cl-eval-region (start end)
  "Send region between point and mark to CL process, without echoing."
  (interactive "r")
  ;; check that expressions are complete.  Take overhanging ones.
  (save-excursion
    (goto-char start)
    (setq end (progn
		(while (and (< (point) end)
			    (scan-sexps (point) 1))
		  (goto-char (scan-sexps (point) 1))
		  (skip-chars-forward " \t\n" end))
		(point)))
    (cl-send-string "(progn\n")
    (cl-send-region start end)
    (cl-send-string "\n(values))\n"))) ;send final newline

;;; If the beginning of the form doesn't match cl-fast-compile-regexp,
;;; we save it to a file (with an in-package statement at the top),
;;; compile it using cl-compile-command, and load it using
;;; inferior-lisp-load-command.  Otherwise, we send it directly to CL,
;;; relying on a CL macro called compile-def, which is defined by
;;; run-cl.  This macro may be redefined in a given lisp environment
;;; to allow compilation of things like methods (see cl-clos.el and
;;; cl-pcl.el).  Compilation occurs in the package of the buffer.  We
;;; echo a shorthand expression into the CL-shell buffer, which
;;; indicates the symbol being compiled and the package.  *** BUG:
;;; Behavior is different when compilation happens through a file: the
;;; value of the compilation is not returned...
;;; *** Conses a string here.
(defun cl-compile-form ()
  "Send the current top-level sexp to the CL process created by M-x
run-cl, and compile it in the package of the current buffer.  The
point is moved to the end of the sexp, and if *cl-echo-commands* is
non-nil a shorthand expression is echoed to the *lisp* buffer."
  (interactive)
  (or (cl-process) (error "CL process is not running!"))
  (end-of-defun)			;move to end of sexp
  (let ((cl-package (cl-get-buffer-package))
	the-string fn-name)
    (save-excursion			;leave point at end of defun
      (beginning-of-defun)
      (setq the-string 
	    (buffer-substring (point) (save-excursion (forward-sexp 1) (point))))
      ;; Set up fn-name and the-string, depending on compiling mode:
      (if (null (looking-at cl-fast-compile-regexp))
	  (setq the-string (cl-with-package
			    cl-package
			    (concat "(funcall (compile nil #'(lambda () "
				    the-string
				    ")))\n"))
		fn-name (concat (buffer-substring
				 (point)
				 (progn (forward-char 1) (forward-sexp 2) (point)))
				" ... )"))
	  (forward-char 1)		;skip open paren
	  (forward-sexp 1)		;skip "defun"
	  (skip-chars-forward " \t\n")	;skip whitespace to function name
	  (setq fn-name (buffer-substring (point) (progn (forward-sexp 1) (point))))
	  (setq the-string
		(cl-with-package cl-package
				 (concat "(user::compile-def " the-string ")")))))
    (if *cl-echo-commands*
	(cl-send-string-with-echo
	 the-string
	 (concat "(compile-def '" fn-name " :pkg " cl-package ")")
	 t)				;no history recording
	(cl-send-string (concat the-string "\n")))))

(defun cl-load-file (pathname)
  "Load file of current buffer into the CL process."
  (interactive
   (let ((default-file-name (cl-strip-file-extension buffer-file-name)))
     (list
      (read-file-name "CL load file: " default-file-name default-file-name nil))))
  (let ((buffer (or (get-file-buffer pathname) (get-file-buffer (concat pathname ".lisp")))))
    (if (and buffer 
	     (buffer-modified-p buffer)
	     (y-or-n-p
	      (concat "Buffer " (buffer-name buffer) " modified, save it first? ")))
	(save-buffer buffer)))
  (if *cl-echo-commands*
      (cl-send-string-with-echo (format cl-load-command pathname))
      (cl-send-string
       (concat "(progn "
	       (format cl-load-command pathname)
	       "(values))\n"))))
   
(defun cl-compile-file (pathname)
  "Ask CL to compile file of current buffer."
  (interactive 
   (list
    (expand-file-name
     (read-file-name "CL compile file: " buffer-file-name buffer-file-name t))))
  (let ((buffer (get-file-buffer pathname)))
    (if (and buffer 
	     (buffer-modified-p buffer)
	     (y-or-n-p 
	      (concat "Buffer " (buffer-name buffer) " modified, save it first? ")))
	(save-buffer buffer)))
  (if *cl-echo-commands*
      (cl-send-string-with-echo (format cl-compile-command pathname))
      (cl-send-string
       (concat "(progn "
	       (format cl-compile-command pathname)
	       "(values))\n"))))

;;;; ----------------- CL utilities ------------------
;;; This stuff is specific to Common Lisp (as opposed to other Lisps).

;;; Ask CL to macroexpand-1 the current sexp in the package of the buffer.
(defun cl-macroexpand-1 (&optional in-situ)
  "Ask the cl-shell process to call macroexpand-1 on the sexp surrounding
or following the point.  With a prefix arg, insert the lowercase macroexpansion
at the point.  Otherwise, display uppercase macroexpansion in a pop-up help
buffer."
  (interactive "P")			;takes a prefix arg
  (if (numberp in-situ)		;with prefix, insert at point
      (cl-send-request cl-input-stream-id (cl-get-buffer-package)
		       (concat "(write (macroexpand-1 '"
			       (cl-get-sexp)
			       ") :pretty t :level nil :length nil :case :downcase)"))
      (cl-send-request cl-help-stream-id (cl-get-buffer-package)
		       (concat "(write (macroexpand-1 '"
			       (cl-get-sexp)
			       ") :pretty t :level nil :length nil)"))))

(defun cl-documentation (symbol)
  "Ask the cl-shell process for documentation on the given symbol (a string)."
  (interactive (cl-ask-for-symbol "CL documentation of: "))
  (let ((pkg (cl-get-buffer-package)))
    (cl-send-request cl-help-stream-id pkg
      (format "(let ((*print-length* nil))
                 (format t \"~A:~%%~A\" %s
	           (cond ((fboundp %s) (documentation %s 'function))
                         ((boundp %s)  (documentation %s 'variable))
                         (t (format nil \"unbound in package ~A.\" %s)))))"
			   symbol symbol symbol symbol symbol pkg))))

(defun cl-apropos (symbol)
  "Ask the cl-shell process to run apropos on the given symbol (a string)."
  (interactive (cl-ask-for-symbol "CL apropos of: "))
  (let ((pkg (cl-get-buffer-package)))
    (cl-send-request cl-help-stream-id pkg
       (format "(apropos %s %s)" symbol pkg))))
  
(defun cl-describe ()
  "Ask the cl-shell process for a description of the contents of symbol."
  (interactive)
  (let ((sym-or-expr (cl-get-sexp-or-symbol)))
    (cl-send-request cl-help-stream-id (cl-get-buffer-package)
	(format "(describe %s)" sym-or-expr))))

;;; Ask the user for a symbol, taking as a default the symbol closest
;;; to the cursor.  Return a list containing the symbol, suitable for
;;; use by the interactive function.
(defun cl-ask-for-symbol (&optional prompt)
  (list (cl-add-quote (car (find-tag-tag (or prompt "Symbol: "))))))

;;; Could use tmc/utils.el or allegro/keys.el versions.
;;; Get the surrounding sexp, or if the point is on a paren, the
;;; sexp associated with that paren.  Gives an error if sexps are incomplete.
(defun cl-get-sexp ()
  (save-excursion
    (cond ((= (following-char) ?\() nil) ;great
	  ((= (preceding-char) ?\)) (backward-sexp 1))  ;goto start of sexp
	  ((looking-at "[ \t\n]*(") (goto-char (1- (match-end 0))))
	  (t (backward-up-list 1)))
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

;;; This is used by cl-describe
(defun cl-get-sexp-or-symbol ()
  (let ((prefix nil))
    (save-excursion
      (forward-sexp 1)
      (backward-sexp 1)
      (skip-chars-backward " \t\n")
      (if (= (preceding-char) ?\() (setq prefix "#'")) ;function
      (skip-chars-forward " \t\n")
      ;(if (= (following-char) ?\() nil (setq prefix "'")) ;symbol
      (if prefix
	  (concat prefix (buffer-substring (point) (progn (forward-sexp 1) (point))))
	  (buffer-substring (point) (progn (forward-sexp 1) (point)))))))

;;; Make sure the-string starts with a single quote, and turn single
;;; colon into double colon so that non-exported symbols don't cause
;;; problems.
(defun cl-add-quote (the-string)
  (let ((colon-pos (string-match ":+" the-string)))
    (if (and colon-pos			;found one
	     (> colon-pos 0)		;not at beginning (not a keyword)
	     (= (1+ colon-pos) (match-end 0))) ;only one
	(setq the-string
	      (concat (substring the-string 0 colon-pos)
		      ":"
		      (substring the-string colon-pos))))
    (cond ((char-equal (aref the-string 0) ?\') ;check for quote
	   the-string)
	  ((string= (substring the-string 0 2) "\#\'")
	   (substring the-string 1))
	  (t (concat "'" the-string)))))

;;; Arg can be a symbol or a string.  Returns string of symbol without
;;; package prefix, and without quote.  Also slashifies all characters
;;; which are special Emacs regexp characters, since the returned
;;; string is go be used in a regexp.  We also throw in a downcase
;;; to cover the situations when case-fold-search is nil.
(defun cl-strip-package (symbol)
  (let ((the-string (regexp-quote (downcase (format "%s" symbol)))))
    (if (char-equal (aref the-string 0) ?\')
	(setq the-string (substring the-string 1)))
    (if (and (not (char-equal (aref the-string 0) ?\:))
	     (string-match ":+" the-string))
	(setq the-string (substring the-string (match-end 0))))
    the-string))

(defun cl-strip-file-extension (filename)
  (let* ((fullname (expand-file-name filename))
	 (file (file-name-nondirectory fullname))
	 (barefile ""))
    ;; concat up to last "." into barefile
    (while (string-match "\\.[^. ]*$" file)
      (setq barefile (concat barefile (substring file 0 (match-beginning 0))))
      (setq file (substring file (+ (match-beginning 0) 1))))
    (if (string= barefile "")
	fullname
	(concat (file-name-directory filename) barefile))))

;;; Take a string, and return a string which can be read by the CL
;;; reader.  Basically, inserts a backslash in front of every
;;; quotation mark or backslash.  DO NOT use the resulting string in a
;;; format statement.  If it has a % character in it, it will fail!
(defun cl-make-readable-string (the-string)
  (save-excursion
    (set-buffer (get-buffer-create "*CL compilation*"))
    (erase-buffer)
    (insert the-string)
    (goto-char (point-min))
    (insert ?\")			;put quotation mark at beginning
    (while (re-search-forward "\\(\"\\|\\\\\\)" nil 'move) 
      (backward-char 1)
      (insert ?\\)
      (forward-char 1))
    (goto-char (point-max))
    (insert ?\")			;put quotation mark at end
    (buffer-string)))      

;;;; ----------------- Low-level CL input functions -----------------

;;; Echo string to *lisp* buffer with reindentation, and then send to
;;; CL.  Adds a newline.  Marker handling should be the same as for
;;; complete sexps in cl-shell-send-input.
(defun cl-send-string-with-echo (the-string &optional the-echo-string no-history)
  (if (null the-echo-string) (setq the-echo-string the-string))
  (let ((buf (current-buffer))
	(cl-buf (cl-buffer)))
    (if (and (null (get-buffer-window cl-buf)) (eq *cl-pop-up* t))
	(display-buffer cl-buf))	;splits window if pop-up-windows is non-nil
    (set-buffer cl-buf)
    (goto-char (process-mark (cl-process)))
    (insert-before-markers the-echo-string) ;insert, pushing markers forward
    (insert-before-markers "\n")
    (save-excursion			;reindent the expression
      (narrow-to-region
       (point)
       (progn (backward-sexp 1) (save-excursion (beginning-of-line) (point))))
      (unwind-protect (indent-sexp) (widen)))
    (if (get-buffer-window cl-buf)	;this seems to be necessary...
	(set-window-point (get-buffer-window cl-buf) (point)))
    (set-buffer buf)
    (if (null no-history) (shell-add-history the-string cl-buf))
    (setq the-string (concat the-string "\n"))
    (cl-send-string the-string)))

;;; This kludge seems to be necessary to avoid dropping data...
(defvar *cl-packet-size* 255
  "Size of chunks sent to the CL subprocess.")

;;; Low-level function for sending a region from the current buffer to 
;;; the CL process.
(defun cl-send-region (start end)
  (let ((packet-end (min end (+ start *cl-packet-size*))))
    (if (and (condition-case nil
		 (progn (process-send-region 
			 (cl-process)
			 start packet-end)
			t)
	       (error 
		(error "cl-send-region: Error sending region to Common Lisp.")
		nil))
	     (< packet-end end))
	(cl-send-region packet-end end))))

;;; Low-level function for sending a string to the CL process.  Tacks
;;; a newline onto the end.
(defun cl-send-string (string)
  (let ((start 0)
	(end (length string)))
    (while (and (< start end)
		(condition-case nil
		    (progn (process-send-string
			    (cl-process)
			    (substring string start 
				       (min end (+ start *cl-packet-size*))))
			   t)
		  (error
		   (error "cl-send-string: Error sending string to Common Lisp.")
		   nil)))
      (setq start (+ start *cl-packet-size*)))))

;;; The-string should be a common lisp command.  It will be evaluated by CL and
;;; should print the desired help information to *standard-output*.
;;; NOTE: this function is now obsolete.  Use cl-send-request.
(defun cl-send-help-request (the-string)
  (cl-send-string
   (cl-concatenate-stream-id the-string cl-help-stream-id)))

;;; Send string to CL, to be evaluated in package, with output printed
;;; between markers for the given stream-id.  This is used to request
;;; information from CL such as macroexpansion, documentation, etc.
;;; It could be shadowed in multi-processing lisps to talk to a help
;;; process.
(defun cl-send-request (stream-id package string)		       
  (cl-send-string
   (cl-concatenate-stream-id
    (cl-with-package package string)
    stream-id)))

;;; Produces a string to which may be sent to CL.  The cl-command
;;; should be a string containing a CL command which prints to
;;; standard output.  Don't bother sending the close-marker if the
;;; resulting string is to be sent to CL at top-level.
(defun cl-concatenate-stream-id (cl-command stream-id &optional close-marker)
  (if close-marker
      (format "(progn 
	         (format *standard-output* \"[[%s>>\")
                 (force-output *standard-output*)
                 %s
                 (force-output *standard-output*)
                 (format *standard-output* \"<<%s]]\")
                 (force-output *standard-output*)
                 (values))\n"
	      stream-id cl-command stream-id)
      (format "(progn 
	         (format *standard-output* \"[[%s>>\")
                 (force-output *standard-output*)
                 %s
                 (force-output *standard-output*)
                 (values))\n"			;used to return T for parsing
	      stream-id cl-command)))

;;;; ----------------- CL output filter ------------------

;;; CL can talk to emacs by sending strings to standard-output inside
;;; of special-purpose markers.  There are four different markers
;;; defined below.  The input-stream inserts the string at the point
;;; (as if the user had typed it), the message-stream marker inserts
;;; the string into the minibuffer, the help-stream inserts the string
;;; into a pop-up help buffer, and the eval stream causes Emacs to
;;; read from the string and eval the result.  You should make an
;;; effort to ensure that the stream markers are sent un-interrupted.
;;; Example usage:
;;;   (progn
;;;     (format *standard-output* "[[MESSAGE-STREAM>>")
;;;     (force-output *standard-output*)
;;;     (format *standard-output* "Stick this string in the minibuffer")
;;;     (force-output *standard-output*)
;;;     (format *standard-output* "<<MESSAGE-STREAM]]")
;;;     (force-output *standard-output*))

;;; Strings identifying the various lisp filter states.  The states
;;; allow parsing of special purpose information coming from the lisp
;;; process.  We could make do with a single type of stream (the
;;; eval-stream): the others are here for convenience and historical
;;; reasons!  NOTE: These strings must consist of all Capital letters
;;; and hyphens.  They must be sent unbroken to the lisp filter: the
;;; parsing done in the following routines is crude and will not
;;; recognize the markers if they are segmented!!
(defconst cl-input-stream-id "INPUT-STREAM")  ;insert stuff at point for use as input
(defconst cl-message-stream-id "MESSAGE-STREAM") ;insert stuff in minibuffer
(defconst cl-help-stream-id "HELP-STREAM")   ;display in pop-up help buffer.
(defconst cl-eval-stream-id "EVAL-STREAM") ;stuff to be evaluated by emacs

;;; This function does the right thing with special purpose streams.
(defun cl-handle-special-stream (the-state the-string)
  (cond ((string= the-state cl-input-stream-id)
	 (insert the-string))
	((string= the-state cl-message-stream-id)
	 (message the-string))
	((string= the-state cl-help-stream-id)
	 (cl-pop-up-help the-string))
	((string= the-state cl-eval-stream-id)
	 (eval (cl-read-from-string the-string)))
	(t (beep) (message (format "Unknown filter state: %s" the-state)))))

;;; Extension of Emacs reader to allow reading of CL pathnames as strings.
(defun cl-read-from-string (the-string)
  (let ((pos 0))
    (while (setq pos (string-match *cl-pathname-prefix-string* the-string pos))
      (setq the-string
	    (concat (substring the-string 0 pos)
		    (substring the-string (+ (match-beginning 0) 2)))))
    (read the-string)))

;;; Pop up help text in the *CL Help* buffer.  User can hit space to
;;; bury.  If you have only one window, it will be split, and the new
;;; window will be shring-wrapped around the help string.  If you
;;; don't bury it with by hitting space, it will be un-shrink-wrapped,
;;; so as not to leave around windows of annoying sizes.
(defun cl-pop-up-help (text)
  (if (< (length text) (screen-width))
      (message (cl-shrink-whitespace text))
      (let ((orig-window (selected-window))
	    (window-config (current-window-configuration))
	    (one-p (one-window-p t))
	    (help-buffer (get-buffer-create "*CL Help*"))
	    (pop-up-windows t)
	    char string)
	(save-excursion
	  (pop-to-buffer help-buffer)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert text)
	  (goto-char (point-min))
	  (if one-p (shrink-window-if-larger-than-buffer (selected-window)))
	  (set-buffer-modified-p nil)
	  (setq buffer-read-only t)
	  (select-window orig-window)
	  (message (substitute-command-keys
		    "<<< Press Space to bury *CL Help* buffer now (\\[cl-bury-help] later) >>>")))
	(if (= (setq char (read-char)) ?\ )
	    (progn (message "")
		   (set-window-configuration window-config)
		   ;; explicitly bury help buffer, AFTER doing set-configuration, since
		   ;; it may have been showing in the original window config.
		   (cl-bury-help))
	    (progn (message "")
		   (set-window-configuration window-config)
		   (display-buffer help-buffer) ;re-display it, not shrunken
		   (setq unread-command-char char)
		   (setq last-command-char char)
		   (setq last-input-char char)
		   (setq string (read-key-sequence nil))
		   (call-interactively (key-binding string)))))))

(defun cl-bury-help ()
  (interactive)
  (let ((help-buffer (get-buffer "*CL Help*"))) 
    (if (null help-buffer)
	nil
      (replace-buffer-in-windows help-buffer)
      (bury-buffer help-buffer))))

(defun cl-shrink-whitespace (the-string)
  "Replace each whitespace substring of THE-STRING by a single space."
  (let ((start 0))
    (while (setq start (string-match "[ \t\n]+" the-string start))
      (setq the-string
	    (concat (substring the-string 0 start)
		    " "
		    (substring the-string (match-end 0) (length the-string))))
      (setq start (+ 1 start)))
    the-string))

;;; The function is passed strings which are output from the CL
;;; process.  All strings are written into the CL shell buffer, and
;;; then cl-parse-output is called to remove prompts and determine
;;; whether the string is normal output or needs to be handled in a
;;; special manner.  In general, the current buffer and point are
;;; preserved, except that if any part of the string is real output
;;; from CL and *cl-pop-up* is non-nil, the CL shell buffer will be
;;; popped up.  
(defun cl-output-filter (proc the-string)
  (let* ((buf (current-buffer))
	 (buf-point (point-marker))	;necessary if buf = cl-buf!
	 (cl-buf (process-buffer proc))
	 (process-mark (process-mark proc))
	 start-point			;a temporary variable
	 cl-output-p cl-terminator)
    (set-buffer cl-buf)			;don't pop up CL buffer yet.
    (goto-char process-mark)
    (setq start-point (point))
    (insert-before-markers the-string) ;move process-mark forward
    (goto-char start-point)		 ;can't use save-excursion here!
    (while (< (point) (marker-position process-mark))
      (if (setq cl-terminator (cl-parse-output process-mark))
	  (cond ((null cl-filter-state)	;end of real lisp output segment
		 (if (or cl-shell-initialized-p (eq cl-terminator 'error))
		     nil
		   (cl-send-startup-commands))
		 (setq cl-output-p t))
		(t (let ((the-state cl-filter-state)
			 (the-string cl-filter-string))
		     (setq cl-filter-state nil)
		     (let ((cl-buff-point (point)))
		       (set-buffer buf)
		       (goto-char buf-point)
		       (cl-handle-special-stream the-state the-string)
		       (set-buffer buf)
		       (set-marker buf-point (point))
		       (set-buffer cl-buf)
		       (goto-char cl-buff-point)))))))
    (set-buffer buf)			;reset point in original buffer
    (goto-char buf-point)
    (set-buffer (window-buffer))	;set buffer to be one in selected window
    (if (and cl-output-p		;pop up if output, according to *cl-pop-up*
	     (null (get-buffer-window cl-buf))		
	     (or (eq *cl-pop-up* t)
		 (eq *cl-pop-up* 'output)
		 (and (eq *cl-pop-up* 'error) (eq cl-terminator 'error))))
	(display-buffer cl-buf))))	;show it, but don't select it

;;; Parse the buffer starting at the point and going no further than
;;; end-mark.  Clean up prompts and look for special purpose streams.
;;; Set buffer variables cl-filter-state and cl-filter-string.  Leave
;;; point at end of parsed material, and return type of terminator.
;;; This is t for a normal prompt, 'error if error prompt, and state
;;; if state-close has been reached (indicating end-of-segment) or
;;; unterminated.
(defun cl-parse-output (end-mark)
  (let ((start-point (point)))		;start of inserted string
    (if (null cl-filter-state)	;not in the middle of parsing special stream
	(if (and (re-search-forward (format "\\(%s\\|%s\\|%s\\|%s\\)"
					    cl-filter-state-open
					    *cl-prompt* *cl-error-prompt*
					    cl-filter-state-close)
				    end-mark 'move)
		 (goto-char (match-beginning 0)))
	    (cond ((looking-at *cl-prompt*)
		   (if (null cl-replacement-prompt)
		       (goto-char (match-end 0))
		     (replace-match "")
		     (insert-before-markers cl-replacement-prompt))
		   t)			;end-of-segment
		  ((looking-at *cl-error-prompt*)  (goto-char (match-end 0)) 'error)
		  ((looking-at cl-filter-state-open)
		   (setq cl-filter-state (cl-grab-state-id))
		   (setq cl-filter-string "")
		   nil)			;not end-of-segment
		  ((looking-at cl-filter-state-close)
		   (cond ((null (re-search-backward cl-filter-state-open
						    (- (point) 1000) t))
			  (message "Unmatched closing SPECIAL TAG")
			  (setq cl-filter-state (cl-grab-state-id))
			  (setq cl-filter-string (cl-grab-region start-point (point))))
			 (t		;set up to parse it on next pass...
			  (setq cl-filter-state (cl-grab-state-id))
			  (setq cl-filter-string "")
			  nil)))))	;not end-of-segment
	(cond ((re-search-forward cl-filter-state-close end-mark t) 
	       (if (not (string= cl-filter-state (cl-grab-state-id)))
		   (message "Non-matching closing stream marker: %s."
			    cl-filter-state))
	       (setq cl-filter-string 
		     (concat cl-filter-string (cl-grab-region start-point (point))))
	       t)			;end-of-segment
	      ((re-search-forward (format "\n%s" *cl-error-prompt*) end-mark t)
	       (setq cl-filter-string
		     (concat cl-filter-string
			     (cl-grab-region start-point (match-beginning 0))))
	       'error)
	      ((re-search-forward *cl-prompt* end-mark t)
	       (let ((the-string (cl-grab-region start-point (point)))
		     (extra-prompt-re	;find optional T and prompt
		      (format "[ \t\n]*\\(t\\|T\\)?[ \t\n]*\\(%s\\|%s\\)"
			      *cl-prompt* *cl-error-prompt*)))
		 (setq the-string (concat cl-filter-string the-string))
		 (setq cl-filter-string
		       (substring the-string 0
				  (string-match extra-prompt-re the-string))))
	       t) 			;end of segment
	      ((re-search-forward cl-filter-state-open end-mark t)
	       (message "Attempted nested SPECIAL STREAMS")
	       (cl-grab-state-id)	;remove marker
	       (setq cl-filter-string 
		     (concat cl-filter-string (cl-grab-region start-point (point))))
	       nil)			;not end-of-segment
	      (t (goto-char end-mark)
		 (setq cl-filter-string 
		       (concat cl-filter-string (cl-grab-region start-point (point))))
		 nil)))))		;not end-of-segment

;;; Variables containing regular expressions which flag the beginning 
;;; and end of special purpose input to the lisp process.
;;; Syntax is [[input-identifier>>input-string<<input-identifier]]
;;; The close marker is not necessary if CL will print a prompt at the end
;;; (ie if CL is at top-level).
(defconst cl-filter-state-open "\\[\\[[A-Z---]+>>")
(defconst cl-filter-state-close "<<[A-Z---]+\\]\\]")

;;; Assume the last re-search matched a state-id, return the string identifying
;;; the state, and erase the match from the buffer.
(defun cl-grab-state-id ()
  (let ((the-state-id (buffer-substring (+ (match-beginning 0) 2)
					(- (match-end 0) 2))))
    (replace-match "")
    the-state-id))

;;; Delete region, returning as a string
(defun cl-grab-region (start end)
  (let ((the-string (buffer-substring start end)))
    (delete-region start end)
    the-string))

;;; Add shell history mechanism.
(load "shell-history")
