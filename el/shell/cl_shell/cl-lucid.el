;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE:          cl-lucid.el
;;; DESCRIPTION:   Extensions to cl-shell.el for Lucid Common Lisp
;;; AUTHOR:        Eero Simoncelli, 
;;;                Vision Science Group, 
;;;                MIT Media Laboratory.
;;; CREATED:       December, 1989
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file contains additional hacks for use with code in
;;; cl-shell.el when using Lucid Common Lisp.  It is loaded
;;; automatically by run-cl if :Lucid is on the *features* lisp in the
;;; Common Lisp environment.  We alter the code from cl-shell.el which
;;; sends things to lisp so that it records the proper source-file for
;;; object/function definitions.  We also provide access to Lucid's
;;; arglist and source-file capabilities.  This is made more useful if
;;; you load the file source-file-extensions.lisp into your CL world.

(require 'cl-shell)
(require 'ehelp)
(provide 'cl-lucid)			;used in cl-pcl and cl-flavors

(define-key lisp-mode-map "\C-c\C-a" 'cl-arglist)
(define-key cl-shell-mode-map "\C-c\C-a" 'cl-arglist)

;;; Don't clobber the usual emacs find-tag command, which is sometimes
;;; useful.  It should, however, be fixed to handle multiple definitions.
(define-key lisp-mode-map "\C-c," 'cl-edit-next-definition)
(define-key cl-shell-mode-map "\C-c," 'cl-edit-next-definition)
(define-key lisp-mode-map "\C-c." 'cl-edit-definition)
(define-key cl-shell-mode-map "\C-c." 'cl-edit-definition)

;;; Define numerical continuation args (C-c <digit>).
(define-key lisp-mode-map     "\C-c0" 'cl-send-digit)
(define-key cl-shell-mode-map "\C-c0" 'cl-send-digit)
(define-key lisp-mode-map     "\C-c1" 'cl-send-digit)
(define-key cl-shell-mode-map "\C-c1" 'cl-send-digit)
(define-key lisp-mode-map     "\C-c2" 'cl-send-digit)
(define-key cl-shell-mode-map "\C-c2" 'cl-send-digit)
(define-key lisp-mode-map     "\C-c3" 'cl-send-digit)
(define-key cl-shell-mode-map "\C-c3" 'cl-send-digit)
(define-key lisp-mode-map     "\C-c4" 'cl-send-digit)
(define-key cl-shell-mode-map "\C-c4" 'cl-send-digit)
(define-key lisp-mode-map     "\C-c5" 'cl-send-digit)
(define-key cl-shell-mode-map "\C-c5" 'cl-send-digit)
(define-key lisp-mode-map     "\C-c6" 'cl-send-digit)
(define-key cl-shell-mode-map "\C-c6" 'cl-send-digit)
(define-key lisp-mode-map     "\C-c7" 'cl-send-digit)
(define-key cl-shell-mode-map "\C-c7" 'cl-send-digit)
(define-key lisp-mode-map     "\C-c8" 'cl-send-digit)
(define-key cl-shell-mode-map "\C-c8" 'cl-send-digit)
(define-key lisp-mode-map     "\C-c9" 'cl-send-digit)
(define-key cl-shell-mode-map "\C-c9" 'cl-send-digit)

;;; Add Lucid binary file extensions to the list of filename
;;; completions to be ignored by Emacs.  
;;; *** Should only do this if these are not already there (pushnew)!
(setq completion-ignored-extensions
      (append '(".sbin" ".sbin3" ".hbin")
	      completion-ignored-extensions))

;;; Add some more special forms to the indentation list. 
;;; These will probably eventually be part of Common Lisp
(put 'loop 'common-lisp-indent-hook 1)

(put 'define-condition 'common-lisp-indent-hook 1)
(put 'handler-bind 'common-lisp-indent-hook 1)
(put 'handler-case 'common-lisp-indent-hook 1)
(put 'restart-bind 'common-lisp-indent-hook 1)
(put 'restart-case 'common-lisp-indent-hook 1)
(put 'with-simple-restart 'common-lisp-indent-hook 1)

;;; ----------------- Modified cl-shell-mode variables ----------------

(setq *cl-prompt* "^> ")

(setq *cl-error-prompt* "^\\(->\\)+ ")

(setq *cl-pathname-prefix-string* "#P")

;;; Used in cl-compile-file.  Set :if-source-only and :if-source-newer
;;; behavior to avoid prompting user when loading a file during a
;;; cl-compile-form!
(setq inferior-lisp-load-command 
      "(load \"%s\" :verbose nil :if-source-only :load-source
                    :if-source-newer :load-source)\n")

;; Used in cl-load-file
(setq cl-load-command "(load \"%s\" :if-source-only :query
                                    :if-source-newer :compile)\n")

;;; Use a quiet compile command.
(setq cl-compile-command
      "(compile-file \"%s\" :messages nil :file-messages nil)\n")

(defun cl-abort () (interactive) (cl-send-string ":a\n"))

(defun cl-backtrace () (interactive) (cl-send-string ":b\n"))

(defun cl-send-digit ()
  (interactive)
  (cl-send-string (concat (char-to-string last-input-char) "\n")))

;;; ------------------ Arglists  ------------------

;;; Ask Lucid for an arglist.  Pretty-pring it in package of symbol.
;;; Must be careful not to generate an error in here!  *** SHould this
;;; print the symbol too?  *** could use momentary-string-display to
;;; put it in the buffer ...
(defun cl-arglist (symbol)
  (interactive (cl-get-function-name "Arglist of CL function: "))
  (cl-send-request cl-help-stream-id (cl-get-buffer-package)
    (format "(if (and (symbolp %s) (fboundp %s))
               (let ((*package* (symbol-package %s)))
                (declare (special *package*))
                (write (lucid::arglist %s) :pretty t :level nil :length nil))
              (format t \"%s does not seem to be a symbol with a function binding.\"))"
	   symbol symbol symbol symbol symbol)))

;;; *** Could use the Emacs function function-called-at-point in help.el
(defun cl-get-function-name (&optional prompt)
  (let (f-name)
    (save-excursion
      (condition-case ()		;catch bad sexp errors
	  (progn
	    (cond ((= (following-char) ?\() nil) ;leave point where it is.
		  (t (backward-up-list 1)))
	    (if (looking-at "(+") (goto-char (match-end 0)))
	    (setq f-name (buffer-substring
			  (progn (skip-chars-forward " \t\n") (point))
			  (progn (forward-sexp 1) (point))))
	    (if (or (string= f-name "apply")
		    (string= f-name "funcall")
		    (string= (substring f-name 0 3) "def") ;defun, defmacro, etc
		    (string= (substring f-name 0 3) "map")) ;mapcar, mapcan, etc
		(setq f-name (buffer-substring
			      (progn (skip-chars-forward " \t\n") (point))
			      (progn (forward-sexp 1) (point))))))
	(error nil)))
    (setq f-name (read-no-blanks-input (or prompt "Function name: ") (or f-name "")))
    ;; (if (null f-name) (setq f-name (read-string (or prompt "Function name: "))))
    (if (string= f-name "") (error "No function name specified."))
    (list (cl-add-quote f-name))))

;;; This comes from help.el:
(defun function-called-at-point ()
  (condition-case ()
      (save-excursion
	(save-restriction
	  (narrow-to-region (max (point-min) (- (point) 1000)) (point-max))
	  (backward-up-list 1)
	  (forward-char 1)
	  (let (obj)
	    (setq obj (read (current-buffer)))
	    (and (symbolp obj) (fboundp obj) obj))))
    (error nil)))

;;; *** Could also modify cl-documentation to print the arglist first.

;;; --------------------- Edit definition --------------------

;;; *** This code is slightly broken because of CL package prefixing.
;;; It is a pain to make Emacs lisp deal with this - future versions
;;; will attempt to fix the problems...

;;; Ask Lucid to get the source files and pass it back to Emacs as a
;;; list argument to cl-edit-multiple-definitions.  Note that this
;;; requires no special CL code - Lucid's get-source-file returns the
;;; desired list.  New types of object/function can be used by calling
;;; record-source-file with a type argument which is a list.  Several
;;; such extensions have been provided in the file
;;; source-file-extensions.lisp.  The first element of the list will
;;; be used to by Emacs index into the *cl-definition-regexp-alist* to
;;; get a regexp to search for the definition.  *** Should this do an
;;; apropos on symbol? (like the Emacs find-tag).
(defun cl-edit-definition (symbol)
  (interactive (cl-get-function-name "Goto CL definition of: "))
  (cl-send-request cl-eval-stream-id (cl-get-buffer-package)
      (format "(let ((*print-length* nil)
                     (*print-level* nil))
                 (format lisp:*standard-output*
                         \"(cl-edit-multiple-definitions \\\"%s\\\"  '~S)\"
                         (if (fboundp %s)
                             (lucid::get-source-file %s nil t)
                             :unbound)))"
	      (substring symbol 1)	;get rid of quote
	      symbol symbol)))

;;; *** Warning: the symbols used as keys for the alist are
;;; case-sensitive.  The extended definition type :STRUCT-FUNCTION is
;;; defined in source-file-extensions.lisp.  It allows Emacs to go to
;;; the defstruct when you ask to edit the definition of an accessor,
;;; constructor, etc.
(setq *cl-definition-regexp-alist*
      (append *cl-definition-regexp-alist*
	      '((FUNCTION . "(defun[ \t\n]*%s")
		(VARIABLE . "(def\\(var\\|parameter\\|constant\\)[ \t\n]*%s")
		(STRUCTURE . "(defstruct[ \t\n]*(?%s")
		(TYPE  . "(deftype[ \t\n]*%s")
		(:STRUCT-FUNCTION . cl-make-defstruct-regexp))))

;;; For functions defined by side-effect of a defstruct, search for
;;; the defstruct instead of a function definition!
(defun cl-make-defstruct-regexp (symbol type-spec)
  (format "(defstruct[ \t\n]*(?%s" (cl-strip-package (car (cdr type-spec)))))

(defun cl-definition-regexp (symbol type-spec)
  (setq symbol (cl-strip-package symbol))
  (let* ((type (if (listp type-spec) (car type-spec) type-spec))
	 (regexp-or-func (cdr (assoc type *cl-definition-regexp-alist*))))
    (cond ((null regexp-or-func)	;default regexp
	   (format "(def[^ \t\n]*[ \t\n]*(?%s" symbol))
	  ((stringp regexp-or-func)
	   (format regexp-or-func symbol))
	  ((symbolp regexp-or-func)
	   (funcall regexp-or-func symbol type-spec)))))

;;; This is called by CL with the symbol, and a list containing
;;; type-spec/filename pairs.  A type-spec is either a symbol like
;;; 'function or 'variable, or a list like '(:struct-function foo).
;;; See the alist *cl-definition-regexp-alist*.  If there are many
;;; source files, we allow the user to choose which definition to
;;; edit.
(defun cl-edit-multiple-definitions (symbol type-spec-and-file-list)
  (cond ((eq type-spec-and-file-list ':UNBOUND)
	 (message "Symbol %s has no function binding." symbol))
	((or (null type-spec-and-file-list) (eq type-spec-and-file-list 'NIL))
	 (message "No source file recorded for %s" symbol))
	((= (length type-spec-and-file-list) 1)
	 (cl-goto-definition symbol (car type-spec-and-file-list)))
	(t (let ((user-choice (cl-choose-definition symbol type-spec-and-file-list)))
	     (cond ((numberp user-choice)  ;user chose a definition to edit.
		    (setq cl-edit-next-definition-form
			  (list 'cl-edit-multiple-definitions
				symbol type-spec-and-file-list))
		    (setq user-choice
			  (max 0 (min (1- (length type-spec-and-file-list))
				      user-choice)))
		    (cl-goto-definition
		     symbol (nth user-choice type-spec-and-file-list)))
		   ((eq user-choice 'edit-all) ;user wants to cycle through all
		    (cl-cycle-through-definitions symbol type-spec-and-file-list))
		   (t (message "Cancelled.")))))))

;;; If user hits return, we just cycle through the definitions like
;;; the usual next-tag function.
(defun cl-cycle-through-definitions (symbol type-spec-and-file-list)
  (let ((type-spec-and-file (car type-spec-and-file-list))
	(rest-of-list (cdr type-spec-and-file-list)))
    (setq cl-edit-next-definition-form
	  (if rest-of-list
	      (list 'cl-cycle-through-definitions
		    symbol rest-of-list)
	      nil))
    (cl-goto-definition symbol type-spec-and-file)))

;;; Provide both standard and "electric" cursor movement keybindings.
(defvar cl-choose-definition-map
  (let ((map (make-keymap)))
    (fillarray map 'cl-choose-definition-undefined)
    ;;(suppress-keymap map) ;supress modifying keystrokes
    (define-key map (char-to-string meta-prefix-char) (copy-keymap map))
    (define-key map "\C-n" 'next-line)
    (define-key map "\C-p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "\C-v" 'scroll-up)
    (define-key map "\M-v" 'scroll-down)
    (define-key map "d" 'scroll-up)
    (define-key map "u" 'scroll-down)
    (define-key map "\M-<" 'beginning-of-buffer)
    (define-key map "\M->" 'end-of-buffer)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)
    (define-key map "e" '(lambda () (interactive)
			  (throw 'exit (1- (count-lines 1 (point))))))
    (define-key map "a" '(lambda () (interactive) (throw 'exit 'edit-all)))
    (define-key map "\C-m" '(lambda () (interactive) (throw 'exit 'edit-all)))
    (define-key map "q" '(lambda () (interactive) (throw 'exit 'quit)))
    (define-key map "Q" '(lambda () (interactive) (throw 'exit 'quit)))
    (define-key map "\C-g" '(lambda () (interactive) (throw 'exit 'quit)))    
    map))

(defvar cl-choose-definition-error-message
  "e to edit this def, <CR> or a to edit all sequentially, q to quit: ")

(defun cl-choose-definition-undefined ()
  (interactive)
  (message cl-choose-definition-error-message)
  (beep))
(put 'cl-choose-definition-undefined 'suppress-keymap t)

;;; This asks the user to select a definition to edit.  Returns a
;;; number, or 'edit-all to indicate users choice.  Anything else is
;;; interpreted as a cancellation.  *** BUG: screws up current-buffer.
(defun cl-choose-definition (symbol type-spec-and-file-list)
  (save-window-excursion
    (let ((buf (get-buffer-create "*CL definitions*"))
	  (first-spec-and-file))
      (pop-to-buffer buf)
      (use-local-map cl-choose-definition-map)
      (erase-buffer)
      (insert (format "Select a definition of %s to edit:\n" symbol))
      (while type-spec-and-file-list
	(setq first-spec-and-file (car type-spec-and-file-list))
	(setq type-spec-and-file-list (cdr type-spec-and-file-list))
	(insert (format "%s\n" first-spec-and-file)))
      (goto-line 2)			;put point on first definition
      (catch 'exit
	(unwind-protect
	     (Electric-command-loop
	      'exit
	      cl-choose-definition-error-message)
	  (message "")		;get rid of minibuffer prompt
	  (condition-case ()	;make sure user can get rid of this buffer!
	      (funcall (or default-major-mode 'fundamental-mode))
	    (error nil))
	  (bury-buffer buf))))))

;;; Holds the form to be evaluated on a call to cl-edit-next-definition.
(defvar cl-edit-next-definition-form nil)

(defun cl-edit-next-definition ()
  (interactive)
  (if (null cl-edit-next-definition-form)
      (message "No more definitions to edit.")
      (apply (car cl-edit-next-definition-form)
	     (cdr cl-edit-next-definition-form))))

;;; Load the file into Emacs and goto the definition containing
;;; symbol.  type-and-file-spec should be an element of the list
;;; returned by CL from a call to get-source-file.
(defun cl-goto-definition (symbol type-spec-and-file)
  (let ((type-spec (car type-spec-and-file))
	(filename (cdr type-spec-and-file)))
    (cond ((null filename)		;if defined at top-level.
	   (message "%s definition for %s has no source file."
		    type-spec symbol))
	  ((null (file-readable-p filename))
	   (message "Cannot open source file %s" filename))
	  (t
	   (let* ((buf (find-file-noselect (expand-file-name filename)))
		  regexp)
	     (if (get-buffer-window buf) ;already showing?
		 (set-buffer buf)
		 (pop-to-buffer buf))
	     (goto-char (point-min))
	     (setq regexp (cl-definition-regexp symbol type-spec))
	     (if (re-search-forward regexp nil t)
		 (goto-char (match-beginning 0))
		 (message "Can't find a %s definition for %s." type-spec symbol))
	     (select-window (get-buffer-window buf)))))))

;;; -------------------- Modified lisp-mode buffer commands --------------------

;;; We modify these to record the source file of functions correctly.
;;; This is done by rebinding the variable lucid::*source-pathname*.
;;; *** This is a bit gross.  We should come up with a clever macro to
;;; do this...

;(defun cl-eval-form ()
;  "Send the current top-level sexp to the CL process created by
;M-x run-cl, moving to end of sexp.  If *cl-echo-commands* is non-nil,
;echo the sexp into cl-shell buffer."
;  (interactive)
;  (end-of-defun)			;move to end of defun
;  (let* ((the-string 
;	  (save-excursion		;leave point at end of defun
;	    (buffer-substring (progn (beginning-of-defun) (point))
;			      (progn (forward-sexp 1) (point)))))
;	 (full-string
;	  (concat "(let ((lucid::*source-pathname* \""  buffer-file-name  "\"))\n"
;		  the-string
;		  ")\n")))
;    (if *cl-echo-commands*
;	(cl-send-string-with-echo full-string the-string)
;	(cl-send-string (concat full-string "\n")))))

(defun cl-eval-region ()
  "Send region between point and mark to CL process, without echoing."
  (interactive)
  ;; check that expressions are complete.  Take overhanging ones.
  (let ((start (min (point) (mark)))
	(end (max (point) (mark))))
    (save-excursion
      (goto-char start)
      (setq end (progn
		  (while (and (< (point) end)
			      (scan-sexps (point) 1))
		    (goto-char (scan-sexps (point) 1))
		    (skip-chars-forward " \t\n" end))
		  (point)))
      (cl-send-string
       (concat "(let ((lucid::*source-pathname* (truename \""
	       (expand-file-name buffer-file-name)
	       "\")))\n"))
      (cl-send-region start end)
      (cl-send-string "\n(values))\n")))) ;send final newline

(defun cl-compile-form ()
  "Send the current top-level sexp to the CL process created by M-x
run-cl, and compile it in the package of the current buffer.  The
point is moved to the end of the sexp, and if *cl-echo-commands* is
non-nil a shorthand expression is echoed to the *lisp* buffer."
  (interactive)
  (or (cl-process) (error "CL process is not running!"))
  (end-of-defun)			;move to end of sexp
  (let ((cl-package (cl-get-buffer-package))
	(source-file-name (expand-file-name buffer-file-name))
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
    (setq the-string
	  (concat "(let ((lucid::*source-pathname* (truename \""
		  source-file-name
		  "\")))\n"
		  the-string
		  ")"))
    (if *cl-echo-commands*
	(cl-send-string-with-echo
	 the-string
	 (concat "(compile-def '" fn-name " :pkg " cl-package ")")
	 t)				;no history recording
	(cl-send-string (concat the-string "\n")))))

