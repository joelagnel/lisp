;;; include.el: Mic's (include FILE) byte-compiling form.
;; Tested with GNU Emacs 19.29, byte-compile-version "FSF 2.10"
;; Time-stamp: <96/08/14 20:50:41 mic>

(provide 'include)

;; Description:
;;
;; This file provides the (include FILE) form:
;;
;; Interpreted:              does (load FILE) [with "Loading" message]
;; Compiled at top-level:    inserts FILE.el in the compiler input
;; Compiled, not top-level:  compiles as (load FILE), with a warning
;;
;; Suppose a large emacs lisp package (gnus, vm, w3, .emacs, etc)
;; consists of several smaller el files that are always loaded
;; together.  Then it saves space and time to compile and load those
;; source files as a single elc file.  Of course if some parts are
;; only conditionally needed, then it still makes sense to compile and
;; load those parts as separate elc files.
;;
;; Also, we redefine locate-library (from lisp/help.el).
;; This file may be loaded before bytecomp.el.  If this were a
;; standard feature, parts would belong in byte-run.el.


;;; Copyright (C) 1995  Michelangelo Grigni
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.


;;; Installation:
;;
;; Suppose file foo.el has the top-level form (include "foo-bar").
;; Assuming you will not need the include feature when foo.elc is
;; actually running, put the following line near the top of foo.el:
;;
;; (eval-when-compile (require 'include))
;;
;; To use include all the time, put the following in your ~/.emacs:
;;
;; (require 'include)
;;
;; Or, if your ~/.emacs is compiled and and you want to include *this*
;; file when compiling it, use these lines:
;;
;; (fset 'include 'load)
;; (include "include")
;;
;; The first fset line is not really needed, but it will prevent some
;; trouble if you ever need to load the uncompiled source.


;;; Interpreted Form:

;;;###autoload
(defun include (file)
  ;; NOTE: this should *not* be a defsubst.  It effectively behaves
  ;; like a defsubst anyway, because of byte-compile-include (thus
  ;; compiled output do not refer to include).  If it were a defsubst,
  ;; then byte-compile-include would never be called (this a dubious
  ;; choice made by the byte-compiler), and so we would not see
  ;; warnings about (include) forms at non-top-level.
  "Equivalent to (load FILE).
When seen at top level, the byte-compiler substitutes FILE.el in its
input stream. Otherwise this is (load FILE).  Note a \"Loading ...\"
message warns you that the byte-compiler inclusion did not happen."
  (load file))
;; NOT a defsubst: (put 'include 'byte-optimizer nil)


;;; include-dependencies:
;;
;; Record file dependencies during compilation and loading.  Then when
;; we save file2.el, we may offer to save its includer file1.el.  The
;; command parse-save-compile-buffer (lisp-mic.el) implements this.
;;
;; BUG: `include-dependencies' records absolute filenames, so the elc
;; info is incorrect on another filesystem.  Maybe better to use the
;; un-expanded file arguments to byte-compile-file and include.

(defvar include-dependencies nil
  "List of (INCLUDED . PARENT) pairs, built up by include.
A pair asserts that PARENT was compiled with INCLUDED source.
This list is modified both during compilation and loading.")

;; Save between emacs sessions (if you use desktop):
(add-hook 'desktop-globals-to-save 'include-dependencies)

(defsubst include-dependencies-update (included parent)
  ;; This is a defsubst, so that include.el does not need to be loaded
  ;; to load a compiled inclusion.
  "Update `include-dependencies'.
Arguments are the INCLUDED and PARENT filenames."
  ;; (defvar include-dependencies nil)
  (let ((pair (cons included parent)))
    (or (member pair include-dependencies)
	(setq include-dependencies (cons pair include-dependencies)))))

(defsubst include-dependencies-remove (parent)
  "Remove all `include-dependencies' with cdr PARENT."
  (defvar include-dependencies nil)
  (let (item)
    (while (setq item (rassoc parent include-dependencies))
      (setq include-dependencies (delete item include-dependencies)))))


;;; Compiled Form, not Top-Level:

;; This is not seen if include is a defsubst or a defmacro.
(put 'include 'byte-compile
     (defun byte-compile-include (form)
       (byte-compile-warn "%S not at top-level: treating as load" form)
       ;; Simulate a defsubst or macro:
       (byte-compile-normal-call (cons 'load (cdr form)))))


;;; Compiled Form, Top-Level:

(defvar byte-compile-include-warn nil
  "Should byte-compiler log every (include FILE) entry and exit.")

(defconst include-file-stack nil
  "Stack of values of (filename . byte-compile-file-name).")

(put
 'include
 'byte-hunk-handler
 (defun byte-compile-file-form-include (form)
   "Handles top-level `(include FILE)' forms."
   ;; First of all, are we inside byte-compile-from-buffer?
   (if (not (and (boundp 'inbuffer) (eq inbuffer (current-buffer))))
       (progn
	 (byte-compile-warn
	  "%S outside byte-compile-from-buffer: treating as load" form)
	 (byte-compile-keep-pending (cons 'load (cdr form))))
     (let ((file (eval (nth 1 form)))	; the new filename
	   length parse topfile first)
       ;; Initialize `include-file-stack', if not done already.
       ;; This stuff ought to move into byte-compile-close-variables.
       (if (assq 'include-file-stack (buffer-local-variables))
	   nil
	 ;; This is the first `include' during this compilation:
	 (set (make-local-variable 'include-file-stack) nil)
	 (setq first t))
       (setq file
	     (or (locate-library
		  file
		  (and (featurep 'jka-compr)
		       '(".el" ".el.gz" ".el.Z")))
		 file)
	     topfile			; the top-level file
	     (if include-file-stack
		 ;; sacrificial cons cells ...
		 (car (car (reverse include-file-stack)))
	       filename)
	     )
       (and (string-match "\\.elc\\'" file)
	    ;; We will signal a file-error if no source exists in same
	    ;; directory as the version that would be loaded:
	    (setq file (substring file 0 -1)))
       ;; Offer-save code swiped from byte-compile-file:
       (let ((b (get-file-buffer file)))
	 (and b (buffer-modified-p b)
	      (y-or-n-p
	       (format "save buffer %s before including? " (buffer-name b)))
	      (save-excursion (set-buffer b) (save-buffer))))
       ;; Update `include-dependencies' now:
       (if first
	   (progn
	     ;; Forget old dependencies during compilation:
	     (include-dependencies-remove filename)
	     ;; Forget old dependencies during load:
	     (byte-compile-file-form
	      (` (include-dependencies-remove (, filename))))))
       (include-dependencies-update file topfile)
       (save-excursion
	 (insert
	  ;; Forms to compile *after* the included file:
	  (format "\n%S\n%S\n%S\n"
		  ;; Honor any eval-after-load hooks.
		  ;; Note: XEmacs 19.12 does not have `after-load-alist'.
		  (` (and (boundp 'after-load-alist)
			  (eval (cons 'progn
				      (cdr (assoc (, (nth 1 form))
						  after-load-alist))))))
		  ;; Update include-dependencies at load time:
		  (` (include-dependencies-update (, file)  (, topfile)))
		  ;; Pop `include-file-stack' later in compilation:
		  (` (eval-when-compile
		       (byte-compile-file-form-include-done '(, form))))
		  )))

       (setq
	length
	;; The next line is the basic trick, it may signal a file-error:
	(nth 1 (insert-file-contents file))
	;; Check for balanced parentheses:
	parse
	(save-excursion (parse-partial-sexp (point) (+ (point) length))))
       (or (and (eq 0 (car parse)) (eq 0 (nth 6 parse)))
	   (error "unbalanced parentheses in file `%s'" file))
       (and byte-compile-include-warn
	    (byte-compile-warn
	     "%s %S" (make-string (1+ (length include-file-stack)) ?\>) form))
       ;; Push state on `include-file-stack', two variable values:
       ;; * `filename' is a file or nil, as in byte-compile-from-buffer.
       ;;   This is used in the messages.
       ;; * `byte-compile-current-file' may be filename, buffer, or nil.
       ;;   This is used in the log, and becomes nil once logged.
       (setq
	include-file-stack
	(cons (cons filename byte-compile-current-file) include-file-stack)
	filename file			; or (abbreviate-file-name file)
	byte-compile-current-file	; no need if already logged
	(if byte-compile-include-warn nil filename)))
     nil)))

(defun byte-compile-file-form-include-done (form)
  ;; Pop state from `include-file-stack':
  (setq
   filename (car (car include-file-stack))
   byte-compile-current-file
   (if (or byte-compile-include-warn byte-compile-current-file)
       (cdr (car include-file-stack))
     ;; Included file was logged, need to relog parent on next warning:
     (or filename "[top level]"))	; log location of further errors
   include-file-stack (cdr include-file-stack))
  ;; Log the Exit *after* popping, in the context of the parent filename:
  (and byte-compile-include-warn
       (byte-compile-warn
	"%s %S"	(make-string (1+ (length include-file-stack)) ?\<) form)))

;; For learning about the parser:
;; (defun parse-region (a b)
;;   "Run parse-partial-sexp on region."
;;   (interactive "r")
;;   (message "%S" (save-excursion (parse-partial-sexp a b))))
;; (local-set-key "\C-c\C-p" 'parse-region)


;;; New locate-library:

;; Redefine locate-library of help.el (both include.el and load-wrap.el):
(defun locate-library (library &optional nosuffix)
  ;; Quieter, and accepts list-of-suffixes as nosuffix.
  "Find the full path name of Emacs library LIBRARY.
This command searches the directories in `load-path' like `M-x load-library'
to find the file that `M-x load-library RET LIBRARY RET' would load.
Optional NOSUFFIX set to t means do not try suffixes \".elc\" or \".el\"
on the specified LIBRARY (like `load-file' instead of `load-library')."
  (interactive "sLocate library: ")
  (let ((suffixes (cond
		   ((consp nosuffix) nosuffix)
		   (nosuffix '(""))
		   (t '(".elc" ".el" ""))))
	(noisy (interactive-p)))
    (catch 'answer
      (mapcar
       (lambda (dir)
	 (mapcar
	  (lambda (suf)
	    (let ((try (expand-file-name (concat library suf) dir)))
	      (and (file-readable-p try)
		   (null (file-directory-p try))
		   (progn
		     (and noisy (message "Library is file %s" try))
		     (throw 'answer try)))))
	  suffixes))
       load-path)
      (and noisy (message "No library %s in search path" library))
      nil)))


;;; Todo, End.
;;
;; (debug-on-entry 'byte-compile-include)
;; (debug-on-entry 'byte-compile-file-form-include)
;;
;; #1: make `load-history' reflect the included file?  Maybe bad idea.
;;
;; #2: Rewrite to include and compile at any level.  Sketch: at top
;; level, read the file a form at a time, compiling each form as done
;; in byte-compile-file-form-progn.  Otherwise, read the entire file
;; into a progn form, and compile that.  This would allow things like:
;;
;; (let (protect-this-var) (include "sloppy-package")))
;;
;; (defun foo (x) (include "foo"))
;; (foo x)  ; this loads "foo.elc" (or "foo.el" or "foo")
;; (byte-compile 'foo)
;; (foo x)  ; this uses a compiled definition
;;
;; #3: maintain (listof (master-file included-file)) data structure,
;; to suggest recompilation of master(s) after modifying an included file.

;; Eof.
