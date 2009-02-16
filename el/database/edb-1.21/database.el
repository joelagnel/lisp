;;; database.el --- EDB, the Emacs database; replaces forms editing modes

;; Copyright (C) 1991-1995 Michael D. Ernst <mernst@theory.lcs.mit.edu>

;; Author: Michael Ernst <mernst@theory.lcs.mit.edu>
;; Keywords: EDB, database, forms
;; Version: 1.21
;; Release-Date: Jul 18 1995

;;; Commentary:

;; EDB is a flexible, customizable database program for Emacs.
;; See the texinfo documentation database.texi for complete installation
;; and usage instructions for EDB, the Emacs database.  The README file
;; also contains installation instructions.

;; LCD Archive Entry:
;; edb|Michael Ernst|mernst@theory.lcs.mit.edu
;; |Customizable database program for Emacs; replaces forms editing modes
;; |Jul 18 1995|1.21|~/packages/edb.tar.Z|

;; When changing these, change the LCD Archive Entry and header too.
(defconst edb-version "1.21")
(defconst edb-date "Jul 18 1995")	; release date

;; EDB is distributed under the same conditions as GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY.  No author or distributor accepts responsibility to anyone
;; for the consequences of using it or for whether it serves any particular
;; purpose or works at all, unless he says so in writing.  Refer to the GNU
;; Emacs General Public License for full details.

;; Everyone is granted permission to copy, modify and redistribute GNU
;; Emacs, but only under the conditions described in the GNU Emacs General
;; Public License.  A copy of this license is supposed to have been given
;; to you along with GNU Emacs so you can know your rights and
;; responsibilities.  It should be in a file named COPYING.  If not, write
;; to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;; 02139, USA for a copy.  Among other things, the copyright notice and
;; this notice must be preserved on all copies.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global variables
;;;

(defvar db-databases nil
  "Assoc list of database names and databases.")

;; Alist of typenames and displayspecs.
(defvar db-displaytypes nil)

(defvar db-recordfieldtypes nil
  "Alist of typenames and recordfieldspecs.")

(defvar db-inform-interval 10
  "When doing a lengthy computation, inform the user of progress every this
many records.  If nil, don't inform.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database messages
;;;

;; T if logging has been done recently (since the user was last shown the log).
(defvar db-logged nil)

;; Postpend STRING to buffer *Database-Log*.
(defun db-log (string)
  (in-buffer "*Database-Log*"
    (save-excursion
      (goto-char (point-max))
      (insert string "\n")))
  (setq db-logged t))

;; Format message, display it, and log it in buffer *Database-Log*.
(defun db-message (format-string &rest args)
  (let ((formatted (apply 'format format-string args)))
    (db-log formatted)
    (db-best-fit-message formatted)))

;; Like `db-message', but prepends \"Warning: \".
(defmacro db-warning (format-string &rest args)
  (` (db-message (concat "Warning: " (, format-string)) (,@ args))))
(fset 'db-warn 'db-warning)


;;;
;;; Debugging messages
;;;

(defvar db-disable-debugging-support t
  "If non-nil, then debugging calls will be compiled out of the source and the
variable `db-debug-p' will have no effect.  Setting this variable at run-time
has no effect if you are running EDB compiled; you must set it when you compile
EDB, or run EDB interpreted.  Defaults to t.")

(defvar db-debug-p nil
  "*Non-nil if database debugging is enabled.  Defaults to nil.
Has no effect on code compiled with `db-disable-debugging-support' set.")

(defmacro db-debug (&rest body)
  "Execute BODY if `db-debug-p' is non-nil.
See also variable `db-disable-debugging-support'."
  (if (and (boundp 'db-disable-debugging-support)
	   (not db-disable-debugging-support))
      (` (if db-debug-p
	     (progn
	       (,@ body))))))
(put 'db-debug 'edebug-form-spec '(&rest form))

(defmacro db-debug-log (string)
  (` (db-debug (db-log (, string)))))
;; (defun db-debug-log (string)
;;   (db-debug (db-log string)))

(defmacro db-debug-message (format-string &rest args)
  (` (db-debug (db-message (, format-string) (,@ args)))))
(put 'db-debug-message 'edebug-form-spec '(&rest form))
;; (defun db-debug-message (format-string &rest args)
;;   (db-debug-log (apply 'format format-string args)))

;;; Debugging proper

(defun db-prepare-to-debug ()
  "Prepare to debug EDB.
Set variables `debug-on-error', `db-disable-debugging-support', and `db-debug-p'.
Also load uncompiled EDB source."
  (interactive)
  (setq debug-on-error t
	db-disable-debugging-support nil
	db-debug-p t)
  (load-database 'uncompiled))

(defun edb-version ()
  "Return a string describing the version of EDB that is running."
  (interactive)
  (let ((version-info (format "EDB %s of %s" edb-version edb-date)))
    (if (interactive-p)
	(message version-info)
      version-info)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Loading
;;;

(defvar edb-directory nil
  "A string, the name of the directory containing the EDB source files.")

(defvar db-running-lucid-emacs (string-match "Lucid" emacs-version))

;; How to add a file to EDB:
;; * add its name to one of these lists
;; * add autoloads below, if necessary
;; * add a one-line description to the README file
;; * add it to the edbtoftp script

;;; Files comprising EDB.
;; List of EDB source files loaded by `load', sans extensions.
(defconst edb-essential-file-names
  (append '("db-rep" "db-format" "db-file-io" "db-interfa"
	    "db-types" "db-time")))
;; List of EDB source files loaded by `require', sans extensions.
(defconst edb-required-file-names
  '("db-util"))
;; List of EDB source files loaded by `autoload', sans extensions.
(defconst edb-autoloaded-file-names
  '("db-convert" "db-isbn" "db-rdb"
    "db-search" "db-sort" "db-summary"
    "db-tagged" "db-two-dbs"))
;; List of all EDB source files, sans extensions.
;; Does not include \"database\", the top-level file.
(defconst edb-file-names
  (append edb-required-file-names
	  edb-essential-file-names
	  edb-autoloaded-file-names))

(defconst edb-source-file-names
  (mapcar (function (lambda (file-name) (concat file-name ".el")))
	  edb-file-names))

;;;###autoload
(defun load-database (&optional uncompiled)
  "Load all the files of EDB, the Emacs database.
With prefix argument, load source, not compiled, code; run EDB interpreted."
  (interactive "P")
  (if (not (featurep 'db-util))
      (error "How could load-database be defined without db-util being required?"))
  (let* ((edb-dir (and edb-directory (expand-file-name edb-directory)))
	 ;; In case compiled files are on load-path, sources are in
	 ;; edb-directory, and edb-directory is not on load-path.
	 (load-path (if (and edb-dir (not (member edb-dir load-path)))
			(cons edb-dir load-path)
		      load-path)))
    (mapcar (function load)
	    (if uncompiled
		edb-source-file-names
	      edb-file-names))))

;; ;; Useful during debugging.
;; (defun db-reset ()
;;   "Reset global database variables."
;;   (interactive)
;;   ;; I need to think about whether this should set any other variables.
;;   ;; If db-databases is set to nil, then any data display or summary buffers
;;   ;; should be killed.
;;   (setq db-databases nil
;; 	db-recordfieldtypes nil
;; 	db-displaytypes nil
;; 	))


;;;###autoload
(defun edb-update (&optional directory)
  "Install the EDB update found in the current buffer after point.
EDB is assumed to be in the directory specified by `edb-directory'.
\(If that variable is not set, the user is prompted for the location of
the files.\)

If you have trouble with this command, it is likely that your version of EDB
is not exactly the same as the last release.  You might have an old
release, or you might have a pre-release."
  (interactive)
  (setq directory (file-name-as-directory
		   (expand-file-name
		    (or directory
			edb-directory
			(read-file-name "What directory contains EDB? "
					nil default-directory t)))))
  (if (not (file-directory-p directory))
      (error "%s is not a directory." directory))

  (let* ((diff-begin (progn (goto-char (point-min))
			   (re-search-forward "^begin 644 ")
			   (match-beginning 0)))
	 (filename (buffer-substring (point)
				     (progn (end-of-line) (point))))
	 (diff-end (progn (goto-char (point-max))
			 (re-search-backward "^end\n")
			 (match-end 0))))
    (write-region diff-begin
		  diff-end
		  (concat directory filename "UUE"))
    (message "uudecoding, uncompressing, and applying patch...")
    (shell-command (concat
		    "cd " directory "; "
		    "uudecode " filename ".UUE; "
		    "zcat " filename " | patch"))
    (message "uudecoding, uncompressing, and applying patch...done"))

  (load-database t)
  ;; This ought to know about dependencies on macros, so that if they
  ;; change, then all the dependent files are recompiled, too.
  (byte-recompile-directory directory)
  ;; Call db-reset here only if you're brave and believe you will never
  ;; have any unsaved changes when you call edb-update.
  (load-database)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autoloads
;;;

;; I must declare some variables to be buffer-local here, so that they can
;; be set differently in different data display buffers even though the
;; packages that use them have not been loaded yet.  I would prefer to
;; have the variables' default values and documentation strings be
;; available as well, but I want the definitions to be near where the
;; variables are used, and I don't want to have to keep two copies
;; up-to-date.
;; Luckily, many of the interactively-called functions are in db-interfa
;; instead of one of the autoloaded files.

;;; db-convert.el
(autoload 'db-convert "db-convert"
	  "Convert DATABASE's field structure.  To be autoloaded." t)

;;; db-rdb.el
(autoload 'db-rdb-setup "db-rdb"
	  "Prepare EDB to read files in RDB format.  To be autoloaded." t)

;;; db-sort.el
(autoload 'database-sort "db-sort"
	  "Sort and return DATABASE, which is also side-effected.  To be autoloaded." t)
(autoload 'database-sort-interface "db-sort")
(make-variable-buffer-local 'dbf-field-priorities)
(make-variable-buffer-local 'dbf-hidden-to-end-p)

;;; db-two-dbs.el
(autoload 'db-process-two-databases "db-two-dbs")
(autoload 'db-merge "db-two-dbs"
	  "Merge two read-in databases.  To be autoloaded." t)
(autoload 'databases-compatible "db-two-dbs")

;;; db-search.el
(autoload 'db-parse-match-pattern "db-search") ; should be called first
(autoload 'db-print-match-pattern "db-search")
(autoload 'db-match "db-search")
(make-variable-buffer-local 'dbf-field-search-defaults)

;;; db-tagged.el
(autoload 'db-tagged-setup "db-tagged"
	  "Prepare EDB to read files in tagged format.  To be autoloaded." t)

;;; db-summary.el
(autoload 'db-summary "db-summary"
	  "Display a summary of all database records.  To be autoloaded." t)
;;   "T if this buffer is a database summary buffer."
(defsubst db-summary-buffer-p ()
  (eq major-mode 'database-summary-mode))
(autoload 'dbf-make-summary-maker "db-summary")
(autoload 'format->lines-and-stringform-list "db-summary")
(make-variable-buffer-local 'dbs-data-display-buffer)
(make-variable-buffer-local 'dbs-index)
(make-variable-buffer-local 'dbs-no-of-records)
(make-variable-buffer-local 'dbs-point)
(make-variable-buffer-local 'dbs-index-fraction)
(make-variable-buffer-local 'dbs-recompute-p)
(make-variable-buffer-local 'dbf-summary-format)
(make-variable-buffer-local 'dbf-summary-function)
(make-variable-buffer-local 'dbf-summary-buffer)
(make-variable-buffer-local 'dbf-summary-show-hidden-records-p)
(make-variable-buffer-local 'dbf-summary-recompute-all-p)
(make-variable-buffer-local 'dbfs-lines)


;; A future version of Emacs will autoload with-electric-help.
;; It's possible that this will make it look like ehelp is already loaded,
;; if some other package checks (fboundp 'with-electric-help).
(if (not (fboundp 'with-electric-help))
    (autoload 'with-electric-help "ehelp"
  "Arguments are THUNK &optional BUFFER NOERASE.  BUFFER defaults to \"*Help*\".
To be autoloaded."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiling
;;;

;;;###autoload
(defun byte-compile-database-all (&optional directory)
  "Compile all source (.el) files in EDB, the Emacs database, unconditionally.
Calls `byte-compile-database'."
  (interactive)
  (byte-compile-database directory t))

;; Cannibalized in part from byte-recompile-directory.
;;;###autoload
(defun byte-compile-database (&optional directory all)
  "Compile source (.el) files in EDB, the Emacs database, which need it.
If optional prefix argument ALL is non-nil, every source file is recompiled.
You are likely to want unconditional recompilation if you have tried to
compile any of the files by hand and want to replace the (possibly incorrect)
results."
  ;; This nonsense is just to get access to current-prefix-arg.
  (interactive (list
		(or edb-directory
		    (read-file-name "What directory contains EDB? "
				    nil default-directory t))
		current-prefix-arg))

  (setq directory (file-name-as-directory
		   (expand-file-name
		    (or directory
			edb-directory
			(read-file-name "What directory contains EDB? "
					nil default-directory t)))))

  (if (not (file-directory-p directory))
      (error "%s is not a directory." directory))

  ;; Load EDB, in source form, to get proper definitions for macros, etc.
  ;; EDB is already loaded if this function is defined, but the source
  ;; might be different than the .elc files (a good reason for compiling),
  ;; or some files might have changed since database.el was loaded.
  ;; Because of this call, all EDB files should be written so as to be
  ;; loadable multiple times, even though in the ordinary course of things
  ;; they will only be loaded once.
  (load-database t)

  (let ((files (cons "database.el" edb-source-file-names))
	(count 0)
	source dest)
    (while files
      (setq source (expand-file-name (car files) directory))
      (setq dest (concat (file-name-sans-versions source) "c"))

      ;; Compile unless a newer .elc file exists.
      (if (or all (not (file-newer-than-file-p dest source)))
	  (progn (byte-compile-file source)
		 (setq count (1+ count))))
      (setq files (cdr files)))
    (message "Done (Total of %d file%s compiled)"
	     count (if (= count 1) "" "s")))

  ;; Hide uninteresting byte compiler warnings.
  (if (get-buffer "*Compile-Log*")
      (in-buffer-simple "*Compile-Log*"
	(save-excursion
	  (goto-char (point-min))
	  ;; (delete-matching-lines "with-electric-help is not known to be defined")
	  ;; (delete-matching-lines "function link-set-record being redefined")
	  ;; (delete-matching-lines "link-set-record defined multiple times")
	  ;; (delete-matching-lines "free variable mode-motion-hook")
	  ;; Lucid support
	  ;; (delete-matching-lines "db-lucid")
	  (delete-matching-lines "make-extent")
	  (delete-matching-lines "map-extent")
	  (delete-matching-lines "delete-extent")
	  (delete-matching-lines "mouse-track")
	  ;; Get rid of references to functions/files that no longer have errors.
	  (goto-char (point-min))
	  (replace-string
	   "  ** The following functions are not known to be defined: \n\n"
	   "\n")
	  (goto-char (point-min))
	  (replace-string "While compiling the end of the data:\n\n" "\n")
	  (goto-char (point-min))
	  (delete-matching-lines "199.\nWhile compiling .*:\n\\(\\'\\|\n\f\\)"))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; One-time setup
;;;

;; For compilation from batch mode.  I should really check that database.el
;; is either in the current directory or on load-path.
(if (and noninteractive (not edb-directory))
    (setq edb-directory default-directory))

;; Make sure edb-directory is in load-path, for people who want EDB in its
;; own directory, but don't want that on load-path until EDB is loaded.
(let* ((edb-dir (and edb-directory (expand-file-name edb-directory))))
  (and edb-dir (not (member edb-dir load-path))
       (setq load-path (cons edb-dir load-path))))

;;; Actually load the database.
;; Perform requires
(require 'cl)
(require 'easymenu)
(condition-case nil
    (require 'db-util)
  (file-error
   (error "Can't find db-util on load-path.  Set `edb-directory' or `load-path'.")))
;; Load EDB files.
(mapcar (function load)	edb-essential-file-names)

(if (not (assoc 'dbc-hide-p minor-mode-alist))
    (setq minor-mode-alist (cons '(dbc-hide-p " Hide") minor-mode-alist)))
(add-hook 'kill-buffer-hook 'db-kill-buffer-hook)

;; At the end of the file in case this load aborts.
(provide 'database)			; provide before running hooks

(defvar db-load-hooks nil
  "Function or list of functions run after loading EDB.
You can use this to load extensions, redefine EDB functions,
customize key bindings, etc.")

(run-hooks 'db-load-hooks)

;;; database.el ends here
