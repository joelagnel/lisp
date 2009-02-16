;;; fff.el --- fast file finder

;; Copyright (C) 1996, 1997, 1999, 2004 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Status: Works in Emacs 19 or later and XEmacs 19.15 or later.
;;         Works in XEmacs 19.14 & earlier with reduced functionality.
;; Keywords: extensions, searching, files, commands, tools
;; Created: 1996-03-26

;; $Id: fff.el,v 1.18 2004/04/28 00:24:09 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides several shortcut commands for visiting or
;; inserting files without having to specify them by their complete name.
;; For example, you can visit programs in your exec-path (some of which may
;; be humanly-readable shell scripts or config files), or anything else
;; which is quickly locatable via a prebuilt database or path list.
;; Completion is also available for many commands.

;; Using a prefix arg will display a menu of all possible matches,
;; and you can select one of them by moving point and pressing return.

;; There are also documented subroutines for common searching operations
;; and basic interactive behavior to minimize any effort required to add
;; additional shortcut commands.

;; To use this package, put the following in your .emacs:
;;
;;   (autoload 'fff-find-file-in-envvar-path                    "fff" nil t)
;;   (autoload 'fff-insert-file-in-envvar-path                  "fff" nil t)
;;   (autoload 'fff-find-file-in-exec-path                      "fff" nil t)
;;   (autoload 'fff-insert-file-in-exec-path                    "fff" nil t)
;;   (autoload 'fff-find-file-in-path                           "fff" nil t)
;;   (autoload 'fff-insert-file-in-path                         "fff" nil t)
;;   (autoload 'fff-find-file-in-locate-db                      "fff" nil t)
;;   (autoload 'fff-insert-file-in-locate-db                    "fff" nil t)
;;
;; The command `fff-install-map' will bind these commands to a common
;; prefix of "C-c C-f" (you can change this).  To find a list of them, run
;; that command and then type "C-c C-f C-h".
;;
;; If you are using Emacs 19 or later, you can have this happen
;; automatically by putting the following in your .emacs:
;;
;;     (eval-after-load "fff" '(fff-install-map))

;; This package is most useful if you have an up-to-date `locate' database
;; (built by the `updatedb' program from the GNU findutils), since that
;; provides a fast way to search for files by name all over the system.
;; Red Hat 5.0 and later update this database automatically.

;; I wrote this package I got sick of typing long file names for things way
;; down in source trees, or having to go look for them first because I
;; didn't even know where they were.

;; Related works:
;;    * filecache.el by Peter Breton <pbreton@i-kinetics.com>.
;;      The major difference seems to be how it works by layering on top of
;;      the normal find-file, insert-file, etc. commands.  Its primitives
;;      for constructing new caches or search paths are different.

;;; Code:


(defvar fff-locate-program "locate"
  "*Name of program to invoke which reads the `locate' database.
This variable is used by the function `fff-locate-files-in-locate-db'.")

(defvar fff-locate-program-args nil
  "*Additional args to the program which searches the `locate' database.
This variable is used by the function `fff-locate-files-in-locate-db'.")

(defvar fff-match-predicate 'fff-file-nondirectory-p
  "*Default matching predicate for commands in this package.
If `nil', no predicate is used; all files match.

This variable only used by interactive commands defined in this package.

Utility functions in this package which take a predicate argument do not
refer to this variable for a default; if no predicate is specified, none is
used and all files match.")

(defvar fff-sorting-predicate nil
  "*Predicate used to sort file names in display menu.
If `nil', no predicate is used; files are presented in the order listed.
This is used by `fff-display-matches'.")

(defvar fff-map-prefix "\C-c\C-f"
  "*Default prefix on which keybindings will go.
If you change this at runtime, you will need to re-invoke `fff-install-map'.")

(defvar fff-map nil
  "*Keymap for FFF commands.
Type ``\\[fff-command-prefix] \\<fff-map>\\[describe-prefix-bindings]'' \
for a list of bindings.")

;; (consp (cdr foo)) is faster than (length foo) for merely
;; determining if the length of a list is > 1.
(defsubst fff-length1-p (l)
  (and (consp l)
       (not (consp (cdr l)))))

;; default obarray size; should be prime for good hashing characteristics
(defvar fff-default-obarray-size 29)


;;; Finding files in lisp-based paths other than load-path.
;;; For load-path based functions, see fff-elisp.el.

;; This might seem like a silly command, but usually there are a lot of
;; shell scripts and config files in exec-path that are humanly readable.
;; Besides, some people actually edit binaries.
;;;###autoload
(defun fff-find-file-in-exec-path (file &optional which)
  "Visit the first file named FILE in `exec-path'.

If called interactively with a generic prefix argument and there
is more than one possible match, a list is displayed from which
the user can select the desired match.  If called from a program
and there is more than one match, an error is signalled.

If called interactively with a numeric prefix argument N and
there are at least N many matches, the Nth file will be visited.

If no matches are found, an error is signalled."
  (interactive (list (fff-completing-read-file-in-path
                      "Find file (fff exec-path): " exec-path)
                     current-prefix-arg))
  (fff-<op>-file-in-path file 'exec-path which fff-match-predicate
                         'find-file (interactive-p)))

;;;###autoload
(defun fff-insert-file-in-exec-path (file &optional which)
  "Insert the file named FILE found in `exec-path' into current buffer.
This function behaves exactly like `fff-find-file-in-exec-path', except
that the contents of the file is inserted in the current buffer instead of
being visited in another buffer."
  (interactive (list (fff-completing-read-file-in-path
                      "Insert file (fff exec-path): " exec-path)
                     current-prefix-arg))
  (fff-<op>-file-in-path file 'exec-path which fff-match-predicate
                         'insert-file (interactive-p)))

;;;###autoload
(defun fff-find-file-in-envvar-path (file envvar &optional which)
  "Visit the file named FILE in path specified by ENVIRONMENT variable.

If called interactively with a generic prefix argument and there
is more than one possible match, a list is displayed from which
the user can select the desired match.  If called from a program
and there is more than one match, an error is signalled.

If called interactively with a numeric prefix argument N and
there are at least N many matches, the Nth file will be visited.

If no matches are found, an error is signalled."
  (interactive (list (read-string "Find file (fff envvar): ")
                     (completing-read "In path (env var): "
                                      'fff-complete-envvar)
                     current-prefix-arg))
  (fff-<op>-file-in-path file (getenv envvar) which fff-match-predicate
                         'find-file (interactive-p)))

;;;###autoload
(defun fff-insert-file-in-envvar-path (file envvar &optional which)
  "Insert the file named FILE found in ENVVAR path into current buffer.
This function behaves exactly like `fff-find-file-in-envvar-path', except
that the contents of the file is inserted in the current buffer instead of
being visited in another buffer."
  (interactive (list (read-string "Insert file (fff envvar): ")
                     (completing-read "In path (env var): "
                                      'fff-complete-envvar)
                     current-prefix-arg))
  (fff-<op>-file-in-path file (getenv envvar) which fff-match-predicate
                         'insert-file (interactive-p)))

;;;###autoload
(defun fff-find-file-in-path (file path &optional which)
  "Visit the file named FILE in PATH.
PATH may be a list of directory names,
 a string consisting of colon-separated directory names,
 or a symbol name whose value is one of the above.

If called interactively with a generic prefix argument and there
is more than one possible match, a list is displayed from which
the user can select the desired match.  If called from a program
and there is more than one match, an error is signalled.

If called interactively with a numeric prefix argument N and
there are at least N many matches, the Nth file will be visited.

If no matches are found, an error is signalled."
  (interactive (list (read-string "Find file (fff path): ")
                     (fff-read-eval-sexp "In path (sexp): ")
                     current-prefix-arg))
  (fff-<op>-file-in-path file path which fff-match-predicate
                         'find-file (interactive-p)))

;;;###autoload
(defun fff-insert-file-in-path (file path &optional which)
  "Insert the file named FILE found in PATH into current buffer.
This function behaves exactly like `fff-find-file-in-path', except that the
contents of the file is inserted in the current buffer instead of being
visited in another buffer."
  (interactive (list (read-string "Insert file (fff path): ")
                     (fff-read-eval-sexp "In path (sexp): ")
                     current-prefix-arg))
  (fff-<op>-file-in-path file path which fff-match-predicate
                         'insert-file (interactive-p)))

;;; Generic backend for finding files in a path.
;;; You can use this to implement your own commands. e.g. a command to find
;;; files located in directories specified in the TEXINPUTS environment
;;; variable.
(defun fff-<op>-file-in-path (file path which pred op interactivep)
  (let* ((realpath (cond ((symbolp path)
                         (symbol-value path))
                        (t path)))
         (matches (fff-files-in-directory-list file realpath (not which) pred)))
    (cond ((fff-length1-p matches)
           (message "%s" (car matches))
           (funcall op (car matches)))
          ((null matches)
           (signal 'file-error
                   (list (format "File %s not found%s" file
                                 (if (symbolp path)
                                     (format " in %s" path)
                                   "")))))
          ((and (numberp which)
                (<= which (length matches)))
           (funcall op (nth (1- which) matches)))
          (t
           (if interactivep
               (fff-display-matches file matches op)
             (signal 'file-error
                     (list (format "Multiple instances of %s" file)
                           (cons 'path: path)
                           (cons 'predicate: pred)
                           (cons 'matches: matches))))))))


;;; Commands/functions for locating files via `locate' database.

;;;###autoload
(defun fff-find-file-in-locate-db (file &optional which)
  "Visit the file named FILE in a buffer.
The complete file name is searched for in an external `locate' database.
FILE must be a literal filename; no regexps are allowed.

If called interactively with a generic prefix argument and there
is more than one possible match, a list is displayed from which
the user can select the desired match.  If called from a program
and there is more than one match, an error is signalled.

If called interactively with a numeric prefix argument N and
there are at least N many matches, the Nth file will be visited.

If no matches are found, an error is signalled."
  (interactive (list (read-string "Find file (fff locate): ")
                     current-prefix-arg))
  (funcall 'fff-<op>-file-in-locate-db
           file which fff-match-predicate 'find-file (interactive-p)))

;;;###autoload
(defun fff-insert-file-in-locate-db (file &optional which)
  "Insert the file named FILE into current buffer.
This function behaves exactly like `fff-find-file-in-locate-db', except
that the contents of the file is inserted in the current buffer instead of
being visited in another buffer."
  (interactive (list (read-string "Insert file (fff locate): ")
                     current-prefix-arg))
  (funcall 'fff-<op>-file-in-locate-db
           file which fff-match-predicate 'insert-file (interactive-p)))

(defun fff-<op>-file-in-locate-db (file which pred op interactivep)
  (and interactivep
       (message "Searching for %s with `locate'..." file))
  (let ((matches (fff-locate-files-in-locate-db file (not which) pred)))
    (cond ((fff-length1-p matches)
           (message "%s" (car matches))
           (funcall op (car matches)))
          ((null matches)
           (signal 'file-error
                   (list (format "No matches for %s in locate database"
                                 file)
                         (cons 'predicate pred))))
          ((and (numberp which)
                (<= which (length matches)))
           (funcall op (nth (1- which) matches)))
          (t
           (if interactivep
               (fff-display-matches file matches op)
             (signal 'file-error
                     (list (format "Multiple matches for %s" file)
                           (cons 'predicate pred)
                           (list 'matches: matches))))))))

(defun fff-locate-files-in-locate-db (file &optional firstp pred)
  "Return a list of files named FILE meeting PRED in a `locate' database.
FILE must be a literal filename; no regexps are allowed.
Optional PRED may be any lisp function that takes one argument, a
  string representing the name of a file.
  It should return true if the file name should be included in the list of
  return values.  One common useful predicate is 'file-readable-p .
  If no predicate is specified, all files names named FILE are matched.

Return a list of the names found, in the order they appeared in the
database, or `nil' if none.
Optional third arg FIRSTP means return only the first match found.

The `locate' database must be kept reasonably up-to-date or this function
cannot be expected to find all existing occurences of a file.  On systems
where it is installed, it is usually run once a day or once a week via a
cron job.

The database is not read directly.  The program specified by the variable
``fff-locate-program'' is used to parse the database and print a list of
file names, one per line, on standard output.

Additional arguments can be specified in the variable named
``fff-locate-program-args'', which are passed to the locate
program before the name of the file."
  (let* ((re-file (format "/%s$" (regexp-quote file)))
         (found nil)
         (args (if fff-locate-program-args
                   (append (copy-sequence fff-locate-program-args)
                           file)
                 (list file)))
         (buf (generate-new-buffer (concat " *locate-" file "*")))
         beg end candidate)
    (save-excursion
      (set-buffer buf)
      (fundamental-mode)
      (buffer-disable-undo (current-buffer))
      (apply 'call-process fff-locate-program nil t nil args)
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward re-file nil t)
          (beginning-of-line)
          (setq beg (point))
          (end-of-line)
          (setq end (point))
          (setq candidate (buffer-substring beg end))
          (cond ((or (null pred)
                     (funcall pred candidate))
                 (setq found (cons candidate found))
                 (and firstp
                      (goto-char (point-max))))))))
    (kill-buffer buf)
    found))


;;; Display mode functions

(put 'fff-display-matches-mode 'mode-class 'special)

(defvar fff-display-matches-buffer-name "*File Name Matches*")
(defvar fff-display-matches-mode-map)

(defvar fff-display-matches-mode-selection-data)

;; Used internally by fff-display-matches-use-current-buffer-p.
(defvar fff-use-current-buffer-first-call-p)

(defun fff-display-matches (file matches &optional action buffer descrip)
  (and fff-sorting-predicate
       (setq matches (sort matches fff-sorting-predicate)))
  (let* ((buf (fff-display-matches-prepare-buffer))
         (orig-buf (current-buffer))
         (display-buf (or buffer orig-buf))
         (startpos 0)
         (l matches))
    (unwind-protect
        (progn
          (set-buffer buf)
          (goto-char (point-min))
          (cond (action
                 (insert "In this buffer, type RET to select "
                         "the match near point.\n")
                 (cond (descrip
                        (insert descrip "\n"))
                       ((and (symbolp action)
                             (commandp action))
                        (insert "That selection will invoke the command `"
                                (symbol-name action)
                                "' on it.\n"))
                       (t
                        (insert "That selection will invoke the function "
                                "specified by the value of the variable "
                                "`fff-display-matches-mode-selection-action'"
                                ".\n")))
                 (insert "\n")))
          (insert "Files found matching \"" file "\":\n\n")
          (setq startpos (point))
          (while l
            (insert (car l)
                    (if (file-directory-p (car l))
                        "/\n"
                      "\n"))
            (setq l (cdr l)))
          ;; If the order or number of these args are changed, update
          ;; fff-display-matches-select-match as well.
          (fff-display-matches-mode action
                                    display-buf
                                    (buffer-name display-buf)
                                    (set-marker (make-marker) startpos))
      (set-buffer orig-buf)))
    (fff-display-buffer buf nil startpos t)
    (message "Multiple matches for %s" file)))

(defun fff-display-matches-prepare-buffer ()
  (let ((buf (get-buffer-create fff-display-matches-buffer-name)))
    (save-excursion
      (set-buffer buf)
      (widen)
      (fundamental-mode)
      (buffer-disable-undo (current-buffer))
      (setq buffer-read-only nil)
      (erase-buffer))
    buf))

(defun fff-display-matches-mode (&rest data)
  "Major mode for buffers showing lists of possible matches for fff commands.
Type RET in the list to select the match near point.

This mode is used to display a menu of all the matching file names found by
a search.  Usually, you only get a menu if you used a prefix arg with one of
the commands and there is more than one possible match for the file name."
  (widen)
  (fundamental-mode)
  (kill-all-local-variables)
  (cond ((and (boundp 'fff-display-matches-mode-map)
              (keymapp fff-display-matches-mode-map)))
        (t
         (let ((map (make-sparse-keymap))
               (fn 'fff-display-matches-select-match)
               (keys '("\n" "\r")))
           (while keys
             (define-key map (car keys) fn)
             (setq keys (cdr keys)))
           (setq fff-display-matches-mode-map map))))
  (use-local-map fff-display-matches-mode-map)

  (make-local-variable 'fff-display-matches-mode-selection-data)
  (setq fff-display-matches-mode-selection-data data)

  (buffer-disable-undo (current-buffer))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (setq major-mode 'fff-display-matches-mode)
  (setq mode-name "FFF Display Matches"))

(defun fff-display-matches-select-match ()
  (interactive)
  (or (eq major-mode 'fff-display-matches-mode)
      (error "This command is inappropriate for this mode."))
  (let* ((data fff-display-matches-mode-selection-data)
         (fn      (nth 0 data))
         (buf     (nth 1 data))
         (bufname (nth 2 data))
         (pos     (nth 3 data))
         beg name)
    (cond (fn
           (and (< (point) pos)
                (error "Point is not positioned on a file name."))
           (save-excursion
             (beginning-of-line)
             (setq beg (point))
             (end-of-line)
             (setq name (buffer-substring beg (point))))
           (and (= (length name) 0)
                (error "Point is not positioned on a file name."))
           (and buf
                ;; Check that buffer still exists.
                ;; Only a killed buffer can have a null buffer name.
                (cond ((or (buffer-name buf)
                           (fff-display-matches-use-current-buffer-p bufname
                                                                     fn
                                                                     pos))
                       (fff-display-buffer buf nil nil t)
                       (funcall fn name))
                      (t
                       (error "Original buffer \"%s\" killed." bufname))))))))

(defun fff-display-matches-use-current-buffer-p (bufname op pos)
  (cond ((not (boundp 'fff-use-current-buffer-first-call-p))
         (make-local-variable 'fff-use-current-buffer-first-call-p)
         (setq fff-use-current-buffer-first-call-p t)
         (let ((p (point-marker)))
           (goto-char pos)
           (forward-line -2)
           (let ((buffer-read-only nil))
             (insert-before-markers "*** Note: original buffer \""
                                    bufname
                                    "\" no longer exists!\n\n"))
           (goto-char p))))
  (yes-or-no-p (format "Perform %s from current buffer? " op)))

;; When you modify a buffer and want to reset point, but the buffer is
;; already being displayed by a window, you can't actually change point
;; in that window unless you select it first.
(defun fff-display-buffer (buffer &optional not-this-window-p point selectp)
  (let ((old-win (selected-window))
        (old-buf (current-buffer))
        (win (display-buffer buffer not-this-window-p)))
    (and point
         (unwind-protect
             (progn
               (set-buffer buffer)
               (select-window win)
               (goto-char point)
               ;; Fake a prefix arg to keep from redrawing the whole frame.
               ;; Fortunately, this recentering doesn't cause actual redisplay
               ;; on the screen until we're done.
               ;; We need to do this to update emacs' data structures with
               ;; regard to what's going to be visible in the window when that
               ;; redisplay does actually happen.
               (recenter '(0))
               (cond ((and (pos-visible-in-window-p (point-max))
                           ;; Don't bother if whole buffer is visible
                           (not (pos-visible-in-window-p (point-min))))
                      ;; The last line is blank.
                      (goto-char (point-max))
                      (forward-line -1)
                      (recenter -1)
                      (goto-char point))))
           (select-window old-win)
           (set-buffer old-buf)))
    (and selectp
         (progn
           (set-buffer buffer)
           (select-window win)))
    win))


;;; Utility functions

(defun fff-files-in-directory-list (file path &optional firstp pred)
  "Return a list of all files named FILE located in PATH.

FILE may be a string containing a single file name or it
may be a list of file names to search for.
PATH may be a list of strings or a single string composed of
colon-separated directory names.

If more than one file name is specified, then the list returned will
contain all the matches for each element of PATH grouped together, e.g.

   \(fff-files-in-directory-list '\(\"foo\" \"bar\"\) '\(\"dir1\" \"dir2\"\)\)
   => '\(\"dir1/foo\" \"dir1/bar\" \"dir2/foo\" \"dir2/bar\"\)

   NOT '\(\"dir1/foo\" \"dir2/foo\" \"dir1/bar\" \"dir2/bar\"\)

Optional third argument PRED can be an arbitrary function of one
argument \(e.g. 'file-readable-p\), which should return non-`nil' if a file
name candidate should be returned.

If optional fourth argument FIRSTP is non-`nil', then return only the
first name found \(as a single-element list\)."
  (and (stringp file)
       (setq file (list file)))
  (and (stringp path)
       (setq path (fff-path-string->list path)))
  (let ((matches nil)
        flist f)
    (while path
      (setq flist file)
      (while flist
        (setq f (expand-file-name (concat (file-name-as-directory (car path))
                                          (car flist))))
        (setq flist (cdr flist))
        (and (file-exists-p f)
             (or (null pred)
                 (funcall pred f))
             ;; Avoid duplicates
             (not (member f matches))
             (progn
               (setq matches (cons f matches))
               (and firstp
                    (setq file nil
                          flist nil
                          path nil)))))
      (setq path (cdr path)))
    (nreverse matches)))

(defun fff-suffix (str suffix-list)
  (cond ((stringp str)
         (mapcar (function (lambda (ext) (concat str ext))) suffix-list))
        ((consp str)
         (apply 'nconc (mapcar (function (lambda (ext)
                                    (mapcar (function (lambda (s)
                                                        (concat s ext)))
                                            str)))
                        suffix-list)))))

(defun fff-file-name-completions-in-path
  (name-regexp path-list &optional predicate filter)
  "Return an obarray containing file name completions.
All file names matching NAME-REGEXP, located in directories listed in
PATH-LIST, which satisfy optional arg PREDICATE, are put into the obarray
after being filtered through optional FILTER for potential edits.

If NAME-REGEXP is nil, then all files are candidates.

PREDICATE and FILTER should be functions which take one argument, a string
representing a file name."
  (let ((completions (make-vector fff-default-obarray-size 0))
        (files nil))
    (while path-list
      (and (file-directory-p (car path-list))
           (setq files (directory-files (car path-list) nil name-regexp t)))
      (while files
        (cond ((file-directory-p (car files)))
              ((or (null predicate)
                   (funcall predicate (car files)))
               (intern (if filter
                           (funcall filter (car files))
                         (car files))
                       completions)))
        (setq files (cdr files)))
      (setq path-list (cdr path-list)))
    completions))

(defun fff-completing-read-file-in-path
  (prompt path-list &optional predicate require-match init hist filter)
  "Read a file name with completion.
Arguments are like those of `completing-read', except second argument
PATH-LIST specifies a list of directories containing candidate file names.
Optional last argument FILTER modifies candidate file names.

If PATH-LIST is a symbol, call that symbol as a function and use the return
value as the path list."
  (let ((completion-table nil)
        (completer (make-symbol "completer")))
    (fset completer
          (function
           (lambda (str pred action)
                        (or completion-table
                            (setq completion-table
                                  (fff-file-name-completions-in-path
                                   nil path-list predicate filter)))
                        (if action
                            (all-completions str completion-table pred)
                          (try-completion str completion-table pred)))))
    (and (symbolp path-list)
         (setq path-list (funcall path-list)))
    (completing-read prompt completer predicate require-match init hist)))

;; size needs to be prime for good hashing characteristics.
(defun fff-symbol-list->obarray (list &optional obarray-or-size filter)
  (let ((new-obarray (if (vectorp obarray-or-size) ; poor man's obarray test
                         obarray-or-size
                       (make-vector (or obarray-or-size
                                        fff-default-obarray-size) 0)))
        (elt nil))
    (while list
      (setq elt (car list))
      (setq list (cdr list))
      (and filter
           (setq elt (funcall filter elt)))
      (intern (if (symbolp elt)
                  (symbol-name elt)
                elt)
              new-obarray))
    new-obarray))

(defun fff-complete-envvar (string predicate &optional allp)
  (let ((table (fff-env->obarray))
        (fn (if allp 'all-completions 'try-completion)))
    (funcall fn string table predicate)))

(defun fff-env->obarray (&optional envlist)
  (let ((new-obarray (make-vector fff-default-obarray-size 0))
        (list (or envlist process-environment)))
    (save-match-data
      (while list
        (and (string-match "=" (car list))
             (intern (substring (car list) 0 (1- (match-end 0))) new-obarray))
        (setq list (cdr list))))
    new-obarray))

(defun fff-path-string->list (path)
  "Convert a colon-separated path string into a list.
Any null paths are converted to \".\" in the returned list so that
elements of the path may be treated consistently when prepending them to
file names."
  (let* ((list (fff-string-split path ":"))
         (l list))
    (while l
      (and (string= "" (car l))
           (setcar l "."))
      (setq l (cdr l)))
    list))

(defun fff-string-split (string separator &optional limit)
  "Split STRING at occurences of SEPARATOR.  Return a list of substrings.
SEPARATOR can be any regexp, but anything matching the separator will never
appear in any of the returned substrings.
If optional arg LIMIT is specified, split into no more than that many
fields \(though it may split into fewer\)."
  (let ((string-list nil)
        (len (length string))
        (pos 0)
        (splits 0)
        str)
    (save-match-data
      (while (<= pos len)
        (setq splits (1+ splits))
        (cond ((and limit
                    (>= splits limit))
               (setq str (substring string pos))
               (setq pos (1+ len)))
              ((string-match separator string pos)
               (setq str (substring string pos (match-beginning 0)))
               (setq pos (match-end 0)))
              (t
               (setq str (substring string pos))
               (setq pos (1+ len))))
        (setq string-list (cons str string-list))))
    (nreverse string-list)))

;; This doesn't always eval the sexp read from the minibuffer.
;; If it's a symbol and that symbol is bound to a value, return that symbol
;; instead of the symbol's value.
(defun fff-read-eval-sexp (prompt)
  (let ((result nil)
        (sexp nil))
    (while (null result)
      (condition-case errlist
          (setq sexp (read-from-minibuffer prompt nil minibuffer-local-map t)
                result (if (and (symbolp sexp)
                                (boundp sexp))
                           sexp
                         (eval sexp)))
        (error (message "Error: %s: %s"
                        (mapconcat 'symbol-name (cdr errlist) " ")
                        (get (car errlist) 'error-message))
               (sit-for 5))))
    result))

(defun fff-file-nondirectory-p (f)
  (and (file-exists-p f)
       (not (file-directory-p f))))

(defun fff-insert-file-contents-next-region (file size)
  (let* ((point (point))
         (beg (buffer-size))
         (end (+ beg size))
         (inserted 0))
    (goto-char (point-max))
    (setq inserted (nth 1 (insert-file-contents file nil beg end)))
    (goto-char point)
    inserted))


;;; Keymap installation

(defun fff-emacs-variant ()
  (let ((version (emacs-version))
        (alist '(("XEmacs"       . xemacs)
                 ("Lucid"        . lemacs)
                 ("^GNU Emacs"   . emacs)))
        result)
    (save-match-data
      (while alist
        (cond
         ((string-match (car (car alist)) version)
          (setq result (cdr (car alist)))
          (setq alist nil))
         (t
          (setq alist (cdr alist))))))
    result))

(defconst fff-menu-bar-support-p
  (and (string-lessp "19" emacs-version)
       ;; I haven't implemented menu bar support for XEmacs yet.
       (memq (fff-emacs-variant) '(emacs))))

(defun fff-controlify-key-sequence (key-sequence)
  (setq key-sequence (copy-sequence key-sequence))
  (let* ((tmpl (copy-sequence "?\\C-*"))
         (tmplidx (1- (length tmpl)))
         (len (length key-sequence))
         (i 0))
    (while (< i len)
      (aset tmpl tmplidx (aref key-sequence i))
      (aset key-sequence i (read tmpl))
      (setq i (1+ i))))
  key-sequence)

(defun fff-make-sparse-keymap (&optional string)
  (if (and string
           fff-menu-bar-support-p
           (eq (fff-emacs-variant) 'emacs))
      (make-sparse-keymap string)
    (make-sparse-keymap)))

(defun fff-define-key (seq fn &optional menu-descrip ctrlify-p)
  (let ((fndef (if (and fff-menu-bar-support-p menu-descrip)
                   (cons menu-descrip fn)
                 fn)))
    (and ctrlify-p
         (setq seq (fff-controlify-key-sequence seq)))
    (define-key fff-map seq fndef)))

(defun fff-install-map (&optional overridep keymap-prefix)
  "Install the fff keymap."
  (interactive "P")

  (cond ((null fff-map)
         ;(setq fff-map (fff-make-sparse-keymap "FFF"))
         (setq fff-map (fff-make-sparse-keymap))

         ;; Listed in reverse of desired order so that menu bar will be in
         ;; correct order.

         (fff-define-key "\C-h" 'describe-prefix-bindings)

         (fff-define-key "\C-i\C-f"
                         'fff-insert-file-in-locate-db
                         "Insert file from `locate' DB")

         (fff-define-key "\C-i\C-p"
                         'fff-insert-file-in-path
                         "Insert file from path")

         (fff-define-key "\C-i\C-v"
                         'fff-insert-file-in-envvar-path
                         "Insert file from environment path")

         (fff-define-key "\C-i\C-e"
                         'fff-insert-file-in-exec-path
                         "Insert file from exec-path")

         (fff-define-key "\C-f"
                         'fff-find-file-in-locate-db
                         "Find file from `locate' DB")

         (fff-define-key "\C-p"
                         'fff-find-file-in-path
                         "Find file from path")

         (fff-define-key "\C-v"
                         'fff-find-file-in-envvar-path
                         "Find file from environment path")

         (fff-define-key "\C-e"
                         'fff-find-file-in-exec-path
                         "Find file from exec-path")
         ))

  (fset 'fff-command-prefix fff-map)
  (and keymap-prefix
       (setq fff-map-prefix keymap-prefix))

  ;; XEmacs doesn't have menu-bar-final-items.  Expect a compiler warning.
  (and fff-menu-bar-support-p
       (boundp 'menu-bar-final-items)
       (not (memq 'fff menu-bar-final-items))
       (setq menu-bar-final-items (cons 'fff menu-bar-final-items)))

  (let ((current-binding (key-binding fff-map-prefix))
        (description (key-description fff-map-prefix)))
    (cond ((eq current-binding 'fff-command-prefix))
          ((and current-binding
                (not overridep))
           (error "Prefix \"%s\" is already bound" description))
          (t
           (fff-uninstall-map)
           (cond (fff-map-prefix
                  (global-set-key fff-map-prefix 'fff-command-prefix)

                  (and fff-menu-bar-support-p
                       (global-set-key [menu-bar fff]
                                       (cons "FFF" fff-map)))))))
    (and (interactive-p)
         (message "fff commands are on prefix \"%s\"" description))))

(defun fff-uninstall-map ()
  (interactive)

  (and fff-menu-bar-support-p
       (global-unset-key [menu-bar fff]))

  (let ((existing (where-is-internal 'fff-command-prefix)))
    (while existing
      (global-unset-key (car existing))
      (setq existing (cdr existing)))))

(provide 'fff)

;;; fff.el ends here
