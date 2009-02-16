;;; grep+.el --- Extensions to standard library `grep.el'.
;; 
;; Filename: grep+.el
;; Description: Extensions to standard library `grep.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 2005-2006, Drew Adams, all rights reserved.
;; Created: Fri Dec 16 13:36:47 2005
;; Version: 22.0
;; Last-Updated: Sun Sep 10 16:14:15 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 138
;; URL: http://www.emacswiki.org/cgi-bin/wiki/grep+.el
;; Keywords: tools, processes, compile
;; Compatibility: GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   `avoid', `compile', `compile+', `compile-', `fit-frame',
;;   `font-lock', `frame-cmds', `frame-fns', `grep', `misc-fns',
;;   `strings', `syntax', `thingatpt', `thingatpt+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;;
;;  Extensions to standard library `grep.el':
;;
;;    1. Additional keys are bound here.
;;    2. Mouse-over is active on the entire hit line, not just on the
;;       file-name part.
;;    3. `grep' command provides a default search string in all cases,
;;       and that default value is better.
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'grep+)
;;
;;
;;  New user option defined here:
;;
;;    `grep-default-regexp-fn'.
;;
;;
;;  New function defined here:
;;
;;    `grep-default-regexp-fn'.
;;
;;
;;  ***** NOTE: The following variable defined in `grep.el'
;;              has been REDEFINED HERE:
;;
;;  `grep-regexp-alist' - Regexp matches whole line, so mouse-over it.
;;
;;
;;  ***** NOTE: The following minor mode defined in `grep.el'
;;              has been REDEFINED HERE:
;;
;;  `grep-mode' - No change.  Redefined here so it uses modified value
;;                of `grep-regexp-alist'.
;;
;;
;;  ***** NOTE: The following function defined in `grep.el'
;;              has been REDEFINED HERE:
;;
;;  `grep-default-regexp-fn' - Uses `grep-default-regexp-fn'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;; 
;; 2006/09/10 dadams
;;     Updated definition of grep-mode with latest from grep.el.
;; 2005/12/17 dadams
;;     Added: grep-default-regexp-fn, grep-default-command.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; ;; Floor, Boston, MA 02110-1301, USA.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'compile+ nil t) ;; (no error if not found) - to pick up enhancements for grep too.
(require 'font-lock) ;; font-lock-keyword-face
(require 'grep)
(require 'thingatpt+ nil t) ;; (no error if not found) symbol-name-nearest-point

;;;;;;;;;;;;;;;;;;;;;;;


;;;###autoload
(defcustom grep-default-regexp-fn (if (fboundp 'symbol-name-nearest-point)
                                      'symbol-name-nearest-point
                                    'word-at-point)
  "*Function of 0 args called to provide default search regexp to \\[grep].
Some reasonable choices:
`word-nearest-point', `symbol-name-nearest-point', `sexp-nearest-point'.

If this is nil and no prefix arg is given to `grep', then no
defaulting is done.

If this is not a function, then function `grep-default-regexp-fn' does
the defaulting otherwise."
  :type '(choice (const :tag "No default search regexp (unless you use `C-u')" nil)
          (function :tag "Function of zero args to provide default search regexp"))
  :group 'grep)

;;;###autoload
(defun grep-default-regexp-fn ()
  "*Function of 0 args called to provide default search regexp to \\[grep].
No defaulting is done if option `grep-default-regexp-fn' is nil.
Otherwise, the defaulting function is provided by the first of these
that references a defined function:
  - variable `grep-default-regexp-fn'
  - variable `find-tag-default-function'
  - the `find-tag-default-function' property of the `major-mode'
  - function `symbol-name-nearest-point', if bound
  - function `grep-tag-default'"
  (cond ((fboundp grep-default-regexp-fn) grep-default-regexp-fn)
        (find-tag-default-function)
        ((get major-mode 'find-tag-default-function))
        ((fboundp 'symbol-name-nearest-point) 'symbol-name-nearest-point)
        (t 'find-tag-default)))



;;; REPLACE ORIGINAL in `grep.el'
;;;
;;; Use `grep-default-regexp-fn' to define `tag-default'.
;;;###autoload
(defun grep-default-command ()
  (let ((tag-default (shell-quote-argument (or (funcall (grep-default-regexp-fn)) "")))
        (sh-arg-re "\\(\\(?:\"\\(?:[^\"]\\|\\\\\"\\)+\"\\|'[^']+'\\|[^\"' \t\n]\\)+\\)")
        (grep-default (or (car grep-history) grep-command)))
    ;; Replace the thing matching for with that around cursor.
    (when (or (string-match
               (concat "[^ ]+\\s +\\(?:-[^ ]+\\s +\\)*" sh-arg-re "\\(\\s +\\(\\S +\\)\\)?")
               grep-default)
              (string-match "\\(\\)\\'" grep-default)) ; If the string is not yet complete.
      (unless (or (not (stringp buffer-file-name))
                  (when (match-beginning 2)
                    (save-match-data
                      (string-match (wildcard-to-regexp (file-name-nondirectory
                                                         (match-string 3 grep-default)))
                                    (file-name-nondirectory buffer-file-name)))))
        (setq grep-default (concat (substring grep-default 0 (match-beginning 2))
                                   " *."
                                   (file-name-extension buffer-file-name))))
      (replace-match tag-default t t grep-default 1))))



;;; REPLACE ORIGINAL in `grep.el'
;;;
;;; Use `grep-default-regexp-fn' to define default search string.
;;;###autoload
(defun grep (command-args &optional highlight-regexp)
  "Run `grep', with user-specified args, and collect output in a buffer.
COMMAND-ARGS are the user-specified arguments.
While `grep' runs asynchronously, you can use the
\\[next-error] command (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error]
in the *grep* output buffer, to find the text that `grep' hits refer to.

This command uses a special history list for its COMMAND-ARGS, so you can
easily repeat a grep command.

The text (regexp) to find is defaulted, based upon
`grep-default-regexp-fn'.

If a prefix arg is provided, then the default text is substituted
into the last grep command in the grep command history (or into
`grep-command' if that history list is empty).  That is, the same
command options and files to search are used as the last time.

If specified, optional second arg HIGHLIGHT-REGEXP is the regexp to
temporarily highlight in visited source lines."
  (interactive
   (progn
     (unless (and grep-command
                  (or (not grep-use-null-device) (eq grep-use-null-device t)))
       (grep-compute-defaults))
     (let ((default (grep-default-command)))
       (list (read-from-minibuffer
              "grep <pattern> <files> :  "
              (if current-prefix-arg
                  default
                (concat grep-command (and grep-default-regexp-fn
                                          (funcall (grep-default-regexp-fn))) " "))
              nil nil 'grep-history
              (if current-prefix-arg nil default))))))

  ;; Setting process-setup-function makes exit-message-function work
  ;; even when async processes aren't supported.
  (compilation-start (if (and grep-use-null-device null-device)
                         (concat command-args " " null-device)
                       command-args)
                     'grep-mode nil highlight-regexp))

;; New bindings.
(define-key grep-mode-map "g" 'grep)
(define-key grep-mode-map "G" 'grep)
(define-key grep-mode-map "n" 'next-error-no-select)
(define-key grep-mode-map "N" 'next-error-no-select)
(define-key grep-mode-map "p" 'previous-error-no-select)
(define-key grep-mode-map "P" 'previous-error-no-select)

;; New face values
(set-face-foreground grep-match-face nil)
(set-face-background grep-match-face "SkyBlue")
(setq grep-hit-face font-lock-keyword-face)



;;; REPLACE ORIGINAL `grep-regexp-alist' defined in `grep.el'.
;;;
;;; Use mouseover on whole line.  Same as original, except for this.
(unless (featurep 'grep+)
  (setq grep-regexp-alist
        (mapcar (lambda (elt)`(,(concat (car elt) ".*") ,@(cdr elt))) grep-regexp-alist)))



;;; REPLACE ORIGINAL `grep-mode' defined in `grep.el'.
;;;
;;; Re-create, so it uses the modified `grep-regexp-alist'.
;;; This definition SHOULD BE THE SAME AS THE ORIGINAL in `grep.el'.
(define-compilation-mode grep-mode "Grep"
  "Sets `grep-last-buffer' and `compilation-window-height'."
  (setq grep-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-error-face)
       grep-hit-face)
  (set (make-local-variable 'compilation-error-regexp-alist)
       grep-regexp-alist)
  (set (make-local-variable 'compilation-process-setup-function)
       'grep-process-setup)
  (set (make-local-variable 'compilation-disable-input) t))

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'grep+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; grep+.el ends here
