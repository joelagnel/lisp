;;;; misc-elisp-tools.el -- assorted elisp-related tools
;;; Time-stamp: <2006-01-24 12:30:47 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'misc-elisp-tools)

(defun stack-trace (off)
  "Set stack-trace-on-error, or (with prefix argument) reset it."
  (interactive "p")
  (setq stack-trace-on-error (if (= 1 off) t nil))
  (message "Stack traces on error %sabled"
           (if stack-trace-on-error "en" "dis")))

(defun toggle-stack-trace ()
  "Toggle stack-trace-on-error."
  (interactive)
  (setq stack-trace-on-error (not stack-trace-on-error))
  (message "Stack traces on error %sabled"
           (if stack-trace-on-error "en" "dis")))

(defun re-load (file)
  "Byte-compile FILE from .el to .elc form, and load the .elc form."
  (interactive "fFile to recompile: ")
  (save-some-buffers)
  (byte-compile-file file)
  (load (file-base-name file)))

(defun current-defun-name ()
  "Return the name of the current defun"
  (save-excursion
    (end-of-defun)
    (beginning-of-defun)
    (down-list 1)
    (forward-sexp 2)
    (let* ((end-of-name (point))
	   (start-of-name (progn (forward-sexp -1) (point))))
      (intern (buffer-substring start-of-name end-of-name)))))

(defun execute-current-defun ()
  "Call interactively the function point is within.
Intended particularly for handsfree use."
  (interactive)
  (call-interactively (current-defun-name)))

;; I wish I was deep as well as macho! (From T-shirt showing Rodin's Thinker)
(setq max-lisp-eval-depth 4000)
(setq max-specpdl-size 3000)

(modify-syntax-entry ?: "." lisp-mode-syntax-table)

;;; end of misc-elisp-tools.el
