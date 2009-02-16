;;;; dwim-find.el -- find something about the thing at point
;;; Time-stamp: <2005-03-08 11:58:40 john>

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

(provide 'dwim-find)

;; old stuff to raid:
;; (defvar major-modes-likely-to-contain-filenames
;;   '(
;;     shell-mode
;;     sh-mode
;;     html-helper-mode
;;     )
;;   "If in one of these modes, look for text as a filename rather than a tag.")

;; (defun probably-in-quoted-text ()
;;   "Return whether we appear to be in quoted text"
;;   (save-excursion
;;     (let ((end (point))
;; 	  (in nil))
;;       (beginning-of-line 1)
;;       (while (<= (point) end)
;; 	(forward-char)
;; 	(when (eq (char-after) 34)
;; 	  (setq in (not in))))
;;       in)))

;; (defun dwim-find-at-point ()
;;   "Treat the text around point as a filename or a tag to find, whichever makes more sense."
;;   (interactive)
;;   (cond
;;    ((memq major-mode major-modes-likely-to-contain-filenames)
;;     (call-interactively 'find-file-at-point))
;;    ((probably-in-quoted-text)
;;     (call-interactively 'find-file-at-point))
;;    (t
;;     (call-interactively 'find-tag-with-hooks))))


;; todo: understand (require ...) using the load-path

;;;###autoload
(defun dwim-find ()
  "Find the thing at point.
Make sensible guesses as to whether it's a tag, a filename, a lisp function name, or whatever."
  (interactive)
  ;; add citation.el's facilities
  (cond
   ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
    (let ((variable (variable-at-point))
	  (function (function-called-at-point)))
      (cond
       ((symbolp variable)
	(find-variable-other-window variable))
       ((symbolp function)
	(cond
	 ((eq function 'require)
	  (find-library
	   (save-excursion
	     (backward-up-list)
	     (down-list)
	     (forward-sexp 2)
	     (let ((end (point)))
	       (backward-sexp 1)
	       (buffer-substring-no-properties (1+ (point)) end)))))
	 (t
	  (find-function-other-window (function-called-at-point))))))))
   ((file-exists-p (thing-at-point 'filename))
    (find-file-other-window (thing-at-point 'filename)))
   (t (message "Don't know how to find this"))))

;;;###autoload
(defun dwim-describe ()
  "Find information about the thing at point.
Make sensible guesses as to whether it's a tag, a filename, a lisp function name, or whatever."
  (interactive)
  (cond
   ((memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
    (let ((variable (variable-at-point))
	  (function (function-called-at-point)))
      (cond
       ((symbolp variable)
	(describe-variable variable))
       ((symbolp function)
	(describe-function (function-called-at-point))))))
   (t (message "Don't know how to describe this"))))

;;; end of dwim-find.el
