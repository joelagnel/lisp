;;; lmselect.el --- Lisp Machine SELECT key esque thingy.

;; Copyright (C) 2002  Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of version 2 of the GNU General
;; Public License as published by the Free Software Foundation.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(eval-and-compile
  (defvar lmselect-key 'f7))

(defun lmselect-filter-buffer-list (filter &optional frame)
  "Return a list of buffers that pass FILTER restricted to FRAME."
  (let ((retval '()))
    (mapc (lambda (buffer)
	    (when (funcall filter buffer)
	      (add-to-list 'retval buffer)))
	  (buffer-list frame))
    (nreverse retval)))

(defun lmselect-cycle-buffer (filter &optional starter)
  "Cycle through buffers that pass FILTER, or call STARTER if none."
  (let ((buffers (lmselect-filter-buffer-list filter))
	(buffer nil))
    (if (and starter (not buffers))
	(funcall starter)
      (while (and (not buffer) buffers)
	(setq buffer (car buffers))
	(setq buffers (cdr buffers))
	(when (eq buffer (current-buffer))
	  (setq buffer nil)))
      (when buffer
	(bury-buffer)
	(switch-to-buffer buffer)))))

(defmacro lmselect-define-cycler (key filter &optional starter)
  `(global-set-key
    ,(vector lmselect-key key)
    (lambda (arg)
      (interactive "P")
      (lmselect-cycle-buffer
       ,filter
       ,starter))))

(put 'lmselect-define-cycler 'lisp-indent-function 1)

;; you configure it like this:

;; make SELECT-l cycle lispy buffers
(lmselect-define-cycler ?l
  (lambda (buffer)
    (with-current-buffer buffer
      (memq major-mode
	    '(emacs-lisp-mode
	      lisp-interaction-mode
	      lisp-mode))))
  (lambda ()
    (find-file (read-file-name "Find Lisp file: " "~/elisp/"))))

(lmselect-define-cycler ?s
  (lambda (buffer)
    (with-current-buffer buffer
      (memq major-mode
	    '(eshell-mode shell-mode terminal-mode term-mode))))
  'eshell)

;; for erc
(lmselect-define-cycler ?i
  (lambda (buffer)
    (with-current-buffer buffer
      (eq major-mode 'erc-mode)))
  'erc-select)

(provide 'lmselect)
;;; lmselect.el ends here
