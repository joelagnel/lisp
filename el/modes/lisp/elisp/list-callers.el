;;; Saved through ges-version 0.3.3dev at 2004-05-14 11:51
;;; ;;; From: Helmut Eller <e9626484@stud3.tuwien.ac.at>
;;; ;;; Subject: Re: list-callers.el
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Wed, 05 May 2004 19:37:47 GMT
;;; ;;; Organization: Customers chello Austria

;;; [1. text/plain]

;;; Helmut Eller <e9626484@stud3.tuwien.ac.at> writes:

;;; > This is a little tool to find the callers of a Lisp function.
;;; > Position point over an interesting function name and call it with `M-x
;;; > lc-list-callers'.  This pops you in a window with a list of the
;;; > callers of that function.

;;; Here's the second version with some fine tuning.  The main entry point
;;; was renamed to M-x list-callers.  Lists are now scanned iteratively to
;;; avoid the most common stack overflows.  The window configuration gets
;;; now restored after pressing `q'.

;;; Helmut.


;;; [2. application/emacs-lisp; list-callers.el]

;;; list-callers.el --- Find the callers of a Lisp function
;; 
;; Copyright (C) 2004  Helmut Eller
;;
;; You can redistribute this file under the terms of the GNU General
;; Public License.
;;

;;; Commentary:
;;
;; This is a little tool to find the callers of a Lisp function.
;; Position point over an interesting function name and call it with
;; `M-x list-callers'.  This pops you in a window with a list of the
;; callers of that function.
;;
;; The tool grovels through all named function objects to see if the
;; function references the symbol.  It is only a heuristic, but works
;; good enough for simple cases.  Things may get slow if your Emacs
;; image is really large and contains huge interlinked objects.
;;
;; The code should work with GNU Emacs 20 and Emacs 21.  XEmacs is not
;; supported.
;;

;;; Thanks:
;;
;; Andrew M. Scott for valuable feedback and pointing out that
;; function-at-point is not pre-loaded.
;;

;;; Code:

(eval-and-compile
  (require 'cl)
  (require 'find-func))

(defsubst lc-byte-code-constants (bytecode)
  "Access the constant vector of the bytecode-function BYTECODE."
  (aref bytecode 2))

(defun lc-references-symbol-p (object symbol seen-nodes)
  "Test if OBJECT contains a reference to SYMBOL.
SEEN-NODES is used to detect cycles."
  (if (memq object seen-nodes)
      nil
    (push object seen-nodes)
    (etypecase object
      (symbol
       (eq object symbol))
      (cons
       (let ((flag nil))
	 ;; iterate over lists to save stack space
	 (while (and (not flag)
		     (consp object))
	   (setq flag (lc-references-symbol-p (car object) symbol seen-nodes))
	   (setq object (cdr object))
	   (cond ((memq object seen-nodes)
		  (setq object nil))
		 (t
		  (push object seen-nodes))))
	 (or flag
	     (lc-references-symbol-p object symbol seen-nodes))))
      ((or number string bool-vector char-table buffer frame subr)
       nil)
      (vector
       (loop for elt across object
	     thereis (lc-references-symbol-p elt symbol seen-nodes)))
      (byte-code-function
       (lc-references-symbol-p (lc-byte-code-constants object)
			       symbol seen-nodes)))))

(defun lc-find-referrers (symbol)
  "Return a list of all named functions referring SYMBOL."
  (check-type symbol symbol)
  (let ((referrers '()))
    (mapatoms (lambda (atom)
		(when (and (fboundp atom)
			   (lc-references-symbol-p (symbol-function atom)
						   symbol '()))
		  (push atom referrers))))
    referrers))

(defun lc-find-function-at-point-other-window ()
  "Display the source of the function at point in other window."
  (interactive)
  (destructuring-bind (buffer &rest point)
      (find-function-noselect (function-at-point))
    (with-current-buffer buffer
      (goto-char point)
      (save-selected-window
	(let ((win (display-buffer buffer t)))
	  (set-window-point win (point))
	  (select-window win)
	  (recenter 3))))))

(defvar lc-old-window-config nil
  "Buffer local variable use to restore the window configuration.")

(defun lc-display-callers (callers)
  "Display a buffer to browse a list of CALLERS."
  (with-current-buffer (get-buffer-create "*callers*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (set (make-local-variable 'lc-old-window-config)
	 (current-window-configuration))
    (let ((keymap (make-sparse-keymap)))
      (define-key keymap [return] 'lc-find-function-at-point-other-window)
      (define-key keymap (kbd "RET") 'lc-find-function-at-point-other-window)
      (define-key keymap [?q] 'lc-quit)
      (use-local-map keymap)
      (dolist (symbol callers)
	(let ((start (point)))
	  (insert (symbol-name symbol) "\n")))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (select-window (display-buffer (current-buffer))))))

(defun lc-quit ()
  "Kill the *callers* buffer and restore the window configuration."
  (interactive)
  (let ((buffer (current-buffer)))
    (set-window-configuration lc-old-window-config)
    (kill-buffer buffer)))

(defun lc-read-function-name ()
  "Read a function name much like C-h f does.  Return a symbol."
  (let* ((default (function-called-at-point))
	 (string (completing-read
		  (cond (default
			  (format "Function (default %s): " default))
			(t "Function: "))
		  obarray 'fboundp t nil nil (symbol-name default))))
    (when (equal string "")
      (error "No function name specified"))
    (intern string)))

(defun list-callers (symbol)
  "List the callers of the function at point.
If called non-interactively display the callers of SYMBOL."
  (interactive (list (lc-read-function-name)))
  (cond ((or (not symbol)
	     (not (symbolp symbol)))
	 (error "Bad argument: %S" symbol))
	(t
	 (let* ((referrers (lc-find-referrers symbol))
		(referrers (sort referrers #'string<)))
	   (lc-display-callers referrers)))))

(mapc #'byte-compile
      '(lc-references-symbol-p
	lc-find-referrers))

(provide 'list-callers)

;;; list-callers.el ends here

