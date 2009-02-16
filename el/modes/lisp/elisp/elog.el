;;; elog.el --- simple log for emacs lisp debugging

;; Copyright (C) 1996 by Michelangelo Grigni

;; Author: Michelangelo Grigni <mic@caribou>
;; Keywords: tools, lisp

;; This file could be part of GNU Emacs.

;;; Commentary:

;; This file provices a few simple functions to log messages and
;; values to a "*log*" buffer.

;;; History:

;; Created years ago.  Used to be called trace.el, until v19
;; introduced trace.el and the (real-valued) log function.
;;
;; 3/96: converted to lisp-mnt format, added `elog-function' to
;; overcome some problems with `trace-function'.

;;; Code:

(provide 'elog)

(defun elog (&rest args)
  "Print arguments to a \"*log*\" buffer, and display it.
With no arguments, erase the buffer."
  (apply 'elog-quiet args)
  (display-buffer "*log*"))

(defun elog-quiet (&rest args)
  "Print arguments to a \"*log*\" buffer.
With no arguments, erase the buffer."
  (save-excursion
    (set-buffer (get-buffer-create "*log*"))
    (if (not args)
	(erase-buffer)
      (goto-char (point-max))
      (let ((standard-output (current-buffer))
	    (print-escape-newlines t))
	(mapcar (function (lambda (x) (prin1 x) (princ " "))) args)
	(terpri)
	))))

(defun elog-backtrace nil
  "Print a backtrace into the \"*log*\" buffer."
  (let ((standard-output (get-buffer-create "*log*")))
    (backtrace)))

(defun elog-frame (&optional depth)
  "Describe frame at DEPTH \(default 0 for caller\), in \"*log*\" buffer."
  (elog-quiet (cdr (backtrace-frame (+ 4 (or depth 0))))))

(eval-when-compile (require 'backquote))
(defun elog-function (fsym)
  "Toggle elog advice on function FSYM."
  (interactive
   (list (intern (completing-read "Elog function: " obarray 'fboundp t))))
  (require 'advice)
  (if (assq 'elog (ad-get-enabled-advices fsym 'around))
      (progn
	(ad-remove-advice fsym 'around 'elog)
	(message "Removed elog advice on `%s'" fsym))
    (ad-add-advice
     fsym
     `(elog nil t
	    (advice lambda nil
		    "Quietly record I/O in the \"*log*\" buffer."
		    (elog-quiet ',fsym '-> (ad-get-args 0))
		    ad-do-it
		    (elog-quiet ',fsym '<- ad-return-value)))
     'around nil)
    (message "Added elog advice on `%s'" fsym))
  (ad-activate-on fsym nil))
;; For example: (elog-function 'read-file-name-internal)
;; Note: (trace-function 'read-file-name-internal) does not work
;; nearly as well.

;; I almost always use elog just for temporary debugging, so
;; it is probably a mistake if I byte-compile it.
(put 'elog 'byte-obsolete-info "Are you sure?")
(put 'elog-quiet 'byte-obsolete-info "Are you sure?")

;;; elog.el ends here
