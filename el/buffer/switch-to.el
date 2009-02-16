;;; Saved through ges-version 0.3.2dev at 2002-12-18 14:15
;;; ;;; From: Benjamin Drieu <bdrieu@april.org>
;;; ;;; Subject: switch-to.el 1.3
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Wed, 18 Dec 2002 15:57:49 +0100
;;; ;;; Organization: APRIL - http://www.april.org/

;;; --=-=-=


;;; This is an unreleased code snippet that I really find handful.  Better
;;; alternatives may have been written in the meantime, but this has
;;; proven to be very useful to emacs fans I know.

;;; ;; Getting tired of switching to buffers by typing their names, even
;;; ;; with some help from tab completion?  This small program allows you
;;; ;; to define a special behavior to M-<function key> keys.

;;; ;; i.e. you can switch to all buffers visiting elisp code by cycling
;;; ;; all buffers matching "\.el$".

;;; ;; This was the initial objective of that code, but it was stupid to
;;; ;; simply switch to the *Group* buffer if I wished to read my mail.
;;; ;; Recent versions of this package allow you to evaluate elisp
;;; ;; expressions instead (i.e "(gnus)"), and define the function
;;; ;; "switch-to-matched-buffer" to switch to buffer according to their
;;; ;; name.

;;; ;; To install this package, just insert following lines in your .emacs
;;; ;; file.  In that example, M-f1 switch to a shell buffer (shell-toggle
;;; ;; is needed), M-f2 invokes gnus and M-f3, M-f4, M-f5 switch
;;; ;; respectively to Perl, Emacs-Lisp and C buffers.

;;; ;; (autoload 'switch-to-init "switch-to")
;;; ;; (switch-to-init '((shell-toggle nil)
;;; ;;                  (gnus)
;;; ;;                  (switch-to-matched-buffer-in-one-window "\.pl$")
;;; ;;                  (switch-to-matched-buffer "\.el$")
;;; ;;                  (switch-to-matched-buffer "\.c$")))

;;; -- 
;;; o Benjamin Drieu:       bdrieu@april.org
;;;                         benj@grassouille.org
;;; o APRIL:                http://www.april.org/

;;; --=-=-=
;;; Content-Type: application/emacs-lisp
;;; Content-Disposition: attachment; filename=switch-to.el
;;; Content-Transfer-Encoding: 8bit

;;; switch-to.el --- use M-function keys to cycle buffers

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: Benjamin Drieu <benj@april.org>
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Same conditions apply to this program

;;; Version:

;; $Id: switch-to.el,v 1.3 2002/12/18 14:54:02 benj Exp $

;;; Commentary:

;; Getting tired of switching to buffers by typing their names, even
;; with some help from tab completion?  This small program allows you
;; to define a special behavior to M-<function key> keys.

;; i.e. you can switch to all buffers visiting elisp code by cycling
;; all buffers matching "\.el$".

;; This was the initial objective of that code, but it was stupid to
;; simply switch to the *Group* buffer if I wished to read my mail.
;; Recent versions of this package allow you to evaluate elisp
;; expressions instead (i.e "(gnus)"), and define the function
;; "switch-to-matched-buffer" to switch to buffer according to their
;; name.

;; To install this package, just insert following lines in your .emacs
;; file.  In that example, M-f1 switch to a shell buffer (shell-toggle
;; is needed), M-f2 invokes gnus and M-f3, M-f4, M-f5 switch
;; respectively to Perl, Emacs-Lisp and C buffers.

;; (autoload 'switch-to-init "switch-to")
;; (switch-to-init '((shell-toggle nil)
;;                  (gnus)
;;                  (switch-to-matched-buffer-in-one-window "\.pl$")
;;                  (switch-to-matched-buffer "\.el$")
;;                  (switch-to-matched-buffer "\.c$")))

;;; Code:

(defun switch-to-matched-buffer (regexp &optional arg)
  (interactive "sSwitch to next buffer matched by: ")
  (let ((l (switch-to-make-buffer-list regexp (buffer-list))))
    (if l
	(switch-to-buffer (or
			   (switch-to-get-buffer l)
			   (car l)) 'NORECORD)
      (princ (format "No buffer matching \"%s\"" regexp)))))


(defun switch-to-matched-buffer-in-one-window (regexp &optional arg)
  (interactive "sSwitch to next buffer matched by: ")
  (let ((l (switch-to-make-buffer-list regexp (buffer-list))))
    (if l
	(let (w
	      (buffer-list (mapcar (lambda (w) (window-buffer w)) (window-list))))
	  (while buffer-list
	    (if (member (buffer-name (car buffer-list)) l)
		(setq w (get-buffer-window (get-buffer (car buffer-list)))))
	    (setq buffer-list (cdr buffer-list)))
	  (if w
	      (progn 
		(select-window w)
		(switch-to-buffer (or
				   (switch-to-get-buffer l)
				   (car l)) 'NORECORD))
	    (switch-to-make-window (car l))))    
      (princ (format "No buffer matching \"%s\"" regexp)))))


(defun switch-to-make-window (buffer)
  (if (window-minibuffer-p)
      (other-frame -1))
    
  (let* ((this-buffer (current-buffer))
	 (this-window (selected-window))
	 (disp-buf (set-buffer (get-buffer-create buffer))))
    
    (if (cdr (assq 'unsplittable (frame-parameters)))
	(display-buffer disp-buf)
      (unless (or (special-display-p (buffer-name disp-buf))
		  (same-window-p (buffer-name disp-buf))
		  (get-buffer-window buffer))
	(erc-select-lowest-window)
      (if (<= (window-height) 10)
	  (switch-to-buffer buffer)
	(if (not (window-minibuffer-p))
	    (split-window))
	(pop-to-buffer disp-buf))
	(shrink-window-if-larger-than-buffer (get-buffer-window disp-buf t))
	(if (> (window-height (get-buffer-window disp-buf t)) 10)
	    (shrink-window (- (window-height (get-buffer-window disp-buf t)) 10)))))))


(defun switch-to-make-buffer-list (regexp l)
  (if (null l)
      nil
    (if (string-match regexp (buffer-name (car l)))
	(append
	 (list (buffer-name (car l)))
	 (switch-to-make-buffer-list regexp (cdr l)))
      (switch-to-make-buffer-list regexp (cdr l)))))

    
(defun switch-to-get-buffer (l &optional buffer)
  (cond 
   ((null l)
    nil)
   ((string= (car l) (or buffer (buffer-name)))
    (cadr l))
   ((switch-to-get-buffer (cdr l)))))


;;;###autoload
(defun switch-to-init (l &optional n)
  "Map a bunch of keys (M-f1 to M-fn) to elisp commands specified as arguments"
  (if l
    (let ((function (intern (format "switch-to-function-%d" (or n 1))))
	  (silly (intern (format "M-f%d" (or n 1)))))
      (fset function (list 'lambda () '(interactive) (car l)))
      (global-set-key `[,silly] function)
      (switch-to-init (cdr l) (1+ (or n 1))))))

(provide 'switch-to)

;;; switch-to.el ends here

;;; --=-=-=--

