;;; msgp.el --- make echo area messages persistent.
;; Time-stamp: <2004-05-06 10:56:12 deego>
;; Copyright (C) 2004 D. Goel
;; Emacs Lisp Archive entry
;; Filename: msgp.el
;; Package: msgp
;; Author: D. Goel <deego@gnufans.org>
;; Keywords:
;; Version:
;; URL: http://gnufans.net/~deego
;; For latest version:

(defconst msgp-home-page
  "http://gnufans.net/~deego")


 
;; This file is NOT (yet) part of GNU Emacs.
 
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

;; See also:


;; Quick start:
(defconst msgp-quick-start
  "Help...
Add (msgp-install) to .emacs
"
)

(defun msgp-quick-start ()
  "Provides electric help from variable `msgp-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert msgp-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst msgp-introduction
  "Help...
Motivated by a question from TD on #emacs. 
"
)

;;;###autoload
(defun msgp-introduction ()
  "Provides electric help from variable `msgp-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert msgp-introduction) nil) "*doc*"))

(defconst msgp-version "0.0-DUMMY")


;; todo: 
;; remove quotes before lambdas.
;; disable advice by default.

;;; Real Code:
(defvar msgp-last nil)

(defadvice message (after msgp-save () )
  ""
  (when (current-message) (setq msgp-last (current-message))))

(defun msgp-install ()
  (interactive)
  (ad-activate 'message)
  (add-hook 'post-command-hook
	    '(lambda ()
	       (unless (current-message)
		 (message msgp-last)))))
  





(provide 'msgp)
(run-hooks 'msgp-after-load-hook)



;;; msgp.el ends here
