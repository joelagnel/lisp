;; -*- auto-recompile: t -*-
;;; mlp.el --- facilitates adding multiple directories to load-path
;; Time-stamp: <2003-02-13 13:12:42 deego>
;; Copyright (C) Deepak Goel 2002
;; Emacs Lisp Archive entry
;; Filename: mlp.el
;; Package: mlp
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.1
;; URL: http://www.glue.umd.edu/~deego/emacs/emacspub/lisp-mine/mlp/
;; For latest version: 

(defvar mlp-home-page  
  "http://www.glue.umd.edu/~deego/emacs/emacspub/lisp-mine/mlp/")


 
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
;; load-pathological by Brian P. Templeton
;; 
;; Quick start:
(defvar mlp-quick-start
  "Type M-x mlp-introduction.."
)

(defun mlp-quick-start ()
  "Provides electric help for function `mlp-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert mlp-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar mlp-introduction
  "mlp (multi-load-path) facilitates adding multiple directories to
load-path.  

When emacs is being initiated, emacs has the capability to add
directories recursively.  There, emacs looks for a file subdirs.el in
the paths being added.  mlp is just an attempt to bring the smae
functionality to the user.

The main function provided is mlp-add-to-load-path (and some cousins).
When you use this function to add something to your load-path, emacs
looks for subdirs.el in that load-path, and processes it too

Depending on what you put in subdirs.el, mlp can recursively add
subdirectories to the load-path, or mlp can add any specified paths to
your load-path.  Moreover, mlp will *further* look for presence of
subdirs.el in each of the subdirectories thus added and process them,
just like emacs does at starup.

All the code here is mostly ripped off from emacs itself.  All that is
removed is 'top-level-restriction'. 

Hm, I am perhaps never gonna post this to g.e.sources.  I use it, but
perhaps the user who wants such a functionality can do add
(normal-top-level...) stuff by hand to their .emacs..
 " )

(defun mlp-introduction ()
  "Provides electric help for function `mlp-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert mlp-introduction) nil) "*doc*"))

;;; Commentary:
(defvar mlp-commentary
  "see introductiopn
"
)

(defun mlp-commentary ()
  "Provides electric help for function `mlp-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert mlp-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defvar mlp-new-features
  "Help..."
)

(defun mlp-new-features ()
  "Provides electric help for function `mlp-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert mlp-new-features) nil) "*doc*"))

(defvar mlp-version "0.1")

;;==========================================
;;; Code:

(defvar mlp-before-load-hooks nil)
(defvar mlp-after-load-hooks nil)
(run-hooks 'mlp-before-load-hooks)

;;;###autoload
(defun mlp-add-to-load-path (&rest paths)
  "Adds paths to the load-path along with processing of any
subdirs.el.."
  (mlp-add-paths-to-load-path paths))


;;;###autoload
(defun mlp-add-paths-to-load-path (paths &optional arg)
  "This function not only adds paths to load-paths, but also tries to
process the file subdirs.el if it exists in that path..
arg is passed directly to add-to-list.  This function need not be
called directly..  instead, use the easier mlp-add-to-load-path and 
mlp-add-to-end-of-load-path .. the subdirs.el largely mirrors the
emacs's default way of doing it..
"
  (unless 
      (null paths)
    (let*
	((path (car paths)))
      (add-to-list 'load-path path arg)
      (mlp-ignore-errors
       (let* ((default-directory path)
	      (subdir-path 
	       (expand-file-name "subdirs.el" path)))
	 (when (file-exists-p subdir-path)
	   (load subdir-path t nil t)))))))




(defmacro mlp-ignore-errors (&rest body)
  "Copied from ignore-errors-my, which:
Like ignore-errors, but tells the error..
Improved for me by Kalle on 7/3/01:
 * used backquote: something i was too lazy to convert my macro to..
 * removed the progn: condition-case automatically has one..
 * made sure that the return is nil.. just as it is in ignore-errors. "
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
	(ding t)
	(message "IGNORED ERROR: %s" (error-message-string ,err))
	(sit-for 1)
	nil))))


(provide 'mlp)
(run-hooks 'mlp-after-load-hooks)



;;; mlp.el ends here
