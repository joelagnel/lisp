;; -*- auto-recompile: t -*-
;;; findutils.el --- not released yet.. but works.. elisp find utilities.
;; Time-stamp: <2003-04-04 16:50:37 deego>
;; Copyright (C) 2002 D. Goel
;; Copyright (C) 2002 Free Software Foundation, Inc.
;; Emacs Lisp Archive entry
;; Filename: findutils.el
;; Package: findutils
;; Author: D. Goel <deego@glue.umd.edu>
;; Version: 0.3.3
;; Author's homepage: http://www.glue.umd.edu/~deego
;; For latest version: 

(defvar findutils-home-page  "http://www.glue.umd.edu/~deego")


 
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
(defvar findutils-quick-start
  "Help..."
)

;;;###autoload
(defun findutils-quick-start ()
  "Provides electric help regarding `findutils-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert findutils-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar findutils-introduction
  "Help..."
)

;;;###autoload
(defun findutils-introduction ()
  "Provides electric help regarding `findutils-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert findutils-introduction) nil) "*doc*"))

;;; Commentary:
(defvar findutils-commentary
  "Help..."
)

;;;###autoload
(defun findutils-commentary ()
  "Provides electric help regarding `findutils-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert findutils-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defvar findutils-new-features
  "Help..."
)

;;;###autoload
(defun findutils-new-features ()
  "Provides electric help regarding `findutils-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert findutils-new-features) nil) "*doc*"))

;;; TO DO:
(defvar findutils-todo
  "Help..."
)

;;;###autoload
(defun findutils-todo ()
  "Provides electric help regarding `findutils-todo'."
  (interactive)
  (with-electric-help
   '(lambda () (insert findutils-todo) nil) "*doc*"))

(defvar findutils-version "0.3.3")

;;==========================================
;;; Code:

(defcustom findutils-before-load-hooks nil "")
(defcustom findutils-after-load-hooks nil "")
(run-hooks 'findutils-before-load-hooks)


(require 'dired)
(require 'find-lisp)

(defvar findutils-lisp-regexp "")

(defun findutils-always-true-pred (&rest args)
  t)
(defun findutils-always-false-pred (&rest args)
  nil)

(defun findutils-nondirectoryp-pred (file dir )
  (let ((foo (concat dir file)))
    (and (not (file-directory-p foo))
	 (string-match findutils-lisp-regexp foo)
	 t)))
	 

(defcustom findutils-exclude-list nil "")

(defun findutils-directoryp-pred (file dir )
  "For exclude-list.. comparison is done using expand-file-name and
directory-file-name.....
the advantage, hopefully,is that:
/home/ = /home.
/home/deego = ~ 
"
  (and (file-directory-p (concat dir file))
       (string-match findutils-lisp-regexp (concat dir file))
       (not (member
	     (directory-file-name (expand-file-name file dir))
	     (mapcar '(lambda (arg)
			(directory-file-name (expand-file-name arg)))
		     findutils-exclude-list)))))
  
  

;;;###autoload
(defun findutils-top-files (dir &optional regexp)
  "Does not descend...
When regexp, matches only the files satisfying regexp."
  (unless regexp (setq regexp ""))
  (let ((findutils--lisp-regexp regexp))
    (find-lisp-find-files-internal
     dir
     'findutils-nondirectoryp-pred
     'findutils-always-false-pred
    )))


;;;###autoload
(defun findutils-top-directories (dir &optional regexp)
  "Does not descend...
When regexp, matches only the files satisfying regexp."
  (unless regexp (setq regexp ""))
  (let ((findutils-lisp-regexp regexp))
    (find-lisp-find-files-internal
     dir
     'findutils-directoryp-pred
     'findutils-always-false-pred
     )))



;;;###autoload
(defun findutils-all-subdirectories (dir &optional not-self-p not-parent-p)
  "Descends..

note that because of the way findutils works.. the last directory in
this list will be the directory itself..  moreover, the penultimate
one will be the parent-directory... 

if you do not like that, one option is to simply remove them from the
list so obtained..  A more robust option is to specify variables
not-self-p and not-parent-p above..  I suspect your most commmon
invocation will look like: 
	   \(findutils-all-subdirectories <name> nil t\).  


"
  (let ((find-lisp-regexp "")
	(findutils-exclude-list (copy-list findutils-exclude-list))
	(exp-name (expand-file-name dir)))
    (when not-self-p
      (add-to-list 'findutils-exclude-list
		   exp-name))
    (when not-parent-p
      (add-to-list 
       'findutils-exclude-list 
       (expand-file-name (file-name-directory exp-name))))
    (find-lisp-find-files-internal
     dir
     'findutils-directoryp-pred
     'findutils-always-false-pred
     )))





(provide 'findutils)
(run-hooks 'findutils-after-load-hooks)



;;; findutils.el ends here
