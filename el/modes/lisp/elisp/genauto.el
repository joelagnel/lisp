;;;-*- auto-recompile: t -*-
;;; genauto.el --- helps generate autoloads for your elisp packages
;;; Time-stamp: <2002-04-09 10:21:13 deego>
;; Copyright (C) Deepak Goel 2002
;; Emacs Lisp Archive entry
;; Filename: genauto.el
;; Package: genauto
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.1beta
;; For latest version: 

(defvar genauto-home-page  
  "http://www.glue.umd.edu/~deego/emacspub/lisp-mine/genauto")


 
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
;;   none

;; Quick start:
(defvar genauto-quick-start
"
Quick instructions:
In your .emacs.
(require 'genauto)
Then restart emacs (or eval the above statement). 
Then M-x genauto-doit.
Then add to emacs: (load \"~/emacs/loaddefs-my\")
"
)

;;;###autoload
(defun genauto-quick-start ()
  "Provides electric help for function `genauto-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert genauto-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar genauto-introduction
"

If you are a heavy emacs-user, you probably have more .el files
downloaded than came with emacs.  Plus you may have created tons of
your own function-definitions in various files...And you don't like to
\(require\) all of them in .emacs because that slows down the loading
of .emacs, right?  Genauto is a small utility that assists you in
generating autoload-definitions for those downloaded \(or for any
other\) files.

See also M-x genauto-quick-start. 

Please note that genauto MAY NOT work for Xemacs.  It uses the
function update-autoloads-from-directories, defined in emacs'
autoload.el, but \(perhaps\) not in Xemacs.  Patches are welcome. 


 Uwe Brauer might possibly be looking into porting genauto to Xemacs.  "




)

(defun genauto-introduction ()
"Provides electric help for function `genauto-introduction'."
(interactive)
(with-electric-help
 '(lambda () (insert genauto-introduction) nil) "*doc*"))

;;; Commentary:
(defvar genauto-commentary
"Type M-x genauto-quick-start and M-x genauto-introduction.

For further customization, see variables:
genauto-specified-directoried-p
genauto-list
genauto-predicate
genauto-file

and the various hooks.

"
)

(defun genauto-commentary ()
"Provides electric help for function `genauto-commentary'."
(interactive)
(with-electric-help
 '(lambda () (insert genauto-commentary) nil) "*doc*"))

;;; History:

;;; Bugs:

;;; New features:
(defvar genauto-new-features
"posting to g.e.sources for the first time.. so none"
)

(defun genauto-new-features ()
"Provides electric help for function `genauto-new-features'."
(interactive)
(with-electric-help
 '(lambda () (insert genauto-new-features) nil) "*doc*"))

(defvar genauto-version "0.1beta")

;;==========================================
;;; Code:
(eval-when-compile (require 'cl))
(defvar genauto-before-load-hooks nil)
(defvar genauto-after-load-hooks nil)
(run-hooks 'genauto-before-load-hooks)


(defvar genauto-specified-directories-p nil

"When non-nil, genauto-doit will generate autoloads from a
list of directories as defined in generate-list rather than just the
load-path.."  )


(defvar genauto-predicate 'genauto-predicate-default
"For example, see genauto-predicate-deepak.
The default: genauto-predicate-default, will generate genauto's from
you entire load-path, provided the particular path in question had
your username somewhere in it.  The assumption is that your downloaded
packages are somewhere in your home-directory and need autoloads to be
generated.  Whereas, for other packages that come with emacs, the
autoloads are already loaded when emacs starts.  This was just an
example. You may define your own predicate within your own .emacs... "
)

(defun genauto-predicate-default (path)
  "for doc, see the var genauto-predicate"
  (let  ((user (getenv "USER")))
    (unless (stringp user) (setq user ""))
    (and
     (string-match user (file-truename path))
     ;; this so that it works on nickel etc..
     (file-exists-p path))))


(defun genauto-predicate-deepak (path)
  "Generate autoloads only from paths that are in my home.. else emacs
should have already taken care of the autoloads.. "
  (and
   (or
    (string-match "deego" (file-truename path))
    ;; this so that it works on nickel etc..
    (string-match "dgoel" (file-truename path))
    )
   (file-exists-p path)))

(defvar genauto-list nil)

(defvar genauto-file "~/emacs/loaddefsmy.el")
 
;;;###autoload
(defun genauto-doit()
 "Generate autoloads please.
Into the file ~/emacs/loaddefsmy.el.. generates autoloads from
directories which satisfy genauto-predicate"
 (interactive)
 (require 'cl)
 (find-file genauto-file)
 ;; save the previous version in case user wants to retreive it
 (kill-region (point-min) (point-max))
 (insert ";This file is automatically generated via genauto. \n;DONOT EDIT BY HAND.\n\n")
 (write-file genauto-file)
 (let 
     ((generated-autoload-file genauto-file))
   (apply 'update-autoloads-from-directories  
	  (genauto-eliminate-nondirectories
	   (mapcan
	    (lambda (arg)
	      (if (funcall genauto-predicate arg) (list arg) nil))
	    (if genauto-specified-directories-p
		genauto-list
	      load-path)))))
   
 (goto-char (point-max))
 (insert "\n\n\n;;;Generated autoloads-file  ends here..\n")
 (write-file genauto-file)
 (byte-compile-file genauto-file)
 (load genauto-file)
 )


;;; 2002-04-08 T20:32:33-0400 (Monday)    Deepak Goel
(defun genauto-eliminate-nondirectories (path)
  (mapcan
   '(lambda (arg)
      (if (file-directory-p arg) (list arg) nil))
   path))

(run-hooks 'genauto-after-load-hooks)
(provide 'genauto)


;;;genauto.el ends here..
