;; STATUS: UNFINISHED
;;; heretic.el --- psychoanalyse heretics 
;; Time-stamp: <2003-03-01 21:39:27 deego>
;; Copyright (C) 2003 D. Goel
;; Emacs Lisp Archive entry
;; Filename: heretic.el
;; Package: heretic
;; Author: D. Goel <deego@glue.umd.edu>
;; Keywords:
;; Version:
;; Author's homepage: http://deego.gnufans.org/~deego
;; For latest version:

(defconst heretic-home-page
  "http://deego.gnufans.org/~deego")


 
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

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defconst heretic-introduction
  "The world is full of heretics who don't believe in the ONE TRUE
RELIGION.  Sometimes they even get the nerve to advocate their cults
on our holy newsgroup alt.religion.emacs.  They are clearly in need of
*serious* psychoanalysis.  Heretic.el helps them communicate with
the TRUE ONE who can examine these lost souls. (M-x heretic-reply-gnus)

But heretics are not limited merely to newsgroups, nor are believers
limited to gnus.  heretic.el helps believers deal with heretics in all
sorts of situations (M-x heretic-analyze-region and M-x
heretic-insert-and generally, heretic-analysis). 

Tested only with gnu emacs 21.
" )2

;;;###autoload
(defun heretic-introduction ()
  "Provides electric help from variable `heretic-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert heretic-introduction) nil) "*doc*"))

(defconst heretic-version "0.0-DUMMY")
;;==========================================
;;; Requires:
(eval-when-compile (require 'cl))

;;; Code:

(defgroup heretic nil
  "The group heretic."
  :group 'applications)
(defcustom heretic-before-load-hooks nil
  "Hooks to run before loading heretic."
  :group 'heretic)
(defcustom heretic-after-load-hooks nil
  "Hooks to run after loading heretic."
  :group 'heretic)
(run-hooks 'heretic-before-load-hooks)



;; Real Code 
;; invoke THE DOCTOR 
(require 'doctor)

(defvar heretic-analysis nil
  "A list of elements.. each element is of the form (QUESTION
REPLY). ")

(defun heretic-initialize ()
  (interactive)
  (setq heretic-analysis nil))

(defun heretic-analyze-text (text)
  (doctor)
  (save-excursion
    (set-buffer "*doctor*")
    (goto-char (point-max))
    (
)))
;;;###autoload
(defun heretic-analyze-region ()
  
  ()
)


(provide 'heretic)
(run-hooks 'heretic-after-load-hooks)



;;; heretic.el ends here
