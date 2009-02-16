;;; Saved through ges-version 0.3.3dev at 2003-05-15 10:04
;;; From: paolino <paolino.gnu@disi.unige.it>
;;; Subject: Re: multi-mode-mode.el
;;; Newsgroups: gnu.emacs.sources
;;; Date: Thu, 15 May 2003 08:04:31 GMT
;;; Organization: [Infostrada]

;;; multi-mode-mode.el --- Switches between major modes, depending on position of point in file

;; Copyright (C) 2003 Paolo Gianrossi

;; Emacs Lisp Archive Entry
;; Filename: multi-mode-mode.el
;; Author: Paolo Gianrossi <paolino.gnu@disi.unige.it>
;; Maintainer: Paolo Gianrossi <paolino.gnu@disi.unige.it>
;; Version: 1.0
;; Created: 04/17/2003
;; Revised: 05/12/2003
;; Keywords: major mode switching 
;; Description: A way to switch between major modes depending on position of point
;; URL: http://digilander.iol.it/linsky/emacs/

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA



;;; Commentary:

;; multi-mode-mode is a fairly easy way to have the correct major mode
;; anytime, even when editing files where different kinds of code are held
;; together. A paradigmatic situation is e.g. in bash scripts which
;; create, say, a C header file or a Makefile. 


;;; Installation:

;;   Just compile `multi-mode-mode.el' and put it in your load-path.

;; Add to your `~/.emacs.el':
;;  (load-library "multi-mode-mode")
;;  ; uncomment following to activate multi-mode-mode at startup	       
;;  ; (multi-mode-mode 1)						       
;;  ; uncomment following and bind to anything you like to quickly change mode 
;;  ;(global-set-key [f9] ' multi-mode-cond-switch)

;; Usage: 

;; You have to manually insert the mode-switch markers in the files
;; you want this to work in. For example, if you have a large
;; scratch-file like I do (been keeping one since 1997, it's now >
;; 27000 lines and still growing) and want to "mark" different code
;; snippets for easy mode-switching, it might look like this:

;; ---- snip ----

;; -*-Mode: c -*-

;; int some_c_code(char* v, int c);

;; -*-Mode: lisp -*-

;; (defun my-defun ()
;;   (interactive)
;;   (message "my defun is very nice"))

;; ---- snap ----
;; (Thanks to Barman Brakjoller for this)

;;; TODO

;; * find a way to make the check happen whenever you move the point
;; * better format the "wrong mode string warning"



(defvar multi-mode-mode t
  "Mode variable to set multi-mode")

(defvar multi-mode-mode-prefix
  "-\*-Mode:[ \t]*"
  "*Left delimiter rexp for the mode string."
)

(defvar multi-mode-mode-suffix
  "[\t ]*-\*-"
  "*Right delimiter rexp for the mode string."
)

(defvar multi-mode-mode-rexp
  (concat "\\(" multi-mode-mode-prefix "\\)\\(.*\\)\\(" multi-mode-mode-suffix "\\)")
  "*Rexp for the mode string."
)

(defvar multi-mode-modes-alist '(
			      ("c" . (c-mode))
			      ("C" . (c-mode))
			      ( "make" .(makefile-mode))
			      ( "makefile" .(makefile-mode))
			      ( "text" .(text-mode))
			      ( "lisp" .(lisp-mode))
			      ( "interlisp" . (lisp-interaction-mode))
			      ( "sh" . (sh-mode))
			      ( "shell" . (sh-mode))
			      )
  "*Alist containing couples modestring-mode function"
      )



(defun multi-mode-cond-switch ()
  "Switch to defined mode if it has to"
  (interactive) 
  (if multi-mode-mode 
      (multi-mode-switch-to-mode) 
    ) 
) 

(defun multi-mode-switch-to-mode ()
  "Switches to defined mode"
  (interactive)
  (save-excursion
    (save-restriction
      (save-match-data
	(if  (re-search-backward multi-mode-mode-rexp nil t nil)
      (let ((gop t) str (list multi-mode-modes-alist))
	(setq str (match-string 2))
	(while (and list gop)
	  (if (string-match (car (car list)) str)
	      (progn 
		(eval (cdr (car list))) 
		(setq gop nil)))
	  (setq list (cdr list)) )
	(if (null list)
	    (message (format "Mode %s unknown!" str ) )
	  )
	)
      )
	)
      )
    )
  )



(defun multi-mode-mode (&optional ARG) 
  "Multi-mode minor mode"
  (interactive "P")
  (setq multi-mode-mode
        (if (null ARG)
            (not multi-mode-mode)
          (> (prefix-numeric-value ARG) 0)))
  )


 (if (not (assq 'multi-mode-mode minor-mode-alist))
     (setq minor-mode-alist
           (cons '(multi-mode-mode " MultiMode")
                 minor-mode-alist)))

(provide 'multi-mode-mode)

;;; -- 
;;; "I don't think so" (as Descartes said disappearing)
;;; Paolo Gianrossi
;;; Students' Representative @ DISI
;;; University of Genoa

;;; Co-administrator of the GNU@DISI project
;;; 1999s079@educ.disi.unige.it, paolino@linux.it, 
;;; paolino@member.fsf.org, paolino.gnu@disi.unige.it

