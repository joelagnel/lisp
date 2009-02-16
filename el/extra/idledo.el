;; -*- auto-recompile: t -*-
;;; idledo.el --- do stuff when emacs is idle..
;; Time-stamp: <2002-05-04 04:09:03 deego>
;; Copyright (C) Deepak Goel 2001
;; Emacs Lisp Archive entry
;; Filename: idledo.el
;; Package: idledo
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.1.3
;; Author's homepage: http://www.glue.umd.edu/~deego
;; REQUIRES: timerfunctions.el 1.2.7 or later.
;; ALSO uses: emacs' 'cl (for all the backquoting..)
;; For latest version:

(defvar idledo-home-page  "http://www.glue.umd.edu/~deego")

;; Requires: timerfunctions.el
;; See also: Jari's tinyload.el (implements an idle-loading of files..)

 
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
 



;; Quick start:
(defvar idledo-quick-start
  "Drop idledo.el and timerfunctions.el somewhere in your load-path. In your
.emacs, type \(require 'idledo\) and \(require 'timerfunctions\) Create
idledo-list, either by hand, or by using one of the many functions
provided.  Then, write \(idledo-start\), and idledo will do the tasks
mentioned in the idledo-list whenever emacs is idle.

PS: timerfunctions.el can be obtained from:
http://www.glue.umd.edu/~deego/emacspub/lisp-mine/timerfunctions/   
"
)

(defun idledo-quick-start ()
  "Provides electric help for function `idledo-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar idledo-introduction
  "Idledo does stuff for you when emacs is idle.

The actions can be simple one-time actions or repetitive.  You can
include as many actions as you want.  Thus, if you leave emacs running
for sometime, take a trip and come back, you can now start your gnus or
eshell or w3 instantly.. When you are using gnus, you can check mail
periodically..  

Even though it seems to do all i can fancy needing, idledo will
nonetheless someday be interfaced with a prioritizer, which will include
all sorts of enhanced capabilites, like enhanced repetitive actions, weighting
of actions etc.

See also M-x idledo-commentary
"
)

(defun idledo-introduction ()
  "Provides electric help for function `idledo-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-introduction) nil) "*doc*"))

;;; Commentary:
(defvar idledo-commentary
  "First type M-x idledo-introduction.  
Also see M-x idledo-quick-start

You give idledo a list of commands, and it will eval them when emacs is
idle and you are away..  Thus, if you take a hike and come back, your
w3, gnus, eshell should all start instantly..  Your gnus-news should
be checked periodically for you.. and *Group* buffer updated.. of
course, you have to set this all up :/\)

If emacs is idle *not* because you are away, but because you are
deeply absorbed using info, you probably don't want idledo springing into
action and loading eshell for you.. So, idledo tries to alert you before
loading anything, and gives you enough time to cancel any action
before it is taken..

As an example, see the function idledo-deepak.  I call that function
from my .emacs as follows..

/(idledo-deepak/)

where:
\(defun gf-periodically-check-mail-my \(\)
  ;; this let so that no copies made when new mail checked..
  \(let \(\(ebackup-max-copies 0\)\)
    \(require 'idledo\)
    ;; this is an example of using idledo to do repetitive stuff..
    \(idledo-add-periodic-action-to-beginning-crude
     ;; the function below is defined to essentially call
     ;; gnus-group-get-new-news
     '\(gf-get-new-news-once-my\)\)\)\)
"
)

(defun idledo-commentary ()
  "Provides electric help for function `idledo-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-commentary) nil) "*doc*"))

;;; History:

;;; New features:
(defvar idledo-new-features
  "0.1 new features:
* Now called idledo, to avoid a name-conflict with another package.
  Sorry about that, and Thanks to all who pointed this out.
* Macros like ido-add-require now called idledo-require.
* Minor bug fixed in idledo-add-periodic-action-to-beginning-crude
"
)

(defun idledo-new-features ()
  "Provides electric help for function `idledo-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert idledo-new-features) nil) "*doc*"))

(defvar idledo-version "0.1.3")

(defvar idledo--todo 
"TODO: 
* Ideally, one should be able to cancel the timer if idledo-list
   becomes nil.

* Write a prioritizer, and interface the same with idledo. The priotizer
  should. among other things like weights and \(arbitrarily specified\)
  repetitivity, try to support different idle times for different
  tasks..
" )


;;==========================================
;;; Code:

(require 'cl)


(defvar idledo-list nil
  "A list of actions to perform..")

(defvar idledo-active-p nil
  "If t, no more idledo's can be initiated.. 
The aim is to only have one idledo active at a time.

Why?  I don't know.  You can easily setq this to nil, and start yet
another idledo-start if you want.  

Why do i want only one idledo at a time?  My experience is that \(GNU\)
emacs bahaves unpredictably if the activation of 2 or more timers
collide... maybe i am wrong?  It seems to me that sometimes, both get
executed, someimtes one, and sometimes none..  Although the one or
none situations seem to be rare, each of thses situations can be
potentially bad..particularly if: Suppose the timer is a
self-reinforcing timer \(as can be done by calls to
tf-run-with-idle-timer\).  Then, the very first time it fails to get
executed, the process gets killed and you want get those cherished
repetitions as long as emacs remains idle..


")

(defvar idledo-interval 300
  "The interval to wait the first time emacs goes idle..
An additional small interval will be allowed to enable the user to
cancel the action. 

Note that you can assign to this this interval any expression that
will be eval'ed at run-time \(see timerfunctions.el for more details..\)
" )

(defvar idledo-subsequent-interval 1
  "If emacs remains idle, wait this much time more before performing
the next action..
Note that you can assign to this this interval any expression that
will be eval'ed at run-time \(see timerfunctions.el for more details..\)
")

(defvar idledo-small-interval  10
  "Before beginning any action, idledo will flash a warning, and will
wait for these many seconds.. if you do something in this time, the
action will be cancelled. 

Note that you can assign to this this interval any expression that
will be eval'ed at run-time \(see timerfunctions.el for more details..\)
")


(defun idledo-start ()
  "This starts idledo. See also idledo-active-p."
  (interactive)
  (if (not idledo-active-p)
      (progn
	(setq idledo-active-p t)
	(tf-run-with-idle-timer 
	 'idledo-interval t
	 'idledo-subsequent-interval 
	 t nil 
	 'idledo-one-action))
    (error "Idledo is already active")))
    
(defvar idledo-done-interval 1
  "Idledo will wait for this much time before flashing a 'done-action'
message "
)


(defvar idledo-action-imminent-string 
  "idledo imminent unless keypress ---> ")

(defun idledo-one-action ()
  "Internal.
Does one instance of processing of action. "
  (when (not (null idledo-list))
    (message 
     (concat idledo-action-imminent-string
	     (idledo-shorten (format "%S" (car idledo-list)))))
    (if (sit-for idledo-small-interval)
	(progn
	  (message 
	   (concat "IDLEDO doing action.."
		   (idledo-shorten (format "%S" (car idledo-list)))))
	  (let ((carval (car idledo-list)))
	    (setq idledo-list (cdr idledo-list))
	    (idledo-ignore-errors (eval  carval)))
	  (sit-for idledo-done-interval)
	  (message "%S more idledo(s) remainig.. "
		   (length idledo-list)))
      (message 
       (concat "IDLEDO's action canceled.."
	       (idledo-shorten (format "%S" (car idledo-list)))))
      )))
  
  
(defun idledo-add-periodic-action-crude (action)
  "Is a crude mechanism for adding action to the idledo-list and make it
repetitive.  ACTION is a list which will be evaled to perform an
eval. 
Note that the ACTION this way is added to the END of idledo-list.
And ACTION is added to list no matter what (even if there is a similar
action already waiting in the list).
"
  (setq 
   idledo-list
   (append 
    idledo-list
    (list
     `(progn
	,action
	(idledo-add-periodic-action-crude
	 ,action))))))

(defun idledo-add-periodic-action-to-beginning-crude (action)
  "Is a crude mechanism for adding action to the idledo-list and make it
periodic.  ACTION is a list which will be evaled to perform an
eval. 
Note that the ACTION this way is added to the BEGINNING and subsequent
calls are also added to the beginning of the list.
And ACTION is added to list no matter what (even if there is a similar
action already waiting in the list).
"
  (idledo-add-action-forced
   `(progn
      ,action
      (idledo-add-periodic-action-to-beginning-crude
       (quote ,action)))))


   

;;;###autoload
(defun idledo-add-to-end-of-list (list-var element)
  "Like add-to-list, but adds at the end, if added at all."
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var (append  (symbol-value list-var) (list element)))))

(defun idledo-add-action (action)
  "Action is an expression to be evaled.  Action is added at the
beginning if at all. See similar commands too."
  (add-to-list 'idledo-list action))

(defun idledo-add-action-forced (action)
  (setq idledo-list (cons action idledo-list)))

(defun idledo-add-action-at-end (&rest actions)
  (mapcar 
   (lambda (action)
     (idledo-add-to-end-of-list 'idledo-list action))
   actions))

(defmacro idledo-load (&rest files)
  (cons 'progn
	(mapcar
	 (lambda (arg)
	   `(idledo-add-action-at-end '(load ,arg)))
	 files)))

;;; 2001-11-03 T13:42:01-0500 (Saturday)    Deepak Goel
(defmacro idledo-load-now (&rest files)
  (cons 'progn
	(mapcar
	 (lambda (arg)
	   `(idledo-add-action '(load ,arg)))
	 files)))


(defmacro idledo-require (&rest features)
  (cons 'progn
	(mapcar 
	 (lambda (arg)
	   `(idledo-add-action-at-end '(require ,arg)))
	 features)))


(defmacro idledo-require-now (feature)
  `(idledo-add-action '(require ,feature)))

(defun idledo-add-action-at-end-forced (action)
  (setq idledo-list (append idledo-list (list action))))

(defun idledo-initialize (initial-list)
  (setq idledo-list initial-list))

(defun idledo-remove-action (action)
  (idledo-remove-from-list 'idledo-list action))

(defun idledo-remove-from-list (listname elt)
  "INTERNAL"
  (set listname (idledo-list-without-element 
		 (eval listname)
		 elt)))

(defun idledo-list-without-element (list elt)
  "INTERNAL"
  (if (null list)
      list
    (if (equal (car list) elt)
	(idledo-list-without-element (cdr list) elt)
      (cons
       (car list)
       (idledo-list-without-element
	(cdr list) elt)))))



(defun idledo-shorten (string)
  "Internal, return a shortened version with no newlines.
Internal, returns a shortened version of STRING with no newlines."
  (let
      ((string-no-enter
	(with-temp-buffer
	  (insert string)
	  (goto-char (point-min))
	  (while (search-forward "\n" nil t)
	    (replace-match " " nil t))
	  (buffer-substring (point-min) (point-max)))))
    (if (> (length string-no-enter) 55)
	(substring string-no-enter 0 55)
		   string-no-enter)))


(defmacro idledo-ignore-errors (&rest body)
  "Like ignore-errors, but tells the error..
Improved for me by Kalle on 7/3/01:
 * used backquote: something i should have done long ago.
 * removed the progn: condition-case automatically has one..
 * made sure that the return is nil.. just as it is in ignore-errors. "
  (let ((err (gensym)))
    `(condition-case ,err (progn ,@body)
       (error
	(ding t)
	(ding t)
	(ding t)
	(message "IGNORED ERROR: %s" (error-message-string ,err))
	(sit-for 1)
	nil))))


;;;###autoload
(defun idledo-deepak ()
  "Sample of code to include in your .emacs.. 
See this and `idledo-deepak-setup'.
Define a similar function idledo-yourname for yourself in your .emacs,
and call it in yr .emacs by inserting (idledo-yourname) somewhere. "
  (interactive)
  (setq idledo-list nil)
  (idledo-add-action '(idledo-deepak-setup))
  
  
  (setq idledo-action-imminent-string 
	"idledo imminent--> ")
  (idledo-start)

)

  




;;;###autoload
(defun idledo-length-list ()
  "For you to quickly find the length of idledo-list..
If you use idledo bigtime, you will frequently find yourself wanting
to find out the length.. and you don't want to eval that parenthesised
expression all the time.. perhaps.. "
  (interactive)
  (message (format "%S" (length idledo-list))))

(defun idledo-deepak-setup ()
  " Called by idledo-deepak.  This extra step is taken so that setting
up idledo itself takes place only when emacs has gone idle..
"
  ;; The preference in all of below should be to load stuff that takes
  ;; time asap.. small libraries can always be loaded later.. or even
  ;; if they are not loaded, they do not make the user wait anyways
  ;; when they finally get  loaded..

  ;; once bbdb is loaded.. let's get the frobnicating stuff over with..
  (idledo-add-action 
   '(progn
      (miniedit-install)
      nil))

  (idledo-require 'bbdb 'bbdb-com 'bbdb-gnus)
  (idledo-add-action 
   '(progn
      (unless (file-locked-p "~/emacs/.bbdb")
	(bbdb-records))
      nil))
  (idledo-require-now 'mailabbrev)
  (idledo-load "gnus-functions-my")
  (idledo-load "macros-my")
  (idledo-add-action '(load "aliases-my"))
  (idledo-load "mode-hook-functions-my")
  (idledo-require 'disp-table)
  (idledo-require 'gnus-score 'gnus 'gnus-msg)
  (idledo-require 'gnus-cache)
  (idledo-require 'gnus-ml 'gnus-cite)
  (idledo-require 'timerfunctions)


  (idledo-require 'esh-mode
		  'em-alias)

  (idledo-require 'em-banner 'em-basic 'em-cmpl 'em-dirs 'em-glob
		  'em-hist 'em-ls 'em-prompt 'em-script 'em-term 
		  'em-xtra 'etags
		  'ange-ftp 
		  ;; no longer needed since pcomplete is now bundled
		  ;; with emacs (21..)
		  ;;'pcmpl-auto 
		  'pcomplete 
		  ;; 2002-05-02 T11:57:07-0400 (Thursday)    D. Goel
		  'shellhist
		  ;; 2002-05-02 T11:57:25-0400 (Thursday)    D. Goel
		  'pcmpl-unix
		  
		  ;; no longer needed since eshell is now bundled
		  ;; with emacs (21)
		  ;;'eshell-auto 

		  'em-unix 'bytecomp 'eshell 'runshell )
  (idledo-load "cl-seq")

  (idledo-require 'autokey)
  (idledo-require 'thingatpt 'ispell 'info)
  (idledo-require 'elder)
  
  (idledo-require 'mail-extr )
  (idledo-require 'autorevert 'view)
  (idledo-require 'time-stamp )
  (idledo-load "kinsoku")
  (idledo-require 'edlib )
  (idledo-require 'phonemode)
  
  ;; bytecomp should be required before this...
  (idledo-add-action-at-end '(load "byte-opt"))
  
  ;;(idledo-load 'tex-mode)
  (idledo-require 'boxquote)
  (idledo-require 'dired)
  (idledo-require 'bytecomp)
  (idledo-require 'find-func)
  (idledo-require 'diff 'diff-mode)
  (idledo-require 'add-log)
  (idledo-require 'calendar)
  (idledo-require 'mule-util)
  (idledo-require 'cal-move)
  (idledo-require 'advice)
  (idledo-require 'browse-kill-ring)
  (idledo-require 'debug)
  (idledo-require 'ell)
  (idledo-require 'table)
  (idledo-require 'tabify)
  
  ;; 2002-04-25 T15:43:21-0400 (Thursday)    Deepak Goel
  ;; this will shorten the time it takes to find a tag..
  (idledo-add-action 
   '(progn
      (visit-tags-table  "~/TAGS")
      nil))
  (idledo-require 'gnus-cus)
  )

(provide 'idledo)

;;; idledo.el ends here
