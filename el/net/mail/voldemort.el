;; -*- auto-recompile: t -*-
;;; voldemort.el --- quotes from voldemort for my signature.
;; Time-stamp: <2001-07-26 07:50:23 deego>
;; Copyright (C) Deepak Goel 2001
;; Emacs Lisp Archive entry
;; Filename: voldemort.el
;; Author: Deepak Goel <dgoel@wam.umd.edu>
;; Version: 

;; Quick start:
(defvar voldemort-quick-start
  "See introduction.."
)

(defun voldemort-quick-start ()
  "Provides electric help for function `voldemort-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert voldemort-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar voldemort-introduction
  "This is a file i use in conjunction with faith.
You may not find this useful..
Helps faith choose quotes from Voldemort.. 


Voldemort is a list of quotes from he who must not be named, and i define
quote is randomly selected and inserted into my .sig...
"
)

(defun voldemort-introduction ()
  "Provides electric help for function `voldemort-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert voldemort-introduction) nil) "*doc*"))

;;; Commentary:
(defvar voldemort-commentary
  "see introduction

"
)

(defun voldemort-commentary ()
  "Provides electric help for function `voldemort-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert voldemort-commentary) nil) "*doc*"))

;;; History:

;;; New features:
(defvar voldemort-new-features
  ""
)

(defun voldemort-new-features ()
  "Provides electric help for function `voldemort-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert voldemort-new-features) nil) "*doc*"))

;;==========================================
;;; Code:


;; Are not making this autoload-able.  Because then, the defvar
;; defniniton goes into the loaddefs, and any changes i make *here* do
;; not take effect :(
(defvar voldemort-quotes 
  (list
   "There won't be anything we won't say to people to try and convince 
them that our way is the way to go."

"640K ought to be enough for anybody."

"If you can't make it good, at least make it look good." 

"I believe OS/2 is destined to be the most important operating system,
[and program] of all time. As the successor to DOS.. it creates
incredible opportunities for everyone.."

"There are people who don't like capitalism, and people who don't like 
PCs. But there's no-one who likes the PC who doesn't like Microsoft.." 

"OS/2 PM is a tremendously rich environment..  Smalltalk/V PM is the
kind of tool that will make OS/2 the successor to MS/DOS."

"Microsoft has not changed any of its plans for Windows.. we will not
include things like threads and preemptive multitasking in Windows. By
the time we added that, you would have OS/2."

"No, the best way to prepare is.. to study .. programs that other
people have written. In my case, I went to the garbage cans.. and I
fished out listings of their operating system."


"If you don't know what you need Windows NT for, you don't need it." 

"<Win 2.11> New interface closely resembles Presentation Manager,
preparing you for the wonders of OS/2!"

"Microsoft programs are generally bug-free.  [You] have to wait weeks
if not months until someone calls in with a bug in one of our
programs. 99.99% of calls turn out to be user mistakes. "

"I know not a single less irrelevant reason for an update than bugfixes. 
The reasons for updates are to present more new features."


"The next generation of interesting software will be made on a Macintosh, 
not an IBM PC." 
)
  "Quotes form voldemort.. used in my mail-signatures..")


(provide 'voldemort)

;;; voldemort.el ends here
