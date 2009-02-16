;;; -*- Mode: Emacs-Lisp -*-

;;; custom-ilisp.el --
;;; ILISP Customization.
;;; WARNING!  This file is not loaded yet.  Loading it may cause some
;;; problems.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: custom-ilisp.el,v 1.2 2001/05/12 22:10:53 marcoxa Exp $

(defgroup ILisp nil
  "The ILisp Inferior Lisp System."
  :group 'programming
  :group 'lisp)

(defgroup ILisp-interaction nil
  "ILisp interaction customization."
  :group 'ILisp)

(defcustom ilisp-*prefix* "\C-z"
  "Prefix sequence for ILisp commands."
  :group 'ILisp-interaction
  )

(defcustom lisp-no-popper 'message
  "*T if all output goes to the inferior LISP rather than in a pop-up window.
Use 'message' if you want output of one line to go to the echo area
(usually the Minibuffer) or to a pop-up window if more.  You should
probably also set 'comint-always-scroll' to T as well so that output is
always visible."
  :group 'ILisp-interaction
  :options '(nil t message)
  )


;;; end of file -- custom-ilisp.el -*-

