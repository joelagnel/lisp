
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:




(defvar phone-n2l-modeline-string " N->L"
  "The string displayed in the mode line when in phone-n2l mode.")


(defvar phone-l2n-modeline-string " L->N"
  "The string displayed in the mode line when in phone-l2n mode.")

;; ;;;###autoload
;; (defvar phone-n2l-mode nil 
;;   "See the function with the same name")

;; ;;;###autoload
;; (defvar phone-l2n-mode nil
;;   "See the function with the same name")  

;; (make-variable-buffer-local 'phone-n2l-mode)
;; (make-variable-buffer-local 'phone-n2l-mode)



(defun phone-l2n-insert-command ()
  "Just a test right now, by inserting an extra 1..."
  (interactive)
  (self-insert-command 1)
  (insert "1")
  )

(defun phone-n2l-insert-command ()
  "Will do more stuff.. just tests right now.. by inserting an extra 1.."
  (interactive)
  (self-insert-command 1)
  (insert "1")
  )

(easy-mmode-define-minor-mode 
 phone-n2l-mode "TEST" nil
 phone-n2l-modeline-string 
 (cons
  'keymap
  (let ((phone-n2l-tmplist nil))
    (do 
	((i 48 (+ i 1)))
	((> i 57) nil)
      (add-to-list 'phone-n2l-tmplist (cons 
				       i
				       'phone-n2l-insert-command)))
    phone-n2l-tmplist)))

(easy-mmode-define-minor-mode
 phone-l2n-mode
  "Toggle phone-l2n mode.
With arg, turn phone-l2n mode on iff arg is positive.

When this mode is active, entering letters shall lead to the
entering of the various possible associated numbers.  You can cycle
through the various choices (numbers/letters) by pressing TAB.
"
  nil  phone-n2l-modeline-string
  (cons
   'keymap
   (append
    (let ((phone-l2n-tmplist nil))
      (do 
	  ((i 65 (+ i 1)))
	  ((> i 90) nil)
	(add-to-list 'phone-l2n-tmplist (cons 
					 i
					 'phone-l2n-insert-command)))
      phone-l2n-tmplist)
    (let ((phone-l2n-tmplist nil))
      (do 
	  ((i 97 (+ i 1)))
	  ((> i 113) nil)
	(add-to-list 'phone-l2n-tmplist (cons 
					 i
					 'phone-l2n-insert-command)))
      phone-l2n-tmplist))))





;;; ;;;###autoload
;;; (defun phone-n2l-mode (arg)
;;;   "Toggle phone-n2l mode.
;;; With arg, turn phone-n2l mode on iff arg is positive.

;;; When this mode is active, entering numbers shall lead to the
;;; entering of the various possible associated letters.  You can cycle
;;; through the various choices by pressing TAB.
;;; "

;;;   (interactive "P")
;;;   (setq phone-n2l-mode
;;; 	(if (if (null arg) (not phone-n2l-mode)
;;; 	      (> (prefix-numeric-value arg) 0))
;;; 	    'phone-n2l-mode))
;;;   (force-mode-line-update))



; ;;;###autoload
; (defun phone-l2n-mode (arg)
;   "Toggle phone-l2n mode.
; With arg, turn phone-l2n mode on iff arg is positive.

; When this mode is active, entering letters shall lead to the
; entering of the various possible associated numbers.  You can cycle
; through the various choices (numbers/letters) by pressing TAB.

; "
;   (interactive "P")
;   (setq phone-l2n-mode
; 	(if (if (null arg) (not phone-l2n-mode)
; 	      (> (prefix-numeric-value arg) 0))
; 	    'phone-l2n-mode))
;   (force-mode-line-update))







;;; phone.el ends here
