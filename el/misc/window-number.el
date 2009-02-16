;;; window-number.el

;; Copyright (C) 2004 Johann "Myrkraverk" Oskarsson
;; <myrkraverk@users.sourceforge.net>

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

;; Introduction
;; ============

;; Window number mode allows you to select windows by numbers.  This
;; edition now works with XEmacs as well as GNU Emacs.  The window
;; numbers do not show up in the mode-line in XEmacs yet, instead a
;; -?- is displayed.  Hopefully this can be fixed soon, but really
;; depends on XEmacs developers.

;; Installation
;; ============

;; Drop this file into your load path.  C-h v load-path RET or F1 v
;; load-path RET will help.  Then place the following lines into your
;; .emacs or ~/.xemacs/init.el and uncomment them.

;; ----------------------------------------------------------------------------

;; (autoload 'window-number-mode "window-number"
;;   "A global minor mode that enables selection of windows according to
;; numbers with the C-x C-j prefix.  Another mode,
;; `window-number-meta-mode' enables the use of the M- prefix."
;;   t)

;; (autoload 'window-number-mode "window-number"
;;   "A global minor mode that enables use of the M- prefix to select
;; windows, use `window-number-mode' to display the window numbers in
;; the mode-line."
;;   t)

;; ----------------------------------------------------------------------------

;; Then you can use M-x window-number-mode RET to turn the mode on, or
;; place (window-number-mode 1) and (window-number-meta-mode 1) into
;; your .emacs or ~/.xemacs/init.el.

;; ----------------------------------------------------------------------------

;; Code starts here.

;; ----------------------------------------------------------------------------

(defun window-number-list ()
    "Returns a list of the windows, in fixed order and the minibuffer (even
if not active) last."
    (let* ((walk-windows-start (car (set-difference
				     (window-list (selected-frame) t)
				     (window-list))))
	   (walk-windows-current walk-windows-start) 
	   list)
      (unwind-protect
	  (save-window-excursion
	    (while (progn
		     (setq walk-windows-current
			   (next-window walk-windows-current t))
		     (setq list (cons walk-windows-current list))
		     (not (eq walk-windows-current walk-windows-start))))
	     (reverse (cons (car list) (cdr list)))))))

(unless (featurep 'xemacs)
  (defalias 'window-number-list 'window-list))
    
(defun window-number-select-nth (number)
  "Selects the nth window."
  (interactive "P")
  (if (integerp number)
      (let ((window (nth number (window-number-list))))
	(if (and window (or (not (window-minibuffer-p window)) (minibuffer-window-active-p window)))
		(select-window window)
	  (error "No such window.")))))

(defun window-number ()
  "Returns the the number of the current window."
  (1- (length (memq (selected-window)
		    (nreverse (window-list))))))

(defun window-number-string ()
  "Returns the string containing the number of the current window"
  (propertize
   (concat " -" (number-to-string (window-number)) "-")
   'face
   'window-number-face))

(defvar window-number-mode-map nil
  "Keymap for the window number mode.")

(unless window-number-mode-map
  (setq window-number-mode-map (make-sparse-keymap))

  (define-key window-number-mode-map (kbd "C-x C-j 0")
    (lambda nil (interactive)
      (window-number-select-nth 0)))

  (define-key window-number-mode-map (kbd "C-x C-j 1")
    (lambda nil (interactive)
      (window-number-select-nth 1)))
  (define-key window-number-mode-map (kbd "C-x C-j 2")
    (lambda nil (interactive)
      (window-number-select-nth 2)))
  
  (define-key window-number-mode-map (kbd "C-x C-j 3")
    (lambda nil (interactive)
      (window-number-select-nth 3)))
  
  (define-key window-number-mode-map (kbd "C-x C-j 4")
    (lambda nil (interactive)
      (window-number-select-nth 4)))
  
  (define-key window-number-mode-map (kbd "C-x C-j 5")
    (lambda nil (interactive)
      (window-number-select-nth 5)))
  
  (define-key window-number-mode-map (kbd "C-x C-j 6")
    (lambda nil (interactive)
      (window-number-select-nth 6)))
  
  (define-key window-number-mode-map (kbd "C-x C-j 7")
    (lambda nil (interactive)
      (window-number-select-nth 7)))
  
  (define-key window-number-mode-map (kbd "C-x C-j 8")
    (lambda nil (interactive)
      (window-number-select-nth 8)))
  
  (define-key window-number-mode-map (kbd "C-x C-j 9")
    (lambda nil (interactive)
      (window-number-select-nth 9))))

(defvar window-number-meta-mode-map nil
  "Keymap for the window number meta mode.")

(unless window-number-meta-mode-map
  (setq window-number-meta-mode-map (make-sparse-keymap))

  (define-key window-number-meta-mode-map (kbd "M-0")
    (lambda nil (interactive)
      (window-number-select-nth 0)))

  (define-key window-number-meta-mode-map (kbd "M-1")
    (lambda nil (interactive)
      (window-number-select-nth 1)))

  (define-key window-number-meta-mode-map (kbd "M-2")
    (lambda nil (interactive)
      (window-number-select-nth 2)))

  (define-key window-number-meta-mode-map (kbd "M-3")
    (lambda nil (interactive)
      (window-number-select-nth 3)))

  (define-key window-number-meta-mode-map (kbd "M-4")
    (lambda nil (interactive)
      (window-number-select-nth 4)))

  (define-key window-number-meta-mode-map (kbd "M-5")
    (lambda nil (interactive)
      (window-number-select-nth 5)))

  (define-key window-number-meta-mode-map (kbd "M-6")
    (lambda nil (interactive)
      (window-number-select-nth 6)))

  (define-key window-number-meta-mode-map (kbd "M-7")
    (lambda nil (interactive)
      (window-number-select-nth 7)))

  (define-key window-number-meta-mode-map (kbd "M-8")
    (lambda nil (interactive)
      (window-number-select-nth 8)))

  (define-key window-number-meta-mode-map (kbd "M-9")
    (lambda nil (interactive)
      (window-number-select-nth 9))))

(if (featurep 'xemacs)
    (define-minor-mode window-number-mode
      "A global minor mode that enables selection of windows according to
numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
  :global t
  :init-value nil
  :lighter " -?-")
  
  (define-minor-mode window-number-mode
    "A global minor mode that enables selection of windows according to
numbers with the C-x C-j prefix.  Another mode,
`window-number-meta-mode' enables the use of the M- prefix."
    :global t
    :init-value nil
    :lighter (:eval (window-number-string))))

(define-minor-mode window-number-meta-mode
  "A global minor mode that enables use of the M- prefix to select
windows, use `window-number-mode' to display the window numbers in
the mode-line."
  :global t
  :init-value nil)

;;(push (cons 'my-window-number-meta-mode my-window-number-mode-map)
;;       minor-mode-map-alist)


(defface window-number-face
  '((((type tty) (class color))
     (:background "red"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((type x w32 mac))
     (:foreground "red")))
  "The face used for the window number in the mode-line.")

;;; window-number.el ends here.


