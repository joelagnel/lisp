;; poj-cycle-buffers.el - rotates buffers my way
;;
;; Copyright (c) 2003 Per Jonsson <poj+emacshacks@lysator.liu.se>
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;; Release Version: 0.2, 2003-11-22
;; 
;; $Id: poj-cycle-buffer.el,v 1.3 2003/11/22 17:50:48 poj Exp $

(defvar poj-cycle-buffer-pointer ())
(defvar poj-cycle-buffer-ring ())
(defvar poj-cycle-buffer-force t 
  "*If true poj-cycle-buffer-next, will force rotate to the buffer
  following this one in the ring")

(defun poj-cycle-buffer-register (buf &optional no-msg)
  "Registers a buffer in the poj-cycle-buffer-ring"
  (interactive "BBuffer to register: ")
  (if (and (bufferp (get-buffer buf))
	   (not (member buf poj-cycle-buffer-ring)))
      (progn (setq poj-cycle-buffer-ring 
		   (cons (get-buffer buf) poj-cycle-buffer-ring))
	     (or no-msg (message "Buffer registered in ring")) 'reg)
    (progn (or no-msg (message "Buffer already registered")) 'no-reg)))

(defun poj-cycle-buffer-register-current ()
  "Registers the current buffer in poj-cycle-buffer-ring if it's not
  already registered.
  
  If it's already registered, it removes it."
  (interactive)
  (if (equal (poj-cycle-buffer-register (current-buffer) t) 'reg)
      (progn (setq poj-cycle-buffer-pointer (cdr poj-cycle-buffer-ring))
	     (message "Buffer registered in ring"))
    (progn (poj-cycle-buffer-remove-current-from-ring)
	   (message "Buffer removed from ring"))))

(defun poj-cycle-buffer-next ()
  "Cycles through the buffers in poj-cycle-buffer-ring in
   forward direction"
  (interactive)
  (if (null poj-cycle-buffer-ring)
      (message 
       "Cant't rotate buffers, because there is no buffers in the ring")
    (let ((rot-to (car poj-cycle-buffer-pointer)))
      (cond ((null rot-to)     
	     (setq poj-cycle-buffer-pointer poj-cycle-buffer-ring)
	     (setq rot-to (car poj-cycle-buffer-pointer)))
	    (poj-cycle-buffer-force
	     (setq poj-cycle-buffer-pointer
		   (cdr (member (current-buffer) poj-cycle-buffer-ring)))
	     (setq rot-to (car poj-cycle-buffer-pointer))))
      (switch-to-buffer rot-to)
      (setq poj-cycle-buffer-pointer (cdr poj-cycle-buffer-pointer)))))


(defun poj-cycle-buffer-prev ()
  "Cycles through the buffers in poj-cycle-buffer-ring in
   backwards direction"
  (interactive)
  (if (null poj-cycle-buffer-ring)
      (message 
       "Cant't rotate buffers, because there is no buffers in the ring")
    (let ((rot-to (car (cdr (member (current-buffer) 
				    (reverse poj-cycle-buffer-ring))))))
      (if (null rot-to)     
	  (setq rot-to (car (reverse poj-cycle-buffer-pointer))))
      (switch-to-buffer rot-to)
      (setq poj-cycle-buffer-pointer 
	    (cdr (member (current-buffer) poj-cycle-buffer-ring))))))

(defun poj-cycle-buffer-remove-current-from-ring ()
  "Removes the current buffer from poj-cycle-buffer-ring"
  (interactive)
  (setq poj-cycle-buffer-pointer ())
  (setq poj-cycle-buffer-ring 
	(delete (current-buffer) poj-cycle-buffer-ring)))
	

(defun poj-cycle-buffer-install ()
  "Setup keybindings and hooks for poj-cycle-buffer"
  (global-set-key "\C-x\C-b"              ;Replaces the default 
		  'poj-cycle-buffer-next) ;binding on this keycombo
  (global-set-key "\C-x\M-b"              ;No default binding on my system
		  'poj-cycle-buffer-prev)
  (global-set-key "\C-x\C-r"              ;No default keybinding, at least not
		  'poj-cycle-buffer-register-current) ; on my system
  (add-hook 'kill-buffer-hook 'poj-cycle-buffer-remove-current-from-ring))

(defun poj-cycle-buffer-report-ring ()
  "Reports which buffers that are registered in poj-buffer-ring"
  (interactive)
  (message "%s" poj-cycle-buffer-ring))
