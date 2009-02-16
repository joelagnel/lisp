;;; Alpha-like mark stack (version 0.1)
;;; Copyright (C) 2002 Stefan D. Bruda (bruda@ubishops.ca)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;
;;; Commentary:
;; 
;; Implements a "lite" variant of Emacs markers, similar to Alpha's
;; mark stack. Works with pairs point/buffer, that will be henceforth
;; called lite markers. Two commands are provided:
;; `interactive-lite-mark-push' (bound, Alpha-like, to C-,) pushes on
;; a proprietary stack the current position and buffer as a lite
;; marker; `interactive-lite-mark-pop' (bound, Alpha-like, to C-.)
;; sets point to the position stored at the top of the proprietary
;; mark stack (switching the buffer if necessary), and pops the stack.
;; 
;; To use these commands, place this file where Emacs can see it, and
;; evaluate (or place in your init file -- normally .emacs or init.el)
;; one of the following lines:
;;
;;    (load "lite-markers")
;;    (require 'lite-markers)
;;

(defvar lite-mark-stack nil 
  "The lite markers' stack. 
A lite marker is a cons cell ( point . buffer ).")

(defun lite-mark-pop ()
  "Pops the lite markers' stack `lite-mark-stack' and returns the popped value."
  (if lite-mark-stack
      (let ((temp (car lite-mark-stack)))
        (setq lite-mark-stack (cdr lite-mark-stack))
        temp)
    (error "Mark stack is empty.")))

(defun lite-mark-push (point buf)
  "Assembles POINT and BUF into a lite marker and pushes the result to `lite-mark-stack'."
  (setq lite-mark-stack (cons (cons point buf) lite-mark-stack)))

(defun interactive-lite-mark-push ()
  "Pushes the current position (point and buffer) to `lite-mark-stack'."
  (interactive)
  (lite-mark-push (point) (current-buffer))
  (message "Mark %d pushed (position %s in %s)." 
           (length lite-mark-stack)
           (point)
           (current-buffer)))

(defun interactive-lite-mark-pop ()
  "Jumps to the position stored on the top of `lite-mark-stack' and pops this position.
If necessary, switches buffers as well. If the buffer referred to by
the top lite marker does not exist, just pops `lite-mark-stack'."
  (interactive)
  (let* ((marker (lite-mark-pop))
         (point (car marker))
         (buf (cdr marker)))
    (unless (and (get-buffer buf) (buffer-live-p buf))
      (error "Mark %d popped and ignored (there is no buffer to switch to!)." 
             (+ (length lite-mark-stack) 1) ))
    (switch-to-buffer buf)
    (setq point 
          (cond ((> point (point-max)) (point-max))
                ((< point (point-min)) (point-min))
                (t point)))
    (goto-char point)
    (message "Mark %d popped (position %s in %s)." 
             (+ (length lite-mark-stack) 1)
             point buf)))

;; The actual bindings: 

(define-key global-map [(control \.)] 'interactive-lite-mark-pop)
(define-key global-map [(control \,)] 'interactive-lite-mark-push)

;; Make myself known to the world: 

(provide 'lite-markers)

;; lite-markers.el ends here