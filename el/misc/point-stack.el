;;; point-stack.el
;; This file is free software.
;; Copyright Greg Novak (novak@ucolick.org) December 2004
;; Released under the GPL, available at http://www.gnu.org
;;
;; Provide a stack of buffer names and point locations
;;
;; I find this useful because I often need to do something elsewhere
;; in a file and then return to what I was doing.  Ie, I need to run
;; around in the file to find a function definition or something.
;; Then I want to be taken right back to where I was before the
;; "context switch"
;;
;; You'll probably want the following in your .emacs file.  My
;; mnemonic for the two commands is "Mark location" and "go Back"
;;
;; (require 'point-stack) 
;; (global-set-key "\C-cm" 'point-stack-push)
;; (global-set-key "\C-cb" 'point-stack-pop)

(defvar point-stack nil)

(defun point-stack-push ()
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))))

(provide 'point-stack)
;;; point-stack.el ends here
