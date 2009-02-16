;;comint
;; Do not create backup files
(setq make-backup-files nil) 

;;shell clear
(add-hook 'shell-mode-hook 'n-shell-mode-hook)
;make the shell prompt readonly
;(setq comint-prompt-read-only t)

(defun open-shell-window (&optional n)
  (interactive)
  (split-window-vertically -10)
  (other-window 1)
  (shell))

(defun n-shell-mode-hook ()
  "12Jan2002 - sailor, shell mode customizations."
  (local-set-key '[up] 'comint-previous-input)
  (local-set-key '[down] 'comint-next-input)
  (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  (setq comint-input-sender 'n-shell-simple-send)
  )

(defun n-shell-simple-send (proc command)
  (cond
   ;; Checking for clear command and execute it.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer))

   ;; Checking for man command and execute it.
   ((string-match "^[ \t]*man[ \t]*" command)
    (comint-send-string proc "\n")
    (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
    (setq command (replace-regexp-in-string "[ \t]+$" "" command))
    ;;(message (format "command %s command" command))
    (funcall 'man command))
   ;; Send other commands to the default handler.
   (t (comint-simple-send proc command))))



; Check for shebang magic in file after save, make executable if found.
(setq my-shebang-patterns 
      (list "^#!/usr/.*/perl\\(\\( \\)\\|\\( .+ \\)\\)-w *.*" 
	    "^#!/usr/.*/sh"
	    "^#!/usr/.*/bash"
	    "^#!/bin/sh"
	    "^#!/bin/bash"))
(add-hook 
 'after-save-hook 
 (lambda ()
   (if (not (= (shell-command (concat "test -x " (buffer-file-name))) 0))
       (progn 
	 ;; This puts message in *Message* twice, but minibuffer
	 ;; output looks better.
	 (message (concat "Wrote " (buffer-file-name)))
	 (save-excursion
	   (goto-char (point-min))
	   ;; Always checks every pattern even after
	   ;; match.  Inefficient but easy.
	   (dolist (my-shebang-pat my-shebang-patterns)
	     (if (looking-at my-shebang-pat)
		 (if (= (shell-command  
			 (concat "chmod u+x " (buffer-file-name)))
			0)
		     (message (concat 
			       "Wrote and made executable " 
			       (buffer-file-name))))))))
     ;; This puts message in *Message* twice, but minibuffer output
     ;; looks better.
     (message (concat "Wrote " (buffer-file-name))))))


(load "de-vars")
(setq load-path (cons (concat root-path "shell") load-path))

(autoload 'who-calls "who-calls" nil t)
(define-key emacs-lisp-mode-map "\C-c\C-w" 'who-calls)


;; (require 'shell-toggle)
;; (autoload 'shell-toggle "shell-toggle" 
;;   "Toggles between the *shell* buffer and whatever buffer you are editing." t) 
;; (global-set-key [f6] 'shell-toggle)


;;extra
;; (defun my-shell-command (command &optional output-buffer)
;;   "Beep 3 times if command execution time is longer than 10 seconds."
;;   (interactive (list (read-from-minibuffer "Shell command: "
;;                                            nil nil nil 'shell-command-history)
;;                      current-prefix-arg))
;;   (if (not (and (eq system-type 'windows-nt)
;;                 ;; calling date/time on window$ is dangerous,
;;                 ;; because it waits for user input and hangs up
;;                 (member (upcase command) '("DATE" "TIME"))))
;;       (let ((start-time (nth 1 (current-time))))
;;         (shell-command command output-buffer)
;;         (if (> (- (nth 1 (current-time)) start-time) 10)
;;             (dotimes (x 3)
;;               (sleep-for 0.2)
;;               (beep t))))))
;; (define-key esc-map "!" 'my-shell-command)
;; (define-key my-map  "!" 'my-shell-command)

;; ;; this command have so many bindings because it's difficult to type with AltGr
;; (define-key esc-map "|"    'shell-command-on-region-or-buffer)
;; (define-key esc-map "\M-|" 'shell-command-on-region-or-buffer) ; `M-ESC |'
;; (define-key global-map [(control ?|)] 'shell-command-on-region-or-buffer)
;; (define-key my-map "|" 'shell-command-on-region-or-buffer)


(require 'comint-scroll-to-bottom)
(add-hook 'comint-mode-hook 'comint-add-scroll-to-bottom)

;;shell completion

(require 'shell-command)
(shell-command-completion-mode)

(setq shell-command-completion-mode t)

(require 'bash-start)
(global-set-key (kbd "C-c t") 'bash-start)

(require 'mc-start)
(global-set-key (kbd "C-c m") 'mc-start)


(provide 'de-shell)