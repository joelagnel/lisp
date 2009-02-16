(set-language-environment "English")


(load "de-vars")
(setq load-path (cons (concat root-path "shell") load-path))



(defun eshell-clear ()
  "Mimics the behavior of clear"
  (interactive)
  (recenter 0)
  (end-of-line))

;(global-set-key "\C-x\C-l" 'eshell-clear)



(require 'esh-toggle)
;;(global-set-key [f6] 'eshell-toggle)

(setq eshell-prompt-function
      (lambda ()
        (concat " $ ")))


;; (setq eshell-prompt-function
;;   (lambda ()
;;     (concat (format-time-string "%Y-%m-%d %H:%M" (current-time))
;;       (if (= (user-uid) 0) " # " " $ "))))

(add-hook 'eshell-mode-hook
            '(lambda () (define-key eshell-mode-map "\C-l" 'eshell-clear)))



(defun open-eshell-window (&optional n)
  (interactive)
  (split-window-vertically -8)
  (other-window 1)
  (eshell))

(defun eshell-here ()
  "Run eshell in the current directory."
  (interactive)
  (let ((dir default-directory))
    (open-eshell-window)
    (unless (string= default-directory dir)
      (message "Switching to %s" dir)
      (eshell/cd (list dir))
      (eshell-emit-prompt))))




(global-set-key [f6] 'eshell-here)

(defun eshell/perldoc (&rest args)
  "Like `eshell/man', but invoke `perldoc'."
  (funcall 'perldoc (apply 'eshell-flatten-and-stringify args)))

    (defun perldoc (man-args)
      (interactive "sPerldoc: ")
      (require 'man)
      (let ((manual-program "perldoc"))
        (man man-args)))


;; RET-able ls

;;  (eval-after-load "em-ls"
;;     '(progn
;;        (defun ted-eshell-ls-find-file-at-point (point)
;;          "RET on Eshell's `ls' output to open files."
;;          (interactive "d")
;;          (find-file (buffer-substring-no-properties
;;                      (previous-single-property-change point 'help-echo)
;;                      (next-single-property-change point 'help-echo))))

;;        (defun pat-eshell-ls-find-file-at-mouse-click (event)
;;          "Middle click on Eshell's `ls' output to open files.
;;  From Patrick Anderson via the wiki."
;;          (interactive "e")
;;          (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

;;        (let ((map (make-sparse-keymap)))
;;          (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
;;          (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
;;          (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
;;          (defvar ted-eshell-ls-keymap map))

;;        (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
;;          "Eshell's `ls' now lets you click or RET on file names to open them."
;;          (add-text-properties 0 (length ad-return-value)
;;                               (list 'help-echo "RET, mouse-2: visit this file"
;;                                     'mouse-face 'highlight
;;                                     'keymap ted-eshell-ls-keymap)
;;                               ad-return-value)
;;          ad-return-value)))


;;show process-state

;; (defvar kai-eshell-process-running "[E]"
;;    "*String shown in the mode line while a process is running in eshell.")
 
;;  (defvar kai-eshell-process-stopped ""
;;    "*String shown in the mode line while no process is running in eshell.")
 
;;  (defvar kai-eshell-mode-line-process nil
;;    "String shown in the mode line to indicate whether eshell command running.
;;  Set to `kai-eshell-process-running' or `kai-eshell-process-stopped', as
;;  appropriate.")
 
;;  (setq mode-line-process 'kai-eshell-mode-line-process)
;;  (setq-default mode-line-process 'kai-eshell-mode-line-process)
 
;;  (defadvice eshell-command-started (before kai-eshell activate)
;;    (setq kai-eshell-mode-line-process kai-eshell-process-running))
 
;;  (defadvice eshell-command-finished (before kai-eshell activate)
;;    (setq kai-eshell-mode-line-process kai-eshell-process-stopped))

