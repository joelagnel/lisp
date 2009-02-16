;;;
;; -*-emacs-lisp-*-
;;
;; Some ideas kindly borrowed from:
;;
;; Alvaro Lopez Ortega <alvaro@alobbs.com>
;; J.T. Halbert
;; François Fleuret <francois.fleuret@epfl.ch>
;; Eric Knauel <knauel@informatik.uni-tuebingen.de>
;; Charles Curlye <charlescurley@charlescurley.com>
;; StefanKamphausen (emacs-wiki)

;; It's better to set the preferences in the .Xresources so that the
;; window is not first displayed with the wrong options

;; Emacs.menuBar:          off
;; Emacs.verticalScrollBars:       off
;; Emacs.toolBar:          off
;; Emacs.internalBorder:      1

;; *custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "Latin-1")
 '(default-input-method "latin-9-prefix")
 '(display-time-mode t)
 '(erc-modules (quote (autojoin button fill irccontrols match netsplit noncommands completion readonly ring services stamp track list)))
 '(iso-ascii-convenient t)
 '(transient-mark-mode nil))

;; Some paths, always first
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp"))
(setq diary-file "~/.emacs.d/diary")
(load "/usr/share/emacs/site-lisp/site-gentoo")
(load "~/.emacs.d/skeletons")

;; Requires
;;;;;;;;;;;;;
(require 'bbdb)
(require 'dired+)
(require 'dired-details+)
(require 'wdired)
(require 'emms-setup)
(require 'elscreen)
(require 'elscreen-dired)
(require 'font-lock)
(require 'no-word)
(require 'recentf)
(require 'smarty-mode)
(require 'table)
(require 'background)
(require 'pair-mode)
(require 'mingus)
(require 'ipython)


;; My info
(setq user-full-name "Rodrigo Lazo")

;; Default mode
(setq default-major-mode 'outline-mode)

;; abbrev mode
(setq abbrev-file-name "~/.emacs.d/abbrev_defs"
      default-abbrev-mode t)
(quietly-read-abbrev-file)

;; Color theme if inside X
(if window-system
    (progn 
      (color-theme-initialize)
      (color-theme-vim-colors)))

;;(setq tex-dvi-view-command "xdvi")

;; avoid cjk
(setq utf-translate-cjk-mode nil)

;; Spell program
(setq-default ispell-program-name "aspell")

;; No splash screen
(setq inhibit-startup-message t)

;; Make yes - no into y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Show the clock
(display-time)

;; indent code
(setq c-default-style
      '((c-mode . "k&r")))

;; Open word files using antiword
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

;; Recentf mode
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)

;; To ease things with the mark ring
(setq set-mark-command-repeat-pop 1)

;; nxml autoload
(add-to-list 'auto-mode-alist
	     (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
		   'nxml-mode))

(setq magic-mode-alist
      (cons '("<\\?xml " . nxml-mode)
	    magic-mode-alist))

;; shell in a new tab of elscreen
(defun elscreen-shell-screen ()
  "Creates a new screen with shell in it"
  (interactive)
  (let ((cur-buffer (buffer-name)))
    (progn
      (eshell)
      (switch-to-buffer cur-buffer)
      (elscreen-create)
      (switch-to-buffer "*eshell*"))))

;; Some global keybindings
(global-set-key "\eg" 'goto-line)
(define-key global-map [f7] 'bookmark-set)
(define-key global-map [f8] 'bookmark-jump)
(define-key global-map [f12] 'calendar)
(define-key global-map [C-next] 'elscreen-next)
(define-key global-map [C-prior] 'elscreen-previous)
(define-key global-map [?\C-.] 'dabbrev-expand)
(define-key global-map [?\C->] 'hide-subtree)
(define-key global-map [?\C-<] 'show-children)
(define-key global-map [?\s-<] 'show-entry)
(define-key global-map "\C-z\C-K" 'elscreen-kill-and-buffer)
(define-key global-map [f11] 'elscreen-shell-screen)
(define-key global-map [f4] 'elscreen-kill-and-buffer)
(define-key global-map [?\C-ç] 'ispell-word)
(define-key global-map "\C-x5g" 'gnus-other-frame)
(define-key global-map "\C-x\C-b" 'electric-buffer-list)

;; emms
;; (emms-standard)
;; (emms-default-players)

;; dired
(setq dired-recursive-deletes 1
      dired-recursive-copies 1)
(toggle-dired-find-file-reuse-dir 1)
(setq dired-listing-switches "-lha") 
(add-hook 'dired-mode-hook
	  '(lambda()
	     (define-key dired-mode-map "\C-c\C-p" '(lambda()
						      (interactive)
						      (progn 
							(message "Playing file")
							(emms-play-dired))))))

;; Background

(autoload 'background "background" nil t)

;; BigBrotherDataBase

 (bbdb-initialize 'gnus 'message)
 (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
 (add-hook 'mail-setup-hook 'bbdb-insinuate-sendmail)

;; C
(add-hook 'c-mode-hook
	  (function (lambda ()
		      (outline-minor-mode)
		      )))

;; C++ 
(add-hook 'c++-mode-hook
	  '(lambda ()
	     (define-key c++-mode-map "\C-cc" 'compile)
	     (outline-minor-mode)))

;; Python
(add-hook 'python-mode-hook
	  '(lambda ()
	     (pair-mode)))

;; Lisp
(add-hook 'lisp-mode-hook
	  '(lambda ()
	     (pair-mode)))
;; XML
(add-hook 'nxml-mode-hook
	  '(lambda()
	     (define-key nxml-mode-map "\C-c\C-t" 'sgml-tag)))

;; Calendar
(add-hook 'calendar-mode-hook
	  '(lambda()
	     (setq mark-diary-entries 1)))

;; ERC
;; fires up a new frame and opens your servers in there. You will need
;; to modify it to suit your needs.
(setq erc-server-coding-system 
      (quote
       (utf-8 . utf-8)))

(defun irc ()
  "Start to waste time on IRC with ERC."
  (interactive)
  (select-frame (make-frame '((name . "Emacs IRC")
 			      (minibuffer . t))))
  (call-interactively 'erc-server-select))


(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs-es" "#aqpglug" "#gentoo-gwn-es" "#gentoo-es" "#gentoo-pe" "#bsd.pe")))

(setq erc-prompt-for-nickserv-password nil)


(defun irc-elscreen ()
  "Creates a new screen for each irc channel"
  (interactive)
  (select-frame-by-name "Emacs IRC")  
  (let ((lista (mapcar (function buffer-name) (buffer-list)))
	(first-buff (buffer-name)))
    (dolist (buff lista) 
      (if (and (string-match "^[#][.]*" buff) (not (string= first-buff buff)))
	  ((lambda ()
	     (elscreen-create)
	     (switch-to-buffer buff)))))))

;; W3M
;; Create a new tab using elscreen
(defun w3m-url-to-new-tab ()
  "Creates a new elscreen tab for the new w3m sesion"
  (interactive)
  (call-interactively 'w3m-goto-url-new-session)
  (let ((cur-buf (buffer-name)))
    (switch-to-buffer (other-buffer))
    (elscreen-create)
    (switch-to-buffer cur-buf)))

(add-hook 'w3m-mode-hook
	  '(lambda ()
	     (define-key w3m-mode-map "G" 'w3m-url-to-new-tab)))

;; elscreen
(defun elscreen-kill-and-buffer ()
  "Kills the current tab and also the buffer that is editing. If
is just one screen, only kills the buffer"
  (interactive)
  (let ((cur-buf (buffer-name)))
    (kill-buffer (buffer-name))
    (if (> (elscreen-get-number-of-screens) 1)
	(elscreen-kill))))

;; smtp mode

(setq send-mail-function 'smtpmail-send-it) ; if you use `mail'
(setq smtpmail-default-smtp-server "smtp.gmail.com") ; set before loading library
;;(setq smtpmail-debug-info t) ; only to debug problems
(setq smtpmail-auth-credentials		; or use ~/.authinfo
      '(("smtp.gmail.com" 25 "rlazo.paz@gmail.com" "PASSWD")))
(setq smtpmail-starttls-credentials
      '(("smtp.gmail.com" 25 "~/.my_smtp_tls.key" "~/.my_smtp_tls.cert")))
;; weird but I dont have this files and it still works

;; auto-add signature
(setq mail-setup-hook
      (function
       (lambda ()
	 (mail-signature) )))

;; An alarm clock
;; by: Mathias Dahl <brakjoller@gmail.com> emacs-help list

(defvar alarm-clock-timer nil
  "Keep timer so that the user can cancel the alarm")

(defun alarm-clock-message (text)
  "The actual alarm action"
  (message-box text))

(defun alarm-clock ()
  "Set an alarm.
The time format is the same accepted by `run-at-time'.  For
example \"11:30am\"."
  (interactive)
  (let ((time (read-string "Time: "))
        (text (read-string "Alarm message: ")))
    (setq alarm-clock-timer (run-at-time time nil 'alarm-clock-message text))))

(defun alarm-clock-cancel ()
  "Cancel the alarm clock"
  (interactive)
  (cancel-timer alarm-clock-timer))

;; time-stamp
(setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S %u %f")
(add-hook 'write-file-hooks 'time-stamp)

;; A function to insert the time stamp at point.
(defun stamp ()
  "Insert at point the dummy time stamp string to activate the time stamp facility."
  (interactive "*")
  (insert "Time-stamp: <>")             ;insert the bare bones
  (time-stamp)                          ;call the function to fill it in
                                        ;where we put it.
  )

;; Make file starting with she-bangs executables at save-time
(add-hook 'after-save-hook
	  #'(lambda ()
	      (and (save-excursion
		     (save-restriction
		       (widen)
		       (goto-char (point-min))
		       (save-match-data
			 (looking-at "^#!"))))
		   (not (file-executable-p buffer-file-name))
		   (shell-command (concat "chmod u+x " (shell-quote-argument buffer-file-name)))
		   (message
		    (concat "Saved as script: " buffer-file-name)))))
