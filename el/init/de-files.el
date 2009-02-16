;; dired tricks

;;; File Processing

(load "de-vars")
(setq load-path (cons (concat root-path "files") load-path))


(require 'color-file-completion)
(require 'nc)

(require 'ffap-)

(require 'ff-paths)
(ff-paths-install)

;; Run program associated with a file

(require 'run-assoc)


(setq associated-program-alist 
      '(( "xchm" "\\.chm$")
	( "evince" "\\.pdf$")
	( "evince" "\\.ps$")
	( "mpg123"  "\\.mp3$")
	( "firefox"   "\\.html$")
	( "firefox"   "\\.htm$")
	( "mplayer" "\\.mov$")
	( "mplayer" "\\.mpg$")
	( "mplayer" "\\.mpeg$")
	( "ogg123"  "\\.ogg$")

	))
(define-key dired-mode-map [C-return] 'dired-run-associated-program)

;; igrep
(require 'igrep)
(require 'ifind)

(autoload 'igrep "igrep"  "*Run `grep` PROGRAM to match REGEX in FILES..." t)
(autoload 'igrep-find "igrep" "*Run `grep` via `find`..." t)
(autoload 'igrep-visited-files "igrep" "*Run `grep` ... on all visited files." t)
(autoload 'dired-do-igrep "igrep" "*Run `grep` on the marked (or next prefix ARG) files." t)
(autoload 'dired-do-igrep-find "igrep" "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
(autoload 'Buffer-menu-igrep "igrep" "*Run `grep` on the files visited in buffers marked with '>'." t)
(autoload 'igrep-insinuate "igrep" "Define `grep' aliases for the corresponding `igrep' commands." t)


;;dired-stuff


(setq load-path (cons (concat root-path "files/dired") load-path))

(require 'dired-sort-map)
(put 'erase-buffer 'disabled nil)

;;hide dired details

(require 'dired-details)
(dired-details-install)

(require 'dired-isearch)

(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)


;;dired-x

(add-hook 'dired-load-hook
               (lambda ()
                 (load "dired-x")
                 ;; Set dired-x global variables here.  For example:
                 (setq dired-guess-shell-gnutar "rm")
		 (setq dired-x-hands-off-my-keys nil)
                 ))
(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Set dired-x buffer-local variables here.  For example:
	    ;;(dired-omit-mode 1)
	    ))

;;info-mode
;; use lynx style key bindings in info mode
(defun info-lynx-keybindings () 
  (define-key Info-mode-map [right] 'Info-follow-nearest-node)
  (define-key Info-mode-map [down]  'Info-next-reference)
  (define-key Info-mode-map [up]    'Info-prev-reference)
  (define-key Info-mode-map [left]  'Info-last))
(add-hook 'Info-mode-hook 'info-lynx-keybindings)

(defun dired-lynx-keybindings ()
  (define-key dired-mode-map [left]  'dired-up-directory)
  (define-key dired-mode-map [right]  'dired-view-file))
(add-hook 'dired-mode-hook 'dired-lynx-keybindings)
(global-set-key (kbd "<kp-delete>") 'kill-buffer)


;; find file at point
(global-set-key (kbd "C-c f") 'find-file-at-point)
(setq ffap-machine-p-known 'accept)


;;dired-x


;; (load "dired-x")
;; (add-hook 'dired-mode-hook
;;           (function (lambda ()
;;                       ;; Set buffer-local variables here.  For example:
;;                       (dired-omit-mode 1)
;;                       )))

;; (setq dired-omit-files
;;                 (concat dired-omit-files "\\|^\\..+$"))




;; dired-dd
;; (setq load-path (cons "~/repository/lisp/el/files/dired/dired-dd" load-path))

;; (require 'dired-dd)


;; (defun spawn-shell-in-current-directory ()
;;   (interactive)
;;   (dired-dd-shell-command "hackd"))

;; (global-set-key (kbd "C-c t") 'spawn-shell-in-current-directory)
