;;;; This file contains example expressions that could be placed in
;;;; your personal emacs startup (.emacs) file.  It is divided into
;;;; sections corresponding to the files in the distibution.  The
;;;; lines marked ";***" should be customized for your system.

;;; Make sure emacs knows where to find the emacs-lisp code:
(setq load-path (cons "/usr/local/emacs/extensions" load-path)) ;***

(require 'rmail)

;;; Reverse the action of the reply key 'r': default behavior does NOT
;;; reply to CC: list!!  Calling with a prefix means reply to CC: list.
(defvar standard-rmail-reply (symbol-function 'rmail-reply))
(defun rmail-reply (&optional recipients-too)
  "Reply only to the sender of the current message.  Prefix argument
means CC: all other recipients of original message.  While composing
the reply, use \\[mail-yank-original] to yank the original message
into it."
  (interactive "P")
  (funcall standard-rmail-reply (not recipients-too)))


;;;; -----------------------------------------------------------------
;;;; cl-shell.el 
;;;; (also: cl-lucid.el, cl-clos.el, cl-pcl.el, cl-flavors.el,
;;;; cl-obvius.el, shell-history.el, source-file-extensions.lisp).

;;; To run Common Lisp as a sub-shell inside emacs, type "M-x run-cl".
;;; To get a list of key bindings, type "C-h f cl-shell-mode", or see
;;; the file cl-shell.doc

;;; Command for running a Common Lisp sub-shell.
(setq *cl-program* "/usr/local/bin/lucid-pcl.5-89")  ;***
(autoload 'run-cl "cl-shell" "" t)

;;; These are correct defaults for Lucid Common Lisp
(setq *cl-prompt* "^> ")
(setq *cl-error-prompt* "^\\(->\\)+ ")

;;; Bind C-M-l globally to goto *lisp* buffer, or run lisp if it isn't
;;; running.
(global-set-key "\C-\M-l" 'cl-goto-lisp-buffer)
(autoload 'cl-goto-lisp-buffer "cl-shell" "" t)

;;; Some cl-shell parameters:
(setq *cl-echo-commands* t)		;echo defuns in lisp....
(setq *cl-pop-up* t)			;see cl-shell.el
(setq *cl-confirm-input* t)

;;; Run another lisp, with a different prompt, and send some
;;; initialization commands to it.
(defun run-development-lisp ()
  (interactive)
  (require "cl-shell")
  (let ((*cl-replacement-prompt*  "Lucid-4.0> "))  ;***
    (run-cl "/usr/local/lucid-4.0"))     	;load Beta version of Lucid4.0
  (cl-send-string "(load \"/usr/local/lucid-4.0/patches\")\n"))  ;***

;;; Stuff for running OBVIUS (Object-Based Vision and Image
;;; Undertanding System).
(setq *obvius-program* "/usr/local/bin/obvius")  ;***
(autoload 'run-obvius  "cl-obvius" "" t)


;;;; -----------------------------------------------------------------
;;;; completion.el  (also always-complete.el, advise.el).

;;; Completion code from TMC.  Default binding is M-<return>.
(global-set-key "\M-\r" 'complete)
(autoload 'complete "always-complete" "" t)

;;; If you don't want the minibuffer reminders, use this line instead
;;; of the one above.
; (autoload 'complete "always-complete" "" t)


;;;; -----------------------------------------------------------------
;;;; shell-history.el

;;; Add shell history mechanism:
(require 'shell)
(autoload 'shell-add-history "shell-history")

(defvar standard-shell-send-input (symbol-function 'shell-send-input))
(defun shell-send-input ()
  (interactive)
  (funcall standard-shell-send-input)
  (shell-add-history (buffer-substring last-input-start last-input-end)))

(define-key shell-mode-map "\M-\C-y" 'shell-yank-history)
(define-key shell-mode-map "\M-\C-z" 'shell-yank-history-forward)
(define-key shell-mode-map "\M-p" 'shell-yank-matching-history)
(define-key shell-mode-map "\M-n" 'shell-yank-matching-history-forward)


;;;; -----------------------------------------------------------------
;;;; misc-extensions.el

;;; Note: you must have set your load-path (see first command in this
;;; file) for this to work!
(load "misc-extensions")

;;; Set indentation for Common Lisp
(setq lisp-indent-hook 'common-lisp-indent-hook)

;;; Some useful functions which delete excess whitespace
(global-set-key "\C-x " 'delete-forward-whitespace) 
(rebind-keys-which-call 'just-one-space 'my-just-one-space) ;typically M-spc

;;; When a close-paren is typed on top of an existing paren, blink the
;;; match, but don't insert a new paren
(setq blink-paren-hook
      '(lambda ()
	 (if (and (not (eobp))
		  (char-equal (char-after (point)) last-input-char))
	     (delete-char 1))		;UGLY - this sets modification flag!
	 (blink-matching-open)))

;;; C-M-d parallel to C-d, M-d for consistency:
(global-set-key "\M-\C-d" 'kill-sexp)

;;; C-M-s repositions window with point or defun at top
(global-set-key "\M-\C-s" 'reposition-point-at-top)
(define-key lisp-mode-map "\M-\C-s" 'reposition-defun-at-top)
(define-key emacs-lisp-mode-map "\M-\C-s" 'reposition-defun-at-top)

;;; Make these characters magic: they insert a matching close.
(global-set-key "\"" 'insert-double-quotes)
(global-set-key "(" 'my-insert-parentheses)

(define-key lisp-mode-map "\e(" 'grow-list-forward)
(define-key emacs-lisp-mode-map "\e(" 'grow-list-forward)

;; Grep for symbol nearest mouse in *.lisp files.
(define-key lisp-mode-map "\M-\C-g" 'cl-grep-for-symbol)

;;; Switch bindings so that standard carriage return indents the new line:
(define-key lisp-mode-map "\C-m" 'newline-and-indent)
(define-key lisp-mode-map "\n" 'newline)     ;this is control-<cr> on Suns
(define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
(define-key emacs-lisp-mode-map "\n" 'newline)

;;; The next 4 expressions fix comment paragraph filling in lisp:
(rebind-keys-which-call 'fill-paragraph 'lisp-fill-paragraph lisp-mode-map)
(rebind-keys-which-call 'fill-paragraph 'lisp-fill-paragraph emacs-lisp-mode-map)
(setq lisp-mode-hook
      '(lambda () 
	(setq paragraph-start
	 (concat "^[ \t]*[^ ; \t]\\|^[ \t]*$\\|" (or paragraph-start "")))
	(setq paragraph-separate paragraph-start)
	(setq buffer-file-name
	      (and buffer-file-name (expand-symlinks buffer-file-name)))))
(setq emacs-lisp-mode-hook
      '(lambda () 
	(message "Running lisp-mode-hook.")
	(setq paragraph-start
	 (concat "^[ \t]*[^ ; \t]\\|^[ \t]*$\\|" (or paragraph-start "")))
	(setq paragraph-separate paragraph-start)))

;; Modify this function: When window is split, switch to a new buffer
;; in the new window!
(defvar standard-split-window-vertically
  (symbol-function 'split-window-vertically))

(defun split-window-vertically (size)
  "Split current window into two windows, one above the other.
This window becomes the uppermost of the two, and gets
ARG lines.  No arg means split equally."
  (interactive "P")
  (funcall standard-split-window-vertically size)
  (switch-to-buffer-other-window (other-buffer)))

;;; Modify definition in files.el to expand all symlinks in filename.
(defvar standard-find-file-noselect (symbol-function 'find-file-noselect))

(defun find-file-noselect (filename &optional nowarn)
  "Read file FILENAME into a buffer and return the buffer.
If a buffer exists visiting FILENAME, return that one,
but verify that the file has not changed since visited or saved.
The buffer is not selected, just returned to the caller."
  (funcall standard-find-file-noselect (expand-symlinks filename) nowarn))

;;; X windows (or OpenWindows) mouse and function-key bindings:
(if (and (eq window-system 'x) (= window-system-version 11))
    (progn
      (require 'x-mouse)    ; load this from the standard Emacs distribution
   ;;; RAW: these are all standard bindings
      (define-key mouse-map x-button-left       'x-mouse-set-point)
      (define-key mouse-map x-button-middle     'x-paste-text)
      (define-key mouse-map x-button-right      'x-cut-text)
   ;;; C: deleting text: (these are parallel to raw bindings)
      (define-key mouse-map x-button-c-left     'x-mouse-set-point)
      (define-key mouse-map x-button-c-middle   'x-copy-sexp)
      (define-key mouse-map x-button-c-right    'x-cut-and-wipe-text)
   ;;; M: destructive sexp manipulations
      (define-key mouse-map x-button-m-left     'x-move-sexp)
      (define-key mouse-map x-button-m-middle   'x-replace-sexp)      
      (define-key mouse-map x-button-m-right    'x-move-and-replace-sexp)
   ;;; S: buffer scrolling
      (define-key mouse-map x-button-s-left     'x-line-to-top)
      (define-key mouse-map x-button-s-middle   'x-line-to-middle)
      (define-key mouse-map x-button-s-right    'x-line-to-bottom)
   ;;; M-S: window operations
      (define-key mouse-map x-button-m-s-left   'x-mouse-keep-one-window)
      (define-key mouse-map x-button-m-s-right  'x-mouse-select-and-split)
   ;;; C-S: menus (these may not work if you didn't compile emacs with HAVE_X_MENUS).
      (define-key mouse-map x-button-c-s-left   'x-buffer-menu)
      (define-key mouse-map x-button-c-s-middle 'x-help)

   ;;; Function  key bindings:
      (load "sun-X-fn-keys")

      (define-X-fn-key "L1" 'keyboard-quit)            ;Stop
      (define-X-fn-key "L2" 'electric-command-history) ;Again
      (define-X-fn-key "L3" 'list-buffers)	       ;Props
      (define-X-fn-key "L4" 'undo)		       ;Undo
      (define-X-fn-key "L9" 'bury-buffer)	       ;Find
      (define-X-fn-key "L6" 'copy-region-as-kill)      ;Copy
      (define-X-fn-key "L8" 'yank-pop)		       ;Paste
      (define-X-fn-key "L10" 'kill-region)	       ;Cut

      (define-X-fn-key "R7" 'beginning-of-buffer)      ;Home
      (define-X-fn-key "R13" 'end-of-buffer)	       ;End
      (define-X-fn-key "R9" 'scroll-down)	       ;PgUp
      (define-X-fn-key "R15" 'scroll-up)	       ;PgDn
      (define-X-fn-key "R11" 'recenter)		       ;5 on middle of keypad
      ))




