;;; readonly.el:  a minor-mode for all buffer-read-only buffers
;; By Michelangelo Grigni <mic@mathcs.emory.edu>
;; Time-stamp: <96/03/12 20:30:15 mic>
(provide 'readonly)

;; This needs `minor-mode-map-alist', so it will not work in Emacs 18.
;; This file is available online in:
;; ftp://ftp.mathcs.emory.edu/pub/mic/emacs/
;;
;; If you really just want a minor version of view-mode (that also
;; works in Emacs 18), you could get mview.el.Z from:
;; ftp://archive.cis.ohio-state.edu:/pub/gnu/emacs/elisp-archive/modes/
;;
;; The main difference between a read-only mode and a view-mode is
;; that a view-mode is explicitly requested by the user, while a
;; read-only mode applies to *all* read-only buffers, including things
;; like rmail, gnus, vm, read-only files, etc.  For that reason, you
;; may not want "q" to kill (or bury) every read-only buffer!
;;
;; How this works: suppose you type the SPC (space) key in a read-only
;; buffer.  The command readonly-try is called.  First, it will try to
;; execute the standard (non-read-only) binding of the SPC key.  If
;; that command works, that is all that happens.  If the command fails
;; with a buffer-read-only error, readonly-try will then execute SPC's
;; binding in `readonly-map' (scroll-up, by default).
;;
;; The `minor-mode-map-alist' entry is enabled by the standard
;; `buffer-read-only' variable.
;;
;; The effect: in an editing mode like text-mode, M-x toggle-read-only
;; has the side-effect of making the spacebar change from self-insert
;; to scroll-up, and similarly for all other keys in `readonly-map'.
;;
;; WARNING: a second keymap, `readonly-minor-map', is computed from
;; `readonly-map' and put in `minor-mode-map-alist'.  Thus if you add
;; or remove bindings in `readonly-map', you should reload this file
;; to update `readonly-minor-map'.
;;
;; This only works if `inhibit-read-only' is nil.  It could also use
;; first-change-hook, after-change-function, before-change-function.
;; The standard "q" binding may kill too aggressively for you.


;; Keymaps:  `readonly-map' and the derived `readonly-minor-map'

(defvar readonly-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)
    ;; (define-key map "?" 'readonly-help)
    ;; (define-key map "a" 'readonly-add-to-file-hotlist)
    ;; Exit commands:
    (define-key map "b" 'readonly-bury)
    (define-key map "e" 'readonly-edit)
    ;; These two are dangerous, so make them upper case:
    (define-key map "K" 'readonly-kill)
    (define-key map "Q" 'readonly-quit)	; one of the above three
    (define-key map [backspace] 'scroll-down)
    ;; Arguably, delete should scroll-up if it would delete-char:
    (define-key map [delete] 'scroll-down)
    ;; The remaining bindings were swiped from mview.el:
    (define-key map "."	'set-mark-command)
    (define-key map "@"	'pop-global-mark)
    (define-key map "/"	'isearch-forward)
    (define-key map "=" 'what-line)
    (define-key map "\\" 'isearch-backward)
    (define-key map "g" 'goto-line)
    (define-key map "s"	'isearch-forward)
    (define-key map "x"	'exchange-point-and-mark)
    ;; (define-key map "z" 'bury-buffer)
    map)
  "Bindings used by readonly-try in `buffer-read-only' buffers.
If you modify this map, reload the \"readonly\" source to update the
derived `readonly-minor-map' keymap.")

(defun readonly-remap (map f)
  ;; Copy a sparse keymap MAP, replacing all commands with F.
  (cons 'keymap
	(mapcar
	 (lambda (pair)
	   (cons (car pair)
		 (if (keymapp (cdr pair)) (readonly-remap (cdr pair) f) f)))
	 (cdr map))))

(defconst readonly-minor-map
  ;; A defconst, so it is recomputed by reloading.
  (let ((map (readonly-remap readonly-map 'readonly-try)))
    ;; Note: rebinding "\^h" tickles a bug in 19.22 hexl!
    (define-key map "\^hk" 'readonly-describe-key)
    (define-key map "\^hc" 'readonly-describe-key-briefly)
    map)
  "The `minor-mode-map-alist' entry enabled by `buffer-read-only'.")

;; The main glob of glue:

(defun readonly-try nil
  "Try default command on this key, then maybe a `readonly-map' binding.
Tries second only if the first signals a `buffer-read-only' error."
  (interactive)
  (let ((key (this-command-keys)) default)
    ;; Fragile attempt to strip prefix chars.  What would be robust?
    (and current-prefix-arg
	 (string-match "\\`[-\^u\e0-9]+" key)
	 (setq key (substring key (match-end 0))))
    (setq default (or (let (buffer-read-only) (key-binding key))
		      'self-insert-command))
    (condition-case err
	(call-interactively default)
      (buffer-read-only
       (call-interactively (lookup-key readonly-map key))))))

;;; Exit commands:

(defun readonly-bury nil
  "Noisily bury this buffer."
  (interactive)
  (message "Buried buffer %s" (prog1 (buffer-name) (bury-buffer))))

(defun readonly-edit nil
  ;; This command name is a little misleading, since it does not
  ;; change the major-mode to an editing mode.
  "Noisily make a readonly buffer modifiable."
  (interactive)
  (setq buffer-read-only nil)
  (message "Buffer modifiable, use %s to restore read-only."
	   (if (memq (key-binding "\^x\^q")
		     '(toggle-read-only vc-toggle-read-only))
	       "C-x C-q"
	     (substitute-command-keys "\\[toggle-read-only]"))))

(defun readonly-kill nil
  "Noisily kill this buffer."
  (interactive)
  (let ((name (buffer-name)))
    (and (kill-buffer nil) (message "Killed buffer %s" name))))

(defvar temp-buffer-p nil)		; from show-temp-buffer
(defun readonly-quit (&optional edit)
  "Noisily kill, bury, or edit the current buffer.
A temporary buffer or unmodified file buffer is killed.
A modified buffer is made modifiable (ready to edit).
Other unmodified buffers are simply buried.
Optional EDIT (prefix) argument forces an edit."
  (interactive "P")
    (cond
     ((and (not edit)
	   (or temp-buffer-p
	       ;; (memq major-mode '(help-mode)) ; more?
	       ;; Unmodified and recoverable?
	       (and (or buffer-file-name revert-buffer-function)
		    (not (buffer-modified-p)))))
      (readonly-kill))
     ((or edit (buffer-modified-p))
      (readonly-edit))
     (t (readonly-bury))))


;; Helpers:  modified describe-key commands

(defun readonly-describe-key (key)
  "Describe KEY, with message about its readonly behavior."
  (interactive "kDescribe key: ")
  (let ((buffer-read-only (not (eq (key-binding key) 'readonly-try))))
    (describe-key key))
  (readonly-describe-key-briefly key))

(defun readonly-describe-key-briefly (key)
  "Briefly describe the readonly behavior of KEY."
  (interactive "kDescribe key briefly: ")
  (let ((robind (key-binding key))
	(rwbind (let (buffer-read-only) (key-binding key)))
	(keydes (key-description key)))
    (cond
     ((not robind)
      (message "%s is undefined" keydes robind))
     ((eq robind rwbind)
      (message "%s runs the command %s" keydes robind))
     ((eq robind 'readonly-try)
      (setq robind (lookup-key readonly-map key))
      (if (eq robind rwbind)
	  (message "%s runs the command %s" keydes robind)
	(message "%s == readonly-try: try %s, then maybe %s"
		 keydes rwbind robind)))
     (t (message "%s runs %s, shadowing %s" keydes robind rwbind)))))


;; Install:

(setcdr (or (assq 'buffer-read-only minor-mode-map-alist)
	    (car (setq  minor-mode-map-alist
			(cons '(buffer-read-only) minor-mode-map-alist))))
	readonly-minor-map)
;; To disable: (setcdr readonly-minor-map nil)

;; Read-only state is already visible in the usual mode-line-format,
;; so we do not need to display it again!  However, we make an
;; "invisible" entry, for describe-mode:
(setcdr (or (assq 'buffer-read-only minor-mode-alist)
	    (car (setq  minor-mode-alist
			(cons '(buffer-read-only) minor-mode-alist))))
	'(""))

(defun buffer-read-only nil
  ;; This function exists simply to be documented by describe-mode.
  "A minor-mode for all read-only buffers.
\\{readonly-map}"
  (error "function buffer-read-only is bogus"))

;; eof
