;;; locate-dired.el: yet another locate utility.

;;
;; Authour: Seiichi Namba <sn@asahi-net.email.ne.jp>
;; Copyright (C) 1997,1998 Seiichi Namba <sn@asahi-net.email.ne.jp>
;;

;;
;; Similar to find-dired, but seems like that
;; I have written from scratch.
;;
;; Can't Handle `R'ecursively listed file/dir ?
;;

;;
;; Wed Jan 21 20:25:42 1998 Fix, mimic dired somemore.
;;
;; * Now "/" at the top of filename is trimmed. Don't forget that
;;   you are always in "/" in the locate-dired output buffer.
;;

(eval-and-compile (require 'dired))

;; Suggested key binding:
;; (global-set-key "\C-xL" 'locate-dired)
;; (global-set-key "\C-x4L" 'locate-dired-other-frame)

;; I claim more generic name for this.
(fset 'locate 'locate-dired)

;; User options.
(defvar locate-dired-listing-switches "-ald"
  "Maybe you prefer
\(setq locate-dired-listing-switches dired-listing-switches\)
You had better include -d which may be what you want, I guess.
Dropping `d' is not a quite good idea.
See also `dired-listing-switches'")
(defvar locate-find-file-func 'find-file-other-window
  "Used by command locate-find-file, which opens all located files.
Setup this variable in ~/.emacs, for example:
\(setq locate-find-file-func 'find-file-other-frame\)")
(defvar locate-dired-locate-command "locate"
  "locate, or other fast find command name.")
(defvar locate-dired-last-target nil
  "Holds most recent locate command issued if it succeeded.")

(defvar locate-dired-use-other-window nil
  "Decides if locate-dired should use other window for output. Default nil.
If you like older versions' behavior, set to this to t.")

(defun locate-dired-other-frame ()
  "Very cheap other-frame version  of locate-dired."
  (interactive)
  ;;(require 'dired-dd)
  (dired-dd-switch-frame (make-frame))
  (call-interactively 'locate-dired)
  (delete-other-windows))

(defun locate-dired (input)
  "Just like find-dired, but uses `locate' command.  Asks file name substring.
Multiple args to `locate' command should be separated by C-q C-j in minibuffer.
`locate' command can be set to variable `locate-dired-locate-command'."
  (interactive
   (list (read-string "locate what file: " (or locate-dired-last-target nil))))
  (let ((buffer (concat "*locate-dired:" input "*")) tem ret)
    (setq ret (get-command-output
	       (setq tem
;;                     (concat "ls " locate-dired-listing-switches
;;                                 " `"
;;                                 locate-dired-locate-command
;;                                 " " input "`"
;;                                 ;;" 2>/dev/null"
;;                                 )
;;		     (format "%s %s | xargs ls %s"
			     ;; locate-dired-locate-command input
		     (format "%s | tr '\\n' '\\0' | xargs -0 ls %s"
			     (dired-shell-stuff-it locate-dired-locate-command
						   (dired-split "\n" input) nil nil)
			     locate-dired-listing-switches)
		     )
	       buffer t t))		; ignore-error, nolist
    ;; Guess always scceed...
    (if (= 0 ret)
	;;(setq locate-dired-last-target tem)
	(setq locate-dired-last-target input))
    (save-excursion
      (set-buffer buffer)
      ;; `cd' to "/" because this is `locate' application
      (setq default-directory "/")
      (setq buffer-read-only nil)
      (dired-mode "/" locate-dired-listing-switches)
      (if (fboundp 'dired-simple-subdir-alist)
	  (dired-simple-subdir-alist)
	(set (make-local-variable 'dired-subdir-alist)
	     (list (cons default-directory (point-min-marker)))))

      ;; Fake some more...
      (setq buffer-read-only nil)
      ;;(cd "/");; Not here but there above.
      (goto-char (point-min))
      (insert "  /:\n")
      (insert (format "  %s\n" tem))

      ;; Here we are at the top of the list.

      ;; I forgot to make this buffer dired-look-alike.
      (indent-rigidly (point) (point-max) 2)
      (dired-insert-set-properties (point) (point-max)) ; do it cheap.

      ;; Guess leaving "/" looks nice in this buffer, but
      ;; may raise spurious warning, so...
      ;; mimic dired-insert-set-properties here:
      (save-excursion
	(while (< (point) (point-max))
	  (condition-case nil
	      (if (dired-move-to-filename)
		  (if (looking-at "/")
		      (delete-char 1)))
	    (error nil))
	  (forward-line 1)))

      (if (featurep 'hilit19)
	  ;;(hilit-repaint-command nil)
	  (hilit-rehighlight-region (point-min) (point-max) t))
      (setq buffer-read-only t))
    (if locate-dired-use-other-window
	(pop-to-buffer buffer)
      (switch-to-buffer buffer))
    ))

(defun locate-find-file (input)
  "Opens all the files found by locate command."
  (interactive (list (read-string "locate-find what file: "
				  (or locate-dired-last-target nil))))
  (mapcar locate-find-file-func
	  (get-command-output
	   ;; (concat "locate " input)
	   (dired-shell-stuff-it locate-dired-locate-command
				 (dired-split "\n" input) nil nil)
	   " *tmp" t nil) ; ignore-error, list
	  ))

;; Locating tool.  Locate is fast enough, so no async execution.
(defun get-command-output (cmd buffer &optional ignore-error nolist)
  "Execute command COMMANDSTR in BUF.  Further two more optional args:
Ignore errors if IGNORE-ERROR is non-nil.
Returns lines of its output as list unless NOLIST is non-nil."
  ;; `locate' is fast enough. I do not use async exectution.
  (let* ((buf (get-buffer-create buffer)) (tem nil) ret)
    (save-excursion
      (set-buffer buf)
      (widen)
      (kill-all-local-variables)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq ret (call-process shell-file-name nil t nil "-c" cmd))
      (if nolist
	  ret				;; Just returned value
	(if (or ignore-error (= 0 ret))
	    (progn
	      (goto-char (point-min))
	      (while (< (point) (point-max))
		(setq
		 tem
		 (append
		  tem
		  (list (buffer-substring
			 (point) (save-excursion (end-of-line 1) (point))) )))
		(forward-line 1) )
	      tem)
	  (error
	   (format "locate errors: %s" (buffer-substring (point-min) (point-max))))
	  )))))
;tests
;(get-command-output "ls -l `locate 152x`")
;(mapcar 'find-file-other-window (get-command-output "locate 152x" " *tmp"))

(provide 'locate-dired)
