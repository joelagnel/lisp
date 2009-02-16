;; dired-dd-mew.el -- Really experimental.

;; Wed Dec  9 17:10:28 1998: 1st draft
;; Fri Dec 11 13:26:03 1998: inserting order nreversed. Might be more rational.
;;                           Added modifier check (accepts only '(drag)).
;; Fri Dec 11 01:37:24 1998: A slight fix

;; Freeware. GPL2 applied. (c) 1998 S.Namba <sn@asahi-net.email.ne.jp>

;; Drag-and-drop MIME attachment file(s) into mew's draft mode buffer.
;; Supports only "attach by copying".

;; Briefly tested in mew-1.93+emacs-20.2 and mew-1.70+mule-2.3 (19.28 based).

;; To load (in your ~/.emacs):
;;  (add-hook
;;   'dired-load-hook
;;   (function
;;    (lambda ()
;;      (load "dired-x")
;;      ;; Set dired-x variables here.
;;      ;; To and flo...
;;      (if window-system
;;	  (progn (require 'dired-dd)
;;		 (require 'dired-dd-mew))))))

(require 'dired-dd)

;;
;; A handler function dealing with mew's draft buffer.
;; 

;;
;; For those who ever want to write similar handler functions,
;;
;; As for the argument which must be supported by this handler, Here is
;; an excerpt from docstring of variable dired-dd-non-dired-drop-handlers:
;;
;; ARGLIST, whose format is:
;; 
;;   (FILENAME-LIST WINDOW MODIFIERS POINT)
;; 
;; wherein
;; 
;;   FILENAME-LIST: a list of filenames (in full path description)
;;   WINDOW:        target window (probably useless)
;;   MODIFIERS:     modifier key list at the relevant drag-and-drop operation
;;   POINT:         buffer position corresponding to point where file is dropped.
;; 
;; Not enough ?  Don't forget that we are lisp programmers.  You can dare to
;; use variables such as `modifiers' `event' `start-window' etc.,
;; which are defined in upper functions.  Refer to the upper functions
;; dired-dd-drag-drop, and  dired-dd-drop-from-to in dired-dd.el,
;; for those parameters.
;; 
;; The handler function can assume that current window/buffer is
;; already switched to target buffer when it is called.  It is
;; responsibility of the handler function to examine whether the
;; current window/buffer is suitable for its job.  The examination
;; should be strict as possible, because it is not predictable that
;; the user registers what handler function in what order.
;;
;; The handler function must return non-nil if its operation was successful,
;; otherwise nil.
;;

(defun dired-dd-mew-draft-handler (arglist)
  "Cheap drag-and-drop handler for Mew's draft mode buffer:
whether operation should be done or not is determined by examining major-mode,
modifier keys of the drop event etc., within this function.
Returns non-nil always mew-attach-copy is called, otherwise nil.
Seems like multiple marked files can be dropped at least in my Mews."
  ;; Current arglist passed by dired-dd: (fn-list to modifiers end-point)
  ;; You can also use variables such as `modifiers', `event', `start-window'
  ;; etc., which are defined in upper functions.  Refer to the upper functions
  ;; dired-dd-drag-drop and  dired-dd-drop-from-to in dired-dd.el,
  ;; for those parameters.
  (let ((files (car arglist)) 
	(modifiers (car (cdr (reverse arglist)))) ; use it in arglist
	result)
    (cond
     ((and
       (eq 'mew-draft-mode major-mode)
       ;; Modifier check (accepts only plain drag)
       (equal '(drag) modifiers))
      (goto-char (car (reverse arglist))) ; works fine in older emacsen
      (mapcar
       (lambda (file)
	 (setq result (mew-attach-copy file (file-name-nondirectory file))))
       (nreverse files))
      ;; mew-attach-copy seems like returning nil even when it succeeded, so
      ;;result 
      t)
     (t nil) )))

;; After you defined the handler function,
;; register it in dired-dd-non-dired-drop-handlers in a list form:
(setq dired-dd-non-dired-drop-handlers
      (cons 'dired-dd-mew-draft-handler dired-dd-non-dired-drop-handlers))

;; add-hook may be used like this:
;;(add-hook 'dired-dd-non-dired-drop-handlers 'dired-dd-mew-draft-handler)
;; Give non-nil as second arg if your handler should be appended.

(provide 'dired-dd-mew)
