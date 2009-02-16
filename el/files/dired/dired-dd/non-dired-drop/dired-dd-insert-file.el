;; dired-dd-insert-file.el -- Another example of dired-dd-non-dired-drop-handler

;; Fri Dec 11 01:37:24 1998: 1st draft
;; Fri Dec 11 13:26:03 1998: inserting order nreversed. Might be more rational.

;; Freeware. GPL2 applied. (c) 1998 S.Namba <sn@asahi-net.email.ne.jp>

;; "Insert-file" by Drag-and-drop in any non-dired-buffer.
;; Another sample of `dired-dd-non-dired-drop-handler'.  Inserts file content
;; at the dropped point by S-drag-mouse-2.  Buffer mode is not checked,
;; so that file(s) can be drop-inserted into any non-dired buffer.
;; It is safer to register at the end of dired-dd-non-dired-drop-handlers.

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
;;		 (require 'dired-dd-insert-file))))))

(require 'dired-dd)

;;
;; A handler function dealing with inserting file.
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
;; use variables such as `modifiers', `event', `start-window' etc.,
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

(defun dired-dd-insert-file (arglist)
  "Insert-file by Drag-and-drop in any non-dired-buffer.
If only shift key is used as modifier on drag-and-drop,
file content is inserted, otherwise NOP.
Major-mode is not tested.  So it might be safe to register 
this at the end of dired-dd-non-dired-drop-handlers.
Returns non-nil if it succeeded, otherwise nil."
  ;; Current arglist passed by dired-dd: (fn-list to modifiers end-point)
  ;; You can also use variables such as `modifiers', `event', `start-window'
  ;; etc., which are defined in upper functions.  Refer to the upper functions
  ;; dired-dd-drag-drop and  dired-dd-drop-from-to in dired-dd.el,
  ;; for those parameters.
  (let ((files (car arglist)) result)
    (cond
     ;; modifiers can be referred directly like this:
     ((and (= 1 (length (delq 'drag modifiers)))
	   (eq 'shift (car (member 'shift modifiers))))
      (goto-char (car (reverse arglist))) ; works fine in older emacsen
      (mapcar
       (lambda (file)
	 (setq result (insert-file file)))
       (nreverse files))
      ;; insert-file seems like returning nil even when it succeeded, so
      ;;result 
      t)
     (t nil) )))

;; After you defined the handler function,
;; register it in dired-dd-non-dired-drop-handlers in a list form
;; (Safer to register at the end of dired-dd-non-dired-drop-handlers ?):
(setq dired-dd-non-dired-drop-handlers 
      (append dired-dd-non-dired-drop-handlers (list 'dired-dd-insert-file)))

(provide 'dired-dd-insert-file)
