;; dired-dd-lm.el --- Really experimental.

;; Freeware. GPL2 applied. (c) 2000 S.Namba <sn@asahi-net.email.ne.jp>

;; dired-dd Drag&Drop interface to lm.el:
;; drag-and-drops MIME attachment file(s) into any mail draft mode buffer.

;;
;; To get dired-dd, see
;; http://www.asahi-net.or.jp/~pi9s-nnb/dired-dd-home.html
;;

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
;;		 (require 'dired-dd-lm))))))

;; Sat Apr  1 13:14:52 2000 1st draft
;; Sat Apr  1 21:15:17 2000 Slight fix on docstring
;; Sun Apr  9 23:58:59 2000 Added howto-load-this comment above

(require 'dired-dd)
(require 'lm)
(provide 'dired-dd-lm)

(defun dired-dd-lm-framework-insert (arglist)
  "Attach a file to mail body in region by drag-and-drop in any buffer, \
which seems like to be a mail draft buffer.

All the files are attached with base64 encoding as a binary file.

Returns non-nil if it succeeded, otherwise nil."
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
       ;; Modifier check -- accepts only plain drag
       (equal '(drag) modifiers)
       ;; Check if this is a mail drafting buffer:
       (or (let ((case-fold-search t))
	     (string-match "draft\\|mh\\|mail\\|mew\\|message" mode-name))))
      ;;; (goto-char (car (reverse arglist))) ; works fine in older emacsen

      (or (mark t) (set-mark (point)))	; if no region, create it.
      ;; Multiple file handling 
      (setq result
	    (lm-framework-region (region-beginning) (region-end) "" files))
      ;;result 
      t)
     (t nil) )))

;; Register handler to the handler list.
(or (member 'dired-dd-lm-framework-insert dired-dd-non-dired-drop-handlers)
    (setq dired-dd-non-dired-drop-handlers
	  (cons 'dired-dd-lm-framework-insert dired-dd-non-dired-drop-handlers)))
