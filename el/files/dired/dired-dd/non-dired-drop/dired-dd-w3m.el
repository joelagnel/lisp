;; dired-dd-w3m.el --- drop handler for *w3m* buffer.
;; See http://emacs-w3m.namazu.org/ for emacs-w3m (better-than-w3)
;; web browser.

;; Freeware. GPL2 applied. (c) 2001 S.Namba <sn@asahi-net.email.ne.jp>

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
;;		 (require 'dired-dd-w3m))))))

(require 'dired-dd)
;; (require 'w3m)
(autoload 'w3m-goto-url "w3m" nil t)
(provide 'dired-dd-w3m)

(defvar dired-dd-w3m-new-session t
  "Option referred by `dired-dd-w3m-drop-handler'.
Turning off this makes opening only the top file in the marked files.")

(defun dired-dd-w3m-drop-handler (arglist)
  "Drop-into-*w3m*-buffer handler.
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
       ;; Accept only plain drag
       (equal '(drag) modifiers)
       ;; Check if this is the `*w3m*' buffer
       (let ((case-fold-search t)) (string-match "w3m" mode-name)))

      ;; We do not support the Multiple file, use the 1st and ignore the rest.
      ;; The file extension is not checked here. Accept all sort of files
      ;; and leave them to w3m.

      (mapc
       (lambda (f) (w3m f 
			;; This is useless (C-u ignores mark and
			;; force to process dragged file only according
			;; to `dired' convension here.
			;; (if (and (boundp 'arg) arg) arg)
			dired-dd-w3m-new-session ;; in dired-dd.el.
			)) (nreverse files))

      ;; should return t here.
      t)
     (t nil) )))

;; Register handler to the handler list.
(or (member 'dired-dd-w3m-drop-handler dired-dd-non-dired-drop-handlers)
    (setq dired-dd-non-dired-drop-handlers
	  (cons 'dired-dd-w3m-drop-handler dired-dd-non-dired-drop-handlers)))
