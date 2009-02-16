;;; SYNOPSIS: Moves cursor to places where changes occurred.
;;; AUTHOR: GPL(C) Mohsin Ahmed, http://www.cs.albany.edu/~mosh

;;; SEE: (describe-variable 'buffer-undo-list)

(defvar mosh-scan-undos nil "Local copy of undo list.")

(defun mosh-scan-undos ()
  "Takes point to each point in the undo list."
  (interactive)
  (if (eq buffer-undo-list t) (error "No undo list here."))
  (if (eq last-command this-command) nil
    (setq mosh-scan-undos buffer-undo-list))
  (let (first a b)
    (setq first nil)
    (while (and mosh-scan-undos (null first))
      (setq first (car mosh-scan-undos))
      (setq mosh-scan-undos (cdr mosh-scan-undos))
    )
    (message "first=%s" first)
    (if (null first) (error "No more"))
    ;; First contains a non-nil entry.
    (if (consp first)
        (progn
          (setq a (car first))
          (setq first (cdr first))
          (message "a=%s (type=%s), first=%s" a (type-of a) first)
          (if (eq a t)    (setq b a a (car first)))
          (if (stringp a) (setq b a a (abs first)))
          (message "final a=%s, b=%s" a b)
    ))
    (if (integer-or-marker-p a)
        (progn
          (message "mosh-scan-undos going to char %s, undo=(%s)" a b)
          (goto-char a)))
  )
)

;; From ralf@natlab.research.philips.com  Thu Jun 20 13:07:10 1996
;; Make it buffer local.
;;     (if (null mosh-scan-undos)
;;         (progn
;;           (make-local-variable 'mosh-scan-undos)
;;           (setq mosh-scan-undos buffer-undo-list)))


;; buffer-undo-list 's value is nil
;; Local in buffer scanundo.el; global value is nil

;; EOF
