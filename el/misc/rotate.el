;; rotate.el -- rotate region or rectangle 90 degrees.
;; Copyright (c) 1998 Lars Clausen (lrclause@cs.uiuc.edu)
;;  You may change and redistribute this program or derivatives thereof,
;;  as long as this copyright notice remains intact, and the source is
;;  made publicly available.
;; Version 0.1:  Initial version.
;; Version 0.2:  Fixed mirroring/rotating problem
;; TODO:  Use vectors for efficiency.
;; TODO:  Way to allow mirroring for interactive use (C-u M-x rotate...)
;; Mail bugreports, ideas, grilled cheese etc to lrclause@cs.uiuc.edu

(defun combine-lists (l)
  (if (car l)
      (cons (mapcar 'car l)
	    (combine-lists (mapcar 'cdr l)))
    '()))

(defun fill-line (n ch l)
  (concat l (make-string (- n (length l)) ? )))

(defun list-to-string (l)
  "Takes a list of characters and returns a string consisting of those characters."
  (eval (cons 'concat (mapcar 'char-to-string l))))

;test
;of a
;fine
;pro-
;gram

(defun rotate-lists (l mirror)
  "Takes a list of strings, and returns a list of rotated strings.
Each string is filled with spaces to make them of equal length.
If mirror is true, mirror along x=-y axis rather than rotate."
  (let* ((max-len (eval (cons 'max (mapcar 'length l))))
	 (filled-lines (mapcar '(lambda (l) (fill-line max-len " " l)) l))
	 (list-lines (mapcar 'string-to-list filled-lines))
	 (lines-list (combine-lists (mapcar (if mirror 'identity 'reverse) list-lines))))
    (mapcar 'list-to-string lines-list)))

(defun rotate-region (beg end &optional p)
  (interactive "rp")
  (let* ((lines (rotate-lists (split-string (buffer-substring beg end) "\n") p))
	 (newlines (mapcar '(lambda (s) (concat s "\n")) lines)))
    (delete-region beg end)
    (eval (cons 'insert newlines))
    (set-mark beg)))
    
(defun copy-rectangle-rotate-text (beg end &optional p)
  (interactive "r")
  (let ((rect (kill-rectangle beg end)))
    (goto-char beg)
    (insert-rectangle rect)
    (set-mark beg)
    (setq killed-rectangle (rotate-lists (mapcar 'identity rect) p))))

(defun rotate-rectangle (beg end &optional p)
  (interactive "r")
  (let* ((rect (kill-rectangle beg end))
	 (lines (mapcar 'identity rect)) ;; To force it to a list :(
	 (senil (rotate-lists lines p)))
    (goto-char beg)
    (insert-rectangle senil)
    (set-mark beg)))
