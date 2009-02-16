;; make-hex.el -- functions for drawing small ASCII hexes
;; (c) Copyright 1998 Lars Clausen
;; Distribute and modify as you wish, as long as the copyright notice stays.
;; No warranty, no liability, no nothing.  Comments welcome.
;;                         __
;; A hex looks like this: /  \
;;                        \__/

;; Comment these two lines out if you don't want keys set automatically
(global-set-key "\C-ch" 'make-hex)
(global-set-key "\C-c\C-h" 'make-hex-square)

(defvar make-hex-last-dir nil
  "Whether to make the next left-or-right moving hex end a line up.
This is used internally in make-hex to get correct horizontal lines of
hexes from a sequence of make-hex commands.")

(defun make-hex (count)
  "Make a hex at point, trying to align with other hexes.
Actually, it doesn't try very hard to align it yet.
Prefix arg means make that many hexes in the picture-movement direction
 (rightwards, if not in picture mode). "
  (interactive "p") 
  ;; Find the nearest hex
  (if (not count) (setq count 1))
  (if (< count 0)
      (progn
	(setq make-hex-last-dir nil)
	(setq count (- 0 count)))
    )
  (if (= count 0)
      (setq count 1))
  (while (> count 0)
    (if (not (looking-at "[\\\\/_]"))
	(progn
	  (picture-move-down 1)
	  (picture-forward-column 1)))
    (if (looking-at "__")
	(picture-forward-column 1)
      (if (looking-at "/")
	  (progn
	    (picture-move-down 1)
	    (picture-forward-column 2))
	(if (looking-at "\\\\")
	    (picture-forward-column 2))))
    (picture-move-up 2)
    (picture-backward-column 1) 
    (picture-insert-rectangle '("__"))
    (picture-backward-column 3)
    (picture-move-down 1)
    (picture-insert-rectangle '("/  \\" "\\__/"))
    (cond ((and (= picture-horizontal-step 1)
		(= picture-vertical-step 1))
	   (picture-backward-column 1))
	  ((and (= picture-horizontal-step 1)
		(= picture-vertical-step 0))
	   (picture-backward-column 1)
	   (if make-hex-last-dir
	       (progn
		 (picture-move-up 1)
		 (setq make-hex-last-dir nil))
	     (setq make-hex-last-dir t)))
	  ((and (= picture-horizontal-step 1)
		(= picture-vertical-step -1))
	   (picture-backward-column 1)
	   (picture-move-up 1))
	  ((and (= picture-horizontal-step 0)
		(= picture-vertical-step 1))
	   (picture-backward-column 3)
	   (picture-move-down 1))
	  ((and (= picture-horizontal-step 0)
		(= picture-vertical-step -1))
	   (picture-backward-column 3)
	   (picture-move-up 2))
	  ((and (= picture-horizontal-step -1)
		(= picture-vertical-step 1))
	   (picture-backward-column 6))
	  ((and (= picture-horizontal-step -1)
		(= picture-vertical-step 0))
	   (picture-backward-column 6)
	   (if make-hex-last-dir
	       (progn
		 (picture-move-up 2)
		 (setq make-hex-last-dir nil))
	     (setq make-hex-last-dir t)))
	  ((and (= picture-horizontal-step -1)
		(= picture-vertical-step -1))
	   (picture-backward-column 6)
	   (picture-move-up 2)))
    (setq count (1- count)))
  )

(defun make-hex-square (width)
  "Make a square of hexes.
Prefix argument is how many hexes wide to make the square.  Default is 3."
  (interactive "p")
  (if (not width) (setq width 3))
  (if (< width 1) (setq width 3))
  (let ((old-movement (cons picture-horizontal-step picture-vertical-step))
	(line width))
    (setq picture-horizontal-step 1)
    (setq picture-vertical-step 0)
    (while (> line 0)
      (make-hex width)
      (picture-backward-column (1- (* 3 width)))
      (picture-move-down 1)
      (setq line (1- line))
      (setq make-hex-last-dir nil))
    (setq picture-horizontal-step (car old-movement))
    (setq picture-vertical-step (cdr old-movement)))
  )

