;;; Debugging info for self: Saved through ges-version 1.5dev
;;; From: Luke Gorrie <luke@bluetail.com>
;;; Subject: chop.el 1.1
;;; Newsgroups: gnu.emacs.sources
;;; Date: 20 Jun 2002 11:58:12 +0200

;;; chop.el -- Interactive binary search for a line within a window.
;;; Version 1.1, June 2002.
;;; (Probably only works in GNU Emacs 21.)
;;;
;;; Maintained by Luke Gorrie <luke@bluetail.com>
;;; Contibutors: D. Goel, Rob Giardina, John Wiegley, Per Cederqvist

;; Interactively-driven binary search to move the point to a
;; particular line on the screen, by successive choppings in half.

;; Setup:
;;
;; Bind chop-move-{up,down} to keys. Otherwise they probably won't
;; work right, since M-x mucks with `last-command'.

;; Instructions:
;;
;; Use the following algorithm to move to any visible line in O(log N)
;; steps for a window N lines high:
;;
;; 1. Choose a line, L.
;; 2. Press either UP or DOWN. This starts by moving the point to the center.
;; 3. Repeat until L is the current line (or close enough):
;;      If L is above the point, press UP. Otherwise press DOWN.
;;
;; In practice, a few chops usually gets you pretty close. Then you
;; can zero in with line/paragraph/defun-based motion.

;; Change history:
;;
;; 1.1: Uniform "chop-" namespace (you have to update your key bindings!)
;;
;;      Option to skip first step when starting from near the middle
;;      (chop-lines-near-middle)
;;
;;      Autoload cookies

(defvar chop-size nil
  "Number of lines \"chopped off\" in the previous chop, as floating-point.
Only meaningful for consecutive chops.")

(defvar chop-current-line nil
  "Current line number, as floating-point.
Only meaningful for consecutive chops.")

(defvar chop-lines-near-middle nil
  "When a first chop is made from within `chop-lines-near-middle'
lines from the middle of the screen, the first step is skipped. This
makes the chop initially quarter the screen, rather than halving it.

The setting t means always skip the first chop; nil means never skip
it.")

;;;###autoload
(defun chop-move-up ()
  "Move by one 'chop' into the upper half of the remaining space."
  (interactive)
  (chop-move -1))

;;;###autoload
(defun chop-move-down ()
  "Move by one 'chop' into the lower half of the remaining space."
  (interactive)
  (chop-move 1))

(defun chop-move (dir)
  "Move by one 'chop'. DIR is the direction: -1 for upwards, 1 for downwards."
  (setq this-command 'chop-move)
  (if (chop-first-p)
      (chop-new dir)
    (chop-continue dir)))

(defun chop-first-p ()
  (or current-prefix-arg
      (not (eq last-command 'chop-move))))

(defun chop-new (dir)
  "Make a first chop, leaving the point in the middle of the window."
  (let ((origin (point))
	(half (/ (chop-last-visible-line-number) 2.0)))
    (setq chop-size half)
    (setq chop-current-line half)
    (move-to-window-line (round half))
    (when (or (eq chop-lines-near-middle t)
	      (and (numberp chop-lines-near-middle)
		   (<= (count-lines origin (point)) chop-lines-near-middle)))
      (chop-continue dir))))

(defun chop-continue (dir)
  "Make the next chop."
  (setq chop-size (/ chop-size 2))
  (incf chop-current-line (* dir chop-size))
  (move-to-window-line (min (chop-last-visible-line-number)
			    (round chop-current-line))))

(defun chop-last-visible-line-number ()
  "Window height, minus 1 to index from 0, minus 1 to account for modeline."
  (- (window-height) 2))

(provide 'chop)

;;; chop.el ends here

