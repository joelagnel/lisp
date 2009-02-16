;;;-*- auto-recompile: t -*-
;; Time-stamp: <2001-03-01 15:04:42 deego>
;; GPL'ed under GNU'S public license..
;; Copyright (C) Deepak Goel 2000
;; Emacs Lisp Archive entry
;; Filename: game.el
;; Package: game
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.0alpha

;;; Commentary: Helps create games easily.  Maps the game-space you
;;;  want to a smaller, visible space of, say, 80x24.




(defvar game-version "0.0alpha")


(defvar game-space '(700.0 200.0))


(defvar game-visible-space-default '(69.0 23.0)
  "These should please be reals"
)


(defvar game-visible-space '(69 23)
 "These should please be integers. "
)

(defvar game-reduction-factor '(10.0 10.0))



(defun game-make-float ()
  (setq game-space (list (float (first game-space))
			 (float (second game-space))))
  (setq game-visible-space-default (list (float (first
						 game-visible-space-default))
			 (float (second game-visible-space-default))))
  (setq game-reduction-factor (list (float (first game-reduction-factor))
			 (float (second game-reduction-factor))))
  )



(defun game-set-visible-space ()
  "Sets the game-visible-space <= game-visible-space-default.
Also sets the game-reduction-factor.
"
  (interactive)
  (game-make-float)
  (let ((faca 	
	 (ceiling (/ (float (first game-space)) 
		     (float (first
			     game-visible-space-default)))))
	(facb
	 (ceiling (/ (float (second game-space))
		     (float (second
			     game-visible-space-default))))))
    (setq game-reduction-factor 
	  (list (float faca) (float facb)))
    (setq game-visible-space
	  (list
	   (ceiling (/ (first game-space)
		       faca))
	   (ceiling (/ (second game-space)
		       facb))))))


(defvar game-buffer-name "*GAME*")


(defvar game-empty-line
      " ")


(defvar game-empty-char "'"
 "A string of length one, to be put for blank space..")


(defun game-create-game ()
  "Creates a game"
  (interactive)
  (switch-to-buffer game-buffer-name)
  (delete-other-windows)
  (set-buffer game-buffer-name)
  (game-unset-most-keys)
  (kill-region (point-min) (point-max))
  (game-set-visible-space)
  (setq game-empty-line "")
  (let (loop-var)
    (dotimes
	(loop-var (ceiling (first game-visible-space)))
      (setq game-empty-line
	    (concat game-empty-char game-empty-line)))
    (dotimes
	(tmp-var (ceiling (second game-visible-space)))
      (insert game-empty-line "\n"))))
      

(defun game-unset-most-keys ()
  ""
  (fundamental-mode)
  (local-unset-key "a")
  (local-unset-key "b")
  (local-unset-key "c")
  (local-unset-key "d")
  (local-unset-key "e")
  (local-unset-key "f")
  (local-unset-key "g")
  (local-unset-key "h")
  (local-unset-key "i")
  (local-unset-key "j")
  (local-unset-key "k")
  (local-unset-key "l")
  (local-unset-key "m")
  (local-unset-key "n")
  (local-unset-key "o")
  (local-unset-key "p")
  (local-unset-key "q")
  (local-unset-key "r")
  (local-unset-key "s")
  (local-unset-key "t")
  (local-unset-key "u")
  (local-unset-key "v")
  (local-unset-key "w")
  (local-unset-key "x")
  (local-unset-key "y")
  (local-unset-key "z")
	       
  (local-unset-key "A")
  (local-unset-key "B")
  (local-unset-key "C")
  (local-unset-key "D")
  (local-unset-key "E")
  (local-unset-key "F")
  (local-unset-key "G")
  (local-unset-key "H")
  (local-unset-key "I")
  (local-unset-key "J")
  (local-unset-key "K")
  (local-unset-key "L")
  (local-unset-key "M")
  (local-unset-key "N")
  (local-unset-key "O")
  (local-unset-key "P")
  (local-unset-key "Q")
  (local-unset-key "R")
  (local-unset-key "S")
  (local-unset-key "T")
  (local-unset-key "U")
  (local-unset-key "V")
  (local-unset-key "W")
  (local-unset-key "X")
  (local-unset-key "Y")
  (local-unset-key "Z")
	       
)
   


(defun game-put (ptx pty &optional string)
  "Puts the string.. after mapping the pts onto this game.
PTX and PTY are places in the game-space.. which are then converted to
game-visible-space..   We recommend string be only one char long.. 
"
  (if (null string)
      (setq string game-empty-char))
  (game-visible-put
   (/ ptx (first game-reduction-factor))
   (/ pty (second game-reduction-factor))
   string)
)



(defun game-visible-put (ptx pty &optional string)
  "Puts the string..  PTX and PTY are places in the
game-visible-space.. 
We recommend string be only one char long..  and do not currently
guarantee against deleting newlines for string longer than that..
"
  (if (or (null string) (< (length string) 1))
      (setq string game-empty-char))
  (if (< ptx 1) (setq ptx 1))
  (if (> ptx (- (first game-visible-space) (- (length string) 1)))
      (setq ptx (- (first game-visible-space) (- (length string) 1))))
  (if (< pty 1) (setq pty 1))
  (if (> pty (second game-visible-space))
      (setq pty (second game-visible-space)))
  (goto-char (game-visible-find-point ptx pty))
  (forward-char (- (length string) 1))
  (backward-delete-char (length string))
  (insert string)
)



(defun game-visible-find-point (ptx pty)
  "Finds the \( point\) just after this position."
  (+
   (point-min)
   (* (- (round pty) 1) (+ (first game-visible-space) 1))
   (round ptx)))


(defun game-visible-find-coordinates (&optional point)
  "Finds visible coordinates of the thing just before point."
  (interactive)
  (if (null point)
      (setq point (point)))
  (list
    (% (- point (point-min))
       (+ (first game-visible-space) 1))
    (1+
     (/ (- point (point-min))
	(+ (first game-visible-space) 1))))
)


(defun game-coordinates-to-visible (pta ptb)
  (let
      ((ptx (round (/ pta (first game-reduction-factor))))
       (pty (round (/ ptb (second game-reduction-factor)))))
    (when (< ptx 1) (setq ptx 1))
    (when (> ptx (first game-visible-space))
      (setq ptx (first game-visible-space)))
    (when (< pty 1) (setq pty 1))
    (when (> pty (second game-visible-space))
      (setq pty (second game-visible-space)))
    (list ptx pty))
)


(defun game-visible-to-coordinates (ptx pty)
  (when (< ptx 1) (setq ptx 1))
  (when (> ptx (first game-visible-space))
    (setq ptx (first game-visible-space)))
  (when (< pty 1) (setq pty 1))
  (when (> pty (second game-visible-space))
    (setq pty (second game-visible-space)))
  (list
   (* (first game-reduction-factor) ptx)
   (* (second game-reduction-factor) pty)))



(provide 'game)
