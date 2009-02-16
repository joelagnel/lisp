;;;-*- auto-recompile: t -*-
;; Time-stamp: <2001-04-15 04:45:18 deego>
;; GPL'ed under GNU'S public license..
;; Copyright (C) Deepak Goel 2000
;; Emacs Lisp Archive entry
;; Filename: arkanoid.el
;; Package: arkanoid
;; Author: Deepak Goel <deego@glue.umd.edu>
;; Version: 0.1.3alpha

;;; Commentary: a text-only primitive Arkanoid game, uses game.el
;; An emacs-based arkanoid, always under construction, but always playable.


;;; This game updates things every few milliseconds. 
;;; Will not work on computers that cannot inform emacs about
;;; millisecond counts.

;;; Works on any frame-size, but the larger the frame the better the
;;; 'graphics'.

;;; QUICKSTART:
;;; Drop this and game.el in your load-path, and add
;;; (require 'arkanoid) to .emacs
;;; Type M-x arkanoid to play.

;;; For keybindings, type M-x ark-help.




;;; ALL CONTRIBUTIONS AND ENHANCEMENTS ARE MOST WELCOME. 

(require 'game)
(require 'cl)

;;;###autoload
(defun ark-help ()
  (interactive)
  (save-excursion
    (switch-to-buffer "*Ark-help*")
    (delete-other-windows)
    (set-buffer "*Ark-help*")
    (kill-region (point-min) (point-max))
    (insert "KEYBINDINGS:\n\n\n")
    (insert " Pad-right: Rightarrow or\n"
	    "            .\n\n")
    (insert " Pad-left: Leftarrow or\n"
	    "            ,\n\n")
    )
  )


(defvar arkanoid-version "0.1.3alpha")

(defvar ark-last-event nil)

;;;###autoload
(defun arkanoid ()
  (interactive)
  (ark-setup)
  (ark-play-initial)
)


(defun ark-setup ()
  (setq game-empty-char " ")
  (setq game-buffer-name "*ARKANOID*")
  (setq game-visible-space-default
	(list (- (frame-width) 3)
	      (- (frame-height) 3)))
  (setq game-space '(1000.0 1000.0))
  )

(defvar ark-ball-size 1)
(defvar ark-pad-size 1)
(defvar ark-num-balls 3)
(defvar ark-num-balls-default 3)
(defvar ark-ball-pos '(1000 1000))
(defvar ark-ball-vel '(40 -40)) 
(defvar ark-ball-vel-range 
  '(-300 300))

(defvar ark-pad-pos '(500 990))

(defvar ark-score 0)

(defun ark-list-add (&rest lists)
  (apply 'ark-list-op
   '(+ 0)
   lists))


;;;Wed Jan 17 11:20:35 2001
;;;###autoload
(defun ark-list-op (oplist &rest lists)
  "Applies (first oplist) to the rest of the lists..
oplist's second element should be a default.
LIST-OP-MY areturns a list, each element of whose is
the result of appling (car oplist) to the corresponding element of
lists..  IF some lists are too short, the default is taken from the
second element of oplist.
This (list-op-my '(+ 0) '(1 2 3) '(1 2)) will give
   '(2 4 3).  Thus, u see why we needed the 0.. "

  (if (every 'null lists) nil 
    (cons
     (apply 
      (first oplist)
      (mapcar
       (lambda (ls)
	 (if (null ls) (second oplist)
	   (car ls)))
       lists))
     (apply 'ark-list-op oplist (mapcar 'cdr lists))))
  )


(defun ark-list-multiply-by (number list)
  (ark-list-op
   (list
    (lambda (arg) (* number arg))
    1)
   list))

(defun ark-reflect-ball-y ()
  (setq ark-ball-vel
	(list (first ark-ball-vel)
	      (- (second ark-ball-vel)))))

(defun ark-reflect-ball-x ()
  (setq ark-ball-vel
	(list (- (first ark-ball-vel))
	      (second ark-ball-vel))))


(defun ark-tweak-vel ()
  (setq ark-ball-vel
	(ark-list-add
	 ark-ball-vel
	 (list
	  (ark-error ark-ball-vel-error-margin)
	  (ark-error ark-ball-vel-error-margin))))
  (if (< (first ark-ball-vel)
	 (first ark-ball-vel-range))
      (setq ark-ball-vel 
	    (list
	     (first ark-ball-vel-range)
	     (second ark-ball-vel))))
  (if (< (second ark-ball-vel)
	 (first ark-ball-vel-range))
      (setq ark-ball-vel 
	    (list
	     (first ark-ball-vel)
	     (first ark-ball-vel-range))))
  (if (> (first ark-ball-vel)
	 (second ark-ball-vel-range))
      (setq ark-ball-vel 
	    (list
	     (second ark-ball-vel-range)
	     (second ark-ball-vel))))
  (if (> (second ark-ball-vel)
	 (second ark-ball-vel-range))
      (setq ark-ball-vel 
	    (list
	     (first  ark-ball-vel)
	     (second ark-ball-vel-range))))

)


(defvar ark-ball-vel-error-margin 
  10
  "This is the erorr in the ball's speed.."
)

(defun ark-error (number)
  (let ((ran (- (random* 100) 50)))
    (/ (* number ran) 50.0))
)




(defun ark-move-ball ()
  "Moves ball based on the current velocity.
Assuming that one millisecond has passed."
  (when (or
	 (ark-ball-near-pad-p)
	 (< (second ark-ball-pos)
	    1))
    (progn
      (ark-reflect-ball-y)
      (setq ark-score (1+ ark-score))
      )
    )
  (when (or 
	 (< (first ark-ball-pos) 1)
	 (> (first ark-ball-pos) 1000))
    (progn
      (ark-reflect-ball-x)
      (setq ark-score (1+ ark-score))
      ))
  (ark-tweak-vel)
  (let ((old-pos ark-ball-pos))
    (setq ark-ball-pos
	  (ark-list-add 
	   (copy-list ark-ball-pos)
	   (ark-list-multiply-by 1.0 ark-ball-vel)))
    (game-put (first old-pos)
	      (second old-pos))
    (ark-put-ball))
  (when ark-last-event
    (game-put (first ark-pad-pos)
	      (second ark-pad-pos) "  ")
    (if (or
	 (equal ark-last-event 'right)
	 (equal ark-last-event '270D)
	 (equal ark-last-event 46))
	(setq ark-pad-pos
	      (list (+ (first ark-pad-pos) 50)
		    (+ (second ark-pad-pos) 0))))
    (if (or
	 (equal ark-last-event 'left)
	 (equal ark-last-event '270C)
	 (equal ark-last-event '44)
	 )
	(setq ark-pad-pos
	      (list (- (first ark-pad-pos) 50)
		    (- (second ark-pad-pos) 0))))
    (ark-put-pad))
  (ark-message "please save ball ")
)


(defun ark-put-pad (&optional string)
  (if (null string) (setq string "=="))
  (game-put (first ark-pad-pos)
	    (second ark-pad-pos)
	    string))

(defun ark-put-ball (&optional string)
  (if (null string) (setq string "o"))
  (game-put (first ark-ball-pos)
	    (second ark-ball-pos)
	    string))


(defun ark-ball-near-pad-p ()
  (and (< (abs
	   (- (first ark-ball-pos)
	      (first ark-pad-pos)))
	  100)
       (< (abs
	   (- (second ark-ball-pos)
	      (second ark-pad-pos)))
	   100))
)



(defun ark-ball-dead-p ()
  "If ball falls..
If ball is near the bottom end, and if the pad is nowhere near it, and
if it still has a bottom going velocity. "
  (if 
      (and
       (> (second ark-ball-pos) 1000)
       (not
	(ark-ball-near-pad-p))
       (> (second ark-ball-vel)
	  0))
      (setq ark-ball-dead t))
  ark-ball-dead
)

(defvar ark-ball-dead nil)


(defun ark-pad-reset ()
  (setq ark-pad-pos '(500 990))
)

(defun ark-ball-reset ()
  (setq ark-ball-vel-error-margin 0.2)
  (setq ark-ball-pos '(300 400))
  (setq ark-ball-vel '(40 -40))
  (setq ark-pad-pos '(1000 500))
  (setq ark-ball-vel-range '(-300 300))
)



(defun ark-set-keys ()
  (local-set-key " " 'ark-play)
  (local-set-key "n" 'ark-play-initial)

  (local-set-key "a" 'ark-play-initial)
  (local-set-key "b" 'ark-play-initial)
  (local-set-key "c" 'ark-play-initial)
  (local-set-key "d" 'ark-play-initial)
  (local-set-key "e" 'ark-play-initial)
  (local-set-key "f" 'ark-play-initial)
  (local-set-key "g" 'ark-play-initial)
  (local-set-key "h" 'ark-play-initial)
  (local-set-key "i" 'ark-play-initial)
  (local-set-key "j" 'ark-play-initial)
  (local-set-key "k" 'ark-play-initial)
  (local-set-key "n" 'ark-play-initial)
  (local-set-key "o" 'ark-play-initial)
  (local-set-key "p" 'ark-play-initial)
  (local-set-key "q" 'ark-play-initial)
  (local-set-key "r" 'ark-play-initial)
  (local-set-key "s" 'ark-play-initial)
  (local-set-key "t" 'ark-play-initial)
  (local-set-key "u" 'ark-play-initial)
  (local-set-key "v" 'ark-play-initial)
  (local-set-key "w" 'ark-play-initial)
  (local-set-key "x" 'ark-play-initial)
  (local-set-key "y" 'ark-play-initial)
  (local-set-key "z" 'ark-play-initial)
)

(defun ark-play-initial ()
  (interactive)
  (game-create-game)
  (ignore-errors (set-mouse-color "white"))
  (ignore-errors (set-cursor-color "white"))
  (ignore-errors (set-background-color "white"))
  (ignore-errors (set-foreground-color "blue"))
  (ark-set-keys)
  (setq ark-num-balls ark-num-balls-default)
  (setq ark-score 0)
  ;;====================================================
  (ark-play)
)


(defun ark-play ()
  (interactive)
  (when (> ark-num-balls 0)
    (ark-ball-reset)
    (ark-pad-reset)
    (ark-put-ball)
    (ark-put-pad)
    (setq ark-ball-dead nil)
    (ark-message "la la la :) ")
    (while (not (ark-ball-dead-p))
      (ark-move-ball)
      (goto-char (point-min))
      (if (input-pending-p)
	  (setq ark-last-event
		(read-event))
	(setq ark-last-event nil))
      (discard-input)
      (sit-for  0  100)
      )
    (setq ark-num-balls (- ark-num-balls 1))
    (ark-message "your Ball has died  :( ")
    (discard-input)
    (sit-for 1 300)
    (discard-input)
    (if (> ark-num-balls 0)
	(ark-message "Press SPC to continue")
      (ark-message "Press n for new game."))
    (ark-put-ball " ")
    (ark-put-pad "  ")
    )
  )



(defun ark-message (&rest args)
  (message
   (concat 
    (apply 'format args)
    " | "
    (ark-score-message)
    " | "
    (ark-balls-message ark-num-balls )
    )))

(defun ark-balls-message (num)
  (if (<=  num 0) ""
    (concat " o " (ark-balls-message (- num 1))))
)

(defun ark-score-message   ()
  (format "Score:  %S" ark-score))
