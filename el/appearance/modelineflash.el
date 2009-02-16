; From eli@mojave.cs.cornell.edu Tue Sep 12 12:00:03 2000
; Date: 07 Sep 2000 17:13:57 -0400
; From: Eli Barzilay <eli@mojave.cs.cornell.edu>
; Newsgroups: gnu.emacs.sources
; Subject: Re: maze.el

; Benjamin Rutt <brutt@brutt.dsl.xmission.com> writes:

; > heh, this is pretty funny.  bit of a pain in the neck to finish it
; > though.  At least I know I won, because it tried to call the
; > undefined "logo" function :) Between this and the recent
; > color-cycle.el, these are clever ways to waste CPU cycles!

; Here's yet another way to waste some more cycles,
; eval and then M-x toggle-modeline-flash.

;           ((lambda (x) (x x)) (lambda (x) (x x)))          Eli Barzilay:
;          http://www.cs.cornell.edu/eli/meaning.html        Maze is Life!

;-------------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; modeline flash

(defvar modeline-flash-delay .1)
(defvar modeline-flash-width 12)
(defvar modeline-flash-message "Hey!!! Why are you not working??")
(defvar modeline-flash-filler ?-)


(defvar modeline-flash-active nil)
(defvar modeline-flash nil)
(defvar modeline-flash-counter (- modeline-flash-width))

(defun modeline-flash-substring ()
  (setq modeline-flash-counter
        (1+ (if (>= modeline-flash-counter (length modeline-flash-message))
              (- modeline-flash-width)
              modeline-flash-counter)))
  (let* ((a modeline-flash-counter)
         (b (+ a modeline-flash-width))
         (l (length modeline-flash-message)))
    (cond
      ((and (>= a 0) (<= b l))
       (substring modeline-flash-message a b))
      ((or (>= a l) (<= b 0))
       (make-string modeline-flash-width modeline-flash-filler))
      ((>= a 0)
       (concat (substring modeline-flash-message a)
               (make-string (- modeline-flash-width (- l a))
                            modeline-flash-filler)))
      (t
       (concat (make-string (- modeline-flash-width b) modeline-flash-filler)
               (substring modeline-flash-message 0 b))))))

(defun modeline-flash ()
  (setq modeline-flash (modeline-flash-substring))
  (force-mode-line-update t))

(defun toggle-modeline-flash ()
  (interactive)
  (cancel-function-timers 'modeline-flash)
  (if modeline-flash-active
    (progn
      (setcdr modeline-flash-active
              (cdr (cdr modeline-flash-active)))
      (setq modeline-flash-active nil)
      (force-mode-line-update t))
    (progn
      (setq modeline-flash-active
            (nthcdr (- (length (default-value 'mode-line-format)) 2)
                    (default-value 'mode-line-format)))
      (setcdr modeline-flash-active
              (cons (list "--" 'modeline-flash) (cdr modeline-flash-active)))
      (run-at-time nil modeline-flash-delay 'modeline-flash))))

;;; Modifications by Deepak..
(defun turn-on-modeline-flash ()
  (if modeline-flash-active
      ""
    (toggle-modeline-flash))
  "")

(defun turn-off-modeline-flash ()
  (if modeline-flash-active 
      (toggle-modeline-flash)
    "")
)

(defun idle-modeline-flash-mode ()
  (while (sit-for 1)
    (turn-on-modeline-flash))
  (turn-off-modeline-flash))

(defun run-idle-modeline-flash ()
  (run-with-idle-timer 5 t
		     'idle-modeline-flash-mode))

