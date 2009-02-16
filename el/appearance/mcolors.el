;;; SYNOPSIS: color customisation setup routines.
;;; AUTHOR: GPL(C) Mohsin Ahmed, http://www.cs.albany.edu/~mosh

(defvar mosh-back-color "grey12")
(defvar mosh-BACK-color "grey15" "emphasised background color")
(defvar mosh-fore-color "grey85")
(defvar mosh-mode-color "LightGoldenrod3")
(defvar mosh-curs-color "yellow")

(if gnuemacs (fset 'find-face 'internal-find-face))

(defun map-face-colors (&rest args)
  "Usage: (this myface        darkbackcolor  darkforecolor
                              lightbackcolor lightforecolor
             ...repeats...)
   Set the back/foreground colors of face together.
   If color is an integer NN (0..99), it is mapped to greyNN."

  (let (myface darkbackcolor darkforecolor lightbackcolor lightforecolor)
    (while args
      (setq myface          (car args))
      (setq darkforecolor   (car (cdr args)))
      (setq darkbackcolor   (car (cdr (cdr args))))
      (setq lightforecolor  (car (cdr (cdr (cdr args)))))
      (setq lightbackcolor  (car (cdr (cdr (cdr (cdr args))))))
      (setq args            (cdr (cdr (cdr (cdr (cdr args))))))

      (if (numberp darkbackcolor)
          (setq darkbackcolor (concat "grey" (number-to-string darkbackcolor))))
      (if (numberp darkforecolor)
          (setq darkforecolor (concat "grey" (number-to-string darkforecolor))))
      (if (numberp lightbackcolor)
          (setq lightbackcolor (concat "grey" (number-to-string lightbackcolor))))
      (if (numberp lightforecolor)
          (setq lightforecolor (concat "grey" (number-to-string lightforecolor))))

      (if (find-face myface) nil (make-face myface))

      (cond
       ((eq hilit-background-mode 'dark)
        (set-face-foreground myface darkforecolor)
        (set-face-background myface darkbackcolor))
       (t
        (set-face-foreground myface lightforecolor)
        (set-face-background myface lightbackcolor))
        )
)))


(defun mosh-set-colors
  (&optional backcolor forecolor modecolor cursorcolor backlit)
  "Set the color and faces, to call from gnu .emacs
   backlit is light/dark."

  (interactive)

  (setq frame-background-mode backlit)
  (setq hilit-background-mode backlit)

  (if backcolor   (setq mosh-back-color   backcolor ))
  (if forecolor   (setq mosh-fore-color   forecolor ))
  (if modecolor   (setq mosh-mode-color   modecolor ))
  (if cursorcolor (setq mosh-curs-color cursorcolor ))
  
  (set-border-color "red")
  (set-mouse-color "yellow")

  (setq cperl-dark-background mosh-back-color)

  (cond
   (gnuemacs
    (set-cursor-color mosh-curs-color)
    (set-background-color mosh-back-color)
    (set-foreground-color mosh-fore-color))
   (xemacs
    (set-face-background 'default mosh-back-color)
    (set-face-foreground 'default mosh-fore-color)))

  (map-face-colors
   ; facename  darkfg   darkbg brightfg brightbg  <-- format.
   
    'default                         mosh-fore-color    mosh-back-color
                                     mosh-fore-color    mosh-back-color
    'italic                          "lightblue"        mosh-back-color
                                     "mediumblue"        mosh-back-color
    'bold                            "white"            "SaddleBrown"
                                     "white"            "burlywood4"
    'bold-italic                     "gold"             "blue"
                                     "gold"             "blue"
    'highlight                       mosh-back-color    "red"
                                     mosh-fore-color    "red"
    'isearch                         mosh-back-color    "lightblue"
                                     mosh-fore-color    "SandyBrown"
    'modeline                        mosh-back-color    mosh-mode-color
                                     mosh-fore-color    mosh-mode-color
    'query-replace                   "DarkBlue"         "thistle"
                                     "DarkBlue"         "thistle"
    'region                          "white"            "DarkOliveGreen"
                                     mosh-fore-color    "DarkOliveGreen"
    'region                          mosh-back-color    "lightblue"
                                     mosh-fore-color    "lightblue"
    'secondary-selection             mosh-back-color    "SandyBrown"
                                     mosh-fore-color    "SandyBrown"
    'underline                       "thistle"          "brown"
                                     mosh-fore-color    "sienna3"
    'show-paren-match-face           "black"            "red"
                                     "black"            "red"
    'ediff-current-diff-face-A       "red"              mosh-back-color
                                     "firebrick4"       mosh-back-color
    'ediff-current-diff-face-B       "yellow"           mosh-back-color
                                     "yellow"           mosh-back-color
    'font-lock-comment-face          "LightSalmon"      mosh-back-color
                                     "DarkRed"          mosh-back-color
  ; 'font-lock-function-name-face    "white"            "SaddleBrown"
  ;                                  "yellow"           "burlywood4"
    'font-lock-function-name-face    "white"            mosh-back-color
                                     "yellow"           mosh-back-color
    'font-lock-emphasized-face       "tomato"           mosh-back-color
                                     "DarkGreen"        mosh-back-color
    'font-lock-other-emphasized-face "PaleGreen"        mosh-back-color
                                     "PaleGreen"        mosh-back-color
    'font-lock-keyword-face          "Turquoise"        mosh-back-color
                                     "Darkblue"       mosh-back-color
    'font-lock-string-face           "LightSalmon"      mosh-back-color
                                     "DarkRed"          mosh-back-color  
    'font-lock-variable-name-face    "DarkGoldenrod"    mosh-back-color
                                     "DarkOrchid4"      mosh-back-color
    'font-lock-reference-face        "CadetBlue"        mosh-back-color
                                     "DarkBlue"         mosh-back-color
    'font-lock-warning-face          "Red"              mosh-back-color
                                     mosh-fore-color    "burlywood4"
    'font-lock-constant-face         "SkyBlue1"         mosh-back-color
                                     "SkyBlue4"         mosh-back-color
    'font-lock-builtin-face          "LightSteelBlue"   mosh-back-color
                                     "darkmagenta"      mosh-back-color
    'font-lock-type-face             "ForestGreen"      mosh-back-color
                                     "navy"             mosh-back-color
    'cperl-array-face                "LightGreen"       mosh-back-color
                                     "DarkGreen"        mosh-back-color
    'cperl-hash-face                 "red"              mosh-back-color
                                     "coral4"           mosh-back-color
    'vhdl-font-lock-attribute-face   "red"              mosh-back-color
                                     "coral4"           mosh-back-color
    'vhdl-font-lock-directive-face   "ForestGreen"      mosh-back-color
                                     "navy"             mosh-back-color
  )
)

; Test: (mosh-set-colors "grey12" "grey85" "LightGoldenrod3" "yellow")

;; ===================================================================
;; Coloring functions.
;; All other color functions call mosh-color-region
;; Gnuemacs: (overlay-lists) ; show overlays.

(defun mosh-color-region (from to &optional face)
    "Color region with face, default face is bold."
    (interactive "r")
    (or (find-face face) (setq face 'bold))
    (cond (gnuemacs (overlay-put (make-overlay from to) 'face face))
          (xemacs   (facemenu-add-face face from to))
))



(defun mosh-color-x-region (startf endf face)
  "Color region defined by motion functions startf and endf,
   donot move point.
   Eg: usage:
     (mosh-color-x-region 'beginning-of-line 'end-of-line 'blue)
     (mosh-color-x-region 'ignore 'end-of-line 'green)."
  (save-excursion (mosh-color-region
     (progn (funcall startf) (point))
     (progn (funcall endf) (point))
     face)
))


(defun mosh-color-char (face &optional point)
  "Color char with face."
  (let ((start (or point (point)))
        (end   (1+ (point))))
    (mosh-color-region start end face)
))

; Doesn't work without setting mark?
;(facemenu-set-face 'red (point) (+ 5 (point))) s
;(mosh-color-char 'bold (point))l

(defun mosh-color-word (count face)
    "Boldy go over coloring next count words."
  (let ((start (point)) end)
      (mosh-move-word count)
      (setq end (point))
    (mosh-color-region start end face))
)

; (mosh-color-line 'green)
; (setq face 'green)
(defun mosh-color-line (face)
  "Color current line with face."
  (let (start end)
    (save-excursion
      (beginning-of-line) (setq start (point))
      (end-of-line)       (setq end   (point)))
    (mosh-color-region start end face))
)

;  USAGE: (mosh-color-regexp "\t+\\| +$")
;  What:  show all tabs, and trailing spaces.

(defun mosh-color-regexp (regexp &optional color)
  "SYNOPSIS: Color all regexp in the buffer."
  (interactive "sRegexp to color:")
  (or color
      (setq color (read-face-name "Face:")))
  (save-excursion
    (beginning-of-buffer)
    (while (re-search-forward regexp (point-max) t)
      (mosh-color-region (match-beginning 0) (match-end 0) color)
)))


(defun mosh-color-rectangle (start end &optional color)
  "color the rectange formed by start and end with color."
    (interactive "r")
    (operate-on-rectangle
     '(lambda (a b c) (mosh-color-region a (point) color))
     start end t)
)

(defun mosh-color-tabs () "Color potential troublesome whitespaces."
  (interactive)
  (mosh-color-regexp "[ \t]+$"     'highlight) ; trailing ws.
  (mosh-color-regexp "\t+"         'bold)      ; tabs.
)

(defun mosh-color-match (&optional face submatch)
  "Color the last matched string."
  (interactive)
  (or submatch (setq submatch 0))
  (mosh-color-region (match-beginning submatch)
                     (match-end submatch)
                     face)
)

(defun mosh-color-c ()
  "Color some c syntax, See hilit19 and font-lock modes."
  (interactive)
  (mosh-color-regexp "[0-9_A-Za-z]+ *:[^:]" 'tomato)
  (mosh-color-regexp "[{}()]" 'saffron)
  (mosh-color-regexp
   (concat
    "\\b\\("
    "break\\|switch\\|default\\|case\\|continue\\|goto\\|"
    "return\\|if\\|else\\|while\\|struct\\|typedef"
    "\\)\\b")
   'tomato)
  (mosh-color-tabs)
)

(defun mosh-bold-pattern (regexp)
  "Color like vim's hlsearch"
 ;(mosh-color-regexp "mosh" 'bold)
 ;(remove-text-properties (point-min) (point-max) '(face nil) (current-buffer))
  (hs-discard-overlays (point-min) (point-max) 'face 'bold)
  (mosh-color-regexp regexp 'bold)
)

(provide 'mcolors)

; EOF
