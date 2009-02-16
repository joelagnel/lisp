;;; Saved through ges-version 0.3.3dev at 2002-12-29 14:21
;;; From: Ian Zimmerman <itz@speakeasy.org>
;;; Subject: wmanip.el -- now with Commentary explaining intended usage :)
;;; Newsgroups: gnu.emacs.sources
;;; Cc: debian-emacsen@lists.debian.org
;;; Date: 28 Dec 2002 12:33:38 -0800
;;; Organization: http://extra.newsguy.com


;;; wmanip.el --- manipulate windows from the keyboard

;; Copyright (C) Ian Zimmerman, December 2002

;; Terms: GNU General Public License, Version 2

;;; Commentary:

;; This is _not_ a permanent minor mode like, say, Auto
;; Fill, which you turn on and go about your editing.  What I wanted to
;; do was: get in through my preferred binding (C-cw), adjust the windows
;; to my liking, and get out with Esc-Esc.

;;; Code:

(defvar wmanip-mode nil
  "The global flag for the wmanip minor mode.")

(defun wmanip-mode (&optional arg)
  "Set or toggle the wmanip minor mode.
\\<wmanip-mode-map>  This mode provides a keyboard interface to the window
manipulation commands {enlarge,shrink}-window{,-horizontally} which is
actually usable.  The keybindings for these commands that are built
into Emacs (C-x^, C-x{, C-x}) are terrible because repeating them
means stretching your hands back and forth over the keyboard.

\\[other-window] - Select the ARG'th different window on this frame.
\\[enlarge-window] - Make current window ARG lines bigger.
\\[shrink-window] - Make current window ARG lines smaller.
\\[enlarge-window-horizontally] - Make current window ARG columns wider.
\\[shrink-window-horizontally] - Make current window ARG columns narrower."

  (interactive "P")
  (let ((goal
         (if (null arg) (not wmanip-mode)
           (> (prefix-numeric-value arg) 0))))
    (if (or (and goal wmanip-mode) (and (not goal) (not wmanip-mode))) nil
      (setq wmanip-mode (not (null goal)))
      (force-mode-line-update 'all))))

(defsubst turn-on-wmanip-mode ()
  "Turn on the wmanip minor mode."
  (interactive)
  (wmanip-mode 1))

(defsubst turn-off-wmanip-mode ()
  "Turn off the wmanip minor mode."
  (interactive)
  (wmanip-mode 0))

(or (assq 'wmanip-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(wmanip-mode " Wmanip") minor-mode-alist)))

(defconst wmanip-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap "\C-i" 'other-window)
    (define-key kmap "\M-\e" 'turn-off-wmanip-mode)
    (define-key kmap "/" 'enlarge-window)
    (define-key kmap "\\" 'shrink-window)
    (define-key kmap "[" 'enlarge-window-horizontally)
    (define-key kmap "]" 'shrink-window-horizontally)
    (define-key kmap "-" 'negative-argument)
    (mapcar (lambda (ch) (define-key kmap ch 'digit-argument))
            '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
    kmap)
  "Keymap used in wmanip mode.")

(or (assq 'wmanip-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (cons (cons 'wmanip-mode wmanip-mode-map) minor-mode-map-alist)))

(provide 'wmanip)

;;; wmanip.el ends here

