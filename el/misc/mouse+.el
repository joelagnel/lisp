;;; mouse+.el --- Extensions to `mouse.el'.
;; 
;; Filename: mouse+.el
;; Description: Extensions to `mouse.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams
;; Copyright (C) 1996-2006, Drew Adams, all rights reserved.
;; Created: Fri Jun 28 14:47:12 1996
;; Version: 21.0
;; Last-Updated: Sat Aug 12 17:52:46 2006 (-25200 Pacific Daylight Time)
;;           By: dradams
;;     Update #: 299
;; URL: http://www.emacswiki.org/cgi-bin/wiki/mouse+.el
;; Keywords: mouse
;; Compatibility: GNU Emacs 20.x, GNU Emacs 21.x, GNU Emacs 22.x
;; 
;; Features that might be required by this library:
;;
;;   `mouse'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;;    Extensions to `mouse.el'.
;;
;;  Command `mouse-flash-position' highlights the character after the
;;  mouse pointer position, even as you drag it.  This can help make
;;  it clearer exactly where a `yank' will occur when you use
;;  `mouse-2'.  When you press `mouse-2', if the highlighted position
;;  is not exactly what you want, just keep `mouse-2' held while you
;;  move to the right location.  To enable this behavior, bind
;;  `mouse-flash-position' to `down-mouse-2'.
;;
;;
;;  Faces defined here:
;;
;;    `mouse-flash-position'.
;;
;;  Commands defined here:
;;
;;    `mouse-flash-position'.
;;
;;  Non-interactive functions defined here:
;;
;;    `mouse-flash-posn-track', `mouse-move-flash-posn-overlay'.
;;
;;  Constants defined here:
;;
;;    `mouse-flash-posn-overlay'.
;;
;;
;;  ***** NOTE: The following functions defined in `mouse.el' have
;;              been REDEFINED HERE:
;;
;;  `mouse-choose-completion' - Iconify *Completions* frame afterward.
;;  `mouse-tear-off-window' - Don't delete window if it is alone in
;;                            frame.  Instead, clone frame and window.
;;  `mouse-yank-secondary' - Error if (x-get-selection 'SECONDARY)=nil
;;
;;
;;  Do this in your init file (~/.emacs or ~/_emacs):
;;
;;   (require 'mouse+)
;;
;;  Suggested bindings:
;;
;;   (global-set-key [down-mouse-2] 'mouse-flash-position)
;;   (global-set-key [mode-line C-mouse-1] 'mouse-tear-off-window)
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change log:
;;
;; 2006/08/12 dadams
;;    mouse-flash-posn-overlay: Added mouse-face to overlay.
;;    mouse-flash-posn-track:
;;      Replaced push with setq...cons, to avoid runtime require of cl.el for Emacs 20.
;; 2006/08/11 dadams
;;    Added: mouse-flash-position (face and command), mouse-flash-posn-overlay,
;;           mouse-flash-posn-track, mouse-move-flash-posn-overlay.
;; 2004/09/28 dadams
;;    Added: mouse-yank-secondary.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:


(require 'mouse)
(and (< emacs-major-version 20)
     (eval-when-compile (require 'cl))) ;; when, unless

;;;;;;;;;;;;;;;;;;;;;;

(defface mouse-flash-position '((t (:background "Yellow")))
  "*Face used to highlight mouse position temporarily."
  :group 'mouse)

(defconst mouse-flash-posn-overlay
    ;; Create and immediately delete, to get "overlay in no buffer".
  (let ((ol (make-overlay (point-min) (point-min))))
    (delete-overlay ol)
    (overlay-put ol 'face 'mouse-flash-position)
    (overlay-put ol 'mouse-face 'mouse-flash-position)
    (overlay-put ol 'priority 1000000)
    ol)
  "Overlay to highlight current mouse position.")

(defun mouse-move-flash-posn-overlay (ol start end)
  "Move `mouse-flash-posn-overlay' to position END.
START is the position of the start of the current drag operation."
  (unless (= start end)
    ;; Go to START first, so that when we move to END, if it's in the middle
    ;; of intangible text, point jumps in the direction away from START.
    ;; Don't do it if START=END, otherwise a single click risks selecting
    ;; a region if it's on intangible text.  This exception was originally
    ;; only applied on entry to mouse-drag-region, which had the problem
    ;; that a tiny move during a single-click would cause the intangible
    ;; text to be selected.
    (goto-char start)
    (goto-char end)
    (setq end (point)))
  (move-overlay ol end (min (point-max) (1+ end))))

;; Inspired from `mouse-drag-region'.
;;;###autoload
(defun mouse-flash-position (start-event)
  "Highlight the mouse position as you drag the mouse.
This must be bound to a button-down mouse event.  If you bind this to
`down-mouse-2', and `mouse-2' is bound to `mouse-yank-at-click' (the
default), then the yank occurs just before the highlighted character.

If you want to prevent the `mouse-2' up-button yank from taking place,
perhaps because you changed your mind, you can press and hold `C-g'
while releasing the mouse button (press `mouse-2'; drag; press `C-g';
release `mouse-2'; release `C-g')."
  (interactive "e")
  (run-hooks 'mouse-leave-buffer-hook)  ; Let temporary modes such as isearch turn off.
  (mouse-flash-posn-track start-event))

(defun mouse-flash-posn-track (start-event)
  "Track mouse drags by highlighting the mouse position"
  (mouse-minibuffer-check start-event)
  (let* ((original-window (selected-window))
         (echo-keystrokes 0)
	 (start-posn (event-start start-event))
	 (start-point (posn-point start-posn))
	 (start-window (posn-window start-posn))
	 (start-window-start (window-start start-window))
	 (start-hscroll (window-hscroll start-window))
	 (bounds (window-edges start-window))
	 (make-cursor-line-fully-visible nil)
	 (top (nth 1 bounds))
	 (bottom (if (window-minibuffer-p start-window)
		     (nth 3 bounds)
		   (1- (nth 3 bounds))))) ; 1-: Don't count the mode line.
    (mouse-move-flash-posn-overlay mouse-flash-posn-overlay start-point start-point)
    (overlay-put mouse-flash-posn-overlay 'window start-window)
    (deactivate-mark)
    (unwind-protect
         (let (event end end-point last-end-point)
           (track-mouse
             (while (progn (setq event (read-event))
                           (or (mouse-movement-p event)
                               (memq (car-safe event) '(switch-frame select-window))))
               (unless (memq (car-safe event) '(switch-frame select-window))
                 (setq end (event-end event)
                       end-point (posn-point end))
                 (when (numberp end-point) (setq last-end-point end-point))
                 (cond
                   ((and (eq (posn-window end) start-window) ; Moving within original window.
                         (integer-or-marker-p end-point))
                    (mouse-move-flash-posn-overlay mouse-flash-posn-overlay
                                                   start-point end-point))
                   (t
                    (let ((mouse-row (cddr (mouse-position))))
                      (cond
                        ((null mouse-row))
                        ((< mouse-row top)
                         (mouse-scroll-subr start-window (- mouse-row top)
                                            mouse-flash-posn-overlay start-point))
                        ((>= mouse-row bottom)
                         (mouse-scroll-subr start-window (1+ (- mouse-row bottom))
                                            mouse-flash-posn-overlay start-point)))))))))
           ;; In case we did not get a mouse-motion event for the final move of
           ;; the mouse before a drag event, pretend that we did get one.
           (when (and (memq 'drag (event-modifiers (car-safe event)))
                      (setq end (event-end event)  end-point (posn-point end))
                      (eq (posn-window end) start-window)
                      (integer-or-marker-p end-point))
             (mouse-move-flash-posn-overlay mouse-flash-posn-overlay start-point end-point))
           (when (consp event)          ; Handle the terminating event.
             (let ((fun (key-binding (vector (car event)))))
               ;; Run the binding of the terminating up-event, if possible.
               (let* ((stop-point (if (numberp (posn-point (event-end event)))
                                      (posn-point (event-end event))
                                    last-end-point))
                      (drag-end (if (and stop-point (< stop-point start-point))
                                    (overlay-start mouse-flash-posn-overlay)
                                  (overlay-end mouse-flash-posn-overlay)))
                      (drag-start (- (+ (overlay-end mouse-flash-posn-overlay)
                                        (overlay-start mouse-flash-posn-overlay))
                                     drag-end))
                      last-command this-command)
                 (delete-overlay mouse-flash-posn-overlay)
                 (when (and (= start-hscroll (window-hscroll start-window))
                            (or end-point
                                (= (window-start start-window) start-window-start)))
                   (setq unread-command-events (cons event unread-command-events)))))))
      (delete-overlay mouse-flash-posn-overlay))))



;; REPLACES ORIGINAL in `mouse.el':
;; Only delete window if it is not the only one in frame.
;; Otherwise, this clones the frame and window.
;;;###autoload
(defun mouse-tear-off-window (event)
  "Create a new frame displaying buffer of window clicked on.
If window is not the only one in frame, then delete it.
Otherwise, this command effectively clones the frame and window."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let* ((window (posn-window (event-start event)))
	 (buf (window-buffer window))
	 (frame (make-frame)))
    (select-frame frame)
    (switch-to-buffer buf)
    (save-window-excursion (select-window window)
                           (unless (one-window-p) (delete-window window)))))



;; REPLACES ORIGINAL in `mouse.el':
;; Fixes bug when (x-get-selection 'SECONDARY) returns nil
;;;###autoload
(defun mouse-yank-secondary (event)
  "Insert the secondary selection at the position clicked on.
Move point to the end of the inserted text.
If `mouse-yank-at-point' is non-nil, insert at point
regardless of where you click."
  (interactive "e")
  ;; Give temporary modes such as isearch a chance to turn off.
  (run-hooks 'mouse-leave-buffer-hook)
  (or mouse-yank-at-point (mouse-set-point event))
  (let ((secondary (x-get-selection 'SECONDARY)))
    (if secondary
        (insert (x-get-selection 'SECONDARY))
      (error "No secondary selection"))))

  

;; REPLACES ORIGINAL in `mouse.el': 
;; Iconify *Completions* frame after choosing completion.
;; Free variable COMPLETION-REFERENCE-BUFFER is defined in `simple.el'.
;  ;;;###autoload
;(defun mouse-choose-completion (event)
;  "Click on an alternative in the `*Completions*' buffer to choose it."
;  (interactive "e")
;  ;; Give temporary modes such as isearch a chance to turn off.
;  (run-hooks 'mouse-leave-buffer-hook)
;  (let ((buffer (window-buffer))
;        choice
;	base-size)
;    (save-excursion
;      (set-buffer (window-buffer (posn-window (event-start event))))
;      (when completion-reference-buffer   ; Defined in `simple.el'.
;        (setq buffer completion-reference-buffer))
;      (setq base-size completion-base-size)
;      (save-excursion
;	(goto-char (posn-point (event-start event)))
;	(let (beg end)
;	  (when (and (not (eobp)) (get-text-property (point) 'mouse-face))
;            (setq end (point))
;            (setq beg (1+ (point))))
;	  (unless beg (error "No completion here"))
;	  (setq beg (previous-single-property-change beg 'mouse-face))
;	  (setq end (or (next-single-property-change end 'mouse-face)
;			(point-max)))
;	  (setq choice (buffer-substring beg end)))))
;    (save-window-excursion
;      (select-window (posn-window (event-start event)))
;      (when (one-window-p t 'selected-frame) (iconify-frame (selected-frame))))
;    (choose-completion-string choice buffer base-size))) ; In `simple+.el'.

;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mouse+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mouse+.el ends here
