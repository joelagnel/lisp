;; teatime.el --- Remind me about my cup of tea

;; Author: Mark Triggs <mst@dishevelled.net>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A lovely idea from gteatime.  Probably the best idea ever conceived.

;;; Code:

(defvar *tea-brewing-seconds* (* 3 60))


(defun make-teacup (colour)
  (create-image (format "/* XPM */
static char * teacup_xpm[] = {
\"18 13 3 1\",
\" 	c None\",
\".	c #000000\",
\"#	c %s\",
\"                  \",
\"  ............    \",
\"  .##########.    \",
\"  .##########...  \",
\"  .##########.  . \",
\"  .##########.  . \",
\"  .##########.  . \",
\"  .##########. .  \",
\"  .##########..   \",
\"  ..########..    \",
\"   ..######..     \",
\"    ........      \",
\"                  \"};"
                        colour) 'xpm t
                        :ascent 'center))


(defvar *teacup-colours*
  '("#ffffff" "#e2b47c" "#ce7100" "#9c5500" "#693900" "#4f2b00" "#000000"))

(defvar *current-teacup* nil)

(defvar *teacup-timer* nil)



(defun teacup-show ()
  (if (and *current-teacup*
           (nth *current-teacup* *teacup-colours*))
      (let ((map (make-sparse-keymap)))
        (define-key map [mode-line mouse-1]
          'stop-tea)
        `(:propertize "!"
                      display ,(make-teacup (nth *current-teacup*
                                                 *teacup-colours*))
                      help-echo "Don't forget your tea!"
                      keymap ,map))
    '()))


(defun stop-tea ()
  (interactive)
  (when (timerp *teacup-timer*)
    (cancel-timer *teacup-timer*))
  (setq *current-teacup* nil)
  (force-mode-line-update))


(defun make-tea ()
  "Start a new cup of tea."
  (interactive)
  (stop-tea)
  (setq *current-teacup* 0)
  (setq *teacup-timer* (run-at-time
                        (truncate *tea-brewing-seconds*
                                  (1- (length *teacup-colours*)))
                        (truncate *tea-brewing-seconds*
                                  (1- (length *teacup-colours*)))
                        'next-teacup))
  (force-mode-line-update))


(defun next-teacup ()
  "Move to the next cup of tea."
  (interactive)
  (incf *current-teacup*)
  (when (>= *current-teacup* (1- (length *teacup-colours*)))
    (when (timerp *teacup-timer*)
      (cancel-timer *teacup-timer*)))
  (force-mode-line-update))


(push '(:eval (teacup-show)) global-mode-string)



(provide 'teatime)
;;; teatime.el ends here
