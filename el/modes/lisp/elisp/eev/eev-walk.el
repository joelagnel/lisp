;;; eev-walk.el -- execute textual series of Emacs "actions" step by step

;; Copyright (C) 2005 Free Software
;; Foundation, Inc.
;;
;; This file is part of GNU eev.
;;
;; GNU eev is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU eev is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Author:     Eduardo Ochs <edrx@mat.puc-rio.br>
;; Maintainer: Eduardo Ochs <edrx@mat.puc-rio.br>
;; Version:    2005jul24
;; Keywords:   e-scripts, help, hyperlinks, hypertext, processes,
;;             shell, tex
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-walk.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-walk.el.html>
;;       See also: <http://angg.twu.net/eev-current/README.html>

;;; Commentary:

;; This is a visual version of the `eesteps' feature or eev.
;; It was inspired by:
;; http://lists.gnu.org/archive/html/emacs-devel/2005-06/msg01574.html
;; http://article.gmane.org/gmane.emacs.devel/39555
;;
;; More later.

(require 'eev)



;;;
;;; Basic functions
;;; 2005jul16--2005jul24
;;;

(defvar eewalk-marker nil)		; a buffer or window or frame

(defmacro eewalk-on-marker (&rest body)
  `(cond ((bufferp eewalk-marker)
	  (with-current-buffer eewalk-marker . ,body))
	 ((windowp eewalk-marker)
	  (with-selected-window eewalk-marker . ,body))
	 ((framep eewalk-marker)
	  (with-selected-window (frame-selected-window eewalk-marker)
	    . ,body))
	 (t (error "Not buffer, window, or frame: %S" eewalk-marker))))

(defun eewalk-skip-whitespace ()
  (looking-at "\\([ \t]*\\(;[^\n]*\\)?\n?\\)*")
  (goto-char (match-end 0)))
  
(defun eewalk-read ()
  (if (or (= (point) (point-max)) (= (char-after) ?\)))
      (error "No more steps for eewalk"))
  (read (current-buffer)))

(defun eewalk-flash-next-step ()
  (interactive)
  (eewalk-on-marker
   (save-excursion
     (eewalk-skip-whitespace)
     (eeflash (point) (progn (eewalk-read) (point))))))

(defun eewalk-do-walk ()
  (interactive)
  (eesteps-perform
   (eewalk-on-marker (eewalk-read)))
  (eewalk-on-marker (eewalk-skip-whitespace)))

(defun eewalk-do-walk-or-set (&optional arg)
  (interactive "P")
  (cond ((eq arg 0) (eewalk-flash-next-step))
	((eq arg 1) (eewalk-use-this-window)) ; mnemonic: wIndow - I=1
	((eq arg 2) (eewalk-use-this-buffer)) ; mnemonic: Buffer - B=2
	((eq arg 5) (eewalk-orig-frame))
	((eq arg 6) (eewalk-eewalk-frame))
	(t (eewalk-do-walk))))



;;;
;;; Functions to select a buffer of frame
;;; (and to switch between them)
;;;

(defvar eewalk-orig-frame nil)

(defun eewalk-use-this-window ()
  (interactive)
  (setq eewalk-marker (selected-window))
  (eewalk-on-marker (eewalk-skip-whitespace))
  (message
   "(use <F8> to read a step (from this window) and execute it (anywhere))")
  '(use <F8> to read a step (from this window) and execute it (anywhere)))

(defun eewalk-use-this-buffer ()
  (interactive)
  (setq eewalk-marker (current-buffer))
  (eewalk-on-marker (eewalk-skip-whitespace))
  (message
   "(use <F8> to read a step (from this buffer) and execute it (anywhere))")
  '(use <F8> to read a step (from this buffer) and execute it (anywhere)))

(defun eewalk-orig-frame ()
  (interactive)
  (select-frame-set-input-focus eewalk-orig-frame))

(defun eewalk-eewalk-frame ()
  (interactive)
  (select-frame-set-input-focus		; right now this works only
   (window-frame eewalk-marker))) 	; when eewalk-marker is a window



;;;
;;; Support for creating and using a dedicated frame
;;;

(defvar eewalk-make-frame-commands nil)

(defun  eewalk-make-frame-hook-function (frame)
  (remove-hook 'after-make-frame-functions
	       'eewalk-make-frame-hook-function)
  (select-frame-set-input-focus frame)
  (eval (cons 'progn eewalk-make-frame-commands)))

(defmacro eewalk-make-frame (&rest commands)
  "Switch to another frame and run COMMANDS there."
  `(progn (setq eewalk-make-frame-commands ',commands)
	  (add-hook 'after-make-frame-functions
		    'eewalk-make-frame-hook-function 'append)
	  (make-frame-command)))

(defmacro eewalk-make-frame-or-next (&rest commands)
  "Switch to the next frame or to another frame and run COMMANDS there.
Create another frame if there is only one frame visible.  The
other frame will be visiting the same buffer as this one, and
will have the same value of point."
  `(if (eq 1 (length (visible-frame-list)))
       (eewalk-make-frame . ,commands)
     (let ((buffer (current-buffer))
	   (pos (point)))
       (other-frame 1)
       (switch-to-buffer buffer)
       (goto-char pos)
       . ,commands)))



;;;
;;; eewalk-region
;;;

(defvar eewalk-region-prefix "\
;; Use <f8> to read steps from this window and execute
;; them (anywhere). Each step is either a string --
;; meaning a series of keys, in the format used by
;; `edmacro-mode' -- or a sexp to be evaluated. See:
;;   (find-efunctiondescr 'steps)
\(eewalk-orig-frame)\n\n")

(defun eewalk-region (s &optional e)
  (interactive "r")
  (let ((str (concat eewalk-region-prefix (ee-se-to-string s e))))
    (setq eewalk-orig-frame (selected-frame))
    (eval `(eewalk-make-frame-or-next
	    (let ((ee-buffer-name "*eewalk*")) (find-estring ,str))
	    ;; (eewalk-use-this-window)
	    ;; (eewalk-orig-frame)
	    (setq eewalk-marker (selected-window))
	    ))))

(defalias   'eewr 'eewalk-region)
(eeb-define 'eewr-bounded 'eewalk-region "\n;;\n" nil t t)

;; (eewr "\"foo\"")
;; (eewalk-duplicate-frame (find-estring "foo"))
;; (setq eewalk-marker (selected-window))
;; (eewalk-on-marker (eewalk-skip-whitespace))


(provide 'eev-walk)


;; Local Variables:
;; mode:              outline-minor
;; coding:            raw-text-unix
;; ee-anchor-format:  "«%s»"
;; ee-anchor-format:  "defun %s "
;; ee-comment-prefix: ";;"
;; no-byte-compile:   t
;; End:
