;;; bbdb-display-fsf.el -- FSF Specific definitions for bbdb-display.

;;; This file is an extension to the Insidious Big Brother Database (aka BBDB),
;;; Copyright (c) 1994 Boris Goldowsky <boris@cs.rochester.edu>
;;; Derived from bbdb-lucid.el, (c) 1992 Jamie Zawinski <jwz@lucid.com>.
;;; Last change 1-jun-94.

;;; This is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or (at your
;;; option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'lmenu) ; for 19.24 
; (require 'menubar "lmenu") ; for 19.23 and before

(define-key bbdb-mode-map [down-mouse-3] 'bbdb-menu)

(or (internal-find-face 'bbdb-name)
    (face-differs-from-default-p (make-face 'bbdb-name))
    (copy-face 'underline 'bbdb-name))

(or (internal-find-face 'bbdb-company)
    (face-differs-from-default-p (make-face 'bbdb-company))
    (copy-face 'italic 'bbdb-company))

(or (internal-find-face 'bbdb-field-value)
    (copy-face 'default 'bbdb-field-value))

(or (internal-find-face 'bbdb-field-name)
    (face-differs-from-default-p (make-face 'bbdb-field-name))
    (copy-face 'bold 'bbdb-field-name))

(defalias 'bbdb-extent-start-position 'overlay-start)

(defun bbdb-extent-face (e)
  (overlay-get e 'face))

(defun bbdb-overlay-length (o)
  (- (overlay-end o) (overlay-start o)))

(defun bbdb-extent-at (pos buffer prop)
  ;; compatibility function added by Bng.
  ;; in lucid, extent-at
  "Find smallest overlay enclosing POSITION in BUFFER that has PROPERTY.
Returns nil if no such overlay was found."
  (interactive "e")
  (let ((obuf (current-buffer)))
    (set-buffer buffer)
    (let* ((list (overlays-at pos))
	   best bestlength)
      (while list
	(if (overlay-get (car list) prop)
	    (let ((length (bbdb-overlay-length (car list))))
	      (if (or (null best) (< length bestlength))
		  (setq best (car list)
			bestlength length))))
	(setq list (cdr list)))
      (set-buffer obuf)
      best)))

(defun bbdb-make-extent (from to &optional face highlight)
  (let ((o (make-overlay from to)))
    (overlay-put o 'bbdb t)
    (if face
	(overlay-put o 'face face))
    (if (eq highlight 'highlight)	;ignore if it is 'region
	(overlay-put o 'mouse-face 'highlight))
    o))

(defun bbdb-delete-extents ()
  ;; delete existing extents
  (let* ((ol (overlay-lists))
	 (overlays (append (car ol) (cdr ol))))
    (while overlays
      (if (overlay-get (car overlays) 'bbdb)
	  (delete-overlay (car overlays)))
      (setq overlays (cdr overlays)))))

(provide 'bbdb-display-fsf)

;;; bbdb-display-fsf.el ends here.
