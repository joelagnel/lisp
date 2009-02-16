;;; bbdb-display-lucid.el -- Lucid GNU Emacs definitions for bbdb-display.

;;; This file is the part of the Insidious Big Brother Database (aka BBDB),
;;; Copyright (c) 1992 Jamie Zawinski <jwz@lucid.com>.
;;; Derived from bbdb-lucid.el, (c) 1992 Jamie Zawinski <jwz@lucid.com>.
;;; Last change 31-may-94.

;;; The Insidious Big Brother Database is free software; you can redistribute
;;; it and/or modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or (at your
;;; option) any later version.
;;;
;;; BBDB is distributed in the hope that it will be useful, but WITHOUT ANY
;;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;;; details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; This code is kind of kludgey, mostly because it needs to parse the contents
;;; of the *BBDB* buffer, since BBDB doesn't save the buffer-positions of the
;;; various fields when it fills in that buffer (doing that would be slow and
;;; cons a lot, so it doesn't seem to be worth it.)

;;; Only needed since bbdb.el assumes the old version of bbdb-display:
(remove-hook 'bbdb-list-hook 'bbdb-fontify-buffer)

(define-key bbdb-mode-map 'button3 'bbdb-menu)

(or (find-face 'bbdb-name)
    (face-differs-from-default-p (make-face 'bbdb-name))
    (set-face-underline-p 'bbdb-name t))

(or (find-face 'bbdb-company)
    (face-differs-from-default-p (make-face 'bbdb-company))
    (make-face-italic 'bbdb-company))

(or (find-face 'bbdb-field-value)
    (make-face 'bbdb-field-value))

(or (find-face 'bbdb-field-name)
    (face-differs-from-default-p (make-face 'bbdb-field-name))
    (copy-face 'bold 'bbdb-field-name))

(defalias 'bbdb-extent-start-position 'extent-start-position)

(defalias 'bbdb-extent-face 'extent-face)

(defalias 'bbdb-extent-at 'extent-at)

(defun bbdb-delete-extents ()
  (map-extents (function (lambda (x y)
			   (if (extent-property x 'bbdb)
			       (delete-extent x))))
	       (current-buffer) (point-min) (point-max) nil))

(defun bbdb-make-extent (from to &optional face highlight)
  (let ((e (make-extent from to)))
    (set-extent-property e 'bbdb t)
    (if face
	(set-extent-face e face))
    (if highlight
	(set-extent-property e 'highlight t))
    e))

(provide 'bbdb-display-lucid)
