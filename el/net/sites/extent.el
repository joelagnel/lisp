;;; extent.el - extent compatibility for Emacs

;; Copyright (C) 2002 Sean MacLennan
;; Revision:   1.0
;; Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABIL`ITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; These extents *only* work on buffers, not strings

(require 'overlay)

;; SAM Can these be defalias?

;; complete - except that we do not support strings
(defun make-extent (from to &optional buffer-or-string)
  (and buffer-or-string (not (bufferp buffer-or-string))
       (error "make-extent: third arg must be a buffer"))
  (make-overlay from to buffer-or-string))

;; complete
(defun extent-start-position (extent) (overlay-start extent))
(defun extent-end-position (extent) (overlay-end extent))
(defun extentp (object) (overlayp object))

;; Currently only returns first overlay in list...
;; Not correct but works for slashdot.el and lxr.el
;; incomplete (very)
(defun extent-at (pos &optional object)
  (save-excursion
    (and object
	 (bufferp object)
	 (set-buffer object))
    (let ((overlays (overlays-at pos)))
      (car overlays))))

;; complete
(defun set-extent-property (extent property value)
  (overlay-put extent property value))

;; complete
(defun extent-property (extent property &optional default)
  (if default
      ;; Trickier - must handle nil property values
      (if (plist-member (overlay-properties extent) property)
	  (overlay-get extent property)
	default)
    (overlay-get extent property)))

;; complete
(defun set-extent-face (extent face)
  (overlay-put extent 'face face))


;; complete
(defun set-extent-mouse-face (extent face)
  (overlay-put extent 'mouse-face face))

;; complete
(defun set-extent-keymap (extent keymap)
  (overlay-put extent 'keymap keymap))

;; SAM ???
(defun next-extent (extent)
  (let* ((pos (1- (overlay-end extent)))
	 (next (next-overlay-change pos))
	 (overlay (overlays-at next)))
    (message "pos %S next %S overlay %S" pos next overlay)
    (when overlay
      (setq overlay (car overlay))
      (when (eq overlay extent) (error "PROBLEMS"))
      )
    overlay))

;; These should really not be here. But I use it with extents.
;; complete
(defun event-point (event) (nth 1 (event-start event)))

;; complete
(defun event-window (event) (car (event-start event)))

;; complete
(defun event-button (event)
  (let ((mouse (car event)))
    (cond ((eq mouse 'mouse-1) 1)
	  ((eq mouse 'mouse-2) 2)
	  ((eq mouse 'mouse-3) 3)
	  ((eq mouse 'mouse-4) 4)
	  ((eq mouse 'mouse-5) 5)
	  (t (error "Unsupported mouse event %S" mouse)))))

(provide 'extent)
