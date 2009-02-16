;;; buffer-ring.el --- allow easy switching between a set of buffers

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; I can't help but get the feeling I'm re-inventing the wheel here, but this
;; code lets you maintain a ring of buffers you are currently interested in,
;; and makes it easy to quickly switch between them.

;;; Code:

(defvar buffer-ring '() "A ring of buffers currently of interest")
(defvar buffer-index 0 "The current index into BUFFER-RING")

(defun add-buffer-to-ring (&optional buffer)
  "Add BUFFER to BUFFER-RING. If BUFFER is not supplied, add the
current buffer"
  (interactive)
  (cond ((null buffer) (pushnew (current-buffer) buffer-ring))
        ((bufferp buffer) (pushnew buffer buffer-ring))
        ((stringp buffer) (let ((b (find-if (lambda (b)
                                              (string= (buffer-name b) buffer))
                                            (buffer-list))))
                            (when b
                              (pushnew b buffer-ring))))
        (t (error "Invalid buffer - %s" buffer))))

(defun remove-buffer-from-ring (&optional buffer)
  "Remove BUFFER from BUFFER-RING. If BUFFER is not supplied, remove the
current buffer"
  (interactive)
  (cond ((null buffer) (setq buffer-ring
                             (remove (current-buffer) buffer-ring)))
        ((bufferp buffer) (setq buffer-ring (remove buffer buffer-ring)))
        ((stringp buffer) (let ((b (find-if (lambda (b)
                                              (string= (buffer-name b) buffer))
                                            (buffer-list))))
                            (when b
                              (setq buffer-ring
                                    (remove b buffer-ring)))))
        (t (error "Invalid buffer - %s" buffer))))

(defun switch-next-buffer-ring ()
  "Switch to the next buffer in BUFFER-RING"
  (interactive)
  (when (> (length buffer-ring) 1)
    (while (equal (current-buffer) (nth buffer-index buffer-ring))
      (setq buffer-index (mod (1+ buffer-index) (length buffer-ring))))
    (switch-to-buffer (nth buffer-index buffer-ring))))


(add-hook 'kill-buffer-hook 'remove-buffer-from-ring)

(provide 'buffer-ring)
;;; buffer-ring.el ends here
