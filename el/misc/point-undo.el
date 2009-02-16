;;; point-undo.el --- undo/redo position

;;  Copyright (C) 2006 rubikitch <rubikitch@ruby-lang.org>
;;  Version: $Id: point-undo.el 1221 2006-02-27 03:54:15Z rubikitch $

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;    This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;    You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; This package allows you to undo/redo point.
;; It is like w3m's UNDO/REDO commands.

;;; Setup:

;; (require 'point-undo)
;; (define-key global-map [f5] 'point-undo)
;; (define-key global-map [f6] 'point-redo)

;;; History:
;; 
;; [2006/02/27] Initial release

;;; Code:
(defvar point-undo-list nil)
(make-variable-buffer-local 'point-undo-list)

(defvar point-redo-list nil)
(make-variable-buffer-local 'point-redo-list)

(defun point-undo-pre-command-hook ()
  "Save positions before command."
  (unless (or (eq this-command 'point-undo)
              (eq this-command 'point-redo))
    (setq point-undo-list (cons (point) point-undo-list))
    (setq point-redo-list nil)
  ))
(add-hook 'pre-command-hook 'point-undo-pre-command-hook)

(defun point-undo ()
  "Undo position."
  (interactive)
  (let ((pt (car point-undo-list)))
    (when pt
      (setq point-undo-list (cdr point-undo-list))
      (setq point-redo-list (cons (point) point-redo-list))
      (goto-char pt)
      )))

(defun point-redo ()
  "Redo position."
  (interactive)
  (when (or (eq last-command 'point-undo)
            (eq last-command 'point-redo))
    (let ((pt (car point-redo-list)))
      (when pt
        (setq point-undo-list (cons (point) point-undo-list))
        (setq point-redo-list (cdr point-redo-list))
        (goto-char pt)))))

(provide 'point-undo)

;;; point-undo.el ends here

