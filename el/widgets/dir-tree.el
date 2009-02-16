;;; dir-tree.el --- Sophisticated example of `tree-widget' usage

;; Copyright (C) 2001, 2003 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 16 Feb 2001
;; Keywords: extensions
;; Revision: $Id: dir-tree.el,v 1.4 2003/09/30 12:01:33 ponced Exp $

(defconst dir-tree-version "1.1")

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This library is a sophisticated example of usage of the
;; `tree-widget' library.
;;
;; It defines the command `dir-tree' which displays a directory tree.
;; A directory content is read when unfolding the corresponding node
;; and reused later.  To refresh a directory content just click on the
;; corresponding node name.  Also, you can change the select/unselect
;; state of a file entry by clicking on it.
;;
;; To install and use, put this file on your Emacs-Lisp load path and
;; add the following into your ~/.emacs startup file:
;;
;; (require 'dir-tree)
;;

;;; History:
;;

;;; Code:
(require 'tree-widget)

;;; Compatibility
;;
(if (fboundp 'overlay-lists)
    (defalias 'dir-tree-overlay-lists
      'overlay-lists)
  (defalias 'dir-tree-overlay-lists
    '(lambda () (list (extent-list)))))

(if (fboundp 'delete-overlay)
    (defalias 'dir-tree-delete-overlay
      'delete-overlay)
  (defalias 'dir-tree-delete-overlay
    'delete-extent))

;;; Widgets
;;
(define-widget 'dir-tree-dir-widget 'tree-widget
  "Directory Tree widget."
  :dynargs        'dir-tree-expand-dir
  :has-children   t)

(define-widget 'dir-tree-file-widget 'push-button
  "File widget."
  :doc            "*"
  ;; When unfolding the parent directory tree keep :doc value which
  ;; could be changed by `dir-tree-toggle-selection'
  :keep           '(:doc)
  :format         "%[%t%]%d"
  :button-face    'default
  :notify         'dir-tree-toggle-selection)

;;; Callbacks
;;
(defun dir-tree-toggle-selection (widget &rest ignore)
  "Change the selected state of WIDGET.
IGNORE other arguments."
  (let ((f (widget-get widget :tag ))
        (p (widget-get widget :path))
        (s (string= (widget-get widget :doc) "*")))
    (if s
        (widget-put widget :doc "-")
      (widget-put widget :doc "*"))
    ;; Redraw the tree node.
    (widget-value-set widget (widget-value widget))
    (message "File %s %s" p (if s "unselected" "selected"))))

(defun dir-tree-refresh-dir (widget &rest ignore)
  "Refresh WIDGET parent tree children.
IGNORE other arguments."
  (let ((tree (widget-get widget :parent)))
    ;; Clear the tree children cache.
    (widget-put tree :args nil)
    ;; Redraw the tree node.
    (widget-value-set tree (widget-value tree))))

(defun dir-tree-widget (e)
  "Return a widget to display file or directory E."
  (if (file-directory-p e)
      `(dir-tree-dir-widget
        :node (push-button
               :tag ,(file-name-as-directory (file-name-nondirectory e))
               :format "%[%t%]\n"
               :notify dir-tree-refresh-dir)
        :path ,e)
    `(dir-tree-file-widget
      :path ,e
      :tag  ,(file-name-nondirectory e))
    ))

(defun dir-tree-list (dir)
  "Return the list of entries in DIR.
Place directories first."
  (let ((entries (directory-files dir 'full))
        files dirs entry)
    (while entries
      (setq entry   (car entries))
      (setq entries (cdr entries))
      (if (not (string= (substring entry -1) "."))
          (if (file-directory-p entry)
              (setq dirs (cons entry dirs))
            (setq files (cons entry files)))))
    (nreverse (nconc files dirs))))

(defun dir-tree-expand-dir (tree)
  "Return TREE widget children.
Reuse :args cache if exists."
  (or (widget-get tree :args)
      (let ((dir (widget-get tree :path)))
        (message "Reading directory '%s'..." dir)
        (condition-case err
            (prog1
                (mapcar 'dir-tree-widget (dir-tree-list dir))
              (message "Reading directory '%s'...done" dir))
          (error
           (message "%s" (error-message-string err))
           nil)))))

;;; Command
;;
(defun dir-tree (root)
  "Display a tree of entries in ROOT directory."
  (interactive "DRoot: ")
  (switch-to-buffer (format "* %s directory tree*" root))
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (dir-tree-overlay-lists)))
    (mapcar 'dir-tree-delete-overlay (car all))
    (mapcar 'dir-tree-delete-overlay (cdr all)))
  (tree-widget-set-theme "folder")
  (widget-insert (format "%s directory tree. \n\n" root))
  (let ((default-directory root))
    (widget-create (dir-tree-widget root)))
  (use-local-map widget-keymap)
  (widget-setup)
  (goto-char (point-min)))

(provide 'dir-tree)

;;; dir-tree.el ends here
