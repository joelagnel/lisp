;;; fstree.el --- Directory tree views
;;
;; Author: Mark Triggs <mst@dishevelled.net>
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;
;; Commentary:
;;
;; This uses tree-widget to display a directory tree that you can quickly
;; navigate and use to find files.  Bind `fstree-switch' to a key to use it.
;;
;;; Code:
;;

(require 'tree-widget)

(defvar fstree-hide-directories "^\\({arch}\\|\\.arch-ids\\|.bzr\\|CVS\\)$"
  "Don't expand directories matching this regular expression")

(defun fstree-expandable-p (directory)
  (and (not fstree-lazy-mode)
       (not (string-match fstree-hide-directories directory))))


(defvar fstree-directory-trees (make-hash-table :test #'equal))

(defun fstree-directory-tree (directory &optional force)
  (when (or force (not (gethash directory fstree-directory-trees)))
    (puthash directory (fstree-build-directory-tree directory)
             fstree-directory-trees))
  (gethash directory fstree-directory-trees))


(defun fstree-build-directory-tree (path &optional base)
  (let ((full-path (if base (concat base "/" path) path)))
    (if (file-directory-p full-path)
        (let ((expand-tree
               `(lambda (widget)
                  (mapcar (lambda (entry)
                            (fstree-build-directory-tree
                             entry ,full-path))
                          (remove-if (lambda (entry)
                                       (string-match "^\\.+$" entry))
                                     (directory-files ,full-path))))))
          (if (or (null base)
                  (fstree-expandable-p path))
              `(tree-widget :open t
                            :full-path ,full-path
                            :tag ,path
                            ,@(funcall expand-tree nil))
            `(tree-widget :open nil
                          :tag ,path
                          :has-children t
                          :full-path ,full-path
                          :dynargs ,expand-tree)))
      `(tree-widget :tag ,(propertize path :full-path full-path)))))


(defun fstree-insert-directory-tree (directory &optional reload)
  (widget-create
   'tree-widget :open t
   :no-leaf-handle ""
   :node (fstree-directory-tree directory reload)))


(defun fstree-normalise-directory (directory)
  (replace-regexp-in-string "/*$" "" directory))


(defvar fstree-last-dir nil
  "The last directory viewed with fstree.")

(defvar fstree-lazy-mode t
  "Whether fstree is in lazy mode.")


(defun fstree-build-buffer (directory &optional reload)
  (setq fstree-last-dir directory)
  (let ((window-configuration (current-window-configuration)))
    (let ((buffer (get-buffer-create "*fstree*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (fstree-insert-directory-tree directory reload)
          (fstree-mode)
          (goto-char (point-min))
          (set (make-local-variable 'fstree-root) directory)))
      buffer)))

(defun fstree-clear-cache ()
  (setq fstree-directory-trees
        (make-hash-table :test #'equal)))


(defun fstree-switch (&optional directory)
  (interactive (cond ((or current-prefix-arg (not fstree-last-dir))
                      (fstree-clear-cache)
                      (list (fstree-normalise-directory
                             (expand-file-name
                              (read-directory-name "Directory? ")))))
                     (t (list fstree-last-dir))))
  (let ((window-configuration (current-window-configuration)))
    (delete-other-windows)
    (split-window-horizontally)
    (enlarge-window (truncate (window-width) 2) t)
    (switch-to-buffer (or (and (not current-prefix-arg)
                               (get-buffer "*fstree*"))
                          (fstree-build-buffer directory)))
    (set (make-local-variable 'fstree-window-configuration)
         window-configuration)))


(defun fstree-find-file-at-point ()
  (interactive)
  (let ((file (get-text-property (1- (line-end-position)) :full-path)))
    (cond (file
           (fstree-quit)
           (find-file file))
          (t (fstree-move-to-nearest-button)
             (widget-button-press (point))))))


(defun fstree-quit ()
  (interactive)
  (set-window-configuration fstree-window-configuration)
  (when (string= (buffer-name (current-buffer))
                 "*fstree*")
    (bury-buffer)))


(defun fstree-move-up-dir ()
  (interactive)
  (unless (string= fstree-root "/")
    (setq fstree-root (file-name-directory fstree-root)))
  (fstree-refresh t))


(defun fstree-new-directory (&optional directory)
  (interactive (list (fstree-normalise-directory
                      (expand-file-name
                       (read-directory-name "Directory? "
                                            fstree-last-dir)
                       fstree-last-dir))))
  (setq fstree-root directory)
  (fstree-refresh t))


(defun fstree-refresh (&optional no-reload)
  (interactive)
  (fstree-clear-cache)
  (let ((window-configuration fstree-window-configuration))
    (fstree-build-buffer fstree-root (not no-reload))
    (set (make-local-variable 'fstree-window-configuration)
         window-configuration)))


(defun fstree-show-all ()
  (interactive)
  (let ((fstree-lazy-mode nil))
    (fstree-refresh)))

(defun fstree-next-file ()
  (interactive)
  (next-line 1)
  (beginning-of-line)
  (search-forward-regexp "-[-,] " nil t)
  (goto-char (match-end 0)))

(defun fstree-prev-file ()
  (interactive)
  (next-line -1)
  (beginning-of-line)
  (search-forward-regexp "-[-,] " nil t)
  (goto-char (match-end 0)))


(defvar fstree-mode-map (make-sparse-keymap) "The keymap for fstree")
(define-key fstree-mode-map (kbd "RET") 'fstree-find-file-at-point)
(define-key fstree-mode-map (kbd "SPC") 'fstree-expand-this-tree)
(define-key fstree-mode-map (kbd "a") 'fstree-show-all)
(define-key fstree-mode-map (kbd "d") 'fstree-open-in-dired)
(define-key fstree-mode-map (kbd "q") 'fstree-quit)
(define-key fstree-mode-map (kbd "^") 'fstree-move-up-dir)
(define-key fstree-mode-map (kbd "g") 'fstree-refresh)
(define-key fstree-mode-map (kbd "f") 'fstree-new-directory)
(define-key fstree-mode-map (kbd "TAB") 'widget-forward)
(define-key fstree-mode-map [backtab] 'widget-backward)
(define-key fstree-mode-map (kbd "n") 'fstree-next-file)
(define-key fstree-mode-map (kbd "p") 'fstree-prev-file)

(define-derived-mode fstree-mode fundamental-mode "fstree"
  "Mode for showing directory trees."
  (use-local-map fstree-mode-map)
  (setq buffer-read-only t))


;; Gross code to expand a subtree
(defun fstree-move-to-button-start ()
  (while (not (looking-at "\\["))
    (backward-char 1)))


(defun fstree-expand-this-node ()
  (when (looking-at "\\[\\+\\]")
    (widget-button-press (point))))


(defun fstree-widget-tag-at-point ()
  (widget-get (widget-get (widget-at (point)) :parent) :tag))


(defun fstree-move-to-nearest-button ()
  (goto-char (line-beginning-position))
  (while (and (< (point) (line-end-position))
              (not (get-char-property (point) 'button)))
    (forward-char 1))
  (unless (get-char-property (point) 'button)
    (ignore-errors (widget-backward 1))))


(defun fstree-find-parent-dir ()
  (unless (get-char-property (point) 'button)
    (end-of-line)
    (search-backward-regexp "[`|]--"))
  (let ((col (current-column)))
    (while (and (<= (- col (current-column)) 1))
      (ignore-errors (widget-backward 1)))))


(defun fstree-open-in-dired ()
  (interactive)
  (save-excursion
    (if (= (save-excursion
             (fstree-move-to-nearest-button)
             (current-line-number))
           (current-line-number))
        (fstree-move-to-nearest-button)
      (fstree-find-parent-dir))
    (let ((dir (widget-get (widget-get (widget-at (point))
                                       :parent)
                           :full-path)))
      (fstree-quit)
      (dired dir))))

(defun fstree-expand-this-tree ()
  (interactive)
  (unless (get-char-property (point) 'button)
    (error "Not currently looking at a button..."))
  (save-excursion
    (fstree-move-to-button-start)
    (fstree-expand-this-node)
    (let ((start-col (current-column)))
      (widget-forward 1)
      (while (> (current-column) start-col)
        (unless (string-match fstree-hide-directories
                              (fstree-widget-tag-at-point))
          (fstree-expand-this-node))
        (widget-forward 1)))))


(provide 'fstree)
;;; fstree.el ends here
