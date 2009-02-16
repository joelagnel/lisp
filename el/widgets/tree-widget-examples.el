;;; tree-widget-examples.el --- basic examples using the tree-widget

;; Copyright (C) 2001, 2003 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 27 Nov 2001
;; Keywords: extensions
;; Revision: $Id: tree-widget-examples.el,v 1.4 2003/09/29 13:41:21 ponced Exp $

(defconst tree-widget-examples-version "1.2")

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
;; This library gives simple examples of use of the `tree-widget'.
;;

;;; History:
;;

;;; Code:
(require 'tree-widget)

;;; Compatibility
;;
(cond
 ((featurep 'xemacs)
  (defalias 'tree-widget-example-overlay-lists
    (lambda () (list (extent-list))))
  (defalias 'tree-widget-example-delete-overlay
    'delete-extent))
 (t
  (defalias 'tree-widget-example-overlay-lists
    'overlay-lists)
  (defalias 'tree-widget-example-delete-overlay
    'delete-overlay))
 )

;;; The examples
;;
(defun tree-widget-example-close (&rest ignore)
  "Close the current dialog.
IGNORE arguments."
  (interactive)
  (kill-buffer (current-buffer)))

(defun tree-widget-example-1 (&optional theme)
  "A simple usage of the `tree-widget'.
Optional argument THEME is an image theme to use to draw the tree.  It
default to the global theme defined in option `tree-widget-theme'.
To be prompted for a theme, use
 \\[universal-argument] \\[tree-widget-example-1]."
  (interactive
   (list (if current-prefix-arg
             (completing-read "Theme name: "
                              '(("default" . "default")
                                ("folder"  . "folder")))
           nil)))
  (switch-to-buffer "*`tree-widget' example 1*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (tree-widget-example-overlay-lists)))
    (mapcar #'tree-widget-example-delete-overlay (car all))
    (mapcar #'tree-widget-example-delete-overlay (cdr all)))
  (tree-widget-set-theme theme)
  (widget-insert (format "%s. \n\n" (buffer-name)))
  (widget-create
   ;; Open this level.
   'tree-widget :open t
   ;; Use a push button for this node.
   :node '(push-button
           :tag "Root"
           :format "%[%t%]\n"
           :notify
           (lambda (&rest ignore)
             (message "This is the Root node")))
   ;; Add subtrees (their nodes defaut to items).
   '(tree-widget :tag "Node-1")
   '(tree-widget :tag "Node-2"
                 (tree-widget :tag "Empty-2.1")
                 (tree-widget :tag "Node-2.2"
                              (item :tag "Leaf-2.2.1")
                              (item :tag "Leaf-2.2.2")))
   '(tree-widget :tag "Node-3"
                 (tree-widget :tag "Empty-3.1")
                 (item        :tag "Leaf-3.2")))
  (widget-insert "\n")
  ;; Insert the Close button
  (widget-create 'push-button
                 :button-keymap tree-widget-button-keymap ; XEmacs
                 :keymap        tree-widget-button-keymap ; Emacs
                 :notify 'tree-widget-example-close
                 "Close")
  (use-local-map widget-keymap)
  (widget-setup))

(defvar tree-widget-example-11-open-nodes nil
  "List of name of tree nodes opened.
Initially all tree nodes are closed.")

(defun tree-widget-example-11-open-p (node-name)
  "Return non-nil if NODE-NAME is the name of an open tree node."
  (member node-name tree-widget-example-11-open-nodes))

(defun tree-widget-example-11-after-toggle-fcn (tree)
  "Update the list of open nodes `tree-widget-example-11-open-nodes'.
Called after each folding/unfolding of the `tree-widget' TREE.
See also the hook `tree-widget-after-toggle-functions'."
  (let ((node-name (widget-get tree :node-name))
        (open      (widget-get tree :open)))
    (if open
        (add-to-list 'tree-widget-example-11-open-nodes node-name)
      (setq tree-widget-example-11-open-nodes
            (delete node-name tree-widget-example-11-open-nodes)))))

(defun tree-widget-example-11 ()
  "A simple usage of the `tree-widget'.
Each node open/close state is persistent accross invocations of the
command."
  (interactive)
  (switch-to-buffer "*`tree-widget' example 11*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (tree-widget-example-overlay-lists)))
    (mapcar #'tree-widget-example-delete-overlay (car all))
    (mapcar #'tree-widget-example-delete-overlay (cdr all)))
  (make-local-hook 'tree-widget-after-toggle-functions)
  (add-hook 'tree-widget-after-toggle-functions
            'tree-widget-example-11-after-toggle-fcn nil t)

  (widget-insert (format "%s. \n\n" (buffer-name)))
  (widget-create
   ;; Open this level.
   'tree-widget
   :node-name "0" ;; Node unique ID
   :open (tree-widget-example-11-open-p "0")
   ;; Use a push button for this node.
   :node '(push-button
           :tag "Root"
           :format "%[%t%]\n"
           :notify
           (lambda (&rest ignore)
             (message "This is the Root node")))
   ;; Add subtrees (their nodes defaut to items).
   `(tree-widget :tag "Child-1"
                 :node-name "1"
                 :open ,(tree-widget-example-11-open-p "1")
                 )
   `(tree-widget :tag "Child-2"
                 :node-name "2"
                 :open ,(tree-widget-example-11-open-p "2")
                 (tree-widget :tag "Child-2.1"
                              :node-name "2.1"
                              :open ,(tree-widget-example-11-open-p "2.1")
                              )
                 (tree-widget :tag "Child-2.2"
                              :node-name "2.2"
                              :open ,(tree-widget-example-11-open-p "2.2")
                              (tree-widget :tag "Child-2.2.1"
                                           :node-name "2.2.1"
                                           :open ,(tree-widget-example-11-open-p "2.2.1")
                                           )
                              (tree-widget :tag "Child-2.2.2"
                                           :node-name "2.2.2"
                                           :open ,(tree-widget-example-11-open-p "2.2.2")
                                           )))
   `(tree-widget :tag "Child-3"
                 :node-name "3"
                 :open ,(tree-widget-example-11-open-p "3")
                 (tree-widget :tag "Child-3.1"
                              :node-name "3.1"
                              :open ,(tree-widget-example-11-open-p "3.1")
                              )
                 (tree-widget :tag "Child-3.2"
                              :node-name "3.2"
                              :open ,(tree-widget-example-11-open-p "3.2")
                              )))
  (widget-insert "\n")
  ;; Insert the Close button
  (widget-create 'push-button
                 :button-keymap tree-widget-button-keymap ; XEmacs
                 :keymap        tree-widget-button-keymap ; Emacs
                 :notify 'tree-widget-example-close
                 "Close")
  (use-local-map widget-keymap)
  (widget-setup))

(defun tree-widget-example-2-dynargs (widget)
  "Return the children definitions of WIDGET.
Reuse the cached :args property value if exists."
  (or (widget-get widget :args)
      '((tree-widget :tag "Empty-2.1")
        (tree-widget :tag "Node-2.2"
                     (item :tag "Leaf-2.2.1")
                     (item :tag "Leaf-2.2.2")))))

(defun tree-widget-example-2 ()
  "A simple usage of the `tree-widget' with dynamic expansion."
  (interactive)
  (switch-to-buffer "*`tree-widget' example 2*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (let ((all (tree-widget-example-overlay-lists)))
    (mapcar #'tree-widget-example-delete-overlay (car all))
    (mapcar #'tree-widget-example-delete-overlay (cdr all)))

  (widget-insert (format "%s. \n\n" (buffer-name)))

  (widget-create
   ;; Open this level.
   'tree-widget :open t
   ;; Use a push button for this node.
   :node '(push-button
           :tag "Root"
           :format "%[%t%]\n"
           :notify
           (lambda (&rest ignore)
             (message "This is the Root node")))
   ;; Add subtrees (their nodes defaut to items).
   '(tree-widget :tag "Node-1")
   ;; Dynamically retrieve children of this node.
   '(tree-widget :tag "Node-2"
                 :dynargs tree-widget-example-2-dynargs
                 :has-children t)
   '(tree-widget :tag "Node-3"
                 (tree-widget :tag "Empty-3.1")
                 (item        :tag "Leaf-3.2")))
  (widget-insert "\n")
  ;; Insert the Close button
  (widget-create 'push-button
                 :button-keymap tree-widget-button-keymap ; XEmacs
                 :keymap        tree-widget-button-keymap ; Emacs
                 :notify 'tree-widget-example-close
                 "Close")
  (use-local-map widget-keymap)
  (widget-setup))

(provide 'tree-widget-example)

;;; tree-widget-examples.el ends here
