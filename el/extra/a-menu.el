;;; a-menu.el --- create a menu from a specified directory

;; Copyright (C) 2001-2004  Seiji Zenitani <zenitani@mac.com>

;; Author: Seiji Zenitani <zenitani@mac.com>
;; Version: 1.3
;; Keywords: tools
;; Created: 2001-08-01
;; Compatibility: Emacs 21, 20
;; URL: http://home.att.ne.jp/alpha/z123/elisp-e.html

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

;; This package provides `a-menu' function.
;; It enables you to build a your own menu in the menu bar,
;; based on the structure of the specified directory.
;;
;; Let us assume the following files and directories:
;;
;;     ~/directory/
;;     ~/directory/Test
;;     ~/directory/Test/Item_1.el
;;     ~/directory/Test/Item_2.el.
;;
;; A code
;;
;;     (a-menu "~/directory/Test")
;;
;; will create "Test" menu in the menu bar, and the names of
;; the *.el files are shown in it. Choosing one of menu items loads
;; the relevant *.el file, and so emacs executes lisp codes in it.
;; To create a hierarchical structure in the menu, just make subdirectories.


;;; Code:

(defun a-menu (dir &optional sub-menu)
  "Enable you to build a your own menu in the menu bar,
based on the directory named DIR.

The second argument SUB-MENU is internally used."
  (interactive "P")
  (let* ((dir (expand-file-name dir))
         (nondir (file-name-nondirectory dir))
         (menu (concat "menu-bar-" dir "-menu")))

    (or (file-directory-p dir)(error (format "%s not found" dir)))
    (eval
     (read
      (concat
       "(progn "
       "(setq " menu " (make-sparse-keymap))"
       "(define-key " menu " [a-menu-" dir "-refresh] "
       " '(\"Refresh This Menu\" . (lambda() (interactive)"
       "(a-menu \"" dir "\" " (format "%s" sub-menu) "))))"
       "(define-key " menu " [a-menu-" dir "-last-separator] '(\"---\"))"
       ")\n"
       )))
    
    (let ((elc (reverse (directory-files dir t))))
      (while elc
        (let* ((item (car elc))
               (item-name (file-name-sans-extension
                           (file-name-nondirectory item))))
          (if (file-readable-p item)
              (cond
               ((file-directory-p item)
                (if (not (string-match "^\\." (file-name-nondirectory item)))
                    (a-menu item t)))
               ((string-match "\\.el$" item)
;;               ((not (equal ".DS_Store" (file-name-nondirectory item))) ;; Mac OS X
                (let ((menu-name item-name))
                  (while (string-match "_" menu-name)
                    (setq menu-name (replace-match " " t nil menu-name)))
                  (eval
                   (read
                    (format "(define-key %s [a-menu-%s]
 '(\"%s\" .(lambda()(interactive)(load-file \"%s\"))))\n"
                            menu item-name menu-name item)
                    ))
                  ))
               )))
        (setq elc (cdr elc))
        ))
      (let ((parent (substring (file-name-directory dir) 0 -1))
            (menu-name nondir))
        (while (string-match "_" menu-name)
          (setq menu-name (replace-match " " t nil menu-name)))
        (if sub-menu
            (eval
             (read
              (concat
               "(define-key menu-bar-" parent "-menu [a-menu-" nondir "] "
               "(list 'menu-item \"" menu-name "\" menu-bar-" dir "-menu" "))"
               )))
          (eval
           (read
            (concat
             "(progn"
             "(global-set-key [menu-bar a-menu-" dir "] (cons \""
             menu-name "\" " menu "))\n"
             "(setq menu-bar-final-items (cons 'a-menu-" dir
             " menu-bar-final-items))\n)"
             )))
        ))

    ))

(provide 'a-menu)

(a-menu "e:/lisp/emacs/elisp")

;;; a-menu.el ends here.