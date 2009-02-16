;;; bookmark-popmenu.el -- Another abuse example of emacs menu system.

;; Authour: Seiichi Namba <sn@asahi-net.email.ne.jp>
;; Copyright (C) 1997,1998 Seiichi Namba <sn@asahi-net.email.ne.jp>

;; Sat Jul  4 16:49:02 1998 Changed bindig to M-S... to match dired-dd
;; Sun Dec 14 01:02:49 1997 keymap version.
;; Fri Dec 12 15:34:14 1997
;;
;; You may need these lines
;; (if (> (string-to-int emacs-version) 19)
;;       (setq bookmark-file "~/.emacs-bkmrks20")
;;   (setq bookmark-file "~/.emacs-bkmrks19"))
;; (if (file-exists-p bookmark-file)
;;     (bookmark-load bookmark-file t t))
;; before
;; (if window-system (load "bookmark-popmenu"))
;; line in your ~/.emacs.

;; (require 'bookmark)			; not required.

;;
;; You can use these in dired-mode. Try minibuffer area.
;;
;;(defvar mybookmarkmenu nil)

;; This should be made from copy-keymap'ed map ?
(setq mybookmarkmenu
 (cons 'keymap
       (cons "Bookmark Menu" (copy-keymap menu-bar-bookmark-map))))
(fset 'my-bookmark-menu mybookmarkmenu)

;;Changed bindig to M-S... to match dired-dd
;;(define-key global-map [C-M-mouse-2] 'my-bookmark-menu)
;;(define-key global-map [C-M-mouse-1] 'my-bookmark-jump)
(define-key global-map [M-S-mouse-2] 'my-bookmark-menu)
(define-key global-map [M-S-mouse-1] 'my-bookmark-jump)

(defun my-bookmark-jump (ev &optional arg)
  (interactive "e\nP")
  (mouse-set-point ev)
  (if arg 
      (bookmark-jump arg)
    ;; mule2(19.34) has 20.2 compat bookmark.el
    ;;(if (> (string-to-int emacs-version) 19)
    (if (string< emacs-version "19.29")  ;; in 19.28.xx => t. 
	(bookmark-make-menu-bar-with-function 'bookmark-jump
					    "Bookmark Jump Menu"
					    "--- Jump to Bookmark ---"
					    ev)
      (bookmark-popup-menu-and-apply-function  ;; 20.x sheame.
	 'bookmark-jump "Jump to Bookmark" ev)) ))

;;menu-bar-bookmark-map  ==>
;;(keymap (jump "Jump to bookmark" (nil) . bookmark-menu-bar-jump)
;;	(set "Set bookmark" ([24 114 109] . "  (C-x r m)") . bookmark-set)
;;	(insert "Insert contents" (nil) . bookmark-menu-bar-insert)
;;	(locate "Insert location" (nil) . bookmark-menu-bar-locate)
;;	(rename "Rename bookmark" (nil) . bookmark-menu-bar-rename)
;;	(delete "Delete bookmark" (nil) . bookmark-menu-bar-delete)
;;	(edit "Edit Bookmark List" ([24 114 108] . "  (C-x r l)") . list-bookmarks)
;;	(save "Save  (in default file)" (nil) . bookmark-save)
;;	(write "Write (to another file)" (nil) . bookmark-write)
;;	(load "Load a bookmark file" (nil) . bookmark-load)
;;	"Bookmark functions."))

(provide 'bookmark-popmenu)
