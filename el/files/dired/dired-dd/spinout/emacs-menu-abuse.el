;;; emacs-menu-abuse.el -- Abusing emacs menu-bar as a Big Fat popup menu.

;; Perhaps makes sense in emacs-19.xx only.

;; Authour: Seiichi Namba <sn@asahi-net.email.ne.jp>
;; Copyright (C) 1997,1998 Seiichi Namba <sn@asahi-net.email.ne.jp>

;; Emacs-20 has C-down-mouse-3 for popup menu, but it's only for local mode.
;; This package enables to raise whole emacs menu bar (including local mode
;; menu) as A Big Fat popup menu.
;;
;; Of course, it makes sense in emacs-19.xx (mule-2.3), which lacks local 
;; mode menu popup.

;; BUG: Some (maybe a lot of, in emacs-20) menu items may not work.
;;      Regard this file as just another example of Emacs keymap handling.

;;
;; Version 2
;;
;; Old emacs-menu-abuse.el worked fine, but was not smart.
;; Mon Dec 15 20:41:20 1997, S.Namba <sn@asahi-net.email.ne.jp>
;;

;; Install:
;; Just load or autoload.  For example, include
;;
;;     (define-key global-map [C-M-mouse-3] 'menu:abuse-emacs-map-func)
;;       ;;or
;;       ;;(define-key global-map [C-M-down-mouse-3] 'menu:abuse-emacs-map-func)
;;     (require 'emacs-menu-abuse)
;;
;; in ~/.emacs, to use C-M-mouse-3 (or any other binding) for big fat menu.

;; If you want replace normal C-down-mouse-3 local mode map, use
;; (define-key global-map [C-down-mouse-3] 'menu:abuse-emacs-map-func)
;; in place of binding code above.

;;
;; Use "Rebuild" entry in the menu if your menu does not match with menu bar.
;; Even with that some menu might not work, though...
;; 
(defvar menu:abuse-menu-name "Emacs Big Fat Menu")
;; Noone is supposed to be initialized this map already here.
(setq menu:abuse-emacs-map (make-sparse-keymap menu:abuse-menu-name))
;; Real function is used now.  It can rebuild map by itself.
;;(fset 'menu:abuse-emacs-map-func menu:abuse-emacs-map)

(defun menu:abuse-emacs-map-func (ev &optional arg)
  "Steal original big emacs menu bar and pop it up."
  (interactive "e\nP")
  ;;(mouse-set-point ev)
  ;; Always make map all over.
  (menu:abuse-rebuild-map)
  (let* ((val (x-popup-menu ev menu:abuse-emacs-map))
	 (key (vector (car val) (car (cdr val)))) 
	 (fun (lookup-key menu:abuse-emacs-map key)))
    (if (and (symbolp fun)
	     (fboundp fun))
	;; Fake last-command-event mostly for menu-bar-select-buffer (emacs-20),
	;; but `buffer' entry is useless for buffer switching.
	;; You should use C-down-mouse-1 to switch buffer.
	;; No harm for emacs-19.  This `append' usage found in Elisp Info.
	;; But, even with this, lot more menu entries may not work well.
	(let ((last-command-event (car-safe (cdr-safe (append key nil)))))
	  (call-interactively fun)))))

(defun menu:abuse-rebuild-map ()
  "Call if your menu is not match with menu bar."
  (interactive)
  ;; Clear.
  (setq menu:abuse-emacs-map nil
	menu:abuse-emacs-map (make-sparse-keymap menu:abuse-menu-name))
  ;; Real function is used now.
  ;;(fset 'menu:abuse-emacs-map-func menu:abuse-emacs-map)
  (menu:abuse-do-map)
  (x-popup-menu nil menu:abuse-emacs-map))
(fset 'rebuild-my-menu 'menu:abuse-rebuild-map)

(defun menu:abuse-do-map ()
  ;; Mapping menu:abuse-rebuild-map directly in menu map causes
  ;; key name to be displayed in emacs-19, which is annoying.
  (define-key menu:abuse-emacs-map [menu:abuse-rebuild]
    (cons 
     "Rebuild This Menu" 'rebuild-my-menu))
  (define-key menu:abuse-emacs-map [menu:abuse-junk1]
    (cons 
     "----"  'undefined))
  ;; get global map
  (mapcar
   (function
    (lambda (tem)
      (if (and
	   (listp tem)
	   (car-safe tem)
	   (not (atom (cdr-safe tem))))
	  (define-key
	    menu:abuse-emacs-map
	    (vector (car-safe tem))
	    (cons (car-safe (cdr-safe tem))
		  (cdr (cdr tem))))
	'x
	)))
   (reverse (lookup-key global-map [menu-bar])))
  ;; get local map.
  ;; Some buffer has no local map (like fundamental-mode in emacs-20 ?).
  (if (null (current-local-map))
      ()
    (define-key menu:abuse-emacs-map [menu:abuse-junk2]
      (cons 
       "----"  'undefined))
    (mapcar
     (function
      (lambda (tem)
	(if (and
	     (listp tem)
	     (car-safe tem)
	     (not (atom (cdr-safe tem))))
	    (define-key
	      menu:abuse-emacs-map
	      (vector (car-safe tem))
	      (cons (car-safe (cdr-safe tem))
		    (cdr (cdr tem))))
	  'x
	  )))
     (reverse (lookup-key (current-local-map) [menu-bar])))) )

;; Make a cache.
(menu:abuse-rebuild-map)
;; Not required.  Now we use real function which remaps.
;;(add-hook 'menu-bar-update-hook 'menu:abuse-do-map)
;menu-bar-update-hook
;(rebuild-my-menu menu-bar-update-buffers win:update-menu-bar)

(provide 'emacs-menu-abuse)
