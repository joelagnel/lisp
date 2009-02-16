;;; toggle-window-dedicated.el -- Toggle wether a window is dedicated 

;;; Copyright (c) 2001 by Daniel Lundin <daniel@codefactory.se>
;;; Copyright (c) 2001 CodeFactory AB

;; Author: Daniel Lundin <daniel@codefactory.se>


;; Toggle window dedication
(defun toggle-window-dedicated ()
"Toggle wether the current active window is dedicated or not"
(interactive)
(message 
 (if (let (window (get-buffer-window (current-buffer)))
       (set-window-dedicated-p window 
		(not (window-dedicated-p window))))
    "Window '%s' is dedicated"
    "Window '%s' is normal")
 (current-buffer)))

;;Sample key binding
(global-set-key [pause] 'toggle-window-dedicated)

