
(defun x-window-command-menu (arg)
  (let ((menu-selection
	 (x-popup-menu
	  arg
	  '("Window Command Menu"
	    ("Vertical window commands"
	     ("Split window" . split-window-vertically)
	     ("Enlarge window" . enlarge-window)
	     ("Shrink window" . shrink-window-vertically)
	     ("One window" . delete-other-windows))
	    ("Horizontal Window Commands"
	     ("Split window" . split-window-horizontally)
	     ("Enlarge window" . enlarge-window-horizontally)
	     ("Shrink window" . shrink-window-horizontally)
	     ("One window" . delete-other-windows))))))
    (if (and (menu-selection (x-mouse-select arg))
	     (call-interactively menu-selection)))))

(define-key mouse-map x-button-c-right 'x-window-command-menu) 