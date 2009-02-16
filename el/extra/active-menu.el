;;                                                     -*- Emacs-Lisp -*-

;;; active-menu.el --- Show menubar only when the mouse is at the top of
;;;                    the frame.

;;
;; Copyright (C) 2002, 2003 Claus Brunzema <mail@cbrunzema.de>
;;                          Stefan Kamphausen <mail@skamphausen.de>

;; Version: 1.2.0
;; $Id: active-menu.el,v 1.19 2003/10/31 10:51:24 chb Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;; -----------------------------------------------------------------------


;;; Commentary:

;; Prerequisites:
;;
;; I made active-menu with the following xemacs:
;; XEmacs 21.4 (patch 6) "Common Lisp" [Lucid] (i386-debian-linux) of Sat Apr  6 2002 on eeyore"
;; Please let me know if other versions work.


;; Installation:
;;
;; put this file in your load-path and the following in your init
;; file (~/.emacs or ~/.xemacs/init.el) if you want to use the
;; customisation facility:
;;
;; (require 'active-menu)
;;
;; If you turn active-menu on and off frequently, you might want to use
;;
;; (autoload 'active-menu
;;           "active-menu"
;;           "Show menu only when mouse is at the top of the frame."
;;           t)
;;
;; instead. Now you can toggle active-menu with M-x active-menu (it is
;; deactivated by default). If your frames change their height when
;; the menu is toggled, try to find a suitable value for
;; `active-menu-frame-compensation'. If you do not like the flicker
;; when the mouse leaves the frame through the top border, customize
;; the value of `active-menu-delay'.


;; History:
;; 2003-10-31  Claus Brunzema
;;         * Added patches from Zajcev Evgeny: active-menu will ignore
;;           all frames that the predicate in
;;           active-menu-frame-validator returns nil for,
;;           active-menu-frame-compensation can be nil now.
;;         * Version 1.2.0
;; 2003-10-18  Claus Brunzema
;;         * Small documentation addition.
;;         * Version 1.1.2
;; 2003-10-12  Claus Brunzema
;;         * Check if mouse-position-as-motion-event is nil in
;;           active-menu-timer-function (Thanks to Diedrich Vorberg).
;;         * Version 1.1.1
;; 2003-10-06  Claus Brunzema
;;         * Added active-menu-delay (Thanks to Diedrich Vorberg for
;;           the suggestion).
;;         * Version 1.1.0
;; 2002-09-30  Claus Brunzema
;;         * Correct menu-hiding when frame is not focused on startup.
;;         * Fixed behavior with accelerate-menu (Thanks to Thomas
;;           Link for the suggestion).
;;         * Version 1.0.4
;; 2002-08-11  Claus Brunzema
;;         * Version 1.0.3
;; 2002-08-08  Claus Brunzema
;;         * Added active-menu-deselect-frame-hook-function. The
;;           menubar being visible only when it is needed is the whole
;;           point of active-menu. Obviously the menubar is not needed
;;           when the xemacs window isn't even selected.
;; 2002-07-18  Claus Brunzema
;;         * Modeline redraw fix
;; 2002-07-17  Claus Brunzema
;;         * Customizable active-menu-sensitive-height
;;           (a suggestion from Stefan Kamphausen)
;;         * Documentation fixes
;;         * Version 1.0.1
;; 2002-07-13  Claus Brunzema
;;         * More customisation stuff
;;         * Version 1.0.0
;; 2002-07-12  Claus Brunzema
;;         * Restricted toggling of the menubar to the
;;           currently selected frame
;;         * Customisation fixes
;;         * Redraw fixes
;; 2002-06-15  Claus Brunzema
;;         * Small cleanups
;; 2002-06-07  Claus Brunzema
;;         * Fixed and documented the require-way
;; 2002-06-06  Claus Brunzema
;;         * Code cleanup.
;;         * Version 0.9.7 released into public
;; 2002-06-02  Claus Brunzema
;;         * Frame compensation
;; 2002-05-30  Claus Brunzema
;;         * Big rewrite by Claus Brunzema
;; 2002-05-28  Stefan Kamphausen
;;         * Idea and initial implementation

;; ToDo:
;;
;; - save and restore menubar-visible-p
;; - maybe show different menus if you hit different areas on the top
;;   border.
;; - make it a real package

;; Bugs / strange things:
;;
;; - the menubar stays visible if you pop up some menus and click
;;   outside all menus to cancel any selection. It is removed after
;;   the first mouse move or key press.
;; - The timer for the delay doesn't seem to be 100% reliable, or I
;;   have done something wrong (weird race condition somewhere?). If
;;   someone knows more about timers or can reproduce errors, please
;;   send me the details.

;;; Code:

;; Customisation ----------------------------------------------------------
(defgroup active-menu nil
  "Show menubar only if the mouse is at the top of the frame."
  :link '(url-link :tag "Homepage" 
                   "http://www.cbrunzema.de/software.html#active-menu")
  :link '(emacs-commentary-link :tag "Commentary in active-menu.el"
                                "active-menu.el")
  :prefix "active-menu-"
  :group 'mouse
  :group 'gui)

(defcustom active-menu-activated nil
  "*When t, active-menu is activated."
  :type 'boolean
  :initialize #'(lambda (symbol value)
                  (setq active-menu-activated nil))
  :set #'(lambda (symbol value)
           (if value
               (turn-on-active-menu)
             (turn-off-active-menu)))
  :group 'active-menu)

(defcustom active-menu-sensitive-height 5
  "*The pixelrange of the sensitive area for active-menu."
  :type 'integer
  :group 'active-menu)

(defcustom active-menu-delay nil
  "*The number of seconds the mouse has to be in the sensitive area
  before the menu is activated. This can be a float value. nil means
  no delay."
  :type '(choice (const nil) number)
  :group 'active-menu)

(defcustom active-menu-frame-compensation 1
  "*Number of extra textlines to add when the menu is hidden.
When the menubar is hidden, the frame will be expanded by that many
lines to compensate for the height change (set to nil to shut off the
height compensation). This is, I admit it, an ugly hack. There has to
be a way to calculate this number automatically. If you know how to do
it, send mail to mail@cbrunzema.de quick, please."
  :type '(choice (const nil) integer)
  :group 'active-menu)

(defcustom active-menu-frame-validator 'active-menu-valid-frame-p
  "*Function to call in order to validate frame.
This function is called with a frame as the first argument, it should
return non-nil if frame is aptitude to show/hide menubar."
  :type 'function
  :group 'active-menu)


;; Variables --------------------------------------------------------------
(defvar active-menu-original-mouse-motion-handler nil)
(defvar active-menu-suspend nil)
(defvar active-menu-command-skip 0)
(defvar active-menu-timer nil)


;; Functions (internal) --------------------------------------------------
(defun active-menu-valid-frame-p (frame)
  "Return non-nil if FRAME is valid frame to hide/show menubar.
minibuffer-only frames, speedbar and ediff control frames are
considered invalid. If you have additions to this list, send mail to
mail@cbrunzema.de."
  (not
   (or (frame-minibuffer-only-p frame)
       (and (boundp 'speedbar-frame)
            (eq frame speedbar-frame))
       (and (boundp 'ediff-control-frame)
            (eq frame ediff-control-frame)))))

(defun active-menu-redraw ()
  "Redraw selected frame and all modelines if not suspended."
  (unless active-menu-suspend
    (redraw-frame (selected-frame))
    (redraw-modeline t)))

(defun active-menu-show-menubar ()
  "Show the menubar."
  (let ((frame (selected-frame)))
    (unless (specifier-instance menubar-visible-p frame)
      (set-specifier menubar-visible-p t frame)
      (when (numberp active-menu-frame-compensation)
        (set-frame-height frame
                          (- (frame-height)
                             active-menu-frame-compensation)))
      (active-menu-redraw))))

(defun active-menu-hide-menubar ()
  "Hide the menubar."
  (let ((frame (selected-frame)))
    (when (specifier-instance menubar-visible-p frame)
      (set-specifier menubar-visible-p nil frame)
      (when (numberp active-menu-frame-compensation)
        (set-frame-height frame
                          (+ (frame-height)
                             active-menu-frame-compensation)))
      (active-menu-redraw))))

(defun active-menu-timer-funtion (dummy-arg)
  "Show the menubar if the mouse is still in the sensitive area."
  (setq active-menu-timer nil)
  (let ((mouse-position-event (mouse-position-as-motion-event))) 
    (when (and mouse-position-event
               (<= (event-y-pixel mouse-position-event)
                   active-menu-sensitive-height))
      (active-menu-show-menubar))))

(defun active-menu-menubar-maybe-show (event) 
  "Hide or prepare to show the menubar according to the mouse position."
  (if (null event)
      (active-menu-hide-menubar)
    (when (and (motion-event-p event)
               (funcall active-menu-frame-validator (event-frame event)))
      (if (<= (event-y-pixel event) active-menu-sensitive-height)
          (unless active-menu-timer
            (if (or (null active-menu-delay)
                    (zerop active-menu-delay))
                (funcall #'active-menu-timer-funtion nil)
              (setq active-menu-timer
                    (add-timeout active-menu-delay
                                 #'active-menu-timer-funtion
                                 nil))))
        (progn
          (when active-menu-timer
            (disable-timeout active-menu-timer)
            (setq active-menu-timer nil))
          (active-menu-hide-menubar))))))

(defun active-menu-post-command-hook-function ()
  ;; this is needed to eventually remove the menu after an item
  ;; is selected (and the mouse isn't moved after the click).
  (when active-menu-suspend
    (if (zerop active-menu-command-skip)
        (setq active-menu-suspend nil)
      (decf active-menu-command-skip)))
  (unless active-menu-suspend  
    (active-menu-menubar-maybe-show
     (mouse-position-as-motion-event))))
  
(defun active-menu-mouse-motion-handler (event)
  (unless active-menu-suspend
    (active-menu-menubar-maybe-show event)
    (funcall active-menu-original-mouse-motion-handler event)))

(defun active-menu-deselect-frame-hook-function ()
  (unless active-menu-suspend
    (active-menu-hide-menubar)))

(defun active-menu-activate-menubar-hook-function ()
  (unless active-menu-suspend
    ;; we must ignore the command that invoked the menubar and
    ;; thus the activate-menubar-hook. The next command will be the
    ;; one coming from the menubar, so we will end the suspension in
    ;; active-menu-post-command-hook-function on that one. See
    ;; active-menu-post-command-hook-function, too.
    (setq active-menu-command-skip 1)
    (setq active-menu-suspend t)
    (active-menu-show-menubar))
  t)


(defun active-menu-install-handler-and-hook ()
  "Install mouse handler and hook functions for active-menu.
Don't use this, use `turn-on-active-menu' instead."
  (setq active-menu-suspend nil)
  (setq active-menu-original-mouse-motion-handler mouse-motion-handler)
  (setq mouse-motion-handler #'active-menu-mouse-motion-handler)
  (add-hook 'post-command-hook
            #'active-menu-post-command-hook-function)
  (add-hook 'deselect-frame-hook
            #'active-menu-deselect-frame-hook-function)
  (add-hook 'activate-menubar-hook
            #'active-menu-activate-menubar-hook-function)
  (active-menu-menubar-maybe-show (mouse-position-as-motion-event))
  (setq active-menu-activated t))

(defun active-menu-remove-handler-and-hook ()
  "Remove mouse handler and hook functions for active-menu.
Don't use this, use `turn-off-active-menu' instead."
  (setq mouse-motion-handler active-menu-original-mouse-motion-handler)
  (remove-hook 'post-command-hook
               #'active-menu-post-command-hook-function)
  (remove-hook 'deselect-frame-hook
               #'active-menu-deselect-frame-hook-function)
  (remove-hook 'activate-menubar-hook
               #'active-menu-activate-menubar-hook-function)
  (when active-menu-timer (disable-timeout active-menu-timer))
  (setq active-menu-timer nil)
  (active-menu-show-menubar)
  (setq active-menu-activated nil))


;;; Functions (external) --------------------------------------------------
(defun turn-on-active-menu ()
  "Turn on active-menu (you guessed it)."
  (interactive)
  (unless active-menu-activated
    (active-menu-install-handler-and-hook)))

(defun turn-off-active-menu ()
  "Turn off active-menu."
  (interactive)
  (when active-menu-activated
    (active-menu-remove-handler-and-hook)))

;;;###autoload
(defun active-menu (&optional arg)
  "Toggle active menu.
With arg, turn active-menu on iff arg is positive."
  (interactive "P")
  (if (or (and arg (> (prefix-numeric-value arg) 0))
          (and (null arg) (not active-menu-activated)))
      (turn-on-active-menu)
    (turn-off-active-menu)))

(provide 'active-menu)

;;; active-menu.el ends here
