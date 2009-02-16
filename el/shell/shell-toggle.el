;;; shell-toggle.el --- Toggle to and from the *shell* buffer
;;; Version 1.2 - 98-11-19
;;; Copyright (C) 1997, 1998 Mikael Sjödin (mic@docs.uu.se)
;;;
;;; Author: Mikael Sjödin  --  mic@docs.uu.se
;;;
;;; This file is NOT part of GNU Emacs.
;;; You may however redistribute it and/or modify it under the terms of the GNU
;;; General Public License as published by the Free Software Foundation; either
;;; version 2, or (at your option) any later version.
;;;
;;; The file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; ----------------------------------------------------------------------
;;; Description:
;;;
;;; Provides the command shell-toggle which toggles between the
;;; *shell* buffer and whatever buffer you are editing.
;;;
;;; This is done in an "intelligent" way.  Features are:
;;; o Starts a shell if non is existing.
;;; o Minimum distortion of your window configuration.
;;; o When done in the shell-buffer you are returned to the same window
;;;   configuration you had before you toggled to the shell.
;;; o If you desire, you automagically get a "cd" command in the shell to the
;;;   directory where your current buffers file exists; just call
;;;   shell-toggle-cd instead of shell-toggle.
;;; o You can convinently choose if you want to have the shell in another
;;;   window or in the whole frame.  Just invoke shell-toggle again to get the
;;;   shell in the whole frame.
;;;
;;; This file has been tested under Emacs 20.2.
;;;
;;; This file can be obtained from http://www.docs.uu.se/~mic/emacs.html

;;; ----------------------------------------------------------------------
;;; Installation:
;;;
;;; o Place this file in a directory in your 'load-path.
;;; o Put the following in your .emacs file:
;;;   (autoload 'shell-toggle "shell-toggle" 
;;;    "Toggles between the *shell* buffer and whatever buffer you are editing."
;;;    t)
;;;   (autoload 'shell-toggle-cd "shell-toggle" 
;;;    "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
;;;   (global-set-key [M-f1] 'shell-toggle)
;;;   (global-set-key [C-f1] 'shell-toggle-cd)
;;; o Restart your Emacs.  To use shell-toggle just hit M-f1 or C-f1
;;;
;;; For a list of user options look in code below.
;;;

;;; ----------------------------------------------------------------------
;;; BUGS:
;;;  No reported bugs as of today

;;; ----------------------------------------------------------------------
;;; Thanks to:
;;;   Christian Stern <Christian.Stern@physik.uni-regensburg.de> for helpful
;;;   sugestions.

;;; ======================================================================
;;; User Options:

(defvar shell-toggle-goto-eob t
  "*If non-nil `shell-toggle' will move point to the end of the shell-buffer
whenever the `shell-toggle' switched to the shell-buffer.

When `shell-toggle-cd' is called the point is allways moved to the end of the
shell-buffer")

(defvar shell-toggle-automatic-cd t
  "*If non-nil `shell-toggle-cd' will send the \"cd\" command to the shell.
If nil `shell-toggle-cd' will only insert the \"cd\" command in the 
shell-buffer.  Leaving it to the user to press RET to send the command to 
the shell.")

;;; ======================================================================
;;; Commands:

(defun shell-toggle-cd ()
  "Calls `shell-toggle' with a prefix argument.  Se command `shell-toggle'"
  (interactive)
  (shell-toggle t))

(defun shell-toggle (make-cd)
  "Toggles between the *shell* buffer and whatever buffer you are editing.
With a prefix ARG also insert a \"cd DIR\" command into the shell, where DIR is
the directory of the current buffer.

Call twice in a row to get a full screen window for the *shell* buffer.

When called in the *shell* buffer returns you to the buffer you were editing
before caling the first time.

Options: `shell-toggle-goto-eob'"
  (interactive "P")
  ;; Try to descide on one of three possibilities:
  ;; If not in shell-buffer, switch to it.
  ;; If in shell-buffer and called twice in a row, delete other windows
  ;; If in shell-buffer and not called twice in a row, return to state before
  ;;  going to the shell-buffer 
  (if (eq major-mode 'shell-mode)
      (if (and (or (eq last-command 'shell-toggle)
		   (eq last-command 'shell-toggle-cd))
	       (not (eq (count-windows) 1)))
	  (delete-other-windows)
	(shell-toggle-buffer-return-from-shell))
    (shell-toggle-buffer-goto-shell make-cd)))

;;; ======================================================================
;;; Internal functions and declarations

(defvar shell-toggle-pre-shell-win-conf nil
  "Contains the window configuration before the *shell* buffer was selected")



(defun shell-toggle-buffer-return-from-shell ()
  "Restores the window configuration used before switching the *shell* buffer.
If no configuration has been stored, just burry the *shell* buffer."
  (if (window-configuration-p shell-toggle-pre-shell-win-conf)
      (progn
	(set-window-configuration shell-toggle-pre-shell-win-conf)
	(setq shell-toggle-pre-shell-win-conf nil)
	(bury-buffer (get-buffer "*shell*")))
    (bury-buffer))
  )


(defun shell-toggle-buffer-goto-shell (make-cd)
  "Switches other window to the *shell* buffer.  If no *shell* buffer exists
start a new shell and switch to it in other window.  If argument MAKE-CD is
non-nil, insert a \"cd DIR\" command into the shell, where DIR is the directory
of the current buffer.

Stores the window cofiguration before creating and/or switching window."
  (setq shell-toggle-pre-shell-win-conf (current-window-configuration))
  (let ((shell-buffer (get-buffer "*shell*"))
	(cd-command
	 ;; Find out which directory we are in (the method differs for
	 ;; different buffers)
	 (or (and make-cd 
		  (buffer-file-name)
		  (file-name-directory (buffer-file-name))
		  (concat "cd " (file-name-directory (buffer-file-name))))
	     (and make-cd
		  list-buffers-directory
		  (concat "cd " list-buffers-directory)))))

    ;; Switch to an existin shell if one exists, otherwise switch to another
    ;; window and start a new shell
    (if shell-buffer
	(switch-to-buffer-other-window shell-buffer)
      (shell-toggle-buffer-switch-to-other-window)
      ;; Sometimes an error is generated when I call `shell'
      ;; (it has to do with my shell-mode-hook which inserts text into the
      ;; newly created shell-buffer and thats not allways a good idea).
      (condition-case the-error
	  (shell)
	(error (switch-to-buffer "*shell*"))))
    (if (or cd-command shell-toggle-goto-eob)
	(goto-char (point-max)))
    (if cd-command
	(progn
	  (insert cd-command)
	  (if shell-toggle-automatic-cd
	      (comint-send-input))
	  ))))

(defun shell-toggle-buffer-switch-to-other-window ()
  "Switches to other window.  If the current window is the only window in the
current frame, create a new window and switch to it.

\(This is less intrusive to the current window configuration then 
`switch-buffer-other-window')"
  (let ((this-window (selected-window)))
    (other-window 1)
    ;; If we did not switch window then we only have one window and need to
    ;; create a new one.
    (if (eq this-window (selected-window))
	(progn
	  (split-window-vertically)
          (other-window 1)))))

    
(provide 'shell-toggle)
