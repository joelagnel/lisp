;; chronometer.el --- a [not so] simple chronometer for Emacs

;; Copyright  (C)  2004  Marcelo Toledo <marcelo@marcelotoledo.org>

;; Author: Marcelo Toledo <marcelo@marcelotoledo.org>
;; Maintainer: Marcelo Toledo <marcelo@marcelotoledo.org>
;; URL: http://www.marcelotoledo.org/stuff/projetos/chronometer
;; Created: 21 Jul 2004
;; Version: 1.0
;; Keywords: tools, convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;; Commentary:

;; This is the first release, expect more in the next and feel free to
;; feed me with patches, wishes, bugs and whatever you you think will
;; worth it.

;; This mode is for counting times. I basically created it because I
;; used to cook pizza while coding and I used to forget the time and a
;; few minutes later I could smell a burned pizza. The result of this
;; is displayed in pictures, right here:

;; http://www.marcelotoledo.org/fotos/2003/pizza_queimada

;; You can use it for whatever purpose you like, but the typical
;; scenario is to keep alerted of how much time has past.

;; To make it available for Emacs, put it in your `load-path' and
;; insert (require 'chronometer) in your .emacs.

;; Use `chronometer' to start the chronometer, it will automaticaly
;; start from zero and will keep incrementing every one second.

;; From now on you may want to play with the following keybindings:

;; * a - set alarm
;; * u - unset alarm
;; * p - toggle pause
;; * r - restart
;; * h - hide
;; * q - exit
;; * ? - help

;; TODO:

;; * stop/start
;; * count down (maybe)
;; * allow more input formats for the alarm
;; * make it unable to set alarm to less then the elapsed time is
;; * execute a command when reach alarm
;; 

;; Code:

(defgroup chronometer nil
  "Simple Emacs Chronometer."
  :prefix "chronometer"
  :version "21.1"
  :group 'tools
  :group 'convenience)

(defvar chronometer-default-buffer "*chronometer*"
  "The default working buffer.")

(defvar chronometer-start-time nil
  "Time that the chronometer has been started. If it's paused this value will be incremented.")

(defvar chronometer-alarm nil
  "Minute that you wanna be alerted.")

(defvar chronometer-alarm-ringing nil
  "If the alarm is ringing.")

(defvar chronometer-alarm-ringing-message t
  "If 'chronometer-alarm-ringing' is t then the ringing message will first appear else it will first appear blank.")

(defvar chronometer-timer nil
  "Holds the timer object.")

(defvar chronometer-paused nil
  "If the chronometer is paused this variable will be true, otherwise false.")

(defconst chronometer-prompt "Chronometer===> "
  "The prompt that will be displayed in the chronometer buffer.")

(defconst chronometer-interval 1
  "The chronometer buffer is updated every chronometer-interval second(s).")

(defconst chronometer-running nil
  "If chronometer is running this variable will be true, otherwise false.")

(defvar chronometer-map nil
  "Chronometer mode map.")
(unless chronometer-map
  (setq chronometer-map (make-keymap))
  (define-key chronometer-map "u" 'chronometer-unset-alarm)
  (define-key chronometer-map "a" 'chronometer-set-alarm)
  (define-key chronometer-map "q" 'chronometer-quit)
  (define-key chronometer-map "p" 'chronometer-toggle-pause)
  (define-key chronometer-map "?" 'chronometer-help)
  (define-key chronometer-map "r" 'chronometer-restart)
  (define-key chronometer-map "h" 'chronometer-hide))


;;;###autoload
(defun chronometer ()
  "Run the Emacs Chronometer. See the documentation for 'chronometer-mode' for more information."
  (interactive)
  (if (not chronometer-running)
      (progn
        (chronometer-unset-alarm)
        (if chronometer-paused
            (chronometer-toggle-pause))        
        (chronometer-restart)
        (get-buffer-create chronometer-default-buffer)
        (setq chronometer-timer (run-with-timer 1 chronometer-interval 'chronometer-loop))
        (setq chronometer-running t)))
  (cond ((not (get-buffer-window chronometer-default-buffer))
         (let ((split-window-keep-point nil)
               (window-min-height 2))
           (select-window (split-window-vertically (if (and (fboundp 'face-attr-construct)
                                                            (plist-get (face-attr-construct 'modeline) :box)) -3 -2)))
           (switch-to-buffer chronometer-default-buffer)))
        ((not (eq (current-buffer) chronometer-default-buffer))
         (select-window (get-buffer-window chronometer-default-buffer))))
  (chronometer-mode)
  (setq buffer-read-only t))


(defun chronometer-toggle-pause ()
  "Toggle pause."
  (interactive)
  (if chronometer-paused
      (setq chronometer-paused nil)
    (setq chronometer-paused t)))


(defun chronometer-set-alarm ()  
  "Set alarm to the minute you would like to alerted."
  (interactive)
  (let ((value (read-from-minibuffer "Set alarm to what minute? ")))
    (setq chronometer-alarm value)))


(defun chronometer-unset-alarm ()
  "Unset alarm."
  (interactive)
  (setq chronometer-alarm nil
        chronometer-alarm-ringing nil
        chronometer-alarm-ringing-message t))


(defun chronometer-restart ()
  "Start chronometer from zero."
  (interactive)
  (setq chronometer-start-time (current-time)))


(defun chronometer-loop ()
  "This function runs every 'chronometer-interval' second(s) and display data in the buffer."
  (with-current-buffer chronometer-default-buffer
    (if chronometer-paused
        (chronometer-increment-start-time))
    (let ((time-elapsed (format-time-string "%H:%M:%S" (time-subtract (current-time) chronometer-start-time) t))
          (minutes-elapsed (+ (* (string-to-int (format-time-string "%H" (time-subtract (current-time) chronometer-start-time) t)) 60) (string-to-int (format-time-string "%M" (time-subtract (current-time) chronometer-start-time) t))))
          (inhibit-read-only t))
      (erase-buffer)
      (goto-char (point-min))
      (insert-string chronometer-prompt time-elapsed)
      (if chronometer-paused
          (insert-string " (Paused)"))
      (if chronometer-alarm
          (insert-string " (Alarm: " chronometer-alarm " min.)"))
      (if (and (>= minutes-elapsed (string-to-int chronometer-alarm)) (null chronometer-alarm-ringing))
          (progn
            (setq chronometer-alarm-ringing t)
            (chronometer)))
      (if chronometer-alarm-ringing
          (if chronometer-alarm-ringing-message
              (progn
                (setq chronometer-alarm-ringing-message nil)
                (insert-string " Alarm!! Type 'u' to stop ringing!")
                (beep))
            (progn
              (setq chronometer-alarm-ringing-message t)
              (insert-string "         Type 'u' to stop ringing!")
              (beep)))))))


(defun chronometer-increment-start-time ()
  "Add one second in 'chronometer-start-time'."
  (setf (cadr chronometer-start-time) (+ (cadr chronometer-start-time) 1)))


(defun chronometer-cancel-timer ()
  "Cancel the chronometer timer."
  (cancel-timer chronometer-timer))


(defun chronometer-hide ()
  "Hide chronometer buffer."
  (interactive)
  (set-buffer chronometer-default-buffer)
  (while (get-buffer-window chronometer-default-buffer)
    (delete-window (get-buffer-window chronometer-default-buffer))))

(defun chronometer-quit ()
  "Quit chronometer."
  (interactive)
  (set-buffer chronometer-default-buffer)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (while (get-buffer-window chronometer-default-buffer)
    (delete-window (get-buffer-window chronometer-default-buffer)))
  (setq chronometer-running nil)
  (kill-buffer chronometer-default-buffer)
  (chronometer-cancel-timer)
  (message "Bye"))


(defun chronometer-mode ()
  "A [not so] simple chronometer for Emacs.

This is the first release, expect more in the next and feel free to feed
me with patches, wishes, bugs and whatever you you think will worth it.

This mode is for counting times. I basically created it because I used
to cook pizza while coding and I used to forget the time and a few
minutes later I could smell a burned pizza. The result of this is
displayed in pictures, right here:

http://www.marcelotoledo.org/fotos/2003/pizza_queimada

You can use it for whatever purpose you like, but the typical scenario
is to keep alerted of how much time has past.

To make it available for Emacs, put it in your `load-path' and insert
(require 'chronometer) in your .emacs.

Use `chronometer' to start the chronometer, it will automaticaly start
from zero and will keep incrementing every one second.

>From now on you may want to play with the following keybindings:

\\{chronometer-map}"
  (kill-all-local-variables)

  (setq major-mode 'chronometer-mode
        mode-name "Chronometer")

  (use-local-map chronometer-map))


(defun chronometer-help ()
  "Quick reference:
* a - set alarm.
* u - unset alarm.
* p - toggle pause.
* r - restart.
* h - hide.
* q - exit.
* ? - help."
  (interactive)
  (if (eq last-command 'chronometer-help)
    (let ((mode-name "chronometer-mode")
          (major-mode 'chronometer-mode)
          (g-map (current-global-map))
          (win (selected-window)))
      (require 'ehelp)
      (describe-mode)
      (select-window win))
    (message nil))
  (let ((one (one-window-p t))
        (win (selected-window))
        (help-buf (get-buffer-create "*Help*")))
    (save-window-excursion
      (with-output-to-temp-buffer "*Help*"
        (princ (documentation 'chronometer-help)))
      (if one
          (shrink-window-if-larger-than-buffer
           (get-buffer-window help-buf)))
      (message "Type any key to continue.")
      (select-window win)
      (sit-for 360))
    (select-window win)))

(provide 'chronometer)

;; chronometer ends here
