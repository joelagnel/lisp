;;; disptime.el --- display time, load average, etc. in mode line

;; Copyright (C) 1996, 97, 2000 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Status: Works in Emacs 19 and XEmacs.
;; Created: 1996-05-03

;; $Id: disptime.el,v 1.11 2005/12/27 05:18:49 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is my personal replacement for time.el.

;;; Code:

(defconst disptime-xemacs-p
  (and (string-match "XEmacs\\|Lucid" (emacs-version)) t))

(if disptime-xemacs-p
    (require 'itimer)
  (require 'timer))

(defvar disptime-update-interval 60
  "*Seconds between updates of time in the mode line.")

(defvar disptime-show-time-24hr-p nil
  "*Non-nil indicates time should be displayed as hh:mm, 0 <= hh <= 23.
Nil means 1 <= hh <= 12, and an AM/PM suffix is used.")

(defvar disptime-show-day-and-date-p nil
  "*If non-`nil', display the month and date.")

(defvar disptime-show-load-average-p nil
  "*If non-`nil', display the system load average.")

(defvar disptime-show-mail-p t
  "*If non-`nil', indicate in mode line when new mail is pending.
If this is set to the symbol `count', indicate exactly how many messages
are pending.

Turning this off may help if you're busy working and feel distracted by the
knowledge that you have new mail.")

(defvar disptime-mail-file nil
  "*File name of mail inbox file, for indicating existence of new mail.
If `nil', use the default, which is system-dependent and is the same
as used by Rmail.

If this variable is set to the symbol `remote', then the program specified
via `disptime-count-mail-program' is expected know where to find and count
the number of incoming email messages.  For example, it may have to contact
a pop server.")

(defvar disptime-default-directory "~/"
  "*Directory to change to before running any update routines.
Run timer routines in this directory before doing anything else.
This will avoid running programs with unpredictable \(or possibly even
unreachable\) subdirectories.")

(defvar disptime-hook nil
  "*Functions to be called when the time is updated on the mode line.")

;; Using a pty is wasteful, and the separate session causes
;; annoyance sometimes (some systems kill idle sessions).
(defconst disptime-process-connection-type nil)

(defvar disptime-mode-line-update-functions
  '(disptime-set-time-string
    disptime-set-day-and-date-string
    disptime-set-load-average-string
    disptime-set-mail-string))

(defconst disptime-mode-line-format
  '(disptime-enabled
    (""
     (disptime-show-day-and-date-p
      (disptime-day-and-date-string
       ("" disptime-day-and-date-string " ")))
     disptime-time-string
     (disptime-show-load-average-p
      (disptime-load-average-string
       (" " disptime-load-average-string)))
     (disptime-show-mail-p
      (disptime-mail-string
       (" " disptime-mail-string))))))


(defvar disptime-enabled nil)
(defvar disptime-timer nil)

(defmacro disptime-run-at-time (time repeat function &rest args)
  (if disptime-xemacs-p
      (list 'start-itimer
            (if (symbolp function)
                (symbol-name function)
              "anonymous function")
            function time repeat)
    (append (list 'run-at-time time repeat function) args)))

(defmacro disptime-cancel-timer (timer)
  (let ((fn (if disptime-xemacs-p 'delete-itimer 'cancel-timer)))
    (list 'and timer (list fn timer))))

;;;###autoload
(defun disptime-enable ()
  (interactive)
  (or global-mode-string (setq global-mode-string '("")))
  (or (memq 'disptime-mode-line-format global-mode-string)
      (setq global-mode-string
            (append global-mode-string '(disptime-mode-line-format))))
  (setq disptime-enabled t)
  (disptime-update)
  (disptime-reset-timer)
  (disptime-force-mode-line-update))

(defun disptime-disable ()
  (interactive)
  (disptime-cancel-timer disptime-timer)
  (setq disptime-timer nil)
  (setq disptime-enabled nil)
  (disptime-force-mode-line-update))

(defun disptime-reset-timer ()
  (disptime-cancel-timer disptime-timer)
  (setq disptime-timer nil)
  (if disptime-update-interval
      (setq disptime-timer
            (disptime-run-at-time disptime-update-interval
                                  nil
                                  'disptime-update))
    (disptime-disable)))

(defun disptime-update ()
  (let ((fns disptime-mode-line-update-functions)
        (process-environment (default-value 'process-environment))
        (default-directory (expand-file-name disptime-default-directory)))
    (while fns
      (funcall (car fns))
      (setq fns (cdr fns))))
  (disptime-reset-timer)
  (disptime-force-mode-line-update))


;;; Date and time

(defvar disptime-time-string nil)
(defvar disptime-day-and-date-string nil)

(defun disptime-set-time-string ()
  (if disptime-show-time-24hr-p
      (disptime-set-time-string-24hr)
    (disptime-set-time-string-12hr)))

(defun disptime-set-time-string-24hr ()
  (setq disptime-time-string
        (substring (current-time-string) 11 16)))

(defun disptime-set-time-string-12hr ()
  (let* ((time (current-time-string))
         (24-hours (substring time 11 13))
         (hour (string-to-int 24-hours))
         (12-hours (int-to-string (1+ (% (+ hour 11) 12))))
         (am-pm (if (> hour 11) "pm" "am")))
    (setq disptime-time-string
          (concat 12-hours ":" (substring time 14 16) am-pm))))

(defun disptime-set-day-and-date-string ()
  (if disptime-show-day-and-date-p
      (let* ((tm-str (current-time-string)))
        (setq disptime-day-and-date-string
               (concat (substring tm-str 4 7)
                       " "
                       (if (= (aref tm-str 8) ? )
                           (substring tm-str 9 10)
                         (substring tm-str 8 10)))))
    (setq disptime-day-and-date-string nil)))


;;; Load average

(defvar disptime-load-average-string nil)

(defvar disptime-load-average-program "uptime")
(defvar disptime-load-average-program-arguments nil)
(defvar disptime-load-average-data-regexp "load averages?: \\([0-9.]+\\)")
(defvar disptime-load-average-data)
(defvar disptime-load-average-process nil)

(defconst disptime-builtin-load-average-p
  (and (fboundp 'load-average)
       (cond ((memq system-type '(linux lignux gnu/linux windows-nt)) t)
             ;; This is cheesy, but it may catch cases where load-average
             ;; works but this system configuration isn't recognized below.
             ;; The exception handler is in place because on some systems,
             ;; load-average signals an error with the message "load
             ;; average is not implemented on this system".
             ((condition-case nil
                  (if (equal '(0 0 0) (load-average))
                      nil
                    t)
                (error nil)) t)
             ((and (boundp 'system-configuration)
                   system-configuration)
              (let ((data (match-data))
                    (re-list '("netbsd" "linux"))
                    (found nil))
                (while re-list
                  (if (string-match (car re-list) system-configuration)
                      (setq found t
                            re-list nil)
                    (setq re-list (cdr re-list))))
                (store-match-data data)
                found)))))

(defun disptime-set-load-average-string ()
  (if disptime-show-load-average-p
      (if disptime-builtin-load-average-p
          (disptime-set-load-average-string-builtin)
        (disptime-set-load-average-string-extern))
    (setq disptime-load-average-string nil)))

(defun disptime-set-load-average-string-builtin ()
  (let ((avg (format "%03d" (car (load-average)))))
    (setq disptime-load-average-string
          (disptime-shorten-load-average-string
           (concat (substring avg 0 -2) "." (substring avg -2))))))

(defun disptime-set-load-average-string-extern ()
  (setq disptime-load-average-data "")
  (apply 'disptime-start-process
         'disptime-load-average-process
         'disptime-load-average-filter
         'disptime-load-average-sentinel
         disptime-load-average-program
         disptime-load-average-program-arguments))

(defun disptime-load-average-filter (proc s)
  (setq disptime-load-average-data (concat disptime-load-average-data s)))

(defun disptime-load-average-sentinel (proc msg)
  (delete-process disptime-load-average-process)
  (setq disptime-load-average-process nil)
  (let ((data (match-data)))
    (if (string-match disptime-load-average-data-regexp
                      disptime-load-average-data)
        (setq disptime-load-average-string
              (disptime-shorten-load-average-string
               (substring disptime-load-average-data
                          (match-beginning 1)
                          (match-end 1))))
      (setq disptime-load-average-string nil))
    (store-match-data data))
  (disptime-force-mode-line-update))

(defun disptime-shorten-load-average-string (s)
  (if (and (> (length s) 1)
           (= (aref s 0) ?0)
           (= (aref s 1) ?.))
      (substring s 1)
    s))


;;; Mail presence and counting

(defvar disptime-mail-string nil)
(defvar disptime-mail-last-modtime nil)

(defvar disptime-count-mail-program "grep")
(defvar disptime-count-mail-program-arguments '("-c" "^From "))
(defvar disptime-count-mail-data-regexp "^\\([0-9]+\\)")
(defvar disptime-count-mail-data)
(defvar disptime-count-mail-process nil)

(defconst disptime-default-mail-file
  (or (getenv "MAIL")
      (concat rmail-spool-directory (user-login-name))))

(defun disptime-set-mail-string ()
  (cond (disptime-show-mail-p
         (let* ((mail-file (or disptime-mail-file
                               disptime-default-mail-file))
                (attr (and (not (eq mail-file 'remote))
                           (file-attributes (if (fboundp 'file-chase-links)
                                                (file-chase-links mail-file)
                                              mail-file))))
                (empty-p (if attr
                             (<= (or (nth 7 attr) 0) 0)
                           t)))
           (cond ((eq mail-file 'remote)
                  (disptime-count-mail-messages mail-file))
                 (t
                  (if empty-p
                      (setq disptime-mail-string nil)
                    (if (eq disptime-show-mail-p 'count)
                        (disptime-count-mail-messages mail-file attr)
                      (setq disptime-mail-last-modtime nil)
                      (setq disptime-mail-string "Mail")))))))
        (t
         (setq disptime-mail-last-modtime nil)
         (setq disptime-mail-string nil))))

(defun disptime-count-mail-messages (&optional mail-file attributes)
  (let ((modtime (nth 5 attributes))
        (args disptime-count-mail-program-arguments)
        (checkp nil))
    (cond ((eq mail-file 'remote)
           (setq checkp t))
          (t
           (setq args (append args (list mail-file)))
           (cond ((or (null disptime-mail-last-modtime)
                      (not (equal disptime-mail-last-modtime modtime)))
                  (setq disptime-mail-last-modtime modtime)
                  (setq checkp t)))))
    (cond (checkp
           (setq disptime-count-mail-data "")
           (apply 'disptime-start-process
                  'disptime-count-mail-process
                  'disptime-count-mail-filter
                  'disptime-count-mail-sentinel
                  disptime-count-mail-program
                  args)))))

(defun disptime-count-mail-filter (proc s)
  (setq disptime-count-mail-data (concat disptime-count-mail-data s)))

(defun disptime-count-mail-sentinel (proc msg)
  (delete-process disptime-count-mail-process)
  (setq disptime-count-mail-process nil)
  (let ((data (match-data))
        (count nil))
    (cond ((string-match disptime-count-mail-data-regexp
                         disptime-count-mail-data)
           (setq count (substring disptime-count-mail-data
                                  (match-beginning 1) (match-end 1)))
           (if (string= count "0")
               (setq disptime-mail-string nil)
             (setq disptime-mail-string (concat "M:" count))))
          (t
           (setq disptime-mail-string "Mail?")))
    (store-match-data data))
  (disptime-force-mode-line-update))

(defun disptime-toggle-showing-mail ()
  "Toggle notification of pending email.
Sometimes knowing you have email is just too damned distracting."
  (interactive)
  (setq disptime-show-mail-p (not disptime-show-mail-p)))


;;; Utility functions.

(defun disptime-start-process (procsym filter sentinel program &rest args)
  (let* ((process-connection-type disptime-process-connection-type)
         (proc (symbol-value procsym)))
    (and (processp proc)
         (process-status proc)
         (delete-process proc))
    (setq proc (apply 'start-process program nil program args))
    (process-kill-without-query proc)
    (and sentinel
         (set-process-sentinel proc sentinel))
    (and filter
         (set-process-filter proc filter))
    (set procsym proc)))

(defun disptime-force-mode-line-update ()
  (if (fboundp 'force-mode-line-update)
      (force-mode-line-update 'all)
    (save-excursion (set-buffer (other-buffer)))
    (set-buffer-modified-p (buffer-modified-p)))
  (sit-for 0))

(provide 'disptime)

;;; disptime.el ends here.
