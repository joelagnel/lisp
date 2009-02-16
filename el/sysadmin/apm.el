 ;;; apm.el -- Using APM (Advanced Power Management) From Within (X)Emacs
;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Dinesh G Dutt <d...@cisco.com>
;; Maintainer: Dinesh G Dutt <d...@cisco.com>
;; Created: 16 Mar, 2000
;; Version: 2.1
;; Keywords: APM system

;;; Commentary:
;;
;; This module allows APM to be used from within (X)Emacs. APM is the power
;; management module available on Linux machines running on PCs. APM can be
;; used to track the remaining battery charge, to suspend or put in standby the
;; PC and to also perform various actions when the battery level reaches a low
;; threshold watermark.

;; In the current release, no support is provided for taking actions based on
;; the battery level. This release only supports using the functionality
;; provided by the command "apm" from within (X)Emacs and to also display the
;; battery status in the modeline.

;; The user can configure the following parameters:
;;
;;     The command used to obtain status (by default "/usr/bin/apm")
;;     Whether the battery status is displayed in the modeline
;;     If the battery status is displayed, how often the display is updated.
;;     The format of the output (remaining time in minutes or hours)

;;; Code:

;; Variables

(defconst apm-version "2.1"
  "Version of this program, APM.")

(defvar apm-status-string nil)

(defvar apm-is-supported
  (and (equal system-type 'linux) (file-exists-p "/proc/apm"))
  "If t, indicates that APM support is available."
)

;; Customization

(defgroup apm nil
  "Use APM (power management on Linux within Emacs."
  :group 'applications)

(defcustom apm-update-interval 60
  "Time interval in seconds between modeline updates."
  :type  'integer
  :group 'apm)

(defcustom apm-command "/usr/bin/apm"
  "Path and command name to be invoked for APM client."
  :type 'file
  :group 'apm)

(defcustom apm-offline-output-format "AC off-line"
  "The output of apm command when the power source is the battery."
  :type 'regexp
  :group 'apm)

(defcustom apm-status-output-format ".* \\([0-9]+%\\) (\\([0-9]+ min\\)"
  "Regexp that captures the percentage and time values from apm command.
The percentage is assumed to be the first match and the time the second.
Folks that wish to see the time remaining in hours instead of minutes need
to change this variable and the ``apm-command-status-option'' variable."
  :type 'regexp
  :group 'apm)

(defcustom apm-command-status-option "-m"
  "Command line arg to be passed to APM to display status.
If you change this, please change the `apm-status-output-format' as well. By
default, the display of battery time remaining is in minutes. It can be
modified to display the time remaining in hours. Just set this string to the
null or empty string."
  :type 'string
  :group 'apm)

(defcustom apm-command-suspend-option "-s"
  "Command line arg to be passed to APM to suspend machine."
  :type 'string
  :group 'apm)

(defcustom apm-command-standby-option "-S"
  "Command line arg to be passed to APM to put machine in standby mode."
  :type 'string
  :group 'apm)

(defcustom apm-pre-suspend-hook nil
  "List of hook functions executed just before APM suspend is invoked.
You can do things like save buffers, run the feedmail queue etc."
  :type 'hook
  :group 'apm)

(defcustom apm-pre-standby-hook nil
  "List of hook functions executed just before APM standby is invoked.
You can do things like save buffers, run the feedmail queue etc."
  :type 'hook
  :group 'apm)

;; Functions

(defun apm-suspend ()
  "Function used to suspend the machine using APM."
  (interactive)
  (if apm-is-supported
      (progn
        (run-hooks 'apm-pre-suspend-hook)
        (call-process apm-command nil 0 nil apm-command-suspend-option))
    (error "APM is not supported on this machine.")))

(defun apm-standby ()
  "Function used to put the machine in standby mode using APM."
  (interactive)
  (if apm-is-supported
      (progn
        (run-hooks 'apm-pre-standby-hook)
        (call-process apm-command nil 0 nil apm-command-standby-option))
    (error "APM is not supported on this machine.")))

(defun apm-status ()
  "Displays time left in minutes before battery is over."
  (interactive)
  (let (output)
    (if apm-is-supported
        (progn
          (setq output (apm-time-remaining))
          (message "%s" output))
      (error "APM is not supported on this machine."))))

(defun apm-time-remaining ()
  "Returns the battery time left as a string."
  (call-process apm-command nil (get-buffer-create " *apm-Status*") nil
                apm-command-status-option)
  (let (output-string)
    (unwind-protect
        (save-excursion
          (save-restriction
            (set-buffer (get-buffer " *apm-Status*"))
            (goto-char 0)
            (end-of-line)
            (setq output-string (buffer-string (point-min) (point)))))
      (kill-buffer (get-buffer " *apm-Status*")))
    output-string))

;;;###autoload
(defun apm-update-modeline (enable)
  "Updates the modeline with the current battery status.
A leading - in the display indicates that the battery is
charging."
  (interactive)
  (if (not apm-is-supported)
      (error "APM is not supported on this machine.")
    (if enable
        (progn
          (unless (memq 'apm-status-string global-mode-string)
            (setq global-mode-string (append global-mode-string
                                             '(apm-status-string))))
          (apm-update)
          (start-itimer "APM Timer" 'apm-update apm-update-interval
                        apm-update-interval)
          (setq apm-in-modeline t))
      (if apm-in-modeline
          (progn
            (delete-itimer "APM Timer")
            (setq apm-status-string nil)
            (setq apm-in-modeline nil))))))

(defun apm-update ()
  "Function to update the battery status string (APM under Linux)."
  (if apm-is-supported
      (progn
        (call-process apm-command nil (get-buffer-create " *apm-Status*") nil
                      apm-command-status-option)
        (unwind-protect
            (save-excursion
              (save-restriction
                (set-buffer (get-buffer " *apm-Status*"))
                (goto-char 0)
                (setq apm-status-string "")
                (if (looking-at apm-offline-output-format)
                    (if (re-search-forward apm-status-output-format)
                        (setq apm-status-string
                              (concat "(" (match-string 1) " "
                                      (match-string 2) ")")))
                  (if (re-search-forward apm-status-output-format)
                      (setq apm-status-string (concat "(" (match-string 1) " -"
                                                      (match-string 2) ")")))
                (force-mode-line-update)
                (kill-buffer (get-buffer " *apm-Status*")))))))))

(provide 'apm)

;;;###autoload
(defcustom apm-in-modeline nil
  "If true, the modeline is updated with the current battery level."
  :require 'apm
  :set (lambda (symbol value)
         (apm-update-modeline value))
  :type 'boolean
  :group 'apm)

;;; apm.el ends here 