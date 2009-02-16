;;; webster.el --- emacs interface to webster servers

;; Copyright (C) 1994, 1996 Noah S. Friedman

;; Author: Noah Friedman <friedman@prep.ai.mit.edu>
;; Maintainer: friedman@prep.ai.mit.edu
;; Keywords: extensions, dictionary

;; $Id: webster.el,v 1.6 1996/10/31 04:51:53 friedman Exp $

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
;; Inc.; 675 Massachusetts Avenue; Cambridge, MA 02139, USA.

;;; Commentary:

;; This is a complete rewrite of a webster interface written by Jason
;; R. Glasgow <glasgow@cs.yale.edu> and subsequently hacked on by numerous
;; others.  The current version tries to be more robust in the following
;; ways:
;;
;;  1. Automatically reopen server connection if it gets closed or buffer
;;     is killed.
;;  2. Handle replies that come in multiple chunks.
;;  3. Take advantage of emacs 19 facilities, if present.

;;; Code:


;;;###autoload
(defvar webster-server (or (getenv "WEBSTERHOST") "localhost")
  "*Host name of a webster server, specified as a string.")

;;;###autoload
(defvar webster-port (let ((port (getenv "WEBSTERPORT")))
                       (if port
                           (string-to-int port)
                         103))
  "*TCP port of webster server on webster-server, specified as an integer.
This is usually 103 or 2627.")

;;;###autoload
(defvar webster-mode-hook nil
  "*Hook to be run by webster-mode, after everything else.")

(defvar webster-mode-map nil
  "*Keymap for the webster buffer.")
(cond
 ((null webster-mode-map)
  (setq webster-mode-map (make-keymap))
  (define-key webster-mode-map "?" 'describe-mode)
  (define-key webster-mode-map "d" 'webster-define)
  (define-key webster-mode-map "e" 'webster-endings)
  (define-key webster-mode-map "h" 'describe-mode)
  (define-key webster-mode-map "q" 'webster-quit)
  (define-key webster-mode-map "s" 'webster-spell)))

(defvar webster-buffer-name "*webster*"
  "*The name to use for webster interaction buffer.")

;; The current webster process, if any.
(defvar webster-process nil)

;; String used to store partial results of DEFINEs or other requests, which
;; may come in multiple chunks.
(defvar webster-partial-output nil)


;;;###autoload
(defun webster-define (word)
  "Look up a word in Webster's dictionary."
  (interactive (webster-prompt "Look up word"))
  (webster-send "DEFINE" word))

;;;###interactive
(defun webster-endings (word)
  "Look up possible endings for a word in Webster's dictionary."
  (interactive (webster-prompt "Find endings for word"))
  (webster-send "ENDINGS" word))

;;;###interactive
(defun webster-spell (word)
  "Look up possible correct spellings for a word in Webster's dictionary."
  (interactive (webster-prompt "Possible correct spellings for word"))
  (webster-send "SPELL" word))

;; webster mode is suitable only for a specific kind of process.
(put 'webster-mode 'mode-class 'special)

(defun webster-mode ()
  "Major mode for interacting with webster server.
\\{webster-mode-map}
Use webster-mode-hook for customization."
  (kill-all-local-variables)
  (setq major-mode 'webster-mode)
  (setq mode-name "webster")
  (setq mode-line-process '(":%s"))
  (use-local-map webster-mode-map)
  (run-hooks 'webster-mode-hook))

(defun webster-filter (proc string)
  (let ((match-data (match-data)))
    (unwind-protect
        (cond
         ((string= string "SPELLING 0\C-m\n")
          (setq string "Word not found.")
          (setq webster-partial-output nil))
         ((string= string "SPELLING 1\C-m\n")
          (setq string "Word spelled correctly.")
          (setq webster-partial-output nil))
         ((string-match "\\(\200\\|\0\\)$" string)
          (setq string
                (concat (or webster-partial-output "")
                        ;; Delete ending control char plus trailing newline
                        (substring string 0 (- (match-end 0) 2))))
          (setq webster-partial-output nil))
         (t
          (setq webster-partial-output
                (concat (or webster-partial-output "")
                        string))
          (setq string nil)))
      (store-match-data match-data)))
  (and string
       (webster-display proc string)))

(defun webster-display (proc string)
  (let* ((orig-buffer (current-buffer))
         (proc-buffer (process-buffer proc))
         (window (get-buffer-window proc-buffer))
         (proc-mark (process-mark proc))
         region-begin)
    (unwind-protect
        (cond
         ((and (null window)
               (not (string-match "\n" string)))
          (message "%s" string))
         (t
          (set-buffer proc-buffer)
          (setq region-begin (marker-position proc-mark))
          (goto-char proc-mark)
          (insert-before-markers string "\n\n")
          (goto-char region-begin)
          (while (search-forward "\C-m" proc-mark 'goto-end)
            (delete-char -1))
          (goto-char (point-max))

          ;; recenter acts on the selected window, and it can get confused
          ;; if the selected buffer is not in the selected window.
          (select-window (display-buffer proc-buffer))
          (recenter -1))))
      (set-buffer orig-buffer)
      (select-window (get-buffer-window orig-buffer))))

(defun webster-send (request word)
  ;; Start or restart webster server connection, if necessary.
  (cond
   ((null webster-process)
    (webster-start))
   ((and (memq (process-status webster-process) '(open run))
         (memq (process-buffer webster-process) (buffer-list))))
   (t
    (webster-start)))
  (process-send-string webster-process (concat request " " word "\n")))

(defun webster-start (&optional host port)
  (or host (setq host webster-server))
  (or port (setq port webster-port))

  (or (null webster-process)
      (delete-process webster-process))
  (setq webster-process nil)
  (setq webster-partial-output nil)

  (let ((orig-buffer (current-buffer))
        (buffer (get-buffer-create webster-buffer-name)))
    (unwind-protect
        (progn
          (message "Opening webster connection to %s..." host)
          (set-buffer buffer)
          (webster-mode)
          (setq webster-process
                (open-network-stream webster-buffer-name buffer host port))
          (process-kill-without-query webster-process)
          (set-marker (process-mark webster-process) (point-max))
          (set-process-filter webster-process 'webster-filter)
          (message "Opening webster connection to %s...done" host))
      (set-buffer orig-buffer))))

(defun webster-quit ()
  "Close connection to webster server.
Buffer is not deleted."
  (interactive)
  (cond
   ((processp webster-process)
    (let ((buffer (process-buffer webster-process)))
      (message "Closing webster connection to %s..." webster-server)
      (delete-process webster-process)
      (message "Closing webster connection to %s...done" webster-server)
      (and (memq buffer (buffer-list))
           (bury-buffer buffer))
      (setq webster-process nil)))
   (t
    (message "There is no open webster connection."))))

(defun webster-prompt (prompt)
  (let ((default-word (if (fboundp 'current-word)
                          (current-word)
                        (webster-current-word)))
        word)
    (cond
     ((string= default-word "")
      (setq prompt (concat prompt ": ")))
     (t
      (setq prompt (format "%s (%s): " prompt default-word))))
    (setq word (read-string prompt))
    (if (string= word "")
        (list default-word)
      (list word))))

;; Taken from unix-apropos by Henry Kautz.
;; This is only needed if `current-word' isn't defined already.
(defun webster-current-word ()
  "Word cursor is over, as a string."
  (save-excursion
    (let (beg end)
      (re-search-backward "\\w" nil 2)
      (re-search-backward "\\b" nil 2)
      (setq beg (point))
      (re-search-forward "\\w*\\b" nil 2)
      (setq end (point))
      (buffer-substring beg end))))


(or (fboundp 'defalias)
    (fset 'defalias 'fset))

;;;###autoload
(defalias 'webster 'webster-define)

(provide 'webster)

;;; webster.el ends here
