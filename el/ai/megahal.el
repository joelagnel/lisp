;;; megahal.el --- interface to MegaHAL conversation simulator

;; Copyright (C) 2006  Magnus Henoch

;; Author: Magnus Henoch <m...@freemail.hu>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; MegaHAL is a "conversation simulator" which can be found at
;; <URL:http://megahal.alioth.debian.org/>.  It accepts natural
;; language as input, and gives responses as output.  This file
;; provides an Emacs interface to MegaHAL.

;; Example:
;;
;; (megahal-send "Can you hear me?")
;; (megahal-wait)
;; (message "%s" (megahal-read))

;;; Code:

(defvar megahal-program (executable-find "megahal"))

(defvar *megahal-process* nil)

(defun megahal-start ()
  "Start a MegaHAL process.
Normally there is no need to call this function directly, as
`megahal-send' calls it if there is no running MegaHAL process."
  (interactive)
  (unless *megahal-process*
    (setq *megahal-process*
          (start-process "megahal" (generate-new-buffer-name "megahal")
                         megahal-program "-p"))
    ;; Ignore inital output.
    (with-current-buffer (process-buffer *megahal-process*)
      (while (accept-process-output *megahal-process* 1)
        (erase-buffer)))
    ;; We want to save MegaHAL's brain when Emacs exits.
    (add-hook 'kill-emacs-hook 'megahal-stop)))

(defun megahal-stop ()
  "Stop the MegaHAL process, causing it to save its brain."
  (interactive)
  (when *megahal-process*
    (process-send-string *megahal-process* "#quit\n\n")
    (accept-process-output *megahal-process* 1)
    (delete-process *megahal-process*)
    (setq *megahal-process* nil)))

(defun megahal-send (string)
  "Send a string to MegaHAL."
  (unless *megahal-process*
    (megahal-start))
  (process-send-string *megahal-process*
                       (concat
                        (replace-regexp-in-string "\n" " " string)
                        "\n\n")))

(defun megahal-wait ()
  "Wait for MegaHAL to answer."
  (accept-process-output *megahal-process* 5))

(defun megahal-read ()
  "Return MegaHAL's response."
  (with-current-buffer (process-buffer *megahal-process*)
    (delete-and-extract-region (point-min) (point-max))))

(provide 'megahal)
;;; megahal.el ends here
