;; simple mode for running nmap scans
;;
;; Copyright (C) 2003 Will Glozer
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59
;; Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
;; ----------------------------------------------------------------------
;;
;; this utility maintains a history of arguments passed to nmap and will
;; highlight found ports

(defcustom nmap-buffer "*nmap*"
  "buffer to display nmap output in"
  :type 'string
  :group 'nmap)

(defcustom nmap-program "nmap"
  "nmap executable to run"
  :type 'string
  :group 'nmap)

(defcustom nmap-program-args "-sS"
  "arguments to pass to nmap"
  :type 'string
  :group 'nmap)

(defface nmap-open-port-face
  '((t (:foreground "red")))
  "face for open ports"
  :group 'nmap)

(defvar nmap-arg-history nil
  "history of nmap execution arguments")

;; --

(defun nmap ()
  "execute an nmap scan"
  (interactive)
  (let ((buffer (get-buffer-create nmap-buffer))
        (args (split-string (read-string "nmap args: "
                                         nmap-program-args
                                         'nmap-arg-history))))
    (with-current-buffer buffer
      (erase-buffer)
      (set-process-filter
       (apply #'start-process "nmap" buffer nmap-program args)
       #'nmap-process-filter))))

(defun nmap-process-filter (process input)
  "filter function that handles nmap output"
  (with-current-buffer (process-buffer process)
    (insert-string input)
    (if (string-match "\\([0-9]+\\)\\(/.+\\)" input)
        (while (match-beginning 1)
          (set-text-properties (match-beginning 1)
                               (match-end 1)
                               '(face nmap-open-port-face))
          (string-match "\\([0-9]+\\)\\(/.+\\)" input (1+ (match-end 1)))))))
