;;; rme.el --- RME Hammerfall mixer interface

;; Copyright (C) 2006  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: processes, multimedia

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
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file implements a simple interface for controlling the RME
;; Hammerfall (Multiface) soundcard matrix mixer.
;;
;; Interactively, attenuation values are converted to/from dB.

;;; TODO:

;; Support for double-speed channel mappings.

;;; Code:

(defvar rme-input-channel-names
  '(("analog 1" . 0) ("analog 2" . 1) ("analog 3" . 2) ("analog 4" . 3)
    ("analog 5" . 4) ("analog 6" . 5) ("analog 7" . 6) ("analog 8" . 7)
    ("playback 1" . 26) ("playback 2" . 27) ("playback 3" . 28) ("playback 4" . 29)
    ("playback 5" . 30) ("playback 6" . 31) ("playback 7" . 32) ("playback 8" . 33))
  "Mapping of channel names to RME channel numbers.")

(defvar rme-output-channel-names
  '(("analog 1" . 0) ("analog 2" . 1) ("analog 3" . 2) ("analog 4" . 3)
    ("analog 5" . 4) ("analog 6" . 5) ("analog 7" . 6) ("analog 8" . 7)
    ("lineout 1" . 26) ("lineout 2" . 27))
  "Mapping of channel names to RME channel numbers.")

(defvar rme-last-attenuations-used nil
  "Internally used to remember mixer settings for prompting.")

(defsubst rme-input-channel-number (name)
  (cdr (assoc name rme-input-channel-names)))

(defsubst rme-output-channel-number (name)
  (cdr (assoc name rme-output-channel-names)))

(defun rme-int-to-db (value)
  (if (<= value 0) -1.0e+INF (* 20 (log (/ (min value 65535) 32768.0) 10))))

(defun rme-db-to-int (db)
  (max (min (round (* 32768.0 (expt 10.0 (/ (float db) 20)))) 65535) 0))

(defun rme-remember-attenuation (from to value)
  (let ((match (assoc (cons from to) rme-last-attenuations-used)))
    (if match
	(setcar (cdr match) value)
      (setq rme-last-attenuations-used
	    (append rme-last-attenuations-used
		    (list (list (cons from to) value)))))))

(defun rme-recall-attenuation (from to)
  (or (cadr (assoc (cons from to) rme-last-attenuations-used)) 32768))

(defun rme-set-attenuation (from to value)
  "Set attenuation for a particular input/output channel pair.
If called interactively, prompts for channel names (completion)
and offer the last rememebered attenuation value as default."
  (interactive
   (let* ((input (completing-read "Route from: " rme-input-channel-names))
	  (output (completing-read (format "Route from %s to: " input) rme-output-channel-names)))
     (list (rme-input-channel-number input) (rme-output-channel-number output)
	   (read-minibuffer (format "Route from %s to %s with attenuation (dB): " input output)
			    (format "%0.1f" (rme-int-to-db (rme-recall-attenuation (rme-input-channel-number input) (rme-output-channel-number output))))))))
  (when (eq 0 (call-process (executable-find "amixer") nil nil nil
			    (format "-c%d" rme-card-number)
			    "cset"
			    "numid=5"
			    (format "%d,%d,%d" from to (rme-db-to-int value))))
    (rme-remember-attenuation from to (rme-db-to-int value))))

(defvar rme-card-regex "^ *\\([0-9]+\\) .*H-DSP - Hammerfall DSP"
  "*Regular expression used to search for the H-DSP card.")

(defvar rme-card-number (with-temp-buffer
			  (insert-file-contents "/proc/asound/cards")
			  (when (re-search-forward rme-card-regex nil t)
			    (read (match-string 1))))
  "*ALSA card number of RME Hammerfall card to use.")


;;; Routing groups

(defvar rme-routes
  '(("Music"
     ("playback 1" . "analog 1")
     ("playback 2" . "analog 2")))
  "Defines groups of channels to route at once.")

(provide 'rme)
;;; rme.el ends here
