;;; amixer --- Utility function for setting the audio volume

;; Copyright (C) 2004 Ole Arndt

;; Author: Ole Arndt <ole@sugarshark.com>
;; Maintainer: Ole Arndt <ole@sugarshark.com>
;; Keywords: audio, multimedia, external, alsa
;; Time-stamp: Sun Feb 15 22:14:11 2004
;; Version: 0.5
;; X-URL: http://www.sugarshark.com/elisp/mylisp/amixer.el

;; This file is *NOT* (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Introduction
;; ------------
;;
;; This package sets the volume via the alsa mixer program
;;
;; To use amixer, insert in your ~/.emacs:
;;
;;    (require 'amixer)
;;
;; and bind some keys to the functions:
;;
;;    (global-set-key [(control ?c) ?# ?+] 'amixer-increment-volume)
;;    (global-set-key [(control ?c) ?# ?-] 'amixer-decrement-volume)
;;    (global-set-key [(control ?c) ?# ?v] 'amixer-set-volume)
;;
;; Todo:
;; -----
;;
;; The volume shouldn't be 'write-only', be should be read back from the mixer.
;;

(defgroup amixer nil
  "Interface to the command line alsa mixer."
  :group 'external)

(defcustom amixer-mixer-program "amixer"
  "External mixer program  (to adjust volume)."
  :type 'string
  :group 'amixer)

(defcustom amixer-master-volume 60
  "Volume in percent."
  :type 'integer
  :group 'amixer)

(defcustom amixer-volume-increment 10
  "Volume increment value."
  :type 'integer
  :group 'amixer)

(defcustom amixer-master-volume-id 40
  "Identifer of the master volume control.
Call 'amixer controls' and take the number of an entry that looks like:

  numid=40,iface=MIXER,name='Master Playback Volume'

The master volume id in this example is 40.
"
  :type 'integer
  :group 'amixer)

(defcustom amixer-card-id 0
  "Alsa Id of the sound card. Use 0 for the first card."
  :type 'integer
  :group 'amixer)

;;;###autoload
(defun amixer-set-volume (num)
  "Set volume using 'amixer-mixer-program' to num percent."
  (interactive "nVolume: ")
  (setq amixer-master-volume (max 0 (min 100 num)))
  (message "Volume: %d%%" amixer-master-volume)
  (start-process "mixer-process" nil 
		 amixer-mixer-program "-c" (int-to-string amixer-card-id) 
		 "cset" (concat "numid=" (int-to-string amixer-master-volume-id))
		 (concat  (int-to-string amixer-master-volume) "%")))

;;;###autoload
(defun amixer-increment-volume ()
  "Ajust volume up by default increment."
  (interactive)
  (amixer-set-volume (+ amixer-master-volume  amixer-volume-increment)))

;;;###autoload
(defun amixer-decrement-volume ()
  "Ajust volume down by default increment."
  (interactive)
  (amixer-set-volume (- amixer-master-volume amixer-volume-increment)))

(provide 'amixer)