;;; osd.el --- on-screen display
;; Copyright (C) 2003, 2004  Daniel Brockman

;; Author:     Daniel Brockman <drlion@deepwood.net>
;; URL:        http://www.deepwood.net/software/osd.el
;; Updated:    2004-10-08
;; Keywords:   multimedia
;; Created:    2003-12-14

;; This file is not part of GNU Emacs.
;; This file is released under the GNU General Public License.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file implements a thin Emacs front-end to a small program
;; called `osd_cat', written by Martijn van de Streek
;; <martijn@foodfight.org>.  That program in turn is a front-end to
;; the X OSD Library, a cool piece of software for overlaying text on
;; an X display written by Andre Renaud <andre@ignavus.net>.  While
;; I'm dropping names, I might as well mention Tim Wright
;; <tim@ignavus.net>, the current maintainer of the X OSD Library, who
;; apparently also made significant contributions to the
;; aforementioned `osd_cat'.

;; Presumably, an Emacs front-end to an X OSD library is useful in any
;; situation where the user needs or wants to be informed of something
;; regardless of whether or not their eyes are currently focused on an
;; Emacs frame.  The reason I wrote it because I thought it would be
;; neat to have the artist's name and title of the song pop up
;; whenever EMMS starts playing something new.

;; EMMS, by the way, is the Emacs Multi-Media System created by Jorgen
;; Schaefer <forcer@forcix.cx> and Ulrik Jensen <terryp@daimi.au.dk>,
;; inspired by an earlier music player front-end for Emacs called
;; `mp3player'.  For more information, see
;; <http://www.gnu.org/software/emms>.

;;; Instructions:

;; First of all, you will probably want to put this file somewhere
;; Emacs looks, most likely `~/.elisp/', and add the following line to
;; your `.emacs':

;;  (require 'osd)

;; To get the basic functionality working, you have to download and
;; install the X OSD Library and the `osd_cat' utility if you don't
;; already have them.  You can get these packages from, for instance,
;;  - the web (<http://www.ignavus.net/>),
;;  - Debian's APT (`xosd-bin' and `libxosd2'), or
;;  - Gentoo's Portage (`x11-libs/xosd').

;; For neat EMMS integration, try this:

;;  (add-hook 'emms-player-start-hook
;;            (lambda () (osd-broadcast-string
;;                        (emms-playlist-get-current))

;; If you think the on-screen text looks a little boring, try using
;; the following settings instead of the defaults.  These are designed
;; to match the Zenburn color theme, which can be found at
;; <http://www.deepwood.net/software/zenburn/zenburn.el>.

;;  (setq osd-program-args
;;        '("--pos"          "top"
;;          "--offset"       "30"
;;          "--align"        "center"
;;          "--delay"        "3"
;;          "--color"        "#dcdccc"
;;          "--outline"      "1"
;;          "--shadow"       "3"
;;          "--shadowcolour" "#1e2320"
;;          "--font"
;;          "-*-new century schoolbook-bold-i-*-*-24-*-*-*-*-*-*-*"))

;;; Wishlist:

;; Add more use cases.  This will also reveal any missing
;; functionality.  Drop me a line if you have any ideas about ways to
;; use this module.

;; Perhaps make the arguments to `osd_cat' customizable in a more
;; user-friendly manner.


;;; Customizable settings

(defgroup osd nil
  "X on-screen display front-end."
  :prefix "osd-")

(defcustom osd-default-display nil
  "The default X display for `osd-show-string' to use.
If nil, use the display inherited from the environment."
  :group 'osd
  :type 'string)

(defcustom osd-broadcast-displays (list 'osd-default-display)
  "The default set of X displays for `osd-broadcast-string' to use.
Each element should be either nil, a string, or a symbol:
 - A nil element represents the display inherited from the environment.
 - All string elements should contain the identifier of an X display
   (e.g., `example.org:0').
 - Symbol elements get treated as if their values had been present
   instead."
  :group 'osd
  :type '(repeat string))

(defcustom osd-program "osd_cat"
  "The program performing the actual on-screen display.
The program will be searched for in PATH and should read its input
from the standard input stream.  It will be given the arguments
specified in `osd-program-args'."
  :group 'osd
  :type 'string)

(defcustom osd-program-args '("--pos" "middle" "--align" "center")
  "The list of arguments to send to `osd-program'."
  :group 'osd
  :type '(repeat string))


;;; Implementation

(defun osd-show-string (string)
  "Display STRING on the default X display.
See `osd-default-display'."
  (interactive "sShow string: ")
  (osd-show-string-on-display string osd-default-display))

(defun osd-show-string-on-inherited-display (string)
  "Display STRING on the X display inherited from the environment."
  (let ((process (apply 'start-process
                        (concat "osd: " string) nil osd-program
                        osd-program-args)))
    (process-send-string process (concat string "\n"))
    (process-send-eof process)))

(defun osd-show-string-on-display (string &optional display)
  "Display STRING on the X display DISPLAY.
If display is nil, the display inherited from the environment is used
instead."
  (interactive "sShow string: \nsShow string on display: ")
  (let ((original-environment process-environment))
    (when display
      (add-to-list 'process-environment (concat "DISPLAY=" display)))
    (osd-show-string-on-inherited-display string)
    (setq process-environment original-environment)))

(defun osd-broadcast-string (string &optional displays)
  "Display STRING on each X display in DISPLAYS.
If DISPLAYS is nil, it defaults to `osd-broadcast-displays'."
  (interactive "sBroadcast string: ")
  (dolist (display (or displays osd-broadcast-displays))
    (osd-show-string-on-display string (if (symbolp display)
                                           (symbol-value display)
                                         display))))

(provide 'osd)
;;; osd.el ends here
