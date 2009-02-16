;;; EMPI-FORWARDER.EL --- EMPI handler for forwarding missing features in other handlers

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	22 May 2004
;; Version: 	1.0
;; Keywords:	music, empi

;; This file is (strangely) *NOT* part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 1, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License can be obtained from this program's
;; author (send electronic mail to <andyetitmoves@gmail.com>) or from the Free
;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; empi-forwarder|R.Ramkumar|<andyetitmoves@gmail.com>
;; |EMPI handler for forwarding missing features in other handlers
;; |$Date$|$Revision$|~/packages/empi-forwarder.el

;;; Code:

(require 'empi-elisp)
(require 'empi-core)

(defvar empi-forward 'empi-elisp-command)

;; pause for empi-mpd not working properly.

(defun plist-replace-key (plist old new)
  (catch 'plist-replace-key-found
    (while plist
      (when (eq (car plist) old)
	(setcar plist new)
	(throw 'plist-replace-key-found plist))
      (setq plist (cddr plist)))))

(defsubst safe-plist-modify-key (plist old new)
  (and (listp plist) (plist-replace-key plist old new)) plist)

(defun empi-forward-baladd (by)
  (let ((bal (empi-forward-query :qbalance)))
    (when (numberp bal)
      (safe-plist-modify-key (empi-forward-command :balance (+ bal by))
			     :balance :baladd))))

(put 'empi-forward :baladd 'empi-forward-baladd)

(defun empi-forward-balsub (by)
  (let ((bal (empi-forward-query :qbalance)))
    (when (numberp bal)
      (safe-plist-modify-key (empi-forward-command :balance (- bal by))
			     :balance :balsub))))

(put 'empi-forward :balsub 'empi-forward-balsub)

(defun empi-forward-qsonglength ()
  (let (qper qtime)
    (and (setq qper (empi-forward-query :qtime%)) (numberp qper)
	 (>= qper 0) (<= qper 100)
	 (setq qtime (empi-forward-query :qtime)) (wholenump qtime)
	 (round (/ (* qtime 100) qper)))))

(put 'empi-forward :qsonglength 'empi-forward-qsonglength)

(defun empi-forward-qtime ()
  (let (qsl qper)
    (and (setq qper (empi-forward-query :qtime%)) (numberp qper)
	 (>= qper 0) (<= qper 100)
	 (setq qsl (empi-forward-query :qsonglength)) (wholenump qsl)
	 (round (/ (* qper qsl) 100)))))

(put 'empi-forward :qtime 'empi-forward-qtime)

(defun empi-forward-qtime% ()
  (let (qsl qtime)
    (and (setq qsl (empi-forward-query :qsonglength)) (wholenump qsl)
	 (setq qtime (empi-forward-query :qtime)) (wholenump qtime)
	 (round (/ (* qtime 100) qsl)))))

(put 'empi-forward :qtime% 'empi-forward-qtime%)

(provide 'empi-forwarder)

;;; EMPI-FORWARDER.EL ends here
