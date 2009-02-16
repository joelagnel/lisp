;;; EMPI-ELISP.EL --- Backend for EMPI to use elisp functions.

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	16 May 2004
;; Version: 	1.0
;; Keywords:	empi, music

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
;; empi-elisp|R.Ramkumar|<andyetitmoves@gmail.com>
;; |Backend for EMPI to use elisp functions.
;; |$Date$|$Revision$|~/packages/empi-elisp.el

;;; Code:

(defun empi-elisp-command (ctx cmd &rest args)
  (and (plist-member (symbol-plist ctx) cmd)
       (apply (or (get ctx cmd) (get ctx :defhandler))
	      (append (and (listp (setq ctx (get ctx :prefix))) ctx) args))))

(provide 'empi-elisp)

;;; EMPI-ELISP.EL ends here
