;;; EMPI-PROC.EL --- EMPI backend for interfacing through external processes.

;; Copyright (C) 2004 R.Ramkumar

;; Author: 	R.Ramkumar <andyetitmoves@gmail.com>
;; Created: 	12 May 2004
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

;; A copy of the GNU General Public License can be obtained from this
;; program's author (send electronic mail to
;; <andyetitmoves@gmail.com>) or from the Free Software Foundation,
;; Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; empi-proc|R.Ramkumar|<andyetitmoves@gmail.com>
;; |EMPI backend for interfacing through external processes.
;; |$Date: 2004/05/12 10:53:04 $|$Revision: 1.1 $|~/packages/empi-proc.el

;;; Code:

(require 'empi-utils)

(defun empi-proc-command (ctx cmd &rest args)
  (and (plist-member (symbol-plist ctx) cmd)
       (let (prog res)
	 (or (setq prog (get ctx :ihandle))
	     (error "No process specified to execute command %s" cmd))
	 (setq prog (apply 'empi-add-prop prog :ihandle ctx cmd args))
	 (apply 'empi-build-arg-list prog ctx cmd args)
	 (and prog
	      (with-temp-buffer
		(if (setq res (condition-case nil
				  (apply 'call-process (car prog) nil
					 t nil (cdr prog))
				 (error nil)))
		    (progn
		      ;; we don't need signal names
		      (and (stringp res) (setq res 127))
		      (apply 'empi-format-output (buffer-string)
			     res ctx cmd args))
		  (message "Unable to spawn process %s." prog) nil))))))

(provide 'empi-proc)

;;; EMPI-PROC.EL ends here
