;;; EMPI-PIPE.EL --- EMPI backend for interfacing through pipes.

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
;; empi-pipe|R.Ramkumar|<andyetitmoves@gmail.com>
;; |EMPI backend for interfacing through pipes.
;; |$Date: 2004/05/12 10:54:49 $|$Revision: 1.1 $|~/packages/empi-pipe.el

;;; Code:

(defun special-read-file-function (func &optional stream)
  (throw 'extract-over (funcall func stream)))

(defun special-read-file (filename func &optional init finish)
  (let ((load-read-function '(lambda(&optional stream)
			       (special-read-file-function func stream)))
	(load-source-file-function nil)
	(load-path '("."))
	(load-history nil) ret)
    (while (not ret)
      (and init (funcall init))
      (setq ret (catch 'extract-over
		  (condition-case sig
		      (load filename nil t t)
		    (error (not (eq (car sig) 'file-error))))))
      (and finish (funcall finish))) ret))

(defun stream-read-integer (&optional stream)
  (let (char (num-read 0))
    (while (progn (setq char (get-file-char))
		  (and (>= char 48) (<= char 57)))
      (setq num-read (+ (* num-read 10) (- char 48)))) num-read))

(defun stream-read-line (&optional stream)
  (let (char (str ""))
    (while
	(progn
	  (setq char (get-file-char))
	  (not (or (< char 0) (= char ?\n))))
      (setq str (concat str (list char)))) str))

(require 'empi-utils)

(defun empi-pipe-command (ctx cmd &rest args)
  (let (ccell ifile ofile prog)
    (when (setq ccell (get ctx cmd))
      (unless (and (setq ifile (get ctx 'ihandle)) (stringp ifile))
	(error "No input pipe specified"))
      (unless (and (setq ofile (get ctx 'ohandle)) (stringp ofile))
	(error "No outpipe pipe specified"))
      (apply 'empi-build-arg-list prog ctx cmd args)
      (with-temp-buffer
	;; Implement me !!
	  ))))

(provide 'empi-pipe)

;;; EMPI-PIPE.EL ends here
