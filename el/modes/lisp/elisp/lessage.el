;;;; lessage.el -- like message but less intrusive
;;; Time-stamp: <2004-12-04 13:40:27 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

(provide 'lessage)

(defvar lessages t
  "Spool space for lessage and dlesg, which see.")

(defun lessage (&rest args)
  "Like message, but puts the result onto a list for despooling later with dlesg.
Use this for debugging messages that you do not want to clutter the minibuffer."
  (when (listp lessages)
    (push (apply 'format args) lessages)))

(defun dlesg ()
  "Despool messages from lessage.
Named after dmesg on Unix."
  (with-output-to-temp-buffer "*Lessages*"
    (mapcar
     (lambda (str)
       (princ str)
       (princ "\n"))
     (nreverse lessages)))
  (setq lessages t))

;;; end of lessage.el




