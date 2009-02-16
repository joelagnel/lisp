;;; mew-mule.el --- Environment of Mule common for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Jul 15, 1998

;;; Code:

;;
;; Charset
;;

(defun mew-charset-m17n ()
  (if (string= mew-charset-m17n mew-utf-8)
      (if mew-internal-utf-8p
	  mew-utf-8
	(if (condition-case nil (require 'un-define) (file-error nil))
	    mew-utf-8
	  mew-iso-2022-jp-2))
    mew-iso-2022-jp-2))

(defun mew-charset-guess-string (str)
  (let ((ecsdb (mew-ecsdb-guess-string str)))
    (if ecsdb
	(mew-cs-to-charset (mew-ecsdb-cs ecsdb)) ;; not hcs, take care
      (mew-charset-m17n))))

(defun mew-ecsdb-guess-string (str)
  (with-temp-buffer
    (insert-string str)
    (mew-ecsdb-guess-region (point-min) (point-max))))

(defun mew-charset-guess-region (beg end)
  "Guess charset for the region."
  (interactive "r")
  (let ((ecsdb (mew-ecsdb-guess-region beg end))
	ret)
    (if (null ecsdb)
	(setq ret (mew-charset-m17n))
      (setq ret (mew-cs-to-charset (mew-ecsdb-cs ecsdb)))) ;; not hcs
    (if (interactive-p) (message "%s" ret)) ;; for debug
    ret))

(defun mew-ecsdb-guess-region (beg end)
  (let* ((tcsl (mew-find-cs-region beg end))
	 (N (length tcsl))
	 (alst mew-cs-database-for-encoding)
	 i ecsdb acsl csl ret)
    (while alst
      (setq ecsdb (car alst))
      (setq acsl (mew-ecsdb-lcs ecsdb))
      (setq alst (cdr alst))
      (catch 'loop
	(setq i 0)
	(while (< i N)
	  (if (member (nth i tcsl) acsl)
	      ()
	    (setq ecsdb nil)
	    (setq acsl nil)
	    (throw 'loop nil))
	  (setq i (1+ i))))
      (if (null ret)
	  (setq ret ecsdb)
	(if (and acsl (< (length acsl) (length csl)))
	    (setq ret ecsdb csl acsl))))
    ret))

(defun mew-charset-sanity-check (beg end)
  "Eliminate invalid characters"
  (interactive "r")
  (when (/= mew-mule-ver 0)
    (let ((lcs (mew-find-cs-region beg end)))
      (cond
       ((member mew-lc-kana lcs)
	(require 'mew-lang-jp)
	(mew-zenkaku-katakana-region beg end))
       ((and (memq 'latin-iso8859-1 lcs) (memq 'latin-iso8859-15 lcs)) ;; xxx
	(require 'mew-lang-latin)
	(mew-latin0-region beg end))))))

;;;
;;;
;;;

(defsubst mew-ecsdb-lcs (ecsdb)
  (nth 0 ecsdb))

(defsubst mew-ecsdb-cs (ecsdb)
  (nth 1 ecsdb))

(defsubst mew-ecsdb-cte (ecsdb)
  (nth 2 ecsdb))

(defsubst mew-ecsdb-b-or-q (ecsdb)
  (nth 3 ecsdb))

(defsubst mew-ecsdb-hcs (ecsdb)
  (if (nth 4 ecsdb) (nth 4 ecsdb) (mew-ecsdb-cs ecsdb)))

(defsubst mew-ecsdb-cs-for-arg (ecsdb)
  (let* ((cs (mew-ecsdb-cs ecsdb))
	 (acs (cdr (assoc cs mew-cs-database-for-arg))))
    (or acs cs)))

;;

(defsubst mew-dcsdb-charset (dcsdb)
  (car dcsdb))

(defsubst mew-dcsdb-cs (dcsdb)
  (cdr dcsdb))

;;

(defun mew-cs-to-charset (cs)
  (let ((ecsdb (rassq cs mew-cs-database-for-decoding)))
    (if (null ecsdb)
	(mew-charset-m17n)
      (mew-dcsdb-charset ecsdb))))

(defun mew-charset-to-cs (charset)
  (if (null charset)
      nil
    (let ((ecsdb (assoc (downcase charset) mew-cs-database-for-decoding)))
      (if (null ecsdb)
	  mew-cs-unknown
	(mew-dcsdb-cs ecsdb)))))

(defun mew-charset-to-ecsdb (charset)
  (mew-assoc-equal (mew-charset-to-cs charset) mew-cs-database-for-encoding 1))

(defun mew-charset-to-cte (charset)
  (mew-ecsdb-cte (mew-charset-to-ecsdb charset)))

;;

(defvar mew-charset-list
  (mapcar 'mew-dcsdb-charset mew-cs-database-for-decoding))

;;

(defun mew-cs-encode-arg (arg)
  (let ((cs (mew-ecsdb-cs-for-arg (mew-ecsdb-guess-string arg))))
    (if (mew-coding-system-p cs)
	(mew-cs-encode-string arg cs)
      arg)))

(provide 'mew-mule)

;;; Copyright Notice:

;; Copyright (C) 1998-2005 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-mule.el ends here
