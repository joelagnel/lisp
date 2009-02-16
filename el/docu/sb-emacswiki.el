;;; Saved through ges-version 0.3.3dev at 2004-05-14 11:49
;;; ;;; From: Markus Knittig <dev-null@knittig.de>
;;; ;;; Subject: Re: sb-emacswiki.el --- emacswiki shimbun backend
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Wed, 05 May 2004 20:36:19 +0200
;;; ;;; Organization: Arcor
;;; ;;; Reply-To: Markus Knittig <news@knittig.de>

;;; [1. text/plain]

;;; Quote [ David Hansen * 3899. September 1993 ]

;;; >> it's so easy (thanks to TSUCHIYA Masatoshi), here's another
;;; >> shimbun.  Read http://www.emacswiki.org/ diffs in your
;;; >> favorite emacs MUA.
;;; > Next try.  This time with attached code.
;;; [Code]

;;; I've also work on a shimbun module for the emacswiki.
;;; Here is my result (works fine for me):

;;; [2. application/emacs-lisp; sb-emacswiki.el]

;;; sb-emacswiki.el --- shimbun backend for emacswiki.org

;; Copyright (C) 2004 Markus Knittig

;; Author: Markus Knittig <markus.knittig@arcor.de>
;; Version: 0.1
;; Keywords: emacs-w3m, shimbun, hypermedia
;; $Id: sb-emacswiki.el,v 1.2 2004/04/11 10:25:09 mak Exp $

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
 
;;; Code:

(require 'shimbun)

(luna-define-class shimbun-emacswiki (shimbun) ())

(defvar shimbun-emacswiki-url "http://www.emacswiki.org/cgi-bin/")

(defvar shimbun-emacswiki-x-face-alist
  '(("default" . "X-Face: $p=x#`BtebCh##L#!YuLDwGcD=bY[)A}<9x=g3nh8rWyz.b2x^y4=?Oefp(g\\X|+iJzc{;:\
d=#WTRE}roS_oh%+gkXq%T,%)fk}Rg,MhE&ytEeE:dqbm\"yU")))

(defvar shimbun-emacswiki-group-path-alist
  '(("RecentChanges" . "wiki.pl?action=rc")
    ("ModificationRecentes" . "emacs-fr.pl?action=rc")))

(defvar shimbun-emacswiki-groups
  (mapcar 'car shimbun-emacswiki-group-path-alist))
(luna-define-method shimbun-index-url ((shimbun shimbun-emacswiki))
  (concat shimbun-emacswiki-url
     	  (cdr (assoc (shimbun-current-group-internal shimbun)
     		      shimbun-emacswiki-group-path-alist))))

(defun shimbun-emacswiki-get-headers ()
  (let ((regexp "<li>\\(.+?\\) (<a.+?href=\"\\([^\"]+\\)\">diff</a>) (.+?)  <a.+?>\\(.+?\\)</a>  \. \. \. \.  \\(.+?\\) </li>")
	(regexp-day "<strong>\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)")
	day subject from date id url headers summary authorsubject)
    (catch 'stop
      ;;find the day
      (while (re-search-forward regexp-day nil t nil)
	(setq day (match-string 1))
	;;find the entry
	(while (re-search-forward regexp nil t nil)
	  (setq date (concat day " " (match-string 1)))
	  (setq url (w3m-expand-url (match-string 2)  shimbun-emacswiki-url ))
	  (setq edited (match-string 3))
	  (setq subject edited)
 	  (setq authorsubject (match-string 4))
	  (if (not (string-match "<a.+?>\\(.+?\\)</a> \\(.*\\)" authorsubject))
	      (when (string-match "\\(.+?\\) \\(.*\\)" authorsubject)
		(setq from (match-string 1 authorsubject))
		(setq summary (match-string 2 authorsubject)))
	    (setq from (match-string 1 authorsubject))
	    (setq summary (match-string 2 authorsubject)))
	  (setq from (concat from " <invalid@emacswiki.org>"))
	  ;;check if summary is present
 	  (when (string-match "<strong>\\(.+?\\)</strong> \\[[a-z]\\{2\\}\\]" summary)
 	    (setq subject (concat subject " " (match-string 1 summary))))
	  ;;create an id
	  (setq id (concat "<" date "." edited "@emacswiki.org>" ))
	  (when (shimbun-search-id shimbun id)
	    (throw 'stop nil))
	  (push (shimbun-make-header
		 0 (shimbun-mime-encode-string subject)
		 (shimbun-mime-encode-string from)
		 date id "" 0 0 url)
		headers))))
    headers))

(luna-define-method shimbun-get-headers ((shimbun shimbun-emacswiki) &optional range)
  (shimbun-emacswiki-get-headers))

(defun shimbun-emacswiki-wash-article (header)
  (save-excursion
    (let ((regexp-begin "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}</a></span>")
	  (regexp-end "<div></div>")
	  begin-region end-region)
    (when (re-search-forward regexp-begin nil t)
      (setq end-region (point))
      (setq begin-region (point-min))
      (delete-region begin-region end-region))
    (when (re-search-forward regexp-end nil t)
      (setq begin-region (point))
      (setq end-region (point-max))
      (delete-region begin-region end-region)))))

(luna-define-method shimbun-make-contents ((shimbun shimbun-emacswiki) header)
		    (shimbun-emacswiki-wash-article header)
		    (shimbun-header-insert-and-buffer-string shimbun header "utf-8" t))

(provide 'sb-emacswiki)
;;; sb-emacswiki.el ends here
;;; [3. text/plain]


;;; Best regards,
;;;  Markus

