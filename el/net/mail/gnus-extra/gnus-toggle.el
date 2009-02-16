;; From nobody Sat Jan 20 21:04:41 2001
;; Path: news.wam.umd.edu!news.umd.edu!bloom-beacon.mit.edu!news-peer.gip.net!news.gsl.net!gip.net!news.maxwell.syr.edu!feed2.onemain.com!feed1.onemain.com!newsfeed2.earthlink.net!newsfeed.earthlink.net!news.mindspring.net!not-for-mail
;; From: burton@relativity.yi.org (Kevin A. Burton)
;; Newsgroups: gnu.emacs.sources
;; Subject: gnus-toggle.el
;; Date: 20 Jan 2001 13:01:01 -0800
;; Organization: OpenPrivacy.org
;; Lines: 132
;; Message-ID: <m3k87qey5u.fsf@relativity.yi.org>
;; NNTP-Posting-Host: d8.af.66.4b
;; Mime-Version: 1.0
;; Content-Type: text/plain; charset=us-ascii
;; X-Server-Date: 20 Jan 2001 22:41:00 GMT
;; User-Agent: Gnus/5.0807 (Gnus v5.8.7) Emacs/20.7
;; Xref: news.wam.umd.edu gnu.emacs.sources:9221

;; -----BEGIN PGP SIGNED MESSAGE-----
;; Hash: SHA1


;;; gnus-toggle.el --- toggle back and forth between gnus and your normal emacs
;;                    buffers.

;; Copyright (C) 1997-2000 Free Software Foundation, Inc.

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords:
;; Version: 

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; Gnus needs a decent mechanism to switch back and forth between your gnus
;; session and your regular Emacs session.  I generally go back and forth
;; between code and e-mail so something that keeps track of how my Emacs windows
;; are setup is a good thing.

;;; Install:

;; In your .emacs add a (require 'gnus-toggle) line.  From within emacs you can
;; then run gnus-toggle to switch back and forth between gnus and your regular
;; emacs buffers.

;;; History:
;; 
;; Sat Jan 20 12:48:43 2001 (burton):  init

;;; Code:
(defvar gnus-toggle-default-window-configuration nil
  "Window configuration in use right before you run `gnus-toggle'.")

(defvar gnus-toggle-within-gnus-window-configuration nil
  "Window configuration in use right before you run `gnus-toggle'.")

;;;###autoload
(defun gnus-toggle()
  "If the current buffer is not a gnus buffer, select the last gnus
configuration, else restore your normal workspace."

  (interactive)
  
  (if (or (equal major-mode 'gnus-group-mode)
          (equal major-mode 'gnus-summary-mode)
          (equal major-mode 'gnus-article-mode))
      (progn
        
        (message "Leaving gnus... ")
        
        (setq gnus-toggle-within-gnus-window-configuration (current-window-configuration))
        
        (if gnus-toggle-default-window-configuration
            (set-window-configuration gnus-toggle-default-window-configuration)
          ;;if we don't have a window config... this might be  because we just started emacs/gnus
          (switch-to-buffer "*scratch*")))
    (progn

      (setq gnus-toggle-default-window-configuration (current-window-configuration))

      (if gnus-toggle-within-gnus-window-configuration
          (progn

            (if (get-buffer "*Group*")
                (progn
                  (message "Entering gnus... ")
                  (set-window-configuration
                   gnus-toggle-within-gnus-window-configuration))
              (error "Gnus is not running")))))))


(defun gnus-toggle-update-window-configuration()
  "Update the window configuration used by gnus."
  (interactive)

  (if (or (equal major-mode 'gnus-group-mode)
          (equal major-mode 'gnus-summary-mode)
          (equal major-mode 'gnus-article-mode))

      (setq gnus-toggle-within-gnus-window-configuration (current-window-configuration))))

;;add hooks so that gnus window configuration is updated at regular intervals so
;;that if the user changes buffers manually we at least have a somewhat
;;up-to-date copy of what the gnus configuration looked like.

(add-hook 'gnus-group-mode-hook 'gnus-toggle-update-window-configuration)
(add-hook 'gnus-summary-mode-hook 'gnus-toggle-update-window-configuration)
(add-hook 'gnus-article-mode-hook 'gnus-toggle-update-window-configuration)

(provide 'gnus-toggle)

;;; gnus-toggle.el ends here


;; - -- 
;; Kevin A. Burton ( burton@apache.org, burton@openprivacy.org, burtonator@acm.org )
;;         Cell: 408-910-6145 URL: http://relativity.yi.org ICQ: 73488596 

;; Any programming language is at its best before it is implemented and used.



;; -----BEGIN PGP SIGNATURE-----
;; Version: GnuPG v1.0.4 (GNU/Linux)
;; Comment: Get my public key at: http://relativity.yi.org/pgpkey.txt

;; iD8DBQE6afyMAwM6xb2dfE0RAlvWAJwJgXvOIzpDH3AbN13aFeNGUWS6JACglmn8
;; qxFXy7pjCuHKwjl/CNxIzAM=
;; =2jp+
;; -----END PGP SIGNATURE-----



;; SDI Kennedy Legion of Doom Panama World Trade Center assassination Treasury
;; Cocaine FSF bomb FBI AK-47 class struggle South Africa Khaddafi

