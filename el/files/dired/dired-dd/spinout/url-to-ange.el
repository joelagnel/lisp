;;; url-to-ange.el: Converts URL expression into ange-ftp expression.

;; Authour: Seiichi Namba <sn@asahi-net.email.ne.jp>
;; Copyright (C) 1996,1997,1998 Seiichi Namba <sn@asahi-net.email.ne.jp>

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 2 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;; Started: Sat Jun  1 13:35:19 1996

;; Most primitive non-interactive usage.
;; 
;; (url-to-ange "ftp://foo.bar.com/pub") =>"/ftp@foo.bar.com:/pub"
;; "file://" or "http://" type is not supported at all.
;; 
;; Rough interactive shell open-ftp-url, and open-ftp-url-region
;; is included.
;; 
;; Also patches dired-filename-at-point in dired-x.el.  This enables
;; opening URL already existing in a buffer with
;; 
;;   C-u C-x C-f (dired-x-find-file) or
;;   C-u C-x 4 f (dired-x-find-file-other-window)
;; 
;; Try moving on next URL, then hit C-u C-x 4 f RET
;;   ftp://ftp.gnu.org/pub/gnu

;; Now we need not require this, right ?
;;(require 'ange-ftp)

;; Convert URL to ange-ftp
(defun url-to-ange (s)
  "Converts URL string into ange-ftp expression, and return it.
Non-interactive usage:

 (url-to-ange \"ftp://foo.bar.com/pub\")
            =>\"/ftp@foo.bar.com:/pub\"

\"file://\" or \"http://\" type is not supported at all.

If interactively called, asks URL."
  (interactive "sURL: ")
  (let* ((tem (substring s 6))
	 (topslash (string-match "/" tem)))
    (concat 
     "/ftp@"
     (substring tem 0 topslash)
     ":"
     (substring tem (string-match "/" tem)))))

;; Two primitive user interfaces for url-to-ange.
;; Sat Jun  1 13:35:26 1996
(defun open-ftp-url (s)
  "Asks for URL and opens the location with dired-other-window."
  (interactive "sURL: ")
  (dired-other-window (url-to-ange s)))
(defun open-ftp-url-region (rb re)
 "Regard REGION as url expression, and opens the location with open-ftp-url."
  (interactive "r")
  (open-ftp-url (buffer-substring (region-beginning) (region-end))))

;; Patch added to use url-to ange in dired-x
;; copied from dired-x.el of mule 2.3. Sun Jun  9 21:47:56 1996
;; Seems like working fine in emacs-20.2 (or needs some modification ?).
(require 'dired-x)			; then patch this...
(defun dired-filename-at-point ()

  ;; Get the filename closest to point, but do not change position.  Has a
  ;; preference for looking backward when not directly on a symbol.  Not
  ;; perfect - point must be in middle of or end of filename.

  (let ((filename-chars ".a-zA-Z0-9---_/:$+")
        (bol (save-excursion (beginning-of-line) (point)))
        (eol (save-excursion (end-of-line) (point)))
        start end filename)

    (save-excursion
      ;; First see if just past a filename.
      (if (not (eobp))
          (if (looking-at "[] \t\n[{}()]") ; whitespace or some parens
              (progn
                (skip-chars-backward " \n\t\r({[]})")
                (if (not (bobp))
                    (backward-char 1)))))

      (if (string-match (concat "[" filename-chars "]")
                        (char-to-string (following-char)))
          (progn
            (skip-chars-backward filename-chars)
            (setq start (point))
            (if (string-match "[/~]" (char-to-string (preceding-char)))
                (setq start (1- start)))
            (skip-chars-forward filename-chars))

        (error "No file found around point!"))

      ;; Return string.
      ;; Convert to ange format if it is ftp://....
      (let ((tem (buffer-substring start (point))))
	(if (string-match "ftp://" tem)
	    (url-to-ange tem)
	  (expand-file-name tem))))))

(provide 'url-to-ange)
