;;; ysq.el --- look up stock quotes using Yahoo

;; Copyright (C) 1999, 2000 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: extensions
;; Created: 1999-03-31

;; $Id: ysq.el,v 1.6 2002/06/26 00:05:50 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Updates of this program may be available via the URL
;; http://www.splode.com/~friedman/software/emacs-lisp/

;;; Code:

(defvar ysq-server "quote.yahoo.com")
(defvar ysq-port   "http")
(defvar ysq-cgi    "/d/quotes.csv")

(defvar ysq-query-tickers '("aol"))

(defvar ysq-query-fields
  '(ticker-symbol                       ; Should be first; search key
    52-week-high
    52-week-low
    ask
    bid
    change
    company-name
    daily-high
    daily-low
    date
    dividend-date
    dividend/share
    earnings/share
    error
    ex-dividend-date
    last-price
    market-capitalization
    opening-price
    previous-close
    price/earning-ratio
    stock-exchange-name
    time
    volume
    yield
    ;; Must be last because of parsing considerations,
    ;; see ysq-ticker-collect-parse.
    shares-outstanding))

(defvar ysq-field-alist
  '((52-week-high          "k")
    (52-week-low           "j")
    ;; redundant; see j,k
    (52-week-range         "w")
    (ask                   "a")
    (bid                   "b")
    (change                "c1")
    ;; redundant; see c1,[(l1-p)/p]*100
    (change-value-percent  "c")
    (company-name          "n"  ysq-read-company-name)
    (daily-high            "h")
    (daily-low             "g")
    (date                  "d1")
    ;; redundant; see h,g
    (day-range             "m")
    (dividend-date         "r1")
    (dividend/share        "d")
    (earnings/share        "e")
    (error                 "e1" ysq-read-error)
    (ex-dividend-date      "q")
    ;; `info' contains a string of the form "cnsprmi", which means that
    ;; Yahoo has more information for this stock, e.g. Chart, News, SEC,
    ;; Msgs, Profile, Research, Insider.  Not very useful to this
    ;; interface.
    (info                  "i")
    (last-price            "l1")
    ;; redundant; see l1
    (last-price-y2         "y2")
    ;; redundant; see d1,l1
    (last-trade-time-price "n1" ysq-read-string)
    (market-capitalization "j1" ysq-read-string)
    (opening-price         "o")
    (previous-close        "p")
    (price/earning-ratio   "r")
    (shares-outstanding    "j2" ysq-read-shares-outstanding)
    (stock-exchange-name   "x")
    (ticker-symbol         "s")
    (time                  "t1")
    ;; I don't know what these fields are meant to contain, but the cgi
    ;; seems to return something for them; usually N/A.
    (unknown-m1            "m1")
    (unknown-n2            "n2")
    (unknown-w1            "w1")
    (volume                "v")
    (yield                 "y")))

(defvar ysq-reader-default 'ysq-read)

(defun ysq-field-form-identifier (field &optional field-alist)
  (nth 1 (assq field (or field-alist ysq-field-alist))))

(defun ysq-field-reader (field &optional field-alist)
  (or (nth 2 (assq field (or field-alist ysq-field-alist)))
      ysq-reader-default))

(defun ysq-open (server port sentinel)
  (let* ((newbuf (generate-new-buffer " *ysq data*"))
         (proc (open-network-stream "ysq" newbuf server port)))
    (buffer-disable-undo newbuf)
    (set-process-sentinel proc sentinel)
    proc))

(defun ysq-send (proc &rest args)
  (while args
    (process-send-string proc (car args))
    (setq args (cdr args))))


(defvar ysq-ticker-collected-async-data nil)
(defvar ysq-ticker-async-process nil)

(defun ysq-ticker-collect (&rest ticker-names)
  (ysq-ticker-collect-internal 'ysq-ticker-collect-sentinel-sync
                               'ysq-ticker-collector-sync
                               ticker-names))

(defun ysq-ticker-collect-async (&rest ticker-names)
  (or (ysq-process-live-p ysq-ticker-async-process)
      (setq ysq-ticker-collected-async-data nil)
      (setq ysq-ticker-async-process
            (ysq-ticker-collect-internal 'ysq-ticker-collect-sentinel-async
                                         nil ticker-names))))

(defun ysq-ticker-collect-internal (sentinel collector ticker-names)
  (or ticker-names
      (setq ticker-names ysq-query-tickers))
  (let* ((proc    (ysq-open ysq-server ysq-port sentinel))
         (fields  (mapconcat 'ysq-field-form-identifier ysq-query-fields ""))
         (tickers (mapconcat 'identity ticker-names "+")))

    (ysq-send proc
              (format "GET %s?%s&%s&%s HTTP/1.0\r\n"
                      ysq-cgi
                      (format "f=%s" fields)
                      (format "s=%s" tickers)
                      "e=.csv")
              ;; Put any http 1.0 headers here.
              "\r\n")

    (if collector
        (funcall collector proc)
      proc)))

(defun ysq-process-live-p (proc)
  (and (processp proc)
       (memq (process-status proc) '(open run))))

;; This sentinel is a no-op, but keeps any process-exit message from being
;; inserted into the process buffer.
(defun ysq-ticker-collect-sentinel-sync (proc string)
  nil)

(defun ysq-ticker-collector-sync (proc)
  (while (ysq-process-live-p proc)
    (accept-process-output))
  (let ((result nil))
    (save-excursion
      (set-buffer (process-buffer proc))
      (goto-char (point-min))
      (setq result (ysq-ticker-collect-parse)))
    (kill-buffer (process-buffer proc))
    result))

(defun ysq-ticker-collect-sentinel-async (proc string)
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (point-min))
    (setq ysq-ticker-collected-async-data (ysq-ticker-collect-parse))
    (kill-buffer (process-buffer proc)))
  (setq ysq-ticker-async-process nil))

(defun ysq-ticker-collect-parse ()
  (save-match-data

    ;; easier to get rid of trailing RETs in a single pass.
    (while (re-search-forward "\r$" nil t)
      (replace-match ""))
    (goto-char (point-min))

    (cond ((looking-at "^HTTP/1\\.")
           (re-search-forward "^$")
           (forward-char)))

    (let ((all-data nil))
      (while (not (eobp))
        (let ((field ysq-query-fields)
              (data nil)
              parsed p)
          (while (not (eolp))
            (setq p (point))

            ;; Special hack because, annoyingly, the shares outstanding are
            ;; printed with commas in the number and are not quoted to avoid
            ;; ambiguity with the spreadsheet field separator.
            (cond ((eq (car field) 'shares-outstanding)
                   (re-search-forward "\\([0-9,]+\\)\\|$" nil t))
                  ((char-equal (char-after) ?\")
                   (re-search-forward "\\(\"\\)," nil t))
                  (t
                   (re-search-forward ",\\|$" nil t)))
            (goto-char (match-end 0))
            (setq parsed
                  (funcall (ysq-field-reader (car field))
                           (buffer-substring p (or (match-end 1)
                                                   (match-beginning 0)))))
            (cond (parsed
                   (setq data (cons (cons (car field) parsed) data))
                   (and (eq (car field) 'ticker-symbol)
                        (setq ticker-symbol parsed))))
            (setq field (cdr field)))
          (setq all-data (cons (nreverse data) all-data))
          (forward-char)))
      (nreverse all-data))))


;; Readers for various kinds of data.

(defun ysq-read (obj)
  (and (ysq-read-string obj)
       (read obj)))

(defun ysq-read-error (obj)
  (save-match-data
    (cond ((null (ysq-read-string obj))
           nil)
          ((string-match "No such ticker symbol" obj)
           (substring obj (match-beginning 0) (match-end 0)))
          ((string-match "^\".*\"$" obj)
           (substring obj 1 -1)))))

(defun ysq-read-company-name (obj)
  (setq obj (ysq-read obj))
  (save-match-data
    (cond ((not (stringp obj))
           obj)
          ((string-match "[ \t]+$" obj)
           (substring obj 0 (match-beginning 0)))
          (t obj))))

(defun ysq-read-shares-outstanding (obj)
  (save-match-data
    (let ((s "")
          (p 0))
      (and (string-match "^\\s-+" obj)
           (setq p (match-end 0)))
      (while (string-match ",+" obj p)
        (setq s (concat s (substring obj p (match-beginning 0))))
        (setq p (match-end 0)))
      ;; Return a string because in many cases, the number of shares
      ;; outstanding a company might have overflows emacs' maximum integer
      ;; range on 32-bit machines.  For example, AOL has over 933 million
      ;; shares outstanding as of 1999-04-04.
      (concat s (substring obj p)))))

(defun ysq-read-string (obj)
  (cond ((or (string= obj "N/A")
             (string= obj "\"N/A\""))
         nil)
        (t obj)))

(provide 'ysq)

;; ysq.el ends here
