;;; fmailutils.el -- random mail frobnication utilities

;; Copyright (C) 1992, 1993, 1995, 1997, 1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com
;; Keywords: mail, extensions
;; Status: Works in Emacs 19, 20, and XEmacs

;; $Id: fmailutils.el,v 1.14 1999/10/06 09:20:58 friedman Exp $

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

;; These functions are meant to be primitives with which to build other
;; header-parsing and manipulating routines.

;; These functions use mail-header-separator to determine the end of the
;; mail header list in a given buffer.  This string is by default set to
;; "--text follows this line--" but you can change it to something else
;; temporarily via a (let ...).  To hack incoming email messages in their
;; presentation buffer, for example, you would bind that variable to "".

;; I find it particular useful to put this in my .emacs:
;;    (require 'fmailutils)
;;    (add-hook 'mail-send-hook 'fmailutils-add-fcc-related-headers)
;; So that my saved messages get threaded properly with VM and GNUS.

;; Inspiration for this package came from a couple of mail-parsing
;; functions written by Mike Williams around 1991 or so.

;; New versions of this program should usually be available from
;; http://www.splode.com/~friedman/software/emacs-lisp/index.html

;;; Code:

(require 'sendmail)

(defconst fmailutils-header-name-regexp "^[^:\n\t ]+:")

(defvar fmailutils-set-from-address-hook nil
  "*Hooks to run in fmailutils-set-from-address after everything else.
These functions are run while preserving the original modification
state of the buffer, i.e. if the composition buffer initially is
marked \"unmodified\", the buffer will remain \"unmodified\" after the
hooks are run, even if one of the hooks makes changes to the buffer.")

(defmacro fmailutils-save-state (&rest body)
  (list 'let '((case-fold-search t))
        (cons 'save-excursion
              body)))

;; indent like save-excursion
(put 'fmailutils-save-state 'lisp-indent-function 0)


(defun fmailutils-header-separator-position ()
  "Return point of the beginning of the mail-header-separator line.
Returns nil if there isn't one."
  (fmailutils-save-state
    (goto-char (point-min))
    (and (re-search-forward
          (concat "^" (regexp-quote mail-header-separator) "$") nil t)
         (match-beginning 0))))

(defun fmailutils-body-start-position ()
  "Return point of the beginning of the message body."
  (fmailutils-save-state
    (goto-char (point-min))
    (and (re-search-forward
          (concat "^" (regexp-quote mail-header-separator) "$") nil t)
         (1+ (match-end 0)))))

(defun fmailutils-header-start (&optional header relative)
  "Return point where header HEADER begins.
If header name is not specified, use current header.
Return nil if header doesn't exist or point isn't in a mail header.
If optional arg RELATIVE is non-nil, search for position of first header at or
after point before wrapping around to beginning of message headers."
  (cond (header
         (fmailutils-save-state
           (and (fmailutils-position-on-field header 'soft relative)
                (fmailutils-current-header-start))))
        (t
         (fmailutils-current-header-start))))

(defun fmailutils-current-header-start ()
  "Return position where current header name begins.
This position is not the beginning of the header contents, but the
position where the actual header name begins.

If point is not in a mail header \(e.g. it is in the body of the message\),
return nil."
  (let ((sep-pos (fmailutils-header-separator-position))
        (pos (point)))
    (cond ((and sep-pos (< pos sep-pos))
           (fmailutils-save-state
             (if (re-search-forward fmailutils-header-name-regexp sep-pos t)
                 (goto-char (match-beginning 0))
               (goto-char sep-pos))
             (re-search-backward fmailutils-header-name-regexp nil t)
             (match-beginning 0))))))

(defun fmailutils-next-header-start (&optional count wrap)
  "Return position where next mail header begins.
If optional numeric arg COUNT is non-nil, skip forward or backward that
 many headers.
If at beginning or end of headers and optional arg WRAP is non-nil, return
 position of end or beginning of headers, respectively.  Otherwise return nil."
  (and (null count)
       (setq count 1))
  (let ((current (if (> count 0)
                     (fmailutils-current-header-contents-begin)
                   (fmailutils-current-header-start))))
    (and current
         (let* ((next-start current)
                (min (point-min))
                (max (fmailutils-header-separator-position))
                (forwardp (> count 0))
                (fn (if forwardp 're-search-forward 're-search-backward))
                (bound (if forwardp max min)))
           (fmailutils-save-state
             (while (and current
                         (not (zerop count)))
               (goto-char next-start)
               (cond ((funcall fn fmailutils-header-name-regexp bound t)
                      (setq current (match-beginning 0))
                      (setq next-start (if forwardp
                                           (match-end 0)
                                         (match-beginning 0)))
                      (setq count (if forwardp
                                      (1- count)
                                    (1+ count))))
                     (wrap
                      (setq current (if forwardp min max))
                      (setq next-start current))
                     (t
                      (setq current nil)))))
           current))))

(defun fmailutils-current-header-contents-begin ()
  "Return position of beginning of contents for current header."
  (let ((start (fmailutils-current-header-start)))
    (and start
         (fmailutils-save-state
           (goto-char start)
           (re-search-forward ":[ \t]?")
           (match-end 0)))))

(defun fmailutils-current-header-contents-end ()
  "Return position of end of contents for current header."
  (let ((start (fmailutils-current-header-contents-begin))
        (max (fmailutils-header-separator-position)))
    (and start
         (fmailutils-save-state
           (goto-char start)
           (if (re-search-forward fmailutils-header-name-regexp max t)
               (goto-char (match-beginning 0))
             (goto-char max))
           (skip-chars-backward "\n")
           (point)))))

(defun fmailutils-current-header ()
  "Return name of mail header at point.
If point is not in a mail header \(e.g. it is in the body of the message\),
return nil."
  (let ((start (fmailutils-current-header-start)))
    (and start
         (fmailutils-save-state
           (goto-char start)
           (re-search-forward fmailutils-header-name-regexp nil t)
           (buffer-substring (match-beginning 0) (1- (match-end 0)))))))

(defun fmailutils-header-contents (&optional header relative all)
  "Return a list containing contents of header named HEADER.
If no occurrences of header exist in the current mail buffer, return nil.
If optional arg RELATIVE is non-nil, return contents of first matching
 header at after point; otherwise, return contents of first matching header.
If optional arg ALL is non-nil, list contains all header contents as
 separate elements, in order; relative arg is ignored."
  (or header
      (setq header (fmailutils-current-header)))
  (and all
       (setq relative nil))
  (fmailutils-save-state
    (save-restriction
      (let ((re-header (fmailutils-make-header-regexp header))
            (contents-list nil)
            (doit t)
            end beg)
        (while (and doit
                    (fmailutils-position-on-field header 'soft relative))
          (setq end (point))
          (re-search-backward re-header)
          (setq beg (match-end 0))
          (setq contents-list (cons (buffer-substring beg end) contents-list))
          (narrow-to-region (1+ end) (point-max))
          (setq doit all))
        (nreverse contents-list)))))

(defun fmailutils-get-header-contents (header)
  "This function is obsolete.  Use fmailutils-header-contents."
  (fmailutils-header-contents header nil t))

(defun fmailutils-get-header-names (&optional uniquep)
  "Return a list of all existing mail headers, or nil if none.
If optional argument UNIQUEP is non-nil, only list each header name once,
even if it appears more than once in the mail headers."
  (fmailutils-save-state
    (let ((mail-headers-end (fmailutils-header-separator-position))
          mail-header-list this-mail-header)
      (cond (mail-headers-end
             (goto-char (point-min))
             (while (re-search-forward fmailutils-header-name-regexp
                                       mail-headers-end t)
               (setq this-mail-header
                     (buffer-substring (match-beginning 0)
                                       (1- (match-end 0))))
               (or (and uniquep (member this-mail-header mail-header-list))
                   (setq mail-header-list
                         (cons this-mail-header mail-header-list))))
             (nreverse mail-header-list))))))

(defun fmailutils-put-header (header contents)
  "Add HEADER to the current mail message, with CONTENTS.
If the header already exists in the message, place this header and contents
one below last occurence, on a new line.

Use `fmailutils-put-unique-header' if you want to overwrite pre-existing
headers and their contents."
  (if (fmailutils-position-on-field header 'soft)
      (fmailutils-save-state
        (save-restriction
          (let ((mail-header-end (fmailutils-header-separator-position)))
            (while (fmailutils-position-on-field header 'soft)
              (narrow-to-region (1+ (point)) mail-header-end))
            (insert header ": " contents "\n"))))
    ;; use fmailutils-put-unique-header, which will put the header at the
    ;; end of the header list.  This is where we really want it since no
    ;; previous header of the same name exists anyway.
    (fmailutils-put-unique-header header contents)))

(defun fmailutils-put-unique-header (header contents &optional replace relative)
  "Add HEADER to the current mail message, with the given CONTENTS.
If the header already exists, the contents are left unchanged,
 unless optional argument REPLACE is non-nil.
If optional arg RELATIVE is non-nil and point is in message headers, then
 replace contents of the next occurence of HEADER after point.  If there is
 no such header after point, then behave normally, i.e. replace the first
 incidence of header anywhere in message headers."
  (fmailutils-save-state
    (let ((header-exists (fmailutils-position-on-field header nil relative))
          beg end)
      ;; Delete old contents if replace is set
      (and header-exists
           replace
           (progn
             (setq end (point))
             (re-search-backward (fmailutils-make-header-regexp header))
             (setq beg (goto-char (match-end 0)))
             (delete-region beg end)))
      ;; Add new contents if replace is set, or this is a new header.
      (and (or (not header-exists) replace)
           (insert contents)))))

(defun fmailutils-remove-header (header &optional relative all)
  "Remove first instance of HEADER \(and contents\).
If optional arg RELATIVE is non-nil, remove the first instance of header
 found at or after point; otherwise, start from the top of all the headers.
If optional third argument ALL is non-nil, every instance is removed."
  (fmailutils-save-state
    (let ((doit t)
          beg end)
      (while (and doit
                  (fmailutils-position-on-field header 'soft relative))
        (setq end (point))
        (re-search-backward fmailutils-header-name-regexp)
        (setq beg (match-beginning 0))
        (delete-region beg (1+ end))
        (setq doit all)))))

(defun fmailutils-append-header-contents (header contents &optional relative)
  "Append CONTENTS to existing contents of header HEADER.
Otherwise create new header and append contents to it.
If RELATIVE is non-nil, search for the next header named HEADER after point
and append contents there.  Otherwise, apply to first instance of named
header."
  (fmailutils-save-state
    (fmailutils-position-on-field header nil relative)
    (insert contents)))


;; mail-position-on-field is broken in every version of Lucid/XEmacs up to
;; and including version 20.2.  There's a bug I fixed in 1992, with regard
;; to inserting the first header into a buffer, that never found its way
;; into the XEmacs branch until mid 1997.  In any case, this version has
;; some different behavior, to wit:
;;
;;   1) This function avoids moving point if `soft' is non-nil and field
;;      doesn't exist.
;;   2) New optional arg `relative'.
;;
(defun fmailutils-position-on-field (field &optional soft relative)
  "Move point to header named FIELD, creating header if necessary.
If header contains data, put point at the end of that data.
If optional argument SOFT is non-nil and no header by that name already
 exists, do nothing at all.
If optional argument RELATIVE is non-nil, search for header at or after point.
 If none is found, wrap search around the beginning of the message headers."
  (let ((case-fold-search t)
        (end (fmailutils-header-separator-position))
        (pos (point)))
    (save-match-data
      (cond ((and relative
                  (< pos end))
             (goto-char (fmailutils-current-header-start)))
            (t
             (goto-char (point-min))))
      (cond ((re-search-forward (fmailutils-make-header-regexp field) end t)
             (re-search-forward "^[^ \t]" end 'move)
             (beginning-of-line)
             (skip-chars-backward "\n")
             t)
            (relative
             ;; Try again without relative positioning
             (fmailutils-position-on-field field soft))
            ((not soft)
             (goto-char end)
             (insert field ": \n")
             (skip-chars-backward "\n")
             nil)
            (t
             (goto-char pos)
             nil)))))

(defun fmailutils-make-header-regexp (header)
  (concat "^" (regexp-quote header) ":[ \t]?"))

(defun fmailutils-quote-address-comment (comment)
  "Properly quote comment if it contains RFC822 `special' characters."
  (save-match-data
    ;; These `specials' are defined in rfc822
    (if (string-match "[][()<>@,;:\\\\\".]" comment)
        (if (string-match "\"" comment)
            (concat "\""
                    (fmailutils-replace-string-regexp comment "\"" "\\\"")
                    "\"")
          (concat "\"" comment "\""))
      comment)))

(defun fmailutils-replace-string-regexp (string regexp replacement &optional count)
  "In string, replace occurences matching regexp with replacement.
Optional argument count means replace first count occurences found,
otherwise replace all of them.  The original string is not modified."
  (save-match-data
    (let ((pos 0)
          (newstr ""))
      (while (and (or (null count)
                      (prog1
                          (> count 0)
                        (setq count (1- count))))
                  (string-match regexp string pos))
        (setq newstr
              (concat newstr
                      (substring string pos (match-beginning 0))
                      replacement))
        (setq pos (match-end 0)))
      (concat newstr (substring string pos)))))


;; Return a string suitable for use as a Message-Id token.
;; This is useful for maintaining proper threading in messages you save to
;; folders via FCC headers.
(defun fmailutils-make-message-id ()
  "Return a string suitable for use as a Message-Id token."
  (let* ((tm (current-time))
         (timestr (cond
                   ((fboundp 'format-time-string)
                    (format-time-string "%Y%m%d%H%M%S" tm))
                   (t
                    ;; To support Emacs 19.28 and earlier.
                    (let ((str (current-time-string tm)))
                      (format "%s%02d%s%s%s%s"
                              (substring str 20 24)
                              (length (member (substring str 4 7)
                                              '("Dec" "Nov" "Oct"
                                                "Sep" "Aug" "Jul"
                                                "Jun" "May" "Apr"
                                                "Mar" "Feb" "Jan")))
                              (substring str 8 10)
                              (substring str 11 13)
                              (substring str 14 16)
                              (substring str 17 19))))))
         (pid (if (fboundp 'emacs-pid)
                  (emacs-pid)
                (random 999999)))
         (msec (or (nth 2 tm) 0)))
    (format "<%s.%d.FMU%d@%s>" timestr msec pid (system-name))))

;; This function is useful for putting dates on mail-send-hook if you want
;; to control the date header format in your outgoing messages, or just
;; provide dates in messages saved to folders via the FCC header.
(defun fmailutils-rfc822-date (&optional time)
  "Return a string of the form \"Thu, 01 Jan 1970 00:00:00 -0000 \(UTC\)\"."
  (or time (setq time (current-time)))
  (let ((datestr (cond
                  ((fboundp 'format-time-string)
                   (format-time-string "%a, %d %b %Y %H:%M:%S" time))
                  (t
                   ;; To support Emacs 19.28 and earlier.
                   (let ((str (current-time-string time)))
                     (format "%s, %s %s %s %s"
                             (substring str  0  3)
                             (substring str  8 10)
                             (substring str  4  7)
                             (substring str 20 24)
                             (substring str 11 19))))))
        (tzoff (fmailutils-rfc822-time-zone-offset time))
        (tznam (fmailutils-rfc822-time-zone-name time)))
    (cond ((and tzoff tznam)
           (format "%s %s (%s)" datestr tzoff tznam))
          ((or tzoff tznam)
           (format "%s %s" (or tzoff tznam)))
          (t datestr))))

(defun fmailutils-rfc822-time-zone-offset (&optional time)
  (and (fboundp 'current-time-zone)
       (let* ((sec (or (car (current-time-zone time)) 0))
              (absmin (/ (abs sec) 60)))
         (format "%c%02d%02d"
                 (if (< sec 0) ?- ?+)
                 (/ absmin 60)
                 (% absmin 60)))))

(defun fmailutils-rfc822-time-zone-name (&optional time)
  (cond ((fboundp 'current-time-zone)
         (nth 1 (current-time-zone time)))
        ((getenv "TZ")
         ;; This can be utterly wrong, particular for posix timezone specs,
         ;; but about as correct as is worthwhile for supporting emacs 18.
         (substring (getenv "TZ") 0 3))))


;; Useful mail mode hacks

(defun fmailutils-fcc (file)
  "Add a new FCC field, with file name completion."
  (interactive (list (read-file-name "Folder carbon copy: "
                                     (or (and (boundp 'mail-folder-directory)
                                              mail-folder-directory)
                                         default-directory))))
  (fmailutils-put-unique-header "Fcc" file 'force-replace))

;(define-key mail-mode-map "\C-c\C-f\C-f" 'fmailutils-fcc)

(defun fmailutils-add-fcc-related-headers (&optional forcep replacep)
  "Add Date and Message-Id headers to messages saved via Fcc headers.
When called from lisp, this function only adds the Date and Message-Id
headers if an Fcc header is already present or the optional argument FORCEP
is non-nil.  If called interactively, the headers are added unconditionally.
The argument REPLACEP means replace any Date or Message-Id headers
which may already be present.

If you put this function on mail-send-hook, this will help document when
you sent the message as well as allow for proper threading when you visit
that folder with a mail reader that supports threads."
  (interactive)
  (fmailutils-save-state
    (cond ((or forcep
               (interactive-p)
               (fmailutils-position-on-field "Fcc" 'soft))
           (fmailutils-put-unique-header "Date"
                                         (fmailutils-rfc822-date)
                                         replacep)
           (fmailutils-put-unique-header "Message-Id"
                                         (fmailutils-make-message-id)
                                         replacep)))))

(defun fmailutils-set-from-address (&optional addr name style replyto)
  "Set the `From' header address of the current mail message.
If nil, ADDR defaults to the value of `user-mail-address'.
If nil, NAME defaults to the value of the `NAME' environment variable if
 defined, otherwise the value of the function `user-full-name'.
The argument STYLE chooses the style of address format to use.
It may be the symbol `<>' or nil.
If the value of REPLYTO is non-nil, set the `Reply-To' header as well.

After all else is done, the hook `fmailutils-set-from-address-hook' is run.

If the value of STYLE is the symbol `<>', then by default, the
address is formatted in the style

\tFrom: Noah Friedman <friedman@splode.com>

Otherwise, the style used is

\tFrom: friedman@splode.com \(Noah Friedman\)"
  (interactive "sHost or domain name:")
  (let* ((buffer-modified-state (buffer-modified-p))
         (fromstr (fmailutils-make-from-address addr name style)))
    (fmailutils-put-unique-header "From" fromstr 'replace)
    (and replyto
         (fmailutils-put-unique-header "Reply-To" fromstr 'replace))
    (run-hooks 'fmailutils-set-from-address-hook)
    (set-buffer-modified-p buffer-modified-state)))

(defun fmailutils-make-from-address (addr name style)
  (or addr
      (setq addr user-mail-address))
  (or (eq name 'none)
      (setq name (fmailutils-quote-address-comment
                  (or name (getenv "NAME") (user-full-name)))))
  (cond ((eq name 'none)
         addr)
        ((eq style '<>)
         (format "%s <%s>" name addr))
        (t
         (format "%s (%s)" addr name))))

;;;###autoload
(defun fmailutils-goto-next-header-or-tab (&optional count)
  "If in header area, go to beginning of next header.
If point is not in the header area, call tab-to-tab-stop.

With numeric prefix arg, skip forward that many headers.
If prefix arg is negative, skip backward that many headers.

If either the head or tail of the headers are reached, wrap around
to the other end and continue."
  (interactive "p")
  (let* ((headers-end
          (save-excursion
            (cond ((fboundp 'mail-text)
                   (mail-text)
                   (point))
                  (t
                   (goto-char (point-min))
                   (re-search-forward
                    (concat "^" (regexp-quote mail-header-separator) "$"))
                   (match-beginning 0)))))
         (forwardp (or (null count) (> count 0)))
         (fn (if forwardp 're-search-forward 're-search-backward))
         (la (if forwardp 2 1))
         (nla (if forwardp 1 2))
         (re "^[^:\n\t ]+:[ \t]?")
         (i (abs count)))
    (cond ((>= (point) headers-end)
           (tab-to-tab-stop))
          (t
           (save-restriction
             (narrow-to-region (point-min) headers-end)
             (while (not (zerop i))
               (cond
                ((funcall fn re nil t (if (looking-at re) la nla)))
                (t
                 (goto-char (point-min))
                 (funcall fn re nil t)))
               (setq i (1- i)))
             (goto-char (match-end 0)))))))

(provide 'fmailutils)

;;; fmailutils.el ends here
