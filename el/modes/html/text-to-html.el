;;; text-to-html.el  ---  Plain-text to HTML converter.

;; Copyright (C)  2003  Marco Parrone.

;; Filename: text-to-html.el
;; Version: 0.1.1 (alpha)
;; Updated: 2003-07-18
;; Keywords: text2html, plain-text, html, converter
;; Author: Marco Parrone <marc0@autistici.org>
;; Maintainer: Marco Parrone <marc0@autistici.org>
;; Description: Plain-text to HTML converter.
;; Language: Emacs Lisp
;; Compatibility: Emacs 21
;; Location: http://savannah.nongnu.org/cgi-bin/viewcvs/*checkout*/emhacks/emhacks/elisp/text-to-html.el?rev=HEAD&content-type=text/plain

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

;;; Commentary:

;; text-to-html.el is a script to convert a portion of text from
;; plain-text to HTML.
;;
;; The conversion consists in matching the URLs and replacing them
;; with HTML links.
;;
;; Another conversion is to start a paragraph where there are two
;; consecutive newlines without non-white characters in the middle.
;;
;; The rest is to add the tag `<br>' to newlines, and to make words
;; tagged for *bold* in bold, and the ones tagged for being
;; _underlined_ will be made underlined.
;;
;; In addition, using `text-to-html-on-buffer-with-template', two
;; template snip of HTML will be added, one at the top of the page and
;; one at the bottom, to make a valid HTML page.
;;
;; This is for texts that were not intended to be an HTML page, so it
;; does not supports special markups.
;;
;; The code is very generalized, and using customization the program
;; can be used to do any number of replacements using regular
;; expressions, and the top and bottom templates can be changed too,
;; so it's possible to use the program for conversions not related to
;; the text-to-html one.

;;; Code:

(defgroup text-to-html nil
  "Plain-text to HTML converter."
  :prefix "text-to-html-"
  :group 'editing)

(defcustom text-to-html-top-template
  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
          \"http://www.w3.org/TR/html4/strict.dtd\">
<html>\n<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\">
<meta name=\"generator\" content=\"text2htmlm0\">
<title>text-to-html output</title>\n</head>\n<body>\n"
  "*Template to add at the top of the buffer, used by
`text-to-html-on-buffer-with-template'."
  :type '(string)
  :group 'text-to-html)

(defcustom text-to-html-bottom-template "</body>\n</html>\n"
  "*Template to add at the bottom of the buffer, used by
`text-to-html-on-buffer-with-template'."
  :type '(string)
  :group 'text-to-html)

(defcustom text-to-html-replacements-table
  '(("\\(\\(http\\|ftp\\|https\\|ftps\\|mailto\\|gopher\\)://\\([[:alnum:]]\\|\\.\\|_\\|-\\|/\\|\\?\\|=\\|~\\|%\\|\\)*\\)" . "<a href=\"\\1\">\\1</a>")
    ("\n[ \t\n]*\n" . "\n<p>")
    ("\n" . "<br>\n")
    ("\\*\\([^ \t\n]*\\)\\*" . "<b>\\1</b>")
    ("_\\([^ \t\n]*\\)_" . "<u>\\1</u>"))
  "*Matching and replacing rules (list of pairs of strings, use regular
expressions)."
  :type '(sexp)
  :group 'text-to-html)

(defun text-to-html-on-region (start end)
  "Convert the plain-text in a region to HTML format."
  (interactive "r")
  (dolist (replacement-description text-to-html-replacements-table)
    (goto-char (if start start 0))
    (while 
	(re-search-forward (car replacement-description) nil t)
      (replace-match (cdr replacement-description)))))

(defun text-to-html-on-buffer ()
  "Convert the plain-text in the buffer to HTML format."
  (interactive)
  (goto-char 0)
  (text-to-html-on-region nil nil))

(defun text-to-html-on-buffer-with-template ()
  "Convert the plain-text in the buffer to HTML format,
adding some template HTML markup at the top and at the bottom
of the page, so to make the buffer a valid HTML page."
  (interactive)
  (goto-char 0)
  (text-to-html-on-region nil nil)
  (goto-char 0)
  (insert text-to-html-top-template)
  (goto-char (point-max))
  (insert text-to-html-bottom-template))

;; Local Variables:
;; mode: emacs-lisp
;; mode: auto-fill
;; fill-column: 70
;; comment-column: 32
;; End:

;;;; text-to-html.el ends here.
