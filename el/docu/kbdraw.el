;;; kbdraw.el -- generates html to render a keyboard layout

;; Copyright (C) 2000,2002 Neil W. Van Dyke

;; Author:   Neil W. Van Dyke <neil@neilvandyke.org>
;; Version:  0.2
;; X-URL:    http://www.neilvandyke.org/kbdraw/
;; X-CVS:    $Id: kbdraw.el,v 1.17 2002/10/16 00:49:32 nwv Exp $ GMT

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.  This
;; is distributed in the hope that it will be useful, but without any warranty;
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose.  See the GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License along with
;; Emacs; see the file `COPYING'.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.")

;;; Commentary:

;; `kbdraw.el' takes a specification of a keyboard layout as an Emacs Lisp
;; s-expression and generates HTML code to render the layout in a Web browser.
;;
;; This is something I hacked up in a few hours to help me in designing
;; keyboard layouts and showing the layouts to other people via the Web.
;;
;; You can either include the HTML in your Web pages, or take a screenshot of
;; the HTML as viewed in your own Web browser and publish it in PNG/JPEG/GIF
;; form.
;;
;; For a demo, do M-x kbdraw-sample RET.  The sample keyboard defined in
;; `kbdraw-sample-1' will be displayed in your Web browser via `browse-url'.
;;
;; This package requires a little bit of Emacs Lisp knowledge on the part of
;; the user.  The following rough grammar spec might help.  See
;; `kbdraw-sample-1' for an example.
;;
;;     BOARD ::= { BOARD-ELEMENT }+
;;
;;     BOARD-ELEMENT ::= KEY-WIDTH-DEFAULT | ROW | VSPACE
;;
;;     KEY-WIDTH-DEFAULT ::= (key-width-default WIDTH)
;;     ROW ::= (row { ROW-ELEMENT }+ )
;;     VSPACE ::= (vspace)
;;
;;     ROW-ELEMENT ::= KEY | HSPACE
;;
;;     KEY ::= KEYNAME | (KEYNAME &optional SHIFTED-KEYNAME WIDTH)
;;     KEYNAME ::= STRING
;;     SHIFTED-KEYNAME ::= KEYNAME | nil
;;     WIDTH ::= INTEGER
;;    
;;     HSPACE ::= (hspace &optional WIDTH)
;;
;; One way you *may* wish to use this package is to have a file such as
;; `my-keyboard.el' that defines the keyboard that will be rendered to
;; `my-keyboard.html'.  Then you can eval the buffer or Lisp form to update the
;; HTML file.
;;
;;     (require 'kbdraw)
;;
;;     (let ((kbdraw-board-color "black")
;;           (kbdraw-key-color   "white")
;;           (kbdraw-label-color "#f00000"))
;;
;;       (kbdraw-preview "my-keyboard.html"
;;
;;                       '((key-width-default 4)
;;                         (row "7" "8" "9")
;;                         (row "4" "5" "6")
;;                         (row "1" "2" "3")
;;                         (row ("0" nil 8) "Enter"))))
;;
;; Or just bang around, er, *rapid prototype* in the `*scratch*' buffer:
;;
;;     (kbdraw-preview "foo.html" '((row "A" "B" "See")))
;;
;; Please note that this was a quick&dirty package, and I'm no longer using or
;; maintaining it.

;;; Change Log:

;; [Version 0.2, 15-Oct-2002] Updated email address.  Converted element and
;; attribute names to lowercase, to be slightly closer to XHTML, so long as we
;; were posting a new version.
;;
;; [Version 0.1, 10-Nov-2000] Hacked up and spat out.

;;; Code:

(defvar kbdraw-sample-1
  '((key-width-default 4)
    (row "Esc" (hspace)
         "F1" "F2"  "F3"  "F4"  (hspace 2)
         "F5" "F6"  "F7"  "F8"  (hspace 2)
         "F9" "F10" "F11" "F12")
    (vspace)
    (row ("`" "~")
         ("1" "!") ("2" "@") ("3" "#") ("4" "$") ("5" "%") 
         ("6" "^") ("7" "&") ("8" "*") ("9" "(") ("0" ")")
         ("-" "_") ("=" "+") ("\\" "|") ("Back"))
    (row ("Tab" nil 6)
         "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P"
         ("[" "{") ("]" "}") ("" nil 6))
    (row ("Capslock" nil 7)
         "A" "S" "D" "F" "G" "H" "J" "K" "L"
         (";" ":") ("'" "\"") ("Enter" nil 9))
    (row ("Shift" nil 9)
         "Z" "X" "C" "V" "B" "N" "M"
         ("," "<") ("." ">") ("/" "?") ("Shift" nil 7) ("Special"))
    (row ("Ctrl" nil 5) ("Logo" nil 5) ("Alt" nil 5)
         ("Space" nil 25)
         ("Alt" nil 5) ("Logo" nil 5) ("Menu" nil 5) ("Ctrl" nil 5))))

(defvar kbdraw-board-color       "#909090")
(defvar kbdraw-board-padding     1)
(defvar kbdraw-board-spacing     0)
(defvar kbdraw-key-color         "#f0f0f0")
(defvar kbdraw-key-padding       1)
(defvar kbdraw-key-spacing       1)
(defvar kbdraw-key-width-default 4)
(defvar kbdraw-label-color       "#000000")
(defvar kbdraw-plain-p           nil)

(defun kbdraw-sample ()
  (interactive)
  (message "kbdraw-sample...")
  (kbdraw-preview "~/kbdraw-sample.html"
                  kbdraw-sample-1
                  "<body bgcolor=\"#ffffff\">\n"
                  "\n</body>\n")
  (message "kbdraw-sample...done"))

(defun kbdraw-to-file (filename keyboard &optional pre-html post-html)
  (save-excursion
    (set-buffer (find-file-noselect filename))
    (delete-region (point-min) (point-max))
    (when pre-html (insert pre-html))
    (insert (kbdraw-html-format keyboard))
    (when post-html (insert post-html))
    (message "kbdraw (saving)...")
    (save-buffer)))

(defun kbdraw-preview (filename keyboard &optional pre-html post-html)
  (message "kbdraw-preview (generating)...")
  (kbdraw-to-file filename keyboard pre-html post-html)
  (message "kbdraw (viewing)...")
  (require 'browse-url)
  (browse-url (browse-url-file-url (expand-file-name filename)))
  (message "kbdraw-preview...done"))

(defun kbdraw-html-format (pad-elements)
  (let ((kbdraw-key-width-default kbdraw-key-width-default))
    ;; Note: We are copying `kbdraw-key-width-default' via the questionable
    ;; magic of dynamic scoping so that we can be lazy about the keyboard spec
    ;; overriding the value for the duration of this function call.
    (concat 
     (if kbdraw-plain-p
         "<table border=\"1\">\n"
       (concat
        "<table border=\"0\" bgcolor=\""
        kbdraw-board-color
        "\" cellpadding=\""
        (number-to-string kbdraw-board-padding)
        "\" cellspacing=\""
        (number-to-string kbdraw-board-spacing)
        "\"><tr><td>"
        "<table border=\"0\" bgcolor=\""
        kbdraw-key-color
        "\" cellpadding=\""
        (number-to-string kbdraw-key-padding)
        "\" cellspacing=\""
        (number-to-string kbdraw-key-spacing)
        "\">\n"))
     (mapconcat (function
                 (lambda (element)
                   (unless (and (listp element)
                                (> (length element) 0)
                                (memq (car element)
                                      '(key-width-default row vspace)))
                     (error "Expected (row ...) or (vspace): %s" 
                            (prin1-to-string element)))
                   (cond ((eq (car element) 'key-width-default)
                          (setq kbdraw-key-width-default (nth 1 element))
                          nil)
                         ((eq (car element) 'row)
                          (kbdraw-html-format-row (cdr element)))
                         ((eq (car element) 'vspace)
                          (kbdraw-html-format-vspace))
                         (t (error)))))
                pad-elements
                "")
     (if kbdraw-plain-p
         "</table>"
       "</table></td></tr></table>"))))
  
(defun kbdraw-html-format-row (row-elements)
  (concat "<tr>\n"
          (mapconcat 'kbdraw-html-format-key row-elements "")
          "</tr>\n"))

(defun kbdraw-html-format-vspace ()
  (concat "<tr>\n"
          (if kbdraw-plain-p
              (concat "<td><font size=\"-3\"></font></td>\n")
            (concat "<td bgcolor=\""
                    kbdraw-board-color
                    "\"><font size=\"-3\">&nbsp;</font></td>\n"))
          "</tr>\n"))

(defun kbdraw-html-format-key (keyspec)
  (if (stringp keyspec)
      (kbdraw-html-format-key-parsed keyspec nil nil)
    (if (listp keyspec)
        (if (= (length keyspec) 0)
            (error)
          (if (eq (car keyspec) 'hspace)
              (apply 'kbdraw-html-format-key-parsed nil nil (cdr keyspec))
            (apply 'kbdraw-html-format-key-parsed keyspec))))))

(defun kbdraw-html-format-key-parsed (keyname &optional shifted-keyname width)
  (let* ((implicit-width (or width kbdraw-key-width-default)))
    (concat "<td align=\"center\" valign=\"middle\""
            (if (/= implicit-width 1)
                (concat " colspan=\""
                        (format "%d" implicit-width)
                        "\"")
              "")
            ">"
            (if shifted-keyname
                (concat (kbdraw-html-format-keyname shifted-keyname
                                                    implicit-width)
                        "<br>")
              "")
            (if keyname
                (kbdraw-html-format-keyname keyname implicit-width)
              "")
            (if (and keyname (not shifted-keyname))
                "<br>&nbsp;"
              "")
            "</td>\n")))

(defun kbdraw-html-format-keyname (keyname width)
  ;; Note: `width' confuses the key layout grid measures with the keyname width
  ;; measures, but it's a close enough kludge for our purposes -- we just want
  ;; to tend to add a little horizontal padding to keynames.
  (unless keyname (error))
  (let* ((keyname-len (length keyname))
         (fontsize    (if (> keyname-len 1)
                          (- (ceiling (/ (float keyname-len)
                                         (float width))))))
         (font-tag-p  (or fontsize
                          (and (not kbdraw-plain-p) kbdraw-label-color))))
    (concat (if font-tag-p
                (concat "<font"
                        (if fontsize (format " size=\"%d\""  fontsize) "")
                        (if (and (not kbdraw-plain-p) kbdraw-label-color)
                            (format " color=\"%s\"" kbdraw-label-color)
                          "")
                        ">")
              "")
            (kbdraw-html-quote
             (cond ((>= keyname-len width) keyname)
                   ((= keyname-len 0)      (make-string width 32))
                   (t (let ((left (max 0 (ceiling (/ (- width keyname-len)
                                                     2.0)))))
                        (concat (make-string left 32)
                                keyname
                                (make-string left 32))))))
            (if font-tag-p "</font>" ""))))

(defun kbdraw-html-quote (str)
  (save-match-data
    (let* ((str-length (length str))
           (start      nil)
           (substrings '())
           (specials    " \"&<>")
           (pattern    (concat "\\([^" specials "]*\\)"
                               "\\(["  specials "]\\)?")))
      (while (and (or (not start) (< start str-length))
                  (string-match pattern str start))
        (setq start (match-end 0))
        (when (match-beginning 1)
          (setq substrings (nconc substrings (list (match-string 1 str)))))
        (when (match-beginning 2)
          (let ((n (match-string 2 str)))
            (setq substrings
                  (nconc substrings
                         (list (cdr (assoc (match-string 2 str)
                                           '((" "  . "&nbsp;")
                                             ("\"" . "&quot;")
                                             ("&"  . "&amp;")
                                             ("<"  . "&lt;")
                                             (">"  . "&gt;"))))))))))
      (or (apply 'concat substrings) ""))))

(provide 'kbdraw)

;;; kbdraw.el ends here
