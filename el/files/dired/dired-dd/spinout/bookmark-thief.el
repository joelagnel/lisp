;;; bookmark-thief.el: URL bookmark stealer from Netscape bookmarks.html

;; Authour: Seiichi Namba <sn@asahi-net.email.ne.jp>
;; Copyright (C) 1997,1998 Seiichi Namba <sn@asahi-net.email.ne.jp>

;; Started: Sun Dec 28 14:23:48 1997

;; This program's needs shell command defined by variable
;; url-bookmark-command, whose default value is
;; "~/bin/bmk.rip %s | sort"

;; Example of the `bmk.rip' script:
;;
;;#!/usr/bin/perl -nl
;;#require "jcode.pl";                      # for japanese user only
;;$sep=",,,,,";
;;
;;#  $code = &jcode'convert(*_, 'euc');     # for japanese user only
;;  if($_ =~ /HREF/) {
;;    s/^.*\s+HREF=//;
;;    s/\s* ADD_DATE=.*\"\>/$sep\"/;
;;    s/<\/A>/\"/;
;;    s/\"//g;
;;
;;    ($Fld1,$Fld2) = split($sep, $_, 9999);
;;    substr($Fld2, 39) = "" if (length($Fld2) > 39);
;;    printf "%-40s%s\n","$Fld2","$Fld1";
;;  }
;;
;; This `bmk.rip' just generates a data having format:
;;
;; DESCRIPTION    URL
;;
;; You can steal any URL bookmark file provided that your script
;; defined in variable url-bookmark-command generate data of that format.

;; Also set variable url-bookmark-bookmark to point at your
;; (Netscape's) bookmark file.  Current default value of
;; url-bookmark-bookmark is "~/.netscape/bookmarks.html".

;; To popup *url-bookmark* buffer in other frame,
;; but this disables url-bookmark-ange-ftp-url-at-point etc.
;;(setq special-display-regexps
;;      '("\\*Help" "\\*[Ss]hell" "\\*RMAIL*" "\\*memo\\*" "\\*diff"
;;	"*unit-table*"
;;	"\\*AddressBook"
;;	"\\*Buffer List"
;;	"\\*thesaurus"
;;	"\\*JEdic" "\\*EJdic"
;;	"\\*url-bookmark"))

;; Requires html-helper-mode (html-helper-mode-2.19.tar.gz or later).
(require 'browse-url)
(require 'dired-x)
(require 'url-to-ange)

(defvar url-bookmark-buf      "*url-bookmark*")
(defvar url-bookmark-bookmark "~/.netscape/bookmarks.html")
(defvar url-bookmark-command  "~/bin/bmk.rip %s | sort")
(defvar url-bookmark-command-line
	(format url-bookmark-command url-bookmark-bookmark))

(defvar url-bookmark-mode-map ())
(if url-bookmark-mode-map
    nil
  (progn
    (setq url-bookmark-mode-map (make-sparse-keymap))
    (define-key url-bookmark-mode-map [mouse-2] 'browse-url-at-mouse)
    (define-key url-bookmark-mode-map "\C-m"
      'url-bookmark-browse-url-at-point)
    (define-key url-bookmark-mode-map [M-S-mouse-2]
      'url-bookmark-ange-ftp-url-at-mouse)
    (define-key url-bookmark-mode-map "d" 'url-bookmark-ange-ftp-url-at-point)
    (define-key url-bookmark-mode-map "n" 'url-bookmark-foward)
    (define-key url-bookmark-mode-map " " 'scroll-up)
    (define-key url-bookmark-mode-map "p" 'url-bookmark-backward)
    (define-key url-bookmark-mode-map "g" 'revert-url-bookmark)
    (define-key url-bookmark-mode-map "\177" 'scroll-down)
    ))

(fset 'revert-url-bookmark 'raise-url-bookmark)
(defun raise-url-bookmark ()
 "Raise users' bookmark file and set major mode to url-bookmark-mode.
URL Bookmark file is specifed with variable url-bookmark-bookmark, whose
default value is \"~/bookmarks.html\".

Normally, user must setup bookmark processing commands in two variables
`url-bookmark-command' and `url-bookmark-command-line', whose default
setting is:

  (defvar url-bookmark-command  \"~/bin/bmk.rip %s | sort\")
  (defvar url-bookmark-command-line
	  (format url-bookmark-command url-bookmark-bookmark))

bmk.rip is a perl script which is included at the bottom of
bookmark-thief.el.

See also `url-bookmark-mode'."
  (interactive)
  (get-buffer-create url-bookmark-buf)
  (save-excursion
    (set-buffer url-bookmark-buf)
    (setq buffer-read-only nil)
    (erase-buffer)
;; I don't know this is working at all.  (featurep 'mule) test needed.
;;    (if (>= (string-to-int emacs-version) 20)
;;	(find-operation-coding-system 'call-process "euc-japan" "euc-japan" ))
    (shell-command url-bookmark-command-line t)
    (goto-char (point-min))
    (while (search-forward-regexp
	    "^.+\\(http:\\|ftp:\\|file:\\).*$" (point-max) t)
      (put-text-property (match-beginning 1) (point)
			 'mouse-face 'highlight))
    (goto-char (point-min))
    (setq buffer-modified-p nil
	  buffer-read-only t)
    (url-bookmark-mode))
  (pop-to-buffer url-bookmark-buf))

(defun url-bookmark-mode ()
  "\

Use mouse command on mouse-face'ed area to call for WWW browser.
    \\[browse-url-at-mouse]
or keyboard command
    \\[url-bookmark-browse-url-at-point]
Other Keybindings:

\\{url-bookmark-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map url-bookmark-mode-map)
  (setq major-mode 'url-bookmark-mode)
  (setq mode-name "Url-Bookmark")
  (run-hooks 'url-bookmark-mode-hook))

(defun url-bookmark-browse-url-at-point ()
  "Call for WWW browser passing url expression contained in current line."
  (interactive)
  (url-bookmark-goto-url-pos)
  (call-interactively 'browse-url-at-point))

(defun url-bookmark-ange-ftp-url-at-mouse (ev)
  "Call for ange-ftp-dired passing url expression contained in current line."
  (interactive "e")
  (mouse-set-point ev)
  (url-bookmark-ange-ftp-url-at-point1))

(defun url-bookmark-ange-ftp-url-at-point ()
  "Call for ange-ftp-dired passing url expression contained in current line."
  (interactive)
  (url-bookmark-goto-url-pos)
  (url-bookmark-ange-ftp-url-at-point1))

(defun url-bookmark-ange-ftp-url-at-point1 ()
  (let ((bol (save-excursion (beginning-of-line nil) (point)))
	(eol (save-excursion (end-of-line nil) (point)))
	(buf (current-buffer)))
    (setq buffer-read-only nil)
    (remove-text-properties bol eol
			    '(mouse-face))
    (dired-x-find-file (dired-filename-at-point))
    (save-excursion
      (set-buffer buf)
      (beginning-of-line nil)
      (search-forward-regexp
       "^.+\\(http:\\|ftp:\\|file:\\).*$" (point-max) (1+ eol))
      (put-text-property (match-beginning 1) (point)
			 'mouse-face 'highlight)
      (setq buffer-read-only t)
      (url-bookmark-goto-url-pos))))

(defun url-bookmark-foward (n)
  "Goto next url position (in next line)."
  (interactive "p")
  (next-line n)
  (url-bookmark-goto-url-pos))

(defun url-bookmark-backward (n)
  "Goto previous url position (in previous line)."
  (interactive "p")
  (next-line (- n))
  (url-bookmark-goto-url-pos))

(defun url-bookmark-goto-url-pos ()
  (beginning-of-line nil)
  (if
      (search-forward-regexp "[ \t]+\\(http:\\|ftp:\\|file:\\)"
			     (save-excursion (end-of-line nil)
					     (point))
			     t 1)
      ;;(backward-word 1)
      (goto-char (match-beginning 1))
    (end-of-line nil)))

(provide 'bookmark-thief)		; run-hooks after provide,
(run-hooks 'bookmark-thief-load-hook)	; to suppress infinite loop.
