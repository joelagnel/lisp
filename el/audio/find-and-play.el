;;; Saved through ges-version 0.3.3dev at 2003-06-30 11:18
;;; From: marc0 <marc0@autistici.org>
;;; Subject: find-and-play.el
;;; Newsgroups: gnu.emacs.sources
;;; Date: Fri, 27 Jun 2003 21:16:49 GMT
;;; Organization: individual

;;; find-and-play.el  ---  Interface to find-and-play.

;; NO (C)  2003  marc0.

;; This software (find-and-play) is in the public domain, to be freely
;; used, shared, and modified.  There are (by intention) no legal
;; restraints on what you can do with it.

;; Filename: find-and-play.el
;; Version: 0.2.0 (alpha)
;; Updated: 2003-06-27
;; Keywords: music, player, mp3
;; Author: marc0 <marc0@autistici.org>
;; Maintainer: marc0 <marc0@autistici.org>
;; Description: interface to find-and-play (a music files searcher and player)
;; Language: Emacs Lisp
;; Compatibility: Emacs 21
;; Location: http://www.autistici.org/marc0/elisp/find-and-play.el
;; find-and-play location: http://www.autistici.org/marc0/scripts/find-and-play

;;; Code:

(setq find-and-play-default-directory "~/")

(defun m0-find-and-play (directory)
  "Find all the music files in a directory and play them."
  (shell-command (concat
		  "find-and-play "
		  (shell-quote-argument directory) " >& /dev/null & ")
		 (concat "*(find-and-play " directory ")*")))

(defun m0-launch-player-select-directory ()
  ""
  (interactive)
  (m0-find-and-play (expand-file-name
		     (read-file-name "Directory to play: "
				     find-and-play-default-directory))))

(define-key global-map (kbd "C-c l p d") 'm0-launch-player-select-directory)

;;;; find-and-play.el ends here.

;;; -- 
;;; marc0@autistici.org http://www.autistici.org/marc0
;;; (rot13-string "znep0@pelcgberoryf.arg")
;;; 2143 9E77 D5E6 115A 48AD  A170 D0EE F736 4E88 99C2

