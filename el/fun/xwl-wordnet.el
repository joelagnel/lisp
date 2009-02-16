;;; xwl-wordnet.el --- an interface for Word Net

;; Copyright (C) 2004 William XWL

;; Author: William XWL <william.xwl@gmail.com>
;; Maintainer: William XWL <william.xwl@gmail.com>
;; Created: 2004/10/21 19:44:23
;; Version: v 1.0
;; Keywords: convenience
;; Last updated: 2005/07/18 00:43:06

;; This file is not part of GNU Emacs.

;;{{{ GPL

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}

;;; Commentary:

;; A simple interface for the dictionary - Word Net

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'wx-wordnet)

;; Recommended key binding:
;;   (global-set-key (kbd "M-s") 'wordnet-search)

;;{{{ History

;; wx-wordnet.el is an interface for the dictionary -- Word Net, which's
;; very wonderful! Although there is already a dictionary.el, which is used
;; as an interface for dictd, the dictionary.el has not made full use of
;; Word Net! It only uses wordnet's "-overview" option. The other options
;; like "-syns, -hypo, ants, ..." are all ignored!

;; I've also found "Thomas Link AKA samul AT web DOT de" 's wordnet.el
;; before i wrote this, but it won't work, which kept on complaining some
;; functions not found. It was based on XEmacs, but i'm using GNU Emacs.
;; Having tried to hack wordnet.el for a while, i gave up. :( And I wrote
;; this simple one myself.

;;}}}

;;; Change Log:

;;; Code:

;;; Variables:

(defvar wordnet-options
  "-antsn -antsv -antsa -antsr\
	-hypen -hypev\
	-hypon -hypov\
	-entav\
	-synsn -synsv -synsa -synsr\
	-smemn\
	-ssubn\
	-sprtn\
	-membn\
	-subsn\
	-partn\
	-meron\
	-holon\
	-causv\
	-perta -pertr\
	-attrn -attra\
	-derin -deriv\
	-domnn -domnv -domna -domnr\
	-domtn -domtv -domta -domtr\
	-famln -famlv -famla -famlr\
	-framv\
	-coorn -coorv\
	-simsv\
	-hmern\
	-hholn\
	-grepn -grepv -grepa -grepr\
	-over")

;;; Functions:

(defun wordnet-quit ()
  "Bury Word Net buffer."
  (interactive)
  (delete-windows-on (buffer-name)))

(defun wordnet-next-sense ()
  "Goto next Sense."
  (interactive)
  (end-of-line)
  (search-forward-regexp "^Sense\\ [0-9]\\|^[0-9]" nil t)
  (beginning-of-line))

(defun wordnet-prev-sense ()
  "Goto previous Sense."
  (interactive)
  (beginning-of-line)
  (search-backward-regexp "^Sense\\ [0-9]\\|^[0-9]" nil t)
  (beginning-of-line))

(defun wordnet-antonyms ()
  "Goto -ants{n|v|a|r}."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^Antonyms\\ of" nil t)
  (beginning-of-line))

(defun wordnet-synonyms ()
  "Goto -syns{n|v|a|r}."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^Synonyms\\ of\\|^Synonyms/Hypernyms\\|Similarity\\ of" nil t)
  (beginning-of-line))

(defun wordnet-hyponyms ()
  "Goto -hypo{n|v}, -tree{n|v}."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^Hyponyms\\ of" nil t)
  (beginning-of-line))

(defun wordnet-overview ()
  "Goto -overview."
  (interactive)
  (goto-char (point-min))
  (search-forward-regexp "^Overview\\ of" nil t)
  (beginning-of-line))


(define-derived-mode wordnet-search-mode fundamental-mode
  "Word Net"
  "A major mode for Word Net dictionary search.

\\{wordnet-search-mode-map}"
  (define-key wordnet-search-mode-map (kbd "q") 'wordnet-quit)
  (define-key wordnet-search-mode-map (kbd "n") 'wordnet-next-sense)
  (define-key wordnet-search-mode-map (kbd "p") 'wordnet-prev-sense)
  (define-key wordnet-search-mode-map (kbd "a") 'wordnet-antonyms)
  (define-key wordnet-search-mode-map (kbd "s") 'wordnet-synonyms)
  (define-key wordnet-search-mode-map (kbd "h") 'wordnet-hyponyms)
  (define-key wordnet-search-mode-map (kbd "o") 'wordnet-overview))

(defun wordnet-search (word)
  "Search the `word' with Word Net if given. It presents the word at point
as default input and allows editing it."
  (interactive
   (list (read-string "wn: " (current-word))))
  (unless word
    (setq word (read-string "wn: ")))

  ;; make sure whether we are in *Word Net* buffer.
  (let ((wn nil))
    (if (eq major-mode 'wordnet-search-mode)
	(setq wn t))
    (let ((buf (get-buffer-create "*Word Net*")))
      (with-current-buffer buf
	(progn
	  (wordnet-search-mode)
	  (shell-command (concat "wordnet " word " " wordnet-options) buf)
	  (setq buffer-read-only t)
	  (unless wn
	    (other-window 1)))))))

(provide 'xwl-wordnet)

;;; xwl-wordnet.el ends here
