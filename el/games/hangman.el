;;; hangman.el --- Hangman game

;;; Copyright (C) 1997  Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <zappo@gnu.ai.mit.edu>
;; Version: 0.1
;; Keywords: games
;; X-RCS: $Id: hangman.el,v 1.3 1997/09/12 22:07:31 zappo Exp $
;;
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
;; program's author (see below) or write to:
;;
;;              The Free Software Foundation, Inc.
;;              675 Mass Ave.
;;              Cambridge, MA 02139, USA.
;;
;; Please send bug reports, etc. to zappo@gnu.ai.mit.edu.
;;
;; Future versions of checkdoc will appear at:
;;   ftp://ftp.ultranet.com/pub/zappo/hangman-*.el
;;

;;; Commentary:
;;
;; Allows user to play hangman iff /usr/dict/words or compatible file
;; is installed.

;;; History:
;;; 0.1  Initial revision

;;; Code:

(defvar hm-user-proper-nouns-flag t
  "*Non-nil means to allow proper nouns as potential words.")

(defvar hm-shortest-word 4
  "*The sortest word allowed for a potential word.")

(defvar hm-dictionary-file "/usr/dict/words"
  "The file where a list of words is stored.")

(defvar hm-hooks nil
  "Hooks run when entering hangman mode.")

(defvar hm-map nil
  "Keymap used in hangman mode.")

(if hm-map
    nil
  (setq hm-map (make-keymap "Hangman"))
  (let ((i ?a))
    (while (not (char-equal ?Z i))
      (define-key hm-map (char-to-string i) 'hm-self-guess-char)
      (setq i (1+ i)))))

;;; Game Mode
;;
(defalias 'hangman 'hm-mode)

(defun hm-mode ()
  "Major mode for playing the hangman game against emacs."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Hangman*"))
  (kill-all-local-variables)
  (setq major-mode 'hm-mode
	mode-name "HangMan")
  (use-local-map hm-map)
  (hm-initialize)
  (run-hooks 'hm-hooks))

;;; Game playing functions and variables
;;
(defvar hm-current-word nil
  "This is not the word the user must guess represented as a vector.")

(defvar hm-current-guess-string nil
  "The string representing what the user has guessed correctly so far.")

(defvar hm-wrong-guess-string nil
  "The letters guessed so far.")

(defvar hm-num-failed-guesses nil
  "The number of errors so far.")

(defvar hm-win-statistics [ 0 0 ]
  "The number of won and lost games since emacs bootup.")

(defvar hm-vector
  [ "+---------+\n|         |\n|           \n|          \n|           \n\
|          \n|           \n|            \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|           \n\
|          \n|           \n|            \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|         | \n\
|         |\n|           \n|            \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|         |\\\n\
|         |\n|           \n|            \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|        /|\\\n\
|         |\n|           \n|            \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|        /|\\\n\
|         |\n|        /  \n|       /    \n|"
    "+---------+\n|         |\n|        ( )\n|         +\n|        /|\\\n\
|         |\n|        / \\\n|       /   \\\n|"]
  "Vector of hangman states.")

(defmacro hm-with-writable (&rest forms)
  "Allow the buffer to be writable and evaluate FORMS.
Turn read only back on when done."
  (list 'let '((hm-with-writable-buff (current-buffer)))
	'(toggle-read-only -1)
	(cons 'progn forms)
	'(save-excursion (set-buffer hm-with-writable-buff)
			 (toggle-read-only 1))))
(put 'hm-with-writable 'lisp-indent-function 0)

(defun hm-initialize ()
  "Initialize this buffer w/ a new word."
  (interactive)
  (set (make-local-variable 'hm-current-word) (hm-fetch-random-word))
  (set (make-local-variable 'hm-current-guess-string)
       (hm-make-guess-string hm-current-word))
  (set (make-local-variable 'hm-num-failed-guesses) 0)
  (set (make-local-variable 'hm-wrong-guess-string) "")
  (toggle-read-only 1)
  (hm-refresh)
  t)

(defun hm-self-guess-char ()
  "Guess the character that was pressed."
  (interactive)
  (if (hm-win)
      nil
    (let ((c last-input-char) (i 0) (found 0)
	  (case-fold-search nil))
      (hm-already-guessed c)
      (while (< i (length hm-current-word))
	(if (char-equal (aref hm-current-word i) c)
	    (progn
	      (setq found (1+ found))
	      (aset hm-current-guess-string (* i 2) (upcase c))
	      (hm-fontify-char hm-current-guess-string (* 2 i)
			       (if (facep 'font-lock-function-name-face)
				   'font-lock-function-name-face
				 'bold))
	      ))
	(setq i (1+ i)))
      (if (/= found 0)
	  (message "Found %d occurances of %c" found c)
	(message "No uccurances of %c" c)
	(setq hm-num-failed-guesses (1+ hm-num-failed-guesses)
	      hm-wrong-guess-string (concat hm-wrong-guess-string " "
					    (char-to-string (upcase c))))))
    (hm-refresh)
    (hm-win t)))

(defun hm-already-guessed (c)
  "Signal an error if character C has already been played."
  (let ((case-fold-search t) (re (char-to-string c)))
    (if (or (string-match re hm-wrong-guess-string)
	    (string-match re hm-current-guess-string))
	(error "You have already guessed %c" c))))

(defun hm-win (&optional dostats)
  "Do the right thing if the game has been won.
Optional argument DOSTATS will update the statistics if set."
  (let ((case-fold-search nil))
    (if (string-match "[a-z_] " hm-current-guess-string)
	(if (= hm-num-failed-guesses (1- (length hm-vector)))
	    (progn
	      (if dostats
		  (aset hm-win-statistics 1 (1+ (aref hm-win-statistics 1))))
	      (setq hm-current-guess-string
		    (hm-make-guess-string hm-current-word
					  hm-current-guess-string))
	      (hm-refresh)
	      (if (y-or-n-p "You lost! Play again?")
		  (hm-initialize)
		t)))
      (if dostats
	  (aset hm-win-statistics 0 (1+ (aref hm-win-statistics 0))))
      (hm-refresh)
      (if (y-or-n-p "You won!  Play again?")
	  (hm-initialize))
      t)))

;;; Rendering
;;
(defun hm-refresh ()
  "Refresh the hangman buffer w/ new images."
  (hm-with-writable
    (erase-buffer)
    (insert (aref hm-vector hm-num-failed-guesses))
    (goto-char (point-min))
    (forward-line 2)
    (end-of-line)
    (insert "         Failed Letters: " hm-wrong-guess-string)
    (forward-line 2)
    (end-of-line)
    (insert "            " hm-current-guess-string)
    (forward-line 2)
    (end-of-line)
    (insert (format "         Games won: %d    Games Lost: %d"
		    (aref hm-win-statistics 0) (aref hm-win-statistics 1)))))

;;; Text Properties
;;
(defun hm-fontify-char (string idx face)
  "Fontify one character in STRING at position IDX with FACE."
  (if (fboundp 'put-text-property)
      (put-text-property  idx (1+ idx) 'face face string)))

;;; Word Retrieval
;;
(defun hm-make-guess-string (string &optional finish)
  "Return a string representing a new guess string based on STRING.
Optional argument FINISH non-nil means to not replace characters with _."
  (let ((ns "") (i 0))
    (while (< i (length string))
      (cond ((and (>= (aref string i) ?A) (<= (aref string i) ?z))
	     (if finish
		 (if (char-equal (aref finish (* 2 i)) ?_)
		     (progn
		       (aset finish (* 2 i) (aref string i))
		       (hm-fontify-char finish (* 2 i)
					(if (facep 'font-lock-comment-face)
					    'font-lock-comment-face
					  'underline))))
	       (setq ns (concat ns "_ "))))
	    (t
	     (setq ns (concat ns (aref string i) " "))))
      (setq i (1+ i)))
    (or finish ns)))

(defun hm-fetch-random-word ()
  "Return a random word that will match the options applied by the user."
  (let ((word (hm-fetch-one-random-word))
	(case-fold-search nil))
    (while (or (string-match "^[A-Z]" word)
	       (< (length word) hm-shortest-word))
      (setq word (hm-fetch-one-random-word)))
    word))

(defun hm-fetch-one-random-word ()
  "Return a word from the system dictionary."
  (save-excursion
    (set-buffer (find-file-noselect hm-dictionary-file))
    (goto-char (random (point-max)))
    (beginning-of-line)
    (prog1
	(buffer-substring-no-properties (point) (progn (end-of-line) (point)))
      (goto-char (point-min)))))

(provide 'hangman)
;;; hangman.el ends here
