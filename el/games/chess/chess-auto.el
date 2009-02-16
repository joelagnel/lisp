;;; -*-emacs-lisp-*-

(defun generate-autoloads ()
  (interactive)
  (require 'autoload)
  (setq generated-autoload-file (car command-line-args-left))
  (setq command-line-args-left (cdr command-line-args-left))
  (batch-update-autoloads))

;;; Generated autoloads follow (made by autoload.el).

;;;### (autoloads (chess-create-display chess) "chess" "chess.el"
;;;;;;  (16451 27143))
;;; Generated autoloads from chess.el

(autoload (quote chess) "chess" "\
Start a game of chess, playing against ENGINE (a module name)." t nil)

(defalias (quote chess-session) (quote chess))

(autoload (quote chess-create-display) "chess" "\
Create a display, letting the user's customization decide the style.
If MODULES-TOO is non-nil, also create and associate the modules
listed in `chess-default-modules'." nil nil)

;;;***

;;;### (autoloads (chess-link) "chess-link" "chess-link.el" (16384
;;;;;;  1382))
;;; Generated autoloads from chess-link.el

(autoload (quote chess-link) "chess-link" "\
Play out a game between two engines, and watch the progress.
If you want to run an engine as a bot, make the transport the first
engine, and the computer the second engine." t nil)

;;;***

;;;### (autoloads (chess-pgn-mode chess-pgn-read) "chess-pgn" "chess-pgn.el"
;;;;;;  (16422 22371))
;;; Generated autoloads from chess-pgn.el

(autoload (quote chess-pgn-read) "chess-pgn" "\
Read and display a PGN game after point." t nil)

(autoload (quote chess-pgn-mode) "chess-pgn" "\
A mode for editing chess PGN files." t nil)

(defalias (quote pgn-mode) (quote chess-pgn-mode))

(add-to-list (quote auto-mode-alist) (quote ("\\.pgn\\'" . chess-pgn-mode)))

;;;***

;;;### (autoloads (chess-puzzle) "chess-puzzle" "chess-puzzle.el"
;;;;;;  (16419 30370))
;;; Generated autoloads from chess-puzzle.el

(autoload (quote chess-puzzle) "chess-puzzle" "\
Pick a random puzzle from FILE, and solve it against the default engine.
The spacebar in the display buffer is bound to `chess-puzzle-next',
making it easy to go on to the next puzzle once you've solved one." t nil)

;;;***

;;;### (autoloads (chess-fischer-random-position) "chess-random"
;;;;;;  "chess-random.el" (16384 1383))
;;; Generated autoloads from chess-random.el

(autoload (quote chess-fischer-random-position) "chess-random" "\
Generate a Fischer Random style position." nil nil)

;;;***

;;;### (autoloads (chess-tutorial) "chess-tutorial" "chess-tutorial.el"
;;;;;;  (16415 62299))
;;; Generated autoloads from chess-tutorial.el

(autoload (quote chess-tutorial) "chess-tutorial" "\
A simple chess training display." t nil)

;;;***

;;;### (autoloads (chess-ics) "chess-ics" "chess-ics.el" (16450 29493))
;;; Generated autoloads from chess-ics.el

(autoload (quote chess-ics) "chess-ics" "\
Connect to an Internet Chess Server." t nil)

;;;***
