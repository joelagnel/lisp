;;;; embedded-commands.el
;;; Time-stamp: <2006-03-24 13:02:13 jcgs>

;;  This program is free software; you can redistribute it and/or modify it
;;  under the terms of the GNU General Public License as published by the
;;  Free Software Foundation; either version 2 of the License, or (at your
;;  option) any later version.

;;  This program is distributed in the hope that it will be useful, but
;;  WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;  General Public License for more details.

;;  You should have received a copy of the GNU General Public License along
;;  with this program; if not, write to the Free Software Foundation, Inc.,
;;  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA

;;; This is to handle words apparently entered as embedded text but
;;; actually representing a stream of commands. This is meant for use
;;; with voice input; it would seem a little strange to me to use it
;;; with typed input.

(provide 'embedded-commands)

(require 'cl)
(require 'command-phrases)

(defvar embedded-commands-mode-status-words-string ""
  "Status string for embedded commands mode.")

(defvar embedded-commands-mode-status-flag-string "\" "
  "Status string for embedded commands mode.")

(defvar embedded-commands-mode nil
  "Whether embedded-commands mode is active.")

(pushnew '(embedded-commands-mode (" Embedded \"" 
				   embedded-commands-mode-status-words-string
				   embedded-commands-mode-status-flag-string))
	  minor-mode-alist :test 'equal)

;; (defvar embedded-command-root-possibilities nil
;;   "The possible roots for finding command word sequences.")

(defvar embedded-command-possibilities nil
  "The current possibities in finding command word sequences.")

;; it would be silly to have part of a command in one buffer, and part in another...
;; also stops stuff that goes into the echo area disturbing the command recognition
;; that we want
(make-variable-buffer-local 'embedded-command-possibilities)

(defvar noise-words '("of" "the" "with")
  "*Words we ignore.
This should be set to the words the speech system sometimes adds to what you really said.")

(defvar reset-words '("reset" "restart")
  "*Words that tell the parser to give up on a partially parsed phrase.")

(defvar help-words '("help" "choices" "possibilities")
  "*Words that trigger a help display.")

(defvar forget-it-words '("dictation")
  "*Words that say that a partially recognized command is to be treated as text.")

(defun embedded-copy-face (old-face new-face &optional frame new-frame)
  "Safe caller for copy-face, which does not exist on TTY-only emacsen."
  (if (and (boundp 'emacs-major-version)
	   (>= emacs-major-version 21))
      new-face
    (if (fboundp 'copy-face)
	(copy-face old-face new-face frame new-frame)
      new-face)))

(defun embedded-set-face-foreground (face color &optional frame)
  "Safe caller for set-face-foreground, which does not exist on TTY-only emacsen."
  (if (and (boundp 'emacs-major-version)
	   (>= emacs-major-version 21))
      (set face (cons 'foreground-color color))
    (when (fboundp 'copy-face) (set-face-foreground face color frame))))

(defconst embedded-default-face
  (embedded-copy-face 'default 'embedded-default-face)
  "Face for words that we have not yet looked at.")

(defconst embedded-done-face
  (embedded-copy-face 'default 'embedded-done-face)
  "Face for words that have been done.")

(defconst embedded-error-face
  (embedded-copy-face 'default 'embedded-error-face)
  "Face for words that have not been recognized.")

(defconst embedded-busy-face
  (embedded-copy-face 'default 'embedded-busy-face)
  "Face for words that make up a command that has been recognized and is being executed.")

(defconst embedded-partway-face
  (embedded-copy-face 'default 'embedded-partway-face)
  "Face for words that have been recognized as part of an incomplete command.")

(defconst embedded-noise-face
  (embedded-copy-face 'default 'embedded-noise-face)
  "Face for words that have been recognized as noise.
These get deleted once the command has been completed.")

(embedded-set-face-foreground embedded-error-face "red")
(embedded-set-face-foreground embedded-partway-face "orange")
(embedded-set-face-foreground embedded-busy-face "green")
(embedded-set-face-foreground embedded-done-face "blue")
(embedded-set-face-foreground embedded-noise-face "grey")

(defvar processed-words nil
  "List of words we have processed, but may yet need post-processing.

Each element is an overlay, with properties 'type, 'string, and 'extra
as well as 'face.

The possible values of the 'type property are the cars of the list
processing-marker-faces.

The 'string property contains the word as a string.

Dependent words are words which have been removed from this list to
associate them with another member of the list. They are stored on the
'extra property of the member with which they are associated.

Post-processing is things like aliasing mistaken attempts, and
removing words from the buffer.

Words are pushed onto the head of the list as they are processed,
therefore the list is in the reverse order to the text.  This should
be handy in terms of the order in which we want to delete the words
(with each deletion not moving the words which are still to be
deleted).

Add words to the list using mark-processed-word, and consume the list
using postprocess-words.")

(defvar processing-marker-faces
  `((partway . ,embedded-partway-face)
    (final . ,embedded-done-face)
    (help . ,embedded-done-face)
    (noise . ,embedded-noise-face)
    (reset . ,embedded-error-face)
    (unrecognized . ,embedded-error-face))
  "Faces to use in mark-processed-word")

(defvar debug-postprocess nil
  "Whether postprocessing embedded-commands should output messages.")

(defun mark-processed-word (type string start end &optional extra)
  "Mark a word as having been processed (and presumably needing postprocessing).
See documentation of the variable processed-words for more detail."
  (when debug-postprocess (message "Marking %s as a %S" string type))
  (let ((previous (car processed-words)))
    (cond
     ((and previous
	   (eq type 'unrecognized)
	   (eq (overlay-get previous 'type) 'unrecognized)
	   (eq start (overlay-start previous))) ;;;;;;;;;;;;;;;; probably not quite right!
      ;; contiguous with a previous unrecognized word
      ;; seems to be trying to merge them?
      (progn
	(overlay-put previous 'string string)
	(move-overlay previous (overlay-start previous) end)))
     ((and previous
	   (eq start (overlay-start previous))
	   (eq end (overlay-end previous)))
      ;; same location as before, just update the data
      (overlay-put previous 'type type)
      (overlay-put previous 'extra (or extra (overlay-get previous 'extra))))
     ((and nil previous
	   (eq (overlay-get previous 'type) 'unrecognized))
      (message "Previous word \"%s\" unrecognized, possibly an intron to \"%s\"" (overlay-get previous 'string) string)
      ;; todo: make this put the previous word as an "extra" for our current word
      )
     (t
      (message "mark-processed-word: default case for \"%s\"" string)
      (let ((overlay (make-overlay start end)))
	(push overlay
	      processed-words)
	(if (and previous
		 (eq (overlay-get previous 'type) 'unrecognized))
	    (progn
	    (overlay-put overlay 'extra (list previous))
	    (message "Moving unrecognized onto extra: Processed-words was %S (%S)" processed-words (embedded-words-string))
	    (rplacd processed-words (cddr processed-words))
	    (message "Moving unrecognized onto extra: Processed-words now %S (%S)" processed-words (embedded-words-string))

	    )
	  (overlay-put overlay 'extra extra))
	(overlay-put overlay 'string string)
	(overlay-put overlay 'type type)
	(overlay-put overlay 'face (cdr (assoc type processing-marker-faces)))))))
  (when debug-postprocess (display-embedded-words)))

(defun embedded-words-string (&optional words)
  "Return a string representing the command words so far."
  (mapconcat (lambda (word-overlay)
	       (let ((extra (overlay-get word-overlay 'extra)))
		 (if extra
		     (concat "(" (embedded-words-string extra) ")" 
			     (overlay-get word-overlay 'string))
		   (overlay-get word-overlay 'string))))
	     (reverse
	      (if words
		  words
		processed-words))
	     " "))

(defun display-embedded-words ()
  "Display the current embedded words."
  (if processed-words
      (with-output-to-temp-buffer "*Embedded words*"
	(display-embedded-words1 "")
	(message "Embedded words are %S" (embedded-words-string)))
    (if (get-buffer-window "*Embedded words*")
	(delete-window (get-buffer-window "*Embedded words*")))))

(defun display-embedded-words1 (prefix &optional words)
  "Internal function to display the current embedded words."
  (dolist (word (or words processed-words))
    (princ
     (format "%s% 14S \"%s\" (chars %d..%d)\n"
	     prefix
	     (overlay-get word 'type) 
	     (overlay-get word 'string) 
	     (overlay-start word) (overlay-end word)))
    (let ((extra (overlay-get word 'extra)))
      (when extra
	(display-embedded-words1 (concat prefix "  ") extra)))))

(defun handle-intruding-words (unwanted wanted)
  "Do something about UNWANTED which was said just before WANTED.
It might be a mis-hearing, or just a grunt from the user."
  (message "%S appears as a predecessor to %S" unwanted wanted)
  ;; todo: ask the user if UNWANTED should be registered as an alias for WANTED, or just ignored always, or what
  (require 'soundex)
  (let ((sound-unwanted (soundex unwanted))
	(sound-wanted (soundex wanted)))
    (if (string= sound-unwanted sound-wanted)
	(message "%S might be a misrecognition of %S" unwanted wanted))))

(defvar removed-words nil
  "Words removed by the most recent postprocess-words.
They can get put back by embedded-command-abandon.")

(defun postprocess-words (&optional words)
  "Do any postprocessing needed on processed-words.
See documentation of the variable processed-words for more detail."
  ;; todo: look at intruding words within the command phrase, make sure they are removed properly, and remember them for potential aliasing
  (let ((where-I-was (make-marker)))
    (set-marker where-I-was (point))
    (when debug-postprocess (message "postprocessing: marker set to %S" where-I-was))
    (dolist (word (or words processed-words))
      (when debug-postprocess (message "Postprocessing %S \"%s\" (chars %d..%d which are \"%s\")"
				       (overlay-get word 'type)
				       (overlay-get word 'string)
				       (overlay-start word) (overlay-end word)
				       (buffer-substring-no-properties (overlay-start word) (overlay-end word))))
      (let ((extra (overlay-get word 'extra)))
	(when extra
	  (message "Postprocessing dependent words %S" extra)
	  (handle-intruding-words (overlay-get (car extra) 'string) (overlay-get word 'string))
	  (postprocess-words extra)))
      ;; postprocess-word word
      (save-excursion ; for while we nip over to the buffer containing the word
	(set-buffer (overlay-buffer word)) ; might not be in that buffer now
	(push (cons (overlay-start word) (buffer-substring-no-properties (overlay-start word) (overlay-end word)))
	      removed-words)
	(delete-region (overlay-start word) (overlay-end word))
	(delete-overlay word)))
    (when (null words)
      (when debug-postprocess
	(message "Completed postprocessing, clearing processed-words; removed-words are %S" removed-words))
      (setq processed-words nil))
    (when debug-postprocess (message "Marker now %S" where-I-was))
    (goto-char where-I-was)

    ;; finally, adjust the whitespace; this will probably take some fine tuning
    ;; it might be best to set an undo marker when we set command-phrase-start,
    ;; and undo to there, instead of the whole process above?
    (just-one-space)
    (if (looking-at "[][{};,.`'\"()]")
	(delete-horizontal-space))
    ))

(defun embedded-commands-current-choices (&optional big)
  "Display the current choices."
  (interactive "P")
  (if big
      (with-output-to-temp-buffer "*Embedded commands status*"
	(unless (string= embedded-commands-mode-status-words-string "")
	  (princ (format "You have said \"%s\"\n\n" embedded-commands-mode-status-words-string)))
	(princ "You can now say:\n  \"")
	(princ (mapconcat 'car
			  (rplacd embedded-command-possibilities
				  (sort (cdr embedded-command-possibilities)
					(function (lambda (a b)
						    (string< (car a) (car b))))))
			  "\" or\n  \""))
	(princ "\"\n\nAlternatively, you can say:\n  \"")
	(princ (mapconcat 'identity reset-words "\" or\n  \""))
	;; todo: change the following, split into two kinds of reset
	(princ "\"\nto reset the command phrase, leaving those words as text."))
    (message "Choices are: %s" (mapconcat 'car (cdr embedded-command-possibilities) ", "))))

(defvar embedded-show-choices-on-error t
  "*Whether to show the current choices after any unrecognized word.")

(defvar embedded-show-choices-partway t
  "*Whether to show the current choices after every partly-recognized phrase.")

(defvar embedded-show-choices-always t
  "*Whether to show the current choices after every step of processing.")

(defvar embedded-remove-executed-commands t
  "*Whether to remove command words once they have been used.")

(defvar embedded-commands-dry-run t
  "*Whether to just pretend to do embedded commands.")

(defvar embedded-partial-most-recent-word-end nil
  "Where the most recent word recognized ends.")

(make-variable-buffer-local 'embedded-partial-most-recent-word-end)

(defun embedded-delete-overlay (overlay)
  "Delete OVERLAY and any extras associated with it."
  (let ((extra (overlay-get overlay 'extra)))
    (if extra
	(mapcar 'embedded-delete-overlay extra))
    (delete-overlay overlay)))

(defun embedded-commands-reset-state ()
  "Reset the state of the embedded-command recogniser."
  (interactive)
  (setq embedded-command-possibilities coph:phrase-tree
	embedded-commands-mode-status-words-string ""
	embedded-commands-mode-status-flag-string "\" "
	embedded-partial-most-recent-word-end nil)
  (mapcar 'embedded-delete-overlay processed-words)
  (setq processed-words nil))

(defvar embedded-introns nil
  "The suspected introns we have seen.")

(defun embedded-intron (start end)
  "Handle intron between START and END."
  (when debug-embedded-commands
    (message "already inside a command, intron=%S"
	     (buffer-substring-no-properties start end)))
  (if (> end start)
      (save-excursion
	(goto-char start)
	(while (re-search-forward "\\<\\(\\w+\\)\\>" end t)
	  (let ((word (match-string 1))
		(start (match-beginning 1))
		(end (match-end 1)))
	    (when debug-embedded-commands (message "Intron word %S" word))
	    (push word embedded-introns)
	    (mark-processed-word 'unrecognized word start end) ;; todo: make them go onto the dependents list of the following real hit
	    )))))

(defvar debug-embedded-commands t
  "Whether to burble on about what we are up to.")

(defun embedded-commands-reset-command ()
  "Reset the embedded-commands system, removing the current command words."
  (interactive)
  (postprocess-words)
  (embedded-commands-reset-state))

(defun embedded-commands-forget-it-command ()
  "Reset the embedded-commands system, leaving the current command words as ordinary dictated text."
  (interactive)
  (embedded-commands-reset-state))  

(defun embedded-commands-abandon-command ()
  "Abandon an embedded command.
If it has been partially recognized, turn the words back into ordinary words.
If we have started running it (typically in the interactive argument reader)
throw out of that, and put the words back into the buffer."
  (interactive)
  (cond
   ((and embedded-commands-mode
	 embedded-command-possibilities)
    (progn
      (message "got partial phrase, resetting")
      (embedded-commands-reset-state)
      ;; for the benefit of use on handsfree-main-menu-hook, return
      ;; non-nil if there was a partial command to cancel; this will
      ;; prevent it from doing its normal menu action, thus letting us
      ;; use the same key or pedal as menu selection, which makes
      ;; sense because it does not make sense to start a menu action
      ;; while entering an embedded command
      t))
   (embedded-command-running
    (message "running command")
    (throw 'abandon-embedded-command t))
   (t
    ;; for the benefit of use on handsfree-main-menu-hook, return nil
    ;; if there was no partial command to cancel, in which case it
    ;; should carry on to do the normal menu
    nil)))

(defun embedded-command-abandon ()
  "Abandon running an embedded command.
This is called if we throw while in the call-interactively that runs
the embedded commands, and can be triggered by doing
\\[embedded-commands-abandon-command] while the command is prompting
for its arguments. You could bind it to a suitable interjection in
your normal voice command system.
This puts the command words back."
  (save-excursion
    (dolist (word removed-words)
      (goto-char (car word))
      (insert (cdr word) " ")))
  (message "Sorry!"))

(defvar embedded-command-running nil
  "Whether we are currently running an embeded command.
Bound to t when we are doing so.")

(defun handle-possible-command-word (word word-start word-end)
  "Try to process WORD as a command. WORD-START, WORD-END show where it is.
WORD is taken as it is; WORD-START and WORD-END also include any white space
which should be removed when the word is removed.

If the command is defined as a symbol, and that symbol has
pre-embedded-command-function or post-embedded-command-function
properties, run those functions before or after the function, passing
the function symbol as an argument. (These are like before- or after-
advice on the function, but are run only when it is used as an
embedded command. They can be used for tidying up whitespace, which
the embedded command system can leave a bit messy.)

Return whether we modified the text (which we normally do when we take an action)."
  (when debug-embedded-commands
    (message "Trying to handle word \"%s\" among (%S ...)"
	     word
	     (first (car (cdr embedded-command-possibilities)))))
  (setq removed-words nil)
  (let ((hit nil))
    (cond
     
     ;; first, look for some fixed commands:
     ;;   commands which reset us to the start of a phrase:
     ((member word reset-words)
      (when debug-embedded-commands (message "That's a reset-word"))
      (mark-processed-word 'reset word word-start word-end)
      (embedded-commands-reset-command)
      t)

     ((member word forget-it-words)
      (when debug-embedded-commands (message "That's a forget-it-word"))
      (mark-processed-word 'reset word word-start word-end)
      (embedded-commands-forget-it-command)
      )

     ;;    commands show the current possibilities:
     ((member word help-words)
      (when debug-embedded-commands (message "That's a help-word"))
      (embedded-commands-current-choices t)
      (mark-processed-word 'help word word-start word-end)
      t)

     ;; if it's not a fixed command, see whether it is one of the current possibilities:
     ((setq hit (assoc word embedded-command-possibilities))
      (when debug-embedded-commands (message "That's a hit, to %S" (cdr hit)))
      (mark-processed-word 'partway word word-start word-end)
      ;; it is a current possibility, see whether it completes the phrase, or what
      (cond
       ;; null -- should not happen; weird but might as well reset
       ((null (cdr hit))
	(when debug-embedded-commands (message "... a null hit"))
	(setq embedded-command-possibilities coph:phrase-tree
	      command-phrase-start word-end
	      embedded-partial-most-recent-word-end word-end)
	nil)

       ;; maps to a list, so there are further possibilities
       ((consp (cdr hit))
	(when debug-embedded-commands (message "... a list hit"))
	(if (eq embedded-command-possibilities coph:phrase-tree)
	    (setq command-phrase-start word-start)
	  (embedded-intron embedded-partial-most-recent-word-end word-start))
	(setq embedded-command-possibilities hit
	      embedded-commands-mode-status-words-string (concat
							  embedded-commands-mode-status-words-string
							  " " word)
	      embedded-commands-mode-status-flag-string (concat "... {"
								(mapconcat 'car
									   (cdr embedded-command-possibilities)
									   "|")
								"}\" "))
	(setq embedded-partial-most-recent-word-end word-end)
	(when embedded-show-choices-partway
	  (embedded-commands-current-choices))
	nil)

       ;; maps to a symbol, so we have a complete phrase, and can execute a command
       ((or (symbolp (cdr hit))
	    (stringp (cdr hit)))
	(when debug-embedded-commands (message "... a symbol hit"))
	(setq embedded-commands-mode-status-words-string ""
	      embedded-commands-mode-status-flag-string "\" ")
	(if embedded-partial-most-recent-word-end
	    (embedded-intron embedded-partial-most-recent-word-end word-start))
	(mark-processed-word 'final word word-start word-end)
	(postprocess-words)
	(let ((ccbuff (current-buffer))
	      (command (cdr hit)))
	  (if embedded-commands-dry-run
	      (message "Would execute command %S, but am dummy-running" command)
	    (condition-case error-var
		(let* ((embedded-command-running t)
		       (command-symbol (if (symbolp command)
					   command
					 (intern command)))
		       (pre-function (get command-symbol 'pre-embedded-command-function))
		       (post-function (get command-symbol 'post-embedded-command-function)))
		  (when (catch 'abandon-embedded-command
			  (when (functionp pre-function)
			    (funcall pre-function command-symbol))
			  (call-interactively command-symbol)
			  (when (functionp post-function)
			    (funcall post-function command-symbol))
			  nil)
		    (message "Command abandoned")
		    (embedded-command-abandon)))
	      (error (progn
		       (message "Error %s in command %s" error-var command)
		       (embedded-commands-reinitialize))))))
	;; get ready for next command
	(setq embedded-command-possibilities coph:phrase-tree)
	(message "completed command phrase")
	t)

       ((vectorp (cdr hit))
	;; key-sequence (e.g. space-bar is [32])
	(when debug-embedded-commands (message "... a vector hit"))
	(setq embedded-commands-mode-status-words-string ""
	      embedded-commands-mode-status-flag-string "\" ")
	(mark-processed-word 'final word word-start word-end)
	(postprocess-words)
	(let ((command (key-binding (cdr hit) t)))
	  (if embedded-commands-dry-run
	      (message "Would execute command %S, but am dummy-running" command)
	    (condition-case error-var
		(let ((embedded-command-running t))
		  (when (catch 'abandon-embedded-command
			  (call-interactively command)
			  nil)
		    (message "Command abandoned")
		    (embedded-command-abandon)))
	      (error (progn
		       (message "Error in command %s" command)
		       (embedded-commands-reinitialize)))))
	  )
	(message "Done command")
	;; get ready for next command
	(setq embedded-command-possibilities coph:phrase-tree)
	t)))

     ;; I would also like to do some clever stuff about seeing whether we are at
     ;; the start of a new phrase (particularly if there was only one completion
     ;; for a current incomplete phrase) and also to see whether we have simply
     ;; missed a word in a phrase and can pick up again by implication.... such
     ;; things will fit in here

     ;; we recognise it as a word which is often picked up in error by the recognition engine
     ;; but do this only once we are in a phrase!
     ((and (member word noise-words) 
	   (not (null embedded-command-possibilities))
	   (not (eq embedded-command-possibilities coph:phrase-tree)))
      (message "%s identified as noise word" word)
      (mark-processed-word 'noise word word-start word-end)
      (when embedded-show-choices-on-error
	(embedded-commands-current-choices))
      nil)

     ;; we don't recognise it, mark it as unrecognised
     ;; it is probably text being dictated, and we hope that some other syntax colouring mechanism will get to it soon
     (t
      (message "%s occurs in a command phrase but is not valid here" word)
      ;; (mark-processed-word 'unrecognized word word-start word-end)
      nil
      )
     )))

(defvar words-since-recognized-word 0
  "How many words have been entered since we last recognised a word.
This is necessarily somewhat approximate, as we don't detect all things
that mark the end of entering a word.")

(make-variable-buffer-local 'words-since-recognized-word)

(defvar embedded-forget-it-threshold 3
  "*The number of unrecognised words after a recognised word after which it is assumed that you are not really entering a command.")

(defun embedded-commands-note-unrecognized-word (&rest ignore)
  "Note that we have had another word which we did not recognise."
  (setq words-since-recognized-word
	(1+ words-since-recognized-word))
  (when (>= words-since-recognized-word embedded-forget-it-threshold)
    (embedded-commands-forget-it-command)
    (setq words-since-recognized-word 0)))

(add-hook 'smart-space-hooks 'embedded-commands-note-unrecognized-word)

(defun embedded-commands-abbrev-hook-function ()
  "A function to be used as the expansion hook on words that we recognise as part of embedded commands."
  (message "In embedded-commands-abbrev-hook-function, activated by char %S" last-command-char)
  (setq words-since-recognized-word 0)
  (when embedded-commands-mode
    (let* ((causing-command-char last-command-char)
	   (expansion-point (point))
	   (start (progn (forward-word -1) (point)))
	   (end
	    (if t expansion-point (progn (forward-word 1) (point))))
	   (word (buffer-substring-no-properties start end))
	   )

      ;; todo: sort out the following whitespace adjustment:
      ;; we need to do some kind of elision of adjacent words, but that should probably be up to postprocess-words
      ;; (if (eq (char-before start) ? ) (setq start (1- start)))
      (if (eq (char-after end) ? ) (setq end (1+ end)))

      ;; todo: go back to remembered position from the start of this function, if no command was executed
      (if (handle-possible-command-word word start end)
	  (insert causing-command-char)
	(goto-char expansion-point)))))

(defun register-embedded-command-word (word)
  "Register WORD to be processed by embedded-commands-abbrev-hook-function whenever it is said (or typed)."
  (message "Defining %s as an embedded command word" word)
  ;; todo: try to make this respect the existing abbrev function, you can get this using (symbol-function (intern-soft word global-abbrev-table))
  (define-abbrev
    global-abbrev-table
    word
    word				; might be able to give a special value, see doc of define-abbrev
    'embedded-commands-abbrev-hook-function))

(defvar command-phrase-start nil
  "Where the current command phrase starts.")

(make-variable-buffer-local 'command-phrase-start)

(defvar noise-words-to-delete nil
  "The list of starts and ends of the latest noise words.")

(make-variable-buffer-local 'noise-words-to-delete)

(defvar in-embedded-processing nil
  "Whether we are already in the embedded command buffer processing.
Stops it recursing if the commands it triggers affect that buffer.")

(defvar embedded-ignore-buffers-regexp "Minibuf"
  "Pattern for buffers in which we want to suppress embedded command processing.")

(defvar embedded-commands-initialized nil
  "Whether embedded-commands has been initialized.")

(defun embedded-commands-reinitialize (&rest ignorable)
  "Reinitialize the embedded-commands system."
  (interactive)
  (setq embedded-command-possibilities coph:phrase-tree))

(defvar embedded-words-not-allowed-at-start-of-command
  '("in")
  "*Words which are not allowed as the first word of a command phrase.
Such commands must be prefixed with the word \"command\".")

(defun modify-single-word-commands (command)
  "If COMMAND is a single word, transform it, otherwise return COMMAND."
  ;; todo: perhaps not enter it at all if it is a noise-word
  (if (and (string-match "^\\(\\w+\\) " (car command))
	   (not (member (match-string 1 (car command))
			embedded-words-not-allowed-at-start-of-command)))
      command
    (message "Prefixing single-word command %S with \"command\"" command)
    (cons (format "command %s" (car command))
	  (cdr command))))

(defun embedded-commands-initialize (&optional force)
  "Initialize the embedded-commands system."
  (message "Initializing embedded commands system")
  (coph:ensure-tree 'modify-single-word-commands force 'register-embedded-command-word)
  (mapcar (lambda (l) (mapcar 'register-embedded-command-word l))
	  (list noise-words reset-words help-words))
  (setq embedded-command-possibilities coph:phrase-tree)
  (embedded-commands-reinitialize)
  ;; (add-hook 'before-change-functions 'embedded-commands-reinitialize)
  (add-hook 'end-sentence-hook 'embedded-commands-forget-it-command)
  (add-hook 'handsfree-main-menu-hook 'embedded-commands-abandon-command)
  (setq embedded-commands-initialized t))

(defvar abbrev-ambiently-on nil
  "Whether abbreviation expansion was already on in this buffer when embedded-commands turned it on.")

(make-variable-buffer-local 'abbrev-ambiently-on)

(defun embedded-generic-text-hook (&optional com)
  "Hook function to forget any partially-recognized commands when we start a new text construct."
  (embedded-commands-forget-it-command)
  nil)

(defun embedded-question-mark (n)
  "A binding for the ? character, for use in embedded commands."
  (interactive "p")
  (if (and embedded-commands-mode
	   (not (eq embedded-command-possibilities coph:phrase-tree)))
      (embedded-commands-current-choices)
    (self-insert-command)))

(defvar embedded-command-map (make-sparse-keymap "Embedded commands")
  "Special bindings for embedded commands mode.")

(define-key embedded-command-map "?" 'embedded-question-mark)

(unless (assoc 'embedded-commands-mode minor-mode-map-alist)
  (setq minor-mode-map-alist
	(cons (cons 'embedded-commands-mode
		    embedded-command-map)
	      minor-mode-map-alist)))

(defun toggle-embedded-commands ()
  "Switch embedded command recognition on or off.
Switching it on forces abbreviation expansion on, as it uses the expansion hooks to do its work."
  (interactive)
  (setq embedded-commands-mode
	(not embedded-commands-mode))
  (if embedded-commands-mode
      (progn
	(unless embedded-commands-initialized
	  (embedded-commands-initialize))
	(setq abbrev-ambiently-on abbrev-mode)
	(add-hook 'generic-text-after-hooks 'embedded-generic-text-hook)
	(abbrev-mode 1))
    (progn
      (setq abbrev-mode abbrev-ambiently-on)
      (remove-hook 'generic-text-after-hooks 'embedded-generic-text-hook)
      (embedded-commands-reset-state))
    )
  (message "Embedded commands now %s" (if embedded-commands-mode "on" "off")))

(defvar vr-embedded-commands-top-commands
  '(toggle-embedded-commands
    ("display word tree" . coph:display-tree)
    ("reset" . embedded-commands-reset-command)
    ("forget it" . embedded-commands-forget-it-command)
    ("abandon" . embedded-commands-forget-it-command)
    )
  "Top-level voice commands for embedded commands.")
