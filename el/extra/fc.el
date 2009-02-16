;;; flashcard.el -- a package for drilling on questions and answers.
;;; Copyright (c) 1994 by Lars Huttar <huttar@cs.oberlin.edu>
;;; Freely distributable under the GNU GPL.

;;; Version: 0.95 (beta)
;;; Release date: Wed Jun  8 09:50:13 1994

;;; The author welcomes suggestions and bug reports.
;;; However, the author offers NO WARRANTY, express or implied.

;;; The purpose of `flashcard' is to drill the user over a
;;; user-created database of questions and answers.  It records
;;; scores, and can tailor the drill according to the user's past
;;; performance.  To quickly summarize its features, I'll describe an
;;; example.

;;; I create a vocabulary file; the top line indicates that there are
;;; two fields in this file, called `dutch' and `english'.  (But I
;;; could have three or more fields if desired.)  I then enter a bunch
;;; of pairs of Dutch and English words, one pair on a line.  E.g.:
;;;     !% Fields: dutch	english
;;;
;;;     wie	who
;;;     huis	house
;;;     straat	street

;;; etc.  Then I can use `flashcard' to drill me on this file, either
;;; presenting me with the `dutch' field and letting me think of the
;;; `english' field, or vice versa.  `flashcard' will not check my
;;; answers (unless I'm in multiple-choice mode); I must judge if I
;;; got the answer right or wrong, and hit `r' or `w' accordingly.
;;; `flashcard' keeps score for each field and each record: how many
;;; times I've gotten it right, how many times I've gotten it wrong,
;;; and when was the last time I got it right.  This information is
;;; stored in a separate score file.

;;; At some later date I can come back to the same vocabulary
;;; file, and have `flashcard' drill me on selected cards, based
;;; on criteria such as "all cards which I haven't gotten right
;;; in a month" or "all cards which I've gotten right less than
;;; 75% of the time."
;;; Multiple vocabulary files can be loaded, mixed together, and
;;; drilled on simultaneously.  When the drilling is done, scores will
;;; be saved to the appropriate separate score files.

;;; Someday perhaps there will be info documentation with all the
;;; information organized, and a nice tutorial.

;;; Some sample Japanese vocabulary files can be obtained by anonymous
;;; ftp from:
;;;     crl.nmsu.edu:CLR/multiling/japanese/vocabulary
;;;     kuso.shef.ac.uk:pub/japanese/larstango
;;;     ftp.cc.monash.edu.au:pub/nihongo/mangajin-vocab
;;; However, you need Mule (Multilingual Emacs) or Nemacs (Japanese
;;; Emacs) to display the characters.

;;; Installation:
;;; Copy flashcard.el into a directory in your load-path.  Byte-compile
;;; it if desired.  Put the following in your .emacs file:
;;;    (autoload 'flashcard "flashcard"
;;;       "Use Flashcard package to drill on flashcards." t)

;;; LCD Archive Entry:
;;; flashcard.el|Lars Huttar|huttar@cs.oberlin.edu
;;; |Selective drilling on flashcards, tailored to your past performance
;;; |1994/06/08 09:50:13|0.95|

;;; Note: There is only occurrence of Flashcard going on per emacs process.

;;; To do:
;;; (x means done or fixed)
;;; x put in a proper LCD Entry.
;;; - document what "Overall" and "This card" fields mean.
;;;   I.e. "This card" is persistent, and is specific not only to the
;;;   card but also to the question/answer field combination.
;;; x speed up response time (especially to `r' and `w' commands) and/or
;;;   display "Working..."-type message.
;;; x There seems to be a bug: fc-modified-scores is not being set
;;;   to nil after saving.
;;; x oops, the first card is not being filtered for elegibility
;;;   criteria.  I guess we need to set fc-current-card = -1 and
;;;   call fc-next-card.
;;; x probably remove redisplay-question/answer; just erase the buffer
;;;   and start over? (what about case of redisplay answer and ... uh...)
;;; x finish multi-choice support by modifying fc-display-answer, (done)
;;;   fc-redisplay-{question,answer}, and the mechanism for changing
;;;   the options fc-num-choices and fc-multiple-choice (to update
;;;   immediately).
;;; - maybe put something in the modeline indicating the filename (or
;;;   title) of the (first?) file being drilled on.
;;; x move defun-subst's before the defun's?
;;; x enhance options help by letting user get help for one option
;;;   at the bottom of the help buffer by hitting that key.
;;; - maybe a command to go backward without undo.
;;; x add "faces" for a snazzier look; highlight the question and
;;;   the answer with a different weight/color/etc.
;;; - maybe I should re-instate separate keys for load-file (chucking
;;;   previously loaded vocab) and add-file (adding to previously
;;;   loaded vocab).
;;; x new variable fc-num-choices, which controls how many choices
;;;   to present in multiple-choice mode.
;;; x Maybe a new variable, fc-filter-equals or some such.
;;;   This would mean, filter out cards on which the question field
;;;   and the answer field are equal.
;;; x In options, if new value == old value, don't update anything. (done)
;;; x Selection based on score criteria is already in place, and,
;;;   I believe, tested.  Now set up an interface for setting the
;;;   criteria.  Use letters l,p,r for options. (done)
;;; x Enhance options help to show current values of options.  Use
;;;   columns "Key", "Option name", "Current Value". (done)
;;; - This is probably not something I'll do before the first release,
;;;   but it's something to think about: auto-saving of scores.
;;;   Save the current score data to an auto-save file(s) periodically.
;;;   Emacs would take care of this for us, if we kept the score data
;;;   in buffers that were visiting those score files.  However, that
;;;   would be a pretty radical change to the current data structures.
;;;   Is there some way to have an arbitrary hook called at auto-save
;;;   time, to save some kind of state to a file?  Hmm, there is an
;;;   auto-save-hook variable, but it's not available in Mule.
;;;   Certainly, we want to be able to save score data when Emacs
;;;   gets a fatal error.
;;;   How does save-some-buffers EXITING work? (it saves some
;;;   non-file buffers)
;;;   Until this is solved, say in the docs that the user should
;;;   save frequently to avoid losing data.
;;; x update the Question/Answer ("foo"): "bar" stuff when changing
;;;   the fc-question/answer-field, even when question-field and
;;;   answer-field are the same.  To do this, abstract out the
;;;   functions to redraw question and answer (and other) lines.
;;;   (done, I believe)
;;; x Make sure field names are consistent, what with defaults and
;;;   all.  (done, I think)
;;; x put the loaded records in right order, not reverse order.
;;;   Do this by keeping an fc-records-tail variable and using it
;;;   to extend the list. (done, I think)
;;; x keep track of the user's scores.  Can currently load multiple files,
;;;   drill on them together, and save the data into their respective
;;;   score files.  Tested with multiple files but not in multiple
;;;   directions (e.g. eng->dutch, dutch->eng).
;;; / when sure about score file i/o, change %d's to %x's in format.
;;;   Actually, why bother?  It probably wouldn't make things clearer,
;;;   and the space savings would only be about 10%.
;;; x perhaps add a multiple choice option.  Then the program could
;;;   easily check the user's answer.  If so, we should reserve
;;;   the keys `a' thru `e' or so.  Or maybe use digits?  Digits.
;;; - maybe let some fields be declared non-unique; e.g.
;;;   a gender field for nouns.  Flashcard would not attempt
;;;   to drill from that field to other fields, nor store data
;;;   about such drills.
;;; - a command, available in either of the two states, that marks the
;;;   current flashcard (record) as being well-known, i.e. tells
;;;   Flashcard not to drill on it anymore.  The flashcard is not
;;;   erased from the vocab file, but its score data contains a flag
;;;   so it will no longer be drilled on.  Thus different students
;;;   using the same vocab file can have different words marked as
;;;   too easy in their own score files.
;;; - Provide a way to clear the scores for the current card; perhaps
;;;   to edit all three values arbitrarily.
;;; x add the ability to back up (just one question or all the way
;;;   through a quiz?), undoing whatever score modifications were
;;;   done.  (done, just one question)
;;; x set up synchronized variables fc-current-card-num and fc-current-card,
;;;   to cut down on repeated arefs.  (fc-current-card-num would be what
;;;   fc-current-card used to be.)  (done)
;;; x structure / abstract out the line-positions of the question,
;;;   answer, and help areas.  Adjust help according to the number
;;;   of fields and whether the display-extra-fields variable is set.
;;;   Compute each field's position based on the previous, not as
;;;   an absolute position. (done)
;;; x Possibly add a variable to turn off help, for slow terminals.


;;; User variables:

(defvar fc-flashcard-dir "~/flashcard"
  "*The default directory for finding flashcard files.
Should not end with a slash unless it is just \"/\".")

(defvar fc-score-dir "~/flashcard"
  "*The default directory for saving flashcard score files.
Should not end with a slash unless it is just \"/\".")

(defvar fc-score-ext "fs"
  "*The extension used on flashcard score files (not including period).")

(defvar fc-field-delimiter "\t"
  "*The default field delimiter string in flashcard files; a regexp.")

(defvar fc-record-delimiter "\n"
  "*The default record delimiter string in flashcard files; a regexp.")

(defvar fc-default-fields '("kanji" "hiragana" "english")
  "*Default list of field names (strings) for flashcard files.
Usually not used, as the field names should be supplied in the files.")

(defvar fc-show-other-fields 'answer-only
  "*When to show fields other than the question and answer fields.
If nil, never; if t, always; otherwise, only when displaying an answer.")

(defvar fc-auto-shuffle t
  "*If non-nil, shuffle cards at the start of each drill.")

(defvar fc-multiple-choice nil
  "*If non-nil, drill in multiple-choice mode.")

(defvar fc-num-choices 4
  "*How many choices to present when fc-multiple-choice is non-nil.")

(defvar fc-limit-percent nil
  "*Maximum percentage-right for cards to be drilled.
Limit drill to cards whose percentage guessed right is lower than or
equal to this value.  If nil, no limit.")

(defvar fc-limit-num-right nil
  "*Maximum number of times guessed right for cards to be drilled.
Limit drill to cards that have been guessed right this many times or
fewer.  If nil, no limit.")

(defvar fc-limit-last-right nil
  "*Minimum no. of days since last time gotten right, for cards to be drilled.
Limit drill to cards that not have been guessed right as recently as
this many days ago.  If nil, no limit.")

(defvar fc-filter-equals t
  "*If non-nil, don't drill on cards whose question and answer are the same.")

(defvar fc-display-help t
  "*If non-nil, display help below question/answer.")

(defvar fc-question-face 'bold
  "*Typeface for question text, if possible.  Nil for none.
Here's an example of an easy way to create a custom face (in emacs 19):
  (require 'hilit19)
  (setq fc-question-face (hilit-lookup-face-create 'firebrick-italic))
")
;;or
;;  (setq fc-question-face 'question)
;;  (hilit-translate question 'hex-007039-bold)
;; no, I guess that doesn't work.

(defvar fc-answer-face 'bold
  "*Typeface for answer text, if possible.  Nil for none.
See documentation for fc-question-face.")

(defvar flashcard-mode-hooks nil
  "*Hooks run after initializing flashcard-mode, before loading any files.
A good place to set key bindings in flashcard-mode-map.")

(defvar flashcard-mode-map nil
  "*Keymap for flashcard-mode.")

(if flashcard-mode-map nil
  ;; if flashcard-mode-map not set by user
  (setq flashcard-mode-map (make-keymap))
  ;(suppress-keymap flashcard-mode-map)

  (define-key flashcard-mode-map (kbd "C-c C-q") 'fc-quit)
  (define-key flashcard-mode-map (kbd "C-c C-?") 'describe-mode)
  (define-key flashcard-mode-map (kbd "C-c C-h") 'describe-mode)
  (define-key flashcard-mode-map (kbd "C-c C-o") 'fc-options)
  (define-key flashcard-mode-map (kbd "C-c C-f") 'fc-load-flashcard-file)
  (define-key flashcard-mode-map (kbd "C-c C-d") 'fc-start-drill)
  (define-key flashcard-mode-map (kbd "C-c C-s") 'fc-save-scores)
  (define-key flashcard-mode-map (kbd "C-c C-c") 'fc-clear-data)

  (define-key flashcard-mode-map (kbd "RET") 'fc-answer-check)
  (define-key flashcard-mode-map (kbd "C-c C-g") 'fc-give-up)
  (define-key flashcard-mode-map (kbd "C-c C-r") 'fc-answer-right)
  ;(define-key flashcard-mode-map "y" 'fc-answer-right)
  (define-key flashcard-mode-map (kbd "C-c C-w") 'fc-answer-wrong)
  ;(define-key flashcard-mode-map "n" 'fc-answer-wrong)
  (define-key flashcard-mode-map (kbd "C-c \C-m") 'fc-multi-proceed)
  (define-key flashcard-mode-map (kbd "C-c C-k") 'fc-skip)
  (define-key flashcard-mode-map (kbd "C-c C-u") 'fc-undo)

  (define-key flashcard-mode-map (kbd "0") 'fc-choose)
  (define-key flashcard-mode-map (kbd "1") 'fc-choose)
  (define-key flashcard-mode-map (kbd "2") 'fc-choose)
  (define-key flashcard-mode-map (kbd "3") 'fc-choose)
  (define-key flashcard-mode-map (kbd "4") 'fc-choose)
  (define-key flashcard-mode-map (kbd "C-c C-5") 'fc-choose)
  (define-key flashcard-mode-map (kbd "C-c C-6") 'fc-choose)
  (define-key flashcard-mode-map (kbd "C-c C-7") 'fc-choose)
  (define-key flashcard-mode-map (kbd "C-c C-8") 'fc-choose)
  (define-key flashcard-mode-map (kbd "C-c C-9") 'fc-choose)
  (define-key flashcard-mode-map (kbd "C-c C--") 'fc-undefined)

  (if (and (boundp 'fc-debug) fc-debug)
      (define-key flashcard-mode-map "I"                      ;; debugging
        '(lambda () (interactive)
           (fc-init)
           (message "fc initialized"))))

  (substitute-key-definition 'undefined 'fc-undefined flashcard-mode-map)
  )

;; Internal variables:

(defvar fc-state nil
  "Current state of flashcard program.")
;; such as:
;;    nil - uninitialized
;;    init - initialized but not drilling yet (options screen)
;;    question - have asked a question, waiting for user to hit
;;               fc-answer-check, fc-give-up, or fc-skip
;;    answer-check - have displayed answer, waiting for user to hit
;;               fc-answer-rigth, fc-answer-wrong, or fc-skip.
;;    answer-gave-up - have displayed answer, waiting for user to hit
;;               fc-give-up, or fc-skip.


(defvar fc-fields nil "List of field names (strings) for flashcards in use.")
(defvar fc-num-fields nil
  "Number of fields per flashcard (nil means uninitialized).")

(defvar fc-question-field nil
  "Numeric index (into fc-fields) of field currently serving as question.
Changing this variable directly (using setq or set-variable), in mid-drill,
is NOT advisable.  Use the `\\[fc-options]' command instead.
But using setq or set-variable before starting a drill is fine.")

(defvar fc-answer-field nil
  "Numeric index (into fc-fields) of field currently serving as answer.
Changing this variable directly (using setq or set-variable), in mid-drill,
is NOT advisable.  Use the `\\[fc-options]' command instead.
But using setq or set-variable before starting a drill is fine.")

(defvar fc-current-score-field nil
  "Index of score field corresponding to current q-a combo.")

(defvar fc-records nil "Flashcard record data structure.")
(defvar fc-records-tail nil "Points to end of record data structure.")
(defvar fc-num-records 0 "Number of flashcard records in memory.")

(defvar fc-files nil
  "List of files currently loaded.
For each, there is a vector containing the file name, its title, and
the number of the last card loaded from the file.")
(defvar fc-files-tail nil "Points to end of fc-files list.")

(defvar fc-modified-scores nil
  "Non-nil if scores have been modified, i.e. user has been drilling
and hasn't saved score data.")

(defvar fc-debug nil "If non-nil, display debugging messages.")

(defvar fc-current-card-num nil
  "Numeric index of current flashcard in permutation.")
(defvar fc-current-record nil "Record of current flashcard being drilled on.")

(defvar fc-num-right nil "Number of cards gotten right so far.")
(defvar fc-num-wrong nil "Number of cards gotten wrong so far.")

(defvar fc-permutation nil
  "A permutation representing a shuffling of the cards.")

(defvar fc-question-help
  "Press \\[fc-answer-check] to submit your answer.
\\[fc-options] m to switch to multiple-choice mode."
  "Help string displayed after asking a question.
Command keys are substituted.")

(defvar fc-question-multi-help
  "Press the number of the correct answer,
\\[fc-options] m to switch to single-choice mode."
  "Help string displayed after asking a multiple-choice question.
Command keys are substituted.")

(defvar fc-answer-help
  "Press\t\\[fc-answer-right]\t to submit your answer."
  "Help string displayed after displaying an answer when user has not given up.
Command keys are substituted.")

(defvar fc-answered-help
  "Press \\[fc-answer-check] to go to the next question."
  "Help string displayed after displaying an answer when user has not given up.
Command keys are substituted.")

(defvar fc-answer-multi-help
  "Press\t\\[fc-multi-proceed]\tto proceed as indicated,
\t\\[fc-answer-right]\tto count this one right,
\t\\[fc-answer-wrong]\tto count this one wrong,
\t\\[fc-skip]\tto skip this question."
  "Help string displayed after a multiple-choice answer.
Command keys are substituted.")

(defvar fc-gave-up-help
  "Press\t\\[fc-give-up] again to proceed, or
\t\\[fc-skip] to skip this question."
  "Help string displayed after displaying an answer when user has given up.
Command keys are substituted.")

(defvar fc-done-help
  "Press\t\\[fc-save-scores] to save score data,
\t\\[fc-start-drill] to drill again,
\t\\[fc-quit] to leave Flashcard."
  "Help string displayed when a quiz is finished.
Command keys are substituted.")

(defvar fc-fields-error
  "The question and answer fields are the same.  Use `o q' or `o a'."
  "Error string displayed with question and answer fields are the same.")


(defvar fc-options-alist
  '((?a fc-answer-field fc-field-name)
    (?q fc-question-field fc-field-name)
    (?s fc-auto-shuffle)
    (?f fc-show-other-fields)
    (?p fc-limit-percent)
    (?n fc-limit-num-right)
    (?l fc-limit-last-right)
    (?e fc-filter-equals)
    (?m fc-multiple-choice)
    (?c fc-num-choices))
  "Alist of flashcard options ((CHAR SYM . DISPLAY-FUNC) ...)
where CHAR is the character for option variable SYM, and optional DISPLAY-FUNC
is a function to display the value of the option variable.")


(defvar fc-scores-backup nil "Backup of most recently changed scores.")
(defvar fc-num-right-backup nil "Backup of fc-num-right.")
(defvar fc-num-wrong-backup nil "Backup of fc-num-wrong.")
(defvar fc-current-card-num-backup nil
  "Numeric index of card whose scores are backed up in fc-scores-backup.")
(defvar fc-current-score-field-backup "Backup of fc-current-score-field.")

(defvar fc-choices nil "Sequence of random choices for multiple-choice.")
(defvar fc-current-choice nil "Choice chosen for current question.")
(defvar fc-correct-choice nil "Correct choice for current question.")

(defvar fc-timestamp-file "timestmp"
  "Temporary file used in a hack for getting the current time.")

;; Functions

;; First, use defsubst sometimes if available.
(if (fboundp 'defsubst)
    (fset 'defun-subst 'defsubst)
  (fset 'defun-subst 'defun))

(defun-subst fc-drilling-p ()
  "Return non-nil if drilling has started but not finished."
  (memq fc-state '(question answer-check answer-gave-up)))


(defun-subst fc-num-score-fields ()
  (* fc-num-fields (1- fc-num-fields)))
; = number of ways to drill: e.g. english->kanji, kanji->english,
; kanji->kana, etc.

;; (defun-subst match-string (&optional which)
;;   (if (not which) (setq which 1))
;;   (buffer-substring (match-beginning which) (match-end which)))

;; Data abstraction functions.  Make these defsubst if possible.
(defun-subst fc-get-filename (file-structure)
  (aref file-structure 0))

(defun-subst fc-get-last-card (file-structure)
  (aref file-structure 2))

;; Data abstraction functions.  Make these defsubst if possible.
(defun-subst fc-get-field (record field-num)
  (or (elt (car record) field-num) ""))

(defun-subst fc-make-record (field-values)    ;; field-values is a list.
  (cons field-values (make-vector (fc-num-score-fields) nil)))
 ;; The list of field-values goes in the car.  Scores (nil until used)
 ;; go in the cdr.  Each nil will become a sub-vector of 3 zeroes
 ;; when used.

(defun-subst fc-get-num-right (record) (fc-get-score record 0))
(defun-subst fc-get-num-wrong (record) (fc-get-score record 1))
(defun-subst fc-get-last-right (record) (fc-get-score record 2))

(defun-subst fc-set-num-right (record r) (fc-set-score record 0 r))
(defun-subst fc-set-num-wrong (record w) (fc-set-score record 1 w))
(defun-subst fc-set-last-right (record tm) (fc-set-score record 2 tm))

(defun-subst fc-check-score-field ()
  (if (>= fc-current-score-field 0)
      t
    (error fc-fields-error)))

(defun-subst delete-line ()
  "Delete line point is on.  Does not put deleted text into kill ring.
Does not delete newline."
  (delete-region
   (progn (beginning-of-line) (point))
   (progn (end-of-line) (point))))

(defun-subst fc-insert-help (str)
  "Insert help string STR if fc-display-help is non-nil.
Command keys are substituted."
  (if fc-display-help (insert (substitute-command-keys str))))

;;;###autoload
(defun flashcard (&optional file)
  "Use flashcard package to drill on flashcards."
  (interactive)
;   (list
;    (expand-file-name
;     (read-file-name "Flashcard file: " fc-flashcard-dir nil nil))

  (let ((buf (get-buffer "*Flashcard*")))
    (if buf
        (switch-to-buffer buf)
      (switch-to-buffer (get-buffer-create "*Flashcard*"))
      (flashcard-mode)
      (insert (substitute-command-keys
               "Welcome to Flashcard.  Type \\[describe-mode] for help."))
      (if file
          (fc-load-flashcard-file file)
        (call-interactively 'fc-load-flashcard-file)))))

(defun flashcard-mode ()
;; Need more documentation here.
  "Major mode for drilling on flashcards.

Short list of default key bindings:
f    Load a flashcard file
d    Start drilling on flashcards in memory
s    Save accumulated score data
o    Set flashcard options
o h  Options help
h    Mode help
q    Quit
u    Undo last card

Key bindings:
\\{flashcard-mode-map}
"
  (if (eq major-mode 'flashcard-mode)
      (error "You are already in flashcard-mode.")
    (setq mode-name "Flashcard")
    (setq major-mode 'flashcard-mode)
    (put 'flashcard-mode 'mode-class 'special) ; Now isn't that special...
    (use-local-map flashcard-mode-map)
    (setq tab-width 8) ;; becomes local
    (fc-init)
    (run-hooks 'flashcard-mode-hooks)
    ))

(defun fc-undefined ()
  "Function called when an undefined key sequence is it.  Displays help."
  (interactive)
  (ding)
  (message (substitute-command-keys
            "Type \\[describe-mode] for help.")))

(defun fc-init ()
  "Initialize flashcard package."
  (setq fc-state 'init
        fc-fields nil
        fc-records nil
        fc-records-tail nil
        fc-num-records 0
        fc-num-right 0
        fc-num-wrong 0
        fc-question-field 0
        fc-answer-field 1
        fc-files nil
        fc-files-tail nil
  ))

(defun fc-score-file-name (flashcard-file-name)
  "Make a score file name from a flashcard file name."
  (let ((fname (expand-file-name
                (concat fc-score-dir "/"
                        (file-name-nondirectory flashcard-file-name)))))
    (if (string-match (concat "\\." fc-score-ext "$") fname)
        fname
      (concat fname "." fc-score-ext))))

(defun fc-load-flashcard-file (file-name)
  (interactive
   ;; I would use (interactive "fLoad flashcard file:"), but I want the
   ;; default directory to be right.
   (list
    (expand-file-name
     (read-file-name "Load flashcard file: " (concat fc-flashcard-dir "/") nil t))))
  "Load a flashcard file so it can be drilled on."
  (if (file-directory-p file-name)
      (signal 'file-error (list "Can't load a directory as a flashcard file"
                                file-name)))
  (save-window-excursion
    (set-buffer (get-buffer-create " *flashcard-file*"))
    (erase-buffer)
    (message "Loading...")
    (insert-file-contents file-name)
    (let ((old-tail fc-records-tail)
          (old-num-records fc-num-records)
          (title (fc-parse-region (point-min) (point-max))))
      ;; Turn lists into vectors, for random access speed.
      (if (listp fc-fields) (setq fc-fields (apply 'vector fc-fields)))
      (setq fc-fields-alist
            (let ((i -1))
              (mapcar '(lambda (e) (setq i (1+ i)) (cons e i)) fc-fields)))
      (setq fc-num-fields (length fc-fields))
      ; (if (listp fc-records) (setq fc-records (apply 'vector fc-records)))
      ;; Let fc-records remain a list.  The shuffle, fc-permutation,
      ;; will contain pointers to the various parts of fc-records.
      (fc-add-file file-name title fc-num-records)
      (fc-load-scores-file file-name
                           (if old-tail (cdr old-tail) fc-records)
                           (- fc-num-records old-num-records))
      ))
  (message (substitute-command-keys
            "Loading...done.  Type `\\[fc-start-drill]' to begin drill.")))
    ;; Maybe that should be in the buffer, not a minibuffer message.


(defun fc-save-scores ()
  "Save score data from all loaded files into respective score files."
  (interactive)
  (if (or fc-modified-scores
          (and (interactive-p)
               (y-or-n-p "Scores have not been modified; save anyway? ")))
      (let ((flist fc-files)
            (rlist fc-records)
            (start-from 0))
        (while flist
          (setq rlist
                (fc-save-scores-file
                 (fc-get-filename (car flist)) rlist
                 (- (fc-get-last-card (car flist)) start-from)))
          (setq start-from (fc-get-last-card (car flist)))
          (setq flist (cdr flist)))
        (setq fc-modified-scores nil))))

(defun fc-save-scores-file (fname records nrec)
  "Save score data for a given file into its proper score file."
  (save-window-excursion
    (set-buffer (get-buffer-create " *flashcard-scores*"))
    (erase-buffer)
    (while (> nrec 0)
      (fc-insert-score-data (car records))
      (setq nrec (1- nrec))
      (setq records (cdr records)))
    (if (not (file-directory-p fc-score-dir))
        (fc-make-directory fc-score-dir))
    (write-region (point-min) (point-max)
                  (fc-score-file-name fname))
    records))

(defun fc-insert-score-data (record)
  "Insert numerical score data for RECORD at point in current buffer."
  (let ((fc-current-score-field 0))
    (while (< fc-current-score-field (fc-num-score-fields))
      (let ((nr (fc-get-num-right record))
            (nw (fc-get-num-wrong record))
            (nl (fc-get-last-right record)))
        (if (or (> nr 0) (> nw 0) (> nl 0))
            (insert (format "%d %d %d," nr nw nl)) ;; make these %x
          (insert ","))
        )
      (setq fc-current-score-field (1+ fc-current-score-field))))
  (delete-backward-char 1) ;; change last comma to a newline
  (newline))


(defun fc-load-scores-file (fname records nrec)
  "Load score data for a given file from its score file."
  (let ((sfname (fc-score-file-name fname)))
    (if (not (file-readable-p sfname))
        nil
      (save-window-excursion
        (set-buffer (get-buffer-create " *flashcard-scores*"))
        (erase-buffer)
        (insert-file-contents sfname)
        (while (and (> nrec 0) (not (eobp)))
          (fc-read-score-data (car records))
          (setq nrec (1- nrec))
          (setq records (cdr records)))))))

(defun fc-read-score-data (record)
  "Read score data at point into RECORD."
  (let ((fc-current-score-field 0))
    (while (and
            (< fc-current-score-field (fc-num-score-fields))
            (not (eobp)))
      (if (looking-at "\\([0-9]+\\) \\([0-9]+\\) \\([0-9]+\\)")
          (let ((nr (string-to-int (match-string 1)))
                (nw (string-to-int (match-string 2)))
                (nl (string-to-int (match-string 3))))
            (fc-set-num-right record nr)
            (fc-set-num-wrong record nw)
            (fc-set-last-right record nl)
            (goto-char (1+ (match-end 0))))
        (forward-char)) ;; skip comma or newline
      (setq fc-current-score-field (1+ fc-current-score-field)))))

(defun fc-parse-region (min max)
  "Parse flashcard records in region MIN to MAX in current buffer.
Return title of file if found."
  (let ((title ""))
    (goto-char min)
    (while (< (point) max)
      (cond
       ((looking-at "^!% *Title: *\\([^\n]+\\)$")
        (setq title (match-string 1)))
       ((looking-at "^!% *Fields: *\\([^\n]+\\) *$")
        (let ((fields-string (match-string 1))
              (new-fields
               (fc-parse-record fc-field-delimiter
                                (match-beginning 1) (match-end 1) t)))
          (if fc-fields
              (if (not (equal-sequence new-fields fc-fields))
                  (error "New fields %s differ from old fields %s"
                         new-fields fc-fields))
            (setq fc-fields new-fields)
            (setq fc-num-fields (length fc-fields))
            )))
       ((looking-at "^!% *\\([^:\n]*\\)")
        ;; error
        (error "Unknown !%% line: %s" (match-string 1)))
       ((looking-at "^!"))                ; Ignore comments.
       ((looking-at "^[ \t]*$"))          ; Ignore blank lines.
       (t                                 ;Anything else is a flashcard record.
        (if fc-fields
            t
          (setq fc-fields fc-default-fields)
          (setq fc-num-fields (length fc-fields)))
        (fc-add-record
         (save-excursion (fc-parse-record fc-field-delimiter (point) max))))
       ) ;; end cond
      (if (re-search-forward fc-record-delimiter max 1)
          (goto-char (match-end 0)))
      ;;...
      ;; kill buffer?
      )
    title))

(defun fc-parse-record (delim min max &optional not-record)
  "Parse and record beginning at MIN, using arg DELIM as a field delimiter.
Arg MAX limits end of record.  Return as a list if optional third arg
NOT-RECORD is non-nil; otherwise, return as a record."
  (goto-char min)
  (let* ((pt (point))
         (eor                           ; find end of record
          (if (re-search-forward fc-record-delimiter max t)
              (match-beginning 0)
            max))
         (fields nil)
         (next nil))
    ;; Don't check for a correct number of fields.
    (goto-char pt)
    (while (< pt eor)
      (if (re-search-forward delim eor 1)
          (setq next (match-end 0)
                field-end (match-beginning 0))
        (setq next eor
              field-end eor))
      (if (> (point) pt)
          (let ((string (buffer-substring pt field-end)))
            (setq fields (nconc fields
                                (cons
;;                                 (if not-record (intern string) string)
                                 string
                                 nil))
                  pt next))))
    (if not-record
        fields                          ; Just return the list.
      (fc-make-record fields))))        ; Make a full record.

(defun fc-add-record (record)
  "Add RECORD to fc-records data structure."
  (if fc-records
      (progn
        (setcdr fc-records-tail (cons record nil))
        (setq fc-records-tail (cdr fc-records-tail)))
    (setq fc-records (cons record nil))
    (setq fc-records-tail fc-records))
  (setq fc-num-records (1+ fc-num-records)))

(defun fc-add-file (file title numlastcard)
  "Add [FILE TITLE NUMLASTCARD] to fc-files data structure."
  (let ((newcell (cons (vector file title numlastcard) nil)))
    (if fc-files
        (progn
          (setcdr fc-files-tail newcell)
          (setq fc-files-tail (cdr fc-files-tail)))
      (setq fc-files newcell)
      (setq fc-files-tail fc-files)))
  ;; (setq fc-num-files (1+ fc-num-files))
  )

(defun fc-get-score (record n)
  (let ((scores (aref (cdr record) fc-current-score-field)))
    (if scores
        (aref scores n)
      0)))

(defun fc-set-score (record n v)
  (let ((scores (aref (cdr record) fc-current-score-field)))
    (if scores
        (aset scores n v)
      (let ((scores (make-vector 3 0)))
        (aset scores n v)
        (aset (cdr record) fc-current-score-field scores)))))

(defun fc-backup-scores ()
  "Backup enough data about current card/scores for undoing score changes."
  (setq fc-current-card-num-backup fc-current-card-num
        fc-num-right-backup fc-num-right
        fc-num-wrong-backup fc-num-wrong
        fc-current-score-field-backup fc-current-score-field
        fc-scores-backup (copy-sequence
                          (aref (cdr fc-current-record)
                                fc-current-score-field))
        ))

(defun goto-line-create (n)
  (goto-line n)
  (let ((d (- n (count-lines (point-min) (point)))))
    (if (not (bolp)) (setq d (1+ d)))
    (if (> d 1)
        (newline (1- d)))))

(defun fc-set-buffer-lines ()
  "Determine where to display things in the buffer."
  (setq fc-question-line 5              ; constant.
        fc-answer-line (+ fc-question-line 3
                          (if fc-multiple-choice fc-num-choices 0))
        fc-help-line
        (+ fc-answer-line 3
           (if fc-show-other-fields
               (- fc-num-fields 2)
             0))))

(defun fc-start-drill (&optional arg)   ;; finally, the meat.
  "Begin drilling on flashcards currently loaded."
  (interactive "P")
  (if (< fc-num-records 1)
      (error "You must load a flashcard file before drilling."))
  (fc-set-buffer-lines)
  ;; Reset overall scores.
  (setq fc-num-right 0
        fc-num-wrong 0)
  (setq fc-current-score-field
        (fc-compute-score-field fc-question-field fc-answer-field))
  (fc-shuffle)
  ;; We set the current card to -1 so the fc-next-card starts searching
  ;; for the next valid card at index 0.
  (setq fc-current-card-num -1)
  (fc-next-card))

(defun fc-shuffle ()
  "Create a permutation that represents a shuffling of the cards."
  (if (or (not fc-permutation) (< (length fc-permutation) fc-num-records))
      (setq fc-permutation (make-vector fc-num-records nil)))
  (let ((i 0) j tmp (records-left fc-records))
    (while (< i fc-num-records)
      (aset fc-permutation i (car records-left))
      (setq i (1+ i)
            records-left (cdr records-left)))
    (if (not fc-auto-shuffle)
        nil
      (random t) ;; Seed random number generator.
      (setq i 0)
      (while (< i fc-num-records)
        (setq j (mod (abs (random)) fc-num-records))
        ;; I'm not sure if abs is always defined in emacs 18.  If not,
        ;; (defun abs (a) (if (< a 0) (- a) a))
        ;; in emacs-19, (setq j (random fc-num-records))
        ;; j could = i; doesn't matter.
        (setq tmp (aref fc-permutation i))
        (aset fc-permutation i (aref fc-permutation j))
        (aset fc-permutation j tmp)
        (setq i (1+ i))))))

(defun fc-do-current-card ()
  "Start processing on current card."
  (erase-buffer)
  (setq fc-current-choice nil)
  (if fc-multiple-choice
      (fc-make-choices))
  (fc-display-question)
  (setq fc-state 'question))

(defun fc-make-choices ()
  "Prepare a sequence of random choices for multiple choice."
;  (if (or (not fc-choices) (< (length fc-choices) fc-num-choices))
;      (setq fc-choices (make-vector fc-num-choices -1)))
  (setq fc-choices nil
        fc-current-choice nil
        fc-correct-choice (mod (abs (random)) fc-num-choices))
  ;; avoid infinite loop
  (if (< fc-num-records fc-num-choices)
      (error "Can't pick %d choices out of only %d records."
             fc-num-choices fc-num-records))
  (let ((i 0)
        n
        (inv-cc (- fc-num-choices fc-correct-choice 1)))
    ;; inv-cc is a mirror image of fc-correct-choice, for building the
    ;; list backwards.
    (while (< i fc-num-choices)
      (if (= i inv-cc)
          (setq n fc-current-card-num)
        (setq n (mod (abs (random)) fc-num-records))
        (while (or (= n fc-current-card-num) (memq n fc-choices))
          (setq n (mod (abs (random)) fc-num-records))))
      (setq fc-choices (cons n fc-choices))
      (setq i (1+ i)))))

(defun fc-display-question ()
  "Display current question and question help."
  (let* ((question (fc-get-field fc-current-record fc-question-field))
         (cok (/= fc-question-field fc-answer-field))
         (cr (if cok (fc-get-num-right fc-current-record) nil))
         (cw (if cok (fc-get-num-wrong fc-current-record) nil))
         (nc (if fc-multiple-choice fc-num-choices 0))
         (mark (make-marker)))
    (goto-line-create 1)
    (insert (format
             "Card #%d of %d    Overall: %d r %d w (%d%%)"
             (1+ fc-current-card-num) fc-num-records
             fc-num-right fc-num-wrong (fc-percent fc-num-right fc-num-wrong)))
    (if cok
        (insert (format "    This card: %d r %d w (%d%%)"
                        cr cw (fc-percent cr cw))))
    (goto-line-create fc-help-line)
    ;; The above kludge is to assure that there are already newlines
    ;; after where the question will be.  If we allowed the newlines
    ;; to be added later, they could inherit text properties from the
    ;; question.  Is there a less kludgy way to avoid this?
    (goto-line-create fc-question-line)
    (insert (format "Question (%s): " (elt fc-fields fc-question-field)))
    (fc-insert-with-face question fc-question-face)
    (if fc-multiple-choice (fc-display-choices))
    (goto-line-create fc-answer-line)
    (insert (format "Answer (%s): " (elt fc-fields fc-answer-field)))
    ;; save the start of a user's answer as a marker, for later retrieval
    (set-marker mark (point))
    (put 'fc-answer-line 'user-input-start mark)
    (save-excursion
      (if (and cok (eq fc-show-other-fields t))
          (progn
            (goto-line-create (1+ fc-answer-line))
            (fc-insert-other-fields fc-current-record)))
      (goto-line-create fc-help-line);; skip answer space
      (fc-insert-help
       (if fc-multiple-choice fc-question-multi-help fc-question-help))
      ;; Make sure the question line is still visible.
      (goto-line fc-question-line)
      (beginning-of-line))))
;;       (if (> (window-start) (point)) ;; question line has scrolled off
;;           (set-window-start (selected-window) (point)))))

;; (defun fc-redisplay-question-line ()
;;   "Update question line in Flashcard buffer."
;;   (let ((question (fc-get-field fc-current-record fc-question-field)))
;;     (goto-line-create fc-question-line)
;;     (delete-line)
;;     (insert (format "Question (%s): " (elt fc-fields fc-question-field)))
;;     (fc-insert-with-face question fc-question-face)))

(defun fc-display-choices ()
  "Display choices for multiple-choice question.  Uses fc-choices."
  (goto-line-create (1+ fc-question-line))
  (let ((ch fc-choices) (i 0))
    (while ch
      (let* ((ir (car ch))
             (answer (fc-get-field (aref fc-permutation ir) fc-answer-field)))
        (insert (format "\t%d. %s\n" (1+ i) answer)))
      (setq ch (cdr ch)
            i (1+ i)))))

(defun fc-answer-check ()
  "User command called when user knows answer to question, or wants to move to
  the next question."
  (interactive)
  (cond
   ((eq fc-state 'question)
    (fc-check-score-field)
    (setq fc-state 'answer-check)
    (fc-check-answer))
   ((eq fc-state 'answer-check)
    (fc-next-card))
   (t
    ;(if (or (not (eq fc-state 'question)) fc-multiple-choice)
      (ding))))

(defun fc-choose ()
  "User command to answer multiple choice question.
This function should be called with a key sequence ending in a digit."
  (interactive)
  (if (not (eq fc-state 'question))
      (ding)
    (let* ((choice (string-to-int (char-to-string last-command-char))))
      (if (or (zerop choice) (> choice fc-num-choices))
          (ding)
        (setq fc-current-choice (1- choice))
        (fc-check-score-field)
        (setq fc-state 'answer-check)
        (fc-check-answer)))))

(defun fc-give-up ()
  "User command to give up on current card."
  (interactive)
  (cond
   ((eq fc-state 'question)
    (fc-check-score-field)
    (setq fc-state 'answer-gave-up)
    (fc-display-answer))
   ((eq fc-state 'answer-gave-up)
    ;; (fc-check-score-field) ; not necess. since fc-answer-wrong will check.
    (fc-answer-wrong))
   (t (ding))))

(defun fc-display-answer ()
  "Display current answer and answer help.
Assumes that fc-display-question has already been called."
  (interactive)
  (let ((answer (fc-get-field fc-current-record fc-answer-field)))
    (if fc-multiple-choice
        (progn
          ;; Urg.  fc-current-choice might not be set because we might
          ;; have just switched to multi-choice mode in mid-question.
          ;; So do we assume they got it right or wrong??
          ;; Actually, we can hack it to "skip" this question (unless
          ;; the user forces 'r' or 'w') by leaving fc-current-choice at nil.
          (if (not fc-current-choice) nil
            (goto-line-create (+ fc-question-line fc-current-choice 1))
            (beginning-of-line)
            (insert
             (if (= fc-current-choice fc-correct-choice)
                 "Right!" "Wrong."))
            (goto-line-create (+ fc-question-line fc-correct-choice 1))
            (re-search-forward "[0-9]\. ")
            (fc-apply-face-region fc-answer-face (match-beginning 0)
                                  (progn (end-of-line) (point))))))
    (goto-line-create fc-answer-line)
    (end-of-line)
    (fc-insert-with-face answer fc-answer-face)
    (if (and fc-show-other-fields (not (eq fc-show-other-fields t)))
        ;; if fc-show-other-fields is nil, we don't show them, but
        ;; if it's t, we already displayed them during fc-display-question.
        (progn
          (goto-line-create (1+ fc-answer-line))
          (fc-insert-other-fields fc-current-record)))
    (goto-line-create fc-help-line)
    (delete-region (point) (point-max)) ;; clear out old help
    (cond
     ((eq fc-state 'answer-check)
      (fc-insert-help (if fc-multiple-choice
                          fc-answer-multi-help fc-answer-help)))
     ((eq fc-state 'answer-gave-up)
      (fc-insert-help fc-gave-up-help))
     (t
      (error "Unexpected state %s in fc-display-answer" fc-state)))
    (goto-line fc-question-line)
    (beginning-of-line)
    (if (> (window-start) (point)) ;; question line has scrolled off
        (set-window-start (selected-window) (point)))
    ))

(defun fc-check-answer ()
  "Compare the user-provided answer with the real answer.
Assumes that fc-display-question has already been called."
  (interactive)
  (let ((question (fc-get-field fc-current-record fc-question-field))
        (answer (fc-get-field fc-current-record fc-answer-field))
        (user-answer-start (get 'fc-answer-line 'user-input-start))
        user-answer user-correct)

    (if fc-multiple-choice
          ;; Urg.  fc-current-choice might not be set because we might
          ;; have just switched to multi-choice mode in mid-question.
          ;; So do we assume they got it right or wrong??
          ;; Actually, we can hack it to "skip" this question (unless
          ;; the user forces 'r' or 'w') by leaving fc-current-choice at nil.
          (if (not fc-current-choice) nil
             (setq user-correct (= fc-current-choice fc-correct-choice)))

      ;; extract the user's answer
      (end-of-line)
      (setq user-answer (buffer-substring user-answer-start (point)))
      (if (string= answer user-answer)
            (setq user-correct t)))

    (fc-answer-result user-correct)

    (goto-line fc-question-line)
    (kill-line)

    (if user-correct
        (fc-insert-with-face (format "Correct! %s is %s." question answer)
                             fc-answer-face)
      (fc-insert-with-face (format "Sorry. %s is %s, not %s" question answer
                                   user-answer)
                           fc-answer-face))


;    (fc-insert-with-face answer fc-answer-face)
    (if (and fc-show-other-fields (not (eq fc-show-other-fields t)))
        ;; if fc-show-other-fields is nil, we don't show them, but
        ;; if it's t, we already displayed them during fc-display-question.
        (progn
          (goto-line-create (1+ fc-answer-line))
          (fc-insert-other-fields fc-current-record)))

    (goto-line-create fc-help-line)
    (delete-region (point) (point-max)) ;; clear out old help
;;     (cond
;;      ((eq fc-state 'answer-check)
;;       (fc-insert-help (if fc-multiple-choice
;;                           fc-answer-multi-help fc-answer-help)))
;;      ((eq fc-state 'answer-gave-up)
;;       (fc-insert-help fc-gave-up-help))
;;      (t
;;       (error "Unexpected state %s in fc-display-answer" fc-state)))
;;     (goto-line fc-question-line)
;;     (beginning-of-line)
;;     (if (> (window-start) (point)) ;; question line has scrolled off
;;         (set-window-start (selected-window) (point)))
;;     ))

    (if user-correct
        (progn
          (sit-for 0 100)
          (fc-next-card))
      (fc-insert-help fc-answered-help))))

(defun fc-redisplay-answer-line ()
  "Update answer line in Flashcard buffer, as well as other fields."
  (let ((answer (fc-get-field fc-current-record fc-answer-field))
        (answer-state (memq fc-state '(answer-check answer-gave-up))))
    (goto-line-create fc-answer-line)
    (delete-line)
    (insert (format "Answer (%s): " (elt fc-fields fc-answer-field)))
    (if answer-state
        (fc-insert-with-face answer fc-answer-face))
    (goto-line-create (1+ fc-answer-line))
    (delete-line)
    (if (and fc-show-other-fields
             (or answer-state (eq fc-show-other-fields t)))
        (fc-insert-other-fields fc-current-record))))

(defun fc-skip ()
  "User command to skip current card and proceed to next one."
  (interactive)
  (fc-next-card)) ;; Don't modify score.

(defun fc-undo ()
  "Go back to the last card drilled on, undoing its effects on scoring."
  (interactive)
  (if (not fc-current-card-num-backup)
      (error "Sorry, no backup data available."))
  (fc-check-score-field)
  (if (/= fc-current-score-field fc-current-score-field-backup)
      (error "Can't undo past changed question/answer fields."))
  (setq fc-current-card-num fc-current-card-num-backup
        fc-current-card-num-backup nil ;; no more backup data left
        fc-num-right fc-num-right-backup
        fc-num-wrong fc-num-wrong-backup
        fc-current-record (aref fc-permutation fc-current-card-num)
        )
  (aset (cdr fc-current-record) fc-current-score-field fc-scores-backup)
  ;; go back and display the undone card.
  (fc-do-current-card))

(defun fc-next-card ()
  "Proceed to next card."
  (fc-check-score-field)
  (let ((found nil))
    (while (and (< fc-current-card-num (1- fc-num-records)) (not found))
      (setq fc-current-card-num (1+ fc-current-card-num))
      (setq fc-current-record (aref fc-permutation fc-current-card-num))
      (if (fc-meets-criteria fc-current-record)
          (setq found t)))
    (if (not found)
        (fc-drill-done)
      (fc-do-current-card))))

(defun fc-meets-criteria (record)
  "Return t if RECORD falls within drill limits."
  (let ((cr (fc-get-num-right record))
        (cw (fc-get-num-wrong record))
        (cl (fc-get-last-right record)))
    (not   ;; t if does not fail any criterion
     (or    ;; t if fails any criterion
      (and fc-limit-num-right (> cr fc-limit-num-right))
      (and fc-limit-percent (> (fc-percent cr cw) fc-limit-percent))
      (and fc-limit-last-right (> cl 0)
               (< (fc-num-days-ago cl) fc-limit-last-right))
      (and fc-filter-equals
           (equal (fc-get-field record fc-question-field)
                  (fc-get-field record fc-answer-field)))
      ))))

(defun fc-answer-result (correct)
  "Update stats depending on if the answer was correct or not,
and move on to the next card"
  (fc-check-score-field)
  (fc-backup-scores)

  (if correct
      (progn
        (setq fc-num-right (1+ fc-num-right))
        ;; Also keep data on this particular card.
        (fc-set-num-right fc-current-record
                          (1+ (fc-get-num-right fc-current-record)))
        (fc-set-last-right fc-current-record (fc-timestamp)))
    (setq fc-num-wrong (1+ fc-num-wrong))
    ;; Also keep data on this particular card.
    (fc-set-num-wrong fc-current-record
                      (1+ (fc-get-num-wrong fc-current-record))))

  (or fc-modified-scores (setq fc-modified-scores t)))

(defun fc-answer-right ()
  (interactive)
  (message "obsoleted"))

(defun fc-answer-wrong ()
  (interactive)
  (message "obsoleted"))


(defun fc-multi-proceed ()
  "User command to score the current card according to the selected choice."
  (interactive)
  (if (not (and fc-multiple-choice (eq fc-state 'answer-check)))
      (ding)
    (cond
     ((null fc-current-choice) ;; If no choice was made, do nothing.
      (fc-next-card))
     ((= fc-current-choice fc-correct-choice)
      (fc-answer-right))
     (t
      (fc-answer-wrong)))))

(defun fc-insert-other-fields (card)
  "Insert labels and contents of other fields on current line."
  (let ((field-num 0))
    (while (< field-num fc-num-fields)
      (if (and
           (/= field-num fc-question-field)
           (/= field-num fc-answer-field))
          (insert (format "  (%s): %s"
                          (elt fc-fields field-num)
                          (fc-get-field card field-num))))
      (setq field-num (1+ field-num)))))

(defun fc-insert-with-face (string face)
  "Insert STRING at point, setting its text property in the buffer to FACE.
See also fc-apply-face-region."
  (let ((oldpoint (point)))
    (insert string)
    (fc-apply-face-region face oldpoint (point))))

(defun fc-apply-face-region (face from to)
  "Apply FACE to region FROM TO.
If FACE is nil or typefaces are not available, do nothing.
Use put-text-property if available; otherwise, if attribute-on-region
is available, FACE must be one of the symbols bold, inverse, or underline."
    (cond
     ((null face) nil)                  ;; Do nothing if face is nil.
     ((fboundp 'put-text-property)      ;; Apply text property if possible
      (put-text-property from to 'face face)) ;; (in Emacs 19)
     ((fboundp 'attribute-on-region)    ;; Use attributes (in Mule)
      (attribute-on-region face from to))))

(defun fc-drill-done ()
  "Function called when drill is finished."
  (fc-display-options)
  (goto-line-create fc-question-line)
  (insert (substitute-command-keys
           "Drill done.  Press `\\[fc-save-scores]' to save score data."))
  (setq fc-state 'init))

(defun fc-display-options ()
  "Displays flashcard option screen."
  (erase-buffer)
  (goto-line-create 1)
  (insert (format "Stats so far: %d right, %d wrong (%d%%)"
                  fc-num-right fc-num-wrong
                  (fc-percent fc-num-right fc-num-wrong)))
  ; ... etc.
  ; options:
  ; <q>uestion field, <a>nswer field (change with completing read)
  ; limits on drill: <p>ercentage right, <n>umber right, <l>ast time
  ; gotten right
  ; [fc-show-other-fields (probably not on options screen)]
  ; <m>ultiple-choice (boolean)
  (goto-line-create fc-help-line)
  (fc-insert-help fc-done-help))

(defun equal-sequence (a b)
  "Determine whether sequences A and B contain equal objects."
  ;; like equal, except that any type of sequence can be equal-sequence
  ;; to any other type.
  (catch 'is-equal
    (or (equal a b)
        (let ((lena (length a))
              (lenb (length b))
              (i 0))
          (if (/= lena lenb) nil
            (while (< i lena)
              (if (not (equal (elt a i) (elt b i)))
                  (throw 'is-equal nil))
              (setq i (1+ i)))
            t)))))

; Must supply a hacked current-time for pre-emacs-19.  Here's a better
; alternative than the call to perl that used to be used here: modify
; a file using write-region, then use file-attributes to get the last
; modification time, as two 16-bit integers.  It's still bad in that
; it messes with the file system unnecessarily; but it's a lot better
; than calling perl.

(if (not (fboundp 'current-time))
    (defun current-time ()
"Return the current time, as the number of seconds since 12:00 AM January 1970.
The time is returned as a list of two integers.  The first has the
most significant 16 bits of the seconds, while the second has the
least significant 16 bits."
      (let* ((time-file (concat fc-score-dir "/" fc-timestamp-file))
             (file-attr (progn
                          (if (not (file-directory-p fc-score-dir))
                              (fc-make-directory fc-score-dir))
                          ;; cause file to be modified.
                          (write-region 1 1 time-file nil 1)
                          (file-attributes time-file)))
             (mod-time (if file-attr (elt file-attr 5) nil)))
        (if mod-time mod-time '(0 0)))))

(if (not (fboundp 'abs))
    (defun abs (a) (if (< a 0) (- a) a)))

;; We only take the 20 most significant bits.  This gets us down
;; almost to within an hour.
(defun fc-timestamp ()
  (let ((ct (current-time)))
    (logior (lsh (car ct) 4) (lsh (car (cdr ct)) -12))))

(defun fc-num-days-ago (timestamp)
  (let ((now (fc-timestamp)))
    ;; Timestamp is number of seconds since 1/1/70, with the last
    ;; 12 bits chopped off.  That's seconds/4096.  A day is 24*3600 seconds.
    ;; 4096 / (24*3600) = 32 / 675
    (/ (* (- now timestamp) 32) 675)))

;; Calculate percentage... a is what percent of a + b?
;; Don't die on division by zero.  Round to nearest integer.
(defun fc-percent (a b)
  (if (zerop (+ a b))
      0        ;; So that questions that have never been answered
               ;; are counted as having a "right" percentage of zero.
    (/ (+ (* 100 a) (/ (+ a b) 2))
       (+ a b))))

(defun fc-compute-score-field (qfield afield)
  "Find the score field used to record scores drilling from QFIELD to AFIELD."
  (if (= qfield afield)
      (error fc-fields-error))
  (+ (* qfield (1- fc-num-fields)) afield
     (if (< qfield afield) -1 0)))
;; Example: fields are:  kanji, hiragana, english
;; Order of score fields is: kanji->hiragana, kanji->english,
;; hiragana->kanji, hiragana->english, english->kanji, english->hiragana.
;; (fc-compute-score-field (hiragana=1 english=2))
;; => (+ (* 1 (1- 3)) 2 -1) == 3

(defun fc-options ()
  "Set Flashcard options."
  ;; This is a shortcut; we really want an options display screen.
  (interactive)
  (if (fc-sit-for 1 t)
      (message "Flashcard option: (aqsfprlemc or ? for help) - "))
  (let ((done nil))
    (while (not done)
      (let ((ch (read-char)))
        (setq done t)
        (cond
         ;; abort options
         ((eq ch ?\C-g) t)
         ((eq ch ?\C-m) t)            ; RET also aborts.
         ;; options help
         ((eq ch ??) (fc-display-option-help) (setq done nil))
         ;; set answer field
         ((eq ch ?a) (fc-set-answer-field))
         ;; set question field
         ((eq ch ?q) (fc-set-question-field))
         ;; toggle fc-auto-shuffle
         ((eq ch ?s)
          (setq fc-auto-shuffle (not fc-auto-shuffle))
          (message "fc-auto-shuffle is now %s" fc-auto-shuffle))
         ;; set fc-show-other-fields
         ((eq ch ?f)
          (setq fc-show-other-fields
                (cond ((null fc-show-other-fields) 'answer-only)
                      ((eq fc-show-other-fields t) nil)
                      (t t)))
          (message "fc-show-other-fields is now %s" fc-show-other-fields)
          (if (fc-drilling-p) (fc-redisplay-answer-line)))
         ;; set fc-limit-percent
         ((eq ch ?p)
          (let ((newval (read-minibuffer
                         (format "Max percent right (nil for no limit): (%s) "
                                 fc-limit-percent))))
            (if (and
                 (or (not newval)
                     (and (numberp newval) (<= newval 100) (>= newval 0)))
                 (not (eq newval fc-limit-percent))) ; note, (eq 1 1) => t
                (progn
                  (setq fc-limit-percent newval)
                  (message "fc-limit-percent is now %s" newval)))))
         ;; set fc-limit-num-right
         ((eq ch ?n)
          (let ((newval
                 (read-minibuffer
                  (format "Max number of times right (nil for no limit): (%s) "
                          fc-limit-num-right))))
            (if (and
                 (or (not newval)
                     (and (numberp newval) (>= newval 0)))
                 (not (eq newval fc-limit-num-right)))
                (progn
                  (setq fc-limit-num-right newval)
                  (message "fc-limit-num-right is now %s" newval)))))
         ;; set fc-limit-last-right
         ((eq ch ?l)
          (let ((newval
                 (read-minibuffer
                  (format
                   "Min days since last time right (nil for no limit): (%s) "
                   fc-limit-last-right))))
            (if (and
                 (or (not newval)
                     (and (numberp newval) (>= newval 0)))
                 (not (eq newval fc-limit-last-right)))
                (progn
                  (setq fc-limit-last-right newval)
                  (message "fc-limit-last-right is now %s" newval)))))
         ;; toggle fc-filter-equals
         ((eq ch ?e)
          (setq fc-filter-equals (not fc-filter-equals))
          (message "fc-filter-equals is now %s" fc-filter-equals))
         ;; toggle fc-multiple-choice
         ((eq ch ?m)
          (setq fc-multiple-choice (not fc-multiple-choice))
          (message "fc-multiple-choice is now %s" fc-multiple-choice)
          (fc-set-buffer-lines)
          (if (fc-drilling-p)
              (progn
                (if fc-multiple-choice (fc-make-choices))
                (fc-check-qa-fields)))) ;; really for redisplay.
         ;; set fc-num-choices
         ((eq ch ?c)
          (let ((newval (read-minibuffer
                         (format "Number of choices in multiple-choice: (%s) "
                                 fc-num-choices))))
            (if (and (numberp newval) (> newval 0) (/= newval fc-num-choices))
                (progn
                  (setq fc-num-choices newval)
                  (message "fc-num-choices is now %s" newval)
                  (fc-set-buffer-lines)
                  (if (and (eq fc-state 'question) fc-multiple-choice)
                      (progn
                        (fc-make-choices)
                        (fc-check-qa-fields))) ;; redisplay
                  ))))
         ;; display doc string for an option
         ((eq ch ?h)
          (fc-display-option-help (read-char)))
         (t (ding))
         )))))

(defun fc-display-option-help (&optional char)
  "Display help for flashcard options.

If optional argument CHAR is non-nil (a character), display the
documentation string for the corresponding option in the
*Help* buffer.  Otherwise, display general option help."
  (interactive)                         ; why not.
  (if char
      (let ((option (assq char fc-options-alist)))
        (if option
            (describe-variable (car (cdr option)))
          (ding)))

    (with-output-to-temp-buffer "*Help*"
      (princ "\
Flashcard Options:  (for specific help, type `h' and the option letter)

Key\tOption\t\t\tCurrent value
===\t======\t\t\t=============\n")
      (let ((vars fc-options-alist))
        (while vars
          (let* ((next-var (car vars))
                 (key-char (car next-var))
                 (sym (car (cdr next-var)))
                 (val (symbol-value sym))
                 (first-part (format "%c\t%s" key-char sym)))
            (princ first-part)
            (princ (make-string (- 26 (length first-part)) ?\ ))
                                        ; space to column 40
            (princ
             (if (cdr (cdr next-var))
                 (apply (car (cdr (cdr next-var))) val nil)
               val))
            (princ "\n"))
          (setq vars (cdr vars)))))))

(defun fc-set-question-field ()
  "Prompt for a field name and set the question field to it."
  (if (> fc-num-fields 2)
      (let* ((str
              (completing-read
               (format "Set question field to: (%s) "
                       (elt fc-fields fc-question-field))
               fc-fields-alist nil t))
             (qnum (cdr (assoc str fc-fields-alist))))
        (if (or (null qnum) (= qnum fc-question-field))
            nil
          (setq fc-question-field qnum)
          (fc-check-qa-fields)
          (message "Question field is now %s" str)))
    (fc-swap-qa-fields)))

(defun fc-set-answer-field ()
  "Prompt for a field name and set the answer field to it."
  (if (> fc-num-fields 2)
      (let* ((str
              (completing-read
               (format "Set answer field to: (%s) "
                       (elt fc-fields fc-answer-field))
               fc-fields-alist nil t))
             (anum (cdr (assoc str fc-fields-alist))))
        (if (or (null anum) (= anum fc-answer-field))
            nil
          (setq fc-answer-field anum)
          (fc-check-qa-fields)
          (message "Answer field is now %s" str)))
    (fc-swap-qa-fields)))

(defun fc-swap-qa-fields ()
  "Swap question and answer fields."
  (let ((old-qfield fc-question-field))
    (setq fc-question-field fc-answer-field)
    (setq fc-answer-field old-qfield))
  (fc-check-qa-fields)
  (message "Swapped question and answer fields (now %s -> %s)"
           (elt fc-fields fc-question-field)
           (elt fc-fields fc-answer-field)))

(defun fc-check-qa-fields ()
  "Update the situation based on new fc-question/answer-field."
  (if (not (fc-drilling-p))
      nil                  ; do nothing; it'll be done when we start.
    (if (= fc-question-field fc-answer-field)
        (progn
          (setq fc-current-score-field -1)
          (erase-buffer)
          (fc-display-question)
          ;; (fc-redisplay-question-line)
          (fc-redisplay-answer-line))
      (setq fc-current-score-field
            (fc-compute-score-field fc-question-field fc-answer-field))
      (erase-buffer)
      (fc-display-question)
      (if (memq fc-state '(answer-check answer-gave-up))
          (fc-display-answer)))))

(if (string< emacs-version "19")
    (fset 'fc-sit-for 'sit-for)
  (defun fc-sit-for (secs &optional no-redisplay)
    "Same as sit-for under emacs-18.59: No millisecond argument."
    (sit-for secs nil no-redisplay)))

;; Has to be a function(?) because it gets passed around as a value.
(defun fc-field-name (f)
  (or (elt fc-fields f) f))

(defun fc-clear-data (&optional quitting-p)
  "Clear flashcards and score data from memory.
Confirmation is required if score data has been modified and not saved.
Optional arg QUITTING-P, if non-nil, means use `quit' instead of `clear'
in confirmation message.  Returns t if data was cleared, nil otherwise."
  (interactive "P")
  (if (and fc-modified-scores
           (y-or-n-p "Score data is modified.  Save it? "))
      (fc-save-scores))
  (if (or (not fc-modified-scores)
          (yes-or-no-p (format "Score data will be lost.  Really %s? "
                               (if quitting-p "quit" "clear"))))
      (progn
        (fc-init)
        (erase-buffer)
        (insert
         (substitute-command-keys
          "All data cleared.  Press `\\[fc-load-flashcard-file]'")
         " to load a new flashcard file.")
        t)
    nil))

(defun fc-quit ()
  "Quit Flashcard.  Clear all data and kill *Flashcard* buffer.
If there is unsaved score data, user will be queried to save it."
  (interactive)
  (if (fc-clear-data t)  ;; save score data and/or confirm.
      (kill-buffer "*Flashcard*")))

(defun fc-make-directory (dir)
  "Create directory DIR."
  (if (fboundp 'make-directory)
      (make-directory dir t)
    (call-process "mkdir" nil (get-buffer-create " *mkdir-lossage*")
                  nil (expand-file-name dir))))

(provide 'flashcard)
