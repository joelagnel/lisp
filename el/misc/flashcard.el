;;; flashcard.el --- An extensible fact-learning program

;; Copyright (C) 2004, 2005, 2006  Jorgen Schaefer

;; Author: Jorgen Schaefer
;; Keywords: applications, flashcard
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl/FlashCard

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; This mode helps you learning facts. To use it, just put the
;; following in your .emacs file:

;; (add-to-list 'auto-mode-alist '("\\.deck\\'" . flashcard-mode))

;; Now open any file with a .deck extension (a new file will do). You
;; will now be put into flashcard. The first thing you need to do is
;; to add cards from any file. Example:

;; M-x flashcard-import-from-colon-file

;; Now you should get questions asked from flashcard. Just save and
;; kill the buffer as usual. If you want to continue where you left
;; off, just open the buffer again like any other file.

;; CAVEAT: Your dialog with the program is NOT saved when you save the
;; buffer!

;; A full bells-and-whistles setup would include the following:

;; (add-hook 'flashcard-mode-hook
;;           'flashcard-add-scroll-to-bottom)
;; (add-hook 'flashcard-positive-feedback-functions
;;           'flashcard-feedback-highlight-answer)
;; (add-hook 'flashcard-positive-feedback-functions
;;           'flashcard-feedback-congratulate)
;; (add-hook 'flashcard-positive-feedback-functions
;;           'flashcard-method-leitner-positive-feedback)

;; For more documentation, see the website:
;; http://www.emacswiki.org/cgi-bin/wiki.pl/FlashCard

;;; Code:

(defvar flashcard-version "2.3.3"
  "The version string for flashcard.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; User customization
(defgroup flashcard nil
  "A mode to learn facts."
  :prefix "flashcard-"
  :group 'applications)

(defcustom flashcard-coding-system (if (featurep 'xemacs)
                                       'binary
                                     'emacs-mule)
  "*The coding system flashcard should use for reading and writing
files.

This should ideally, be a \"catch-all\" coding system, like
`emacs-mule', or `iso-2022-7bit'."
  :type 'symbol
  :group 'flashcard)

(defcustom flashcard-mode-hook nil
  "This hook is run whenever `flashcard-mode' is turned on."
  :type 'hook
  :options '(flashcard-add-scroll-to-bottom)
  :group 'flashcard)

(defcustom flashcard-insert-hook nil
  "This hook is run whenever `flashcard-insert' inserts data into the
flashcard buffer."
  :type 'hook
  :group 'flashcard)

(defcustom flashcard-pre-question-hook nil
  "This hook is run before a question is inserted into the flashcard
buffer."
  :type 'hook
  :group 'flashcard)

(defcustom flashcard-wash-question-hook nil
  "This hook is run after a question is inserted into the flashcard
buffer, with the buffer narrowed to this question. Here is the right
place to add any functions that change the look of questions, e.g.
adding images or similar."
  :type 'hook
  :group 'flashcard)

(defcustom flashcard-post-question-hook nil
  "This hook is run after a question is insert into the flashcard
buffer, and after all functions in `flashcard-wash-question-hook' have
been run. No narrowing is in effect here."
  :type 'hook
  :group 'flashcard)

(defcustom flashcard-positive-feedback-functions nil
  "This hook is run whenever the user answered a card, and should be
used to give positive feedback. The functions in this list will be
passed a single argument, which is the result of
`flashcard-method-check-answer-function'."
  :type 'hook
  :options '(flashcard-feedback-highlight-answer
             flashcard-feedback-congratulate)
  :group 'flashcard)

(defgroup flashcard-faces nil
  "Faces for flashcard."
  :prefix "flashcard-"
  :group 'flashcard)

(defface flashcard-input-face '((t (:foreground "cyan")))
  "The face to use for your input to flashcard."
  :group 'flashcard-faces)

(defface flashcard-question-face '((t (:foreground "cyan")))
  "The face to use for questions."
  :group 'flashcard-faces)

(defface flashcard-answer-face '((t (:foreground "green")))
  "The face to use for answers."
  :group 'flashcard-faces)

(defface flashcard-input-correct-face '((t (:foreground "green" :bold t)))
  "The face to use for correct answers."
  :group 'flashcard-faces)

(defface flashcard-input-wrong-face '((t (:foreground "red")))
  "The face to use for incorrect answers."
  :group 'flashcard-faces)

(defgroup flashcard-methods nil
  "Different methods for flashcard."
  :group 'flashcard)

(defgroup flashcard-method-interface nil
  "Method interface for flashcard. It's usually not very wise to set
these by hand. Instead, try M-x flashcard-method-*."
  :prefix "flashcard-method-"
  :group 'flashcard)

(defcustom flashcard-method 'leitner
  "Which method is currently in effect."
  :type 'symbol
  :options '(leitner)
  :group 'flashcard-method-interface)

(defcustom flashcard-method-get-card-function 'flashcard-method-leitner-get-card
  "The function called to retrieve a new card from a deck. It is
passed a single argument, the DECK where the card should come from.
You can expect this to stay the same in the current buffer."
  :type 'function
  :options '(flashcard-method-leitner-get-card)
  :group 'flashcard-method-interface)

(defcustom flashcard-method-check-answer-function 'flashcard-method-leitner-check-answer
  "The function called to check for the correctness of an answer. It
is passed two arguments, the CARD to be answered and the ANSWER of the
user. The return value is passed to
`flashcard-method-check-answer-function' and ignored otherwise."
  :type 'function
  :options '(flashcard-method-leitner-check-answer)
  :group 'flashcard-method-interface)

(defcustom flashcard-method-answered-function 'flashcard-method-leitner-answered
  "The function called to tell the method how well the user answered a
question. It is passed two arguments, the CARD and the GRADE, which is
the result of `flashcard-method-check-answer-function'. This should
update the card accordingly."
  :type 'function
  :options '(flashcard-method-leitner-answered)
  :group 'flashcard-method-interface)

(defcustom flashcard-method-correct-p-function 'identity
  "This function is used when one needs to know whether the return
value of `flashcard-method-check-answer-function' says that the answer
was correct or not, and is passed exactly that value as the single
argument. When the backend uses grades instead of boolean values, it
should set this to a useful function."
  :type 'function
  :options '(identity)
  :group 'flashcard-method-interface)

(defcustom flashcard-method-initialize-card-function 'flashcard-method-leitner-initialize-card
  "This function is used to initialize a card. There's no provision
taken to avoid initializing a card that's already initialized, so
check for that as well. Initializing means adding all the notes you
need in `flashcard-method-get-card' to retrieve cards."
  :type 'function
  :options '(flashcard-method-leitner-initialize-card)
  :group 'flashcard-method-interface)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal variables
(defvar flashcard-deck []
  "The current flashcard deck.")
(make-variable-buffer-local 'flashcard-deck)

(defvar flashcard-card nil
  "The current card being asked.")
(make-variable-buffer-local 'flashcard-card)

(defvar flashcard-marker nil
  "The input/output marker used by flashcard.")
(make-variable-buffer-local 'flashcard-marker)

(defvar flashcard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'flashcard-input)
    map)
  "Keymap for flashcard mode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Frontend (the major mode itself)
(defun flashcard-mode ()
  "Major mode to learn facts. It will ask you questions. Just write
the answer and submit it using \\[flashcard-input]

Turning on `flashcard-mode' runs the hook `flashcard-mode-hook'."
  (interactive)
  (let ((deck (flashcard-parse-buffer)))
    (unless (flashcard-deck-p deck)
      (error "This buffer is not a flashcard deck!"))
    (kill-all-local-variables)
    (use-local-map flashcard-mode-map)
    (setq mode-name "Flashcard"
          major-mode 'flashcard-mode)
    ;; Local variables
    (setq flashcard-deck deck))
  ;; Saving this file works differently
  (set-buffer-file-coding-system flashcard-coding-system)
  (if (featurep 'xemacs)
      (add-hook 'write-file-data-hooks  'flashcard-save-buffer)
    (add-hook 'write-contents-hooks 'flashcard-save-buffer))
  (add-hook 'change-major-mode-hook 'flashcard-unparse-buffer nil t)
  ;; Initialize the cards
  (flashcard-deck-initialize flashcard-deck)
  ;; Now set up the buffer
  (widen)
  (delete-region (point-min) (point-max))
  (set-buffer-modified-p nil)
  ;; Input/output marker
  (setq flashcard-marker (make-marker))
  (set-marker flashcard-marker (point-max))
  ;; And run the mode hook
  (run-hooks 'flashcard-mode-hook)
  ;; Now ask the user
  (flashcard-ask))

;; New buffers shouldn't inherit this mode
(put 'flashcard-mode 'mode-class 'special)

(defun flashcard-parse-buffer ()
  "Parse the current buffer and return the new value."
  (if (= (point-min) (point-max))
      (flashcard-make-deck)
    (condition-case nil
        (save-excursion
          (goto-char (point-min))
          (read (current-buffer)))
      (end-of-file
       (error "I could not parse the current buffer as a flashcard file.")))))

(defun flashcard-unparse-buffer ()
  "Delete the contents of the current buffer and insert the value of
`flashcard-deck' together with some annotations."
  (widen)
  (delete-region (point-min) (point-max))
  (insert ";;; This file is a flashcard.el deck file, and is not meant to be\n"
          ";;; edited by hand. Please open it in Emacs, and do M-x flashcard-mode\n"
          ";;; to use it.\n")
  (print flashcard-deck (current-buffer))
  (insert "\n"
          ";;; Local Variables: ***\n"
          ";;; coding: " (symbol-name flashcard-coding-system) " ***\n"
          ;; We can't add "mode: flashcard" here since emacs will call
          ;; the mode function as soon as it finds it, and that will
          ;; delete the other stuff here.
          ";;; End: ***\n")
  (goto-char (point-min)))

(defun flashcard-save-buffer ()
  "Save a representation of the current buffer to `buffer-file-name'."
  (let ((deck flashcard-deck))
    (with-temp-file buffer-file-name
      (let ((flashcard-deck deck))
        (set-buffer-file-coding-system flashcard-coding-system)
        (flashcard-unparse-buffer)))
    (set-buffer-modified-p nil)
    (set-visited-file-modtime)
    t))

(defun flashcard-insert (&rest args)
  "Insert ARGS into the flashcard buffer after the last output."
  ;; If point is on the marker, it won't move when inserting stuff, so
  ;; emulate that by hand.
  (save-restriction
    (widen)
    (let ((movep (= (point) flashcard-marker)))
      (save-excursion
        (goto-char flashcard-marker)
        (apply #'insert args)
        (set-marker flashcard-marker (point)))
      (when movep
        (goto-char flashcard-marker)))
    (run-hooks 'flashcard-insert-hook)))

(defun flashcard-input (&optional n)
  "When in the input area, send the current input to flashcard. Else,
run `newline'."
  (interactive "*P")
  (cond
   ((< (point) flashcard-marker)
    (newline n))
   ((not flashcard-card)
    (error "There's currently no question asked!"))
   (t
    (let ((from (marker-position flashcard-marker))
          (to (point-max))
          (data (buffer-substring flashcard-marker (point-max))))
      (goto-char (point-max))
      (newline)
      (newline)
      (set-marker flashcard-marker (point))
      (add-text-properties from to '(face flashcard-input-face
                                     rear-nonsticky t))
      (flashcard-handle-user-input data)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Backend interface
(defun flashcard-ask ()
  "Ask the next question. This is the main driver method."
  (let ((card (or (flashcard-deck-empty-p flashcard-deck)
                  (flashcard-method-get-card flashcard-deck))))
    (cond
     ;; Empty deck
     ((eq card t)
      (setq flashcard-card nil)
      (flashcard-insert
       "The current deck is empty! Either add or import new cards using\n"
       "`flashcard-add-card' or `flashcard-import-from-*'\n"
       "\n"))
     ;; No card from backend
     ((not card)
      (setq flashcard-card nil)
      (flashcard-insert
       "Congratulations! You have finished this deck!\n"
       "Please either add or import new cards using\n"
       "`flashcard-add-card' or `flashcard-import-from-*',\n"
       "or start this deck over using `flashcard-deck-clean'.\n"
       "\n"))
     ;; No card, but backend supplied reason
     ((stringp card)
      (setq flashcard-card nil)
      (flashcard-insert card
                        "\n"))
     ;; We got a card!
     (t
      (setq flashcard-card card)
      (flashcard-insert-question card)))))

(defun flashcard-insert-question (card)
  "Insert the question from CARD into the flashcard buffer."
  (run-hooks 'flashcard-pre-question-hook)
  (let ((old-marker (marker-position flashcard-marker)))
    (let ((question (flashcard-card-question card)))
      (flashcard-insert question)
      ;; If the question didn't end with a newline, insert one.
      (unless (char-equal ?\n (aref question (1- (length question))))
        (flashcard-insert "\n")))
    (narrow-to-region old-marker flashcard-marker)
    (add-text-properties (point-min) (point-max)
                         '(face flashcard-question-face
                           rear-nonsticky t))
    (run-hooks 'flashcard-wash-question-hook)
    (widen)
    (run-hooks 'flashcard-post-question-hook)
    (flashcard-insert "\n")
    (goto-char flashcard-marker)))

(defun flashcard-handle-user-input (data)
  "Handle DATA as input submitted by the user."
  (let ((grade (flashcard-method-check-answer flashcard-card data)))
    (flashcard-method-answered flashcard-card grade)
    (run-hook-with-args 'flashcard-positive-feedback-functions grade)
    (flashcard-ask)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Method interface functions
(defun flashcard-method-get-card (deck)
  "Ask the current method to get a card from the deck DECK.

This uses `flashcard-method-get-card-function'."
  (funcall flashcard-method-get-card-function deck))

(defun flashcard-method-check-answer (card answer)
  "Ask the current method whether ANSWER is a correct answer to CARD.
Return a boolean, a grade, or whatever the current method seems fit.
The method is not expected to update the card now, use
`flashcard-method-answered' for this!

This uses `flashcard-method-check-answer-function'."
  (funcall flashcard-method-check-answer-function card answer))

(defun flashcard-method-answered (card grade)
  "Tell the current method that the user has answered CARD with GRADE
- which is the value returned by `flashcard-method-check-answer'. The
method should now update the card accordingly.

This uses `flashcard-method-answered-function'."
  (funcall flashcard-method-answered-function card grade))

(defun flashcard-method-correct-p (grade)
  "Ask the backend whether the return value of
`flashcard-method-check-answer' means that the answer was correct or
not.

This uses `flashcard-method-correct-p-function'."
  (funcall flashcard-method-correct-p-function grade))

(defun flashcard-method-initialize-card (card)
  "Initialize CARD. This shouldn't do anything when CARD is already
initialized.
This uses `flashcard-method-initialize-card-function'."
  (funcall flashcard-method-initialize-card-function card))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some nice functions
(defun flashcard-feedback-highlight-answer (grade)
  "Highlight the answer the user has given with
`flashcard-input-correct-face' or `flashcard-input-wrong-face'.
This should be added to `flashcard-positive-feedback-functions'."
  (save-excursion
    (let ((start nil)
          (end (point-max)))
      (while (and (> end (point-min))
                  (not (eq (get-text-property end 'face)
                           'flashcard-input-face)))
        (setq end (1- end)))
      (setq start end)
      (while (and (> start (point-min))
                  (eq (get-text-property start 'face)
                      'flashcard-input-face))
        (setq start (1- start)))
      (when (not (= start end))
        (add-text-properties start (1+ end)
                             (list 'face
                                   (if (flashcard-method-correct-p grade)
                                       'flashcard-input-correct-face
                                     'flashcard-input-wrong-face)))))))

(defun flashcard-feedback-congratulate (grade)
  "Congratulate the user when the answer was correct."
  (when (flashcard-method-correct-p grade)
    (let ((congrats '("Congratulations!"
                      "Good work!"
                      "Very good!"
                      "You're doing great!")))
      (flashcard-insert (nth (random (length congrats))
                             congrats)
                        "\n"
                        "\n"))))

(defun flashcard-add-scroll-to-bottom ()
  "Add `flashcard-scroll-to-bottom' to `window-scroll-functions', so
that the input line is always the last line of the window in which the
flashcard buffer is displayed."
  (add-hook 'window-scroll-functions 'flashcard-scroll-to-bottom nil t))

(defun flashcard-scroll-to-bottom (window display-start)
  "Recenter WINDOW so that point is on the last line.

This is added to `window-scroll-functions' by
`flashcard-add-scroll-to-bottom'.

The code is shamelessly taken (but adapted) from ERC."
  (when (and window (window-live-p window))
    (let ((resize-mini-windows nil))
      (save-selected-window
        (select-window window)
        (save-restriction
          (widen)
          (when (>= (point) flashcard-marker)
            (save-excursion
              (recenter -1)
              (sit-for 0))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deck interface
(defun flashcard-make-deck ()
  "Return a new and empty deck."
  (vector '*flashcard-deck* ; Marker
          '()   ; No cards
          '())) ; No notes

(defun flashcard-deck-p (deck)
  "Returns non-nil if DECK is a flashcard deck."
  (and (vectorp deck)
       (= 3 (length deck))
       (eq '*flashcard-deck*
           (aref deck 0))))

(defun flashcard-deck-cards (deck)
  "Return a list of cards in DECK."
  (aref deck 1))

(defun flashcard-deck-set-cards (deck cards)
  "Set the cards in DECK to CARDS."
  (aset deck 1 cards))

(defun flashcard-deck-get-note (deck name)
  "Return the value of the note named NAME in DECK."
  (cdr (assq name (aref deck 2))))

(defun flashcard-deck-set-note (deck name value)
  "Set the note named NAME to VALUE."
  (aset deck
        2
        (flashcard-assq-set (aref deck 2)
                            name
                            value)))

(defun flashcard-deck-clean-notes (deck)
  "Clean all notes on DECK."
  (aset deck 2 '()))

(defun flashcard-deck-empty-p (deck)
  "Return non-nil if DECK has no cards in it."
  (null (flashcard-deck-cards deck)))

(defun flashcard-deck-initialize (deck)
  "Initialize all cards in the deck using `flashcard-method-initialize-card'."
  (mapc #'flashcard-method-initialize-card
        (flashcard-deck-cards deck)))

(defun flashcard-deck-clean (deck)
  "Clean DECK. That is, remove all notes on all cards, and all notes
on the deck."
  (interactive (list flashcard-deck))
  (mapc #'flashcard-card-clean-notes
        (flashcard-deck-cards deck))
  (flashcard-deck-clean-notes deck)
  (flashcard-deck-initialize deck)
  (set-buffer-modified-p t)
  (when (and (interactive-p)
             (not flashcard-card))
    (flashcard-ask)))

(defun flashcard-add-card (deck card)
  "Add CARD to DECK."
  (interactive (list flashcard-deck
                     (flashcard-make-card
                      (read-from-minibuffer "Question: ")
                      (read-from-minibuffer "Answer: "))))
  (flashcard-method-initialize-card card)
  (flashcard-deck-set-cards deck
                            (cons card
                                  (flashcard-deck-cards deck)))
  (set-buffer-modified-p t)
  (when (and (interactive-p)
             (not flashcard-card))
    (flashcard-ask)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; card interface
(defun flashcard-make-card (question answer)
  "Return a new card with QUESTION and ANSWER."
  (vector '*flashcard-card*
          question
          answer
          '())) ; No notes

(defun flashcard-card-p (card)
  "Return non-nil if CARD is a flashcard-card."
  (and (vectorp card)
       (= 4 (length card))
       (eq '*flashcard-card* (aref card 0))))

(defun flashcard-card-question (card)
  "Return the question of CARD."
  (aref card 1))

(defun flashcard-card-answer (card)
  "Return the answer of CARD."
  (aref card 2))

(defun flashcard-card-get-note (card name &optional default)
  "Return the note named NAME of CARD. If the note doesn't exist,
return DEFAULT."
  (let ((note (assq name (aref card 3))))
    (if note
        (cdr note)
      default)))

(defun flashcard-card-set-note (card name value)
  "Set the note named NAME of CARD to VALUE."
  (aset card 3
        (flashcard-assq-set (aref card 3)
                            name
                            value))
  (set-buffer-modified-p t))

(defun flashcard-card-clean-notes (card)
  "Clean all notes in CARD."
  (aset card 3 '())
  (set-buffer-modified-p t))

(defun flashcard-assq-set (list name value)
  "Set the value of NAME to VALUE in the association list LIST."
  (let ((elt (assq name list)))
    (if (not elt)
        (cons (cons name value)
              list)
      (setcdr elt value)
      list)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Import methods

(defun flashcard-import-from-colon-file (deck file)
  "Import cards for DECK from FILE, which should be a colon-separated
file. I.e.:

Question : Answer"
  (interactive (list flashcard-deck
                     (read-file-name "File: " nil nil t)))
  (unless (eq major-mode 'flashcard-mode)
    (error "You're not in a deckfile."))
  (with-temp-buffer
    (insert-file-contents file)
    (while (re-search-forward "^ *\\(.*\\) +: +\\(.*\\)$" nil t)
      (flashcard-add-card deck
                          (flashcard-make-card (match-string 1)
                                               (match-string 2)))))
  (when (and (interactive-p)
             (not flashcard-card))
    (flashcard-ask)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Leitner method

;; User customization
(defgroup flashcard-method-leitner nil
  "The original Leitner method."
  :prefix "flashcard-method-leitner-"
  :group 'flashcard-methods)

(defcustom flashcard-method-leitner-compartment-sizes [50 100 250 400 700]
  "The sizes of the compartments of your cardbox, in cards. Leitner
originally suggested 5 compartments of 1cm, 2cm, 5cm, 8cm and 14cm
size each. You can squeeze up to 50 cards in a 1cm slot, but that's a
real tight fit.

You can set this to a vector of a different size, and flashcard will
do The Right Thing."
  :type '(vector integer integer integer integer integer)
  :group 'flashcard-method-leitner)

(defcustom flashcard-method-leitner-minimum-size 3
  "How many cards should usually stay in a compartment. Leitner
originally suggested 3."
  :type 'integer
  :group 'flashcard-method-leitner)

(defcustom flashcard-method-leitner-bundle-size 100
  "Up to how many cards to take from a compartment to make room for
new cards. Leitner originally suggested the width of a finger as a
size, which for me is roughly 100 cards."
  :type 'integer
  :group 'flashcard-method-leitner)

(defcustom flashcard-method-leitner-prepare-functions '()
  "These functions will be called before a bundle of cards is taken
from the deck. You can add functions that shuffle the cards or
something here. The functions in this list will be passed two
arguments: The deck and the compartment number that we're interested
in."
  :type 'hook
  :group 'flashcard-method-leitner)

;; Local variables
(defvar flashcard-method-leitner-bundle nil
  "The current bundle of cards being asked.")
(make-variable-buffer-local 'flashcard-method-leitner-bundle)

;; Implementation
(defun flashcard-method-leitner ()
  "Use the Leitner method in flashcard.
This implements the algorithm of the Lernmaschine in Sebastian
Leitner's book 'So lernt man lernen'. The original algorithm was
tailored for a non-computer approach, specifically a box for
flashcards (that is, cards with a question on one side, and the answer
on the other. The box itself was meant to have 5 compartments of 1cm, 2cm,
5cm, 8cm and 14cm size each (`flashcard-method-leitner-compartment-sizes').

All your cards go into the first compartment. You start by asking
yourself questions from the first compartment. When you get the answer
right, the card goes right into compartment two. If not, it stays in
compartment one. When the first compartment has three or less cards
left (`flashcard-method-leitner-minimum-size'), you add new cards to
the first compartment. When another compartment gets full, you empty
it by asking yourself a number of cards from that
compartment (`flashcard-method-leitner-bundle-size').

Whenever you get a card wrong, it goes right back into compartment
one.

For more information about this method, see my summary of the book at
http://www.forcix.cx/books/sebastian_leitner_-_so_lernt_man_lernen.html
- the article is in German, though."
  (interactive)
  (message "Setting up the Leitner method...")
  (setq flashcard-method 'leitner
        flashcard-method-get-card-function 'flashcard-method-leitner-get-card
        flashcard-method-check-answer-function 'flashcard-method-leitner-check-answer
        flashcard-method-answered-function 'flashcard-method-leitner-answered
        flashcard-method-correct-p-function 'identity
        flashcard-method-initialize-card-function 'flashcard-method-leitner-initialize-card)
  ;; Add positive feedback function
  (add-hook 'flashcard-positive-feedback-functions
            'flashcard-method-leitner-positive-feedback)
  ;; Initialize the cards
  (flashcard-deck-initialize flashcard-deck)
  (message "Setting up the Leitner method...ok"))

(defun flashcard-method-leitner-initialize-card (card)
  "Initialize CARD to be used with the Leitner method. For this, we
only have to remember the compartment the card is in."
  (unless (flashcard-card-get-note card 'leitner-compartment)
    (flashcard-card-set-note card 'leitner-compartment 0)))

(defun flashcard-method-leitner-get-card (deck)
  "Return a new card from DECK according to the algorithm developed by
Sebastian Leitner. We depend on this being the same deck between
invocations in the same buffer."
  ;; If there are cards still in the bundle, take those
  (if flashcard-method-leitner-bundle
      (let ((card (car flashcard-method-leitner-bundle)))
        (setq flashcard-method-leitner-bundle
              (cdr flashcard-method-leitner-bundle))
        card)
    ;; Else, find a suitable new bundle
    (let ((counts (flashcard-method-leitner-compartment-counts deck)))
      (or (flashcard-method-leitner-get-card-from-full-compartment counts deck)
          (flashcard-method-leitner-get-card-from-first-compartment counts deck)
          (flashcard-method-leitner-get-card-from-fullest-compartment counts deck)))))

(defun flashcard-method-leitner-get-card-from-full-compartment (counts deck)
  "If any compartment in COUNTS is full, return a bundle of that
compartment from DECK. Else, return nil."
  (let ((i (1- (length counts))))
    ;; Find a full compartment, start at the end
    (while (and (>= i 0)
                (< (aref counts i)
                   (aref flashcard-method-leitner-compartment-sizes i)))
      (setq i (1- i)))
    (when (>= i 0)
      (flashcard-method-leitner-get-bundle deck i))))

(defun flashcard-method-leitner-get-card-from-first-compartment (counts deck)
  "If there are more then `flashcard-method-leitner-minimum-size'
cards in the first compartment, return a bundle from that compartment
from DECK. Else, return nil."
  (when (> (aref counts 0) flashcard-method-leitner-minimum-size)
    (flashcard-method-leitner-get-bundle deck 0)))

(defun flashcard-method-leitner-get-card-from-fullest-compartment (counts deck)
  "Return a bundle from the compartment from DECK that has according
to COUNTS the most cards in it."
  (let ((max 0)
        (max-i 0)
        (i 0))
    (while (< i (length counts))
      (when (> (aref counts i) max)
        (setq max (aref counts i)
              max-i i))
      (setq i (1+ i)))
    (flashcard-method-leitner-get-bundle deck max-i)))

(defun flashcard-method-leitner-get-bundle (deck compartment)
  "Set up a bundle from DECK's compartment COMPARTMENT to be asked
now, and return the first question of that."
  (run-hook-with-args 'flashcard-method-leitner-prepare-functions deck compartment)
  ;; Premature Optimization. We walk through the cards, keeping a
  ;; pointer to the last element of our list so we can append in O(1).
  ;; We also keep a sentinel so we don't have to special-case for
  ;; anything.
  (let* ((result (list 'sentinel))
         (end result)
         (num 0)
         (cards (flashcard-deck-cards deck)))
    (while (and cards
                (< num flashcard-method-leitner-bundle-size))
      (when (= (flashcard-card-get-note (car cards) 'leitner-compartment)
               compartment)
        (setcdr end (list (car cards)))
        (setq end (cdr end)
              num (1+ num)))
      (setq cards (cdr cards)))
    ;; (cdr result) now is the list of cards
    (setq flashcard-method-leitner-bundle (cddr result))
    (cadr result)))

(defun flashcard-method-leitner-check-answer (card answer)
  "Insert the answer, ask the user whether he was correct, and return
the reply."
  (if (string= (flashcard-card-answer card)
               answer)
      t
    (flashcard-insert "The correct answer is:\n"
                      (propertize (flashcard-card-answer card)
                                  'face 'flashcard-answer-face
                                  'rear-nonsticky t)
                      "\n"
                      "\n")
    (y-or-n-p "Was your answer correct? ")))

(defun flashcard-method-leitner-answered (card answer-correct-p)
  "Move CARD according to whether the user answered correctly. This is,
if ANSWER-CORRECT-P, advance it by one compartment, else move it to
the first compartment."
  (flashcard-card-set-note
   card
   'leitner-compartment
   (if answer-correct-p
       (1+ (flashcard-card-get-note card 'leitner-compartment))
     0)))

(defun flashcard-method-leitner-positive-feedback (correctp)
  "Give the user some feedback on his progress. Tell him how many
cards are left in each compartment."
  (when (eq flashcard-method 'leitner)
    (let ((counts (flashcard-method-leitner-compartment-counts flashcard-deck))
          (i 0))
      (flashcard-insert "Deck compartments: [")
      (while (< i (length counts))
        (flashcard-insert (format "%3i" (aref counts i)))
        (when (< i (1- (length counts)))
          (flashcard-insert " |"))
        (setq i (1+ i))))
      (flashcard-insert " ]\n")
    (flashcard-insert "\n")))

(defun flashcard-method-leitner-compartment-counts (deck)
  "Return a vector the same size as
`flashcard-method-leitner-compartment-sizes', with each element being
the number of cards in that slot."
  (let* ((size (length flashcard-method-leitner-compartment-sizes))
         (vec (make-vector size 0)))
    (mapc (lambda (card)
            (let ((compartment (flashcard-card-get-note card
                                                        'leitner-compartment)))
              (when (and (<= 0 compartment)
                         (< compartment size))
                (aset vec
                      compartment
                      (1+ (aref vec compartment))))))
          (flashcard-deck-cards deck))
    vec))

(provide 'flashcard)
;;; flashcard.el ends here
