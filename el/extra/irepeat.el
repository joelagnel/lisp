;;; Saved through ges-version 0.3.3dev at 2004-11-20 15:05
;;; ;;; From: burton@openprivacy.org (Kevin A. Burton)
;;; ;;; Subject: irepeat.el - repeat through history data FAST
;;; ;;; Newsgroups: gmane.emacs.sources
;;; ;;; Date: 22 Mar 2002 00:02:34 -0800

;;; [[PGP Signed Part:Undecided]]
;;; [1. text/plain]


;;; OK...

;;; I have been playing with this for a few months.  Took me about an hour to write
;;; and then I kept tweaking it.

;;; It is now at a point where I feel others might benefit from playing with this.

;;; I would REALLY like to get feedback - especially when compared to the standard
;;; Emacs completion mechanism.

;;; Basically the concept here is to give you access to a LRU sorted history list
;;; (command-history, file-name-history, buffer-list, semantic tokens) and work with
;;; them.

;;; Anyway... let me know what you think...

;;; Kevin

;;; irepeat.el --- repeat through history data FAST

;; $Id: irepeat.el,v 1.12 2002/03/22 19:58:36 burton Exp $

;; Copyright (C) 2000-2003 Free Software Foundation, Inc.
;; Copyright (C) 2000-2003 Kevin A. Burton (burton@openprivacy.org)

;; Author: Kevin A. Burton (burton@openprivacy.org)
;; Maintainer: Kevin A. Burton (burton@openprivacy.org)
;; Location: http://relativity.yi.org
;; Keywords: 
;; Version: 1.0.0

;; This file is [not yet] part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2 of the License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program; if not, write to the Free Software Foundation, Inc., 59 Temple
;; Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; (disclaimer: This package requires GNU Emacs 21)

;; This is an 'intelligent repeat' package for Emacs. Basically it is modeled
;; after the reverse-i-search functionality in bash.
;;
;; Essentially this package allows you to *QUICKLY* go over a list of items and
;; select the best one (think of it as an isearch for lists).  In most
;; situations this is much faster than Emacs programmed completion.  Usually
;; this can be done in 2 or 3 key strokes, much faster than Emacs programmed
;; completion.  irepeat can handle huge lists and quickly jump to the best entry
;; in the list.
;;
;; There are implementation of irepeat for buffer switching, complex command
;; reptition, etc.  Most of these emulate the standard Emacs functions but use
;; irepeat instead.
;;
;; An added advantage is that since most are history lists, the more you access
;; a function or buffer, the higher it is on the list and the quicker you can
;; access it.  This basically provides a "first in last out" stack of data at
;; your fingertips.
;;
;; This package also has some cool features that you get for free.  For example
;; if you use irepeat-switch-to-buffer as a filter.  You can just enter a file
;; extension '.el' to narrow this list down to all Emacs Lisp files.

;;; Installation:

;; Installation is easy.  Just add to your load-path an do a (require 'irepeat).
;; The key bindings for irepeat functions such as `irepeat-switch-to-buffer'
;; should be done by the user as in most situations people may want to remap
;; Emacs standard key bindings.  For example I rebind 'C-x b' as
;; `irepeat-switch-to-buffer'.
;; 
;; Also I would highly recommend setting `history-length' to something longer
;; than the default, 30 is just too short.  For irepeat a value of 100 or higher
;; is recommended.  I run with a `history-length' of 250.
;;
;; NOTE: If you enjoy this software, please consider a donation to the EFF
;; (http://www.eff.org)

;;; Code:

;;; NOTES:
;;

;;; TODO:

;;; HIGH PRIORITY:

;; MAJOR BUG:  IF THIS is in this command history:
;;  (browse-url "http://s0b.bluestreak.com/ix.e?ht&s=38885&u=http%3A//adserver.usermagnet.com/cgi-bin/advertpro/banners.fpl%3Fregion%3D7%26bust%3D379951%26keyword%3DNULL&x=Insert_Click_Track_URL_Here" t)
;; 
;;   we get: apply: Not enough arguments for format string
;;
;;   We need to escape this here!

;; MAJOR NEW FEATURE:
;;
;; - Ability to display a graphical preview window as a buffer.  this would
;; include all the options around the match and highlight them with an overlay.

;; - BUG: when appending to the command-history, we need to prune the history so
;; that it isn't greater than history-length.
;;
;; - can we put the history-length var in the irepeat customization buffer?

;;; LOW PRIORITY:
;;
;; - REF: optionally put the user in a (read-string) prompt before evaluating
;;   the match so that values can be changed.  Could this be done with a control
;;   enter?
;;
;; - if we have a match... the arrows up/down/etc should go through the history
;;   as well.  This should search from index backwards or forwards for the next
;;   match.
;;
;; - SPEED IMPROVEMENT... if I type in an extra character.  We don't always have
;; to search FORWARD.  We can search from the current index on.  Since the
;; current request didn't match any of the previous entries it clearly will not
;; match a second time.  Note that this ISN'T the case if we hit the 'backspace'
;; key... we have to start from 0 again.

;;; RFE: could we support backward-kill-word like normal completion??
;;
;;  THIS COULD BECOME THE ESCAPE KEY

;;; RFE: we should support a type of plugin style operation for the 'tab' key.
;;
;;   1. expand the current match.  (I THINK WE SHOULD GET RID OF THIS) matches
;;   that are too long will fold over into two lines :(
;;
;;   2. cycle through all matches.
;;
;;   3. show a completion style buffer ALA programmed completion.

;;; RFE: ability to narrow down the types of objects that irepeat-semantic-jump
;;; can jump to (functions, variables, imports, etc)

;; RFE: when use faces is off... we should just MERGE faces.  this way we can
;; boldify certain regions.

;; RFE: ability to show ALL matches in an associated buffer (like completion).
;; This list would get smaller as the user typed in a more complex command.
;;
;;    - we would need to ONLY do this when the current text is greater than X
;;      items.
;;
;;    - should be disabled by default due to performance issues.
;;
;;    - should support shortening text (aka filename history)
;;
;;    - make the necessary selections bold.
;;
;;    - ability to run C-? to show the list.
;;
;;    - I think we whould MANUALLY have to invoke this...
;; 

;; how does completing-help hook into completion?  Maybe we can use a similar
;; mechanism within irepeat

;; RFE: should we always default to the first item in the list?

;; RFE: the standard Emacs completion allows the user to enter a '?'.  Maybe we
;; should just reserve this key sequence for us.

;; RFE: is there a way that I can provide addition 'help' or 'hints' when
;; completing.  For example with buffer completion it would be nice to include
;; the parent directory.
;;
;;     Example:
;;
;;     (irepeat-switch-to-buffer): [2/218] 'irepeat.el': irep  /home/burton/el
;;
;;     The cursor would be at 'irep' and we would put a font-lock-comment-face
;;     on /home/burton/el so that it doesn't visually interfere with our input.
;;
;;     We would provide a function named irepeat-get-hint which would return nil
;;     if there is no hint for the given object.
;;
;;     BUG THE MAIN PROBLEM IS: how do we put text AFTER the cursor in a
;;     read-event

;; RFE: maybe irepeat should use try-completion so that we can expand a prompt.
;; So for example if we have jde-and.el and jde-docindex.el and we type 'j tab'
;; we can expand to 'jde-'

;; RFE: could we maybe cache to-string operations on lists?  This way we can
;; quickly go ove ra list multiple times without having to call to-string a few
;; hundred times.

(require 'font-lock)
(require 'semantic)

(defcustom irepeat-command-prompt "%s [%i/%i] '%s': %s"
  "Prompt used for interactively prompting for a irepeat."
  :group 'irepeat
  :type 'string)

(defcustom irepeat-quit-message "%s: quit"
  "Message format used when quiting an irepeat."
  :group 'irepeat
  :type 'string)

(defcustom irepeat-use-faces t
  "If non-nil, use faces to highlight irepeat matches, etc."
  :group 'irepeat
  :type 'boolean)

(defcustom irepeat-sit-after-selection t
  "Display the match and sit for a short time after a selection has been made."
  :group 'irepeat
  :type 'boolean)

(defcustom irepeat-expand-match-after-selection t
  "Expand the match after a selection has been made."
  :group 'irepeat
  :type 'boolean)

(defface irepeat-match-face '((t (:inherit font-lock-constant-face)))
  "Face used for the last available match."
  :group 'irepeat)

(defface irepeat-hit-face '((t (:bold t :inherit font-lock-constant-face)))
  "Face used for the last available match."
  :group 'irepeat)

(defface irepeat-blink-line-face '((t (:background "DarkBlue")))
  "Face used for the last available match."
  :group 'irepeat)

(defcustom irepeat-semantic-token-display-function 'semantic-uml-concise-prototype-nonterminal
  "*Semantic function to use for displaying tokens in the methods buffer.  Some
useful functions are found in `semantic-token->text-functions'.  This
functionality also allows the user to display tokens as UML.  To enable this
functionality set this value to either
`semantic-uml-concise-prototype-nonterminal',
`semantic-uml-prototype-nonterminal', or `semantic-uml-abbreviate-nonterminal'."
  :group 'irepeat
  :type semantic-token->text-custom-list
  :initialize 'custom-initialize-default)

(defvar irepeat-blink-line-overlay nil "Overlay used for blinking lines.")

(defun irepeat-complex-command()
  "Repeat a comlex command interactively.  See `repeat-complex-command' for more
information."
  (interactive)
  
  (irepeat "(irepeat-complex-command):" command-history
           (lambda(match)
             (setq command-history (cons match command-history))
               
             (eval match))))

(defun irepeat-switch-to-buffer()
  "Run `switch-to-buffer' but prompt for the buffer name with irepeat."
  (interactive)

  (let((buffer-list '())
       (prompt-prefix "(irepeat-switch-to-buffer):"))

    ;;go through the buffer list and make sure we don't include any hidden buffer
    (dolist(buffer (buffer-list (selected-frame)))

      ;;it is faster if we just did a substring..  We could use regexp (and I
      ;;think it would be more readable) but this will be slower.
      (when (not (string-equal " " (substring (buffer-name buffer) 0 1)))
        (add-to-list 'buffer-list buffer t)))

    ;;TODO: maybe we should ue other-buffer here... ???

    ;;make sure the current buffer isn't in the list.
    (setq buffer-list (delete (current-buffer) buffer-list))
    
    (let((first-element (cons 0 (nth 0 buffer-list))))
    
      (irepeat prompt-prefix buffer-list
               (lambda(match)
                 
                 (switch-to-buffer match)) nil first-element))))

(defun irepeat-semantic-jump()
  "Jump to a semantic token in the current buffer with irepeat."
  (interactive)

    (let((prompt-prefix "(irepeat-semantic-jump):")
         (nonterminal-list '())
         (nonterminal nil)
         (irepeat-sit-after-selection nil))

      ;;if this buffer isn't bovinated go ahead and do it... this only happens
      ;;the first time we run ig.
      (semantic-bovinate-toplevel) 
      
      (save-excursion

        (beginning-of-buffer)
        
        (while (setq nonterminal (semantic-find-nonterminal-by-overlay-next (point)))

          (when nonterminal

            ;;debug message...

            ;;add this nonterminal to the list as a string...
            (add-to-list 'nonterminal-list nonterminal)
            
            ;;goto the end of the nonterminal for better performance.
            
            (goto-char (semantic-token-start nonterminal)))))

      ;;call into irepeat.
      (irepeat prompt-prefix nonterminal-list
               (lambda(match)
                 
                 (goto-char (semantic-token-start match))
                 
                 (recenter)
                 
                 ;;temporarily place an overlay across the whole line and then
                 ;;remove it.
                 (irepeat-blink-line)) t)))

(defun irepeat-find-file()
  "Use the `file-name-history' to find a file."
  (interactive)
  
  (let((prompt-prefix "(irepeat-find-file): ")
       (first-element (cons 0 (nth 0 file-name-history))))
  
    (irepeat prompt-prefix file-name-history (lambda(match)
                                               (find-file match)) nil first-element)))

(defun irepeat(irepeat-prompt-prefix irepeat-list success-function &optional
irepeat-no-faces first-element)
  "Use irepeat on the given `irepeat-list' and return the match or nil if no
match was found.  The `irepeat-prompt-prefix' is used as a prefix when
prompting.  When irepeat is successful we call `success-function' with one
argument, the match found.  When `irepeat-no-faces' is true, we do not use the
'face property for irepeat prompts.  If `first-element' is specified, this is
the first element of the `irepeat-list' that we should use as the default input.
This can be used with `irepeat-switch-to-buffer' etc to provide default input."

  ;;define irepeat completion variables.  These variable names are used within
  ;;this function and some utiity functions to determine how completion works.
  ;;
  ;; `irepeat-query' : the text the user has currently entered.  For example if
  ;; you want to look for a buffer named 'irepeat.el' and you have typed 'ir'
  ;; this value will be set to 'ir'.

  ;; `irepeat-event' : the current character (event) you have just entered.

  ;; `irepeat-current-match' : the current element within irepeat-list that is
  ;; matching (or nil)

  ;; `irepeat-keep-matching' : if non-nil, we should keep matching on a branch
  ;; of the while loop. If nil, we should stop (probably because we need to
  ;; exit).

  ;; `irepeat-index' : if a match has been found based on the current query,
  ;; irepeat-index is set to the current element index within the given list.

  ;; `irepeat-skip-guess' : when we are reading events, will skip over guessing
  ;; another match. 
  
  (let((irepeat-query "")
       (irepeat-event nil)
       (irepeat-current-match nil)
       (irepeat-keep-matching t)
       (irepeat-index -1)
       (irepeat-skip-guess nil))

    (setq irepeat-query "")
    
    (when first-element
      (setq irepeat-index (car first-element))
      (setq irepeat-current-match (cdr first-element)))
    
    ;; `irepeat-match-result' : if non-nil we have found a match for the current
    ;; event that is about to be entered.
    
    (while (and irepeat-keep-matching
                (let(irepeat-match-result)
                  
                  (setq irepeat-event (irepeat-read-event irepeat-current-match irepeat-query))
                  
                  (if (not (integerp irepeat-event))
                      (progn
                        
                        ;;protect against non character events here - handle
                        ;;other types of events

                        ;;if we hit a backspace... remove some from the 
                        (if (equal irepeat-event 'backspace)
                            (irepeat-handle-backspace)
                          (if (equal irepeat-event 'tab)
                              (irepeat-handle-tab)
                            (if (equal irepeat-event 'return)
                                (irepeat-handle-return)
                              ;;if we go up or down... allow us to walk up the list.
                              (if (or (equal irepeat-event 'up)
                                      (equal irepeat-event 'down))
                                  (irepeat-handle-up-or-down)
                                (if (equal irepeat-event 'escape)
                                    (irepeat-handle-escape)

                                  ;; else... stop by default.
                                  
                                  (setq irepeat-keep-matching nil)
                                  (setq irepeat-current-match nil)
                                  (setq irepeat-match-result nil)))))))
                    
                    ;;make sure this is an acceptable character. if it isn't,
                    ;;we should return
                    (if (or (<= irepeat-event 31)
                            (>= irepeat-event 127))
                        (progn
                          
                          (setq irepeat-match-result nil)
                          (setq irepeat-keep-matching nil)
                          (setq irepeat-current-match nil))
                      
                      ;;else we can keep going.
                      
                      (setq irepeat-query (concat irepeat-query (char-to-string irepeat-event)))
                      
                      ;;also make sure that the irepeat-event is not a control character. AKA ^H
                      (when (char-valid-p irepeat-event)
                        (setq irepeat-match-result t)))
                    
                    irepeat-match-result)))

      (if irepeat-skip-guess
          ;;skil this round.
          (setq irepeat-skip-guess nil)
        
        (let((guessed-match (irepeat-guess-match irepeat-query)))

          (setq irepeat-index (car guessed-match))
          (when irepeat-index
            (setq irepeat-index (1- irepeat-index)))

          (setq irepeat-current-match (cdr guessed-match)))))

    ;;ok... handle a successful match or a quit.
    (if irepeat-current-match
        (funcall success-function irepeat-current-match)
      (irepeat-message-without-log irepeat-quit-message irepeat-prompt-prefix))
    
    irepeat-current-match))

(defun irepeat-read-event(match prompt)
  "Read an event from the minibuffer.  This is used to setup additional features
we need (such as text echoing)."

  (let((cursor-in-echo-area t))

    ;;display the message with font properties in the minibuffer.
    (irepeat-message-without-log (irepeat-make-prompt match prompt))

    (setq irepeat-event (read-event))))

(defun irepeat-make-prompt(match prompt)
  "Return a prompt with faces attached.  The `match' param can be a string or a
symbol representing a match - AKA a lisp expression.  The `prompt' param is the
text the user has currently typed."

  (let(result match-string hit-face)

    (if match
        (setq match-string (irepeat-to-string match))
      (setq match-string ""))

    (when irepeat-use-faces

      ;;determine the face to use

      ;;FIXME: we should use our own custom face list here.  This should be
      ;;merged from the SOURCE property 'face, its attributes poped out, and a
      ;;new list created with (bold . t)_
      
      (if irepeat-no-faces
          (setq hit-face 'bold)

        ;;decorate the match.
        (put-text-property 0 (length match-string) 'face 'irepeat-match-face match-string)

        (setq hit-face 'irepeat-hit-face))

      ;;decorate the hit
      (when (> (length match-string) 0)
        (if (string-match (regexp-quote prompt) match-string)
            (put-text-property (match-beginning 0) (match-end 0) 'face hit-face match-string))))

    (when (null irepeat-index)
      (setq irepeat-index -1))
    
    (setq result (format irepeat-command-prompt
                         irepeat-prompt-prefix
                         irepeat-index
                         (length irepeat-list)
                         match-string
                         prompt))
    
    result))

(defun irepeat-selected(match prompt)
  "When a match is found update the prompt.  This basically expands the prompt
and displays it for a set amount of time."

  (when irepeat-sit-after-selection

    (if irepeat-expand-match-after-selection
        (irepeat-message-without-log (irepeat-make-prompt match (irepeat-to-string match)))
      (irepeat-message-without-log (irepeat-make-prompt match prompt)))
    
    (sit-for .3)
    
    (irepeat-message-without-log nil)))
  
(defun irepeat-guess-match(prompt)
  "Search for the matching prompt within `irepeat-list'.  Return the the match."

  ;;if the prompt is "" we obviously don't have a match.
  (if (string-equal "" prompt)
      nil
    ;;ok... test for a match.
    (let((not-found t)
         (index 0) 
         (current nil)
         (result nil))

      (while (and not-found
                  (< index (length irepeat-list)))

        (setq current (nth index irepeat-list))
        
        (when (string-match (regexp-quote prompt) (irepeat-to-string current))

          (setq not-found nil)

          (setq result current))

        (setq index (1+ index)))

      (when (not result)
        (setq index -1))
      
      ;;return a cons cell with the index and result.
      (cons index result))))

(defun irepeat-to-string(object)
  "Convert the given match to a string.  For example if this is an object, use
  `prin1-to-string'."

  (if (stringp object)
      ;;if this is already a string, we don't need to do anything.
      (substring object 0 (length object))
    (if (semantic-token-p object)
        (funcall irepeat-semantic-token-display-function object nil t)
      (if (bufferp object)
          (let((buffer-name (buffer-name object)))
            
            ;;we can't use the real buffername because we need to set text
            ;;properties on it.  Return a copy.
            
            (irepeat-to-string buffer-name))
        (prin1-to-string object)))))

(defun irepeat-blink-line()
  "Blink the currently selected line."
  (interactive)

  (let((overlay (make-overlay (point-at-bol) (1+ (point-at-eol)) (current-buffer))))

    (overlay-put overlay 'face 'irepeat-blink-line-face)

    (overlay-put overlay 'priority 1)

    (setq irepeat-blink-line-overlay overlay)
    
    ;;delete this overlay...
    (add-hook 'pre-command-hook 'irepeat-unblink-line)))

(defun irepeat-unblink-line()

  (delete-overlay irepeat-blink-line-overlay)

  (setq irepeat-blink-line-overlay nil)
  
  (remove-hook 'pre-command-hook 'irepeat-unblink-line))

(defun irepeat-message-without-log(&rest args)
  "Like `message' but don't log it on the message log. All arguments ARGS are
transfered to function `message'.  Note that we could potentially use other
minibuffer function other than `message' but it appears that these don't
preserve text properties."

  (let ((message-log-max nil))
    (apply 'message args)))

;;; KEY HANDLING FUNCTIONS

(defun irepeat-handle-backspace()
  "Handle the backspace character on `read-event'"

  (when (> (length irepeat-query) 0)
    (setq irepeat-query (substring irepeat-query 0 (1- (length irepeat-query)))))
  
  (setq irepeat-match-result t))

(defun irepeat-handle-tab()
  "Handle the tab character on `read-event'"

  ;;expand the irepeat-query based on the irepeat-current-match
  ;;(when irepeat-current-match
    ;;(setq irepeat-query (irepeat-to-string irepeat-current-match)))

  (setq irepeat-match-result t))

(defun irepeat-handle-return()
  "Handle the return character on `read-event'"

  (when irepeat-current-match
    (irepeat-selected irepeat-current-match irepeat-query))
  
  ;;if we hit return... stop.
  (setq irepeat-skip-guess t)
  (setq irepeat-keep-matching nil)
  (setq irepeat-match-result t))

(defun irepeat-handle-escape()
  "Handle the return character on `read-event'"

  (setq irepeat-index -1)
  (setq irepeat-match-result t)
  (setq irepeat-keep-matching t)
  (setq irepeat-current-match nil))

(defun irepeat-handle-up-or-down()
  "Handle the return character on `read-event'"

  ;;make sure to start at zero

  (let((not-found t)
       (dont-search nil))

    (if (and (equal irepeat-event 'down)
             (equal irepeat-index 0))
        (progn

          ;;when we are at the beginning of the list (0) obviously we can't go
          ;;down any farther so set the index to -1 and the match to nil.

          (setq dont-search t)
          (setq irepeat-current-match nil)
          (setq irepeat-index -1))
      (if (and (equal irepeat-event 'up)
               (equal irepeat-index nil))
          (setq irepeat-index 0)))

    (while (and not-found
                (not dont-search)
                (>= irepeat-index -1)
                (< irepeat-index (length irepeat-list)))

      ;;figure out how to change the irepeat-index
      (if (equal irepeat-event 'up)
          (setq irepeat-index (1+ irepeat-index))
        (setq irepeat-index (1- irepeat-index)))

      (setq current (nth irepeat-index irepeat-list))
      
      (when (string-match (regexp-quote irepeat-query) (irepeat-to-string current))
        
        (setq irepeat-current-match current)
        
        (setq not-found nil)))

    (when not-found
      ;;if we searched throught the whole list, and didn't find anything, set
      ;;the current match to nothing. (makes sense)
      (setq irepeat-current-match nil)))

  (setq irepeat-skip-guess t)
  
  (setq irepeat-keep-matching t))
  
;;; key bindings.
(global-set-key [?\C-r] 'irepeat-complex-command)
(global-set-key "\C-cj" 'irepeat-semantic-jump)
(global-set-key "\C-xb" 'irepeat-switch-to-buffer)

(provide 'irepeat)

;;; irepeat.el ends here

;;; -- 
;;; Kevin A. Burton ( burton@apache.org, burton@openprivacy.org, burtonator@acm.org )
;;;              Location - San Francisco, CA, Cell - 415.595.9965
;;;         Jabber - burtonator@jabber.org,  Web - http://relativity.yi.org/

;;; These are dangerous days.
;;; To say what you feel is to dig your own grave.
;;; Remember what I told you.
;;; If they hated me, they will hate you.
;;;     - Sinead O'Connor

;;; [[End of PGP Signed Part]]
;;; [2. text/plain]


;;; _______________________________________________
;;; Gnu-emacs-sources mailing list
;;; Gnu-emacs-sources@gnu.org
;;; http://mail.gnu.org/mailman/listinfo/gnu-emacs-sources


