;;{{{ Copyright

;; Copyright (C) 1999-2006 Jesper K. Pedersen <blackie@blackie.dk>
;;
;; Author: Jesper Kjær Pedersen <blackie@blackie.dk>
;; Home page: http://www.blackie.dk/emacs/
;; This project started: 25 May 1999
;; This release: 
;; Version 1.3.1
;; Keywords: macros

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
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;}}}
;;{{{ Commentary

;; Keyboard Macros are a very powerful tool, if you know how to use them
;; the right way!  Without this extension it is, however, a bit difficult in
;; Emacs to define and maintain several macros at a time. This problem is
;; solved with this package.
;;
;; When you have loaded this package Emacs will, upon completion of 
;; macro definition, ask you which key you want to assign this macro
;; to and ask for a description of the macro. 
;; If something is already bound to this key, Emacs will ask you 
;; whether you want to override this binding. Furthermore, this
;; package also takes care of saving the macro to your .emacs file 
;; for later Emacs sessions.
;;
;; The description for all the defined macros may be obtained with the
;; function `pm-manage-macros'. Using this function you can also 
;; manage you power-macros. You execute this function by pressing `M-x' and
;; typing `pm-manage-macros'.
;;
;; In case you want macros to be save to a different file you may set the
;; variable `power-macros-file'. This can be done interactively for the
;; current session using the function `pm-change-save-file-for-this-session'

;;}}}
;;{{{ Installation description

;; To install this package, copy this file to a directory in your 
;; load-path and insert the following into your .emacs file:
;; (require 'power-macros)
;; 
;; To configure the package, simply type:
;; `M-x customize-group RET power-macros'

;;}}}
;;{{{ History information

;; 0.1 First official release.
;; 0.1.1 Fixed a few spelling errors (thanks to
;;       camille.troillard@worldonline.fr).
;; 0.2 Made the package into a real minor mode, added a menu to the
;;     menu-bar, and made it customizable using customize.
;; 0.2.1 Added support for XEmacs menus,
;;       thanks to Jan Vroonhof <vroonhof@math.ethz.ch>)
;;       and added a power-macros-hook.
;;       Fixed installation description.
;; 0.3a  - Major rewrites.
;;       - Added support for binding keys either global, local, or for
;;         a given mode.
;;       - Added support for saving and loading macros in several files.
;;       - Made the search for a possible collision with the key being
;;         bound.
;;       - Made managing of macros very easy using a context sensitive
;;         buffer with the description of all the macros.
;; 0.4a  - Major rewrites (again). This time due to realizing that 
;;         there is no such thing as a buffer local key map, which is 
;;         used together with major-mode map, minor-modes maps, and 
;;         the global map.
;;       - Lots of code cleanup, this is the last release before a 
;;         stable release is made.
;;       - Made it work with XEmacs (again).
;; 1.0   - Minor bug-fixes - and of course first stable release.
;; 1.1a1 - Answering with either RET or C-g to the question on
;;         which key to bind keyboard macros now makes power macros
;;         stop. This is for calling power-macros directly after 
;;         having defined a macro.
;;       - Macros are now by default saved to .emacs. This also means 
;;         that the files macros are saved to are edited rather than 
;;         overwritten.
;;       - Removed pm-load as this function is not necessary anymore,
;;         simply used load-file instead.
;;       - Macros are now properly removed from files in case where
;;         the last macro is removed.
;;       - When power-macro-mode has been loaded, pm-define will now
;;         by default be invoked when a macro has been recorded. This
;;         may be changed with the variable
;;         `power-macros-run-on-end-macro'.
;;       - As a result of this new strategy,
;;         `power-macros-advice-end-kbd-macro' has been removed.
;;       - Cleaned up the descriptions, so they now follow the
;;         convention that it should be possible to read the first
;;         line of the description separately.
;;       - The keybinding for `pm-manage-macros' has been removed on
;;         request from Richard Stallman. Now you need to invoke it
;;         as M-x pm-manage-macros.
;;       - Pressing one of the power-macro bound keys in the manage
;;         buffer, now has the same effect on the description text in
;;         front of a field, as it has on the field itself.
;;       - The package is no longer a minor-mode. This is due to a
;;         request from Richard Stallman, who wants to include it with
;;         Emacs version 21. This means that you only need to load
;;         this package and then it will activate upon completing
;;         macro definition.
;;       - The package does no longer depend on the `cl' package.
;; 1.1a2 - Fixed a bug which made 1.1a1 break (when the function
;;         y-n-question wasn't defined).
;; 1.1a3 - Documentation changes (thanks to David Wolff
;;         <dwolff@world.com>).
;;       - Lots of documentation changes on request from RMS.
;;       - pm-edit-desc do no longer copy the global keymap, 
;;         RMS told me that this was not necesary.  
;;       - pm-ask-for-mode now ask with simple y/n question on request 
;;         from RMS.
;;       - It is now possible to continue defining a macro from the
;;         managing buffer.
;;         It does, however, require a patch for it to work in 
;;         Gnu Emacs 20.3.1
;;       - `pm-edit-field' now asks whether you want to copy the macro 
;;         using y-or-n-p.
;;       - `pm-move-outside' now asks using `yes-or-no-p'.
;;       - Made the function pm-available-modes much simpler and much
;;         faster thanks to John Wiegley <johnw@gnu.org>.
;; 1.1a4 - pm-define now tells the user how to restart power-macros, when
;;         he has pressed RET to the question on which key to bind the
;;         macro.
;;       - Moved power-macros from customize group `Convenience'
;;         to `convenience'.
;;       - Added support in the manage buffer for macros not defined
;;         with power-macros.
;;       - The user will now be asked for a new key if he discovers that
;;         the key he was about to bind to (with pm-defin) is no good.
;;       - When asking for a key to bind to, the cursor will now be located
;;         in the minibuffer.
;;       - The user is now queryed for a name for the macro.
;;       - When the option `power-macros-verbose-on-load' is set,
;;         power-macros will tell which macros it loads.
;;       - Mode specific macro editing does now work (with regard to the
;;         names of the functions bound to mode specific keys).
;;       - Keys read now works with XEmacs.
;; 1.1a5 - A huge thanks to Rob van Vulpen <R.Vulpen@elsevier.nl>, who
;;         spell checked, grammer checked and much more on version 1.1a5.
;;       - It didn't work for XEmacs 20, since no (require 'edmacro) wasn't included,
;;         thanks to David Berthelot <d_berthelot@yahoo.com> for pointing this out for me.
;;       - Replaced edmacro-events-to-keys with events-to-keys for XEmacs.
;; 1.2   - 1.2 Released.
;; 1.2.1 - Fixed a bug, which made it impossible to edit global macros.
;;       - Added the possibility to define a macro without binding it to a key.
;;         thanks to Rob van Vulpen <R.Vulpen@elsevier.nl> for this idea.
;; 1.2.2 - Fixed a bug, where power-macros didn't insert a new line before
;;         writting the macro to a file. Thanks to Yaroslav Bulatov
;;         <bulatov@engr.orst.edu> 
;;         Added functions pm-change-save-file-for-this-session. Thanks to
;;         Yaroslav Bulatov <bulatov@engr.orst.edu> for this suggestion.
;;       - Fixed a bug where power-macros tried to save to a directory
;;         rather than a file, when Emacs was started with the "-q" option.
;; 1.2.3 - Fixed a bug, where power-macros would be confused if the macro contained 
;;         unmatched parenthesis.
;; 1.3   - New option power-macros-run-macros-protected. With this on, the macros will run in an escaped environment,
;;         kill ring and search info are restored after macro execution. In other words changes 
;;         done to those during execution of the macro do not persists.
;; 1.3.1 - The implementation above broke pm-manage-macros, so here is a second attempt that seems to work.
;;}}}
;;{{{ User definitions

(defgroup power-macros nil
  "Power Macros makes it much easier to work with macros.
The following features are the reason for that:
- With power-macros you may bind a macro directly to a key after 
  having defined it.
- With power-macros you may give your macros a description, which 
  makes it easier to maintain them.
- With power-macros you may define macros as being global or 
  major-mode specific.
- With power-macros your macros are automatically saved to your .emacs 
  file for later sessions."

  :group 'convenience)

(defcustom power-macros-file 
	(if (equal user-init-file "") "~/.emacs" user-init-file)
  "File in which new macros are saved by default.
If you change the file name to something different from `.emacs' then
you should insert the following line into your .emacs file:
\(load power-macros-file)
to ensure that macros are still loaded on startup."
  :type 'file
  :group 'power-macros)

(defcustom power-macros-save-on-change t
  "Non-nil means save macro to their files whenever a change is made.
That is, whenever a change is made in the macro-manage buffer."
  :type 'boolean
  :group 'power-macros)

(defcustom power-macros-run-on-end-macro t
  "Non-nil means that power-macros is executed upon macro definition.
When disabled you may invoke power-macros (that is `pm-define' to
be accurate) by pressing \\[pm-define]."
  :type 'boolean
  :group 'power-macros)

(defcustom power-macros-verbose-on-load t
  "Non-nil means that `pm-def-macro' will inform which macros are loaded."
  :type 'boolean
  :group 'power-macros)

(defcustom power-macros-run-macros-protected 't
  "Non-nil means that the macros execution do not effect clipboard and
search info after end of execution.
Changes will only take effect after macro are reloaded from file"

  :type 'boolean
  :group 'power-macros)

;;}}}

;;--------------------------------------------------
;;                   CODE
;;--------------------------------------------------

;;{{{ To-do

; TO-DO:
; - When moving a macro from one file to another when managing the 
;   macros, it should be checked for possible collisions with a power
;   macro in the new file.
; - When the user ends editing a keyboard macro (with kbd-edit-kbdmacro)
;   it should be checked if the power macros should be saved.
; - pm-set-mode-key should check for minor-modes too.
; - pm-update-manage-buffer should also update pm-tuple-list.

;;}}}

(require 'edmacro)
(require 'advice)
(eval-when-compile (require 'cl))

;;{{{ Misc. setup

(message "Loading power-macros...")

(defvar pm-warn-buffer nil
  "Buffer used when generating warnings on possible key overriding.")

(defvar pm-macro-files '()
  "List of files containing power macro definitions.") 

(defvar pm-continuing-macro nil
  "Contains the macro name when continuing a macro definition.")

;;------------------------------------------------------------
;; Variables used in the power-macro major mode
;;------------------------------------------------------------
(defvar pm-normal-edit-map (make-keymap)
  "This key map is used when not editing the description field.")

(defvar pm-desc-edit-map (make-keymap)
  "This key map is used when editing the description field.")

(defvar pm-token-list nil
  "List of fields in the manage buffer.
Each element in the list is a list itself with the following
items:
1: The type (either text, key, type, file, or description)
2: The start point of the text in front of the field
3: The start point of the field
4: The end point of the field.")

(defvar pm-tuple-list nil
  "List of of each of the tuple in the power-macro buffer.
Each tuple consist of the following information:
\(physical start point, logical start point, end point, macro name)")

(defvar pm-start-point nil
  "Start of the description field being edited in the mange buffer.")

(defvar pm-current-end-point nil
  "End of the description field being edited in the manage buffer.")

(defvar pm-old-end-point nil
  "Original end of the description field being edited in the manage buffer.
That is the location of the end before the editing started.")

(defvar pm-old-desc nil
  "Copy of the description field being edited in the manage buffer.")

(defvar pm-current-macro nil
  "Name of macro for which the description field is being edited.")

(defvar pm-del-text "Marked for deletion"
  "Text used to indicate that a record has been marked for deletion.")

(defvar pm-sort-function 'string<
  "Latest function used to sort the content of the management buffer.")

;; Key bindings for power macros.
(substitute-key-definition 'end-kbd-macro 'pm-end-kbd-macro global-map)
(substitute-key-definition 'start-kbd-macro 'pm-start-kbd-macro global-map)

;;}}}
;;{{{ Definition of macros

(defun pm-start-kbd-macro (arg)
  "Starts defining a keyboard macro."
  (interactive "P")
  (setq pm-continuing-macro nil)
  (start-kbd-macro arg))

(defun pm-end-kbd-macro ()
  "End defining keyboard macros and call power-macros on the new macro."
  (interactive)
  (end-kbd-macro)
  (if pm-continuing-macro
      (pm-end-of-continued-definition)
    (if power-macros-run-on-end-macro
        (pm-define))))

(defun pm-define ()
  "Bind the latest defined keyboard macro to a key, and describe it.
This will make Emacs ask for 
- a key to bind the macro to
- which type it should be 
- if the macro should be saved for future Emacs session
- a description for the macro."
  (interactive)

  ;; Test if a macro exists.
  (or last-kbd-macro
      (error (substitute-command-keys "No keyboard macro has been defined yet. Define one with \\[start-kbd-macro]")))

  (let ((ask-again t))
    (while ask-again
      (let* ((cursor-in-echo-area t) ; Ensures that the cursor is in 
                                     ; the echo aread for the next question.
             (k (pm-read-key-sequence 
                 "Bind macro to which key? (C-g to quit, RET not to bind to a key) ")))
        (if (member k '("\C-g" [(control ?g)]))
            (progn
              (message 
               (substitute-command-keys 
                "Macro not bound to any key. Press `\\[pm-define]' if you regret this."))
              (setq ask-again nil))
          
          (let* ((key (if (member k '("\C-m" [return]))
                          nil
                        k))
                 (mode (pm-ask-for-mode t))
                 (file (if (y-or-n-p "Should the macro be saved for future sessions? ")
                           (expand-file-name power-macros-file) ""))
                 (existing-name (if key 
                                    (pm-get-macros key mode file)
                                  nil))
                 (macro-name (pm-ask-for-macro-name existing-name)))
        
            ;; Verify if it is ok to bind to the key given.
            (when (or (eq key nil)
                      (catch 'pm-stop-def
                        (pm-possible-override t key mode file)
                        t))
              
              
              ;; Bind and describe the key.
              (pm-describe key mode file macro-name existing-name)
              (setq ask-again nil))
          ))))))


(defun pm-describe (key mode file macro-name old-name)
  "This function sets up a buffer for describing a keyboard macro."
  (let* ((desc (if key 
                   (pm-get-desc key mode file)
                 ""))
         (buffer (current-buffer)))
    (switch-to-buffer (pm-create-buffer))
    (text-mode)
    (use-local-map (make-keymap))
    (local-set-key [(control c) (control c)] 
                   `(lambda () (interactive) 
                      (pm-end-description ,key ',mode ,file 
                                          ',macro-name ',old-name)))
    (insert "Type a description of the macro below this line, and press C-c C-c\n")
    (insert "------------------------------------------------------------------\n")
    (if desc
        (insert desc))))

(defun pm-end-description (key mode file macro-name old-name)
  "Finish macro definition when the user has typed a description.

This function is invoked when the user presses C-c C-c in the
description buffer presented to him when he has just defined a macro. 

This function takes care of:
- Assigning the macro to the key.
- Removing the description buffer.
- Invoking the save function.
- Saving the description information"

  (beginning-of-buffer)
  (if (re-search-forward "-------------- *$" nil t)
      (forward-char))

  (pm-set-info mode key (buffer-substring (point) (point-max)) 
               file macro-name)
  (kill-buffer (current-buffer))
  (name-last-kbd-macro macro-name)
  (if key
      (pm-set-key macro-name))

  ;; Now delete the old macro if the names differs.
  (if (and (not (eq macro-name old-name)) 
           (not (eq old-name nil)))
      (unintern old-name))

  (pm-maybe-save)
  (message "Defined `%s'." macro-name))


(defun pm-ask-for-mode (initial-definition)
  "Function used to ask about kind of macro (global or mode specific).
Returns either `global' indicating that the macro should be a global
macro, or a symbol representing the chosen mode, for example
`c++-major-mode'."
  
  (let* ((global-def 
          (not (if initial-definition
                   (y-or-n-p "Should the macro only be defined for the current major mode? ")
                 (y-or-n-p "Should the macro only be defined for a single major mode? ")))))

    (if global-def
        'global

      ;; The macro should be mode specific!
      (if initial-definition
          major-mode

        ;; The mode is being changed from the manage-buffer, ask for the new mode.
        (let ((mode (completing-read 
                     "Mode name: " (pm-available-modes)
                     nil 1)))
          (if (string= mode "")
              (error "No mode selected.")
            (intern mode)))))))

;;}}}
;;{{{ Manage buffer

;;{{{ Building the buffer.

(defun pm-manage-macros ()
  "Bring up a manage buffer for editing properties of power macros."
  (interactive)

  ;; Switch to the buffer and insert the header.
  (switch-to-buffer (pm-create-buffer))
  (pm-macro-manage-mode)
  (use-local-map pm-normal-edit-map)

  (insert (substitute-command-keys 
"Manage macro key bindings, file save to macro to, and much more.

In this buffer you have the following possibilities:
- Press enter or the left mouse button on one of the fields to edit
  them. When you are done editing the key, type or file field, you
  are asked if your edits should form a new macro, or just update the
  existing one.
- Move from field to field with the tabulator key.
- Move from macro to macro by pressing page up / page down.
- Mark a macro for deletion by pressing `d'.
- Remove the deletion mark by pressing `u'.
- Delete macros marked for deletion by pressing `x'.
- Sort the entries by pressing either `s' or `S' on top of the field
  you wish to use for sorting.
- To edit one of the macros, simply press `e' on top of the macro.
- Continue recording a macro by pressing `c'.
- Press C-c C-c or \\[kill-buffer] to kill the buffer.

"))

  (let (key desc file macro-name mode p1 p2 tuple-start)
    (setq pm-token-list '())
    (setq pm-tuple-list '())    
    (dolist (macro (pm-sort (pm-get-available-macros)))
      (pm-initialize-macro macro)
      (setq  key (get macro 'key)
             desc (get macro 'documentation)
             file (get macro 'file)
             mode (get macro 'mode)
             )

      ;;---------- Separator
      (setq tuple-physical-start (point))
      (insert "--------------------------------------------------\n")
      (setq tuple-logical-start (point))

      ;; Insert a number of spaces equal to the length of the delete
      ;; text, to avoid that all the other fields moves, when a tuple
      ;; is marked for deletion.
      (insert (make-string (length pm-del-text) ?\ ) "\n")
      
      ;;---------- Name
      (setq p1 (point))
      (insert "Name: ")
      (setq p2 (point))
      (insert (symbol-name macro) "\n")
      (push (list 'name p1 p2 (point)) pm-token-list)
      (add-text-properties p2 (point) '(mouse-face highlight))

      ;;---------- Key
      (setq p1 (point))
      (insert "Key : ")
      (setq p2 (point))
      (if key
          (insert (key-description key) "\n")
        (insert "Not bound to any key.\n"))
      (push (list 'key p1 p2 (point)) pm-token-list)
      (add-text-properties p2 (point) '(mouse-face highlight))

      ;; ---------- Type
      (setq p1 (point))
      (insert "Type: ")
      (setq p2 (point))
      (insert (if (eq mode 'global) "global"
                (if (not mode) "unknown"
                  (concat "mode - " (symbol-name mode))))
              "\n")
      (push (list 'type p1 p2 (point)) pm-token-list)
      (add-text-properties p2 (point) '(mouse-face highlight))

      ;; ---------- File
      (setq p1 (point))
      (insert "File: ")
      (setq p2 (point))
      (insert (if (string= file "") "none" file) "\n")
      (push (list 'file p1 p2 (point)) pm-token-list)
      (add-text-properties p2 (point) '(mouse-face highlight))

      ;; ---------- Description
      (setq p1 (point))
      (insert "Description: ")
      (setq p2 (point))
      (if (string= desc "")
          (insert "No description.")
        (insert desc))
      (add-text-properties p2 (point) '(left-margin 3))
      (fill-region p2 (point))
      (insert "\n")
      (push (list 'desc p1 p2 (point)) pm-token-list)
      (add-text-properties p2 (point) '(mouse-face highlight))

      ;;---------- End of tuple
      (if (not (boundp 'xemacs-logo))
          (insert "\n"))

      (push (list tuple-physical-start tuple-logical-start (point) 
                  macro) pm-tuple-list))
    
    (setq pm-token-list (nreverse pm-token-list))
    (setq pm-tuple-list (nreverse pm-tuple-list))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    ))

;;}}}
;;{{{ Major mode definition

(define-derived-mode pm-macro-manage-mode fundamental-mode 
  "Macro Manage" "Mode for managing macros."

  ;; ---------- Normal edit map
  (define-key pm-normal-edit-map [(tab)] 'pm-next-field)
  (define-key pm-normal-edit-map [(meta tab)] 'pm-prev-field)
  (define-key pm-normal-edit-map [(next)] 'pm-next-macro)
  (define-key pm-normal-edit-map [(prior)] 'pm-prev-macro)
  (define-key pm-normal-edit-map [(control c) (control c)] 
    (lambda () (interactive) (kill-buffer (current-buffer))))
  (define-key pm-normal-edit-map [(return)] 'pm-edit-field-return)
  (if (boundp 'xemacs-logo)
      (define-key pm-normal-edit-map [(button1)] 'pm-edit-field-mouse)
    (define-key pm-normal-edit-map [(mouse-1)] 'pm-edit-field-mouse))
  (define-key pm-normal-edit-map [(d)] 'pm-set-delete-mark)
  (define-key pm-normal-edit-map [(u)] 'pm-unset-delete-mark)
  (define-key pm-normal-edit-map [(x)] 'pm-execute-deletion)
  (define-key pm-normal-edit-map [(e)] 'pm-edit-kbd-macro)
  (define-key pm-normal-edit-map [(s)] (lambda () (interactive) 
                                         (pm-set-sort-func 'string<)))
  (define-key pm-normal-edit-map [(S)] (lambda () (interactive) 
                                         (pm-set-sort-func 'pm-string>)))
  (define-key pm-normal-edit-map [(c)] 'pm-continue-define-macro)

  ;; ---------- Description edit map
  ;; Ordinary movement functions should still exists in the rest of
  ;; the buffer when editing the description field. Therefore only 
  ;; inserting commands are disabled. This does however not disable
  ;; commands like delete, return and backspace.
  ;; Therefore the rest of the buffer is also made read-only.

  (suppress-keymap pm-desc-edit-map)
  (substitute-key-definition 'undefined 'pm-move-outside 
                             pm-desc-edit-map)
  (define-key pm-desc-edit-map [(return)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(delete)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(backspace)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(control k)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(control w)] 'pm-move-outside)
  (define-key pm-desc-edit-map [(control g)] 
    (lambda () (interactive) (pm-end-edit-or-abort-desc-field 'abort)))
  
 )

;;}}}
;;{{{ Movement functions

(defun pm-next-field ()
  "Jumps to the next field in the manage buffer."
  (interactive)
  (pm-jump (lambda (x y) (< x y)) pm-token-list))

(defun pm-prev-field ()
  "Jumps to the previous field in the manage buffer."
  (interactive)
  (pm-jump (lambda (x y) (> x y)) (reverse pm-token-list)))

(defun pm-next-macro ()
  "Jumps to the key field of the next macro in the manage buffer."
  (interactive)
  (pm-jump (lambda (x y) (< x y)) pm-token-list 'name)  
)

(defun pm-prev-macro ()
  "Jumps to the key field of the previous macro in the manage buffer."
  (interactive)
  (pm-jump (lambda (x y) (> x y)) (reverse pm-token-list) 'name)  
)

(defun pm-jump (dirp l &optional type)
  "Jumps to the next or previous field in the manage buffer.
If the optional type is given, then the field must be of the given
type."
  (let ((p (point))
        (found nil)
        (first-pos nil))
    (while l
      (when (and (not first-pos) (or (not type) (eq type (caar l))))
        (setq first-pos (cadar l)))
      (if (and (funcall dirp p (cadar l)) (or (not type) 
                                              (eq type (caar l))))
          (progn 
            (goto-char (cadar l))
            (setq l '())
            (setq found t))
        (setq l (cdr l))))
    (if (not found)
        (goto-char first-pos))))

;;}}}
;;{{{ Editing functions

(defun pm-edit-field-return ()
  "See the description for `pm-edit-field'."
  (interactive)
  (pm-edit-field nil))

(defun pm-edit-field-mouse (event)
  "See the description for `pm-edit-field'."
  (interactive "e")
  (if (boundp 'xemacs-logo)
      (goto-char (event-closest-point event)))
  (pm-edit-field t))

(defun pm-edit-field (is-mouse)
  "Edits the field at point or under mouse in the manage buffer.

If the field is a description field, Emacs lets the user edit it in
the manage buffer. Otherwise he is asked for a new value in the
mini-buffer.

This function is invoked when either the enter key or the
first mouse button is pressed in the manage buffer."

  (let* ((elm (pm-get-context))
        (widget-type (car elm))
        (startText (cadr elm))
        (startField (caddr elm))
        (end (cadddr elm))
        (macro (if (not (eq widget-type 'text))
                   (pm-get-macro-at-point)
                 nil))
        (key (get macro 'key))
        (file (get macro 'file))
        (mode (get macro 'mode))
        (desc (get macro 'documentation))
        (do-reload nil)
        do-copy tuple)

    (cond 
     
     ((eq widget-type 'text)
      (if (not is-mouse)
          (beep)))

     ((eq widget-type 'desc)
      (pm-edit-desc startField end macro))

     ((eq widget-type 'name)
      (pm-edit-name startField end macro))

     (t
      (progn
        ;; Ask if the macro should be copied before editing.
        (setq do-copy 
              (y-or-n-p
               "Do you want to copy the macro before editing it? "))
        
        ;; Call the specific editing functions.
        (cond ((eq widget-type 'key)
               (setq key (pm-edit-key startField end mode file)))
              ((eq widget-type 'type)
               (setq mode (pm-edit-type startField end key file)))
              ((eq widget-type 'file)
               (setq file (pm-edit-file startField end)))
              (t (error "Unknown widget-type `%s'" widget-type)))
        
        ;; Now delete the original macro if there is one on the 
        ;; given key.
        (let ((existing-macro (pm-get-macros key mode file)))
          (if existing-macro
              (progn
                (pm-delete-macro existing-macro)
                (setq do-reload t))))
        
        ;; Update the key bindings.
        (if do-copy
            ;; Copy the macro
            (let ((other-macro (pm-ask-for-macro-name nil)))
              (fset other-macro (symbol-function macro))
              (setplist other-macro 
                        (copy-sequence (symbol-plist macro)))
              (setq macro other-macro); this makes the code below
                                        ; common for both paths
              (setq do-reload t)
              )
          ;; Edit the key
          (pm-unset-key macro)
          )
        
        ;; Set the new value for the new macro.
        (pm-set-info mode key desc file macro)
        
        ;; Define the binding for the new key.
        (pm-set-key macro)
        
        ;; Update the buffer if type is copy.
        (if do-reload
            (pm-manage-macros))))) ; Lets restart, that's the easiest!

    (set-buffer-modified-p nil)))

;;}}}
;;{{{ Editing of key, type, file, and name

(defun pm-edit-name (start end macro)
  "Change the name for macro at point in the manage buffer."
  (let ((new-macro (pm-ask-for-macro-name macro)))
    (if (eq macro new-macro)
        (message "Name is unchanged.")
      
      (fset new-macro (symbol-function macro))
      (setplist new-macro
                (copy-sequence (symbol-plist macro)))
      (pm-delete-macro macro)
      (pm-set-key new-macro)

      (pm-manage-macros))))

(defun pm-edit-key (start end mode file)
  "Change the key for macro at point in the manage buffer.

This function is invoked when enter is pressed on the `key' field of
the manage buffer."
  (let* ((cursor-in-echo-area t)
         (key (pm-read-key-sequence "Which key: "))
         (key-descs (key-description key)))

    ;; Check if the new binding is ok.
    (pm-possible-override nil key mode file)

    (pm-update-manage-buffer start end key-descs)
    key ; Return value.
    ))


(defun pm-edit-type (start end key file)
  "Change the type for macro at point in the manage buffer.

This function is invoked when enter is pressed on the `type' field of
the manage buffer."
  (let ((mode (pm-ask-for-mode nil))
        text)
    (if (not (eq mode 'global))
             (setq text (format "mode - %s" (symbol-name mode)))
      (setq text (symbol-name mode)))

    ;; Check if the new binding is ok.
    ;; (pm-possible-override nil key mode file)
    ;; This check has been disabled as it most often only will report a
    ;; conflict with the macro itself.

    (pm-update-manage-buffer start end text)
    mode ; Return value.
    ))

(defun pm-edit-file (start end)
  "Change the file to which macro at point, in the manage buffer, is saved. 

This function is invoked when enter is pressed on the `file' field of
the manage buffer."
  (let* ((old-name (buffer-substring start end))
         (file (pm-read-file-name "New file name: " 
                                  (file-name-directory old-name) nil)))
    (pm-update-manage-buffer start end 
                             (if (string= file "") "none" file))
    file))

(defun pm-read-file-name (prompt dir default)
  "Reads a filename from the mini-buffer and checks if it is a directory."
  (let ((save (y-or-n-p "Should the macro be saved for future sessions? "))
        (file-name-history pm-macro-files))
    (if save
        (let* ((f (read-file-name prompt dir default))
               (file (if (string= f "") f (expand-file-name f))))
          (if (and file (file-directory-p file))
              (error "Filename is a name of a directory.")
            file))
      "")))

;;}}}
;;{{{ Editing of description

(defun pm-edit-desc (start end macro)
  "Set up the manage buffer to edit the description for macro at point.

This means that the whole buffer (except the region containing the
description) is read-only.

This function is invoked when enter is pressed on the description
field in the manage buffer."

  ;; Make the desc part writable.
  (setq buffer-read-only nil)

  ;; Go to start of region and record information about the text.
  (goto-char start)
  (setq pm-start-point (point))
  (setq pm-current-end-point (copy-marker end))
  (setq pm-old-end-point end)
  (setq pm-old-desc (buffer-substring start (- end 1)))
  (setq pm-current-macro macro)

  ;; Change the bindings.
  (use-local-map pm-desc-edit-map)
  (let ((keym (copy-keymap global-map))) ;; It is necesary to copy the map for
                                         ;; things to work in XEmacs.
    (define-key keym [(control c) (control c)] 
      (lambda () (interactive) (pm-end-edit-or-abort-desc-field 'end)))
    (define-key keym [(control g)] 
      (lambda () (interactive) 
        (pm-end-edit-or-abort-desc-field 'abort)))
    (if (boundp 'xemacs-logo)
        (add-text-properties start end `(keymap ,keym))
      (add-text-properties start end `(local-map ,keym))))

  ;; Make it impossible to insert at the beginning of the buffer.
  (add-text-properties 1 2 '(front-sticky (read-only)))
  
  ;; Make it possible to insert at the first point in the description
  ;; area.
  (add-text-properties (- start 1) start 
                       '(rear-nonsticky (face read-only)))

  ;; Now make everything else than the description area read-only.
  ;; This is to ensure that the user has not bound any strange key,
  ;; which is not disabled by the new key map.
  (make-face 'pm-face)
  (set-face-foreground 'pm-face "grey")
  (add-text-properties (point-min) start '(read-only t face pm-face))
  (add-text-properties (- end 1) (point-max) 
                       '(read-only t face pm-face))
  (message "Press C-c C-c to submit changes or C-g to abort.")
)

(defun pm-end-edit-or-abort-desc-field (type)
  "Restores the editing properties of the manage buffer.

When the user ends editing a description field in the manage buffer,
then the manage buffer must be activated again, that is pressing
enter on one of the fields should edit this field, etc."
  (setq inhibit-read-only t)
  (remove-text-properties (point-min) (point-max) 
                          '(read-only t local-map t keymap t face t))
  (setq inhibit-read-only nil)

  (if (eq type 'abort)
      (progn
        (delete-region pm-start-point pm-current-end-point)
        (goto-char pm-start-point)
        (insert pm-old-desc "\n"))
    (progn
      (pm-update-info-lists pm-start-point (- pm-current-end-point
                                              pm-old-end-point))
      (set-text-properties pm-start-point (point) 
                           '(mouse-face highlight))
      (put pm-current-macro 'documentation 
           (buffer-substring-no-properties pm-start-point 
                                           pm-current-end-point))))
  (setq pm-current-end-point nil)
  (use-local-map pm-normal-edit-map)
  (setq buffer-read-only t))

(defun pm-move-outside (&rest x)
  "Query user for edits outside the description field of the manage buffer.

When the user is editing a description in the manage buffer, then
he must complete these edits before proceeding with other edits. If he
makes an edit outside the region containing the description, then he
is queried whether he wants to discard his edits."

  (interactive)

  (let ((discard
         (yes-or-no-p "You have moved outside the description area, discard edits? ")))
    (if discard
        (pm-end-edit-or-abort-desc-field 'abort)
      (progn
        ;; c or control g has been pressed.
        (goto-char pm-start-point)
        (message "Continue editing the description. End with either C-c C-c or C-g")
        ))))

;;}}}
;;{{{ Info-list management

(defun pm-get-context ()
  "Returns the type of field at point in the manage buffer.

The return value is an item from the list `pm-token-list'."

  (let ((p (point))
        (l pm-token-list)
        (last-end (point-min))
        type start end res elm startText startField) 
    (if (null l)
        (list 'text nil nil nil)
      (if (< p (cadr (car l)))
          (list 'text (point-min) nil (cadr (car l)))
        (progn
          (while l
            (setq elm (car l)
                  l (cdr l)
                  type (car elm)
                  startText (cadr elm)
                  startField (caddr elm)
                  end (cadddr elm))
            (if (< p startText)
                (setq res (list 'text nil nil nil))
              (if (< p end)
                  (setq res elm)))
            (if (not (null res))
                (setq l '()))
            (setq last-end end))
          
          (if (null res)
              (list 'text nil nil nil)
            res))))))


(defun pm-update-info-lists (start incr)
  "Updates `token-list' and `tuple-list' with INCR at pos START."

  ;; Update the token list.
  (let ((res '())
        tuple-type tuple-start tuple-end)
    (dolist (elm (reverse pm-token-list))
      (setq tuple-type (car elm)
           tuple-startText (cadr elm)
           tuple-startField (caddr elm)
           tuple-end (cadddr elm))
      (if (> tuple-startField start)
          (push (list tuple-type (+ tuple-startText incr) 
                      (+ tuple-startField incr)
                      (+ tuple-end incr)) res)
        (if (= tuple-startField start)
            (push (list tuple-type tuple-startText tuple-startField 
                        (+ tuple-end incr)) res)
          (push elm res))))
    (setq pm-token-list res))

  ;; Update the tuple list.
  (let ((res '())
        p-start l-start end name)
    (dolist (elm (reverse pm-tuple-list))
      (setq p-start (car elm)
            l-start (cadr elm)
            end (caddr elm)
            name (cadddr elm))
      (if (> p-start start)
          (push (list (+ p-start incr) (+ l-start incr) (+ end incr) 
                      name) res)
        (push elm res)))
    (setq pm-tuple-list res)))


(defun pm-update-manage-buffer (start end text)
  "Inserts TEXT in place of content from START to END in the manage buffer.
Furthermore it also makes the text highlight when mouse is over the
given region."

  (setq buffer-read-only nil)
  
  (delete-region start end)
  (goto-char start)
  (insert text "\n")
  
  (add-text-properties start (+ start (length text))
                       '(mouse-face highlight))
  (setq buffer-read-only t)

  ;; Now update the info lists.
  (pm-update-info-lists start (+ 1 (- (length text) (- end start))))
)
    
(defun pm-delete-tuple-in-token-list (start end)
  "Removes tuples from `pm-token-list' in the region between START and END."
  (let ((l pm-token-list) 
        elm t-start t-end)
    (setq pm-token-list '())
    (while l
      (setq elm (car l)
            l (cdr l)
            t-start (cadr elm)
            t-end (cadddr elm))
      (if (not (and (>= t-start start) (<= t-end end)))
          (push elm pm-token-list)))
    (setq pm-token-list (nreverse pm-token-list))))

(defun pm-get-delete-pos ()
  "Finds position for \"delete\" for macro at point in manage buffer.
That is, the position where the word \"delete\" may be inserted when
the user presses `d' in the manage buffer."
  (pm-get-tuple-info 'cadar))

(defun pm-get-macro-at-point ()
  "Returns the name of the macro at point."
  (pm-get-tuple-info 'cadddar))

(defun pm-get-tuple-info (f)
  "Returns information from the tuple at point in the manage buffer.
The actual information is determined by the function F given as
argument to the function."
  (let ((p (point))
        (l pm-tuple-list)
        (res nil))
    (if (null l)
        (error "No macros exists!")
      (if (< p (cadar l))
          (error "Not located on a macro!"))
      (while l
        (if (< p (caddar l))
            (setq res (funcall f l)
                  l '())
          (setq l (cdr l))))
      (if (eq res nil)
          (error "Not located on a macro!"))
      res)))

;;}}}
;;{{{ Deletion

(defun pm-set-delete-mark ()
  "Mark the macro at point, in the manage buffer, for deletion."
  (interactive)
  (pm-set-or-unset-delete-mark 'set))

(defun pm-unset-delete-mark ()
  "Remove the deletion mark, for macro at point, in manage buffer."
  (interactive)
  (pm-set-or-unset-delete-mark 'unset))

(defun pm-set-or-unset-delete-mark (type)
  "Set or unset the deletion mark for macro at point in the manage buffer."
  (save-excursion
    (let ((pos (pm-get-delete-pos)))
      (setq buffer-read-only nil)
      (goto-char pos)
      (delete-region pos (+ pos (length pm-del-text)))
      (if (eq type 'set)
          (progn
            (insert pm-del-text)
            (set-text-properties pos (point) '(face modeline)))
        (insert (make-string (length pm-del-text) ?\ )))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil))))


(defun pm-execute-deletion ()
  "Delete the macros marked for deletion in the manage buffer."
  (interactive)
  (setq buffer-read-only nil)
  (save-excursion
    (let ((new-list '())
          elm p-start l-start end name)
    
      (while pm-tuple-list
        (setq elm (car pm-tuple-list)
              pm-tuple-list (cdr pm-tuple-list)
              p-start (car elm)
              l-start (cadr elm)
              end (caddr elm)
              name (cadddr elm))
        (goto-char l-start)
        (if (looking-at pm-del-text)
            (progn
              (delete-region p-start end)
              (pm-delete-tuple-in-token-list p-start end)
              (pm-update-info-lists p-start (- p-start end))
              (pm-delete-macro name))
          (push elm new-list)))
      (setq pm-tuple-list (nreverse new-list))))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (pm-maybe-save))

(defun pm-delete-macro (macro)
  "Deletes the macro named MACRO, and remove its key-bindings."
  (pm-unset-key macro)
  (unintern macro))

;;}}}
;;{{{ Edit macro definition

;; The tricky part with the function below is that edit-kbd-macro does 
;; not support a back-end to enter the edit buffer. Thus I need to go
;; trough the front-end, which requires that I answer a question in
;; the mini-buffer. Luckily this can be solved with a macro ;-)

(defun pm-edit-kbd-macro ()
  "Edits the keyboard macro at point in the manage buffer."
  (interactive)
  (let* ((macro (pm-get-macro-at-point))
         (macro-str (symbol-name macro))
         (mode (get macro 'mode))
         (new-mac (vconcat [?\M-x] "edit-kbd-macro" [return ?\M-x]
                           macro-str [return])))

    ;; I need to switch buffer to avoid that d,e,x have the meaning
    ;; pm-delete-macro, pm-edit-kbd-macro etc. 
    (when (not (eq mode 'global))
      (switch-to-buffer (get-buffer-create "*power-macro-dummy*"))
      (funcall mode))
      
    (execute-kbd-macro new-mac)
    ;; Any idea how I can get pm-maybe-save executed when the user
    ;; finishes editing the macro? The hook doesn't work when I enter
    ;; the editing the above way.
    ))

;;}}}
;;{{{ Continuing macro definition

(defun pm-continue-define-macro ()
  "Continue defining macro definition from manage macro buffer."
  (interactive)
  (let* ((macro (pm-get-macro-at-point))
         (definition (copy-sequence (symbol-function macro))))
    (setq last-kbd-macro definition)
    (switch-to-buffer nil)
    (if (y-or-n-p "Continue macro definition in this buffer? ")
        (progn
          (setq pm-continuing-macro macro)
          (start-kbd-macro t))
      (progn
        (switch-to-buffer nil)
        (message "Aborted continued macro definition")))))
                                     
(defun pm-end-of-continued-definition ()
  "Ends the continued definition of a macro."
  (interactive)
  (fset pm-continuing-macro (copy-sequence last-kbd-macro))
  (setq pm-continuing-macro nil)
  (pm-save))

;;}}}
;;{{{ Sorting

(defun pm-sort (macros)
  "Sorts the macros in the manage buffer.
This is done based on the value of the variable `pm-sort-function'."
  (sort macros pm-sort-function))

(defun pm-set-sort-func (direction)
  "Sorts the tuple in the manage buffer based on the field at point."
  (let* ((elm (pm-get-context))
        (type (car elm)))

    (if (eq type 'text)
        (error "You must press `s' on top of a field.")
      (if (eq type 'name)
          (setq pm-sort-function 'string<)
        (setq pm-sort-function
              `(lambda (x y) (,direction (format "%s" (get x ',type))
                                         (format "%s" (get y ',type))))))))
  (pm-manage-macros))

(defun pm-string> (x y) (not (string< x y)))

;;}}}

;;}}}
;;{{{ Saving

(defun pm-maybe-save ()
  "Verifies whether changes in the manage buffer should be saved.

This function is invoked whenever a change is made in the manage
buffer."
  (if power-macros-save-on-change
      (pm-save)))


(defun pm-save ()
  "Saves the macros to files."
  (interactive)
  (let (macros)
    (dolist (file pm-macro-files)
      (setq macros (pm-select `(lambda (name) 
                                 (equal (get name 'file) ,file)) 
                              (pm-get-available-macros)))
      (set-buffer (find-file-noselect file))
      (pm-save-delete file)
      
      (dolist (macro macros)        
        (pm-write-tuple-to-buffer (current-buffer) macro))

      (when (null macros)
        (setq pm-macro-files (delete file pm-macro-files)))
      
      (write-file file)
)))


(defun pm-save-delete (file)
  "Delete all the invocations of `pm-def-macro' in the current buffer.
Leave point at the location of the last call, or after the last
expression."
  (goto-char (point-min))
  (let ((last-pos (point-max))
        sexp point-before-sexp)
    (while (setq point-before-sexp (point)
                 sexp 
                 (condition-case nil
                     (read (current-buffer))
                   (end-of-file nil)))

      ; Repeat until we have read the whole buffer.
      (when (and (listp sexp) (eq (car sexp) 'pm-def-macro))
        (delete-region point-before-sexp (point))

        ; ensure that no spaces are added just because we save a
        ; number of times
        (when (and (looking-at "$") (/= (point) (point-max)))
          (delete-char 1)) 
        (when (and (looking-at "$") (/= (point) (point-max)))
          (delete-char 1)) 

        (setq last-pos (point))))
    (goto-char last-pos)))

(defun pm-write-tuple-to-buffer (buf macro)
  "This functions prints out MACRO to BUF."
  (let* ((mode (get macro 'mode))
        (key (get macro 'key))
        (desc (get macro 'documentation))
        )
    
    ;; print header.
    (princ (concat "\n(;-" (pm-center ?- 66 " power-macros ") "-\n") buf)
    (let ((text (concat (if key 
                            (key-description key)
                          "Unbound")
                        " - "
                        (if (eq mode 'global)
                            "global definition"
                          (concat "specific for " 
                                  (symbol-name mode))))))
      (princ (concat " ;-" (pm-center ?\ 66 text) "-\n") buf))
    (princ (concat " ;" (make-string 68 ?-) "\n") buf)
                                    
    (princ "\tpm-def-macro\n\t'" buf)
    (prin1 macro buf) (princ "\n\t'" buf)
    (prin1 mode buf) (princ " " buf)
    (prin1 key buf) (princ "\n\t" buf)
    (prin1 desc buf) (princ "\n\t" buf)
    (prin1 (format-kbd-macro macro) buf)
    (princ (concat "\n ;" (make-string 68 ?-) "\n") buf)
    (princ ")\n\n" buf)
))


(defun pm-center (fill-char num text)
  "Returns TEXT with FILL-CHAR around it to center it on a line width NUM."
  (let* ((mid (max (/ (- num (length text)) 2) 0))
        (extra (max (- num (length text) (* 2 mid)) 0)))
    (concat (make-string mid fill-char) text 
            (make-string (+ extra mid) fill-char))))

(defun pm-change-save-file-for-this-session (filename)
	"Changes the file that macros are stored in for this session."
   (interactive "FFile to store macros in  ")
   (setq power-macros-file filename)
   (message "Macros are now stored in %s" filename))

;;}}}
;;{{{ Loading

(defun pm-def-macro (macro-name mode key desc macro-def)
  "Defines a new macro.
This function is the one written into the init file."
  (let* ((file (if load-file-name 
                   load-file-name 
                 (if buffer-file-name
                     buffer-file-name
                   (error "I can't figure out the current file name. You must call pm-def-macro from your .emacs file or another file loaded!")))))

    ;; Verify that no symbol exists with the given name.
    (if (fboundp macro-name)
        (error "A macro or function with name `%s' does already exists." macro-name))

    ;; Define the macro.
    (fset macro-name (read-kbd-macro macro-def))
    
    ;; Set the info about the macro.
    (pm-set-info mode key desc (expand-file-name file) macro-name)

    ;; Bind the key.
    (if key
        (pm-set-key macro-name))

    ;; Inform the definition of the macro.
    (if power-macros-verbose-on-load
        (message "loaded macro `%s'" macro-name))))

;;}}}
;;{{{ Database Management

;; These functions takes care of managing the key bindings,
;; descriptions and file information The data is located as
;; properties in the symbol defining the macro.
;;
;; The following elements exist:
;;     key           - The key for which this macro is bound.
;;     documentation - The description of this macro.
;;     file          - The file in which this macro is saved.
;;     mode          - The name of the major mode for mode specific
;;                     macros or the symbol global.

(defun pm-get-macros (key mode file &optional all)
  "Returns the macro bound to KEY for mode MODE for file FILE.
Any of the fields might be nil, which means that they are wild-cards. 
If ALL is t, then a list of matches is returned."
  (let ((list (pm-get-available-macros))
        (res '())
        macro)
    (while list
      (setq macro (car list)
            list (cdr list))
      (if (and (or (null key) (equal (get macro 'key) key))
               (or (null mode) (eq (get macro 'mode) mode))
               (or (null file) (equal (get macro 'file) file)))
          (push macro res)))

    (if all
        res
      (if (> (length res) 1)
          (error "Internal error: More that one tuple matched the query.")
        (car res)))))

(defun pm-get-desc (key mode file)
  "Returns macro description for the given key/mode/file."
  (let ((name (pm-get-macros key mode file)))
    (if (null name)
        ""
      (get name 'documentation))))

(defun pm-set-info (mode key desc file macro-name)
  "Inserts information into the power-macro database."
  (put macro-name 'mode mode)
  (put macro-name 'key key)
  (put macro-name 'documentation desc)
  (put macro-name 'file file)
  (if (and (not (string= file "")) (not (member file pm-macro-files)))
      (push file pm-macro-files)))

(defun pm-get-available-macros ()
  "Returns the keyboard macros defined."
  (let* ((macros '())
         (find-macros 
         (lambda (s)
           (if (and (fboundp s)
                    (or (stringp (symbol-function s))
                        (vectorp (symbol-function s))))
               (setq macros (cons s macros))))))
    (mapatoms find-macros)
    macros))

(defun pm-initialize-macro (macro)
  "Initialized a macro with the attributes required by power-macro."
  (let ((key (get macro 'key))
        (desc (get macro 'documentation))
        (file (get macro 'file))
        mode map map-name)

    (when (eq key nil)
      ;; Search for a key where this macro is bound to.
      (let ((keys (where-is-internal macro global-map)))
        (if keys
            ;; The key was bound to the global map.
            (progn
              (put macro 'key (car keys))
              (put macro 'mode 'global))

          ;; Search for a key binding in one of the mode maps.
          (dolist (mode-l (pm-available-modes))
            (setq mode (car mode-l))
            (setq map-name (intern (concat mode "-map")))
            (setq map (if (boundp map-name)
                          (eval map-name)
                        nil))
            (when (keymapp map)
              (setq keys (where-is-internal macro map))
              (when keys
                (put macro 'key (car keys))
                (put macro 'mode (intern mode))))))))

    (when (eq file nil)
      (put macro 'file ""))

    (when (eq desc nil)
      (put macro 'documentation ""))))

;;}}}
;;{{{ Key binding check

;;{{{ Description

; The function pm-possible-override takes care of warning the user if
; he is about to overwrite a key or shadow key. Below the rules for
; checking this are described.

;--------------- meaning of symbols ---------------
; 
; (+) Means that the given test is implemented.
; (-) Means that the given test is not implemented. This is either
;     because I don't know how to do it, or because I find it very
;     unlikely that it will ever be used.
;
; (key, mode, file) in set pm-bind
;     Check if pm-macros contain a definition for the given
;     key,type,mode,file tuple.
;     A dash means that the given field does have a meaning (only used
;     for mode).
;     An asterisk is a wildcard meaning that this field should not be
;     included when searching for tuples.

; Key in set map.
;    Check if key is bound in the given map.
;
; Override.
;    This means that if the given check returns true, then the new
;    definition will override the given match(es).
;
; Shadows.
;    This means that if the given check returns true, then the new
;    definition will shadow the given match(es). Which means that the
;    existing definition(s) will not be visible.
;
; Invisible
;    This means that if the given check returns true, then the new
;    definition will have no effect. 
;
; Note: that the two clauses above might only be true for the given 
; mode or the given buffer.
; 
; 1)
;    This means test number one for both new bindings and existing
;    bindings.
; 1 new)
;    This means test number one. It should, however, only be done for
;    new bindings.

;--------------- Want to bind a global key ---------------
; (+) 1)     Override  - (key,global,-,file) in set PM-bind
; (-) 2)     Override  - (key,global,-,*)    in set PM-bind
; (+) 3)     Override  - key in set global-map
; (-) 4 new) Invisible - (key,active-modes,*) in set PM-bind
; (+) 5 new) Invisible - key in set *-active-mode-map
; (-) 6)     Invisible - (key,*-mode,*) in set PM-bind
; (-) 7)     Invisible - key in set *-mode-map

;--------------- Want to bind a mode specific key ---------------
; (+) 1)      Override  - (key,mode,file) in set PM-bind
; (-) 2)      Override  - (key,mode,*) in set PM-bind
; (+) 3)      Override  - key in set mode-map
; (-) 4)      Shadows   - (key,global,*) in set PM-bind
; (+) 5)      Shadows   - key in set global-map
; (-) 6 new)  Invisible - (key,active-minor-modes,*) in set PM-bind
; (+) 7 new)  Invisible - key in set active-minor-modes-maps
; (-) 8)      Invisible - (key,*-minor-mode,*) in set PM-bind
; (-) 9)      Invisible - key in set *-minor-modes-map

;;}}}
;;{{{ pm-possible-override

(defun pm-possible-override (new-def key mode file) 
  "Check whether the new key binding violates an existing one.
For a list of all the different checks, please see the description in
the source code."

  (let* ((buf (pm-create-buffer "*Warning*"))
         (any-output nil)
         main-macro macro-list macro)
    (setq pm-warn-buffer buf)
    (pm-insert "Your new definition might violate an existing definition. Below you
see a report of what will happen if you make your binding.")

    
    ;; First we need to attach power-macro information to every macro.
    (dolist (macro (pm-get-available-macros))
      (pm-initialize-macro macro))

    ;; -------------------------------
    ;; Rule no. 1 is the same for both
    ;; -------------------------------
    (setq main-macro (pm-get-macros key mode file))
    (if main-macro
        (progn
          (setq any-output t)
          (pm-insert "\n
---------------------------------------------------------------------

This definition will " (pm-outstand "replace") " an existing macro on the same key,
with the same type and defined in the same file. Its description is:\n")
          (pm-insert (pm-indent (get main-macro 'documentation)))))

      
    ;; -------- The rest of the rules ---------
    (setq any-output (or (cond 
                          ((eq mode 'global) 
                           (pm-override-global new-def key file 
                                               main-macro))
                          
                          (t
                           (pm-override-mode new-def key file mode 
                                             main-macro))) 
                         any-output))

    (pm-insert "\n") ; This is to ensure that there is a location, 
                     ; which is not with the left-margin property.

    (if any-output
        (save-excursion
          (switch-to-buffer buf)
          (pm-indent-regarding-to-properties)
          (setq buffer-read-only t)
          (goto-char (point-min))
          (if (not (yes-or-no-p "Continue defining the macro? "))
              ;; Stop defining the macro.
              (progn
                (kill-buffer buf)
                (throw 'pm-stop-def nil)))))
    
      ;; Finally kill the buffer.
    (kill-buffer buf)))

;;}}}
;;{{{ pm-override-global

(defun pm-override-global (new-def key file main-macro)
  "Checks if the new global key binding violates an existing binding.
See the source code for all the different checks."

  (let ((any-output nil))
        

    ;; --------------------------------
    ;; Rule no. 3
    ;; Override - key in set global-map
    ;; --------------------------------
    (setq any-output 
          (or (pm-check-binding 'global key 'conflict main-macro) 
              any-output))

    ;; ----------------------------------------
    ;; Rule no. 5 (only for new macros)
    ;; Invisible - key in set *-active-mode-map
    ;; ----------------------------------------
    (if new-def
        (setq any-output
              (or (pm-check-binding 'mode key 'invisible  
                                    main-macro major-mode) 
                  any-output)))

    (if new-def
        (setq any-output
              (or (pm-check-binding 'minor-mode key 'invisible 
                                    main-macro) 
                  any-output)))


    any-output)) ; Return value.

;;}}}
;;{{{ pm-override-mode

(defun pm-override-mode (new-def key file mode main-macro)
  "Checks if the new mode key binding violates an existing bindings.
See the source code for all the different checks."

  (let ((any-output nil))

    ;; ---------------------------------------
    ;; Rule no. 3
    ;; Override  - key in set current-mode-map
    ;; ---------------------------------------
    (setq any-output
          (or (pm-check-binding 'mode key 'conflict  main-macro mode)
              any-output))
    

    ;; ---------------------------------
    ;; Rule no. 5
    ;; Shadows   - key in set global-map
    ;; ---------------------------------
    (setq any-output
          (or (pm-check-binding 'global key 'shadows main-macro)
              any-output))

    ;; ----------------------------------------------
    ;; Rule no. 7 (only for new macros)
    ;; Invisible - key in set active-minor-modes-maps
    ;; ----------------------------------------------
    (if new-def
        (setq any-output
              (or (pm-check-binding 'minor-mode key 'invisible 
                                    main-macro)
                  any-output)))

    any-output)) ; Return value.

;;}}}
;;{{{ pm-check-binding

(defun pm-check-binding (type key warn-type main-macro &optional mode)
  "Verify if a key binding of type TYPE exists for KEY.
If this is the case then this function will insert a warning of type
WARN-TYPE, telling that there is a problem. 
The function returns whether there was anything to report.

WARN-TYPE is one of: conflict, invisible, shadows
TYPE is one of: global, mode, minor-mode."

  (let* (map-name
         (func (cond ((eq type 'global) (pm-lookup-key global-map key))
                     ((eq type 'mode)
                      (progn
                        (setq map-name (intern 
                                        (concat (symbol-name mode) 
                                                "-map")))
                        (if (boundp map-name)
                            (pm-lookup-key (eval map-name) key)
                          nil)))
                     ((eq type 'minor-mode) 
                      (pm-get-minor-mode-binding key))
                     (t (error "Internal error: Unknown type."))))
         (where (symbol-name type)))
    (if (and func (not (eq func main-macro)))
        (progn

          (pm-insert "\n---------------------------------------------------------------------\n\n")
          ;; Insert the warning text.
          (if (eq warn-type 'conflict)
              (pm-insert "This definition will " 
                         (pm-outstand "conflict") " with ")
            (if (eq warn-type 'invisible)
                (pm-insert "This definition will " 
                           (pm-outstand "be invisible") " because of ")
              (if (eq warn-type 'shadows)
                  (pm-insert "This definition will " 
                             (pm-outstand "shadow") " for the ")
              (error (format "Unknown type %s" warn-type)))))

          ;; Now insert the description for the function.
          (if (symbolp func) ;; This is a function name.
              (if (fboundp func) ;; The function exists.

                  (if (member func (pm-get-available-macros))
              
                      ;; This is a macro with a description.
                      (pm-insert "the " where " macro loaded from\nthe file '" 
                                 (get func 'file) 
                                 "'. Its description is:\n"
                                 (pm-indent (get func 'documentation)))
                      
                    ;; This is another existing function.
                    (pm-insert "the " where " function '" 
                               (symbol-name func) 
                               "'\nwhich has the following description:\n"
                               (pm-indent (documentation func))))

                ;; The function doesn't seem to be defined.
                (pm-insert "the " where 
                           " function '" (symbol-name func) "' which,\n"
                           "however, doesn't seem to be defined. (At least not at the moment.)"))
            
            ;; Hmm it was not a symbol, then it might be a 
            ;; lambda expression.
            ;; Does there exists other possibilities?
            (pm-insert "the following " where " definition:\n" 
                       (format "%s" func)))


          t) ;; Return value.
      nil)))

;;}}}
;;{{{ Misc

(defun pm-outstand (text)
  "Adds bold properties to the string given as argument."
  (let ((new-text (concat text))) ;; We need to copy it to avoid adding
                                  ;; properties to the original widget.
    (add-text-properties 0 (length new-text) '(face bold) new-text)
    new-text))

(defun pm-indent (text)
  "Returns TEXT but with the exception that it has indentation properties."
  (let ((new-text (concat text))) ;; We need to copy it to avoid adding
                                  ;; properties to the original widget.
    (add-text-properties 0 (length new-text) '(left-margin 3) new-text)
    new-text))


(defun pm-indent-regarding-to-properties ()
  "Indent the warning buffer with respect to the text property `left-margin'.
This function runs through the buffer with warnings about overriding
keys etc. and executes fill-region on the locations where the text 
property left-margin is set."
  (let ((cont t)
        (end (point-min))
        start)
    (while cont
      (setq cont nil)
      (setq start (next-single-property-change end 'left-margin))
      (if start
          (if (not (get-text-property start 'left-margin))
              (setq cont t
                    end (+ end 1))
            (progn
              (setq end (next-single-property-change start 
                                                     'left-margin))
              (if end
                  (progn
                    (fill-region start end)
                    (setq end (+ end 1))
                    (setq cont t)))))))))
  


(defun pm-lookup-key (map key)
  "Returns the function definition bound to key if it exists.
If no definition is bound to the key, then nil is returned."
  (let ((func (lookup-key map key)))
    (if (and func (not (numberp func)))
        func
      nil)))



(defun pm-get-minor-mode-binding (key)
  "Returns the visible definitions of KEY in the active minor modes."
  (let ((maps (current-minor-mode-maps))
        (binding nil))
    (while maps
      (setq binding (lookup-key (car maps) key))
      (if (and binding (not (numberp binding)))
          (setq maps '())
        (setq binding nil))
      (setq maps (cdr maps)))
    binding))

(defun pm-insert (&rest args)
  "Inserts text into the buffer pm-warn-buffer."
  (save-excursion
    (set-buffer pm-warn-buffer)
    (eval `(insert ,@args))))

;;}}}

;;}}}
;;{{{ Actual key (un)binding

(defun pm-unset-key (macro)
  "Removes the binding for MACRO."
  (let ((key (get macro 'key))
        (mode (get macro 'mode)))
    (when key
      (if (eq mode 'global)
          (if (eq (lookup-key global-map key) macro)
              (global-unset-key key))
        
        ;; Mode specific binding.
        (let* ((map (intern (concat (symbol-name mode) "-map"))))
          (if (boundp map)
              (if (eq (lookup-key (eval map) key) macro)
                  (define-key (eval map) key nil))
            
            ;; No map exists, we must then unadvice the function and run
            ;; through all buffers.
            (if (fboundp mode)
                (pm-set-or-unset-mode-key macro mode key 'unset))))))))

(defun pm-set-key (macro)
  "Bind MACRO as described in the power-macro database."

  (let ((mode (get macro 'mode))
        (key (get macro 'key)))
    (if (eq mode 'global)
        (global-set-key key `(lambda () (interactive) (pm-run-macro-protected ',macro)))

      ;; Major-mode macro.
      (let ((map (intern (concat (symbol-name mode) "-map"))))
        (if (boundp map)
            (define-key (eval map) key `(lambda () (interactive) (pm-run-macro-protected ',macro)))

          ;; A mode-map didn't exists so now we need to make the 
          ;; binding much more manual.
          (pm-set-or-unset-mode-key macro mode key 'set))))))

(defun pm-set-or-unset-mode-key (macro mode key type)
  "(Un)set binding for MACRO in existing and future buffers with mode MODE."

  (let ((all-buffers (buffer-list))
        (adv-name (intern (concat (symbol-name macro) "-advice"))))

    ;; Run through all the buffers and bind the key for buffers 
    ;; with the given major mode.
    (dolist (buf all-buffers)
      (save-excursion 
        (set-buffer buf)
        (when (eq major-mode mode)
          (if (eq type 'set)
              (local-set-key key `(lambda () (interactive) (pm-run-macro-protected ',macro)))
            
            ;; Unset the key if it is bound to the macro.
            (if (and (current-local-map)
                     (eq (lookup-key (current-local-map) key) `(lambda () (interactive) (pm-run-macro-protected ',macro))))
                (local-unset-key key))))))
    
    (if (eq type 'set)
        (eval `(defadvice ,mode (after ,adv-name activate)
                 (local-set-key ,key (lambda () (interactive) (pm-run-macro-protected ',macro)))))
      (ad-remove-advice mode 'after adv-name))
    ))

;;}}}
;;{{{ Utility functions

(defun pm-create-buffer (&optional name)
  "Switches to the named buffer or to the power-macros buffer.
The buffer is created if it doesn't exist."
  (let* ((nm (if name name "*Power Macros Description*"))
        (old-buf (get-buffer nm)))
    (if old-buf
        (kill-buffer old-buf))
    (get-buffer-create nm)))


(defun pm-ass-remove (var list)
  "Removes VAR from assoc list LIST."
  (if (null list)
      '()
    (if (eq (caar list) var)
        (pm-ass-remove var (cdr list))
      (cons (car list) (pm-ass-remove var (cdr list))))))


;; Does select not exist in core lisp?
(defun pm-select (func list)
  "Returns a list with elements from LIST, which matches the predicate FUNC."
  (let ((res '()))
    (while list
      (if (funcall func (car list))
          (push (car list) res))
      (setq list (cdr list)))
    (nreverse res)))

(defun pm-available-modes ()
  "Returns the list of available modes."
  (let ((mode-list (mapcar 'symbol-name
                           (apropos-internal "-mode\\'" 'fboundp))))
    (mapcar 'list mode-list)))

(defun cadddar (l)
  (cadddr (car l)))

(defun pm-new-dummy-macro-name ()
  "Generates a new name for a macro."
  (let ((num 1)
        (name 'pm-macro-1))
    (while (fboundp name)
      (setq num (+ num 1))
      (setq name (intern (format "pm-macro-%s" num))))
    (symbol-name name)))


(defun pm-ask-for-macro-name (old-name)
  "Ask for a name for a new macro."
  (let ((ask-again t)
       (name (intern 
              (if old-name (symbol-name old-name) (pm-new-dummy-macro-name)))))
    (while ask-again
      (setq name
            (intern (read-from-minibuffer "A short unique name for the macro: "
                                          (symbol-name name))))
      (if (and (not (eq name old-name)) (fboundp name))
          (progn
            (message "Symbol exists!")
            (sit-for 1))
        (setq ask-again nil)))
    name)) ; Return value.

(defun pm-read-key-sequence (question)
  "Reads a key sequence."
  (let ((key (read-key-sequence question nil t)))
    (if (boundp 'xemacs-logo)
        (events-to-keys key)
      key)))

(defun pm-run-macro-protected (name)
  "Execute the named macro in a protected environment.
The changes the macro does to the clipboard and search word are 'local' to the macro.
In other words, after the macro has executed, the kill-ring are uneffected. 
Also the current search word (C-s C-s) is uneffected." 
  (let ((kill-ring-copy kill-ring)
        (kill-ring-yank-pointer-copy kill-ring-yank-pointer)
        (search-ring-copy search-ring)
        (search-ring-yank-pointer-copy search-ring-yank-pointer)
        (regexp-search-ring-copy regexp-search-ring)
        (regexp-search-ring-yank-pointer-copy regexp-search-ring-yank-pointer)
        (clipboard-copy (condition-case nil (get-clipboard)))
        )
    (progn)
    (execute-kbd-macro name)
    (if power-macros-run-macros-protected
        (progn
          (setq kill-ring kill-ring-copy
                kill-ring-yank-pointer kill-ring-yank-pointer-copy
                search-ring search-ring-copy
                search-ring-yank-pointer search-ring-yank-pointer-copy
                regexp-search-ring regexp-search-ring-copy
                regexp-search-ring-yank-pointer regexp-search-ring-yank-pointer-copy)
          (own-clipboard clipboard-copy)))))

(defun pm-run-last-kbd-macro ()
  "Runs last keyboard macro, and restores clipboard and search info at end of macro execution"
  (interactive)
  (pm-run-macro-protected last-kbd-macro))

; Make the compiler shut up under GNU-Emacs:
(eval-when-compile 
  (or (boundp 'xemacs-logo) 
      (boundp 'event-closest-point)
      (defun event-closest-point (dummy) 'dummy))
  (or (boundp 'xemacs-logo)
      (boundp 'events-to-keys)
      (defun events-to-keys(dummy) 'dummy))
)

;;}}}

(provide 'power-macros)
(message "Loading power-macros...done")


