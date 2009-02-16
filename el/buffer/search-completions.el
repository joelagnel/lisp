;;; Saved through ges-version 0.3.3dev at 2003-04-12 09:56
;;; ;;; From: lawrence mitchell <wence@gmx.li>
;;; ;;; Subject: search-completions.el --- isearching of the *Completions* buffer.
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Fri, 11 Apr 2003 17:43:06 +0100
;;; ;;; Organization: funfunfun
;;; ;;; Mail-Copies-To: nobody

;;; Long ago in the midsts of time (1994ish) there was a package
;;; which allowed searching the *Completions* buffer.  It broke with
;;; the introduction of Emacs 20, and doesn't seem to have been
;;; fixed since.

;;; Since it seemed vaguely useful to me, I managed to get it
;;; working on Emacs 21.  And so, I thought I'd post it, just in
;;; case anyone else used to use it and then stopped, or something.

;;; Only lightly tested with Emacs 21.  Bug reports for XEmacs/Emacs
;;; 20, etc... welcome.  (Yes, I know it really ought to get a
;;; namespace cleanup).

;;; ------ Cut here ------
;; search-completions
;; Description:  runs isearch in *Completions* buffer, and returns the
;;               completion point is on when the isearch terminates.
;;               Narrows completions interactively with regexp matches.
;; Author:    Radey Shouman              <rshouman@chpc.utexas.edu>
;; File:      search-completions.el
;; $modified: Fri Mar 25 15:09:03 1994 by rshouman $
;;
;; Modified 2003-04-10: lawrence mitchell <wence@gmx.li> to work with Emacs 21.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; LCD Archive Entry:
;; search-completions|Radey Shouman|rshouman@chpc.utexas.edu|
;; Use isearch to select a completion, narrow completions with regexps.|
;; 25-March-1994|1.04|~/misc/search-completions.el.Z|

;; INSTALLATION:
;; byte-compile it, then
;;
;;(autoload 'isearch-completions "search-completions"
;;   "search completions buffer" t)
;;(autoload 'narrow-completions "search-completions"
;;   "narrow completions buffer with a regexp" t)
;;(autoload 'completions-scroll-up "search-completions")
;;(autoload 'completions-scroll-down "search-completions")
;;(autoload 'completions-scroll-up-1 "search-completions")
;;(autoload 'completions-scroll-down-1 "search-completions")
;;(let ((map-list (list minibuffer-local-completion-map
;;        minibuffer-local-must-match-map))
;;      map)
;;  (while map-list
;;    (setq map (car map-list))
;;    (setq map-list (cdr map-list))
;;    (define-key map "\C-v" 'completions-scroll-up)
;;    (define-key map "\M-v" 'completions-scroll-down)
;;    (define-key map "\C-n" 'completions-scroll-up-1)
;;    (define-key map "\C-p" 'completions-scroll-down-1)
;;    (define-key map "\C-s" 'isearch-completions)
;;    (define-key map "\C-x\C-n" 'narrow-completions)
;;    (define-key map "\C-x\C-w" 'widen-completions)))
;;
;;      or
;;
;; (require 'search-completions)
;;
;; This will bind your keys, unless you bind the variable
;; search-completions-dont-bind-my-keys to something non-nil.
;;
;; If you want to change the isearch exit character just for
;; isearch-completions, set the variable isearch-completions-exit-char to
;; the character you want (I like 13, RET).
;;
;; If you want to exit the minibuffer as soon as you exit the search, set
;; the variable isearch-completions-exit to t.  This will have an effect
;; during filename completion only if the filename in the minibuffer is
;; not a directory.
;;
;; This function uses a recursive minibuffer to read the regexp.
;; If enable-recursive-minibuffers is nil, then
;;
;; (setq narrow-completions-enable t)
;;
;; will allow the use of a recursive minibuffer for this function only.
;; If anyone ever autoloads this in a site-init.el, I would recommend that
;; narrow-completions-enable be t, but that narrow-completions be disabled
;; by default.
;;
;; USE:
;; isearch-completions:
;; To search through the completions buffer using isearch, type C-s in the
;; minibuffer window, then use the normal isearch commands to find the
;; completion you want.  If you exit the search with ESC, the completion
;; near point will be put in the minibuffer, if it is the sole completion,
;; the minibuffer will be exited with that value read.  If you decide that
;; you don't want to grab a value from the completions buffer, exit the search
;; using a random control character, the minibuffer will then be selected
;; without changing its contents.
;;
;; narrow-completions:
;; To narrow the possible completions with a regular expression, type
;; C-xC-n in the minibuffer, then type in a regular expression.
;; This will cause only completions matching the regular expression to
;; be considered.  You can recursively narrow the search by typing
;; C-xC-n again, or widen the completion table again by typing C-xC-w.
;; Using a prefix arg means that only completions *not* matching the regexp
;; will be considered.
;;
;; When doing file name completion, subdirectories will be displayed as
;; completions whether they match the current regexp(s) or not.
;;
;; For example, to see all variables with both "buffer" and "window" in
;; their names:
;; C-h v C-x C-n buffer RET C-x C-n window RET TAB
;;
;; To visit a file with a ".c" or ".h" extension:
;; C-x C-f C-n \.[ch] RET SPACE
;;
;; This package also defines the functions read-directory-name and
;; read-file-name-narrowed, for reading file names restricted by a
;; regexp.  See the documentation of those functions for an explanation
;; of how to use them.
;;

;; BUGS:
;; If minibuffer-completion-table is a symbol, i.e. programmed completion,
;; isearch-completions and narrow-completions assume they're doing filename
;; completion.  Programmed completion for some other purpose would probably
;; not work very well with this package.

;; CHANGES:
;; 2003-04-10: lawrence mitchell <wence@gmx.li>
;;   Modified to work with Emacs 21.
;;
;; 17-3-94:
;;   Fixed up the test to see whether isearch was exited normally or
;;   escaped from to work with 19.  (still a disgusting kluge, though)
;; 3-8-93:
;;   Replaced forward-word in isearch-completions-grab with skip-chars-forward,
;;     buffer names with spaces and *'s were confusing it.
;; 2-8-93:
;;   Mods to make this work under lemacs, isearch-forward isn't quite
;;     the same as for gnumacs 19.
;; 31-7-93:
;;   Various emacs-19 compatibility changes -- zap-to-char is different,
;;     the completions buffer is called "*Completions*", isearch has it's
;;     own mode so search-exit-char isn't defined any more.  Added regexp
;;     option.
;; 21-5-93:
;;   Added scrolling functions, cleaned up key-definitions a bit.
;; 8-4-93:
;;   Fixed bug causing isearch-completions to fail for alists if the
;;   string found was not the sole completion by replacing try-completion
;;   with assoc.
;; 1-4-93:
;;   Fixed up key definitions to work with gmhist, suggestion of
;;   Bill Benedetto <benedett@gentire.com>.
;;   Changed function called in isearch-completions to try to create
;;   a completions window to minibuffer-complete from minibuffer-complete-word.
;; 27-3-93:
;;   Added narrow-completions.
;; 21-3-93:
;;   Made isearch-completions exit minibuffer if isearch-completions-option
;;   is non-nil, added isearch-completions-char.
;; 20-3-93:
;;   Added test to see if isearch was terminated "normally".
;;   Removed interactive call, on advice from Dan LaLiberte.
;; 11-3-93:
;;   Changed (erase-buffer) in minibuffer to (zap-to-char -1 ?/)
;;     if completing on file names, after suggestion from Antonio DeSimone.
;;   Added test for proper completion when minibuffer-completion-table is
;;     and alist, fixed up code to do the right thing even if completions
;;     have imbedded spaces, after suggestion from Shiono Junichi.


(defvar isearch-completions-exit-char 13
  "*Character to exit incremental search of the completions buffer.
isearch-completions will insert the line found in the completions
buffer only if this character was the one used to terminate the
search.  nil means use search-exit-char if version 18.
This variable is ignored in version 19.")

(defvar isearch-completions-exit nil
  "*If non-nil, exiting a completions search also exits the minibuffer.
This will have an effect during filename completion only if the
filename in the minibuffer is not a directory. ")

;; The number of times has narrow-completions has been called for this
;; completing-read.
(defvar narrow-completions-depth 0)

(defvar narrow-completions-enable nil
  "*Enable recursive minibuffers for the function narrow-completions.
Or just set `enable-recursive-minibuffers' non-nil. ")

;; Name of completions buffer used by isearch-completions.
(defconst completions-buffer-name "*Completions*")

;;  Completions that will be returned for a complete directory name.
(defconst narrow-completions-dir-completions '("../" "./"))

(defun isearch-completions (&optional regexp)
  "Do an isearch in the *Completions* buffer.
If the isearch is terminated by typing isearch-exit-char (normally
ESC), then whatever is near point is inserted in the minibuffer.  If
the isearch is terminated by typing a random control character, the
minibuffer contents are not changed. "
  (interactive "P")
  (let ((currwin (selected-window))
        (compwin (get-buffer-window completions-buffer-name))
        found)
    ;; If there isn't a completions buffer, make one.
    (or compwin
        (progn
          (if (symbolp minibuffer-completion-table)
              (minibuffer-complete-word)
              (minibuffer-complete))
          (setq compwin (get-buffer-window completions-buffer-name))))
    (if (null compwin)
        nil
        (let ((search-exit-char isearch-completions-exit-char)
              (search-exit-option (or search-exit-option
                                      isearch-completions-exit)))
          (unwind-protect
               (progn
                 (select-window compwin)
                 (and (bobp) (forward-line 2))
                 (isearch-mode t regexp nil t)
                 (setq found (isearch-completions-grab)))
            (select-window currwin))
          (if (or (eq last-input-char search-exit-char)
                  (and (boundp 'isearch-mode-map)
                       (eq 'isearch-exit
                           (lookup-key isearch-mode-map
                                       (vector last-input-char)))))
              ;; If it's programmed completion, assume filename completion.
              (if (symbolp minibuffer-completion-table)
                  (progn
                    (zap-to-char -1 ?/)
                    (insert ?/)
                    (insert found)
                    (if (and isearch-completions-exit
                             (not (string=
                                   (file-name-directory found) found)))
                        (exit-minibuffer)
                        (minibuffer-complete-word)))
                  (beginning-of-line)
                  (delete-region (point) (point-at-eol))
                  (insert found)
                  (if isearch-completions-exit
                      (exit-minibuffer))))))))

(defun isearch-completions-grab ()
  (save-excursion
    (let* ((opoint (point))
           (start (progn (skip-chars-backward "^ \n\t")
                         (point)))
           (end (progn (skip-chars-forward "^ \n\t")
                       (point)))
           (found (buffer-substring start end))
           (table minibuffer-completion-table)
           (pred minibuffer-completion-predicate))

      (if (not (listp table))
          found
          (if (assoc found table)
              found
              (setq start (progn (beginning-of-line) (point)))
              (setq found "")
              (while (not (assoc found table))
                (if (eobp) (error "end of buffer"))
                (skip-chars-forward "^ \t\n")
                (setq found (buffer-substring start (point)))
                (skip-chars-forward " \t"))
              (if (>= (point) opoint)
                  found
                  (skip-chars-forward " \t")
                  (setq start (point))
                  (setq found "")
                  (while (not (assoc found table))
                    (if (eobp) (error "end of buffer"))
                    (skip-chars-forward "^ \t\n")
                    (setq found (buffer-substring start (point)))
                    (skip-chars-forward " \t"))
                  found))))))

(defun completions-scroll-up (&optional arg)
  "If the completions window is displayed, scroll it upward ARG lines. "
  (interactive "P")
  (save-excursion
    (let ((currwin (selected-window))
          (compwin (get-buffer-window completions-buffer-name)))
      (if compwin
          (unwind-protect
               (progn
                 (select-window compwin)
                 (scroll-up (if arg (prefix-numeric-value arg))))
            (select-window currwin))))))


(defun completions-scroll-down (&optional arg)
  "If the completions window is displayed, scroll it downward ARG lines. "
  (interactive "P")
  (save-excursion
    (let ((currwin (selected-window))
          (compwin (get-buffer-window completions-buffer-name)))
      (if compwin
          (unwind-protect
               (progn
                 (select-window compwin)
                 (scroll-down (if arg (prefix-numeric-value arg))))
            (select-window currwin))))))


(defun completions-scroll-down-1 ()
  "If the completions window is displayed, scroll it downward 1 line. "
  (interactive)
  (completions-scroll-down 1))


(defun completions-scroll-up-1 ()
  "If the completions window is displayed, scroll it upward 1 line. "
  (interactive)
  (completions-scroll-up 1))


(defun narrow-completions (regexp &optional complement)
  "Restrict possible completions to those matching REGEXP.  With optional
COMPLEMENT non-nil or interactive prefix arg, restrict completions to those
not matching REGEXP.  If file name completion is being done, include
subdirectories also. This function should be called from the minibuffer. "
  (interactive
   (let ((enable-recursive-minibuffers (or enable-recursive-minibuffers
                                           narrow-completions-enable)))
     (list (read-string "regexp: ")
           (if current-prefix-arg t nil))))
  (let ((narrow-completions-depth (1+ narrow-completions-depth))
        (minibuffer-completion-table minibuffer-completion-table)
        (minibuffer-completion-predicate minibuffer-completion-predicate))
    ;; If it's programmed completion, assume filename completion.
    (if (symbolp minibuffer-completion-table)
        (progn
          (setq minibuffer-completion-predicate
                (cons regexp
                      (cons complement
                            (cons minibuffer-completion-table
                                  minibuffer-completion-predicate))))
          (setq minibuffer-completion-table 'narrow-completions-internal)
          (if (get-buffer-window completions-buffer-name)
              (minibuffer-complete-word))
          (recursive-edit))
        (if (or
             (listp minibuffer-completion-table)
             (arrayp minibuffer-completion-table))
            (let ((completions
                   (all-completions "" minibuffer-completion-table
                                    minibuffer-completion-predicate)))
              (setq minibuffer-completion-table (mapcar 'list completions))
              (setq minibuffer-completion-predicate
                    (if complement
                        (` (lambda (arg)
                             (not (string-match (, regexp) (car arg)))))
                        (` (lambda (arg)
                             (string-match (, regexp) (car arg))))))
              (if (get-buffer-window completions-buffer-name)
                  nil
                  (minibuffer-complete))
              (recursive-edit))
            (error "Cannot narrow completions for this table. ")))
    (setq unread-command-events (list last-input-char))))

(defun widen-completions ()
  "Undo the effect of the last narrow-completions."
  (interactive)
  (if (zerop narrow-completions-depth)
      (message "Completions not narrowed")
      (setq last-input-char
            (if (get-buffer-window completions-buffer-name)
                (if (symbolp minibuffer-completion-table)
                    32
                    ?\t)
                -1))
      (exit-recursive-edit)))

(defun narrow-completions-quit ()
  "Quit from this narrowed completing-read, and all those that called it. "
  (interactive)
  (if (zerop narrow-completions-depth)
      (abort-recursive-edit)
      (exit-recursive-edit)))

;; PREDICATE here has the form:
;; (regexp complement original-table . original-predicate)
;; where regexp is the narrowing regexp, and complement is t if we
;; want to exclude matches to this regexp.
;; original-table and original-predicate are the arguments that
;; were passed to the completion-table calling this one.
;; original-table should thus be either 'read-file-name-internal, or
;; 'narrow-completions-internal for multiple narrowings.
(defun narrow-completions-internal (string predicate action)
  "Completion table function for narrow-completions, called only
for narrowing programmed completion. "
  (let ((regexp (car (nthcdr 0 predicate)))
        (complement (car (nthcdr 1 predicate)))
        (table (car (nthcdr 2 predicate)))
        (predicate (nthcdr 3 predicate)))
    (if (eq action 'lambda)
        (and (try-completion string table predicate)
             (or
              (if regexp
                  (if complement
                      (not (string-match regexp string))
                      (string-match regexp string)))
              (string= string (file-name-directory string)))
             t)
        (let ((completions (all-completions string table predicate))
              (fn (if regexp
                      (if complement
                          (function
                           (lambda (arg)
                            (if (or (not (string-match regexp arg))
                                    (string= arg (file-name-directory arg)))
                                arg)))
                          (function
                           (lambda (arg)
                            (if (or (string-match regexp arg)
                                    (string= arg (file-name-directory arg)))
                                arg))))
                      (function (lambda (arg)
                        (if (string= arg (file-name-directory arg))
                            arg))))))
          (setq completions
                (delq nil (mapcar fn completions)))
          (if action
              completions
              (let* ((subdir (file-name-directory string))
                     (name (file-name-nondirectory string))
                     (completion
                      (try-completion name (mapcar 'list completions))))
                (if (or (eq completion t)
                        (and
                         (subset completions narrow-completions-dir-completions)
                         (subset narrow-completions-dir-completions completions)))
                    string
                    (concat (or subdir "") completion))))))))

;; Stolen from the tree-dired distribution.
(or (fboundp 'member)
    (defun member (x y)
      "Like memq, but uses `equal' for comparison.
This is a subr in Emacs 19."
      (while (and y (not (equal x (car y))))
        (setq y (cdr y)))
      y))

(defun subset (set1 set2)
  "Returns t if list SET1 is a subset of list SET2, nil otherwise.
Membership is tested with member. "
  (catch 'exit
    (mapcar (function (lambda (arg)
              (or (member arg set2)
                  (throw 'exit nil))))
            set1)
    t))

;; Might as well define these, they might be handy some day.

(defun read-file-name-narrowed (prompt dir default mustmatch
                                regexp &optional complement)
  "Read file name, prompting with PROMPT and completing in directory DIR.
Value is not expanded!  You must call expand-file-name yourself.
Default name to DEFAULT if user enters a null string.
Fourth arg MUSTMATCH non-nil means require existing file's name.
Non-nil and non-t means also require confirmation after completion.
DIR defaults to current buffer's directory default.  Offer as completions
only file namess matching REGEXP, or directories, with optional COMPLEMENT,
offer only file names not matching REGEXP. "
  (let ((predicate (append (list regexp complement)
                           (cons 'read-file-name-internal dir))))
    (completing-read prompt 'narrow-completions-internal
                     predicate mustmatch default)))

(defun read-directory-name (prompt dir default mustmatch)
  "Read directory name, prompting with PROMPT and completing in directory DIR.
Value is not expanded!  You must call expand-file-name yourself.
Default name to DEFAULT if user enters a null string.
Fourth arg MUSTMATCH non-nil means require existing file's name.
Non-nil and non-t means also require confirmation after completion.
DIR defaults to current buffer's directory default. "
  (read-file-name-narrowed prompt dir default mustmatch nil))

;; Add key definitions.
(if (and (boundp 'search-completions-dont-bind-my-keys)
         search-completions-dont-bind-my-keys)
    nil
    (let ((map-list (list minibuffer-local-completion-map
                          minibuffer-local-must-match-map))
          map)
      (if (boundp 'gmhist-completion-map)
          (setq map-list (append map-list
                                 '(gmhist-completion-map
                                   gmhist-must-match-map))))
      (while map-list
        (setq map (car map-list))
        (setq map-list (cdr map-list))
        (define-key map "\C-v" 'completions-scroll-up)
        (define-key map "\M-v" 'completions-scroll-down)
        (define-key map "\C-n" 'completions-scroll-up-1)
        (define-key map "\C-p" 'completions-scroll-down-1)
        (define-key map "\C-s" 'isearch-completions)
        (define-key map "\C-x\C-n" 'narrow-completions)
        (define-key map "\C-x\C-w" 'widen-completions)
        (define-key map "\C-g" 'narrow-completions-quit))))

(provide 'search-completions)

;;; ------ Cut here ------
;;; ;;; -- 
;;; ;;; lawrence mitchell <wence@gmx.li>

