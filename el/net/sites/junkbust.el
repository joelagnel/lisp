;;; Debugging info for self: Saved through ges-version 1.5dev
;;; ;;; From: "Neil W. Van Dyke" <neil@NOSPAMneilvandyke.org>
;;; ;;; Subject: junkbust.el 0.9 - tools for configuring the Internet Junkbuster Proxy
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: 15 Oct 2002 20:48:40 -0400
;;; ;;; Organization: none

;;; I'm still maintaining this package (at least until I move to Privoxy).


;;; junkbust.el -- tools for configuring the Internet Junkbuster Proxy

;; Copyright (C) 2000-2002 Neil W. Van Dyke

;; Author:   Neil W. Van Dyke <neil@neilvandyke.org>
;; Version:  0.9
;; X-URL:    http://www.neilvandyke.org/junkbust-emacs/
;; X-CVS:    $Id: junkbust.el,v 1.92 2002/10/16 00:45:47 nwv Exp $ GMT

;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.  This
;; is distributed in the hope that it will be useful, but without any warranty;
;; without even the implied warranty of merchantability or fitness for a
;; particular purpose.  See the GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License along with
;; Emacs; see the file `COPYING'.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.")

;;; Commentary:

;; Introduction:
;;   
;;   `junkbust.el' adds some features to Emacs for configuring the Internet
;;   Junkbuster Proxy(tm), aka Junkbuster, which is a GPL'd filtering HTTP
;;   proxy from Junkbusters Corp. (`http://www.junkbuster.com/ijb.html').
;;   Different people use Junkbuster for different reasons, including those of
;;   privacy, usability, aesthetics, and bandwidth.
;;
;;   The three main features offered in this version of `junkbust.el' are:
;;
;;   * Functions for adding new rules to the Junkbuster blocklist without
;;     having to manually edit the blocklist file.  The default URL is taken
;;     from the native selection service.  `junkbust.el' generates a pattern
;;     from a given URL, including special-case handling of URLs for
;;     Akamai-hosted objects.  The blocklist file is automatically checked out
;;     writable from VC if necessary.
;; 
;;     As of version 0.4, `junkbust.el' also generates more "generalized"
;;     versions of the blocklist pattern, which are accessible via the
;;     minibuffer history (usually invoked with M-p) when the user is prompted
;;     to edit the rule.  Currently, the generated generalizations are for
;;     numeric parts of the trailing part of the URL path (which the author
;;     often finds useful).  For example, if the original URL ends in
;;     `box1-27-3.jpg', the default rule would end in `box1-27-3\\.jpg', and
;;     the two alternative rules would end in `box1-27-[0-9]+\\.jpg' and
;;     `box[0-9]+-[0-9]+-[0-9]+\\.jpg'.
;;
;;   * Fontifying of the Junkbuster blocklist.  This is helpful in reading the
;;     path regular expression, and in distinguishing the domain name and path
;;     parts of the rule patterns (which have different syntax).
;;
;;   * Organizing of the blocklist file, with the `junkbust-blocklist-organize'
;;     function, sorting by positive/negative, domain name hierarchy (i.e., so
;;     that `doubleclick.net' rules appear near `www.doubleclick.net' ones),
;;     etc., and right-justifying the domain name patterns.  This organization
;;     is good for visualizing many opportunities to combine or generalize
;;     rules.
;;
;;   These features can be invoked from within the Emacs UI, or you can use the
;;   special `gnuserv' support to invoke `junkbust' features from external
;;   processes (e.g., window manager menus and key bindings, Web browser
;;   menus).
;;
;;   Note: "Internet Junkbuster Proxy" is a trademark of Junkbusters Corp.
;;         `junkbust.el' is some complementary Emacs Lisp code to help people
;;         use Junkbuster through Emacs.  `junkbust.el' is not part of
;;         Junkbuster, and is neither provided nor supported by Junkbusters
;;         Corp.

;; System Requirements:
;;
;;   The `junkbust.el' package was written using FSF GNU Emacs 20.7 on a
;;   GNU/Linux system, and should work with recent Emacs versions on Unix
;;   variants.  `junkbust.el' is not officially supported by the author under
;;   Emacs running on non-Unixish platforms (but it'll probably work on those
;;   platforms after properly setting the `junkbust-blocklist-file'
;;   customization variable).
;;
;;   `junkbust.el' was developed for use with Junkbuster 2.0.2, and was tested
;;   with Steven Waldherr's slightly-modified version, which was installed as
;;   an RPM (`http://www.waldherr.org/junkbuster/').  It should work with any
;;   recent Junkbuster version.
;;
;;   The remote feature was tested with `gnuserv' 2.1alpha
;;   (`http://www-uk.hpl.hp.com/people/ange/gnuserv/') by Andy Norman.

;; Installation:
;;
;;   1. Put this `junkbust.el' file somewhere in your Emacs Lisp load path.
;;
;;   2. Add the following to your `.emacs' file (or elsewhere):
;;
;;          (require 'junkbust)
;;
;;   3. Use the Customize feature of Emacs to set various options of the
;;      Data->Junkbust group.  You should especially check to make sure that
;;      `junkbust-blocklist-file' is set correctly for your Junkbuster 
;;      installation.
;;
;;   4. You may wish to bind global keys to some functions.  For example:
;;
;;          (global-set-key   [f6] 'junkbust-block-url-edit-rule)
;;          (global-set-key [S-f6] 'junkbust-block-url-edit-url)
;;          (global-set-key [C-f6] 'junkbust-blocklist-file-visit)
;;
;;   5. Make sure that you have write permission to the Junkbuster
;;      configuration files.  For example, if you installed Junkbuster as
;;      user `root' on a Unix box, and you normally run your Emacs as user
;;      `jlopez', you might do something like this:
;;
;;          chown -R jlopez /etc/junkbuster
;;          chmod 0755 /etc/junkbuster
;;          chmod 0644 /etc/junkbuster/*
;;
;;   6. Optionally, if you want to use `junkbust' features from remote
;;      processes via `gnuserv', you can use `gnudoit' to call the functions
;;      that begin with `junkbust-remote-' (which try to do the right thing
;;      about creating frame, focusing, etc.).  For example, if you're using
;;      the Fvwm2 window manager, you might wish to bind a key to invoke a menu
;;      such as:
;;
;;    AddToMenu JunkbusterMenu "Junkbuster" Title
;;    + "Block &Pattern"  Exec gnudoit '(junkbust-remote-block-url-edit-rule)'
;;    + "Block &Verbatim" Exec gnudoit '(junkbust-remote-block-url-edit-url)'
;;    + "&Edit Blocklist" Exec gnudoit '(junkbust-remote-blocklist-file-visit)'

;; How To Use It:
;;
;;   The most frequent use of `junkbust.el' might be to quickly add blocklist
;;   rules while you're browsing the Web, by copying the URL from the Web
;;   browser and invoking `junkbust-block-url-edit-url' or
;;   `junkbust-block-url-edit-rule'.  For example, if you see an image in Web
;;   page viewed in Netscape Navigator, you might right-click on the image,
;;   select "Copy Image Location" from the pop-up menu, select "Block URL (edit
;;   Rule)" from the Tools->Junkbuster menu, and press Return.

;; Author's To-Do List:
;;
;;   * In `junkbust-block-url-prompt', strip leading whitespace, then limit it
;;     to one line.
;;
;;   * Redo `junkbust-blocklist-font-lock-keywords', since they're all wrong.
;;
;;   * Do metasequence fontifying in the domain name part of the blocklist
;;     rules.
;;
;;   * Add more substantial function doc for `junkbust-blocklist-mode'.
;;
;;   * Add a function to organize the blocklist file.
;;
;;   * Support other Junkbuster config files.
;;
;;   * Add a mode menu to `blocklist-mode'.
;;
;;   * Add documentation about Netscape selection crash bug.
;;
;;   * Improve code in `junkbust-remote-block-url-backend'.
;;
;;   * Maybe warn on dangerous blocklist patterns such as `ad*.*.*'.
;;
;;   * Add remote feature tailored to minibuffer frames.
;;
;;   * Add remote feature that will let us get the URL from an environment
;;     variable, which would be useful with Larswm.
;;
;;   * Fontify "~" in blocklist when there's no domain part.
;;
;;   * Make programmatic blocklist rule insertion indent automatically.
;;
;;   * Add indent function for interactive blocklist editing.
;;
;;   * Make blocklist organizing sort IP address patterns numerically.
;;
;;   * Remove duplicate blocklist rules when organizing.

;;; Change Log:

;; [Version 0.9, 15-Oct-2002]
;; * Changed `plain' face to `default'.
;; * Updated email address.
;;
;; [Version 0.8, 19-Jul-2001]
;; * Minor extension to Akamai URL pattern.
;;
;; [Version 0.7, 09-Feb-2001]
;; * Added a mode keymap and menu for `junkbust-blocklist-mode'.
;; * Made `junkbust-blocklist-patterns-for-url' work for `https' URLs.
;; * Fixed free variables.
;;
;; [Version 0.6, 25-Jan-2001]
;; * `junkbust-blocklist-organize' now keeps the point on the rule it was on 
;;   before organizing.
;; * `junkbust-blocklist-organize' now removes duplicate rules.
;; * Added blocklist rule matching of `ms.akamai.net' and `g.a.yimg.com' Akamai
;;   URLs.
;; * Fixed bug in `junkbust-blocklist-generalize-path' that was preventing part
;;   of Akamai URLs from being properly escaped.
;;
;; [Version 0.5, 09-Dec-2000]
;; * Added `junkbust-blocklist-organize'.
;; * Made `junkbust-block-url-prompt' not use default history, even if there
;;   are no generalized patterns available.
;;
;; [Version 0.4, 04-Dec-2000]
;; * Added blocklist rule "generalization" feature.
;; * Added `junkbust-use-x-selection-p' feature.
;; * Made `junkbust-remote-blocklist-file-visit' raise the frame.
;; * Match more Akamai URLs.
;; * Fixed typo in `junkbust-block-url-prompt'.
;;
;; [Version 0.3, 25-Nov-2000]
;; * Fixed the `about:' handling. Duh.
;;
;; [Version 0.2, 25-Nov-2000]
;; * Support for `gnuserv'.
;; * Improved Akamai handling in `junkbust-blocklist-pattern-for-url'.
;; * Strips leading `about:' in URLs.
;;
;; [Version 0.1, 07-Nov-2000] Initial release.

;;; Code:

(defconst junkbust-version "0.9")

(require 'custom)
(require 'font-lock)

;; Customization:

(defgroup junkbust nil
  "Tools for configuring the Internet Junkbuster Proxy."
  :group  'data
  :prefix "junkbust-")

(defcustom junkbust-blocklist-file "/etc/junkbuster/blocklist"
  "Absolute filename of the blocklist file."
  :group 'junkbust
  :type  'file)

(defcustom junkbust-organize-blocklist-p t
  "Is the blocklist a list of positive rules followed by a list of negatives?"
  :group 'junkbust
  :type  'boolean)

(defcustom junkbust-blocklist-left-width 33
  "Width of column in which domain part of blocklist rule is right-justified."
  :group 'junkbust
  :type  'integer)

(defcustom junkbust-use-x-selection-p t
  "Should the X primary selection be used instead of the kill ring?"
  :group 'junkbust
  :type  'boolean)

(defcustom junkbust-remote-block-url-width 100
  "Width of the remote URL blocking prompt window."
  :group 'junkbust
  :type  'integer)

(defcustom junkbust-use-font-lock-p t
  "Should the blocklist file be fontified?"
  :group 'junkbust
  :type  'boolean)

(defcustom junkbust-blocklist-mode-hook nil
  "Hook variable for `blocklist-mode'."
  :group 'junkbust
  :type  'hook)

(defface junkbust-tilde-face
  '((((class color)) (:foreground "Magenta" :bold t))
    (t               (:underline t)))
  "Face used for the negation tilde."
  :group 'junkbust)

(defface junkbust-domain-name-face
  '((((class color)) (:foreground "Blue"))
    (t               ()))
  "Face used for domain names."
  :group 'junkbust)

(defface junkbust-first-slash-face
  '((t (:bold t)))
  "Face used for the first slash character in blocklist rules."
  :group 'junkbust)

(defface junkbust-metasequence-face
  '((((class color)) (:foreground "Red" :bold t))
    (t               (:underline t)))
  "Face used for metasequence characters."
  :group 'junkbust)

(defface junkbust-comment-face
  '((((class color)) (:foreground "DarkGreen"))
    (t               (:italic t)))
  "Face used for comments."
  :group 'junkbust)

;; Option Variables:

(defvar junkbust-blocklist-font-lock-keywords
  `(
    ;; Domain name parts and optional first-slash.
    ("^[ \t]*\\(~\\)?[ \t]*\\([^ \t\n/]+\\)\\(/\\)?"
     (1 'junkbust-tilde-face       keep t)
     (2 'junkbust-domain-name-face keep t)
     (3 'junkbust-first-slash-face keep t))
    
    ;; First-slash without domain name part.
    ("^[ \t]*\\(~\\)?[ \t]*\\(/\\)"
     (1 'junkbust-tilde-face       keep t)
     (2 'junkbust-first-slash-face keep t))

    ;; Escaped square bracket in the URL path part.
    ("\\(\\\\\\)\\(\\[\\]\\)"
     (1 'junkbust-metasequence-face keep t)
     (2 'default                    keep t))
    
    ;; Character classes in the URL path part.
    ("\\(\\[^?\\]?\\)\\([^]]*\\)\\(\\]\\)?"
     (1 'junkbust-metasequence-face keep t)
     (2 'default                    keep t)
     (3 'junkbust-metasequence-face keep t))
    
    ;; Metacharacter and escaped-metacharacter sequences in the URL path part.
    (,(concat 
       "\\("                            ; <1
       "\\\\"
       "\\)"                            ; >1
       "\\("                            ; <2
       "[][*+?.\\()|]"
       "\\)"                            ; >2
       "\\|"                            ; |0
       "\\("                            ; <3
       "[*+?.\\()|]"
       "\\)"                            ; >3
       )
     (1 'junkbust-metasequence-face keep t)
     (2 'default                    keep t)
     (3 'junkbust-metasequence-face keep t))
    ))

(defvar junkbust-blocklist-syntax-table
  (let ((n (make-syntax-table)))
    (modify-syntax-entry ?\" "w   " n)
    (modify-syntax-entry ?\# "<   " n)
    (modify-syntax-entry ?\n ">   " n)
    n))

;; Other Global Variables:

(defvar junkbust-block-url-prompt-history nil)

(defvar junkbust-blocklist-duplicates nil
  "Global variable due to imperfect structuring.")

(defvar junkbust-blocklist-mode-map nil)

;; Functions:

(defun junkbust-block-url-backend (url visit-p edit-url-p edit-rule-p)
  (unless url (setq url (junkbust-selection-without-text-properties)))
  (when (or (not edit-url-p)
            (setq url (junkbust-block-url-prompt "verbatim" url)))
    (let* ((patterns (junkbust-blocklist-patterns-for-url url))
           (rule     (car patterns))
           (history  (cdr patterns)))
      (when (or (not edit-rule-p)
                (setq rule 
                      (junkbust-block-url-prompt "pattern" rule nil history)))
        (junkbust-blocklist-add-rule rule visit-p)))))

(defun junkbust-block-url-edit-rule (&optional arg)
  "Add a new rule to the Junkbuster blocklist.  Prompts for the rule, using a
rule pattern generated from the the the current kill (or text selection) as the
default, and adds it to the blocklist file.  If a blank line is entered at the
prompt, then instead of adding a new rule, the blocklist file is simply visited
for manual viewing and editing."
  (interactive "P")
  (junkbust-block-url-backend nil arg nil t))

(defun junkbust-block-url-edit-url (&optional arg)
  "Add a new rule to the Junkbuster blocklist.  Prompts for the URL, using the
the current kill (or text selection) as the default.  The URL is converted to
an appropriate pattern rule and added to the blocklist file.  If a blank line
is entered at the prompt, then instead of adding a new rule, the blocklist file
is simply visited for manual viewing and editing."
  (interactive "P")
  (junkbust-block-url-backend nil arg t nil))

(defun junkbust-block-url-prompt (what default &optional allow-blocklist-visit
                                       history)
  (setq junkbust-block-url-prompt-history history)
  (let ((value (read-string (concat "Block URL " what ": ")
                            (when default (cons default 0))
                            'junkbust-block-url-prompt-history)))
    (junkbust-string-clear-text-properties value)
    (if (junkbust-string-blank-p value)
        (progn (when allow-blocklist-visit (junkbust-blocklist-file-visit))
               nil)
      value)))

(defun junkbust-blocklist-add-rule (rule visit-p)
  (let (saved-buf saved-pnt saved-mrk)

    ;; If we don't want to show the user the buffer, save this excursion.
    (unless visit-p
      (setq saved-buf (current-buffer))
      (setq saved-pnt (point-marker))
      (setq saved-mrk (copy-marker (mark-marker))))
    
    ;; Visit the blocklist file.
    (junkbust-blocklist-file-visit t (not visit-p))
    
    ;; Move the point to where the rule should be inserted.
    (if junkbust-organize-blocklist-p
        ;; The file is supposed to be organized, so put the point at the end of
        ;; the respective positive or negative list.  Note that the respective
        ;; list might not exist.
        (save-match-data
          (let ((positive-pat "^[ \t]*[^ \t~#\r\n]")
                (negative-pat "^[ \t]*~"))
            (goto-char (point-max))
            (if (string-match "\\`[ \t]*~" rule)
                ;; It's a negative rule, so put it after the last negative rule
                ;; or at the end of the file.
                (when (re-search-backward negative-pat nil t)
                  (end-of-line))
              ;; It's a positive rule, so put it after the last positive rule,
              ;; before the first negative rule, or at the end of the file.
              (if (re-search-backward positive-pat nil t)
                  (end-of-line)
                (goto-char (point-min))
                (if (re-search-forward negative-pat nil t)
                    (beginning-of-line)
                  (goto-char (point-max)))))))
      ;; We aren't supposed to organize the file, so just put the point at the
      ;; end of the file.
      (goto-char (point-max)))
    
    ;; Insert the rule before or after the current line, and leave the point at
    ;; the beginning of the line we just inserted.
    (if (bolp)
        (progn (insert rule)
               (save-excursion (newline)))
      (end-of-line)
      (newline)
      (insert rule)
      (when (eobp) (save-excursion (newline))))
    (beginning-of-line)
    
    ;; Save the buffer.
    (save-buffer)
    
    ;; If we don't want to show the user the buffer, restore from this
    ;; excursion.
    (unless visit-p
      (set-buffer saved-buf)
      (goto-char saved-pnt)
      (set-marker (mark-marker) saved-mrk))

    ;; State what rule we added.
    (message "Added Junkbuster blocklist rule: %s" rule)))

(defun junkbust-blocklist-file-visit (&optional ensure-writable-p
                                                dont-switch-p)
  (interactive)
  (junkbust-file-visit junkbust-blocklist-file
                       ensure-writable-p
                       'junkbust-blocklist-mode
                       dont-switch-p))

(defun junkbust-blocklist-generalize-path (str)
  (save-match-data
    (if (string-match "\\`\\(.*/\\)\\([^/]+\\)\\'" str)
        (let* ((left          (junkbust-blocklist-path-escape
                               (match-string 1 str)))
               (r             (match-string 2 str))
               (right-normal  (junkbust-blocklist-path-escape            r))
               (right-lastnum (junkbust-blocklist-generalize-str-lastnum r))
               (right-allnum  (junkbust-blocklist-generalize-str-allnum  r)))
          (mapcar (function (lambda (new-right)
                              (concat left new-right)))
                  ;; TODO: Write a simple convenience function for here.
                  (delq nil (list right-normal
                                  (unless (member right-lastnum
                                                  (list right-normal))
                                    right-lastnum)
                                  (unless (member right-allnum
                                                  (list right-normal
                                                        right-lastnum))
                                    right-allnum)))))
      (list (junkbust-blocklist-path-escape str)))))
                      
(defun junkbust-blocklist-generalize-rule (rule-prefix url-path
                                                       &optional rule-suffix)
  (if (and url-path (not (string= url-path "")))
      (mapcar (function (lambda (rule-middle)
                          (concat rule-prefix
                                  rule-middle
                                  (or rule-suffix ""))))
              (junkbust-blocklist-generalize-path url-path))))

(defun junkbust-blocklist-generalize-str-allnum (str)
  (save-match-data
    (let ((str-length (length str))
          (start      nil)
          (substrings '())
          (pattern    (concat "\\(\\(%[0-9][0-9]\\|\\\\.\\|[^%0-9]\\)+\\)?"
                              "\\([0-9]+\\)?")))
      (while (and (or (not start) (< start str-length))
                  (string-match pattern str start))
        (setq start (match-end 0))
        (let ((non (match-string 1 str))
              (num (match-string 3 str)))
          (when non
            (setq substrings
                  (nconc substrings
                         (list (junkbust-blocklist-path-escape non)))))
          (when num
            (setq substrings (nconc substrings (list "[0-9]+"))))))
      (or (apply 'concat substrings) ""))))

(defun junkbust-blocklist-generalize-str-lastnum (str)
  (save-match-data
    (when (string-match "[0-9]+\\([^0-9]*\\)\\'" str)
      (let ((left  (substring str 0 (match-beginning 0)))
            (right (match-string 1 str)))
        (concat (junkbust-blocklist-path-escape left)
                "[0-9]+"
                (junkbust-blocklist-path-escape right))))))

(defun junkbust-blocklist-mode ()
  "Major mode for Junkbuster blocklist files.

This is part of the `junkbust' package by Neil W. Van Dyke
<neil@neilvandyke.org>.

Key bindings:

\\{junkbust-blocklist-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (setq major-mode 'junkbust-blocklist-mode)
  (setq mode-name "Junkbuster-Blocklist")
  (use-local-map junkbust-blocklist-mode-map)
  (set-syntax-table junkbust-blocklist-syntax-table)
  (setq font-lock-defaults '(junkbust-blocklist-font-lock-keywords nil))
  (when junkbust-use-font-lock-p
    (turn-on-font-lock))
  (run-hooks 'junkbust-blocklist-mode-hook))


(defun junkbust-blocklist-organize ()
  (interactive)
  (save-match-data
    (let ((negative-rules '())
          (positive-rules '())
          (initial-point  (point))
          (initial-rule   nil)
          (start-point    nil)
          (fontified      font-lock-fontified))
      
      (message "Organizing...")
      
      ;; Un-font-lock if necessary.
      (when fontified
        (message "Organizing (un-fontifying)...")
        (font-lock-mode))
      
      ;; Scan and remove all the rules.
      (message "Organizing (scanning)...")
      (let ((pattern (concat "[ \t]*"
                             "\\(~" "\\)?"         ; 1
                             "\\([^ :/\n]+" "\\)?" ; 2
                             "\\(:[0-9]+" "\\)?"   ; 3
                             "\\(/[^ \t\n]+\\)?"   ; 4
                             "[ \t]*"
                             "\\(#[^\n]*\\)?"      ; 5
                             "\n?")))
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (if (and (not (looking-at "[ \t]*\\(#[^\n]*\\)?$"))
                   (and (looking-at pattern)))
              (let ((tilde       (match-string 1))
                    (domain      (match-string 2))
                    (port        (match-string 3))
                    (path        (match-string 4))
                    (match-begin (match-beginning 0))
                    (match-end   (match-end       0)))
                (when domain
                  (setq domain (downcase domain)))
                (when port
                  (setq port (string-to-int (substring port 1))))
                (when (and path (string-match "\\`/[ \t]*\\'" path))
                  (setq path nil))
                (if (or domain port path)
                    (let ((rule (vector (junkbust-domain-pattern-reverse
                                         domain)
                                        port
                                        path)))
                      (unless start-point
                        (setq start-point match-begin))
                      (when (and (>= initial-point match-begin)
                                 (<  initial-point match-end))
                        (setq initial-rule rule))
                      (if tilde
                          (setq negative-rules (cons rule negative-rules))
                        (setq positive-rules (cons rule positive-rules)))
                      (delete-region match-begin match-end)
                      (setq initial-point (- initial-point
                                             (- match-end match-begin))))
                  (forward-line 1)))
            (forward-line 1))))

      ;; Sort the rules.
      (message "Organizing (sorting)...")
      (setq positive-rules
            (sort positive-rules 'junkbust-blocklist-organize-sort-predicate))
      (setq negative-rules
            (sort negative-rules 'junkbust-blocklist-organize-sort-predicate))
      
      ;; Insert the sorted rules back into the buffer.
      (message "Organizing (inserting)...")
      (let (initial-point-1 initial-point-2)
        (setq junkbust-blocklist-duplicates 0)
        (goto-char start-point)
        (junkbust-open-line-with-one-preceding-blank-line)
        (setq initial-point-1
              (junkbust-blocklist-organize-rules-insert positive-rules
                                                        initial-rule
                                                        nil))
        (junkbust-open-line-with-one-preceding-blank-line)
        (setq initial-point-2
              (junkbust-blocklist-organize-rules-insert negative-rules
                                                        initial-rule
                                                        t))
        (junkbust-open-line)
        (setq initial-point (or initial-point-1 initial-point-2)))
      
      ;; Re-font-lock.
      (when fontified
        (message "Organizing (re-fontifying)...")
        (font-lock-mode))

      ;; Remove any excess blank lines at end of file.
      (goto-char (point-max))
      (junkbust-open-line)

      ;; Position buffer in window.
      (goto-char (or initial-point (point-min)))

      ;; Done.
      (message "Organizing (%d positive rules, %d negative rules%s)...done"
               (length positive-rules)
               (length negative-rules)
               (if (= junkbust-blocklist-duplicates 0)
                   ""
                 (format ", %d duplicates"
                         junkbust-blocklist-duplicates))))))

(defun junkbust-blocklist-organize-rules-insert (rules selected-rule
                                                       negative-p)
  (let ((indent-tabs-mode nil)
        (last-rule-str    nil)
        (selected-point   nil)
        rule-str)
    (mapcar (function (lambda (rule)
                        (let ((domain (junkbust-domain-pattern-reverse
                                       (aref rule 0)))
                              (port   (aref rule 1))
                              (path   (aref rule 2)))
                          (when port
                            (setq port (concat ":" (number-to-string port))))
                          (setq rule-str (concat (if negative-p "~" "")
                                                 (or domain "")
                                                 (or port "")
                                                 (or path "")))
                          (if (string= rule-str last-rule-str)
                              (progn
                                (setq junkbust-blocklist-duplicates
                                      (1+ junkbust-blocklist-duplicates))
                                (when (eq rule selected-rule)
                                  (setq selected-point (save-excursion
                                                         (forward-line -1)
                                                         (beginning-of-line)
                                                         (point)))))
                            (setq last-rule-str rule-str)
                            (when (eq rule selected-rule)
                              (setq selected-point (point)))
                            (when (or domain port)
                              (indent-to (max (- junkbust-blocklist-left-width
                                                 (+ (length domain)
                                                    (if negative-p 1 0)))
                                              0)))
                            (insert rule-str "\n")))))
            rules)
    selected-point))
  
(defun junkbust-blocklist-organize-sort-predicate (a b)
  (let (result)
    (setq result (junkbust-compare-strings (aref a 0) (aref b 0)))
    (when (= result 0)
      (setq result (junkbust-compare-numbers (aref a 1) (aref b 1)))
      (when (= result 0)
        (setq result (junkbust-compare-strings (aref a 2) (aref b 2)))))
    (< result 0)))

(defun junkbust-blocklist-path-escape (str)
  (save-match-data
    (let* ((str-length (length str))
           (start      nil)
           (substrings '())
           (metachars  "].?*(|)\\")
           (pattern    (concat "\\([^"
                               metachars
                               "]+\\)?"
                               "\\(["
                               metachars
                               "]\\)?")))
      (while (and (or (not start) (< start str-length))
                  (string-match pattern str start))
        (setq start (match-end 0))
        (when (match-beginning 1)
          (setq substrings (nconc substrings (list (match-string 1 str)))))
        (when (match-beginning 2)
          (setq substrings (nconc substrings
                                  (list "\\" (match-string 2 str))))))
      (or (apply 'concat substrings) ""))))

(defun junkbust-blocklist-patterns-for-url (url)
  (when (and url (stringp url))
    (save-match-data
      (setq url (copy-sequence url))
      (junkbust-string-clear-text-properties url)
      ;; Note: We intentionally don't bother with degenerate URLs that have
      ;;       unescaped whitespace in the middle.
      (if (let ((case-fold-search t))
            (string-match (concat "\\`"
                                  "[ \t\n]*"
                                  "\\(about:\\)?"
                                  "\\(https?://?\\)?"
                                  "\\([-a-z0-9\\.]*\\)"
                                  "\\(:[0-9]*\\)?"
                                  "\\([^ \t\n]*\\)")
                          url))
          (let ((domain (downcase (match-string 3 url)))
                (port   (or (match-string 4 url) ""))
                (path   (match-string 5 url)))
            (cond
             
             ;; Match Akamai-hosted URL.
             ((string-match (concat "\\`"
                                    "\\([a-z0-9]*\\.\\)?"
                                    "\\("
                                    "[eg]\\.akamai\\(tech\\)?\\.net"
                                    "\\|"
                                    "ms\\.akamai\\.net"
                                    "\\|"
                                    "g\\.ak\\.nbci\\.com"
                                    "\\|"
                                    "g\\.a\\.yimg\\.com"
                                    "\\)"
                                    "\\'")
                            domain)
              (setq domain (match-string 2 domain))
              (let (encoded-left unencoded-right)
                (if (string-match
                     "/[0-9a-f]+/[0-9]+/[0-9]+/[-.0-9a-z]+\\(/.*\\)"
                     path)
                    (setq encoded-left    "/[^/]+/[^/]+/[^/]+/[^/]+"
                          unencoded-right (match-string 1 path))
                  (setq encoded-left    ""
                        unencoded-right path))
                (junkbust-blocklist-generalize-rule
                 (concat domain port encoded-left) unencoded-right "")))
             
             ;; Default URL handling.
             (t (junkbust-blocklist-generalize-rule (concat domain port)
                                                    path
                                                    ""))))))))

(defun junkbust-compare-strings (a b)
  (if a
      (if b
          (let ((result (compare-strings a 0 nil b 0 nil)))
            (if (eq result t)
                0
              (if (< result 0) -1 1)))
        1)
    (if b -1 0)))

(defun junkbust-compare-numbers (a b)
  (if a
      (if b
          (if (= a b)
              0
            (if (< a b) -1 1))
        1)
    (if b -1 0)))

(defun junkbust-domain-pattern-reverse (domain)
  (when domain
    (save-match-data
      (if (string-match "\\`[0-9.]+\\'" domain)
          domain
        (mapconcat 'identity
                   (reverse (junkbust-split-on-char domain ?.))
                   ".")))))

(defun junkbust-feature-available-p (feature)
  (condition-case nil
      (progn (require feature) t)
    (error nil)))

(defun junkbust-file-visit (filename ensure-writable-p desired-major-mode
                                     &optional dont-switch-p)
  ;; Visit file `filename' with major mode `desired-major-mode'.  If the file
  ;; buffer is read-only, and `ensure-writable-p' is non-nil, then either make
  ;; the buffer writable with `vc-toggle-read-only' or generate an error.  To
  ;; avoid having a slow major mode startup (slow, due e.g. to complex
  ;; font-lock on large buffers) *twice* when we need to call
  ;; `vc-toggle-read-only' after loading, we initially load the file in
  ;; `fundamental-mode' and later ensure that the major mode is set to
  ;; `desired-major-mode'.  `dont-switch-p' is a kludge because we shouldn't
  ;; have done it like this, and we don't want to rewrite several functions
  ;; just to make `gnuserv' not barf when `junkbust-remote-block-url-backend'
  ;; tries to switch a buffer in a minibuffer-only frame.
  (let (buf)
    (setq buf (let ((auto-mode-alist     nil)
                    (default-major-mode 'fundamental-mode))
                (find-file-noselect filename)))
    (if dont-switch-p
        (set-buffer buf)
      (switch-to-buffer buf))
    (when (and ensure-writable-p buffer-read-only)
      (if (junkbust-feature-available-p 'vc)
          (vc-toggle-read-only)
        (barf-if-buffer-read-only)))
    (unless (eq major-mode desired-major-mode)
      (funcall desired-major-mode))))

(defun junkbust-open-line ()
  (insert "\n")
  (save-excursion (insert "\n\n"))
  (save-match-data (delete-blank-lines)))

(defun junkbust-open-line-with-one-preceding-blank-line ()
  (junkbust-open-line)
  (unless (bobp) (insert "\n")))

(defun junkbust-remote-block-url-backend (func)
  (let (frame saved-window-configuration saved-mouse-pixel-position)
    (setq saved-window-configuration (current-window-configuration))
    (setq saved-mouse-pixel-position (mouse-pixel-position))
    (unwind-protect
        (save-excursion
          (progn
            (setq frame
                  (make-frame
                   `((height         . 1)
                     (left           . 0)
                     (menu-bar-lines . 0)
                     (minibuffer     . only)
                     (title          . "Junkbuster Block URL")
                     (top            . 0)
                     (unsplittable   . t)
                     (width          . ,junkbust-remote-block-url-width))))
            (sleep-for 0 10)
            (select-frame frame)
            (set-mouse-pixel-position frame 0 0)
            (funcall func nil)))
      ;; unwind-protect
      (when (frame-live-p frame)
        (delete-frame frame))
      (let ((old-frame (car      saved-mouse-pixel-position))
            (old-x     (car (cdr saved-mouse-pixel-position)))
            (old-y     (cdr (cdr saved-mouse-pixel-position))))
        (when old-frame
          (select-frame old-frame)
          (condition-case nil
              (set-window-configuration saved-window-configuration)
            (error nil))
          (when (and (integerp old-x) (integerp old-y))
            (funcall 'set-mouse-pixel-position old-frame old-x old-y)))))))
            
(defun junkbust-remote-block-url-edit-rule ()
  (junkbust-remote-block-url-backend 'junkbust-block-url-edit-rule))

(defun junkbust-remote-block-url-edit-url ()
  (junkbust-remote-block-url-backend 'junkbust-block-url-edit-url))

(defun junkbust-remote-blocklist-file-visit ()
  (let (buffer window frame)
    (if (and (setq buffer (get-file-buffer junkbust-blocklist-file))
             (setq window (get-buffer-window buffer t))
             (setq frame  (window-frame window)))
        (make-frame-visible frame)
      (setq frame (make-frame)))
    (sleep-for 0 10)
    (select-frame frame)
    (raise-frame frame)
    (set-mouse-pixel-position frame
                              (/ (frame-pixel-width  frame) 2)
                              (/ (frame-pixel-height frame) 2))
    (junkbust-blocklist-file-visit t)))

(defun junkbust-selection-without-text-properties ()
  (let ((str (copy-sequence (if (and (eq window-system 'x)
                                     junkbust-use-x-selection-p)
                                (condition-case nil
                                    (x-get-selection)
                                  (error ""))
                              (current-kill 0 nil)))))
    (junkbust-string-clear-text-properties str)
    str))

(defun junkbust-split-on-char (string char)
  ;; Note: Taken from spamprod.el by me.
  ;; Note that this could be more elegant.
  (if (not (and string char))
      nil
    (if (stringp char)
        (if (= (length char) 1)
            (setq char (aref 0 char))
          (error "junkbust-split-on-char: char is a string of length /= 1.")))
    (let* ((start 0)
           (length (length string))
           (i      0)
           (result '()))
      (while (<= i length)
        (if (or (= i length)
                (= (aref string i) char))
            (progn
              (setq result (nconc result (list (substring string start i))))
              (setq start (1+ i))))
        (setq i (1+ i)))
      result)))

(defun junkbust-string-blank-p (str)
  (when (or (null str)
            (save-match-data (string-match "\\`[ \f\t\n\r\v]*\\'" str)))
    t))

(defun junkbust-string-clear-text-properties (str)
  (set-text-properties 0 (length str) nil str)
  str)

;; Mode Keymaps:

(setq junkbust-blocklist-mode-map
      (let ((km (make-sparse-keymap)))
        (define-key km "\C-c\C-o" 'junkbust-blocklist-organize)
        (define-key km "\C-c\C-p" 'junkbust-block-url-edit-pattern)
        (define-key km "\C-c\C-v" 'junkbust-block-url-edit-verbatim)
        ;; Mode Menu
        (define-key km [menu-bar] (make-sparse-keymap))
        (define-key km [menu-bar junkbust-blocklist]
          (cons "Junkbuster-Blocklist"
                (let ((mm (make-sparse-keymap "Junkbuster-Blocklist")))
                  (define-key mm [junkbust-block-url-edit-rule]
                    '("Block Pattern"  . junkbust-block-url-edit-rule))
                  (define-key mm [junkbust-block-url-edit-url]
                    '("Block Verbatim" . junkbust-block-url-edit-url))
                  (define-key mm [separator-1]
                    '("--"))
                  (define-key mm [junkbust-blocklist-file-visit]
                    '("Organize" . junkbust-blocklist-organize))
                  mm)))
        km))

;; Tools Menu:

(when (junkbust-feature-available-p 'menu-bar)
  (define-key menu-bar-tools-menu [junkbust]
    (cons "Junkbuster"
          (let ((m (make-sparse-keymap "Junkbuster")))
            (define-key m [junkbust-block-url-edit-rule]
              '("Block Pattern"  . junkbust-block-url-edit-rule))
            (define-key m [junkbust-block-url-edit-url]
              '("Block Verbatim" . junkbust-block-url-edit-url))
            (define-key m [separator-1]
              '("--"))
            (define-key m [junkbust-blocklist-file-visit]
              '("Visit Blocklist File" . junkbust-blocklist-file-visit))
            m))))

;; End:

(provide 'junkbust)

;;; junkbust.el ends here

