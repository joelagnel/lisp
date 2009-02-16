;;; nero.el --- a fast Lynx-based browser for Emacs

;; Copyright (C) 2005 Joe Corneli <jcorneli@math.utexas.edu>

;; Time-stamp: <jac -- Fri Apr 15 11:41:45 CDT 2005>

;; This file is not part of GNU Emacs, but it is distributed under
;; the same terms as GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Nero is a fast interactive web browser that runs under Emacs.  It
;; uses 'lynx -dump' to generate page content.  Navigation is
;; typically done via numbered links, a la Conkeror.  See
;; `nero-follow-link'.  Note that cookies that have been set by Lynx
;; continue to apply to Nero.  Nero itself does not set cookies.

;; Better-than-just-browsing interaction with various useful web
;; services is done via elvi, a la Surfraw.  The variable `nero-elvi'
;; contains a list of the currently available elvi.  See
;; `nero-defelvis' for instructions on how to write your own elvi.
;; Elvi are conveniently accessed using the function `nero-query'.

;; Fast access to commonly used pages is available through a
;; programmable hotlist, a la webjump.  The variable `nero-jumps'
;; contains a list of the currently available jumps.  See
;; `nero-defjump' for instructions on how to write your own jumps.
;; Jumps are conveniently accessed using the function `nero-jump'.

;; Standard bookmarks are also supported, see the variable
;; `nero-bookmarks' for details.

;; Facilities for "tabbed browsing" a la (insert name of browser) are
;; available.  Use `nero-new-tab' to make a new tab and
;; `nero-show-tabs' (and/or `nero-next-tab', `nero-previous-tab') to
;; switch between tabs.

;; Editing source code for remote pages is supported; see
;; `nero-edit-source'.  Viewing the GCIDE is partially supported; see
;; `nero-browse-url-force-html' (note: I am planning to introduce
;; improved GCIDE browsing in a subsequent emacs package).

;; Nero saves pages that have been browsed for later use.  This means
;; that reloading must be specifically requested; otherwise, moving
;; around through the pages that have already been loaded once is much
;; faster.  One simple further use of the saved information is
;; provided by `nero-collect-matching-pages', which is can be used to
;; search for pages you have browsed that match a given regexp.
;; Future versions of nero may further capitalize on the presence of
;; this saved information.

;; Nero provides facilities that let you easily plug in Emacs Lisp
;; code to do the following two things:
;;
;;  - to change the way pages display; or,
;;  - to automatically browse the web for you.
;;
;; An example of a function that will adjust the way pages display is
;; `nero-oddmuse-recent-changes-handler', which I use to simplify the
;; presentation of OddMuse recent changes pages.

;; An example of a function with programmatic web browsing behavior is
;; `nero-query-mapquest', which quickly finds and displays maps using
;; the mapquest service, cutting out several steps from the usual
;; interactive routine for achieving the same result.

;; If you want to write you own extensions, these examples may help;
;; but you should also see `nero-browse-url', the main function for
;; display, and `nero-basic-process-processing', the point of entry
;; for new content in the Nero recordkeeping system, for key details.

;; If you are just getting started using Nero, I would advise reading
;; over the user-modifiable variables (those with docstrings marked
;; with a star, below) and settting them to suit your taste and
;; environment.  You should also run `nero-mode-map' from the nero
;; buffer and get to know the key commands.  The examples below should
;; be enough to get you started, if you have everything configured
;; properly (and if not, you should find out as you try the examples).

;; Example usage:
;;
;; M-x nero-browse-url RET http://www.roman-emperors.org/nero.htm RET
;; 7 RET
;; b
;; C-s otho C-a RET
;; o
;; M-x nero-query-wikipedia RET fiddle rome RET
;; 11 M-x nero-download-link RET ~/roman-empire.html
;; C-x C-f ~/roman-empire.html
;; M-x nero-preview-current-buffer RET
;; ,
;; M-x nero-browse-url RET http://www.gnu.org/graphics/gnu-head.jpg RET
;; M-x nero-query-mapquest-local RET 9th and Hennepin RET [requires xv]
;; M-x nero-jump RET aster TAB RET
;; ...

;; Support:
;; 
;; Checks to: 421 Cedar Ave. #11, Minneapolis, MN, 55454.
;; Ideas for works to be done on commission: see AsteroidMeta.
;; Thanks.

;; Ideas for future work:
;;
;; * decode japanese etc.
;;   - I tried to do this, but ran into snags.  It shouldn't be
;;     hard, fetch headers and set codings, but I'm going to save
;;     it for now.
;;
;; * set things up so that several nero windows can be used at once.
;;   - (But tabs seem to work fine for me.)
;;
;; * Maybe add some "session management" features for storing
;;   histories between sessions.
;;   - Could be fun; should be totally modular wrt the current code.
;;   - maybe no page should be deleted from the multiverse, but
;;     rather, when they would be deleted they could be moved
;;     to a hidden "unused" thread.

;;; Code:

(require 'cl)

(defvar nero-homepage "http://www.ma.utexas.edu/~jcorneli/"
  "*Variable for user homepage.")

(defvar nero-bookmarks "~/bookmarks/map.html"
  "*Variable for user bookmarks.
A standard HTML file, bookmarks are written out in the format
used by Lynx.  Add bookmarks using `nero-add-bookmark-url',
`nero-add-bookmark-link' or `nero-add-bookmark-arbitrary-url'.
Access bookmarks using the function `nero-bookmarks'.")

(defvar nero-default-page nero-bookmarks
  "*New tabs open to this page, by default.")

(defvar nero-lynx-program "lynx"
  "*The name of the excecutable that supplies your Lynx.
On some systems this is \"lynx.stable\".")

;; thanks Christoph Conrad for the tip
(defvar nero-references-regexp "^References$"
  "*The regexp produced by your Lynx to mark the references section.
This may vary depending on your locale, so watch out!  Use 
'lynx -dump <some webpage>' to test.")

(defvar nero-images nil
  "*Whether or not nero should display links for images.")

(defvar nero-mode-hook nil
  "*Functions to run when `nero-mode' runs.")

(defvar nero-exiting-hook nil
  "*Functions to run when `nero-finished' runs, before killing nero.")

(defvar nero-before-browsing-hook nil
  "*Functions to run when `nero-browse-url' runs.
This can be used to associate certain actions with opening a
link.")

(defvar nero-after-download-hook nil
  "*Functions to run when `nero-download-url' has finished.")

(defvar nero-after-loading-hook nil
  "*Functions to run when `nero-basic-process-processing' finishes.")

(defvar nero-before-loading-hook nil
  "*Functions to run when `nero-basic-process-processing' starts.")

(defvar nero-fullscreen nil
  "*Whether or not nero should take up only one window.
If non-nil, delete other windows when loading a page.")

(defvar nero-erase-before-load nil
  "*Whether or not nero should erase the window when loading a new page.
If non-nil, delete other windows when loading a page.")

(defvar nero-long-thread nil
  "*Should `nero-ariadnes-thread' respond to `nero-restore-page'?")

;; the behavior for different image types should perhaps depend on
;; what configuration options we have.  But it is easiest to assume
;; that users can tweak the variable on their own.
(defvar nero-extensions-and-instructions '(("jpg" . image)
                                           ("png" . image)
                                           ("eps" . image)
                                           ("gif" . download)
                                           ("ps" . download)
                                           ("pdf" . download))
  "*File extensions and instructions for nero on how to handle
them This is an alist; the car of each element is a string giving
the file extension, and the cdr is one of several special handler
descriptions, or a function to call. For more details, see
`nero-process-nontext-file'.")

(defvar nero-decode-unicode t
  "*Whether or not to decode unicode strings (like \"&#29572;\"
or \"&#x7e41;\").")

(defvar nero-links-display-default t
  "*Whether or not links appear in the buffer by default.
Toggle the display of links with `nero-toggle-links-display'.")

(defvar nero-links-exist-default t
  "*Whether or not there are links in the buffer.
Toggle the existence of links with `nero-toggle-links-exist'.")

(defvar nero-width 75
  "*Width of the nero display, in columns.
To change interactively, use `nero-set-width'.")

(defvar nero-default-coding-system 'iso-latin-1
  "*Default coding system for translating process output.")

(defvar nero-default-flags nil
  "*Default flags to pass to 'lynx -dump', as a string (or nil).
No need to set the width as a flag here, that is maintained
automatically based on the value of `nero-width'.")

(defvar nero-tab-names-use-defaults t
  "*Whether to prompt the user for tab names or just use defaults.")

(defvar nero-default-elvis "Google"
  "*The default elvis for use with `nero-query'.")

(defvar nero-marking-elvi t
  "*Whether or not pages browsed using elvi should be marked.")

(defvar nero-home-city "Minneapolis"
  "*Default city for use with `nero-query-mapquest'.")

(defvar nero-home-state "MN"
  "*Default state for use with `nero-query-mapquest'.")

(defvar nero-Nero-buffer "*Nero*"
  "*The name of the buffer in which Nero primarily runs.")

(defvar nero-Nero-Tabs-buffer " *Nero Tabs*"
  "*The name of the buffer in which Nero lists tabs.")

(defvar nero-Nero-Refs-buffer " *Nero Refs*"
  "*The name of the buffer in which Nero lists references
This buffer is not usually seen by the user.")

(defvar nero-Nero-Matches-buffer " *Nero Matches*"
  "*The name of the buffer in which Nero lists matches.")

(defvar nero-Nero-Scratch-buffer " *Nero Scratch*"
  "*A place for nero to do text manipulations.")

(defvar nero-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map)
    (define-key map "a" 'nero-add-bookmark-url)
    (define-key map "A" 'nero-add-bookmark-link)
    (define-key map "b" 'nero-back)
    (define-key map "B" 'nero-back-to-beginning)
    (define-key map "c" 'nero-collect-matching-pages)
    (define-key map "C" 'nero-collect-matching-pages-1)
    (define-key map "d" 'nero-download-link)
    (define-key map "D" 'nero-recursively-download)
    (define-key map "e" 'nero-edit-source)
    (define-key map "f" 'nero-forward)
    (define-key map "F" 'nero-forward-to-end)
    (define-key map "g" 'nero-browse-url)
    (define-key map "G" 'nero-get-modified-url)
    (define-key map "h" 'nero-home)
    (define-key map "j" 'nero-jump)
    (define-key map "l" 'nero-kill-ring-save-current-link)
    (define-key map "L" 'nero-inspect-current-link)
    (define-key map "m" 'nero-exchange-point-and-mark)
    (define-key map "M" 'nero-mark-page)
    (define-key map "n" 'nero-return-to-nero-now)
    (define-key map "o" 'nero-open-tab)
    (define-key map "O" 'nero-new-tab)
    (define-key map "p" 'nero-get-tab)
    (define-key map "P" 'nero-rename-tab)
    (define-key map "q" 'nero-finished)
    (define-key map "r" 'nero-reload)
    (define-key map "R" 'nero-vanilla)
    (define-key map "s" 'nero-show-timescape)
    (define-key map "S" 'nero-show-ariadnes-thread)
    (define-key map "t" 'nero-toggle-links-display)
    (define-key map "T" 'nero-toggle-links-exist)
    (define-key map "u" 'nero-kill-ring-save-current-url)
    (define-key map "U" 'nero-inspect-current-url)
    (define-key map "v" 'nero-bookmarks)
    (define-key map "V" 'nero-browse-file)
    (define-key map "w" 'nero-show-tabs)
    (define-key map "x" 'nero-external-current-url)
    (define-key map "X" 'nero-external-current-link)
    (define-key map "y" 'nero-yank-marked-page)
    (define-key map ";" 'nero-hide)
    (define-key map "," 'nero-previous-tab)
    (define-key map "." 'nero-next-tab)
    (define-key map "/" 'nero-query-default-elvis)
    (define-key map "?" 'nero-query)
    (define-key map (kbd "RET") 'nero-follow-link)
    (define-key map (kbd "TAB") 'nero-move-to-next-link)
    (define-key map [(shift return)] 'nero-follow-link-new-tab)
    (define-key map [(shift tab)] 'nero-move-to-previous-link)
    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "DEL") 'scroll-down)
    map)
  "Keymap for nero major mode.")

(defvar nero-mode-syntax-table
  (let ((nero-mode-syntax-table text-mode-syntax-table))
    nero-mode-syntax-table)
  "Syntax table for nero mode.")

(defvar nero-link-regexp "\\[\\([0-9]+\\)\\]"
  "Regular expression that tells nero what links look like.
The first parenthesized subexpression is the unique
string (representing a base 10 number) denoting a link; the page
with the associated number which will be sought among the
references when `nero-follow-link-internal' runs.")

(defvar nero-font-lock-keywords
  `((,nero-link-regexp . font-lock-keyword-face))
  "Font lock for `nero-mode'.
Currently, only numbered links are fontified.")

(defvar nero-old-layout nil 
"The window layout as it was before nero started.
The old layout is restored when either `nero-hide' or
`nero-finished' runs.")

(defvar nero-history nil "Record of pages already visited.
See also `nero-future', `nero-ariadnes-thread'.")

(defvar nero-future nil
  "Record of pages visited and then retreated from with `nero-back'.
See also `nero-history', `nero-ariadnes-thread'.")

(defvar nero-mark nil
  "A distinguished page.
The user can declare any page to be `nero-mark'.  See
`nero-mark-page' and `nero-exchange-point-and-mark'.")

(defvar nero-marks nil
  "A list of distinguished moments.
Every moment set by `nero-mark-page' is added to this
list.")

(defvar nero-tab nil
  "The current tab.
See `nero-set-tab' and `nero-get-tab'.")

(defvar nero-tabs nil
  "A list of tabs.
Every tab set by `nero-set-tab' is added to this list.")

(defvar nero-multiverse nil
  "All the timescapes (tabs) stored in the nero system.
This variable is \"highly non-user visible.\" If you are a user,
you can probably stop reading now.

If you're a programmer extending the nero system, then you should
know that this variable is a list of timescapes; the car of each
timescape is its name (like \"Tab 1\", for example), the next
element is the history associated with this timescape, and the
third element is the future associated with this timescape.  

Each element of a timescape is a page, here represented as a list
of the associated url, the rendered content, references to other
urls, the location of the point, and another list of certain
miscellaneous properties.

To understand how the system works in more detail, note that
`nero-browse-url' calls `nero-update-timescape'. This function
calls `nero-add-history-element', and will call `nero-set-tab' if
no tabs have currently been defined.  In turn, `nero-set-tab'
calls `nero-add-multiverse-element', finally adjusting the
variable you are reading about now.  In other words, the system
is designed so that every page you browse is associated with some
tab.")

(defvar nero-ariadnes-thread nil "Full record of the pages visited.
Every page loaded by `nero-browse-url' is added to this list.
Even more extensive recording is done if `nero-long-thread' is
non-nil.")

(defvar nero-links-sanctuary nil
  "Save links here when deleting with `nero-toggle-links-exist'.")

(defvar nero-elvi nil
  "List of elvi known to nero.
Adjusted whenever `nero-defelvis' adds a new elvis.")

(defvar nero-jumps nil
  "List of jumps known to nero.
Adjusted whenever `nero-defjump' adds a new jump.")

;; this could be switched by `nero-mode' to be buffer local if we
;; transition to allowing multiple `nero-mode' windows at once.
(defvar nero-buffer-in-nero-mode nil
  "Says whether or not the *Nero* buffer is in `nero-mode'.
This variable is investigated every time `nero-browse-url' runs.
If we're not in the right mode, `nero-browse-url' runs `nero-mode'
and sets this variable to t.  Whenever the *Nero* buffer is killed,
the variable is set back to nil.")

(defvar nero-width-flag (concat "-width=" (int-to-string nero-width))
  "To adjust this variable, please use `nero-set-width'")

(defvar nero-links-display nero-links-display-default 
  "*Whether or not links appear in the current buffer.
Resets to the value of `nero-links-display-default' whenever a page
is reloaded.")

(defvar nero-links-exist nero-links-exist-default
  "Whether or not there are links in the current buffer.
Resets to the value of `nero-links-exist-default' whenever a page
is reloaded.")

(defvar nero-after-download-internal-hook nil
  "Run after download without changing `nero-after-download-hook'.
The variable is immediately set back to nil after the hook runs.")

(defvar nero-after-loading-internal-hook nil
  "Run after download without changing `nero-after-loading-hook'.
The variable is immediately set back to nil after the hook runs.")

(defvar nero-before-loading-internal-hook nil
  "Run before rendering without changing `nero-before-loading-hook'.
The variable is immediately set back to nil after the hook runs.")

(defvar nero-state-curpoint nil "State of current point.")
(defvar nero-state-URL nil "State of URL.")
(defvar nero-state-flags nil "State of flags.")
(defvar nero-state-handler nil "State of handler.")
(defvar nero-state-mode nil "State of mode.") 
(defvar nero-state-action nil "State of action.")
(defvar nero-state-ephem nil "State of ephemeralness.") 
(defvar nero-state-revise nil "State of revision.")
(defvar nero-state-tab nil "State of tab.")

(defvar download-process nil "The download process")
(defvar load-buffer-process nil "The load buffer process")
(defvar load-url-process nil "The load url process")

(defun nero-mode ()
  "Major mode for browsing the web.
Commands:
\\{nero-mode-map}
Entry to this mode calls the value of `nero-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table nero-mode-syntax-table)
  (use-local-map nero-mode-map)
  (setq major-mode 'nero-mode)
  (set (make-local-variable 'font-lock-defaults)
       '(nero-font-lock-keywords))
  (set (make-local-variable 'nero-buffer-in-nero-mode) t)
  (setq major-mode 'nero-mode)
  (setq mode-name "nero")
  (add-hook 'kill-buffer-hook 'nero-wind-down nil t)
  ;; I don't know if this is needed or not
  (font-lock-fontify-buffer)
  ;; run hook before beginning
  (run-hooks 'nero-mode-hook))

(defun nero-carte-blanche ()
  "Clear the *Nero* and  *Nero Refs* buffers."
  (interactive)
  (set-buffer (get-buffer-create nero-Nero-buffer))
  (erase-buffer)
  (set-buffer (get-buffer-create nero-Nero-Refs-buffer))
  (erase-buffer))

(defun nero-tabula-rosa ()
  "Give nero's history and future a fresh start.
The only thing in the history will be the current page."
  (interactive)
  (setq nero-history (last nero-history)
        nero-future nil))

(defun nero-scorched-earth ()
  "Set each of nero's state variables to nil."
  (interactive)
  (setq nero-history nil
        nero-future nil)
  (setq nero-old-layout nil
        nero-ariadnes-thread nil
        nero-tabs nil
        nero-marks nil
        nero-tab nil
        nero-mark nil
        nero-multiverse nil))

(defun nero-download-url (URL &optional saveas)
  "URL is a web address or path to a file.
Running this command downloads the URL and saves it to a file.
If you want to check on the progress of the download, have a
look at the *Async Shell Command* buffer."
  (interactive 
   (list (read-string (concat "URL "
                              "(default "
                              (nero-current-url)
                              "): ")
                      nil
                      nil
                      (nero-current-url))))
  (let ((saveas
         (or saveas
             (read-string "Save as: "
                          (let* ((filename (concat 
                                            (replace-regexp-in-string 
                                             ".*/" "" URL))))
                            (expand-file-name (if (equal filename "")
                                                  "index.html"
                                                filename)
                                              default-directory))
                          nil
                          "~/nero-download"))))
    (let ((winconf (current-window-configuration)))
      (setq download-process (start-process
                              "download"
                              nil
                              "wget"
                              "-O" 
                              saveas
                              URL))
      (set-process-sentinel download-process 'download-process-sentinel)
      (set-window-configuration winconf))))

(defun download-process-sentinel (proc signal)
  (when (string-equal signal "finished\n")
    (run-hooks 'nero-after-download-hook)
    (run-hooks 'nero-after-download-internal-hook)
    (when nero-after-download-internal-hook
      (setq nero-after-download-internal-hook nil))
    (message "Download finished.")))

(defun nero-recursively-download (URL &optional level)
  "Recursively download the contents of URL.
Calls 'wget -r -l 1' to do the work. If LEVEL is a number,
use that number instead."
  (interactive (list (read-string (concat "URL: "
                                          "(default "
                                          (nero-current-url)
                                          "): ")
                                  nil
                                  nil
                                  (nero-current-url))))
  (let ((dl (or (int-to-string level) "1")))
    (setq download-process (start-process "download"
                                          nil
                                          "wget"
                                          "-r"
                                          "-l"
                                          dl
                                          URL))
    (set-process-sentinel download-process 'download-process-sentinel)))

(defun nero-recursively-download-current-page (level)
  "Use `nero-recursively-download' to save page and subpages to LEVEL."
  (interactive "p")
  (nero-recursively-download (nero-current-url) level))

(defun nero-external-current-url ()
  "Use `browse-url' to view the current url."
  (interactive)
  (browse-url (nero-current-url)))

(defun nero-external-current-link ()
  "Use `browse-url' to view the current link."
  (interactive)
  (browse-url (nero-current-link)))

(defun nero-browse-url-force-html (URL)
  "Like `nero-browse-url', but sends the '-force-html' flag to Lynx."
  (interactive "MURL: ")
  (nero-browse-url URL "-force-html"))

(defun nero-edit-source (&optional mode)
  "Edit the source code for the current page.
Optional argument MODE is the mode to edit in; it defaults
to `html-mode'."
  (interactive)
  (nero-edit-source-at-url (nero-current-url)))

(defun nero-edit-source-at-url (url &optional mode)
  "Edit the source code found at the location URL.
Optional argument MODE is the mode to edit in; it defaults
to `html-mode'."
  (interactive "MURL: ")
  (let ((nero-lynx-program (concat nero-lynx-program " -source"))
        (nero-images nil)
        (nero-default-flags nil)
        (nero-Nero-buffer (nero-buffername-from-url url)))
    (nero-browse-url url nil nil (or mode 'html-mode) nil t t)))

(defun nero-buffername-from-url (url)
  "Generate an acceptable name for a buffer based on URL."
  (let ((start-with-this (replace-regexp-in-string ".*/" "" url)))
    (if (equal start-with-this "")
        "index.html"
      start-with-this)))

(defun nero-preview-region (&optional flags)
  "Preview the current region in nero.
If given, FLAGS is a string giving any additional flags to be
passed to the 'lynx -dump' rendering engine."
  (interactive)
  (nero-preview-string (buffer-substring-no-properties
                        (region-beginning)
                        (region-end))
                       flags))

(defun nero-preview-string (str &optional flags)
  "Preview string STR in nero.
If given, FLAGS is a string giving any additional flags to be
passed to the 'lynx -dump' rendering engine."
  (interactive)
  (with-temp-buffer
    (insert str)
    (nero-preview-current-buffer flags)))

(defun nero-preview-buffer (&optional buffer flags)
  "Preview BUFFER (by default, the current buffer) in nero.
If given, FLAGS is a string giving any additional flags to be
passed to the rendering engine. See also `nero-preview-string',
`nero-preview-region', and `nero-preview-current-buffer'."
  (interactive (list (read-string (concat "Buffer "
                                          "(default "
                                          (buffer-name (current-buffer))
                                          "): ")
                                  nil
                                  nil
                                  (buffer-name (current-buffer)))))
  (set-buffer (get-buffer-create buffer))
  (nero-browse-url (buffer-name) flags (cons 'buffer (buffer-name))
                   nil nil nil t))

(defun nero-preview-current-buffer (&optional flags)
  "Like `nero-preview-buffer' but automatically use the current buffer."
  (interactive)
  (nero-browse-url (buffer-name) flags (cons 'buffer (buffer-name))))

(defun nero-get-modified-url (URL)
  "Get a new url based on the current URL.
The current url appears in the minibuffer for editing.  Note, you
are not editing the contents of this url!"
  (interactive (list (read-string "URL: " (nero-current-url)
                                  nil nil (nero-current-url))))
  (nero-browse-url URL))

(defun restore-nero-environment ()
  "Restore settings which may have changed to their defaults.
Called by `nero-browse-url', `nero-restore-page'."
  (when nero-fullscreen (delete-other-windows))
  (setq nero-links-exist nero-links-exist-default
        nero-links-display nero-links-display-default
        nero-links-sanctuary nil))

(defun nero-browse-file (filename)
  "Speed up browsing files in nero by using `read-file-name'.
Calls `nero-browse-url'.  If you need more detailed control of
nero behavior, just use that function instead.  This is a
convenience function."
  (interactive (list (read-file-name "File: " nil "" nil)))
  (nero-browse-url filename))

(defun nero-processed-before (url)
  "Check to see if we have processed URL before.
If so, return the page corresponding to the URL.  Otherwise,
return nil."
  (let ((i 0)
        (len (length nero-multiverse))
        (found nil))
    (while (and (not found)
                (< i len))
      (let* ((j 0)
             (tab (cadr (nth i nero-multiverse)))
             (len2 (length tab)))
        (setq i (1+ i))
        (while (and (not found)
                    (< j len2))
          (let ((candidate (nth j tab)))
            (if (equal (car candidate) url)
                (setq found candidate)
              (setq j (1+ j)))))))
    found))

;; double check use of `nero-restore-page'.
(defun nero-browse-url (URL &optional
                            flags handler mode action ephem revise)
  "URL is a web address or path to a file.
Running this command displays a rendered version of the URL in
the *Nero* buffer.  

If given, FLAGS is a string giving any additional flags to be
passed to the 'lynx -dump' rendering engine.

If non-nil, HANDLER is either a flag specifying the behavior for
`nero-process-nontext-file' or `nero-generate-content', or a
function that takes URL, FLAGS and ACTION as its arguments.

MODE says what mode to put the resulting buffer in.  If MODE is
nil, the resulting buffer is put into `nero-mode', and a list of
references is produced which make the page suitable for
interactive browsing.

Optional argument ACTION can be used to give nero special
instructions on how to deal with the file after it is loaded; if
non-nil, it should be a function that takes zero arguments.

Optional argument EPHEM, if non-nil, says to be ephemeral, i.e.,
to not update the timescape.

If REVISE is non-nil, revise the data we have stored about the
current page (downloading and rendering again, but not adding
cells to the history) instead of adding a new page."
  (interactive (list (completing-read 
                      "URL: " 
                      (apply
                       #'concatenate
                       'list
                       (map 'list (lambda (tab) 
                                    (mapcar (lambda (page)
                                              (car page))
                                            (cadr tab)))
                            nero-multiverse)))))
  (restore-nero-environment)
  (run-hooks 'nero-before-browsing-hook)
  ;; set up environment
  (unless nero-old-layout
    (setq nero-old-layout (current-window-configuration)))
  (switch-to-buffer (get-buffer-create nero-Nero-buffer))
  (unless nero-buffer-in-nero-mode
    (if mode
        (funcall mode)
      (nero-mode)))
  ;; save state for use when asynchronous processes finish
  (setq nero-state-curpoint (point)
        nero-state-URL URL
        nero-state-flags flags
        nero-state-handler handler
        nero-state-mode mode
        nero-state-action action
        nero-state-ephem ephem
        nero-state-revise revise)
  ;; process the content if/as needed
  (let ((old-content (nero-processed-before URL)))
    (cond
     ((and (not revise) old-content)
      (nero-restore-page old-content nil))
     ((functionp handler)
      (funcall handler URL flags action))
     ((nero-process-nontext-file URL handler) t)
     (t (nero-generate-content URL flags handler)))))

(defun nero-process-nontext-file (URL &optional handler)
  "Called by `nero-browse-url' to handle some non-text files specially.
URL is the location of the file. 

If non-nil, HANDLER should either be equal to 'image, 'download,
or a function that takes URL as an argument."
  (let* ((filename (replace-regexp-in-string ".*/" "" URL))
         (extension (replace-regexp-in-string ".*\\." "" filename))
         (handle (or handler
                     (cdr (assoc extension
                                 nero-extensions-and-instructions)))))
    (cond
     ((eq handle 'image)
      (set-buffer (get-buffer-create nero-Nero-buffer))
      (erase-buffer)
      (insert-image
       (create-image
        (with-temp-buffer
          ;; thanks for the suggestion Miles Bader
          (set-buffer-multibyte nil)
          (if (file-regular-p URL)
              (insert-file-contents URL)
            (url-insert-file-contents URL))
          (buffer-string))
        nil t))
      (nero-revise-or-update-timescape))
     ((eq handle 'download)
      (nero-download-url URL))
     ;; everything else we process as a text file.
     (t nil))))

;; and thanks once again Miles Bader!
(defun nero-translate-unicode-codepoints-to-utf-16 ()
  "Do replacements to decode and render unicode codepoint strings."
  (goto-char (point-min))
  (while (re-search-forward "&#\\(x\\)?\\([0-9a-f]+\\);" nil t)
    (let* ((ucs (string-to-number (match-string 2)
                                  (if (match-beginning 1) 16 10)))
           (decoded (decode-char 'ucs ucs)))
      (when decoded
        (delete-region (match-beginning 0) (match-end 0))
        (insert-char decoded 1)))))

(defun nero-generate-content (URL flags handler)
  "Puts initial text in the *Nero* buffer.
This includes the normal (rendered) text of URL, as well as any
references found in the page.  The references will later be moved
to the *Nero Refs* buffer.  FLAGS are strings to feed to the
inferior lynx process.

HANDLER explains how to deal with the \"URL\", specifically, if
its `car' is equal to 'buffer or it is a function, then we do
something different from usual.  If HANDLER is a function, it
should two (or more) arguments, the three necessary arguments
being URL and FLAGS."
  (when nero-erase-before-load
    (erase-buffer))
  (when nero-decode-unicode
    (add-hook 'nero-after-loading-internal-hook
              'nero-translate-unicode-codepoints-to-utf-16))
  (if (and (listp handler)
           (eq (car handler) 'buffer))
      (nero-generate-buffer-content URL flags (cdr handler))
    (nero-generate-url-content URL flags)))

(defun nero-generate-buffer-content (URL flags buf)
  "Called by `nero-generate-content' to process buffers."
  (save-excursion
    (set-buffer buf)
    (write-region (point-min) (point-max) ".nero.html")
    (let ((width (or nero-width-flag "-width=75"))
          (images (if nero-images "-image_links" ""))
          (dfl (if nero-default-flags nero-default-flags ""))
          (fl (if flags flags "")))
      (set-buffer (get-buffer-create nero-Nero-Scratch-buffer))
      (erase-buffer)
      (setq load-buffer-process
            (start-process
             "load-buffer"
             nero-Nero-Scratch-buffer
             "lynx"
             width
             images
             dfl
             fl
             "-dump"
             "~/.nero.html"))
      (set-process-sentinel load-buffer-process 
                            'load-buffer-process-sentinel))))

(defun load-buffer-process-sentinel (proc signal)
  (when (string-equal signal "finished\n")
    (delete-file "~/.nero.html")
    (nero-basic-process-processing)))

(defun nero-basic-process-processing ()
  "Once a page is downloaded and rendered, fit the content into nero."
  (set-buffer (get-buffer-create nero-Nero-Scratch-buffer))
  (let ((str (buffer-string)))
    (set-buffer nero-Nero-buffer)
    (unless nero-erase-before-load
      (erase-buffer))
    (insert str)
    ;; run first set of hooks
    (run-hooks 'nero-before-loading-hook 
               'nero-before-loading-internal-hook)
    (setq nero-before-loading-internal-hook nil)
    ;; create references and update timescape
    (unless nero-state-mode (nero-generate-references))
    (unless nero-state-ephem
      (nero-revise-or-update-timescape))
    ;; run second set of hooks
    (run-hooks 'nero-after-loading-hook
               'nero-after-loading-internal-hook)
    (setq nero-after-loading-internal-hook nil)
    (goto-char (point-min)))
  (message "Page loaded.")
  (when (functionp nero-state-action)
    (funcall nero-state-action)))

(defun nero-revise-or-update-timescape ()
  "Set appropriate cells in `nero-multiverse', if needed."
  (let ((props (list nero-state-flags 
                     nero-state-handler 
                     nero-state-mode 
                     nero-state-action
                     nero-state-ephem)))
    (if (and nero-state-revise nero-multiverse)
        (nero-revise-current-page nero-state-URL 
                                  nero-state-curpoint 
                                  props)
      (nero-update-timescape nero-state-URL 
                             nero-state-curpoint 
                             props))))

(defun nero-generate-url-content (URL flags)
  "Called by `nero-generate-content' to process urls."
  (let ((width (or nero-width-flag "-width=75"))
        (images (if nero-images "" "-image_links"))
        (dfl (if nero-default-flags nero-default-flags ""))
        (fl (if flags flags "")))
    (set-buffer (get-buffer-create nero-Nero-Scratch-buffer))
    (erase-buffer)
    (setq load-url-process
          (start-process
           "load-buffer"
           nero-Nero-Scratch-buffer
           "lynx"
           width
           images
           dfl
           fl
           "-dump"
           URL))
    (set-process-coding-system 
     load-url-process nero-default-coding-system)
    (set-process-sentinel load-url-process 
                          'load-url-process-sentinel)
    (message "Loading...")))

(defun load-url-process-sentinel (proc signal)
  (when (string-equal signal "finished\n")
    (nero-basic-process-processing)))

(defun nero-generate-references ()
  "Called by `nero-browse-url' to set up the  *Nero Refs* buffer."
  (save-excursion
    (set-buffer nero-Nero-buffer)
    (goto-char (point-max))
    (if (search-backward-regexp nero-references-regexp nil t)
        (let ((nero-references (buffer-substring-no-properties
                                (match-beginning 0) (point-max))))
          (delete-region (match-beginning 0) (point-max))
          (save-excursion
            (set-buffer (get-buffer-create nero-Nero-Refs-buffer))
            (delete-region (point-min) (point-max))
            (insert nero-references)))
      (set-buffer (get-buffer-create nero-Nero-Refs-buffer))
      (delete-region (point-min) (point-max)))))

(defun nero-update-timescape (URL curpoint props)
  "Called by `nero-browse-url' to set history and future, etc.
This function will also initiate the creation of a new tab, if no
tabs have been created so far."
  ;; add to history
  (unless (equal (nero-current-url) URL)
    (nero-add-history-element URL props))
  ;; save location of point in previous timeframe
  (when (> (length nero-history) 1)
    (setcar (nthcdr 3 (nero-previous-page)) curpoint))
  ;; update future as needed.  If we've moved forward in time one
  ;; unit, we don't need the last cell in the future anymore.
  (if (equal (caar (last nero-future)) URL)
      (setq nero-future (nbutlast nero-future))
    ;; we maintain this "weird" nil-headed structure for `nero-future'
    ;; so we can consistenly use tail pointers.
    (setq nero-future (if (not nero-future)
                          (list nil)
                        (nbutlast nero-future
                                  (1- (length nero-future))))))
  ;; set up a a default tab if there are no tabs yet
  (unless nero-tabs (nero-set-tab "Tab 1"))
  ;; update our ariadne's thread (this is where the non-nil return
  ;; value comes from, by the way)
  (setq nero-ariadnes-thread
        (cons (nero-current-url) nero-ariadnes-thread)))

(defun nero-revise-current-page (url curpoint props)
  "Called by `nero-browse-url' to revise the last history cell."
  (setcar (last nero-history) 
          (nero-new-content url props curpoint))
  (cond
   ((not nero-tabs) (nero-set-tab "Tab 1"))
   (t t)))

(defun nero-add-history-element (URL props)
  "Called by `nero-update-timescape' to add a history element."
  (setq nero-history
        (nconc nero-history (list (nero-new-content URL props 1)))))

(defun nero-new-content (URL props pt)
  "Called by `nero-add-history-element' to make a new history element.
Also called by `nero-revise-current-page' to update the content
of an existing history element."
  (list
   URL
   (save-excursion
     (set-buffer (get-buffer-create nero-Nero-buffer))
     ;; use properties to make images work
     (buffer-substring (point-min)
                       (point-max)))
   (save-excursion
     (set-buffer 
      (get-buffer-create nero-Nero-Refs-buffer))
     ;; there probably won't be properties here.
     (buffer-substring (point-min)
                       (point-max)))
   pt
   props))

(defun nero-mark-page ()
  "Use this function to set `nero-mark'.
Use `nero-exchange-point-and-mark' to retrieve to this \"moment
in time\" (see its docstring for details).  Note that `set-mark',
a familiar editing command, is in some ways analogous to this
command."
  (interactive)
  (message "Page marked.")
  (setq nero-mark (cons nero-tab (nero-current-page))
        nero-marks (add-to-list 'nero-marks nero-mark)))

(defun nero-exchange-point-and-mark (&optional nonmarking)
  "Use this function to retreive `nero-mark'.
If `nero-mark' has not been set, this function behaves like
`nero-return-to-nero-now'. Warning: Either the past or the future
may be different from how they were when you ran
`nero-mark-page' (and in fact they probably will be).

Optional argument NONMARKING means just go to the mark, don't
mark the current page."
  (interactive)
  (let ((mark nero-mark))
    (when mark
      (unless nonmarking (nero-mark-page))
      (nero-get-tab (car mark))
      (nero-restore-page (cdr mark) t))))

(defun nero-goto-mark ()
  "Retreive the marked page but do not mark the current page.
Uses `nero-exchange-point-and-mark'."
  (interactive)
  (nero-exchange-point-and-mark 'nonmarking))

;; rather than just deleting these, it would be nicer to save them.
;; My excuse for the time being is that this is what emacs does with
;; its marks.  Both could be improved.
(defun nero-yank-marked-page ()
  "Unwind to a previously marked page."
  (interactive)
  (if nero-marks
      (progn (nero-goto-mark)
             (setq nero-marks (cdr nero-marks)
                   nero-mark (car nero-marks)))
    (message "No pages marked.")))

(defun nero-return-to-nero-now ()
  "Use this function to return to the \"present\".
This is useful to return from the displays produced by
`nero-show-tabs' and `nero-show-ariadnes-thread', both of which
are considered to be \"outside of time\"."
  (interactive)
  (nero-restore-page (nero-current-page) nil))

(defun nero-collect-matching-pages (regexp)
  "List the pages from the current tab that match REGEXP.
The list is browsable."
  (interactive "MRegexp: ")
  (let ((matches nil))
    (dolist (page nero-history)
      (let ((pt 
             (with-temp-buffer (insert (second page))
                               (goto-char (point-min))
                               (search-forward-regexp regexp nil t))))
        (when pt (setq matches (cons (cons (first page) pt) matches)))))
    (dolist (page (cdr nero-future))
      (let ((pt
             (with-temp-buffer (insert (second page))
                               (goto-char (point-min))
                               (search-forward-regexp regexp nil t))))
        (when pt (setq matches (cons (cons (first page) pt) matches)))))
    (nero-show-matches matches)))

(defun nero-collect-matching-pages-1 (regexp)
  "List all the pages in `nero-multiverse' that match REGEXP.
The list is browsable."
  (interactive "MRegexp: ")
  (let ((matches nil))
    (dolist (tab nero-multiverse)
      (dolist (page (second tab))
        (let ((pt
               (with-temp-buffer
                 (insert (second page))
                 (goto-char (point-min))
                 (search-forward-regexp regexp nil t))))
          (when pt (setq matches 
                         (cons (cons (first page) pt) matches)))))
      (dolist (page (cdr (third tab)))
        (let ((pt
               (with-temp-buffer
                 (insert (second page))
                 (goto-char (point-min))
                 (search-forward-regexp regexp nil t))))
          (when pt (setq matches
                         (cons (cons (first page) pt) matches))))))
    (nero-show-matches matches)))

(defun nero-show-matches (matches)
  "Show matching pages, as given by `nero-collect-matching-pages'
or `nero-collect-matching-pages-1'."
  (set-buffer (get-buffer-create nero-Nero-Refs-buffer))
  (erase-buffer)
  (let ((number 1)
        (mats matches))
    (while mats
      (insert
       (concat " " (int-to-string number)
               ". " (caar mats)
               " " (int-to-string (cdar mats)) "\n"))
      (setq mats (cdr mats))
      (setq number (1+ number))))
  (nero-mark-up-references-for-browsing nero-Nero-Matches-buffer)
  (set-buffer nero-Nero-Matches-buffer))

(defun nero-show-timescape ()
  "Show browsing history and future, indicating current page.
The page is navigable, or you can use `nero-return-to-nero-now'
to return to browsing the current page."
  (interactive)
  (set-buffer nero-Nero-Refs-buffer)
  (erase-buffer)
  (let ((number 1)
        (hist (reverse nero-history))
        (fut nero-future))
    (while fut
      (when (caar fut)
          (insert (concat " " (int-to-string number) ". " 
                          (caar fut) "\n"))
          (setq number (1+ number)))
      (setq fut (cdr fut)))
    ;; mark the page that's current
    (insert
     (concat " " (int-to-string number) ". " (caar hist) " ***\n"))
    (setq hist (cdr hist))
    (setq number (1+ number))
    (while hist
      (insert (concat " " (int-to-string number) ". " (caar hist) "\n"))
      (setq hist (cdr hist))
      (setq number (1+ number))))
  (nero-mark-up-references-for-browsing))

(defun nero-show-ariadnes-thread ()
  "Show a page displaying a full record of the pages that have loaded.
See `nero-ariadnes-thread'."
  (interactive)
  (set-buffer nero-Nero-Refs-buffer)
  (erase-buffer)
  (let ((number 1)
        (thread nero-ariadnes-thread))
    (while thread
      (insert (concat
               " "
               (int-to-string number)
               ". "
               (car thread) "\n"))
      (setq thread (cdr thread))
      (setq number (1+ number))))
  (nero-mark-up-references-for-browsing))

(defun nero-show-tabs ()
  "Switch to the *Nero Tabs* buffer, a marked-up list of tabs.
Following a link in this buffer makes the linked tab
current. "
  (interactive)
  (cond
   (nero-tabs
    (set-buffer (get-buffer-create nero-Nero-Refs-buffer))
    (erase-buffer)
    (let ((number 1)
          (tabs nero-tabs))
      (while tabs
        (insert
         (concat " " (int-to-string number)
                 ". " (nero-tab-currently-browsing (car tabs))
                 " " (car tabs) "\n"))
        (setq tabs (cdr tabs))
        (setq number (1+ number))))
    (nero-mark-up-references-for-browsing nero-Nero-Tabs-buffer)
    (set-buffer nero-Nero-Tabs-buffer)
    (save-excursion (when (search-forward-regexp nero-tab nil t)
                      (goto-char (line-beginning-position))
                      (insert "**")
                      (delete-char 2))))
   (t (nero-set-tab "Tab 1")
      (nero-show-tabs))))

(defun nero-tab-currently-browsing (name)
  "Return the url that the tab named NAME is currently browsing.
But if there is none, return the empty string."
  (or (caar (last (cadr (assoc name nero-multiverse))))
      ""))

(defun nero-mark-up-references-for-browsing (&optional buffer)
  "Turns a numbered list of references into something browsable.
Called by various functions, including `nero-show-tabs' and
`nero-show-timescape'.  BUFFER defaults to `nero-Nero-buffer'."
  (let ((content (buffer-string)))
    (switch-to-buffer (get-buffer-create (or buffer
                                             nero-Nero-buffer)))
    ;; in a new buffer, switch to *Nero Mode*
    (unless (string= major-mode "nero-mode")
      (nero-mode))
    (erase-buffer)
    (insert "\n")
    (insert content)
    (goto-char (point-min)))
  (while (re-search-forward
          "^ \\([0-9]+\\)\. \\([^ \n]*\\)\\(.*\\)" nil t)
    (let* ((num (match-string 1))
           (fp (match-string 2))
           (sp (match-string 3))
           (beg (match-beginning 0))
           (end (match-end 0))
           (replacement
            (cond
             ((equal sp " ***")
              (concat "** [" num "] "  fp ))
             ((not (equal sp ""))
              (concat "   [" num "] "
                      (progn
                        (setq sp (replace-regexp-in-string
                                  "^ +" "" sp))
                        (cond 
                         ((equal buffer nero-Nero-Tabs-buffer)
                          (concat "\"" sp "\" viewing "  fp ""))
                         ((equal buffer nero-Nero-Matches-buffer)
                          ;; ok, not totally clean, but it works...
                          (concat fp " " (progn (set-text-properties
                                                 0 (length sp)
                                                 '(invisible t)
                                                 sp)
                                                sp)))))))
             (t
              (concat "   [" num "] " fp)))))
      (delete-region beg end)
      (insert replacement)))
  (font-lock-fontify-buffer)
  (goto-char (point-min)))

;; should complement this with a `nero-delete-tab'.
(defun nero-new-tab (&optional name where)
  "Open a new tab, named NAME.
WHERE controls the url to display and defaults to
`nero-default-page'.  NAME defaults to whatever is returned by
`nero-default-new-tab-name'.  See `nero-set-tab' for further
information on creating tabs."
  (interactive (list (if nero-tab-names-use-defaults
                         (nero-default-new-tab-name)
                       (read-string
                        (concat "Name (default: "
                                (nero-default-new-tab-name)
                                "): ")
                        nil
                        nil
                        (nero-default-new-tab-name)))
                     (read-string
                      (concat "URL (default: "
                              nero-default-page
                              "): ")
                      nil
                      nil
                      nero-default-page)))
  (nero-set-tab (or name
                    (nero-default-new-tab-name))
                (or where
                    nero-default-page)))

(defun nero-open-tab (arg)
  "Open a new tab, to the `nero-default-page' or the current page.
The behavior is governed by the prefix argument ARG."
  (interactive "P")
  (if arg (nero-new-tab nil (nero-current-url))
    (nero-new-tab)))

(defun nero-set-tab (name &optional url)
  "Set up a new tab named NAME.
If optional argument URL is t, the tab will open to the
`nero-default-page'.  If nil, to the current page.
Otherwise, the new tab will open to URL.

Note: only built-in functions should call this with a nil URL."
  (nero-add-multiverse-element name url)
  (setq nero-tabs (add-to-list 'nero-tabs name t))
  ;; identify the tab we just created as active
  (setq nero-tab name))

(defun nero-rename-tab (newname)
  "Change the name of the current tab."
  (interactive (list (read-string
                      "New name: "
                      nil
                      nil
                      nero-tab)))
  (setcar (assoc nero-tab nero-multiverse) newname)
  (setq nero-tabs (substitute newname nero-tab nero-tabs :test #'equal))
  (setq nero-tab newname))

(defun nero-add-multiverse-element (name url)
  "Called by `nero-set-tab' to add a multiverse element.
The new element is named NAME.  If URL is given, it will open to
a new tab browsing URL.  Otherwise, it will preserve the current
url, and just set it up so that it is inside a tab."
  (setq nero-state-tab name)
  (add-hook 'nero-after-loading-internal-hook
            (lambda ()
              (setq nero-multiverse
                    (if nero-multiverse
                        (add-to-list 
                         'nero-multiverse
                         (let ((newtab (list 'foo)))
                           (setcar newtab nero-state-tab)
                           (setcdr newtab (list nero-history))
                           (setcdr (cdr newtab) (list nero-future))
                           newtab)
                         t)
                      (list (let ((newtab (list 'foo)))
                              (setcar newtab nero-state-tab)
                              (setcdr newtab (list nero-history))
                              (setcdr (cdr newtab) (list nero-future))
                              newtab))))))
  (when url
    (setq nero-history nil
          nero-future nil)
    (nero-browse-url url)))

(defun nero-default-new-tab-name ()
  "Generate the default name of a new tab."
  (concat "Tab "
          (int-to-string
           (1+ (length nero-tabs)))))

(defun nero-get-tab (name)
  "Retrieve and activate the tab NAME.
Interactively, name is found using a completing read over the
list of all tabs."
  (interactive (list (let ((completion-ignore-case t))
                       (completing-read "Tab: " nero-tabs))))
  (setcar (nthcdr 3 (nero-current-page)) (point))
  (unless (equal name "")
    (setq nero-history nil
          nero-future nil)
    (let ((newtab (assoc name nero-multiverse)))
      (when newtab
        (setq nero-tab name
              nero-history (nconc nero-history (second newtab))
              nero-future (nconc nero-future (third newtab)))
        (nero-restore-page (nero-current-page) t)))))

(defun nero-previous-tab ()
  "Cycles through `nero-multiverse' to make the previous tab current."
  (interactive)
  (if (eq (length nero-multiverse) 1)
      (message "No previous tab.")
    (let ((current-tab (member-if 
                        (lambda (elt) (equal (car elt) nero-tab)) 
                        nero-multiverse)))
      (if (eq (length current-tab) (length nero-multiverse))
          (nero-get-tab (caar (last nero-multiverse)))
        (nero-get-tab (caar (last (butlast nero-multiverse
                                           (length current-tab)))))))))

(defun nero-next-tab ()
  "Cycles through `nero-multiverse' to make the next tab current."
  (interactive)
  (if (eq (length nero-multiverse) 1)
      (message "No next tab.")
    (let ((current-tab (member-if 
                        (lambda (elt) (equal (car elt) nero-tab)) 
                        nero-multiverse)))
      (if (eq (length current-tab) 1)
          (nero-get-tab (caar nero-multiverse))
        (nero-get-tab (caadr current-tab))))))

(defun nero-reload ()
  "Reload the current url.
This is useful if you suspect its contents might have changed.
Uses whatever properties the page was loaded with before; see
`nero-browse-url' for details."
  (interactive)
  (when (apply #'nero-browse-url 
               (nero-current-url)
               ;; make sure the page is revised.
               (append (fifth (nero-current-page)) (list t)))))

(defun nero-reload-vanilla ()
  "Reload the current url.
This is useful if you suspect its contents might have changed.
Unlike `nero-reload', this version does not use known properties."
  (interactive)
  (when (apply #'nero-browse-url 
               (nero-current-url)
               ;; make sure the page is revised.
               (list nil nil nil nil nil t))))

(defun nero-restore-page (page update-timescape)
  "Utility to display a PAGE that has been saved in nero's history.
If UPDATE-TIMESCAPE is non-nil, run `nero-update-timescape'."
  (restore-nero-environment)
  (switch-to-buffer (get-buffer-create nero-Nero-Refs-buffer))
  (delete-region (point-min) (point-max))
  (insert (third page))
  (switch-to-buffer (get-buffer-create nero-Nero-buffer))
  (delete-region (point-min) (point-max))
  (insert (second page))
  (goto-char (fourth page))
  (when update-timescape
      (nero-update-timescape (first page) (point) (fifth page)))
  (when nero-long-thread
    (setq nero-ariadnes-thread
          (cons (first page) nero-ariadnes-thread))))

(defun nero-back ()
  "Display the previous page you visited."
  (interactive)
  (if (eq (length nero-history) 1)
      (message "Already at beginning of history.")
    (setq nero-future (nconc nero-future (last nero-history))
          nero-history (nbutlast nero-history))
    (nero-restore-page (nero-current-page) nil)))

(defun nero-back-to-beginning ()
  "Display the first page you visited.
Runs `nero-mark-page' first."
  (interactive)
  (nero-mark-page)
  (while (> (length nero-history) 1)
    (nero-back)))

(defun nero-forward ()
  "Display the next page you visited."
  (interactive)
  (if (not (cdr nero-future))
      (message "Already at end of future.")
    (setq nero-history (nconc nero-history (last nero-future))
          nero-future (nbutlast nero-future))
    (nero-restore-page (nero-current-page) nil)))

(defun nero-forward-to-end ()
  "Display the final page you visited.
Runs `nero-mark-page' first."
  (interactive)
  (nero-mark-page)
  (while (> (length nero-future) 2)
    (nero-forward)))

(defun nero-down (&optional handler action ephemeral)
  "Use the next link spotted in this buffer.
How we actually make use of this link depends on HANDLER.
See `nero-follow-link-internal'."
  (while (and (looking-at "[0-9]*\\]")
              (not (bobp)))
    (backward-char 1))
  (when (search-forward-regexp nero-link-regexp nil t)
    (when nero-links-display
        (backward-char 1))
    (nero-follow-link-internal 
     (match-string-no-properties 1) 
     handler
     action
     ephemeral)))

(defun nero-follow-link-internal (number &optional 
                                         handler action ephem)
  "Read in the NUMBER of a link and display the page it leads to.
If HANDLER is equal to 'wget, then download; if it is equal to
'copy-link, copy the url accessed by link to the kill ring.
Otherwise, browse the page that link links to as usual.  ACTION
and EPHEM are passed to `nero-browse-url' as the arguments of the
same names. See its docstring for details."
  (let ((buf (buffer-name (current-buffer))))
    (save-excursion
      (set-buffer (get-buffer-create nero-Nero-Refs-buffer))
      (goto-char (point-min))
      (when (search-forward-regexp
             (concat " +" number "\\. \\([^ \n]*\\)") nil t)
        (let ((url (match-string-no-properties 1))
              (beg (match-beginning 1))
              (end (match-end 1)))
          (cond
           ((equal buf nero-Nero-Tabs-buffer)
            (nero-restore-tab))
           ((equal buf nero-Nero-Matches-buffer)
            (nero-restore-match url))
           ((eq handler 'wget)
            (nero-download-url url))
           ((eq handler 'copy-link)
            (kill-ring-save beg end))
           ((eq handler 'return-link)
            (buffer-substring-no-properties beg end))
           (t (nero-browse-url url nil nil nil action ephem))))))))

(defun nero-restore-tab ()
  "Runs when we follow a link in the  *Nero Tabs* buffer.
Called by `nero-follow-link-internal'."
  (let ((tab (buffer-substring (1+ (point)) (line-end-position))))
    (nero-get-tab tab)))

;; makes use of invisible text that indicates where to move the point
;; to, see `nero-mark-up-references-for-browsing'.
(defun nero-restore-match (url)
  "Runs when we follow a link in the  *Nero Matches* buffer.
Called by `nero-follow-link-internal'."
  (let ((pt (buffer-substring (save-excursion
                                (goto-char (line-end-position))
                                (backward-word 1)
                                (point)) 
                              (line-end-position))))
    (nero-restore-page (nero-page-from-url url) t)
    (goto-char (string-to-int pt))))

(defun nero-page-from-url (url)
  "Given a URL, return the page browsing that url, if any."
  (let ((mult nero-multiverse)
        (ret nil))
    (while mult
      (let ((tab (cadar mult)))
        (while tab
          (when (equal (caar tab) url)
            (setq ret (car tab)))
          (setq tab (cdr tab)))
        (setq mult (cdr mult))))
    ret))

(defun nero-download-link (&optional number)
  "Like `nero-follow-link', but the page is saved, not displayed."
  (interactive "P")
  (if number
      (nero-follow-link-internal
       (int-to-string (prefix-numeric-value number))
       'wget)
    (nero-down 'wget)))

(defun nero-kill-ring-save-current-url ()
  "Copy the current url to the kill ring.
If `x-select-enable-clipboard' is non-nil and you are running
Emacs under X, this makes it easy to paste the url into other
programs."
  (interactive)
  (with-temp-buffer
    (insert (nero-current-url))
    (kill-ring-save (point-min) (point-max)))
  (message "Current URL copied to kill-ring."))

(defun nero-kill-ring-save-current-link ()
  "Copy the current link to the kill ring.
If `x-select-enable-clipboard' is non-nil and you are running
Emacs under X, this makes it easy to paste the url into other
programs."
  (interactive)
  (save-excursion (nero-down 'copy-link))
  (message "URL of current link copied to kill-ring."))

(defun nero-toggle-images ()
  "Toggle display of images in nero."
  (interactive)
  (setq nero-images (not nero-images)))

(defun nero-toggle-decoding ()
  "Toggle parsing of unicode characters."
  (interactive)
  (setq nero-decode-unicode (not nero-decode-unicode)))

(defun nero-toggle-links-display ()
  "Toggle whether or not links are visible in the current page.
Even if the links are not visible, you can still follow them
using `nero-follow-link', and they will still be copied to
another buffer if you select and copy text that contains them.
See `nero-toggle-links-exist' for a \"firmer\" function that
accomplishes something similar."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq nero-links-display (not nero-links-display))
    ;; what to do now?
    (if nero-links-display
        ;; show links
        (remove-text-properties (point-min) (point-max)
                                '(invisible t
                                  intangible t))
      ;; hide links
      (while (re-search-forward nero-link-regexp nil t)
        (set-text-properties (match-beginning 0)
                             (match-end 0)
                             '(invisible t
                               intangible t))))))

(defun nero-toggle-links-exist ()
  "Toggle whether or not links exist in the current page.
If there are links (as is typically the case), they will be
deleted.  But if there are no links (as will be the case after
running `nero-toggle-links-exist' an odd number of times) running
the function again will cause the links to be automagicially
restored.  See `nero-toggle-links-display' for a \"softer\"
function that accomplishes something similar."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq nero-links-exist (not nero-links-exist))
    ;; what to do?
    (if nero-links-exist
        ;; replace the links we saved
        (let ((offset 0))
          (while nero-links-sanctuary
            (goto-char (+ (first (car nero-links-sanctuary)) offset))
            (let ((str (second (car nero-links-sanctuary))))
              (insert str)
              (setq offset (+ offset (length str))))
            (setq nero-links-sanctuary (cdr nero-links-sanctuary))))
      ;; delete the links
      (while (re-search-forward nero-link-regexp nil t)
        (let ((match (match-string 0)))
          (replace-match "")
          (setq nero-links-sanctuary
                (append nero-links-sanctuary
                        (list (list (point) match)))
                nero-links-exist nil))))))

(defun nero-set-width (width)
  "Make the width of text in nero equal to WIDTH.
Interactively, WIDTH is a numerical prefix argument."
  (interactive "nNew width: ")
  (setq nero-width width)
  (setq nero-width-flag (replace-regexp-in-string "[0-9]+"
                                                  (int-to-string width)
                                                  nero-width-flag))
  (nero-reload))

(defun nero-set-width-temporarily (width)
  "Reload the current page being with width equal to WIDTH.
Interactively, WIDTH is a numerical prefix argument."
  (interactive "nNew width: ")
  (let ((nero-width-flag
         (replace-regexp-in-string "[0-9]+"
                                   (int-to-string width)
                                   nero-width-flag)))
    (nero-reload)))

(defun nero-follow-link (&optional number handler action ephem)
  "By default, opens the page linked to by the next link in the buffer.
See `nero-move-to-next-link' for the definition of \"next link\".
Interactively, if a numerical argument is specified, open the
page linked to by the link bearing that NUMBER.  HANDLER should
be one of the values handled by `nero-follow-link-internal'.
ACTION and EPHEM are like the arguments to `nero-browse-url' with
the same name; they are passed to that function eventually."
  (interactive "P")
  (if number
      (nero-follow-link-internal 
       (int-to-string (prefix-numeric-value number))
       handler
       action
       ephem)
    (nero-down 
     handler
     action
     ephem)))

(defun nero-follow-link-new-tab (&optional number)
  "Like `nero-follow-link' but opens the page in a new tab.
The tab is created with the default name."
  (interactive "P")
  (let ((url (if number
                 (nero-nth-link (prefix-numeric-value number))
               (nero-current-link))))
    (nero-new-tab nil url)))

(defun nero-move-to-next-link ()
  "Position the cursor on the next link that appears in the buffer.
The \"next link\" is any link such that the cursor is not past
the the right brace that denotes the link's end, and such that no
right brace of any other link intervenes."
  (interactive)
  (unless nero-links-display
    (goto-char (next-property-change (point))))
  (if (search-forward-regexp nero-link-regexp nil t)
      (backward-char 1)
    (message "No more links in buffer.")))

(defun nero-move-to-previous-link ()
  "Position the cursor on the previous link that appears in the buffer.
The \"previous link\" is any link such that the cursor is not
before the the right brace that denotes the link's end, and such
that no right brace of any other link intervenes."
  (interactive)
  (unless nero-links-display
    (goto-char (previous-property-change (point))))
  (search-backward-regexp nero-link-regexp nil t)
  (forward-char 1)
  (while (looking-at "[0-9]")
    (forward-char 1)))

(defun nero-home ()
  "Open `nero-homepage' with `nero-browse-url'."
  (interactive)
  (nero-browse-url nero-homepage))

(defun nero-bookmarks ()
  "Open the `nero-bookmarks' file with `nero-browse-url'."
  (interactive)
  (nero-browse-url nero-bookmarks))

(defun nero-add-bookmark-url (name)
  "Add a bookmark for the current page to the `nero-bookmarks' file.
NAME is a user-specified string that says what to call the bookmark."
  (interactive "MBookmark name: ")
  (with-temp-buffer
    (insert "<LI><a href=\"" (nero-current-url) "\">" name "</a>\n")
    (write-region (point-min) (point-max) nero-bookmarks t)))

(defun nero-add-bookmark-link (name)
  "Add a bookmark for the current link to the `nero-bookmarks' file.
NAME is a user-specified string that says what to call the bookmark."
  (interactive "MBookmark name: ")
  (save-excursion
    (let ((link (nero-current-link)))
      (with-temp-buffer
        (insert "<LI><a href=\"" link "\">" name "</a>\n")
        (write-region (point-min) (point-max) nero-bookmarks t)))))

(defun nero-add-bookmark-arbitrary-url (name URL)
  "Add a bookmark for the current page to the `nero-bookmarks' file.
NAME is a user-specified string that says what to call the bookmark."
  (interactive "MBookmark name: \nMURL: ")
  (with-temp-buffer
    (insert "<LI><a href=\"" URL "\">" name "</a>\n")
    (write-region (point-min) (point-max) nero-bookmarks t)))

(defun nero-inspect-current-url ()
  "Show the current url in the echo area."
  (interactive)
  (message (concat "Currently viewing: " (nero-current-url))))

(defun nero-inspect-current-link ()
  "Show the current link in the echo area."
  (interactive)
    (save-excursion
      (message (concat "Current link: " (nero-down 'return-link)))))

;; everywhere *else* we may as well call `nero-current-url' - makes
;; the code easier to read.
(defun nero-current-url ()
  "Return the current url (if any)."
  (caar (last nero-history)))

(defun nero-current-link ()
  "Return the current link (if any)."
  (nero-down 'return-link))

(defun nero-nth-link (n)
  "Return the link numbered N (if any)."
  (nero-follow-link-internal (int-to-string n) 'return-link))

(defun nero-current-tab ()
  "Return the current tab (if any)."
  nero-tab)

(defun nero-current-page ()
  "Return the current page (if any)."
  (car (last nero-history)))

(defun nero-previous-page ()
  "Return the previous page (if any)."
  (car (last nero-history 2)))

(defun nero-hide ()
  "Restore the window configuration that was active before nero ran."
  (interactive)
  (set-window-configuration nero-old-layout))

(defun nero-finished (&optional yes!)
  "Quit this nero session."
  (interactive (list (y-or-n-p "Are you sure you want to quit? ")))
  (if (not yes!)
      (message "Excellent!!!")
    (run-hooks 'nero-exiting-hook)
    (kill-buffer nero-Nero-buffer)))

(defun nero-wind-down ()
  "Kill nero's associated buffers, restoring all windows and variables."
  (when (equal (buffer-name) nero-Nero-buffer)
    (when (buffer-live-p (get-buffer nero-Nero-Refs-buffer))
      (kill-buffer nero-Nero-Refs-buffer))
    (when (buffer-live-p (get-buffer nero-Nero-Tabs-buffer))
      (set-buffer nero-Nero-Tabs-buffer)
      (fundamental-mode)
      (kill-buffer nero-Nero-Tabs-buffer))
    (setq nero-buffer-in-nero-mode nil)
    (nero-scorched-earth)))

(defun ungooglify-url (url)
  "Google may doctor up a URL before handing it to you.
This function corrects for this unseemly behavior."
   (replace-regexp-in-string "&[^+]*?$" ""
    (replace-regexp-in-string "\\(^.*?q=\\)" "" url)))

(defun ungooglify-references ()
  "Ungooglify non-google URLs in the  *Nero Refs* buffer."
  (save-excursion 
    (set-buffer (get-buffer-create nero-Nero-Refs-buffer))
    (goto-char (point-min))
    (while (re-search-forward 
            "http://www.google.com/url\\?sa=U&start=.*$" nil t)
      (let ((match (match-string 0)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert (ungooglify-url match))))
    (setcar (cddr (nero-current-page)) (buffer-string))))

;; Maybe should be addjusted to support things like
;; `nero-query-mapquest'.
(defmacro nero-defelvis (name baseurl separator
                              &optional trailing run-after-load)
  "Create an elvis to use with nero.
Elvi were made popular by the Shell Users' Revolutionary Front
Rage Against The Web.  They are functions for interacting with
useful web services through a simple non-web interface.  This
macro implements the same basic strategy, this time for
Emacs/Nero users.

NAME is how you might refer to the elvis in prose. BASEURL is the
beginning part of the URL for the service.  SEPARATOR is the
symbol to substitute for spaces in the final url we build from
the arguments given to the function defined by `nero-defelvis'.
that implements the elvis.  TRAILING, if present, plays a role
similar to that of BASEURL, but at the end of the built-up
string.  The names of elvi defined using this function are
automatically added to the list `nero-elvi'.

Optional argument RUN-AFTER-LOAD is a function to call after 
the main body of the elvis runs.  If `nero-marking-elvi' is
non-nil, the final thing that happens is that a mark is set."
  (declare (indent defun))
  `(add-to-list 'nero-elvi
                (cons ,name
                      (defun ,(intern
                               (concat "nero-query-"
                                       (downcase
                                        (replace-regexp-in-string
                                         " " "-" name))))
                        (search-string)
                        ,(concat "The " name " elvis for nero.
SEARCH-STRING will be submitted to the service and you will be
shown the results, just as if you were using a \"normal\" web
browser.")
                        (interactive "MSearch string: ")
                        (when ,run-after-load
                          (add-hook 'nero-after-loading-internal-hook 
                                    ,run-after-load))
                        (when nero-marking-elvi
                           (add-hook 'nero-after-loading-internal-hook
                                     'nero-mark-page))
                        (nero-browse-url
                         (concat ,baseurl
                                 (replace-regexp-in-string
                                  " " "+" search-string)
                                 (if ,trailing
                                     ,trailing
                                   "")))))))

(nero-defelvis "Google"
  "http://www.google.com/search?hl=en&ie=ISO-8859-1&q="
  "+"
  "&btnG=Google+Search"
  #'ungooglify-references)

(nero-defelvis "Free Software Directory"
  "http://directory.fsf.org/search/fsd-search.py?q="
  "+")

(nero-defelvis "Help GNU Emacs"
  "http://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
  "+"
  "&submit=Search%21&idxname=help-gnu-emacs&max=20&result=normal&sort=score")

(nero-defelvis "GNU Emacs Sources"
  "http://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
  "+"
  "&submit=Search%21&idxname=gnu-emacs-sources&max=20&result=normal&sort=score")

(nero-defelvis "Emacs Devel"
  "http://lists.gnu.org/archive/cgi-bin/namazu.cgi?query="
  "+"
  "&submit=Search%21&idxname=emacs-devel&max=20&result=normal&sort=score")

(nero-defelvis "EmacsWiki"
  "http://www.emacswiki.org/cgi-bin/wiki?search="
  "+"
  "&lang=&dosearch=Go%21")

;; an oddmuse wiki 
(nero-defelvis "AsteroidMeta"
  "http://oddwiki.taoriver.net/wiki.pl/AsteroidMeta?search="
  "+"
  "&dosearch=Go%21")

(nero-defelvis "PlanetMath"
  "http://planetmath.org/?op=search&term="
  "+")

(nero-defelvis "Slashdot"
  "http://slashdot.org/search.pl?query="
  "+")

(nero-defelvis "Math Biographies"
  "http://www-history.mcs.st-andrews.ac.uk/Search/historysearch.cgi?SUGGESTION="
  "+")

;; actually a google search, or was last time I checked.
(nero-defelvis "Wikipedia"
  "http://www.google.com/search?domains=en.wikipedia.org&num=50&ie=iso-8859-1&oe=iso-8859-1&q="
  "+"
  "&btnG=Google+Search&sitesearch=en.wikipedia.org"
  #'ungooglify-references)

;; Note that the reason I'm using xv is that my emacs isn't built with
;; gif support.
(defun nero-query-mapquest (&optional address city state zipcode)
  "Download and view a map to ADDRESS from mapquest.com.
CITY and STATE default to `nero-home-city' and `nero-home-state'.
ZIPCODE is usually not necessary.  To call this function
without all the prompting, use `nero-query-mapquest-defaults'."
  (interactive (list (read-string "Address: " nil nil "")
                     (read-string (concat "City (default: " 
                                          nero-home-city "): ")
                                  nil
                                  nil
                                  nero-home-city)
                     (read-string (concat "State (default: " 
                                          nero-home-state "): ")
                                  nil
                                  nil
                                  nero-home-state)
                     (read-string (concat "Zip code: ") nil nil "")))
  (let ((nero-images t)
        (url 
         (concat 
          "http://www.mapquest.com/maps/map.adp?searchtype"
          "=address&country=US&addtohistory=&searchtab=home&address="
          (replace-regexp-in-string
           " " "+" address)
          "&city="
          (replace-regexp-in-string
           " " "+" city)
          "&state="
          state
          "&zipcode="
          zipcode)))
    (nero-browse-url
     url
     nil 
     nil
     nil
     (lambda ()
       (let ((nero-images t))
         (cond 
          ((search-forward-regexp "mqmapgend" nil t)
           (nero-display-mapquest-map t))
          ((search-forward-regexp "Get map of " nil t)
           (nero-move-to-previous-link)
           (nero-follow-link
            nil
            nil
            #'nero-display-mapquest-map
            t))
          (t
           "No map found."))))
     t)))

(defun nero-display-mapquest-map (&optional found)
  "Subroutine of `nero-query-mapquest' that displays the map."
  (unless found
    (search-forward-regexp "mqmapgend" nil t))
  (nero-move-to-previous-link)
  (add-hook 'nero-after-download-internal-hook
            (lambda () (start-process "show-map" 
                                      nil
                                      "xv"
                                      ".map.gif")))
  (add-hook 'nero-after-download-internal-hook
            'nero-revise-or-update-timescape)
  (nero-download-url (nero-current-link) ".map.gif"))

(defun nero-query-mapquest-local (&optional address)
  "View a map to ADDRESS in `nero-home-city'/`nero-home-state'.
See `nero-query-mapquest'."
  (interactive "MAddress: ")
  (nero-query-mapquest address nero-home-city nero-home-state))


(add-to-list 'nero-elvi (cons "Mapquest local" 
                              'nero-query-mapquest-local))

(defun nero-query (elvis search-string)
  "Use any defined nero ELVIS, selected via completion over names.
SEARCH-STRING is the query to submit."
  (interactive (list
                 (cdr (assoc
                      (let* ((completion-ignore-case t)
                             (el (completing-read "Elvis: "
                                                 nero-elvi)))
                        (if (equal el "")
                            nero-default-elvis
                          el))
                      nero-elvi))
                (read-string
                 "Search string: "
                 nil
                 nil
                 nil)))
  (funcall elvis search-string))

(defun nero-query-default-elvis (search-string)
    "Submit SEARCH-STRING to `nero-default-elvis'."
  (interactive (list (read-string
                      "Search string: "
                      nil
                      nil
                      nil)))
  (funcall (cdr (assoc nero-default-elvis nero-elvi))
           search-string))

(defun nero-follow-numbers (lis handler)
  "Used by `nero-defjump' to add a nested list of actions.
Each action corresponds to following a given numbered link in LIS.
The last link in the list is followed using HANDLER."
  (let (ret)
    (nero-recursively-add-number-actions (reverse lis) handler ret)))

(defun nero-recursively-add-number-actions (lis handler ret)
  "Build a nested list of actions for `nero-follow-numbers'."
  (when lis
    (setq ret (nero-recursively-add-number-actions
               (cdr lis)
               handler
               (if ret
                   `(lambda () 
                      (nero-follow-link
                       ,(car lis)
                       nil
                       ,ret
                       t))
                 `(lambda () 
                    (nero-follow-link
                     ,(car lis)
                     ,handler))))))
  ret)

(defmacro nero-defjump (name baseurl 
                             &optional handler run-after-load 
                             &rest numbers)
  "Create a jump to be used with nero.
Nero jumps are like bookmarks or \"webjumps\", but they can
do a bit more.  The most basic nero jump is essentially just
a bookmark.

The reason you might want more is that sometimes urls change, you
know a consistent way to find them, starting from some
\"landmark\".  You don't want to redefine your bookmark every
time the url changes, but you don't want to physically go through
the process of browsing to the final url either.  What to do?
This macro takes the approach of having nero do the browsing for
you.

NAME is how you might refer to this jump in prose.  BASEURL
is the URL to start from. NUMBERS, if present, indicate the
numbered links to follow, in order.

A function called \"nero-jump-to-\" NAME (suitably marked up)
will be created when this macro runs.

HANDLER can be used to specify alternative instructions on how to
load the final page accessed by the jump.  For one example,
see `nero-oddmuse-recent-changes-handler'."
  (declare (indent defun))
  `(add-to-list 
    'nero-jumps
    (cons ,name 
          (defun ,(intern
                   (concat "nero-jump-to-"
                           (downcase
                            (replace-regexp-in-string
                             " " "-" name))))
            (&optional insert)
            (interactive "P")
            (if insert
                (insert ,baseurl)
              (nero-browse-url 
               ,baseurl
               nil
               ;; this is last,
               ;; use the handler 
               (unless 
                   ,(car numbers)
                 ,handler)
               nil 
               ;; the action we take once the page has loaded
               ;; (typically involves following numbered links)
               ,(nero-follow-numbers numbers handler)
               ;; the first page is not the last page; be ephemeral
               (when ,(car numbers) t)
               ;; maybe revise, otherwise, add.
               (when ,(equal (nero-current-url) baseurl) t)))))))

(nero-defjump "Joe's homepage"
  "http://www.ma.utexas.edu/~jcorneli/")

;; mainly for testing
(nero-defjump "Musical letters"
  "http://www.ma.utexas.edu/~jcorneli/"
  nil
  nil
  13
  2)

(nero-defjump "PlanetMath"
  "http://planetmath.org/")

(nero-defjump "GNU Emacs Sources"
  "http://mail.gnu.org/archive/html/gnu-emacs-sources/"
  nil
  nil
  3)

(nero-defjump "Help GNU Emacs"
  "http://mail.gnu.org/archive/html/help-gnu-emacs/"
  nil
  nil
  3)

(nero-defjump "Emacs Devel"
  "http://mail.gnu.org/archive/html/emacs-devel/"
  nil
  nil
  3)

(nero-defjump "Emacs Lisp List"
  "http://www.anc.ed.ac.uk/~stephen/emacs/ell.html")

;; I use this wiki a lot, but of course you don't have to...
(nero-defjump "AsteroidMeta"
  "http://oddwiki.taoriver.net/wiki.pl/AsteroidMeta/HomePage")

(nero-defjump "AsteroidMeta recent changes"
  "http://oddwiki.taoriver.net/wiki.pl/AsteroidMeta?action=rc;days=1;all=0;showedit=0"
  #'nero-oddmuse-recent-changes-handler)

(defun nero-oddmuse-recent-changes-handler (URL flags action)
  "Simplify the presentation of an OddMuse RecentChanges pages."
  (add-hook 'nero-before-loading-internal-hook
        (lambda ()
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward 
                    " (\\[[0-9]+\\]\\(diff\\|history\\))" 
                    nil t)
              (replace-match ""))
            (goto-char (point-min))
            (while (re-search-forward "[^0-9]\\([ ]\\.\\|\\.[ ]\\)" 
                                      nil t)
              (replace-match "" nil nil nil 1))
            (goto-char (point-min))
            (while (re-search-forward
                    "^ +\\(\\*.+UTC\\)\\( (new)\\)?" 
                    nil t)
              (replace-match "\n\\1\\2\n\n")
              (goto-char (line-end-position))
              (forward-char 1)
              (if (looking-at " +\\[")
                  (progn (delete-horizontal-space)
                         (delete-char -1))
                (delete-char -1)
                (delete-horizontal-space)
                (insert " ")
                (search-forward-regexp "\\[" nil t)
                (backward-char 1))
              (backward-char 1)
              (when (looking-at "\\.")
                (forward-char 1)
                (insert " "))
              (search-forward "-" nil t)
              (replace-match "\n Summary:"))
            (goto-char (point-min))
            (while (re-search-forward "^       "
                                      nil t)
              (replace-match "")
              (delete-char -1)
              (insert " ")))))
  (nero-generate-content URL flags nil))

(defun nero-jump (&optional arg)
  "Use any defined nero jump, selected via completion over names.
If optional ARG is non-nil, the base url for the jump will be
inserted instead."
  (interactive "P")
  (apply
   (cdr (assoc
         (let ((completion-ignore-case t))
           (completing-read "Jump to: " nero-jumps))
         nero-jumps))
   arg))

;; not useful for people who don't use `simple-wiki-edit-mode', but
;; quite useful for those who do.  You will need to load the following
;; configuration to make this function useful:
; (add-to-list 'swd-wiki-defs-list
;              `("AsteroidMeta"
;                "http://oddwiki.taoriver.net/wiki.pl/AsteroidMeta/"
;                "?action=browse&raw=2&id="
;                "?action=index&raw=1"
;                "?action=rc&raw=1"
;                1.1
;                swd-usemod-wiki-save
;                utf-8))
(defun nero-swe-asteroid-page ()
  "Example showing how to integrate nero with `simple-wiki-edit-mode'."
  (interactive)
  (if (functionp 'swc-browse)
      (swc-browse "AsteroidMeta"
                  ;; to do this really right, need lots of
                  ;; translations, I'm sure.  Maybe simple wiki edit
                  ;; provides something that does this?
                  (replace-regexp-in-string "%2c" ","
                  (replace-regexp-in-string ".*/" ""
                                            (nero-current-url))))
    (message
     "You need simple-wiki-edit (and some configs) to use this.")))

(provide 'nero)

;;; nero.el ends here
