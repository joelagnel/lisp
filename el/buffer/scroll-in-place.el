
;;;; -*-Emacs-Lisp-*- Improved Vertical Scrolling Commands
;;;; Written by Eric Eide, last modified on 1994/11/18 21:23:01.
;;;; (C) Copyright 1993, 1994, Eric Eide and the University of Utah
;;;;
;;;; COPYRIGHT NOTICE
;;;;
;;;; This program is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the Free
;;;; Software Foundation; either version 2 of the License, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;;;; for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;;;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;;; AUTHORS
;;;;
;;;; This package was written by Eric Eide (eeide@cs.utah.edu) and was based on
;;;; a very similar package ("scroll-fix") by Joe Wells.  Almost all of the
;;;; code in this file is original, but I owe a great debt to Mr. Wells for his
;;;; ideas and his original implementation.
;;;;
;;;;   Eric Eide (eeide@cs.utah.edu)
;;;;   University of Utah
;;;;   3190 Merrill Engineering Building
;;;;   Salt Lake City, Utah  84112
;;;;
;;;;   Joe Wells (jbw@cs.bu.edu)
;;;;
;;;; Joe Wells' "scroll-fix" package is Copyright (C) 1988, 1989, and 1991 by
;;;; the Free Software Foundation.  It is distributed under the terms of the
;;;; GNU General Public License.

;;;; LISP CODE DIRECTORY INFORMATION
;;;;
;;;; LCD Archive Entry:
;;;; scroll-in-place|Eric Eide|eeide@cs.utah.edu|
;;;; Improved vertical scrolling commands|
;;;; 1994/11/18 21:23:01|1.3|~/misc/scroll-in-place.el.Z|

;;;; SUMMARY
;;;;
;;;; This package provides improved vertical scrolling commands for GNU Emacs.
;;;; These new commands offer the following features:
;;;;
;;;; + When a scrolling command is executed, GNU Emacs tries to keep point as
;;;;   close as possible to its original window position (window line and
;;;;   column).  This is what "scroll in place" means: point stays "in place"
;;;;   within the window.  (There are times when point must be moved from its
;;;;   original window position in order to execute the scroll; see below.)
;;;;
;;;;   The variable `scroll-in-place', which is true by default, determines
;;;;   whether or not the standard GNU Emacs scrolling commands (`scroll-down',
;;;;   `scroll-up', `scroll-other-window-down', and `scroll-other-window') use
;;;;   the "in place" features listed here.  When `scroll-in-place' is `nil'
;;;;   the standard GNU Emacs scrolling commands essentially just call the
;;;;   original versions of themselves.  (Note that even when `scroll-in-place'
;;;;   is `nil' the new versions of `scroll-down' and `scroll-up' have slightly
;;;;   different behavior when a minibuffer window is the selected window.  See
;;;;   below.)
;;;;
;;;;   It is possible to turn off (or turn on) "in place" scrolling for certain
;;;;   buffers by making buffer-local bindings of the variable `scroll-in-
;;;;   place' for those buffers.  The variable `scroll-in-place' is not usually
;;;;   buffer-local, but you can make it so if you desire.
;;;;
;;;; + Because the improved scrolling commands keep point at its original
;;;;   window position, these scrolling commands are "reversible."  The
;;;;   `scroll-up' command undoes the effect of the immediately previous
;;;;   `scroll-down' command (if any) and vice versa.  In other words, if you
;;;;   scroll up and then immediately scroll back down, the window config-
;;;;   uration is restored to its exact original state.  This allows you to
;;;;   browse through a buffer more easily, as you can always get back to the
;;;;   original configuration.
;;;;
;;;;   Note, however, that the improved scrolling commands are guaranteed to be
;;;;   reversible only if there are no intervening non-scrolling commands.
;;;;   Also, if you give a prefix argument to a scrolling command (in order to
;;;;   specify the number of lines to scroll by), previous scrolling commands
;;;;   may no longer be reversible.  More specifically, if the new prefix
;;;;   argument has a different magnitude than the previous scrolling distance,
;;;;   then any previous scrolling commands are not reversible.  The new prefix
;;;;   argument takes precedence.
;;;;
;;;;   You might find it useful to think of the scrolling commands as forming
;;;;   "chains."  A scrolling command either starts or continues a chain.  By
;;;;   issuing a non-scrolling command or by changing the number of lines to be
;;;;   scrolled, you break the chain.  (Note that simply changing the scrolling
;;;;   direction won't break the chain; changing the absolute number of lines
;;;;   to be scrolled is what breaks the chain.)  Scrolling commands are
;;;;   guaranteed to be reversible only within the current chain.  Hopefully
;;;;   that's clear enough.
;;;;
;;;; + When a scrolling command is given a prefix argument (which specifies the
;;;;   number of lines to scroll by), then that argument becomes the default
;;;;   scrolling distance for all immediately subsequent scrolling commands.
;;;;   This means that you can easily set the scrolling distance for a chain
;;;;   of scrolling commands.  Note that a new prefix argument or any non-
;;;;   scrolling command breaks the chain (as described above), and any further
;;;;   scrolling commands will use the usual defaults (or the prefix argument
;;;;   you specify at that time, of course).
;;;;
;;;;   However, there are cases in which one doesn't want the current scrolling
;;;;   command to use the default scrolling distance that was set by the
;;;;   previous scrolling command.  For example, suppose that you had special
;;;;   commands that scrolled one line up and one line down.  When you invoke
;;;;   one of these commands, the "in place" scrolling routines set the default
;;;;   scrolling distance to be just one line.  Now suppose that you use one of
;;;;   your special commands and then immediately invoke `scroll-up' (`C-v'),
;;;;   expecting it to scroll by a near windowful of text.  You would be
;;;;   disappointed --- because the previous command set the default scrolling
;;;;   distance to be just one line, `scroll-up' just scrolls by one line.
;;;;
;;;;   To solve this problem, "scroll-in-place" allows you to divide scrolling
;;;;   commands into separate "groups."  Commands in a group can only form
;;;;   chains with (and therefore, inherit defaults from) commands in the same
;;;;   group.  (Note that no command can be in more than one group.)  If you
;;;;   invoke a scrolling command that is not in the same group as that of the
;;;;   immediately previous scrolling command, then the previous chain is
;;;;   broken and you start a new chain --- with a new set of defaults.
;;;;
;;;;   So to solve the problem described above, you could put your one-line
;;;;   scrolling commands in their own group.  Once that is done, the standard
;;;;   scrolling commands will not form chains with your one-line scrolling
;;;;   commands, and therefore will not use the default scrolling distance set
;;;;   by those commands.  Problem solved!
;;;;
;;;;   By default, all "in place" scrolling commands are in a single group.  If
;;;;   you want to partition some commands into separate groups, you must do
;;;;   that yourself *before* any "in place" commands are invoked.  For more
;;;;   information about grouping commands, see the documentation for the
;;;;   variables `scroll-command-groups' and `scroll-default-command-group'.
;;;;
;;;; + The improved scrolling commands will avoid displaying empty lines past
;;;;   the end of the buffer when possible.  In other words, just as you can't
;;;;   see "dead space" before the beginning of the buffer text, the new
;;;;   scrolling commands try to avoid displaying "dead space" past the end of
;;;;   the buffer text.  This behavior is somewhat configurable; see the
;;;;   documentation for the variable `scroll-allow-blank-lines-past-eob'.
;;;;
;;;;   Dead space will be displayed if it is necessary in order to make a
;;;;   previous scrolling action reversible, however.
;;;;
;;;; + If the scrolling commands cannot keep point at its initial window
;;;;   position (because a buffer boundary is on screen and the window can't be
;;;;   scrolled as far as necessary to keep point at the right place), point is
;;;;   allowed to temporarily stray from its initial window position.  That is,
;;;;   point moves the correct number of window lines, even if it means that it
;;;;   has to stray from its desired window position.  This straying is undone
;;;;   when (and if) the scrolling action is reversed.
;;;;
;;;; + If a scrolling command tries to move point past a buffer boundary, point
;;;;   is instead moved to the boundary (the beginning or the end of the buffer
;;;;   as appropriate) and an appropriate message is displayed.  This motion is
;;;;   reversible, of course.
;;;;
;;;;   However, if point was already at the buffer boundary when the scrolling
;;;;   command was invoked, the command signals an appropriate error instead.
;;;;
;;;; + When a minibuffer window is the selected window, the new versions of
;;;;   `scroll-up' and `scroll-down' either scroll the window in the variable
;;;;   `minibuffer-scroll-window' (which is usually the window of completions)
;;;;   or the `next-window' if there is no `minibuffer-scroll-window'.  This is
;;;;   usually much more useful than scrolling the minibuffer itself.  (Note
;;;;   that this feature is available even when the variable `scroll-in-place'
;;;;   is `nil'.)
;;;;
;;;; + When a scrolling command is scrolling a window other than the selected
;;;;   window, it will signal an appropriate buffer boundary error if the
;;;;   window cannot be scrolled (because the appropriate buffer boundary is
;;;;   already visible).  This means that an error is signalled even in cases
;;;;   that would be allowed (by "straying" point or by moving it to the buffer
;;;;   boundary) if the window were selected.
;;;;
;;;;   (If an error were not signalled in these cases, then there would be many
;;;;   cases in which the last scroll in a particular direction would appear to
;;;;   do nothing because only the point position would change --- the
;;;;   displayed text would stay the same!  To avoid these cases the scrolling
;;;;   commands signal boundary errors "prematurely" when the window to be
;;;;   scrolled is not selected.)
;;;;
;;;; So how is this package different than Joe Wells' "scroll-fix" package?
;;;;
;;;; + This package provides "in place" behavior for the standard GNU Emacs
;;;;   commands by default; "scroll-fix" does not.
;;;;
;;;; + "scroll-fix" behaves differently when the window is near a buffer
;;;;   boundary.  Instead of allowing point to stray, "scroll-fix" first does
;;;;   an incomplete scroll (i.e., moves point less than the full distance in
;;;;   order to keep point at the desired window position) and then pops point
;;;;   to the buffer boundary.  I think that the behavior of this package is
;;;;   somewhat move intuitive, especially for small scrolling distances.
;;;;
;;;; + The scrolling commands in this package will appropriately signal buffer
;;;;   boundary errors; the commands in "scroll-fix" never signal boundary
;;;;   errors.  This makes it difficult to allow "scroll-fix" to replace the
;;;;   standard `scroll-down' and `scroll-up' commands because some other
;;;;   packages (e.g., VM and GNUS) expect the scrolling commands to signal
;;;;   these errors as necessary.
;;;;
;;;; + This package handles long lines correctly.  (But see PROBLEMS, below.)
;;;;
;;;; + "scroll-fix" handles prefix arguments differently.  In "scroll-fix", a
;;;;   number-containing prefix argument always breaks any running chain of
;;;;   scrolling commands.  The prefix argument `-' (the symbol minus,
;;;;   generated by `C-u -') causes a temporary change in direction --- a
;;;;   change for only the current command.  In this package, however, a
;;;;   number-containing prefix argument only breaks a running chain if it has
;;;;   a different magnitude than the default scrolling distance, and the
;;;;   prefix argument `-' causes a permanent change in the sign of the default
;;;;   scrolling distance --- a change visible to immediately subsequent
;;;;   scrolling commands.
;;;;
;;;; + This package keeps track of the set of "in place" scrolling commands
;;;;   dynamically, in order to detect "chains" of scrolling commands.
;;;;   "scroll-fix" has a fixed list of scrolling commands, so "scroll-fix"
;;;;   cannot keep track of some chains.  (Again, "scroll-fix" interacts badly
;;;;   with VM and GNUS.)  And because "scroll-fix" keeps a static list of
;;;;   scrolling commands, it is a bad idea to call its "in place" commands
;;;;   from a program.  This package, because it maintains the information
;;;;   dynamically, has no such problems.
;;;;
;;;; + This package allows one to divide the "in place" scrolling commands into
;;;;   groups; a command in a group only forms chains with the members of its
;;;;   group.  "scroll-fix" has no notion of command groups.
;;;;
;;;; + This package provides "in place" versions of the standard GNU Emacs
;;;;   commands `scroll-other-window-down' and `scroll-other-window'.
;;;;
;;;; + This package will refuse to scroll non-selected windows (by signalling
;;;;   an error) when the displayed text would not change, as described in the
;;;;   feature list above.
;;;;
;;;; + When a minibuffer window is selected, this package always scrolls a
;;;;   window other than the minibuffer.  "scroll-fix" will scroll another
;;;;   window only if the entire minibuffer contents are visible.
;;;;
;;;; + "scroll-fix" provides a command to toggle the "in place" behavior of the
;;;;   standard GNU Emacs commands.  This package doesn't; you'll have to set
;;;;   the option manually with the command `set-variable'.
;;;;
;;;; + This package has gratuitous variable renaming (insert smile here!):
;;;;
;;;;   "scroll-fix" user variable            Equivalent in this package
;;;;   -----------------------------------------------------------------------
;;;;   scroll-in-place                       (none)
;;;;   scroll-in-place-replace-original      scroll-in-place
;;;;   scroll-in-place-eob-blank-allowed     scroll-allow-blank-lines-past-eob
;;;;
;;;; + This package allows programmers to specify the default scrolling
;;;;   distance (i.e., the default distance used when starting a new chain of
;;;;   scrolling commands) for custom scrolling commands.

;;;; COMMANDS AND FUNCTIONS
;;;;
;;;; This package provides the following "in place" versions of GNU Emacs'
;;;; standard vertical scrolling commands:
;;;;
;;;;   scroll-down-in-place
;;;;   scroll-up-in-place
;;;;   scroll-other-window-down-in-place
;;;;   scroll-other-window-in-place
;;;;
;;;; The variable `scroll-in-place', which is true by default, determines
;;;; whether or not the new versions of the standard GNU Emacs scrolling
;;;; commands (`scroll-down', `scroll-up', `scroll-other-window-down', and
;;;; `scroll-other-window') use the "in place" features listed above.  When
;;;; `scroll-in-place' is `nil' the standard GNU Emacs scrolling commands
;;;; essentially just call the original versions of themselves.  (Note that
;;;; even when `scroll-in-place' is `nil' the new versions of `scroll-down' and
;;;; `scroll-up' have slightly different behavior when a minibuffer window is
;;;; the selected window.  See the feature list above.)
;;;;
;;;; NOTE that this package redefines the standard GNU Emacs commands `scroll-
;;;; down', `scroll-up', `scroll-other-window-down', and `scroll-other-window'
;;;; (in order to check the variable `scroll-in-place', as described above).
;;;; The command `scroll-other-window-down' first appeared as a standard
;;;; command in the FSF's GNU Emacs 19.26.
;;;;
;;;; This package also provides the following functions and variables which are
;;;; of use to programmers:
;;;;
;;;;   scroll-window
;;;;   scroll-window-in-place
;;;;   scroll-window-in-place-continue-sequence
;;;;   scroll-default-lines (variable)
;;;;   scroll-command-groups (variable)
;;;;
;;;; The `scroll-window-in-place' function is the heart of the "in place"
;;;; scrolling commands.  `scroll-window' is a function that checks the
;;;; variable `scroll-in-place' and calls the appropriate scrolling function
;;;; (either `scroll-window-in-place' or one of the original versions of
;;;; `scroll-down' and `scroll-up').  The function `scroll-window-in-place-
;;;; continue-sequence' is provided in order to preserve running "chains" of
;;;; scrolling commands as described above.
;;;;
;;;; The variable `scroll-default-lines' determines the default scrolling
;;;; distance when a new chain of "in place" scrolling commands begins.  If
;;;; this variable is not a number, then the default distance is the height of
;;;; the window to be scrolled minus `next-screen-context-lines'.  The variable
;;;; `scroll-command-groups' contains the explicit groups of "in place"
;;;; scrolling commands; for more information read the variable documentation.

;;;; YOUR .EMACS FILE
;;;;
;;;; To use this package, you simply need to load it from within your ".emacs"
;;;; file:
;;;;
;;;;   (require 'scroll-in-place)
;;;;
;;;; By default, this package provides for the standard GNU Emacs vertical
;;;; scrolling commands (`scroll-down', `scroll-up', `scroll-other-window-
;;;; down', and `scroll-other-window') to use the "in place" features.  If you
;;;; would rather not have this, set the variable `scroll-in-place' to `nil':
;;;;
;;;;   (setq scroll-in-place nil)
;;;;
;;;; When `scroll-in-place' is `nil' you will have to bind keys in order to
;;;; call the "in place" scrolling commands.  For example, you might want to do
;;;; the following:
;;;;
;;;;   (global-set-key "\M-v" 'scroll-down-in-place)
;;;;   (global-set-key "\C-v" 'scroll-up-in-place)
;;;;
;;;; Sun users should also read the PROBLEMS section, below.
;;;;
;;;; ADVANCED CUSTOMIZATION
;;;;
;;;; If you want to partition certain "in place" scrolling commands into
;;;; separate groups, you should do something like the following:
;;;;
;;;;   ;; Make one group containing the commands `scroll-down-one-line' and
;;;;   ;; `scroll-up-one-line'.  (These are not standard GNU Emacs commands.)
;;;;   (setq scroll-command-groups
;;;;         (list '(scroll-down-one-line scroll-up-one-line)))
;;;;
;;;; You could write the `scroll-down-one-line' command like this:
;;;;
;;;;   (defun scroll-down-one-line (arg)
;;;;     "Scroll down one line, or number of lines specified by prefix arg."
;;;;     (interactive "P")
;;;;     (let ((scroll-default-lines 1))
;;;;       (scroll-down-in-place arg)))
;;;;
;;;; If you want to disable "in place" scrolling for windows that display a
;;;; particular buffer (while leaving it available in other windows), you can
;;;; make `scroll-in-place' a buffer-local variable for that buffer and then
;;;; bind that local copy of `scroll-in-place' to `nil'.  This is the kind of
;;;; thing that one generally does in a major mode hook.  For example, you can
;;;; disable "in place" scrolling of GNUS article windows with the following
;;;; code:
;;;;
;;;;   (setq gnus-article-mode-hook
;;;;         (function (lambda ()
;;;;                     (make-local-variable 'scroll-in-place)
;;;;                     (setq scroll-in-place nil))))
;;;;   ;; Set the variable `gnus-Article-mode-hook' instead if you are using
;;;;   ;; an old version of GNUS, say version 3.13 or 3.14.
;;;;
;;;; The variable `scroll-allow-blank-lines-past-eob' can also be made local to
;;;; particular buffers, if you desire.  (But why would you want to do that?)

;;;; PROBLEMS
;;;;
;;;; + It is sometimes difficult for one's eyes to follow an incomplete scroll
;;;;   (i.e., a scroll in which the text doesn't move as far as one expected),
;;;;   especially when the scrolled window is not selected (and therefore that
;;;;   window's point is not highlighted).  One can lose one's place in the
;;;;   text.
;;;;
;;;; + The names `scroll-down-in-place' and `scroll-up-in-place' conflict with
;;;;   two commands in the GNU Emacs terminal-specific file "term/sun.el".
;;;;   This means that in order to load this package correctly, Sunterm users
;;;;   will have to use the hook `term-setup-hook'.  For example, you might put
;;;;   the following form in your ".emacs" file:
;;;;
;;;;   (setq term-setup-hook (function (lambda () (require 'scroll-in-place))))
;;;;
;;;;   If this is confusing, get help from your local GNU Emacs guru.
;;;;
;;;; + `scroll-determine-goal-column' tries to honor the variable `track-eol'
;;;;   if it is set.  But when lines are being wrapped we can't move point past
;;;;   the wrap --- or else it is possible that scrolling won't work correctly.
;;;;   In short, this package honors `track-eol' as best it can.
;;;;
;;;; + `scroll-window-in-place' can become confused when something changes the
;;;;   window "out from under it."  By "confused" I mean that it is possible
;;;;   for `scroll-window-in-place' to think that it should continue the
;;;;   running sequence of "in place" scrolls when it should really probably
;;;;   start a new sequence.  For example, if a process filter inserts text
;;;;   into the buffer and moves point, `scroll-window-in-place' loses track of
;;;;   where point should be and where the window should start.  Commands that
;;;;   call a "scroll in place" function and then subsequently move point can
;;;;   also confuse `scroll-window-in-place'.
;;;;
;;;;   To correct some of this confusion, `scroll-window-in-place' could keep
;;;;   track of the final positions of `window-start' and `window-point',
;;;;   possibly with both markers and character positions.  In my experience
;;;;   the "in place" scrolling commands are almost never confused (except by
;;;;   fancy packages that do their own fancy kinds of scrolling, as described
;;;;   below), so the extra sanity checking isn't worth the effort.  If your
;;;;   mileage varies let me know.
;;;;
;;;; + The "in place" scrolling commands can interact poorly with packages that
;;;;   provide their own special scrolling commands.  For example, there are
;;;;   varying degrees of conflict with Rmail, VM, and GNUS.
;;;;
;;;;   RMAIL
;;;;
;;;;   In the version of Rmail that is part of the FSF's GNU Emacs 19 (19.25
;;;;   through 19.28 at least), the command `rmail-summary-scroll-msg-down' in
;;;;   the file "rmailsum.el" fails to work properly when "in place" scrolling
;;;;   is enabled for the Rmail message buffer.  (The source of the conflict:
;;;;   the "in place" scrolling commands and Emacs' standard scrolling commands
;;;;   interpret the argument '- in different ways.)  Fortunately it is easy to
;;;;   patch Rmail.  Send me mail if you would like to receive a copy of these
;;;;   patches.
;;;;
;;;;   I know of no conflicts between the "in place" scrolling commands and
;;;;   older versions of Rmail (i.e., versions that came with GNU Emacs 18).
;;;;
;;;;   VM
;;;;
;;;;   `scroll-window-in-place' is *very* confused by VM 5's message scrolling
;;;;   commands, especially because VM 5 rebuilds Emacs' window configuration
;;;;   from scratch so often.  I have written an experimental set of patches
;;;;   for VM 5.70 that allows VM 5 to use the "scroll-in-place" features; send
;;;;   me mail if you would like to receive a copy of these patches.  I hope
;;;;   that someday my patches will be incorporated into VM.
;;;;
;;;;   `scroll-window-in-place' is not confused by VM 4.41's message scrolling
;;;;   commands, however.
;;;;
;;;;   GNUS
;;;;
;;;;   `scroll-window-in-place' can be *slightly* confused by GNUS' article
;;;;   scrolling commands because they move point to the last line of the
;;;;   article window and then scroll the text.  (This is the case for at least
;;;;   GNUS versions 3.13 through 4.1, inclusive.)  The potential conflict is
;;;;   so minor, however, that you'll probably never notice it.  I never do.
;;;;
;;;;   A severe conflict, however, exists between the "in place" scrolling
;;;;   commands and the add-on "gnus-hide" package.  "gnus-hide" can elide
;;;;   signatures at the ends of articles but it does so in a way that causes
;;;;   `scroll-window-in-place', as invoked by the GNUS scrolling commands, not
;;;;   to signal end-of-buffer conditions at the right times.  Someday I may
;;;;   write new article scrolling commands for GNUS.
;;;;
;;;; + Process filters that call scrolling functions can cause confusion.  They
;;;;   may break running chains of "in place" scrolling commands and they may
;;;;   set up inappropriate defaults for future scrolling commands.  Maybe this
;;;;   is a moot problem, as I am currently unaware of any process filters that
;;;;   invoke scrolling commands (although many filters move point around,
;;;;   which will also confuse `scroll-window-in-place').

;; (provide 'scroll-in-place) at the end of this file.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the variable declarations, both user options and internal
;;;; variables.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar scroll-in-place t
  "*When this variable is true (i.e., non-`nil'), the standard GNU Emacs
vertical scrolling commands `scroll-down', `scroll-up', `scroll-other-window-
down', and `scroll-other-window' will attempt to keep point at its current
position in the window (window line and column).  In other words, point stays
\"in place\" within the window.

When this variable is `nil' the standard GNU Emacs vertical scrolling commands
behave as usual.  The \"in place\" equivalents, however, are still available as
separate commands.

This variable may be made buffer-local in order to disable (or enable) \"in
place\" scrolling in particular buffers."
  ;; I have thought about dividing `scroll-in-place' into three variables: a
  ;; list of commands that always scroll in place, a list of commands that
  ;; never scroll in place, and a flag that determines the default behavior of
  ;; other scrolling commands.  This could make it easier to make "in place"
  ;; scrolling the default because one could single out certain ill-behaved
  ;; commands.  But as of now I'm sure that the added complexity would really
  ;; be worth it.
  )

(defvar scroll-allow-blank-lines-past-eob nil
  "*When this variable is `nil' the \"in place\" scrolling commands will avoid
displaying empty lines past the end of the buffer text.  In other words, just
as you can't see \"dead space\" before the beginning of the buffer text, the
\"in place\" scrolling commands try to avoid displaying \"dead space\" past the
end of the buffer text.  This helps make the most of window real estate.

Note that sometimes it is necessary to display \"dead space\" in order to make
a previous scrolling action reversible.

When this variable is non-`nil' the \"in place\" scrolling commands will always
allow blank lines to be shown past the end of the buffer.")

;;;;
;;;; The following variables are not user options, but are intended to be set
;;;; by code outside this package.
;;;;

(defvar scroll-default-lines nil
  "The default number of lines to be scrolled by when a new sequence of \"in
place\" scrolling commands begins.  Of course, when an explicit number of lines
is specified, that explicit number takes precedence.  See the documentation for
the function `scroll-window-in-place' for more information.

If this variable is not bound to a number, then the default number of lines is
the height of the window to be scrolled minus `next-screen-context-lines'.

This variable should not be set globally!  Commands that want to specify a
default scrolling distance should just bind the variable `scroll-default-lines'
temporarily.")

(defvar scroll-command-groups nil
  "The explicitly specified \"groups\" of \"in place\" scrolling commands.
This variable should be set before or immediately after the \"in place\"
scrolling package is loaded, and then not changed after that.

Usually, \"in place\" scrolling commands share state (e.g., the number of lines
to scroll by) with any and all immediately previous \"in place\" scrolling
commands.  Sometimes, however, this is undesirable.  In these cases the \"in
place\" scrolling commands can be divided into groups.  A command in a group
only shares state with members of its group.

Each element of `scroll-command-groups' is a list that contains all of the
members of a unique command group.  For example, if there were only one
explicit group and that group contained the commands `scroll-down-one-line' and
`scroll-up-one-line', then `scroll-command-groups' would be set to:

  ((scroll-down-one-line scroll-up-one-line))

Commands that are not in any explicitly specified group are added to a default
group.  That group is stored in the variable `scroll-default-command-group'.

The \"in place\" scrolling functions assume that all of the scrolling command
groups are nonintersecting (i.e., no command is in more than one group) and
only contain \"in place\" scrolling commands.")

;;;;
;;;; The variables below this point are internal to this package.
;;;;

(defvar scroll-default-command-group nil
  "The set of \"in place\" scrolling commands that are not members of any
explicitly defined group of commands.  This set of commands is an implicitly
defined group, constructed as \"in place\" commands are invoked, and members of
this group share state among themselves.  See the documentation for the
variable `scroll-command-groups' for more information.")

(defvar scroll-initially-displayed-lines 0
  "The number of window lines that contained buffer text when the current
sequence of \"in place\" scrolling commands started.  Unless the variable
`scroll-in-place-allow-blank-lines-past-eob' is true, the \"in place\"
scrolling commands ensure that at least this many text lines are visible at all
times.")

(defvar scroll-previous-window nil
  "The window that was most recently scrolled by an \"in place\" scrolling
command.")

(defvar scroll-previous-lines 0
  "The number of window lines that the previous \"in place\" scrolling command
attempted to scroll.")

(defvar scroll-goal-column 0
  "The desired horizontal window position for point, used by the \"in place\"
scrolling commands.")

(defvar scroll-boundary-previous-point nil
  "The value of point before point was moved to a buffer boundary.")

(defvar scroll-boundary-previous-lines 0
  "The number of lines that point moved when it moved to a buffer boundary.")

(defvar scroll-boundary-error-command nil
  "The value of `this-command' when an \"in place\" scrolling command signalled
a buffer boundary error.  This is used to decide how subsequent scrolling
commands should recover from the error.")

(defvar scroll-boundary-error-point nil
  "The value of point when an \"in place\" scrolling command signalled a buffer
boundary error.  This is used to decide how subsequent scrolling commands
should recover from the error."
  ;; This variable is used as a flag, indicating whether or not the previous
  ;; "in place" scrolling command signalled an error.
  )

(defvar scroll-window-debt 0
  "The difference between the number of lines an \"in place\" scrolling command
tried to scroll a window and the number of lines that the window actually
scrolled.  This difference is the \"debt\" in the window's starting position.
Subsequent \"in place\" scrolling commands try to make up this debt.")

(defconst scroll-pos-visible-bug-p
  ;; On September 14, 1993, David Hughes <djh@Harston.CV.COM> told me that
  ;; Lucid GNU Emacs 19.8 had inherited the bug from Epoch... sigh.
  (let ((old-match-data (match-data)))
    (unwind-protect
	(or (and (boundp 'epoch::version)
		 (if (string-match "\\`4\\." emacs-version) t nil)
		 )
	    (and (string-match "Lucid" emacs-version)
		 (if (string-match "\\`19\\.8\\." emacs-version) t nil)
		 )
	    )
      (store-match-data old-match-data)))
  "A flag, set when this version of GNU Emacs has a buggy version of the
function `pos-visible-in-window-p' that returns `nil' when given `(point-max)'
and `(point-max)' is on the last line of the window.  Currently, this flag is
set for all versions of Epoch 4 and for Lucid GNU Emacs 19.8.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the window-choosing auxiliary functions used by the new scrolling
;;;; commands.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scroll-choose-window ()
  "Choose the window to be scrolled by the commands `scroll-down', `scroll-up',
`scroll-down-in-place', and `scroll-up-in-place'.

The rules are simple.  If the selected window is not a minibuffer window, then
just choose the selected window.

However, when a minibuffer window is selected, look first for the `minibuffer-
scroll-window'.  The `minibuffer-scroll-window' is usually the window that
displays completions.  If it exists, choose it; otherwise choose the next
window after the selected window in the canonical ordering of windows.  The
next window is generally the one below the selected window, or the one at the
top of the screen if the selected window is at the bottom of the screen."
  (let ((selected-window (selected-window)))
    (if (if (fboundp 'window-minibuffer-p)
	    ;; The v19 idiom.
	    (window-minibuffer-p selected-window)
	  ;; The v18 idiom.
	  (eq selected-window (minibuffer-window)))
	;; A minibuffer window is selected --- scroll some other window.
	(if (and (windowp minibuffer-scroll-window)
		 (if (fboundp 'window-live-p)
		     ;; The v19 idiom.
		     (window-live-p minibuffer-scroll-window)
		   ;; The v18 idiom: `window-point' is `nil' if the window has
		   ;; been deleted.
		   (window-point minibuffer-scroll-window))
		 )
	    minibuffer-scroll-window
	  ;; We know that the (selected) minibuffer is active, so `next-window'
	  ;; will examine all of the frames/screens that share this minibuffer.
	  ;; Should we consider `other-window-scroll-buffer' here?  I don't
	  ;; believe so.
	  (next-window selected-window))
      selected-window)))

;;;
;;;
;;;

(defun scroll-choose-other-window ()
  "Choose the window to be scrolled by the commands `scroll-other-window-down',
`scroll-other-window', `scroll-other-window-down-in-place', and `scroll-other-
window-in-place'.

The rules are these.  If the selected window is not a minibuffer window, then
choose either:

  + a window that displays the `other-window-scroll-buffer', if that buffer
    exists.  Note, this function will display that buffer if necessary.

  + the next window after the selected window in the canonical ordering of
    windows.  The next window is generally the one below the selected window,
    or the one at the top of the screen if the selected window is at the bottom
    of the screen.

However, when a minibuffer window is selected, look first for the `minibuffer-
scroll-window'.  The `minibuffer-scroll-window' is usually the window that
displays completions.  If it exists, choose it; otherwise choose the window to
be scrolled as described above (`other-window-scroll-buffer' or next window).

This function is essentially a Lisp version of the function `other-window-for-
scrolling' which first appeared in the FSF's GNU Emacs 19.26."
  (let* ((no-error nil)
	 (selected-window (selected-window))
	 (other-window nil))
    (setq other-window
	  (cond ((and (if (fboundp 'window-minibuffer-p)
			  ;; The v19 idiom.
			  (window-minibuffer-p selected-window)
			;; The v18 idiom.
			(eq selected-window (minibuffer-window)))
		      (windowp minibuffer-scroll-window)
		      (if (fboundp 'window-live-p)
			  ;; The v19 idiom.
			  (window-live-p minibuffer-scroll-window)
			;; The v18 idiom: `window-point' is `nil' if
			;; the window has been deleted.
			(window-point minibuffer-scroll-window))
		      )
		 ;; Don't signal an error when `minibuffer-scroll-window' is
		 ;; the minibuffer itself --- which would be really weird, but
		 ;; isn't necessarily erroneous.
		 (setq no-error t)
		 minibuffer-scroll-window)
		
		((and ;; `other-window-scroll-buffer' is an Emacs 19 invention.
		      (boundp 'other-window-scroll-buffer)
		      (bufferp other-window-scroll-buffer)
		      ;; `buffer-name' is `nil' if the buffer has been killed.
		      (buffer-name other-window-scroll-buffer))
		 ;; This is what FSF GNU Emacs 19.26 does, but it occurred to
		 ;; me: what if one of these forms returns the selected window?
		 ;; Signalling an error would be bad news, so I added a flag.
		 (setq no-error t)
		 (or (get-buffer-window other-window-scroll-buffer)
		     (display-buffer other-window-scroll-buffer t)))
		
		((let ((next-window (next-window selected-window)))
		   (if (eq next-window selected-window)
		       nil
		     next-window)))
		
		(t
		 ;; In Emacs 19 (FSF, Lucid, and XEmacs), look for a window on
		 ;; another visible frame or screen.  This could be written for
		 ;; Epoch, too, I suppose...
		 (condition-case nil
		     (let ((this-window (next-window selected-window nil t)))
		       (while (not (or (eq this-window selected-window)
				       (scroll-choose-window-frame-visible-p
					this-window)))
			 (setq this-window (next-window this-window nil t)))
		       this-window)
		   ;; In older versions of Emacs, `next-window' didn't accept
		   ;; three arguments.  Catch this error and then return the
		   ;; selected window --- which will cause another error to be
		   ;; signalled later on.
		   (wrong-number-of-arguments selected-window))
		 )
		))
    
    (if (and (not no-error)
	     (eq selected-window other-window))
	(error "There is no other window."))
    other-window))

;;;
;;;
;;;

(defun scroll-choose-window-frame-visible-p (window)
  "Return a true value if the frame or screen of the given WINDOW is visible."
  (cond ((fboundp 'window-frame)
	 ;; For FSF GNU Emacs 19.
	 (eq t (frame-visible-p (window-frame window))))
	((fboundp 'window-screen)
	 ;; For XEmacs and Lucid GNU Emacs.
	 (screen-visible-p (window-screen window)))
	(t t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the "in place" scrolling commands (interactive functions) and the
;;;; replacements for the standard GNU Emacs vertical scrolling commands.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; Here are the new scroll "in place" commands.
;;;;

(defun scroll-down-in-place (&optional lines)
  "Scroll the text of the current window downward by LINES lines, leaving point
as close as possible to its current window position (window line and column).
In other words, point is left \"in place\" within the window.  As a special
case, when the current window is a minibuffer window, this command scrolls the
`minibuffer-scroll-window' (which is usually the list of completions) if it
exists, or otherwise the next window in the canonical ordering of windows.

If the optional argument LINES is `nil', scroll the window by the same amount
it was moved by the immediately previous \"in place\" scrolling command, or by
the value of the variable `scroll-default-lines' (usually almost a windowful)
if the previous command was not an \"in place\" scrolling command (or when that
previous command scrolled some other window, or when other circumstances
prevent the previous scrolling distance from being used).  If LINES is the
symbol `-', then the scrolling distance is determined as if LINES had been
`nil' and then that distance is multiplied by -1.

If the window cannot be scrolled by the full distance, point is allowed to
stray from its initial position so that it can move the full number of lines.
If point cannot move the full number of lines, point is moved to the buffer
boundary.  Any immediately subsequent \"in place\" scrolling commands will try
to restore point to its initial window position."
  (interactive "P")
  (scroll-window-in-place (scroll-choose-window) lines -1))

;;;
;;;
;;;

(defun scroll-up-in-place (&optional lines)
  "Scroll the text of the current window upward by LINES lines, leaving point
as close as possible to its current window position (window line and column).
In other words, point is left \"in place\" within the window.  As a special
case, when the current window is a minibuffer window, this command scrolls the
`minibuffer-scroll-window' (which is usually the list of completions) if it
exists, or otherwise the next window in the canonical ordering of windows.

If the optional argument LINES is `nil', scroll the window by the same amount
it was moved by the immediately previous \"in place\" scrolling command, or by
the value of the variable `scroll-default-lines' (usually almost a windowful)
if the previous command was not an \"in place\" scrolling command (or when that
previous command scrolled some other window, or when other circumstances
prevent the previous scrolling distance from being used).  If LINES is the
symbol `-', then the scrolling distance is determined as if LINES had been
`nil' and then that distance is multiplied by -1.

If the window cannot be scrolled by the full distance, point is allowed to
stray from its initial position so that it can move the full number of lines.
If point cannot move the full number of lines, point is moved to the buffer
boundary.  Any immediately subsequent \"in place\" scrolling commands will try
to restore point to its initial window position."
  (interactive "P")
  (scroll-window-in-place (scroll-choose-window) lines 1))

;;;
;;; The command `scroll-other-window-down' first appeared in FSF GNU Emacs
;;; 19.26.
;;;

(defun scroll-other-window-down-in-place (&optional lines)
  "Scroll the text of the next window downward by LINES lines, leaving point in
that window as close as possible to its current window position (window line
and column).  In other words, point is left \"in place\" within the window.
The next window is generally the one below the current one, or the one at the
top of the screen if the current window is at the bottom of the screen.  In
special circumstances this command will scroll a window other than the next
window.  Read the documentation for the function `scroll-choose-other-window'
for details.

If the optional argument LINES is `nil', scroll the window by the same amount
it was moved by the immediately previous \"in place\" scrolling command, or by
the value of the variable `scroll-default-lines' (usually almost a windowful)
if the previous command was not an \"in place\" scrolling command (or when that
previous command scrolled some other window, or when other circumstances
prevent the previous scrolling distance from being used).  If LINES is the
symbol `-', then the scrolling distance is determined as if LINES had been
`nil' and then that distance is multiplied by -1.

If the window cannot be scrolled by the full distance, point is allowed to
stray from its initial position so that it can move the full number of lines.
If point cannot move the full number of lines, point is moved to the buffer
boundary.  Any immediately subsequent \"in place\" scrolling commands will try
to restore point to its initial window position.

If it is impossible to scroll the text of the window at all (because a buffer
boundary is already visible), this command signals a buffer boundary error.
The error is signalled even if point could otherwise move the full number of
lines."
  (interactive "P")
  (scroll-window-in-place (scroll-choose-other-window) lines -1))

;;;
;;;
;;;

(defun scroll-other-window-in-place (&optional lines)
  "Scroll the text of the next window upward by LINES lines, leaving point in
that window as close as possible to its current window position (window line
and column).  In other words, point is left \"in place\" within the window.
The next window is generally the one below the current one, or the one at the
top of the screen if the current window is at the bottom of the screen.  In
special circumstances this command will scroll a window other than the next
window.  Read the documentation for the function `scroll-choose-other-window'
for details.

If the optional argument LINES is `nil', scroll the window by the same amount
it was moved by the immediately previous \"in place\" scrolling command, or by
the value of the variable `scroll-default-lines' (usually almost a windowful)
if the previous command was not an \"in place\" scrolling command (or when that
previous command scrolled some other window, or when other circumstances
prevent the previous scrolling distance from being used).  If LINES is the
symbol `-', then the scrolling distance is determined as if LINES had been
`nil' and then that distance is multiplied by -1.

If the window cannot be scrolled by the full distance, point is allowed to
stray from its initial position so that it can move the full number of lines.
If point cannot move the full number of lines, point is moved to the buffer
boundary.  Any immediately subsequent \"in place\" scrolling commands will try
to restore point to its initial window position.

If it is impossible to scroll the text of the window at all (because a buffer
boundary is already visible), this command signals a buffer boundary error.
The error is signalled even if point could otherwise move the full number of
lines."
  (interactive "P")
  (scroll-window-in-place (scroll-choose-other-window) lines 1))

;;;;
;;;; Here are the replacements for GNU Emacs' standard vertical scrolling
;;;; commands.
;;;;

(or (fboundp 'original-scroll-down)
    (fset 'original-scroll-down (symbol-function 'scroll-down)))
(or (fboundp 'original-scroll-up)
    (fset 'original-scroll-up (symbol-function 'scroll-up)))
(or (fboundp 'original-scroll-other-window-down)
    ;; `scroll-other-window-down' first appeared in FSF GNU Emacs 19.26.
    (if (fboundp 'scroll-other-window-down)
	(fset 'original-scroll-other-window-down
	      (symbol-function 'scroll-other-window-down))
      ))
(or (fboundp 'original-scroll-other-window)
    (fset 'original-scroll-other-window (symbol-function 'scroll-other-window))
    )

;;;
;;;
;;;

(defun scroll-down (&optional lines)
  "Scroll the text of the current window downward by LINES lines.  As a special
case, when the current window is a minibuffer window, this command scrolls the
`minibuffer-scroll-window' (which is usually the list of completions) if it
exists, or otherwise the next window in the canonical ordering of windows.

The argument LINES is optional.  Its meaning depends on the current value of
the variable `scroll-in-place'.

When the variable `scroll-in-place' is true, this command works just like the
command `scroll-down-in-place', scrolling the current window and leaving point
\"in place\" within the window.  See the documentation for the command
`scroll-down-in-place' for more information.

When the variable `scroll-in-place' is `nil' this command invokes the standard
GNU Emacs version of `scroll-down'.  In that case, when LINES is `nil' the
current window is scrolled by nearly a complete windowful of text.

Note that this command correctly handles cases in which `scroll-in-place' has a
buffer-local value in the window to be scrolled.  That value is honored."
  (interactive "P")
  (scroll-window (scroll-choose-window) lines -1))

;;;
;;;
;;;

(defun scroll-up (&optional lines)
  "Scroll the text of the current window upward by LINES lines.  As a special
case, when the current window is a minibuffer window, this command scrolls the
`minibuffer-scroll-window' (which is usually the list of completions) if it
exists, or otherwise the next window in the canonical ordering of windows.

The argument LINES is optional.  Its meaning depends on the current value of
the variable `scroll-in-place'.

When the variable `scroll-in-place' is true, this command works just like the
command `scroll-up-in-place', scrolling the current window and leaving point
\"in place\" within the window.  See the documentation for the command
`scroll-up-in-place' for more information.

When the variable `scroll-in-place' is `nil' this command invokes the standard
GNU Emacs version of `scroll-up'.  In that case, when LINES is `nil' the
current window is scrolled by nearly a complete windowful of text.

Note that this command correctly handles cases in which `scroll-in-place' has a
buffer-local value in the window to be scrolled.  That value is honored."
  (interactive "P")
  (scroll-window (scroll-choose-window) lines 1))

;;;
;;; NOTE that in the FSF GNU Emacs 19.26 version of `scroll-other-window-down',
;;; the `lines' argument is required.  I've left it optional in order to be
;;; like `scroll-other-window'.
;;;

(defun scroll-other-window-down (&optional lines)
  "Scroll the text of the next window downward by LINES lines.  The next window
is generally the one below the current one, or the one at the top of the screen
if the current window is at the bottom of the screen.  In special circumstances
this command will scroll a window other than the next window.  Read the
documentation for the function `scroll-choose-other-window' for details.

The argument LINES is optional.  Its meaning depends on the current value of
the variable `scroll-in-place'.

When the variable `scroll-in-place' is true, this command works just like the
command `scroll-other-window-down-in-place', scrolling the next window and
leaving point \"in place\" within that window.  See the documentation for the
command `scroll-other-window-down-in-place' for more information.

When the variable `scroll-in-place' is `nil' this command invokes the standard
GNU Emacs version of `scroll-other-window-down'.  In that case, when LINES is
`nil' the next window is scrolled by nearly a complete windowful of text.
\(Note that `scroll-other-window-down' first appeared as a standard command in
the FSF's GNU Emacs 19.26.  If the builtin version of that command is not
available in the current Emacs system, an equivalent action is invoked
instead.)

Note that this command correctly handles cases in which `scroll-in-place' has a
buffer-local value in the window to be scrolled.  That value is honored."
  (interactive "P")
  ;; This code is similar to the body of `scroll-window', below.
  (let* ((other-window (scroll-choose-other-window))
	 (other-window-buffer (window-buffer other-window)))
    (if ;; Allow `scroll-in-place' to be a buffer-local variable.
	(save-excursion (set-buffer other-window-buffer) scroll-in-place)
	(scroll-window-in-place other-window lines -1)
      
      ;; Paranoid, we forcibly break any running sequence of "in place"
      ;; scrolling commands.
      (setq scroll-previous-window nil)
      ;; For XEmacs and Lucid GNU Emacs, preserve the region's state.
      (if (boundp 'zmacs-region-stays)
	  (setq zmacs-region-stays t))
      (if (fboundp 'original-scroll-other-window-down)
	  (original-scroll-other-window-down lines)
	;; `scroll-other-window-down' first appeared as a builtin in FSF GNU
	;; Emacs 19.26, so it may not be available in the current Emacs system.
	;; Do the equivalent thing.
	(original-scroll-other-window (cond
				       ((null lines) '-)
				       ((eq lines '-) nil)
				       (t (- (prefix-numeric-value lines)))
				       ))
	))
    ))

;;;
;;;
;;;

(defun scroll-other-window (&optional lines)
  "Scroll the text of the next window upward by LINES lines.  The next window
is generally the one below the current one, or the one at the top of the screen
if the current window is at the bottom of the screen.  In special circumstances
this command will scroll a window other than the next window.  Read the
documentation for the function `scroll-choose-other-window' for details.

The argument LINES is optional.  Its meaning depends on the current value of
the variable `scroll-in-place'.

When the variable `scroll-in-place' is true, this command works just like the
command `scroll-other-window-in-place', scrolling the next window and leaving
point \"in place\" within that window.  See the documentation for the command
`scroll-other-window-in-place' for more information.

When the variable `scroll-in-place' is `nil' this command invokes the standard
GNU Emacs version of `scroll-other-window'.  In that case, when LINES is `nil'
the next window is scrolled by nearly a complete windowful of text.

Note that this command correctly handles cases in which `scroll-in-place' has a
buffer-local value in the window to be scrolled.  That value is honored."
  (interactive "P")
  ;; This code is similar to the body of `scroll-window', below.
  (let* ((other-window (scroll-choose-other-window))
	 (other-window-buffer (window-buffer other-window)))
    (if ;; Allow `scroll-in-place' to be a buffer-local variable.
	(save-excursion (set-buffer other-window-buffer) scroll-in-place)
	(scroll-window-in-place other-window lines 1)
      
      ;; Paranoid, we forcibly break any running sequence of "in place"
      ;; scrolling commands.
      (setq scroll-previous-window nil)
      ;; For XEmacs and Lucid GNU Emacs, preserve the region's state.
      (if (boundp 'zmacs-region-stays)
	  (setq zmacs-region-stays t))
      (original-scroll-other-window lines))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the new functions `scroll-window-in-place', `scroll-window', and
;;;; `scroll-window-in-place-continue-sequence'.  These functions are intended
;;;; to be available to programs outside this package.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scroll-window-in-place (window lines direction)
  "Scroll WINDOW vertically by the given number of window LINES in the given
DIRECTION, leaving the window's point as close as possible to its original
window position (window line and column).  In other words, the window's point
is left \"in place\" within the window.

Note that the window to be scrolled does not have to be the selected window,
and that this function does not change which window is selected.

LINES specifies the number of window lines to scroll and is interpreted as if
it were a raw prefix argument.  If LINES is `nil', the window is scrolled by
the amount it was moved by the immediately previous \"in place\" scrolling
command, or by the value of the variable `scroll-default-lines' (by default,
almost a windowful) if the previous command was not an \"in place\" scrolling
command (or when WINDOW is not the previously scrolled window, or when the
value of `this-command' is not in the same group as the previous scrolling
command (see the documentation for the variable `scroll-command-groups'), or
when other circumstances prevent the previous scrolling distance from being
used).  If LINES is the symbol `-', then the scrolling distance is determined
as if LINES had been `nil' and then that distance is multiplied by -1.

DIRECTION determines the direction of the scrolling motion.  The values -1 and
`down' indicate downward motion; the values 1 and `up' indicate upward motion.
Any other value causes an error.

If the window cannot be scrolled by the full distance (because the window hits
the boundary of its buffer), the window's point is allowed to stray from its
initial position so that it can move the full number of lines.  If point cannot
move the full number of lines, point is moved to the buffer boundary (unless it
was already there, in which case a buffer boundary error is signalled instead).
Any immediately subsequent \"in place\" scrolling commands will try to restore
point to its initial window position.

Unless the variable `scroll-allow-blank-lines-past-eob' is true, this function
avoids displaying blank lines past the end of the buffer except as necessary to
make a previous \"in place\" scrolling action reversible.  Effectively, this
means that this function will not display any more past-end-of-buffer blank
lines than were visible when the current sequence of \"in place\" scrolling
commands started.  When the variable `scroll-allow-blank-lines-past-eob' is
true, this function will display as many blank lines as is necessary to keep
point \"in place\" in the window.

Note that if WINDOW is not the selected window and it is impossible to scroll
the text of WINDOW at all (because a buffer boundary is already visible), then
this function signals a buffer boundary error.  The error is signalled even if
point could otherwise move the full number of lines."
  (let* (;; Make sure that the user doesn't quit in the middle and leave us
	 ;; with our variables out of sync.
	 (inhibit-quit t)
	 (original-window (selected-window))
	 (original-buffer (current-buffer))
	 (window-height (- (window-height window)
			   (if (if (fboundp 'window-minibuffer-p)
				   ;; The v19 idiom.
				   (window-minibuffer-p window)
				 ;; The v18 idiom.
				 (eq window (minibuffer-window)))
			       0 1)))
	 (this-command-group (scroll-get-command-group this-command))
	 (continue-scroll-p
	  (and ;; We're scrolling the previously scrolled window...
	       (windowp scroll-previous-window)
	       (eq window scroll-previous-window)
	       ;; ...and the last command was an "in place" scrolling command
	       ;; that can be continued by this command.
	       (if (eq last-command t)
		   ;; If the previous command signalled an error, the value of
		   ;; `last-command' is `t'.  Try to see if we signalled the
		   ;; error and if point is where we left it.  (NOTE that FSF
		   ;; GNU Emacs 19.23+ no longer sets `last-command' to `t'
		   ;; when a command signals an error.  This is OK because the
		   ;; else part of this `if' does the appropriate thing.)
		   (and	scroll-boundary-error-point
			(eq (window-point window) scroll-boundary-error-point)
			(memq scroll-boundary-error-command this-command-group)
			)
		 ;; Otherwise...
		 (memq last-command this-command-group))
	       ))
	 (lines-value (prefix-numeric-value lines))
	 )
    
    ;; For XEmacs and Lucid GNU Emacs, preserve the region's state.  Note that
    ;; these Emacsen will forcibly deactivate the region if we signal an error
    ;; later on.  Is this bad?
    (if (boundp 'zmacs-region-stays)
	(setq zmacs-region-stays t))
    ;; Parse the direction into a unit distance (1 or -1).
    (setq direction (scroll-parse-direction direction))
    
    (setq scroll-previous-window window
	  ;; `(setq scroll-boundary-error-command nil)' is not necessary.
	  scroll-boundary-error-point nil)
    (unwind-protect
	(progn
	  ;; `select-window' does an implicit `set-buffer'.
	  (select-window window)
	  
	  (if (or ;; The current command is not a continuation of a running
		  ;; sequence of "in place" scrolling commands...
		  (not continue-scroll-p)
		  ;; ...or we were given an explicit number of lines to scroll,
		  ;; and that number has a different magnitude than the last
		  ;; number of lines we scrolled...
	          (and (or (numberp lines) (consp lines))
		       (/= scroll-previous-lines lines-value)
		       (/= scroll-previous-lines (- lines-value)))
		  ;; ...or the last successful scrolling command moved to a
		  ;; buffer boundary, but the buffer is no longer in the state
		  ;; we left it.  (This can occur if, for example, we signal an
		  ;; end-of-buffer error and something catches it and moves
		  ;; point or renarrows.  VM, for example, does this.)
		  (and scroll-boundary-previous-point
		       (or (not (or (bobp) (eobp)))
			   (< scroll-boundary-previous-point (point-min))
			   (> scroll-boundary-previous-point (point-max))
			   (eq scroll-boundary-previous-point (point)))))
	      
	      ;; We're starting a new sequence of scrolling commands.
	      (setq lines (if (or (numberp lines) (consp lines))
			      lines-value
			    ;; The default number of lines...
			    (* (if (eq lines '-) -1 1)
			       (if (numberp scroll-default-lines)
				   scroll-default-lines
				 (max (- window-height
					 next-screen-context-lines)
				      1))))
		    scroll-previous-lines lines
		    scroll-goal-column (scroll-determine-goal-column window)
		    scroll-boundary-previous-point nil
		    ;; `(setq scroll-boundary-previous-lines 0)' is not
		    ;; necessary.
		    scroll-window-debt 0
		    scroll-initially-displayed-lines
		    (if scroll-allow-blank-lines-past-eob
			0
		      (save-excursion
			(goto-char (window-start window))
			(vertical-motion (1- window-height)))))
	    
	    ;; Otherwise we want to scroll by the same number of lines (but
	    ;; possibly in a different direction) that we scrolled in previous
	    ;; invocations of this function.
	    (cond ((null lines)
		   (setq lines scroll-previous-lines))
		  ((eq lines '-)
		   (setq lines (- scroll-previous-lines)
			 scroll-previous-lines lines))
		  (t
		   (setq lines lines-value
			 scroll-previous-lines lines)))
	    )
	  
	  (setq lines (* direction lines))
	  
	  ;; If point is not in the window, center window around point.  We try
	  ;; to account for a bug in `pos-visible-in-window-p' in some versions
	  ;; of Emacs (see `scroll-pos-visible-bug-p', above).
	  (save-excursion
	    (if (pos-visible-in-window-p (let ((point (point)))
					   (if (and scroll-pos-visible-bug-p
						    (= point (point-max)))
					       (max (1- point) (point-min))
					     point))
					 window)
		nil
	      (vertical-motion (/ (- window-height) 2))
	      (set-window-start window (point))))
	  
	  (cond ((and scroll-boundary-previous-point
		      ;; `lines' is the same sign as the direction from point
		      ;; to the `scroll-boundary-previous-point'.
		      (cond ((> lines 0)
			     (> (- scroll-boundary-previous-point (point)) 0))
			    ((< lines 0)
			     (< (- scroll-boundary-previous-point (point)) 0))
			    (t nil)))
		 ;; We're moving away from the buffer boundary.
		 (goto-char scroll-boundary-previous-point)
		 ;; Always move here (i.e., don't reject cases in which the
		 ;; window doesn't move).
		 (scroll-set-window-start window
					  (- scroll-boundary-previous-lines))
		 ;; (message "Back, window debt is %s." scroll-window-debt)
		 (setq scroll-boundary-previous-point nil))

		((= lines 0)
		 ;; We're going nowhere, so save ourselves some work.
		 ;; (message "Scrolled zero lines.")
		 )
		
		(t
		 ;; Perform the scrolling motion.
		 (let ((initial-point (point))
		       (moved nil))
		   ;; First move point and see how far it goes.
		   (setq moved (vertical-motion lines))
		   (if (= moved lines)
		       (progn
			 ;; Point moved the full distance.  Move to the desired
			 ;; column and then try to move the window the full
			 ;; distance, too.
			 (move-to-column (+ (current-column)
					    scroll-goal-column))
			 (or (scroll-set-window-start window moved
						      original-window)
			     (scroll-signal-boundary-error initial-point
							   lines))
			 ;; (message "Normal, window debt is %s."
			 ;;          scroll-window-debt)
			 )
		     ;; Point couldn't move all the way.  Move to the buffer
		     ;; boundary if we're not already there, or signal a buffer
		     ;; boundary error otherwise.
		     (let ((boundary-point (if (< lines 0)
					       (point-min)
					     (point-max)))
			   (boundary-symbol (if (< lines 0)
						'beginning-of-buffer
					      'end-of-buffer)))
		       (if (= initial-point boundary-point)
			   (scroll-signal-boundary-error initial-point lines)
			 ;; Scroll the window by as many lines as point could
			 ;; move.
			 (or (scroll-set-window-start window moved
						      original-window)
			     (scroll-signal-boundary-error initial-point
							   lines))
			 (message "%s" (get boundary-symbol 'error-message))
			 ;; (message "Boundary, window debt is %s."
			 ;;          scroll-window-debt)
			 (setq scroll-boundary-previous-lines moved)
			 (setq scroll-boundary-previous-point initial-point)
			 (goto-char boundary-point))
		       )))
		 )))
      
      ;; The unwind forms of the `unwind-protect', above.  Restore the
      ;; originally selected window and current buffer.
      (select-window original-window)
      (set-buffer original-buffer)))
  
  ;; The standard GNU Emacs scrolling commands return `nil' so we do, too.
  nil)

;;;
;;;
;;;

(defun scroll-window (window lines direction)
  "Scroll WINDOW vertically by the given number of window LINES in the given
DIRECTION.  Note that the window to be scrolled does not have to be the
selected window, and that this function does not change which window is
selected.

When the variable `scroll-in-place' is true, this function simply invokes the
function `scroll-window-in-place' to scroll the window and leave point \"in
place\" within that window.  See the documentation for `scroll-window-in-place'
for more information.

When the variable `scroll-in-place' is `nil' this function invokes the original
version of the standard GNU Emacs command `scroll-down' or `scroll-up', as
determined by DIRECTION, to scroll the window.  If DIRECTION is -1 or `down',
the original `scroll-down' is called; if DIRECTION is 1 or `up', the original
`scroll-up' is called.  Any other DIRECTION is an error.  LINES is interpreted
as if it were a raw prefix argument.  If LINES is `nil', the window is scrolled
by almost a complete windowful.  If LINES is the symbol `-', the window is
scrolled by almost a complete windowful in the opposite direction.

Note that this function correctly handles cases in which `scroll-in-place' has
a buffer-local value in the WINDOW's buffer.  That value is honored."
  (let ((current-buffer (current-buffer))
	(selected-window (selected-window))
	(window-buffer (window-buffer window)))
    (if ;; Allow `scroll-in-place' to be a buffer-local variable.
	(if (eq current-buffer window-buffer)
	    scroll-in-place
	  (save-excursion (set-buffer window-buffer) scroll-in-place))
	(scroll-window-in-place window lines direction)
      
      (unwind-protect
	  (progn
	    ;; Paranoid, we forcibly break any running sequence of "in place"
	    ;; scrolling commands.
	    (setq scroll-previous-window nil)
	    ;; For XEmacs and Lucid GNU Emacs, preserve the region's state.
	    (if (boundp 'zmacs-region-stays)
		(setq zmacs-region-stays t))
	    (select-window window)
	    (if (= (scroll-parse-direction direction) 1)
		(original-scroll-up lines)
	      (original-scroll-down lines)))
	(select-window selected-window)
	(set-buffer current-buffer))
      )))

;;;
;;; The following function is sometimes useful.  For example, I call it from
;;; functions that are invoked by certain mouse button down events in order to
;;; preserve any running chain of "in place" scrolling commands.  This lets me
;;; continue the sequence from my mouse button up functions.
;;;
;;; I haven't yet needed a function to purposely break a running sequence of
;;; "in place" scrolling commands.  Such a function would be easy to write,
;;; however; just set the variable `scroll-previous-window' to `nil'.
;;;

(defun scroll-window-in-place-continue-sequence ()
  "If the previous command was a \"scroll in place\" command, set the variable
`this-command' to the name of that previous command.  This ensures that any
running sequence of \"in place\" scrolling commands will not be broken by the
current command.  See the documentation for the commands `scroll-down-in-place'
and `scroll-up-in-place' for more information about \"in place\" scrolling.

NOTE that you don't need to call this function if the current command scrolls
in place!  You only need to call this function when the current command is not
a \"scroll in place\" command but you still want to preserve any running
sequence of \"in place\" commands.  Such situations are rare.

NOTE that this function sets `this-command' in order to trick the \"in place\"
scrolling commands.  If something else subsequently sets `this-command', any
running sequence of scrolling commands will probably be broken anyway."
  (if (if (eq last-command t)
	  ;; If `last-command' is `t', then the previous command signalled an
	  ;; error.  See if the last invocation of `scroll-window-in-place'
	  ;; signalled an error.  (NOTE that FSF GNU Emacs 19.23+ no longer
	  ;; sets `last-command' to `t' when a command signals an error.  This
	  ;; is OK because the else part of this `if' does the appropriate
	  ;; thing.)
	  scroll-boundary-error-point
	;; Otherwise, the value of `last-command' must belong to some group of
	;; "in place" scrolling commands.
	(or (memq last-command scroll-default-command-group)
	    (let ((groups scroll-command-groups)
		  (found nil))
	      (while (and groups (not found))
		(if (memq last-command (car groups))
		    (setq found t)
		  (setq groups (cdr groups)))
		)
	      found)))
      (setq this-command last-command)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Here are the various auxiliary functions called by the function `scroll-
;;;; window-in-place'.  None of the functions are intended to be called from
;;;; outside this package.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scroll-get-command-group (command)
  "Return the group of \"in place\" scrolling commands that contains the given
COMMAND.  This is the list of commands with which the given command may share
state and form \"chains.\"

This function is an auxiliary for the function `scroll-window-in-place'.  Don't
call this function from other code."
  ;; This function assumes that the given command is an "in place" scrolling
  ;; command.
  (let ((groups scroll-command-groups)
	(found nil))
    (while (and groups (not found))
      (if (memq command (car groups))
	  (setq found t)
	(setq groups (cdr groups)))
      )
    (if groups
	(car groups)
      ;; Otherwise return the default command group.  If necessary, add the
      ;; given command to the default command group.
      (or (memq command scroll-default-command-group)
	  (setq scroll-default-command-group
		(cons command scroll-default-command-group)))
      scroll-default-command-group)
    ))

;;;
;;;
;;;

(defun scroll-parse-direction (direction)
  "Return the signed unit distance for the given DIRECTION.  If DIRECTION is
unacceptable, signal an error."
  (cond ((or (eq direction 1) (eq direction -1)) direction)
	((eq direction 'up) 1)
	((eq direction 'down) -1)
	(t (signal 'args-out-of-range (list 'direction direction)))
	))

;;;
;;;
;;;

(defun scroll-determine-goal-column (window)
  "Return the goal column for the \"in place\" vertical scrolling commands.
This is the horizontal window position at which these commands try to keep
point.

This function is an auxiliary for the function `scroll-window-in-place'.  Don't
call this function from other code."
  ;; NOTE that `window' must be the selected window!  `scroll-window-in-place'
  ;; ensures that this is so.
  (cond ((or truncate-lines
	     (and truncate-partial-width-windows
		  (< (window-width window) (screen-width)))
	     (> (window-hscroll window) 0))
	 ;; Lines in this window are being truncated.
	 (if (and track-eol (eolp))
	     9999
	   (current-column)))
	((and track-eol (eolp))
	 ;; In some ways this isn't quite right, as point doesn't track the
	 ;; ends of wrapped lines.  But if it did so, point would be on the
	 ;; wrong window line.  This is the best we can do.
	 (1- (window-width window)))
	(t (% (current-column) (1- (window-width window))))
	))

;;;
;;;
;;;

(defun scroll-set-window-start (window lines &optional original-window)
  "Move the `window-start' of the given window, which must be the selected
window.  If the window was successfully scrolled, update the variable
`scroll-window-debt' and return `t'.  Otherwise return `nil'.

This function is an auxiliary for the function `scroll-window-in-place'.  Don't
call this function from other code."
  (save-excursion
    (goto-char (window-start window))
    ;; Try to move the window start by the specified number of lines.  In
    ;; addition, try to make up any existing debt in the window start's
    ;; position and make sure that we don't move too close to the end of the
    ;; buffer.
    (let ((moved (+ (vertical-motion (+ lines
					scroll-window-debt
					scroll-initially-displayed-lines))
		    (vertical-motion (- scroll-initially-displayed-lines)))))
      ;; If we're not scrolling the `original-window' (i.e., the originally
      ;; selected window), punt if we didn't move the window start at all.
      (if (and original-window
	       (not (eq window original-window))
	       (= moved 0))
	  nil
	;; Otherwise update the window start and keep track of the debt in our
	;; position.  Return `t' to indicate success.
	(set-window-start window (point))
	(setq scroll-window-debt (- (+ lines scroll-window-debt) moved))
	t))
    ))

;;;
;;;
;;;

(defun scroll-signal-boundary-error (initial-point lines)
  "Move point to its initial location and signal an appropriate buffer boundary
error.

This function is an auxiliary for the function `scroll-window-in-place'.  Don't
call this function from other code."
  (goto-char initial-point)
  ;; Remember what we were doing and where point was when we signalled the
  ;; error so that subsequent "in place" scrolling commands can decide how to
  ;; recover.
  (setq scroll-boundary-error-command this-command
	scroll-boundary-error-point initial-point)
  (signal (if (< lines 0) 'beginning-of-buffer 'end-of-buffer)
	  nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Finally, here is the `provide' statement.
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'scroll-in-place)

;; End of file.

