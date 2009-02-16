This is the `doc/keys.e' file of GNU eev.
Author and version (of the headers): Eduardo Ochs, 2005jan06
The body is this file old - most of it is from 2001 or before.
This file is in the Public Domain.




 «index»
 «.main-keys»		(to "main-keys")
 «.beginner-stuff»	(to "beginner-stuff")


 «main-keys»  (to ".main-keys")

This is a list of the keys and commands of Emacs that I consider most
important; the expressions between parentheses at the right are
hyperlinks to info pages, and you can follow those with C-e C-x C-e or
(if you have eev installed) with M-e. See the tutorial for more
information.

The entried marked with "(*)" are non-standard, and will work only if
you have run `eev-install-aggressively' - which is the default if you
have 

(find-enode "Commands")
(find-enode "Keys")
 
(find-efile "")
(find-efile "info.el" "define-key Info-mode-map")


Basic key sequences (``bindings'')
==================================

C-_     -- undo				(find-enode "Undo")
C-x C-e -- eval-last-sexp		(find-elinode "Run a Program")
	    other evaluation commands:	(find-enode "Lisp Eval")
C-g     -- keyboard-quit		(find-enode "Quitting")
M-x     -- execute-extended-command	(find-enode "M-x")
	    more about the minibuffer:	(find-enode "Minibuffer")
tab	-- for completion:		(find-enode "Completion")
	   for indentation:		(find-enode "Indentation")
	   in programming modes:	(find-enode "Basic Indent")

M-e	-- end-of-line-then-eval-last-sexp (*)
M-k	-- kill-buffer (*)

C-x C-s -- save-buffer			(find-enode "Saving")
C-x C-c -- save-buffers-kill-emacs	(find-enode "Saving")
C-x 1   -- delete-other-windows		(find-enode "Change Window")
C-x C-f -- find-file 			(find-enode "Visiting")
					(find-enode "Dired")
C-x 0   -- delete-window 		(find-enode "Change Window")
C-x o   -- other-window 		(find-enode "Other Window")
C-x b   -- switch-to-buffer		(find-enode "Select Buffer")
C-x k   -- kill-buffer			(find-enode "Kill Buffer")

C-h i   -- info				(find-enode "Misc Help")
C-h k   -- describe-key			(find-enode "Key Help")
C-h b   -- describe-bindings		(find-enode "Misc Help")
C-h m   -- describe-mode		(find-enode "Misc Help")
C-h l   -- view-lossage			(find-enode "Misc Help")
C-h f   -- describe-function		(find-enode "Name Help")
C-h w   -- where-is			(find-enode "Name Help")
C-h F	-- view-emacs-FAQ		(find-enode "Misc Help")
					(find-efaqnode "Top")

left    -- backward-char		(find-enode "Moving Point")
right   -- forward-char			(find-enode "Moving Point")
up      -- previous-line		(find-enode "Moving Point")
down    -- next-line			(find-enode "Moving Point")
C-a     -- beginning-of-line		(find-enode "Moving Point")
C-e     -- end-of-line			(find-enode "Moving Point")
M-<     -- beginning-of-buffer		(find-enode "Moving Point")
M->     -- end-of-buffer		(find-enode "Moving Point")

(backspace) -- delete-backward-char	(find-enode "Erasing")
(del)   -- delete-char			(find-enode "Erasing")
C-k     -- kill-line			(find-enode "Killing by Lines")
C-Spc   -- set-mark-command		(find-enode "Setting Mark")
C-w     -- kill-region			(find-enode "Other Kill Commands")
C-y     -- yank				(find-enode "Kill Ring")
M-w     -- kill-ring-save		(find-enode "Kill Ring")

M-q	-- fill-paragraph		(find-enode "Fill Commands")

C-s     -- isearch-forward		(find-enode "Incremental Search")
C-r     -- isearch-backward		(find-enode "Incremental Search")
M-C-s   -- isearch-forward-regexp	(find-enode "Regexp Search")
M-C-r   -- isearch-backward-regexp	(find-enode "Regexp Search")
M-%     -- query-replace		(find-enode "Replace")

C-x (   -- start-kbd-macro		(find-enode "Keyboard Macros")
C-x )   -- end-kbd-macro		(find-enode "Keyboard Macros")
C-x e   -- call-last-kbd-macro		(find-enode "Keyboard Macros")

C-q     -- quoted-insert		(find-enode "Inserting Text")
M-1     -- digit-argument		(find-enode "Arguments")
C-u     -- universal-argument		(find-enode "Arguments")
C-x =	-- what-cursor-position		(find-enode "Position Info")

M-|     -- shell-command-on-region	(find-enode "Single Shell")
M-.	-- find-tag			(find-enode "Tags")

C-x r k -- kill-rectangle		(find-enode "Rectangles")
C-x r y -- yank-rectangle		(find-enode "Rectangles")

C-t	-- transpose-chars		(find-enode "Transpose")
M-c	-- capitalize-word		(find-enode "Case")
M-l	-- downcase-word		(find-enode "Case")
M-u	-- upcase-word			(find-enode "Case")

M-x pwd					(find-enode "File Names")

M-x fundamental-mode			(find-elnode "Major Modes")
M-x text-mode 
M-x tcl-mode           
M-x c-mode             
M-x emacs-lisp-mode			(find-enode "Lisp Eval")

M-x sort-lines				(find-enode "Sorting")
M-x sort-columns			(find-enode "Sorting")


Some info mode bindings
=======================

q	-- Info-exit			(find-enode "Misc Help")
					(find-texinode "Overview")
(ret)	-- Info-follow-nearest-node
n	-- Info-next
p	-- Info-prev
u	-- Info-up
l	-- Info-last
?	-- Info-summary
(tab)	-- Info-next-reference
t	-- Info-top-node


Some picture mode bindings
==========================

M-x picture-mode			(find-enode "Picture")
C-c C-c -- picture-mode-exit		(find-enode "Picture")
C-c .   -- picture-movement-down	(find-enode "Insert in Picture")
C-c >   -- picture-movement-right	(find-enode "Insert in Picture")


Bindings activated by ee-invade-global-keymap
===============================================

M-e	-- end-of-line-then-eval-last-sexp
M-k	-- kill-buffer			(find-enode "Kill Buffer")
f3	-- ee-bounded


Other non-standard bindings that I set
======================================

M-o	-- other-window			(find-enode "Other Window")
ins     -- overwrite-mode		(find-enode "Minor Modes")
f1	-- buffer-menu			(find-enode "Several Buffers")
f4      -- redraw-display		(find-elnode "Refresh Screen")
f5	-- call-last-kbd-macro		(find-enode "Keyboard Macros")
f6      -- picture-mode			(find-enode "Picture")




 «beginner-stuff»  (to ".beginner-stuff")

If you have never used Emacs before,
====================================
...then take a look at this section first! You will need to have
notions of a few things:

  * Emacs uses something called "buffers", both to edit files and to
    keep temporary information; you'll need to know how to switch to
    the buffer that you want - you can always use C-x b and TAB for
    that, but in the beginning it is easier to use Emacs on X rather
    than on console mode, and on X you can use the "Buffers" entry on
    Emacs's menu bar.

 (find-enode "Buffers")
 (find-enode "Menu Bar")
C-x b	-- switch-to-buffer		(find-enode "Select Buffer")
TAB	-- minibuffer-complete		(find-enode "Completion")
C-x k   -- kill-buffer			(find-enode "Kill Buffer")

  * Emacs uses the term "window" in a sense that is a bit different
    from the modern meaning; take a look at the manual page about
    "frames" for curiosity, but don't pay much attention to that - you
    won't be using frames much. You WILL need to know some keys for
    dealing with windows, though, because sometimes you'll type
    something wrong and Emacs will bring up windows that you don't
    want, and you will need to get rid of them.

 (find-enode "Windows")
 (find-enode "Basic Window")
 (find-enode "Frames")
C-x 0	-- delete-window		(find-enode "Change Window")
C-x 1	-- delete-other-windows		(find-enode "Change Window")
C-x 2	-- split-window-vertically	(find-enode "Split Window")
C-x 3	-- split-window-horizontally	(find-enode "Split Window")
C-x o	-- other-window			(find-enode "Other Window")

  * These things like "C-x o" are names of keys or of sequences of
    keys, and these things like "other-window" are the names of the
    Lisp functions that are associated to those keys; these concepts
    are a bit too technical, so just take a quick look at the pages
    below...

 (find-enode "Keys")
 (find-enode "Commands")
M-x     -- execute-extended-command	(find-enode "M-x")

  * Some commands expect extra input in the "minibuffer" - like C-x b
    (i.e., switch-to-buffer), that was mentioned above... you need to
    know how to "interrupt" these commands, and, if you entered one of
    them on purpose, how to use completion for them.

C-g     -- keyboard-quit		(find-enode "Quitting")
TAB	-- minibuffer-complete		(find-enode "Completion")

  * You will probably also want to know how to use Emacs as a text
    editor, how to save a file, quit Emacs, undo changes, how to
    abandon a file without saving its changes, etc; the really most
    essential commands for that are listed below, but I believe that
    the best way to go is to first learn how to create and follow
    hyperlinks (look at the eev tutorial!) and only then start to
    treat Emacs as a text editor...

C-_     -- undo				(find-enode "Undo")
C-x C-s -- save-buffer			(find-enode "Saving")
C-x C-c -- save-buffers-kill-emacs	(find-enode "Saving")
C-x C-f -- find-file 			(find-enode "Visiting")

 (find-eev "e/tutorial.e")
 (find-eev "e/tutorial.pt.e")




# (find-angg ".emacs" "same-window-buffer-names")
# (find-enode "Force Same Window")
# (find-es "escripts" "emacs-tutor.th")

#  Local Variables:
#  coding:               raw-text-unix
#  ee-anchor-format:     "«%s»"
#  End:
