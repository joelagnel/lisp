NOTATION/MISC:
   C-<char>  		hold control key down while pressing char
   M-<char>		hold "meta" (diamond next to shift bar) down while pressing char
   ESC <char>		Press "Esc" key, and then char.  Has same effect as M-<char>
   cursor		the rectangular block that determines where your type characters are entered
   mouse		the up-left-pointing arrow that corresponds to the current mouse position.
   buffer		the in-memory chunk of text that you are editing (this is typically an in-memory 
  			copy of a file).
   sub-window		a sub-portion of the Emacs window.  Each sub-window displays a buffer.
  			the current subwindow/buffer is the one containing the cursor.
   minibuffer		the last line in the Emacs window: used for echoing messages and user entry of 
  			information.  Use <tab> or <space> for completion of command and file names.
   expression		a word, or the region between two parentheses
   top-level-form  	a parenthesized lisp expression with left paren at beginning of line
   <right>,<left>,<middle>  	mouse button clicks
   **                   indicates an EXTENSION to standard emacs

   - to get out of any command or confusing state, type C-g.
   - to undo ANY NUMBER of editing commands, type C-x u (or press the "undo" key **)
   - to refresh the emacs window, type C-l.	
   - when you type a right-paren ")" while the cursor is ON a right-paren, the cursor
     briefly jumps to the matching paren.  A NEW PAREN IS NOT INSERTED. **
   - Press the "Help" key to get a buffer listing the current function key bindings **
   - To exit emacs, type C-x C-c.
   
FILES/BUFFERS/SUB-WINDOWS
   C-x C-f		load file into a buffer in current sub-window
   C-x C-s, C-x C-w	save current buffer to its-own/another file
   C-x 2		split subwindow 
   C-x o		move cursor to another sub-window
   C-x 1		single subwindow 
   C-x b		switch to another buffer in the current window
   C-x C-b		pop up a list the current buffers
   C-x k		kill buffer (defaults to buffer containing cursor)

MOVEMENT:  You can also use the left-mouse, or numeric keypad keys ** to move around.
   C-f, M-f, C-M-f	move forward one character/word/expression
   C-b, M-b, C-M-b	move backward one character/word/expression
   C-n, C-p		move to next/previous line
   C-a, M-a, C-M-a	move to beginning of line/sentence/top-level-form
   C-e, M-e, C-M-e	move to end of line/sentence/top-level-form
   M-<, M->		beginning/end of buffer
   C-v, M-v		scroll down/up one screenful
   C-s, C-r 		incremental search forward/reverse
   M-%			string replacement (queries for each replacement)

CUT/COPY/PASTE:
   C-d, M-d, M-C-d      delete character/word/expression under cursor  **M-C-d
   C-k,C-M-k		kill line/expression
   C-y,M-y		yank previous deletion. To yank deletions before that, keep pressing M-y
   <left>		move cursor to mouse
   <right>		capture text between cursor and mouse for mouse-pasting
   <middle>		mouse-paste (see previous command).
   C-<middle>		copy expression at mouse location to cursor location **
   C-<right>            capture and cut text between cursor and mouse for mouse-pasting **
   M-/			expand  word you're typing to previous matching word.
   C-t, M-t, C-M-t	Transpose characters/words/expressions around cursor

LISP-MODE (this mode is automatically used for buffers of .lisp files):
   <tab>,C-M-q,M-q	indent line/expression-at-point/surrounding region ** (including comments!)
   C-c e, C-c c		evaluate/compile surrounding top-level-form **
   C-c <num>		send num to Common Lisp (useful for aborting/continuing in the debugger) **
   C-M-l		pop up the *lisp* interactive buffer **
   C-c C-a		ask for argument-list of a Common Lisp function **
   C-c C-h		ask for list of all Common Lisp symbols containing a substring **
   C-c C-i		ask for a description of a Common Lisp object **
   C-c .		bring up a buffer with the source code for a Common Lisp function **
   
THE *LISP* BUFFER (interactive buffer running CL, created by "M-x run-cl"): **
   M-p, M-n		cycle back through previous commands matching what you've typed so far.
   C-c C-c		send keyboard interrupt to Common Lisp process
   C-c <num>, C-c C-a, C-c C-h, C-c C-i, C-c .	as in lisp-mode
