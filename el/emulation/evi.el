;; Copyright (c) 1992, 1993, 1994 Jeffrey R. Lewis
;; All rights reserved.
;;
;; Redistribution and use in source and compiled forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright notice,
;;    this list of conditions and the following disclaimer.
;; 2. Redistributions in compiled form must either be accompanied by the
;;    source, or reproduce the above copyright notice, this list of conditions
;;    and the following disclaimer in the documentation and/or other materials
;;    provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
;; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.

;; LCD Archive Entry:
;; evi|Jeffrey R. Lewis|jlewis@cse.ogi.edu
;; |Emulate Vi - enhanced emacs vi emulator for vi-heads
;; |2-23-94|1.0i|~/modes/evi.tar.Z
(defvar evi-version "Evi 1.0i of 2-23-94")
(defvar evi-copyright "Copyright (c) 1992, 1993, 1994 Jeffrey R. Lewis")

;; Here follows Evi 1.0, an even better vi emulator aimed at those who either
;; are well accustomed to vi, or who just simply happen to like its style of
;; editing better than emacs' default.  Evi's first goal is vi compatibility.
;; Its second goal is to be an extension of vi, taking advantage of features
;; supplied by the emacs environment, without simply becoming emacs with vi'ish
;; key bindings.

;; You can get the latest copy of evi or its documentation by sending an
;; e-mail message to mail-server@brandx.rain.com.  The body of the message
;; should look like:
;;      begin
;;	send <files>
;;      end
;; where <files> is a space separated list taken from: evi.el, evi.tex
;; (documentation in TeX), evi.info (documentation in emacs info format) or
;; evi.tar.gz (for a tar of all three).  Any file may be requested in either
;; compressed (.Z) or gzipped (.gz) format by appending the appropriate suffix.
;; Compressed or gzipped files will be uuencoded.  For example, the following
;; will fetch evi.el.gz and evi.info.gz:
;;      begin
;;	send evi.el.gz evi.info.gz
;;      end
;; Alternately, you can pick up a copy of evi from the elisp archive,
;; archive.cis.ohio-state.edu, in the file
;;      pub/gnu/emacs/elisp-archive/modes/evi.tar.Z
;; This copy is, however, not kept as up-to-date.

(defmacro evi-defbuffervar (name default-value documentation)
  (list 'progn (list 'defvar name nil documentation)
	       (list 'make-variable-buffer-local (list 'quote name))
	       (list 'set-default (list 'quote name) default-value)))

(defvar evi-inhibit-startup-message nil)

(defvar evi-emacs-version
  (let ((version (emacs-version)))
	(cond ((string-match "Emacs 18\\|Epoch 4" version)
	        'emacs18)
	      ((string-match "Emacs 19\.[0-9]*\.[0-9]* Lucid" version)
	        'lucid19)
	      ((string-match "Emacs 19" version)
		'emacs19))))

(cond
  ((eq evi-emacs-version 'emacs18)
    (defun evi-fill-keymap (keymap def)
      (fillarray keymap def))

    (defun evi-keymap-bindings (map)
      (evi-keymap-bindings2 map ""))

    (defun evi-keymap-bindings2 (map prefix)
      (if (arrayp map)
	  (let ((i 0)
		(len (length map))
		(binding)
		(keys)
		(mappings nil))
	    (while (< i len)
	      (setq binding (aref map i))
	      (if binding
		  (progn
		    (setq keys (concat prefix (char-to-string i)))
		    (if (keymapp binding)
			(setq mappings
			      (nconc (evi-keymap-bindings2 binding keys)
				     mappings))
		      (setq mappings (cons (cons keys binding) mappings)))))
	      (setq i (1+ i)))
	    mappings)
	(let ((bindings (cdr map))
	      (mappings nil))
	  (while bindings
	    (let* ((binding (car bindings))
		   (keys (concat prefix (char-to-string (car binding)))))
	      (if (keymapp (cdr binding))
		  (setq mappings
			(nconc (evi-keymap-bindings2 (cdr binding) keys)
			       mappings))
		(setq mappings (cons (cons keys (cdr binding)) mappings))))
	    (setq bindings (cdr bindings)))
	  mappings))))
  ((eq evi-emacs-version 'emacs19)
    (defun evi-fill-keymap (keymap def)
      (fillarray (car (cdr keymap)) def))

    (defun evi-keymap-bindings (map)
      (evi-keymap-bindings2 map ""))

    (defun evi-keymap-bindings2 (map prefix)
      (let ((bindings (cdr map))
	    (mappings nil))
	(while bindings
	  (if (vectorp (car bindings))
	      (setq mappings
		    (nconc (evi-vector-keymap-bindings (car bindings) prefix)
			   mappings))
	    (let* ((binding (car bindings))
		   (key (if (integerp (car binding))
			    (char-to-string (car binding))
			  (concat "<" (prin1-to-string (car binding)) ">")))
		   (keys (concat prefix key)))
	      (if (keymapp (cdr binding))
		  (setq mappings
			(nconc (evi-keymap-bindings2 (cdr binding) keys)
			       mappings))
		(setq mappings (cons (cons keys (cdr binding)) mappings)))))
	  (setq bindings (cdr bindings)))
	mappings))

    (defun evi-vector-keymap-bindings (map prefix)
      (let ((i 0)
	    (len (length map))
	    (binding)
	    (keys)
	    (mappings nil))
	(while (< i len)
	  (setq binding (aref map i))
	  (if binding
	      (progn
		(setq keys (concat prefix (char-to-string i)))
		(if (keymapp binding)
		    (setq mappings
			  (nconc (evi-keymap-bindings2 binding keys)
				 mappings))
		  (setq mappings (cons (cons keys binding) mappings)))))
	  (setq i (1+ i)))
	mappings)))
  ((eq evi-emacs-version 'lucid19)
    (defun evi-fill-keymap (keymap def)
      (let ((i 128))
	(while (<= 0 (setq i (1- i)))
	  (define-key keymap (make-string 1 i) def))
	keymap))

    (defun evi-keymap-bindings (map)
      (let ((mappings nil))
	(evi-keymap-bindings2 map "")
	mappings))

    (defun evi-keymap-bindings2 (map prefix)
      (map-keymap
       (function
	 (lambda (key def)
	   (let* ((keys (concat prefix (single-key-description key))))
	     (if (keymapp def)
		 (setq mappings
		       (nconc (evi-keymap-bindings2 def keys) mappings))
	       (setq mappings
		     (cons (cons keys def) mappings)))))) map))))

(defvar evi-initialized nil)

(defvar evi-interactive t)

(defvar evi-mode-hook nil
  "*Function or functions called upon entry to evi.")

(evi-defbuffervar evi-enabled nil
  "If t, currently emulating vi in this buffer.")

(defvar evi-debug nil
  "If t, errors generated by emacs are not handled.")

(defvar evi-supress-ex-startup nil
  "If t, don't source .exrc or EXINIT at startup.")

(defvar evi-report-unsupported-options nil
  "If t, give an error if a :set option is used that isn't supported.
Otherwise, these are silently ignored.")

(evi-defbuffervar evi-mode 'vi
  "Current vi mode, one of vi, insert or replace.")

(evi-defbuffervar evi-mode-string nil
  "String describing current evi mode.  This is displayed in the mode line.")

(defvar evi-minibuf-contents nil
  "Contents of last minibuf read.")

(evi-defbuffervar evi-in-minibuf nil
  "If t, we are currently editing in the minibuffer")

(defvar evi-enable-emacs-commands nil
  "If t, emacs commands will be visible")

(defvar evi-meta-prefix-char nil
  "Meta-prefix-char to use while in Evi buffers.")

(defvar evi-emacs-meta-prefix-char nil
  "Meta-prefix-char that emacs uses.")

(defvar evi-parameterized-macro nil
  "If t, currently executing a parameteized macro.")

(defvar ex-input-escapes nil
  "If t, backslash escapes in ex commands will be processed.")

(defvar evi-read-only-buffers nil
  "If t, read-only files will have read-only buffers")

(defvar evi-last-point nil
  "Used to calculate line number updates.")

(defvar evi-mark nil
  "Used to define regions for operator commands.")

(defvar evi-prev-file nil
  "Filename of previous file edited.")

(defvar evi-directory-stack nil)

(defvar evi-process-buffer nil)

(defvar evi-abbrev-list nil)

(evi-defbuffervar evi-emacs-local-map nil
  "Emacs' local map.  \(buffer specific\)")

(defvar evi-emacs-local-suppress-key-list '(?\b ?\t ?\e ?\C-?)
  "Keys from emacs local map that are to be suppressed.")

(defvar evi-prompted nil
  "If t, the current command was prompted for.")

(evi-defbuffervar evi-replace-max nil
  "Maximum excursion of a replace, after which it switches to insert.")

(evi-defbuffervar evi-overstruck-char nil
  "Value of the character overstruck by the `$' marking a partial line change.")

(evi-defbuffervar evi-context nil
  "Current motion context.  One of to-end, to-next, whole-line, or nil.
The value of this variable is passed to evi-motion-command, and is set by
prefix operators like 'd' or '>' to control the type of region defined by
the following motion command.")

(defvar evi-prefix-count nil
  "Current prefix count.")

(defvar evi-last-prefix-count nil
  "Last prefix count.")

(defvar evi-prefix-count-multiplier nil
  "Current prefix count multiplier.")

(defvar evi-register-spec nil
  "Current register to use for deletes, yanks, puts, etc.")

(defvar evi-last-register-spec nil
  "Last register used for deletes, yanks, puts, etc.")

(defvar evi-digit-register 8
  "Current delete-ring register cursor.  Points to the register that
will be register 1.")

(defvar evi-repeat-count 0
  "The number of times the current command has been repeated via `.'.")

(defvar evi-hidden-repeat-count 0
  "The hidden copy of evi-repeat-count, which isn't visible unless actually
repeating a command.")

(defvar evi-last-macro-register nil
  "Last register used to invoke a macro via \\[evi-register-macro].")

(defvar evi-registers (make-vector 72 nil)
  "Vi registers.  0-8 are the delete ring, 9 is the unnamed text register,
10-35 are the alphabetic text registers, and 36-71 are the mark registers.
Each text register is a cons cell with the car being the text in the register
and the cdr being a flag indicating whether or not the text is whole lines.")

(defvar evi-register-unnamed 9
  "Symbolic name for the unnamed register.  Shouldn't change.")

(defvar evi-region-shape 'chars
  "Specifies the shape of the region for the current operation - one of
chars, lines, or rectangle.  The value of this variable is stored in the cdr
of any register that gets stored as a result of the current command.")

(evi-defbuffervar evi-current-indentation 0
  "The indentation of the most recently auto-indented line.  Used by
evi-newline to determine when to kill auto-indented whitespace.
\(buffer specific\)")

(evi-defbuffervar evi-goal-column 0
  "The column that vertical cursor motion will try to preserve, if possible.")

(evi-defbuffervar evi-reset-goal-column t
  "If t, a horizontal motion has been performed, thus goal column must be reset.")

(defvar evi-search-pattern nil
  "The last pattern specified for searching.")

(defvar evi-search-forward t
  "If t, the last search command was a forward search.")

(defvar evi-find-character nil
  "The last character specified for finding.")

(defvar evi-find-forward t
  "If t, the last find command was a forward search.")

(defvar evi-find-up-to nil
  "If t, the last find command was a find up to command.")

(defvar ex-previous-re nil
  "Last regular expression searched for in :subst command.")

(defvar ex-previous-replacement nil
  "Last replacement used in :subst command.")

(evi-defbuffervar evi-context-ring (make-vector 10 nil)
  "The last 10 contexts for this buffer.  A context is a location in the buffer
where only relative motions were performed.  A new context is thus saved each
time a non-relative motion is performed.")

(evi-defbuffervar evi-context-ring-cursor 0
  "The cursor pointing to the last context in the context ring.")

(defvar evi-last-shell-command nil
  "The last shell command run.")

(defvar ex-work-space (get-buffer-create " *ex-work-space*")
  "Evi work space for parsing ex commands.")

(defvar ex-tag nil
  "Last tag specified.")

(defun evi-make-keymap (name small &optional fill)
  (let ((map (if small (make-sparse-keymap) (make-keymap))))
    (if (fboundp 'set-keymap-name)
	(set-keymap-name map name))
    (if fill
	(evi-fill-keymap map fill))
    map))

(defconst evi-top-level-map
  (evi-make-keymap 'evi-top-level-map nil 'evi-top-level))

(defconst evi-vi-map (evi-make-keymap 'evi-vi-map nil)
  "The keymap used in vi mode.")

(defconst evi-param-map (evi-make-keymap 'evi-param-map t)
  "The keymap used for parameterized macros.")

(defconst evi-motion-map (evi-make-keymap 'evi-motion-map nil)
  "The keymap used for operand motions.")

(defconst evi-map-map (evi-make-keymap 'evi-map-map t)
  "The keymap used for map macros.")

(defconst evi-input-map (evi-make-keymap 'evi-input-map nil 'evi-self-insert)
  "The keymap used in input modes.")

(defconst evi-replace-map
  (evi-make-keymap 'evi-replace-map nil 'evi-self-replace)
  "The keymap used in replace mode.")

(defconst evi-insert-map (evi-make-keymap 'evi-insert-map t)
  "The insert mode specific input map.")

(defconst evi-read-string-map (evi-make-keymap 'evi-read-string-map t)
  "The evi-read-string specific command map.")

(defconst evi-read-string-input-map
  (evi-make-keymap 'evi-read-string-input-map t)
  "The evi-read-string specific input map.")

(defconst evi-ex-map (evi-make-keymap 'evi-ex-map t)
  "The keymap used when reading ex commands from the minibuffer")

(defconst evi-ex-input-map (evi-make-keymap 'evi-ex-input-map t)
  "The keymap used when reading ex commands from the minibuffer (insert-mode)")

(defconst evi-input-map-map (evi-make-keymap 'evi-input-map-map t)
  "The keymap used for input map macros.")

(defconst evi-shell-map (evi-make-keymap 'evi-shell-map t)
  "The local keymap used in command mode in a shell buffer.")

(evi-defbuffervar evi-buffer-local-vi-map 
  (evi-make-keymap 'evi-buffer-local-vi-map t)
  "The keymap for buffer specific additions to the vi command map")

(defconst evi-minibuf-input-map (evi-make-keymap 'evi-minibuf-input-map t))

(defvar evi-Z-map (evi-make-keymap 'evi-Z-map t))
(defvar evi-lbrack-map (evi-make-keymap 'evi-lbrack-map t))
(defvar evi-lbrack-m-map (evi-make-keymap 'evi-lbrack-m-map t))
(defvar evi-rbrack-map (evi-make-keymap 'evi-rbrack-map t))

(cond ((eq evi-emacs-version 'emacs19)
        (defconst evi-vi-keymap-list 
	  (list (cons 'param evi-param-map) (cons 'map evi-map-map)
		'evi-buffer-local-vi-map evi-vi-map
		function-key-map (cons 'cond-emacs (cons 'local nil))
		(cons 'cond-emacs (cons 'global nil)))))
      ((eq evi-emacs-version 'lucid19)
        (defconst evi-vi-keymap-list 
	  (list (cons 'param evi-param-map) (cons 'map evi-map-map)
		'evi-buffer-local-vi-map evi-vi-map
		(cons 'cond-emacs (cons 'local nil))
		(cons 'cond-emacs (cons 'global nil)))))
      (t
        (defconst evi-vi-keymap-list 
	  (list (cons 'param evi-param-map) (cons 'map evi-map-map)
		evi-buffer-local-vi-map evi-vi-map
		(cons 'cond-emacs (cons 'global nil))))))

(evi-defbuffervar evi-keymap-list nil
  "Keymap list")

(defconst evi-all-keymaps '(vi insert replace ex)
  "All Evi keymaps.")

(evi-defbuffervar evi-register-parameter nil
  "Register specification to the current parameterized macro.")

(evi-defbuffervar evi-prefix-count-parameter nil
  "Prefix count to the current parameterized macro.")

(evi-defbuffervar evi-insert-point nil
  "The point at which the current insert command began.")

;; Vi option variables
;; ZZ - could/should make some of these buffer local after reading EXINIT

(defconst evi-option-list
  '((("autoindent" "ai") . (bool . evi-auto-indent))
    (("autoprint" "ap") . (bool . nil))
    (("autowrite" "aw") . (bool . nil))
    (("backslash-escapes" "be") . (bool . ex-input-escapes))
    (("beautify") . (bool . nil))
    (("command-line-editing" "cle") . (bool . evi-command-line-editing))
    (("directory" "dir") . (string . nil))
    (("edcompatible" "ed") . (bool . nil))
    (("enable-emacs-commands" "ee") . (bool . evi-enable-emacs-commands))
    (("errorbells" "eb") . (bool . evi-error-bell))
    (("exrc") . (bool . evi-local-exrc))
    (("flash") . (bool . nil))
    (("global-directory" "gd") . (bool . evi-global-directory))
    (("hardtabs" "ht") . (number . nil))
    (("ignorecase" "ic") . (bool . evi-ignore-case))
    (("inhibit-startup-message" "ism") . (bool . evi-inhibit-startup-message))
    (("ishell" "ish") . (string . explicit-shell-file-name))
    (("lisp") . (bool . nil))
    (("list") . (bool . nil))
    (("magic") . (bool . evi-search-magic))
    (("mesg") . (bool . nil))
    (("meta-prefix" "mp") . (char . evi-meta-prefix-char))
    (("modeline") . (bool . nil))
    (("mode-specific-insert-bindings" "msb") .
     (bool . evi-insert-mode-local-bindings))
    (("modified-paragraph") . (bool . evi-modified-paragraph))
    (("novice") . (bool . nil))
    (("number" "nu") . (bool . evi-number))
    (("optimize" "opt") . (bool . nil))
    (("paragraphs" "para") . (string . nil))
    (("prompt") . (bool . nil))
    (("readonly" "ro") . (bool . evi-buffer-read-only))
    (("readonly-buffers") . (bool . evi-read-only-buffers))
    (("redraw") . (bool . nil))
    (("remap") . (bool . evi-remap))
    (("report") . (number . nil))
    (("scroll") . (number . evi-scroll-count))
    (("sections" "sect") . (string . nil))
    (("shell" "sh") . (string . shell-file-name))
    (("shiftwidth" "sw") . (number . evi-shift-width))
    (("showmatch" "sm") . (bool . evi-show-match))
    (("showmode") . (bool . evi-show-mode))
    (("slowopen" "slow") . (bool . nil))
    (("sourceany") . (bool . nil))
    (("tabstop" "ts") . (number . evi-tab-width))
    (("tags") . (string . nil))
    (("taglength" "tl") . (number . nil))
    (("term") . (string . nil))
    (("terse") . (bool . nil))
    (("timeout") . (bool . evi-timeout))
    (("timeoutlen") . (number . evi-timeout-length))
    (("ttytype" "tty") . (string . nil))
    (("warn") . (bool . nil))
    (("word") . (string . evi-word))
    (("Word") . (string . evi-Word))
    (("wrapmargin" "wm") . (number . evi-wrap-margin))
    (("wrapscan" "ws") . (bool . evi-search-wraparound))
    (("writeany" "wa") . (bool . nil))))

(defvar evi-set-options nil
  "List of options that have been set.")

(defconst evi-auto-indent nil
  "*If t, automatically indents text inserted on a new line.")

(defconst evi-command-line-editing nil
  "*If t, command-line editing is enabled.")

(defun evi-command-line-editing (enable)
  (evi-define-key '(read-string read-string-input ex ex-input)
		  "\e" (if enable nil 'evi-exit-minibuf)))

(defconst evi-error-bell nil
  "*If t, ring bell on error.")

(defconst evi-local-exrc nil
  "*If t, source local .exrc file at startup.")

(defconst evi-global-directory t
  "*If t, a global current directory is used (this is the default).")

(defconst evi-ignore-case nil
  "*If t, ignore case in searches.")

(defconst evi-search-magic t
  "*If t, search patterns are normal regular expressions.  This is the default.
Otherwise, the `magic' characters `.' `[' and `*' are treated as literals and
must be escaped to get their regular expression interpretation.")

(defconst evi-insert-mode-local-bindings nil
  "*If t, emacs buffer-local key bindings will be enabled in insert mode.")

(defconst evi-modified-paragraph nil
  "*If t, a modified paragraph motion will be used that is similar to
sentence motion.")

(defconst evi-number nil
  "*If t, tracks line and column number in status line (NOT).")

(evi-defbuffervar evi-buffer-read-only nil
  "*If t, the current buffer is read-only")

(defconst evi-remap t
  "*If t, nested map macros are expanded")

(defconst evi-scroll-count nil
  "*The number of lines to scroll.")

(defconst evi-shift-width 8
  "*The number of colums shifted by > and < command, and ^T and ^D
in insert mode.")

(defconst evi-show-match nil
  "*If t, show matching parentheses.")

(defun evi-show-match (val)
  (setq blink-matching-paren val))

(defconst evi-show-mode t
  "*If t, show current vi mode.")

(defconst evi-tab-width 8
  "*Distance between tab stops")

(defun evi-tab-width (width)
  (setq tab-width width))

(defconst evi-timeout t
  "*If t, keys in multi-character maps must be typed within one second of each
other, otherwise the partial command aborted.")

(defconst evi-timeout-length 500
  "*Not implemented.")

(defconst evi-word "[a-zA-Z0-9_]+\\|[^a-zA-Z0-9_ \t\n]+\\|^[ \t]*\n"
  "*Regular expression to describe words for w, b and e commands.")

(defconst evi-Word "[^ \t\n]+\\|^[ \t]*\n"
  "*Regular expression to describe words for W, B and E commands.")

(defconst evi-wrap-margin 0
  "*If non-zero, the amount of right margin past which wraparound occurs.")

(defun evi-wrap-margin (margin)
  (if (eq evi-emacs-version 'emacs18)
      (if (= margin 0)
	  (setq auto-fill-hook nil)
	(setq fill-column (- (window-width) margin)
	      auto-fill-hook 'do-auto-fill))
    (if (= margin 0)
	(setq auto-fill-function nil)
      (setq fill-column (- (window-width) margin)
	    auto-fill-function 'do-auto-fill))))

(defconst evi-search-wraparound t
  "*If t, search wraps around the end of the file.")

;; these are intended to be ordered roughly in order of frequency of use
(defvar ex-commands
  '((("edit" . 1) . ((0 . ((nil . "!") (t . offset) (t . file))) . ex-edit))
    (("buffer" . 1) . ((0 . ((nil . "!") (t . buffer))) . ex-change-buffer))
    (("read" . 1) .
     ((1 . ((t . (if "!" shell-command)) (t . file))) . ex-read))
    (("write" . 1) . ((2 . ((nil . "!")
			    (t . (if "!" shell-command))
			    (t . ">>") (t . file))) . ex-write))
    (("kill" . 1) . ((0 . ((nil . "!") (t . buffer))) . ex-kill-buffer))
    (("next" . 1) . ((0 . ((nil . "!") (t . files))) . ex-next))
    (("Edit" . 1) .
     ((0 . ((nil . "!") (nil . offset) (t . file))) . ex-edit-other-window))
    (("Buffer" . 1) .
     ((0 . ((nil . "!") (t . buffer))) . ex-change-buffer-other-window))
    (("Kill" . 1) .
     ((0 . ((nil . "!") (t . buffer))) . ex-kill-buffer-delete-windows))
    (("Write" . 1) . ((0 . ((nil . "!"))) . ex-write-all-buffers))
    (("Next" . 1) . ((0 . ((nil . "!") (t . files))) . ex-next-other-window))
    (("set" . 2) . ((0 . ((nil . settings))) . ex-set))
    (("substitute" . 1) .
     ((2 . ((t . regular-expression) (backup . regular-expression2)
	    (nil . "g") (nil . "c"))) . ex-substitute))
    (("global" . 1) .
     ((2 . ((nil . "!") (t . regular-expression) (t . command))) . ex-global))
    (("vglobal" . 1) .
     ((2 . ((t . regular-expression) (t . command))) . ex-vglobal))
    (("map" . 3) .
     ((0 . ((nil . "!") (t . map) (t . words))) . ex-map))
    (("gdb" . 2) . ((0 . ((t . file))) . ex-gdb))
    (("wk" . 2) . ((0 . nil) . ex-write-kill))
    (("wq" . 2) . ((0 . ((nil . "!"))) . ex-write-quit))
    (("Wq" . 2) . ((0 . ((nil . "!"))) . ex-write-all-and-quit))
    (("visual" . 2) . ((0 . ((nil . "!") (t . offset) (t . file))) . ex-edit))
    (("Visual" . 2) .
     ((0 . ((nil . "!") (nil . offset) (t . file))) . ex-edit-other-window))
    (("abbreviate" . 2) .
     ((0 . ((t . abbrev) (t . words))) . ex-abbrev))
    (("append" . 1) . ((1 . nil) . ex-not-implemented))
    (("args" . 2) . ((0 . nil) . ex-not-implemented))
    (("bind" . 2) .
     ((0 . ((nil . "!") (t . word) (t . rest-of-line))) . ex-elisp-bind))
    (("bug" . 3) . ((0 . ((t . words))) . ex-report-bug))
    (("cd" . 2) . ((0 . ((t . file))) . ex-change-directory))
    (("change" . 1) . ((2 . nil) . ex-not-implemented))
    (("chdir" . 3) . ((0 . ((t . file))) . ex-change-directory))
    (("copy" . 2) . ((2 . ((t . address))) . ex-copy))
    (("delete" . 1) . ((2 . ((t . register))) . ex-delete))
    (("dirs" . 2) . ((0 . nil) . ex-directory-stack))
    (("elisp" . 2) . ((0 . ((t . rest-of-line))) . ex-elisp-execute))
    (("evilist" . 4) . ((0 . ((t . words))) . ex-mail-list))
    (("file" . 1) . ((0 . ((t . file))) . ex-file))
    (("insert" . 1) . ((1 . nil) . ex-not-implemented))
    (("join" . 1) . ((2 . nil) . ex-not-implemented))
    (("killprocess" . 5) . ((0 . ((t . process))) . delete-process))
    (("list" . 1) . ((2 . nil) . ex-not-implemented))
    (("mail" . 3) . ((0 . ((t . words))) . ex-mail))
    (("mark" . 2) . ((1 . ((t . mark))) . ex-mark))
    (("move" . 1) . ((2 . ((t . address))) . ex-move))
    (("number" . 2) . ((2 . nil) . ex-not-implemented))
    (("popd" . 2) . ((0 . nil) . ex-pop-directory))
    (("preserve" . 3) . ((0 . nil) . ex-preserve))
    (("previous" . 4) . ((0 . nil) . ex-not-implemented))
    (("print" . 1) . ((2 . nil) . ex-print))
    (("pushd" . 4) . ((0 . ((t . file))) . ex-push-directory))
    (("put" . 2) . ((1 . ((t . register))) . ex-put))
    (("quit" . 1) . ((0 . ((nil . "!"))) . ex-quit))
    (("recover" . 3) . ((0 . ((nil . "!") (t . file))) . ex-recover))
    (("initialize" . 3) . ((0 . nil) . ex-initialize))
    (("rewind" . 3) . ((0 . nil) . ex-not-implemented))
    (("send" . 3) . ((0 . ((nil . "!"))) . ex-send-mail))
    (("shell" . 2) . ((0 . nil) . ex-shell))
    (("source" . 2) . ((0 . ((t . file))) . ex-source-file))
    (("tag" . 1) . ((0 . ((t . word))) . ex-tag))
    (("unabbreviate" . 3) . ((0 . ((t . abbrev))) . ex-unabbrev))
    (("undo" . 1) . ((0 . nil) . ex-not-implemented))
    (("unmap" . 3) . ((0 . ((nil . "!") (t . word))) . ex-unmap))
    (("version" . 2) . ((0 . nil) . ex-evi-version))
    (("xit" . 1) . ((0 . ((nil . "!"))) . ex-write-all-and-quit))
    (("yank" . 1) . ((2 . ((t . register))) . ex-yank))
    (("!" . 1) . ((2 . ((nil . "&") (t . shell-command))) . ex-shell-command))
    (("<" . 1) . ((2 . nil) . ex-shift-left))
    (("=" . 1) . ((2 . nil) . ex-not-implemented))
    ((">" . 1) . ((2 . nil) . ex-shift-right))
    (("&" . 1) . ((2 . nil) . ex-substitute-again))
    (("@" . 1) . ((2 . nil) . ex-not-implemented))
    (("" . 0) . ((2 . nil) . ex-null)))
"Ex commands table

The car of an item in the list is a pair of the full name of a command with
the length of the shortest prefix that's unambiguous.  The cdr of an item
is a pair of a description of arguments with the name of the lisp function
to invoke for this command.  The description of argument is a pair of the
number of addresses this command accepts and a list describing its
subsequent arguments and how to parse them.  Each element of the list of
subsequent argument descriptions is a pair.  The car of this pair is `t'
if the parser should eat whitespace before the arg, and `nil' if it
shouldn't.  The cdr describes the argument itself.  The possible values
are as follows (symbols are indicated by prefixing with \"'\", asterisked
items are completable):
      a string		    literally match that string
      'address		    an ex line address
      'register		    a register name
    * 'file		    a file name (`%', `#' and wildcards are expanded)
    * 'files		    several file names (similarly expanded)
    * 'buffer		    a buffer name (can include spaces)
      'rest-of-line	    the rest of the line (can include `|')
    * 'process		    a process name (can include spaces and `|')
      'word		    a space-delimited word
      'words		    several words
    * 'map		    a :map macro name
    * 'abbrev		    an :abbrev macro name
      'regular-expression   a regular expression
      'regular-expression2  same but `&' is special
      'command		    an ex command
    * 'settings		    :set settings (`wm=8', etc)
    * 'shell-command	    a shell command (`%', `#' are expanded)
      'offset		    a line offset (`+5')
      'mark		    a mark

In summary (postfix `*' means `list-of'):

   ((full-name . prefix-len) .
    ((num-of-addrs . (eat-whitespace? . arg-descr)*) . lisp-function))")


;; Macros

(defmacro evi-defmotion (&rest args)
  (let* ((direction (car args))
	 (function (car (cdr args)))
	 (params (nth 2 args))
	 (documentation (nth 3 args))
	 (body (nthcdr 4 args))
	 (do-function (intern (concat "do-" (symbol-name function)))))
    ; ZZ some rather narly hard-coding here, but does the trick for now
    (cond ((eq (car params) '&char)
	    (` (progn (defun (, function) () (, documentation)
			(interactive)
			(evi-motion-command (quote (, do-function))
					    (quote (, direction))
					    (evi-adjust-count) evi-context
					    (evi-read-command-char)))
		      (defun (, do-function) (, (cdr params)) (,@ body)))))
	  ((eq (car params) '&string)
	    (` (progn (defun (, function) () (, documentation)
			(interactive)
			(evi-extend-continuation
			  'evi-after-string-arg
			  (list 'quote (quote (, do-function)))
			  (list 'quote (quote (, direction)))
			  (evi-adjust-count) (list 'quote evi-context))
			(evi-read-string (, (car (cdr params)))))
		      (defun (, do-function) (, (cdr (cdr params)))
			(,@ body)))))
	  (t
	    (` (progn (defun (, function) () (, documentation)
			(interactive)
			(evi-motion-command
			  (quote (, do-function)) (quote (, direction))
			  (evi-adjust-count) evi-context))
		      (defun (, do-function) (, params) (,@ body))))))))

(defun evi-after-string-arg (func dir count context)
  (evi-motion-command func dir count context evi-minibuf-contents))

(defmacro evi-iterate (count &rest body)
  (list 'let (list (list 'count count))
	  (append (list 'while (list '> 'count 0)) body
		  (list (list 'setq 'count (list '1- 'count))))
	  (list '= 'count 0)))

(defmacro evi-break ()
  (list 'setq 'count -1))

(defmacro evi-enumerate-condition (item list condition &rest body)
  (list 'let (list (list 'list list) (list item))
    (append
      (list 'while
	(list 'and 'list
	      (list 'progn (list 'setq item '(car list)) condition)))
      (if body
	(append body '((setq list (cdr list))))
	'((setq list (cdr list)))))
    'list))

(defmacro evi-iterate-list (item list &rest body)
  (list 'let (list (list 'list list) (list item) '(found))
    (append
      (list 'while 'list)
      (append (list (list 'setq item '(car list)))
	      body '((setq list (cdr list)))))))

(defmacro evi-find (item list pred)
  (list 'let (list (list 'list list) (list item) '(found))
    (list 'while
      (list 'and 'list
	    (list 'progn (list 'setq item '(car list) 'found pred)
			 '(not found)))
      '(setq list (cdr list)))
    'found))

(defmacro evi-set-goal-column ()
  (` (if evi-reset-goal-column
       (setq evi-goal-column (current-column)
	     evi-reset-goal-column nil))))

(defmacro evi-reset-goal-column ()
  (` (setq evi-reset-goal-column t)))

(defmacro evi-register-text (register)
  (list 'car register))

(defmacro evi-register-shape (register)
  (list 'cdr register))

;; Keymaps

(defun evi-define-key (maps key def)
  (evi-enumerate-condition map maps t
    (funcall 'define-key
	     (symbol-value (intern (concat "evi-" (symbol-name map) "-map")))
	     key def)))

(defun evi-define-macro (maps key macro)
  (evi-enumerate-condition map maps t
    (eval (list 'define-key
		(intern (concat "evi-" (symbol-name map) "-map")) 'key
		(list 'quote (list 'lambda ()
		  '(interactive) (list 'evi-internal-macro macro)))))))

(defun evi-make-local-keymap (keydefs)
  (let ((keymap (make-sparse-keymap)))
    (if (fboundp 'set-keymap-name)
	(set-keymap-name keymap 'evi-local))
    (mapcar '(lambda (keydef)
	       (define-key keymap (eval (car keydef)) (nth 1 keydef)))
	    keydefs)
    keymap))

(defun evi-unbound ()
  (interactive)
  (evi-error "Nothing bound to `%c'" last-command-char))

;					"\C-a"
(evi-define-key '(vi)			"\C-b" 'evi-scroll-page-backward)
(evi-define-key '(vi)			"\C-c" 'keyboard-quit)
(evi-define-key '(vi)			"\C-d" 'evi-scroll-text-forward)
(evi-define-key '(vi)			"\C-e" 'evi-scroll-cursor-forward)
(evi-define-key '(vi)			"\C-f" 'evi-scroll-page-forward)
(evi-define-key '(vi)			"\C-g" 'evi-file-info)
(evi-define-key '(vi motion)		"\C-h" 'evi-backward-char)
(evi-define-key '(vi)			"\C-i" 'evi-unbound)
(evi-define-key '(vi motion)		"\C-j" 'evi-next-line)
;					"\C-k"
(evi-define-key '(vi)			"\C-l" 'evi-redraw)
(evi-define-key '(vi motion)		"\C-m" 'evi-beginning-of-next-line)
(evi-define-key '(vi motion)		"\C-n" 'evi-next-line)
;					"\C-o"
(evi-define-key '(vi motion)		"\C-p" 'evi-previous-line)
;					"\C-q"
(evi-define-key '(vi)			"\C-r" 'evi-redraw)
;					"\C-s"
;					"\C-t"
(evi-define-key '(vi)			"\C-u" 'evi-scroll-text-backward)
;					"\C-v"
;					"\C-w"
;					"\C-x"
(evi-define-key '(vi)			"\C-y" 'evi-scroll-cursor-backward)
(evi-define-key '(vi)			"\C-z" 'suspend-emacs)
;					"\C-[" (ESC)
;					"\C-\"
(evi-define-key '(vi)			"\C-]" 'evi-tag)
(evi-define-key '(vi)			"\C-^" ":e#\n")

(evi-define-key '(vi motion)		" " 'evi-forward-char)
(evi-define-key '(vi)			"!" 'evi-shell-filter)
(evi-define-key '(vi)			"\"" 'evi-prefix-register)
(evi-define-key '(vi)			"#" 'evi-unbound)
(evi-define-key '(vi motion)		"$" 'evi-end-of-line)
(evi-define-key '(vi motion)		"%" 'evi-paren-match)
(evi-define-key '(vi)			"&" ":s\n")
(evi-define-key '(vi motion)		"'" 'evi-goto-mark-vertical)
(evi-define-key '(vi motion)		"(" 'evi-backward-sentence)
(evi-define-key '(vi motion)		")" 'evi-forward-sentence)
(evi-define-key '(vi)			"*" 'evi-send-to-process)
(evi-define-key '(vi motion)		"+" 'evi-beginning-of-next-line)
(evi-define-key '(vi motion)		"," 'evi-find-next-character-reverse)
(evi-define-key '(vi motion)		"-" 'evi-beginning-of-previous-line)
(evi-define-key '(vi)			"." 'evi-repeat)
(evi-define-key '(vi motion)		"/" 'evi-search-forward)
(evi-define-key '(vi motion)		"0" 'evi-digit-or-beginning-of-line)
(evi-define-key '(vi motion)		"1" 'evi-prefix-digit)
(evi-define-key '(vi motion)		"2" 'evi-prefix-digit)
(evi-define-key '(vi motion)		"3" 'evi-prefix-digit)
(evi-define-key '(vi motion)		"4" 'evi-prefix-digit)
(evi-define-key '(vi motion)		"5" 'evi-prefix-digit)
(evi-define-key '(vi motion)		"6" 'evi-prefix-digit)
(evi-define-key '(vi motion)		"7" 'evi-prefix-digit)
(evi-define-key '(vi motion)		"8" 'evi-prefix-digit)
(evi-define-key '(vi motion)		"9" 'evi-prefix-digit)
(evi-define-key '(vi)			":" 'evi-ex-command)
(evi-define-key '(vi motion)		";" 'evi-find-next-character)
(evi-define-key '(vi)			"<" 'evi-shift-left)
(evi-define-key '(vi)			"=" 'evi-indent)
(evi-define-key '(vi)			">" 'evi-shift-right)
(evi-define-key '(vi motion)		"?" 'evi-search-backward)
(evi-define-key '(vi)			"@" 'evi-register-macro)

(evi-define-macro '(vi)			"A" "$#a")
(evi-define-key '(vi motion)		"B" 'evi-backward-Word)
(evi-define-macro '(vi)			"C" "&c#$")
(evi-define-macro '(vi)			"D" "&d#$")
(evi-define-key '(vi motion)		"E" 'evi-end-of-Word)
(evi-define-key '(vi motion)		"F" 'evi-find-char-backwards)
(evi-define-key '(vi motion)		"G" 'evi-goto-line)
(evi-define-key '(vi motion)		"H" 'evi-goto-top-of-window)
(evi-define-macro '(vi)			"I" "^#i")
(evi-define-key '(vi)			"J" 'evi-join-lines)
(evi-define-key '(vi)			"K" 'evi-unbound)
(evi-define-key '(vi motion)		"L" 'evi-goto-bottom-of-window)
(evi-define-key '(vi motion)		"M" 'evi-goto-middle-of-window)
(evi-define-key '(vi motion)		"N" 'evi-search-next-reverse)
(evi-define-key '(vi)			"O" 'evi-open-before)
(evi-define-key '(vi)			"P" 'evi-put)
(evi-define-key '(vi)			"Q" 'evi-quit-evi)
(evi-define-key '(vi)			"R" 'evi-replace)
(evi-define-macro '(vi)			"S" "&c#c")
(evi-define-key '(vi motion)		"T" 'evi-find-char-backwards-after)
(if (boundp 'buffer-undo-list)
    (evi-define-key '(vi)		"U" 'evi-undo-line))
(evi-define-key '(vi)			"V" 'evi-unbound)
(evi-define-key '(vi motion)		"W" 'evi-forward-Word)
(evi-define-macro '(vi)			"X" "&d#h")
(evi-define-macro '(vi)			"Y" "&y#y")
(evi-define-key '(vi)			"Z" (cons 'prefix evi-Z-map))
(evi-define-key '(Z)			  "Z" ":Wq!\n")

(evi-define-key '(vi)			"[" (cons 'prefix evi-lbrack-map))
(evi-define-key '(motion)		"[" (cons 'prefix evi-lbrack-m-map))
(evi-define-key '(lbrack)		  "\"" 'evi-register-string)
(evi-define-key '(lbrack)		  "'" 'evi-register-char)
(evi-define-key '(lbrack lbrack-m)	  "(" 'evi-parameterized-macro)
(evi-define-key '(lbrack lbrack-m)	  "[" 'evi-backward-section)
(evi-define-key '(lbrack)		  "b" 'evi-buffer-name)
(evi-define-key '(lbrack)		  "u" 'evi-undo-more)
(evi-define-key '(lbrack)		  "{" 'evi-loop-over-lines-in-region)
(evi-define-key '(vi)			"\\" 'evi-unbound)
(evi-define-key '(vi motion)		"]" (cons 'prefix evi-rbrack-map))
(evi-define-key '(rbrack)		  "]" 'evi-forward-section)
(evi-define-key '(vi motion)		"^" 'evi-goto-indentation)
(evi-define-key '(vi)			"_" 'evi-prompt-repeat)
(evi-define-key '(vi motion)		"`" 'evi-goto-mark-horizontal)

(evi-define-key '(vi)			"a" 'evi-insert-after)
(evi-define-key '(vi motion)		"b" 'evi-backward-word)
(evi-define-key '(vi)			"c" 'evi-change)
(evi-define-key '(vi)			"d" 'evi-delete)
(evi-define-key '(vi motion)		"e" 'evi-end-of-word)
(evi-define-key '(vi motion)		"f" 'evi-find-character)
(evi-define-key '(vi)			"g" 'evi-unbound)
(evi-define-key '(vi motion)		"h" 'evi-backward-char)
(evi-define-key '(vi)			"i" 'evi-insert)
(evi-define-key '(vi motion)		"j" 'evi-next-line)
(evi-define-key '(vi motion)		"k" 'evi-previous-line)
(evi-define-key '(vi motion)		"l" 'evi-forward-char)
(evi-define-key '(vi)			"m" 'evi-set-mark)
(evi-define-key '(vi motion)		"n" 'evi-search-next)
(evi-define-key '(vi)			"o" 'evi-open-after)
(evi-define-key '(vi)			"p" 'evi-put-after)
(evi-define-key '(vi)			"q" 'evi-unbound)
(evi-define-key '(vi)			"r" 'evi-replace-char)
(evi-define-macro '(vi)			"s" "&c#l")
(evi-define-key '(vi motion)		"t" 'evi-find-character-before)
(evi-define-key '(vi)			"u" 'evi-undo)
(evi-define-key '(vi)			"v" 'evi-unbound)
(evi-define-key '(vi motion)		"w" 'evi-forward-word)
(evi-define-macro '(vi)			"x" "&d#l")
(evi-define-key '(vi)			"y" 'evi-yank)
(evi-define-key '(vi)			"z" 'evi-window-control)

(evi-define-key '(vi motion)		"{" 'evi-backward-paragraph)
(evi-define-key '(vi motion)		"|" 'evi-goto-column)
(evi-define-key '(vi motion)		"}" 'evi-forward-paragraph)
(evi-define-key '(vi)			"~" 'evi-toggle-case)

(evi-define-key '(param)		"&" 'evi-register-parameter)
(evi-define-key '(param)		"#" 'evi-prefix-count-parameter)

(evi-define-key '(motion)		"a" 'evi-region-arbitrary)
(evi-define-key '(motion)		"m" 'evi-region-mouse)
(evi-define-key '(motion)		"r" 'evi-region-rectangle)
(evi-define-key '(motion)		"R" 'evi-region-rows)
(evi-define-key '(motion)		"C" 'evi-region-columns)

; ZZ should define for replace mode also?
(evi-define-key '(input) "\C-q" 'evi-quoted-insert)
(evi-define-key '(input) "\C-v" 'evi-quoted-insert)

(evi-define-key '(input replace) "\C-c" 'evi-keyboard-quit)
(evi-define-key '(input replace) "\e" 'evi-resume-continuation)

(evi-define-key '(insert) "\C-d" 'evi-backward-indent)
(evi-define-key '(insert) "\C-h" 'evi-insert-mode-delete-backward-char)
(evi-define-key '(insert) "\C-j" 'evi-newline)
(evi-define-key '(insert) "\C-m" 'evi-newline)
(evi-define-key '(insert) "\C-t" 'evi-forward-indent)
(evi-define-key '(insert) "\C-u" 'evi-insert-mode-kill-line)
(evi-define-key '(insert) "\C-w" 'evi-insert-mode-delete-backward-word)
(evi-define-key '(insert) "\C-x" 'evi-insert-mode-kill-line)
(evi-define-key '(insert) "\177" 'evi-insert-mode-delete-backward-char)

(evi-define-key '(replace) "\C-d" 'evi-replace-mode-backward-indent)
(evi-define-key '(replace) "\C-h" 'evi-replace-mode-delete-backward-char)
;(evi-define-key (replace) "\C-t" 'evi-forward-indent)
(evi-define-key '(replace) "\C-u" 'evi-replace-mode-kill-line)
(evi-define-key '(replace) "\C-w" 'evi-replace-mode-delete-backward-word)
(evi-define-key '(replace) "\C-x" 'evi-replace-mode-kill-line)
(evi-define-key '(replace) "\177" 'evi-replace-mode-delete-backward-char)

(evi-define-key '(read-string ex)
			    "\C-h" 'evi-backward-char-maybe-abort)
(evi-define-key '(read-string-input ex-input)
			    "\C-h" 'evi-delete-backward-char-maybe-abort)
(evi-define-key '(read-string read-string-input ex ex-input)
			    "\C-j" 'evi-exit-minibuf)
(evi-define-key '(read-string read-string-input ex ex-input)
			    "\C-m" 'evi-exit-minibuf)
(evi-define-key '(read-string read-string-input ex ex-input)
			    "\e" 'evi-exit-minibuf)
(evi-define-key '(read-string ex)
			    "\177" 'evi-backward-char-maybe-abort)
(evi-define-key '(read-string-input ex-input)
			    "\177" 'evi-delete-backward-char-maybe-abort)

(evi-define-key '(ex-input) "\C-i" 'ex-complete)

(evi-define-key '(shell) "\C-m" 'evi-shell-send-input)

(defun evi-init-special-keys ()
(cond
  ((eq evi-emacs-version 'emacs19)
    (ex-map nil [left] "h")
    (ex-map nil [right] "l")
    (ex-map nil [up] "k")
    (ex-map nil [down] "j")

    ;(defun evi-top-level-event (e)
    ;  (setq last-command-event e)
    ;  (evi-top-level))

    (defun evi-posn-buffer (position)
      "Return the buffer of the window in POSITION."
      (let ((window (posn-window position)))
	(and (integer-or-marker-p (posn-point position))
	     window (window-buffer window))))

    (defun evi-mouse-drag-text (event handler)
      "\
Highlight text dragged by mouse; the event described by the highlight text
is passed to HANDLER, as is ARG (if provided)."
      (let ((frame (selected-frame))
	    (start (posn-point (event-start event)))
	    (overlay (make-overlay 1 1 (evi-posn-buffer (event-start event)))))
	(track-mouse
	  (overlay-put overlay 'face 'region)
	  (while
	      (cond
	       ((not (listp (setq event (read-event)))))
	       ((eq (car event) 'switch-frame)
		(if (eq (selected-frame) frame)
		    (move-overlay overlay start start))
		t)
	       ((mouse-movement-p event)
		(let ((end (posn-point (event-start event))))
		  (if (and (integer-or-marker-p end)
			   (eq (window-frame (posn-window
					      (event-start event)))
			       frame)
			   (eq (evi-posn-buffer (event-start event))
			       (overlay-buffer overlay)))
		      (move-overlay overlay start end)
		    (move-overlay overlay start start)))
		t))))
	(prog1
	    (funcall handler event)
	  (or unread-command-events (sit-for 9999))
	  (delete-overlay overlay))))

    (defun evi-mouse-select-dragged-text (event)
      "Place the dragged region into the mouse selection.
Return the string in that region for insertion by the caller.
If EVENT is a click rather than a drag, invoke the appropriate binding.
This is intended for a button-down binding."
      (let ((string "")
	    (window (posn-window (event-start event)))
	    (region (list (posn-point (event-start event))
			  (posn-point (event-end event)))))
	(if (eq (event-start event) (event-end event))
	    (evi-mouse-set-point event)
;	    (setq unread-command-events
;		  (append unread-command-events (list event)))
	  (if (eq (evi-posn-buffer (event-start event))
		  (evi-posn-buffer (event-end event)))
	      (save-excursion
		(save-window-excursion
		  (let ((buffer (window-buffer (selected-window)))
			(point (point)))
		    (select-window window)
		    (setq string (apply 'buffer-substring region))
		    (if (eq window-system 'x)
			(x-set-selection nil string)))))))
	string))

    (defun evi-mouse-copy-dragged-text (event)
      "Copy the dragged text to the mouse selection."
      (interactive "e")
      (evi-mouse-drag-text event 'evi-mouse-select-dragged-text))

    (defun evi-mouse-paste-selection (event)
      "Insert the X selection at the click point."
      (interactive "e")
      (if (eq window-system 'x)
	  (progn (evi-push-macro (x-selection))
		 (while evi-current-macro
		   (evi-do-command)))))

    (defun evi-mouse-set-point (event)
      "Move point to the position clicked on with the mouse."
      (interactive "e")
      ;; Use event-end in case called from mouse-drag-region.
      ;; If EVENT is a click, event-end and event-start give same value.
      (let ((posn (event-end event)))
	(and (window-minibuffer-p (posn-window posn))
	     (not (minibuffer-window-active-p (posn-window posn)))
	     (error "Minibuffer window is not active"))
	(select-window (posn-window posn))
	(if (numberp (posn-point posn))
	    (progn (goto-char (posn-point posn))
		   (evi-fixup-cursor 'vertical)))))

    (define-key evi-top-level-map [down-mouse-1] 'evi-mouse-copy-dragged-text)
    (define-key evi-top-level-map [drag-mouse-1] 'evi-mouse-copy-dragged-text)
    (define-key evi-top-level-map [mouse-1] 'evi-mouse-set-point)
    (define-key evi-top-level-map [mouse-2] 'evi-mouse-paste-selection)

; this would be better, but causes problems:
;    (define-key evi-top-level-map [mouse-1] 'evi-top-level-event)
;    (evi-define-key '(vi motion) [mouse-1] 'evi-mouse-set-point)
    )

  ((eq evi-emacs-version 'lucid19)
    ;; must find out how/if this interacts with the definition of ESC
    (let ((maps '(vi motion)))
      (evi-define-key maps 'down	  'evi-next-line)
      (evi-define-key maps 'up	  'evi-previous-line)
      (evi-define-key maps 'left	  'evi-backward-char)
      (evi-define-key maps 'right	  'evi-forward-char)

      (evi-define-key maps 'button1 'evi-mouse-track)
      (evi-define-key maps 'button2 'evi-x-set-point-and-insert-selection)
      (evi-define-key maps '(control button1) 'evi-mouse-track-insert)
      (evi-define-key maps '(control button2) 'evi-x-mouse-kill))

    (defun evi-mouse-track (event)
      (interactive "e")
      (mouse-track event)
      (evi-fixup-cursor 'vertical))

    (defun evi-mouse-track-insert (event)
      (interactive "e")
      (mouse-track-insert event)
      (evi-fixup-cursor 'vertical))

    (defun evi-x-mouse-kill (event)
      (interactive "e")
      (x-mouse-kill event)
      (evi-fixup-cursor 'vertical))

    (defun evi-x-set-point-and-insert-selection (event)
      (interactive "e")
      (x-set-point-and-insert-selection event)
      (evi-fixup-cursor 'vertical)))
  (t
    ;; else version 18
    (define-key function-keymap "l" 'evi-backward-char)
    (define-key function-keymap "r" 'evi-forward-char)
    (define-key function-keymap "u" 'evi-previous-line)
    (define-key function-keymap "d" 'evi-next-line)))
)

;; Command macros

(defun evi-parameterized-macro ()
  (interactive)
  (evi-extend-continuation 'evi-parameterized-macro-after)
  (evi-read-string "\("))

(defun evi-parameterized-macro-after ()
  (evi-push-macro evi-minibuf-contents
		  'evi-parameterized-macro-after-after
		  evi-register-parameter evi-prefix-count-parameter
		  evi-keymap-list)
  (setq evi-register-parameter evi-register-spec
	evi-register-spec nil
	evi-prefix-count-parameter evi-prefix-count
	evi-prefix-count nil
	evi-keymap-list (cons evi-param-map evi-keymap-list)))

(defun evi-parameterized-macro-after-after (reg-spec count keymap-list)
  (setq evi-register-parameter reg-spec
	evi-prefix-count-parameter count
	evi-keymap-list keymap-list))

(defvar evi-internal-macro-keys (make-string 32 0))

(defun evi-internal-macro (macro)
  (evi-push-macro macro
		  'evi-internal-after evi-parameterized-macro evi-remap
		  evi-register-spec evi-prefix-count
		  evi-command-keys evi-command-keys-index)
  (setq evi-register-parameter evi-register-spec
	evi-register-spec nil
	evi-prefix-count-parameter evi-prefix-count
	evi-prefix-count nil
	evi-parameterized-macro t
	evi-remap nil
	evi-command-keys evi-internal-macro-keys
	evi-command-keys-index 0))

(defun evi-internal-after (param-flag remap-flag reg count command-keys index)
  (setq evi-parameterized-macro param-flag
	evi-remap remap-flag
	evi-register-spec reg
	evi-prefix-count count
	evi-command-keys command-keys
	evi-command-keys-index index)
  (evi-save-command-keys))

(defun evi-register-macro (char &optional count)
  (interactive (evi-character-arg))
  (let* ((evi-last-command-keys nil)
	 (register-number (evi-register-number char))
	 (macro (evi-register-text (aref evi-registers register-number))))
    (setq evi-last-macro-register register-number)
    (evi-push-macro macro)
    (evi-get-command)))

;; And now we have to do our own keyboard macros...  emacs `keyboard' macros
;; don't cut it as they don't believe in hierarchical commands - the macro
;; has to terminate at the same lisp execution depth as it started.  This
;; is OK for emacs 'cause emacs commands don't build on each other like vi
;; commands do.  If anyone has any idea of how to make emacs `keyboard' macros
;; behave in a manner independent of their execution context, please let me
;; know.
(defvar evi-unread-command-char nil)
(defvar evi-macro-stack nil)
(defvar evi-current-macro nil)
(defvar evi-current-macro-index nil)

(defun evi-execute-macro (macro)
  (evi-push-macro macro)
  (while evi-current-macro
    (evi-get-command)))

; ZZ
(defvar evi-minibuf-prompt nil)
(defvar evi-minibuf nil)
(defvar evi-minibuf-prev nil)
(defvar evi-minibuf-user-window nil)

; needn't mess with the display if we're just in a macro...

(defun evi-read-string (prompt &optional initial vi-map input-map)
  (if evi-in-minibuf
      (evi-error "Can't use minibuffer inside minibuffer"))
  ;; this whole thing seems unduly complicated...
  (set-buffer (get-buffer-create (concat " *evi-" prompt "-Minibuf*")))
  ;; we want undo information anyway (despite the space in the buffer name)
  (if (eq buffer-undo-list t)
      (setq buffer-undo-list nil))
  (evi)
  (evi-set-continuation 'evi-read-string-after)
  (evi-mark-continuation)
  (evi-push-continuation 'evi-standard-continuation)
  (setq evi-minibuf-prompt prompt
	evi-minibuf (current-buffer)
	evi-minibuf-prev (window-buffer (minibuffer-window))
	evi-minibuf-user-window (selected-window))
  ;; doesn't seem to work (or evi-current-macro ...)
  (set-window-buffer (minibuffer-window) (current-buffer))
  (select-window (minibuffer-window))
  (setq evi-in-minibuf t)
  (goto-char (point-max))
  (if (eq evi-emacs-version 'emacs18)
      (display-buffer (current-buffer))
    (message nil))
  (insert prompt)
  ;;ZZ would be best if insert point could be set here
  (if initial
      (insert initial))
  (setq evi-buffer-local-vi-map (or vi-map evi-read-string-map)
	evi-minibuf-input-map (or input-map evi-read-string-input-map))
  (evi-insert))

(defun evi-exit-minibuf-window ()
  (goto-char (1- (point-max)))
  (beginning-of-line)
  ;; ZZ - kludge-check for "[{" and escape the `['
  (if (looking-at (concat (if (= (aref evi-minibuf-prompt 0) ?\[) "\\")
			  evi-minibuf-prompt " *$"))
      (delete-region (point) (point-max))
    (goto-char (point-max)))
  (set-buffer evi-minibuf)
  (set-window-buffer (minibuffer-window) evi-minibuf-prev)
  (select-window evi-minibuf-user-window)
  (setq evi-in-minibuf nil))

(defun evi-read-string-after ()
  (setq evi-minibuf-contents
	(buffer-substring (+ (point) (length evi-minibuf-prompt))
			  (progn (end-of-line) (point))))
  (evi-exit-minibuf-window)
  (evi-pop-continuation))

(defun evi-exit-minibuf ()
  (interactive)
  (beginning-of-line)
  (evi-sit-for 0)
  (evi-unwind-continuations))

(defun evi-abort-minibuf ()
  (evi-exit-minibuf-window)
  (evi-discard-continuations)
  (evi-push-continuation 'evi-standard-continuation))

(cond ((eq evi-emacs-version 'lucid19)
	(defun evi-read-char ()
	  (if evi-unread-command-char
	      (prog1 evi-unread-command-char
		(setq evi-unread-command-char nil))
	    (or (evi-read-macro)
		(let ((event (allocate-event)))
		  (while (progn
			   (next-event event)
			   (not (key-press-event-p event)))
		    (dispatch-event event))
		  (or (event-to-character event t t)
		      (event-key event)))))))
      ((eq evi-emacs-version 'emacs19)
	(defun evi-read-char ()
	  (if evi-unread-command-char
	      (prog1 evi-unread-command-char
		(setq evi-unread-command-char nil))
	    (or (evi-read-macro)
		(let ((ev (read-event)))
		  (or (and (symbolp ev)
			   (get (car (get ev 'event-symbol-element-mask))
				'ascii-character))
		      ev))))))
      (t
	(defun evi-read-char ()
	  (if evi-unread-command-char
	      (prog1 evi-unread-command-char
		(setq evi-unread-command-char nil))
	    (or (evi-read-macro)
		(read-char))))))

(defun evi-push-macro (macro &optional after &rest args)
  (setq evi-macro-stack (cons (cons (cons evi-current-macro
					  evi-current-macro-index)
				    (cons after args))
			      evi-macro-stack)
	evi-current-macro macro
	evi-current-macro-index 0))

(defun evi-pop-macro ()
  (let ((after (cdr (car evi-macro-stack))))
    (setq evi-current-macro (car (car (car evi-macro-stack)))
	  evi-current-macro-index (cdr (car (car evi-macro-stack)))
	  evi-macro-stack (cdr evi-macro-stack))
    (if (car after)
	(apply (car after) (cdr after)))))

(defun evi-discard-macros ()
  (setq evi-current-macro nil
	evi-macro-stack nil))

(defun evi-read-macro ()
  (while (and evi-current-macro
	      (= evi-current-macro-index (length evi-current-macro)))
    (evi-pop-macro))
  (if evi-current-macro
      (prog1 (aref evi-current-macro evi-current-macro-index)
	(setq evi-current-macro-index (1+ evi-current-macro-index)))))

(defun evi-sit-for (count)
  (if (or evi-unread-command-char
	  (and evi-current-macro
	       (/= evi-current-macro-index (length evi-current-macro))))
      nil
    (sit-for count)))

(defun evi-register-parameter ()
  (interactive)
  (setq evi-register-spec evi-register-parameter)
  (evi-push-continuation 'evi-prompt))

(defun evi-prefix-count-parameter ()
  (interactive)
  (setq evi-prefix-count evi-prefix-count-parameter)
  (evi-push-continuation 'evi-prompt))

;; Errors

(defun evi-error (&rest args)
  (throw 'abort (apply 'format args)))

(defun evi-warning (&rest args)
  (if evi-interactive
      (progn
	(evi-discard-continuations)
	(evi-push-continuation 'evi-standard-continuation)
	(throw 'abort (apply 'format args)))
    (princ (apply 'format args))
    (terpri)))

;; Continuations

(defvar evi-debug-cont nil)

; ZZ need this be buffer specific??  yes!!!
(evi-defbuffervar evi-continuation-stack nil
  "The continuation stack.")

(defun evi-set-continuation (func &rest args)
  (setq evi-continuation-stack (cons (cons func args) nil)))

(defun evi-push-continuation (func &rest args)
  (setq evi-continuation-stack (cons (cons func args) evi-continuation-stack))
  (if evi-debug-cont (evi-db (concat "{PU:" (prin1-to-string evi-continuation-stack) "}\n")))
  )

(defun evi-extend-continuation (func &rest args)
  (setq evi-continuation-stack
	(if (eq (car evi-continuation-stack) 'suspend)
	    (cons (cons func args) evi-continuation-stack)
	  (cons (list 'progn (cons func args) (car evi-continuation-stack))
		(cdr evi-continuation-stack))))
  (if evi-debug-cont (evi-db (concat "{EX:" (prin1-to-string evi-continuation-stack) "}\n")))
  )

(defun evi-suspend-continuation ()
  (setq evi-continuation-stack (cons 'suspend evi-continuation-stack))
  (if evi-debug-cont (evi-db (concat "{SU:" (prin1-to-string evi-continuation-stack) "}\n")))
  )

(defun evi-resume-continuation ()
  (interactive)
  (setq evi-continuation-stack (cdr evi-continuation-stack))
  (if evi-debug-cont (evi-db (concat "{RE:" (prin1-to-string evi-continuation-stack) "}\n")))
  )

(defun evi-mark-continuation ()
  (setq evi-continuation-stack (cons 'mark evi-continuation-stack))
  (if evi-debug-cont (evi-db (concat "{MA:" (prin1-to-string evi-continuation-stack) "}\n")))
  )

(defun evi-unwind-continuations ()
  (while (not (eq (car evi-continuation-stack) 'mark))
    (or evi-continuation-stack
	(evi-error "No continuation mark found!"))
    (setq evi-continuation-stack (cdr evi-continuation-stack)))
  (setq evi-continuation-stack (cdr evi-continuation-stack)))

(defun evi-reset-continuations ()
  (while (and evi-continuation-stack
	      (not (eq (car evi-continuation-stack) 'mark)))
    (setq evi-continuation-stack (cdr evi-continuation-stack))))

(defun evi-pop-continuation ()
  (if evi-debug-cont (evi-db (concat "{PO:" (prin1-to-string evi-continuation-stack) "}\n")))
  (or (eq (car evi-continuation-stack) 'suspend)
      (eval (prog1 (car evi-continuation-stack)
		   (setq evi-continuation-stack
			 (cdr evi-continuation-stack))))))

(defun evi-discard-continuation ()
  (if evi-debug-cont (evi-db (concat "{DI:" (prin1-to-string evi-continuation-stack) "}\n")))
  (setq evi-continuation-stack (cdr evi-continuation-stack)))

(defun evi-discard-continuations ()
  (if evi-debug-cont (evi-db (concat "{DS:" (prin1-to-string evi-continuation-stack) "}\n")))
  (while (and evi-continuation-stack
	      (not (eq (car evi-continuation-stack) 'suspend)))
    (setq evi-continuation-stack (cdr evi-continuation-stack)))
  (if evi-continuation-stack
      (setq evi-continuation-stack (cdr evi-continuation-stack))))

;; Top level

(defun evi-top-level ()
  (interactive)
  ;; ZZ
  (if (null evi-continuation-stack)
      (progn (message "Glitch!")
	     (evi-push-continuation 'evi-standard-continuation)))
  (if (eq evi-emacs-version 'emacs19)
      (setq evi-unread-command-char last-command-event)
    (setq evi-unread-command-char last-command-char))
  (evi-do-command)
  (while evi-current-macro
    (evi-do-command)))

(defvar evi-delayed-buffer-change-request nil)

; ZZ needs to be initialized at beginning of emulation in any given buffer
(defun evi-standard-continuation ()
  ;; do some preemptive finishing up of macros to get the execution time
  ;; of any `macro-afters' to happen at the right time
  (while (and evi-current-macro
	      (= evi-current-macro-index (length evi-current-macro)))
    (evi-pop-macro))
  (setq evi-prompted nil
	evi-prefix-count nil
	evi-register-spec nil)
  (evi-erase-keys)
  (evi-push-continuation 'evi-standard-continuation)
  (if evi-delayed-buffer-change-request
      (progn
	(if (eq (car evi-delayed-buffer-change-request) 'other)
	    (switch-to-buffer-other-window
	     (car (cdr evi-delayed-buffer-change-request)))
	  (switch-to-buffer (car (cdr evi-delayed-buffer-change-request))))
	(eval (cdr (cdr evi-delayed-buffer-change-request)))
	(setq evi-delayed-buffer-change-request nil))))

(defun evi-prompt ()
  (and (not evi-in-minibuf) (evi-sit-for 1)
       (progn (message "%s -" (evi-prompt-keys-description))
	      (setq evi-prompted t))))

(defun evi-do-command ()
  (let ((message (if evi-debug
		     (catch 'abort
		       (evi-get-command)
		       (evi-pop-continuation)
		       nil)
		   (condition-case code
		       (catch 'abort
			 (evi-get-command)
			 (evi-pop-continuation)
			 nil)
		     (error
		       (evi-reset-top-level)
		       (signal (car code) (cdr code)))
		     (quit
		       (evi-reset-top-level)
		       (signal (car code) (cdr code)))))))
    (if message
	(progn (if (eq message t)
		   nil
		 (evi-soft-reset)
		 (if evi-error-bell (beep))
		 (or evi-in-minibuf
		     (message message)))
	       (evi-fixup-cursor 'horizontal)))))

(defun evi-reset-top-level ()
;  may need code to bail out of a `change' gracefully (cleaning up the $)?
  (if evi-in-minibuf
      (evi-abort-minibuf))
  (setq evi-mode 'vi
	evi-context nil
	evi-prefix-count nil
	evi-prefix-count-multiplier nil
	evi-register-spec nil
	evi-prompted nil
	evi-overstruck-char nil
	evi-keymap-list evi-vi-keymap-list)
  (evi-change-mode-id "Vi")
  (evi-refresh-mode-line)
  (evi-erase-keys)
  (evi-discard-macros)
  (evi-set-continuation 'evi-standard-continuation)
  (evi-fixup-cursor 'horizontal))

; ZZ will this work properly if in the middle of an internal macro?
(defun evi-soft-reset ()
  (setq evi-mode 'vi
	evi-context nil
	evi-prefix-count nil
	evi-prefix-count-multiplier nil
	evi-register-spec nil
	evi-prompted nil
	evi-overstruck-char nil
	evi-keymap-list evi-vi-keymap-list)
  (or evi-in-minibuf
      (progn (evi-change-mode-id "Vi")
	     (evi-refresh-mode-line)))
  (evi-erase-keys)
  (evi-reset-continuations)
  (evi-push-continuation 'evi-standard-continuation)
  (evi-fixup-cursor 'horizontal))

(defun evi-emacs-command ()
  (interactive)
  (evi-unread-command-char last-command-char)
  (let ((evi-keymap-list
	 (if (boundp 'minor-mode-map-alist)
	     (list (cons 'emacs (cons 'local nil))
		   (cons 'emacs (cons 'global nil)))
	   (if evi-emacs-local-map
	       (list evi-emacs-local-map (current-global-map))
	     (list (current-global-map))))))
    (if evi-global-directory
	(setq default-directory (evi-current-directory)))
    (evi-get-command)))

(defun evi-db (string)
  (let ((buf (current-buffer))
	(dbuf (get-buffer-create "*debug*")))
    (or (eq buf dbuf)
	(progn
	  (set-buffer dbuf)
	  (insert string)
	  (set-buffer buf)))))

(defun evi-get-command ()
  (let* ((inhibit-quit t)
	 (char (evi-read-command-char))
	 (keys (if (integerp char) (char-to-string char) (vector char)))
	 (keydef))
    (evi-enumerate-condition keymap evi-keymap-list
      (progn
	(cond
	  ((keymapp keymap)
	    (setq keydef (lookup-key keymap keys)))
	  ((symbolp keymap)
	    (setq keydef (lookup-key (symbol-value keymap) keys)))
	  ;; otherwise, we have a conditional keymap - the car is an
	  ;; identifying symbol and the cdr is the keymap
	  ((eq (car keymap) 'param)
	    (if evi-parameterized-macro
		(setq keydef (lookup-key (cdr keymap) keys))
	      (setq keydef nil)))
	  ((eq (car keymap) 'map)
	    (if (or evi-remap (not evi-current-macro))
		(setq keydef (lookup-key (cdr keymap) keys))
	      (setq keydef nil)))
	  ;; otherwise, we have an emacs keymap
	  ((or evi-enable-emacs-commands (eq (car keymap) 'emacs))
	    ;; we now have a pair where the car indicates local or global
	    ;; and the cdr is a list of chars that we want to pass-thru
	    ;; this keymap.  also we map our meta-prefix to emacs' meta-prefix
	    (if (memq (aref keys 0) (cdr (cdr keymap)))
		(setq keydef nil)
	      (setq keydef
		(lookup-key (if (eq (car (cdr keymap)) 'global)
				(current-global-map)
			      (current-local-map))
			    ;; must use `eq' because key may be a symbol
			    (if (eq (aref keys 0) evi-meta-prefix-char)
				(if (eq evi-emacs-version 'lucid19)
				    (if (= (length keys) 1)
					(setq char (+ (evi-read-command-char)
						      128)
					      keys (char-to-string char))
				      (concat
				       (char-to-string (+ (aref keys 1) 128))
				       (substring keys 2)))
				  (concat
				    (char-to-string evi-emacs-meta-prefix-char)
				    (substring keys 1)))
			      keys)))))
	  (t
	    (setq keydef nil)))
	(while
	  (cond ((keymapp keydef)
		  (if (and evi-timeout (evi-sit-for 1))
		      (setq keydef nil)
		    (setq char (evi-read-command-char)
			  keys (concat keys (char-to-string char))
			  keydef (lookup-key keydef (char-to-string char))))
		  t)
		((stringp keydef)
		  (if evi-prompted (message ""))
		  (setq last-command-char char
			last-command-event char
			evi-prompted nil)
		  (evi-push-macro
		    keydef
		    (function (lambda (lck)
				(setq evi-last-command-keys lck)))
		    evi-last-command-keys)
		  nil)
		((vectorp keydef)
		  ;; A function-key-map binding, i.e. we found a sequence
		  ;; associated with some function key.  Set the char to
		  ;; the indicated function key, and start over.
		  (setq char (aref keydef 0)
			keys keydef
			;; a hack - reset the loop to the beginning
			;; of the keymap list
			list (cons nil evi-keymap-list)
			keydef nil)
		  nil)
		((commandp keydef)
		  (if evi-prompted (message ""))
		  (setq last-command-char char
			last-command-event char
			evi-prompted nil
			quit-flag nil
			inhibit-quit nil)
		  (call-interactively keydef)
		  nil)
		((and (consp keydef) (eq (car keydef) 'prefix))
		  (evi-extend-continuation (function
					     (lambda (kl)
					       (setq evi-keymap-list kl)))
					   (list 'quote evi-keymap-list))
		  (setq evi-keymap-list (list (cdr keydef)))
		  (evi-push-continuation 'evi-prompt)
		  nil)
		((numberp keydef)
		  ;; the input almost matched a longer prefix in a
		  ;; previous keymap
		  (evi-push-macro (substring keys keydef))
		  (setq char (aref keys (1- keydef))
			keys (substring keys 0 keydef)
			keydef (lookup-key keymap keys)))
		(t
		  (setq keydef nil))))
	(not keydef)))
    (or keydef (progn (if evi-error-bell (beep))
		      ;; ZZ!
		      ;; (if (= char ?\C-z) (kill-emacs))
		      (evi-error "Unknown command `%s'"
				 (evi-keys-description))))))

(defun evi-read-command-char ()
  (let ((char (evi-read-char)))
    (or evi-current-macro
	;; ZZ punt for now...
	(if (integerp char)
	    (evi-add-key char)
	  (evi-add-key ?#)))
    char))

(defun evi-unread-command-char (char)
  (setq evi-unread-command-char char)
  (evi-lose-key))

;; Interactive args

(defun evi-count-arg ()
  (list evi-prefix-count))

(defun evi-register-args ()
  (list (car evi-register-spec) (cdr evi-register-spec) evi-prefix-count))

(defun evi-character-arg ()
  (list (evi-read-command-char) evi-prefix-count))

(defun evi-context-arg ()
  (list evi-context))

;; Mode line

(defvar evi-mode-line-format " Evi:%-6s")

(defun evi-in-mode-line-p (var)
  (if (listp mode-line-buffer-identification)
      (memq var mode-line-buffer-identification)
    nil))

(defun evi-install-in-mode-line (var)
  (or (evi-in-mode-line-p var)
      (setq mode-line-buffer-identification
	    (if (listp mode-line-buffer-identification)
		(append mode-line-buffer-identification (list var))
	      (cons mode-line-buffer-identification (list var))))))

(defun evi-deinstall-from-mode-line (var)
  (if (evi-in-mode-line-p var)
      (setq mode-line-buffer-identification
	    (evi-filter (function (lambda (mode-var) (not (eq var mode-var))))
			mode-line-buffer-identification))))

(defun evi-change-mode-id (string)
  "Change Evi's mode identification string to STRING."
  (setq evi-mode-string (format evi-mode-line-format string)))

(defun evi-refresh-mode-line ()
  "Redraw mode line."
  (set-buffer-modified-p (buffer-modified-p)))

;; Initializing

(defun evi-initialize ()
  (setq evi-initialized t
	evi-directory-stack (list default-directory)
	evi-emacs-meta-prefix-char meta-prefix-char)
  (evi-init-special-keys)
  (if (eq evi-emacs-version 'emacs18)
      (let ((temp-buffer-show-hook 'evi-startup-show-hook))
	(with-output-to-temp-buffer "*Startup*"
	  (evi-customize)))
    (let ((temp-buffer-show-function 'evi-startup-show-hook))
      (with-output-to-temp-buffer "*Startup*"
	(evi-customize))))
  (if (boundp 'minor-mode-map-alist)
      (setq minor-mode-map-alist
	    (cons (cons 'evi-enabled evi-top-level-map) minor-mode-map-alist)))
  (or evi-meta-prefix-char
      (setq evi-meta-prefix-char evi-emacs-meta-prefix-char))
  (evi-welcome-message))

(defun evi-startup-show-hook (buf)
  (let ((curbuf (current-buffer)))
    (set-buffer buf)
    (if (not (and (bobp) (eobp)))
	(progn
	  (goto-char (point-min))
	  (insert "The following problems were found at startup:\n")
	  (display-buffer buf)
	  (message "Use `z1=' to show only `%s'" (buffer-name curbuf))))
    (set-buffer curbuf)))

(defun evi-customize ()
  ; mimic emacs startup behaviour:
  ;   if su'd, use effective login name to find startup files (??)
  (let* ((user-name (user-login-name))
	 (home (if (string= user-name (user-real-login-name))
		 "~"
		 (concat "~" user-name))))
    (if (file-readable-p "~/.evirc") (load-file (concat home "/.evirc")))
    (if (file-readable-p ".evirc")
	(load-file ".evirc"))
    (let* ((evi-interactive nil)
	   (source)
	   (message (catch 'abort
		      (or evi-supress-ex-startup
			  (progn
			    (setq source "~/.exrc")
			    (evi-do-ex-command-file (concat home "/.exrc"))))
		      (setq source "~/.exrc.evi")
		      (evi-do-ex-command-file (concat home "/.exrc.evi"))
		      (or evi-supress-ex-startup
			  (progn
			    (if evi-local-exrc
				(progn (setq source ".exrc")
				       (evi-do-ex-command-file ".exrc")))
			    (setq source "EXINIT")
			    (let ((exinit (getenv "EXINIT")))
			      (if exinit
				  (evi-do-ex-command-string exinit)))))
		      (if evi-local-exrc
			  (progn (setq source ".exrc.evi")
				 (evi-do-ex-command-file ".exrc.evi")))
		      (setq source "EVIINIT")
		      (let ((exinit (getenv "EVIINIT")))
			(if exinit
			    (evi-do-ex-command-string exinit)))
		      nil)))
      (if message
	  (progn
	    (beep)
	    (if (not (y-or-n-p (concat "Error in " source
				 (if (eq message t) "" (concat ": " message))
				 ". Continue? ")))
		(kill-emacs)))))))

(defun evi-welcome-message ()
  (and (not evi-inhibit-startup-message) (not noninteractive)
       (eq (current-buffer) (get-buffer "*scratch*"))
       (not (input-pending-p))
       (progn
	 (insert evi-version ".  " evi-copyright ".

Evi is an enhanced emulator for Vi that runs in an emacs environment.
If you are familiar with vi, you should have very few problems using Evi.
By default, Evi is setup to emulate vi as closely as it can; however,
there are a number of new commands (mostly `:' commands) that support
the multiple window/multiple buffer environment that emacs provides.
In addition, file/buffer/command/etc completion is supported for `:'
commands (use `TAB' to complete).

Type `Q' to exit Evi (back to emacs).  Type `ZZ' to save *all* modified
files and exit emacs.  Type `:w' to save the current file, and `:W' to
save all modified files.  Type `:q' to quit "
		 (cond ((eq evi-emacs-version 'emacs18)
			 "emacs.")
		       ((eq evi-emacs-version 'emacs19)
			 "the current frame, and
quit emacs if only one frame exists.")
		       ((eq evi-emacs-version 'lucid19)
			 "the current screen, and
quit emacs if only one screen exists.")) "

You may be interested in the following enhancements, which are disabled
by default: using emacs commands, command line editing, backslash escapes
(such as `\\n') and disabling this message(!).  For more information, consult
the Evi manual (in the files `evi.info' or `evi.tex').  Hopefully this is
available by using `M-x info' and selecting the `Evi' menu item.  If
necessary, consult your nearest emacs guru for advice.")
	 (set-buffer-modified-p nil)
	 (sit-for 120)
	 (save-excursion
	   ;; evidently we must worry about having been switched to another
	   ;; buffer while waiting...
	   (set-buffer (get-buffer "*scratch*"))
	   (erase-buffer)
	   (set-buffer-modified-p nil))
	 (setq inhibit-startup-message t))))

;; Startup & Shutdown

(defun evi ()
  "Start vi emulation in this buffer."
  (interactive)
  (or evi-enabled
      (progn
	(or evi-initialized
	    (evi-initialize))
	(setq evi-emacs-local-map (current-local-map))
	(evi-install-in-mode-line 'evi-mode-string)
	(if (eq evi-emacs-version 'lucid19)
	    (set (make-local-variable 'interrupt-char) ?\C-c))
	(set (make-local-variable 'echo-keystrokes) 0)
	(make-local-variable 'blink-matching-paren)
	(if evi-meta-prefix-char
	    (set (make-local-variable 'meta-prefix-char)
		 evi-meta-prefix-char))
	(evi-set-continuation 'evi-standard-continuation)
	(setq evi-keymap-list evi-vi-keymap-list)))
  (or (boundp 'minor-mode-map-alist)
      (use-local-map evi-top-level-map))
  (and buffer-read-only buffer-file-name
       (progn (or evi-read-only-buffers
		  (toggle-read-only))
	      (setq evi-buffer-read-only t)))
  (evi-change-mode-id "Vi")
  (or evi-enabled
      (progn
	(evi-show-match evi-show-match)
	(evi-tab-width evi-tab-width)
	(evi-wrap-margin evi-wrap-margin)
	(run-hooks 'evi-mode-hook)))
  (setq evi-enabled t)
  (evi-refresh-mode-line))

(defun evi-quit-evi ()
  "Quit vi emulation in this buffer."
  (interactive)
  (setq evi-enabled nil)
  (evi-deinstall-from-mode-line 'evi-mode-string)
  (or (boundp 'minor-mode-map-alist)
      (use-local-map evi-emacs-local-map))
  (kill-local-variable 'meta-prefix-char)
  (if (eq evi-emacs-version 'lucid19)
      (kill-local-variable 'interrupt-char))
  (evi-refresh-mode-line))

;; Minibuffer

(defun evi-backward-char-maybe-abort (&optional count)
  "Backup, aborting command if at beginning of input."
  (interactive (evi-count-arg))
  (let ((start (point)))
    (beginning-of-line)
    (if (< (- start (or count 1) (point)) (length evi-minibuf-prompt))
	(evi-abort-minibuf)
      (goto-char start)
      (do-evi-backward-char count))))

(defun evi-delete-backward-char-maybe-abort ()
  "Backup and delete previous character, aborting command if at
beginning of input."
  (interactive)
  (let ((start (point)))
    (beginning-of-line)
    (if (<= (- start (point)) (length evi-minibuf-prompt))
	(evi-abort-minibuf)
      (goto-char start)
      (delete-backward-char 1))))

;; Scrolling

(defun evi-scroll-page-forward (&optional count)
  "Scroll COUNT pages forward."
  (interactive (evi-count-arg))
  (scroll-up (if (eq (or count 1) 1)
	       (- (window-height) 3)
	       (* (1- (window-height)) (or count 1))))
  (evi-reset-goal-column))

(defun evi-scroll-page-backward (&optional count)
  "Scroll COUNT pages backward."
  (interactive (evi-count-arg))
  (scroll-down (if (eq (or count 1) 1)
		 (- (window-height) 3)
		 (* (1- (window-height)) (or count 1))))
  (evi-reset-goal-column))

(defun evi-scroll-text-forward (&optional count)
  "Scroll COUNT lines forward.  Default is one half of a page or the last COUNT
specified to either \\[evi-scroll-text-forward] or \\[evi-scroll-text-backward] if one was previously
given.  The position of the cursor on the screen is maintained."
  (interactive (evi-count-arg))
  (evi-set-goal-column)
  (let ((line-count (if count
		      (setq evi-scroll-count count)
		      (or evi-scroll-count (/ (1- (window-height)) 2))))
	(window-line (count-lines (window-start) (1+ (point)))))
    (scroll-up line-count)
    (forward-line (min (1- window-line) line-count))
    (evi-move-to-column evi-goal-column)))

(defun evi-scroll-text-backward (&optional count)
  "Scroll COUNT lines backward.  Default is one half of a page or the last COUNT
specified to either \\[evi-scroll-up] or \\[evi-scroll-down] if one was previously
given.  The position of the cursor on the screen is maintained."
  (interactive (evi-count-arg))
  (evi-set-goal-column)
  (let ((line-count (if count
		      (setq evi-scroll-count count)
		      (or evi-scroll-count (/ (1- (window-height)) 2))))
	(window-line (count-lines (window-start) (1+ (point)))))
    (scroll-down line-count)
    (forward-line (- (min (- (1- (window-height)) window-line) line-count)))
    (evi-move-to-column evi-goal-column)))

(defun evi-scroll-cursor-forward (&optional count)
  "Scroll COUNT lines forward.  Maintain cursor position in the file
if possible."
  (interactive (evi-count-arg))
  (evi-set-goal-column)
  (scroll-up (or count 1))
  (evi-move-to-column evi-goal-column))

(defun evi-scroll-cursor-backward (&optional count)
  "Scroll COUNT lines backward.  Maintain cursor position in the file
if possible."
  (interactive (evi-count-arg))
  (evi-set-goal-column)
  (scroll-down (or count 1))
  (evi-move-to-column evi-goal-column))

(defun evi-window-control (char &optional linenumber)
  "Position current line on the screen according to the following character.
With a prefix count, position that line."
  (interactive (evi-character-arg))
  (if linenumber
    (do-evi-goto-line linenumber))
  (cond ((and (>= char ?0) (<= char ?9))
	  (let* ((count (evi-read-number (- char ?0)))
		 (char (evi-read-command-char)))
	    (cond ((= char ?.) (enlarge-window (- count (1- (window-height)))))
		  ((= char ?+) (enlarge-window count))
		  ((= char ?-) (shrink-window count))
		  ((= char ?=) (cond ((= count 0) (delete-window))
				     ((= count 1) (delete-other-windows))
				     ((= count 2) (split-window-vertically))
				     (t (evi-error "Invalid window op"))))
		  ((= char ?|) (cond ((= count 0) (delete-window))
				     ((= count 1) (delete-other-windows))
				     ((= count 2)
					(split-window-horizontally)))))))
	((or (= char ?f) (= char ?n)) (select-window (next-window)))
	((or (= char ?b) (= char ?p)) (select-window (previous-window)))
	(t
	  (let ((position
		  (cond ((or (eq char ?\r) (eq char ?H)) 0)
			((or (eq char ?.) (eq char ?M)) (/ (window-height) 2))
			((or (eq char ?-) (eq char ?L)) (- (window-height) 2))
			(t (evi-error "Invalid window op")))))
	    (recenter position))))
  (if evi-prompted (message "")))

;; unlike the motion commands, the scroll commands have no wrapper function
;; to fixup the cursor, soo...
(defun evi-move-to-column (column)
  (move-to-column column)
  (if (and (eolp) (not (bolp)))
    (backward-char)))

;; Insert mode

(defun evi-insert (&optional count)
  "Enter insert mode."
  (interactive (evi-count-arg))
  (setq evi-insert-point (point))
  (evi-enter-insert count))

(defun evi-insert-after (&optional count)
  "Enter insert mode after the current point."
  (interactive (evi-count-arg))
  (forward-char)
  (evi-insert count))

(defun evi-open-after (&optional count)
  "Open a new line below the current one and enter insert mode."
  (interactive (evi-count-arg))
  (end-of-line)
  (insert ?\n)
  (setq evi-insert-point (point))
  (evi-maybe-indent)
  (evi-enter-insert count)
  (evi-no-undo-boundary))

(defun evi-open-before (&optional count)
  "Open a new line above the current one and enter insert mode."
  (interactive (evi-count-arg))
  (beginning-of-line)
  (insert ?\n)
  (backward-char)
  (setq evi-insert-point (point))
  (evi-maybe-indent t)
  (evi-enter-insert count))

(defun evi-enter-insert (count)
  (evi-extend-continuation 'evi-exit-insert count)
  (evi-suspend-continuation)
  (evi-insert-mode))

(defun evi-insert-mode ()
  (setq evi-mode 'insert)
  (and (eobp) (not buffer-read-only)
       (progn (newline 1) (backward-char 1)))
  (or evi-in-minibuf
      (progn (evi-change-mode-id "Insert")
	     (evi-refresh-mode-line)))
  (setq evi-keymap-list
	(append (list evi-input-map-map)
		(if evi-in-minibuf
		    (list evi-minibuf-input-map))
		(if (boundp 'minor-mode-map-alist)
		    (if evi-insert-mode-local-bindings
			(list (cons 'emacs
				    (cons 'local
					  evi-emacs-local-suppress-key-list))))
		  (if (and evi-insert-mode-local-bindings evi-emacs-local-map)
		      (list (cons evi-emacs-local-suppress-key-list
				  evi-emacs-local-map))))
		(list evi-insert-map evi-input-map))))

(defun evi-exit-insert (&optional count)
  (evi-maybe-kill-indentation)
  (evi-exit-input-mode count)
  (if (not (bolp)) (backward-char))
  (evi-reset-goal-column)
  (evi-save-command-keys))

(defun evi-exit-input-mode (&optional count)
  "Exit from an input mode."
  (interactive)
  (ex-expand-abbrev)
  (if count
    (let ((input-string (buffer-substring evi-insert-point (point))))
      (evi-iterate (1- count)
	(insert input-string))))
  (setq evi-mode 'vi
	evi-keymap-list evi-vi-keymap-list)
  (or evi-in-minibuf
      (progn (evi-change-mode-id "Vi")
	     (evi-refresh-mode-line))))

(defun evi-insert-mode-delete-backward-char ()
  "Backup and delete previous character, but no further than insert point."
  (interactive)
  (if (> (point) evi-insert-point)
    (delete-backward-char 1)
    (message "Beginning of inserted text")))

(defun evi-insert-mode-delete-backward-word ()
  "Backup and delete previous word, but no further than insert point."
  (interactive)
  (if (<= (point) evi-insert-point)
      (message "Beginning of inserted text")
    (let ((start (point)))
      (do-evi-backward-word)
      (if (< (point) evi-insert-point)
	  (goto-char evi-insert-point))
      (delete-region (point) start))))

(defun evi-insert-mode-kill-line ()
  "Kill current line, but no further than insert point."
  (interactive)
  (if (<= (point) evi-insert-point)
      (message "Beginning of inserted text")
    (let ((start (point)))
      (beginning-of-line)
      (and evi-in-minibuf
	   ;; ZZ - kludge-check for "[{" and escape the `['
	   (looking-at (concat (if (= (aref evi-minibuf-prompt 0) ?\[) "\\")
			       evi-minibuf-prompt))
	   (forward-char (length evi-minibuf-prompt)))
      (if (< (point) evi-insert-point)
	  (goto-char evi-insert-point))
      (delete-region (point) start))))

(defun evi-maybe-indent (&optional forward)
  (interactive)
  (if evi-auto-indent
    (progn
      (let ((start (point)))
	(skip-chars-forward " \t")
	(delete-region start (point)))
      (if (or (not evi-insert-mode-local-bindings)
	      (eq indent-line-function 'indent-to-left-margin))
	(indent-to (save-excursion
		     (if forward (forward-char) (backward-char))
		     (current-indentation)))
	(indent-according-to-mode))
      (setq evi-current-indentation (current-column)))))

(defun evi-maybe-kill-indentation ()
  (and evi-auto-indent (= evi-current-indentation (current-column))
    (let ((region
	   (save-excursion
	     (let ((start (if (progn (skip-chars-backward " \t") (bolp))
			    (point))))
	       (if (and start (progn (skip-chars-forward " \t") (eolp)))
		 (cons start (point)))))))
      (if region
	(delete-region (car region) (cdr region))))))

(defun evi-newline ()
  "Insert a newline, and indent to the current indentation level.
Kills indentation on current line if the line is otherwise empty."
  (interactive)
  (ex-expand-abbrev)
  (let ((start (point)))
    (insert ?\n)
    (evi-maybe-indent)
    (save-excursion
      (goto-char start)
      (evi-maybe-kill-indentation))))

(defun evi-forward-indent ()
  "Move forward to the next indentation level, defined by shiftwidth."
  (interactive)
  ; eat all preceeding blanks, then fill with tabs, and pad with spaces
  ; to reach the target column
  (let* ((start-column (current-column))
	 (target-column (+ start-column (- evi-shift-width
					   (% start-column evi-shift-width))))
	 (backup-point (save-excursion
			 (skip-chars-backward " ")
			 (point))))
    (delete-backward-char (- (point) backup-point))
    (if indent-tabs-mode
	(while (< (setq start-column (current-column)) target-column)
	  (insert ?\t)))
    (if (> start-column target-column) (delete-backward-char 1))
    (insert-char ?\ (- target-column (current-column)))))

(defun evi-calc-backward-indent ()
  (let* ((start-column (current-column))
	 (offset (let ((toffset (% start-column evi-shift-width)))
		   (if (= toffset 0) evi-shift-width toffset)))
	 (furthest (save-excursion
		     (skip-chars-backward " \t" (max 0 (- (point) offset)))
		     (- start-column (current-column)))))
    (min offset furthest)))

(defun evi-backward-indent ()
  "Move backward to the previous indentation level, defined by shiftwidth."
  (interactive)
  (backward-delete-char-untabify (evi-calc-backward-indent) nil))

(defun evi-replace-mode-backward-indent ()
  "Move backward to the previous indentation level, defined by shiftwidth."
  (interactive)
  (if (<= (point) evi-insert-point)
      (message "Beginning of replaced text")
    (let ((offset (evi-calc-backward-indent)))
      (if (> offset evi-replaced-string-index)
	  (progn (setq evi-replaced-string-index 0)
		 (goto-char evi-insert-point))
	(setq evi-replaced-string-index (- evi-replaced-string-index offset))
	(goto-char (- (point) offset))))))

(defun evi-quoted-insert ()
  (interactive)
  (insert (evi-read-char)))

;; Replace mode

(defun evi-replace ()
  "Enter replace mode."
  (interactive)
  (setq evi-mode 'replace)
  (evi-extend-continuation 'evi-exit-replace)
  (evi-suspend-continuation)
  (evi-replace-mode (1- (point-max))))

(defvar evi-replaced-string nil)
(defvar evi-replaced-string-index nil)

(defun evi-replace-mode (max-replace-position)
  (or evi-replace-max
      (setq evi-replace-max (make-marker)))
  (set-marker evi-replace-max max-replace-position)
  (setq evi-insert-point (point)
	evi-replaced-string ""
	evi-replaced-string-index 0)
  (evi-change-mode-id "Replce")
  (evi-refresh-mode-line)
  (setq evi-keymap-list (list evi-input-map-map evi-replace-map)))

(defun evi-switch-to-insert ()
  ;ZZ(setq evi-command-keys loop-command-keys)
  (set-marker evi-replace-max nil)
  (evi-insert-mode))

(defun evi-exit-replace ()
  (if (eq evi-mode 'insert)
      (evi-exit-insert)
    (if (< evi-replaced-string-index (length evi-replaced-string))
	(let ((start (point)))
	  (delete-region (point)
			 (+ (point) (- (length evi-replaced-string)
				       evi-replaced-string-index)))
	  (insert-before-markers
	    (substring evi-replaced-string evi-replaced-string-index))
	  (goto-char start)))
    (if (eq evi-mode 'change)
	(evi-exit-change-mode))
    (setq evi-overstruck-char nil)
    (evi-exit-input-mode)
    (if (not (bolp)) (backward-char))
    (if evi-replace-max
	(set-marker evi-replace-max nil))
    (evi-reset-goal-column)
    (evi-save-command-keys)))

(defun evi-self-replace ()
  "Replace character under cursor with the command character."
  (interactive)
  (if (or (>= (point) evi-replace-max)
	  (= (following-char) ?\n))
      (progn (evi-push-macro (char-to-string last-command-char))
	     (evi-switch-to-insert))
    (if (= evi-replaced-string-index (length evi-replaced-string))
	(setq evi-replaced-string
	      (concat evi-replaced-string
		      (char-to-string (following-char)))))
    (setq evi-replaced-string-index (1+ evi-replaced-string-index))
    (let ((start (point)))
      (evi-replace-one-char last-command-char)
      ;; if auto-indenting happened...
      (if (> (- (point) start) 1)
	  (setq evi-insert-point (1+ start)
		evi-replaced-string
		(buffer-substring (1+ start) (point))
		evi-replaced-string-index
		(length evi-replaced-string)))))
  (evi-no-undo-boundary))

(defun evi-replace-one-char (char)
  (delete-region (point) (1+ (point)))
  (if (boundp 'buffer-undo-list)
      (if (and evi-overstruck-char (= (point) evi-replace-max))
	(progn (aset (car (car buffer-undo-list))
		     0 evi-overstruck-char)
	       (setq evi-overstruck-char nil))))
  ; ZZ unpleasantly hardcoded?
  (if (or (= char ?\n) (= char ?\r))
    (evi-newline)
    (insert char)))

;; ZZ can probably generalize to account for both insert and replace on
;; at least these three

(defun evi-replace-mode-delete-backward-char ()
  "Backup to previous character, undoing last replacement, but no further
than insert point."
  (interactive)
  (if (<= (point) evi-insert-point)
      (message "Beginning of replaced text")
    (backward-char)
    (setq evi-replaced-string-index (1- evi-replaced-string-index))))

(defun evi-replace-mode-delete-backward-word ()
  "Backup and delete previous word, but no further than insert point."
  (interactive)
  (if (<= (point) evi-insert-point)
      (message "Beginning of replaced text")
    (let ((start (point)))
      (do-evi-backward-word)
      (if (< (point) evi-insert-point)
	  (goto-char evi-insert-point))
      (setq evi-replaced-string-index
	    (- evi-replaced-string-index (- start (point)))))))

(defun evi-replace-mode-kill-line ()
  "Kill current line, but no further than insert point."
  (interactive)
  (if (<= (point) evi-insert-point)
      (message "Beginning of replaced text")
    (let ((start (point)))
      (beginning-of-line)
      (and evi-in-minibuf
	   ;; ZZ - kludge-check for "[{" and escape the `['
	   (looking-at (concat (if (= (aref evi-minibuf-prompt 0) ?\[) "\\")
			       evi-minibuf-prompt))
	   (forward-char (length evi-minibuf-prompt)))
      (if (< (point) evi-insert-point)
	  (goto-char evi-insert-point))
      (setq evi-replaced-string-index 0))))

(defun evi-replace-char (char &optional count)
  "Replace the following COUNT characters with CHAR."
  (interactive (evi-character-arg))
  (if (catch 'abort
	(evi-motion-command 'do-evi-forward-char 'horizontal count 'to-end))
      (evi-error "Can't replace that many characters")
    (evi-exchange-point-and-mark)
    (evi-iterate (or count 1)
      (evi-replace-one-char char))
    ;; ZZ unpleasantly hard-coded?
    ;; should be handled by a general purpose post-auto-indent func
    (if (or (= char ?\n) (= char ?\r))
	(evi-maybe-kill-indentation))
    (if (not (bolp)) (backward-char)))
  (evi-reset-goal-column)
  (evi-save-command-keys))

(defun evi-toggle-case (&optional count)
  "Toggle the case of the following COUNT characters."
  (interactive (evi-count-arg))
  (evi-motion-command 'do-evi-forward-char 'horizontal count 'to-end)
  (save-excursion
    (evi-iterate (- (point) evi-mark)
      (backward-char)
      (let ((char (following-char)))
	(cond ((and (>= char ?a) (<= char ?z))
		(upcase-region (point) (1+ (point))))
	      ((and (>= char ?A) (<= char ?Z))
		(downcase-region (point) (1+ (point))))))))
  (evi-fixup-cursor 'horizontal)
  (evi-reset-goal-column)
  (evi-save-command-keys))

;; Modification operators

(defun evi-change (&optional count)
  "Change operator."
  (interactive (evi-count-arg))
  (evi-extend-continuation 'evi-operator-after-after)
  (evi-push-continuation 'evi-change-internal)
  (evi-operator-command count 'to-end))

(defun evi-change-internal ()
  ; If the region is contained on one line, throw a `$' out to mark the
  ; end of the region, then enter replace mode and delete any un-replaced
  ; text when that is exited, with the replace-max set at the end of the
  ; region so that it will switch to insert mode if necessary.  Otherwise,
  ; delete the region first, and enter insert mode.
  (evi-copy-region-to-registers t)
  ; this makes the undo leave the point at the start of the undone text
  (evi-exchange-point-and-mark)
  (if (or (save-excursion (end-of-line) (> evi-mark (point)))
	  (= (point) evi-mark))
      (progn (delete-region (point) evi-mark)
	     (evi-insert))
    (progn (setq evi-overstruck-char (char-after (1- evi-mark)))
	   (let ((here (point)))
	     (goto-char evi-mark)
	     (delete-region (1- evi-mark) evi-mark)
	     (insert ?$)
	     (if (boundp 'buffer-undo-list)
		 ;; this is a bit of song and dance to get the cursor to
		 ;; end up in the right place after an undo.  the problem
		 ;; is these two previous statements, which are the first
		 ;; things changed, and thus where the cursor will be left
		 ;; after an undo.  first step: erase the fact that we put
		 ;; the dollar sign there in the first place.
		 (setq buffer-undo-list (cdr (cdr buffer-undo-list))))
	     (goto-char here))
	   (setq evi-mode 'change)
	   (evi-extend-continuation 'evi-exit-replace)
	   (evi-suspend-continuation)
	   (evi-replace-mode evi-mark))))

(defun evi-exit-change-mode ()
  (if (and (marker-position evi-replace-max)
	   (< (point) evi-replace-max))
    (let ((overstrike-offset (1- (- evi-replace-max (point)))))
      (and (eq evi-emacs-version 'lucid19)
	   (null (car buffer-undo-list))
	   ;; lemacs somehow sneaks in an undo boundary
	   (setq buffer-undo-list (cdr buffer-undo-list)))
      (delete-region (point) (marker-position evi-replace-max))
      (set-marker evi-replace-max nil)
      (if (boundp 'buffer-undo-list)
	  ;; second step: rewrite the undo record with the
	  ;; original overstruck character
	  (aset (car (car buffer-undo-list))
		overstrike-offset evi-overstruck-char)))))

(defun evi-delete (&optional count)
 "Delete operator."
  (interactive (evi-count-arg))
  (evi-extend-continuation 'evi-operator-after-after)
  (evi-extend-continuation 'evi-delete-internal)
  (evi-operator-command count 'to-next))

(defun evi-delete-internal ()
  (evi-copy-region-to-registers t)
  ; this makes the undo leave the point at the start of the undone text
  (evi-exchange-point-and-mark)
  (if (= (point) evi-mark)
    (message "Nothing deleted")
    (if (eq evi-region-shape 'rectangle)
	(delete-rectangle (point) (1+ evi-mark))
      (delete-region (point) evi-mark)))
  (evi-fixup-cursor (if (eq evi-region-shape 'chars) 'horizontal 'vertical)))

(defun evi-yank (&optional count)
  "Yank operator."
  (interactive (evi-count-arg))
  (evi-extend-continuation 'evi-operator-after-after)
  (evi-extend-continuation 'evi-yank-internal (point))
  (evi-operator-command count 'to-next))

(defun evi-yank-internal (start)
  (evi-copy-region-to-registers nil)
  (if (= evi-mark (point))
    (message "Nothing to yank"))
  (goto-char start))

(defun evi-put-after (&optional register-number register-append count)
  "Put back yanked or deleted text after cursor."
  (interactive (evi-register-args))
  (let ((register
	  (aref evi-registers (or register-number evi-register-unnamed))))
    (if register
	(if (eq (evi-register-shape register) 'lines)
	    (progn (end-of-line)
		   (if (not (eobp)) (forward-char))
		   (save-excursion
		     (evi-iterate (or count 1)
		       (insert (evi-register-text register)))))
	  (if (not (and (bolp) (eolp)))
	      (forward-char))
	  (evi-iterate (or count 1)
	    (if (eq (evi-register-shape register) 'chars)
		(insert (evi-register-text register))
	      (insert-rectangle (evi-register-text register))))
	  (backward-char))
      (if register-number
	  (message "Nothing in register %c"
		   (evi-register-name register-number))
	(message "No text to put"))))
  (evi-reset-goal-column)
  (evi-save-command-keys))

(defun evi-put (&optional register-number register-append count)
  "Put back yanked or deleted text."
  (interactive (evi-register-args))
  (let ((register
	  (aref evi-registers (or register-number evi-register-unnamed))))
    (if register
	(if (eq (evi-register-shape register) 'lines)
	    (progn (beginning-of-line)
		   (save-excursion
		     (evi-iterate (or count 1)
		       (insert (evi-register-text register)))))
	  (evi-iterate (or count 1)
	    (if (eq (evi-register-shape register) 'chars)
		(insert (evi-register-text register))
	      (insert-rectangle (evi-register-text register))))
	  (backward-char))
      (if register-number
	  (message "Nothing in register %c"
		   (evi-register-name register-number))
	(message "No text to put"))))
  (evi-reset-goal-column)
  (evi-save-command-keys))

(defun evi-shift-right (&optional count)
  "Shift right operator."
  (interactive (evi-count-arg))
  (evi-extend-continuation 'evi-operator-after-after)
  (evi-extend-continuation 'evi-shift-internal 1)
  (evi-operator-command count 'whole-lines))

(defun evi-shift-left (&optional count)
  "Shift left operator."
  (interactive (evi-count-arg))
  (evi-extend-continuation 'evi-operator-after-after)
  (evi-extend-continuation 'evi-shift-internal -1)
  (evi-operator-command count 'whole-lines))

(defun evi-shift-internal (direction)
  (if (= evi-mark (point))
    (message "Nothing shifted")
    (indent-rigidly evi-mark (point) (* evi-shift-width direction)))
  (goto-char evi-mark)
  (skip-chars-forward " \t"))

(defun evi-indent (&optional count)
  "Indent region."
  (interactive (evi-count-arg))
  (evi-extend-continuation 'evi-operator-after-after)
  (evi-extend-continuation 'evi-indent-internal)
  (evi-operator-command count 'whole-lines))

(defun evi-indent-internal ()
  (if (= evi-mark (point))
    (message "Nothing indented")
    (indent-region evi-mark (point) nil))
  (goto-char evi-mark)
  (skip-chars-forward " \t"))

(defun evi-shell-filter (&optional count)
  "Filter region thru shell command."
  (interactive (evi-count-arg))
  (evi-push-continuation 'evi-shell-filter2 (point))
  (evi-operator-command count 'whole-lines))

(defun evi-shell-filter2 (start)
  (evi-extend-continuation 'evi-operator-after-after)
  (evi-extend-continuation 'evi-filter-internal start)
  (evi-read-string "!"))

(defun evi-filter-internal (start)
  (if (string= evi-minibuf-contents "!")
      (setq evi-minibuf-contents
	(or evi-last-shell-command
	    (evi-error "No previous shell command to substitute for !")))
    (setq evi-last-shell-command evi-minibuf-contents))
  (shell-command-on-region evi-mark (point) evi-minibuf-contents t)
  (goto-char start))

(defun evi-send-to-process (&optional count)
  "Send region to emacs process buffer."
  (interactive (evi-count-arg))
  (evi-push-continuation 'evi-send-to-process2 (point))
  (evi-operator-command count 'to-next))

(defun evi-send-to-process2 (start)
  (evi-extend-continuation 'evi-operator-after-after)
  (evi-extend-continuation 'evi-to-process-internal start)
  (evi-read-string "*"))

(defun evi-to-process-internal (start)
  (if (string= evi-minibuf-contents "*")
      (or evi-process-buffer
	  (evi-error "No previous process to substitute for *"))
    (setq evi-process-buffer evi-minibuf-contents))
  (send-region evi-process-buffer evi-mark (point))
  (goto-char start)
  (setq evi-delayed-buffer-change-request
	(cons 'other
	      (cons evi-process-buffer
		    '(progn
		       (goto-char (process-mark
				    (get-buffer-process evi-process-buffer)))
		       (evi-insert))))))

; ZZ we may want to expand the region to lines here?  or not
(defun evi-loop-over-lines-in-region (&optional count)
  "Execute a sequence of operations on every line in a region."
  (interactive (evi-count-arg))
  (evi-push-continuation 'evi-loop-over-lines2)
  (evi-operator-command count 'to-end))

(defun evi-loop-over-lines2 ()
  (evi-extend-continuation 'evi-operator-after-after)
  (evi-extend-continuation 'evi-loop-lines-internal)
  (evi-read-string "[{"))

(defun evi-loop-lines-internal ()
  (setq evi-last-command-keys nil
	evi-prefix-count nil)
  (let ((ending-mark (set-marker (make-marker) (point-marker))))
    (goto-char evi-mark)
    (beginning-of-line)
    (evi-push-macro evi-minibuf-contents
		    'evi-loop-lines-internal2 evi-minibuf-contents
		    ending-mark)))

(defun evi-loop-lines-internal2 (macro ending-mark)
  (end-of-line)
  (forward-char)
  (evi-db (concat "{" (prin1-to-string (point)) "," (prin1-to-string (marker-position ending-mark)) "}"))
  (if (< (point) (marker-position ending-mark))
      (evi-push-macro macro 'evi-loop-lines-internal2 macro ending-mark)
    (set-marker ending-mark nil)
    (evi-fixup-cursor 'vertical)))

(defun evi-operator-command (count context)
  (evi-push-continuation 'evi-operator-after (list 'quote evi-keymap-list))
  (setq evi-context context
	evi-prefix-count-multiplier count
	evi-prefix-count nil
	evi-keymap-list
	  (cons (evi-make-local-keymap
		  '(((char-to-string last-command-char) evi-whole-lines)))
		(list (cons 'param evi-param-map) (cons 'map evi-map-map)
		      evi-motion-map)))
  (evi-push-continuation 'evi-prompt))

; this happens after the motion command
(defun evi-operator-after (old-keymap-list)
  (setq evi-keymap-list old-keymap-list
	evi-context nil
	evi-prefix-count-multiplier nil)
  (evi-pop-continuation))

; this happens after the entire operation
(defun evi-operator-after-after ()
  (evi-reset-goal-column)
  (evi-save-command-keys))

(defun evi-join-lines (&optional count)
  "Join together COUNT + 1 lines, supplying appropriate whitespace."
  (interactive (evi-count-arg))
  (let ((starting-point (point))
	(ending-point nil))
    (evi-iterate (max (1- (or count 2)) 1)
      (end-of-line)
      (if (evi-eobp)
	  (progn (or ending-point
		     (setq ending-point starting-point))
		 (evi-break))
	(forward-char)
	(delete-region (1- (point))
		       (progn (skip-chars-forward " \t") (point)))
	(or ending-point
	    (setq ending-point (point)))
	(if (and (/= (preceding-char) ? )
		 (/= (preceding-char) ?\t)
		 (/= (following-char) ?\)))
	    (insert-char ?  (if (= (preceding-char) ?.) 2 1)))))
    (goto-char ending-point))
  (evi-reset-goal-column)
  (evi-save-command-keys))

;; Motion command

(defun evi-exchange-point-and-mark ()
  (let ((temp evi-mark))
    (setq evi-mark (point))
    (goto-char temp)))

(defun evi-expand-region-to-lines (context)
  (evi-exchange-point-and-mark)
  (beginning-of-line)
  (evi-exchange-point-and-mark)
  (end-of-line)
  (if (not (or (eobp) (eq context 'to-end))) (forward-char))
  (setq evi-region-shape 'lines))

; 'normalizing' a horizontal region means expanding the region to whole lines
; when 1) the beginning of the region is on the first non-white character
; of a line, and 2) the ending of the region is on the end of the line

(defun evi-normalize-region ()
  (and (eolp)
       (save-excursion
	 (beginning-of-line)
	 (and (> (point) evi-mark)
	      (progn (goto-char evi-mark)
		     (skip-chars-backward " \t")
		     (bolp))))
       (progn (evi-exchange-point-and-mark)
	      (beginning-of-line)
	      (evi-exchange-point-and-mark)
	      (if (not (eobp))
		(forward-char))
	      (setq evi-region-shape 'lines))))

(defun evi-fixup-cursor (direction)
  (if (eq evi-mode 'vi)
      (if (eq direction 'horizontal)
	  (progn (if (and (eobp) (not (bobp)))
		     (backward-char))
		 (if (and (eolp) (not (bolp)))
		     (backward-char)))
	(if (and (eobp) (not (bobp)))
	    (progn (backward-char) (beginning-of-line))
	  (if (and (eolp) (not (bolp))) (backward-char))))))

(defun evi-motion-command (move-function direction count context &optional arg)
  (if context
      (setq evi-mark (point))
    ; else, maintain the goal column.  kinda gross this being here, but...
    (if (or (eq move-function 'do-evi-next-line)
	    (eq move-function 'do-evi-previous-line))
	(evi-set-goal-column)
      (evi-reset-goal-column)))
  (if arg
      (funcall move-function arg count context)
    (funcall move-function count context))
  (if context
      (progn
	(if (< (point) evi-mark) (evi-exchange-point-and-mark))
	(if (or (eq direction 'vertical) (eq context 'whole-lines))
	    (evi-expand-region-to-lines context)
	  (progn (setq evi-region-shape 'chars)
		 (if (eq context 'to-next)
		     (evi-normalize-region)))))
    ; fixup the location of the cursor, if necessary
    (evi-fixup-cursor direction)))

;; Simple motion commands

(evi-defmotion horizontal evi-forward-char (&optional count context)
  "Move right COUNT characters on the current line."
  (let ((here (point)))
    (end-of-line)
    (if (< (or count 1) (- (point) here))
	(goto-char (+ here (or count 1)))))
  (and (eolp) (not context)
       (evi-error "End of line")))

(evi-defmotion horizontal evi-backward-char (&optional count context)
  "Move left COUNT characters on the current line."
  (let ((here (point)))
    (beginning-of-line)
    (if (< (1- (or count 1)) (- here (point)))
	(goto-char (- here (1- (or count 1))))))
  (if (bolp) (evi-error "Beginning of line") (backward-char)))

(evi-defmotion vertical evi-next-line (&optional count context)
  "Go to ARGth next line."
  (evi-next-line-internal (or count 1))
  (if (null context)
    (progn (evi-adjust-scroll-up)
	   (move-to-column evi-goal-column))))

(evi-defmotion vertical evi-beginning-of-next-line (&optional count context)
  "Go to beginning of ARGth next line."
  (evi-next-line-internal (or count 1))
  (if (null context) (evi-adjust-scroll-up))
  (skip-chars-forward " \t"))

;; ZZ maybe can use goal column in fixup-cursor to remove some of this here??
(defun evi-next-line-internal (count)
  (let* ((starting-point (point))
	 (offset (forward-line count)))
    (or (not (eobp)) (= count 0)
	(progn (goto-char starting-point)
	       (evi-error
		 (if (= count 1)
		     "Last line in buffer"
		   "Not that many lines left in buffer"))))))

(defun evi-adjust-scroll-up ()
  (let ((window-line (count-lines (window-start) (1+ (point))))
       (window-height (1- (window-height))))
    (and (>= (point) (window-end))
         (< window-line (+ window-height (/ window-height 3)))
       (recenter -1))))

(evi-defmotion vertical evi-previous-line (&optional count context)
  "Go to ARGth previous line."
  (evi-previous-line-internal (or count 1))
  (if (null context)
    (progn (evi-adjust-scroll-down)
	   (move-to-column evi-goal-column))))

(evi-defmotion vertical evi-beginning-of-previous-line (&optional count context)
  "Go to beginning of ARGth previous line."
  (evi-previous-line-internal (or count 1))
  (if (null context) (evi-adjust-scroll-down))
  (back-to-indentation))

(defun evi-previous-line-internal (count)
  (let* ((starting-point (point))
	 (offset (forward-line (- count))))
    (if (/= offset 0)
	(progn (goto-char starting-point)
	       (evi-error
		 (if (= count 1)
		     "First line in buffer"
		   "Not that many lines left in buffer"))))))

(defun evi-adjust-scroll-down ()
  (if (< (point) (window-start))
    (let ((window-line (count-lines (1+ (point)) (window-start)))
	  (window-height (1- (window-height))))
      (and (< window-line (/ window-height 3))
	   (recenter 0)))))

(evi-defmotion vertical evi-goto-line (&optional count context)
  "Go to line number LINE, or to end of file if no count specified."
  ; ZZ once again... if we know the move won't be far (like on same screen)
  ; perhaps shouldn't push context...
  (evi-push-context)
  (ex-goto-line count))

(evi-defmotion vertical evi-goto-top-of-window (&optional offset context)
  "Go to the top line of the window.  With an arg, OFFSET, goes to the
OFFSET'th line of the window."
  (move-to-window-line (1- (or offset 1)))
  (or context
      (skip-chars-forward " \t")))

(evi-defmotion vertical evi-goto-middle-of-window (&optional offset context)
  "Go to the middle line of the window."
  (move-to-window-line (/ (window-height) 2))
  (or context
      (skip-chars-forward " \t")))

(evi-defmotion vertical evi-goto-bottom-of-window (&optional offset context)
  "Go to the bottom line of the window.  With an arg, OFFSET, goes to the
OFFSET'th line from the bottom of the window."
  (move-to-window-line (- (1- (window-height)) (or offset 1)))
  (or context
      (skip-chars-forward " \t")))

(evi-defmotion horizontal evi-goto-column (&optional column context)
  "Go to column COLUMN, or as close to that column as possible."
  (move-to-column (1- (or column 1))))

(evi-defmotion vertical evi-whole-lines (&optional count context)
  "Go ARG - 1 lines forward."
  (evi-next-line-internal (1- (or count 1))))

(evi-defmotion horizontal evi-beginning-of-line (&optional count context)
  "Go to beginning of line."
  (beginning-of-line))

; it's not at all clear why this doesn't take a count...
; maybe it should...
(evi-defmotion horizontal evi-goto-indentation (&optional count context)
  "Go to beginning of indented text on current line."
  (beginning-of-line)
  (back-to-indentation))
 
(evi-defmotion horizontal evi-end-of-line (&optional count context)
  "Go to end of line."
  (evi-next-line-internal (1- (or count 1)))
  (end-of-line)
  ;; any sufficiently large number here will do
  (setq evi-goal-column 1000000
	evi-reset-goal-column nil))

;; Word, sentence, paragraph and section motion commands

(defun evi-eobp ()
  (< (- (point-max) (point)) 3))

(evi-defmotion horizontal evi-forward-word (&optional count context)
  "Move to the beginning of the COUNTth next word."
  (evi-forward-word-internal evi-word (or count 1) context))

(evi-defmotion horizontal evi-forward-Word (&optional count context)
  "Move to the beginning of the COUNTth next white-space delimited word."
  (evi-forward-word-internal evi-Word (or count 1) context))

(defun evi-forward-word-internal (pattern count context)
  (and (not context) (evi-eobp)
       (evi-error "End of buffer"))
  (if context
    (setq count (1- count)))
  (if (looking-at pattern)
    (setq count (1+ count)))
  (if (and (re-search-forward pattern nil 'limit count)
	   (or (not (eq context 'to-next))
	       (re-search-forward pattern
		 (save-excursion (end-of-line) (point)) 'limit)))
    (if (eq context 'to-end)
      (if (or (> count 0) (looking-at pattern))
	(goto-char (match-end 0))
	(forward-char))
      (goto-char (match-beginning 0)))
    (if (eobp)
      (backward-char))))

(evi-defmotion horizontal evi-end-of-word (&optional count context)
  "Move to the end of the COUNTth next word."
  (evi-end-of-word-internal evi-word (or count 1) context))

(evi-defmotion horizontal evi-end-of-Word (&optional count context)
  "Move to the end of the COUNTth next whitespace delimited word."
  (evi-end-of-word-internal evi-Word (or count 1) context))

(defun evi-end-of-word-internal (pattern count context)
  (and (not context) (evi-eobp)
       (evi-error "End of buffer"))
  (or context
      (forward-char))
  (if (re-search-forward pattern nil 'limit count)
    (goto-char (- (match-end 0) (if context 0 1)))
    (if (eobp)
      (backward-char))))

(evi-defmotion horizontal evi-backward-word (&optional count context)
  "Move to the beginning of the COUNTth previous word."
  (evi-backward-word-internal evi-word (or count 1)))

(evi-defmotion horizontal evi-backward-Word (&optional count context)
  "Move to the beginning of the COUNTth previous whitespace delimited word."
  (evi-backward-word-internal evi-Word (or count 1)))

(defun evi-backward-word-internal (pattern count)
  (if (bobp)
    (evi-error "Beginning of buffer"))
  (evi-iterate count
    (if (re-search-backward pattern nil 'limit)
      (progn
	(looking-at pattern)
	(let ((end (match-end 0))
	      (at-beginning nil))
	  (while (and (looking-at pattern) (= (match-end 0) end)
		      (not (setq at-beginning (bobp))))
	    (backward-char))
	  (if (not at-beginning)
	    (forward-char))))
      (evi-break))))

(defconst evi-sentence-beginning "\\([.?!][]\"')]*\\([\t\n]\\| [ \t\n]\\)\\|^[ \t]*\n\\|\\`\\)[ \t\n]*[^ \t\n]")

(defconst evi-sentence-ending "\\([.?!][]\"')]*\\([\t\n]\\| [ \t\n]\\)\\|^[ \t]*$\\)")

(defconst evi-paragraph-beginning "\\(^\n\\|\\`\\)[ \t\n]*[^ \t\n]")
(defconst evi-paragraph-beginning-mod "\\(^[ \t]*\n\\|\\`\\)[ \t\n]*[^ \t\n]")

(defconst evi-paragraph-ending "[ \t\n]*[^ \t\n]\n$")
(defconst evi-paragraph-ending-mod "^[ \t]*$")

(defconst evi-section-beginning "^\\({\\|\\.\\(NH\\|SH\\|H\\|HU\\|nh\\|sh\\)[ \t\n]\\)")

(defconst evi-section-ending "[ \t\n]*\n\\(}\\|\\.\\(NH\\|SH\\|H\\|HU\\|nh\\|sh\\)[ \t\n]\\)")

(defun evi-not-at (pattern &optional limit)
  (let ((start (point)))
    (if (re-search-backward pattern limit 'limit)
      (prog1
	(/= (match-end 0) start)
	(goto-char start))
      t)))

(evi-defmotion horizontal evi-forward-sentence (&optional count context)
  "Move to the beginning of the COUNT'th next sentence."
  (and (not context) (evi-eobp)
       (evi-error "End of buffer"))
  (forward-char)
  (and (eq context 'to-next) (evi-not-at evi-sentence-beginning)
       (setq context 'to-end))
  (if (re-search-forward evi-sentence-beginning nil 'limit
			 (- (or count 1) (if context 1 0)))
    (if context
      (if (eq context 'to-end)
	(if (re-search-forward evi-sentence-ending nil 'limit)
	  (skip-chars-backward " \t\n"))
	(if (re-search-forward evi-sentence-beginning
	      (save-excursion
		(re-search-forward evi-paragraph-ending nil 'limit)
		(1- (match-beginning 0)))
	      'limit)
	  (backward-char)))
      (backward-char))))

(evi-defmotion horizontal evi-backward-sentence (&optional count context)
  "Move to the beginning of the COUNT'th previous sentence."
  (if (bobp)
    (evi-error "Beginning of buffer"))
  (skip-chars-backward " \t\n")
  (if (re-search-backward evi-sentence-beginning nil 'limit (or count 1))
    (goto-char (1- (match-end 0)))))

(evi-defmotion horizontal evi-forward-paragraph (&optional count context)
  "Move to the beginning of the COUNT'th next paragraph."
  (and (not context) (evi-eobp)
       (evi-error "End of buffer"))
  (if (not evi-modified-paragraph)
      (if (re-search-forward evi-paragraph-ending nil 'limit (or count 1))
	  (goto-char (match-end 0)))
    (forward-char)
    (and (eq context 'to-next) (evi-not-at evi-paragraph-beginning-mod)
	 (setq context 'to-end))
    (if (re-search-forward evi-paragraph-beginning-mod nil 'limit
			   (- (or count 1) (if (eq context 'to-end) 1 0)))
	(if (eq context 'to-end)
	    (if (re-search-forward evi-paragraph-ending-mod nil 'limit)
		(goto-char (1- (match-beginning 0))))
	  (if context
	      (beginning-of-line))
	  (backward-char)))))

(evi-defmotion horizontal evi-backward-paragraph (&optional count context)
  "Move to the beginning of the COUNT'th previous paragraph."
  (if (bobp)
    (evi-error "Beginning of buffer"))
  (if evi-modified-paragraph
      (if (re-search-backward evi-paragraph-beginning-mod
			      nil 'limit (or count 1))
	  (goto-char (1- (match-end 0))))
    (if (re-search-backward evi-paragraph-beginning nil 'limit (or count 1))
	(goto-char (match-beginning 0)))))

(evi-defmotion horizontal evi-forward-section (&optional count context)
  "Move to the beginning of the COUNT'th next section."
  (and (not context) (evi-eobp)
       (evi-error "End of buffer"))
  (or context
      (evi-push-context (point)))
  (let ((start (point)))
    (skip-chars-forward "^ \t\n")
    (or (eobp)
	(forward-char))
    (and (eq context 'to-next) (evi-not-at evi-section-beginning start)
	 (setq context 'to-end)))
  (if (re-search-forward evi-section-beginning nil 'limit
			 (- (or count 1) (if (eq context 'to-end) 1 0)))
    (if (eq context 'to-end)
	(if (re-search-forward evi-section-ending nil 'limit)
	  (or (eq (preceding-char) ?})
	      (goto-char (match-beginning 0))))
      (goto-char (match-beginning 0))
      (if context
	(backward-char)))))

(evi-defmotion horizontal evi-backward-section (&optional count context)
  "Move to the beginning of the COUNT'th previous section."
  (if (bobp)
    (evi-error "Beginning of buffer"))
  (or context
      (evi-push-context (point)))
  (re-search-backward evi-section-beginning nil 'limit (or count 1)))

(defun evi-region-arbitrary ()
  "Define region bounded by mark and point (containing point)."
  (interactive)
  (if (< (point) evi-mark) (evi-exchange-point-and-mark))
  (forward-char)
  (setq evi-region-shape 'chars))

(defun evi-region-mouse ()
  "Define region bounded by last mouse selection."
  (interactive)
  (setq evi-mark (mark))
  (if (< (point) evi-mark) (evi-exchange-point-and-mark))
  (forward-char)
  (setq evi-region-shape 'chars))

(defun evi-region-rectangle ()
  "Define region as rectangle bounded by mark and point (containing point)."
  (interactive)
  (if (< (point) evi-mark) (evi-exchange-point-and-mark))
  (setq evi-region-shape 'rectangle))

(defun evi-region-rows (context)
  "Define region as rows bounded by mark and point (containing point)."
  (interactive (evi-context-arg))
  (if (< (point) evi-mark) (evi-exchange-point-and-mark))
  (evi-expand-region-to-lines evi-context))

;ZZ - very naive
(defun evi-region-columns ()
  "Define region as columns bounded by mark and point (containing point)."
  (interactive)
  (if (< (point) evi-mark) (evi-exchange-point-and-mark))
  (let ((start-col (save-excursion (goto-char evi-mark) (current-column)))
	(end-col (current-column)))
    (setq evi-mark start-col)
    (goto-char (point-max))
    (if (eolp)
	(backward-char))
    (beginning-of-line)
    (goto-char (+ (point) end-col)))
  (setq evi-region-shape 'rectangle))

;; Searching

(evi-defmotion horizontal evi-search-forward
  (&string "/" string &optional count context)
  "Search forward for the ARGth occurence of a pattern.  A null string will
repeat the previous search."
  (evi-do-vi-search t string (or count 1)))

(evi-defmotion horizontal evi-search-backward
  (&string "?" string &optional count context)
  "Search backward for the ARGth occurence of a pattern.  A null string will
repeat the previous search."
  (evi-do-vi-search nil string (or count 1)))

(defun evi-do-vi-search (search-forward search-spec count)
  (let ((ex-user-buffer (current-buffer)))
    (set-buffer ex-work-space)
    (erase-buffer)
    (insert (if search-forward ?/ ??) search-spec "\n")
    (goto-char (point-min))
    (let ((string (ex-scan-regular-expression))
	  (offset (ex-scan-line-offset)))
      (set-buffer ex-user-buffer)
      (or (string= string "")
	  (setq evi-search-pattern string))
      (if evi-search-pattern
	  (evi-do-search (setq evi-search-forward search-forward)
			 evi-search-pattern count)
	(evi-error "No previous search pattern"))
      (if (> offset 0)
	  (evi-next-line-internal offset)
	(if (< offset 0)
	    (evi-previous-line-internal (- offset)))))))

(evi-defmotion horizontal evi-search-next (&optional count context)
  "Search for the next ARGth occurence of the previous search pattern."
  (if evi-search-pattern
    (evi-do-search evi-search-forward evi-search-pattern (or count 1))
    (evi-error "No previous search pattern")))

(evi-defmotion horizontal evi-search-next-reverse (&optional count context)
  "Search for the next ARGth occurence of the previous search pattern
but look in the opposite direction."
  (let ((evi-search-forward (not evi-search-forward)))
    (do-evi-search-next count context)))

(defun evi-do-search (search-forward search-string count)
  (let ((case-fold-search evi-ignore-case)
	(starting-point (point)))
    (if (if search-forward
	  (evi-search-forward-count search-string count)
	  (evi-search-backward-count search-string count))
      (progn
	; ZZ if we know the search didn't take us far, perhaps we shouldn't
	; push a context...
	(evi-push-context starting-point)
        (goto-char (match-beginning 0)))
      (progn
	(goto-char starting-point)
	(evi-error
	  (concat
	    (if (> count 1) "Nth occurrence not found" "Pattern not found")
	    (if evi-search-wraparound ""
	      (if search-forward
		  " before end of file"
		  " before beginning of file"))))))))

; ZZ use evi-iterate
(defun evi-search-forward-count (string count)
  (if (> count 0)
    (progn (forward-char)
	   (if (re-search-forward string nil t)
	     (evi-search-forward-count string (1- count))
	     (if evi-search-wraparound
	       (progn (goto-char (point-min))
		      (if (re-search-forward string nil t)
			(evi-search-forward-count string (1- count)))))))
    t))

(defun evi-search-backward-count (string count)
  (if (> count 0)
    (if (re-search-backward string nil t)
      (evi-search-backward-count string (1- count))
      (if evi-search-wraparound
	(progn (goto-char (point-max))
	       (if (re-search-backward string nil t)
		 (evi-search-backward-count string (1- count))))))
    t))

(evi-defmotion horizontal evi-find-character (&char char &optional count context)
  "Search for CHAR on the current line.  With COUNT find the COUNT'th occurance."
  (setq evi-find-character char
	evi-find-forward t
	evi-find-up-to nil)
  (evi-find-character-internal (or count 1) context))

(evi-defmotion horizontal evi-find-char-backwards
  (&char char &optional count context)
  "Search backwards for CHAR on the current line.  With COUNT find the
COUNT'th occurance."
  (setq evi-find-character char
	evi-find-forward nil
	evi-find-up-to nil)
  (evi-find-character-backwards-internal (or count 1) context))

(evi-defmotion horizontal evi-find-character-before
  (&char char &optional count context)
  "Search for CHAR on the current line and leave the cursor on the character
before it.  With COUNT find the COUNT'th occurance."
  (setq evi-find-character char
	evi-find-forward t
	evi-find-up-to t)
  (evi-find-character-internal (or count 1) context))

(evi-defmotion horizontal evi-find-char-backwards-after
  (&char char &optional count context)
  "Search backwards for CHAR on the current line and leave the cursor on
the character after it.  With COUNT find the COUNT'th occurance."
  (setq evi-find-character char
	evi-find-forward nil
	evi-find-up-to t)
  (evi-find-character-backwards-internal (or count 1) context))

(evi-defmotion horizontal evi-find-next-character (&optional count context)
  "Search for the next COUNT'th occurence of the previous search character."
  (if evi-find-character
    (if evi-find-forward
      (evi-find-character-internal (or count 1) context)
      (evi-find-character-backwards-internal (or count 1) context))
    (evi-error "No previous search character")))

(evi-defmotion horizontal evi-find-next-character-reverse (&optional count context)
  "Search for the next COUNT'th occurence of the previous search character
in the opposite direction."
  (let ((evi-find-forward (not evi-find-forward)))
    (do-evi-find-next-character count context)))

(defun evi-find-character-internal (count context)
  (forward-char)
  (let ((case-fold-search nil))
    (if (search-forward (char-to-string evi-find-character)
			(save-excursion (end-of-line) (point)) t count)
      (if evi-find-up-to
	(backward-char))
      (progn (backward-char)
	     (evi-error "No more occurences on this line"))))
  (or context
      (backward-char)))

(defun evi-find-character-backwards-internal (count context)
  (let ((case-fold-search nil))
    (or (search-backward (char-to-string evi-find-character)
			 (save-excursion (beginning-of-line) (point)) t count)
	(evi-error "No more occurences on this line")))
  (if evi-find-up-to
    (forward-char)))

(evi-defmotion horizontal evi-paren-match (&optional count context)
  "Move cursor to matching parenthesis, brace or bracket."
  (let ((end-point (save-excursion (end-of-line) (point))))
    (if (re-search-forward "[][(){}]" end-point t)
      (progn (backward-char)
	     (if (looking-at "[({[]")
	       (progn (forward-sexp 1)
		      (or context (backward-char)))
	       (progn (forward-char)
		      (if context (setq evi-mark (1+ evi-mark)))
		      (backward-sexp 1))))
      (evi-error "Nothing on rest of line to balance"))))

;; Repeating

;; ZZ
(defvar evi-command-keys-length 256)
(defvar evi-command-keys (make-string evi-command-keys-length 0)
  "The keystrokes for the current command.")
(defvar evi-last-command-keys nil
  "Command keys for the last complete vi command.")
(defvar evi-command-keys-index 0)
(defvar evi-prompt nil)

(defun evi-add-key (k)
  (if (>= evi-command-keys-index evi-command-keys-length)
      (progn (setq evi-command-keys-length (+ evi-command-keys-length 256)
		   evi-command-keys (concat evi-command-keys
					    (make-string 256 0)))))
  (aset evi-command-keys evi-command-keys-index k)
  (setq evi-command-keys-index (1+ evi-command-keys-index)
	evi-prompt (concat evi-prompt (char-to-string k))))

(defun evi-lose-key ()
  (if (> evi-command-keys-index 0)
      (setq evi-command-keys-index (1- evi-command-keys-index))))

(defun evi-erase-keys ()
  (setq evi-command-keys-index 0
	evi-prompt nil))

(defun evi-copy-keys ()
  (substring evi-command-keys 0 evi-command-keys-index))

(defun evi-keys-description ()
  (mapconcat 'single-key-description (evi-copy-keys) ""))

(defun evi-prompt-keys-description ()
  (mapconcat 'single-key-description evi-prompt ""))

(defun evi-save-command-keys ()
  (setq evi-last-command-keys (evi-copy-keys)
	evi-last-prefix-count evi-prefix-count
	evi-hidden-repeat-count 0
	evi-last-register-spec evi-register-spec))

(defun evi-repeat ()
  "Repeat last modifying command."
  (interactive)
  (if evi-prefix-count
      (setq evi-last-prefix-count evi-prefix-count)
    (setq evi-prefix-count evi-last-prefix-count))
  (if evi-register-spec
      (setq evi-last-register-spec evi-register-spec)
    (setq evi-register-spec evi-last-register-spec))
  (setq evi-repeat-count (1+ evi-hidden-repeat-count))
  (evi-push-continuation 'evi-repeat-continuation)
  (evi-push-macro evi-last-command-keys
		  'evi-repeat-after evi-last-command-keys))

(defun evi-repeat-continuation ()
  )

(defun evi-repeat-after (command-keys)
  (setq evi-last-command-keys command-keys
	evi-hidden-repeat-count evi-repeat-count
	evi-repeat-count 0))

(defun evi-prompt-repeat ()
  "Print last modifying command."
  (interactive)
  (let ((command (evi-read-string "Repeat: " evi-last-command-keys)))
    (evi-execute-macro command)
    (setq evi-last-command-keys command)))

;; Prefix counts

; ZZ used in evi-window-control!
; ZZ need to rewrite that...
(defun evi-read-number (prefix-value)
  (let ((char (evi-read-command-char)))
    (if (and (>= char ?0) (<= char ?9))
      (evi-read-number (+ (* prefix-value 10) (- char ?0)))
      (progn (evi-unread-command-char char)
	     prefix-value))))

(defun evi-prefix-digit ()
  "Prefix count."
  (interactive)
  ;; prefixes aren't a part of the command-keys
  (evi-lose-key)
  (setq evi-prefix-count (+ (* (or evi-prefix-count 0) 10)
			    (- last-command-char ?0)))
  (evi-push-continuation 'evi-prompt))

(defun evi-digit-or-beginning-of-line ()
  (interactive)
  (if evi-prefix-count
      (evi-prefix-digit)
    (evi-beginning-of-line)))

(defun evi-adjust-count ()
  (if evi-prefix-count-multiplier
      (setq evi-prefix-count (* (or evi-prefix-count 1)
				evi-prefix-count-multiplier))
    evi-prefix-count))

;; Registers

(defun evi-prefix-register ()
  "Prefix register."
  (interactive)
  ;; registers aren't a part of the command-keys
  (evi-lose-key)
  (evi-prompt)
  (let* ((char (evi-read-command-char)))
    (evi-lose-key)
    (setq evi-register-spec (cons (evi-register-number char)
				  (not (and (>= char ?a) (<= char ?z))))))
  (evi-push-continuation 'evi-prompt))

(defun evi-register-number (register-name)
  (cond ((and (>= register-name ?a) (<= register-name ?z))
	  (+ (- register-name ?a) 10))
	((and (>= register-name ?A) (<= register-name ?Z))
	  (+ (- register-name ?A) 10))
	((and (>= register-name ?1) (<= register-name ?9))
	 (% (+ evi-digit-register (- register-name ?0) evi-repeat-count) 9))
	((eq register-name ?^)
	  evi-register-unnamed)
	((eq register-name ?@)
	  (or evi-last-macro-register
	      (evi-error "No previous macro register specified")))
	(t (evi-error "Invalid register name"))))

(defun evi-register-name (register-number)
  (if (> register-number 9)
    (+ register-number (- ?a 10))
    (+ register-number ?1)))

(defun evi-copy-region-to-registers (number-register-also)
  (let ((region (if (eq evi-region-shape 'rectangle)
		    (extract-rectangle evi-mark (1+ (point)))
		  (buffer-substring evi-mark (point)))))
    (evi-copy-region-to-register region evi-register-spec)
    (if number-register-also
      (progn (aset evi-registers
		   evi-digit-register (cons region evi-region-shape))
	     (setq evi-digit-register (if (= evi-digit-register 0)
					  8
					(1- evi-digit-register)))))))

(defun evi-copy-region-to-register (region register-spec)
  (let ((register-number (car register-spec)))
    (if (not (eq register-number evi-register-unnamed))
	(aset evi-registers
	      evi-register-unnamed (cons region evi-region-shape)))
    (if register-spec
	(aset evi-registers register-number
	      (if (and (cdr register-spec)
		       (not (eq evi-region-shape 'rectangle)))
		  (let ((register (aref evi-registers register-number)))
		    (cons (concat (car register) region) (cdr register)))
		(cons region evi-region-shape))))))

(defun evi-register-string (count)
  (interactive (evi-count-arg))
  (evi-extend-continuation 'evi-register-string-after count)
  (evi-read-string "\""))

(defun evi-register-string-after (count)
  (setq evi-region-shape 'chars)
  (evi-copy-region-to-register evi-minibuf-contents
    (or evi-register-spec (cons evi-register-unnamed nil))))

(defun evi-register-char (char &optional count)
  (interactive (evi-character-arg))
  (evi-register-string (char-to-string char)))

(defun evi-buffer-name ()
  (interactive)
  (evi-register-string (buffer-name)))

;; Undoing

(defun evi-undo ()
  "Undo previous change."
  (interactive)
  ; ZZ - is this the only place we're concerned with unnecessary output
  ; during a macro?
  (or evi-current-macro evi-in-minibuf
      (message "undo!"))
  (evi-undo-start)
  (evi-undo-one-change)
  (evi-fixup-cursor 'vertical))

(if (boundp 'buffer-undo-list)
    (defun evi-undo-line ()
      "Undo all changes to this line."
      (interactive)
      (evi-undo-start)
      (evi-undo-one-line)
      (evi-fixup-cursor 'vertical)))

(defun evi-undo-start ()
  (undo-start)
  (if (boundp 'buffer-undo-list)
      ; if the first record is a boundary, skip it
      (while (and pending-undo-list (null (car pending-undo-list)))
	(setq pending-undo-list (cdr pending-undo-list)))
    (undo-more 1)))

(defun evi-undo-more ()
  "Continue undoing previous changes."
  (interactive)
  (if (boundp 'buffer-undo-list)
      (if (boundp 'pending-undo-list)
	  (progn (or evi-current-macro evi-in-minibuf
		     (message "undo more!"))
		 (evi-undo-one-change))
	(evi-error "No previous undo to continue"))
    (or evi-current-macro evi-in-minibuf
	(message "undo more!"))
    (evi-undo-one-change))
  (evi-fixup-cursor 'vertical))

(defun evi-undo-one-change ()
  (let ((modified (buffer-modified-p)))
    (undo-more 1)
    (and modified (not (buffer-modified-p))
	 (delete-auto-save-file-if-necessary)))
  (evi-reset-goal-column))

(defvar evi-last-undo-line-mark nil)

(if (boundp 'buffer-undo-list)
    ; undo records are:
    ;   (t . ...) which marks a file save
    ;   ("string" . pos) which undoes a delete
    ;   (pos . pos) which undoes an insert
    (defun evi-undo-one-line ()
      (if (eq evi-last-undo-line-mark (cdr buffer-undo-list))
	(evi-error "No undo for this line"))
      (let* ((begin (save-excursion (beginning-of-line) (point)))
	     (end (save-excursion (end-of-line) (point)))
	     (undo-new nil)
	     (something-to-do nil))
	(evi-enumerate-condition undo-record pending-undo-list
	  (cond ((eq (car undo-record) t)
		  (setq undo-new (nconc undo-new list))
		  nil)
		((stringp (car undo-record))
		  (if (and (>= (cdr undo-record) begin)
			   (<= (cdr undo-record) end))
		    (progn (setq end (+ end (length (car undo-record))))
			   (setq undo-new
				 (nconc undo-new (list undo-record)))
			   (setq something-to-do t)
			   t)
		    (progn (setq undo-new (nconc undo-new (list nil) list))
			   nil)))
		((integerp (car undo-record))
		  (let* ((first (car undo-record))
			 (second (cdr undo-record))
			 (begin2 (if (< first begin) begin first))
			 (end2 (if (> second end) end second))
			 (diff (- end2 begin2)))
		    (if (and (<= first end) (>= second begin) (/= begin2 end2))
		      (progn
			(setq undo-new
			      (nconc undo-new (list (cons begin2 end2))))
			(setq something-to-do t)
			(if (or (< first begin) (> second end))
			  (progn
			    (nconc undo-new (list nil))
			    (if (< first begin)
			      (nconc undo-new (list (cons first begin))))
			    (if (> second end)
			      (nconc undo-new
				(list (cons (- end diff) (- second diff)))))
			    (nconc undo-new (cdr list))
			    nil)
			  (progn (setq end (- end diff))
				 t)))
		      (progn
			     (setq undo-new (nconc undo-new (list nil) list))
			     nil))))
		((eq undo-record nil)
		  t)))
	(if something-to-do
	  (let ((modified (buffer-modified-p)))
	    (setq pending-undo-list undo-new)
	    (undo-more 1)
	    (or evi-current-macro evi-in-minibuf
		(message "Undo!"))
	    (setq evi-last-undo-line-mark buffer-undo-list)
	    (beginning-of-line)
	    (and modified (not (buffer-modified-p))
		 (delete-auto-save-file-if-necessary)))
	  (evi-error "No undo for this line")))
      (evi-reset-goal-column)))

(defun evi-no-undo-boundary ()
  (setq prefix-arg t))

;; Marks

(defun evi-set-mark (char &optional count)
  "Mark location."
  (interactive (evi-character-arg))
  (cond ((and (>= char ?a) (<= char ?z))
	  (aset evi-registers (+ (- char ?a) 36) (point-marker)))
	((eq char ?.)
	  (setq evi-mark (point)))))

(evi-defmotion horizontal evi-goto-mark-horizontal (&optional count context)
  "Goto a mark."
  (evi-goto-mark-internal (evi-read-command-char) context))

(evi-defmotion vertical evi-goto-mark-vertical (&optional count context)
  "Goto a mark.  If an operand, define a whole lines region."
  (evi-goto-mark-internal (evi-read-command-char) context)
  (or context
    (back-to-indentation)))

(defun evi-goto-mark-internal (char &optional context)
  (cond ((and (>= char ?a) (<= char ?z))
	  (let ((marker (aref evi-registers (+ (- char ?a) 36))))
	    (if (not (eq (current-buffer) (marker-buffer marker)))
	      (progn (switch-to-buffer (marker-buffer marker))
		     ; unpleasant, but best we can do... (?)
		     (if context (setq evi-mark (point)))))
	    (evi-push-context)
	    (goto-char marker)))
	((or (eq char ?`) (eq char ?'))
	  (goto-char (evi-exchange-context)))
	((eq char ?.)
	  (goto-char (evi-pop-context)))
	((eq char ?,)
	  (goto-char (evi-unpop-context)))))

(defun evi-push-context (&optional offset)
  (let ((marker (if offset (set-marker (make-marker) offset) (point-marker))))
    (aset evi-context-ring evi-context-ring-cursor marker)
    (setq evi-context-ring-cursor
	  (if (= evi-context-ring-cursor 9) 0 (1+ evi-context-ring-cursor)))))

(defun evi-pop-context ()
  (setq evi-context-ring-cursor
    (if (= evi-context-ring-cursor 0) 9 (1- evi-context-ring-cursor)))
  (aref evi-context-ring evi-context-ring-cursor))

(defun evi-unpop-context ()
  (setq evi-context-ring-cursor
    (if (= evi-context-ring-cursor 9) 0 (1+ evi-context-ring-cursor)))
  (aref evi-context-ring evi-context-ring-cursor))

(defun evi-exchange-context ()
  (let ((cursor
	 (if (= evi-context-ring-cursor 0) 9 (1- evi-context-ring-cursor))))
    (prog1 (aref evi-context-ring cursor)
	   (aset evi-context-ring cursor (point-marker)))))

;; Misc

(defun evi-redraw ()
  "Redraw the display."
  (interactive)
  (cond ((eq evi-emacs-version 'emacs18)
	  (redraw-display))
	((eq evi-emacs-version 'emacs19)
	  (redraw-frame (selected-frame)))
	((eq evi-emacs-version 'lucid19)
	  (redraw-screen (selected-screen)))))

(defun evi-file-info ()
  "Give information on the file associated with the current buffer."
  (interactive)
  (let* ((line-number (count-lines 1 (min (1+ (point)) (point-max))))
	 (total-lines (1- (+ line-number (count-lines (point) (point-max)))))
	 (file-name buffer-file-name))
    (message "\"%s\"%s%s line %d of %d, column %d --%d%%--"
	     (if file-name
	       (if evi-global-directory
		 (evi-abbreviate-file-name file-name (evi-current-directory))
		 file-name)
	       "")
	     (if evi-buffer-read-only
	       " [Read only]" "")
	     (if (buffer-modified-p) " [Modified]" "")
	     line-number
	     total-lines
	     (1+ (current-column))
	     (/ (* line-number 100) total-lines))))

(defun evi-abbreviate-file-name (file-name directory &optional abbrev)
  (let* ((length (length directory))
	 (ends-in-slash (= (aref directory (1- length)) ?/)))
    (if (and (> length 0)
	     (>= (length file-name) length)
	     (string= (substring file-name 0 length) directory))
      (concat (or abbrev "")
	      (substring file-name
			 (+ length (if (or abbrev ends-in-slash) 0 1))))
      file-name)))

(defun evi-tag ()
  "Go to the tag which is the next word in the buffer."
  (interactive)
  (evi-motion-command 'do-evi-forward-word 'horizontal 1 'to-end)
  (ex-tag (buffer-substring evi-mark (point))))

(defun evi-make-char-table ()
  (let ((table (make-vector 256 0))
	(i ?:))
    (while (<= ?0 (setq i (1- i)))
      (aset table i 1))
    (setq i ?\[)
    (while (<= ?A (setq i (1- i)))
      (aset table i 2))
    (setq i ?\{)
    (while (<= ?a (setq i (1- i)))
      (aset table i 2))
    (setq i ? )
    (while (<= 0 (setq i (1- i)))
      (aset table i 4))
    table))

(defvar evi-char-table (evi-make-char-table))

(defun evi-is-num (c)
  (= (logand (aref evi-char-table c) 1) 1))

(defun evi-is-alpha (c)
  (= (logand (aref evi-char-table c) 2) 2))

(defun evi-is-alphanum (c)
  (/= (logand (aref evi-char-table c) 3) 0))

(defun evi-is-nonalphanum (c)
  (= (logand (aref evi-char-table c) 3) 0))

(defun evi-is-control-char (c)
  (= (logand (aref evi-char-table c) 4) 4))

(defun evi-is-printable (c)
  (and (not (evi-is-control-char c))
       (< c ?\C-?)))

;; Display of lists

(defun evi-display-and-prompt (command &optional args)
  (let ((window (selected-window))
	(wconf (current-window-configuration)))
    ;; this is for lucid19
    (if (eq window (minibuffer-window))
	(select-window (previous-window)))
    (if (apply command args)
	(progn
	  (select-window (minibuffer-window))
	  (message
	    "Hit SPACE or RET to continue, anything else to keep window")
	  (let ((c (evi-read-char)))
	    (if (or (= c ?\n) (= c ?\r) (= c ? ))
		(set-window-configuration wconf)
	      (select-window window)))))))

(defun evi-display-list-and-prompt (buffer list &optional initial max-len)
  (evi-display-and-prompt
   'evi-display-list (list buffer list initial max-len)))

(defun evi-display-list (buffer list &optional initial max-len)
  (save-excursion
    (set-buffer (get-buffer-create buffer))
    (erase-buffer)
    (evi)
    (if initial
	(insert initial))
    (if (eq max-len 'half)
	(setq max-len (- (/ (window-width) 2) 2)))
    (if list
	(evi-insert-list-pretty list (or max-len (- (window-width) 2))))
    (goto-char (point-min))
    (display-buffer buffer t))
  ;; indicates to evi-display-and-prompt that something was displayed
  t)

(defun evi-insert-list-pretty (list max-len)
  (let* ((len (length list))
	 (max-width (min (evi-max-len list) max-len))
	 (col-width (+ max-width 2))
	 (width (window-width))
	 (cols (/ width col-width))
	 (rows (/ (+ len (1- cols)) cols))
	 (counters nil)
	 (indent))
    (if (< len cols)
	(setq col-width (/ width len)
	      max-width (- col-width 2)
	      cols len
	      rows 1))
    (evi-iterate cols
      (setq counters (cons (nthcdr (* (1- count) rows) list) counters)))
    (evi-iterate rows
      (setq indent 0)
      (evi-iterate-list item counters
	(let ((s (car (nthcdr (- rows count) item))))
	  (if s
	      (progn
		(indent-to indent)
		(insert (if (> (length s) max-width)
			    (concat (substring s 0 (- max-width 2)) "...")
			  s))
		(setq indent (+ indent col-width))))))
      (insert ?\n))))

(defun evi-max-len (list)
  (let ((lengths (mapcar 'length list)))
    (apply 'max lengths)))

(defun evi-pretty-char (c)
  (cond ((evi-is-printable c)
	  (char-to-string c))
	((evi-is-control-char c)
	  (if ex-input-escapes
	      (cond ((= c ?\n) "\\n")
		    ((= c ?\r) "\\r")
		    ((= c ?\t) "\\t")
		    ((= c ?\e) "\\e")
		    (t (concat "\\C-"
			       (char-to-string (+ c (if (< c ?\e) ?` ?@))))))
	    (concat "^" (char-to-string (+ c ?@)))))
	((= c ?\C-?)
	  (if ex-input-escapes "\\C-?" "^?"))
	(t
	  (format "\\%03o" c))))

(defun evi-pretty-string (s)
  (mapconcat 'evi-pretty-char s ""))

; works for maps as well as abbrev lists
(defun evi-pretty-binding (b)
  (concat (evi-pretty-string (car b)) " = "
	  (evi-pretty-string (if (consp (cdr b))
				 (cdr (cdr b))
			       (cdr b)))))

;; Ex

; ZZ this should be cleaned up
(defvar ex-user-buffer nil)
(defvar ex-printed nil)

(defun evi-ex-command ()
  "Execute an ex command."
  (interactive)
  (evi-extend-continuation 'ex-read-command-after
			   (current-window-configuration))
  (setq ex-user-buffer (current-buffer))
  (setq ex-printed nil)
  (evi-read-string ":" nil evi-ex-map evi-ex-input-map))

(defun ex-read-command-after (wconf)
  (set-window-configuration wconf)
  (evi-do-ex-command-string evi-minibuf-contents)
  (if ex-printed
      (save-excursion
	(set-buffer "*Print*")
	(or (eq (point-min) (point-max))
	    (evi-display-and-prompt
	      (lambda ()
		(goto-char (point-min))
		(evi)
		(display-buffer (current-buffer)))))))
  (evi-fixup-cursor 'vertical))

(defun ex-do-completion (name start c-name c-list-fun)
  (if c-name
      (if (stringp c-name)
	  (if (string= name c-name)
	      (evi-display-completions (funcall c-list-fun c-name))
	    (progn (delete-region start (point))
		   (insert c-name)))
	(insert " "))
    (progn (beep) (save-excursion (insert " [no match]"))
	   (sit-for 2)
	   (delete-region (point) (+ (point) 11)))))

(defun evi-display-completions (list)
  (evi-display-list " *Completions*" list "Possible completions are:\n"))

(defun ex-scan-one-command-point ()
  (ex-scan-addresses)
  (let* ((start-of-com (point))
	 (command (ex-scan-command-name)))
    (if (eolp)
	(cons (cons 'command start-of-com) (point))
      (cons (or (ex-scan-parameter-list (cdr (car (cdr command))) t)
		(cons nil (point)))
	    (point)))))

(defun ex-scan-command-point ()
  (let ((res (ex-scan-one-command-point)))
    (skip-chars-forward " \t")
    (while (= (following-char) ?|)
      (forward-char)
      (setq res (ex-scan-one-command-point)))
    res))

(defun ex-is-completable (proto)
  (or (eq proto 'file) (eq proto 'files) (eq proto 'shell-command)
      (eq proto 'buffer)
      (eq proto 'settings)
      (eq proto 'command) (eq proto 'map) (eq proto 'abbrev)
      (eq proto 'process)))

(defun ex-complete ()
  (interactive)
  (let* ((cmd-point (progn (beginning-of-line)
			   (forward-char) ; position after `:'
			   (ex-scan-command-point)))
	 (type (car (car cmd-point)))
	 (start-of-word
	   (max (cdr (car cmd-point))
		(save-excursion (skip-chars-backward "^ \t") (point))))
	 (word (buffer-substring start-of-word (point))))
    (cond ((or (eq type 'file) (eq type 'files) (eq type 'shell-command))
	    ; ZZ perform substitution?
	    (let* ((name (file-name-nondirectory word))
		   (odir (file-name-directory word))
		   (dir (let ((cur-buffer (current-buffer)))
			  (set-buffer ex-user-buffer)
			  (prog1
			    (if odir
				(expand-file-name odir (evi-current-directory))
			      (evi-current-directory))
			    (set-buffer cur-buffer)))))
	      (ex-do-completion name (+ start-of-word (length odir))
		(file-name-completion name dir)
		(function (lambda (c-name)
			    (file-name-all-completions c-name dir))))))
	  ((eq type 'buffer)
	    (let ((buf-list
		   (mapcar 'list
		     (evi-filter (function
				   (lambda (name) (/= (aref name 0) ? )))
				 (mapcar 'buffer-name (buffer-list))))))
	      (ex-do-completion word start-of-word
	        (try-completion word buf-list)
		(function (lambda (c-name)
			    (all-completions c-name buf-list))))))
	  ((eq type 'settings)
	    (if (> (save-excursion (goto-char start-of-word)
				   (skip-chars-forward "^=\n")
				   (point))
		   (point))
		(beep)
	      (if (and (eq (char-after start-of-word) ?n)
		       (eq (char-after (1+ start-of-word)) ?o)
		       (not (eq (char-after (+ start-of-word 2)) ?v)))
		  (setq word (substring word 2)
			start-of-word (+ start-of-word 2)))
	      (let ((settings-list (mapcar 'car evi-option-list)))
		(ex-do-completion word start-of-word
		  (try-completion word settings-list)
		  (function (lambda (c-name)
			      (all-completions c-name settings-list)))))))
	  ((or (eq type 'command) (eq type 'map) (eq type 'abbrev))
	    (let ((cmd-list
		   (if (eq type 'command)
		       (mapcar 'car ex-commands)
		     (if (eq type 'map)
			 (evi-keymap-bindings evi-map-map)
		       evi-abbrev-list))))
	      (ex-do-completion word start-of-word
		(try-completion word cmd-list)
		(function (lambda (c-name)
			    (all-completions c-name cmd-list))))))
	  ((eq type 'process)
	    (let ((proc-list (mapcar
			       (function (lambda (x) (list (process-name x))))
			       (process-list))))
	      (ex-do-completion word start-of-word
	        (try-completion word proc-list)
		(function (lambda (c-name)
			    (all-completions c-name proc-list))))))
	  (t (insert ?\t)))))

(defun evi-filter (pred list)
  (let* ((head (cons nil nil))
	 (end head))
    (while list
      (if (funcall pred (car list))
	  (setq end (setcdr end (cons (car list) nil))))
      (setq list (cdr list)))
    (cdr head)))

(defun evi-do-ex-command-file (filename)
  (if (file-readable-p filename)
    (let ((ex-user-buffer (current-buffer))
	  (def-dir (evi-current-directory))
	  (evi-interactive nil))
      (set-buffer ex-work-space)
      (erase-buffer)
      (let ((default-directory def-dir))
	  (insert-file-contents filename))
      (goto-char (point-min))
      (evi-do-ex-command)
      (set-buffer ex-user-buffer))))

(defun evi-do-ex-command-string (command-string)
  (let ((ex-user-buffer (current-buffer)))
    (set-buffer ex-work-space)
    (erase-buffer)
    (insert command-string "\n")
    (goto-char (point-min))
    (evi-do-ex-command)
    (set-buffer ex-user-buffer)))

;; Note - it is expected that the function that calls this one has set
;; ex-user-buffer, and switched to buffer ex-work-space
(defun evi-do-ex-command ()
  (while (not (eobp))
    (let ((command (ex-scan-command)))
      (set-buffer ex-user-buffer)
      (if evi-global-directory
	  (setq default-directory (evi-current-directory)))
      (eval command)
      (set-buffer ex-work-space)
      (forward-char))))

(defun ex-scan-command ()
  (if (= (following-char) ?:)
      (forward-char))
  (if (= (following-char) ?\")
      (end-of-line))
  (let* ((addresses (ex-scan-addresses))
	 (command-struct (ex-scan-command-name))
	 (number-of-addresses (car (car (cdr command-struct))))
	 (command-name (car (car command-struct)))
	 (command-prototype (cdr (car (cdr command-struct))))
	 (command-function (cdr (cdr command-struct))))
    (if (null command-struct)
      (evi-error "Unknown ex command"))
    (if (> (ex-count-addresses addresses) number-of-addresses)
      (evi-error "The %s command only needs %d addresses"
			    command-name number-of-addresses))
    (let ((parameter-list (ex-scan-parameter-list command-prototype nil)))
      (skip-chars-forward " \t")
      (or (looking-at "[|\n]") (eobp)
	  (evi-error "garbage after end of command: `%s'"
		     (buffer-substring (point)
				       (progn (skip-chars-forward "^|\n")
					      (skip-chars-backward " \t")
					      (point)))))
      (cons command-function
	    (cond ((eq number-of-addresses 1)
		    (cons (list 'quote (car addresses)) parameter-list))
		  ((eq number-of-addresses 2)
		    (cons (list 'quote addresses) parameter-list))
		  (t
		    parameter-list))))))

(defun ex-scan-parameter-list (prototype-list completing)
  (if prototype-list
    (let ((prototype (cdr (car prototype-list)))
	  (skip-white (eq (car (car prototype-list)) t)))
      (if skip-white
	  (skip-chars-forward " \t")
	(if (eq (car (car prototype-list)) 'backup)
	    (backward-char)))
      (let* ((start (point))
	     (param (if (and (listp prototype) (eq (car prototype) 'if))
			(if (ex-scan-parameter (nth 1 prototype))
			    ;; if the test is true, but the body returns `nil',
			    ;; return `t' anyway so we don't lose the info
			    (progn
			      (setq prototype (nth 2 prototype)
				    start (point))
			      (or (ex-scan-parameter prototype)
				  t)))
		      (ex-scan-parameter prototype)))
	     (recurs
	       (if (and completing (eolp) (ex-is-completable prototype))
		   (cons prototype start)
		 (ex-scan-parameter-list (cdr prototype-list) completing))))
	(if completing
	    recurs
	  (cons param recurs))))))

(defun ex-scan-parameter (prototype)
  (cond ((null prototype)
	  nil)
	((stringp prototype)
	  (ex-scan-string prototype))
	((eq prototype 'address)
	  (list 'quote (ex-scan-address)))
	((eq prototype 'register)
	  (list 'quote (ex-scan-register)))
	((eq prototype 'file)
	  (ex-scan-quoted "%#*?$[" " \t|\n"))
	((or (eq prototype 'buffer)
	     (eq prototype 'words))
	  (ex-scan-quoted nil "|\n"))
	((or (eq prototype 'rest-of-line)
	     (eq prototype 'process))
	  (ex-scan-quoted nil "\n"))
	((or (eq prototype 'word)
	     (eq prototype 'map)
	     (eq prototype 'abbrev))
	  (ex-scan-quoted nil " \t|\n"))
	((eq prototype 'regular-expression)
	  (ex-scan-regular-expression))
	((eq prototype 'regular-expression2)
	  (ex-scan-regular-expression t))
	((eq prototype 'command)
	  (list 'quote (ex-scan-command)))
	((eq prototype 'settings)
	  (list 'quote (ex-scan-settings)))
	((eq prototype 'files)
	  (ex-scan-files))
	((eq prototype 'shell-command)
	  (ex-scan-quoted "%#" "\n"))
	((eq prototype 'offset)
	  (ex-scan-edit-offset))
	((eq prototype 'mark)
	  (ex-scan-mark))))

(defun ex-scan-addresses ()
  (skip-chars-forward " \t")
  (if (= (following-char) ?%)
      (progn (forward-char)
	     (cons (cons (cons 'number 1) 0) (cons (cons 'dollar nil) 0)))
    (if (looking-at "[-+0-9.$^'/?]")
      (cons
	(ex-scan-address)
	(progn (skip-chars-forward " \t")
	       (if (= (following-char) ?,)
		 (progn (forward-char)
			(skip-chars-forward " \t")
			(ex-scan-address))
		 (cons (cons nil nil) 0))))
      (cons (cons (cons nil nil) 0) (cons (cons nil nil) 0)))))

(defun ex-scan-address ()
  (cons (ex-scan-linespec) (ex-scan-line-offset)))

(defun ex-scan-linespec ()
  (let ((char (following-char)))
    (cond
      ((and (>= char ?0) (<= char ?9))
	(let ((start (point)))
	  (skip-chars-forward "0-9")
	  (cons 'number (string-to-int (buffer-substring start (point))))))
      ((eq char ?.)
	(forward-char)
	(cons 'dot nil))
      ((eq char ?$)
	(forward-char)
	(cons 'dollar nil))
      ((eq char ?^)
        (forward-char)
	(cons 'prev nil))
      ((eq char ?')
	(forward-char 2)
	(cons 'mark (preceding-char)))
      ((eq char ?/)
	(cons 're-forward (ex-scan-regular-expression)))
      ((eq char ??)
	(cons 're-backward (ex-scan-regular-expression))))))

;; if evi-search-magic is nil, also rework the pattern so that . [ and *
;; become literal, and \. \[ and \* are `magic' (i.e. behave as . [ and *
;; in a regular expression)

(defun ex-scan-regular-expression (&optional esc-ampersand)
  (if (looking-at "[|\n]")
      nil
    (forward-char)
    (let* ((start (point))
	   (stop-chars (concat (if esc-ampersand "&")
			       (if (not evi-search-magic) ".[*")))
	   (skip-chars (concat "^\n\\\\\C-v" stop-chars
			       (char-to-string (preceding-char))))
	   (stop-pat (concat "[\\\\\C-v" stop-chars "]")))
      (skip-chars-forward skip-chars)
      (while (looking-at stop-pat)
	(if (or (= (following-char) ?\\) (= (following-char) ?\C-v))
	    (progn (forward-char)
		   (and (/= (length stop-chars) 0)
			(looking-at (concat "[" stop-chars "]"))
			(delete-region (1- (point)) (point)))
		   (forward-char))
	  (insert "\\")
	  (forward-char))
	(skip-chars-forward skip-chars))
      (prog1
	  (buffer-substring start (point))
	(if (not (eolp))
	    (forward-char))))))

(defun ex-scan-line-offset ()
  (if (looking-at "[0-9+-]")
      (let ((start (point)))
	(forward-char)
	(skip-chars-forward "0-9")
	; if they only put a +/- without an offset, default to +/-1
	(if (and (= (- (point) start) 1) (< (preceding-char) ?0))
	    (if (= (preceding-char) ?+) 1 -1)
	  (string-to-int (buffer-substring start (point)))))
    0))

(defun ex-scan-edit-offset ()
  (if (/= (following-char) ?+)
      nil
    (forward-char)
    (if (evi-is-num (following-char))
	(ex-scan-line-offset)
      -1)))

;; ZZ maybe recognize here that 0 is invalid?
(defun ex-define-region (addresses whole-lines default-whole-file)
  (let ((start (car addresses))
	(end (cdr addresses)))
    (if (and (null (car (car start))) default-whole-file)
	(progn (setq evi-mark (point-min))
	       (goto-char (point-max)))
      (let ((starting-point (point)))
	(ex-goto-address start)
	(setq evi-mark (point))
	(ex-goto-address end starting-point))
      (if whole-lines
	  (evi-expand-region-to-lines 'ex)))))

(defun ex-goto-line (line)
  (if line
      (let ((starting-point (point)))
	(goto-char (point-min))
	(if (or (> (forward-line (1- line)) 0) (and (eobp) (not (bobp))))
	    (progn (goto-char starting-point)
		   (evi-error "Past end of buffer"))))
    (progn (goto-char (point-max))
	   (if (= (preceding-char) ?\n)
	       (forward-line -1)
	     (beginning-of-line)))))

(defun ex-goto-address (address &optional starting-point)
  (let ((token (car (car address)))
	(value (cdr (car address))))
    (cond ((eq token 'number)
	    (ex-goto-line value))
	  ((eq token 'dot)
	    (if starting-point (goto-char starting-point)))
	  ((eq token 'dollar)
	    (ex-goto-line nil))
	  ((eq token 'prev)
	    (if starting-point (goto-char starting-point))
	    (forward-line -1))
	  ((eq token 'mark)
	    (evi-goto-mark-internal value))
	  ((eq token 're-forward)
	    (if (= (length value) 0)
	      (if ex-previous-re
		(setq value ex-previous-re)
		(evi-error "No previous regular expression"))
	      (setq ex-previous-re value))
	    (if starting-point (goto-char starting-point))
	    (end-of-line)
	    (let ((message (catch 'abort
			     (evi-do-search t value 1)
			     nil)))
	      (if message
		(progn (forward-line -1)
		       (evi-error message)))))
	  ((eq token 're-backward)
	    (if starting-point (goto-char starting-point))
	    (evi-do-search nil value 1))))
  (forward-line (cdr address)))

(defun ex-goto-line-after-address (address)
  (if (null (car (car address)))
      (forward-line)
    (if (and (eq (car (car address)) 'number)
	     (= (cdr (car address)) 0))
	(goto-char (point-min))
      (progn (ex-goto-address address)
	     (forward-line)))))

(defun ex-count-addresses (addresses)
  (if (eq (car (car (car addresses))) nil)
    0
    (if (eq (car (car (cdr addresses))) nil)
      1
      2)))

(defun ex-scan-command-name ()
  (skip-chars-forward " \t")
  (let ((start (point)))
    (if (looking-at "[a-zA-Z!<=>&@]")
      (progn (forward-char)
	     (let ((char (preceding-char)))
	       (if (or (and (>= char ?a) (<= char ?z))
		       (and (>= char ?A) (<= char ?Z)))
		 (skip-chars-forward "a-zA-Z")))))
    (ex-lookup-command ex-commands (buffer-substring start (point)))))

(defun ex-lookup-command (command-list command)
  (evi-find cmd-struct command-list
    (if (ex-command-eq command (car cmd-struct))
      cmd-struct)))

(defun ex-command-eq (command command-cell)
  (let ((full-command (car command-cell)))
    (or (string= command full-command)
	(let ((command-length (length command)))
	  (and (>= command-length (cdr command-cell))
	       (< command-length (length full-command))
	       (string= command
			(substring (car command-cell) 0 (length command))))))))

(defun ex-scan-register ()
  (if (evi-is-alpha (following-char))
      (let ((char (following-char)))
	(forward-char)
	(cons (evi-register-number char)
	      (not (and (>= char ?a) (<= char ?z)))))
    (cons evi-register-unnamed nil)))

(defun ex-scan-mark ()
  (if (evi-is-alpha (following-char))
      (let ((char (following-char)))
	(forward-char)
	(+ (- char (if (and (>= char ?a) (<= char ?z)) ?a ?A)) 36))
    (evi-error "marker name required for mark command")))

(defun ex-scan-files ()
  (let ((file)
	(flist nil))
    (while (> (length (setq file (ex-scan-quoted "%#*?$[" " \t|\n"))) 0)
      (setq flist (cons file flist))
      (skip-chars-forward " \t"))
    (cons 'quote (cons (nreverse flist) nil))))

(defun ex-scan-quoted (stop-chars delim-chars)
  (let ((start (point))
	(skip-chars (concat "^\\\\\C-v" stop-chars delim-chars))
	(stop-pat (concat "[\\\\\C-v" stop-chars "]"))
	(expand-glob nil))
    (skip-chars-forward skip-chars)
    (while (looking-at stop-pat)
      (let ((char (following-char)))
	(cond ((= char ?\C-v)
		(delete-region (point) (1+ (point)))
		(forward-char))
	      ((= char ?\\)
	        (if ex-input-escapes
		    (progn
		      (delete-region (point) (1+ (point)))
		      (let ((char (following-char)))
			(cond ((= char ?e)
				(delete-region (point) (1+ (point)))
				(insert ?\e))
			      ((= char ?n)
				(delete-region (point) (1+ (point)))
				(insert ?\n))
			      ((= char ?r)
				(delete-region (point) (1+ (point)))
				(insert ?\r))
			      ((= char ?t)
				(delete-region (point) (1+ (point)))
				(insert ?\t))
			      ((and (= char ?C)
				    (= (char-after (1+ (point))) ?-))
				(let ((char (char-after (+ (point) 2))))
				  (insert (- char (if (< char ?a) ?@ ?`)))
				  (delete-region (point) (+ (point) 3))))
			      (t (forward-char 1)))))
		  (forward-char)))
	      ((= char ?%)
		(let ((file-name (buffer-file-name ex-user-buffer)))
		  (if file-name
		    (progn
		      (delete-region (point) (1+ (point)))
		      (insert file-name))
		    (evi-error
		      "Buffer has no filename to substitute for %%%%"))))
	      ((= char ?#)
	        (if evi-prev-file
		    (progn
		      (delete-region (point) (1+ (point)))
		      (insert evi-prev-file))
		    (evi-error
		      "No alternate filename to substitute for #")))
	      (t
		(setq expand-glob t)
		(forward-char))))
      (skip-chars-forward skip-chars))
    (if expand-glob
      (progn (shell-command-on-region start (point)
	       (concat "echo " (buffer-substring start (point))) t)
	     (goto-char start)
	     (skip-chars-forward (concat "^" delim-chars))))
    (if (/= start (point))
	(buffer-substring start (point)))))

(defun ex-scan-string (string)
  (let ((string-length (length string)))
    (if (<= string-length
	    (- (save-excursion (skip-chars-forward "^|\n") (point))
	       (point)))
      (let ((buffer-string
	      (buffer-substring (point) (+ (point) string-length))))
	(if (string= string buffer-string)
	  (progn (forward-char string-length)
		 t))))))

(defun ex-not-implemented (&optional arg)
  (message "Command not implemented"))

(defun ex-abbrev (abbrev definition)
  (if abbrev
      (let ((elem (assoc abbrev evi-abbrev-list)))
	(if elem
	    (if definition
		(setcdr elem (cons (length abbrev) definition))
	      (message "%s" (evi-pretty-string (cdr (cdr elem)))))
	  (if definition
	      (setq evi-abbrev-list
		    (cons
		     (cons abbrev
			   (cons (length abbrev) definition)) evi-abbrev-list))
	    (evi-error "No abbrev for `%s'" abbrev))))
    (evi-display-list-and-prompt
      "*Abbrevs*" (mapcar 'evi-pretty-binding evi-abbrev-list))))

(defun ex-expand-abbrev ()
  (let ((abbrev evi-abbrev-list)
	(case-fold-search nil))
    (while abbrev
      (if (search-backward (car (car abbrev))
			   (- (point) (nth 1 (car abbrev))) t)
	  (if (evi-is-nonalphanum (preceding-char))
	      (progn
		(delete-region (point) (+ (point) (nth 1 (car abbrev))))
		(insert (cdr (cdr (car abbrev)))))
	    (goto-char (+ (point) (nth 1 (car abbrev))))))
      (setq abbrev (cdr abbrev)))))

(defun evi-self-insert ()
  (interactive)
  (if (evi-is-nonalphanum last-command-char)
      (ex-expand-abbrev))
  (self-insert-command 1)
  (evi-no-undo-boundary))

(defun ex-change-buffer (exclam buffer-name)
  (ex-change-buffer-internal exclam buffer-name nil))

(defun ex-change-buffer-other-window (exclam buffer-name)
  (ex-change-buffer-internal exclam buffer-name t))

(defun ex-change-buffer-internal (exclam buffer-name other-window)
  (or buffer-name
      (setq buffer-name (buffer-name (other-buffer (current-buffer)))))
  (let ((found (ex-verify-buffer buffer-name)))
    (if (or exclam found)
      (if other-window
	(switch-to-buffer-other-window buffer-name)
	(switch-to-buffer buffer-name))
      (message "Buffer \"%s\" does not exist" buffer-name))
    (evi)))
    ; (and exclam (not found)

(defun ex-verify-buffer (buffer-name)
  (evi-find buf (buffer-list) (string= (buffer-name buf) buffer-name)))

(defun evi-expand-file-name (file-name)
  (let* ((expanded-name (expand-file-name file-name))
	 (len (length expanded-name)))
    (if (= (aref expanded-name (1- len)) ?/)
	expanded-name
      (concat expanded-name "/"))))

(defun evi-current-directory ()
  (if evi-global-directory
      (car evi-directory-stack)
    default-directory))

(defun ex-change-directory (directory-name)
  (let ((expnd-dir-name (evi-expand-file-name (or directory-name "~"))))
    (if evi-global-directory
	(setcar evi-directory-stack expnd-dir-name)
      (setq default-directory expnd-dir-name))))

(defun ex-push-directory (directory-name)
  (if directory-name
      (setq evi-directory-stack
	    (cons (evi-expand-file-name directory-name) evi-directory-stack))
    (if (null (cdr evi-directory-stack))
	(evi-error "Only one directory")
      (setq evi-directory-stack
	    (cons (nth 1 evi-directory-stack)
		  (cons (car evi-directory-stack)
			(cdr (cdr evi-directory-stack))))))))

(defun ex-pop-directory ()
  (if (null (cdr evi-directory-stack))
    (evi-error "Only one directory left")
    (setq evi-directory-stack (cdr evi-directory-stack))))

(defun ex-directory-stack ()
  (let ((home (getenv "HOME")))
    (message
      (mapconcat (function
		   (lambda (f)
		     (let* ((dir (evi-abbreviate-file-name f home "~"))
			    (end (1- (length dir))))
		       (if (= (aref dir end) ?/)
			 (substring dir 0 end)
			 dir))))
		 evi-directory-stack " "))))

(defun ex-copy (from-addresses to-address)
  (ex-define-region from-addresses t nil)
  (let ((text (buffer-substring evi-mark (point))))
    (ex-goto-line-after-address to-address)
    (insert text)))

(defun ex-delete (addresses register-struct)
  (let ((evi-register-spec register-struct))
    (ex-define-region addresses t nil)
    (evi-copy-region-to-registers t)
    ; to make undo's come out right
    (if (< evi-mark (point))
      (evi-exchange-point-and-mark))
    (delete-region (point) evi-mark)))

(defun ex-edit (exclam offset file-name)
  (ex-edit-internal exclam offset file-name nil))

(defun ex-edit-other-window (exclam offset file-name)
  (ex-edit-internal exclam offset file-name t))

(defun ex-edit-internal (exclam offset file-name other-window)
  (if (null file-name)
      (if (and (not exclam) (not other-window) (buffer-modified-p))
	  (message "Buffer modified since last save (use :edit! to override)")
	(if other-window
	    (split-window-vertically)
	  (if (null buffer-file-name)
	      (message "Buffer has no file associated with it")
	    (revert-buffer nil t)
	    (evi))))
    (let ((prev-buf (get-buffer (current-buffer))))
      (if other-window
	  (find-file-other-window file-name)
	(find-file file-name))
      (or (eq prev-buf (get-buffer (current-buffer)))
	  (setq evi-prev-file (buffer-file-name prev-buf)))
      (evi)))
  (if offset
      (ex-goto-line (if (= offset -1) nil offset))))

(defun ex-elisp-execute (lisp-expression)
  (eval (car (read-from-string lisp-expression))))

(defun ex-file (file-name)
  (if file-name
      (set-visited-file-name file-name)
    (evi-file-info)))

(defun ex-global (addresses notmatch pattern command)
  (let ((case-fold-search evi-ignore-case)
	(next-line-mark (make-marker))
	(end-line-mark (make-marker))
	(start)
	(none-found t)
	(end-pos (point))
	(large-region))
    (if (= (length pattern) 0)
	(if ex-previous-re
	    (setq pattern ex-previous-re)
	  (evi-error "No previous regular expression"))
      (setq ex-previous-re pattern))
    (ex-define-region addresses t t)
    (evi-exchange-point-and-mark)
    (setq large-region (> (- evi-mark (point)) 5000))
    (if large-region
      (message "running global command... "))
    (set-marker end-line-mark evi-mark)
    (while (< (point) end-line-mark)
      (setq start (point))
      (forward-line)
      (set-marker next-line-mark (point))
      (goto-char start)
      (or (eq (re-search-forward pattern next-line-mark t) notmatch)
	  (progn
	    (goto-char start)
	    (setq none-found nil
		  end-pos (point))
	    (eval command)))
      (goto-char next-line-mark))
    (if large-region
      (message "running global command... complete."))
    (set-marker next-line-mark nil)
    (set-marker end-line-mark nil)
    (goto-char end-pos)
    (if none-found
	(evi-error "No occurance of pattern found"))))

(defun ex-vglobal (addresses pattern command)
  (ex-global addresses t pattern command))

(defun ex-recurse (fun)
  (let ((ex-user-buffer (current-buffer)))
    (set-buffer ex-work-space)
    (let ((work-string (buffer-string))
	  (work-point (point)))
      (set-buffer ex-user-buffer)
      (eval fun)
      (setq ex-user-buffer (current-buffer))
      (set-buffer ex-work-space)
      (erase-buffer)
      (insert work-string)
      (goto-char work-point)
      (set-buffer ex-user-buffer))))

(defun ex-initialize ()
  (ex-recurse '(evi-customize)))

(defun ex-kill-buffer (exclam buffer-name)
  (ex-kill-buffer-internal exclam buffer-name nil))

(defun ex-kill-buffer-delete-windows (exclam buffer-name)
  (ex-kill-buffer-internal exclam buffer-name t))

(defun ex-kill-buffer-internal (exclam buffer windows-too)
  (setq buffer (get-buffer (or buffer (current-buffer))))
  (and (not exclam) (buffer-file-name buffer) (buffer-modified-p buffer)
       (evi-error
	 "No write since last change (use :kill! to override)"))
  (set-buffer buffer)
  (set-buffer-modified-p nil)
  (delete-auto-save-file-if-necessary)
  (if windows-too
      (condition-case nil
	  (delete-windows-on buffer)
	;; ignore error about trying to delete only window on only screen
	(error nil)))
  (kill-buffer buffer)
  (setq ex-user-buffer (current-buffer)))

(defun ex-find-fkey (suffix)
  (let ((key (vector (intern (concat "f" suffix)))))
    (if (or (eq window-system 'x)
	    (evi-find it (evi-keymap-bindings function-key-map)
		      (equal (cdr it) key)))
	key
      (setq key (vector (intern (concat "kp-f" suffix))))
      (if (evi-find it (evi-keymap-bindings function-key-map)
		    (equal (cdr it) key))
	  key))))

(defun ex-map (exclam key definition)
  (let ((map (if exclam evi-input-map-map evi-map-map)))
    (if key
	(progn
	  (if (and (> (length key) 1)
		   (= (aref key 0) ?#) (evi-is-num (aref key 1)))
	      (setq key (or (ex-find-fkey (substring key 1))
			    key)))
	  (if (vectorp key)
	      (define-key evi-top-level-map key 'evi-top-level))
	  (if definition
	      (if exclam
		  (evi-define-key '(input-map) key definition)
		(evi-define-key '(map) key definition))
	    (let ((mapping (lookup-key map key)))
	      (if (stringp mapping)
		  (message "%s" (evi-pretty-string mapping))
		(evi-error "No map for `%s'" key)))))
      (evi-display-list-and-prompt
	"*Mappings*" (mapcar 'evi-pretty-binding (evi-keymap-bindings map))))))

(defun ex-mark (address marker)
  (save-excursion
    (ex-goto-address address (point))
    (aset evi-registers marker (point-marker))))

(defun ex-move (from-addresses to-address)
  (ex-define-region from-addresses t nil)
  (let ((text (buffer-substring evi-mark (point)))
	(to-mark (copy-marker (save-excursion
				(ex-goto-line-after-address to-address)
				(point)))))
    ; to make undo's come out right
    (if (< evi-mark (point))
      (evi-exchange-point-and-mark))
    (delete-region (point) evi-mark)
    (goto-char to-mark)
    (insert text)
    (set-marker to-mark nil)))

(defun ex-preserve ()
  (do-auto-save))

(defun ex-print (addresses)
  (save-excursion
    (ex-define-region addresses t nil)
    (insert (prog1
		(buffer-substring evi-mark (point))
	      (set-buffer (get-buffer-create "*Print*"))
	      (or ex-printed
		  (erase-buffer))
	      (setq ex-printed t)))))

(defun ex-next (exclam files)
  (ex-next-internal exclam files nil))

(defun ex-next-other-window (exclam files)
  (ex-next-internal exclam files t))

(defun ex-next-internal (exclam files other-window)
  (if files
      (let ((next-buffers
	      (mapcar 'find-file-noselect files)))
	(if next-buffers
	    (let ((prev-buf (get-buffer (current-buffer))))
	      (if other-window
		  (switch-to-buffer-other-window (car next-buffers))
		(switch-to-buffer (car next-buffers)))
	      (or (eq prev-buf (get-buffer (current-buffer)))
		  (setq evi-prev-file (buffer-file-name prev-buf)))
	      (evi))))
    (let ((next-buffer (evi-next-file-buffer t)))
      (if next-buffer
	  (progn (setq evi-prev-file buffer-file-name)
		 (bury-buffer (current-buffer))
		 (if other-window
		     (switch-to-buffer-other-window next-buffer)
		   (switch-to-buffer next-buffer))
		 (evi))
	(message "All files are displayed")))))

(defun evi-next-file-buffer (not-in-window)
  (let ((rest-of-list
	  (evi-enumerate-condition buffer (cdr (buffer-list))
	    (or (and not-in-window (get-buffer-window buffer))
		(null (buffer-file-name buffer))))))
    (if rest-of-list
      (car rest-of-list))))

(defun ex-put (address register-struct)
  (ex-goto-line-after-address address)
  (let ((register (aref evi-registers (car register-struct))))
    (if register
      (save-excursion
	(if (eq (evi-register-shape register) 'rectangle)
	    (progn (newline (length (evi-register-text register)))
		   (backward-char (length (evi-register-text register)))))
	(if (eq (evi-register-shape register) 'rectangle)
	    (insert-rectangle (evi-register-text register))
	  (insert (evi-register-text register)))
	(if (eq (evi-register-shape register) 'chars)
	    (insert ?\n)))
      (if evi-register-spec
	(message "Nothing in register %c"
		 (evi-register-name (car evi-register-spec)))
	(message "No text to put")))))

;; ZZ should move to a misc section - actually this shouldn't be here: surely
;; this is defined somewhere else?

(defun evi-list-apply (func l)
  (if l
    (progn (apply func (car l) nil)
	   (evi-list-apply func (cdr l)))))

(cond ((eq evi-emacs-version 'emacs19)
        (defun ex-quit (discard)
	  (if (= (length (frame-list)) 1)
	      (ex-really-quit discard)
	    (delete-frame))))
      ((eq evi-emacs-version 'lucid19)
        (defun ex-quit (discard)
	  (if (= (length (screen-list)) 1)
	      (ex-really-quit discard)
	    (delete-screen))))
      (t
        (defun ex-quit (discard)
	  (ex-really-quit discard))))

(defun ex-really-quit (discard)
  (if discard
      (progn
	(evi-list-apply
	 (function (lambda (buf)
		     (if (buffer-file-name buf)
			 (progn (set-buffer buf)
				(delete-auto-save-file-if-necessary)))))
	 (buffer-list)))
    (let ((modified-buffers
	     (evi-filter
	       (function (lambda (buf)
			   (let ((c (aref (buffer-name buf) 0)))
			     (and (buffer-modified-p buf)
				  (/= c ? ) (/= c ?*)))))
	       (buffer-list))))
      (if modified-buffers
	  (if (or (cdr modified-buffers)
		  (not (eq (car modified-buffers) (current-buffer))))
	      (evi-error "Modified buffers exist (use :quit! to override, :Wq to save buffers and quit)")
	    (evi-error "No write since last change (use :quit! to override)")))))
  (kill-emacs))

(defun ex-read (address shell-command arg)
  (ex-goto-line-after-address address)
  (if shell-command
      (if (eq shell-command t)
	  (evi-error "Incomplete shell escape")
	(call-process shell-file-name nil t nil "-c" shell-command))
    (evi-insert-file arg)))

; there's a bug in insert-file-contents that doesn't record an undo save
; boundary when it's appropriate (ZZ)
(defun evi-insert-file (filename)
  (if (boundp 'buffer-undo-list)
      (progn
	;; the insert will record a save record if appropriate
	(insert ?@)
	(delete-region (1- (point)) (point))
	;; now just erase the existence of the insert and delete
	(setq buffer-undo-list (cdr (cdr buffer-undo-list)))))
  (insert-file-contents filename))

(defun ex-recover (exclam file-name)
  (or file-name
      (if (setq file-name buffer-file-name)
	  (and (not exclam) (buffer-modified-p)
	       (evi-error
		"No write since last change (use :recover! to override)"))
	(evi-error "Buffer has no file associated with it")))
  (recover-file file-name)
  (auto-save-mode 1)
  (message "Auto save mode on")
  (evi))

(defun ex-set (settings)
  (if settings
      (ex-set-internal settings)
    (message (mapconcat 'evi-get-option evi-set-options " "))))

(defun ex-set-internal (settings)
  (if settings
    (let* ((setting (car settings))
	   (name (car setting))
	   (value (cdr setting)))
      (if (string= name "all")
	  (evi-display-list-and-prompt
	    "*Settings*"
	    (mapcar (function (lambda (x) (evi-get-option (car (car x)))))
		    (evi-filter (function (lambda (x) (cdr (cdr x))))
				evi-option-list))
	    nil 'half)
	(if (integerp value)
	    (progn (princ (evi-get-option name))
		   (princ " "))
	  (evi-set-option name value)))
      (ex-set-internal (cdr settings)))))

(defun ex-scan-settings ()
  (skip-chars-forward " \t")
  (let ((settings nil))
    (while (looking-at "[A-Za-z-]")
      (let ((option (let ((start (point)))
		      (skip-chars-forward "A-Za-z-")
		      (buffer-substring start (point)))))
	(cond ((looking-at "=")
		(progn (forward-char 1)
		       (setq settings
			 (cons (cons option (ex-scan-quoted nil " \t|\n"))
			       settings))))
	      ((looking-at "?")
		(progn (forward-char 1)
		       (setq settings
			 (cons (cons option ??) settings))))
	      (t
		(setq settings (cons (cons option t) settings)))))
      (skip-chars-forward " \t"))
    (if (looking-at "[^|\n]")
      (evi-error "Invalid setting%s"
		 (if settings (format " after `%s'" (car (car settings))) "")))
    settings))

(defun evi-get-option (option)
  (let* ((option-struct
	   (or (evi-search-option-list evi-option-list option)
	       (if (and (> (length option) 2)
			(= (aref option 0) ?n) (= (aref option 1) ?o))
		   (evi-search-option-list evi-option-list
					   (substring option 2)))))
	 (type (nth 1 option-struct)))
    (if (eq type nil)
      (evi-error "Invalid option `%s'" option)
      (let* ((long-name (car option-struct))
	     (value (condition-case code
			(eval (cdr (cdr option-struct)))
		      (error nil))))
	(cond
	  ((eq (cdr (cdr option-struct)) nil)
	    (if (or evi-interactive evi-report-unsupported-options)
		(evi-error "Option `%s' not implemented" long-name)
	      (concat long-name "=<ignored>")))
	  ((eq type 'bool)
	    (if (eq value t) long-name (concat "no" long-name)))
	  ((eq type 'number)
	    (concat long-name "=" (if value (int-to-string value) "<undef>")))
	  ((eq type 'string)
	    (concat long-name "="
		    (if value (evi-pretty-string value) "<undef>")))
	  ((eq type 'char)
	    (concat long-name "="
		    (if value (evi-pretty-char value) "<undef>")))
	  (t
	    (evi-error "Internal Error: Invalid type `%s'"
		       (prin1-to-string type))))))))

(defun evi-set-option (option value)
  (let* ((option-struct
	   (or (evi-search-option-list evi-option-list option)
	       (if (and (> (length option) 2)
			(= (aref option 0) ?n) (= (aref option 1) ?o))
		   (prog1
		       (evi-search-option-list evi-option-list
					       (substring option 2))
		     (setq value nil)))))
	 (type (nth 1 option-struct)))
    (cond
      ((eq type nil)
        (evi-warning "Invalid option `%s'" option))
      ((eq (cdr (cdr option-struct)) nil)
	(if (or evi-interactive evi-report-unsupported-options)
	    (evi-warning "Option `%s' not implemented" (car option-struct))))
      ((eq type 'bool)
        (if (stringp value)
	    (progn
	      (evi-warning "Only %s or no%s allowed" option option)
	      (setq option-struct nil))))
      ((eq type 'number)
        (if (stringp value)
	    (setq value (string-to-int value))
	  (evi-warning "Use %s=<number> to set, or %s? to query" option option)
	  (setq option-struct nil)))
      ((eq type 'string)
        (or (stringp value)
	    (progn
	      (evi-warning
		"Use %s=<string> to set, or %s? to query" option option)
	      (set option-struct nil))))
      ((eq type 'char)
        (if (stringp value)
	    (if (= (length value) 1)
		(setq value (aref value 0))
	      (evi-warning 
		"Only single character can be assigned to `%s'" option)
	      (setq option-struct nil))
	  (evi-warning
	    "Use %s=<character> to set, or %s? to query" option option)
	  (setq option-struct nil)))
      (t
	(evi-error "Internal Error: Invalid type `%s'"
		   (prin1-to-string type))))
    (if (cdr (cdr option-struct))
	(progn (set (cdr (cdr option-struct)) value)
	       (or (evi-find opt evi-set-options (equal opt option))
		   (if evi-set-options
		       (nconc evi-set-options (list option))
		     (setq evi-set-options (list option))))))
    (if (fboundp (cdr (cdr option-struct)))
	(funcall (cdr (cdr option-struct)) value))))

(defun evi-search-option-list (option-list option)
  (evi-find option-struct option-list
	    (let ((option-strings (car option-struct)))
	      (if (evi-string-list-match option-strings option)
		  (cons (car option-strings) (cdr option-struct))))))

(defun evi-string-list-match (string-list string)
  (if string-list
    (if (string= string (car string-list))
	t
	(evi-string-list-match (cdr string-list) string))))

(defvar evi-shell-mode-hook nil)

(defun evi-shell-mode-setup ()
  (run-hooks 'evi-shell-mode-hook)
  (set (make-local-variable 'evi-insert-mode-local-bindings) t)
  (set (make-local-variable 'evi-wrap-margin) 0)
  (evi-wrap-margin 0)
  (set (make-local-variable 'evi-emacs-local-suppress-key-list) '(?\e))
  (evi)
  (setq evi-buffer-local-vi-map evi-shell-map))

(defun evi-shell-send-input ()
  (interactive)
  (if (fboundp 'comint-mode)
      (progn
	(comint-send-input)
	(if (eq evi-mode 'vi)
	    (progn
	      (goto-char (process-mark (get-buffer-process (current-buffer))))
	      (evi-insert))))
    (shell-send-input)))

(defun ex-shell ()
  (let ((evi-shell-mode-hook
	 (if (boundp 'shell-mode-hook) shell-mode-hook nil))
	(shell-mode-hook 'evi-shell-mode-setup))
    (shell)
    (evi-insert)))

(defun ex-gdb (program-name)
  (let ((evi-shell-mode-hook
	 (if (boundp 'gdb-mode-hook) gdb-mode-hook nil))
	(gdb-mode-hook 'evi-shell-mode-setup))
    (gdb program-name)
    (evi-insert)))

(defun ex-source-file (file-name)
  (if (file-exists-p file-name)
      (if (file-readable-p file-name)
	  (ex-recurse (list 'evi-do-ex-command-file file-name))
	(evi-warning "Unable to read file `%s'" file-name))
    (evi-warning "No such file or directory: %s" file-name)))

(defun ex-substitute (addresses pattern replacement global query)
  (let ((case-fold-search evi-ignore-case)
	(start (point))
	(end-line-mark (make-marker))
	(none-found t)
	(end-pos (point))
	(large-region))
    (ex-define-region addresses t nil)
    (if pattern
	(if (= (length pattern) 0)
	    (if ex-previous-re
		(setq pattern ex-previous-re)
	      (goto-char start)
	      (evi-error "No previous regular expression"))
	  (setq ex-previous-re pattern))
      (if ex-previous-replacement
	  (setq pattern ex-previous-re
		replacement ex-previous-replacement)
	(goto-char start)
	(evi-error "No previous substitution")))
    (or replacement
	(setq replacement ""))
    (setq ex-previous-replacement replacement)
    ; there are problems with global subst'ing just the beginning or end of a
    ; line, but in those cases you can only match one per line anyway, so
    ; demote to a non-global search
    (if (or (= (aref pattern 0) ?^)
	    (= (aref pattern 0) ?$))
	(setq global nil))
    (evi-exchange-point-and-mark)
    (setq large-region (> (- evi-mark (point)) 5000))
    (if large-region
      (message "running substitute command... "))
    (set-marker end-line-mark evi-mark)
    (while (and (< (point) end-line-mark)
		(re-search-forward pattern end-line-mark t))
      (goto-char (match-beginning 0))
      (setq none-found nil
	    end-pos (point))
      (ex-replace-match query replacement)
      (or global
	  (forward-line)))
    (if large-region
      (message "running substitute command... complete."))
    (set-marker end-line-mark nil)
    (goto-char end-pos)
    (if none-found
	(evi-error "No occurance of pattern `%s' found" pattern))))

(defun ex-substitute-again (addresses)
  (ex-substitute addresses "" "" nil nil))

(if (and (eq evi-emacs-version 'emacs19) (eq window-system 'x))
    (defun evi-hilight-region (start end)
      (let ((ov (make-overlay start (1+ end))))
	(overlay-put ov 'face 'region)
	(sit-for 99999)
	(delete-overlay ov)))
  (defun evi-hilight-region (start end)
    (let ((here (point))
	  (flag nil)
	  (going t))
      (goto-char start)
      (while going
	(if (not (sit-for 1))
	    (setq going nil)
	  (goto-char (if flag start end))
	  (setq flag (not flag))))
      (goto-char here))))

(defun ex-replace-match (query replacement)
  (if (or (not query)
	  (let ((beginning (match-beginning 0))
		(end (match-end 0))
		(answer nil))
	    (while (not answer)
	      (message "replace? (y or n)")
	      (evi-hilight-region beginning (1- end))
	      (setq answer (evi-read-char))
	      ;; ZZ - a bit hardcoded
	      (if (= answer ?\C-c)
		  (keyboard-quit))
	      (if (and (/= answer ?y) (/= answer ?n)
		       (/= answer ?Y) (/= answer ?N))
		  (progn (beep)
			 (setq answer nil))))
	    (or (= answer ?y) (= answer ?Y))))
      ; need to worry about `magic' here?
      (replace-match replacement t nil)
    (goto-char (match-end 0))))

(defun ex-tag (tag)
  (if tag
      (setq ex-tag tag)
    (or ex-tag
	(evi-error "No previous tag specified")))
  (find-tag ex-tag)
  (evi))

(defun ex-unabbrev (abbrev)
  (let ((alist evi-abbrev-list)
	(alist2 nil))
    (while alist
      (if (string= abbrev (car (car alist)))
	  (progn
	    (if alist2
		(setcdr alist2 (cdr alist))
	      (setq evi-abbrev-list (cdr alist)))
	    (setq alist nil))
	(setq alist2 alist alist (cdr alist))))))

(defun ex-unmap (exclam key)
  (if exclam
    (evi-define-key '(input-map) key nil)
    (evi-define-key '(map) key nil)))

(defun ex-evi-version ()
  (message evi-version))

(defun ex-write (addresses exclam shell-command append file-arg)
  (if shell-command
      (if (eq shell-command t)
	  (evi-error "Incomplete shell escape")
	(let ((region (save-excursion (ex-define-region addresses t t)
				      (cons evi-mark (point)))))
	  (evi-display-and-prompt
	    (function
	      (lambda (cmd)
		(shell-command-on-region (car region) (cdr region) cmd)
		(if (eq evi-emacs-version 'emacs19)
		    (and (get-buffer "*Shell Command Output*")
			 (save-excursion
			   (set-buffer "*Shell Command Output*")
			   (> (count-lines (point-min) (point-max)) 1)))
		  t)))
	   (list shell-command))))
    (let ((file-name (or file-arg buffer-file-name)))
      (if (or exclam
	      (and buffer-file-name
		   (string= (file-truename buffer-file-name)
			    (file-truename (if (= (aref file-name 0) ?/)
					       file-name
					     (concat (evi-current-directory)
						     file-name)))))
	      (not (file-exists-p file-name)))
	  (if (or exclam file-arg (not evi-buffer-read-only))
	      (save-excursion
		(ex-define-region addresses t t)
		(if (and (null file-arg)
			 (= evi-mark (point-min)) (= (point) (point-max)))
		    (progn
		      ;; force a write, even if not modified
		      (set-buffer-modified-p t)
		      (basic-save-buffer))
		  (write-region evi-mark (point) file-name append)))
	    (evi-error "File read-only (use :write! to attempt override)"))
	(evi-error "File exists, use :write! to override")))))

(defun ex-write-all-buffers (quietly)
  (save-some-buffers quietly))

(defun ex-write-kill ()
  (set-buffer-modified-p t)
  (basic-save-buffer)
  (ex-kill-buffer nil nil))

(defun ex-write-quit (discard)
  (set-buffer-modified-p t)
  (basic-save-buffer)
  (ex-quit discard))

(defun ex-write-all-and-quit (quietly)
  (save-some-buffers quietly t)
  (ex-quit t))

(defun ex-yank (addresses register-struct)
  (let ((evi-register-spec register-struct))
    (save-excursion
      (ex-define-region addresses t nil)
      (evi-copy-region-to-registers nil))))

(defun ex-shell-command (addresses background shell-command)
  (if (string= shell-command "!")
      (setq shell-command
	(or evi-last-shell-command
	    (evi-error "No previous shell command to substitute for !")))
    (setq evi-last-shell-command shell-command))
  (if background
      (let ((curdir (evi-current-directory)))
	(switch-to-buffer-other-window
	  (get-buffer-create "*Shell Command Output*"))
	(evi)
	(setq default-directory curdir)
	(erase-buffer)
	(start-process shell-command
		       "*Shell Command Output*" "sh" "-c" shell-command)
	(select-window (previous-window)))
    (if (null (car (car (car addresses))))
	(progn
	  (save-excursion
	    (set-buffer (get-buffer-create "*Shell Command Output*"))
	    (evi))
	  (evi-display-and-prompt
	    (function
	      (lambda (cmd)
		(shell-command cmd)
		(if (eq evi-emacs-version 'emacs19)
		    (and (get-buffer "*Shell Command Output*")
			 (save-excursion
			   (set-buffer "*Shell Command Output*")
			   (> (count-lines (point-min) (point-max)) 1)))
		  t)))
	   (list shell-command)))
      (progn (ex-define-region addresses t nil)
	     (shell-command-on-region evi-mark (point) shell-command t)))))

(defun ex-shift-right (addresses)
  (ex-define-region addresses t nil)
  (indent-rigidly evi-mark (point) evi-shift-width)
  (forward-line -1)
  (skip-chars-forward " \t"))

(defun ex-shift-left (addresses)
  (ex-define-region addresses t nil)
  (indent-rigidly evi-mark (point) (- evi-shift-width))
  (forward-line -1)
  (skip-chars-forward " \t"))

(defun ex-null (addresses)
  (ex-define-region addresses t nil)
  (forward-line -1)
  (skip-chars-forward " \t"))

(defvar evi-evi-list "evi-list@brandx.rain.com"
  "Address of site maintaining mailing list for Evi.")

(defvar evi-bug-address "jlewis@cse.ogi.edu"
  "Address of who maintains evi.")

(defun ex-mail (to)
  (mail nil to)
  (evi)
  (message "Type `:send' to send message.  Type `:kill' to abort.")
  (evi-insert))

(defun ex-mail-list (subject)
  (mail nil evi-evi-list subject)
  (evi)
  (goto-char (point-max))
  (insert "Using " evi-version " (" (emacs-version) ").\n\n")
  (message "Type `:send' to send message.  Type `:kill' to abort.")
  (evi-insert))

(defun ex-elisp-bind (input key definition)
  (funcall 'evi-define-key (if input '(insert replace ex) '(vi))
			   key (car (read-from-string definition))))

(defun ex-report-bug (subject)
  (mail nil evi-bug-address subject)
  (evi)
  (goto-char (point-max))
  (insert "In " evi-version " (" (emacs-version) ")\n\n")
  (message "Type `:send' to send bug report.  Type `:kill' to abort.")
  (evi-insert))

(defun ex-send-mail (exclam)
  (mail-send)
  (if exclam
      (ex-kill-buffer t nil)))

(provide 'evi)
