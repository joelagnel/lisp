;;; Saved through ges-version 0.3.3dev at 2004-05-14 13:13
;;; From: usenet@heslin.eclipse.co.uk (Peter Heslin)
;;; Subject: xterm-extras.el
;;; Newsgroups: gnu.emacs.sources
;;; Date: 22 Mar 2004 14:20:12 -0800
;;; Organization: http://groups.google.com

;;; xterm-extras.el --- define additional function key sequences for
;;; recent versions of xterm

;; Copyright (C) 2004 P J Heslin

;; Author: Peter Heslin <p.j.heslin@dur.ac.uk>
;; URL: http://www.dur.ac.uk/p.j.heslin/emacs/download/xterm-extras.el
;; Version: 1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; If you do not have a copy of the GNU General Public License, you
;; can obtain one by writing to the Free Software Foundation, Inc., 59
;; Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; Commentary:

;; This file provides some extra emacs keybindings for the escape
;; sequences transmitted by recent versions of xterm.  It will not
;; work with the older versions of xterm that are still often found
;; in use.  An up-to-date version of xterm can be obtained from
;; http://dickey.his.com/xterm/xterm.html. It includes a file called
;; ctlseqs.ms, which documents the treatment of control sequences by
;; recent xterms.

;; This implementation is somewhat limited in that it assumes a
;; standard PC keyboard and that the Alt key is being used for Meta.
;; Since both xterm and emacs support the use of a Meta key in
;; addition to the Alt key, this file probably should support that
;; usage, but as of yet it does not.  In fact, this file takes the
;; modifier that xterm refers to as "alt" and maps it right to the
;; emacs "meta".  For many users this is convenient, but for some it
;; will be wrong.

;; These key combinations should also work with GNU screen, provided
;; that the TERM environment variable is set to "xterm" rather than
;; "screen".  This can be done in the .screenrc configuration file
;; or by using the "-T" switch like so: screen -T xterm

;; To use, put this file in your load-path, and put these lines in
;; your .emacs file:
;;
;;   (when (string= "xterm" (getenv "TERM"))
;;     (require 'xterm-extras)
;;     (xterm-extra-keys))

;; For some extra keybindings, beyond those available from xterm by
;; default, try putting the following settings in your X resources
;; file:
;;
;; XTerm*eightBitInput: 		false
;; XTerm*metaSendsEscape: 		true
;; XTerm*backarrowKey:    		false
;; XTerm*modifier:                     meta
;; 
;; XTerm.VT100.Translations: #override \
;;  ~Ctrl ~Meta  Shift <Key> Tab:        string(0x1b) string("[z2a")     \n\
;;  ~Ctrl  Meta ~Shift <Key> Tab:        string(0x1b) string("[z3a")     \n\
;;  ~Ctrl  Meta  Shift <Key> Tab:        string(0x1b) string("[z4a")     \n\
;;   Ctrl ~Meta ~Shift <Key> Tab:        string(0x1b) string("[z5a")     \n\
;;   Ctrl ~Meta  Shift <Key> Tab:        string(0x1b) string("[z6a")     \n\
;;   Ctrl  Meta ~Shift <Key> Tab:        string(0x1b) string("[z7a")     \n\
;;   Ctrl  Meta  Shift <Key> Tab:        string(0x1b) string("[z8a")     \n\
;; \
;;  ~Ctrl ~Meta  Shift <Key> Return:     string(0x1b) string("[z2b")     \n\
;;  ~Ctrl  Meta ~Shift <Key> Return:     string(0x1b) string("[z3b")     \n\
;;  ~Ctrl  Meta  Shift <Key> Return:     string(0x1b) string("[z4b")     \n\
;;   Ctrl ~Meta ~Shift <Key> Return:     string(0x1b) string("[z5b")     \n\
;;   Ctrl ~Meta  Shift <Key> Return:     string(0x1b) string("[z6b")     \n\
;;   Ctrl  Meta ~Shift <Key> Return:     string(0x1b) string("[z7b")     \n\
;;   Ctrl  Meta  Shift <Key> Return:     string(0x1b) string("[z8b")     \n\
;; \
;;  ~Ctrl ~Meta  Shift <Key> BackSpace:     string(0x1b) string("[z2c")  \n\
;;  ~Ctrl  Meta ~Shift <Key> BackSpace:     string(0x1b) string("[z3c")  \n\
;;  ~Ctrl  Meta  Shift <Key> BackSpace:     string(0x1b) string("[z4c")  \n\
;;   Ctrl ~Meta ~Shift <Key> BackSpace:     string(0x1b) string("[z5c")  \n\
;;   Ctrl ~Meta  Shift <Key> BackSpace:     string(0x1b) string("[z6c")  \n\
;;   Ctrl  Meta ~Shift <Key> BackSpace:     string(0x1b) string("[z7c")  \n\
;;   Ctrl  Meta  Shift <Key> BackSpace:     string(0x1b) string("[z8c")  \n\
;; \
;;  ~Ctrl ~Meta ~Shift <Key> Pause:       string(0x1b) string("[zd")     \n\
;;  ~Ctrl ~Meta  Shift <Key> Pause:       string(0x1b) string("[z2d")    \n\
;;  ~Ctrl  Meta ~Shift <Key> Pause:       string(0x1b) string("[z3d")    \n\
;;  ~Ctrl  Meta  Shift <Key> Pause:       string(0x1b) string("[z4d")    \n\
;;   Ctrl ~Meta ~Shift <Key> Pause:       string(0x1b) string("[z5d")    \n\
;;   Ctrl ~Meta  Shift <Key> Pause:       string(0x1b) string("[z6d")    \n\
;;   Ctrl  Meta ~Shift <Key> Pause:       string(0x1b) string("[z7d")    \n\
;;   Ctrl  Meta  Shift <Key> Pause:       string(0x1b) string("[z8d")    \n\
;; \
;;  ~Ctrl ~Meta ~Shift <Key> Sys_Req:     string(0x1b) string("[ze")     \n\
;;  ~Ctrl ~Meta  Shift <Key> Sys_Req:     string(0x1b) string("[z2e")    \n\
;;   Ctrl ~Meta ~Shift <Key> Sys_Req:     string(0x1b) string("[z5e")    \n\
;;   Ctrl ~Meta  Shift <Key> Sys_Req:     string(0x1b) string("[z6e")    \n


;; Code:

(defun xterm-extra-remap-function-keys ()
  ;; If the system has an up-to-date xterm termcap entry, these escapes 
  ;; have already been bound, so we remap them to suit our purposes
  (define-key key-translation-map [(f13)] [S-f1])
  (define-key key-translation-map [(f14)] [S-f2])
  (define-key key-translation-map [(f15)] [S-f3])
  (define-key key-translation-map [(f16)] [S-f4])
  (define-key key-translation-map [(f17)] [S-f5])
  (define-key key-translation-map [(f18)] [S-f6])
  (define-key key-translation-map [(f19)] [S-f7])
  (define-key key-translation-map [(f20)] [S-f8])
  (define-key key-translation-map [(f21)] [S-f9])
  (define-key key-translation-map [(f22)] [S-f10])
  (define-key key-translation-map [(f23)] [S-f11])
  (define-key key-translation-map [(f24)] [S-f12])

  (define-key key-translation-map [(f25)] [C-f1])
  (define-key key-translation-map [(f26)] [C-f2])
  (define-key key-translation-map [(f27)] [C-f3])
  (define-key key-translation-map [(f28)] [C-f4])
  (define-key key-translation-map [(f29)] [C-f5])
  (define-key key-translation-map [(f30)] [C-f6])
  (define-key key-translation-map [(f31)] [C-f7])
  (define-key key-translation-map [(f32)] [C-f8])
  (define-key key-translation-map [(f33)] [C-f9])
  (define-key key-translation-map [(f34)] [C-f10])
  (define-key key-translation-map [(f35)] [C-f11])
  (define-key key-translation-map [(f36)] [C-f12])

  (define-key key-translation-map [(f37)] [S-C-f1])
  (define-key key-translation-map [(f38)] [S-C-f2])
  (define-key key-translation-map [(f39)] [S-C-f3])
  (define-key key-translation-map [(f40)] [S-C-f4])
  (define-key key-translation-map [(f41)] [S-C-f5])
  (define-key key-translation-map [(f42)] [S-C-f6])
  (define-key key-translation-map [(f43)] [S-C-f7])
  (define-key key-translation-map [(f44)] [S-C-f8])
  (define-key key-translation-map [(f45)] [S-C-f9])
  (define-key key-translation-map [(f46)] [S-C-f10])
  (define-key key-translation-map [(f47)] [S-C-f11])
  (define-key key-translation-map [(f48)] [S-C-f12]))

(defun xterm-extra-bind-keys ()
  (let ((map (make-sparse-keymap)))

    ;; This section is taken verbatim from term/xterm.el, which we are
    ;; going to overrride here, so we include these bindings again

    (define-key map "\e[A" [up])
    (define-key map "\e[B" [down])
    (define-key map "\e[C" [right])
    (define-key map "\e[D" [left])
    (define-key map "\e[1~" [home])
    (define-key map "\e[2~" [insert])
    (define-key map "\e[3~" [delete])
    (define-key map "\e[4~" [select])
    (define-key map "\e[5~" [prior])
    (define-key map "\e[6~" [next])
    (define-key map "\e[11~" [f1])
    (define-key map "\e[12~" [f2])
    (define-key map "\e[13~" [f3])
    (define-key map "\e[14~" [f4])
    (define-key map "\e[15~" [f5])
    (define-key map "\e[17~" [f6])
    (define-key map "\e[18~" [f7])
    (define-key map "\e[19~" [f8])
    (define-key map "\e[20~" [f9])
    (define-key map "\e[21~" [f10])
    (define-key map "\e[23~" [f11])
    (define-key map "\e[24~" [f12])
    (define-key map "\e[29~" [print])

    (define-key map "\e[2;2~" [S-insert])
    (define-key map "\e[3;2~" [S-delete])
    (define-key map "\e[5;2~" [S-prior])
    (define-key map "\e[6;2~" [S-next])

    (define-key map "\e[2;5~" [C-insert])
    (define-key map "\e[3;5~" [C-delete])
    (define-key map "\e[5;5~" [C-prior])
    (define-key map "\e[6;5~" [C-next])

    (define-key map "\eOA" [up])
    (define-key map "\eOB" [down])
    (define-key map "\eOC" [right])
    (define-key map "\eOD" [left])
    (define-key map "\eOF" [end])
    (define-key map "\eOH" [home])

    (define-key map "\eO2A" [S-up])
    (define-key map "\eO2B" [S-down])
    (define-key map "\eO2C" [S-right])
    (define-key map "\eO2D" [S-left])
    (define-key map "\eO2F" [S-end])
    (define-key map "\eO2H" [S-home])

    (define-key map "\eO5A" [C-up])
    (define-key map "\eO5B" [C-down])
    (define-key map "\eO5C" [C-right])
    (define-key map "\eO5D" [C-left])
    (define-key map "\eO5F" [C-end])
    (define-key map "\eO5H" [C-home])

    ;; Even if the local machine has an up-to-date xterm and termcap
    ;; entry, the remote machine may not have an up-to-date termcap, in
    ;; which case, the escapes dealt with in the function above
    ;; (xterm-extras-remap-function-keys) will not have been bound to any
    ;; mapping, so we must bind the escapes here.  We probably ought not
    ;; to do both, but I don't know how to detect what sort of termcap
    ;; entry for xterm the host machine has.

    (define-key map "\eO2P"    [S-f1])
    (define-key map "\eO2Q"    [S-f2])
    (define-key map "\eO2R"    [S-f3])
    (define-key map "\eO2S"    [S-f4])
    (define-key map "\e[15;2~" [S-f5])
    (define-key map "\e[17;2~" [S-f6])
    (define-key map "\e[18;2~" [S-f7])
    (define-key map "\e[19;2~" [S-f8])
    (define-key map "\e[20;2~" [S-f9])
    (define-key map "\e[21;2~" [S-f10])
    (define-key map "\e[23;2~" [S-f11])
    (define-key map "\e[24;2~" [S-f12])
    
    (define-key map "\eO5P"    [C-f1])
    (define-key map "\eO5Q"    [C-f2])
    (define-key map "\eO5R"    [C-f3])
    (define-key map "\eO5S"    [C-f4])
    (define-key map "\e[15;5~" [C-f5])
    (define-key map "\e[17;5~" [C-f6])
    (define-key map "\e[18;5~" [C-f7])
    (define-key map "\e[19;5~" [C-f8])
    (define-key map "\e[20;5~" [C-f9])
    (define-key map "\e[21;5~" [C-f10])
    (define-key map "\e[23;5~" [C-f11])
    (define-key map "\e[24;5~" [C-f12])
  
    (define-key map "\eO6P"    [S-C-f1])
    (define-key map "\eO6Q"    [S-C-f2])
    (define-key map "\eO6R"    [S-C-f3])
    (define-key map "\eO6S"    [S-C-f4])
    (define-key map "\e[15;6~" [S-C-f5])
    (define-key map "\e[17;6~" [S-C-f6])
    (define-key map "\e[18;6~" [S-C-f7])
    (define-key map "\e[19;6~" [S-C-f8])
    (define-key map "\e[20;6~" [S-C-f9])
    (define-key map "\e[21;6~" [S-C-f10])
    (define-key map "\e[23;6~" [S-C-f11])
    (define-key map "\e[24;6~" [S-C-f12])

    ;; These meta-modified combinations are not defined in termcap or
    ;; term/xterm.el at all.  I am assuming here that the Alt key is
    ;; being used for Meta
    
    (define-key map "\eO3P"    [M-f1])
    (define-key map "\eO3Q"    [M-f2])
    (define-key map "\eO3R"    [M-f3])
    (define-key map "\eO3S"    [M-f4])
    (define-key map "\e[15;3~" [M-f5])
    (define-key map "\e[17;3~" [M-f6])
    (define-key map "\e[18;3~" [M-f7])
    (define-key map "\e[19;3~" [M-f8])
    (define-key map "\e[20;3~" [M-f9])
    (define-key map "\e[21;3~" [M-f10])
    (define-key map "\e[23;3~" [M-f11])
    (define-key map "\e[24;3~" [M-f12])
    
    (define-key map "\eO4P"    [S-M-f1])
    (define-key map "\eO4Q"    [S-M-f2])
    (define-key map "\eO4R"    [S-M-f3])
    (define-key map "\eO4S"    [S-M-f4])
    (define-key map "\e[15;4~" [S-M-f5])
    (define-key map "\e[17;4~" [S-M-f6])
    (define-key map "\e[18;4~" [S-M-f7])
    (define-key map "\e[19;4~" [S-M-f8])
    (define-key map "\e[20;4~" [S-M-f9])
    (define-key map "\e[21;4~" [S-M-f10])
    (define-key map "\e[23;4~" [S-M-f11])
    (define-key map "\e[24;4~" [S-M-f12])
  
    ;; These are often unavailable under X (because Alt-Ctrl-Fx is
    ;; used by default to change VT), but for the sake of completeness
    ;; they are included here.

    (define-key map "\eO7P"    [M-C-f1])
    (define-key map "\eO7Q"    [M-C-f2])
    (define-key map "\eO7R"    [M-C-f3])
    (define-key map "\eO7S"    [M-C-f4])
    (define-key map "\e[15;7~" [M-C-f5])
    (define-key map "\e[17;7~" [M-C-f6])
    (define-key map "\e[18;7~" [M-C-f7])
    (define-key map "\e[19;7~" [M-C-f8])
    (define-key map "\e[20;7~" [M-C-f9])
    (define-key map "\e[21;7~" [M-C-f10])
    (define-key map "\e[23;7~" [M-C-f11])
    (define-key map "\e[24;7~" [M-C-f12])
    
    (define-key map "\eO8P"    [S-M-C-f1])
    (define-key map "\eO8Q"    [S-M-C-f2])
    (define-key map "\eO8R"    [S-M-C-f3])
    (define-key map "\eO8S"    [S-M-C-f4])
    (define-key map "\e[15;8~" [S-M-C-f5])
    (define-key map "\e[17;8~" [S-M-C-f6])
    (define-key map "\e[18;8~" [S-M-C-f7])
    (define-key map "\e[19;8~" [S-M-C-f8])
    (define-key map "\e[20;8~" [S-M-C-f9])
    (define-key map "\e[21;8~" [S-M-C-f10])
    (define-key map "\e[23;8~" [S-M-C-f11])
    (define-key map "\e[24;8~" [S-M-C-f12])

    ;; A number of these are usually not available: for example, Shift +
    ;; Prior (and any other modifier) normally scrolls up; but for
    ;; completeness they are included here.
 
    (define-key map "\e[5;2~"    [S-prior])
    (define-key map "\e[5;3~"    [M-prior])
    (define-key map "\e[5;4~"    [M-S-prior])
    (define-key map "\e[5;5~"    [C-prior])
    (define-key map "\e[5;6~"    [C-S-prior])
    (define-key map "\e[5;7~"    [M-C-prior])
    (define-key map "\e[5;8~"    [M-C-S-prior])
    
    (define-key map "\e[6;2~"    [S-next])
    (define-key map "\e[6;3~"    [M-next])
    (define-key map "\e[6;4~"    [M-S-next])
    (define-key map "\e[6;5~"    [C-next])
    (define-key map "\e[6;6~"    [C-S-next])
    (define-key map "\e[6;7~"    [M-C-next])
    (define-key map "\e[6;8~"    [M-C-S-next])

    ;; The simple shift-key and control-key combinations are already
    ;; defined in the section from term/xterm.el reproduced above.

    (define-key map "\eO3H"    [M-home])
    (define-key map "\eO4H"    [M-S-home])
    (define-key map "\eO6H"    [C-S-home])
    (define-key map "\eO7H"    [M-C-home])
    (define-key map "\eO8H"    [M-C-S-home])
    
    (define-key map "\eO3F"    [M-end])
    (define-key map "\eO4F"    [M-S-end])
    (define-key map "\eO6F"    [C-S-end])
    (define-key map "\eO7F"    [M-C-end])
    (define-key map "\eO8F"    [M-C-S-end])

    (define-key map "\eO3A"    [M-up])
    (define-key map "\eO4A"    [M-S-up])
    (define-key map "\eO6A"    [C-S-up])
    (define-key map "\eO7A"    [M-C-up])
    (define-key map "\eO8A"    [M-C-S-up])
    
    (define-key map "\eO3B"    [M-down])
    (define-key map "\eO4B"    [M-S-down])
    (define-key map "\eO6B"    [C-S-down])
    (define-key map "\eO7B"    [M-C-down])
    (define-key map "\eO8B"    [M-C-S-down])

    (define-key map "\eO3C"    [M-right])
    (define-key map "\eO4C"    [M-S-right])
    (define-key map "\eO6C"    [C-S-right])
    (define-key map "\eO7C"    [M-C-right])
    (define-key map "\eO8C"    [M-C-S-right])

    (define-key map "\eO3D"    [M-left])
    (define-key map "\eO4D"    [M-S-left])
    (define-key map "\eO6D"    [C-S-left])
    (define-key map "\eO7D"    [M-C-left])
    (define-key map "\eO8D"    [M-C-S-left])

    ;; Shift+insert combinations may not be available, if they are set
    ;; to paste from the clipboard
    
    (define-key map "\e[2;2~"    [S-insert])
    (define-key map "\e[2;3~"    [M-insert])
    (define-key map "\e[2;4~"    [M-S-insert])
    (define-key map "\e[2;5~"    [C-insert])
    (define-key map "\e[2;6~"    [C-S-insert])
    (define-key map "\e[2;7~"    [M-C-insert])
    (define-key map "\e[2;8~"    [M-C-S-insert])
  
    (define-key map "\e[3;2~"    [S-delete])
    (define-key map "\e[3;3~"    [M-delete])
    (define-key map "\e[3;4~"    [M-S-delete])
    (define-key map "\e[3;5~"    [C-delete])
    (define-key map "\e[3;6~"    [C-S-delete])
    (define-key map "\e[3;7~"    [M-C-delete])
    (define-key map "\e[3;8~"    [M-C-S-delete])  
  
  (set-keymap-parent map (keymap-parent function-key-map))
  (set-keymap-parent function-key-map map)))

(defun xterm-extra-extra-keys ()

  ;; These are escapes that xterm does not provide by default, but you
  ;; can induce xterm to send them, if you configure it to do so, by
  ;; using the x resource settings given at the start of this file

  (define-key function-key-map "\e[z2a"    [S-tab])
  (define-key function-key-map "\e[z3a"    [M-tab])
  (define-key function-key-map "\e[z4a"    [M-S-tab])
  (define-key function-key-map "\e[z5a"    [C-tab])
  (define-key function-key-map "\e[z6a"    [S-C-tab])
  (define-key function-key-map "\e[z7a"    [M-C-tab])
  (define-key function-key-map "\e[z8a"    [M-C-S-tab])
  
  (define-key function-key-map "\e[z2b"    [S-return])
  (define-key function-key-map "\e[z3b"    [M-return])
  (define-key function-key-map "\e[z4b"    [M-S-return])
  (define-key function-key-map "\e[z5b"    [C-return])
  (define-key function-key-map "\e[z6b"    [C-S-return])
  (define-key function-key-map "\e[z7b"    [M-C-return])
  (define-key function-key-map "\e[z8b"    [M-C-S-return])
  
  (define-key function-key-map "\e[z2c"    [S-backspace])
  (define-key function-key-map "\e[z3c"    [M-backspace])
  (define-key function-key-map "\e[z4c"    [M-S-backspace])
  (define-key function-key-map "\e[z5c"    [C-backspace])
  (define-key function-key-map "\e[z6c"    [C-S-backspace])
  (define-key function-key-map "\e[z7c"    [M-C-backspace])
  (define-key function-key-map "\e[z8c"    [M-C-S-backspace])

  (define-key function-key-map "\e[zd"     [pause])
  (define-key function-key-map "\e[z2d"    [S-pause])
  (define-key function-key-map "\e[z3d"    [M-pause])
  (define-key function-key-map "\e[z4d"    [M-S-pause])
  (define-key function-key-map "\e[z5d"    [C-pause])
  (define-key function-key-map "\e[z6d"    [C-S-pause])
  (define-key function-key-map "\e[z7d"    [M-C-pause])
  (define-key function-key-map "\e[z8d"    [S-M-C-pause])

  (define-key function-key-map "\e[ze"     [Sys_Req])
  (define-key function-key-map "\e[z2e"    [S-Sys_Req])
  (define-key function-key-map "\e[z5e"    [C-Sys_Req])
  (define-key function-key-map "\e[z6e"    [C-S-Sys_Req]))

(defun xterm-extra-screen-keys ()
  ;; These are necessary when running GNU screen inside an xterm,
  ;; since even when running "screen -t xterm", the $TERMCAP screen
  ;; installs for xterm does not seem to correspond to the actual
  ;; escapes that are sent for a few keys:
  (define-key function-key-map "\e[1~"    [home])
  (define-key function-key-map "\e[2~"    [insert])
  (define-key function-key-map "\e[4~"    [end]))

(defun xterm-extra-keys ()
  (xterm-extra-remap-function-keys)
  (xterm-extra-bind-keys)
  (xterm-extra-extra-keys)
  (xterm-extra-screen-keys))

(provide 'xterm-extras)

