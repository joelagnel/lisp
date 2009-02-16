The is the `doc/tutorial.e' file of GNU eev.
This file is in the Public Domain.
Author and version (of these headers): Eduardo Ochs, 2005jan06
Note: this file is old, and it was never a good tutorial!
Try the README instead: <http://angg.twu.net/eev-current/README.html>.



# «.lisp_hiperlinks»	(to "lisp_hiperlinks")
# «.code-c-d»		(to "code-c-d")
# «.eev»		(to "eev")
# «.delimited_blocks»	(to "delimited_blocks")
# «.eeg»		(to "eeg")
# «.anchors»		(to "anchors")
# «.debian»		(to "debian")
# «.mkto_dff_inn»	(to "mkto_dff_inn")
# «.variables»		(to "variables")
# «.glyphs»		(to "glyphs")
# «.eegud»		(to "eegud")
# «.installing»		(to "installing")
# «.demos_eeg4»		(to "demos_eeg4")
# «.links»		(to "links")




  Emacs and eev.el, or: how to automate almost everything
  =======================================================

Part 1: Emacs, Lisp and hyperlinks
----------------------------------

# «lisp_hiperlinks»  (to ".lisp_hiperlinks")

People usually tend to think that Emacs is a text editor. This is not
exactly true, and it would be more correct to say that it a Lisp
environment that is used most of the time as a text editor.

# (find-elnode "Command Loop")

Each key that you type is interpreted by Emacs either as a complete
command or as the prefix of a longer command, composed of several
keys; for example, C-a (that is, "control-a") generally corresponds to
the function `beginning-of-line', that moves the cursor (called the
"point") to the beginning of the current line; C-x is a prefix, and
C-x C-e is a complete command -- `eval-last-sexp', that evaluates the
Lisp expression that is just before point. Another stranger example is
M-x ("M-x"; in most configurations this is the same as "alt-x"), that
lets us execute commands by name -- M-x eval-last-sexp is the same as
C-x C-e.

# (find-enode "Keys")
# (find-enode "Moving Point" "`beginning-of-line'")
# (find-enode "User Input" "Meta")
# (find-enode "User Input" "<ALT> which is really a <META> key")
# (find-enode "M-x")
# (find-enode "Completion")
# (find-enode "Minibuffer")
# (find-enode "Key Help")

Let's suppose that you are editing the text version of this tutorial.
If you place the point just after the "(+ 1 2)" below and type C-x C-e
you'll se a "3" in the last line of the screen (that line is called
the "minibuffer" or the "echo area"); what happened is that you
executed the Lisp expression "(+ 1 2)", which didn't have any very
interesting consequence, and the expression returned the value 3 - the
sum of 1 and 2.

    (+ 1 2)

# (find-elnode "Lists as Boxes")
# (find-elinode "Run a Program")
# (find-enode "Lisp Eval")
# (find-elnode "Functions for Key Lookup" "Function: key-binding")
# (key-binding "\C-x\C-e")

If you evaluate one of the expressions below (also with C-x C-e,
obviously) Emacs will open the file /etc/inittab, in the first case,
and the "file" ~/ -- your home directory -- in the latter; as ~/ is a
directory and not a normal file Emacs will show the contents of
directory in a certain way and enter "Dired mode", a mode in which
certain keys will be interpreted specially -- for example "q" will
leave Dired mode and <enter> will open the file or directory that is
being shown in the current line.

    (find-file "/etc/inittab")
    (find-file "~/")

As the function `find-file' is bound to the key sequence C-x C-f it
would be equivalent to type C-x C-f /etc/inittab or C-x C-f ~/.

# (where-is 'find-file)
# (find-enode "Dired")
# (find-enode "Visiting" "`find-file'")

If you are running an Emacs in which the eev.el library was loaded
then several other functions similar to `find-file' will be available,
and you can use those functions to create hyperlinks in plain text
files; the functions `find-enode', `find-elnode' and `find-elinode',
that have already appeared above without any explanation, are links to
pages (in "info" format) of the Emacs Manual, of the Emacs Lisp
Reference Manual, and of a manual called "Introduction to Emacs Lisp",
respectively.

Calls to these `find-xxx' functions may have an extra parameter
besides the name of the file or the page; that parameter is
interpreted as a string to search for, and the cursor will be placed
just after the first occurence of that string in the text. Note that
that feature doesn't work in the HTMLized version of these pages; the
link takes your browser to the right page, but doesn't make it search
for a string automatically.

# (find-eev "eev-links.el" "find-")
# (find-eev "eev-links.el" "find-" "defun ee-goto-rest")

The `find-fline' function, that is defined in eev.el, is a version of
`find-file' that accepts that extra parameter indicating a position.
It has that weird name because when I started to code those functions,
back in 1995, the extra parameter was always a number, that was
interpreted as a line number. Nowadays those numeric parameters are
still accepted, but they aren't very convenient to use -- if the file
is changed then the paragraph that you are looking for will probably
start at a different line number, but if you search for the right
string or keyword you will still get there.

# «code-c-d»  (to ".code-c-d")

The eev library also defines several other kinds of hyperlinks: to
files, to tags, to HTML documents, to manpages, and some other weirder
ones. Some of the ways to link to manpages and HTML pages will require
tricks from the next section, as they will involve interacting with a
shell.

The main tool for generating new hyperlink functions is the `code-c-d'
function: a call to `code-c-d' like the one below

  (code-c-d "grub" "/usr/src/grub-0.5.95/" "grub")

defines `find-grubfile', `find-grubnode', `find-grubw3',
`find-grubtag' and some other less interesting functions... if you
know Lisp and you want the technical details, when the `code-c-d' line
above is executed Emacs generates the code belows (as a big string)
and evaluates it:

  (setq ee-grubdir "/usr/src/grub-0.5.95/")
  (setq ee-grubtagsfile "/usr/src/grub-0.5.95/TAGS")
  (defun ee-grubfile (str)
    (concat (substitute-in-file-name ee-grubdir) str))
  (defun ee-use-grub-tags ()
    (setq tags-file-name ee-grubtagsfile))
  (defun find-grubfile (str &rest pos-spec-list)
    (ee-use-grub-tags)
    (apply 'find-fline-nosubst (ee-grubfile str) pos-spec-list))
  (defun find-grubtag (str)
    (ee-use-grub-tags) (find-tag str))
  (defun find-grubw3 (furl &rest pos-spec-list)
    (apply 'find-w3 (ee-grubfile furl) pos-spec-list))
  (setq ee-temp-code "grub" ee-temp-infofile "grub")

  (defun find-grubnode (nodename &rest pos-spec-list)
    (find-node2 "grub" nodename pos-spec-list  "grub")))

A detail: the last `defun' wouldn't be generated if we hadn't passed
the third parameter ("grub") to `code-c-d'.

# (find-eev "eev-links.el" "code-c-d")
# (find-elinode "defun")

# (find-eev "eev-links.el" "code-c-d" "defun icode-c-d")
# (icode-c-d "grub" "/usr/src/grub-0.5.95/" "grub")




Part 2: Interpreting a block of text as a series of shell commands
------------------------------------------------------------------

# «eev»  (to ".eev")

Consider the block below:

rm -Rv /tmp/c/
mkdir  /tmp/c/
cd     /tmp/c/
cat > demo.c <<'---'
#include <stdio.h>
main() {
  int sum, i;
  for(sum=0, i=0; i<10; ++i)
    sum += i;
  printf("Sum: %d\n", sum);
}
---
gcc -E demo.c > demo.i 
gcc -S demo.c
gcc -S demo.c -g -o demog.s
gcc -c demo.c
gcc -c demo.c -g -o demog.o
gcc    demo.c -o demo -v	2>&1 | tee ogv
gcc    demo.c -o demog -g -v	2>&1 | tee ogvg
diff ogv ogvg
./demo

It is a series of shell commands (for bash or zsh); it will create a
directory "/tmp/c/" and file called "demo.c" inside it, and then call
gcc in several ways to compile demo.c into something executable and
into several less well-known intermediate forms.

There are several ways to execute a block like that. You could go to a
shell and paste its text there using the mouse, or you could simply
type everything by hand there; or you could save the block to a file
(called "/tmp/xxx", say) using the emacs command `write-region', and
the got to a shell and type "bash /tmp/xxx" or ". /tmp/xxx"...

# (find-enode "M-x")
# (find-enode "Misc File Ops")
# (find-node "(bash)Redirections" "<<")

Eev.el implements two other ways to execute that. The first way -- the
second will be described in the next section -- is to mark the block
and type `M-x eev'; that will save the block in a temporary file
("~/tmp/ee.sh" by default) and then if you type "ee" in a shell
(provided that you have configured that shell correctly, as described
in appendix xxx) then the shell will execute the commands in sequence
in "verbose mode", which means that every command will be displayed
before being run, and that all commands that change the shell state --
like "cd" and "set" -- will work as expected; if we typed the five
lines below

tac <<'---'
  first
  second
---
cd /tmp/

by hand in a shell we would see something like:

/home/edrx(edrx)# tac <<'---'
>   first
>   second
> ---
  second
  first
/home/edrx(edrx)# cd /tmp/
/tmp(edrx)#

and with `M-x eev' and "ee" what we see is:

/home/edrx(edrx)# ee
tac <<'---'
  first
  second
---
  second
  first
cd /tmp/
/tmp(edrx)#

that is close enough; for a way to simulate perfectly what happens
when things are typed by hand see the section [eeg and Expect], below.

[Add note about the behaviour of bash and tcsh on here-documents in
verbose mode; zsh is the only shell that behaves as it should in
that.]

Note (or remember!) that lines beginning with "#" are treated by the
shell as comments, which means that they are not executed, no matter
whet they contain; so we can put Emacs Lisp hyperlinks in comments in
blocks of shell commands and the shell will ignore them... and we will
still be able to follow the hyperlinks when we edit the file in Emacs.

Suppose that we have some file with notes, each one containing a mix
of text, links and blocks of shell code; the links point to all
documentation that we found relevant while we were writing the notes,
and also to other places in other files with notes...

[I need to translate the rest of this paragraph...]

Imagine que temos alguns arquivos de anotações, cada um contendo uma
mistura de texto, links e blocos de código shell; os links apontam
para pontos relevantes em outros dos nossos arquivos de anotações, e
também para outros lugares com documentação importante, e para alguns
scripts que nós escrevemos; esses scripts provavelmente começaram como
blocos de código shell nos arquivos de anotações, e quando nós os
transformamos em scripts nós resolvemos manter os links que eles
continham, já que não havia motivo para tirá-los... bom, com isso o
Emacs acaba virando um ambiente fantástico: cada coisa que escrevemos
aponta para outros lugares relacionados, e seguir os links é facílimo,
e quando os seguimos continuamos dentro do Emacs, de onde os links
continuam funcionando... tudo fica muito próximo e muito acessível.
Para um exemplo realista de quão linkadas as coisas podem ficar, veja
<http://angg.twu.net/>.




Part 3: Delimited blocks
------------------------

# «delimited_blocks»  (to ".delimited_blocks")

If you are running an Emacs in which eev.el was "completely installed"
(i.e., with `ee-invade-global-keymap'; see the appendix) then the F3
key will be bound to an Emacs command called `ee-bounded', that by
default runs `eev-bounded', that, by its turn, act as a sort of `eev'
that finds the limits of the block by itself, searching for certain
delimiters, instead of requiring us to mark the block explicitly. The
default delimiters are "\n#-\n", that is, a line that contain just a
"#-", but I prefer to use "\n#\n"; the "" is a char 15, that can be
typed with `C-q C-o' and that will appear as a red star if you are
using GNU Emacs and have installed the package for "glyphs" that comes
with eev.el. The section [variables] explains how to change the
default globally or for some specific files.

# (find-enode "Inserting Text" "* `C-q'")
# (find-eev "eev.el" "eev")
# (find-eev "eev.el" "bounded")

Example:

#
# (eev-bounded)
# (eev-bounded 'once)
tac <<'---'
  one
  two
---
#
rev <<'---'
  one
  two
---
#

Note that it is possible to execute the block above all at once (i.e.,
ignoring the middle "#") by marking it in the usual way and typing
`M-x eev'; the shell will consider the line with "#" as a comment and
will ignore it.

`eev' is not the only command that has a version supporting
delimiters; eeg and eelatex (two commands that we haven't seen yet)
also have. Example: if you run `M-x eelatex-bounded' from inside the
block below then Emacs will save a temporary script that when executed
with "ee" in a shell will interpret the saved block as a LaTeX text.
Note that it will use "%" as delimiters instead of "#", because
comments in TeX and LaTeX begin with "%". The delimiter string used
for `eelatex-bounded' can also be controlled with the tricks of the
section [variables].

%
% (eelatex-bounded)
% (eelatex-bounded 'once)
A text in \LaTeX

%

If you are not in the mood to type `M-x eelatex-bounded' you can
execute one of the lines of the block that call `eelatex-bounded'...
the eexxx-bounded commands admit an optional parameter, that if
present will mean that the function is to be run "only once", that is,
it should not become the new default action for F3. It is possible to
call these functions in "once" mode using M-x: for example, with `M-1
M-x eelatex-bounded'.

# (find-enode "Completion")
# (find-enode "Arguments")




Part 4: The generic interface: eeg
----------------------------------

# «eeg»  (to ".eeg")

If you know enough about *NIX and shells you should be able to control
most interactive programs using pipes. For example:

#
# (eev-bounded)
echo 1+2 | bc
bc <<'---'
2+3
3+4
---
#

But many interactive programs will behave differently when they notice
that they are receiving their input from a file or a pipe, and
sometimes you may want to get on your screen exactly what you would
you'd get in interactive mode -- by piping input with "<<'xxx'" and
"xxx" you see all the input first, then all the output -- and, of
course, you'd like to get that without having to type everything
manually.

There is a "generic interface" for eev that handles those cases. It is
composed of a program called "eeg", written in Expect, and two Emacs
commands, `M-x eeg' and `M-x eeg-bounded'; `M-x eeg' works like `M-x
eev', but it saves the block in another temporary file, by default
"~/bin/ee.generic"; if everything was installed correctly then the
shell variable $EEG will contain the name of that file, and so in

#
cat > $EEG <<'---'
2+3
3+4
quit
---
eeg bc
#

the "cat" will work as if we had marked the three lines between the
"---"s manually in Emacs and typed `M-x eeg'.

The trick is that when we call bc "through eeg", as in the line "eeg
bc", bc will run normally, in interactive mode, will almost all keys
of the user being sent directly to it, as usual; but there's one key
that is "translated" in a special way: each time the user types `M-l'
eeg will execute an "action" from its list of actions; eeg prepares
this list of actions by reading $EEG, and the first will correspond to
sending "2+3", followed by an <enter> to bc, the second will be "3+4"
plus <enter>, the third "quit" <enter>, and all others will be null.
So, if we execute the block above with `ee' in a shell and we type
`M-l asfdg <enter> M-l M-l', we will see something like

/home/edrx(edrx)# ee
cat > $EEG <<'---'
2+3
3+4
quit
---
eeg bc
bc 1.05
Copyright 1991, 1992, 1993, 1994, 1997, 1998 Free Software Foundation, Inc.
This is free software with ABSOLUTELY NO WARRANTY.
For details type `warranty'.
2+3
5
asdfg
0
3+4
7
quit
/home/edrx(edrx)#

Expect is able to control any program that interacts with the world
via terminals, that is, via stdin, stdout, stderr and /dev/tty; that
includes even "essentially interactive programs" like ssh, telnet,
adduser, emacs and vi, and only excludes the programs that can only be
run through X. The idea is that "everything" (if we are willing to
pretend that programs that only have graphical interfaces "do not
matter" or "do not exist"!) can be controlled by Expect and eeg.

For some programs it may be more convenient to use a variation of eeg
that is not restricted to sending a whole line (terminated by a CR) at
each M-l. Actually this variation -- called eeg4 -- is exactly the
same program as eeg, but when invoked with a different name it runs
with different defaults. At this moment it isn't very well-documented,
so here is a link to its source code:

# (find-eev "eev/eeg4")





Part 5: anchors and local links
--------------------------------

# «anchors»  (to ".anchors")

Sometimes we may want to create "local links" that point to other
positions in the current file; this can be done using the "to"
function. A call like `(to "tag")' (like the one above) will jump to
the first occurrence of the string "«tag»" in the current file; this
string, "«tag»", is obtained from the parameter, "tag", by surrounding
"tag" with the default "anchor delimiters" -- more technically, this
is done in the Lisp code by running `(format ee-anchor-format "tag")',
where the default value for `ee-anchor-format' is "<<%s>>"; this
default can be changed for a specific file by using "file-local
variables", as described in section 7, below. For example, in this
file ee-anchor-format was set to "«%s»".

# (find-eev "eev-links.el" "to_and_back")
# (find-eev "eev.el" "ee-invade-global-")
# (find-eev "eev-vars.el" "variables")

There are several ways to type the "«»" characters. The most standard
(though not very convenient) way is to enter them using their octal
codes, by typing C-q 2 5 3 RET and C-q 2 7 3 RET.

# (find-enode "Inserting Text" "sequence of octal digits")

The function `to' is a particular case of a function that looks for a
certain "anchor" in a certain file (...)

# (find-eev "eev-links.el" "find-anchor")






Part 6: links to information about packages in a Debian system
--------------------------------------------------------------

# «debian»  (to ".debian")
# (find-eev "eev-links.el" "find-Package")
# (find-eev "eev-mklinks.el" "dff")




Part 7: tricks for creating links more quickly
-----------------------------------------------

# «mkto_dff_inn»  (to ".mkto_dff_inn")
# (find-eev "eev-mklinks.el" "inn")
# (find-eev "eev-mklinks.el" "dff")

(Explain also the trick for recording macros)
# (find-es "emacs" "saving_emacs_macros")



Part 8: variables
-----------------

# «variables»  (to ".variables")
# (find-enode "Variables")
# (find-elinode "Variables")
# (find-elnode "Variables")
# (find-eev "eev-vars.el")



Part 9: glyphs
--------------

# «glyphs»  (to ".glyphs")
# (find-eev "glyphs.el")
# (find-angg "vtutil/mathchars.lua")




Part 10: eev and GUD (Emacs's Grand Unified Debugger)
-----------------------------------------------------

# «eegud»  (to ".eegud")

[Show how we made gdb support an `ee' command; explain eegdb,
eegdb-bounded, eegud-gdb, eegud-keys-mode; give a simple example with
just one here-doc'ed C file, and a not-so-simple one with two C files
and dynamic linking (dlopen). Change eegud to make it use
gdb-mode-hook, and fix its behaviour when we try to kill the gdb
window when it's the only one in the screen.]

# (find-eev "eev-gud.el")
# (find-eev "rcfiles/.gdbinit")

# (find-eev "eev-vars.el" "variables")
# (find-eev "rcfiles/.bashrc")
# (find-eev "rcfiles/.cshrc")
# (find-eev "rcfiles/.zshrc")

# (find-enode "Debuggers")
# (find-enode "Starting GUD")
# (find-enode "Commands of GUD")
# (find-enode "GUD Customization")

# (find-es "anatocc" "dlopen")
# (find-es "anatocc" "dlopen-gdb")
# (find-es "lua5" "argpatch-gdb")

# (find-angg ".gdbinit")







Appendix 1: installing eev in a Debian system
---------------------------------------------

# «installing»  (to ".installing")
# (find-eev "INSTALL")



Appendix 2: creating demos with eeg4
------------------------------------

# «demos_eeg4»  (to ".demos_eeg4")
# (find-eev "eeg4")

# (find-eev "demo/")
# (find-eev "demo/dlopen.e")
# (find-eev "demo/dlopen.eeg4")
# (find-eev "demo/emacs0.eeg4")





Appendix 3: the basic kinds of links defined by eev
---------------------------------------------------

# «links»  (to ".links")
# (find-eev "e/links.e")
#
cd ~/eev/
grep defun eev-ALL.el | tee ~/o
#

(find-node "(elisp)Top" "Evaluation")
(find-fline "/etc/fstab")
(find-fline-nosubst "/tmp/~foo")
(find-w3 "/var/www/")

(icode-c-d "CODE" "DIR/" "INFO")

(find-devreftxt "")
(find-pl5pod "syn" "Loop Control")


Links that use anchors:
  (find-anchor "~/eev/e/debian.e" "_partition")
  (find-Package   "/var/lib/dpkg/status" "emacsen-common")
  (find-status    "emacsen-common")
  (find-available "emacsen-common")
  (find-es   "anatocc"  "cdecl")
  (find-angg ".zshrc"   "ee"
  (find-eev  "Makefile" "install")

Links to the source code of Emacs functions and variables:
  (find-efunction 'eev)
  (find-evariable 'ee-file)

Links to the output of programs:
  (find-progoutput "wget --help" "--continue")

Links to manpages (eeman is a hybrid between link and e-script):
  (eeman       "1 man" "System calls")
  (find-man.el "1 man" "System calls")
  (find-man    "1 man" "System calls")

Links to pages of .ps and .dvi files (possibly compressed):
  (find-dvipage 
  (find-zdvipage (fname n)
  (find-pspage (fname n)

;; code-ps
;; code-dvi
;; code-zdvi









# (find-es "anatocc" "cdecl")

find-status
find-vldifile xxx.list

# (find-es "anatocc" "cdecl")
".e"s

# (find-es "anatocc" "dlopen")

alias in .bashrc



;; (describe-function 'find-fline)
;; (describe-function 'find-node)
;; (describe-function 'ee-goto-position)
;; (describe-function 'eev)
;; (describe-function 'eeman)
;; (describe-function 'eecd)
;; (describe-function 'eeg)
;; (describe-function 'eev-bounded)
;; (describe-function 'eelatex)
;; (describe-function 'eelatex-bounded)
;; (describe-function 'eeg-bounded)
;; (describe-function 'end-of-line-then-eval-last-sexp)
;; (describe-function 'ee-bounded)
;; (describe-function 'code-c-d)
;; (describe-function 'inn)
;; (describe-function 'dff)











Apêndice I: Instalando o eev.el num sistema Debian
--------------------------------------------------

# (find-es "escripts")
# (find-es "escripts" "copy_of_edrxs_home_LR")
# (find-eev "README")


Apêndice __: Rodando um demo em eeg2








#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-delimiter-percent: "\n%\n"
#  ee-anchor-format:     "«%s»"
#  End:
