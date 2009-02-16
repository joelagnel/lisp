;;; Major mode for managing a multimedia library.
(defvar music-version "1.13")
;;; Copyright (C) 2001,2002 Stefan D. Bruda (bruda@ubishops.ca)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;
;;; Commentary:
;;
;;  This package provides a major mode for managing a multimedia
;;  library. To load it, put the file where emacs can see it (i.e.,
;;  somwhere in `load-path'), and evaluate the following expression
;;  (or place such an expression in your init file -- normally
;;  ~/.emacs or ~/.xemacs/init.el):
;;
;;    (load "music") 
;;  or
;;    (require 'music)
;;
;;  (the first assumes that you call this file "music.el"), or any
;;  other method you use for loading stuff. You may want to insert in
;;  the same init file (after the above line loading the mode) the
;;  following:
;;
;;    (setq music-home "<your-music-folder>"
;;          music-appl "<your-music-player>")
;;
;;  replacing <your-music-folder> by the root folder of your default
;;  multimedia library, and <your-music-player> to the application you
;;  want to use for playback (see below). These two variables are
;;  actually customizable: choose
;;
;;    Options -> Advanced (Customize) -> Group...
;;
;;  then type music to see the customizable variables.
;; 
;;  To use the whole thing, call the function `music'. That is, type
;;
;;    M-x music
;;
;;  or bind `music' to your favourite keystroke, or... You are good to
;;  go.
;;
;;  For help on using the library, consult the doc string of function
;;  `music-mode' below -- or get a formatted version by typing C-h m
;;  after you launch the mode (with M-x music).
;;
;;  This mode relies on some external player for playback. All the CLI
;;  or GUI based players work quite well, although the code is rather
;;  tailored to the former class. You may need to make some
;;  adjustments if you use a player with graphical user interface. If
;;  you use XMMS, then you can fully integrate it with this mode, so
;;  that it works the same way as a CLI based player: take a look at
;;  the documentation of `music-appl'. You may be able to do similar
;;  things with other players that I have not yet tried. In any case,
;;  the transition is not painful at all. In a nutshell, all you have
;;  to do is to insert the following line in your init file after the
;;  one loading the code:
;;
;;    (setq music-appl "xmms")
;;
;;  (change the string "xmms" with the name of your favourite
;;  application).  Then, read the documentation of `music-appl' for
;;  things you may want to do on the player's side and for more
;;  information about the particularities of the interface.
;;
;;  If you want to use more than one player on a regular basis, set
;;  the variable `music-appl-list' to a list of all the commands that
;;  you want to use for playback (as strings -- take a look at the
;;  definition of `music-appl-list' below for an example). You can
;;  then switch between them in a circular fashion from the menu or by
;;  using the keyboard shortcut ("/" by default).
;; 
;;  Support for ID3 manipulation code is added in version 1.11: You
;;  need to get the file `music-id3.el' and place it somewhere where
;;  Emacs can see it.  Then choose the (only) entry in the `ID3' menu
;;  and this menu will be expanded to contain the ID3 manipulation
;;  commands.  In order for most of these commands to work, the album
;;  title line (in the main buffer) should have the form: artist name,
;;  followed by `music-album-name-separator' (default ": " but
;;  customizable), followed by the album name, OR just the album name.
;;
;;  Caution: Read the commentary in the file `music-id3.el' (and be
;;  warned that it is alpha software) before using it, and even then
;;  use it with caution.
;;
;;  If you improve/change this file, I would appreciate if you send me
;;  a copy. Not only I will be glad to use it, but, unless you tell me
;;  not to, I will post your changes for others to use (I will use it
;;  for sure) and/or praise you. ;-)
;;
;;  NOTES: 
;;  
;;  This mode should work on any OS, as it relies on emacs-lisp to
;;  manipulate files (it is thus Emacs' responsibility to perform such
;;  OS-dependent manipulations correctly. However, I have tested it
;;  only on Unix-like OSes. If you find that it does not work on your
;;  OS please let me know.
;;
;;  The whole thing has been tested on XEmacs 21.1 to 21.4, but I do
;;  not expect problems on (reasonably) earlier versions. I have done
;;  some not-so-exhaustive testing on FSF Emacs (20.7) as well. Again,
;;  let me know if you encounter problems on other Emacs flavours.
;;
;;  Finally, I come from Common Lisp, so I cannot do without the `cl'
;;  package! (This one comes with all the Emacs flavours I know, so I
;;  hope there is no problem here...)
;;
;;  The code is organized in sections, as follows: 
;;    SECTION 0: Customizable variables.
;;    SECTION 1: Initialize the keymap and menus.
;;    SECTION 2: Main functions.
;;    SECTION 3: Rendering functions.
;;    SECTION 4: Expand/collapse code.
;;    SECTION 5: Select/deselect tracks for playback.
;;    SECTION 6: Album/folder info (title and tracks file) manipulation.
;;    SECTION 7: Playback.
;;    SECTION 8: Emacs compatibility functions
;;  Search for the string ";; SECTION" to jump to the next section.
;;
;;  The names of all the entities (functions, variables) defined in
;;  this code begin with the prefix `music'.
;;
;;  KNWON BUGS:
;;
;;  o The tracks and title files MUST NOT contain the following
;;    constructions:
;;
;;        >>
;;        <<
;;        **
;;        ^^
;;        [+]
;;        [-]
;;        [toggle]
;;        (Loop:
;;
;;    These are used as markers in various buffers, if you use them in
;;    your files both the rendering and the select mechanisms will get
;;    confused. Of course, your file names must not contain these
;;    constructions either (since these names appear in the tracks
;;    file).
;;
;;  o Sometimes the pause/resume breaks down. I have no idea how to
;;    fix this since it is not a systematic error and thus I don't
;;    even know its cause.
;;
;;; CHANGELOG (since version 0.96b):
;;
;;  Version 0.96c
;;
;; o  The names of tracks and title files are no longer
;;    hardcoded. Multiple names are also supported, see the
;;    documentation of variables `music-track-files' and
;;    `music-title-files'.  Windoze- and Mac-friendly `_tracks' and
;;    `_title' are now the default names (instead of `.tracks' and
;;    `.title', respectively)
;;
;;  Version 1.0
;;
;;  o Fixed some documentation strings.
;;  o Added code for customizable variables.
;;  o Added comment support to the tracks files.
;;  o Code changes throughout. The code is now OS-independent. All the
;;    references to specific path separators or specific shell
;;    commands have been eliminated. The code relies only on
;;    emacs-lisp now. In fact, the only (two) calls to `shell-command'
;;    are used to launch the playback application.
;;  o The folder expand code is now efficient (it no longer lists the
;;    whole tree, but just the portion to be expanded). Well, the code
;;    for expanding the _whole_ music library is as a consequence less
;;    efficient, but I doubt you will do this too many times :-)
;;
;;  Version 1.02
;;
;;  o Now correctly handles blanks in track files.
;;  o Added a dedicated buffer for playback controls.
;;  o Discovered undeclared local variable `here', fixed (if you
;;    happen now to have a gobal variable called `here', it is no longer
;;    alterred by this code ;-) ).
;;
;;  Version 1.10
;;
;;  o Now provides 'music, as advertised. ;-)
;;  o Split the main menu into two smaller menus.
;;  o Added interface to the ID3 tag manipulation module, though the
;;    module is yet to come (see `music-id3-init-menu').
;;
;;  Version 1.11
;;
;;  o Added ID3 tag manipulation code, placed in a separate file
;;    (`music-id3.el').  Be warned though that the code in there is
;;    not tested very well.
;;
;;  Version 1.12
;;
;;  o Added support for ESD remote speakers (ESDSPEAKER is now
;;    settable from the "Play" menu).
;;  o Added support for HTTP tracks (see the mode help page).
;;  o Added hook functions for the info/tracks buffers so that the
;;    information is updated in the main window as soon as this
;;    information is saved on disk.
;;  o Mouse selection works now (one hopes), except for shift-click
;;    which does not extend the selection.
;;  o Selected albums are correctly marked as such upon folder
;;    expansion.
;;
;;  Version 1.13
;;
;;  o FSF Emacs compatibility fixes.
;;  o Cleanup for the quitting code.

(require 'cl)
(require 'easymenu)
(require 'background)

;; Gnuserv is actually optional (autodetected)
;;(require 'gnuserv)

;; SECTION 0: Customizable variables.

(defgroup music nil
  "Variables for the multimedia library management"
  :tag "Music Library (music)")

(defcustom music-home "/home/Mp3" 
  "Default folder holding the music library. 
The structure of such a library consist in a set of tracks, organized
into albums, organized into folders.

A track is any file. It is the responsibility of the user to ensure
that the player (see `music-appl') knows how to play the particular
format of a track file. An album is a folder containing no
subfolders. This is the only place where stored tracks are recognized
as such. See the documentation of `music-mode' for additional
information about the library format."
  :group 'music
  :type 'string
  :tag "Default library path (music-home)")

(defcustom music-appl-list '("xmms" "plaympeg" "mplayer -quiet")
  "A list of possible applications for playback (see `music-appl'). 
Select one of these using `music-appl-select'."
  :group 'music
  :type '(repeat string)
  :tag "List of available playback applications (music-appl-list)")

(defcustom music-appl "plaympeg" 
  "The name of the music player application. Either provide an absolute
path or make sure that the application is somwhere in your search
path.

Any player can be used (but see below for a discusion), provided that,
of course it understands the format(s) of the tracks being played and
that it takes as command line arguments an unlimited number of
audio/video files and play them in order. This ensures that the player
understands the basic commands `music-play-list',
`music-play-list-continuous', `music-play-list-options',
`music-play-this-track', `music-play-stop',`music-play-skip', and
`music-play-skip-back'. If the player takes only one file as argument,
then everything still works *except* `music-play-list-continuous' and
`music-play-list-options'.

If the application (or rather the OS...) correctly understands SIGSTOP
 (as a request to pause the playback), and SIGCONT (as a request to
continue playback), then `music-play-pause', `music-play-resume', and
`music-play-pause/resume' are also usable.

In particular, the application plaympeg (see homepage at
http://www.lokigames.com/development/smpeg.php3) has all the mentioned
properties. So does mpg123 and mpg321. In general, I assumed a CLI
 (command line interface) based player when writing this mode, although
it has been extended lately to work with GUI players too.

However, the lisp code is clean enough, so that GUI (graphical user
interface) based players (e.g., XMMS) do interface with this mode,
albeit somehow differently (XMMS in one particular GUI based player
that you can neatly configure to work with this mode (see below), but
any other player can be used). Specifically, if you set `music-appl'
to such a player, select some tracks, and call `music-play-list',
you'll discover that the first track in the selection is played, and
nothing else happens afterwards. The reason is, of course, that a GUI
based player does not normally quit after finishing the playback (a
feature expected by `music-play-list').

The workaround is quite simple though: You can continue to use this
mode for managing your music library, while relying exclusively on the
 (GUI based) player for the playback. To do this, forget about
`music-play-list' and use `music-play-list-continuous'
exclusively. Each time you want to play your selection, call
`music-play-list-continuous'. The list of tracks will be passed to the
player, and you can control the playback from the player's controls
 (that normally include everything that is implemented here, i.e.,
pause, random, loop, skip, and quite possibly extra things like fast
forward and volume controls). If you want to play another selection,
simply make that selection (in Emacs) and call
`music-play-list-continuous' again. The existing player will be
killed, and a new one will be launched. (You may want to rebind
`music-play-list-continuous' to a more convenient key, and to modify
the function `music-play-this-track' as well...)

If you use XMMS with the \"Song Change\" plugin, you can configure it
to work in a fully integrated manner with this mode. To do this, open
the XMMS's preferences, click on the \"Effect/General Plugins\" tab,
and select the \"Song Change\" plugin. Then, click \"Configure\" and
provide a \"Playlist end\" command that will quit XMMS. This command
can issue an appropriate kill command, of course. Another solution is
to use the XEmacs mode that launched the playback process. I will
describe the latter. So, provide as \"Playlist end\" command the
following:

  gnuclient -batch -f music-play-stop-this-track 

Finally (as far as XMMS is concerned), enable the plugin (click the
appropriate checkbox) and dismiss the preferences dialog by clicking
OK. For this to work, you have to have the gnuclient software
installed (comes with most XEmacs distributions, so chances are it is
already installed), The current implementation offers support for
gnuclient by starting the server process each time a playback command
is issued. If `music-stop-gnuserv' is non-nil (default) the server is
shut down once the playback terminates. If you use gnuserv for other
things (and thus you don't want it shut down on playback completion),
set `music-stop-gnuserv' to NIL.  You are done, and you have now a
nice XMMS/XEmacs interface. It is probably a good idea to call
`music-play-list-continuous' instead of `music-play-list' for
playback, but this is just it, a good idea, any other variant works
without problems.

You may find arguments for using both kind of players. I am myself
split between these two variants. I used to use CLI based players
 (plaympeg to be more specific), but I am experimenting now with XMMS
and I kind of like what I see... Help me deciding by letting me know
your opinion on this one!"
  :group 'music
  :type 'string
  :tag "Playback application (music-appl)")

(defcustom music-url-types '("http")
  "Types of URLs understood by the mode.  
Do not include the `://', for example write `http' instead of
`http://'."
  :group 'music
  :type '(repeat string)
  :tag "Recognized URLs (music-url-types)")

(defcustom music-track-files '("_tracks" ".tracks")
  "Possible names for the file that contains the track listing in an album. Think twice before changing!

These are possible names for \"hidden\" files (as far as this module
is concerned) that hold the track listing in an album. If you delete a
name from this list, any existing track files with this name will no
longer be considered as such. When a new track file is created, the
first element of this list is used as its name.  See documentation of
`music-mode' for purpose and more details.."
  :group 'music
  :type '(repeat string)
  :tag "Recognized names for the track file (music-track-files)")

(defcustom music-title-files '("_title" ".title")
  "Possible names for the file that contains the title of an album or folder. Think twice before changing!

These are possible names for \"hidden\" files (as far as this module
is concerned) that hold information about the album/folder. If you
delete a name from this list, any existing title files with this name
will no longer be considered as such. If a new title file is created,
the first element of this list is used as its name.  See documentation
for `music-mode' for purpose and more details.."
  :group 'music
  :type '(repeat string)
  :tag "Recognized names for the title file (music-title-files)")

(defcustom music-stop-gnuserv t
  "If non-nil, the gnuserver is shut down once the playback process terminates."
  :group 'music
  :type 'boolean
  :tag "Stop the gnuserver once finished? (music-stop-gnuserv)")

(defcustom music-play-pop-frame t
  "If non-nil, a frame showing playback information will pop up when `music-play-list' is called."
  :group 'music
  :type 'boolean
  :tag "Pop up a frame with playback controls? (music-play-pop-frame)")

;;; ID3 tag manipulation subsystem only.

(defcustom music-album-name-separator ": "
  "The separator between the artist and the album file in an album title.
Used by the ID3 manipulation subsystem only, has no effect on anything else.

Specifically, this variable is used by the functions
`music-id3-import-album', `music-id3-export-whole-album', and
`music-id3-export-this-track-using-current-album'; the first function
gets the album and artist fields from the current ID3 tags and writes
them as album title line in this order and separated by
`music-album-name-separator', while the \"export\" functions assume
such a format for the title line and extract information from this
line.  The artist name should not contain `music-album-name-separator'
as substring."
  :group 'music
  :type 'string
  :tag "Separator between artist and album name in an album title line (music-album-name-separator)"
  )


;; To change the bindings, see section "Initialize the keymap" below.
(defvar music-mode-map (make-sparse-keymap)
  "Keymap for the buffer holding the music library.


\\{music-mode-map}")

(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; SECTION 1: Initialize the keymap and menus.

(let ((map (make-sparse-keymap)))
    (define-key map [return] 'music-toggle)
    (define-key map "a" 'music-select)
    (define-key map "x" 'music-delete)
    (define-key map "X" 'music-delete-all)
    (define-key map " " 'scroll-up-command)
    (define-key map [backspace] 'scroll-down-command)
    (define-key map "i" 'music-show-album-info)
    (define-key map "t" 'music-show-tracks-info)
    (define-key map "q" 'music-quit)
    (define-key map "m" 'music)
    (define-key map [button2] 'music-mouse-toggle)
    (define-key map "D" 'music-rebuild-album)
    (define-key map "d" 'music-rebuild-album-append)
    (define-key map [tab] 'music-collapsed-toggle)
    (define-key map [(shift button1)] 'music-collapsed-mouse-toggle-maybe-extended)
    (define-key map [button1] 'music-collapsed-mouse-toggle-maybe)
    (define-key map "l" 'music-loop-toggle)
    (define-key map "r" 'music-random-toggle)
    (define-key map "p" 'music-play-list-continuous)
    (define-key map "P" 'music-play-list-options)
    (define-key map [f6] 'music-play-this-track)
    (define-key map [f7] 'music-play-list)
    (define-key map [f9] 'music-play-skip)
    (define-key map [f8] 'music-play-skip-back)
    (define-key map [f10] 'music-play-pause)
    (define-key map [f11] 'music-play-resume)
    (define-key map [f12] 'music-play-stop)
    (define-key map "/" 'music-appl-select)
    (define-key map "s" 'music-esd-remote-speakers-interactive)
    (define-key map "S" 'music-esd-remote-speakers-reset)
    (setq music-mode-map map))

(easy-menu-define
 music-menu (list music-mode-map)
 "The music mode menu. Also see `music-mode-map'."
 '("Music"
   ["Add to play list" (music-select) :active t :keys "a"]
   ["Delete from play list" (music-delete) :active t :keys "x"]
   ["Clear list" (music-delete-all) :active music-play-list :keys "X"]
   "---"
   ["Expand album/folder" (music-expand) 
    :active (equal (music-collapsed-p) 'collapsed) :keys "TAB"]
   ["Collapse album/folder" (music-collapse) 
    :active (equal (music-collapsed-p) 'expanded) :keys "TAB"]
   "---"
   ["Edit album info" (music-show-album-info) 
    :active (or (music-get-album)
                (and (music-get-track) (music-prev-album)))
    :keys "i"] 
   ["Edit tracks info" (music-show-tracks-info) 
    :active (or (music-get-album)
                (and (music-get-track) (music-prev-album))) 
    :keys "t"] 
   ["Rebuild album" (music-rebuild-album-append) :active (music-get-album) :keys "d"]
   "---"
   ["Quit" (music-quit) :keys "q"]
   ))

(easy-menu-define
 music-play-menu (list music-mode-map)
 "Playback menu. Also see `music-mode-map'."
 '("Play"
   [">" (music-play-list) :active music-play-list :keys "F7"]
   ["> (continuous)" (music-play-list-continuous) :active music-play-list :keys "p"]
   ["> (this track)" (music-play-this-track) 
    :active (or (music-get-track) (music-get-album)) :keys "F6"]
   ["<<" (music-play-skip-back) :active (and (not music-stopped)
                                             (not music-run-continuous)) :keys "F8"]
   [">>" (music-play-skip) :active (and (not music-stopped)
                                             (not music-run-continuous)) :keys "F9"]
   ["||" (music-play-pause) :active (and (not music-stopped)
                                         (not music-paused)) :keys "F10"]
   ["||>" (music-play-resume) :active (and (not music-stopped)
                                           music-paused) :keys "F11"]
   ["[ ]" (music-play-stop) :active (not music-stopped) :keys "F12"]
   ["> (options)" (music-play-list-options) :active music-play-list :keys "P"]
   "---"
   ["Toggle loop playback" (music-loop-toggle) :keys "l"]
   ["Toggle random playback" (music-random-toggle) :keys "r"]
   ["Switch player" (music-appl-select) :active music-appl-list]
   ["Set ESD speakers" (music-esd-remote-speakers-interactive)
    :active music-stopped :keys "s"]
   ["Reset ESD speakers to local" (music-esd-remote-speakers)
    :active music-stopped :keys "S"]
   ))

(easy-menu-define
 music-play-small-menu (list music-mode-map)
 "Smaller playback menu, for the buffer showing the play controls."
 '("Playback"
   ["<<" (music-play-skip-back) :active (and (not music-stopped)
                                             (not music-run-continuous)) :keys "F8"]
   [">>" (music-play-skip) :active (and (not music-stopped)
                                             (not music-run-continuous)) :keys "F9"]
   ["||" (music-play-pause) :active (and (not music-stopped)
                                         (not music-paused)) :keys "F10"]
   ["||>" (music-play-resume) :active (and (not music-stopped)
                                           music-paused) :keys "F11"]
   ["[ ]" (music-play-stop) :active (not music-stopped) :keys "F12"]
   "---"
   ["Toggle loop playback" (music-loop-toggle) :keys "l"]
   ["Toggle random playback" (music-random-toggle) :keys "r"]
   ))

(easy-menu-define
 music-id3-init-menu (list music-mode-map)
 "The initial menu for manipulation of id3 tags; contains only a command to initialize the true ID3 menu."
 '("ID3"
   ["Expand this menu (DANGEROUS)" (music-add-id3)]
   ))

(defun music-add-id3 ()
  "Loads the ID3 module."
  (interactive)
  (require 'music-id3)
  (music-id3-initialize)
  (message "ID3 menu expanded."))

(defun music-install-music-menu ()
  "Installs `music-menu', the music-mode menu."
  (easy-menu-add music-menu)
  (easy-menu-add music-play-menu)
  (easy-menu-add music-id3-init-menu))


;; SECTION 2: Main functions.

;; Just to provide a mode description...
(defun music-mode ()
  "Major mode for managing a multimedia library.  \\<music-mode-map>

This mode is primarily intended for a music library, but works with
any multimedia files that your player (see `music-appl') understands.
The library will be henceforth referred to as the \"music library.\"

The form of a music library is a tree of folders whose leaves are
albums.  Each album contains a list of files, all of them tracks. For
example, the following structure is a music library

collection1
   album0
      track2.mp3
   subcollection2
      album1
         track1.mp3
         track2.mp3
      album2
        track3.mp3
   subcollection3

Note that \"subcollection3\" is an empty folder. Technically, such
folders are considered albums containing no tracks.

The *only* difference between a music library and your normal file
system is the following: Files in a disk folder containing subfolders
are not visible. There are two special kind of files (see below) that
are also not visible (but do serve a well-defined purpose). All the
tracks should be placed in albums, i.e., disk folders without
subfolders.  URLs are also supported in place of files (see below).

Each album contains in addition a \"tracks file\" (see
`music-track-files') with, at a minimum, the list of tracks as
returned by the (Unix) command \"ls . > <tracks-file-name>\". Each
track entry may be followed by \" ++ \" and the name of the
track. Lines beginning with \"#\" in the tracks file are ignored
 (however, the \"#\" character must be in the first column of the
line). If some track has a name, then this name is displayed in the
listing. Otherwise, the track is identified by its file name. The
tracks file is automatically created if nonexistent when the (content
of the) album is displayed for the first time. It can be edited (in
particular, names can be added to tracks) using the command
\"\\[music-show-tracks-info]\" anywhere inside the current album listing.

A track file may contain a full URL whenever a file name is expected.
If this is the case, then the URL is passed as is to the player.  In
order for this feature to work your player must know what to do with
URLs (as does, for example mplayer, see http://www.mplayerhq.hu/) and
what to do with the type of stream pointed at by the given URL.  The
protocols included in the variable `music-url-types' are the only ones
recognized as URLs.

An album or folder may contain a \"title file\" (see
`music-title-files') whose first line is the name of the
folder/album. This first line is displayed in the listing. If the
title file does not exist, the displayed name of the album/folder is
the name of the disk folder itself. The remainder of the file is
ignored, and can be used for any purpose you like (I use it for
storing information about the CD and/or the artist). The content of
the title file can be viewed/changed using the command
\"\\[music-show-album-info]\" anywhere inside the current album/folder listing.

If your player uses ESD (the \"enlighted sound daemon\") for audio
playback you can set a remote host to actually plays the tunes from
within the mode.  The command \"\\[music-esd-remote-speakers-interactive]\" prompts for a host name 
and a port and sets the environment variable ESPEAKER accordingly (esd
must be of course running on the given machine and listen to the given
port).  The command \"\\[music-esd-remote-speakers-reset]\" reverts to playback on the local host.

Key bindings: 

Displaying the library: 
\\[music]  Prompts for a (possibly different) library folder and
   displays it. The content of the folder is displayed as follows:

   Folders are displayed prefixed by a [+] or [-] sign, depending
   on whether that particular folder is collapsed -- i.e., only 
   the name is visible -- or expanded -- i.e., the content of the
   folder is visible as well.

   Albums are displayed exactly as folders, except that they are
   also prefixed by \"----\". This prefix changes to \"-%%-\"
   when the whole album is selected for playback.

   Tracks are displayed using either their titles (if existent in 
   the tracks file) or their file names. They are prefixed by 
   \"----\", which changes to the index (modulo 100) of that track 
   in the playback list when the track is present. Note: These numbers
   are a guideline only, they are rather unreliable.  However, it is
   guaranteed that a track is selected for playback iff it has such a
   number in front of it.
\\[music-quit]  Kills the buffer displaying the library and quits. 
\\[music-show-album-info]  Displays and lets you edit the information
   associated to an album/folder. The first line of this information is
   the title of the album/folder (and is displayed in the listing),
   the rest is ignored by the program.
\\[music-show-tracks-info]  Displays and lets you edit the information
   associated to the tracks in the current album. Such information
   consists at a minimum in a list of track file names and/or
   URLs. Each file can be followed by \" ++ \" and the name (to be
   displayed in the listing) of that track. C-q adds \" ++ \" to all
   the tracks.
\\[music-rebuild-album-append]  Updates the track list to the actual listing
   of the disk folder corresponding to the current album. Preserves track
   names for those tracks that still exist on disk. New tracks are always
   added at the end of the listing.
\\[music-rebuild-album]  Updates the track list to the actual listing
   of the disk folder corresponding to the current album. Erases all the
   track names. Tracks are listed sorted.
\\[scroll-up-command]
\\[scroll-down-command]  Page-up/page-down through the listing. 

Manipulating the play list: 
\\[music-toggle]
\\[music-mouse-toggle]  Selects/unselects the current
   track/album for playing. Issued on a folder, always selects all the
   albums inside that folder.
\\[music-select]  Selects the current track/album/folder. Using this
   command, tracks/albums may be selected more than once. 
\\[music-delete]  Unselects the current track/album. Does nothing on
   folders. 
\\[music-delete-all]  Clears the list of tracks to be played
\\[music-collapsed-toggle]  Collapses/expands current album/folder. 
\\[music-collapsed-mouse-toggle-maybe]  On the [-]/[+] signs in front
   of an album/folder, collapses/expands current album/folder. On a
   \"[toggle]\" marker, toggles the corresponsing feature (loop or
   random). 
\\[music-collapsed-mouse-toggle-maybe-extended]  On the [-]/[+] signs in front
   of an album/folder, collapses/completely expands current 
   album/folder. 

Playback:
\\[music-play-list]  Plays the current list. Shows information about
   the currently playing track in the buffer displaying the library. 
\\[music-play-list-continuous]  Plays the current list as a large,
   continuous track (skip commands, as well as loop and random
   flags do not work). It is the way to go when using a GUI based
   player. See `music-play-list-continuous' and `music-appl'.
\\[music-play-list-options]  Plays the current list as a large,
   continuous track (see above). Prompts first for a string of options
   to be passed (before the actual list of tracks) to the player
   (useful, for example, for playing movies full-screen if the player
   supports such).
\\[music-play-this-track]  Plays the current track/album. Changes the
   list of tracks to be played. 
\\[music-loop-toggle]  Toggles the loop flag. If this flag is non-nil,
   the current selection is continuously played. 
\\[music-random-toggle]  Toggles the random flag. If this flag is non-nil,
   tracks from the current selection are played at random.
\\[music-esd-remote-speakers-interactive]  Prompts for a host name and
   a port number and sets ESPEAKER accordingly.  ESD must be running
   on the remote host and your playback application must use ESD for
   output for this to have any effect.
\\[music-esd-remote-speakers-reset]  Resets ESD playback to local host.

Commands that can be issued while playback is in progress: 
\\[music-play-skip-back]  Skips backward to the previously played track.
\\[music-play-skip]  Skips forward to the next track in list. 
\\[music-play-pause]  Pauses the playback. 
\\[music-play-resume]  Resumes a stopped playback. 
\\[music-play-stop]  Stops the playback. 

If you have loaded the ID3 manipulation subsysten, new key bindings
are also provided, for: (a) importing the name of a track from the ID3
tag (default: C-c i); (b) exporting the name of the current track to the
ID3 tag, either using the information from the album title line
 (default: C-c t), or by obtaining the information interactively
 (default: C-c t); and (c) Export the whole album to the ID3 tags of
the corresponding files, by taking the album title and artist from the
album title line (default: C-c e).

Here are the actual bindings of the keymap: 
\\{music-mode-map}
" 
  (interactive)
  (music))

(defun music ()
  "Launches `music-mode', the major mode for managing a multimedia library." 
  (interactive)
  (setq music-play-list nil
        music-play-names nil)
  (when running-xemacs
    (custom-set-faces
     '(music-album-face ((((class color)) (:foreground "MidnightBlue"))) t))
    (custom-set-faces
     '(message-music-face ((((class color)) (:foreground "red"))) t)))
  (when (get-buffer "**Play Process Buffer**")  ; the buffer of the playback process. 
    (kill-buffer "**Play Process Buffer**"))
  (setq music-stopped t)
  (music-display)
  (set-buffer "*Play List*")
  (end-of-buffer)
  (insert (concat "\n** end **"
                  "\n\n(Loop: " (if music-loop-p " ON" "off") " [toggle]; Random: " 
                  (if music-random-p " ON" "off") " [toggle])"))
  (save-excursion
    (beginning-of-line)
    (when (and running-xemacs music-loop-p)
      (set-extent-property (make-extent (+ (point) 8) (+ (point) 10)) 
                           'face (find-face 'message-music-face)))
    (when (and running-xemacs music-random-p)
      (set-extent-property (make-extent (+ (point) 30) (+ (point) 32)) 
                           'face (find-face 'message-music-face))))
  (beginning-of-buffer)
  (let ((here (+ (point) 64)))
    (insert (concat "[ Nothing playing ]\n\n(Loop: " 
                    (if music-loop-p " ON" "off") " [toggle]; Random: " 
                    (if music-random-p " ON" "off") 
                    " [toggle])\n\nListing for \"" music-home ":\"\n"))
    (save-excursion
      (forward-line -3)
      (beginning-of-line)
      (when (and running-xemacs music-loop-p)
        (set-extent-property (make-extent (+ (point) 8) (+ (point) 10)) 
                             'face (find-face 'message-music-face)))
      (when (and running-xemacs music-random-p)
        (set-extent-property (make-extent (+ (point) 30) (+ (point) 32)) 
                             'face (find-face 'message-music-face))))
    (when running-xemacs
      (set-extent-property (make-extent here (point)) 'face (find-face 'underline)))
    )
  (beginning-of-buffer)
  (forward-line 1)
  (insert "\n  Hit F7 (selection), F6 (this track), or p (list, continuous) for playback.\n")
  (switch-to-buffer "*Play List*")
  (beginning-of-buffer)
  (forward-line 9)
  (setq mode-name "Music"
        major-mode 'music-mode
        buffer-read-only t
        truncate-lines t)
  (music-install-music-menu)
  (add-hook 'mode-motion-hook 'mode-motion-highlight-line)
  (add-local-hook 'kill-buffer-hook 'music-quit-cleanup t)
  (use-local-map music-mode-map))

(defun music-quit-cleanup ()
  "Clean up upon quit.
Clears the playback list, and kills all the related buffers, as well
as any playback process."
  (music-play-stop)
  (when music-play-frame
    (delete-frame music-play-frame)
    (setq music-play-frame nil))
  (when (get-buffer "**Play Process Buffer**")
    (kill-buffer "**Play Process Buffer**"))
  (setq music-play-list nil
        music-play-names nil
        music-selected-albums nil)
  )

(defun music-quit ()
  "Quits the music mode.
Calls the hook that clears the playback list, and kills all the
related buffers, as well as any playback process."
  (interactive)
  (when (get-buffer "*Play List*")
    (kill-buffer "*Play List*")))

;; SECTION 3: Rendering functions.

(defun music-show-library (indent folder &optional collapsed-p name)
  "Displays the music library rooted at FOLDER using INDENT as indentation string.
The optional argument COLLAPSED-P determines whether the library is
displayed collapsed or not as follows: If COLLAPSED-P is NIL then the
whole directory tree is shown (expanded); if COLLAPSED-P is T then the
directory is shown completely colapsed (i.e., only the title line is
visible); if COLLAPSED-P is not NIL and not T, only the first level is
expanded.  The second optional argument, NAME, provides the name used
in displaying the title line of the folder (if not present, the last
level of the absolute path FOLDER is used). Note that the name is
superseeded anyway by the content of any existing title file (see
`music-title-files'), see `music-show-folder-name' and
`music-show-album-name'. Normally, NAME is used only in the recursive
calls of this function, and is provided simply because it saves some
function calls.

This is the workhorse of the whole rendering process."
  (let ((subdirs (music-list-files folder nil nil)    ;; see the FSF Emacs compatibility 
                                                      ;; functions (end of file)
                 ;;(directory-files folder nil nil nil 'dirs)
                 )
        (subdir-paths (music-list-files folder t nil) ;; Ditto
                      ;;(directory-files folder t nil nil 'dirs)
                      )
        subdirs-pairs
        (name (if name name    ;; find out the name unless provided already
                (if (equal (file-name-nondirectory folder) "")
                    (file-name-nondirectory (substring folder 0 (- (length folder) 1)))
                  (file-name-nondirectory folder)
                  )))
        )
    ;; get rid of `.' and `..' -- FIXME: are they always there (even
    ;; on Mac OS) so that we can just do a cddr?
    (dotimes (i 2)
      (when (or (equal (car subdirs) ".") (equal (car subdirs) ".."))
        (setq subdirs (cdr subdirs)
              subdir-paths (cdr subdir-paths))))
    (setq subdirs-pairs (mapcar* '(lambda (x y) (cons x y)) subdir-paths subdirs))
    (if subdirs ;; folder...
        (cond ((eq collapsed-p nil) ;; expand the whole thing
               (music-show-folder-name indent name folder nil)
               (dolist (dir subdirs-pairs)
                 (music-show-library (concat "   " indent) (car dir) nil (cdr dir))))
              ((eq collapsed-p t)  ;; just show the title
               (music-show-folder-name indent name folder t))
              (t ;; expand first level but call with collapsed-p t
               (music-show-folder-name indent name folder nil)
               (dolist (dir subdirs-pairs)
                 (music-show-library (concat "   " indent) (car dir) t (cdr dir)))))
      ;; album...
      (progn
        (music-show-album-name indent name folder (eq collapsed-p t))
        (unless (eq collapsed-p t)
          (music-show-album (concat "   " indent) name folder)))
      )))

(defun music-display ()
  "Prompts for a location and displays the music library at that location. 
Called by `music'."
  (let ((folder (read-file-name "Show Albums in: " 
                                (file-name-as-directory music-home)
                                (file-name-as-directory music-home))))
    (unless (get-buffer "*Play List*")
      (generate-new-buffer "*Play List*"))
    (setq music-home folder)
    (set-buffer "*Play List*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (music-show-library "" music-home 'almost-collapsed)
    ;(setq buffer-read-only t)
    ))

(defun music-display-restore-expanded (indent folder)
  "Recursively redisplays the whole content of FOLDER using the indentation string INDENT."
  (setq buffer-read-only nil)
  (music-show-library indent folder)
  (setq buffer-read-only t)
)

(defun music-display-restore (indent folder)
  "Redisplays FOLDER using the indentation string INDENT."
  (setq buffer-read-only nil)
  (music-show-library indent folder 'almost-collapsed)
  (setq buffer-read-only t)
)

(defun music-show-folder-name (indent name dir collapsed-p)
  "Prints the name of the folder DIR using INDENT as indentation string
and prefixed by [+] or [-] according to COLLAPSED-P. If a title file
 (see `music-title-files') is present, get the name from there;
otherwise, use NAME as folder's name."
  (insert "\n" indent)
  (let ((here (point))
        (display-name name)
        (ls-result (music-find-title-file dir))
        there)
    (when ls-result
      ;; we have a title...
      (insert "\n^^^^^^")
      (goto-char here)
      (insert-file-contents (expand-file-name ls-result dir))
      (search-forward "\n" nil t)
      (forward-char -1)
      (setq display-name (buffer-substring here (point)))
      (search-forward "^^^^^^" nil t)
      (delete-region here (point)))
    (setq here (point))
    (if collapsed-p
        (insert "[+] " display-name)
      (insert "[-] " display-name))
    (setq there (point))
    (music-align-data 130 (concat "^^" dir "^^"))
    (when running-xemacs
      (set-extent-property (make-extent here there)
                           'face (find-face 'bold))
      (set-extent-property (make-extent (+ here 4) there)
                           'face (find-face 'bold))
      )))

(defun music-show-album-name (indent name dir collapsed-p)
  "Prints the name of the album DIR using INDENT as indentation string
and prefixed by [+] or [-] according to COLLAPSED-P. If a title file
 (see `music-title-files') is present, get the name from there;
otherwise, use NAME as album's name."
  (insert "\n")
  (insert indent)
  ;(insert "---- ")
  (let ((here (point))
        (display-name name)
        (ls-result (music-find-title-file dir))
        there)
    (when ls-result
      ;; we have a title...
      (insert "\n^^^^^^")
      (goto-char here)
      (insert-file-contents (expand-file-name ls-result dir))
      (search-forward "\n" nil t)
      (forward-char -1)
      (setq display-name (buffer-substring here (point)))
      (search-forward "^^^^^^" nil t)
      (delete-region here (point)))
    (setq here (point))
    (if collapsed-p
        (insert "[+] ---- " display-name)
      (insert "[-] ---- " display-name))
    (setq there (point))
    (music-align-data 130 (concat "**" dir "**"))
    (when (member dir music-selected-albums)
      (music-mark-line "%%"))
    (when running-xemacs
        (set-extent-property (make-extent here there) 
                             'face (find-face 'bold))
        (set-extent-property (make-extent (+ here 8) there)
                             'face (find-face 'music-album-face))
        )))

(defun music-show-album (indent name dir)
  "Displays the content of the album DIR using INDENT as indentation string. 
Creates the tracks file (see `music-track-files') if nonexistent."
  (let ((here (point))
        (which 0)
        count
        (track-no 1)
        there
        (ls-result (music-find-track-file dir))
        (files (music-list-files dir nil t) ;; see the FSF Emacs compatibility 
                                            ;; functions (end of file)
               ;;(directory-files dir nil nil nil t)
               )
        track file)
    (unless ls-result
      ;; Create the .tracks file... 
      (setq ls-result (car music-track-files))
      (dolist (ff (append music-title-files music-track-files)) ;; ignore special files.
        (setq files (delete ff files)))
      (save-excursion
        (when (get-buffer " ***Music Temp***")
          (kill-buffer " ***Music Temp***"))
        (generate-new-buffer " ***Music Temp***")
        (set-buffer " ***Music Temp***")
        (insert (mapconcat '(lambda (str) str) files "\n") "\n")
        (append-to-file (point-min) (point-max) (expand-file-name ls-result dir))
        (kill-buffer " ***Music Temp***")))

    (insert "\n^^^^^^")
    (goto-char here)
    (insert "\n")
    (insert-file-contents (expand-file-name ls-result dir))
    (beginning-of-line)
    (setq here (point))
    (while (not (equal (buffer-substring (point) (+ (point) 6)) "^^^^^^"))
      (end-of-line)
      (setq there (point))
      (beginning-of-line)
      (when (equal (buffer-substring here (+ here 1)) "#")
        ;; comment or empty line, skip.
        (delete-region here (+ there 1))
        (end-of-line)
        (setq there (point))
        (beginning-of-line)
        (setq here (point))
        )
      (unless (= here there)
        (if (search-forward " ++ " there t)
            (setq file (buffer-substring here (- (point) 4))
                  track (buffer-substring (point) there))
          (setq file (buffer-substring here there)
                track ;(format "%3s." track-no)
                      (replace-in-string (buffer-substring here there)
                                         "\\.[^\\.]*$" "")
                ))
        (beginning-of-line)
        (setq here (point))
        (delete-region here there)
        (insert (concat indent " ---- " track))
        (setq track-no (+ track-no 1))
        (music-align-data 66 (concat "<<" file ">>"))
        (setq count nil
              which 1)
        (dolist (tr music-play-list)
          (when (equal (if (music-is-url-p file) file (expand-file-name file dir)) tr)
            (setq count which)
            (return)
            )
          (setq which (+ which 1)))
        (when count
          (music-mark-line (format "%d" (mod count 100)))))
      (forward-line 1)
      (beginning-of-line)
      (setq here (point))
      )
    (delete-region (point) (+ (point) 6))
    ;; Cleaning up trailing empty lines... 
    (beginning-of-line)
    (setq here (point))
    (end-of-line)
    (while (= here (point))
      (delete-region (- here 1) here)
      (beginning-of-line)
      (setq here (point))
      (end-of-line))
    ))

; one track: 
; <indent> ---- Track name "align 66" <<track>>
; one album:
; <indent> ---- Album name    "align 130" **folder**


;; The info files (track and title):

(defun music-find-track-file (dir)
  "Returns the name of the tracks file (see `music-track-files') for the album DIR,
NIL if no such file exists."
  (let (name)
    (dolist (file music-track-files name)
      (when (file-exists-p (expand-file-name file dir))
        (setq name file)
        (return name)
        ))))

(defun music-find-title-file (dir)
  "Returns the name of the title file (see `music-title-files') for the album DIR,
NIL if no such file exists."
  (let (name)
    (dolist (file music-title-files name)
      (when (file-exists-p (expand-file-name file dir))
        (setq name file)
        (return name)))))


;; SECTION 4: Expand/collapse code.

(defun music-collapsed-toggle ()
  "Expand/collapse the current entity, as appropriate. 
Does nothing if cursor is not on an album or folder."
  (interactive)
  (if (equal (music-collapsed-p) 'collapsed)
      (music-expand)
    (if (equal (music-collapsed-p) 'expanded)
        (music-collapse)
      nil)))

(defun music-expand ()
  "Expands the current album/folder. 
Does nothing if album/folder is already expanded."
  (save-excursion
    (when (equal (music-collapsed-p) 'collapsed)
      (end-of-line)
      (let ((there (point)) 
            here indent
            sel-p)
        (beginning-of-line)
        (setq here (point))
        (setq sel-p (search-forward "-%%-" there t))
        (beginning-of-line)
        (search-forward-regexp "[^ ]" there t)
        (setq indent (buffer-substring here (- (point) 1)))
        (end-of-line)
        (music-display-restore indent (or (music-get-album) (music-get-folder)))
        (setq buffer-read-only nil)
        ;;(kill-line 2)
        (delete-region here (+ there 1))
        (goto-char here)
        (when sel-p
          (music-mark-line "%%")
          )
        (setq buffer-read-only t)))))

(defun music-expand-expand ()
  "Completely expands the current album/folder. 
Does nothing if album/folder is already expanded. On a folder,
recursively expand the whole content."
  (when (equal (music-collapsed-p) 'collapsed)
    (end-of-line)
    (let ((there (point)) 
          here indent
          sel-p)
      (beginning-of-line)
      (setq here (point))
      (setq sel-p (search-forward "-%%-" there t))
      (beginning-of-line)
      (search-forward-regexp "[^ ]" there t)
      (setq indent (buffer-substring here (- (point) 1)))
      (end-of-line)
      (music-display-restore-expanded indent (or (music-get-album)
                                                 (music-get-folder)))
      (setq buffer-read-only nil)
      ;(kill-line 2)
      (delete-region here (+ there 1))
      (goto-char here)
      (when sel-p
        (music-mark-line "%%")
        )
      (setq buffer-read-only t))))

(defun music-collapse ()
  "Collapses the current album/folder. 
Does nothing if album/folder is already collapsed."
  (save-excursion
    (when (equal (music-collapsed-p) 'expanded)
      (setq buffer-read-only nil)
      (end-of-line)
      (let ((there (point))
            here 
            (beg (point))
            indent
            (done nil))
        (beginning-of-line)
        (setq here (point))
        (search-forward-regexp "[^ ]" there t)
        (setq indent (length (buffer-substring here (- (point) 1))))
        (while (not done)
          (forward-line 1)
          (end-of-line)
          (setq there (point))
          (beginning-of-line)
          (setq here (point))
          (unless (= there here)
            (search-forward-regexp "[^ ]" there t)
            (setq done (<= (- (point) 1) (+ here indent)))))
        (beginning-of-line)
        (delete-region beg (- (point) 1))
        (forward-line -1)
        (beginning-of-line)
        (search-forward "[" nil t)
        (delete-char 1)
        (insert "+")
        (setq buffer-read-only t)))))

;; Mouse manipulation:

(defun music-collapsed-mouse-toggle (ev)
  "Expand/collapse the current entity, as appropriate. 
Does nothing if mouse point is not on an album or folder."
  (interactive "e")
  (if (event-point ev)
      (save-excursion
        (goto-char (event-point ev))
        (music-collapsed-toggle))
    (mouse-track ev)))

(defun music-collapsed-mouse-toggle-maybe (ev)
  "If pointer is on a [+]/[-], expand/collapse the current entity, 
as appropriate. If pointer is on a \"[toggle]\" construction, toggles
the corresponding flag." 
  (interactive "e")
  (if (event-point ev)
      (let (click here there not-useful)
        (save-excursion
          (goto-char (event-point ev))
          (save-excursion  
            (setq click (point))
            (beginning-of-line)
            (setq here (point))
            (end-of-line)
            (setq there (point))
            (goto-char click)
            (search-backward "[" here t)
            (setq here (point))
            (goto-char click)
            (search-forward "]" there t)
            (setq there (point))
            (setq click (buffer-substring here there))
            (setq buffer-read-only nil)
            (if (equal click "[toggle]")
                (save-excursion
                  (when (> here 6)
                    (if (equal (buffer-substring (- here 10) (- here 6)) "Loop")
                        (setq music-loop-p (not music-loop-p))
                      (setq music-random-p (not music-random-p)))
                    (setq buffer-read-only nil)
                    (beginning-of-buffer)
                    (while (search-forward "(Loop:" nil t)
                      (beginning-of-line)
                      (setq here (point))
                      (end-of-line)
                      (setq there (point))
                      (delete-region here there)
                      (insert (concat "(Loop: " (if music-loop-p " ON" "off") " [toggle]; Random: " 
                                      (if music-random-p " ON" "off") " [toggle])"))
                      (save-excursion
                        (beginning-of-line)
                        (when (and running-xemacs music-loop-p)
                          (set-extent-property (make-extent (+ (point) 8) (+ (point) 10)) 
                                               'face (find-face 'message-music-face)))
                        (when (and running-xemacs music-random-p)
                          (set-extent-property (make-extent (+ (point) 30) (+ (point) 32)) 
                                               'face (find-face 'message-music-face))))
                      )
                    (setq buffer-read-only t)))
              (if (or (equal click "[-]") (equal click "[+]"))
                  (music-collapsed-toggle)
                (setq not-useful t) ))
            (setq buffer-read-only t)))
        (if not-useful
            (mouse-track ev) 
          (goto-char (event-point ev))))
        (mouse-track ev)))

(defun music-collapsed-mouse-toggle-maybe-extended (ev)
  "If pointer is on a [+]/[-], expand/collapse the current entity,
as appropriate. If issued on a collapsed folder, recursively expand
the whole content of that folder."
  (interactive "e")
  (if (event-point ev)
      (let (click here there not-useful)
        (save-excursion
          (goto-char (event-point ev))
          (save-excursion  
            (setq click (point))
            (beginning-of-line)
            (setq here (point))
            (end-of-line)
            (setq there (point))
            (goto-char click)
            (search-backward "[" here t)
            (setq here (point))
            (goto-char click)
            (search-forward "]" there t)
            (setq there (point))
            (setq click (buffer-substring here there)))
          (cond ((equal click "[-]") (music-collapse))
                ((equal click "[+]") (music-expand-expand))
                (t (setq not-useful t))))
        (if not-useful
            (mouse-track ev) 
          (goto-char (event-point ev))))
    (mouse-track ev)))

;; Miscelaneous helper functions:

(defun music-collapsed-p ()
  "Return \"collapsed\" or \"expanded\" if current album/folder is collapsed or expanded,
respectively, and NIL is the current line is not an album or folder."
  (save-excursion
    (let (there)
      (end-of-line)
      (setq there (point))
      (beginning-of-line)
      (if (search-forward "[" there t)
          (if (equal (buffer-substring (point) (+ 2 (point))) "-]")
              'expanded
            (when (equal (buffer-substring (point) (+ 2 (point))) "+]")
                'collapsed))
        nil))))

(defun music-align-data (num str)
  "Prints STR starting at column NUM on the current line."
  (beginning-of-line)
  (let ((here (point))
        count)
    (end-of-line)
    (setq count (- num (- (point) here)))
    (while (> count 0)
      (setq count (- count 1))
      (insert " "))
    (insert str)))


;; SECTION 5: Select/deselect tracks for playback.

(defvar music-selected-albums nil
  "Keeps track of selected albums.
Used only for printing correctly the \"%%\" markers on expansion of
folders.")

(defun music-select ()
  "Selects the current album/track/folder for playback."
  (interactive)
  (let ((track (music-get-track))
        (album (music-get-album))
        (folder (music-get-folder))
        (is-collapsed (equal (music-collapsed-p) 'collapsed)))
    (when folder
      (music-expand)
      (setq buffer-read-only nil)
      (save-excursion
          (let (here there (indentation nil))
            (end-of-line)
            (setq there (point))
            (beginning-of-line)
            (setq here (point))
            (when (search-forward "[" there t)
              (setq indentation (buffer-substring here (point))))
            (forward-line 1)
            (end-of-line)
            (setq there (point))
            (beginning-of-line)
            (while (and indentation
                        (not (search-forward indentation 
                                             (+ (point) (length indentation)) t))
                        (not (search-forward "** end **" there t)))
              (when (music-get-album)
                (music-select))
              (when (music-get-folder)
                (music-expand))
              (forward-line 1)
              (end-of-line)
              (setq there (point))
              (beginning-of-line))))
      (setq buffer-read-only t))
    (when (or track album)
      (music-expand)
      (setq buffer-read-only nil)
      (music-mark-line (if album "%%" (format "%d" (mod (+ 1 (length music-play-list)) 100))))
      (if track
          ;; HTTP tracks do not get expanded
          (setq music-play-list 
                (append music-play-list 
                        (list 
                         (if (music-is-url-p track) track
                           (expand-file-name track (music-prev-album)))))
                music-play-names 
                (append music-play-names (list (music-get-name)))
                )
        ;; album:
        (save-excursion
          ;; ****
          (setq music-selected-albums (cons album music-selected-albums))
          (let (atrack aalbum)
            (forward-line 1)
            (setq atrack (music-get-track)
                  aalbum (music-get-album))
            (while (not aalbum)
              (when atrack
                (setq music-play-list (append music-play-list 
                                              (list (if (music-is-url-p atrack) atrack
                                                        (expand-file-name atrack album)))
                                              )
                      music-play-names (append music-play-names (list (music-get-name)))
                      ))
              (music-mark-line (format "%d" (mod (length music-play-list) 100)))
              (forward-line 1)
              (setq atrack (music-get-track)
                    aalbum (music-get-album))))))
      (when is-collapsed
        (music-collapse))
      (setq buffer-read-only t))))

(defun music-delete ()
  "Unselects the (selected tracks from the) current album/track. 
Does nothing on folders."
  (interactive)
  (let ((track (music-get-track))
        (album (music-get-album))
        (is-collapsed (equal (music-collapsed-p) 'collapsed))
        )
    (when (or track album)
      (music-expand)
      (setq buffer-read-only nil)
      (music-mark-line "--")
      (if track
          (let ((track-list nil)
                (track-names nil)
                ;; HTTP tracks do not get expanded
                (track-del (if (music-is-url-p track) track
                             (expand-file-name track (music-prev-album)))))
            (dotimes (i (length music-play-list))
              (unless (equal (nth i music-play-list) track-del)
                (setq track-list (append track-list (list (nth i music-play-list)))
                      track-names (append track-names (list (nth i music-play-names))))))
            (setq music-play-list track-list
                  music-play-names track-names))
        ;; album
        (save-excursion
          (setq music-selected-albums (delete album music-selected-albums))
          (let (atrack aalbum)
            (forward-line 1)
            (setq atrack (music-get-track)
                  aalbum (or (music-get-album) (music-get-folder)))
            (while (not aalbum)
              (let ((track-list nil)
                    (track-names nil)
                    (track-del (if (music-is-url-p atrack) atrack (expand-file-name atrack album)))
                    )
                (dotimes (i (length music-play-list))
                  (unless (equal (nth i music-play-list) track-del)
                    (setq track-list (append track-list (list (nth i music-play-list)))
                          track-names (append track-names (list (nth i music-play-names))))))
                (setq music-play-list track-list
                      music-play-names track-names)
                )
              (music-mark-line "--")
              (forward-line 1)
              (setq atrack (music-get-track)
                    aalbum (or (music-get-album) (music-get-folder)))
              )
            )
          )
        )
      (when is-collapsed
        (music-collapse))
      (setq buffer-read-only t))))

(defun music-toggle ()
  "Selects/unselects the current entity, as appropriate."
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((there (point)))
      (when (music-get-folder)
        (music-select))
      (beginning-of-line)
      (if (search-forward " ---- " there t)
          (music-select)
        (when (or (music-get-album)
                  (music-get-track))
          (music-delete))))))

(defun music-delete-all ()
  "Clears the list of tracks scheduled for playback."
  (interactive)
  (save-excursion
    (setq buffer-read-only nil)
    (beginning-of-buffer)
    (while (search-forward-regexp "-[ %0-9][ %0-9]-" nil t)
      (delete-char -4)
      (insert "----"))
    (setq music-play-list nil
          music-play-names nil
          ;; ****
          music-selected-albums nil)
    (setq buffer-read-only t)))

;; URL helper

(defun music-is-url-p (track)
  "Non-NIL iff TRACK is a URL and starts with a known protocol name.
Known transport protocols are enumerated in `music-url-types'."
  (let (is-url)
    (dolist (x music-url-types)
      (let* ((proto (concat x "://"))
             (len (length proto)))
        (when (equal proto (substring track 0 len))
          (setq is-url t))
      ))
    is-url))

;; Mouse manipulation:

(defun music-mouse-toggle (ev)
  "Selects/unselects the album/track at mouse point."
  (interactive "e")
  (when (event-point ev)
    (goto-char (event-point ev))
    (music-toggle)))

;; Miscelaneous functions:

(defun music-mark-line (str)
  "For any X, replaces \"-XX-\" by \"-STR-\" at the beginning of the current line."
  (save-excursion
    (when (< (length str) 2)
      (setq str (concat " " str)))
    (end-of-line)
    (let ((there (point)))
      (beginning-of-line)
      ;(when (search-forward-regexp "[ ]+\\(\\[[+-]\\] \\)?-" there t)
      ;  (delete-char 2)
      ;  (insert str))
      (when (search-forward-regexp " -[0-9% -][0-9%-]- " there t)
        (forward-char -4)
        (delete-char 2)
        (insert str)
        ))))

(defun music-prev-album ()
  "Returns the absolute path of the nearest album above point."
  (save-excursion
    (let (aalbum)
      (forward-line -1)
      (setq aalbum (music-get-album))
      (while (not aalbum)
        (forward-line -1)
        (setq aalbum (music-get-album)))
      aalbum)))

(defun music-get-name ()
  "Returns the name of the current track (as a string), 
NIL on albums and folders."
  (save-excursion
    (beginning-of-line)
    (let ((here (point))
          there)
      (end-of-line)
      (setq there (point))
      (when (search-backward "<<" here t)
        (setq there (point))
        (search-backward-regexp "-[ 0-9][ 0-9]-" here t)
        (search-backward "----" here t)
        (setq here (+ (point) 4))
        (replace-in-string 
         (replace-in-string (buffer-substring here there)  
                            "^[ ]+" "")
         "[ ]+$" "")))))

(defun music-get-track ()
  "Returns the current track, NIL if not on a track."
  (save-excursion
    (end-of-line)
    (let (here (there (point)))
      (beginning-of-line)
      (when (search-forward "<<" there t)
        (setq here (point))
        (search-forward ">>" there t)
        (setq there (- (point) 2))
        (unless (equal (replace-in-string (buffer-substring here there) " " "") "")
          (buffer-substring here there))))))

(defun music-get-album ()
  "Returns the current album, NIL if not on an album."
  (save-excursion
    (end-of-line)
    (let (here (there (point)))
      (beginning-of-line)
      (when (search-forward "**" there t)
        (setq here (point))
        (search-forward "**" there t)
        (setq there (- (point) 2))
        (buffer-substring here there)))))

(defun music-get-folder ()
  "Returns the current folder, NIL if not on a folder."
  (save-excursion
    (end-of-line)
    (let (here (there (point)))
      (beginning-of-line)
      (when (search-forward "^^" there t)
        (setq here (point))
        (search-forward "^^" there t)
        (setq there (- (point) 2))
        (buffer-substring here there)))))


;; SECTION 6: Album/folder info (title and tracks file) manipulation.

(defvar music-info-thing "### no value ###"
  "Internal use.")

(defvar music-info-map (make-sparse-keymap)
  "Keymap for the info buffers (tracks or album info). Maps ESC to quit
 (`kill-buffer'), C-c C-c to save and quit, and, for tracks info, C-q
to \"prepare the buffer for insertion of titles\" (by inserting the
string \" ++ \" after each line, see `music-show-tracks-info').")


(defun music-show-tracks-info ()
  "Opens a new buffer containing the track list (see `music-track-files') for the current album."
  (interactive)
  (let ((album (if (music-get-track) 
                   (music-prev-album) 
                 (music-get-album))))
    (when album
      (if (music-find-track-file album)
          (find-file (expand-file-name (music-find-track-file album) album))
        (find-file (expand-file-name (car music-track-files) album)))
      (setq mode-name (concat "Tracks [" album "]")
            major-mode 'music-info)
      (make-variable-buffer-local 'music-info-thing)
      (setq music-info-thing album)
      (add-local-hook 'after-save-hook 'music-close-info-hooked t)
      (let ((map (make-sparse-keymap)))
        (define-key map [(escape)] 'music-info-quit)
        (define-key map [(control q)] '(lambda () (interactive)
                                         (beginning-of-buffer)
                                         (replace-string "\n" " ++ \n")))
        (define-key map [(control c)(control c)] 'music-info-save-quit)
        (setq music-info-map map))
      (use-local-map music-info-map))))

(defun music-show-album-info ()
  "Opens a new buffer containing the album/folder info for the current entity."
  (interactive)
  (let ((album (cond ((music-get-track) (music-prev-album))
                     ((music-get-folder))
                     ((music-get-album)))))
    (when album
      (if (music-find-title-file album)
          (find-file (expand-file-name (music-find-title-file album) album))
        (find-file (expand-file-name (car music-title-files) album)))
      (setq mode-name (concat "Info [" album "]")
            major-mode 'music-info)
      (make-variable-buffer-local 'music-info-thing)
      (setq music-info-thing album)
      (add-local-hook 'after-save-hook 'music-close-info-hooked t)
      (let ((map (make-sparse-keymap)))
        (define-key map [(escape)] 'music-info-quit)
        (define-key map [(control c)(control c)] 'music-info-save-quit)
        (setq music-info-map map))
      (use-local-map music-info-map))))

(defun music-close-info-hooked ()
  "Hook function to redisplay the content of the album/folder
after the said content has been changed by `music-show-tracks-info' or
`music-show-album-info'"
  (save-excursion
    (let ((what music-info-thing))
      (when (get-buffer "*Play List*")
        (set-buffer "*Play List*")
        (beginning-of-buffer)
        (when (search-forward what nil t)
          (if (equal (music-collapsed-p) 'collapsed)
              (progn
                (music-expand)
                (music-collapse))
            (music-collapse)
            (music-expand))
          )))))

;; Rebuild the track list:

(defun music-rebuild-album-append ()
  "Rebuilds the current album's listing (i.e., the tracks file, see `music-track-files'),
preserving the names for those tracks already existing on disk.
New tracks are added always at the end of the listing."
  (interactive)
  (when (music-get-album)
    (let* ((dir (music-get-album))
           (files (music-list-files dir nil t) ;; see the FSF Emacs compatibility 
                                               ;; functions (end of file)
                  ;;(directory-files dir nil nil nil t)
                  )
           here there file)
      (dolist (ff (append music-title-files music-track-files)) ;; ignore special files.
        (setq files (delete ff files)))
      (music-collapse)
      (music-show-tracks-info)
      ;; now in the buffer editing the track file...
      (end-of-buffer)
      (insert "^^^^^^\n")
      (beginning-of-buffer)
      (end-of-line)
      (setq there (point))
      (beginning-of-line)
      (setq here (point))
      (while (not (equal (buffer-substring here there) "^^^^^^"))
        (if (or (= here there) (equal (buffer-substring here (+ here 1)) "#") )
            ;; comment or blank, skip line
            (forward-line 1)
          (progn
            (if (search-forward " ++ " there t)
                (setq file (buffer-substring here (- (point) 4)))
              (setq file (buffer-substring here there)))
            (if (member file files) ;; file still exists?
                (progn
                  (setq files (delete file files)) ;; it does, move forward
                  (forward-line 1))
              ;; file no longer exists, remove the line
              (delete-region here (+ there 1)))))
          (end-of-line)
          (setq there (point))
          (beginning-of-line)
          (setq here (point))
        )
      ;; delete marker
      (end-of-buffer)
      (delete-region (point) (- (point) 7))
      ;; insert new files
      (insert (mapconcat '(lambda (str) str) files "\n") "\n")
      (music-info-save-quit)
      )))

(defun music-rebuild-album ()
  "Creates a brand new album listing (i.e., the tracks file, see `music-track-files').
Erases all track titles and comments."
  (interactive)
  (let* ((dir (music-get-album))
         (track-file (if (music-find-track-file dir) 
                         (music-find-track-file dir)
                       (car music-track-files))))
    (when (and dir (y-or-n-p "This will erase track comments. Are you sure? "))
      (condition-case nil
          (delete-file (expand-file-name track-file dir))
       (error nil))
      (music-collapse)))
  )

;; Quit editing the info files:

(defun music-info-quit ()
  (interactive)
  (kill-buffer nil))

(defun music-info-save-quit ()
  (interactive)
  (save-buffer)
  (kill-buffer nil))


;; SECTION 7: Playback.

(defvar music-play-frame nil
  "The popup frame created by `music-play-list' 
when `music-play-pop-frame' is non-nil.")

(defvar music-play-list nil
  "Tracks to be played (as list of files).")

(defvar music-play-names nil
  "Names of the tracks to be played (as list of strings).")

(defvar music-currently-playing nil
  "The index of the currently playing track in the *variable* `music-play-list'.")

(defvar music-process-buffer nil
  "The name of the buffer associated with the playback process.")

(defvar music-process nil
  "The playback process.")

(defvar music-paused nil
  "Non-nil iff the playback process is paused.")

(defvar music-stopped t
  "Non-nil iff no playback process is running.")

(defvar music-skip nil)

(defvar music-playback-list nil
  "Tracks currently played (as list of files).")

(defvar music-playback-names nil
  "Names of the tracks currently played (as list of strings).")

(defvar music-run-continuous nil)

(defvar music-time-string "Total time: "
  "The prefix of the playback time (in seconds) reported by the playback application
 (see `music-appl'). The default value is the one used by plaympeg. 
Used only for cosmetic purposes.")

(defvar music-loop-p nil 
  "If non-nil, the selection is continuously played 
 (i.e., the tracks from the *variable* `music-play-list') is
continuously played. Otherwise, the selection is played once.")

(defvar music-random-p nil
  "If non-nil, tracks from the current selection are played at random
 (i.e., the tracks from the *variable* `music-play-list').")


(defun music-play-list ()
  "Plays the list of tracks (stored as the symbol's value) in order,
using the player whose name is stored in `music-appl'. 

Any player can be used, provided that it understands the format(s) of
the tracks being played and that it takes as command line arguments
the name of one audio/video file and plays it.

If `music-play-pop-frame' is not nil (default, recommended), a frame
showing playback information will pop up when `music-play-list' is
called.

Also see `music-play-track', `music-play-list-sentinel', and the
*variable* `music-play-list'."
  (interactive)
  (random t)
  (when (and music-play-list music-stopped)
    (setq music-playback-list music-play-list
          music-playback-names music-play-names
          music-stopped nil
          music-currently-playing (if music-random-p (random (length music-play-list)) 0)
          music-run-continuous nil)
    (when (and (fboundp 'gnuserv-start) (not gnuserv-process))
      (gnuserv-start))
    (save-excursion
      (when (get-buffer "*Play List*")
        (set-buffer "*Play List*")
        (setq buffer-read-only nil)
        (save-excursion
          (beginning-of-buffer)
          (let ((here (point)))
            (forward-line 3)
            (delete-region here (point))
            (insert "\n"
                    "  <<        >>         ||         ||>        [ ]\n"
                    "  F8        F9         F10        F11        F12\n")))
        (setq buffer-read-only t))
      )
    (when (and music-play-pop-frame (get-buffer "*Play List*"))
      (music-show-control-frame)
      )
    (music-play-track)))

(defun music-play-this-track () 
  "Plays the track under cursor (the whole album if the cursor is on an album title).
Sets the list of tracks (the *variable* `music-play-list') to the
track(s) whose play is initiated.

This function is actually a wrapper for the main function
`music-play-list'. You may want to make it a wrapper for
`music-play-list-continuous' if you use a GUI based player."
  (interactive)
  (when (or (music-get-track) (music-get-album))
    (music-delete-all)
    (music-select)
    (music-play-list)  ;; comment this line and uncomment the line
		       ;; below if using a GUI based player.
    ;; (music-play-list-continuous)
    
    ;; The line below is a matter of taste... 
    ;; (music-delete-all)
    ))

(defun music-play-list-options ()
  "Plays the list of tracks (stored in the *variable* `music-play-list')
in order, using the player `music-appl'. Prompts for a string that is
passed to the player before the tracks themselves (whose intended
purpose is to contain options for the player).

The tracks being played are considered for all practical purposes a
large, single file. Thus, the skip commands do not work. In addition,
the loop and random flags have no effect, and no information about the
track being played in displayed. It is, however, the way to go if you
use a GUI based player, as the playback management is better handled
on the player's side for such players."
  (interactive)
  (music-play-list-continuous (read-string "Options: " "")))

(defun music-play-list-continuous (&optional options)
  "Plays the list of tracks (stored in the *variable* `music-play-list')
in order, using the player `music-appl'. This (not `music-play-list')
is the function to use for playback if `music-appl' is a graphical
user interface based player.

The tracks being played are considered for all practical purposes a
large, single file. Thus, the skip commands do not work. In addition,
the loop and random flags have no effect, and no information about the
track being played in displayed. It is, however, the way to go if you
use a GUI based player, as the playback management is better handled
on the player's side for such players."
  (interactive)
  (music-play-stop)
  (when music-stopped
    (setq music-stopped nil
          music-run-continuous t)
    (save-excursion
      (let ((buf (current-buffer))
            here 
            one-window
            (background-show t)
            (background-select nil))
        (when (and (fboundp 'gnuserv-start) (not gnuserv-process))
          (gnuserv-start))
        (when (get-buffer "**Play Process Buffer**")
          (kill-buffer "**Play Process Buffer**"))
        (if music-play-frame
            (setq one-window t)
          (progn
            (other-window 1)                               ; determine whether
            (setq one-window (equal (current-buffer) buf)) ; a new window will be
            (other-window -1)                              ; popped up when 
                                                           ; the playback starts... 
            ))
        (save-selected-frame
          (when music-play-frame
            (select-frame music-play-frame))
          (setq music-process
                (shell-command (concat music-appl " " (or options "") " "
                                       (mapconcat '(lambda (str) (concat "\"" str "\""))
                                                  music-play-list " ")
                                       " &")))
          (when music-process
            (set-process-sentinel music-process 'music-play-list-triv-sentinel))
          (other-window 1)
          (rename-buffer "**Play Process Buffer**")
          (setq music-process-buffer (current-buffer))
          (bury-buffer nil)
          (if one-window
              (delete-window)
            (other-window 1))
          (delete-other-windows))
        (when (get-buffer "*Play List*")
          (save-excursion
            (set-buffer "*Play List*")
            (setq buffer-read-only nil)
            (save-excursion
              (beginning-of-buffer)
              (setq here (point))
              (forward-line 3)
              (delete-region here (point))
              (insert "[ Playback info not available (see player?) ]\n"
                      "  ||         ||>        [ ]\n"
                      "  F10        F11        F12\n"))
            (setq buffer-read-only t))
          (set-buffer buf))
        ))))

;; Workhorse:

(defun music-play-track ()
  "Plays one track from the current selection (called by `music-play-list'). 
Puts `music-play-list-sentinel' as the sentinel for the playback
process (see `set-process-sentinel'). It is the responsibility of the
sentinel to continue the playback with the next track. In particular,
the sentinel should change `music-currently-playing' so that it points
to the next track. 

Prints information about the track being played at the beginning of
the buffer displaying the music library and in the minibuffer. This
information includes the index of the current track in the current
selection, the name of the track, and the playback time. In some
circumstances (mainly depending on the actual player `music-appl' and
on the system's -- and XEmacs' -- load), the playing time may not be
available. In such cases. \"??:??\" is printed instead of the actual
playback time. NOTE: If `music-appl' does output the time in seconds,
but this time is still not displayed, tweak the sleep time on the line
marked \"(***)\" in the code. A larger time increases the chances for
the time to appear, but may interfere with the other processings you
do with your XEmacs (as the whole application is frozen for that
period of time). For me, 4 tenths of a second is tolerable, but more
becomes annoying, hence the current value."
  (interactive)
  (random t)
  (save-excursion
    (let* ((buf (current-buffer))
           one-window 
           here there (time "??:??") min sec
           (background-show t)      ; make sure that windows pop up as expected... 
           (background-select nil))
      (when (get-buffer "**Play Process Buffer**")
        (kill-buffer "**Play Process Buffer**"))
      (if music-play-frame
          (setq one-window t)
        (progn                     ; not in our designated frame, but in a normal one... 
          (other-window 1)                               ; let's see whether the current 
          (setq one-window (equal (current-buffer) buf)) ; frame contains
          (other-window -1)                              ; more than one window... 
        ))
      (sleep-for 0.1) ; give some time for the previous playback
                      ; process to completely terminate... 
      (save-selected-frame
        (when music-play-frame
          (select-frame music-play-frame))
        (setq music-process
              (shell-command (concat music-appl " " 
                                     (concat "\"" (nth music-currently-playing music-playback-list) "\"")
                                     " &")))
        (when music-process
          (set-process-sentinel music-process 'music-play-list-sentinel))
        (other-window 1)
        (rename-buffer "**Play Process Buffer**")
        (setq music-process-buffer (current-buffer))
        (bury-buffer nil)
        (if one-window
            (delete-window)
          (other-window 1)))
      (when (get-buffer "**Play Process Buffer**") 
        (sleep-for 0.4)  ;;; (***)
        (save-excursion
          (set-buffer "**Play Process Buffer**")
          (beginning-of-buffer)
          (when (search-forward music-time-string nil t)
            (setq here (point))
            (end-of-line)
            (setq there (point))
            (goto-char here)
            (when (search-forward "." there t)
              (setq there (point)))
            (setq sec (string-to-int (buffer-substring here (point))))
            (setq min (/ sec 60)
                  sec (- sec (* min 60))
                  time (format "%d:%02d" min sec))
            )))
      (let ((msg 
             (concat (format "[ Playing track %d" (+ music-currently-playing 1))
                     " (" 
                     (if (nth music-currently-playing music-playback-names)
                         (nth music-currently-playing music-playback-names)
                       "no name")
                     "; "
                     time
                     (format ") of %d selected"  (length music-playback-list)) " ]\n")))
        (when (get-buffer "*Play List*")
          (save-excursion
            (set-buffer "*Play List*")
            (setq buffer-read-only nil)
            (save-excursion
              (beginning-of-buffer)
              (setq here (point))
              (forward-line 1)
              (delete-region here (point))
              (insert msg)
              (music-control-frame-change-track msg)
              )
              (setq buffer-read-only t)))
        (set-buffer buf)
        (message msg)
        )
      )))

;; Sentinels:

(defun music-play-list-triv-sentinel (process change)
  "Sentinel for the process generated by `music-play-list-continuous'
and `music-play-list-options' (see `set-process-sentinel'). Does
nothing special, just gives feedback when the current playback ends
and takes care of the proper handling of pause and resume commands."
  (cond ((eq (process-status process) 'stop)
         (save-excursion
           (when (get-buffer "*Play List*")
             (let (here there)
               (set-buffer "*Play List*")
               (setq buffer-read-only nil)
               (save-excursion
                 (beginning-of-buffer)
                 (forward-char 2)
                 (setq here (point))
                 (insert "PAUSED ")
                 (setq there (- (point) 1))
                 (when running-xemacs
                   (set-extent-property 
                    (make-extent here there) 'face (find-face 'message-music-face))))
               (setq buffer-read-only t)))
           (when (get-buffer " *Play Controls*")
             (let (here there)
               (set-buffer " *Play Controls*")
               (setq buffer-read-only nil)
               (save-excursion
                 (beginning-of-buffer)
                 (forward-char 2)
                 (setq here (point))
                 (insert "PAUSED ")
                 (setq there (- (point) 1))
                 (when running-xemacs
                   (set-extent-property 
                    (make-extent here there) 'face (find-face 'message-music-face))))
               (setq buffer-read-only t)))
           (message "Playback: paused.")))
        ((eq (process-status process) 'run)
         (save-excursion
           (when (get-buffer "*Play List*")
             (set-buffer "*Play List*")
             (setq buffer-read-only nil)
             (save-excursion
               (beginning-of-buffer)
               (forward-char 2)
               (when (equal "PAUSED " (buffer-substring  (point) (+ (point) 7)))
                 (delete-region (point) (+ (point) 7))))
             (setq buffer-read-only t))
           (when (get-buffer " *Play Controls*")
             (set-buffer " *Play Controls*")
             (setq buffer-read-only nil)
             (save-excursion
               (beginning-of-buffer)
               (forward-char 2)
               (when (equal "PAUSED " (buffer-substring  (point) (+ (point) 7)))
                 (delete-region (point) (+ (point) 7))))
             (setq buffer-read-only t))
           (message "Playback: resuming...")))
        (t
         (save-excursion
           (when music-play-frame
             (delete-frame music-play-frame)
             (setq music-play-frame nil))
           (when (get-buffer "*Play List*")
             (set-buffer "*Play List*")
             (setq buffer-read-only nil)
             (save-excursion
               (let (here)
                 (beginning-of-buffer)
                 (setq here (point))
                 (forward-line 3)
                 (delete-region here (point))
                 (insert "[ Nothing playing ]\n"
                         "\n  Hit F7 (selection), F6 (this track), or p (list, continuous) for playback.\n")
                 ))
               (setq buffer-read-only t)))
           (setq music-stopped t
                 music-run-continuous nil)
           (when (and music-stop-gnuserv (fboundp 'gnuserv-shutdown))
             (gnuserv-shutdown))
           (message "Playback: done."))))

(defun music-play-list-sentinel (process change)
  "Sentinel for the process generated by `music-play-track'
 (see `set-process-sentinel'). If there is something still to be played,
calls `music-play-track' again. Implements the loop and random
features, and gives feedback when the playback process is
paused/resumed."
  (cond ((eq (process-status process) 'stop)
         (save-excursion
           (when (get-buffer "*Play List*")
             (let (here there)
               (set-buffer "*Play List*")
               (setq buffer-read-only nil)
               (save-excursion
                 (beginning-of-buffer)
                 (forward-char 2)
                 (setq here (point))
                 (insert "PAUSED ")
                 (setq there (- (point) 1))
                 (when running-xemacs
                   (set-extent-property 
                    (make-extent here there) 'face (find-face 'message-music-face)))
                 )
               (setq buffer-read-only t)))
           (when (get-buffer " *Play Controls*")
             (let (here there)
               (set-buffer " *Play Controls*")
               (setq buffer-read-only nil)
               (save-excursion
                 (beginning-of-buffer)
                 (forward-char 2)
                 (setq here (point))
                 (insert "PAUSED ")
                 (setq there (- (point) 1))
                 (when running-xemacs
                   (set-extent-property 
                    (make-extent here there) 'face (find-face 'message-music-face)))
                 )
               (setq buffer-read-only t)))
           (message "Playback: paused.")))
        ((eq (process-status process) 'run)
         (save-excursion
           (when (get-buffer "*Play List*")
             (set-buffer "*Play List*")
             (setq buffer-read-only nil)
             (save-excursion
               (beginning-of-buffer)
               (forward-char 2)
               (when (equal "PAUSED " (buffer-substring (point) (+ (point) 7)))
                 (delete-region (point) (+ (point) 7))))
             (setq buffer-read-only t))
           (when (get-buffer " *Play Controls*")
             (set-buffer " *Play Controls*")
             (setq buffer-read-only nil)
             (save-excursion
               (beginning-of-buffer)
               (forward-char 2)
               (when (equal "PAUSED " (buffer-substring (point) (+ (point) 7)))
                 (delete-region (point) (+ (point) 7))))
             (setq buffer-read-only t))
           (message "Playback: resuming...")))
        (music-skip (setq music-skip nil))
        (music-stopped
         (when music-play-frame
           (delete-frame music-play-frame)
           (setq music-play-frame nil))
         (save-excursion
           (when (get-buffer "*Play List*")
             (set-buffer "*Play List*")
             (setq buffer-read-only nil)
             (save-excursion
               (let (here)
                 (beginning-of-buffer)
                 (setq here (point))
                 (forward-line 3)
                 (delete-region here (point))
                 (insert "[ Nothing playing ]\n"
                         "\n  Hit F7 (selection), F6 (this track), or p (list, continuous) for playback.\n")
                 ))
             (setq buffer-read-only t)))
           (setq music-run-continuous nil)
           (when (and music-stop-gnuserv (fboundp 'gnuserv-shutdown))
             (gnuserv-shutdown))
           (message "Playback: done."))
        (t ;; play "next" track
         (if music-random-p 
             (setq music-currently-playing 
                   (random (length music-playback-list)))
           (setq music-currently-playing (+ music-currently-playing 1)))
         (if (< music-currently-playing (length music-playback-list))
             (music-play-track)
           (if music-loop-p 
               (progn
                 (setq music-currently-playing 0)
                 (music-play-track))
             (save-excursion
               (music-play-stop)
               (when music-play-frame
                 (delete-frame music-play-frame)
                 (setq music-play-frame nil))
               (when (get-buffer "*Play List*")
                 (set-buffer "*Play List*")
                 (setq buffer-read-only nil)
                 (save-excursion
                   (let (here)
                     (beginning-of-buffer)
                     (setq here (point))
                     (forward-line 3)
                     (delete-region here (point))
                     (insert "[ Nothing playing ]\n"
                             "\n  Hit F7 (selection), F6 (this track), or p (list, continuous) for playback.\n")
                     ))
                 (setq buffer-read-only t
                       music-run-continuous nil))
               (when (and music-stop-gnuserv (fboundp 'gnuserv-shutdown))
                 (gnuserv-shutdown))
               (message "Playback: done.")))))))

;; Playback controls:

(defun music-play-skip ()
  "Skips to the next track in list. 
If already on the last track, does nothing. Ignores the loop and
random flags (i.e., the next track to be played is always the next one
in the playback list). However, subsequent tracks (after the one whose
playback is initiated by this function) will be chosen according to
the loop and/or random flags."

  (interactive)
  (when (and (< music-currently-playing (- (length music-playback-list) 1))
             (not music-stopped)
             (not music-run-continuous))
    (setq music-skip t)
    (music-play-stop)
    (sleep-for 0.2)
    (setq music-currently-playing (+ music-currently-playing 1))
    (setq music-stopped nil)
    (music-play-track)))

(defun music-play-skip-back ()
  "Skip to previous track in list. 
If already on the first track, just starts playing it from the
beginning. Ignores the loop and random flags (i.e., the next track to
be played is always the previous one in the playback list). However,
subsequent tracks (after the one whose playback is initiated by this
function) will be chosen according to the loop and/or random flags."
  (interactive)
  (when (and (not music-stopped)
             (not music-run-continuous))
    (setq music-skip t)
    (music-play-stop)
    (sleep-for 0.2)
    (setq music-stopped nil)
    (setq music-currently-playing (- music-currently-playing 1))
    (when (< music-currently-playing 0)
      (setq music-currently-playing 0))
    (music-play-track)))

(defun music-play-stop-this-track ()
  "Stops the play of the currently playing track."
  (interactive)  
  (when music-process
    (process-send-signal 'SIGKILL music-process))
  ;(when (get-buffer "**Play Process Buffer**")
  ;  (kill-buffer "**Play Process Buffer**")
  ;  )
  ;(when (and music-stop-gnuserv (fboundp 'gnuserv-shutdown))
  ;  (gnuserv-shutdown))
  )

(defun music-play-stop ()
  "Stops the play of the currently playing list. 
That is, kills the process that performs the play."
  (interactive)
  (setq music-stopped t)
  (when (get-buffer "**Play Process Buffer**")
    (when (and music-stop-gnuserv (fboundp 'gnuserv-shutdown))
      (gnuserv-shutdown))
    (kill-buffer "**Play Process Buffer**")
    )
  )

(defun music-play-pause ()
  "Pauses the playback.
This function is dependent on the actual player being used
 (given by `music-appl'), or rather on the OS. Specifically, the
player/OS must understand SIGSTOP as a command to interrupt the
execution and SIGCONT as a command to continue from wherever it
stopped."
  (interactive)
  (setq music-paused t)
  (when music-process
    (process-send-signal 'SIGSTOP music-process)
  ))
  
(defun music-play-resume ()
  "Resumes the playback that has been paused by `music-play-pause'. 
This function is dependent on the actual player being used
 (given by `music-appl'), or rather on the OS. Specifically, the 
player/OS must understand SIGSTOP as a command to interrupt the 
execution and SIGCONT as a command to continue from wherever it 
stopped."
  (interactive)
  (when music-paused
    (setq music-paused nil)
    (when music-process
      (process-send-signal 'SIGCONT music-process)
  )))

(defun music-play-pause/resume ()
  "Pauses the playback process or resumes a paused process
by calling `music-play-pause' or `music-play-resume' as
appropriate. This function is dependent on the actual player being
used (given by `music-appl'), or rather on the OS. Specifically, the
player/OS must understand SIGSTOP as a command to interrupt the
execution and SIGCONT as a command to continue from wherever it
stopped."
  (interactive)
  (if music-paused
      (music-play-resume)
    (music-play-pause)))

;; Select the playback application:

(defun music-appl-select ()
  "Selects the playback application by setting `music-appl' 
to the next available command line in `music-appl-list' (which is
viewed as a circular structure)."
  (interactive)
  (let ((old-appl (member music-appl music-appl-list)))
    (when music-appl
      (setq music-appl (if (cdr old-appl)
                           (car (cdr old-appl))
                         (car music-appl-list)))
      (message "\"%s\" used now as playback application."
               music-appl))))

;; Random and loop:

(defun music-loop-toggle ()
  "Toggles the loop flag (`music-loop-p')."
  (interactive)
  (setq music-loop-p (not music-loop-p))
  (save-excursion
    (when (get-buffer "*Play List*")
      (set-buffer "*Play List*")
      (music-print-loop-random))
    (when (get-buffer " *Play Controls*")
      (set-buffer " *Play Controls*")
      (music-print-loop-random))
    ))

(defun music-random-toggle ()
  "Toggles the random flag (`music-random-p')."
  (interactive)
  (setq music-random-p (not music-random-p))
  (save-excursion
    (when (get-buffer "*Play List*")
      (set-buffer "*Play List*")
      (music-print-loop-random))
    (when (get-buffer " *Play Controls*")
      (set-buffer " *Play Controls*")
      (music-print-loop-random))
    ))

(defun music-print-loop-random ()
  "Updates the visual echo of the random (`music-random-p') and loop (`music-loop-p') flags 
in all the appropriate buffers.  Called whenever these flags change, 
which is to say called from `music-random-toggle' and `music-loop-toggle'."
  (let (here there)
    (save-excursion
      (setq buffer-read-only nil)
      (beginning-of-buffer)
      (while (search-forward "(Loop:" nil t)
        (beginning-of-line)
        (setq here (point))
        (end-of-line)
        (setq there (point))
        (delete-region here there)
        (insert (concat "(Loop: " (if music-loop-p " ON" "off") " [toggle]; Random: " 
                        (if music-random-p " ON" "off") " [toggle])"))
        (save-excursion
          (beginning-of-line)
          (when (and running-xemacs music-loop-p)
            (set-extent-property (make-extent (+ (point) 8) (+ (point) 10)) 
                                 'face (find-face 'message-music-face)))
          (when (and running-xemacs music-random-p)
            (set-extent-property (make-extent (+ (point) 30) (+ (point) 32)) 
                                 'face (find-face 'message-music-face))))
        )
      (setq buffer-read-only t)))
  )

;; ESD remote host

(defun music-esd-remote-speakers-interactive ()
  "Prompts for a host name and a port, and sets the ESD speakers appropriately.
So if your player uses ESD as output the sound will be played on the
respective machine instead of locally.  Of course, the remote machine
must be running ESD which must accept connections on the provided
port.  An empty host name resets the ESD speakers to the local host."
  (interactive)
  (let ((host (read-string "Host (empty for local): " nil nil nil)))
    (if (equal host "")
        (music-esd-remote-speakers)
      (let ((port (read-string "Port: " "5000" nil "5000")))
          (music-esd-remote-speakers host port))
      )))

(defun music-esd-remote-speakers-reset ()
  "Resets the ESD speakers to local playback."
  (interactive)
  (music-esd-remote-speakers)
)

(defun music-esd-remote-speakers (&optional host port)
  "Sets the ESD speakers to HOST:PORT.
If HOST is NIL, sets the speakers to local host.  PORT defaults to
5000."
  (if host
      (let ((port (if port port "5000")))
        (setenv "ESPEAKER" (concat host ":" port))
        (message "ESD speaker set to %s:%s." host port))
    (setenv "ESPEAKER")
    (message "ESD speaker set to local." host port))
)

;; Playback controls

(defun music-control-frame-change-track (msg)
  "Inserts MSG as the first line in `music-play-frame', 
the frame displaying the playback controls."
  (when (get-buffer " *Play Controls*")
    (save-excursion
      (set-buffer " *Play Controls*")
      (setq buffer-read-only nil)
      (beginning-of-buffer)
      (let ((here (point)) there)
        (end-of-line)
        (setq there (point))
        (delete-region here there)
        (insert (replace-in-string msg "\n" "" t)) (forward-line 1))
      (setq buffer-read-only t)
      )))

(defun music-show-control-frame ()
  "Pops up a frame with playback controls."
  (let ((buf (get-buffer-create " *Play Controls*")) 
        (height 8))  ; 5 lines for playback controls and info... 
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      ;; set the mode-line of the pop-up window
      ;(setq modeline-format (concat "-------------------- Play Status (" music-appl ")%-"))
      (erase-buffer)
      (insert (concat "[ Nothing playing!! ]\n"
                      "  <<        >>         ||         ||>        [ ]\n"
                      "  F8        F9         F10        F11        F12\n"
                      "\n(Loop: " 
                      (if music-loop-p " ON" "off") " [toggle]; Random: " 
                      (if music-random-p " ON" "off") 
                      " [toggle])"))
      (beginning-of-buffer)
      (when running-xemacs
        (set-extent-property (make-extent (+ (point) 2 ) (+ (point) 19))
                             'face (find-face 'message-music-face)))
      (when (and running-xemacs music-loop-p) ;; 128
        (goto-char 130)
        (set-extent-property (make-extent (point) (+ (point) 2))
                             'face (find-face 'message-music-face)))
      (when (and running-xemacs music-random-p) ;; 150
        (goto-char 152)
        (set-extent-property (make-extent (point) (+ (point) 2))
                             'face (find-face 'message-music-face)))
      (setq mode-name "Music (play controls)"
            major-mode 'music-mode
            buffer-read-only t
            truncate-lines t)
      (easy-menu-add music-play-small-menu)
      (use-local-map music-mode-map)
      ;; If already have a frame, use it. If not, make a new one...
      (if (and music-play-frame (frame-live-p music-play-frame))
          (let ((s (selected-frame)))
            (select-frame music-play-frame)
            (make-frame-visible music-play-frame)
            (set-frame-height music-play-frame height)
            (switch-to-buffer buf)
            (beginning-of-buffer)
            (forward-char 2)
            (sit-for 0)
            (select-frame s)
            )
        (progn
          (setq music-play-frame (make-frame))
          (set-frame-height music-play-frame height)
          (select-frame music-play-frame)
          (switch-to-buffer buf)
          (beginning-of-buffer)
          (forward-char 2)
          )))))

;; SECTION 8: FSF Emacs compatibility functions

;; The following two functions are borrowed from the XEmacs 21.4.12
;; elisp code.

(unless (fboundp 'replace-in-string)
  (defun replace-in-string (str regexp newtext &optional literal)
    "Replace all matches in STR for REGEXP with NEWTEXT string,
 and returns the new string.
Borrowed from the XEmacs elisp code (subr.el).
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat `\\' in NEWTEXT as special:
  `\\&' in NEWTEXT means substitute original matched text.
  `\\N' means substitute what matched the Nth `\\(...\\)'.
       If Nth parens didn't match, substitute nothing.
  `\\\\' means insert one `\\'.
  `\\u' means upcase the next character.
  `\\l' means downcase the next character.
  `\\U' means begin upcasing all following characters.
  `\\L' means begin downcasing all following characters.
  `\\E' means terminate the effect of any `\\U' or `\\L'."
    ;; No such thing in FSF Emacs?!?
    ;(check-argument-type 'stringp str)
    ;(check-argument-type 'stringp newtext)
    (if (> (length str) 50)
        (let ((cfs case-fold-search))
          (with-temp-buffer
            (setq case-fold-search cfs)
            (insert str)
            (goto-char 1)
            (while (re-search-forward regexp nil t)
              (replace-match newtext t literal))
            (buffer-string)))
      (let ((start 0) newstr)
        (while (string-match regexp str start)
          (setq newstr (replace-match newtext t literal str)
                start (+ (match-end 0) (- (length newstr) (length str)))
                str newstr))
        str)))
)

(unless (fboundp 'save-selected-frame)
  (defmacro save-selected-frame (&rest body)
    "Execute forms in BODY, then restore the selected frame.
Borrowed from the XEmacs elisp code (frame.el).
The value returned is the value of the last form in BODY."
    (let ((old-frame (gensym "ssf")))
      `(let ((,old-frame (selected-frame)))
         (unwind-protect
             (progn ,@body)
           (select-frame ,old-frame)))))
)

(defun music-list-files (folder &optional full files-only)
  "Return a list of names of files in FOLDER.
If FULL is non-nil, absolute pathnames of the files are returned.
If FILES-ONLY is non-NIL, only files in the directory are returned, 
otherwise only subdirectories are returned.
This is basically a wrapped for `directory-files' in XEmacs, and a
work-around for the same function in FSF Emacs, where directory-files
lacks the FILES-ONLY argument."
  (if running-xemacs ;; XEmacs, too easy...
      (if files-only
          (directory-files folder full nil nil t)
        (directory-files folder full nil nil 'dirs)
        )
    ;; FSF Emacs work-around
    (let ((lof (directory-files folder full nil nil))
          lof-ret)
      (dolist (f lof)
        (let ((ff (expand-file-name f folder)))
          (when (and files-only (not (file-directory-p ff)))
            (setq lof-ret (append lof-ret (list f))))
          (when (and (not files-only) (file-directory-p ff))
            (setq lof-ret (append lof-ret (list f))))))
      lof-ret)))

;; Make myself known... 

(provide 'music)

;; end music.el
