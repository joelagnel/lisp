;;;Auto generated

;;;### (autoloads (atom-blogger-delete-entry atom-blogger-publish
;;;;;;  atom-blogger-put-entry atom-blogger-post-entry atom-blogger-new-entry
;;;;;;  atom-blogger-edit-entry) "atom-blogger" "atom-blogger/atom-blogger.el"
;;;;;;  (17692 29757))
;;; Generated autoloads from atom-blogger/atom-blogger.el

(autoload (quote atom-blogger-edit-entry) "atom-blogger" "\
Retrieve entry and prepare it for editting.
The retrieved entry is placed in a buffer ready for editing.
`url' is the URL of the entry,
`auth' is of the form username:password." t nil)

(autoload (quote atom-blogger-new-entry) "atom-blogger" "\
Create a new Blog post." t nil)

(autoload (quote atom-blogger-post-entry) "atom-blogger" "\
Publish the Blog entry in the current buffer." t nil)

(autoload (quote atom-blogger-put-entry) "atom-blogger" "\
Publish the editted Blog entry in the current buffer." t nil)

(autoload (quote atom-blogger-publish) "atom-blogger" "\
Publish current entry." t nil)

(autoload (quote atom-blogger-delete-entry) "atom-blogger" "\
Delete specified entry.
`url' is the URL of the entry,
`auth' is of the form username:password." t nil)

;;;***

;;;### (autoloads (cd-tool) "cd-tool" "cd-tool.el" (17751 59923))
;;; Generated autoloads from cd-tool.el

(autoload (quote cd-tool) "cd-tool" "\
Front-end to CDTool.
Bind this function to a convenient key-
Emacspeak users automatically have
this bound to <DEL> in the emacspeak keymap.

Key     Action
---     ------

+       Next Track
-       Previous Track
SPC     Pause or Resume
e       Eject
=       Shuffle
i       CD Info
p       Play
s       Stop
t       track
c       clip
cap C   Save clip to disk
" t nil)

;;;***

;;;### (autoloads (dtk-set-chunk-separator-syntax dtk-toggle-splitting-on-white-space
;;;;;;  tts-show-debug-buffer tts-restart dtk-select-server dtk-resume
;;;;;;  dtk-pause tts-speak-version dtk-reset-state dtk-toggle-punctuation-mode
;;;;;;  dtk-set-punctuations-to-some dtk-set-punctuations-to-all
;;;;;;  dtk-set-punctuations dtk-set-character-scale dtk-set-predefined-speech-rate
;;;;;;  dtk-set-rate ems-generate-switcher dtk-add-cleanup-pattern
;;;;;;  dtk-notes-shutdown) "dtk-speak" "dtk-speak.el" (17751 59923))
;;; Generated autoloads from dtk-speak.el

(autoload (quote dtk-notes-shutdown) "dtk-speak" "\
Shutdown midi system." t nil)

(defsubst dtk-stop nil "\
Stop speech now." (interactive) (declare (special dtk-speaker-process)) (dtk-interp-stop))

(autoload (quote dtk-add-cleanup-pattern) "dtk-speak" "\
Add this pattern to the list of repeating patterns that
are cleaned up.  Optional interactive prefix arg deletes
this pattern if previously added.  Cleaning up repeated
patterns results in emacspeak speaking the pattern followed
by a repeat count instead of speaking all the characters
making up the pattern.  Thus, by adding the repeating
pattern `.' (this is already added by default) emacspeak
will say ``aw fifteen dot'' when speaking the string
``...............'' instead of ``period period period period
''" t nil)

(autoload (quote ems-generate-switcher) "dtk-speak" "\
Generate desired command to switch the specified state." nil nil)

(autoload (quote dtk-set-rate) "dtk-speak" "\
Set speaking RATE for the tts.
Interactive PREFIX arg means set   the global default value, and then set the
current local  value to the result." t nil)

(autoload (quote dtk-set-predefined-speech-rate) "dtk-speak" "\
Set speech rate to one of nine predefined levels.
Interactive PREFIX arg says to set the rate globally.
Formula used is:
rate = dtk-speech-rate-base + dtk-speech-rate-step * level." t nil)

(autoload (quote dtk-set-character-scale) "dtk-speak" "\
Set scale FACTOR for   speech rate.
Speech rate is scaled by this factor
when speaking characters.
Interactive PREFIX arg means set   the global default value, and then set the
current local  value to the result." t nil)

(autoload (quote dtk-set-punctuations) "dtk-speak" "\
Set punctuation mode to MODE.
Possible values are `some', `all', or `none'.
Interactive PREFIX arg means set   the global default value, and then set the
current local  value to the result." t nil)

(autoload (quote dtk-set-punctuations-to-all) "dtk-speak" "\
Set punctuation  mode to all.
Interactive PREFIX arg sets punctuation mode globally." t nil)

(autoload (quote dtk-set-punctuations-to-some) "dtk-speak" "\
Set punctuation  mode to some.
Interactive PREFIX arg sets punctuation mode globally." t nil)

(autoload (quote dtk-toggle-punctuation-mode) "dtk-speak" "\
Toggle punctuation mode between \"some\" and \"all\".
Interactive PREFIX arg makes the new setting global." t nil)

(autoload (quote dtk-reset-state) "dtk-speak" "\
Restore sanity to the Dectalk.
Typically used after the Dectalk has been power   cycled." t nil)

(autoload (quote tts-speak-version) "dtk-speak" "\
Speak version." t nil)

(autoload (quote dtk-pause) "dtk-speak" "\
Pause ongoing speech.
The speech can be resumed with command `dtk-resume'
normally bound to \\[dtk-resume].  Pausing speech is useful when one needs to
perform a few actions before continuing to read a large document.  Emacspeak
gives you speech feedback as usual once speech has been paused.  `dtk-resume'
continues the interrupted speech irrespective of the buffer
in which it is executed.
Optional PREFIX arg flushes any previously paused speech." t nil)

(autoload (quote dtk-resume) "dtk-speak" "\
Resume paused speech.
This command resumes  speech that has been suspended by executing
command `dtk-pause' bound to \\[dtk-pause].
If speech has not been paused,
and option `dtk-resume-should-toggle' is set,
 then this command will pause ongoing speech." t nil)

(autoload (quote dtk-select-server) "dtk-speak" "\
Select a speech server interactively.
Argument PROGRAM specifies the speech server program.
When called  interactively, The selected server is started immediately. " t nil)

(autoload (quote tts-restart) "dtk-speak" "\
Use this to nuke the currently running TTS server and restart it." t nil)

(autoload (quote tts-show-debug-buffer) "dtk-speak" "\
Select TTS debugging buffer." t nil)

(autoload (quote dtk-toggle-splitting-on-white-space) "dtk-speak" "\
Toggle splitting of speech on white space.
This affects the internal state of emacspeak that decides if we split
text purely by clause boundaries, or also include
whitespace.  By default, emacspeak sends a clause at a time
to the speech device.  This produces fluent speech for
normal use.  However in modes such as `shell-mode' and some
programming language modes, clause markers appear
infrequently, and this can result in large amounts of text
being sent to the speech device at once, making the system
unresponsive when asked to stop talking.  Splitting on white
space makes emacspeak's stop command responsive.  However,
when splitting on white space, the speech sounds choppy
since the synthesizer is getting a word at a time." t nil)

(autoload (quote dtk-set-chunk-separator-syntax) "dtk-speak" "\
Interactively set how text is split in chunks.
See the Emacs documentation on syntax tables for details on how characters are
classified into various syntactic classes.
Argument S specifies the syntax class." t nil)

;;;***

;;;### (autoloads (emacspeak-setup-programming-mode emacspeak-media-player)
;;;;;;  "emacspeak" "emacspeak.el" (17751 59934))
;;; Generated autoloads from emacspeak.el

(defvar emacspeak-media-player (quote emacspeak-m-player) "\
Default media player to use.
This is a Lisp function that takes a resource locator.")

(autoload (quote emacspeak-setup-programming-mode) "emacspeak" "\
Setup programming mode. Turns on audio indentation and
sets punctuation mode to all, activates the dictionary and turns on split caps." nil nil)

;;;***

;;;### (autoloads (emacspeak-alsaplayer-launch) "emacspeak-alsaplayer"
;;;;;;  "emacspeak-alsaplayer.el" (17751 59958))
;;; Generated autoloads from emacspeak-alsaplayer.el

(define-prefix-command (quote emacspeak-alsaplayer-prefix-command) (quote emacspeak-alsaplayer-mode-map))

(autoload (quote emacspeak-alsaplayer-launch) "emacspeak-alsaplayer" "\
Launch Alsaplayer.
user is placed in a buffer associated with the newly created
Alsaplayer session." t nil)

;;;***

;;;### (autoloads (emacspeak-amphetadesk-quick-add emacspeak-amphetadesk)
;;;;;;  "emacspeak-amphetadesk" "emacspeak-amphetadesk.el" (17751
;;;;;;  59924))
;;; Generated autoloads from emacspeak-amphetadesk.el

(autoload (quote emacspeak-amphetadesk) "emacspeak-amphetadesk" "\
Open amphetadesk.
Interactive prefix-arg use-opml opens the myChannels.opml file." t nil)

(autoload (quote emacspeak-amphetadesk-quick-add) "emacspeak-amphetadesk" "\
Quick add URL to Amphetadesk by prompting for URL." t nil)

;;;***

;;;### (autoloads (emacspeak-atom-browse emacspeak-atom-display)
;;;;;;  "emacspeak-atom" "emacspeak-atom.el" (17751 59924))
;;; Generated autoloads from emacspeak-atom.el

(defgroup emacspeak-atom nil "ATOM Feeds for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-atom-display) "emacspeak-atom" "\
Retrieve and display ATOM URL." t nil)

(autoload (quote emacspeak-atom-browse) "emacspeak-atom" "\
Browse specified ATOM feed." t nil)

;;;***

;;;### (autoloads (emacspeak-aumix-volume-decrease emacspeak-aumix-volume-increase
;;;;;;  emacspeak-aumix-wave-decrease emacspeak-aumix-wave-increase
;;;;;;  emacspeak-aumix-reset emacspeak-aumix-reset-options emacspeak-alsactl-program
;;;;;;  emacspeak-aumix-settings-file) "emacspeak-aumix" "emacspeak-aumix.el"
;;;;;;  (17751 59924))
;;; Generated autoloads from emacspeak-aumix.el

(defgroup emacspeak-aumix nil "Customization group for setting the Emacspeak auditory\ndisplay." :group (quote emacspeak))

(defvar emacspeak-aumix-settings-file (when (file-exists-p (expand-file-name ".aumixrc" emacspeak-resource-directory)) (expand-file-name ".aumixrc" emacspeak-resource-directory)) "\
*Name of file containing personal aumix settings.")

(defvar emacspeak-alsactl-program "alsactl" "\
ALSA sound controller used to restore settings.")

(defvar emacspeak-aumix-reset-options (format "-f %s -L 2>&1 >/dev/null" emacspeak-aumix-settings-file) "\
*Option to pass to aumix for resetting to default values.")

(autoload (quote emacspeak-aumix-reset) "emacspeak-aumix" "\
Reset to default audio settings." t nil)

(autoload (quote emacspeak-aumix-wave-increase) "emacspeak-aumix" "\
Increase volume of wave output. " t nil)

(autoload (quote emacspeak-aumix-wave-decrease) "emacspeak-aumix" "\
Decrease volume of wave output. " t nil)

(autoload (quote emacspeak-aumix-volume-increase) "emacspeak-aumix" "\
Increase overall volume. " t nil)

(autoload (quote emacspeak-aumix-volume-decrease) "emacspeak-aumix" "\
Decrease overall volume. " t nil)

;;;***

;;;### (autoloads (emacspeak-appt-repeat-announcement) "emacspeak-calendar"
;;;;;;  "emacspeak-calendar.el" (17751 59925))
;;; Generated autoloads from emacspeak-calendar.el

(autoload (quote emacspeak-appt-repeat-announcement) "emacspeak-calendar" "\
Speaks the most recently displayed appointment message if any." t nil)

;;;***

;;;### (autoloads (emacspeak-custom-goto-toolbar emacspeak-custom-goto-group)
;;;;;;  "emacspeak-custom" "emacspeak-custom.el" (17751 59925))
;;; Generated autoloads from emacspeak-custom.el

(autoload (quote emacspeak-custom-goto-group) "emacspeak-custom" "\
Jump to custom group when in a customization buffer." t nil)

(autoload (quote emacspeak-custom-goto-toolbar) "emacspeak-custom" "\
Jump to custom toolbar when in a customization buffer." t nil)

;;;***

;;;### (autoloads (emacspeak-daisy-next-line emacspeak-daisy-define-outline-pattern
;;;;;;  emacspeak-daisy-previous-line emacspeak-daisy-play-audio-under-point
;;;;;;  emacspeak-daisy-play-content-under-point emacspeak-daisy-mark-position-in-content-under-point
;;;;;;  emacspeak-daisy-save-bookmarks emacspeak-daisy-open-book
;;;;;;  emacspeak-daisy-stop-audio emacspeak-daisy-play-page-range)
;;;;;;  "emacspeak-daisy" "emacspeak-daisy.el" (17751 59926))
;;; Generated autoloads from emacspeak-daisy.el

(autoload (quote emacspeak-daisy-play-page-range) "emacspeak-daisy" "\
Play pages in specified page range." t nil)

(autoload (quote emacspeak-daisy-stop-audio) "emacspeak-daisy" "\
Stop audio." t nil)

(autoload (quote emacspeak-daisy-open-book) "emacspeak-daisy" "\
Open Digital Talking Book specified by navigation file filename.

This is the main entry point to the  Emacspeak Daisy reader.
Opening a Daisy navigation file (.ncx file) results in a
navigation buffer that can be used to browse and read the book." t nil)

(autoload (quote emacspeak-daisy-save-bookmarks) "emacspeak-daisy" "\
Save bookmarks for current book." t nil)

(autoload (quote emacspeak-daisy-mark-position-in-content-under-point) "emacspeak-daisy" "\
Mark current position in displayed content.
No-op if content under point is not currently displayed." t nil)

(autoload (quote emacspeak-daisy-play-content-under-point) "emacspeak-daisy" "\
Play SMIL content  under point." t nil)

(autoload (quote emacspeak-daisy-play-audio-under-point) "emacspeak-daisy" "\
Play audio clip under point." t nil)

(autoload (quote emacspeak-daisy-previous-line) "emacspeak-daisy" "\
Move to previous line." t nil)

(autoload (quote emacspeak-daisy-define-outline-pattern) "emacspeak-daisy" "\
Define persistent outline regexp for this book." t nil)

(autoload (quote emacspeak-daisy-next-line) "emacspeak-daisy" "\
Move to next line." t nil)

;;;***

;;;### (autoloads (emacspeak-eterm-remote-term emacspeak-eterm-cache-remote-host
;;;;;;  emacspeak-eterm-record-window) "emacspeak-eterm" "emacspeak-eterm.el"
;;;;;;  (17751 59927))
;;; Generated autoloads from emacspeak-eterm.el

(defgroup emacspeak-eterm nil "Terminal emulator for the Emacspeak Desktop." :group (quote emacspeak) :prefix "emacspeak-eterm-")

(autoload (quote emacspeak-eterm-record-window) "emacspeak-eterm" "\
Insert this window definition into the table of terminal windows.
Argument WINDOW-ID specifies the window.
Argument TOP-LEFT  specifies top-left of window.
Argument BOTTOM-RIGHT  specifies bottom right of window.
Optional argument RIGHT-STRETCH  specifies if the window stretches to the right.
Optional argument LEFT-STRETCH  specifies if the window stretches to the left." nil nil)

(defvar emacspeak-eterm-remote-hosts-table (make-vector 127 0) "\
obarray used for completing hostnames when prompting for a remote
host. Hosts are added whenever a new hostname is encountered, and the
list of known hostnames is persisted in file named by
emacspeak-eterm-remote-hostnames")

(autoload (quote emacspeak-eterm-cache-remote-host) "emacspeak-eterm" "\
Add this hostname to cache of remote hostnames" nil nil)

(autoload (quote emacspeak-eterm-remote-term) "emacspeak-eterm" "\
Start a terminal-emulator in a new buffer." t nil)

;;;***

;;;### (autoloads (emacspeak-filtertext-revert emacspeak-filtertext)
;;;;;;  "emacspeak-filtertext" "emacspeak-filtertext.el" (17751 59927))
;;; Generated autoloads from emacspeak-filtertext.el

(autoload (quote emacspeak-filtertext) "emacspeak-filtertext" "\
Copy over text in region to special filtertext buffer in
preparation for interactively filtering text. " t nil)

(autoload (quote emacspeak-filtertext-revert) "emacspeak-filtertext" "\
Revert to original text." t nil)

;;;***

;;;### (autoloads (emacspeak-fix-interactive) "emacspeak-fix-interactive"
;;;;;;  "emacspeak-fix-interactive.el" (17751 59927))
;;; Generated autoloads from emacspeak-fix-interactive.el

(defvar emacspeak-commands-dont-fix-regexp (concat "^ad-Orig\\|^mouse\\|^scroll-bar" "\\|^face\\|^frame\\|^font" "\\|^color\\|^timer") "\
Regular expression matching function names whose interactive spec should not be fixed.")

(defsubst emacspeak-should-i-fix-interactive-p (sym) "\
Predicate to test if this function should be fixed. " (declare (special emacspeak-commands-dont-fix-regexp)) (and (commandp sym) (not (get sym (quote emacspeak-checked-interactive))) (stringp (second (ad-interactive-form (symbol-function sym)))) (not (string-match emacspeak-commands-dont-fix-regexp (symbol-name sym)))))

(autoload (quote emacspeak-fix-interactive) "emacspeak-fix-interactive" "\
Auto-advice interactive command to speak its prompt.  
Fix the function definition of sym to make its interactive form
speak its prompts. This function needs to do very little work as
of Emacs 21 since all interactive forms except `c' and `k' now
use the minibuffer." nil nil)

(defsubst emacspeak-fix-interactive-command-if-necessary (command) "\
Fix command if necessary." (and (emacspeak-should-i-fix-interactive-p command) (emacspeak-fix-interactive command)))

;;;***

;;;### (autoloads (emacspeak-forms-find-file) "emacspeak-forms" "emacspeak-forms.el"
;;;;;;  (17751 59927))
;;; Generated autoloads from emacspeak-forms.el

(autoload (quote emacspeak-forms-find-file) "emacspeak-forms" "\
Visit a forms file" t nil)

;;;***

;;;### (autoloads (emacspeak-gridtext-apply emacspeak-gridtext-save
;;;;;;  emacspeak-gridtext-load) "emacspeak-gridtext" "emacspeak-gridtext.el"
;;;;;;  (17751 59927))
;;; Generated autoloads from emacspeak-gridtext.el

(autoload (quote emacspeak-gridtext-load) "emacspeak-gridtext" "\
Load saved grid settings." t nil)

(autoload (quote emacspeak-gridtext-save) "emacspeak-gridtext" "\
Save out grid settings." t nil)

(autoload (quote emacspeak-gridtext-apply) "emacspeak-gridtext" "\
Apply grid to region." t nil)

;;;***

;;;### (autoloads (emacspeak-hide-speak-block-sans-prefix emacspeak-hide-or-expose-all-blocks
;;;;;;  emacspeak-hide-or-expose-block) "emacspeak-hide" "emacspeak-hide.el"
;;;;;;  (17751 59928))
;;; Generated autoloads from emacspeak-hide.el

(autoload (quote emacspeak-hide-or-expose-block) "emacspeak-hide" "\
Hide or expose a block of text.
This command either hides or exposes a block of text
starting on the current line.  A block of text is defined as
a portion of the buffer in which all lines start with a
common PREFIX.  Optional interactive prefix arg causes all
blocks in current buffer to be hidden or exposed." t nil)

(autoload (quote emacspeak-hide-or-expose-all-blocks) "emacspeak-hide" "\
Hide or expose all blocks in buffer." t nil)

(autoload (quote emacspeak-hide-speak-block-sans-prefix) "emacspeak-hide" "\
Speaks current block after stripping its prefix.
If the current block is not hidden, it first hides it.
This is useful because as you locate blocks, you can invoke this
command to listen to the block,
and when you have heard enough navigate easily  to move past the block." t nil)

;;;***

;;;### (autoloads (emacspeak-imcom) "emacspeak-imcom" "emacspeak-imcom.el"
;;;;;;  (17751 59928))
;;; Generated autoloads from emacspeak-imcom.el

(defgroup emacspeak-imcom nil "Jabber access from the Emacspeak audio desktop." :group (quote emacspeak))

(autoload (quote emacspeak-imcom) "emacspeak-imcom" "\
Start IMCom." t nil)

;;;***

;;;### (autoloads (emacspeak-info-wizard) "emacspeak-info" "emacspeak-info.el"
;;;;;;  (17751 59928))
;;; Generated autoloads from emacspeak-info.el

(autoload (quote emacspeak-info-wizard) "emacspeak-info" "\
Read a node spec from the minibuffer and launch
Info-goto-node.
See documentation for command `Info-goto-node' for details on
node-spec." t nil)

;;;***

;;;### (autoloads (emacspeak-keymap-remove-emacspeak-edit-commands
;;;;;;  emacspeak-keymap-choose-new-emacspeak-prefix emacspeak-hyper-keys
;;;;;;  emacspeak-alt-keys emacspeak-super-keys emacspeak-personal-keys)
;;;;;;  "emacspeak-keymap" "emacspeak-keymap.el" (17751 59928))
;;; Generated autoloads from emacspeak-keymap.el

(defvar emacspeak-keymap nil "\
Primary keymap used by emacspeak. ")

(defvar emacspeak-personal-keys nil "\
*Specifies personal key bindings for the audio desktop.
Bindings specified here are available on prefix key C-e x
for example, if you bind 
`s' to command emacspeak-emergency-tts-restart 
then that command will be available on key C-e x s

The value of this variable is an association list. The car
of each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To
enter a key with a modifier, type C-q followed by the
desired modified keystroke. For example, to enter C-s
\(Control s) as the key to be bound, type C-q C-s in the key
field in the customization buffer.  You can use the notation
[f1], [f2], etc., to specify function keys. ")

(defvar emacspeak-super-keys nil "\
*Specifies super key bindings for the audio desktop.
You can turn the right `windows menu' keys on your Linux PC keyboard into a `super' key
on Linux by having it emit the sequence `C-x@s'.

Bindings specified here are available on prefix key `super'
for example, if you bind 
`s' to command emacspeak-emergency-tts-restart 
then that command will be available on key `super  s'

The value of this variable is an association list. The car
of each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To
enter a key with a modifier, type C-q followed by the
desired modified keystroke. For example, to enter C-s
\(Control s) as the key to be bound, type C-q C-s in the key
field in the customization buffer.  You can use the notation
[f1], [f2], etc., to specify function keys. ")

(defvar emacspeak-alt-keys nil "\
*Specifies alt key bindings for the audio desktop.
You can turn the `Pause' key  on your Linux PC keyboard into a `alt' key
on Linux by having it emit the sequence `C-x@a'.

Bindings specified here are available on prefix key `alt'
\(not to be confused with alt==meta)
for example, if you bind 
`s' to command emacspeak-emergency-tts-restart 
then that command will be available on key `ALT  s'

The value of this variable is an association list. The car
of each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To
enter a key with a modifier, type C-q followed by the
desired modified keystroke. For example, to enter C-s
\(Control s) as the key to be bound, type C-q C-s in the key
field in the customization buffer.  You can use the notation
[f1], [f2], etc., to specify function keys. ")

(defvar emacspeak-hyper-keys nil "\
*Specifies hyper key bindings for the audio desktop.
Emacs can use the `hyper' key as a modifier key.
You can turn the `windows' keys on your Linux PC keyboard into a `hyper' key
on Linux by having it emit the sequence `C-x@h'.

Bindings specified here are available on prefix key  `hyper'
for example, if you bind 
`b' to command `bbdb '
then that command will be available on key `hyper b'.

The value of this variable is an association list. The car
of each element specifies a key sequence. The cdr specifies
an interactive command that the key sequence executes. To
enter a key with a modifier, type C-q followed by the
desired modified keystroke. For example, to enter C-s
\(Control s) as the key to be bound, type C-q C-s in the key
field in the customization buffer.  You can use the notation
[f1], [f2], etc., to specify function keys. ")

(autoload (quote emacspeak-keymap-choose-new-emacspeak-prefix) "emacspeak-keymap" "\
Interactively select a new prefix key to use for all emacspeak
commands.  The default is to use `C-e'  This command
lets you switch the prefix to something else.  This is a useful thing
to do if you run emacspeak on a remote machine from inside a terminal
that is running inside a local emacspeak session.  You can have the
remote emacspeak use a different control key to give your fingers some
relief." t nil)

(autoload (quote emacspeak-keymap-remove-emacspeak-edit-commands) "emacspeak-keymap" "\
We define keys that invoke editting commands to be undefined" nil nil)

;;;***

;;;### (autoloads (emacspeak-m-player emacspeak-multimedia) "emacspeak-m-player"
;;;;;;  "emacspeak-m-player.el" (17751 59928))
;;; Generated autoloads from emacspeak-m-player.el

(defgroup emacspeak-m-player nil "Emacspeak media player settings." :group (quote emacspeak))

(autoload (quote emacspeak-multimedia) "emacspeak-m-player" "\
Start or control Emacspeak multimedia player." t nil)

(autoload (quote emacspeak-m-player) "emacspeak-m-player" "\
Play specified resource using m-player.
Optional prefix argument play-list interprets resource as a play-list.
Resource is a media resource or playlist containing media resources.
The player is placed in a buffer in emacspeak-m-player-mode." t nil)

;;;***

;;;### (autoloads (emacspeak-madplay emacspeak-madplay-madplay-call-command
;;;;;;  emacspeak-madplay-madplay-command) "emacspeak-madplay" "emacspeak-madplay.el"
;;;;;;  (17751 59929))
;;; Generated autoloads from emacspeak-madplay.el

(define-prefix-command (quote emacspeak-madplay-prefix-command) (quote emacspeak-madplay-mode-map))

(autoload (quote emacspeak-madplay-madplay-command) "emacspeak-madplay" "\
Execute Madplay command." t nil)

(autoload (quote emacspeak-madplay-madplay-call-command) "emacspeak-madplay" "\
Call appropriate madplay command." t nil)

(autoload (quote emacspeak-madplay) "emacspeak-madplay" "\
Play specified resource using madplay.
Resource is an  MP3 file or directory containing mp3 files.
The player is placed in a buffer in emacspeak-madplay-mode." t nil)

;;;***

;;;### (autoloads (emacspeak-ocr-set-compress-image-options emacspeak-ocr-set-scan-image-options
;;;;;;  emacspeak-ocr-read-current-page emacspeak-ocr-page emacspeak-ocr-backward-page
;;;;;;  emacspeak-ocr-forward-page emacspeak-ocr-open-working-directory
;;;;;;  emacspeak-ocr-scan-and-recognize emacspeak-ocr-recognize-image
;;;;;;  emacspeak-ocr-save-current-page emacspeak-ocr-write-document
;;;;;;  emacspeak-ocr-scan-photo emacspeak-ocr-scan-image emacspeak-ocr-name-document
;;;;;;  emacspeak-ocr emacspeak-ocr-customize) "emacspeak-ocr" "emacspeak-ocr.el"
;;;;;;  (17751 59929))
;;; Generated autoloads from emacspeak-ocr.el

(autoload (quote emacspeak-ocr-customize) "emacspeak-ocr" "\
Customize OCR settings." t nil)

(autoload (quote emacspeak-ocr) "emacspeak-ocr" "\
An OCR front-end for the Emacspeak desktop.  

Page image is acquired using tools from the SANE package.
The acquired image is run through the OCR engine if one is
available, and the results placed in a buffer that is
suitable for browsing the results.

For detailed help, invoke command emacspeak-ocr bound to
\\[emacspeak-ocr] to launch emacspeak-ocr-mode, and press
`?' to display mode-specific help for emacspeak-ocr-mode." t nil)

(autoload (quote emacspeak-ocr-name-document) "emacspeak-ocr" "\
Name document being scanned in the current OCR buffer.
Pick a short but meaningful name." t nil)

(autoload (quote emacspeak-ocr-scan-image) "emacspeak-ocr" "\
Acquire page image." t nil)

(autoload (quote emacspeak-ocr-scan-photo) "emacspeak-ocr" "\
Scan in a photograph.
The scanned image is converted to JPEG." t nil)

(autoload (quote emacspeak-ocr-write-document) "emacspeak-ocr" "\
Writes out recognized text from all pages in current document." t nil)

(autoload (quote emacspeak-ocr-save-current-page) "emacspeak-ocr" "\
Writes out recognized text from current page
to an appropriately named file." t nil)

(autoload (quote emacspeak-ocr-recognize-image) "emacspeak-ocr" "\
Run OCR engine on current image.
Prompts for image file if file corresponding to the expected
`current page' is not found." t nil)

(autoload (quote emacspeak-ocr-scan-and-recognize) "emacspeak-ocr" "\
Scan in a page and run OCR engine on it.
Use this command once you've verified that the separate
steps of acquiring an image and running the OCR engine work
correctly by themselves." t nil)

(autoload (quote emacspeak-ocr-open-working-directory) "emacspeak-ocr" "\
Launch dired on OCR working directory." t nil)

(autoload (quote emacspeak-ocr-forward-page) "emacspeak-ocr" "\
Like forward page, but tracks page number of current document." t nil)

(autoload (quote emacspeak-ocr-backward-page) "emacspeak-ocr" "\
Like backward page, but tracks page number of current document." t nil)

(autoload (quote emacspeak-ocr-page) "emacspeak-ocr" "\
Move to specified page." t nil)

(autoload (quote emacspeak-ocr-read-current-page) "emacspeak-ocr" "\
Speaks current page." t nil)

(autoload (quote emacspeak-ocr-set-scan-image-options) "emacspeak-ocr" "\
Interactively update scan image options.
Prompts with current setting in the minibuffer.
Setting persists for current Emacs session." t nil)

(autoload (quote emacspeak-ocr-set-compress-image-options) "emacspeak-ocr" "\
Interactively update  image compression options.
Prompts with current setting in the minibuffer.
Setting persists for current Emacs session." t nil)

;;;***

;;;### (autoloads (emacspeak-org-popup-input) "emacspeak-org" "emacspeak-org.el"
;;;;;;  (17751 59929))
;;; Generated autoloads from emacspeak-org.el

(autoload (quote emacspeak-org-popup-input) "emacspeak-org" "\
Pops up an org input area." t nil)

;;;***

;;;### (autoloads (emacspeak-personality-prepend emacspeak-personality-append)
;;;;;;  "emacspeak-personality" "emacspeak-personality.el" (17751
;;;;;;  59958))
;;; Generated autoloads from emacspeak-personality.el

(defsubst emacspeak-personality-put (start end personality object) "\
Apply personality to specified region, over-writing any current
personality settings." (when (and (integer-or-marker-p start) (integer-or-marker-p end) (not (= start end))) (let ((v (if (listp personality) (remove-duplicates personality :test (function eq)) personality))) (ems-modify-buffer-safely (put-text-property start end (quote personality) v object)))))

(autoload (quote emacspeak-personality-append) "emacspeak-personality" "\
Append specified personality to text bounded by start and end.
Existing personality properties on the text range are preserved." nil nil)

(autoload (quote emacspeak-personality-prepend) "emacspeak-personality" "\
Prepend specified personality to text bounded by start and end.
Existing personality properties on the text range are preserved." nil nil)

;;;***

;;;### (autoloads (emacspeak-pronounce-dispatch emacspeak-pronounce-edit-pronunciations
;;;;;;  emacspeak-pronounce-refresh-pronunciations emacspeak-pronounce-toggle-use-of-dictionaries
;;;;;;  emacspeak-pronounce-define-pronunciation emacspeak-pronounce-define-template-pronunciation
;;;;;;  emacspeak-pronounce-define-local-pronunciation emacspeak-pronounce-clear-dictionaries
;;;;;;  emacspeak-pronounce-load-dictionaries emacspeak-pronounce-save-dictionaries)
;;;;;;  "emacspeak-pronounce" "emacspeak-pronounce.el" (17753 10048))
;;; Generated autoloads from emacspeak-pronounce.el

(autoload (quote emacspeak-pronounce-save-dictionaries) "emacspeak-pronounce" "\
Writes out the persistent emacspeak pronunciation dictionaries." t nil)

(autoload (quote emacspeak-pronounce-load-dictionaries) "emacspeak-pronounce" "\
Load pronunciation dictionaries.
Optional argument FILENAME specifies the dictionary file." t nil)

(autoload (quote emacspeak-pronounce-clear-dictionaries) "emacspeak-pronounce" "\
Clear all current pronunciation dictionaries." t nil)

(defsubst emacspeak-pronounce-yank-word nil "\
Yank word at point into minibuffer." (interactive) (declare (special emacspeak-pronounce-yank-word-point emacspeak-pronounce-current-buffer)) (let ((string (save-excursion (set-buffer emacspeak-pronounce-current-buffer) (goto-char emacspeak-pronounce-yank-word-point) (buffer-substring-no-properties (point) (save-excursion (forward-word 1) (setq emacspeak-pronounce-yank-word-point (point))))))) (insert string) (dtk-speak string)))

(autoload (quote emacspeak-pronounce-define-local-pronunciation) "emacspeak-pronounce" "\
Define buffer local pronunciation.
Argument WORD specifies the word which should be pronounced as specified by PRONUNCIATION." t nil)

(autoload (quote emacspeak-pronounce-define-template-pronunciation) "emacspeak-pronounce" "\
Interactively define template entries in the pronunciation dictionaries.
Default term to define is delimited by region.
First loads any persistent dictionaries if not already loaded." t nil)

(autoload (quote emacspeak-pronounce-define-pronunciation) "emacspeak-pronounce" "\
Interactively define entries in the pronunciation dictionaries.
Default term to define is delimited by region.
First loads any persistent dictionaries if not already loaded." t nil)

(autoload (quote emacspeak-pronounce-toggle-use-of-dictionaries) "emacspeak-pronounce" "\
Toggle use of pronunciation dictionaries in current buffer.
Pronunciations can be defined on a per file, per directory and/or per
mode basis.
Pronunciations are activated on a per buffer basis.
Turning on the use of pronunciation dictionaries results in emacspeak
composing a pronunciation table based on the currently defined
pronunciation dictionaries.
After this, the pronunciations will be applied whenever text in the
buffer is spoken.
Optional argument state can be used from Lisp programs to
explicitly turn pronunciations on or off." t nil)

(autoload (quote emacspeak-pronounce-refresh-pronunciations) "emacspeak-pronounce" "\
Refresh pronunciation table for current buffer.
Activates pronunciation dictionaries if not already active." t nil)

(autoload (quote emacspeak-pronounce-edit-pronunciations) "emacspeak-pronounce" "\
Prompt for and launch a pronunciation editor on the
specified pronunciation dictionary key." t nil)

(autoload (quote emacspeak-pronounce-dispatch) "emacspeak-pronounce" "\
Provides the user interface front-end to Emacspeak's pronunciation dictionaries." t nil)

;;;***

;;;### (autoloads (emacspeak-realaudio-browse emacspeak-realaudio
;;;;;;  emacspeak-realaudio-write-mp3-clip emacspeak-realaudio-mp3-clipper
;;;;;;  emacspeak-realaudio-set-end-mark emacspeak-realaudio-set-start-mark
;;;;;;  emacspeak-realaudio-get-current-time-in-seconds emacspeak-realaudio-process-sentinel
;;;;;;  emacspeak-realaudio-play) "emacspeak-realaudio" "emacspeak-realaudio.el"
;;;;;;  (17751 59930))
;;; Generated autoloads from emacspeak-realaudio.el

(defvar emacspeak-realaudio-history nil "\
History list holding resources we played recently")

(defvar emacspeak-realaudio-shortcuts-directory (expand-file-name "realaudio/" emacspeak-directory) "\
*Directory where we keep realaudio shortcuts.
I typically keep .ram --RealAudio metafiles-- in this
directory.
Realaudio metafiles typically contain a single line that
specifies the actual location of the realaudio stream
--typically the .ra file.")

(autoload (quote emacspeak-realaudio-play) "emacspeak-realaudio" "\
Play a realaudio stream.  Uses files from your Realaudio
shortcuts directory for completion.  See documentation for
user configurable variable emacspeak-realaudio-shortcuts-directory. " t nil)

(autoload (quote emacspeak-realaudio-process-sentinel) "emacspeak-realaudio" "\
Cleanup after realaudio is done. " nil nil)

(autoload (quote emacspeak-realaudio-get-current-time-in-seconds) "emacspeak-realaudio" "\
Return current time in seconds." t nil)

(autoload (quote emacspeak-realaudio-set-start-mark) "emacspeak-realaudio" "\
Set start mark. Default is to set marker to current play time.
Mark is specified in seconds." t nil)

(autoload (quote emacspeak-realaudio-set-end-mark) "emacspeak-realaudio" "\
Set end mark. Default is to set marker to current play time.
Mark is specified in seconds." t nil)

(defvar emacspeak-realaudio-mp3-clipper "/usr/local/bin/qmp3cut" "\
Executable used to clip MP3 files.")

(autoload (quote emacspeak-realaudio-write-mp3-clip) "emacspeak-realaudio" "\
Writes specified clip from current mp3 stream.
Prompts for start and end times as well as file  to save the clippi" t nil)

(autoload (quote emacspeak-realaudio) "emacspeak-realaudio" "\
Start or control streaming audio including MP3 and
realaudio.  If using `TRPlayer' as the player, accepts
trplayer control commands if a stream is already playing.
Otherwise, the playing stream is simply stopped.  If no
stream is playing, this command prompts for a realaudio
resource.  Realaudio resources can be specified either as a
Realaudio URL, the location of a local Realaudio file, or as
the name of a local Realaudio metafile. Realaudio resources
you have played in this session are available in the
minibuffer history.  The default is to play the resource you
played most recently. Emacspeak uses the contents of the
directory specified by variable
emacspeak-realaudio-shortcuts-directory to offer a set of
completions. Hit space to use this completion list.

If using TRPlayer, you can either give one-shot commands
using command emacspeak-realaudio available from anywhere on
the audio desktop as `\\[emacspeak-realaudio]'.
Alternatively, switch to buffer *realaudio* using
`\\[emacspeak-realaudio];' if you wish to issue many
navigation commands.  Note that buffer *realaudio* uses a
special major mode that provides the various navigation
commands via single keystrokes." t nil)

(autoload (quote emacspeak-realaudio-browse) "emacspeak-realaudio" "\
Browse RAM file before playing the selected component." t nil)

(defvar emacspeak-realaudio-trplayer-keys (list 112 116 115 101 108 105 60 62 46 44 48 57 91 93 123 125) "\
Keys accepted by TRPlayer.")

;;;***

;;;### (autoloads (emacspeak-remote-connect-to-server emacspeak-remote-ssh-to-server
;;;;;;  emacspeak-remote-quick-connect-to-server emacspeak-remote-use-ssh)
;;;;;;  "emacspeak-remote" "emacspeak-remote.el" (17751 59930))
;;; Generated autoloads from emacspeak-remote.el

(defgroup emacspeak-remote nil "Emacspeak remote group." :group (quote emacspeak-remote))

(defvar emacspeak-remote-use-ssh nil "\
Set to T to use SSH remote servers.")

(autoload (quote emacspeak-remote-quick-connect-to-server) "emacspeak-remote" "\
Connect to remote server.
Does not prompt for host or port, but quietly uses the
guesses that appear as defaults when prompting.
Use this once you are sure the guesses are usually correct." t nil)

(autoload (quote emacspeak-remote-ssh-to-server) "emacspeak-remote" "\
Open ssh session to where we came from." t nil)

(autoload (quote emacspeak-remote-connect-to-server) "emacspeak-remote" "\
Connect to and start using remote speech server running on host host
and listening on port port.  Host is the hostname of the remote
server, typically the desktop machine.  Port is the tcp port that that
host is listening on for speech requests." t nil)

;;;***

;;;### (autoloads (emacspeak-rss-browse emacspeak-opml-display emacspeak-rss-display)
;;;;;;  "emacspeak-rss" "emacspeak-rss.el" (17751 59930))
;;; Generated autoloads from emacspeak-rss.el

(defgroup emacspeak-rss nil "RSS Feeds for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-rss-display) "emacspeak-rss" "\
Retrieve and display RSS URL." t nil)

(autoload (quote emacspeak-opml-display) "emacspeak-rss" "\
Retrieve and display OPML  URL." t nil)

(autoload (quote emacspeak-rss-browse) "emacspeak-rss" "\
Browse specified RSS feed." t nil)

;;;***

;;;### (autoloads nil "emacspeak-setup" "emacspeak-setup.el" (17765
;;;;;;  52417))
;;; Generated autoloads from emacspeak-setup.el

(defvar emacspeak-directory (expand-file-name "../" (file-name-directory load-file-name)) "\
Directory where emacspeak is installed. ")

(defvar emacspeak-lisp-directory (expand-file-name "lisp/" emacspeak-directory) "\
Directory where emacspeak lisp files are  installed. ")

(defvar emacspeak-sounds-directory (expand-file-name "sounds/" emacspeak-directory) "\
Directory containing auditory icons for Emacspeak.")

(defvar emacspeak-etc-directory (expand-file-name "etc/" emacspeak-directory) "\
Directory containing miscellaneous files  for
  Emacspeak.")

(defvar emacspeak-servers-directory (expand-file-name "servers/" emacspeak-directory) "\
Directory containing speech servers  for
  Emacspeak.")

(defvar emacspeak-info-directory (expand-file-name "info/" emacspeak-directory) "\
Directory containing  Emacspeak info files.")

(defvar emacspeak-resource-directory (expand-file-name "~/.emacspeak/") "\
Directory where Emacspeak resource files such as
pronunciation dictionaries are stored. ")

(defvar emacspeak-readme-file (expand-file-name "README" emacspeak-directory) "\
README file from where we get SVN revision number.")

(defconst emacspeak-version (format "25.0 %s" (cond ((file-exists-p emacspeak-readme-file) (let ((buffer (find-file-noselect emacspeak-readme-file)) (revision nil)) (save-excursion (set-buffer buffer) (goto-char (point-min)) (setq revision (format "Revision %s" (nth 2 (split-string (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))) (kill-buffer buffer) revision)) (t ""))) "\
Version number for Emacspeak.")

(defconst emacspeak-codename "ActiveDog" "\
Code name of present release.")

;;;***

;;;### (autoloads (emacspeak-sounds-reset-sound emacspeak-set-auditory-icon-player
;;;;;;  emacspeak-toggle-auditory-icons emacspeak-auditory-icon emacspeak-play-midi-icon
;;;;;;  emacspeak-serve-auditory-icon emacspeak-native-auditory-icon
;;;;;;  emacspeak-queue-auditory-icon emacspeak-sounds-select-theme
;;;;;;  emacspeak-play-program emacspeak-sounds-default-theme emacspeak-sounds-define-theme)
;;;;;;  "emacspeak-sounds" "emacspeak-sounds.el" (17751 59931))
;;; Generated autoloads from emacspeak-sounds.el

(defsubst emacspeak-using-midi-p nil "\
Predicate to test if we are using midi." (declare (special emacspeak-auditory-icon-function)) (or (eq emacspeak-auditory-icon-function (quote emacspeak-play-midi-icon)) (eq emacspeak-auditory-icon-function (quote emacspeak-queue-midi-icon))))

(autoload (quote emacspeak-sounds-define-theme) "emacspeak-sounds" "\
Define a sounds theme for auditory icons. " nil nil)

(defvar emacspeak-sounds-default-theme (expand-file-name "default-8k/" emacspeak-sounds-directory) "\
Default theme for auditory icons. ")

(defvar emacspeak-play-program (cond ((getenv "EMACSPEAK_PLAY_PROGRAM") (getenv "EMACSPEAK_PLAY_PROGRAM")) ((file-exists-p "/usr/bin/aplay") "/usr/bin/aplay") ((file-exists-p "/usr/bin/play") "/usr/bin/play") ((file-exists-p "/usr/bin/audioplay") "/usr/bin/audioplay") ((file-exists-p "/usr/demo/SOUND/play") "/usr/demo/SOUND/play") (t (expand-file-name emacspeak-etc-directory "play"))) "\
Name of executable that plays sound files. ")

(autoload (quote emacspeak-sounds-select-theme) "emacspeak-sounds" "\
Select theme for auditory icons." t nil)

(defsubst emacspeak-get-sound-filename (sound-name) "\
Retrieve name of sound file that produces  auditory icon SOUND-NAME." (declare (special emacspeak-sounds-themes-table emacspeak-sounds-current-theme)) (let ((f (expand-file-name (format "%s%s" sound-name (emacspeak-sounds-theme-get-extension emacspeak-sounds-current-theme)) emacspeak-sounds-current-theme))) (if (file-exists-p f) f emacspeak-default-sound)))

(autoload (quote emacspeak-queue-auditory-icon) "emacspeak-sounds" "\
Queue auditory icon SOUND-NAME." nil nil)

(autoload (quote emacspeak-native-auditory-icon) "emacspeak-sounds" "\
Play auditory icon using native Emacs player." nil nil)

(autoload (quote emacspeak-serve-auditory-icon) "emacspeak-sounds" "\
Serve auditory icon SOUND-NAME.
Sound is served only if `emacspeak-use-auditory-icons' is true.
See command `emacspeak-toggle-auditory-icons' bound to \\[emacspeak-toggle-auditory-icons ]." nil nil)

(autoload (quote emacspeak-play-midi-icon) "emacspeak-sounds" "\
Play midi icon midi-NAME." nil nil)

(autoload (quote emacspeak-auditory-icon) "emacspeak-sounds" "\
Play an auditory ICON." nil nil)

(autoload (quote emacspeak-toggle-auditory-icons) "emacspeak-sounds" "\
Toggle use of auditory icons.
Optional interactive PREFIX arg toggles global value." t nil)

(autoload (quote emacspeak-set-auditory-icon-player) "emacspeak-sounds" "\
Select  player used for producing auditory icons.
Recommended choices:

emacspeak-serve-auditory-icon for  the wave device.
emacspeak-queue-auditory-icon when using software TTS.
emacspeak-play-midi-icon for midi device. " t nil)

(autoload (quote emacspeak-sounds-reset-sound) "emacspeak-sounds" "\
Reload sound drivers." t nil)

;;;***

;;;### (autoloads (emacspeak-speak-load-directory-settings emacspeak-speak-message-at-time
;;;;;;  emacspeak-speak-and-skip-extent-upto-this-char emacspeak-speak-and-skip-extent-upto-char
;;;;;;  emacspeak-mark-backward-mark emacspeak-mark-forward-mark
;;;;;;  emacspeak-completions-move-to-completion-group emacspeak-switch-to-completions-window
;;;;;;  emacspeak-speak-spaces-at-point emacspeak-use-customized-blink-paren
;;;;;;  emacspeak-blink-matching-open emacspeak-voicify-region emacspeak-voicify-rectangle
;;;;;;  emacspeak-speak-rectangle emacspeak-view-register emacspeak-speak-sexp-interactively
;;;;;;  emacspeak-speak-word-interactively emacspeak-speak-page-interactively
;;;;;;  emacspeak-speak-paragraph-interactively emacspeak-speak-line-interactively
;;;;;;  emacspeak-speak-help-interactively emacspeak-speak-buffer-interactively
;;;;;;  emacspeak-speak-predefined-window emacspeak-owindow-speak-line
;;;;;;  emacspeak-owindow-previous-line emacspeak-owindow-next-line
;;;;;;  emacspeak-owindow-scroll-down emacspeak-owindow-scroll-up
;;;;;;  emacspeak-speak-previous-window emacspeak-speak-next-window
;;;;;;  emacspeak-speak-other-window emacspeak-speak-current-window
;;;;;;  emacspeak-speak-window-information emacspeak-speak-message-again
;;;;;;  emacspeak-speak-current-percentage emacspeak-speak-current-column
;;;;;;  emacspeak-speak-previous-field emacspeak-speak-next-field
;;;;;;  emacspeak-speak-comint-send-input emacspeak-speak-skim-buffer
;;;;;;  emacspeak-speak-skim-next-paragraph emacspeak-speak-skim-paragraph
;;;;;;  emacspeak-speak-browse-buffer emacspeak-speak-continuously
;;;;;;  emacspeak-execute-repeatedly emacspeak-speak-previous-personality-chunk
;;;;;;  emacspeak-speak-next-personality-chunk emacspeak-speak-this-personality-chunk
;;;;;;  emacspeak-speak-current-mark emacspeak-dial-dtk emacspeak-zap-tts
;;;;;;  emacspeak-speak-current-kill emacspeak-speak-version emacspeak-speak-time
;;;;;;  emacspeak-speak-world-clock emacspeak-read-next-word emacspeak-read-previous-word
;;;;;;  emacspeak-read-next-line emacspeak-read-previous-line emacspeak-speak-buffer-filename
;;;;;;  emacspeak-speak-minor-mode-line emacspeak-speak-mode-line
;;;;;;  emacspeak-speak-minibuffer emacspeak-speak-completions emacspeak-speak-help
;;;;;;  emacspeak-speak-rest-of-buffer emacspeak-speak-front-of-buffer
;;;;;;  emacspeak-speak-other-buffer emacspeak-speak-buffer emacspeak-speak-paragraph
;;;;;;  emacspeak-speak-page emacspeak-speak-sexp emacspeak-speak-sentence
;;;;;;  emacspeak-speak-set-display-table emacspeak-speak-display-char
;;;;;;  emacspeak-speak-char emacspeak-speak-word emacspeak-speak-spell-current-word
;;;;;;  emacspeak-audio-annotate-paragraphs) "emacspeak-speak" "emacspeak-speak.el"
;;;;;;  (17758 21298))
;;; Generated autoloads from emacspeak-speak.el

(autoload (quote emacspeak-audio-annotate-paragraphs) "emacspeak-speak" "\
Set property auditory-icon at front of all paragraphs." t nil)

(defsubst emacspeak-speak-voice-annotate-paragraphs nil "\
Locate paragraphs and voice annotate the first word.
Here, paragraph is taken to mean a chunk of text preceded by a blank line.
Useful to do this before you listen to an entire buffer." (interactive) (declare (special emacspeak-speak-paragraph-personality emacspeak-speak-voice-annotated-paragraphs)) (when emacspeak-speak-paragraph-personality (save-excursion (goto-char (point-min)) (condition-case nil (let ((start nil) (blank-line "
[ 	
]*
") (inhibit-point-motion-hooks t)) (ems-modify-buffer-safely (while (re-search-forward blank-line nil t) (skip-syntax-forward " ") (setq start (point)) (unless (get-text-property start (quote personality)) (skip-syntax-forward "^ ") (put-text-property start (point) (quote personality) emacspeak-speak-paragraph-personality))))) (error nil)) (setq emacspeak-speak-voice-annotated-paragraphs t))))

(defsubst emacspeak-speak-region (start end) "\
Speak region.
Argument START  and END specify region to speak." (interactive "r") (declare (special emacspeak-speak-voice-annotated-paragraphs inhibit-point-motion-hooks voice-lock-mode)) (let ((inhibit-point-motion-hooks t)) (when (and voice-lock-mode (not emacspeak-speak-voice-annotated-paragraphs)) (save-restriction (narrow-to-region start end) (emacspeak-speak-voice-annotate-paragraphs))) (emacspeak-handle-action-at-point) (dtk-speak (buffer-substring start end))))
                         ;

(autoload (quote emacspeak-speak-spell-current-word) "emacspeak-speak" "\
Spell word at  point." t nil)

(autoload (quote emacspeak-speak-word) "emacspeak-speak" "\
Speak current word.
With prefix ARG, speaks the rest of the word from point.
Negative prefix arg speaks from start of word to point.
If executed  on the same buffer position a second time, the word is
spelt instead of being spoken." t nil)

(autoload (quote emacspeak-speak-char) "emacspeak-speak" "\
Speak character under point.
Pronounces character phonetically unless  called with a PREFIX arg." t nil)

(autoload (quote emacspeak-speak-display-char) "emacspeak-speak" "\
Display char under point using current speech display table.
Behavior is the same as command `emacspeak-speak-char'
bound to \\[emacspeak-speak-char]
for characters in the range 0--127.
Optional argument PREFIX  specifies that the character should be spoken phonetically." t nil)

(autoload (quote emacspeak-speak-set-display-table) "emacspeak-speak" "\
Sets up buffer specific speech display table that controls how
special characters are spoken. Interactive prefix argument causes
setting to be global." t nil)

(autoload (quote emacspeak-speak-sentence) "emacspeak-speak" "\
Speak current sentence.
With prefix ARG, speaks the rest of the sentence  from point.
Negative prefix arg speaks from start of sentence to point." t nil)

(autoload (quote emacspeak-speak-sexp) "emacspeak-speak" "\
Speak current sexp.
With prefix ARG, speaks the rest of the sexp  from point.
Negative prefix arg speaks from start of sexp to point.
If option  `voice-lock-mode' is on, then uses the personality." t nil)

(autoload (quote emacspeak-speak-page) "emacspeak-speak" "\
Speak a page.
With prefix ARG, speaks rest of current page.
Negative prefix arg will read from start of current page to point.
If option  `voice-lock-mode' is on, then it will use any defined personality." t nil)

(autoload (quote emacspeak-speak-paragraph) "emacspeak-speak" "\
Speak paragraph.
With prefix arg, speaks rest of current paragraph.
Negative prefix arg will read from start of current paragraph to point.
If voice-lock-mode is on, then it will use any defined personality. " t nil)

(autoload (quote emacspeak-speak-buffer) "emacspeak-speak" "\
Speak current buffer  contents.
With prefix ARG, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point.
 If voice lock mode is on, the paragraphs in the buffer are
voice annotated first,  see command `emacspeak-speak-voice-annotate-paragraphs'." t nil)

(autoload (quote emacspeak-speak-other-buffer) "emacspeak-speak" "\
Speak specified buffer.
Useful to listen to a buffer without switching  contexts." t nil)

(autoload (quote emacspeak-speak-front-of-buffer) "emacspeak-speak" "\
Speak   the buffer from start to   point" t nil)

(autoload (quote emacspeak-speak-rest-of-buffer) "emacspeak-speak" "\
Speak remainder of the buffer starting at point" t nil)

(autoload (quote emacspeak-speak-help) "emacspeak-speak" "\
Speak help buffer if one present.
With prefix arg, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point." t nil)

(autoload (quote emacspeak-speak-completions) "emacspeak-speak" "\
Speak completions  buffer if one present." t nil)

(autoload (quote emacspeak-speak-minibuffer) "emacspeak-speak" "\
Speak the minibuffer contents
 With prefix arg, speaks the rest of the buffer from point.
Negative prefix arg speaks from start of buffer to point." t nil)

(unless (fboundp (quote next-completion)) (progn (defun next-completion (n) "Move to the next item in the completion list.\nWIth prefix argument N, move N items (negative N means move backward)." (interactive "p") (while (and (> n 0) (not (eobp))) (let ((prop (get-text-property (point) (quote mouse-face))) (end (point-max))) (if prop (goto-char (next-single-property-change (point) (quote mouse-face) nil end))) (goto-char (next-single-property-change (point) (quote mouse-face) nil end))) (setq n (1- n)))) (defun previous-completion (n) "Move to the previous item in the completion list." (interactive "p") (setq n (- n)) (while (and (< n 0) (not (bobp))) (let ((prop (get-text-property (1- (point)) (quote mouse-face))) (end (point-min))) (if prop (goto-char (previous-single-property-change (point) (quote mouse-face) nil end))) (goto-char (previous-single-property-change (point) (quote mouse-face) nil end)) (goto-char (previous-single-property-change (point) (quote mouse-face) nil end))) (setq n (1+ n)))) (declaim (special completion-list-mode-map)) (or completion-list-mode-map (make-sparse-keymap)) (define-key completion-list-mode-map (quote [right]) (quote next-completion)) (define-key completion-list-mode-map (quote [left]) (quote previous-completion))))

(autoload (quote emacspeak-speak-mode-line) "emacspeak-speak" "\
Speak the mode-line.
Interactive prefix arg speaks buffer info." t nil)

(autoload (quote emacspeak-speak-minor-mode-line) "emacspeak-speak" "\
Speak the minor mode-information." t nil)

(autoload (quote emacspeak-speak-buffer-filename) "emacspeak-speak" "\
Speak name of file being visited in current buffer.
Speak default directory if invoked in a dired buffer,
or when the buffer is not visiting any file.
Interactive prefix arg `filename' speaks only the final path
component.
The result is put in the kill ring for convenience." t nil)

(autoload (quote emacspeak-read-previous-line) "emacspeak-speak" "\
Read previous line, specified by an offset, without moving.
Default is to read the previous line. " t nil)

(autoload (quote emacspeak-read-next-line) "emacspeak-speak" "\
Read next line, specified by an offset, without moving.
Default is to read the next line. " t nil)

(autoload (quote emacspeak-read-previous-word) "emacspeak-speak" "\
Read previous word, specified as a prefix arg, without moving.
Default is to read the previous word. " t nil)

(autoload (quote emacspeak-read-next-word) "emacspeak-speak" "\
Read next word, specified as a numeric  arg, without moving.
Default is to read the next word. " t nil)

(autoload (quote emacspeak-speak-world-clock) "emacspeak-speak" "\
Display current date and time  for specified zone.
Optional second arg `set' sets the TZ environment variable as well." t nil)

(autoload (quote emacspeak-speak-time) "emacspeak-speak" "\
Speak the time.
Optional interactive prefix arg `C-u'invokes world clock.
Timezone is specified using minibuffer completion.
Second interactive prefix sets clock to new timezone." t nil)

(autoload (quote emacspeak-speak-version) "emacspeak-speak" "\
Announce version information for running emacspeak." t nil)

(autoload (quote emacspeak-speak-current-kill) "emacspeak-speak" "\
Speak the current kill entry.
This is the text that will be yanked in by the next \\[yank].
Prefix numeric arg, COUNT, specifies that the text that will be yanked as a
result of a
\\[yank]  followed by count-1 \\[yank-pop]
be spoken.
 The kill number that is spoken says what numeric prefix arg to give
to command yank." t nil)

(autoload (quote emacspeak-zap-tts) "emacspeak-speak" "\
Send this command to the TTS directly." t nil)

(autoload (quote emacspeak-dial-dtk) "emacspeak-speak" "\
Prompt for and dial a phone NUMBER with the Dectalk." t nil)

(autoload (quote emacspeak-speak-current-mark) "emacspeak-speak" "\
Speak the line containing the mark.
With no argument, speaks the
line containing the mark--this is where `exchange-point-and-mark'
\\[exchange-point-and-mark] would jump.  Numeric prefix arg 'COUNT' speaks
line containing mark 'n' where 'n' is one less than the number of
times one has to jump using `set-mark-command' to get to this marked
position.  The location of the mark is indicated by an aural highlight
achieved by a change in voice personality." t nil)

(autoload (quote emacspeak-speak-this-personality-chunk) "emacspeak-speak" "\
Speak chunk of text around point that has current
personality." t nil)

(autoload (quote emacspeak-speak-next-personality-chunk) "emacspeak-speak" "\
Moves to the front of next chunk having current personality.
Speak that chunk after moving." t nil)

(autoload (quote emacspeak-speak-previous-personality-chunk) "emacspeak-speak" "\
Moves to the front of previous chunk having current personality.
Speak that chunk after moving." t nil)

(autoload (quote emacspeak-execute-repeatedly) "emacspeak-speak" "\
Execute COMMAND repeatedly." t nil)

(autoload (quote emacspeak-speak-continuously) "emacspeak-speak" "\
Speak a buffer continuously.
First prompts using the minibuffer for the kind of action to
perform after speaking each chunk.  E.G.  speak a line at a time
etc.  Speaking commences at current buffer position.  Pressing
\\[keyboard-quit] breaks out, leaving point on last chunk that
was spoken.  Any other key continues to speak the buffer." t nil)

(autoload (quote emacspeak-speak-browse-buffer) "emacspeak-speak" "\
Browse current buffer.
Default is to speak chunk having current personality.
Interactive prefix arg `browse'  repeatedly browses  through
  chunks having same personality as the current text chunk." t nil)

(autoload (quote emacspeak-speak-skim-paragraph) "emacspeak-speak" "\
Skim paragraph.
Skimming a paragraph results in the speech speeding up after
the first clause.
Speech is scaled by the value of dtk-speak-skim-scale" t nil)

(autoload (quote emacspeak-speak-skim-next-paragraph) "emacspeak-speak" "\
Skim next paragraph." t nil)

(autoload (quote emacspeak-speak-skim-buffer) "emacspeak-speak" "\
Skim the current buffer  a paragraph at a time." t nil)

(ems-generate-switcher (quote emacspeak-toggle-comint-output-monitor) (quote emacspeak-comint-output-monitor) "Toggle state of Emacspeak comint monitor.\nWhen turned on, comint output is automatically spoken.  Turn this on if\nyou want your shell to speak its results.  Interactive\nPREFIX arg means toggle the global default value, and then\nset the current local value to the result.")

(autoload (quote emacspeak-speak-comint-send-input) "emacspeak-speak" "\
Causes output to be spoken i.e., as if comint autospeak were turned
on." t nil)

(cond ((fboundp (quote field-beginning)) (defun emacspeak-speak-current-field nil "Speak current field.\nA field is\ndefined  by Emacs 21." (interactive) (emacspeak-speak-region (field-beginning) (field-end)))) (t (defun emacspeak-speak-current-field nil "Speak current field.\nA field is defined currently as a sequence of non-white space characters.  may be made\n  mode specific later." (interactive) (cond ((window-minibuffer-p (selected-window)) (emacspeak-speak-line)) (t (let ((start nil)) (save-excursion (skip-syntax-backward "^ ") (setq start (point)) (skip-syntax-forward "^ ") (emacspeak-speak-field start (point)))))))))

(autoload (quote emacspeak-speak-next-field) "emacspeak-speak" "\
Skip across and speak the next contiguous sequence of non-blank characters.
Useful in moving across fields.
Will be improved if it proves useful." t nil)

(autoload (quote emacspeak-speak-previous-field) "emacspeak-speak" "\
Skip backwards across and speak  contiguous sequence of non-blank characters.
Useful in moving across fields.
Will be improved if it proves useful." t nil)

(autoload (quote emacspeak-speak-current-column) "emacspeak-speak" "\
Speak the current column." t nil)

(autoload (quote emacspeak-speak-current-percentage) "emacspeak-speak" "\
Announce the percentage into the current buffer." t nil)

(autoload (quote emacspeak-speak-message-again) "emacspeak-speak" "\
Speak the last message from Emacs once again.
Optional interactive prefix arg
`from-message-cache' speaks message cached from the most
recent call to function `message'.
The message is also placed in the kill ring for convenient yanking
if `emacspeak-speak-message-again-should-copy-to-kill-ring' is set." t nil)

(autoload (quote emacspeak-speak-window-information) "emacspeak-speak" "\
Speaks information about current window." t nil)

(autoload (quote emacspeak-speak-current-window) "emacspeak-speak" "\
Speak contents of current window.
Speaks entire window irrespective of point." t nil)

(autoload (quote emacspeak-speak-other-window) "emacspeak-speak" "\
Speak contents of `other' window.
Speaks entire window irrespective of point.
Semantics  of `other' is the same as for the builtin Emacs command
`other-window'.
Optional argument ARG  specifies `other' window to speak." t nil)

(autoload (quote emacspeak-speak-next-window) "emacspeak-speak" "\
Speak the next window." t nil)

(autoload (quote emacspeak-speak-previous-window) "emacspeak-speak" "\
Speak the previous window." t nil)

(autoload (quote emacspeak-owindow-scroll-up) "emacspeak-speak" "\
Scroll up the window that command `other-window' would move to.
Speak the window contents after scrolling." t nil)

(autoload (quote emacspeak-owindow-scroll-down) "emacspeak-speak" "\
Scroll down  the window that command `other-window' would move to.
Speak the window contents after scrolling." t nil)

(autoload (quote emacspeak-owindow-next-line) "emacspeak-speak" "\
Move to the next line in the other window and speak it.
Numeric prefix arg COUNT can specify number of lines to move." t nil)

(autoload (quote emacspeak-owindow-previous-line) "emacspeak-speak" "\
Move to the next line in the other window and speak it.
Numeric prefix arg COUNT specifies number of lines to move." t nil)

(autoload (quote emacspeak-owindow-speak-line) "emacspeak-speak" "\
Speak the current line in the other window." t nil)

(autoload (quote emacspeak-speak-predefined-window) "emacspeak-speak" "\
Speak one of the first 10 windows on the screen.
Speaks entire window irrespective of point.
In general, you'll never have Emacs split the screen into more than
two or three.
Argument ARG determines the 'other' window to speak.
Semantics  of `other' is the same as for the builtin Emacs command
`other-window'." t nil)

(autoload (quote emacspeak-speak-buffer-interactively) "emacspeak-speak" "\
Speak the start of, rest of, or the entire buffer.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire buffer." t nil)

(autoload (quote emacspeak-speak-help-interactively) "emacspeak-speak" "\
Speak the start of, rest of, or the entire help.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire help." t nil)

(autoload (quote emacspeak-speak-line-interactively) "emacspeak-speak" "\
Speak the start of, rest of, or the entire line.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire line." t nil)

(autoload (quote emacspeak-speak-paragraph-interactively) "emacspeak-speak" "\
Speak the start of, rest of, or the entire paragraph.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire paragraph." t nil)

(autoload (quote emacspeak-speak-page-interactively) "emacspeak-speak" "\
Speak the start of, rest of, or the entire page.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire page." t nil)

(autoload (quote emacspeak-speak-word-interactively) "emacspeak-speak" "\
Speak the start of, rest of, or the entire word.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire word." t nil)

(autoload (quote emacspeak-speak-sexp-interactively) "emacspeak-speak" "\
Speak the start of, rest of, or the entire sexp.
's' to speak the start.
'r' to speak the rest.
any other key to speak entire sexp." t nil)

(autoload (quote emacspeak-view-register) "emacspeak-speak" "\
Display the contents of a register, and then speak it." t nil)

(autoload (quote emacspeak-speak-rectangle) "emacspeak-speak" "\
Speak a rectangle of text.
Rectangle is delimited by point and mark.
When call from a program,
arguments specify the START and END of the rectangle." t nil)

(autoload (quote emacspeak-voicify-rectangle) "emacspeak-speak" "\
Voicify the current rectangle.
When calling from a program,arguments are
START END personality
Prompts for PERSONALITY  with completion when called interactively." t nil)

(autoload (quote emacspeak-voicify-region) "emacspeak-speak" "\
Voicify the current region.
When calling from a program,arguments are
START END personality.
Prompts for PERSONALITY  with completion when called interactively." t nil)

(autoload (quote emacspeak-blink-matching-open) "emacspeak-speak" "\
Display matching delimiter in the minibuffer." t nil)

(autoload (quote emacspeak-use-customized-blink-paren) "emacspeak-speak" "\
A customized blink-paren to speak  matching opening paren.
We need to call this in case Emacs
is anal and loads its own builtin blink-paren function
which does not talk." t nil)

(autoload (quote emacspeak-speak-spaces-at-point) "emacspeak-speak" "\
Speak the white space at point." t nil)

(autoload (quote emacspeak-switch-to-completions-window) "emacspeak-speak" "\
Jump to the *Completions* buffer if it is active.
We make the current minibuffer contents (which is obviously the
prefix for each entry in the completions buffer) inaudible
to reduce chatter." t nil)

(autoload (quote emacspeak-completions-move-to-completion-group) "emacspeak-speak" "\
Move to group of choices beginning with character last
typed. If no such group exists, then we dont move. " t nil)

(autoload (quote emacspeak-mark-forward-mark) "emacspeak-speak" "\
Cycle forward through the mark ring." t nil)

(autoload (quote emacspeak-mark-backward-mark) "emacspeak-speak" "\
Cycle backward through the mark ring." t nil)

(autoload (quote emacspeak-speak-and-skip-extent-upto-char) "emacspeak-speak" "\
Search forward from point until we hit char.
Speak text between point and the char we hit." t nil)

(autoload (quote emacspeak-speak-and-skip-extent-upto-this-char) "emacspeak-speak" "\
Speak extent delimited by point and last character typed." t nil)

(autoload (quote emacspeak-speak-message-at-time) "emacspeak-speak" "\
Set up ring-at-time to speak message at specified time.
Provides simple stop watch functionality in addition to other things.
See documentation for command run-at-time for details on time-spec." t nil)

(autoload (quote emacspeak-speak-load-directory-settings) "emacspeak-speak" "\
Load a directory specific Emacspeak settings file.
This is typically used to load up settings that are specific to
an electronic book consisting of many files in the same
directory." t nil)

;;;***

;;;### (autoloads (emacspeak-table-make-table) "emacspeak-table"
;;;;;;  "emacspeak-table.el" (17751 59931))
;;; Generated autoloads from emacspeak-table.el

(autoload (quote emacspeak-table-make-table) "emacspeak-table" "\
Construct a table object from elements." nil nil)

;;;***

;;;### (autoloads (emacspeak-table-copy-to-clipboard emacspeak-table-display-table-in-region
;;;;;;  emacspeak-table-view-csv-buffer emacspeak-table-find-csv-file
;;;;;;  emacspeak-table-find-file) "emacspeak-table-ui" "emacspeak-table-ui.el"
;;;;;;  (17758 21298))
;;; Generated autoloads from emacspeak-table-ui.el

(defsubst emacspeak-table-prepare-table-buffer (table buffer &optional filename) "\
Prepare tabular data." (declare (special emacspeak-table positions)) (save-excursion (set-buffer buffer) (let ((i 0) (j 0) (count 0) (row-start 1) (column-start 1) (inhibit-read-only t)) (setq truncate-lines t) (erase-buffer) (set (make-local-variable (quote emacspeak-table)) table) (set (make-local-variable (quote positions)) (make-hash-table)) (when filename (setq buffer-file-name filename)) (setq count (1- (emacspeak-table-num-columns table))) (loop for row across (emacspeak-table-elements table) do (loop for element across row do (setf (gethash (intern (format "element:%s:%s" i j)) positions) (point)) (insert (format "%s%s" (emacspeak-table-this-element table i j) (if (= j count) "
" "	"))) (put-text-property column-start (point) (quote column) j) (setq column-start (point)) (incf j)) (setq j 0) (put-text-property row-start (point) (quote row) i) (setq row-start (point)) (incf i)) (emacspeak-table-mode) (goto-char (point-min)))) (switch-to-buffer buffer) (setq truncate-lines t) (message "Use Emacspeak Table UI to browse this table."))

(autoload (quote emacspeak-table-find-file) "emacspeak-table-ui" "\
Open a file containing table data and display it in table mode.
emacspeak table mode is designed to let you browse tabular data using
all the power of the two-dimensional spatial layout while giving you
sufficient contextual information.  The etc/tables subdirectory of the
emacspeak distribution contains some sample tables --these are the
CalTrain schedules.  Execute command `describe-mode' bound to
\\[describe-mode] in a buffer that is in emacspeak table mode to read
the documentation on the table browser." t nil)

(autoload (quote emacspeak-table-find-csv-file) "emacspeak-table-ui" "\
Process a csv (comma separated values) file.
The processed  data and presented using emacspeak table navigation. " t nil)

(autoload (quote emacspeak-table-view-csv-buffer) "emacspeak-table-ui" "\
Process a csv (comma separated values) data.
The processed  data and presented using emacspeak table navigation. " t nil)

(autoload (quote emacspeak-table-display-table-in-region) "emacspeak-table-ui" "\
Recognize tabular data in current region and display it in table
browsing mode in a a separate buffer.
emacspeak table mode is designed to let you browse tabular data using
all the power of the two-dimensional spatial layout while giving you
sufficient contextual information.  The tables subdirectory of the
emacspeak distribution contains some sample tables --these are the
CalTrain schedules.  Execute command `describe-mode' bound to
\\[describe-mode] in a buffer that is in emacspeak table mode to read
the documentation on the table browser." t nil)

(autoload (quote emacspeak-table-copy-to-clipboard) "emacspeak-table-ui" "\
Copy table in current buffer to the table clipboard.
Current buffer must be in emacspeak-table mode." t nil)

;;;***

;;;### (autoloads (ems-tabulate-parse-region emacspeak-tabulate-region)
;;;;;;  "emacspeak-tabulate" "emacspeak-tabulate.el" (17751 59932))
;;; Generated autoloads from emacspeak-tabulate.el

(autoload (quote emacspeak-tabulate-region) "emacspeak-tabulate" "\
Voicifies the white-space of a table if one found.  Optional interactive prefix
arg mark-fields specifies if the header row information is used to mark fields
in the white-space." t nil)

(autoload (quote ems-tabulate-parse-region) "emacspeak-tabulate" "\
Parse  region as tabular data and return a vector of vectors" nil nil)

;;;***

;;;### (autoloads (emacspeak-tapestry-select-window-by-name emacspeak-tapestry-describe-tapestry)
;;;;;;  "emacspeak-tapestry" "emacspeak-tapestry.el" (17751 59932))
;;; Generated autoloads from emacspeak-tapestry.el

(autoload (quote emacspeak-tapestry-describe-tapestry) "emacspeak-tapestry" "\
Describe the current layout of visible buffers in current frame.
Use interactive prefix arg to get coordinate positions of the
displayed buffers." t nil)

(autoload (quote emacspeak-tapestry-select-window-by-name) "emacspeak-tapestry" "\
Select window by the name of the buffer it displays.
This is useful when using modes like ECB or the new GDB UI where
  you want to preserve the window layout 
but quickly switch to a window by name." t nil)

;;;***

;;;### (autoloads (emacspeak-url-template-fetch emacspeak-url-template-open
;;;;;;  emacspeak-url-template-load emacspeak-url-template-define
;;;;;;  emacspeak-url-template-get) "emacspeak-url-template" "emacspeak-url-template.el"
;;;;;;  (17751 59958))
;;; Generated autoloads from emacspeak-url-template.el

(autoload (quote emacspeak-url-template-get) "emacspeak-url-template" "\
Lookup key and return corresponding template. " nil nil)

(autoload (quote emacspeak-url-template-define) "emacspeak-url-template" "\
Define a URL template.

name            Name used to identify template
template        Template URI with `%s' for slots
generators      List of prompters.
                Generators are strings or functions.
                String values specify prompts.
                Function values are called to obtain values.
post-action     Function called to apply post actions.
                Possible actions include speaking the result.
fetcher         Unless specified, browse-url retrieves URL.
                If specified, fetcher is a function of one arg
                that is called with the URI to retrieve.
documentation   Documents this template resource. " nil nil)

(autoload (quote emacspeak-url-template-load) "emacspeak-url-template" "\
Load URL template resources from specified location." t nil)

(autoload (quote emacspeak-url-template-open) "emacspeak-url-template" "\
Fetch resource identified by URL template." nil nil)

(autoload (quote emacspeak-url-template-fetch) "emacspeak-url-template" "\
Fetch a pre-defined resource.
Use Emacs completion to obtain a list of available resources.
Resources typically prompt for the relevant information
before completing the request.
Optional interactive prefix arg displays documentation for specified resource." t nil)

;;;***

;;;### (autoloads (emacspeak-w3-play-media-at-point emacspeak-w3-browse-atom-at-point
;;;;;;  emacspeak-w3-browse-rss-at-point emacspeak-w3-google-similar-to-this-page
;;;;;;  emacspeak-w3-google-on-this-site emacspeak-w3-google-who-links-to-this-page
;;;;;;  emacspeak-w3-browse-xml-url-with-style emacspeak-w3-browse-url-with-style
;;;;;;  emacspeak-w3-xpath-filter-and-follow emacspeak-w3-class-filter-and-follow
;;;;;;  emacspeak-w3-extract-node-by-id emacspeak-w3-extract-by-id-list
;;;;;;  emacspeak-w3-extract-by-class-list emacspeak-w3-extract-by-class
;;;;;;  emacspeak-w3-extract-tables-by-match-list emacspeak-w3-extract-table-by-match
;;;;;;  emacspeak-w3-extract-tables-by-position-list emacspeak-w3-extract-table-by-position
;;;;;;  emacspeak-w3-extract-nested-table-list emacspeak-w3-extract-nested-table
;;;;;;  emacspeak-w3-extract-print-streams emacspeak-w3-extract-media-streams
;;;;;;  emacspeak-w3-xslt-filter emacspeak-w3-set-xsl-keep-result
;;;;;;  emacspeak-w3-count-tables emacspeak-w3-count-nested-tables
;;;;;;  emacspeak-w3-count-matches emacspeak-w3-xsl-toggle emacspeak-w3-xslt-select
;;;;;;  emacspeak-w3-xslt-apply emacspeak-w3-curl-url-under-point)
;;;;;;  "emacspeak-w3" "emacspeak-w3.el" (17751 59958))
;;; Generated autoloads from emacspeak-w3.el

(autoload (quote emacspeak-w3-curl-url-under-point) "emacspeak-w3" "\
Display contents of URL under point using Curl and W3.  The
document is displayed in a separate buffer. " t nil)

(autoload (quote emacspeak-w3-xslt-apply) "emacspeak-w3" "\
Apply specified transformation to current page." t nil)

(autoload (quote emacspeak-w3-xslt-select) "emacspeak-w3" "\
Select XSL transformation applied to WWW pages before they are displayed ." t nil)

(autoload (quote emacspeak-w3-xsl-toggle) "emacspeak-w3" "\
Toggle  application of XSL transformations.
This uses XSLT Processor xsltproc available as part of the
libxslt package." t nil)

(autoload (quote emacspeak-w3-count-matches) "emacspeak-w3" "\
Count matches for locator  in HTML." t nil)

(autoload (quote emacspeak-w3-count-nested-tables) "emacspeak-w3" "\
Count nested tables in HTML." t nil)

(autoload (quote emacspeak-w3-count-tables) "emacspeak-w3" "\
Count  tables in HTML." t nil)

(autoload (quote emacspeak-w3-set-xsl-keep-result) "emacspeak-w3" "\
Set value of `emacspeak-w3-xsl-keep-result'." t nil)

(autoload (quote emacspeak-w3-xslt-filter) "emacspeak-w3" "\
Extract elements matching specified XPath path locator
from HTML.  Extracts specified elements from current WWW
page and displays it in a separate buffer.  Optional arg url
specifies the page to extract table from.
Optional arg COMPLEMENT inverts the filter.  " t nil)

(autoload (quote emacspeak-w3-extract-media-streams) "emacspeak-w3" "\
Extract links to media streams.
operate on current web page when in a W3 buffer; otherwise prompt for url.
`prompt-url' is the URL to process. Prompts for URL when called
interactively. Optional arg `speak' specifies if the result should be
spoken automatically." t nil)

(autoload (quote emacspeak-w3-extract-print-streams) "emacspeak-w3" "\
Extract links to printable  streams.
operate on current web page when in a W3 buffer; otherwise prompt for url.
`prompt-url' is the URL to process. Prompts for URL when called
interactively. Optional arg `speak' specifies if the result should be
spoken automatically." t nil)

(autoload (quote emacspeak-w3-extract-nested-table) "emacspeak-w3" "\
Extract nested table specified by `table-index'. Default is to
operate on current web page when in a W3 buffer; otherwise
`prompt-url' is the URL to process. Prompts for URL when called
interactively. Optional arg `speak' specifies if the result should be
spoken automatically." t nil)

(autoload (quote emacspeak-w3-extract-nested-table-list) "emacspeak-w3" "\
Extract specified list of tables from a WWW page." t nil)

(autoload (quote emacspeak-w3-extract-table-by-position) "emacspeak-w3" "\
Extract table at specified position.
 Optional arg url specifies the page to extract content from.
Interactive prefix arg causes url to be read from the minibuffer." t nil)

(autoload (quote emacspeak-w3-extract-tables-by-position-list) "emacspeak-w3" "\
Extract specified list of nested tables from a WWW page.
Tables are specified by their position in the list
nested of tables found in the page." t nil)

(autoload (quote emacspeak-w3-extract-table-by-match) "emacspeak-w3" "\
Extract table containing  specified match.
 Optional arg url specifies the page to extract content from.
Interactive prefix arg causes url to be read from the minibuffer." t nil)

(autoload (quote emacspeak-w3-extract-tables-by-match-list) "emacspeak-w3" "\
Extract specified  tables from a WWW page.
Tables are specified by containing  match pattern
 found in the match list." t nil)

(autoload (quote emacspeak-w3-extract-by-class) "emacspeak-w3" "\
Extract elements having specified class attribute from HTML. Extracts
specified elements from current WWW page and displays it in a separate
buffer. Optional arg url specifies the page to extract content from.
Interactive use provides list of class values as completion." t nil)

(autoload (quote emacspeak-w3-extract-by-class-list) "emacspeak-w3" "\
Extract elements having class specified in list `classes' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer. Optional arg url specifies the page to extract
content from. Interactive use provides list of class values as
completion. " t nil)

(autoload (quote emacspeak-w3-extract-by-id-list) "emacspeak-w3" "\
Extract elements having id specified in list `ids' from HTML.
Extracts specified elements from current WWW page and displays it in a
separate buffer. Optional arg url specifies the page to extract
content from. Interactive use provids list of id values as
completion. " t nil)

(autoload (quote emacspeak-w3-extract-node-by-id) "emacspeak-w3" "\
Extract specified node from URI." t nil)

(autoload (quote emacspeak-w3-class-filter-and-follow) "emacspeak-w3" "\
Follow url and point, and filter the result by specified class.
Class can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well." t nil)

(autoload (quote emacspeak-w3-xpath-filter-and-follow) "emacspeak-w3" "\
Follow url and point, and filter the result by specified xpath.
XPath can be set locally for a buffer, and overridden with an
interactive prefix arg. If there is a known rewrite url rule, that is
used as well." t nil)

(autoload (quote emacspeak-w3-browse-url-with-style) "emacspeak-w3" "\
Browse URL with specified XSL style." t nil)

(autoload (quote emacspeak-w3-browse-xml-url-with-style) "emacspeak-w3" "\
Browse XML URL with specified XSL style." t nil)

(autoload (quote emacspeak-w3-google-who-links-to-this-page) "emacspeak-w3" "\
Perform a google search to locate documents that link to the
current page." t nil)

(autoload (quote emacspeak-w3-google-on-this-site) "emacspeak-w3" "\
Perform a google search restricted to the current WWW site." t nil)

(autoload (quote emacspeak-w3-google-similar-to-this-page) "emacspeak-w3" "\
Ask Google to find documents similar to this one." t nil)

(defadvice w3-nasty-disgusting-http-equiv-handling (around fix-bug pre act comp) (let ((emacspeak-use-auditory-icons nil)) (condition-case nil ad-do-it (error (message "caught an error")))))

(autoload (quote emacspeak-w3-browse-rss-at-point) "emacspeak-w3" "\
Browses RSS url under point." t nil)

(autoload (quote emacspeak-w3-browse-atom-at-point) "emacspeak-w3" "\
Browses Atom url under point." t nil)

(autoload (quote emacspeak-w3-play-media-at-point) "emacspeak-w3" "\
Play media url under point " t nil)

;;;***

;;;### (autoloads (emacspeak-w3m-browse-url-with-style emacspeak-w3m-browse-xml-url-with-style
;;;;;;  emacspeak-w3m-preview-this-buffer) "emacspeak-w3m" "emacspeak-w3m.el"
;;;;;;  (17754 30698))
;;; Generated autoloads from emacspeak-w3m.el

(autoload (quote emacspeak-w3m-preview-this-buffer) "emacspeak-w3m" "\
Preview this buffer in w3m." t nil)

(autoload (quote emacspeak-w3m-browse-xml-url-with-style) "emacspeak-w3m" "\
Browse XML URL with specified XSL style in w3m." t nil)

(autoload (quote emacspeak-w3m-browse-url-with-style) "emacspeak-w3m" "\
Browse URL with specified XSL style. in w3m." t nil)

;;;***

;;;### (autoloads (emacspeak-websearch-usenet emacspeak-websearch-emapspeak-near-my-location
;;;;;;  emacspeak-websearch-google-search-in-date-range emacspeak-websearch-google
;;;;;;  emacspeak-websearch-usenet-search emacspeak-websearch-do-post
;;;;;;  emacspeak-websearch-dispatch) "emacspeak-websearch" "emacspeak-websearch.el"
;;;;;;  (17751 59933))
;;; Generated autoloads from emacspeak-websearch.el

(defgroup emacspeak-websearch nil "Websearch tools for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-websearch-dispatch) "emacspeak-websearch" "\
Launches specific websearch queries.
Press `?' to list available search engines.
Once selected, the selected searcher prompts for additional information as appropriate.
When using W3,  this interface attempts to speak the most relevant information on the result page." t nil)

(autoload (quote emacspeak-websearch-do-post) "emacspeak-websearch" "\
Submit a post request. " nil nil)

(autoload (quote emacspeak-websearch-usenet-search) "emacspeak-websearch" "\
Search a Usenet newsgroup." t nil)

(autoload (quote emacspeak-websearch-google) "emacspeak-websearch" "\
Perform a Google search.
Optional interactive prefix arg `lucky' is equivalent to hitting the
I'm Feeling Lucky button on Google." t nil)

(autoload (quote emacspeak-websearch-google-search-in-date-range) "emacspeak-websearch" "\
Use this from inside the calendar to do Google date-range searches." t nil)

(defvar emacspeak-websearch-google-maps-uri "http://maps.google.com/maps?q=%s&output=kml" "\
URL template for Google maps.")

(autoload (quote emacspeak-websearch-emapspeak-near-my-location) "emacspeak-websearch" "\
Perform search relative to `my-location'." t nil)

(autoload (quote emacspeak-websearch-usenet) "emacspeak-websearch" "\
Prompt and browse a Usenet newsgroup.
Optional interactive prefix arg results in prompting for a search term." t nil)

;;;***

;;;### (autoloads (emacspeak-widget-default-summarize emacspeak-widget-summarize
;;;;;;  emacspeak-widget-summarize-parent) "emacspeak-widget" "emacspeak-widget.el"
;;;;;;  (17751 59933))
;;; Generated autoloads from emacspeak-widget.el

(autoload (quote emacspeak-widget-summarize-parent) "emacspeak-widget" "\
Summarize parent of widget at point." t nil)

(autoload (quote emacspeak-widget-summarize) "emacspeak-widget" "\
Summarize specified widget." nil nil)

(autoload (quote emacspeak-widget-default-summarize) "emacspeak-widget" "\
Fall back summarizer for all widgets" nil nil)

;;;***

;;;### (autoloads (emacspeak-wizards-find-emacspeak-source emacspeak-wizards-popup-input-buffer
;;;;;;  emacspeak-wizards-add-autoload-cookies emacspeak-wizards-unhex-uri
;;;;;;  emacspeak-wizards-show-commentary emacspeak-wizards-rivo
;;;;;;  emacspeak-wizards-units emacspeak-wizards-toggle-mm-dd-yyyy-date-pronouncer
;;;;;;  emacspeak-wizards-speak-iso-datetime emacspeak-wizards-tramp-open-location
;;;;;;  emacspeak-wizards-generate-voice-sampler emacspeak-wizards-voice-sampler
;;;;;;  emacspeak-wizards-show-face emacspeak-wizards-find-grep emacspeak-wizards-find-longest-paragraph-in-region
;;;;;;  emacspeak-wizards-find-longest-line-in-region emacspeak-wizards-google-transcode
;;;;;;  emacspeak-wizards-google-hits emacspeak-wizards-vc-n emacspeak-wizards-vc-viewer-refresh
;;;;;;  emacspeak-wizards-vc-viewer emacspeak-wizards-fix-read-only-text
;;;;;;  emacspeak-wizards-fix-typo emacspeak-wizards-spot-words emacspeak-kill-buffer-quietly
;;;;;;  emacspeak-switch-to-previous-buffer emacspeak-wizards-occur-header-lines
;;;;;;  emacspeak-wizards-how-many-matches emacspeak-wizards-count-slides-in-region
;;;;;;  emacspeak-wizards-squeeze-blanks emacspeak-wizards-show-environment-variable
;;;;;;  emacspeak-customize emacspeak-wizards-use-w3-or-w3m emacspeak-wizards-finder-find
;;;;;;  emacspeak-wizards-generate-finder emacspeak-wizards-portfolio-quotes
;;;;;;  emacspeak-wizards-dvi-display emacspeak-wizards-ppt-display
;;;;;;  emacspeak-wizards-xl-display emacspeak-wizards-rpm-query-in-dired
;;;;;;  emacspeak-wizards-shell-toggle emacspeak-annotate-add-annotation
;;;;;;  emacspeak-wizards-get-table-content-from-file emacspeak-wizards-get-table-content-from-url
;;;;;;  emacspeak-wizards-terminal emacspeak-curl emacspeak-lynx
;;;;;;  emacspeak-links emacspeak-skip-blank-lines-backward emacspeak-skip-blank-lines-forward
;;;;;;  emacspeak-show-property-at-point emacspeak-show-personality-at-point
;;;;;;  emacspeak-customize-personal-settings emacspeak-ssh-tts-restart
;;;;;;  emacspeak-emergency-tts-restart emacspeak-speak-show-memory-used
;;;;;;  emacspeak-wizards-show-list-variable emacspeak-clipboard-paste
;;;;;;  emacspeak-clipboard-copy emacspeak-select-this-buffer-next-display
;;;;;;  emacspeak-select-this-buffer-previous-display emacspeak-select-this-buffer-other-window-display
;;;;;;  emacspeak-speak-this-buffer-next-display emacspeak-speak-this-buffer-previous-display
;;;;;;  emacspeak-speak-this-buffer-other-window-display emacspeak-previous-frame-or-buffer
;;;;;;  emacspeak-next-frame-or-buffer emacspeak-frame-label-or-switch-to-labelled-frame
;;;;;;  emacspeak-generate-texinfo-option-documentation emacspeak-generate-texinfo-command-documentation
;;;;;;  emacspeak-generate-documentation emacspeak-learn-emacs-mode
;;;;;;  emacspeak-wizards-move-and-speak emacspeak-cvs-berlios-get-project-snapshot
;;;;;;  emacspeak-cvs-gnu-get-project-snapshot emacspeak-cvs-sf-get-project-snapshot
;;;;;;  emacspeak-cvs-get-anonymous emacspeak-wizards-vi-as-su-file
;;;;;;  emacspeak-wizards-edit-file-as-root emacspeak-wizards-vpn-toggle
;;;;;;  emacspeak-wizards-ppp-toggle emacspeak-wizards-tpctl-display-status
;;;;;;  emacspeak-sudo emacspeak-root emacspeak-speak-telephone-directory
;;;;;;  emacspeak-speak-show-active-network-interfaces emacspeak-speak-hostname
;;;;;;  emacspeak-speak-popup-messages emacspeak-speak-browse-linux-howto
;;;;;;  emacspeak-speak-run-shell-command emacspeak-symlink-current-file
;;;;;;  emacspeak-link-current-file emacspeak-copy-current-file emacspeak-view-emacspeak-faq
;;;;;;  emacspeak-view-emacspeak-tips emacspeak-view-emacspeak-doc
;;;;;;  emacspeak-view-emacspeak-news) "emacspeak-wizards" "emacspeak-wizards.el"
;;;;;;  (17751 59933))
;;; Generated autoloads from emacspeak-wizards.el

(autoload (quote emacspeak-view-emacspeak-news) "emacspeak-wizards" "\
Display info on recent change to Emacspeak." t nil)

(autoload (quote emacspeak-view-emacspeak-doc) "emacspeak-wizards" "\
Display a summary of all Emacspeak commands." t nil)

(autoload (quote emacspeak-view-emacspeak-tips) "emacspeak-wizards" "\
Browse  Emacspeak productivity tips." t nil)

(autoload (quote emacspeak-view-emacspeak-faq) "emacspeak-wizards" "\
Browse the Emacspeak FAQ." t nil)

(autoload (quote emacspeak-copy-current-file) "emacspeak-wizards" "\
Copy file visited in current buffer to new location.
Prompts for the new location and preserves modification time
  when copying.  If location is a directory, the file is copied
  to that directory under its current name ; if location names
  a file in an existing directory, the specified name is
  used.  Asks for confirmation if the copy will result in an
  existing file being overwritten." t nil)

(autoload (quote emacspeak-link-current-file) "emacspeak-wizards" "\
Link (hard link) file visited in current buffer to new location.
Prompts for the new location and preserves modification time
  when linking.  If location is a directory, the file is copied
  to that directory under its current name ; if location names
  a file in an existing directory, the specified name is
  used.  Signals an error if target already exists." t nil)

(autoload (quote emacspeak-symlink-current-file) "emacspeak-wizards" "\
Link (symbolic link) file visited in current buffer to new location.
Prompts for the new location and preserves modification time
  when linking.  If location is a directory, the file is copied
  to that directory under its current name ; if location names
  a file in an existing directory, the specified name is
  used.  Signals an error if target already exists." t nil)

(autoload (quote emacspeak-speak-run-shell-command) "emacspeak-wizards" "\
Invoke shell COMMAND and display its output as a table.  The results
are placed in a buffer in Emacspeak's table browsing mode.  Optional
interactive prefix arg as-root runs the command as root.  Use this for running shell commands that produce
tabulated output.  This command should be used for shell commands that
produce tabulated output that works with Emacspeak's table recognizer.
Verify this first by running the command in a shell and executing
command `emacspeak-table-display-table-in-region' normally bound to
\\[emacspeak-table-display-table-in-region]." t nil)

(autoload (quote emacspeak-speak-browse-linux-howto) "emacspeak-wizards" "\
Browse a Linux Howto file.
We cleanup underlining, and set up outline mode correctly." t nil)

(autoload (quote emacspeak-speak-popup-messages) "emacspeak-wizards" "\
Pop up messages buffer.
If it is already selected then hide it and try to restore
previous window configuration." t nil)

(autoload (quote emacspeak-speak-hostname) "emacspeak-wizards" "\
Speak host name." t nil)

(autoload (quote emacspeak-speak-show-active-network-interfaces) "emacspeak-wizards" "\
Shows all active network interfaces in the echo area.
With interactive prefix argument ADDRESS it prompts for a
specific interface and shows its address. The address is
also copied to the kill ring for convenient yanking." t nil)

(autoload (quote emacspeak-speak-telephone-directory) "emacspeak-wizards" "\
Lookup and display a phone number.
With prefix arg, opens the phone book for editting." t nil)

(autoload (quote emacspeak-root) "emacspeak-wizards" "\
Start a root shell or switch to one that already exists.
Optional interactive prefix arg `cd' executes cd
default-directory after switching." t nil)

(autoload (quote emacspeak-sudo) "emacspeak-wizards" "\
SUDo command --run command as super user." t nil)

(autoload (quote emacspeak-wizards-tpctl-display-status) "emacspeak-wizards" "\
Show display status on thinkpads using tpctl." t nil)

(autoload (quote emacspeak-wizards-ppp-toggle) "emacspeak-wizards" "\
Bring up or bring down ppp." t nil)

(autoload (quote emacspeak-wizards-vpn-toggle) "emacspeak-wizards" "\
Bring up or bring down vpn." t nil)

(autoload (quote emacspeak-wizards-edit-file-as-root) "emacspeak-wizards" "\
Edit file as root using sudo vi.
See /etc/sudoers for how to set up sudo." t nil)

(autoload (quote emacspeak-wizards-vi-as-su-file) "emacspeak-wizards" "\
Launch sudo vi on specified file in a terminal." t nil)

(autoload (quote emacspeak-cvs-get-anonymous) "emacspeak-wizards" "\
Get latest cvs snapshot of emacspeak." t nil)

(autoload (quote emacspeak-cvs-sf-get-project-snapshot) "emacspeak-wizards" "\
Grab CVS snapshot  of specified project from sf.
Ask for module name if prefix argument is given" t nil)

(autoload (quote emacspeak-cvs-gnu-get-project-snapshot) "emacspeak-wizards" "\
Grab CVS snapshot  of specified project from gnu.
Ask for module name if prefix argument is given" t nil)

(autoload (quote emacspeak-cvs-berlios-get-project-snapshot) "emacspeak-wizards" "\
Grab CVS snapshot  of specified project from berlios.de.
Ask for module name if prefix argument is given" t nil)

(autoload (quote emacspeak-wizards-move-and-speak) "emacspeak-wizards" "\
Speaks a chunk of text bounded by point and a target position.
Target position is specified using a navigation command and a
count that specifies how many times to execute that command
first.  Point is left at the target position.  Interactively,
command is specified by pressing the key that invokes the
command." t nil)

(autoload (quote emacspeak-learn-emacs-mode) "emacspeak-wizards" "\
Helps you learn the keys.  You can press keys and hear what they do.
To leave, press \\[keyboard-quit]." t nil)

(autoload (quote emacspeak-generate-documentation) "emacspeak-wizards" "\
Generate docs for all emacspeak commands.
Prompts for FILENAME in which to save the documentation.
Warning! Contents of file filename will be overwritten." t nil)

(autoload (quote emacspeak-generate-texinfo-command-documentation) "emacspeak-wizards" "\
Generate texinfo documentation  for all emacspeak
commands  into file commands.texi.
Warning! Contents of file commands.texi will be overwritten." t nil)

(autoload (quote emacspeak-generate-texinfo-option-documentation) "emacspeak-wizards" "\
Generate texinfo documentation  for all emacspeak
options  into file filename.
Warning! Contents of file filename will be overwritten." t nil)

(defsubst emacspeak-frame-read-frame-label nil "\
Read a frame label with completion." (interactive) (let* ((frame-names-alist (make-frame-names-alist)) (default (car (car frame-names-alist))) (input (completing-read (format "Select Frame (default %s): " default) frame-names-alist nil t nil (quote frame-name-history)))) (if (= (length input) 0) default)))

(autoload (quote emacspeak-frame-label-or-switch-to-labelled-frame) "emacspeak-wizards" "\
Switch to labelled frame.
With optional PREFIX argument, label current frame." t nil)

(autoload (quote emacspeak-next-frame-or-buffer) "emacspeak-wizards" "\
Move to next buffer.
With optional interactive prefix arg `frame', move to next frame instead." t nil)

(autoload (quote emacspeak-previous-frame-or-buffer) "emacspeak-wizards" "\
Move to previous buffer.
With optional interactive prefix arg `frame', move to previous frame instead." t nil)

(autoload (quote emacspeak-speak-this-buffer-other-window-display) "emacspeak-wizards" "\
Speak this buffer as displayed in a different frame.  Emacs
allows you to display the same buffer in multiple windows or
frames.  These different windows can display different
portions of the buffer.  This is equivalent to leaving a
book open at places at once.  This command allows you to
listen to the places where you have left the book open.  The
number used to invoke this command specifies which of the
displays you wish to speak.  Typically you will have two or
at most three such displays open.  The current display is 0,
the next is 1, and so on.  Optional argument ARG specifies
the display to speak." t nil)

(autoload (quote emacspeak-speak-this-buffer-previous-display) "emacspeak-wizards" "\
Speak this buffer as displayed in a `previous' window.
See documentation for command
`emacspeak-speak-this-buffer-other-window-display' for the
meaning of `previous'." t nil)

(autoload (quote emacspeak-speak-this-buffer-next-display) "emacspeak-wizards" "\
Speak this buffer as displayed in a `previous' window.
See documentation for command
`emacspeak-speak-this-buffer-other-window-display' for the
meaning of `next'." t nil)

(autoload (quote emacspeak-select-this-buffer-other-window-display) "emacspeak-wizards" "\
Switch  to this buffer as displayed in a different frame.  Emacs
allows you to display the same buffer in multiple windows or
frames.  These different windows can display different
portions of the buffer.  This is equivalent to leaving a
book open at places at once.  This command allows you to
move to the places where you have left the book open.  The
number used to invoke this command specifies which of the
displays you wish to select.  Typically you will have two or
at most three such displays open.  The current display is 0,
the next is 1, and so on.  Optional argument ARG specifies
the display to select." t nil)

(autoload (quote emacspeak-select-this-buffer-previous-display) "emacspeak-wizards" "\
Select this buffer as displayed in a `previous' window.
See documentation for command
`emacspeak-select-this-buffer-other-window-display' for the
meaning of `previous'." t nil)

(autoload (quote emacspeak-select-this-buffer-next-display) "emacspeak-wizards" "\
Select this buffer as displayed in a `next' frame.
See documentation for command
`emacspeak-select-this-buffer-other-window-display' for the
meaning of `next'." t nil)

(autoload (quote emacspeak-clipboard-copy) "emacspeak-wizards" "\
Copy contents of the region to the emacspeak clipboard.
Previous contents of the clipboard will be overwritten.  The Emacspeak
clipboard is a convenient way of sharing information between
independent Emacspeak sessions running on the same or different
machines.  Do not use this for sharing information within an Emacs
session --Emacs' register commands are far more efficient and
light-weight.  Optional interactive prefix arg results in Emacspeak
prompting for the clipboard file to use.
Argument START and END specifies  region.
Optional argument PROMPT  specifies whether we prompt for the name of a clipboard file." t nil)

(autoload (quote emacspeak-clipboard-paste) "emacspeak-wizards" "\
Yank contents of the Emacspeak clipboard at point.
The Emacspeak clipboard is a convenient way of sharing information between
independent Emacspeak sessions running on the same or different
machines.  Do not use this for sharing information within an Emacs
session --Emacs' register commands are far more efficient and
light-weight.  Optional interactive prefix arg pastes from
the emacspeak table clipboard instead." t nil)

(autoload (quote emacspeak-wizards-show-list-variable) "emacspeak-wizards" "\
Convenience command to view Emacs variables that are long lists.
Prompts for a variable name and displays its value in a separate buffer.
Lists are displayed one element per line.
Argument VAR specifies variable whose value is to be displayed." t nil)

(autoload (quote emacspeak-speak-show-memory-used) "emacspeak-wizards" "\
Convenience command to view state of memory used in this session so far." t nil)

(autoload (quote emacspeak-emergency-tts-restart) "emacspeak-wizards" "\
For use in an emergency.
Will start TTS engine specified by
emacspeak-emergency-tts-server." t nil)

(autoload (quote emacspeak-ssh-tts-restart) "emacspeak-wizards" "\
Restart specified ssh tts server." t nil)

(autoload (quote emacspeak-customize-personal-settings) "emacspeak-wizards" "\
Create a customization buffer for browsing and updating
personal customizations." t nil)

(autoload (quote emacspeak-show-personality-at-point) "emacspeak-wizards" "\
Show value of property personality (and possibly face)
at point." t nil)

(autoload (quote emacspeak-show-property-at-point) "emacspeak-wizards" "\
Show value of PROPERTY at point.
If optional arg property is not supplied, read it interactively.
Provides completion based on properties that are of interest.
If no property is set, show a message and exit." t nil)

(autoload (quote emacspeak-skip-blank-lines-forward) "emacspeak-wizards" "\
Move forward across blank lines.
The line under point is then spoken.
Signals end of buffer." t nil)

(autoload (quote emacspeak-skip-blank-lines-backward) "emacspeak-wizards" "\
Move backward  across blank lines.
The line under point is   then spoken.
Signals beginning  of buffer." t nil)

(autoload (quote emacspeak-links) "emacspeak-wizards" "\
Launch links on  specified URL in a new terminal." t nil)

(autoload (quote emacspeak-lynx) "emacspeak-wizards" "\
Launch lynx on  specified URL in a new terminal." t nil)

(autoload (quote emacspeak-curl) "emacspeak-wizards" "\
Grab URL using Curl, and preview it with W3." t nil)

(autoload (quote emacspeak-wizards-terminal) "emacspeak-wizards" "\
Launch terminal and rename buffer appropriately." t nil)

(autoload (quote emacspeak-wizards-get-table-content-from-url) "emacspeak-wizards" "\
Extract table specified by depth and count from HTML
content at URL.
Extracted content is placed as a csv file in task.csv." t nil)

(autoload (quote emacspeak-wizards-get-table-content-from-file) "emacspeak-wizards" "\
Extract table specified by depth and count from HTML
content at file.
Extracted content is placed as a csv file in task.csv." t nil)

(autoload (quote emacspeak-annotate-add-annotation) "emacspeak-wizards" "\
Add annotation to the annotation working buffer.
Prompt for annotation buffer if not already set.
Interactive prefix arg `reset' prompts for the annotation
buffer even if one is already set.
Annotation is entered in a temporary buffer and the
annotation is inserted into the working buffer when complete." t nil)

(autoload (quote emacspeak-wizards-shell-toggle) "emacspeak-wizards" "\
Switch to the shell buffer and cd to
 the directory of the current buffer." t nil)

(autoload (quote emacspeak-wizards-rpm-query-in-dired) "emacspeak-wizards" "\
Run rpm -qi on current dired entry." t nil)

(autoload (quote emacspeak-wizards-xl-display) "emacspeak-wizards" "\
Called to set up preview of an XL file.
Assumes we are in a buffer visiting a .xls file.
Previews those contents as HTML and nukes the buffer
visiting the xls file." t nil)

(autoload (quote emacspeak-wizards-ppt-display) "emacspeak-wizards" "\
Called to set up preview of an PPT file.
Assumes we are in a buffer visiting a .ppt file.
Previews those contents as HTML and nukes the buffer
visiting the ppt file." t nil)

(autoload (quote emacspeak-wizards-dvi-display) "emacspeak-wizards" "\
Called to set up preview of an DVI file.
Assumes we are in a buffer visiting a .DVI file.
Previews those contents as text and nukes the buffer
visiting the DVI file." t nil)

(autoload (quote emacspeak-wizards-portfolio-quotes) "emacspeak-wizards" "\
Bring up detailed stock quotes for portfolio specified by
emacspeak-websearch-personal-portfolio." t nil)

(autoload (quote emacspeak-wizards-generate-finder) "emacspeak-wizards" "\
Generate a widget-enabled finder wizard." t nil)

(autoload (quote emacspeak-wizards-finder-find) "emacspeak-wizards" "\
Run find-dired on specified switches after prompting for the
directory to where find is to be launched." t nil)

(autoload (quote emacspeak-wizards-use-w3-or-w3m) "emacspeak-wizards" "\
Alternates between using W3 and W3M for browse-url." t nil)

(autoload (quote emacspeak-customize) "emacspeak-wizards" "\
Customize Emacspeak." t nil)

(autoload (quote emacspeak-wizards-show-environment-variable) "emacspeak-wizards" "\
Display value of specified environment variable." t nil)

(autoload (quote emacspeak-wizards-squeeze-blanks) "emacspeak-wizards" "\
Squeeze multiple blank lines in current buffer." t nil)

(autoload (quote emacspeak-wizards-count-slides-in-region) "emacspeak-wizards" "\
Count slides starting from point." t nil)

(autoload (quote emacspeak-wizards-how-many-matches) "emacspeak-wizards" "\
If you define a file local variable
called `emacspeak-occur-pattern' that holds a regular expression
that matches  lines of interest, you can use this command to conveniently
run `how-many' to count  matching header lines.
With interactive prefix arg, prompts for and remembers the file local pattern." t nil)

(autoload (quote emacspeak-wizards-occur-header-lines) "emacspeak-wizards" "\
If you define a file local variable called
`emacspeak-occur-pattern' that holds a regular expression that
matches header lines, you can use this command to conveniently
run `occur' to find matching header lines. With prefix arg,
prompts for and sets value of the file local pattern." t nil)

(autoload (quote emacspeak-switch-to-previous-buffer) "emacspeak-wizards" "\
Switch to most recently used interesting buffer." t nil)

(autoload (quote emacspeak-kill-buffer-quietly) "emacspeak-wizards" "\
Kill current buffer without asking for confirmation." t nil)

(autoload (quote emacspeak-wizards-spot-words) "emacspeak-wizards" "\
Searches recursively in all files with extension `ext'
for `word' and displays hits in a compilation buffer." t nil)

(autoload (quote emacspeak-wizards-fix-typo) "emacspeak-wizards" "\
Search and replace  recursively in all files with extension `ext'
for `word' and replace it with correction.
Use with caution." t nil)

(autoload (quote emacspeak-wizards-fix-read-only-text) "emacspeak-wizards" "\
Nuke read-only property on text range." t nil)

(autoload (quote emacspeak-wizards-vc-viewer) "emacspeak-wizards" "\
View contents of specified virtual console." t nil)

(autoload (quote emacspeak-wizards-vc-viewer-refresh) "emacspeak-wizards" "\
Refresh view of VC we're viewing." t nil)

(autoload (quote emacspeak-wizards-vc-n) "emacspeak-wizards" "\
Accelerator for VC viewer." t nil)

(autoload (quote emacspeak-wizards-google-hits) "emacspeak-wizards" "\
Filter Google results after performing search to show just the
hits." t nil)

(autoload (quote emacspeak-wizards-google-transcode) "emacspeak-wizards" "\
View Web through Google Transcoder." t nil)

(autoload (quote emacspeak-wizards-find-longest-line-in-region) "emacspeak-wizards" "\
Find longest line in region.
Moves to the longest line when called interactively." t nil)

(autoload (quote emacspeak-wizards-find-longest-paragraph-in-region) "emacspeak-wizards" "\
Find longest paragraph in region.
Moves to the longest paragraph when called interactively." t nil)

(autoload (quote emacspeak-wizards-find-grep) "emacspeak-wizards" "\
Run compile using find and grep.
Interactive  arguments specify filename pattern and search pattern." t nil)

(autoload (quote emacspeak-wizards-show-face) "emacspeak-wizards" "\
Show salient properties of specified face." t nil)

(autoload (quote emacspeak-wizards-voice-sampler) "emacspeak-wizards" "\
Read a personality  and apply it to the current line." t nil)

(autoload (quote emacspeak-wizards-generate-voice-sampler) "emacspeak-wizards" "\
Generate a buffer that shows a sample line in all the ACSS settings
for the current voice family." t nil)

(autoload (quote emacspeak-wizards-tramp-open-location) "emacspeak-wizards" "\
Open specified tramp location.
Location is specified by name." t nil)

(autoload (quote emacspeak-wizards-speak-iso-datetime) "emacspeak-wizards" "\
Make ISO date-time speech friendly." t nil)

(autoload (quote emacspeak-wizards-toggle-mm-dd-yyyy-date-pronouncer) "emacspeak-wizards" "\
Toggle pronunciation of mm-dd-yyyy dates." t nil)

(autoload (quote emacspeak-wizards-units) "emacspeak-wizards" "\
Run units in a comint sub-process." t nil)

(autoload (quote emacspeak-wizards-rivo) "emacspeak-wizards" "\
Rivo wizard.
Prompts for relevant information and schedules a rivo job using
  UNIX AT scheduling facility.
RIVO is implemented by rivo.pl ---
 a Perl script  that can be used to launch streaming media and record
   streaming media for  a specified duration." t nil)

(autoload (quote emacspeak-wizards-show-commentary) "emacspeak-wizards" "\
Display commentary. Default is to display commentary from current buffer." t nil)

(autoload (quote emacspeak-wizards-unhex-uri) "emacspeak-wizards" "\
UnEscape URI" t nil)

(autoload (quote emacspeak-wizards-add-autoload-cookies) "emacspeak-wizards" "\
Add autoload cookies to file f.
Default is to add autoload cookies to current file." t nil)

(autoload (quote emacspeak-wizards-popup-input-buffer) "emacspeak-wizards" "\
Provide an input buffer in a specified mode." t nil)

(autoload (quote emacspeak-wizards-find-emacspeak-source) "emacspeak-wizards" "\
Like find-file, but binds default-directory to emacspeak-directory." t nil)

;;;***

;;;### (autoloads (emacspeak-xml-shell) "emacspeak-xml-shell" "emacspeak-xml-shell.el"
;;;;;;  (17751 59933))
;;; Generated autoloads from emacspeak-xml-shell.el

(defgroup emacspeak-xml-shell nil "XML browser for the Emacspeak desktop." :group (quote emacspeak))

(autoload (quote emacspeak-xml-shell) "emacspeak-xml-shell" "\
Start Xml-Shell on contents of system-id." t nil)

;;;***

;;;### (autoloads (emacspeak-xslt-xml-url emacspeak-xslt-url emacspeak-xslt-use-wget-to-download
;;;;;;  emacspeak-xslt-region emacspeak-xslt-options) "emacspeak-xslt"
;;;;;;  "emacspeak-xslt.el" (17751 59934))
;;; Generated autoloads from emacspeak-xslt.el

(defvar emacspeak-xslt-directory (expand-file-name "xsl/" emacspeak-directory) "\
Directory holding XSL transformations.")

(defvar emacspeak-xslt-options "--html --nonet --novalid" "\
Options passed to xsltproc.")

(autoload (quote emacspeak-xslt-region) "emacspeak-xslt" "\
Apply XSLT transformation to region and replace it with
the result.  This uses XSLT processor xsltproc available as
part of the libxslt package." nil nil)

(defvar emacspeak-xslt-use-wget-to-download nil "\
Set to T if you want to avoid URL downloader bugs in libxml2.
There is a bug that bites when using Yahoo Maps that wget can
work around.")

(autoload (quote emacspeak-xslt-url) "emacspeak-xslt" "\
Apply XSLT transformation to url
and return the results in a newly created buffer.
  This uses XSLT processor xsltproc available as
part of the libxslt package." nil nil)

(autoload (quote emacspeak-xslt-xml-url) "emacspeak-xslt" "\
Apply XSLT transformation to XML url
and return the results in a newly created buffer.
  This uses XSLT processor xsltproc available as
part of the libxslt package." nil nil)

;;;***

;;;### (autoloads (tts-eflite) "flite-voices" "flite-voices.el" (17751
;;;;;;  59934))
;;; Generated autoloads from flite-voices.el

(autoload (quote tts-eflite) "flite-voices" "\
Use eflite TTS server." t nil)

;;;***

;;;### (autoloads (voice-setup-toggle-silence-personality turn-on-voice-lock
;;;;;;  voice-lock-mode) "voice-setup" "voice-setup.el" (17751 59958))
;;; Generated autoloads from voice-setup.el

(autoload (quote voice-lock-mode) "voice-setup" "\
Toggle Voice Lock mode.
With arg, turn Voice Lock mode on if and only if arg is positive.

This light-weight voice lock engine leverages work already done by
font-lock.  Voicification is effective only if font lock is on." t nil)

(autoload (quote turn-on-voice-lock) "voice-setup" "\
Turn on Voice Lock mode ." nil nil)

(autoload (quote voice-setup-toggle-silence-personality) "voice-setup" "\
Toggle audibility of personality under point  .
If personality at point is currently audible, its
face->personality map is cached in a buffer local variable, and
its face->personality map is replaced by face->inaudible.  If
personality at point is inaudible, and there is a cached value,
then the original face->personality mapping is restored.  In
either case, the buffer is refontified to have the new mapping
take effect." t nil)

;;;***

;;;### (autoloads (xml-reformat-tags insert-xml read-xml) "xml-parse"
;;;;;;  "xml-parse.el" (17751 59934))
;;; Generated autoloads from xml-parse.el

(autoload (quote read-xml) "xml-parse" "\
Parse XML data at point into a Lisp structure.
See `insert-xml' for a description of the format of this structure.
Point is left at the end of the XML structure read." nil nil)

(autoload (quote insert-xml) "xml-parse" "\
Insert DATA, a recursive Lisp structure, at point as XML.
DATA has the form:

  ENTRY       ::=  (TAG CHILD*)
  CHILD       ::=  STRING | ENTRY
  TAG         ::=  TAG_NAME | (TAG_NAME ATTR+)
  ATTR        ::=  (ATTR_NAME . ATTR_VALUE)
  TAG_NAME    ::=  STRING
  ATTR_NAME   ::=  STRING
  ATTR_VALUE  ::=  STRING

If ADD-NEWLINES is non-nil, newlines and indentation will be added to
make the data user-friendly.

If PUBLIC and SYSTEM are non-nil, a !DOCTYPE tag will be added at the
top of the document to identify it as an XML document.

DEPTH is normally for internal use only, and controls the depth of the
indentation." nil nil)

(autoload (quote xml-reformat-tags) "xml-parse" "\
If point is on the open bracket of an XML tag, reformat that tree.
Note that this only works if the opening tag starts at column 0." t nil)

;;;***

;;;### (autoloads (cd-tool) "cd-tool" "cd-tool.el" (17751 59923))
;;; Generated autoloads from cd-tool.el

(autoload (quote cd-tool) "cd-tool" "\
Front-end to CDTool.
Bind this function to a convenient key-
Emacspeak users automatically have
this bound to <DEL> in the emacspeak keymap.

Key     Action
---     ------

+       Next Track
-       Previous Track
SPC     Pause or Resume
e       Eject
=       Shuffle
i       CD Info
p       Play
s       Stop
t       track
c       clip
cap C   Save clip to disk
" t nil)

;;;***
