;;; EMPI-XMMS-SHELL.EL --- Backend to XMMS for EMPI using xmms-shell

(require 'empi-defutils)
(require 'empi-proc)

(defun empi-xmms-shell-toggle-str (ctx cmd &rest args)
  (toggle-str args "on" "off" "toggle"))

(defvar empi-xmms-shell-vol
  ["The volume of the left channel has been set to \\([-+0-9]+\\)
The volume of the right channel has been set to \\([-+0-9]+\\)"
			     nil $< :qvolume (list (ston 1) (ston 2))])

(defvar empi-xmms-shell-status
  '("status" . ["\\(?:\\(\\(?:Playing\\)\\|\\(?:Current Song\\)\\): \\(.*?\\)\
\\)\\(?:\\(?: (\\([0-9]+\\) kbps, \\([0-9]+\\) hz, \\([0-9]+\\) channels)\\)?
\\)\\(?:Time: \\([0-9]+\\):\\([0-9]+\\.[0-9]+\\)\\)?\\(?:\\( (paused)\\)?
\\)?\\(?:Repeat mode: \\(\\(?:off\\)\\|\\(?:on\\)\\)
\\)?\\(?:Shuffle mode: \\(\\(?:off\\)\\|\\(?:on\\)\\)
\\)?\\(?:Balance: \\([-+0-9]+\\)
\\)?\\(?:Skin: \\(.*\\)
\\)?\\(?:Left Volume: \\([0-9]+\\)
Right Volume: \\([0-9]+\\)\\)?" :playingp (streq 1 "Playing")
:qtitle 2 :qbitrate (* (ston 3) -1000) :qfrequency (ston 4) :qchannels (ston 5)
:qtime (+ (* (ston 6) -60000) (* (ston 7) -1000)) :pausedp (numpred (stringp -8))
:qrepeat (streq 9 "on") :qshuffle (streq 10 "on") :qbalance (ston 11)
:qguiskin 12 :qvolume (list (ston 13) (ston 14))]))

(defvar empi-xmms-shell-titles
  '(lambda (str pexit ctx cmd &rest args)
     (list-all-occur "[ \t]*\\*?[ \t]*[0-9]+\\. \\(.*\\)\n" str (car args))))

(defvar empi-xmms-shell 'empi-proc-command)

;; Note: some of the info reported by executing actions with xmms-shell could be fake.
;; Eg.	:qplpos reported by "backward"/"forward" - it just inc/decrements the number.
;;	This fails in case there are bad entries in the playlist.
;;	:qbalance on "balance" - allows values like -200 and outputs them !

(setplist 'empi-xmms-shell
`(:ihandle "xmms-shell" :prefix "-e" :defhandler ,empi-xmms-shell-status
  :jumpitemnum (["jump " empi-adjust-pos]) :exit "xmmsexit"
  :voladd (["+ " 1] . ,empi-xmms-shell-vol) :play ("play" . ["Playback "])
  :volsub (["- " 1] . ,empi-xmms-shell-vol) :stop ("stop" . ["Playback "])
  :plback "backward" :plnext "forward" :qbalance nil :qtitle nil :qtime nil
  :pause ("pause" . ["\\(?:Playback\\( u?\\)n?paused\\)\\|\\(?:Pause\\)" nil $<
		     :playingp (safe-streq 1 " u") :pausedp (safe-streq 1 " ")])
  :repeat (["repeat " empi-xmms-shell-toggle-str] .
	   ["mode is now" nil $< :qrepeat $?]) :enqueue (["load \"" 1 "\""])
  :shuffle (["shuffle " empi-xmms-shell-toggle-str] .
	    ["mode is now" nil $< :qshuffle $?]) :gui-prefs ("preferences")
  :qplpos ("currenttrack" . ["Current song: \\([0-9]+\\)\\. \\(.*\\)"
			     nil (1- (ston 1)) :qtitle 2]) :gui-enqueue "eject"
  :plclear ("clear") :qplfiles ("list filenames" . ,empi-xmms-shell-titles)
  :qpltitles ("list" . ,empi-xmms-shell-titles) :pausedp nil :playingp nil
  :balance (["balance " 1] . ["" nil $<]) :qgui-skin nil :qvolume nil
  :gui-toggle-show-main ["window main " empi-xmms-shell-toggle-str]
  :gui-toggle-show-playlist ["window playlist " empi-xmms-shell-toggle-str]
  :gui-toggle-show-equalizer ["window equalizer " empi-xmms-shell-toggle-str]
  :gui-toggle-show-all ["window all " empi-xmms-shell-toggle-str]
  :qbitrate nil :qfrequency nil :qchannels nil :qrepeat nil :qshuffle nil
  :pldel ["remove " empi-adjust-pos]))

(provide 'empi-xmms-shell)

;;; EMPI-XMMS-SHELL.EL ends here
