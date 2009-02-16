;;; EMPI-MPC.EL --- Backend to MPD for EMPI using MPC

(require 'empi-defutils)
(require 'empi-proc)

(defgroup empi-mpc-backend nil
  "Backend to MPD for EMPI using MPC."
  :prefix "empi-mpc-" :group 'empi)

(defcustom empi-mpc-db-root (getenv "MPD_DB_ROOT")
  "*MPD database directory root for use by empi-mpc.
Defaults to the value of the environment variable MPD_DB_ROOT,
and nil if this is absent."
  :type '(directory) :group 'empi-mpc-backend)

(defcustom empi-mpc-plentry-format
  "< [[%time%]|[--:--]] > [[%title%]|[%file%]][ - %artist%][ \
- %album%][ - Track %track%]"
  "Format used for titles by the MPD backend for EMPI using MPC.
The string is passed as the --format parameter to the mpc command line."
  :type 'string :group 'empi-mpc-backend)

(defcustom empi-mpc-title-format
  "[[%title%]|[%file%]][ - %artist%][ - %album%][ - Track %track%]"
  "Format used for playlist entries by the MPD backend for EMPI using MPC.
The string is passed as the --format parameter to the mpc command line."
  :type 'string :group 'empi-mpc-backend)

(defun empi-mpc-toggle-str (ctx cmd &rest args)
  (toggle-str args "on" "off"))

(defun empi-mpc-enqueue-args (ctx cmd &rest args)
  (if args
      (mapcar
       '(lambda (item)
	  (file-relative-name (file-truename item) empi-mpc-db-root)) args)
    (error "Insufficient parameters for action enqueue")))

(defvar empi-mpc-status ["\\(?:\\(.+?\\)
\\)?\\(?:\\[p\\(\\(?:laying\\)\\|\\(?:aused\\)\\)]\
 +#\\([0-9]+\\)/\\([0-9]+\\)\
 +\\([0-9]+\\):\\([0-9]+\\) +(\\([0-9]+\\)%)
\\)?\\(?:volume: *\\([0-9]+\\)% +repeat: *\\(\\(?:off\\)\\|\\(?:on\\)\\)\
 +random: *\\(\\(?:off\\)\\|\\(?:on\\)\\)\\)" :playingp (safe-streq -2 "laying")
:pausedp (safe-streq -2 "aused") :qplpos (1- (ston 3)) :qpllength (ston 4)
:qtime (+ (* (ston 5) -60000) (* (ston 6) -1000)) :qtime% (ston 7)
:qvolume (list (ston 8) (ston 8))
:qrepeat (streq 9 "on") :qshuffle (streq 10 "on")])

(defvar empi-mpc-command (vconcat empi-mpc-status [nil t]))
(defvar empi-mpc-title (vconcat empi-mpc-status [:qtitle 1]))

(defvar empi-mpc-titles
  '(lambda (str pexit ctx cmd &rest args)
     (list-all-occur "#[0-9]+) \\(.*\\)\n" str (car args))))

(defvar empi-mpc 'empi-proc-command)
(setplist 'empi-mpc
`(:ihandle "mpc" :defhandler ,(cons nil empi-mpc-status)
  :defnilout ,empi-mpc-command :defout ,empi-mpc-command
  :jumpitemnum (("play" empi-adjust-pos)) :jumptime% (("seek" 1))
  :voladd (("volume" ["+" 1])) :volsub (("volume" ["-" 1]))
  :play "play" :pause "pause" :stop "stop"
  :repeat (("repeat" empi-mpc-toggle-str))
  :shuffle (("random" empi-mpc-toggle-str)) :plback "prev" :plnext "next"
  :enqueue (("add" empi-mpc-enqueue-args) . ["." nil $<]) :plclear "clear"
  :qtitle (("status" "--format" ,empi-mpc-title-format) . ,empi-mpc-title)
  :qrepeat nil :qshuffle nil :qvolume nil :pausedp nil :playingp nil :qtime nil
  :qtime% nil :qplpos nil :pldel (("del" empi-adjust-pos) . ["" t])
  :plmove (("move" empi-adjust-pos) . ["" (not (prednum $?))])
  :qpllength nil :version "version" :misc ((1) . ["" $<])
  :qsonglength (("status" "--format" "%time%") .
		["^\\([0-9]+\\):\\([0-9]+\\)" nil
		 (+ (* (ston 1) -60000) (* (ston 2) -1000))])
  :qpltitles (("playlist" "--format" ,empi-mpc-plentry-format)
	      . ,empi-mpc-titles)
  :qplfiles (("playlist" "--format" "%file%") . ,empi-mpc-titles)))

(provide 'empi-mpc)

;;; EMPI-MPC.EL ends here
