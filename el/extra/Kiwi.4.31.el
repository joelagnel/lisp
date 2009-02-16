;(set (make-local-variable 'debug-on-error) t)
;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Kiwi.el --- A user interface for the Internet Relay Chat
(defconst irc-version "Kiwi v4.31 for GNU Emacs" "Current Kiwi version.")
(defconst irc-ftp-version "4.31" "Current FTP'able version.")
(defconst irc-ftp-file-names (list (format "Kiwi.%s.el.Z" irc-ftp-version)
				 "Kiwi.README"))
(defconst irc-ftp-source (list (list "NIC.FUNet.FI" "pub/unix/irc/Emacs"
				     irc-ftp-file-names)))
(defconst irc-hacker "Klaus.Zeuge@Student.DoCS.UU.SE"
  "Email address of the person to send complaints and ideas too.")
;;;;;;;;;
;; v4.31 Tue Oct 19 20:59:11 1993	sojge@Student.DoCS.UU.SE
;;	- BUGFIX: don't crash if CTCP quote is last in string.
;;	- BUGFIX: name buffer accoring to connected TCP-port, not default
;;	  TCP-port.
;;	- Better handling of channel topic display.
;;	- Added handling of ERR 422 and 443, and RPL 249.
;;	- Clean up CTCP VERSION reply before displaying it.
;; v4.30 Tue Aug 24 20:52:21 1993	sojge@Student.DoCS.UU.SE
;;	- BUG FIX: erroneous format string fixed when handling CTCP ECHO reply.
;;	- Bug fix in irc-execute-notify, don't try to irc-forget a non nick.
;;	- Added backtalk to documentation of /signal.
;;	- Made variable irc-port (ie the TCP port used to connect to an IRC
;;	  server) buffer local so different sessions can use diffferent ports.
;;	- Added handling of RPL 392.
;;	- Better handling of RPL 322.
;;	- Now cleans up QUIT messages.
;;	- Added handling ERR 422, no MOTD file.
;;	- Minor betterification in character mapping.
;; v4.29 Thu Aug 12 03:24:57 1993	sojge@Student.DoCS.UU.SE
;;	- If DEL pressed while point is at first postion in input region,
;;	  scroll down page instead of beep.
;;	- More channel modes explained in irc-explain-mode (k and v).
;;	- Force emacs to offer to save the Kiwi buffer before exiting emacs.
;;	- Splitted up irc-parse-RPL so it's byte-compile'able with v18 emacs.
;;	- Remove the CTCP MOOD command as no one else used it.
;;	- As there are a lot of old clients out there, send several CTCP pings.
;;	- Unless debug-on-error is non-nil, don't crash when finding dots in
;;	  host names, nicks etc.
;;	- New function irc-time-diff used to not send ISON commands more often
;;	  than every 60 second.
;;	- New handling of topic, see /HELP TOPIC.
;;	- Don't care what user tries to /VERSION, let server decide if it
;;	  makes sense.
;;	- Added handling of RPL 305, 306, 371, 374, 392, 394.
;;	- Added handling of RPL 242, 243, 244, 253, 261.
;;	- New format in TRACE output in RPL 200, 211.
;;	- Better parsing of "hello this is ircd version xxx" string at startup.
;;	- Added handling of ERR 436, 444, 445, 446, 467, 475.
;;	- Some minor changes in the CTCP handling. Nothing important.
;;	- make-local-variable doesn't set the variable in emacs v19, so needed
;;	  to add a set in several places. NOW RUNS WITH GNU EMACS VERSION 19.
;;	- Fumbled around some more with character set convertings. Should go
;;	  the v19 way, in the future.
;; v4.28 Mon Apr 19 15:48:45 1993	sojge@Student.DoCS.UU.SE
;;	- Made client somewhat compliant to ircd 2.8.
;;	- Added some (as of yet incomplete) support for different character
;;	  sets like ISO 8859-1 and SIS E47.
;;	- Changed output of /WHO
;;	- Bugfixed irc-lowlevel-dequote to not crash when getting erroneous
;;	  messages breaking the CNTRL/P quoting.
;;	- Bugfixed irc-execute-event.
;;	- Added CTCP MOOD. No /MOOD command yet, though.
;;	- Clean up a users full name in WHO replies.
;; v4.27 Mon Sep 14 19:17:23 1992	sojge@Student.DoCS.UU.SE
;;	- Better handling of versions through irc-ftp-* variables.
;;	- Added variable irc-show-japanese-characters to get better control
;;	  about in what way japanese characters are inserted into buffer.
;;	- Spiffed up CTCP VERSION handling to comply to CTCP.mem v13.
;;	- Added flag irc-drop-ircII-text-kludge-control-characters for those
;;	  users who WANT to see ^B, ^V and ^_ in text.
;;	- Ignore CNTRL/B, CNTRL/V and CNTRL/_ in incoming messages, as those
;;	  are used by the ircII client to toggle reverse, underline and bold
;;	  attributes of text.
;;	- Added variable irc-emacs-knows-ISO8859-1.
;;	- Fixed timing problem when receiving a PRIVMSG just after changing
;;	  the nickname of oneself.
;; v4.26 Thu Aug 20 05:17:00 1992	sojge@Student.DoCS.UU.SE
;;	- Assorted small fixes.
;;	- Some new table headers (like for WHO).
;;	- Added RPL369, end of WHOWAS.
;;	- Added L line reply handling (RPL241).
;;	- Be nicer when sending automatic warnings.
;;	- Better handling of CTCP PING replies.
;;	- Reordered most global variable initialisations.
;; v4.25 Fri May 29 21:00:32 1992	sojge@Student.DoCS.UU.SE
;;	- Added user modes (eg /mode sojge -o)
;;	- Handles display of a channel's ban list (ie reply to /mode #x -b).
;;	- Better handling of some STATS replies.
;;	- Fixed irc-parse-quit.
;;	- Fixed irc-parse-kick.
;;	- Take care of v2.7.2 message ERROR: Closing Link.
;;	- Added CTCP commands ECHO, PING, SOURCE.
;;	- Started on a crude but better /IGNORE command.
;;	- Made Kiwi display unknown NOTICE's in block style.
;;	- Added /PING command, based on CTCP. Coded by Kimmo.Suominen@lut.fi.
;;	- Started on inout and output convertion tables to displat different
;;	  character sets on different equipment.
;; 4.24 Mon Feb 24 17:01:09 1992	Klaus-Zeuge@DoCS.UU.SE
;;	- Still more RPL312 hacking. Hopefully looks OK on a 2.6.2 server now.
;; 4.23 Sat Feb 22 20:57:27 1992	Klaus.Zeuge@DoCS.UU.SE
;;	- Added real version number and edit history.
;; 4.22 Sat Feb 22 17:23:41 1992	Klaus.Zeuge@DoCS.UU.SE
;;	- Bugfixed CTCP VERSION reply from ircII.
;; 4.21 Sat Feb 22 16:38:05 1992	Klaus.Zeuge@DoCS.UU.SE
;;	- Removed annoying bug which mixed up different hash tables.
;;	- /mode can set modes for a user.
;;	- Added Q and Y to /stats.
;;	- /notify, a new command helping to see when your friends enter IRC.
;;	- /topic understands #channels, both when querying and setting.
;;	- /whois takes an optional first argument with name of server which
;;	  should answer the whois question.
;;	- /quit now takes an optional reason.
;;	- Made /quote send an illegal command to server to help synchronizing.
;;	- Created variables irc-notify-looked-for and irc-notify-detected.
;;	- Added user mode in irc-explain-mode, added frontend functions
;;	  irc-explain-user-mode and irc-explain-channel-mode.
;;	- Cope with different RPL312 styles.
;;	- Doing a somewhat better job at parsing ircII's ugly CTCP VERSION
;;	  replies.
;;	- Cope with new RPL201-209, 211, 212, 213-219, 221.
;;	- Cope with some new NOTICE's.
;;	- Cope with new format of "Received SQUIT" NOTICE.
;;	- Cosmetic change in presenting other persons QUIT reasons.
;;	- Renamed irc-notifies into irc-events (lessens confusion with /notify)
;;	- Recognize the 500 series of ERR's from server.
;;	- Do a a sleep-for after EVERY received line from server.
;;	- Moved logging in *debug* buffer into irc-log-in-debug-buffer.
;; v4.20 Wed Dec 18 06:33:57 1991	Klaus.Zeuge@DoCS.UU.SE
;;	- Change a!b@c on incoming messages into a in irc-parse-server-msg.
;;	  (Can you say kludge? I *knew* you could!)
;;	- Added RPL353 and RPL352 as front ends for NAMREPLY and WHOREPLY.
;;	- Parse 2.7(.0) version string correctly.
;;	- Better value for irc-msg-cont-used when reporting a channels mode.
;;	- Added Kimmo's code for detecting people on IRC using the ISON
;;	  mechanism, irc-execute-notify. Also his irc-notify-list etc.
;;	- Added Kimmo's code for /ME and /DESCRIBE commands for CTCP ACTION.
;;	- Added Kimmo's code for recognizing CTCP ACTION commands.
;;	- Added fixes to keep Kiwi.el from choking on 2.7 messages.
;;	  Fixed RPL messages are: 206, 209, 301, 304, 352, 353, 354,
;;	  367, 368, 371, 373, 383, 384. Thank you Kimmo Suominen <kim@lut.fi>
;;	  for supplying fixes.
;;	- Made comand "/join #channel +p" and friends kosher.
;;	- Added : as /MSG like command again after getting requests for it.
;;	- Threw out my irc-current-time based on writing irc-idle-scratch-file
;;	  and replaced it by one donated by Stephen Ma <ma_s@maths.su.oz.au>.
;;	- Removed bug in irc-execute-signal.
;;	- Fixed parse error in irc-parse-pong.
;;	- More defensive handling of RPL319.
;; v4.19 Wed Nov 13 15:18:00 1991	Klaus.Zeuge@DoCS.UU.SE
;;	- Removed forgotten disabling of /KILL command. Ehum...
;;	- Use function make-temp-name when naming irc-idle-scratch-file.
;;	- Better formatting in /membership display.
;;	- Made irc-nuke-whitespace remove trailing spaces too.
;;	- Understand USERHOST replies (RPL302).
;;	- More 442 messages understood. Sigh.
;;	- Incoming TOPIC messages might contain channel name field.
;;	- USERS reply might contain word "display". Still valid.
;;	- Cope with 2.6.2e type of TRACE link lines.
;;	- "Understand" reason field in incoming QUIT messages.
;;	- Added Kims (kim@lut.fi) fix for 2.6.2e type of LUSER reply.
;;	- Fixed bug wtih dropped first character when user says "/mode * o u".
;; v4.18 Tue Oct 22 06:46:49 1991	Klaus.Zeuge@DoCS.UU.SE
;;	- Moved creation of irc-count-incoming-messages to irc-mode from
;;	  irc-parse-server-msg.
;;	- Changed irc-truncate-buffer to chop down to irc-minimum-size whenever
;;	  buffer has grown to irc-maximum-size.
;;	- Made irc-maximum-size = 64KB.
;;	- Added irc-minimum-size = 32KB.
;;	- Made most hash table twice as big.
;; v4.17 Sun Oct 20 17:05:01 1991	Klaus.Zeuge@DoCS.UU.SE
;;	- Rewrote most calls to string-match so they don't depend on . in
;;	  regexps, as . doesn't match \n.
;;	- Made CTCP handling more iterative (recursive handling turned out
;;	  to exhaust stacks in worst cases).
;;	- Made a few more variables buffer local.
;;	- Added low level quoting of \000, \n, \r, \020 and changed CTCP level
;;	  quoting to use only \001 and \\.
;;	- Better irc-execute-stats.
;;	- Replaced all "\\s " by " " and all "\\S " by "[^: ]". Hopefully
;;	  works better.
;;	- Crude RPL303 handling (reply to ISON).
;;	- Ripped out irc-maintain-tree! Hooray! Replaced by hash table code
;;	  BUT named the new functions irc-remember, irc-recall, irc-forget.
;;	  This speeded up /NAMES from some 5 minutes to some 10 seconds.
;; v4.16 Tue Sep 17 21:07:01 1991	Klaus.Zeuge@DoCS.UU.SE
;;	- When seeing idle time from IrcII clients, translate the large number
;;	  of seconds into days, hours, minutes and seconds.
;;	- Writing to /dev/null to keep track of idle isn't portable. Use a
;;	  temporary file in the users home directory instead.
;; v4.15 Thu Aug 22 13:46:03 1991	sojge@mizar.docs.uu.se
;;	- If user connects to server at host foo.bar.se and foo.se is an
;;	  alias for rela.name.se, Kiwi gets confused. Now irc-server is
;;	  set to real (ie generic) name from the servers Welcome message.
;;	- Fixed the macro subfield so it won't evaluate at compile time.
;;	  Sigh. Stupid sojge. Thank you Lennart Staflin, Link|ping.
;;	- Fixed irc-parse-RPL: 317 does NOT give victims nick.
;;	- Fixed irc-maintain-tree to not crash when removing a service.
;;	- Added "Notice unauthorized connect" in irc-parse-notice.
;;	- Added CTCP query and reply for "FINGER". Hopefully this is added in
;;	  a portable way, but I don't really KNOW ... :-(
;;	- Made irc-is-receiver recognize foo@bar.dom as valid receiver.
;; v4.14 Mon Aug 12 10:15:05 1991	sojge@mizar.docs.uu.se
;;	- Fixed /MODE command to work even when saying "/mode ..." in the
;;	  buffer, not only when using C-c m.
;;	- Fixed some ERR-reply parsing code to grok new format.
;;	- Added /LEAVE 0 to leave all channels, and made /JOIN 0 just stop
;;	  talking to the current channel.
;;	- Better nickname control, new scheme with irc-nick-used to keep track
;;	  of what nick we have.
;;	- Renamed irc-linkstree into irc-linksinfo.
;;	- Kludged the mess with 2.6 servers first being names 2.6pre19,
;;	  later 2.6.1a so the latter gets a HIGHER irc-edit-version number.
;;	- Parse RPL 364 messages, ie answers to /LINKS in 2.6.1 servers.
;;	- For now, ignore message 318.
;; v4.13 Thu Jun 13 10:28:31 1991	sojge@mizar.docs.uu.se
;;	- Handle ERR_NICKNAMEINUSE (433) at "login" time.
;;	- More homogenous command parsing (both /-style and keybound).
;;	- Cache subscribed channel names.
;;	- Name  Kiwi buffer "*Kiwi-<server>*".
;;	- Always run in "slow speed terminal" mode.
;;	- Shorter messages displayed when seeing a KILL, skipped path.
;;	- Renamed irc-hosttree into irc-servernames.
;;	- Added code to put communication in -*debug* buffer IFF it exists.
;;	- Changed regexp for host names yet again.
;;	- When getting the windows width, make sure it's the Kiwi window.
;;	- Prepared function irc to handle several sessions.
;;	- Handle some 2.6pre19 messages (324, MODE, 403).
;;	- Bound C-c i to /info.
;;	- Changed C-c m from /motd to /mode.
;;	- Fixed misfeature for v18.57 GNU Emacs. Several return values from
;;	  function process-status are valid. Don't start a new session when
;;	  an old one is running.
;;	- Fixed bug in showing LIST or NAMES reply of Kanjii channel.
;;	- Clean up channel names as they can contain cntrl-chars too.
;;	- Fixed display of time to be nicer.
;;	- Make the (interactive)-part of the irc-execute-* commands more alike
;;	  each other.
;;	- Bugfix: understand ESC in channel names in a few more places.
;;	- Bugfix: understand "i" etc, not only "+i" when parsing MODE.
;;	- Made /COMMAND be shown in buffer when doing command by keys.
;;	- Amazingly how yours truly can mess up code! Fixed /LEAVE again :-)
;; v4.12 Sun Mar  3 02:45:21 1991	sojge@mizar.docs.uu.se
;;	- Changed wording on ctcp (client-to-client protocol) answer for
;;	  a VERSION query.
;;	- Added C-c C-k to do irc-execute-kill.
;;	- Bugfixed irc-execute-leave so it works when typing /LEAVE not just
;;	  when typing C-c q.
;;	- Better handling of /STATS C replies from remote servers.
;;	- Handle error after /kick user when user's not on channel.
;; v4.11 Fri Mar  1 16:31:04 1991	sojge@mizar.docs.uu.se
;;	- Bugfix, corrected misspelled variable for when replying to unkown
;;	  client-to-client PRIVMSG.
;; v4.10 Thu Feb 28 20:06:05 1991	sojge@mizar.docs.uu.se
;;	- New behaivour of /WHO, see /HELP WHO.
;;	- Various bugfixes. (Mostly due to bound keys).
;;	- Replaced irc-read-user/host by irc-read-object.
;; v4.9 Wed Feb 27 20:29:58 1991	sojge@mizar.docs.uu.se
;;	- Added quoting of messages between CNTRL/A:s.
;;	- Better grok of WALLOPS.
;;	- Remember correct spelling of a services nickname when seeing
;;	  NOTICES's from one.
;;	- Bugfixed /CONNECT which w/o "remoteserver" goofed.
;; v4.8 Wed Feb 27 03:57:50 1991	sojge@mizar.docs.uu.se
;;	- More initialisation help.
;;	- Changed client-to-client protocol in non-backward compatible way.
;;	- OK'ed "/version mizar*".
;;	- Better handling of /join 0 when on #channel. Updates irc-channel.
;;	- Changed handling of unknown NOTICE's to be treated as PRIVMSG's.
;;	- Fixed bug when replying to CLIENT ERRMSG.
;;	- Show irc-channel in status line.
;;	- Always send two ^A's, even if to queries/answers are in a row.
;;	- Made /msg grok broadcast addresses ($server, #host) when enabled.
;;	- Fixed bugs in /kick.
;;	- Better help for how to install your personal IRC initiation.
;; v4.7 Sun Feb  3 03:51:59 1991	sojge@mizar.docs.uu.se
;;	- Take care of some new styled NOTICE's from the server.
;;	- Cleaned up the code of irc-parse-priv.
;;	- Bugfixed irc-parse-kick to keep better track of where we are.
;;	- Added command /SERVICE.
;; v4.6 Fri Feb  1 04:03:31 1991	sojge@mizar.docs.uu.se
;;	- Better handling of /WHOWAS.
;;	- Changed string from "Kiwi Operator" into "Kiwi, Operator" while
;;	  enabled.
;; v4.5 Fri Feb  1 02:24:00 1991	sojge@mizar.docs.uu.se
;;	- Added command /USERINFO to set a string which can be queried by other
;;	  users. Augmented commands /INFO and /VERSION to query the user
;;	  settable string and to query some client version from a users client.
;;	- Added knowledge of some more NOTICE's from the server.
;;	- Better handling of STATS, both in giving command (/STATS) and
;;	  parsing L type replies (ie how much data). Added /HELP STATS.
;;	- Display another field in WHOIS/WHOWAS replies, showing servers text
;;	  field from message 312.
;;	- Added in irc-parse-notice case for "*** ..." messages.
;;	- Bugfixed irc-execute-help, so it deals with aliases and topics which
;;	  aren't commands.
;;	- Changed clients name from sojge-irc.el to Kiwi.el
;;	  (Kiwi initially was irc.el).
;;	- Added varibales irc-msg-info-pre and -post for letting user select
;;	  how information (default eg: [Foo has joined channel +Bar], the [
;;	  and ] characters) messages start and end.
;;	- Statistics display nicyfied.
;; v4.4 Mon Dec 17 17:51:24 1990	sojge@mizar.docs.uu.se
;;	- Added column alignment (irc-msg-cont-used) for RPL322 and RPL323
;;	  messages (RPL_LIST and RPL_LISTEND).
;;	- Bugfix: prevents client from crashing (when connected to a 2.6-server
;;	  and doing a TRACE for a nonexistent server) by adding some
;;	  code the for 402 ERR reply (ERR_NOSUCHSERVER).
;; v4.3 Sun Dec 16 19:19:00 1990	sojge@mizar.docs.uu.se
;;	- Added case for trace reply "UNKNOWN" in irc-parse-notice.
;;	- Added key C-c = to do a /MEMBER command.
;;	- Bugfix: changed general regexp in irc-parse-server-msg to not allow
;;	  spaces in the server part.
;;	- Made irc-execute-whois handle the /WHOWAS command.
;;	- Tell the clients version too, after a /VERSION.
;;	- Bugfix: rewrote irc-execute-topic to put the (interactive) "command"
;;	  in "topmode".
;;	- Save username in 2.6 kind of USER-traces.
;;	- Bugfix: Don't remove bogus irc-nick from irc-nicknames on startup
;;	  when setting first nick after default nick failed.
;; v4.2 Sat Dec 15 04:14:22 1990	sojge@mizar.docs.uu.se
;;	- Bugfix in irc-parse-server-msg: NOTICE's don't have a : before them
;;	  (except the optional :server part).
;;	- Can't set topic of a #channel, changed irc-execute-topic.
;;	- Bugfix: irc-parse-notice now recognizes IRC OPERs as users in traces.
;;	- Made "NOTICE -- Received kill" equivalente to older
;;	  "NOTICE: Received kill".
;; v4.1 Thu Dec 13 13:35:43 1990	sojge@mizar.docs.uu.se
;;	- Zapped irc-news to contain little, but current, information.
;;	- Corrected bug in alignment of output of who-replies for
;;	  servers w/o end-of-who markes (hello 2.6.pre14!) in
;;	  irc-parse-whoreply.
;;	- Added knowledge of some trace messages in irc-parse-notice.
;;	- Fixed bug in irc-parse-notice, so the number of invisble users
;;	  will be displayed if you're an enabled irc operator.
;;	  Thank's heu@byron.u.washington.edu.
;;	- Fixed irc-execute-links to only reset list of known hosts on
;;	  a LINKS of all the world. (Ie don't reset on LINKS *.SE).
;;	- Spiffed up sojge-irc's grok of which privmsg go where, changed
;;	  way irc-parse-priv talks to irc-parse-public.
;;	- Added /MEMBER to show which channels the user's listening and
;;	  talking to.
;;	- Indicate ircII's CNTRL/B's by *'s.
;;	- Scrapped [You have joined/left]-messages, instead tell user which
;;	  she's listening and writing to, if any.
;;	- Added NOTICE-pattern matching of OPER to be analogous to USER
;;	  and CHANOP.
;;	  Thank's kim@lut.fi.
;;	- Swapped key definitions of C-c C-q and C-c q.
;;	  Thank's kim@lut.fi.
;;	- Made irc-parse-public look for single old channel in
;;	  irc-subscribed-channels.
;;	- Added better support for irc-subscribed-channels.
;;	- irc-parse-RPL, 472: s/inventation/invitation/.
;;	- Made /WHOWASRPL's be written correctly.
;;	- Rewrote irc-parse-channel to be better in coping with #channels.
;;	- Added #channels to be recognized in irc-is-named-channel and in
;;	  irc-is-channelname.
;;	- Fixed typo in irc-parse-notice.
;; v4.0 Fri Nov 16 07:06:02 1990	sojge@mizar.docs.uu.se
;;	  Seems to work with both v2.5.1 and v2.6pre14 servers.
;;	- Made irc-execute-msg send with PRIVMSG is irc-channel is a #channel.
;;	- Added PART when on 2.6 server in irc-execute-leave.
;;	- Added JOIN when on 2.6 server in irc-execute-join.
;;	- Added 2.6 level reply message 317 in irc-parse-RPL.
;;	- Added 2.6 level commands JOIN and PART in irc-parse-server-msg and
;;	  irc-parse-channel.
;;	- On nonmatcher NOTICE, just give the notice to the user with out
;;	  frightening her with big WARNING signs.
;;	- Removed buggy string-match in irc-parse-notice for svarte being
;;	  matched against a terminal (rt).
;;	- irc-parse-RPL: set irc-msg-cont-used when receiving message 314
;;	  (RPL_WHOWASUSER).
;;	- Made confirmation when sending to several users/channels at once
;;	  being written one confirmation per line.
;; v3.14 Thu Nov  1 19:47:09 1990	sojge@mizar.docs.uu.se
;;	- Made /OOPS work without an argument, if non given, ask for one.
;;	- Added possible bugfix in irc-parse-RPL for message 322.
;;	- irc-execute-command: don't put "'s around help w/ possible commands.
;;	- irc-insert-most: use right-most argument of irc-choose-break-point.
;;	- irc-insert: use right-most argument of irc-choose-break-point.
;;	- irc-choose-break-point: use parameter right-most, not (window-width).
;;	- irc-parse-notice: slightly new "no users logged in" message.
;;	- irc-parse-notice: write "serverlink", not "connection" in /TRACES.
;;	- irc-parse-wallops: set irc-msg-cont-used to max window-width / 2.
;;	- irc-parse-priv: set irc-msg-cont-used to max window-width / 2.
;;	- irc-parse-public: set irc-msg-cont-used to max window-width / 2.
;;	- irc-parse-channel: prevent negative string length in pd.
;; v3.13 Tue Oct  9 04:35:51 1990	sojge@mizar.docs.uu.se
;;	- Sigh, when do I learn not to release code when being ++tired?
;;	  Fixed a newborn bug in irc-execute-msg.
;; v3.12 Tue Oct  9 03:49:14 1990	sojge@mizar.docs.uu.se
;;	- Made a quick and dirty bugfix to prevent crashing in irc-insert-more.
;;	  Sort of works. Sigh.
;;	- Fixed bug in irc-parse-server-msg, do accept NOTICEs from "strange"
;;	  origins...
;;	- Set global irc-msg-cont-used in irc-parse-RPL while parsing WHOIS
;;	  replies.
;;	- Removed two bugs in irc-insert-more.
;;	- Made /WHOIS STRING trying to get information even if STRING isn't
;;	  remembered in irc-nicknames as being present.
;;	- Made /WHO USER do a WHOIS USER, while /WHO CHANNEL does a
;;	  WHO CHANNEL.
;;	- Made irc-execute-wallops confirm if irc-confirm is non-nil.
;; v3.11 Thu Aug 30 17:41:18 1990	sojge@mizar.docs.uu.se
;;	- Sigh, fixed bug in irc-execute-whowas. I'd better do a RPLACA...
;; v3.10 Thu Aug 30 15:47:36 1990	sojge@mizar.docs.uu.se
;;	- Added irc-execute-whowas (long due).
;;	- Fixed som other minor bugs. I hope.
;;	- BUGFIX in irc-remove-from-tree, do comparsion in upcase.
;;	- Made irc-execute-msg tell if receivers are users or channels.
;;	- Fixed bug in irc-is-channel. ("-77?" is NOT a channel ID).
;;	- Recognize Kanji message and replace with generic string.
;; v3.9 Wed Aug 22 08:07:21 1990	sojge@mizar.docs.uu.se
;;	- Added a rudimentary function irc-execute-kick.
;;	- Wrote predicate irc-is-channelname.
;;	- Updated predicate irc-is-nickname to better reflect reality.
;;	- Made irc-execute-msg grok channel's as receipants.
;;	- Added simplistic irc-execute-mode function.
;;	- FIXED BUG in irc-find-to by rewriting it.
;;	- Made message 421 grok 2.5 messages too.
;;	- Added function irc-parse-kick.
;;	- Added some more (let ((irc-msg-cont-used ...) around irc-insert's.
;;	- Made irc-explain-mode work.
;;	- Changed count in irc-execute-send so irc-msg-sent is right justified.
;; v3.8 Sun Aug 19 04:55:31 1990	sojge@mizar.docs.uu.se
;;	- BUGFIX: v3.6 introduced bug making it hard to join negative channels,
;;	  now it's easy again.
;; v3.7 Sat Aug 18 23:48:56 1990	sojge@mizar.docs.uu.se
;;	- Set irc-msg-cont-used before calling irc-insert for some lists.
;;	- Understand ERR_NOTONCHANNEL message.
;;	- Don't explecitly request MOTD (message of the day) when connected
;;	  to v2.5 servers or later. They give it anyway.
;;	- Don't show the "no topic set" messages from remote old server
;;	  at topic changes.
;;	- Collect server names in irc-parse-RPL.
;;	- Added irc-split-lines to make better line splits in irc-insert.
;; v3.6 Fri Aug 17 04:41:19 1990	sojge@mizar.docs.uu.se
;;	- Changed format of how the status in a WHO listing appears.
;;	- Ignore error messages from other old server complaining MODE being
;;	  an unknown command.
;;	- Understands the WHO-reply statuses "channel operator"
;;	  and "channe operator and IRC operator".
;;	- Sort of understands reply message 316 .
;;	- Join channel 007, not +007 at "/join 007" (ie no prepending of + when
;;	  first letter in channelname is a digit).
;;	- Understand reply message 324.
;;	- Say "using default" after a "/query *".
;; v3.5 Thu Jun 14 07:23:51 1990	sojge@emil.csd.uu.se
;;	- Teached /KILL to understand comments.
;;	- Added a simplistic header to linreply display.
;;	- Make it possible for away messages to start with :.
;;	- Save nickname in irc-parse-wallops.
;; v3.4 Mon Jun 11 03:55:08 1990	sojge@emil.csd.uu.se
;;	- Added operator level command /CONNECT.
;;	- Remapped C-C C-S to be irc-execute-squit.
;;	- Added operator level command /SQUIT.
;;	- Added operator level command /WALLOPS.
;;	- Added some support for mnemonic channel ID's, ie not only numbers
;;	  but also strings can identify a channel. Probaly a bit buggy.
;;	  Problem: How long can an ID be?
;;	- Made irc-channel into a string, from being a number.
;;	- Made /WHO SOJGE show a one line WHO reply (plus header) about SOJGE,
;;	  ie not doing a WHOIS for SOJGE. Will probaly change.
;; v3.3 Wed Jun  6 07:26:57 1990	sojge@emil.csd.uu.se
;;	- Keep statistics on LIST replies in a new way.
;;	- Made irc-add-to-tree do it's string comparsions in upper case, this
;;	  prevents the same name in two different cases to be stored.
;;	- Corrected minor bug in irc-parse-notice about reconizing USERS
;;	  replies.
;;	- Changed (irc-insert "foo\nbar") into 
;;	  (irc-insert "foo") (irc-insert "bar")
;;	- Made the "IRC halted" message a bit more flashy.
;; v3.2 Mon Jun  4 19:59:00 1990	sojge@emil.csd.uu.se
;;	- When on a fast terminal (see irc-terminal-is-slow), don't bother
;;	  to collect various replies in temporary storage lists. Display the
;;	  unsorted entrys as fast as they come in.
;;	- Improved handling of /USERS replies.
;; v3.1 Sat May 26 03:05:46 1990	sojge@emil.csd.uu.se
;;	- Corrected bug which prevented LINK to actually display any data...
;; v3.0 Sat May 26 02:21:52 1990	sojge@emil.csd.uu.se
;;	- Changed version from 1.9 to 3.0 in one step.
;;	- Set irc-nick according to incoming NOTICE messages.
;;	- Made the WHOREPLY header be displayed when the first reply comes in.
;;	- Understand message 314, RPL_WHOWASUSER.
;;	- Renamed irc-nameslist (former irc-whotree) into irc-nicknames.
;;	- Added case for "no motd at server", both for 2.4 and pre 2.4 servers.
;;	- Beautified "IRC server xx runs version yy" message.
;;	- Added irc-execute-version.
;;	- Added irc-execute-motd.
;; v1.9 Sat May 19 14:44:58 1990	sojge@emil.csd.uu.se
;;	- Wrote irc-parse-pong to collect host name from ping messages.
;;	- Made irc-extract-hosts skip parts between between nodename and "!".
;;	- Made WHO lists more mnemonic.
;;	- Made LINREPLYs be gathered in a variable before sorting and
;;	  displaying it even for 2.2 type servers.
;;	- Make client ask for the message of the day (MOTD) when seeing
;;	  the (final line of the) servers welcome message, not at "start up".
;;	- Look as far as 1/3 of the line from the right when choosing a
;;	  break point in irc-insert. Makes a few listing look better on
;;	  a 80 column screen.
;;	- Take care of "unknown connection" message in irc-parse-notice.
;; v1.8 Thu May 17 00:53:20 1990	sojge@emil.csd.uu.se
;;	- Say "use /HELP" if user seems totaly confused on channel 0.
;;	- Fixed bug in irc-parse-whoreply, don't add header to irc-whotree.
;;	- Say "no users" on a 2.4 (or later) server if user issued /who 
;;	  for an empty channel.
;; v1.7 Wed May 16 23:34:01 1990	sojge@emil.csd.uu.se
;;	- Show time when being pinged, not every x minutes.
;;	- Don't crash if server closed connection, reopen it,
;;	- Skip some messages like START OF MOTD ...
;;	- Show what remote server, if any, a message came from.
;;	- Take care of a bunch of new NOTICE messages.
;;	- Understand new (server v2.4) type of welcome message.
;;	- Ignore empty lines from server.
;;	- Moved logic about format of different information keeping trees
;;	  into irc-maintain-tree.
;;	- Made (most) error messages start with a % sign.
;;	- Renamed IRC-WHOTREE into IRC-NAMESTREE.
;;	- Keeps track of what version the used server is, and depending on it
;;	  uses (or not) the fact that it's possible to gather WHOREPLYs and
;;	  LINREPLYs before displaying it. Exploits the "end of who/links"
;;	  message from the server.
;;	- Understands new MOTD notices, collects data until end-of-motd-list.
;;	- Better handling of extracting hostnames from different places.
;;	  Uses irc-extract-hostname.
;;	- Made irc-maintain-tree use upcase and irc-extract-hostname on items
;;	  for irc-hosttree.
;;	- Now extracts info from MOTD replies.
;; v1.6 Mon Apr  9 22:25:22 1990	sojge@emil.csd.uu.se
;;	- Improved handling of %-signs in messages (data part so to say) by
;;	  doing a s/%/%%/ in functions irc-parse-public and irc-parse-privmsg.
;; v1.5 Mon Apr  9 17:32:51 1990	sojge@emil.csd.uu.se
;;	- Extract server names from KILL messages paths.
;; v1.4 Fri Apr  6 01:30:46 1990	sojge@emil.csd.uu.se
;;	- Removed s/%/%%/ from irc-parse-server-msg.
;;	- Let function irc check that the *IRC* buffer has an open stream
;;	  "process" associated with it before trying to switch to the buffer.
;;	  This is handy when the stream has been closed.
;; v1.3 Tue Apr  3 23:51:38 1990	sojge@emil.csd.uu.se
;;	- Made irc.el cope with new format on some NOTICE messages.
;;	  (:server NOTICE ...)
;; v1.2 Fri Mar 30 03:42:31 1990	sojge@emil.csd.uu.se
;;	- Changed protocol, initiate with USER <username> * * <realname> now.
;;	- Changed misc. error messages to start with %.
;;	- Made a few "server message unkown" messages tell where they are
;;	  in the code.
;;	- Added code to make WHOIS * invokde WHOIS for all users.
;;	- Made irc-parse-channel set irc-msg-cont-used.
;;	- Added and removed code for automatic greeting of new joiners to
;;	  channels, in irc-parse-channel. But this doesn't seem to be enough.
;;	  More debugging needed ...
;;	- Made irc.el understand MOTD messages from server, not reliying on
;;	  all lines in MOTD file starting with a blank.
;; v1.1 Mon Mar 26 03:37:03 1990	sojge@emil.csd.uu.se
;;	- Cleaned up some more, in how things are displayed. Servers
;;	  domain names are displayed in upper case, in
;;	  irc-parse-linrpl and their information text "as is". Some
;;	  headers  are underlined.
;;	- Corrected (?) bug in irc-parse-RPL which handled RPL_MOTD
;;	  messages incorrectly.
;; v1.0 Wed Feb 28 21:53:49 1990	sojge@emil.csd.uu.se
;;	- Changed look and feel by introducing a couple of variables
;;	  and removing the conserve-space kludge. Apperence is now user
;;	  settable.
;;	- Rewritten buggy irc-insert so it works. No more infinite looping.
;;	- Added a few key bindings.
;;	- Made irc-parse-RPL (at RPL_WHOISSERVER) and irc-parse-notice add
;;	  information to the irc-hostlist, just like irc-parse-linreply does.
;; v0.9 Sometime 1989	flax@mizar.docs.uu.se
;;	- Made irc-linreply add to a list of known hosts so completion of
;;	  host names in irc-execute-admin and others will work. Neat.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author          : David C Lawrence           <tale@pawl.rpi.edu>
;; Created On      : Wed Jun 14 22:22:57 1989
;; Last major modification: Jonas Flygare
;; Last minor modification: Klaus Zeuge
;; Last Modified On: Wed Jan 24 00:54:29 1990
;; Update Count    : 100
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Copyright (C) 1989  David C Lawrence

;;  This program is free software; you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation; either version 1, or (at your option)
;;  any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program; if not, write to the Free Software
;;  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;; Comments and/or bug reports about this interface should be directed to:
;;     Dave Lawrence          <tale@{pawl,itsgw}.rpi.edu>
;;     76 1/2 13th Street     +1 518 273 5385
;;     Troy NY 12180          Generally available on IRC as "tale"

;; History 		
;; 27-Sep-1989		Gnu Maint Acct	(gnu at life.pawl.rpi.edu)
;;    Last Modified: Wed Sep 27 18:56:19 1989 #6
;;    * Fixed a misrefenced variable in irc-parse-server-msg so that lines
;;      which can't be parsed insert only that unparsable line.
;;    * Added irc-parse-topic and changed irc-signals and irc-notifies
;;      appropriately.
;;    * Moved setting of irc-channel to irc-parse-channel from irc-execute-join
;;      because a channel change isn't guaranteed (ie, trying to change to
;;      a channel not in 0-10 which has ten users on it).
;;    * Made irc-message-stamp 'private by default. (Geoff)
;;    * Made irc-time-stamp 0 by default.      (Also Geoff)
;;      This tickled an unseen bug in the start-up function which incremented
;;      irc-last-stamp.  It's fixed now.
;;    * Fixed a bug with interactive irc-execute-names -- prefix arg was being
;;      (list ...)ed.
;;    * Fixed a bug in irc-parse-namreply that dropped a name in the output
;;      when a line got wrapped.
;;    * Added /lusers.  I really dislike the name.
;;    * Added irc-parse-kill.
;;    * Made irc-execute-msg strip one space after the colon in messages;
;;      ie, "tale: hi" will send "hi" as a message not " hi".
;;    * Took out hardcoded width numbers for word-wrapping.  Uses
;;      (window-width) instead.
;;    * Added to the regexp for failed nickname changes in irc-parse-error;
;;      the sentences got changed in the newer servers.
;;    * Wrapped irc-check-time around display-time-filter for better accuracy
;;      if a display-time process is running.
;;    * Made the "IRC Session finished" message include the time.
;;    * Added irc-conserve-space to imitate the C client message style.  I
;;      really dislike it aesthetically, but it is handy for people running
;;      on 24 line screens and at slow baud rates. (Kimmel & others)
;;    * Support IRCNICK and IRCSERVER environment variables at load time. (Kim)
;;    * Made /WHO call /WHOIS if given a non-numeric argument.  (Nate)
;;    * Fixed a bug where an alias would be executed before an exactly matching
;;      command; ie, alias /WHOALIAS whould execute when /WHO was entered.
;;    * Runs things in auto-fill-mode with a fill-column of 75 by default.
;;    * Processes the whole input region as one line instead of line-at-a-time.
;;    * Made irc-change-alias for non-interactive setting/removing of aliases;
;;      ie, via irc-mode-hook.  No frills.  (Chris & Nate)
;;    * _Finally_ got around to adding parsing of numeric notices for new
;;      servers.

;; Defined variables
(provide 'irc)
(require 'backquote)


(defvar irc-abusive-ignore nil
  "If non nil, send back a message to persons being ignored, instead of
being silent, while throwing away messages from them.")


(defvar irc-alias-alist
  '(
    ("?" . "help")			; Abbrevation
    ("BYE" . "quit")
    ("CHANNEL" . "join")
    ("END" . "quit")			; Plenty of ways out
    ("EXIT" . "quit")
    ("IDLE" . "who")			; for people too used to Connect
    ("L" . "list")			; Abbrevation
    ("M" . "msg")			; Abbrevation
    ("MS" . "msg")			; Abbrevation
    ("N" . "names")			; Abbrevation
    ("QUERY" . "send")			; For people used to the C client
    ("STOP" . "quit")			; Plenty of ways out
    ("U". "users")			; Abbrevation
    ("US". "users")			; Abbrevation
    ("USE". "users")			; Abbrevation
    ("USER". "users")			; Abbrevation
    ("W" . "who")			; Abbrevation
    ("WHAT" . "list")			; /WHAT from Connect
    )
  "An association list of command aliases used in irc-mode.
This is the first list checked when irc-mode is looking for a command and it
is maintained with the /ALIAS and /UNALIAS commands.")


(make-variable-buffer-local
 (defvar irc-called-from-buffer nil
   "Flag saying whether a command is called from the irc buffer with /names
or invoked through pressing a key like C-c C-c."))


(make-variable-buffer-local
 (defvar irc-called-from-buffer nil
   "Flag saying whether a command is called from the irc buffer with /names
or invoked through pressing a key like C-c C-c."))


(defconst irc-command-alist
    '(("ADMIN" . "admin")		; Get information about IRC admins
      ("ALIAS" . "alias")		; Add command aliases
      ("AWAY" . "away")			; Give som indication of your presenc
      ("CONFIRM" . "confirm")		; Set message confirmation on or off
      ("DESCRIBE" . "describe")		; CTCP action.
      ("DIE" . "die")			; Tell server to die.
      ("EVENT" . "event")		; Change which events give notification
      ("FINGER" . "finger")		; CTCP finger.
      ("HELP" . "help")			; Get help on the /COMMANDs
      ("HERE" . "here")			; Remove the away mark
      ("IGNORE" . "ignore")		; Ignore messages from a specified user
      ("INFO" . "info")			; Information about users and authors
      ("INVITE" . "invite")		; Ask another user to join your channel
      ("JOIN" . "join")			; Join a channel
      ("KICK" . "kick")			; Boot a user from a channel
      ("LEAVE" . "leave")		; Leave a channel
      ("LINKS" . "links")		; Show which servers are in the IRC-net
      ("LIST" . "list")			; Show a list of channels and topics
      ("LUSERS" . "lusers")		; Get the number of users and servers
      ("MAIL" . "mail")			; Give commands to the mail subsystem.
      ("ME" . "me")			; Send a CTCP ACTION message to channel
      ("MEMBERSHIPS" . "memberships")	; Show subscribed channels.
      ("MODE" . "mode")			; Change or inspect the mode of a chan
      ("MOTD" . "motd")			; Message of the day
      ("MSG" . "privmsg")		; Send a private message to someone
      ("NAMES" . "names")		; Display nick names on each channel
      ("NEWS" . "news")			; Show news about client.
      ("NICKNAME" . "nick")		; Change your IRC nickname
      ("NOTIFY" . "notify")		; Notify when selected nicks are seen
      ("OOPS" . "oops")			; Resend a misdirected message
      ("OPER" . "oper")			; Login as an IRC operator
      ("QUIT" . "quit")			; Exit IRC
      ("QUOTE" . "quote")		; Send raw text to the server
      ("PING" . "ping")			; Send CTCP PING to another user
      ("REDIRECT" . "redirect")		; Send the last message to someone else
      ("SEND" . "send")			; Set the implicit send list for msgs's
      ("SERVICE" . "service")		; Add and subtract to list of automates
      ("SIGNAL" . "signal")		; Change which events give a signal
      ("STAMP" . "stamp")		; Set time notification interval
      ("STATS" . "stats")		; Get statistics from server.
      ("SUMMON" . "summon")		; Ask a user not on IRC to join it
      ("TIME" . "time")			; Get the current time from a server
      ("TOPIC" . "topic")		; Change the topic of the channel
      ("TRACE" . "trace")		; Show the links between servers
      ("UNALIAS" . "unalias")		; Remove command aliases
      ("UNIGNORE" . "unignore")		; Stop ignoring user
      ("USERINFO" . "userinfo")		; Set information about oneself
      ("USERS" . "users")		; Show users logged in at a server
      ("VERSION" . "version")		; Version of a server or a users client
      ("WALLOPS" . "wallops")		; Send a message to all operators
      ("WHO" . "who")			; Get list of users and their channels
      ("WHOIS" . "whois")		; Get longer information about a user
      ("WHOWAS" . "whois"))		; Check who user who has left IRC was.
  "An association list of the regular commands to which all users have access.
Form is (\"COMMAND\" . \"function\") where \"function\" is that last element in
an irc-execute-* symbol.  See also irc-alias-alist and irc-operator-alist.")


(defvar irc-confirm t
  "*If non-nil, provide confirmation for messages sent on IRC.
It should be noted that confirmation only indicates where irc-mode
tried to send the message, not whether it was actually received.
Use the /CONFIRM command to change.")


(make-variable-buffer-local
 (defvar irc-default-to "*;"
   "The default recipient of a message if no : or ; is provided.
\"*\" means the current channel, no matter what it is."))


(make-variable-buffer-local
 (defvar irc-drop-ircII-text-kludge-control-characters t
   "*If nil, show CNTRL/B, CNTRL/V and CNTRL/_ as ^B. ^V and ^_ in text. These
characters are used as a kludge by the client ircII to toggle the attributes
reverse, underline and bold respectivly in text. If non-nil, drop those three
characters."))


(defvar irc-emacs-knows-ISO8859-1 nil
  "*Non-nil if Kiwi shouldn't translate octets in incoming messages with
the high bit set to \xyz strings; nil if translation for such characters
(0240 - 0377) should occur.")


(make-variable-buffer-local
 (defvar irc-events '(ctcp join nick quit topic)
   "Events in IRC that should get notification messages.
A notification message is just one line to indicate that the event has
occurred.  The \"ctcp\", \"join\", \"nick\", \"quit\" and \"topic\"
events are currently supported by the /EVENT command."))


(defconst irc-help-topic-alist
    '((";" . "Usage: RECEIVER[,RECEIVER]...; MESSAGE

Send a message to one or several receivers.
About the same as /MSG and :, see /HELP : and /HELP MSG.

When no receiver is given yet, on a line, and you press ; first on that line,
the last receiver is inserted (this may be a users nickname, a channelname,
a list of receivers, etc)

Don't use any spaces left of the ;.")
      (":" . "Usage: RECEIVER [, RECEIVER] ... ; MESSAGE

Send a message to one or several receivers.
About the same as /MSG and ;, see /HELP ; and /HELP MSG

When no receiver was given yet, and : is inserted first on the line, the
last person you sent to will be the default receiver.

Don't use any spaces left of the :.")
      ("STARTUP" . "Initiation and startup

To make your GNU Emacs know of this IRC user interface, you can add
the following command to your GNU Emacs initialisation file (normaly
.emacs) in your home directory:
        (autoload 'irc \"Kiwi\"
            \"Internet Relay Chat user interface.\" t nil)

This will make your Emacs to load the library file Kiwi.elc or Kiwi.el
when you typing M-x irc RET. The file must be in one of the
directories mentioned in the variable load-path. (You can check this
variables value by typing C-h v load-path RET). You can compile the
file Kiwi.el by typing M-x byte-compile-file RET Kiwi.el RET to get
Kiwi.elc.

At startup time of IRC, you may want to initiate some settings and
give some commands. This can be done by setting some variables in your
GNU Emacs initialisation file (normaly called .emacs). Some typical
initialisations may be

       (setq irc-server \"minsk.docs.uu.se\"      ;Which server to use.
             irc-msg-public \"%s%9s/%s:  \"       ;How to display public msgs
             irc-pop-on-signal 3                ;Use about 1/3 of screen when
                                                ; popping up a Kiwi buffer.
             ;; Give some information about yourself, whatever you want.
             irc-userinfo (concat
                           \"Studying computer science at the university of\"
                           \" Uppsala. Call me on +46 18 463253.\")
             ;; Commands to be executed when seeing the Welcome message
             ;; from a server we just connected to.
             irc-startup-hook '(lambda ()
                                (irc-execute-command \"links *.se\")
                                (irc-execute-command \"who *.se\")))

This will make IRC use about 1/3 of the screen when popping up a
window (this happens for instance when you have deselcted all IRC
windows and someone sends you a message). Also, the information string
for you (which can contain any information you choose) is initiated.
When a server says \"welcome\", you will be shown all active servers
in Sweden (SE) and all active users using IRC in Sweden.

Don't give the leading / in the arguments to irc-execute-command.

If you want more control over how to display public and/or private messages
(at the time exluding WALLs and WALLOPSs) you can use this clumsy way (be
patient, a more user friendly method is on it's way) of setting the variables
irc-public-insert and irc-private-insert to functions taking 3 strings as
arguments, namely the name of the sender, name of the receiver (user or channel
name normaly) and the message itself (in raw format, after extracting the
client-to-client protocol stuff). All this sums up to, that if you don't know
elisp, you probaly don't want to use this method, or else just change the
control string constants in the examples below.
Examples:
  (setq
   irc-public-insert '(lambda (from to msg)
		       (let* ((same-chan (string= (upcase irc-channel)
						  (upcase to)))
			      (hdr
			       (format (if same-chan
					   \"%s %9s:  \"
					   \"%s %9s/%s:  \")
				       (if (and irc-message-stamp
						(not (eq irc-message-stamp
							 'private)))
					   (irc-get-time)
					   \"\")
				       from
				       to))
			      (irc-msg-cont-used
			       (make-string 
				(min (length hdr)
				     (/ (window-width (get-buffer-window
						       (current-buffer)))
					2))
				? )))
			 (irc-insert
			  (concat hdr (irc-clean-up-message msg)))))
   irc-private-insert '(lambda (from to msg)
			(let* ((to-whom
				(cond ((string= (upcase irc-nick-used)
						(upcase to))
				       \"*%s %s*  \")
				      ((irc-is-broadcastname to)
				       (concat \"=%s %s BROADCAST to all\"
					       \"users on IRC server(s)\"
					       \" \\\"%s\\\"=  \"))
				      (t \"*%s %s/%s*  \")))
			       (hdr
				(format to-whom
					(if (and
					     irc-message-stamp
					     (eq irc-message-stamp
						 'private))
					    (irc-get-time)
					    \"\")
					from
					to))
			       (irc-msg-cont-used (make-string 
						   (min (length hdr)
							(/ (window-width (get-buffer-window (current-buffer)))
							   2))
						   ? )))
			  (irc-insert
			   (concat hdr (irc-clean-up-message msg))))))

Hint: the internal control strings uses 2 or 3 of the %s to write the time,
the senders name and the receivers name. If only 2 %s are given, the receivers
name won't be written.

[Note: this is a preliminary version of the help text, please send mail
to Klaus.Zeuge@Student.DoCS.UU.SE if you think it needs to be changed.]"))
  "A-list with car = keyword-string for a topic and cdr = descriptions string
of the keyword.")


(make-variable-buffer-local
 (defvar irc-history nil
   "A list of messages which irc-mode has processed.
This includes both successfully and unsuccessfully sent messages, starting
with the most recent first.  irc-max-history limits the number of items
that can appear in the list when using irc-add-to-history."))


(make-variable-buffer-local
 (defvar irc-ignore-automatic-warnings nil
   "*Variable to say whether to to display or not display automatic warnings
from users. Such warnings start with the marker \"<Automatic warning> \" in
a message either directly to to, or on a channel. The default and RECOMMENDED
value is nil, ie to display. Set to non-nil to supress such messages."))


(make-variable-buffer-local
 (defvar irc-ignores nil
   "A list of NICKNAMES whose events will be ignored.
Messages or other actions (like joining a channel) generated by anyone in
the list will not be displayed or signalled.  This list is maintained with
the /IGNORE and /UNIGNORE commands."))


(make-variable-buffer-local
 (defvar irc-ignore-trunk nil
   "A trunk (for now an array of hash tables) of events and nicks to ignore."))


(defvar irc-max-history 40
  "*The maximum number of messages retained by irc-mode.
This limits messages sent, not messages received.  They are stored to be
easily recalled by irc-history-prev and irc-history-next (C-c C-p and C-c C-n
by default).")


(defvar irc-max-server-message-length 512
  "The maximum length of a message one can send to a server. This include the
trailing CRLF, so the \"real\" message length is 2 less.")


(defvar irc-maximum-size (* 64 1024)
  "*Maximum size that the *Kiwi-server* buffer can attain, in bytes.
The default value of 64KB represents an average of about 1500 lines, or roughly
65 screens on a standard 24 line monitor. See also irc-minimum-size.")


(defvar irc-message-stamp 'private
  "*Mark messages received in IRC with the time of their arrival if non-nil.
If this is the symbol \"private\" or \"public\" then only messages of the
specified type are marked with the time.  WALL messages are always
time-stamped.")


(defvar irc-minimum-size (* 32 1024)
  "*Minimum size that the *Kiwi-server* buffer will get truncated to after
hitting the size in irc-maximum-size.")

(defvar irc-mode-hook nil
  "*Hook to run after starting irc-mode but before connecting to the server.")


(defvar irc-mode-map nil
  "The keymap which irc-mode uses.

Currently set to: \\{irc-mode-map}")


(make-variable-buffer-local
 (defvar irc-msg-info-post (if (boundp 'irc-msg-info-post)
			  irc-msg-info-post
			  "]")
   "*String to end an information message with."))


(make-variable-buffer-local
 (defvar irc-msg-info-pre (if (boundp 'irc-msg-info-pre)
			      irc-msg-info-pre
			      "[")
   "*String to start an information message with."))


(make-variable-buffer-local
 (defvar irc-msg-cont (if (boundp 'irc-msg-cont)
			  irc-msg-cont
			  (make-string 23 (string-to-char " ")))
   "*The string put first on a continued line."))


(make-variable-buffer-local
 (defvar irc-msg-priv (if (boundp 'irc-msg-priv)
			  irc-msg-priv
			  "==%%s%%%ds == ")
   "*The string put first on a line where a private message begins,
using TIME and SENDER strings, and EXTRA string."))


(make-variable-buffer-local
 (defvar irc-msg-public (if (boundp 'irc-msg-public)
			    irc-msg-public
			    "-- %s%9s (on %s) -- ")
   "*The string put first on a line where a public message begins,
using TIME string, SENDER string and CHANNEL number"))


(make-variable-buffer-local
 (defvar irc-msg-sent (if (boundp 'irc-msg-sent)
			  irc-msg-sent
			  "(Message sent to %s)")
   "*Message displayed after sending a string to the server.
Contains TYPE (ie \"channel\" or \"person\") and RECEIVER (ie channel name or
users nick)."))


(make-variable-buffer-local
 (defvar irc-msg-wall (if (boundp 'irc-msg-wall)
			  irc-msg-wall
			  "##%s%s ## ")
   "*The string put first on a line where awrite all (wall) message begins,
using TIME and SENDER strings."))


(make-variable-buffer-local
 (defvar irc-multiple-leave-in-progress nil
   "Flag saying whether a multiple /LEAVE command is in progress, so that
irc-parse-channel shouldn't call irc-show-subscribed-channels."))


(make-variable-buffer-local
 (defvar irc-nick (or (getenv "IRCNICK") (user-login-name))
   "*The nickname with which to enter IRC.
The default value is set from your login name.  Using /NICKNAME changes it."))


(defconst irc-legal-session-name "^\\*Kiwi-\\([^/]+\\)/\\([0-9]+\\)\\*$"
  "A regexp describing a legal session-name. (See irc-get-*-from-session-name).
Two fields are extracted, the first being a host name, and the second a
TCP port number.")


(defvar irc-notify-interval 5
  "*How often to check for people. The time is in minutes.")


(defvar irc-oops "Oops ... please ignore that."
  "*The text to send to the original IRC message recipient when using /OOPS.")


(defconst irc-operator-alist
    '(("CONNECT" . "connect")		; Establish links between servers
      ("KILL" . "kill")			; Forcibly remove a user
      ("REHASH" . "rehash")		; Reread irc.conf
      ("SQUIT" . "squit")		; Quit links between servers
      ("WALL" . "wall"))		; Send a message to everyone on IRC
  "As association list of commands which only an IRC Operator can use.
It is kept as a separate list so that regular users won't wonder how
come the commands don't work for them.")


(make-variable-buffer-local
 (defvar irc-output-character-set (if (boundp 'irc-output-character-set)
				      irc-output-character-set
				      (if irc-emacs-knows-ISO8859-1
					  'ISO-8859-1
					  'ASCII))
   "*Specifies whether the Emacs understands just ASCII (seven bit characters),
ISO-8859/1 (8 bits) or Kanjii (16 bits). The value is one of the following
atoms: ASCII ISO-8859-1 KANJII"))


(defvar irc-pop-on-signal 4
  "*An integer value means to display the *IRC* buffer when a signal is issued.
The number represents roughly how much of the Emacs screen to use when
popping up the IRC window if only one window is visible.  The reciprocal
is used, so a value of 1 causes the window to appear full-screen, 2 makes
the window half of the screen, 3 makes it one third, et cetera.  If the value
is not an integer then no attempt is made to show the *IRC* buffer if it
is not already visible.")


(make-variable-buffer-local
 (defvar irc-port (if (boundp 'irc-port)
		      irc-port
		      (let* ((p (getenv "IRCPORT"))
			     (np (and p (string-to-int p))))
			(if (and (numberp np) (> np 0) (< np 16384)) np 6667)))
   "*The port on which the IRC server responds.
Many sites don't have irc as a named service (ie, it has no entry in
/etc/inetd.conf) so you might have to set this to a number; the most
common configuration is to have IRC respond on port 6667. If there is
an enviroment variable called IRCPORT and it's value is nummeric
between 1 and 16387, that value is used instead."))


(defvar irc-processes nil
  "All currently open streams to irc-servers are kept in this list.
It is used so that the \"irc\" function knows whether to start a new process
by default.")


(make-variable-buffer-local
 (defvar irc-server (if (boundp 'irc-server)
			irc-server
			(or (getenv "IRCSERVER") "irc.nada.kth.se"))
   "*The name of the internet host running the IRC daemon.
IRC servers generally restrict which machines can maintain connetions with
them, so you'll probably have to find a server in your local domain.
Initiated from enviroment variable IRCSERVER."))


(make-variable-buffer-local
 (defvar irc-show-japanese-characters nil
   "*If nil, replace Kanjii characters in text by a marker like \"Old Kanjii\"
followed by a list of integers denoting the 16 bit values of the characters.
If t, enter the text \"as is\" e.i., keep even the ESCape characters.
If neither nil nor t, enter the text in \"raw\" mode e.i., replace the ESCapes
by ^[ and insert the resulting text in the buffer.
The default is nil."))


(make-variable-buffer-local
 (defvar irc-signals '((backtalk t) (private t) (invite t) (wall t)
                       (public) (join) (nick) (topic) (user))
   "Events in IRC that should get signalled when they occur.
Generally the signal is an audible beep.  The value of irc-signals is an
association list of events recognized by irc-mode and is maintained with
the /SIGNAL command."))


(defvar irc-spacebar-pages t
  "*When this variable is non-nil, the following keys are in effect when
point is in the output region.

SPC      scroll-forward    DEL           scroll-backward
TAB      previous-line     LFD or RET    next-line")


(defvar irc-time-stamp 5
  "*How often to insert a time-stamp into *IRC* buffers.
The first time one is based from the hour the IRC process was started so that
values which divide evenly into 60 minutes (like the default of 10) will split
the hour evenly (as in 13:10, 13:20, 13:30, et cetera).  To disable the
time-stamping set this variable to 0.  This can be set with the /STAMP command.

The accuracy of the time-stamping can be improved greatly by running
M-x display-time; with default settings this guarantees that Emacs will have
some sort of predictable activity every minute.  If display-time is not running
and the IRC session is idle, the time-stamp can be up to two minutes late.")

(defvar irc-translation-table-incoming
  (cond
    ((and (boundp 'irc-translation-table-incoming)
	  (arrayp irc-translation-table-incoming)
	  (= 256 (length irc-translation-table-incoming)))
     irc-translation-table-incoming)
    ((and (boundp 'irc-translation-table-incoming)
	  (symbolp irc-translation-table-incoming)
	  (eq 'scandinavian irc-translation-table-incoming))
     irc-translation-table-incoming-scandinavian7)
    (t nil)
    (t 'scandinavian7))			;Obsolete default
  "*Translation table for incoming characters.
If non-nil, single characters are translated one by one according to
this table. The table consists of a 256 elements long array, where each
element is either a string or nil. If the element is nil, the character
with the corresponding code is dropped, if the element is a string, the
string is inserted instead of the character.")

(defvar irc-translation-table-incoming-scandinavian7
  '["^@"				;000
    "^A"				;001
    ""					;002	ircII kludge
    "^C"				;003
    "^D"				;004
    "^E"				;005
    "^F"				;006
    "^G"				;007
    "^H"				;010
    "^I"				;011
    "^J\n"				;012
    "^K"				;013
    "^L"				;014
    "^M"				;015
    "^N"				;016
    ""					;017	ircII kludge
    "^P"				;020
    "^Q"				;021
    "^R"				;022
    "^S"				;023
    "^T"				;024
    "^U"				;025
    ""					;026	ircII kludge
    "^W"				;027
    "^X"				;030
    "^Y"				;031
    "^Z"				;032
    "^["				;033
    "^\\"				;034
    "^]"				;035
    "^^"				;036
    ""					;037	ircII kludge
    " "					;040
    "!"					;041
    "\""				;042
    "#"					;043
    "$"					;044
    "%%"				;045	Need %% to not disturb format.
    "&"					;046
    "'"					;047
    "("					;050
    ")"					;051
    "*"					;052
    "+"					;053
    ","					;054
    "-"					;055
    "."					;056
    "/"					;057
    "0"					;060
    "1"					;061
    "2"					;062
    "3"					;063
    "4"					;064
    "5"					;065
    "6"					;066
    "7"					;067
    "8"					;070
    "9"					;071
    ":"					;072
    ";"					;073
    "<"					;074
    "="					;075
    ">"					;076
    "?"					;077
    "@"					;100
    "A"					;101
    "B"					;102
    "C"					;103
    "D"					;104
    "E"					;105
    "F"					;106
    "G"					;107
    "H"					;110
    "I"					;111
    "J"					;112
    "K"					;113
    "L"					;114
    "M"					;115
    "N"					;116
    "O"					;117
    "P"					;120
    "Q"					;121
    "R"					;122
    "S"					;123
    "T"					;124
    "U"					;125
    "V"					;126
    "W"					;127
    "X"					;130
    "Y"					;131
    "Z"					;132
    "["					;133
    "\\"				;134
    "]"					;135
    "^"					;136
    "_"					;137
    "`"					;140
    "a"					;141
    "b"					;142
    "c"					;143
    "d"					;144
    "e"					;145
    "f"					;146
    "g"					;147
    "h"					;150
    "i"					;151
    "j"					;152
    "k"					;153
    "l"					;154
    "m"					;155
    "n"					;156
    "o"					;157
    "p"					;160
    "q"					;161
    "r"					;162
    "s"					;163
    "t"					;164
    "u"					;165
    "v"					;166
    "w"					;167
    "x"					;170
    "y"					;171
    "z"					;172
    "{"					;173
    "|"					;174
    "}"					;175
    "~"					;176
    "^?"				;177
    "\200"				;200
    "~"					;201 MSDOS
    "\202"				;202
    "\203"				;203
    "{"					;204 MSDOS
    "\205"				;205
    "}"					;206 MSDOS
    "\207"				;207
    "\210"				;210
    "\211"				;211
    "\212"				;212
    "\213"				;213
    "\214"				;214
    "\215"				;215
    "["					;216 MSDOS
    "]"					;217 MSDOS
    "\220"				;220
    "\221"				;221
    "\222"				;222
    "\223"				;223
    "|"					;224 MSDOS
    "\225"				;225
    "\226"				;226
    "\227"				;227
    "\230"				;230
    "\\"				;231 MSDOS
    "^"					;232 MSDOS
    "\233"				;233
    "\234"				;234
    "\235"				;235
    "\236"				;236
    "\237"				;237
    " "					;240
    "!"					;241
    "c"					;242
    "#"					;243
    "$"					;244
    "Y"					;245
    "|"					;246
    "$"					;247
    "\""				;250
    "c"					;251
    "+"					;252
    "<<"				;253
    "!"					;254
    "-"					;255
    "R"					;256
    "~"					;257
    "C"					;260
    "+"					;261
    "2"					;262
    "3"					;263
    "'"					;264
    "u"					;265
    "$"					;266
    "-"					;267
    ","					;270
    "1"					;271
    "0"					;272
    ">>"				;273
    "?"					;274
    "?"					;275
    "?"					;276
    "?"					;277
    "A"					;300
    "A"					;301
    "A"					;302
    "A"					;303
    "["					;304
    "]"					;305
    "["					;306
    "C"					;307
    "E"					;310
    "@"					;311
    "E"					;312
    "E"					;313
    "I"					;314
    "I"					;315
    "I"					;316
    "I"					;317
    "D"					;320
    "N"					;321
    "O"					;322
    "O"					;323
    "O"					;324
    "O"					;325
    "\\"				;326
    "*"					;327
    "\\"				;330
    "U"					;331
    "U"					;332
    "U"					;333
    "^"					;334
    "Y"					;335
    "T"					;336
    "ss"				;337
    "a"					;340
    "a"					;341
    "a"					;342
    "a"					;343
    "{"					;344
    "}"					;345
    "{"					;346
    "c"					;347
    "e"					;350
    "`"					;351
    "e"					;352
    "e"					;353
    "i"					;354
    "i"					;355
    "i"					;356
    "i"					;357
    "d"					;360
    "n"					;361
    "o"					;362
    "o"					;363
    "o"					;364
    "o"					;365
    "|"					;366
    "/"					;367
    "|"					;370
    "u"					;371
    "u"					;372
    "u"					;373
    "~"					;374
    "y"					;375
    "t"					;376
    "y"]				;377
  "*Translation table for incoming characters to translate from ISO 8859-1
to SIS E47. Also handles some MSDOS characters.")


(defvar irc-translation-table-incoming-scandinavian8
  '["^@"				;000
    "^A"				;001
    ""					;002	ircII kludge
    "^C"				;003
    "^D"				;004
    "^E"				;005
    "^F"				;006
    "^G"				;007
    "^H"				;010
    "^I"				;011
    "^J\n"				;012
    "^K"				;013
    "^L"				;014
    "^M"				;015
    "^N"				;016
    ""					;017	ircII kludge
    "^P"				;020
    "^Q"				;021
    "^R"				;022
    "^S"				;023
    "^T"				;024
    "^U"				;025
    ""					;026	ircII kludge
    "^W"				;027
    "^X"				;030
    "^Y"				;031
    "^Z"				;032
    "^["				;033
    "^\\"				;034
    "^]"				;035
    "^^"				;036
    ""					;037	ircII kludge
    " "					;040
    "!"					;041
    "\""				;042
    "#"					;043
    "$"					;044
    "%%"				;045	Need %% to not disturb format.
    "&"					;046
    "'"					;047
    "("					;050
    ")"					;051
    "*"					;052
    "+"					;053
    ","					;054
    "-"					;055
    "."					;056
    "/"					;057
    "0"					;060
    "1"					;061
    "2"					;062
    "3"					;063
    "4"					;064
    "5"					;065
    "6"					;066
    "7"					;067
    "8"					;070
    "9"					;071
    ":"					;072
    ";"					;073
    "<"					;074
    "="					;075
    ">"					;076
    "?"					;077
    "\311"				;100
    "A"					;101
    "B"					;102
    "C"					;103
    "D"					;104
    "E"					;105
    "F"					;106
    "G"					;107
    "H"					;110
    "I"					;111
    "J"					;112
    "K"					;113
    "L"					;114
    "M"					;115
    "N"					;116
    "O"					;117
    "P"					;120
    "Q"					;121
    "R"					;122
    "S"					;123
    "T"					;124
    "U"					;125
    "V"					;126
    "W"					;127
    "X"					;130
    "Y"					;131
    "Z"					;132
    "\304"				;133
    "\326"				;134
    "\305"				;135
    "\334"				;136
    "_"					;137
    "\351"				;140
    "a"					;141
    "b"					;142
    "c"					;143
    "d"					;144
    "e"					;145
    "f"					;146
    "g"					;147
    "h"					;150
    "i"					;151
    "j"					;152
    "k"					;153
    "l"					;154
    "m"					;155
    "n"					;156
    "o"					;157
    "p"					;160
    "q"					;161
    "r"					;162
    "s"					;163
    "t"					;164
    "u"					;165
    "v"					;166
    "w"					;167
    "x"					;170
    "y"					;171
    "z"					;172
    "\344"				;173
    "\366"				;174
    "\345"				;175
    "\374"				;176
    "^?"				;177
    "\200"				;200
    "\374"				;201 MSDOS
    "\202"				;202
    "\203"				;203
    "\344"				;204 MSDOS
    "\205"				;205
    "\345"				;206 MSDOS
    "\207"				;207
    "\210"				;210
    "\211"				;211
    "\212"				;212
    "\213"				;213
    "\214"				;214
    "\215"				;215
    "\305"				;216 MSDOS
    "\304"				;217 MSDOS
    "\220"				;220
    "\221"				;221
    "\222"				;222
    "\223"				;223
    "\366"				;224 MSDOS
    "\225"				;225
    "\226"				;226
    "\227"				;227
    "\230"				;230
    "\326"				;231 MSDOS
    "\334"				;232 MSDOS
    "\233"				;233
    "\234"				;234
    "\235"				;235
    "\236"				;236
    "\237"				;237
    "\240"				;240
    "\241"				;241
    "\242"				;242
    "\243"				;243
    "\244"				;244
    "\245"				;245
    "\246"				;246
    "\247"				;247
    "\250"				;250
    "\251"				;251
    "\252"				;252
    "\253"				;253
    "\254"				;254
    "\255"				;255
    "\256"				;256
    "\257"				;257
    "\260"				;260
    "\261"				;261
    "\262"				;262
    "\263"				;263
    "\264"				;264
    "\265"				;265
    "\266"				;266
    "\267"				;267
    "\270"				;270
    "\271"				;271
    "\272"				;272
    "\273"				;273
    "\274"				;274
    "\275"				;275
    "\276"				;276
    "\277"				;277
    "\300"				;300
    "\301"				;301
    "\302"				;302
    "\303"				;303
    "\304"				;304
    "\305"				;305
    "\306"				;306
    "\307"				;307
    "\310"				;310
    "\311"				;311
    "\312"				;312
    "\313"				;313
    "\314"				;314
    "\315"				;315
    "\316"				;316
    "\317"				;317
    "\320"				;320
    "\321"				;321
    "\322"				;322
    "\323"				;323
    "\324"				;324
    "\325"				;325
    "\326"				;326
    "\327"				;327
    "\330"				;330
    "\331"				;331
    "\332"				;332
    "\333"				;333
    "\334"				;334
    "\335"				;335
    "\336"				;336
    "\337"				;337
    "\340"				;340
    "\341"				;341
    "\342"				;342
    "\343"				;343
    "\344"				;344
    "\345"				;345
    "\346"				;346
    "\347"				;347
    "\350"				;350
    "\351"				;351
    "\352"				;352
    "\353"				;353
    "\354"				;354
    "\355"				;355
    "\356"				;356
    "\357"				;357
    "\360"				;360
    "\361"				;361
    "\362"				;362
    "\363"				;363
    "\364"				;364
    "\365"				;365
    "\366"				;366
    "\367"				;367
    "\370"				;370
    "\371"				;371
    "\372"				;372
    "\373"				;373
    "\374"				;374
    "\375"				;375
    "\376"				;376
    "\377"]				;377
  "*Translation table for incoming characters to translate from SIS E47
to ISO 8859-1. Also handles some MSDOS characters.")


(make-variable-buffer-local
 (defvar irc-userinfo (if (boundp 'irc-userinfo)
			  irc-userinfo
			  nil)
   "*If non-nil, a user settable string to be replied with when the client is
queried by some other user."))




;;; Macros
;;;
(defmacro subfield (str n)
  "Return a substring of STR. The substring is field N, as used in the latest
string-match."
  (` (if (and (numberp (match-beginning (, n)))
	      (numberp (match-end (, n))))
	 (substring (, str) (match-beginning (, n)) (match-end (, n)))
	 "")))


;;; Handle host names.
;;;
;;; First a constant regular expression string matching only host names.
;;;
(defconst irc-hostname "^[A-Za-z][A-Za-z0-9.-.---]*$"
  "A regexp matching a string if and only if the string is a hostname.")


(defconst irc-hostname-complement "\\(^\\.\\|\\.$\\)")


;;; See RFC 952
(defconst irc-hostname-chars '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
			       ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
			       ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
			       ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
			       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?- ?.))

 
(defun irc-is-hostname (name)
  "Returns t if the NAME is a hostname, nil else.
This functions is more paranoid (Hello RFC 952!) than the IRC servers."
  (and (stringp name)
       (not (not (and (string-match irc-hostname name)
		      (string-match "\\." name)
		      (not (string-match (regexp-quote irc-hostname-complement)
					 name)))))))


(defun irc-is-nickname (name)
  "Returns t if the NAME is a valid nick name in IRC."
  (and (numberp (string-match (concat "^[]A-Z[\\^_a-z{|}~`]"
				      "[]A-Z0-9[\\_^a-z{|}~`-`---]*$")
			      name))
       (not (numberp (string-match "\n" name)))))


(defun irc-is-channelname (name)
  "Returns t if NAME is a valid channel identification."
  (or (and (irc-server-has-multijoinable-channels)
	   (= ?# (aref name 0))
	   (irc-is-named-channel name))
      (and (irc-server-has-non-numeric-channel-names)
	   (= ?+ (aref name 0))
	   (irc-is-named-channel name))
      (and (numberp (string-match "^ *-?[0-9]+ *$" name))
	   (irc-is-named-channel (concat "+" name)))))


(defun irc-is-broadcastname (name)
  "Return t if NAME is a valid broadcast address, ie is a server- or hostname
containing wildcards prepended by $ (server) or # (host). Broadcasts came at
the same time as #channels."
  (and (irc-server-has-multijoinable-channels)
       (or (= ?$ (aref name 0)) (= ?# (aref name 0)))))


(defun irc-is-named-channel (name)
  "Return t if NAME is a valid nummeric channel identification."
  (or (and (irc-server-has-multijoinable-channels)
	   (numberp (string-match "^[#+][]/a-zA-Z#+{}[^~()*!@.,0-9_-_---]*$"
				  name)))
      (and (irc-server-has-non-numeric-channel-names)
	   (numberp (string-match "^+[]/a-zA-Z+{}[^~()*!@.,0-9_-_---]*$"
				  name)))))


(defun irc-is-multijoinable-channel (channel)
  "True if CHANNEL is a multijoinable channel, ie a channel one can listen
to along several other multijoinable channels. This feature was introduced in
server version 2.6."
  (and (stringp channel)
       (= (aref channel 0) ?#)))


(defun irc-is-receiver (obj)
  "True if STR could be either a nick or channel name.
If user is an enabled irc operator, broadcast addresses are OK too."
  (or (string= "*" obj)
      (irc-is-nickname obj)
      (and (string-match "^\\([^@]+\\)@\\([^@]+\\)$" obj)
	   (let ((pre (subfield obj 1))
		 (post (subfield obj 2)))
	     (and (irc-is-nickname pre)
		  (irc-is-hostname post))))
      (irc-is-channelname obj)
      (and irc-operator (irc-is-broadcastname obj))))


(defun irc-format-channel (channel &optional width)
  "Format a CHANNEL identification (name, number or * (ie private)) to be left
justified in a 15 character wide field. Ie make what the format string %-15s
SHOULD do. If the channel equals irc-channel, ie the current channel, it is
postpended with a %.

Arguments are a string with the channel ID, and optional a number saying how
wide the resulting string should be at least. If none is given, 15 is used as
the default."
  (let* ((w (max 0 (or (and (numberp width) width) 15)))
	 (isnamed (irc-is-named-channel channel))
	 (c (cond ((string= "*" channel) "Private")
		  ((string= channel irc-channel) (concat channel "%"))
		  (t channel)))
	 (rpad (make-string (max 0 (- w (length c))) ? )))
    (concat c rpad)))


(defun irc-extract-hostname (s)
  "Extract the hostname part from the STRING, ignoring a trailing comment.
The string doesn't start with a host name, return nil."
  (let ((i 0)
	(len (length s)))
    (while (and (< i len)
		(memq (string-to-char (substring s i))
		      irc-hostname-chars))
      (setq i (1+ i)))
    (if (irc-is-hostname (substring s 0 i))
	(substring s 0 i)
	nil)))


(defun irc-extract-hosts (path)
  "Extract and remember the different hosts mentioned in the path string,
\"h1!h2!h3!x\" where x is either a host or a nick name."
  (let* ((first-host (irc-extract-hostname path))
	 (first-! (string-match "!" path))
	 (len (length first-host)))
    (cond ((string= path "") nil)
	  ((and (not first-host) first-!)
	   (irc-extract-hosts (substring path (1+ first-!))))
	  ((not first-host) nil)
	  (t (irc-extract-hosts (substring path len))))))

;; keymap
;; what i tend to like for keys might be very different from what most people
;; find useful; in fact i tend to type the /COMMANDs more than use any bindings

;; there are more things bound here just so people can see the different
;; things available to them
(or irc-mode-map
    (progn
      (setq irc-mode-map (make-keymap))
      (define-key irc-mode-map "\C-j"		'irc-process-input)
      (define-key irc-mode-map "\C-m"		'irc-process-input)
      (define-key irc-mode-map "\C-i"		'irc-tab)
      (define-key irc-mode-map "\C-c\C-a"	'irc-execute-alias)
      (define-key irc-mode-map "\C-c\C-c"	'irc-execute-names)
      (define-key irc-mode-map "\C-c\C-h"	'irc-execute-help)
      (define-key irc-mode-map "\C-c\C-i"	'irc-execute-invite)
      (define-key irc-mode-map "\C-c\C-j"	'irc-execute-join)
      (define-key irc-mode-map "\C-c\C-k"	'irc-execute-kill)
      (define-key irc-mode-map "\C-c\C-l"	'irc-execute-list)
      (define-key irc-mode-map "\C-c\C-m"	'irc-history-menu)
      (define-key irc-mode-map "\C-c\C-n"	'irc-history-next)
      (define-key irc-mode-map "\C-c\C-o"	'irc-execute-oops)
      (define-key irc-mode-map "\C-c\C-p"	'irc-history-prev)
      (define-key irc-mode-map "\C-c\C-q"	'irc-execute-quote)
      (define-key irc-mode-map "\C-c\C-r"	'irc-execute-redirect)
      (define-key irc-mode-map "\C-c\C-s"	'irc-execute-squit)
      (define-key irc-mode-map "\C-c\C-t"	'irc-execute-topic)
      (define-key irc-mode-map "\C-c\C-u"	'irc-kill-input)
      (define-key irc-mode-map "\C-c\C-v"	'irc-execute-version)
      (define-key irc-mode-map "\C-c\C-w"	'irc-execute-who)
      (define-key irc-mode-map "\C-c\C-?"	'irc-kill-input)
      ;; it's nice to bind to a key while in development, but regular users
      ;; wonder about it in production.
      ;; (define-key irc-mode-map "\C-c " 'irc-pong)
      (define-key irc-mode-map "\C-c#" 'irc-execute-lusers)
      (define-key irc-mode-map "\C-c=" 'irc-execute-memberships)
      (define-key irc-mode-map "\C-c?" 'describe-mode)
      (define-key irc-mode-map "\C-ca" 'irc-execute-admin)
      (define-key irc-mode-map "\C-cc" 'irc-execute-connect)
      (define-key irc-mode-map "\C-cf" 'irc-execute-finger)
      (define-key irc-mode-map "\C-ci" 'irc-execute-info)
      (define-key irc-mode-map "\C-ck" 'irc-execute-quit)
      (define-key irc-mode-map "\C-cl" 'irc-execute-links)
      (define-key irc-mode-map "\C-cm" 'irc-execute-mode)
      (define-key irc-mode-map "\C-cn" 'irc-execute-news)
      (define-key irc-mode-map "\C-co" 'irc-execute-oper)
      (define-key irc-mode-map "\C-cp" 'irc-yank-prev-command)
      (define-key irc-mode-map "\C-cq"	'irc-execute-leave)
      (define-key irc-mode-map "\C-cs" 'irc-execute-summon)
      (define-key irc-mode-map "\C-ct" 'irc-execute-trace)
      (define-key irc-mode-map "\C-cu" 'irc-execute-users)
      (define-key irc-mode-map "\C-cv" 'irc-version)
      (define-key irc-mode-map "\C-cw" 'irc-execute-whois)
      (define-key irc-mode-map "\C-?"  'irc-del-backward-char)
      ;; make any self-inserting keys call irc-self-insert
      (mapcar (function
               (lambda (key)
		(define-key irc-mode-map key 'irc-self-insert)))
              (where-is-internal 'self-insert-command nil nil))))

;; filters (mostly irc-parse-*)
;; Filtering of server messages from reception to insertion in the buffer
;; are all done on this page.  In particular, if a new server message has
;; to be dealt with, it should be added in the irc-parse-server-msg function.
(defun irc-filter (proc str)
  "Filtering procedure for IRC server messages.
It waits until everything up to a newline is accumulated before handing the
string over to irc-parse-server-msg to be processed.  If irc-pop-on-signal
is an integer and a signal is issued then the *IRC* buffer will be displayed.

Unless irc-maximum-size is zero or negative, truncate the Kiwi buffer to
be no larger than irc-maximum-size. If it is larger, truncate it to
the size irc-minimum-size."
  (let* ((ibuf (process-buffer proc))
         bell irc-mark-to-point new-point old-irc-mark win)
    (save-excursion
      (set-buffer ibuf)
      ;; still can't tell why i need this; sure, i probably change point
      ;; in ibuf.  but so what?  set-window-point should clean up after that.
      ;; it works with it though and not without it, so it stays.
      (save-excursion 
	;; trim buffer if needed
	(if (> irc-maximum-size 0)
	    (irc-truncate-buffer irc-maximum-size irc-minimum-size))
        (setq irc-mark-to-point   ; so we can restore relative position later
              (- (point) (setq old-irc-mark (goto-char irc-mark)))
              ;; just glue str to the end of any partial line that's there
              irc-scratch (concat irc-scratch str))
        ;; see if it is time for a message
        (irc-check-time)
        (while (string-match "\n" irc-scratch) ; do as many lines as possible
          ;; first use irc-scratch for the dp returned by irc-parse-server-msg
          (setq irc-scratch (irc-parse-server-msg irc-scratch)
		bell (cdr irc-scratch) ; issue a signal?
                ;; now irc-scratch is what it was, minus the line parsed
                irc-scratch (car irc-scratch))
          (if bell
	      (progn
		;; issue a signal; no need to trash someone's kbd-macro over it
		(ding 'no-terminate)
		(irc-minibuffer-message "%sBell in %s%s"
					irc-msg-info-pre
					(buffer-name ibuf)
					irc-msg-info-post))))
        ;; if point was in front of the irc-mark, new-point is figured relative
        ;; to the old mark, otherwise it is relative to the new one
        (setq new-point (+ (if (< irc-mark-to-point 0) old-irc-mark irc-mark)
                           irc-mark-to-point))))
    ;; update point based on whether the buffer is visible
    ;; we have a real problem here if there is more than one window displaying
    ;; the process-buffer and the user is not in the first canonical one.
    ;; i haven't yet determined a nice way to solve this
    (if (setq win (if (eq (window-buffer) ibuf)	;930224/PS: Use selected
		      (selected-window)	; window if it shows that buffer.
		      (get-buffer-window ibuf)))
	(set-window-point win new-point)
	(save-excursion (set-buffer ibuf) (goto-char new-point)))
    ;; if *IRC* isn't visible, a bell was issued and irc-pop-on-signal is an
    ;; integer then show the buffer.
    (if (and (integerp irc-pop-on-signal) bell (not win))
	(progn
	  (setq win (selected-window))
	  (if (/= (irc-count-windows 'no-mini) 1)
	      (display-buffer ibuf) ;don't futz with sizes if more than 1 win's
	      ;; we might be in the mininbuffer at the moment, so insure that
	      ;; this happens starting in the current regular window
	      (select-window (next-window win 'no-mini))
	      ;; full screen doesn't get handled well by the algorithm
	      ;; for the rest
	      (if (= irc-pop-on-signal 1)
		  (set-window-buffer (selected-window) ibuf)
		  (split-window nil (- (screen-height)
				       (/ (screen-height) irc-pop-on-signal)))
		  (display-buffer ibuf)
		  ;; perhaps we need to go back to the minibuffer
		  (select-window win)))))))


(defun irc-string-display-length (str)
  "Return the number of display positions a string would take when
being displayed by GNU-emacs. A character can be displayed in 1, 2 or
4 positions. Also a TAB can be 1 to 8 positions wide.

This function is pessimistic, and will rather return a too large count than a
too small."
  (let ((len 0)
	(i (length str))
	(x 0))
    (while (> i 0)
      (setq i (1- i)
	    x (+ x (let ((c (aref i str)))
		     (cond
		       ((= c \t) 8)	;TAB.
		       ((or (< c ?\040) (= c ?\177)) ;Normal CNTRL character.
			(if ctl-arrow 2 4)) ;^X or \030 notation.
		       ((< c ?\177) 1) ;Regular character.
		       ((< c ?\240) 4) ;Eight bit "CNTRL" chararacter.
		       ((< c ?\400)
			(cond ((eq irc-output-character-set 'ASCII) 4)
			      ((eq irc-output-character-set 'ISO-8859-1) 1)
			      (t 4)))
		       (t 4))))))
    x))


(defun irc-clean-up-message (str)
  "Exchange all \"%\" with \"%%\" in STR. Also exchange CNTRL/X in STR
with ^X. Substitute a ^B for CNTRL/B (which is used by ircII to togle the
boldness of text) if irc-drop-ircII-text-kludge-control-characters is true,
or else drop the character. As a further kudo, replace japanese characters
coded with JIS etc unless irc-show-japanese-characters is nil."
  (let* ((clean "")
	 (dirty-chars (concat "%"
			      "\000\001\002\003\004\005\006\007"
			      "\010\011\012\013\014\015\016\017"
			      "\020\021\022\023\024\025\026\027"
			      "\030\031\032\033\034\035\036\037"

			      "\177"
			      
			      "\200\201\202\203\204\205\206\207"
			      "\210\211\212\213\214\215\216\217"
			      "\220\221\222\223\224\225\226\227"
			      "\230\231\232\233\234\235\236\237"

			      (if irc-emacs-knows-ISO8859-1
				  ""
				  (concat "\240\241\242\243\244\245\246\247"
					  "\250\251\252\253\254\255\256\257"
					  "\260\261\262\263\264\265\266\267"
					  "\270\271\272\273\274\275\276\277"
					  "\300\301\302\303\304\305\306\307"
					  "\310\311\312\313\314\315\316\317"
					  "\320\321\322\323\324\325\326\327"
					  "\330\331\332\333\334\335\336\337"
					  "\340\341\342\343\344\345\346\347"
					  "\350\351\352\353\354\355\356\357"
					  "\360\361\362\363\364\365\366\367"
					  "\370\371\372\373\374\375\376\377"))
			      ""))
	 (dirty (concat "[" dirty-chars "]"))
	 (non-dirty (concat "[^" dirty-chars "]")))
    (if (symbolp irc-translation-table-incoming)
	(cond ((eq 'scandinavian7 irc-translation-table-incoming)
	       (setq irc-translation-table-incoming
		     irc-translation-table-incoming-scandinavian7))
	      ((eq 'scandinavian8 irc-translation-table-incoming)
	       (setq irc-translation-table-incoming
		     irc-translation-table-incoming-scandinavian8))))
    (while (or (string-match dirty str)
	       (and irc-translation-table-incoming
		    (string< "" str)))
      (let ((split (cond ((and (or (eq irc-show-japanese-characters t)
				   (eq irc-show-japanese-characters nil))
			       (string-match (concat
					      "^ ?\033$\\([@AB]\\)\\([^\033]*"
					      "\\)\033\\([()]. ?\\)")
					     str))
			  (cond
			    ((eq irc-show-japanese-characters t)
			     (let* ((pre (subfield str 1))
				    (msg (subfield str 2))
				    (trail (subfield str 3)))
			     (cons (concat "\033$"
					   pre
					   (irc-clean-up-message msg)
					   trail))
				   (substring str (match-end 0))))
			    ((eq irc-show-japanese-characters nil)
			     (let ((msgend (match-end 0))
				   (type (substring str
						    (match-beginning 1)
						    (match-end 1))))
			       (cons (concat irc-msg-info-pre
					     (cond ((string= type "@")
						    "Old Kanjii: ")
						   ((string= type "A")
						    "Probaly chinese: ")
						   ((string= type "B")
						    "Modern Kanjii: ")
						   (t "Unkown 16-bit: "))
					     (irc-clean-up-message
					      (irc-translate-kanji
					       (substring str
							  (match-beginning 2)
							  (match-end 2))))
					     irc-msg-info-post
					     " ")
				     (substring str msgend))))
			    (t (irc-insert (concat "%%Bug in japanese part of"
						   " irc-clean-up-message."))
			       (cons (substring str 0 (match-end 0))
				     (substring str (match-end 0))))))
			 ((not (null irc-translation-table-incoming))
			  (cons (aref irc-translation-table-incoming
				      (aref str 0))
				(substring str 1)))
			 ((and (>= (aref str 0) ?\200) ;8 bit?
			       (or (< (aref str 0) ?\240)
				   (not (eq irc-output-character-set
					    'ISO-8859-1))))
			  (cons (concat (format "\\%o" (aref str 0)))
				(substring str 1)))
			 ((string-match (concat "^" non-dirty "*%") str)
			  (cons (concat (substring str 0 (1- (match-end 0)))
					"%%")
				(substring str (match-end 0))))
			 ((= (aref str 0) ?\t)
			  (cons "^I" (substring str 1)))
			 ((= (aref str 0) ?\177)
			  (cons "^?" (substring str 1)))
			 ((and irc-drop-ircII-text-kludge-control-characters
			       (irc-member-general (aref str 0)
						   '(?\002 ?\017 ?\026 ?\037)
						   '=))
			  (cons "" (substring str 1)))
			 ((and (>= (aref str 0) ?\000)
			       (<= (aref str 0) ?\037))
			  (cons
			   (concat "^" (char-to-string (+ ?@ (aref str 0))))
			   (substring str 1)))
			 ((string-match (concat "^" non-dirty "+") str)
			  (cons (substring str
					   (match-beginning 0)
					   (match-end 0))
				(substring str (match-end 0))))
			 (t (cons (substring str 0 1) (substring str 1))))))
	(setq clean (concat clean (car split))
	      str (cdr split))))
    (concat clean str)))


(defun irc-translate-kanji (str)
  "Translate a japanese Kanjicoded message in STR into nummeric codes.
Just exchange every two octet sequence into their nummeric codes."
  (let ((retval ""))
    (while (>= (length str) 2)
      (let ((double (+ (* (aref str 0) 256) (aref str 1))))
	(setq retval (format "%s%s%d"
			     retval
			     (if (string= "" retval) "" " ")
			     double)
	      str (substring str 2))))
    (if (> (length str) 0)
	(format "ODD %d" (aref str 0))
	retval)))


(defun irc-parse-server-msg (str)
  "Take the first line from STR and pass it on to the appropriate irc-parse-*
function.  If the message is one which irc-mode doesn't recognize, just display
it as the raw server message.

It returns a dotted-pair whose car is the remainder of STR after the first
newline and whose cdr is either t or nil, indicating whether a signal should
be issued."
  (let ((n (string-match "\r" str)))
    (while n
      (setq str (concat (substring str 0 n)
			(substring str (1+ n)))
	    n (string-match "\r" str))))
  (let* ((nl (string-match "\n" str)) 
	 (eol (or nl (length str)))
	 (line (substring str 0 eol))
	 (dummy (irc-log-in-debug-buffer (concat "  " line)))
	 (new (or (string-match "^:\\([^! :]+\\)!\\([^@ :]+\\)@\\([^ :]+\\)"
				line)))
	 (triple (if new (cons (subfield line 1)
			       (cons (subfield line 2)
				     (subfield line 3)))))
	 (rest (if new (substring line (match-end 0))))
	 (irc-userhost (if new (format "%s@%s"
				       (car (cdr triple)) (cdr (cdr triple)))))
	 (line (if new (format ":%s%s" (car triple) rest) line))
	 (last-NOTICE-rcv irc-last-NOTICE-rcv)
	 (last-NOTICE-src irc-last-NOTICE-src))
    (if (not nl)
	(irc-insert "%%WARNING: got incomplete line from server! No newline."))
    (setq irc-last-NOTICE-rcv ""
	  irc-last-NOTICE-src "")
    (if (and nil
	     (or (string-match "\\@VIA\\.CMU1\\.MERIT\\.EDU$"
			       (upcase (or irc-userhost "")))
		 (string-match "\\@35\\.198\\.225\\.[0-9]+$"
			       (upcase (or irc-userhost ""))) 
		 )
	     (string-match (concat "^: *\\([^: ]+\\) +\\(PRIVMSG\\|NOTICE\\)"
				   " +[^:]+:\\(.*\\)"
				   "\\([\0\3-\17\21-\25\27-\37]\\)"
				   "\\(.*\\)$")
			   line))
	(let ((jerk (subfield line 1))
	      (dump (concat (subfield line 3)
			    (subfield line 4)
			    (subfield line 5))))
	  (irc-send (format (concat "PRIVMSG %s :Haha! I saw"
				    " your message \"%s\". Do you think I"
				    " care %s?")
			    jerk dump jerk))

	  ;;(irc-insert "DEBUG: str=\"%s\"." str)
	  ;;(irc-insert "DEBUG: irc-userhost=\"%s\"." irc-userhost)
	  ;;(irc-insert "IGNORED LINE: \"%s\"." line)
	  '(""))
	(if (not (boundp 'irc-count-incoming-messages))
	    (progn
	      (message "?Kiwi: Variable irc-count-incoming-messages unbound")
	      (set (make-local-variable 'irc-count-incoming-messages) nil)))
	(if (not (numberp irc-count-incoming-messages))
	    (progn
	      (message "?Kiwi: irc-count-incoming-messages=%d, not a number"
		       irc-count-incoming-messages)
	      (setq irc-count-incoming-messages 0)))
	(setq line (irc-lowlevel-dequote line)
	      irc-count-incoming-messages (1+ irc-count-incoming-messages))
	(if (= 0 (% irc-count-incoming-messages 50))
	    (irc-send (concat "PONG " irc-server)))
	;(sit-for 0)			;Be nice, make emacs reschedule tasks
	(cons
	 ;; the part of str not being parsed.
	 (substring str (1+ (string-match "\n" str)))
	 (cond
	   ;; each function here should return t or nil indicating whether
	   ;; to issue a signal.  Some of these regexps are fugly because
	   ;; of inconsistent protocol use by the servers.  Fortunately Jarkko
	   ;; is fixing that.
	   ((string-match "^ *$" line) nil)	;Ignore empty lines.
	   ((string-match "^:[^: ]+ +MSG " line) (irc-parse-public line))
	   ((string-match "^:[^: ]+ +CHANNEL " line) (irc-parse-channel line))
	   ((string-match "^:[^: ]+ +JOIN " line) (irc-parse-channel line))
	   ((string-match "^:[^: ]+ +PART " line) (irc-parse-channel line))
	   ((string-match "^:[^: ]+ +INVITE " line) (irc-parse-invite line))
	   ((string-match "^:[^: ]+ +NICK " line) (irc-parse-nick line))
	   ((string-match "^:[^: ]+ +WALL " line) (irc-parse-wall line))
	   ((string-match "^:[^: ]+ +WALLOPS " line) (irc-parse-wallops line))
	   ((string-match "^:[^: ]+ +QUIT " line) (irc-parse-quit line))
	   ((string-match "^:[^: ]+ +KICK " line) (irc-parse-kick line))
	   ((string-match "^:[^: ]+ +KILL " line) (irc-parse-kill line))
	   ((string-match "^:[^: ]+ +TOPIC " line) (irc-parse-topic line))
	   ((string-match "^:[^: ]+ +MODE " line) (irc-parse-mode-reply line))
	   ((string-match "^:[^: ]+ +[023][0-9][0-9][^0-9]" line)
	    (irc-parse-RPL line))
	   ((string-match "^:[^: ]+ +[4-5][0-9][0-9][^0-9]" line)
	    (irc-parse-ERR line))
	   ((string-match (concat "^\\(:?\\)\\([^: ]*\\) *NOTICE"
				  " +\\([^: ]+\\) *:"
				  "\\([^\001]*\001[^\001]*\001\\)")
			  line)
	    (setq irc-last-NOTICE-rcv last-NOTICE-rcv
		  irc-last-NOTICE-src last-NOTICE-src)
	    (let* ((colon (subfield line 1))
		   (from (subfield line 2))
		   (to (subfield line 3))
		   (msg (concat (subfield line 4)
				(substring line (match-end 0))))
		   (frm (if (string= "" from) irc-server from)))
	      (if (irc-is-nickname frm)
		  (irc-remember frm 'irc-nicknames))
	      (while (string-match "^\\([^\001]*\\)\\(\001[^\001]*\001\\)" msg)
		(let ((start (subfield msg 1))
		      (ctcp (subfield msg 2))
		      (end (substring msg (match-end 0))))
		  (setq msg (concat start end))
		  (if (string= "\001\001" ctcp)
		      (setq msg (concat msg "\001"))
		      (irc-parse-CLIENT-answer frm to ctcp))))
	      (if (not (string-match "^ *$" msg))
		  (irc-parse-server-msg
		   (concat colon from " NOTICE " to " :" msg "\n")))))
	   ((string-match "^\\(:[^: ]* +\\)?NOTICE " line)
	    (setq irc-last-NOTICE-rcv last-NOTICE-rcv
		  irc-last-NOTICE-src last-NOTICE-src)
	    (irc-parse-notice line))
	   ((string-match (concat "^\\(:?\\)\\([^: ]*\\) *PRIVMSG"
				  " +\\([^ ]+\\) +:"
				  "\\([^\001]*\001[^\001]*\001\\)") 
			  line)
	    (let* ((colon (subfield line 1))
		   (from (subfield line 2))
		   (to (subfield line 3))
		   (msg (concat (subfield line 4)
				(substring line (match-end 0))))
		   (frm (if (string= "" from) irc-server from)))
	      (if (irc-is-nickname frm)
		  (irc-remember frm 'irc-nicknames))
	      (while (string-match "^\\([^\001]*\\)\\(\001[^\001]*\001\\)" msg)
		(let ((start (subfield msg 1))
		      (ctcp (subfield msg 2))
		      (end (substring msg (match-end 0))))
		  (setq msg (concat start end))
		  (if (string= "\001\001" ctcp)
		      (setq msg (concat msg "\001"))
		      (irc-parse-CLIENT-query frm to ctcp))))
	      (if (not (string-match "^ *$" msg))
		  (irc-parse-server-msg
		   (concat colon from " PRIVMSG " to " :" msg "\n")))))
	   ((string-match "^\\(:[^: ]* +\\)?PRIVMSG " line)
	    (irc-parse-priv line))
	   ((string-match "^PING " line) (irc-pong))
	   ((string-match "^\\(:[^: ^]* +\\)?PONG " line)
	    (irc-parse-pong line))
	   ((string-match "^ERROR " line) (irc-parse-error line))
	   ((string-match "^WHOREPLY " line) (irc-parse-whoreply line))
	   ((string-match "^NAMREPLY " line) (irc-parse-namreply line))
	   ((string-match "^LINREPLY " line) (irc-parse-linreply line))
	   (t (irc-insert (concat "%%Received unkown message from server,"
				  " in function irc-parse-server-msg:"))
	      (irc-insert "%% \"%s\" (cleaned up line)."
			  (irc-clean-up-message line))
	      (irc-insert (concat "%% Please let %s know about this;"
				  " it might be a bug.")
			  irc-hacker)
	      nil)))
	)
    ))

 
(defun irc-parse-channel (str)
  "Examine CHANNEL, JOIN and PART messages from the IRC server.
CHANNEL indicates a user entering or leaving the channel which you are
on, JOIN indicates a user entering a channel and PART indicates a user
leaving the channel. If the user is not being ignored and \"join\" is
in irc-events, a message is inserted indicating the change.  A
message is always provided as confirmation if the user is the irc-mode
user.

This function returns t if a bell should be issued for the \"join\" event,
nil otherwise."
  (if (not (or (string-match (concat "^: *\\([^ ]+\\) +\\(JOIN\\)"
				     " +:?\\([^ ]+\\) +\\([^ ]*\\) *$")
			     str)
	       (string-match (concat "^: *\\([^ ]+\\) +\\(JOIN\\)"
				     " +:?\\([^ ]+\\) *\\(\\)$")
			     str)
	       (string-match (concat "^: *\\([^ ]+\\) +\\(PART\\)"
				     " +:?\\([^ ]+\\) +\\([^ ]*\\) *$")
			     str)
	       (string-match (concat "^: *\\([^ ]+\\) +\\(PART\\)"
				     " +:?\\([^ ]+\\) *\\(\\)$")
			     str)
	       (string-match (concat "^: *\\([^ ]+\\) +\\(CHANNEL\\)"
				     " +:?\\([^ ]+\\) *\\(\\)$")
			     str)))
      (progn (irc-insert (concat "%%Unknown format on command from server,"
				 " please notify %s, it migh be a bug.")
			 irc-hacker)
	     (irc-insert "%% str=\"%s\"." str)
	     (irc-insert "%% Function irc-parse-channel."))
      (let* ((user (subfield str 1))
	     (type (subfield str 2))
	     (channel (subfield str 3))
	     (mode (subfield str 4))
	     (left (or (string= type "PART")
		       (and (string= type "CHANNEL") (string= channel "0"))))
	     (pd (make-string (max 0 (1- (length channel))) ? ))
	     (cont (concat irc-msg-cont pd)))
	(if (string= user irc-nick-used)
	    (progn
	      (if left
		  (irc-forget channel 'irc-subscribed-channels)
		  (irc-remember channel 'irc-subscribed-channels)))
	    )
	(if (string= user irc-nick-used)
	    (progn
	      (if left
		  (if (irc-server-has-multijoinable-channels)
		      (if (or (string= (upcase channel) (upcase irc-channel))
			      (string= irc-channel "0"))
			  (progn
			    (setq irc-channel "0")
			    (irc-show-subscribed-channels))
			  (progn
			    (if nil
				(irc-remember channel
					      'irc-subscribed-channels))
			    (irc-show-subscribed-channels)))
		      (progn
			(if nil
			    (irc-remember irc-channel
					  'irc-subscribed-channels))
			(setq irc-channel "0")
			(irc-show-subscribed-channels)))
		  (progn (setq irc-channel channel)
			 (if nil
			     (irc-remember channel 'irc-subscribed-channels))
			 (irc-send (concat "MODE " irc-channel))
			 (irc-show-subscribed-channels))))
	    (if (and (not (irc-recall user 'irc-ignored-ppl))
		     (memq 'join irc-events))
		(progn
		  (if left
		      (irc-insert "%s%s has LEFT channel %s%s"
				  irc-msg-info-pre
				  user
				  (if (irc-server-has-multijoinable-channels)
				      channel
				      irc-channel)
				  irc-msg-info-post)
		      (progn
			(irc-remember user 'irc-nicknames)
			(irc-insert "%s%s has JOINED channel %s%s"
				    irc-msg-info-pre
				    user
				    channel
				    irc-msg-info-post)))
		  )
		(irc-signal user 'join)))))) ; check for signal for join


(defun irc-parse-CLIENT-answer (sndr rcvr str)
  "Examine a client answer message from another client.

Generally, incoming message will look like \"^ACLIENT keyword arg1 .. argN^A\"
one or several pasted together."
  (if (not (string-match "^\001\\([^\001]*\\)\001$" str))
      (progn (irc-insert "%%Got non-clientmessage in irc-parse-CLIENT-answer:")
	     (irc-insert "%% \"%s\" (str)." str)
	     (irc-insert "%% Please tell %s, probaly en internal error."
			 irc-hacker))
      (setq str (irc-ctcp-dequote
		 (substring str (match-beginning 1) (match-end 1)))))
  (if (not (string-match (concat " *\\([^\001 \t]+\\)"	;1 keyword
				 " *:?"			;optional colon
				 " *\\([^\001]*\\)"		;2 data
				 " *$")
			 str))
      (if (string< "" str)
	  (progn (irc-insert "%%Received unknown CTCP answer message:")
		 (irc-insert "%% \"%s\" (str)." (irc-clean-up-message str))
		 (irc-insert "%% This is probaly NOT a bug.")))
      (let* ((keyw (subfield str 1))
	     (args (subfield str 2))
	     (pre (concat irc-msg-info-pre "User \"" sndr "\" "))
	     (irc-msg-cont-used (make-string (length pre) ? )))
	(cond
	  ((and (string= "MYCROFT" (upcase sndr))
		(string-match "^ *Are +we +having +fun +yet *\\? *$" args))
	   nil)
	  ((string= keyw "ACTION")	;Ignore ACTION replies.
	   )
	  ((string= keyw "CLIENTINFO")
	   (irc-insert (concat "%sis using a client which is using the"
			       " client-to-client-protocol keyword %s in the"
			       " following way: %s%s")
		       pre keyw args irc-msg-info-post))
	  ((string= keyw "ECHO")
	   (irc-insert "%sechoed back \"%s\" to us with (CTCP) ECHO%s"
		       pre args irc-msg-info-post))
	  ((string= keyw "PING")
	   (cond ((not (string-match "^[0-9]+$" args))
		  (irc-insert (concat "%%Received bogus (CTCP) PING reply from"
				      " user %s: \"%s\".")
			      sndr args))
		 (t (let* ((cur (car (cdr (irc-current-time))))
			   (rep (string-to-int args))
			   (ans (- cur rep))
			   (tos (/ (* 10 (if (< ans 0)
					     (+ cur (- 65536 rep))
					     ans))
				   2))
			   (int-part (/ tos 10))
			   (dec-part  (- tos (* 10 int-part))))
		      (irc-insert (concat "%sis about %d.%d seconds"
					  " away from you%s")
				  pre int-part dec-part
				  irc-msg-info-post)))))
	  ((string= keyw "ERRMSG")
	   (cond ((string-match "^PING +RELAY +\\([0-9]+\\)\\( +:.*\\)? *$"
				args)
		  (irc-parse-CLIENT-answer sndr
					   rcvr
					   (format "\001PING %s\001"
						   (subfield args 1))))
		 ((string-match "^ACTION " args)
		  ;; Ignore complaints about CTCP ACTION.
		  )
		 (t (irc-insert (concat "%%Errormessage from client of"
					" user %s: \"%s\".")
				sndr args))))
	  ((string= keyw "FINGER")
	   (let* ((data (if (string-match (concat "^ *Please +check +"
						  "my +USERINFO +instead"
						  " *:")
					  args)
			    (substring args (match-end 0))
			    args)))
	     (if (string-match (concat "^\\(.* +Idle *\\)\\(for +\\)?"
				       "\\([0-9]+\\) *seconds? *$")
			       data)
		 (let* ((m (subfield data 1))
			(idle (subfield data 3))
			(xpre (concat "\"" (irc-clean-up-message m)))
			(idle-time (string-to-int idle))
			(xpost (if (= 1 idle-time) "second" "seconds"))
			(idle-string (if (>= idle-time 60)
					 (format "%s %s\" (%s)"
						 idle
						 xpost
						 (irc-sec-to-time idle-time))
					 (concat idle " " xpost "\""))))
		   (irc-insert "%shas FINGER-info %s%s"
			       pre
			       (concat xpre idle-string)
			       irc-msg-info-post))
		 (irc-insert "%shas fingerinfo \"%s\"%s"
			     pre data irc-msg-info-post))))
	  ((string= keyw "SOURCE")
	   (cond
	     ((string-match "^ *$" args)
	      (irc-insert "%sEnd of source site listing%s"
			  irc-msg-info-pre irc-msg-info-post))
	     ((string-match "^ *\\([^: ]+\\) *: *\\([^: ]+\\) *: *\\(.*\\) *"
			    args)
	      (let ((site (subfield args 1))
		    (dir (subfield args 2))
		    (files (subfield args 3)))
		(irc-insert "%sSource site: %s (directory %s, files %s)%s"
			    irc-msg-info-pre
			    site dir files
			    irc-msg-info-post)))
	     (t (irc-insert "%%Received unknown CTCP %s reply from %s: %s"
			    keyw sndr args))))
	  ((string= keyw "USERINFO")
	   (if (or (string-match "^ *$" args)
		   (string-match
		    "^ *\\(HUH *\\?\\) *WHO +WANTS +TO +KNOW *\\? *$"
		    (upcase args))
		   (string= "<None Supplied>" args)
		   (string= "And what you want to know?" args)
		   (string= "No user info given." args)
		   )
	       (irc-insert (concat "%shasn't given any information"
				   " about him/herself%s")
			   pre irc-msg-info-post)
	       (progn
		 (string-match "\\(\\.?\\)$" args)
		 (let ((info (substring args 0 (match-beginning 0)))
		       (dot (subfield args 2)))
		   (irc-insert (concat "%shas given the information"
				       " \"%s\"%s about him/herself%s")
			       pre
			       (irc-clean-up-message info)
			       dot
			       irc-msg-info-post)))))
	  ((or (string= keyw "VERSION")	;CLIENT_VERSION
	       (string= keyw "VRSN"))
	   (if (string-match "^ *\\([^:]+\\) *: *\\([^:]+\\) *: *\\(.*\\) *$"
			     args)
	       (let ((name (subfield args 1))
		     (vrsn (subfield args 2))
		     (env (subfield args 3)))
		 (irc-insert (concat "%sUser \"%s\" claims to be using"
				     " client %s version %s (%s)%s")
			     irc-msg-info-pre
			     sndr name (irc-clean-up-message vrsn) env
			     irc-msg-info-post))
	       (irc-insert "%sclaims to be using client %s%s"
			   pre
			   (irc-clean-up-message
			    (let ((c (cond
				       ((or (string-match
					     (concat
					      "^ *[Ii][Rr][Cc][Ii][Ii] +"
					      "\\([0-9]+\\.[0-9]+[^:]+\\) *:"
					      "[^0-9]*\\([0-9]\\.[0-9][^ \\.]+"
					      "*\\).*$")
					     args)
					    (string-match
					     (concat
					      "^ *[Ii][Rr][Cc][Ii][Ii] +"
					      "[^:]*: *\\(.*[0-9]\\."
					      "[0-9].*\\) *\\(\\)$")
					     args)
					    (string-match
					     (concat
					      "^ *[Ii][Rr][Cc][Ii][Ii] +"
					      "\\([^:]+\\) *:.*\\(\\)$")
					     args))
					(concat "ircII "
						(subfield args 1)
						(subfield args 2)))
				       ((string-match
					 (concat "^[^:]*: *\\(.*[0-9]+\\."
						 "[0-9]+.*.*\\) *$")
					 args)
					(subfield args 1))
				       ((string-match
					 "^[^:]*[0-9]+\\.[0-9]+[^:]*"
					 args)
					(substring args 0 (match-end 0)))
				       (t args))))
			      (irc-nuke-whitespace
			       (progn
				 (while
				     (or (string-match
					  " The one true client\\." c)
					 (string-match (concat
							" Who could ask for"
							" anything more\\.")
						       c)
					 (string-match "SIAWPIO!" c))
				   (setq c (concat
					    (substring c 0 (match-beginning 0))
					    (substring c (match-end 0)))))
				 c))))
			   irc-msg-info-post)))
	  ((string= keyw "VRSN")
	   (if (string-match "^ *\\([^:]+\\) *: *\\([^:]+\\) *: *\\(.*\\) *$"
			     args)
	       (let ((name (subfield args 1))
		     (vrsn (subfield args 2))
		     (env (subfield args 3)))
		 (irc-insert (concat "%sUser \"%s\" claims to be using"
				     " client %s version %s (%s)%s")
			     irc-msg-info-pre
			     sndr name vrsn env
			     irc-msg-info-post))
	       (irc-insert "%%Received unknown (CTCP) %s reply from %s: %s"
			   keyw sndr args)))
	  (t (irc-insert (concat "%%Received unknown answer with keyword"
				 " \"%s\" from client of user \"%s\":"
				 " \"%s\".")
			 keyw sndr args))))))


(defun irc-parse-CLIENT-query (sndr rcvr str)
  "Examine a client query message from another client.

Generally, incoming message will look like \"^A CLIENT keyword data
... ^A\" where sndr is nick of client query is comming from, rcvr is
nick of receiver (either one self or a channel or a broadcast), and
keyword being a token in the set VERSION, USERINFO, ERRMSG etc. Arg1
to argN are optional." 
  (cond
    ((not (string-match "^\001\\([^\001]*\\)\001$" str))
     (progn (irc-insert "%%Got non-CTCP query in irc-parse-CLIENT-query:")
	    (irc-insert "%% \"%s\" (str)." str)
	    (irc-insert "%% Please tell %s, probaly an internal error."
			irc-hacker)))
    (nil (or (string-match "\\@VIA\\.CMU1\\.MERIT\\.EDU$"
		       (upcase (or irc-userhost "")))
	 (string-match "\\@35\\.198\\.225\\.[0-9]+$" (or irc-userhost "")))
     nil)
    (t (setq str (irc-ctcp-dequote
		  (substring str (match-beginning 1) (match-end 1))))
       (if (not (string-match (concat " *\\([^\001 \t]+\\)" ;1 keyword
				      " *\\([^\001]*\\)" ;2 data
				      " *$")
			      str))
	   (if (and debug-on-error
		    (string< "" str))
	       (progn
		 (irc-insert "%%Received unknown CTCP request:")
		 (irc-insert "%% \"%s\" (str)." (irc-clean-up-message str))
		 (irc-insert "%% This is probaly not a bug.")))
	   (let ((keyw (substring str (match-beginning 1) (match-end 1)))
		 (args (substring str (match-beginning 2) (match-end 2)))
		 (tell (memq 'ctcp irc-events)))
	     (cond
	       ((string= keyw "ACTION") ;CLIENT_ACTION
		(if (string= "" args)
		    (irc-send (format "NOTICE %s :\001ERRMSG ACTION :%s\001"
				      sndr
				      "No argument supplied."))
		    (irc-insert ">>> (To %s) User %s %s"
				rcvr
				sndr
				(irc-clean-up-message args))))
	       ((string= keyw "CLIENTINFO")
		(cond ((string-match "^ *$" args) ;No arguments
		       (irc-send
			(concat "NOTICE " sndr " :\001"
				(irc-ctcp-enquote
				 (concat
				  "CLIENTINFO :You can request help of the"
				  " commands ACTION CLIENTINFO ECHO ERRMSG"
				  " FINGER PING SOURCE USERINFO VERSION"
				  " by giving an argument to CLIENTINFO."))
				"\001"))
		       (if tell
			   (irc-insert
			    "%sAnswered (CTCP) CLIENTINFO query to %s by %s%s"
			    irc-msg-info-pre rcvr sndr irc-msg-info-post)) )
		      ((string-match "^ *\\([^: ]+\\) *$" args) ;1 arg
		       (let* ((w (substring args
					    (match-beginning 1)
					    (match-end 1)))
			      (uw (upcase w))
			      (info (cond
				      ((string= uw "ACTION")
				       (concat
					"ACTION takes one or more arguments,"
					" and displayes them as a \"MUD like"
					" feeling\" to this user. If nick"
					" sojge sends \"falls down\", this"
					" user gets a message looking more or"
					" less like \"User sojge falls"
					" down\"."))
				      ((string= uw "CLIENTINFO")
				       (concat
					"CLIENTINFO with 0 arguments gives"
					" a list of known client query"
					" keywords. With 1 argument,"
					" a description of the client query"
					" keyword is returned."))
				      ((string= uw "ECHO")
				       (concat
					"ECHO returns whatever argument is"
					" given."))
				      ((string= uw "ERRMSG")
				       (concat
					"ERRMSG is returned either when the"
					" query keyword is ERRMSG (in which"
					" case all arguments are echoed) or"
					" when an error in a query is"
					" detected or some other error"
					" happens in connection to CTCP (in"
					" which case the query is returned as"
					" the replies arguments, with a short"
					" error message added)."))
				      ((string= uw "FINGER")
				       (concat
					"FINGER shows user's real name, login"
					" name, client machine and idle"
					" time."))
				      ((string= uw "PING")
				       (concat
					"PING takes a integer number as its"
					" argument and echoes it back to the"
					" sender."))
				      ((string= uw "SOURCE")
				       (concat
					"SOURCE takes 0 arguments and returns"
					" a description of where to find the"
					" source code of the client. The"
					" description is made up out of zero"
					" or more lines followed by an end"
					" marker. Every line is a CTCP"
					" reply with the SOURCE keyword,"
					" a space, the name of a FTP-server, a"
					" colon, a directory name, a colon,"
					" and 0 or more file names."
					" If no file names are given, all the"
					" files in the named directory are"
					" needed. The end marker contains just"
					" the keyword."))
				      ((string= uw "VERSION")
				       (concat
					"VERSION takes 0 arguments and"
					" returns a list of words consisting"
					" of clients name, any number of"
					" versions (starting with the major"
					" version), and ending with the"
					" enviroment the client runs in. A"
					" colon and a plain text descrpition"
					" of the clients version is appended"
					" after the list."))
				      ((string= uw "USERINFO")
				       (concat
					"USERINFO takes no arguments and"
					" returns a user settable"
					" string."))
				      (t nil))))
			 (cond ((stringp info)
				(irc-send
				 (concat "NOTICE " sndr " :\001"
					 (irc-ctcp-enquote
					  (concat "CLIENTINFO :" info))
					 "\001"))
				(if tell
				    (irc-insert
				     (concat "%sAnswered (CTCP)"
					     " CLIENTINFO %s to %s by %s%s")
				     irc-msg-info-pre
				     uw
				     rcvr
				     sndr
				     irc-msg-info-post)))
			       (t (irc-send
				   (concat
				    "NOTICE " sndr " :\001"
				    (irc-ctcp-enquote
				     (concat
				      "ERRMSG " keyw " " args
				      " :Unknown keyword in CLIENTINFO"
				      " client query. Send CLIENTINFO"
				      " CLIENTINFO for help."))
				    "\001"))))))
		      (t (irc-send
			  (concat "NOTICE " sndr " :\001"
				  (irc-ctcp-enquote
				   (concat "ERRMSG " keyw " " args " :"
					   "CLIENTINFO takes 0 or 1"
					   " argument. Send"
					   " CLIENTINFO CLIENTINFO"
					   " for help."))
				  "\001"))
			 (if tell
			     (irc-insert (concat
					  "%sComplained about user %s's"
					  " corrupted CTCP CLIENTINFO query to"
					  " %s%s")
					 irc-msg-info-pre
					 sndr
					 rcvr
					 irc-msg-info-post)))))
	       ((string= keyw "ECHO") 
		(irc-send (format "NOTICE %s :\001ECHO %s\001"
				  sndr
				  (irc-ctcp-enquote args)))
		(if tell
		    (irc-insert (concat "%sAnswered to (CTCP) ECHO request"
					" from %s to %s, echoed back \"%s\"%s")
				irc-msg-info-pre rcvr sndr args
				irc-msg-info-post)))
	       ((string= keyw "ERRMSG")
		(irc-send (format "NOTICE %s :\001ERRMSG %s :No errors.\001"
				  sndr args))
		(cond ((not tell)
		       )
		      ((string-match "^ *PING +RELAY +[0-9]+ *$" args)
		       (irc-insert (concat "%sAnswered to (CTCP) ERRMSG PING"
					   " RELAY request to %s from %s%s")
				   irc-msg-info-pre rcvr sndr
				   irc-msg-info-post))
		      (t (irc-insert (concat "%sAnswered to (CTCP) ERRMSG"
					     " request to %s from %s, echoed"
					     " back \"%s\"%s")
				     irc-msg-info-pre
				     rcvr sndr (concat args " :No errors")
				     irc-msg-info-post))))
	       ((string= keyw "FINGER")
		(let* ((idle (irc-idle-time))
		       (verbose-idle (concat "(" (irc-sec-to-time idle) ") "))
		       (plur (not (= 1 idle))))
		  (irc-send (format (concat "NOTICE %s :\001FINGER :Please"
					    " check my USERINFO instead :%s"
					    " (%s@%s) %d second%s %sha%s"
					    " passed since %s gave a command"
					    " last.\001")
				    sndr
				    (user-full-name)
				    (user-real-login-name)
				    (system-name)
				    idle
				    (if plur "s" "")
				    (if (>= idle 60) verbose-idle "")
				    (if plur "ve" "s")
				    irc-nick-used)))
		(if tell
		    (irc-insert "%sAnswered (CTCP) FINGER query to %s by %s%s"
				irc-msg-info-pre rcvr sndr irc-msg-info-post)))
	       ((string= keyw "PING")
		(cond ((string-match "^[0-9]+$" args)
		       (irc-send (format "NOTICE %s :\001PING %s\001"
					 sndr
					 (irc-ctcp-enquote args)))
		       (if tell
			   (irc-insert
			    "%sAnswered to (CTCP) PING query to %s from %s%s"
			    irc-msg-info-pre rcvr sndr irc-msg-info-post)))
		      (t (irc-send (format "NOTICE %s :\001%s %s :%s\001"
					   sndr
					   "ERRMSG PING"
					   (irc-ctcp-enquote args)
					   "Argument was not a number"))
			 (cond
			   (tell
			    (irc-insert (concat "%%Received unknown CTCP PING"
						" query to %s from user"
						" \"%s\":")
					rcvr sndr)
			    (irc-insert "%% \"%s\" (args)." args)
			    (irc-insert "%% This is probaly NOT a bug."))))))
	       ((string= keyw "SOURCE")
		(let ((s irc-ftp-source))
		  (while (not (null s))
		    (let* ((r (car s))
			   (site (nth 0 r))
			   (dir (nth 1 r))
			   (files (nth 2 r)))
		      (irc-send
		       (concat "NOTICE " sndr " :\001"
			       (irc-ctcp-enquote
				(format "SOURCE %s:%s:%s"
					site
					dir
					(irc-listify files " " "")))
			       "\001"))
		      (setq s (cdr s))))
		  (irc-send (concat "NOTICE " sndr " :\001SOURCE\001"))
		  (if tell
		      (irc-insert
		       "%sAnswered (CTCP) SOURCE query to %s by %s%s"
		       irc-msg-info-pre
		       rcvr sndr
		       irc-msg-info-post))))
	       ((string= keyw "USERINFO") ;CLIENT_USERINFO
		(irc-send (concat "NOTICE " sndr " :\001"
				  (irc-ctcp-enquote
				   (concat "USERINFO :"
					   (if (stringp irc-userinfo)
					       irc-userinfo
					       "")))
				  "\001"))
		(if tell
		    (irc-insert
		     "%sAnswered (CTCP) USERINFO query to %s by %s%s"
		     irc-msg-info-pre rcvr sndr irc-msg-info-post)))
	       ((string= keyw "VERSION")
		(cond ((string-match (concat
				      "^ *\\([^: ]+\\) +v?\\([0-9]+[^ ]+\\)"
				      " +\\(.*\\) *$")
				     irc-version)
		       (irc-send
			(concat "NOTICE " sndr " :\001"
				(irc-ctcp-enquote
				 (format "%s %s:%s:%s, %s, emacs %s"
					 keyw
					 (subfield irc-version 1)
					 (subfield irc-version 2)
					 (subfield irc-version 3)
					 (cond
					   (irc-emacs-knows-ISO8859-1
					    "8bit ISO 8859-1 characters")
					   (irc-translation-table-incoming
					    "8bit characters")
					   (t (concat "7bit ISO 646 characters"
						      " (ie ASCII with"
						      " friends)")))
					 emacs-version))
				"\001"))
		       (if tell
			   (irc-insert
			    "%sAnswered (CTCP) %s query to %s by %s%s"
			    irc-msg-info-pre keyw rcvr sndr
			    irc-msg-info-post)))
		      (t (irc-send (concat "NOTICE " sndr " :\001"
					   (irc-ctcp-enquote
					    (format "%s Kiwi::%s"
						    keyw
						    irc-version))
					   "\001"))
			 (cond
			   (tell
			    (irc-insert
			     "%sAnswered (CTCP) %s query to %s by %s%s"
			     irc-msg-info-pre keyw rcvr sndr irc-msg-info-post)
			    (irc-insert "%% Client doesn't know who it is!")
			    (irc-insert "%% Please tell %s." irc-hacker))))))
	       (t (irc-send
		   (concat "NOTICE " sndr " :\001"
			   (irc-ctcp-enquote
			    (concat "ERRMSG "
				    keyw
				    " "
				    args
				    " :Query is unknown"))
			   "\001"))
		  (cond
		    (tell
		     (irc-insert (concat "%%Received unknown CTCP \"%s\" to %s"
					 " from user \"%s\".")
				 str rcvr sndr))))))))))


(defun irc-parse-ERR (str)
  "Examine a numeric ERR_ message from the IRC server. Numeric
control messages are used by newer servers to aid in generalized
client design; while people are converting to the new servers the
older irc-parse-error, irc-parse-notice, et al, functions are
redundant with irc-parse-ERR and irc-parse-RPL.  Values used by this
function are found in the IRC source file numeric.h.

Note well that some things are still going to come out wrong because the
servers are currently still doing things inconsistently."
  (if (not (string-match (concat "^:\\(.*\\) +\\([4-5][0-9][0-9]\\) +"
				 "\\([^: ]*\\)? *:? *\\(.*\\) *$")
			 str))
      (progn
	(irc-insert (concat "%%Received an ERR message with unknown format in"
			    " function irc-parse-ERR:"))
	(irc-insert "%% \"%s\"." str)
	(irc-insert "%%Please report this to %s." irc-hacker))
      (let*
	  ;; we assume that the server and message are consistent for us; just
	  ;; worry about the numeric value and the rest of the line
	  ((srvr (subfield str 1))
	   (code (subfield str 2))
	   (txt (subfield str 4))
	   (num (string-to-int code))
	   (parorg (if (string= (upcase srvr) (upcase irc-server))
		       ""
		       (concat "(" srvr ") ")))
	   (tmp1 nil))
	(if (not (string= "" srvr))
	    (irc-remember srvr 'irc-servernames))
	(cond
	  ((= num 401)		; ERR_NOSUCHNICK
	   (cond ((string-match (concat "^[^: ]+ +401 +[^: ]+ +"
					":Hunting +for +ghosts +?.*$")
				str)
		  nil)		;Ignore "Hunting for ghosts ?" message.
		 ((string-match (concat "^[^: ]+ +401 +[^: ]+ +"
					":Cannot +kick +user +off +"
					"channel *$")
				str)
		  (irc-insert "%%%sCan't boot that user from that channel."
			      parorg))
		 ((string-match "^[^:@ ]+" txt)
		  (let* ((user (substring txt
					  (match-beginning 0)
					  (match-end 0)))
			 (is-service (irc-recall user 'irc-services)))
		    (if is-service
			(irc-insert (concat "%%Service %s isn't reachable at"
					    " the moment, please try again"
					    " later.")
				    is-service)
			(progn
			  (if (irc-is-nickname user)
			      (irc-forget user 'irc-nicknames))
			  (irc-insert (concat
				       "%%%sThere is no user called"
				       " \"%s\" on IRC at the moment.")
				      parorg
				      user)))
		    (if (and (>= irc-major-version 2)
			     (>= irc-minor-version 6))
			(irc-send (concat "WHOWAS :" user))
			)))
		 (t (irc-insert (concat "%%%sUnrecognized NO SUCH NICK"
					" message follows; please tell %s:")
				parorg
				irc-hacker)
		    (irc-insert "%% \"%s\"." str)
		    (irc-insert "%%Function irc-parse-err, at 401."))))
	  ((= num 402)		; ERR_NOSUCHSERVER
	   (cond ((or (string-match (concat "^ *\\(.+\\) *: *"
					    "\\(\\*\\*\\* * \\)?No +such +"
					    "server *.? *$")
				    txt)
		      (string-match (concat "^:? *\\** *No +such"
					    " +server *( *"
					    "\\([^: ]+\\) *) *.?"
					    " *$")
				    txt))
		  (irc-insert (concat "%%%sThere is no server \"%s\" on the"
				      " IRCnet at the moment.")
			      parorg
			      (irc-nuke-whitespace (subfield txt 1))))
		 (t (irc-insert (concat "%%Unknown ERR 402 message received"
					" in function irc-parse-ERR:"))
		    (irc-insert "%% txt=\"%s\"." txt)
		    (irc-insert "%% Please tell %s, it might be a bug."
				irc-hacker))))
	  ((= num 403)		; ERR_NOSUCHCHANNEL
	   (if (string= (upcase srvr) (upcase irc-server))
	       (let ((chn (if (string-match "^\\([^: ]+\\) *:.*$" txt)
			      (subfield txt 1))))
		 (if chn
		     (irc-insert (concat "%%%sThere is no channel called"
					 " \"%s\" in use right now.")
				 parorg chn)
		     (irc-insert "%%%sNo such channel in use." parorg)))))
	  ((= num 404)		; ERR_CANNOTSENDTOCHAN
	   (if (string-match "^ *\\([^ :]+\\) *:" txt)
	       (irc-insert "%%%sChannel %s rejected the message."
			   parorg (subfield txt 1))
	       (irc-insert "%%%sChannel rejected the message." parorg)))
	  ((= num 405)
	   (cond ((string-match (concat "^ *\\([^ ]+\\) *: *You +have +joined"
					" +too +many +channels.? *$")
				txt)
		  (irc-insert (concat "%%Failed joining channel %s, as you're"
				      " already listening to too many"
				      " channels.")
			      (subfield txt 1)))
		 (t (irc-insert "*** %s %s." parorg txt))))
	  ((= num 406)
	   (if (string-match "^ *\\([^: ]+\\) *: *\\(.*\\) *$"
			     txt)
	       (let ((m (substring txt (match-beginning 2) (match-end 2)))
		     (n (substring txt (match-beginning 1) (match-end 1))))
		 (irc-insert
		  "%%No trace left about \"%s\" in history of server %s."
		  n srvr))
	       (progn
		 (irc-insert "%%Unknown type 406 error message from server:")
		 (irc-insert "%% \"%s\" (str)." str)
		 (irc-insert "%% Please tell %s, it migth be a bug."
			     irc-hacker))))
	  ((= num 411)		; ERR_NORECIPIENT
	   (irc-insert "%%%sThe last message had no recipient." parorg))
	  ((= num 412)		; ERR_NOTEXTTOSEND
	   (irc-insert "%%%sThe last message had no text to send." parorg))
	  ((= num 413)
	   (irc-insert "%%%sNo top level domain specified (no such receiver)."
		       parorg))
	  ((= num 421)		; ERR_UNKNOWNCOMMAND
	   (cond ((string-match "^CLIENT-SYNCH :?\\(.*\\) *:Unknown command$"
				txt)
		  (let* ((cmd (irc-nuke-whitespace (subfield txt 1)))
			 (ucmd (upcase cmd)))
		    (cond
		      ((or t (string-match "^QUOTE " ucmd))
		       (irc-insert "%sEnd of quoted command%s"
				   irc-msg-info-pre irc-msg-info-post))
		      (t (irc-insert (concat "%%Unknown internal"
					     " synchronisation in"
					     " irc-parse-ERR at 421:"))
			 (irc-insert "%% \"%s\" (txt)." txt)
			 (irc-insert "%% Please tell %s, this *is* a bug."
				     irc-hacker)))))
		 ((string-match (concat "^\\(MODE\\|KICK\\|JOIN\\|PART\\)"
					".*Unknown :?command *$")
				txt)
		  nil)
		 ((or (string-match "^\\(.*\\) :?Unknown :?command$" txt)
		      (string-match "^:?Unknown command \\(.*[^ ].*\\) *$"
				    txt))
		  (if (string= (upcase irc-server) (upcase srvr))
		      (irc-insert "%%%sUnknown server command: %s."
				  parorg
				  (subfield txt 1))))
		 (t (irc-insert (concat "%%%sUnkown server command,"
					" reported in unknown format:")
				parorg)
		    (irc-insert "%% \"%s\" (txt)." txt)
		    (irc-insert "%% Function irc-parse-err, at 421."))))
	  ((or (= num 422)		;ERR_NOMOTD
	       (= num 423))		;ERR_NOADMIN
	   (irc-insert "%%%s%s" parorg txt))
	  ((= num 431)		; ERR_NONICKNAMEGIVEN
	   (irc-insert "%%%sNo nickname give to change to." parorg))
	  ((= num 432)		; ERR_ERRONEUSNICKNAME
	   (irc-insert (concat "%%%sBad format for nickname change. Your"
			       " nickname is \"%s\".")
		       parorg irc-nick-used))
	  ((= num 433)		; ERR_NICKNAMEINUSE
	   (if (or (string-match (concat "^:[^: \t]+ +433 +"
					 "\\([^: \t]+\\) +"
					 "\\([^: \t]+\\) +:")
				 str)
		   (string-match (concat "^:[^: \t]+ +433 +\\(\\)"
					 "\\([^: \t]+\\) +:")
				 str))
	       (let ((current (subfield str 1))
		     (failed (subfield str 2)))
		 (irc-insert (concat "%%%sNickname \"%s\" is already being"
				     " used, please choose another one.")
			     parorg
			     failed)
		 (irc-insert "%sHmmm ... looks like you're still %s%s"
			     irc-msg-info-pre
			     (if (string= current "")
				 (concat "without a nickname, use \"/HELP"
					 " NICK\" to get help about how to"
					 " set one")
				 (concat "\"" current "\""))
			     irc-msg-info-post))
	       (irc-insert "%%Unknown ERR 433 type message received:")
	       (irc-insert "%% \"%s\" (str)." str)
	       (irc-insert "%% Please tell %s." irc-hacker)))
	  ((= num 436)
	   (cond ((string-match "^\\([^ :]+\\) :Nickname collision KILL$" txt)
		  (let ((nick (subfield txt 1)))
		    (irc-insert "%sUser %s kiled due to NAMECLASH at %s%s"
				irc-msg-info-pre
				nick
				srvr
				irc-msg-info-post)
		    (irc-forget nick 'irc-nicknames)))
		 (t (irc-insert "%%Unknown ERR 436 type message received:")
		    (irc-insert "%% \"%s\" (txt)." txt)
		    (irc-insert "%% Please tell %s." irc-hacker))))
	  ((= num 441)		; ERR_USERNOTINCHANNEL
	   (if (string= (upcase irc-server) (upcase srvr))
	       (irc-insert "%%%sYou're not on any channel." parorg)))
	  ((= num 442)		; ERR_NOTONCHANNEL
	   (cond ((or (string-match (concat "^ *\\([^ :]+\\) +"
					    "\\([^ :]+\\) *: *"
					    "\\([^ :]+\\) +is +not"
					    " +here *.? *$")
				    txt)
		      (string-match (concat "^ *\\([^: ]+\\) *\\(\\):\\"
					    "([^: ]+\\) +is +not +here *$")
				    str))
		  (let* ((o1 (subfield txt 1))
			 (c (subfield txt 2))
			 (o2 (subfield txt 3))
			 (chn (if (string= c "") irc-channel c))
			 (name (if (string= o1 o2)
				   o1
				   (concat o2 " (" o1 ")"))))
		    (irc-insert (concat "%%You can't change the channel"
					" operator status of %s while s/he"
					" isn't here on %s.")
				name chn)))
		 ((string-match (concat "^ *\\([^ :]+\\) +\\([^ :]+\\)"
					" *: *\\(isn't +on +your"
					" +channel\\|Cannot +kick"
					" +user +off\\ +channel\\)")
				txt)
		  (let ((who (subfield txt 1))
			(chn (subfield txt 2))
			(msg (subfield txt 3)))
		    (cond ((string-match "^isn't" msg)
			   (irc-insert (concat "%%Can't /KICK %s while"
					       " s/he's not on your"
					       " channel %s.")
				       who chn))
			  (t (irc-insert (concat "%%Failed to /KICK %s from"
						 " channel %s.")
					 who chn)))))
		 ((string-match (concat "^: *[^: ]+ +442 +[^: ]+"
					" +\\([^: ]+\\) *: *"
					"isn't *on *your *"
					"channel *!* *$")
				str)
		  (let ((u (subfield str 1)))
		    (irc-insert (concat "%%Can't /kick user %s while s/he's"
					" not on this channel.")
				u)))
		 ((or (string-match (concat ":[^: ]+ +442 +[^: ]+"
					    " +\\([^: ]+\\) +: *You're +not"
					    " +on +channel *$")
				    str)
		      (string-match
		       (concat "^:[^: ]+ +442 +[^: ]+ +\\([^ ]+\\) *: *Not"
			       " *On *Channel$")
		       str)
		      (string-match (concat "^[^: ]+ +442 +[^: ]+"
					    " +\\([^ ]+\\): *You're +not +on"
					    " +channel +\\([^ ]+\\) *$")
				    str)
		      (string-match (concat "^:[^: ]+ +442 +[^: ]+"
					    " *: *You're +not on"
					    " +channel +\\([^ ]+\\) *$")
				    str))
		  (let ((c (subfield str 1)))
		    (irc-insert "%%You're not on channel %s." c)))
		 ((string= (upcase irc-server) (upcase srvr))
		  (irc-insert (concat "%%Unknown format on"
				      " ERR_NOTONCHANNEL message;"
				      " please tell %s, it might be"
				      " a bug:")
			      irc-hacker)
		  (irc-insert "%% \"%s\" (str)." str)
		  (irc-insert "%% Function irc-parse-ERR, at 442."))))
	  ((= num 443)
	   (if (string-match (concat "^\\([^: ]+\\) +\\([^: ]+\\) +"
				     ":is already on channel.?$")
			     txt)
	       (let ((nick (subfield txt 1))
		     (chan (subfield txt 2)))
		 (irc-insert "%%User %s is already on channel %s." nick chan))
	       (irc-insert "%%Unknown ERR433 message seen in irc-parse-ERR:")
	       (irc-insert "%% \"%s\" (txt)." txt)
	       (irc-insert "%% Please tell %s." irc-hacker)))
	  ((= num 444)
	   (if (string-match "^\\([^: ]+\\) +:\\(.*\\)$" txt)
	       (let ((user (subfield txt 1))
		     (mesg (subfield txt 2)))
		 (irc-insert "%%%sNo such user as \"%s\" logged in."
			     parorg user))
	       (irc-insert "%%%sNo such user logged in." parorg)))
	  ((= num 445)
	   (irc-insert "%%%s%s" parorg txt))
	  ((= num 446)
	   (irc-insert "%%%sCommand /%s" parorg txt))
	  ((= num 451)		; ERR_NOTREGISTERED
	   (irc-insert "%%%sYou haven't checked in yet.  Choose a nickname."
		       parorg))
	  ((= num 461)		; ERR_NEEDMOREPARAMS
	   (if (or (string= "" srvr)
		   (string= (upcase irc-server) (upcase srvr)))
	       (irc-insert (concat "%%%sThere weren't enough arguments for"
				   " the last command.")
			   parorg)))
	  ((= num 462)		; ERR_ALREADYREGISTRED
	   (irc-insert "%%%sYou've already registered." parorg))
	  ((= num 463)		; ERR_NOPERMFORHOST
	   (irc-insert "%%%sYour host isn't permitted." parorg))
	  ((= num 464)		; ERR_PASSWDMISMATCH
	   (irc-insert "%%%sThat password is incorrect." parorg))
	  ((= num 465)		; ERR_YOUREBANNEDCREEP
	   (irc-insert "%%%sYou've been banned from IRC." parorg))
	  ((= num 466)		; ERR_YOUWILLBEBANNED
	   (irc-insert "%%%sYou will be banned from IRC (%s)." parorg str))
	  ((= num 467)
	   (if (string-match "^\\([^ :]+\\) :" txt)
	       (irc-insert (concat "%%%sChannel %s already has a key, use"
				   " /MODE %s to see it.")
			   parorg
			   (subfield txt 1)
			   (subfield txt 1))
	       (irc-insert (concat "%%%sChannel already has a key, use /MODE"
				   " to see it.")
			   parorg)))
	  ((= num 471)		; ERR_CHANNELISFULL
	   (string-match "^[^: ]+" txt)
	   (let ((s (substring txt (match-beginning 0) (match-end 0))))
	     (irc-insert (concat "%%%sChannel %s is full. (Check limit with"
				 " /MODE %s)")
			 parorg s s)))
	  ((= num 472)		; ERR_UNKNOWNMODE
	   (if (not (string-match (concat "^: *\\([^: ]+\\) +472 +"
					  "\\([^: ]+\\) +\\(.*\\) +:?is +"
					  "unknown +mode +char +to +me *$")
				  str))
	       (progn (irc-insert (concat "%%Unrecognized type 472 message;"
					  " please tell %s:")
				  irc-hacker)
		      (irc-insert "%% \"%s\"." str)
		      (irc-insert "%% Function irc-parse-ERR, at 472."))
	       (let ((srv (substring str (match-beginning 1) (match-end 1)))
		     (nick (substring str (match-beginning 2) (match-end 2)))
		     (chr (substring str (match-beginning 3) (match-end 3))))
		 (if (not (string= (upcase srvr) (upcase srv)))
		     (irc-remember srv 'irc-servernames))
		 (irc-remember nick 'irc-nicknames)
		 (irc-insert "%%Character \"%s\" is not a MODE character."
			     chr))))
	  ((= num 473)		; ERR_INVITEONLYCHAN
	   (if (not (string-match (concat "^:[^: ]+ +473 +[^: ]+ +"
					  "\\([^ ]+\\) +.*")
				  str))
	       (progn (irc-insert (concat "%%%sUnknown format on"
					  " ERR_INVITEONLYCHAN; please tell"
					  " %s, it might be a bug:")
				  parorg
				  irc-hacker)
		      (irc-insert "%% \"%s\" (str)." str)
		      (irc-insert "%% Function irc-parse-ERR, at 473."))
	       (let ((chn (substring str (match-beginning 1) (match-end 1))))
		 (irc-insert (concat "%%%sYou need an invitation to join"
				     " invitation-only channel \"%s\".")
			     parorg
			     chn))))
	  ((= num 474)
	   (cond
	     ((string-match "^\\([^ ]+\\) +:" txt)
	      (let ((channel (subfield txt 1)))
		(irc-insert (concat "%%Can't join channel %s -- you're"
				    " banned from it.")
			    channel)))
	     (t (irc-insert "%%Unknown ERR474 seen in irc-parse-ERR:")
		(irc-insert "%% \"%s\" (txt)" txt)
		(irc-insert "%% Please tell %s, it might be a bug."
			    irc-hacker))))
	  ((= num 475)			;ERR_CANT_JOIN
	   (cond ((string-match "^\\([^ ]+\\) +:" txt)
		  (let ((channel (subfield txt 1)))
		    (irc-insert (concat "%%Can't join channel %s -- you"
					" haven't supplied the right channel"
					" key/password."))))
		 (t (irc-insert "%%Unknown ERR475 seen in irc-parse-ERR:")
		    (irc-insert "%% \"%s\" (txt)" txt)
		    (irc-insert "%% Please tell %s, it might be a bug."
				irc-hacker))))
	  ((= num 481)		; ERR_NOPRIVILEGES
	   (if (string= (upcase irc-server) (upcase srvr))
	       (if (string-match "CHANNEL" str)
		   (irc-insert
		    (concat "%%%sYou must be a channel operator"
			    " to "
			    (cond
			      ((string-match (concat ":Cannot set topic, not"
						     " channel OPER$")
					     str)
			       "set the channels topic.")
			      (t "do that.")))
		    parorg)
		   (irc-insert (concat "%%%sYou must be an enabled IRC"
				       " Operator to do THAT!")
			       parorg))))
	  ((= num 482)		; ERR_NOOPERHOST
	   (if (and (irc-server-has-channelname-in-msgs)
		    (string-match "^\\([^ ]+\\) *:.*$" txt))
	       (irc-insert "%%%sYou're not a channel operator for %s!"
			   parorg
			   (subfield txt 1))
	       (irc-insert (concat "%%%sYou're not a channel operator on"
				   " that channel!")
			   parorg)))
	  ((= num 491)		; ERR_NOOPERHOST
	   (irc-insert (concat "%%Password check failed at server %s,"
			       " you're still a disabled luser.")
		       (upcase srvr)))
	  ((= num 501)		; ERR_UMODEUNKNOWNFLAG
	   (irc-insert "%%%s." txt))
	  ((= num 502)		; ERR_USERSDONTMATCH
	   (irc-insert (concat "%%You can't change, not even look at, other"
			       " user's modes.")))
	  (t                                 ; default
	   (irc-insert (concat "%%%sUnrecognized numeric ERR message"
			       " follows; please tell %s:")
		       parorg
		       irc-hacker)
	   (irc-insert "%% \"%s\" (str)." str)
	   (irc-insert "%%Function irc-parse-err, at catch all.")))))
  nil)


(defun irc-parse-error (str)
  "Examine an ERROR message from the IRC server.
ERROR is used when something bogus happens like an unparsable command
is issued to the server.  Usually this will not happen unless something
like /QUOTE is used.  This message is also used when a user attempts to
change to a name that already exists.

Returns nil; currently no signals are issued for an error."
  (string-match " +:" str)		;Skip "^ERROR :"
  (setq str (substring str (match-end 0)))
  (cond
    ((string-match (concat "^ *Nickname +[^: ]* +\\(is \\)?"
			   "\\(already\\|not +chan\\|in use\\) *$")
		   str)
     (irc-insert "%%%s" str)
     ;; either we couldn't change the current nickname
     (setq irc-nick (or (and irc-nick (get 'irc-nick 'o-nick))
			;; or we never even had one
			"NO NAME YET (/NICK to set one)"))
     (set-buffer-modified-p (buffer-modified-p))
     (irc-insert (if (get 'irc-nick 'o-nick)
		     "%sHmmm ... looks like you're still \"%s\"%s" 
		     "%s%s%s")
		 irc-msg-info-pre
		 irc-nick
		 irc-msg-info-post))
    ((string-match " *No +option +specified. *Try +MAIL +HELP *$" str)
     (irc-insert "%%No option specified; try /HELP MAIL and /MAIL HELP."))
    ((or (string-match (concat "Closing +Link *: *\\([^[ :]+\\[[^]]+\\]\\)"
			       " *\\(\\)(\\(.*\\)) *$")
		       str)
	 (string-match (concat "Closing +Link *: *\\([^ :]+\\) +\\([^ :]+\\)"
			       " *\\(.\\|\n\\)+$")
		       str))
     (let* ((id (subfield str 1))
	    (srce (subfield str 2))
	    (preason (subfield str 3))
	    (reason (if (and (> (length preason) 0)
			     (= ?\050 (aref preason 0))
			     (= ?\051 (aref preason (1- (length preason)))))
			(substring preason 1 (1- (length preason)))
			preason))
	    (matches (string-match "^\\([^[]+\\)\\[\\([^]]+\\)\\]$" id))
	    (nick (if matches (subfield id 1) ""))
	    (host (if matches (subfield id 2) "")))
       (irc-parse-quit (format ":%s QUIT %s :%s"
			       nick
			       nick
			       reason))))
    (t (irc-insert "%%%s" str)))
  nil)


(defun irc-parse-invite (str)
  "Examine an INVITE message from the IRC server.
INVITE is sent when one user invites another to a channel.
If the inviter is not being ignored a message is inserted in the buffer.

This function returns t if a bell should be issued for the \"invite\" event,
nil otherwise."
  (let ((user (substring str 1 (string-match " +INVITE " str)))
        (to (substring str (match-end 0)
                       (string-match " +:?" str (match-end 0))))
        (channel (substring str (match-end 0))))
    ;; glom a new name, if necessary
    (irc-remember user 'irc-nicknames)
    (if (irc-recall user 'irc-ignored-ppl)
        (irc-send (concat "NOTICE " user " :You are being ignored by \""
			  irc-nick-used
			  "\"."))
	(progn
	  (irc-insert "%sUser %s invites %s to join channel %s%s"
		      irc-msg-info-pre
		      user
		      (if (string= (downcase to) (downcase irc-nick-used))
			  "you"
			  to)
		      (irc-clean-up-message channel)
		      irc-msg-info-post)
	  (irc-signal user 'invite)))))


(defun irc-parse-kick (str)
  "Examine and display a KICK message."
  (if (not
       (string-match "^: *\\([^: ]+\\) +KICK +\\([^: ]+\\) +\\([^: ]+\\) *"
		     str))
      (progn (irc-insert (concat "%%Unknown format on KICK message; please"
				 " tell %s, it might be a bug:")
			 irc-hacker)
	     (irc-insert "%% \"%s\" (str)." str)
	     (irc-insert "%% Function irc-parse-kick."))
      (let ((actor (substring str (match-beginning 1) (match-end 1)))
	    (chan (substring str (match-beginning 2) (match-end 2)))
	    (victim (substring str (match-beginning 3) (match-end 3))))
	(irc-remember actor 'irc-nicknames)
	(irc-remember victim 'irc-nicknames)
	(cond ((and (string= (upcase actor) (upcase victim))
		    (string= (upcase actor) (upcase irc-nick-used)))
	       (irc-insert "%sYou kicked yourself from channel %s%s"
			   irc-msg-info-pre chan irc-msg-info-post))
	      ((string= (upcase actor) (upcase victim))
	       (irc-insert "%sUser %s kicked him/herself from channel %s%s"
			   irc-msg-info-pre actor chan irc-msg-info-post))
	      ((string= (upcase victim) (upcase irc-nick-used))
	       (irc-insert (concat "%%You have been kicked out from"
				   " channel %s by user %s.")
			   chan
			   actor))
	      ((string= (upcase actor) (upcase irc-nick-used))
	       (irc-insert (concat "%sYou have kicked out user %s from"
				   " channel %s%s")
			   irc-msg-info-pre
			   victim
			   chan
			   irc-msg-info-post))
	      (t (irc-insert (concat "%sUser %s kicked out user %s"
				     " from channel %s%s")
			     irc-msg-info-pre
			     actor
			     victim
			     chan
			     irc-msg-info-post)))
	(if (string= (upcase victim) (upcase irc-nick-used))
	    ;; No need to irc-maintain the victim, -parse-chan does that.
	    (progn (if (irc-server-has-multijoinable-channels)
		       (irc-parse-channel (concat ":" victim " PART " chan))
		       (irc-parse-channel (concat ":" victim " CHANNEL 0")))
		   t)
	    nil))))


(defun irc-parse-kill (str)
  "Examine a KILL message from the IRC server.
For a client this means its connexion will be closing momentarily.  This rather
drastic turn of events will always get a signal so this function returns t."
  (if (not (string-match "\\(:[^: ]+ +\\)?KILL +\\([^: ]+\\) +:" str))
      (progn
	(irc-insert "%%Spurios KILL message: \"%s\" (str) in irc-parse-kill."
		    str)
	(irc-insert "%% Please tell %s, it might be a bug." irc-hacker))
      (let* ((s (subfield str 1))
	     (v (subfield str 2))
	     (i (substring str (match-end 0)))
	     (server (irc-nuke-whitespace s))
	     (victim (irc-nuke-whitespace v))
	     (info (irc-nuke-whitespace i)))
	(cond ((string-match "\\([^!]+\\)!\\([^! ]+\\) +"
			     info)
	       (let* ((s (subfield info 1))
		      (o (subfield info 2))
		      (m (substring info (match-end 0)))
		      (c (if (and (= ?\050 (aref m 0))
				  (= ?\051 (aref m (1- (length m)))))
			     (substring m 1 (1- (length m)))
			     m)))
		 (if (string= (upcase irc-nick-used) (upcase victim))
		     (irc-insert (concat "%sYou have been /KILL'ed by operator"
					 " %s@%s (%s)%s")
				 irc-msg-info-pre
				 (irc-nuke-whitespace o)
				 (irc-nuke-whitespace s)
				 (irc-nuke-whitespace c)
				 irc-msg-info-post)
		     (irc-insert "%sOperator %s@%s /KILL'ed user %s (%s)%s"
				 irc-msg-info-pre
				 (irc-nuke-whitespace o)
				 (irc-nuke-whitespace s)
				 victim
				 (irc-nuke-whitespace m)
				 irc-msg-info-post))))
	      (t (irc-insert "%sUser %s KILL'ed by %s%s"
			     irc-msg-info-pre
			     victim
			     info
			     irc-msg-info-post)))))
  t)


(defun irc-parse-linreply (str)
  "Examine a LINREPLY message from the IRC server.
LINREPLY is used to answer a LINKS request to show all the servers on line.
\"Links\" is a bit of a misnomer since little information regarding the
actual structure of the IRCnet can be gained from these messages.

No signals are issued for lines from the LINREPLY."
  (if (not (string-match "^LINREPLY +\\([^: ]+\\) +" str))
      (progn (irc-insert (concat "%%Unkown LINREPLY format in "
				 "function irc-parse-linreply:"))
	     (irc-insert "%% \"%s\"." str)
	     (irc-insert "%% Please tell %s, it may be a bug." irc-hacker))
      (let ((old-mark irc-mark)
	    (server-name (substring str (match-beginning 1) (match-end 1)))
	    (server-info (substring str (match-end 0))))
	(irc-remember server-name 'irc-servernames)
	(if (and (irc-terminal-is-slow)
		 (or (irc-server-has-end-of-links) ;New server version ?
		     (and (= irc-major-version 2) (= irc-minor-version 2))))
	    (progn (irc-remember (format "Server %s: %s"
					 (upcase server-name)
					 server-info)
				 'irc-linksinfo)
		   (if (and (not (irc-terminal-is-slow))
			    (string= (upcase server-name) (upcase irc-server)))
		       (progn
			 (irc-insert irc-links-header)
			 (irc-insert irc-links-stroke)
			 (set-buffer-modified-p (buffer-modified-p))
			 (irc-recall-all-and-display 'irc-linksinfo
						     (progn
						       (string-match
							"^[^: ]* ."
							irc-links-header)
						       (match-end 0))
						     "servers"
						     "server")
			 (irc-forget-all 'irc-linksinfo))))
	(progn (irc-insert "Server %s: %s"
			       (upcase server-name)
			       server-info)))))
  nil)


(defun irc-parse-mode-reply (str)
  "Examine a MODE reply message from the IRC server.
MODE replies are used in respnse to the MODE command to indicate the current,
ie new, status of a specified channel or user.

No signals are issued for MODE replies."
  (cond ((string-match (concat "^ *: *\\([^: ]+\\) +MODE +"
			       "\\([^: ]+\\) +:? *")
		       str)
	 (let ((orig (subfield str 1))
	       (chan (subfield str 2))
	       (mode (irc-nuke-whitespace (substring str (match-end 0)))) )
	   (cond ((irc-is-hostname orig)
		  (irc-remember orig 'irc-servernames))
		 ((irc-is-nickname orig)
		  (irc-remember orig 'irc-nicknames)))
	   (cond ((irc-is-nickname chan)
		  (let ((i (1- (length mode)))
			(um (upcase mode)))
		    (while (and (>= i 0) (not (= ?O (aref um i))))
		      (setq i (1- i)))
		    (while (and (>= i 0)
				(not (= ?- (aref um i)))
				(not (= ?+ (aref um i))))
		      (setq i (1- i)))
		    (if (and (>= i 0) (= ?- (aref um i)))
			(setq irc-operator nil)
			(set-buffer-modified-p (buffer-modified-p))))
		  (irc-insert "%sMode for user %s changed by %s: %s%s"
			      irc-msg-info-pre
			      chan
			      orig
			      (irc-explain-user-mode mode)
			      irc-msg-info-post))
		 ((irc-is-nickname orig)
		  (irc-insert "%sUser %s changed mode for channel %s: %s%s"
			      irc-msg-info-pre
			      orig
			      chan
			      (irc-explain-channel-mode mode)
			      irc-msg-info-post))
		 ((irc-is-hostname orig)
		  (irc-remember orig 'irc-servernames))
		 (t (irc-insert "%%Unknown sender in irc-parse-mode-reply:")
		    (irc-insert "%% \"%s\" (str), \"%s\" (orig)." str orig)
		    (irc-insert "%% Please tell %s, this might be a bug."
				irc-hacker)))))
	(t (irc-insert "%%Unkown format on MODE reply; please tell %s:"
		       irc-hacker)
	   (irc-insert "%% \"%s\"." str)
	   (irc-insert "%% Function irc-parse-mode-reply.")))
  nil)


(defun irc-parse-namreply (str)
  "Examine a NAMREPLY message from the IRC server.
NAMREPLY is used in repsonse to NAMES to indicate what users are on what
channels.  All users on secret or private channels which the client is not
on are grouped together on one private channel.

No signals are issued for NAMREPLYs."
  (if (not (string-match "^NAMREPLY +[^ ]+ +\\([^ ]+\\) +" str))
      (progn (irc-insert (concat "%%Unknown format on NAMREPLY message,"
				 " in function IRC-PARSE-NAMREPLY:"))
	     (irc-insert "%% \"%s\"." str))
      (let* ((channel (substring str (match-beginning 1) (match-end 1)))
	     (users (substring str (match-end 0)))
	     (count 0)
	     (to-insert "")
	     (nick nil)
	     (format-string "%s  %3d   %s")
	     (irc-msg-cont-used irc-names-cont-msg))
	;; yet another source of information for irc-nicknames.
	(while (string-match "^\\([^ ]+\\)\\( \\|$\\)" users)
	  (setq nick (substring users 0 (match-end 1))
		users (substring users (match-end 0))
		count (1+ count)
		to-insert (concat to-insert " " nick))
	  (irc-remember nick 'irc-nicknames)) 
	(if (and (irc-terminal-is-slow) (irc-server-has-end-of-names))
	    (let ((lin (format format-string
				(irc-format-channel channel)
			       count
			       (irc-clean-up-message to-insert)))) 
	      (irc-remember lin 'irc-namtree))
	    (irc-insert format-string
			(irc-format-channel channel)
			count
			(irc-clean-up-message to-insert)))))
  nil)


(defun irc-parse-nick (str)
  "Examine a NICK message from the IRC server.
NICK is sent when a user's nickname is changed, but it is only sent to the
people on the same channel as the user.  If the person changing names is
being ignored, this fact is tracked across the change.  If notification
is not enabled for \"nick\" then no message is inserted.

This function returns t if a signal should be issued for the \"nick\" event,
nil otherwise."
  (cond ((string-match "^: *\\([^ :]+\\) +NICK +:?\\([^ :]+\\) *$" str)
	 (let* ((old (subfield str 1))
		(new (subfield str 2))
		(current irc-nick-used))
	   (if (not (irc-recall old 'irc-services)) ;Always keep /services.
	       (irc-forget old 'irc-nicknames))
	   (irc-remember new 'irc-nicknames)
	   (cond ((string= (upcase irc-nick-used) (upcase old))
		  ;; Our own nick has changed.
		  (setq irc-nick-used new)
		  (set-buffer-modified-p (buffer-modified-p))))
	   (cond ((irc-recall old 'irc-ignored-ppl)
		  (irc-forget old 'irc-ignored-ppl)
		  (irc-remember new 'irc-ignored-ppl)
		  nil)			;Don't signal this.
		 ((and (memq 'nick irc-events)
		       (not (string= (upcase new) (upcase current))))
		  (irc-insert "%s%s is now known as %s%s"
			      irc-msg-info-pre old new irc-msg-info-post)
		  (irc-signal old 'user))
		 (t nil))))
	(t (irc-insert "%%Unknown NICK seen in irc-parse-nick:")
	   (irc-insert "%% \"%s\" (str)." str)
	   (irc-insert "%% Please tell %s, this might be a bug." irc-hacker))))


;irc-remember
(defun irc-parse-notice (str)
  "Examine a NOTICE message from the IRC server.
NOTICE is the catch-all for IRC messages; if it can't be classified as
one of the other currently existing messages then the information is
sent as NOTICE.  This message is overused, even when it another could be
used instead.  For example, if an attempt is made to send to a nickname
which is not on IRC the error reply is sent via NOTICE.

No signal is issued for NOTICE because it is way too random with what it
means."
  (let* ((lst (if (string-match (concat "^ *:? *\\([^: ]*\\)? *NOTICE *"
					"\\([^: ]*\\)? *:")
				str)
		  (list (substring str (match-beginning 1) (match-end 1)) ;srvr
			(substring str (match-beginning 2) (match-end 2)) ;rcvr
			(substring str (match-end 0))) ;msg
		  (list "" "" str)))
	 (srvnam (if (string= (car lst) "") irc-server (car lst)))
	 (srvr (if (string= (upcase srvnam) (upcase irc-server)) "" srvnam))
	 (parorg (if (string= "" srvr) "" (concat "(" srvr ") ")))
	 (rcvr (car (cdr lst)))
	 (msg (car (cdr (cdr lst))))
	 (cmsg (irc-clean-up-message msg))
	 (retval nil))			;Kludge!
    (if (not (stringp irc-server))
	(setq irc-server (if (stringp (default-value 'irc-server))
			     (default-value 'irc-server)
			     "")))
    (if (and rcvr (irc-is-nickname rcvr))
	(setq irc-nick-used rcvr))
    (irc-remember srvr 'irc-servernames)
    (cond
      ((and (string= "POXAV" (upcase srvr))
	    (string-match "weenie" msg))
       )
      ((irc-recall srvr 'irc-services)
       (irc-remember srvr 'irc-services) ;Update spelling.
       (irc-remember srvr 'irc-nicknames)
       (irc-insert "%s%s%s %s"
		   irc-msg-info-pre srvr irc-msg-info-post cmsg))
      ((string-match (concat "^\\*\\*\\* *Notice *-- *Access *denied *"
			     "(\\(.*\\)) *\\(.*\\) *$")
		     msg)
       (let ((reason (subfield msg 1))
	     (refused-server (subfield msg 2)))
	 (irc-insert "%%Refused server-link connection from server %s: %s."
		     (upcase refused-server)
		     reason)))
      ((string-match (concat "^\\*\\*\\* *Notice *-- *Rehashing *Server"
			     " *config *file *(\\(.*\\)) *$")
		     msg)
       (let ((file (subfield msg 1)))
	 (irc-insert "%sServer %s has reloaded it's configuration file %s%s"
		     irc-msg-info-pre
		     (upcase srvnam)
		     file
		     irc-msg-info-post)))
      ((string-match (concat "^\\*\\*\\* *Notice *-- *\\(Hack\\|Fake\\) *:"
			     " *\\([^ ]+\\) +MODE +\\([^ ]+\\)"
			     " *\\([^ ]*\\).*$")
		     msg)
       ;;Ignore this information. It's just a way for enabled operators on 2.7
       ;; server to see people on 2.6 server as they join channels with initial
       ;; modes, such as secret or private. Hopefulle this will go away when
       ;; 2.6 servers go way.
       (let ((user (subfield msg 2))
	     (channel (subfield msg 3))
	     (mode (irc-nuke-whitespace (subfield msg 4))))
	 (cond ((string-match "\\+[^-]*[sp]" mode)
		(irc-insert (concat "%sWarning a user about seeing her/his"
				    " secret channel join%s")
			    irc-msg-info-pre irc-msg-info-post)
		(irc-send (concat "NOTICE " user " :<Automatic warning> My"
				  " client (but not I myself -- I don't even"
				  " know who you are unless you'll tell me or"
				  " you happen to be marked as being AWAY)"
				  " saw you join channel "
				  channel
				  " ("
				  mode
				  "). The fact was broadcasted to everyone"
				  " seeing WALLOPS messages on 2.7 (and later)"
				  " servers. Your channel is probably"
				  " without any special mode on other servers."
				  " (But still look \"valid\" at yous site)."
				  " If you have any questions, then don't send"
				  " them to "
				  irc-nick
				  " but to the channel #Twilight_Zone."
				  ))
		(irc-send (concat "NOTICE " user " :<Automatic warning> If"
				  " you don't care about getting warned, but"
				  " want to think you're invisible while being"
				  " visible, and you happen to use a Kiwi"
				  " client, you can do M-x set-variable RET"
				  " irc-ignore-automatic-warnings RET t"
				  " RET. If you're using ircII, instead do"
				  " /on ^notice \"" irc-nick " <Automatic"
				  " warning>*\: /^comment"))
		))))
      ((string-match (concat "^\\*\\*\\* Message-of-today is missing"
			     " on host \\([^: ]+\\)? *$")
		     msg)
       (let ((host (irc-clean-up-message (subfield msg 1))))
	 (if (not (string= (upcase srvr) (upcase host)))
	     (irc-remember host 'irc-servernames))
	 (irc-insert "%%%sNo message of the day at server %s."
		     (if (or (string= srvr irc-server)
			     (string= srvr ""))
			 ""
			 (concat "(" srvr ") "))
		     (upcase host))))
      ((string-match "^MOTD *- ?*\\([^: ]+\\)? *message +of +the +day *-? *$"
		     msg)
       (let ((server (irc-clean-up-message (substring msg
						      (match-beginning 1)
						      (match-end 1)))))
	 (if (not (string= (upcase srvr) (upcase server)))
	     (irc-remember server 'irc-servernames))
	 (irc-insert "")
	 (irc-insert ">>> Message of the day at server %s:" server)
	 (set-buffer-modified-p (buffer-modified-p))))
      ((string-match "^MOTD *- ?" msg)
       (setq irc-motd-lines (append irc-motd-lines
				    (list (irc-clean-up-message
					   (substring msg (match-end 0)))))))
      ((string-match "^\\* *end +of +/?motd +command. *$" msg)
       (while irc-motd-lines
	 (irc-insert ">>> %s" (car irc-motd-lines))
	 (setq irc-motd-lines (cdr irc-motd-lines))))
      ((string-match (concat "^\\*\\*\\* *Error *: *No +mere +mortals +"
			     "may +trace +the +nets +of +the +universe *$")
		     msg)
       (irc-insert (concat "%%%sYou must be an enabled IRC operator to trace"
			   " the IRCnet, use /OPER to enable yourself. An"
			   " alternative is to use /STATS L, do /HELP STATS.")
		   parorg))
      ((string-match "^Good afternoon, gentleman\\. I am a HAL 9000" msg)
       (if (string= srvr "")
	   (irc-insert "%sOperator status for %s ENABLED%s"
		       irc-msg-info-pre
		       irc-nick-used
		       irc-msg-info-post)
	   (irc-insert "%sOperator status at %s for %s ENABLED%s"
		       irc-msg-info-pre
		       srvr
		       irc-nick-used
		       irc-msg-info-post))
       ;; we've been granted operator privileges.  the string is for mode-line
       (setq irc-operator " IOPR")
       (set-buffer-modified-p (buffer-modified-p)))
      ((string-match (concat "^\\*\\*\\* *notice *-- *Received"
			     " +unauthorized +connection +from +")
		     msg)
       (irc-insert (concat "%%Something at internet host %s tried to"
			   " establish a connection with us. Refused"
			   " as it isn't enabled in the confiugation file.")
		   (irc-nuke-whitespace (substring msg (match-end 0)))))
      ((string-match (concat "^\\*\\*\\* *Notice *\\(:\\|--\\) *Link"
			     " +with +\\([^ ]+\\) +established"
			     " *.? *$")
		     msg)
       ;; In a few seconds, we ought to issue a LINKS command to the server,
       ;; but without displaying the answer, only storing it. Sigh.
       (let* ((h (irc-extract-hostname
		  (irc-clean-up-message
		   (subfield msg 2))))
	      (host (if (stringp h) h "")))
	 (if (not (string= (upcase srvr) (upcase host)))
	     (irc-remember host 'irc-servernames))
	 (irc-insert "%s%sLink with %s established%s"
		     irc-msg-info-pre
		     (if (string= srvr "") "" (concat "(" srvr ")"))
		     (upcase host)
		     irc-msg-info-post))
       (irc-later-execute-lusers))
      ((or (string-match (concat "^\\*\\*\\* *\\([^ ]+\\) +\\([^:]+\\) *"
				 "==> *\\(.+\\) *$")
			 cmsg)
	   ;; 2.6 following
	   (string-match (concat "^\\*\\*\\* +\\(Connection\\) +\\([^ ]+\\) +"
				 "==> +\\([^ ]+\\) +\\[\\([^ ]+\\)\\] *$")
			 cmsg)
	   (string-match (concat "^\\*\\*\\* +\\(Connection\\) +\\([^ ]+\\) +"
				 "==> +\\([^ ]+\\) *$")
			 cmsg)
	   (string-match (concat "^\\*\\*\\* +\\(Link\\) +\\([^ ]+\\) +"
				 "==> +\\([^ ]+\\) *$")
			 cmsg)
	   (string-match (concat "^\\*\\*\\* +\\(<newtype>\\) +\\([^ ]+\\) +"
				 "==> +\\([^ ]+\\) *$")
			 cmsg))
       (let* ((type (substring cmsg (match-beginning 1) (match-end 1)))
	      (lcl (substring cmsg (match-beginning 2) (match-end 2)))
	      (remote (substring cmsg (match-beginning 3) (match-end 3)))
	      (address (if (match-beginning 4)
			   (substring cmsg (match-beginning 4) (match-end 4))
			   remote))
	      (extra (cond
		       ((not (string= (upcase remote) (upcase address)))
			(concat "(" address ")"))
		       ((string-match
			 "^\\([^ ]+\\) +\\([0-9]+\\)S +\\([0-9]+\\)C$"
			 remote)
			(let ((x (subfield remote 1))
			      (s (string-to-int (subfield remote 2)))
			      (c (string-to-int (subfield remote 3))))
			  (setq remote x)
			  (format " (%d server%s, %d client%s)"
				  s
				  (if (= 1 s) "" "s")
				  c
				  (if (= 1 c) "" "s"))))
		       (t "")))
	      (p (cond
		   ((string-match "^ *CLASS\\[\\([0-9]+\\)\\] +\\([^: ]+\\) *$"
				  lcl)
		    (cons (subfield lcl 2) (subfield lcl 1)))
		   ((string-match "^ *CLASS\\[\\([0-9]+\\)\\] *$" lcl)
		    (cons srvr (subfield lcl 1)))
		   (t (cons lcl nil))))
	      (local (irc-nuke-whitespace (car p)))
	      (class (cdr p))
	      (lserv (irc-extract-hostname local))
	      (rserv (irc-extract-hostname remote)))
	 (if (and lserv (not (string= (upcase srvr) (upcase lserv))))
	     (irc-remember lserv 'irc-servernames))
	 (if (and rserv (not (string= (upcase srvr) (upcase rserv))))
	     (irc-remember remote 'irc-servernames))
	 (cond
	   ((string= (upcase type) "LINK")
	    (irc-insert "%s%s to %s%s goes through %s%s"
			irc-msg-info-pre
			type
			(upcase remote)
			extra
			(upcase local)
			irc-msg-info-post))
	   ((and (string-match "\\(CHANOP\\|OPER\\|USER\\)" (upcase type))
		 (numberp (string-match "^\\([^[]*\\)\\[\\([^: ]+\\)\\]$"
					remote)))
	    (let ((user (subfield remote 1))
		  (cm (subfield remote 2)))
	      (irc-remember user 'irc-nicknames)
	      (if (stringp class)
		  (irc-insert
		   "%s%s of class %3s at server %s: \"%s\" (client on %s)%s"
		   irc-msg-info-pre
		   type
		   class
		   (upcase local)
		   user
		   (upcase cm)
		   irc-msg-info-post)
		  (irc-insert "%s%s at server %s: \"%s\" (client on %s)%s"
			      irc-msg-info-pre
			      type
			      (upcase local)
			      user
			      (upcase cm)
			      irc-msg-info-post))))
	   ((and (string-match "\\(UNKNOWN\\)" (upcase type))
		 (numberp (string-match "^\\([^[]*\\)\\[\\([^: ]+\\)\\]$"
					remote)))
	    (let ((cm (substring remote
				 (match-beginning 2)
				 (match-end 2))))
	      (irc-insert (concat "%s%s at server %s from internet"
				  " host %s%s")
			  irc-msg-info-pre
			  (cond ((string= "UNKNOWN" (upcase type))
				 "Half open connection")
				(t (concat "<Unknown type (%s) in function"
					   " irc-parse-notice, please"
					   " tell "
					   irc-hacker
					   ">")))
			  (upcase local)
			  (upcase cm)
			  irc-msg-info-post)))
	   (t (irc-insert "%s%s at server %s to%s server %s%s%s"
			  irc-msg-info-pre
			  (cond ((string= (upcase type) "CONNECTION")
				 "Serverlink")
				((string= (upcase type) "CONNECTING")
				 "Server-search")
				(t type))
			  (upcase local)
			  (if (stringp class)
			      (format " class %3s" class)
			      "")
			  (upcase remote)
			  (downcase extra)
			  irc-msg-info-post)))))
      ((string-match (concat "^\\*\\*\\* *Notice *\\(:\\|--\\)"
			     " *No +response +from +"
			     "\\([^ ]+\\) *, *closing +link *$")
		     cmsg)
       (irc-insert "%%Closed serverlink to %s due to lack of respone."
		   (substring cmsg (match-beginning 2) (match-end 2)))
       (irc-later-execute-lusers))
      ((string-match (concat "^\\*\\*\\* *Class +\\([0-9]+\\) *"
			     "Entries *linked *: *\\([0-9]+\\) *$")
		     cmsg)
       (let ((class (string-to-int (subfield cmsg 1)))
	     (count (string-to-int (subfield cmsg 2))))
	 (irc-insert "%s%s%d service%s linked for class %3d%s"
		     irc-msg-info-pre
		     parorg
		     count
		     (if (= 1 count) "" "s")
		     class
		     irc-msg-info-post)))
      ((string-match (concat "^\\([A-Za-z]+\\) +has +been +used +"
			     "\\([0-9]+\\) +times? +after +"
			     "startup *$")
		     cmsg)
       (let ((cmd (substring cmsg (match-beginning 1) (match-end 1)))
	     (cnt (substring cmsg (match-beginning 2) (match-end 2))))
	 (irc-insert "%sServer %s has seen %s%s command%s of type %s%s"
		     irc-msg-info-pre
		     (upcase (if (string= srvr "") irc-server srvr))
		     (make-string (max 0 (- 5 (length cnt))) ? )
		     cnt
		     (if (string= cnt "1") " " "s")
		     cmd
		     irc-msg-info-post)))
      ((string-match "^\\*\\*\\* *No +such +server *" cmsg)
       (let ((unknown (substring cmsg (match-end 0))))
	 (irc-forget unknown 'irc-servernames)
	 (irc-insert "%%%sNo such server: \"%s\"."
		     (if (string= "" srvr)
			 ""
			 (concat "(" srvr ")"))
		     unknown)))
      ((string-match (concat "^\\*\\*\\* *unknown \\([^: ]+\\) *==> *"
			     "\\([^: ]+\\) *$")
		     cmsg)
       (irc-insert "%sUnknown connection at server %s to host %s%s"
		   irc-msg-info-pre
		   (upcase (substring cmsg (match-beginning 1) (match-end 1)))
		   (upcase (substring cmsg (match-beginning 2) (match-end 2)))
		   irc-msg-info-post))
      ((string-match (concat "^\\*\\*\\* *\\(User\\)?\\(ChanOp\\)?"
			     "\\(Oper\\)? +\\([^: ]+\\) *==> *"
			     "\\([^: ]*\\) *\\[\\(.\\|\n\\)*\\] *$")
		     cmsg)
       (let* ((first (substring cmsg
				(or (match-beginning 1) 0)
				(or (match-end 1) 0)))
	      (second (substring cmsg
				 (or (match-beginning 2) 0)
				 (or (match-end 2) 0)))
	      (third (substring cmsg
				(or (match-beginning 3) 0)
				(or (match-end 3) 0)))
	      (srvr (substring cmsg (match-beginning 4) (match-end 4)))
	      (nick (substring cmsg (match-beginning 5) (match-end 5)))
	      (client (substring cmsg (match-beginning 6) (match-end 6)))
	      (type (cond ((not (string= "" first)) "User")
			  ((not (string= "" second)) "Channel operator")
			  ((not (string= "" third)) "IRC operator")
			  (t "?UNKNOWN?"))))
	 (irc-remember nick 'irc-nicknames)
	 (irc-insert "%s%s at server %s: \"%s\" (client on %s)%s"
		     irc-msg-info-pre
		     type
		     (upcase srvr)
		     nick
		     (upcase client)
		     irc-msg-info-post)))
      ((string-match (concat "^\\(\\*\\*\\*\\)? *\\([0-9]+\\) +users* +"
			     "\\(have\\|has\\) +connection +to +the +"
			     "twilight +zone *$")
		     cmsg)
       (let ((n (string-to-int (subfield cmsg 2))))
	 (irc-insert "*** There %s %d enabled operator%s online."
		     (if (= 1 n) "is" "are")
		     n
		     (if (= 1 n) "" "s"))))
       ((string-match (concat "^\\(\\*\\*\\*\\)? *There \\(is\\|are\\) +"
 			     "+\\([0-9]+\\) +channels*. *$")
 		     cmsg)
        (let ((n (string-to-int (subfield cmsg 3))))
 	 (irc-insert "*** There %s %d channel%s."
 		     (if (= 1 n) "is" "are")
 		     n
 		     (if (= 1 n) "" "s"))))
       ((string-match (concat "^\\(\\*\\*\\*\\)? *There \\(is\\|are\\) +"
 			     "+\\([0-9]+\\) +clients* +connected +"
 			     "to +\\([^ ,]+\\).*$")
 		     cmsg)
        (let ((n (string-to-int (subfield cmsg 3)))
	      (s (subfield cmsg 4)))
 	 (irc-insert "*** There %s %d client%s on %s."
 		     (if (= 1 n) "is" "are")
 		     n
 		     (if (= 1 n) "" "s")
 		     s)))
      ((string-match "^###" cmsg)
       (let ((m (substring cmsg (match-end 0))))
	 (irc-insert "###%s%s"
		     (if (or (string= "" srvr)
			     (string= (upcase srvr) (upcase irc-server)))
			 ""
			 (concat "(" srvr ")"))
		     m)))
      ((string-match (concat "^\\*\\*\\* *welcome +to +the +internet +"
			     "relay +network *, *\\([^: ]+\\) *$")
		     cmsg)		;v2.4 welcome message
       (let ((user (subfield cmsg 1)))
	 (irc-remember user 'irc-nicknames)))
      ((string-match (concat "^\\*\\*\\* *Your +host +is +"
			     "\\([^ ,]+\\) *, *running +version +"
			     "\\([^: ]+\\) *$")
		     cmsg)
       (let* ((s (subfield cmsg 1))
	      (vrsn (subfield cmsg 2))
	      (s (downcase (irc-extract-hostname s))) ;REAL GENERIC servername
	      (cur-bufname (buffer-name (current-buffer)))
	      (new-bufname (irc-host+port-to-buffer-name s irc-port))
	      (pre-kludge nil))
	 (setq-default irc-server s)
	 (setq irc-server s)
	 (if (string< cur-bufname new-bufname)
	     (rename-buffer new-bufname))
	 (if (not (string= (upcase srvr) (upcase s)))
	     (irc-remember s 'irc-servernames))
	 (if (or (string-match (concat "^\\([Ii][Rr][Cc]\\)?\\([0-9]+\\)[^0-9]+\\([0-9]+\\)\\([^0-9]+\\)\\([0-9]+\\)")
			       vrsn)
		 (string-match (concat "^\\([Ii][Rr][Cc]\\)?\\([0-9]+\\)[^0-9]+\\([0-9]+\\)\\(\\)" vrsn))
		 (string-match (concat "^\\([Ii][Rr][Cc]\\)?\\([0-9]+\\)\\(\\)\\(\\)" vrsn)))
	     (let* ((major (subfield vrsn 2))
		    (minor (subfield vrsn 3))
		    (kludge (subfield vrsn 4))
		    (edit (subfield vrsn 5))
		    (major (string-to-int major))
		    (minor (if (string= "" minor) 0 (string-to-int minor)))
		    (edit (if (string= "" edit) 0 (string-to-int edit))))
	       (setq irc-major-version major
		     irc-minor-version minor
		     pre-kludge kludge
		     irc-edit-version edit)
	       )
	     (irc-insert "%%Failed parsing vrsn \"%s\" in irc-parse-notice."
			 vrsn))
	 (if (and (= irc-major-version 2)
		  (= irc-minor-version 6)
		  (not (string= (upcase pre-kludge) "PRE")))
	     (setq irc-edit-version (+ 10000 irc-edit-version)))
	 (irc-insert "")
	 (irc-insert "*** Welcome to the IRC server at %s!" s)
	 (irc-insert "*** The server's version is %s," vrsn)))
      ((string-match "^\\*\\*\\* *This +server +was +created +"
		     cmsg)
       (irc-insert "*** and it was created %s."
		   (substring cmsg (match-end 0))))
      ;; Hello message from a 2.2 server.
      ((string-match (concat "^\\*\\*\\* *Welcome +to +Internet +Relay +"
			     "Server *")
		     cmsg)
       (let ((vrsn (irc-nuke-whitespace (substring cmsg (match-end 0)))))
	 (if (string-match (concat "^[^0-9]*\\([0-9]+\\)\\.\\([0-9]+\\)"
				   "[^0-9]+\\([0-9]+\\)?.*$")
			   vrsn)
	     (let ((maj (substring vrsn (match-beginning 1) (match-end 1)))
		   (min (substring vrsn (match-beginning 2) (match-end 2)))
		   (edt (substring vrsn (match-beginning 3) (match-end 3))))
	       (setq irc-major-version (string-to-int maj)
		     irc-minor-version (string-to-int min)
		     irc-edit-version (string-to-int edt))))
	 (irc-send "MOTD")
	 (irc-insert (concat "*** Welcome to the Internet Relay Chat server "
			     "version %s at %s!")
		     vrsn
		     irc-server)
	 (set-buffer-modified-p (buffer-modified-p))))
       ((string-match (concat "^\\(\\*\\*\\*\\)? *There +are +\\([0-9]+\\) +"
			     "users +\\(and +\\|(\\)?"
			     "\\([^0-9]*[0-9]+ invisible\\)?\\()\\)? *"
			     "on +\\([0-9]+\\) +servers *$")
		     cmsg)
       (let* ((ucount (subfield cmsg 2))
	      (sdelim (subfield cmsg 3))
	      (invstr (subfield cmsg 4))
	      (edelim (subfield cmsg 5))
	      (scount (subfield cmsg 6))
	      (inv (if (string= "" invstr)
		       ""
		       (concat " "  sdelim invstr edelim)))
	      (n (string-to-int ucount)))
	 (irc-insert "*** There %s %s user%s%s on %s server%s."
		     (if (= n 1) "is" "are")
		     ucount
		     (if (= n 1) "" "s")
		     inv
		     scount
		     (if (= (string-to-int scount) 1) "" "s"))))
      ((string-match (concat "^\\(\\*\\*\\*\\)? *There +are \\([0-9]+\\) +"
			     "yet +unknown +connections *$")
		     cmsg)
       (let ((count (substring cmsg (match-beginning 2) (match-end 2))))
	 (irc-insert "*** There %s yet %s unknown connection%s."
		     (if (string= count "1") "is" "are")
		     count
		     (if (string= count "1") "" "s"))))
      ((string-match (concat "^\\(\\*\\*\\*\\)? *I +have +\\([0-9]+\\)"
			     " +clients +and \\([0-9]+\\) +servers *$")
		     cmsg)
       (let* ((c (subfield cmsg 2))
	      (s (subfield cmsg 3))
	      (cn (string-to-int c))
	      (sn (string-to-int s))
	      (isare (if (= 1 cn) "is" "are"))
	      (cstr (if (= 1 cn) "client" "clients"))
	      (sstr (if (= 1 sn) "server" "servers")))
	 (irc-insert "*** There %s %d %s and %d %s on this server."
		     isare cn cstr sn sstr)
	 (run-hooks 'irc-startup-hook)))
      ((string-match (concat "^\\*\\*\\* +Notice *:?-?-? +Received +KILL"
			     " +message +for +\\([^ ]+\\).\\( +From [^ ]+\\)?"
			     " +Path: *")
		     cmsg)
       (let* ((sender (substring cmsg (match-beginning 1) (match-end 1)))
	      (f (subfield cmsg 2))
	      (path (substring cmsg (match-end 0)))
	      (from (cond ((string-match "^From " f)
			   (substring cmsg (match-end 0)))
			  ((irc-is-hostname f) f)
			  (t ""))))
	 (cond ((string-match "\\([^!]+\\)!?(\\([^ ]*\\) *<- *\\([^ ]*\\)) *$"
			      path)
		(let* ((at (subfield path 1))
		       (old (subfield path 2))
		       (new (subfield path 3))
		       (local (irc-is-nickname old))
		       (irc-msg-cont-used (make-string
					   (length (concat irc-msg-info-pre
							   "User "))
					   ? )))
		  (irc-insert
		   (concat"%sUser %s killed due to NAMECLASH at %s, "
			  (if local
			      "local user %s seen on link from %s%s"
			      "was on %s side, now seen from %s%s"))
		   irc-msg-info-pre
		   sender
		   (upcase (irc-nuke-whitespace at))
		   (upcase (irc-nuke-whitespace old))
		   (upcase (irc-nuke-whitespace new))
		   irc-msg-info-post)))
	       ((or (string-match "\\([^!]+\\)!\\([^=][^! ]*\\) +(\\(.*\\))$"
				  path)
		    (string-match "\\([^!]+\\)!\\([^=][^! ]*\\) *\\(\\)$"
				  path))
		(let* ((s (subfield path 1))
		       (o (subfield path 2))
		       (m (subfield path 3))
		       (site (irc-nuke-whitespace s))
		       (oper (irc-nuke-whitespace o))
		       (type (if (string-match "\\." oper)
				 "Server"
				 "Operator"))
		       (m2 (irc-nuke-whitespace m))
		       (mesg (if (string= "" m2) "" (concat " (" m2 ")"))))
		  (if (string-match "\\." oper)
		      (irc-remember oper 'irc-servernames)
		      (irc-remember oper 'irc-nicknames))
		  (irc-insert "%sOperator %s (@%s) /KILL'ed user %s%s%s"
			      irc-msg-info-pre
			      oper
			      site
			      sender
			      mesg
			      irc-msg-info-post))
		(irc-send (concat "WHOWAS :" sender)))
	       (t (irc-insert "%sUser %s KILL'ed, path: %s%s"
			      irc-msg-info-pre
			      sender
			      path
			      irc-msg-info-post)))))
      ((or (string-match "^ *You +have +been +marked +as +being +away *$" cmsg)
	   (string-match "^ *You +have +marked +as +being +away *$" cmsg))
       (if (string= srvr "")
	   (irc-insert (concat "%sYou have been marked as being away,"
			       " use /HERE to revert the effect%s")
		       irc-msg-info-pre
		       irc-msg-info-post)))
      ((string-match "^ *You +are +no +longer +marked +as +being +away *$"
		     cmsg)
       (if (string= srvr "")		;Only display if from local server.
	   (irc-insert "%sYou are no longer marked as being away%s"
		       irc-msg-info-pre
		       irc-msg-info-post)))
      ((string-match (concat "^ *\\([^: ]*\\) *"
			     "\\(tty[^: ]+"
			     "\\|pty/tty[^: ]+"
			     "\\|vt[0-9]+"
			     "\\|pt[^: ]+"
			     "\\|display"
			     "\\|console\\) *"
			     "\\([ -~]+\\)?.*$")
		     cmsg)
       (let ((user (substring cmsg (match-beginning 1) (match-end 1)))
	     (tty (substring cmsg (match-beginning 2) (match-end 2)))
	     (remote (substring cmsg (match-beginning 3) (match-end 3))))
	 (irc-insert "%s%s %s%s %s"
		     user
		     (make-string (max 0 (- 39 (length user))) ? )
		     tty
		     (make-string (max 0 (- 14 (length tty))) ? )
		     remote)))
      ((string-match " *Nobody +logged +in +on +\\([^: ]+\\) *$" cmsg)
       (let* ((s (subfield cmsg 1))
	      (server (irc-extract-hostname s)))
	 (if (and (stringp server)
		  (not (string= (upcase srvr) (upcase server))))
	     (irc-remember server 'irc-servernames))
	 (irc-insert (concat "%%No users logged in on the Internet node"
			     " which runs the IRC server %s at the moment.")
		     server)))
      ((string-match "^ *\\(UserId +Terminal +Host\\) *$" cmsg)
       (let ((m (substring cmsg (match-beginning 1) (match-end 1))))
	 (irc-insert (concat "Login name                              "
			     "TTY            Logged in from"))
	 (irc-insert (concat "--------------------------------------- "
			     "-------------- --------------"))))
      ((string-match (concat "^\\*\\*\\* *\\(Notice\\)? *\\(:\\|--\\)?"
			     " *Connecti\\(ng\\|on\\) *to"
			     " +\\(.+\\) +activated.? *$")
		     cmsg)
       (irc-insert "%s%sTrying to establish a serverlink to %s%s"
		   irc-msg-info-pre
		   parorg
		   (upcase (substring cmsg (match-beginning 4) (match-end 4)))
		   irc-msg-info-post))
      ((string-match (concat "^\\*\\*\\* *Notice *\\(:\\|--\\)"
			     " *Failed *in *connecting *to"
			     " *\\([^: ]+\\) *: *Socket *is"
			     " *not *connected *.? *$")
		     cmsg)
       (irc-insert (concat "%%%sFailed to establish a serverlink to %s;"
			   " no active server at other end.")
		   parorg
		   (upcase
		    (substring cmsg (match-beginning 2) (match-end 2)))))
      ((string-match (concat "^\\*\\*\\* *Notice *: *Connect +"
			     "to +host +\\(.\\|\n\\)* +failed *:")
		     cmsg)
       (let* ((host (substring cmsg (match-beginning 1) (match-end 1)))
	      (reason (substring cmsg (match-end 0)))
	      (server (upcase (irc-extract-hostname host))))
	 (irc-forget server 'irc-servernames) ;Just in case.
	 (irc-insert "%%Failed to establish a serverlink to %s; %s."
		     (upcase server)
		     reason)))
      ((string-match (concat "^\\*\\*\\* Notice: Access denied (no such server"
			     " enabled) \\([^ ]+\\) *\\(\\[[^ ]+\\]\\)? *$")
		     cmsg)
       (let ((claimed (substring cmsg (match-beginning 1) (match-end 1)))
	     (truename (substring cmsg
				  (1+ (or (match-beginning 2) -1))
				  (1- (or (match-end 2) 1)))))
	 (irc-insert (concat "%%Rejected attemp by internethost \"%s\"%s to"
			     " establish a serverlink; that host isn't enabled"
			     " in the configuration file.")
		     (if (string= "" truename) claimed truename)
		     (if (or (string= "" truename)
			     (string= (upcase claimed) (upcase truename)))
			 ""
			 (concat " (claiming to be \"" claimed "\")")))))
      ((string-match (concat "^ *\\*\\*\\* +Notice *: +No +"
			     "response +from +\\([^: ]+\\) +, +"
			     "closing +link *$")
		     cmsg)
       (let ((removed-server (substring cmsg
					(match-beginning 1)
					(match-end 1))))
	 (irc-forget removed-server 'irc-servernames)
	 (irc-insert (concat "%%Failed to get any response whatsoever from"
			     " server %s, removing the serverlink to it.")
		     removed-server))
       (irc-later-execute-lusers))
      ((or (string-match (concat "^ *\\*\\*\\* *Notice *\\(:\\|--\\)"
				 " *Max +buffering +limit +exceed"
				 " +for +\\([^ ]+\\)")
			 cmsg)
	   (string-match (concat "^ *\\*\\*\\* *Notice\\ *\\(:\\|--\\)"
				 " *SendQueued +called +for +a"
				 " +DEADSOCKET +\\([^: ]*\\) *"
				 "\\(:-(\\)? *$")
			 cmsg))
       (let ((host (irc-extract-hostname (substring cmsg
						    (match-beginning 2)
						    (match-end 2)))))
	 (irc-forget host 'irc-servernames)
	 (irc-insert (concat "%%Closed serverlink to %s, max buffering limit"
			     " got exceeded.")
		     (upcase host)))
       (irc-later-execute-lusers))
      ((string-match (concat "^ *\\*\\*\\* *Host +\\(.*\\) +is"
			     " +unknown *\\.* *$")
		     cmsg)
       (irc-insert "%%Host \"%s\" is unknown."
		   (upcase
		    (substring cmsg (match-beginning 1) (match-end 1)))))
      ((string-match (concat "^ *\\*\\*\\* *Notice *-- *Connect +to +host"
			     " +\\(.+\\) +failed *: *\\(.+\\) *$")
		     cmsg)
       (let ((h (subfield cmsg 1))
	     (reason (subfield cmsg 2)))
	 (irc-insert "%%Connect to host %s failed: %s" (upcase h) reason)))
      ((string-match (concat "^ *\\*\\*\\* *Notice *-- *ERROR +"
			     "from +\\(.*\\) *: *SUMMON +No +"
			     "such +host *( *\\(.\\|\n\\)* *) *found"
			     " *$")
		     cmsg)
       (let* ((fld1 (subfield cmsg 1))
	      (fld2 (subfield cmsg 2))
	      (at-server (irc-extract-hostname fld1))
	      (unknown-host (irc-extract-hostname fld2)))
	 (if (and (string< "" at-server)
		  (not (string= (upcase srvr) (upcase at-server))))
	     (irc-remember at-server 'irc-servernames))
	 (irc-insert (concat "%%Summon command failed as server %s doesn't"
			     " know of any Internet host called \"%s\".")
		     (upcase fld1)
		     fld2)))
      ((string-match (concat "^ *\\*\\*\\* *Notice *-- *ERROR"
			     " +from\\ +\\(.*\\) *: *Access +"
			     "denied *( *no +such +server +"
			     "enabled *) *")
		     cmsg)
       (let ((complainer (subfield cmsg 1))
	     (disabled (substring cmsg (match-end 0))))
	 (if (and (irc-is-hostname complainer)
		  (not (string= (upcase srvr) (upcase complainer))))
	     (irc-remember complainer 'irc-servernames))
	 (irc-insert (concat "%%Server %s refuses to accept serverlink from"
			     " host %s as that host isn't enabled in the"
			     " configuration. Use \"/STATS C %s\" to check"
			     " which hosts ARE enabled.")
		     complainer
		     disabled)))
      ((string-match (concat "^ *\\([^: ]+\\) +seems +to +have +"
			     "disabled +summoning")
		     cmsg)
       (irc-insert "%%User %s@%s has disabled summoning."
		   (substring cmsg (match-beginning 1) (match-end 1))
		   srvnam))
      ((string-match (concat "^ *Summoning +user *\\([^: ]*\\) +"
			     "to +irc *$")
		     cmsg)
       (irc-insert "%sSummoning user %s@%s to IRC%s"
		   irc-msg-info-pre
		   (substring cmsg (match-beginning 1) (match-end 1))
		   srvnam
		   irc-msg-info-post))
      ((string-match (concat "^ *\\*\\*\\* *Notice *\\(:\\|--\\)"
			     " *Link +\\(.*\\) +cancelled *,"
			     " *server +\\(.*\\) +already"
			     " +exists *$")
		     cmsg)
       (let ((rhost (substring cmsg (match-beginning 2) (match-end 2)))
	     (rsrvr (substring cmsg (match-beginning 3) (match-end 3))))
	 (irc-insert (concat "%%Server %s %s tried to establish a serverlink"
			     " to us (%s). Refused as we already are linked.")
		     rsrvr rhost irc-server)))
      ((string-match (concat "^ *Connect *: *Server +\\([^ ]+\\) +already"
			     " +exists +from +\\([^ ]+\\) *$")
		     cmsg)
       (let ((other-side (subfield cmsg 1))
	     (trying-side (subfield cmsg 2)))
	 (irc-remember other-side 'irc-servernames)
	 (irc-remember trying-side 'irc-servernames)
	 (irc-insert "%%Server %s is already connected to %s"
		     (upcase other-side)
		     (upcase trying-side))))
      ((string-match (concat "^ *\\*\\*\\* *Notice *\\(:\\|--\\)"
			     " *ERROR +from +\\(.*\\) *:"
			     " *Server +\\(.*\\) +already +exists"
			     " *\\.?\\.?\\.? *$")
		     cmsg)
       (let ((remote (substring cmsg (match-beginning 2) (match-end 2)))
	     (local (substring cmsg (match-beginning 3) (match-end 3))))
	 (if (not (string= (upcase srvr) (upcase remote)))
	     (irc-remember remote 'irc-servernames))
	 (irc-insert (concat "%%Server %s refused to accept a serverlink from"
			     " %s, the servers are already connected.")
		     (upcase remote)
		     (if (string= (upcase local) (upcase irc-server))
			 (concat "this server (" local ")")
			 (concat "other server " local))))) 
      ((string-match (concat "^ *\\*\\*\\* *Notice *\\(:\\|--\\)"
			     " *Server +\\([^: ]+\\) +closed +the"
			     " +connection *.? *$")
		     cmsg)
       (let ((remote (substring cmsg (match-beginning 2) (match-end 2))))
	 (irc-forget remote 'irc-servernames)
	 (irc-insert "%%%sServerlink from %s closed by remote side."
		     parorg
		     (upcase remote))))
      ((string-match (concat "^ *\\*\\*\\* *Notice *\\(:\\|--\\)"
			     " *Access +denied *( *no +such"
			     " +server +enabled *) *")
		     cmsg)
       (let* ((remote (substring cmsg (match-end 0)))
	      (s (irc-extract-hostname remote))
	      (server (if (stringp s) s ""))
	      (rest (substring remote (length server)))
	      (h (irc-nuke-whitespace rest))
	      (host (upcase (if (string= "" h) server h))))
	 (irc-insert (concat "%%Server %s (on host %s) tried to set up a"
			     " server-link to us (%s) but we refused as there"
			     " is no N line accepting that host/server"
			     " combination.")
		     server host irc-server)))
      ((string-match (concat "^ *\\*\\*\\* *Notice *\\(:\\|--\\)"
			     " *Lost +server +connection +to"
			     " +\\(.*\\) *:")
		     cmsg)
       (let ((remote (substring cmsg (match-beginning 2) (match-end 2)))
	     (reason (substring cmsg (match-end 0))))
	 (irc-forget remote 'irc-servernames)
	 (irc-insert "%%Lost serverlink to %s (%s)." (upcase remote) reason)
	 (irc-later-execute-lusers)))
      ((string-match (concat "^ *Connect *: *Host +\\(.\\|\n\\)*"
			     " +not +listed +in +ircd?.conf *$")
		     cmsg)
       (let ((host (substring cmsg (match-beginning 1) (match-end 1))))
	 (irc-insert (concat "%%There is no host matching the description"
			     " \"%s\" in the servers (%s) configuration"
			     " file.")
		     host irc-server)))
      ((string-match "^ *\\(.\\|\n\\)* *: *Privileged +command *$"
		     cmsg)
       (irc-insert (concat "%%You must be an ENABLED IRC operator to use"
			   " command \"%s\". Use /OPER to enable yourself.")
		   (upcase (subfield cmsg 1))))
      ((string-match (concat "^ *\\*\\*\\* *Notice *-- *ERROR +"
			     "from +\\(.*\\) *: *SUMMON +No +"
			     "such +host *( *\\(.*\\) *) *found"
			     " *$")
		     cmsg)
       (let ((complainer (subfield cmsg 1))
	     (nonfound (subfield cmsg 2)))
	 (if (not (string= (upcase srvr) (upcase complainer)))
	     (irc-remember complainer 'irc-servernames))
	 (irc-insert (concat "%%Command /SUMMON found as server \"%s\" doesn't"
			     " know about any server \"%s\".")
		     complainer nonfound)))
      ((and (irc-is-hostname srvr)
	    (string-match (concat "^ *User +\\(.\\|\n\\)* +not +logged"
				  " +in *$")
			  cmsg))
       (irc-insert (concat "%%Command /SUMMON failed as no user called \"%s\""
			   " is logged in at %s at the moment.")
		   (subfield cmsg 1) (upcase srvr)))
      ((or (string-match "^\\*\\*\\* *Notice *:?-* *\\([^ ]*\\) *$" cmsg)
	   (string-match "^\\*\\*\\* *\\([^ ]*\\) *$" cmsg)
	   (string-match "^ *Notice *:?-* *\\([^ ]+\\) *$" cmsg)
	   (string-match "^ *\\([CNIYQK] *:.*\\) *$" cmsg) ;/STATS C
	   (string-match "^ *\\*\\*\\* *\\(.*\\) *$" cmsg))
       (let* ((b (or (match-beginning 1) 0))
	      (e (or (match-end 1) b))
	      (m (substring cmsg b e)))
	 (irc-insert "%s%s%s%s%s%s"
		     irc-msg-info-pre
		     (if (or (string= rcvr "")
			     (string= (upcase rcvr) (upcase irc-nick-used)))
			 ""
			 (concat
			  "To "
			  (cond ((irc-is-nickname rcvr) "user ")
				((irc-is-channelname rcvr) "channel ")
				((irc-is-broadcastname rcvr) "server "))
			  rcvr
			  " "))
		     (cond ((string= srvr "") "")
			   ((or (string-match "^C:" m) (string-match "^N:" m))
			    parorg)
			   (t (concat "from "
				      (if (irc-is-hostname srvr)
					  "server (user?)"
					  "user (server?)")
				      " "
				      srvr)))
		     (if (and (string= srvr "")
			      (or (string= rcvr "")
				  (string= (upcase rcvr)
					   (upcase irc-nick-used))))
			 ""
			 ": ")
		     m
		     irc-msg-info-post)))
      ((or (string-match (concat "^[^: ]+ +[0-9]+ *[0-9]+ *[0-9]+ *"
				 "[0-9]+ *[0-9]+ +\\("
				 "Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun"
				 "\\) +")
			 cmsg)
	   (string-match (concat "^ *Link +SendQ +SendM +SendBytes"
				 " +RcveM +RcveBytes +Open +since"
				 " *$")
			 cmsg))
       (irc-insert cmsg))
;;;      ((string-match "^ *: *\\([^: ]+\\) +NOTICE +\\([^: ]+\\) *:" str)
;;;       (let ((from (substring str (match-beginning 1) (match-end 1)))
;;;	     (to (substring str (match-beginning 2) (match-end 2)))
;;;	     (msg (substring str (match-end 0))))
;;;	 (setq retval (irc-parse-priv (concat ":" from " PRIVMSG " to
;;;					      " :" msg)))))
      ((and (string= (upcase rcvr) (upcase irc-last-NOTICE-rcv))
	    (string= (upcase srvr) (upcase irc-last-NOTICE-src)))
       (setq irc-last-NOTICE-rcv rcvr
	     irc-last-NOTICE-src srvr)
       (let ((irc-msg-cont-used "    "))
	 (irc-insert "*** %s" cmsg)))
      (t (setq irc-last-NOTICE-rcv rcvr
	       irc-last-NOTICE-src srvr)
	 (cond ((irc-is-nickname srvr)
		(irc-remember srvr 'irc-nicknames))
	       ((irc-is-hostname srvr)
		(irc-remember srvr 'irc-servernames)))
	 (irc-insert "")
	 (let ((irc-msg-cont-used "       "))
	   (irc-insert "****** %s %s says to %s:"
		       (cond ((string= "" srvr) "Your")
			     ((irc-is-nickname srvr)
			      "User (or possible server)")
			     (t "Server (or possible user)"))
		       (cond ((string= "" srvr) "server")
			     (t (concat "\"" srvr "\"")))
		       (if (string= (upcase rcvr) (upcase irc-nick))
			   "you"
			   rcvr))
	   (let ((irc-msg-cont-used "   "))
	     (irc-insert "*** %s" cmsg)))))
    retval))


(defun irc-parse-pong (str)
  "Examine a PONG message from the IRC server.
Normaly the server should never send such a message, but when it does,
chances are there's a server name  given."
  (if (string-match "^PONG *\\([^: ]*\\)? *" str)
      (let* ((s (subfield str 1))
	     (data (irc-nuke-whitespace (substring str (match-end 0))))
	     (server (irc-extract-hostname s)))
	(if (irc-is-hostname server)
	    (irc-remember server 'irc-servernames))
	(if data
	    (irc-insert "%sServer %s says PONG, with message \"%s\"%s"
			irc-msg-info-pre
			server
			data
			irc-msg-info-post)
	    (irc-insert "%sServer %s says PONG%s"
			irc-msg-info-pre
			server
			irc-msg-info-post)))))


(defun irc-parse-priv (str)
  "Examine a PRIVMSG message from the IRC server.
PRIVMSG is intended to be used for private message sent between users.
This is not always the case at the moment; servers will use it like either
NOTICE or MSG on occasion.

If it really is a private message, this function returns t if a signal should
be issued for the \"private\" event, nil otherwise."
  ;; This is really gross because it kludges in the fact that PRIVMSG can
  ;; be used to send notification of a change of channel topic.

  (if (not (string-match (concat "^ *: *\\([^: ]*\\) *PRIVMSG"
				 " *\\([^ ]*\\) +:")
			 str))
      (progn
	(irc-insert "%%Unknown PRIVMSG seen in irc-parse-priv:")
	(irc-insert "%% \"%s\" (str)." str)
	(irc-insert "%% Please tell %s, it might be a bug." irc-hacker))
      (let* ((from (substring str (match-beginning 1) (match-end 1))) 
	     (to (substring str (match-beginning 2) (match-end 2)))
	     (msg (substring str (match-end 0)))
	     (time (if (and irc-message-stamp
			    (not (eq 'public irc-message-stamp)))
		       (concat " (" (irc-get-time) ") ")
		       ""))
	     (hdr (format (format irc-msg-priv (+ 7 (length irc-channel)))
			  time
			  from
			  "")))
	(cond ((and (irc-is-nickname from)
		    (not (irc-recall to 'irc-subscribed-channels)))
	       (irc-remember from 'irc-nicknames))
	      ((irc-is-hostname from)
	       (irc-remember from 'irc-servernames)))
	(cond
	  ((and irc-ignore-automatic-warnings
		(string-match "^<Automatic +warning> +" msg))
	   nil)
	  ((irc-recall to 'irc-subscribed-channels)
	   (irc-parse-public (concat ":" from " MSG :" msg) to))
	  ((irc-recall from 'irc-ignored-ppl)
	   (if (and (boundp 'irc-abusive-ignore)
		    irc-abusive-ignore)
	       (irc-send (concat "NOTICE "
				 from
				 " :You are being ignored by "
				 irc-nick-used))))
	  ((string= (upcase to) (upcase irc-nick-used))
	   (setq irc-last-private (concat from ":"))
	   (let ((irc-msg-cont-used (make-string
				     (min (length hdr)
					  (/ (window-width
					      (get-buffer-window
					       (current-buffer)))
					     2))
				     ? )))
	     (if (boundp 'irc-private-insert)
		 (funcall irc-private-insert from to msg)
		 (irc-insert (concat hdr (irc-clean-up-message msg))))
	     (irc-signal from 'private)))
	  ((irc-is-broadcastname to)
	   (let ((irc-msg-cont-used (make-string
				     (min (length hdr)
					  (/ (window-width
					      (get-buffer-window
					       (current-buffer)))
					     2))
				     ? )))
	     (irc-insert (concat hdr
				 "[BROADCAST to all users on IRC "
				 (if (= ?$ (aref to 0))
				     "server(s) "
				     "clients(s) on Internet host(s) ")
				 (upcase (substring to 1))
				 "] "
				 (irc-clean-up-message msg)))
	     (irc-signal from 'wall)))
	  (t (setq irc-last-private (concat from ":"))
	     (let ((irc-msg-cont-used (make-string
				       (min (length hdr)
					    (/ (window-width
						(get-buffer-window
						 (current-buffer)))
					       2))
				       ? )))
	       (irc-insert "*** Private message to \"%s\" follows:" to)
	       (if (boundp 'irc-private-insert)
		   (funcall irc-private-insert from to msg)
		   (irc-insert (concat hdr (irc-clean-up-message msg))))
	       (irc-signal from 'private)))))))


(defun irc-parse-public (str &optional priv-chan)
  "Examine a MSG message from the IRC server.
MSG is sent when someone has sent a message to a channel.  In reality,
sometimes PRIVMSG is used but irc-parse-private should hand those off to
here.

This function returns t if a bell should be issued for the \"public\" or
\"backtalk\" events, nil otherwise."
  (let* ((user (substring str 1 (string-match " MSG :" str)))
	 (rcvr (if priv-chan
		   priv-chan
		   (let ((clst (irc-recall-all 'irc-subscribed-channels)))
		     ;; Find out which of the channel's is "MSG:able".
		     (while (and clst (listp clst))
		       (if (irc-is-multijoinable-channel (car clst))
			   (setq clst (cdr clst))
			   (setq clst (car clst))))
		     ;; As THIS function was activated, we are listening to one
		     ;; of the old type channels (42, +glbf) if priv-chan=nil.
		     (if (not clst)
			 "???"
			 clst))))
	 (msg (substring str (match-end 0)))
	 (about-self (numberp (string-match (regexp-quote irc-nick-used) msg)))
	 (ismem (irc-recall user 'irc-ignored-ppl))
	 (hdr (if ismem ""
		  (format irc-msg-public
			  (if (and irc-message-stamp
				   (not (eq 'private irc-message-stamp)))
			      (concat " (" (irc-get-time) ")")
			      "")
			  user
			  rcvr)))
	 (irc-msg-cont-used (make-string
			     (min (length hdr)
				  (/ (window-width
				      (get-buffer-window (current-buffer)))
				     2))
			     ? )))
    ;; even here we can't guarantee that the sender has already been noted
    ;; someplace else like join or nick -- the sender might be someplace
    ;; else and sending to this channel with PRIVMSG.
    (irc-remember user 'irc-nicknames)
    (cond ((not ismem)			;Not ignored?
	   (progn (if (boundp 'irc-public-insert)
		      (funcall irc-public-insert user rcvr msg)
		      (irc-insert (concat hdr (irc-clean-up-message msg))))
		  (or (irc-signal user 'public)
		      (and about-self (irc-signal user 'backtalk))))))))


(defun irc-parse-quit (str)
  "Examine a QUIT message from the IRC server.
QUIT is used to tell of a user's departure from IRC.  It is currently sent
by the servers to those clients which are on the same channel as the
departing user.

This function returns t if a signal should be issued for the \"join\" event,
since it also signals someone leaving the channel.  It returns nil if no
bell should be issued."
  (if (not (or (string-match "^:? *\\([^ :]+\\)? +QUIT +\\([^ :]+\\) *: *" str)
	       (string-match "^:? *\\([^ :]+\\)? +QUIT *\\(\\): *" str)))
      (progn (irc-insert "%%Unknown QUIT message in irc-parse-quit:")
	     (irc-insert "%% \"%s\" (str)." str)
	     (irc-insert "%% Please tell %s, it might be a bug." irc-hacker))
      (let* ((u (subfield str 1))
	     (user2 (subfield str 2))
	     (c (irc-clean-up-message (irc-nuke-whitespace
				       (substring str (match-end 0)))))
	     (user (if (string= "" u) irc-nick u))
	     (myself (string= (upcase user) (upcase irc-nick)))
	     (uc (upcase c))
	     (comment (cond ((or (string= "" c)
				 (string= (upcase user) uc))
			     ": user quit")
			    ((string= "LEAVING" uc)
			     ": user quit, one ircII user less on IRC")
			    ((string= "BAD LINK?" uc)
			     ": link closed from client's side")
			    ((string-match "^\\([^.!]+\\)![A-Za-z][^ !]+$" c)
			     (format ": user quit past %s" (subfield c 1)))
			    ((string= "KILLED" uc)
			     ": killed")
			    ((string= "PING TIMEOUT" uc)
			     ": killed off due to inactivity")
			    ((string= "DEAD SOCKET" uc)
			     ": dead socket")
			    ((string= "WRITE ERROR" uc)
			     ": write error")
			    ((string-match
			      (concat "^\\([^ ]+\\.[^ .][^ ]*\\)"
				      " +\\([^ ]+\\.[^ .][^ ]*\\)$")
			      c)
			     (format ": netsplit past %s (lost %s)"
				     (subfield c 1)
				     (subfield c 2)))
			    ((string-match "^[^ ]+\\.[^ .][^ ]$" c)
			     (irc-remember c 'irc-servernames)
			     (concat ": netsplit just past " c))
			    (t (concat " (" c ")")))))
	(if (and (string< "" user2)
		 (not (string= (upcase user) (upcase user2)))
		 (boundp 'debug-on-error)
		 debug-on-error)
	    (irc-insert (concat "%sirc-parse-quit: secondary user to QUIT"
				" differed, user=\"%s\", user2=\"%s\"%s")
			irc-msg-info-pre user user2 irc-msg-info-post))
	(irc-forget user 'irc-nicknames)
	(if (and (not (irc-recall user 'irc-ignored-ppl))
		 (memq 'quit irc-events))
	    (progn (irc-insert "%s%s left IRC%s%s"
			   irc-msg-info-pre
			   (if myself "You" user)
			   comment
			   irc-msg-info-post)
		   ;; currently just the join event; some modification will
		   ;; need to be made here when/if Jarkko has QUIT sent to
		   ;; everyone,not just the channel 
		   (irc-signal user 'join))))))


(defun irc-parse-RPL (str)
  "Examine a numeric RPL_ message from the IRC server.
Numeric control messages are used by newer servers to aid in generalized
client design; while people are converting to the new servers the older
irc-parse-error, irc-parse-notice, et al, functions are redundant with
irc-parse-ERR and irc-parse-RPL.  Values used by this function are found
in the IRC source file numeric.h.

Note well that some things are still going to come out wrong because the
servers are currently still doing things inconsistently."
  (if (string-match "^:?[^: ]+ +\\([023]\\)[0-9][0-9] +" str)
      (let ((n (string-to-int (subfield str 1))))
	(cond ((= n 3) (irc-parse-RPL-3xx str))
	      ((= n 2) (irc-parse-RPL-2xx str))
	      (t (irc-parse-RPL-0xx str))))
      (progn 
	(irc-insert "%%Function irc-parse-RPL called with non-RPL:")
	(irc-insert "%% \"%s\" (str)" str)
	(irc-insert "%% Please tell %s." irc-hacker))))


(defun irc-parse-RPL-0xx (str)
  "Examine a numeric RPL_ message from the IRC server.
Numeric control messages are used by newer servers to aid in generalized
client design; while people are converting to the new servers the older
irc-parse-error, irc-parse-notice, et al, functions are redundant with
irc-parse-ERR and irc-parse-RPL.  Values used by this function are found
in the IRC source file numeric.h.

Note well that some things are still going to come out wrong because the
servers are currently still doing things inconsistently."
  (if (string-match (concat "^:?\\([^: ]+\\) *\\([023][0-9][0-9]\\) +"
			    "\\([^: ]+\\)? +:?")
		    str)
      ;; we assume that the server and message are consistent for us; just
      ;; worry about the numeric value and the rest of the line
      (let* ((origin (subfield str 1))
	     (num (string-to-int (subfield str 2)))
	     (user (subfield str 3))
	     (txt (substring str (match-end 0)))
	     (parorg (if (string= (upcase origin) (upcase irc-server))
			 ""
			 (concat "(" origin ") ")))
	     tmp1 tmp2 tmp3 tmp4)
	(irc-remember origin 'irc-servernames)
	(cond
	  ((= num 001)
	   )
	  ((= num 002)
	   (irc-parse-notice (format "NOTICE %s :*** %s" user txt)))
	  ((= num 003)
	   (irc-parse-notice (format "NOTICE %s :*** %s" user txt)))
	  ((= num 004)
	   (irc-insert "*** FEATURES: %s" txt))
	  (t                                 ; default
	   (irc-insert (concat "%%Unrecognized numeric RPL 0xx message; "
			       "please tell %s:")
		       irc-hacker)
	   (irc-insert "%% str=\"%s\"." str)
	   (irc-insert "%% Function irc-parse-RPL-0xx."))))
      ;; else
      (irc-insert (concat "%%Unrecognized nonnumeric RPL 0xx message follows; "
			  "please tell %s:")
		  irc-hacker)
      (irc-insert "%% \"%s\"." str)
      (irc-insert "%% Function irc-parse-RPL-0xx."))
  nil)


(defun irc-parse-RPL-2xx (str)
  "Examine a numeric RPL_ message from the IRC server.
Numeric control messages are used by newer servers to aid in generalized
client design; while people are converting to the new servers the older
irc-parse-error, irc-parse-notice, et al, functions are redundant with
irc-parse-ERR and irc-parse-RPL.  Values used by this function are found
in the IRC source file numeric.h.

Note well that some things are still going to come out wrong because the
servers are currently still doing things inconsistently."
  (if (string-match (concat "^:?\\([^: ]+\\) *\\([023][0-9][0-9]\\) +"
			    "\\([^: ]+\\)? +:?")
		    str)
      ;; we assume that the server and message are consistent for us; just
      ;; worry about the numeric value and the rest of the line
      (let* ((origin (subfield str 1))
	     (num (string-to-int (subfield str 2)))
	     (user (subfield str 3))
	     (txt (substring str (match-end 0)))
	     (parorg (if (string= (upcase origin) (upcase irc-server))
			 ""
			 (concat "(" origin ") ")))
	     tmp1 tmp2 tmp3 tmp4)
	(irc-remember origin 'irc-servernames)
	(cond
	  ((= num 200)
	   (cond ((string-match
		   "^ *Link +\\([^ ]+\\) *:?\\([^ ]+\\) *:?\\([^ ]+\\)? *$"
		   txt)
		  (let ((vrsn (subfield txt 1))
			(goal (subfield txt 2))
			(next (subfield txt 3)))
		    (irc-insert "%sLink to %s passes %s<%s>%s%s"
				irc-msg-info-pre
				(upcase goal)
				(upcase origin)
				(upcase vrsn)
				(if (string= "" next)
				    ""
				    (concat "; next:" next))
				irc-msg-info-post)))
		 (t (irc-insert "%%Unknown RPL200 received in irc-parse-RPL-2xx:")
		    (irc-insert "%% \"%s\" (txt)." txt)
		    (irc-insert "%% Please tell %s, this might be a bug."
				irc-hacker))))
	  ((or (= num 201)		; RPL_TRACECONNECTING "Try."
	       (= num 202))		; RPL_TRACEHANDSHAKE
	   (cond ((string-match "^\\([^ ]+\\) +\\([0-9]+\\) +:?\\([^ ]+\\)"
				txt)
		  (let* ((type (subfield txt 1))
			 (class (subfield txt 2))
			 (host (subfield txt 3))
			 (utype (upcase type))
			 (state (cond ((string= "TRY." utype)
				       "trying to connect to it")
				      ((string= "H.S." utype)
				       "registering us as server at it")
				      (t type))))
		    (irc-insert "%s%sClass %3s half: %s (%s)%s"
				irc-msg-info-pre
				parorg class (upcase host) state
				irc-msg-info-post)))
		 (t (irc-insert "%%Received unknown RPL%d in irc-parse-RPL-2xx:"
				num)
		    (irc-insert "%% \"%s\" (txt)." txt)
		    (irc-insert "%% Please tell %s, it might be a bug."
				irc-hacker))))
	  ((or (= num 203)		; RPL_TRACEUNKNOWN
	       (= num 204)		; RPL_TRACEOPERATOR "Oper"
	       (= num 205))		; RPL_TRACEUSER "User"
	   (cond ((string-match (concat "^\\([^ ]+\\) +\\(-?[0-9]+\\) *:? *"
					"\\(.+\\)\\[\\(.+\\)\\] *$")
				txt)
		  (let* ((type (subfield txt 1))
			 (class (subfield txt 2))
			 (u (subfield txt 3))
			 (cm (subfield txt 4))
			 (user (irc-nuke-whitespace u)))
		    (if (string< "" user)
			(irc-remember user 'irc-nicknames))
		    (irc-insert "%s%sClass %3s %s: \"%s\" (client on %s)%s"
				irc-msg-info-pre
				parorg
				class
				(downcase type)
				user
				(upcase cm)
				irc-msg-info-post)))
		 (t (irc-insert
		     "%%Unknown RPL203:205 seen, in irc-parse-RPL-2xx:")
		    (irc-insert "%% \"%s\" (str); \"%s\" (txt)." str txt)
		    (irc-insert "%% Please tell %s, it might be a bug."
				irc-hacker))))
	  ((= num 206)			; RPL_TRACESERVER "Serv"
	   (if (or (string-match (concat "^\\([^ ]+\\) +\\([0-9]+\\) +\\(\\)"
					 "\\([0-9]+\\)S +\\([0-9]+\\)C +:? *"
					 "\\([^ ]+\\) *$")
				 txt)
		   (string-match (concat "^\\([^ ]+\\) +\\([0-9]+\\) +"
					 "\\([^ ]+\\) +\\([0-9]+\\)S *:?"
					 " *\\([0-9]+\\)C *$")
				 txt)
		   (string-match (concat "^\\([^ ]+\\) +\\([0-9]+\\) *"
					 ":? *\\(.+\\) *\\(\\)\\(\\)$")
				 txt))
	       (let* ((type (subfield txt 1))
		      (class (subfield txt 2))
		      (r (subfield txt 3))
		      (ns (string-to-int (subfield txt 4)))
		      (nc (string-to-int (subfield txt 5)))
		      (remote (if (string= "" r) (subfield txt 6) r)))
		 (irc-remember remote 'irc-servernames)
		 (irc-insert "%s%sClass %3s %s: %s%s%s"
			     irc-msg-info-pre
			     parorg
			     class
			     (downcase type)
			     (upcase remote)
			     (if (and (= ns 0) (= nc 0))
				 ""
				 (format " (%d server%s, %d client%s)%s"
					 ns
					 (if (= 1 ns) "" "s")
					 nc
					 (if (= 1 nc) "" "s")))
			     irc-msg-info-post))
	       (progn
		 (irc-insert "%%Unknown RPL206 received in irc-parse-RPL-2xx:")
		 (irc-insert "%% \"%s\" (txt)." txt)
		 (irc-insert "%% Please tell %s, it might be a bug."
			     irc-hacker))))
	  ;; Missing 207 RPL_TRACESERVICE
	  ;; Missing 208 RPL_TRACENEWTYPE
	  ((= num 209)			; RPL_TRACECLASS
	   (cond ((string-match (concat "^Class +\\([0-9]+\\) *"
					":? *\\([0-9]+\\) *$")
				txt)
		  (let ((class (string-to-int (subfield txt 1)))
			(count (string-to-int (subfield txt 2))))
		    (irc-insert
		     "%s%s%d service%s linked for class %3d%s"
		     irc-msg-info-pre
		     parorg
		     count
		     (if (= 1 count) "" "s")
		     class
		     irc-msg-info-post)))
		 (t (irc-insert "%%Unknown RPL209 message in irc-parse-RPL-2xx:")
		    (irc-insert "%% \"%s\" (txt)" txt)
		    (irc-insert "%% Please tell %s, it might be a bug."
				irc-hacker))))
	  ((= num 211)			;L-lines
	   (cond ((string-match (concat "^\\([^: ]+\\) +\\([0-9]+\\) +"
					"\\([0-9]+\\) +\\([0-9]+\\) +"
					"\\([0-9]+\\) +\\([0-9]+\\) +"
					": *\\(.*\\) *$")
				txt)
		  (let* ((link (subfield txt 1))
			 (sendq (subfield txt 2))
			 (sendm (subfield txt 3))
			 (sendbytes (subfield txt 4))
			 (rcvem (subfield txt 5))
			 (rcvebytes (subfield txt 6))
			 (rest (subfield txt 7))
			 (irc-msg-cont-used
			  "                                                 "))
		    (irc-insert "%8s %8s %10s %8s %10s %s"
				sendq sendm sendbytes rcvem rcvebytes rest)
		    (irc-insert "%s(to %s)" irc-msg-cont-used (upcase link))))
		 ((string-match (concat "^ *\\(Link +\\)?SendQ +SendM"
					" +SendBytes +RcveM +RcveBytes"
					" +:Open since *$")
				txt)
		  (irc-insert (concat
			       " SendQue   S-Msgs    S-Bytes   R-Msgs"
			       "    R-Bytes Date (to link)"))
		  (irc-insert (concat
			       "-------- -------- ---------- --------"
			       " ---------- --------------")))
		 (t (irc-insert "%%Unknown RPL211, in irc-parse-RPL-2xx:")
		    (irc-insert "%% \"%s\" (txt)." txt)
		    (irc-insert "%% Please tell %s, it might be a bug."
				irc-hacker))))
	  ((= num 212)			;RPL_STATSCOMMANDS (ie non CIKLQY)
	   (cond ((string-match "^\\([^ ]+\\) +:?\\([0-9]+\\) *\\([0-9]+\\)?$"
				txt)
		  (let* ((type (subfield txt 1))
			 (count (subfield txt 2))
			 (rcount (subfield txt 3))
			 (n (string-to-int count))
			 (rn (string-to-int rcount))
			 (c (concat (make-string (max 0 (- 5 (length count)))
						 ? )
				    count))
			 (cmds (if (= n 1) "command " "commands")))
		    (irc-insert "%sServer %s has seen %s %s of type %s%s%s"
				irc-msg-info-pre
				(upcase origin)
				c
				cmds
				(upcase type)
				(if (string= "" rcount)
				    ""
				    (format " %s(%d remote?)"
					    (make-string (- 7 (length type))
							 ? )
					    rn))
				irc-msg-info-post)))
		 (t (irc-insert
		     "%%Unknown RPL212 message received in irc-parse-RPL-2xx:")
		    (irc-insert "%% \"%s\" (txt)." txt)
		    (irc-insert "%% Please tell %s, it might be a bug."
				irc-hacker))))
	  ((or (= num 213)		;C-lines
	       (= num 214)		;N-lines
	       (= num 215)		;I-lines
	       (= num 216)		;K-lines
	       (= num 217)		;Q-lines
	       (= num 241)		;L-lines
	       (= num 242)		;Uptime
	       (= num 243)		;O-lines
	       (= num 244)		;H-lines
	       (= num 249)		;other such lines
	       )
	   (cond ((string= (upcase origin) (upcase irc-server))
		  (irc-insert "*** %s" txt))
		 (t (irc-insert "*** (%s): %s" origin txt))))
	  ((= num 218)			;Y-lines
	   (cond ((string-match
		   (concat "^ *Y +\\([0-9]+\\) +\\([0-9]+\\) +\\([0-9]+\\) +:?"
			   " *\\([0-9]+\\) +:?\\([0-9]+\\)")
		   txt)
		  (let* ((class (subfield txt 1))
			 (ping-freq (subfield txt 2))
			 (conn-freq (subfield txt 3))
			 (max-links (subfield txt 4))
			 (plur (if (= 1 (string-to-int max-links)) "" "s"))
			 (sendq (subfield txt 5))
			 (pre (format (concat "*** %sClass: %s, conn. freq."
					      " %ss, ping freq. %ss, ")
				      parorg class conn-freq ping-freq))
			 (irc-msg-cont-used (make-string (length pre) ? )))
		    (irc-insert "%smax# %s link%s, SendQ %s."
				pre
				max-links
				plur
				sendq)))
		 (t (irc-insert "%%Unknown RPL218 in irc-parse-RPL-2xx:")
		    (irc-insert "%% \"%s\" (txt)." txt)
		    (irc-insert "%% Please tell %s, it might be a bug."
				irc-hacker))))
	  ((= num 219)
	   (irc-insert "%sSTATS listing for server %s done%s"
		       irc-msg-info-pre
		       (upcase origin)
		       irc-msg-info-post)
	   (irc-insert ""))
	  ((= num 221)
	   (let ((i (1- (length txt)))
		 (ut (upcase txt)))
	     (while (and (>= i 0) (not (= ?O (aref ut i))))
	       (setq i (1- i)))
	     (while (and (>= i 0)
			 (not (= ?- (aref ut i)))
			 (not (= ?+ (aref ut i))))
	       (setq i (1- i)))
	     (setq irc-operator (if (and (>= i 0) (= ?+ (aref ut i)))
				     " IOPR"
				     nil)))
	   (irc-insert "%sUser %s'%s mode%s: %s%s"
		       irc-msg-info-pre
		       user
		       (if (= ?S (aref (upcase user) (1- (length user))))
			   ""
			   "s")
		       (if (= 2 (length txt)) " is" "s are")
		       (irc-explain-user-mode txt 'no-direction)
		       irc-msg-info-post))
;;;	  ((= num 232)
;;;	   (irc-insert "DEBUG: RPL232 txt=\"%s\"." txt))
;;;	  ((= num 241) SEE RPL213-217
;;;	   )
;;;	  ((= num 242) SEE RPL213-217
;;;	   )
;;;	  ((= num 243) SEE RPL213-217
;;;	   )
;;;	  ((= num 244) SEE RPL213-217
;;;	   )
;;;	  ((= num 249) SEE RPL213-217
;;;	   )
	  ((= num 251)
	   (irc-parse-notice (format "NOTICE %s :*** %s" user txt)))
	  ((= num 252)
	   (if (string-match "^\\([0-9]+\\) +:operator.*online$" txt)
	       (irc-parse-notice (format (concat "NOTICE %s :*** %s users have"
						 " connection to the twilight"
						 " zone")
					 user (subfield txt 1)))
	       (irc-insert "*** %s" txt)))
	  ((= num 253)
	   (if (string-match "^\\([0-9]+\\) *:unknown connection(s)" txt)
	       (let ((n (string-to-int (subfield txt 1))))
		 (irc-insert "*** There %s %d %s of (yet) unknown %s."
			     (if (= 1 n) "is" "are")
			     n
			     (if (= 1 n) "connection" "connections")
			     (if (= 1 n) "type" "types")))
	       (progn (irc-insert "%%Unrecognized RPL 253 message seen:")
		      (irc-insert "%% \"%s\" (txt)" txt)
		      (irc-insert "%% Please tell %s" irc-hacker))))
	  ((= num 254)
	   (if (string-match "^\\([0-9]+\\) :channel" txt)
	       (irc-parse-notice (format (concat "NOTICE %s :*** There are %s"
						 " channels.")
					 user (subfield txt 1)))
	       (irc-insert "*** %s" txt)))
	  ((= num 255)
	   (irc-parse-notice (format "NOTICE %s :*** %s" user txt)))
	  ((= num 256)			;RPL admin, first line
	   (irc-insert "### %s" txt))
	  ((= num 257)			;RPL admin, second line
	   (irc-insert "### %s" txt))
	  ((= num 258)			;RPL admin, third line
	   (irc-insert "### %s" txt))
	  ((= num 259)			;RPL admin, fourth and last line
	   (irc-insert "### %s" txt)
	   (irc-insert "%sEnd of ADMIN for %s%s"
		       irc-msg-info-pre origin irc-msg-info-post)
	   (irc-insert ""))
	  ((= num 261)			;RPL log file
	   (if (string-match "^File +\\(.+\\) +:\\([0-9]+\\)$"
			     txt)
	       (let ((file (subfield txt 1))
		     (dbglvl (subfield txt 2)))
		 (irc-insert
		  "%s%sThe servers log file for debug level %s is %s%s"
		  irc-msg-info-pre
		  parorg
		  dbglvl
		  file
		  irc-msg-info-post))
	       (progn (irc-insert "%%Unknown RPL 261 seen:")
		      (irc-insert "%% \"%s\" (txt)" txt)
		      (irc-insert "%% Please tell %s" irc-hacker))))
	  (t                                 ; default
	   (irc-insert (concat "%%Unrecognized numeric RPL 2xx message; "
			       "please tell %s:")
		       irc-hacker)
	   (irc-insert "%% str=\"%s\"." str)
	   (irc-insert "%% Function irc-parse-RPL-2xx."))))
      ;; else
      (irc-insert (concat "%%Unrecognized nonnumeric RPL 2xx message follows; "
			  "please tell %s:")
		  irc-hacker)
      (irc-insert "%% \"%s\"." str)
      (irc-insert "%% Function irc-parse-RPL-2xx."))
  nil)


(defun irc-parse-RPL-3xx (str)
  "Examine a numeric RPL_ message from the IRC server.
Numeric control messages are used by newer servers to aid in generalized
client design; while people are converting to the new servers the older
irc-parse-error, irc-parse-notice, et al, functions are redundant with
irc-parse-ERR and irc-parse-RPL.  Values used by this function are found
in the IRC source file numeric.h.

Note well that some things are still going to come out wrong because the
servers are currently still doing things inconsistently."
  (if (string-match (concat "^:?\\([^: ]+\\) *\\([023][0-9][0-9]\\) +"
			    "\\([^: ]+\\)? +:?")
		    str)
      ;; we assume that the server and message are consistent for us; just
      ;; worry about the numeric value and the rest of the line
      (let* ((origin (subfield str 1))
	     (num (string-to-int (subfield str 2)))
	     (user (subfield str 3))
	     (txt (substring str (match-end 0)))
	     (parorg (if (string= (upcase origin) (upcase irc-server))
			 ""
			 (concat "(" origin ") ")))
	     tmp1 tmp2 tmp3 tmp4)
	(irc-remember origin 'irc-servernames)
	(cond
	  ((= num 301)                       ; RPL_AWAY
	   (cond ((string-match "^\\([^: ]+\\) :" txt)
		  (let ((nick (subfield txt 1))
			(msg (substring txt (match-end 0))))
		    (irc-remember nick 'irc-nicknames)
		    (irc-insert "%%User %s is away (%s)."
				nick
				(irc-clean-up-message msg))))
		 (t (irc-insert (concat "%%One of the last persons you sent to"
					" is away, delivered your"
					" message anyway.")))))
	  ((= num 302)			     ; RPL_USERHOST
	   (let ((s txt))
	     (while (string-match
		     (concat "^\\([^ =*]+\\)\\(\\*?\\)=\\([+-]\\)"
			     "\\([^@]+\\)@\\([^ ]+\\) *")
		     s)
	       (let* ((nick (subfield s 1))
		      (oper (subfield s 2))
		      (away (subfield s 3))
		      (user (subfield s 4))
		      (host (subfield s 5))
		      (rest (substring s (match-end 0))))
		 (irc-remember nick 'irc-nicknames)
		 (irc-remember host 'irc-servernames)
		 (irc-insert "%s%s \"%s\" is %s@%s%s%s"
			     irc-msg-info-pre
			     (if (string= "*" oper) "Operator" "User")
			     nick
			     user
			     host
			     (if (string= "-" away) " (AWAY)" "")
			     irc-msg-info-post)
		 (setq s rest)))))
	  ((= num 303)			     ; RPL_ISON
	   (let ((data (if (and (> (length txt) 0) (= ?: (aref txt 0)))
			   (substring txt 1) txt))
		 (list ()))
	     (while (string-match "^ *\\([^ ]+\\)" data)
	       (setq list (cons (subfield data 1) list)
		     data (substring data (match-end 0))))
	     (let ((l list)
		   (arrivers ()))
	       (while (not (null l))
		 (if (not (irc-recall (car l) 'irc-notify-detected))
		     (progn (setq arrivers (cons (car l) arrivers))
			    (irc-remember (car l) 'irc-notify-detected)
			    (irc-remember (car l) 'irc-nicknames)
			    ))
		 (setq l (cdr l)))
	       (let ((l (irc-recall-all 'irc-notify-detected))
		     (gonners ()))
		 (while (not (null l))
		   (if (not (irc-list-recall (car l) list))
		       (progn (setq gonners (cons (car l) gonners))
			      (irc-forget (car l) 'irc-notify-detected)))
		   (setq l (cdr l)))
		 (if (not (null arrivers))
		     (irc-insert "%sDETECTED %s on IRC%s"
				 irc-msg-info-pre
				 (irc-listify arrivers ", " "and")
				 irc-msg-info-post))
		 (if (not (null gonners))
		     (irc-insert "%sLOST SIGHT of %s from IRC%s"
				 irc-msg-info-pre
				 (irc-listify gonners ", " "and")
				 irc-msg-info-post))))))
	  ((= num 304)			     ; RPL_TEXT
	   (irc-insert "Text: %s" txt))
	  ((= num 305)
	   (irc-insert "%sYou are no longer marked as being away%s"
		       irc-msg-info-pre irc-msg-info-post))
	  ((= num 306)
	   (irc-insert (concat "%sYou have been marked as being away, use"
			       " /HERE to revert the effect%s")
		       irc-msg-info-pre irc-msg-info-post))
	  ((= num 311)                       ; RPL_WHOISUSER
	   (string-match (concat "^\\([^: ]+\\) +\\([^: ]+\\) +\\([^: ]+\\)"
				 " \\([^: ]+\\) :")
			 txt)
	   (let* ((nick (subfield txt 1))
		  (rn (substring txt (match-end 0)))
		  (user-name (subfield txt 2))
		  (client (subfield txt 3))
		  (c (subfield txt 4))
		  (channel (if (string= c "*")
			       ""
			       (concat " on channel " c)))
		  (cntrl1 (format "User \"%s\" " nick))
		  (real-name (irc-nuke-whitespace rn)))
	     (setq irc-msg-cont-used (make-string (length cntrl1) ? ))
	     (irc-remember nick 'irc-nicknames)
	     (irc-insert (concat cntrl1 "is %s <%s@%s>%s,")
			 (irc-clean-up-message real-name)
			 (irc-clean-up-message user-name)
			 client
			 channel)))
	  ((= num 312)			; RPL_WHOISSERVER
	   (let ((info-hop-count nil)
		 (info-nick nil)
		 (info-real-server-or-relay-name nil)
		 (info-server-name nil)
		 (info-server-descr nil)
		 (found-info nil))
	     (cond
	       ((string-match "^ *\\([^ :]+\\) +\\([^ :]+\\) *: *" txt)
		(setq info-nick (subfield txt 1)
		      info-server-name (subfield txt 2))
		(let ((rst (substring txt (match-end 0))))
		  (cond ((string-match (concat "^... ... +[0-9]+ [0-9]+:"
					       "[0-9]+:[0-9]+ [0-9]+$")
				       rst)
			 (setq info-server-descr (format "gone since %s" rst)
			       found-info t))
			((string-match (concat "^\\([0-9]+\\) *: *\\["
					       "\\([^]]+\\)\\] *\\(.*\\) *$")
				       rst)
			 (setq info-hop-count (subfield rst 1)
			       info-real-server-or-relay-name (subfield rst 2)
			       info-server-descr (subfield rst 3)
			       found-info t))
			((string-match "^\\([0-9]+\\) +:? *\\(.*\\) *$" rst)
			 (setq info-hop-count (subfield rst 1)
			       info-server-descr (subfield rst 2)
			       found-info t))
			((string-match "^\\[\\([^]]+\\)\\] *\\(.*\\) *$"
				       rst)
			 (setq info-real-server-or-relay-name (subfield rst 1)
			       info-server-descr (subfield rst 2)
			       found-info t))
			(t (setq info-server-descr rst
				 found-info t)))))
	       ((string-match (concat "^\\([^ :]+\\) *: *\\([0-9]+\\) *: *"
				      "\\([^ ].*[^ ]\\) *$")
			      txt)
		(setq info-server-name (subfield txt 1)
		      info-hop-count (subfield txt 2)
		      info-server-descr (subfield txt 3)
		      found-info t))
	       ((string-match "^\\([^ :]+\\) *: *\\([^ ].*[^ ]\\) *$" txt)
		(setq info-server-name (subfield txt 1)
		      info-server-descr (subfield txt 2)
		      found-info t)))
	     (if (not found-info)
		 (progn
		   (irc-insert "%%Found unknown RPL312 in irc-parse-RPL-3xx:")
		   (irc-insert "%% \"%s\" (txt)." txt)
		   (irc-insert "%% Please tell %s, it might be a bug."
			       irc-hacker))
		 (irc-remember info-server-name 'irc-servernames)
		 (let ((s (format "%s%son %s (%s)%s."
				  irc-msg-cont-used
				  (if info-real-server-or-relay-name
				      (concat "(according to "
					      info-real-server-or-relay-name
					      ") ")
				      "")
				  info-server-name
				  (irc-clean-up-message
				   (irc-nuke-whitespace info-server-descr))
				  (if info-hop-count
				      (concat " at least "
					      info-hop-count
					      " hops away")
				      ""))))
		   (irc-insert "%s" s)))))
	  ((= num 313)			; RPL_WHOISOPERATOR
	   (string-match "^[^: ]+" txt)
	   (irc-insert "%s\"%s\" is an ENABLED operator on IRC."
		       irc-msg-cont-used
		       (substring txt (match-beginning 0) (match-end 0))))
	  ((= num 314)			     ; RPL_WHOWASUSER
	   (if (string-match (concat "^\\([^: ]+\\) \\([^: ]+\\) \\([^: ]+\\)"
				     " \\([^: ]+\\) *:")
			     txt)
	       (let* ((nick (subfield txt 1))
		      (rn (substring txt (match-end 0)))
		      (user-name (subfield txt 2))
		      (client (subfield txt 3))
		      (c (subfield txt 4))
		      (channel (if (string= c "*")
				   ""
				   (concat " on channel " c)))
		      (real-name (irc-nuke-whitespace rn))
		      (cntrl1 (format "%%User \"%s\" " nick)))
		 (setq irc-msg-cont-used (make-string (length cntrl1) ? ))
		 (if (not (irc-recall nick 'irc-services))
		     (irc-forget nick 'irc-nicknames))
		 (irc-insert (concat "%" cntrl1 "isn't on IRC anymore,"
				     " was %s <%s@%s>%s,")
			     (irc-clean-up-message real-name)
			     (irc-clean-up-message user-name)
			     client
			     channel))))
	  ((= num 315)			     ; RPL_WHOEND
	   (if (irc-nothing-remembered-p 'irc-whotree)
	       (let ((c (if (string-match (concat "^ *: *[^ ]+ +315"
						  " +[^ ]+ +\\([^:]+\\)"
						  " +:")
					  str)
			    (subfield str 1))))
		 (irc-insert (if c "%%No users on \"%s\"." "%%No users.")
			     c))
	       (irc-recall-all-and-display 'irc-whotree
					   ;(string-match "-* *$"
							 ;irc-who-stroke)
					   31
					   "users"
					   "user")
	       (irc-forget-all 'irc-whotree)))
	  ((= num 316)			     ; RPL_WHOISCHANOP
	   (if (not
		(string-match (concat "^:\\([^: ]+\\)? +316 +\\([^: ]+\\) +"
				      "\\([^: ]+\\) +: *has +been +"
				      "touched +by +magic +forces *$")
			      str))
	       (progn (irc-insert (concat "%%Unrecognized type 316 reply; "
					  "please tell %s:")
				  irc-hacker)
		      (irc-insert "%% \"%s\"." str)
		      (irc-insert " Function irc-parse-RPL-3xx, at 316."))
	       (let ((server (substring str (match-beginning 1) (match-end 1)))
		     (own (substring str (match-beginning 2) (match-end 2)))
		     (other (substring str (match-beginning 3) (match-end 3))))
		 (irc-remember own 'irc-nicknames)
		 (irc-remember other 'irc-nicknames)
		 (irc-insert "%s\"%s\" is a channel operator."
			     irc-msg-cont-used
			     other))))
	  ((= num 317)			;RPL_IDLETIME
	   (cond ((string-match (concat "^ *: *[^ ]+ +317 +[^ ]+ "
					"\\([^ ]+ +[0-9]+\\|[0-9]+\\) *:")
				str)
		  (let* ((data (subfield str 1))
			 (dummy1 (progn data))
			 (two (string-match "^\\([^ ]+\\) +\\([0-9]+\\)$"
					    data))
			 (user (subfield data 1))
			 (time (string-to-int
				(if two (subfield data 2) data))))
		    (if (zerop time)
			(irc-insert "%s\"%s\" is actively typing."
				    irc-msg-cont-used
				    user)
			(irc-insert "%sIdle time for user %s is %d second%s%s."
				    irc-msg-cont-used
				    user
				    time
				    (if (= 1 time) "" "s")
				    (if (> 60 time)
					""
					(concat " ("
						(irc-sec-to-time time)
						")"))))))
		 (t (irc-insert (concat "%%Unknown 317 type reply message in"
					" function irc-parse-RPL-3xx."))
		    (irc-insert "%% \"%s\" (str)." str)
		    (irc-insert "%% Please tell %s, it might be a bug."
				irc-hacker))))
	  ((= num 318)			     ; RPL_ENDOFWHOIS
	   )
	  ((= num 319)
	   (cond ((string-match "^\\([^:]*:\\)?" txt)
		  (let ((list (irc-listify
			       (irc-burst-comma
				(irc-nuke-whitespace
				 (substring txt (match-end 0))))
			       ", " "and")))
		    (irc-insert "%sis listening to channel%s %s,"
				irc-msg-cont-used
				(if (string-match " [^ ]" list) "s" "")
				list)))
		 (t (irc-insert (concat "%%Unknown 319 type reply message in"
					" function irc-parse-RPL-3xx."))
		    (irc-insert "%% \"%s\" (txt)." txt)
		    (irc-insert "%% Please tell %s, it might be a bug."
				irc-hacker))))
	  ((= num 321)                       ; RPL_LISTSTART
	   (irc-forget-all 'irc-listtree)
	   (if (not (irc-terminal-is-slow))
	       (progn (irc-insert irc-list-header)
		      (irc-insert irc-list-stroke)
		      (set-buffer-modified-p (buffer-modified-p)))))
	  ((= num 322)                       ; RPL_LIST
	   (if (not (string-match "^\\([^ ]+\\) \\([^ ]+\\) :" txt))
	       (progn (irc-insert (concat "%%Unknown format on RPL_LIST"
					  " message; please tell %s.")
				  irc-hacker)
		      (irc-insert "%% \"%s\" (txt)." txt)
		      (irc-insert "%% Function irc-parse-RPL-3xx, at 322."))
	       (let* ((chan (substring txt (match-beginning 1) (match-end 1)))
		      (count (substring txt (match-beginning 2) (match-end 2)))
		      (topic (irc-nuke-whitespace
			      (substring txt (match-end 0))))
		      (tmpline (format "%s   %2s   "
				    (irc-format-channel chan)
				    count))
		      (top (irc-clean-up-message topic))
		      (line (concat tmpline top))
		      (irc-msg-cont-used (make-string (length tmpline) ? )))
		 (if (irc-terminal-is-slow)
		     (irc-remember line 'irc-listtree)
		     (let ((irc-msg-cont-used (make-string
					       (string-match "-* *$"
							     irc-list-stroke)
					       ? )))
		       (setq irc-list-stats (list (1+ (car irc-list-stats))
						  (+ (nth 1 irc-list-stats)
						     (string-to-int count))))
		       (irc-insert (irc-clean-up-message line))))))) 
	  ((= num 323)                       ; RPL_LISTEND
	   (if (irc-terminal-is-slow)
	       (let ((lincnt 0)
		     (usrcnt 0)
		     (w (irc-recall-all 'irc-listtree))
		     (irc-msg-cont-used (make-string
					 (string-match "-* *$"
						       irc-list-stroke)
					 ? )))
		 (irc-insert irc-list-header)
		 (irc-insert irc-list-stroke)
		 (set-buffer-modified-p (buffer-modified-p))
		 (while w
		   (irc-insert "%s" (car w))
		   (let* ((data (if (string-match (concat
						   "^ *\\([^ ]+\\)"
						   "\\(\\[[\000-\377]*\\]\\)?"
						   " +\\([0-9]+\\)")
						  (car w))
				    (cons
				     (if (match-beginning 2)
					 (concat (subfield (car w) 1)
						 (subfield (car w) 2))
					 (subfield (car w) 1))
				     (subfield (car w) 3))))
			  (name (car data))
			  (count (string-to-int (cdr data))))
		     (setq usrcnt (+ usrcnt count)
			   lincnt (if (string-match "^=Private=$" name)
				      lincnt
				      (1+ lincnt))
			   w (cdr w))))
		 (irc-insert "%s%d visible user%s on %d visible channel%s%s"
			     irc-msg-info-pre
			     usrcnt
			     (if (= usrcnt 1) "" "s")
			     lincnt
			     (if (= lincnt 1) "" "s")
			     irc-msg-info-post)
		 (irc-insert "")
		 (irc-forget-all 'irc-listtree))
	       ;; Else fast terminal.
	       (let ((ch (car irc-list-stats))
		     (us (nth 1 irc-list-stats)))
		 (if (and (zerop us) (zerop ch))
		     (irc-insert "%%No visible channels.")
		     (irc-insert (concat "%s%s user%s on the %s channel%s"
					 " which happen to be visible%s")
				 irc-msg-info-pre
				 (if (zerop us) "No" (int-to-string us))
				 (if (= 1 us) "" "s")
				 (if (zerop ch) "No" (int-to-string ch))
				 (if (= 1 ch) "" "s")
				 irc-msg-info-post)))
	       (setq irc-list-stats '(0 0))))
	  ((= num 324)			     ; RPL_CHANNELMODEIS
	   (let* ((lst (if (irc-server-has-channelname-in-msgs)
			   (if (string-match
				(concat "^ *: *\\([^ ]+\\) +324" ;origin
					" +\\([^ ]+\\)"	;nick
					" +\\([^ ]+\\)"	;channel
					" +\\(\\+[^ ]*\\)" ;mode
					"\\( [-+]?[^ ]+\\)? *$") ;extra
				str)
			       (list (subfield str 1) ;origin
				     (subfield str 2) ;nick
				     (subfield str 3) ;channel
				     (subfield str 4) ;mode (at least a "+")
				     (subfield str 5)))	;limit (optional)
			   (if (string-match
				(concat "^ *: *\\([^ ]+\\) +324 +"
					"\\([^ ]+\\) +\\(+[^ ]*\\)"
					"\\( +[0-9]+\\)? *$")
				str)
			       (list (subfield str 1) ;origin
				     (subfield str 2) ;nick
				     nil ;channel not given
				     (subfield str 3) ;mode (at least "+")
				     (subfield str 4))))) ;limit (optional)
		  (orig (nth 0 lst))
		  (nick (nth 1 lst))
		  (channel (nth 2 lst))
		  (mode (if lst (irc-nuke-whitespace (concat (nth 3 lst)
							     (nth 4 lst)))))
		  (expl (if lst (irc-explain-channel-mode mode 'skip-dir)))
		  (chntxt (concat (if channel "The channel " "The channels")
				  (if channel
				      (concat
				       channel
				       "'"
				       (if (not (= ?s (aref channel
							    (1- (length
								 channel)))))
					   "s"))
				      "")
				  " "))
		  (irc-msg-cont-used (make-string (+ (length chntxt)
						     (length irc-msg-info-pre))
						  ? )))
	     (if (not lst)
		 (progn
		   (irc-insert (concat "%%Received RPL_CHANNELMODEIS reply"
				       " in unknown format:"))
		   (irc-insert "%% \"%s\" (str)." str)
		   (irc-insert "%% In irc-parse-RPL-3xx at 324. Please tell %s."
			       irc-hacker))
		 (progn
		   (if (irc-is-nickname orig)
		       (irc-remember orig 'irc-nicknames))
		   (if (and (irc-is-hostname orig)
			    (not (string= (upcase origin) (upcase orig))))
		       (irc-remember orig 'irc-servernames))
		   (irc-remember nick 'irc-nicknames)
		   (if (or t (string= (upcase orig) (upcase irc-server)))
		       (irc-insert "%s%smode%s %s %s%s"
				   irc-msg-info-pre
				   chntxt 
				   (if (string-match " and " expl) "s" "")
				   (if (string-match " and " expl) "are" "is")
				   expl
				   irc-msg-info-post)
		       ;; Ignore MODE replies from remote servers.
		       )))))
	  ((= num 331)                       ; RPL_NOTOPIC
	   (if (string= parorg "")	;Not when message from remote (old)
	       (if (string-match (concat "^\\([^ ]+\\) +: *No +topic"
					 " +is +set *.? *$")
				 txt)
		   (irc-insert "%sNo topic is set for channel %s%s"
			       irc-msg-info-pre
			       (subfield txt 1)
			       irc-msg-info-post)
		   (irc-insert "%sNo topic is set%s"
			       irc-msg-info-pre
			       irc-msg-info-post))))
	  ((= num 332)                       ; RPL_TOPIC
	   (cond ((and (irc-server-has-end-of-whois)
		       (string-match "^\\([^ ]+\\) *:" txt))
		  (irc-insert "%sThe topic for channel %s is \"%s\"%s"
			      irc-msg-info-pre
			      (irc-clean-up-message (subfield txt 1))
			      (irc-clean-up-message
			       (irc-nuke-whitespace
				(substring txt (match-end 0))))
			      irc-msg-info-post))
		 ((string-match ":?" txt)
		  (irc-insert "%sThe topic is \"%s\"%s"
			      irc-msg-info-pre
			      (irc-clean-up-message (substring txt
							       (match-end 0)))
			      irc-msg-info-post))
		 (t (irc-insert "%sThe topic is \"%s\"%s"
				irc-msg-info-pre
				(irc-clean-up-message txt)
				irc-msg-info-post))))
	  ((= num 341)                       ; RPL_INVITING
	   (string-match (concat "^:\\([^: ]+\\) +341 +[^: ]+ +\\([^: ]+\\) +"
				 "\\([^ ]+\\)")
			 str)
	   (let ((nick (subfield str 2))
		 (channel (subfield str 3)))
	     (irc-remember nick 'irc-nicknames)
	     (irc-insert "%sYou are inviting user \"%s\" to channel %s%s"
			 irc-msg-info-pre
			 nick
			 (irc-clean-up-message channel)
			 irc-msg-info-post)))
	  ((= num 351)                       ; RPL_VERSION | RPL_WHOREPLY
	   (if (string-match "^\\([^: ]+\\) :?\\([^: ]+\\)" txt)
	       (let ((s (substring txt (match-beginning 2) (match-end 2)))
		     (v (substring txt (match-beginning 1) (match-end 1))))
		 (if (not (string= (upcase origin) (upcase s)))
		     (irc-remember s 'irc-servernames))
		 (irc-insert (concat "%sIRC server %s is running version %s,"
				     " and the client you are using is %s%s")
			     irc-msg-info-pre
			     (upcase s)
			     v
			     irc-version
			     irc-msg-info-post))))
	  ((= num 352)
	   (irc-parse-whoreply (format "WHOREPLY %s" txt)))
 	  ((= num 353)                       ; RPL_NAMREPLY
	   (if (or (string-match "^@ +\\(#[^ ]+\\) +:?" txt)
		   (string-match "^ *[=*] +\\([^ ]+\\) *:" txt))
	       (let ((c (subfield txt 1))
		     (n (substring txt (match-end 0))))
		 (irc-parse-namreply
		  (format "NAMREPLY placeholder %s %s" c n)))
	       (irc-parse-namreply (format "NAMREPLY %s" txt))))
 	  ((= num 354)                       ; RPL_ENDOFNAMES
 	   (irc-insert "Names 354: %s" txt))
	  ((= num 361)                       ; RPL_KILLDONE
	   (string-match "^[^: ]+" txt)
	   (irc-insert "%sYou have removed \"%s\" from IRC (/killed)%s"
		       irc-msg-info-pre
		       (substring txt (match-beginning 0) (match-end 0))
		       irc-msg-info-post))
	  ((= num 364)			     ; RPL_LINKS
	   (if (not (or (string-match "^\\([^ ]*\\) \\([^ ]*\\) *:" txt)
			(string-match "^\\([^ ]*\\) *:" txt)))
	       (progn
		 (irc-insert "%%Error in parsing RPL 364, please tell %s:"
			     irc-hacker)
		 (irc-insert "%% \"%s\" (txt)." txt)
		 (irc-insert "%% In function irc-parse-RPL-3xx, at 364."))
	       (let ((s (upcase (subfield txt 1)))
		     (f2 (subfield txt 2))
		     (info (irc-nuke-whitespace
			    (substring txt (match-end 0)))))
		 (irc-remember (format "Server %s%s: %s"
				       s
				       (if (string= "" f2)
					   ""
					   (format " (to %s)" (upcase f2)))
				       info) 
			       'irc-linksinfo)
		 (if (not (string= (upcase origin) (upcase s)))
		     (irc-remember s 'irc-servernames)))))
	  ((= num 365)			     ; RPL_ENDOFLINKS
	   (irc-insert irc-links-header)
	   (irc-insert irc-links-stroke)
	   (set-buffer-modified-p (buffer-modified-p))
	   (irc-recall-all-and-display 'irc-linksinfo
				       (progn (string-match "^[^: ]* ."
							    irc-links-header)
					      (match-end 0))
				       "servers"
				       "server") 
	   (irc-forget-all 'irc-linksinfo))
	  ((= num 366)			     ; RPL_NAMES_END
	   (if (irc-terminal-is-slow)
	       (let ((lincnt 0)
		     (usrcnt 0)
		     (adjust 0)
		     (w (irc-recall-all 'irc-namtree))
		     (irc-msg-cont-used irc-names-cont-msg))
		 (irc-insert "Name of channel  Users  Nicknames")
		 (irc-insert "---------------  -----  ---------")
		 (set-buffer-modified-p (buffer-modified-p))
		 (while w
		   (irc-insert "%s" (car w))
		   (let* ((pair (if (string-match (concat "^ *\\([^ ]+\\) +"
							  "\\([0-9]+\\)")
						  (car w))
				    (cons (subfield (car w) 1)
					  (subfield (car w) 2))
				    (cons "" "0")))
			  (ldiff (if (string= "PRIVATE" (upcase (car pair)))
				     1
				     0))
			  (udiff (string-to-int (cdr pair))))
		     (setq usrcnt (+ usrcnt udiff)
			   lincnt (1+ lincnt)
			   adjust (+ adjust ldiff)
			   w (cdr w))))
		 (let ((adjusted (- lincnt adjust)))
		   (irc-insert (concat "%s%d visible user%s on %d visible"
				       " channel%s or on some private"
				       " channel%s")
			       irc-msg-info-pre
			       usrcnt
			       (if (= usrcnt 1) "" "s")
			       adjusted
			       (if (= adjusted 1) "" "s")
			       irc-msg-info-post)
		   (irc-insert ""))
		 (irc-forget-all 'irc-namtree))
	       ;; Else fast terminal.
	       (irc-insert "%sEnd of NAMES list%s"
			   irc-msg-info-pre
			   irc-msg-info-post)))
 	  ((= num 367)                       ; RPL_BANLIST
	   (cond
	     ((string-match "^\\([^ ]+\\) +\\(.+\\)$" txt)
	      (let ((channel (subfield txt 1))
		    (user (subfield txt 2)))
		(irc-insert (concat "%sUser(s) matching \"%s\""
				    " are banned from %s%s")
			    irc-msg-info-pre user channel irc-msg-info-post)))
	     (t (irc-insert "%%Unknown type 367 reply seen in irc-parse-RPL-3xx:")
		(irc-insert "%% \"%s\" (txt)" txt)
		(irc-insert "%% Please tell %s, it might be a bug."
			    irc-hacker))))
 	  ((= num 368)                       ; RPL_BANLISTEND 
	   (irc-insert (concat "%sEnd of list of banned users (who are"
			       " prohibited to join)%s")
		       irc-msg-info-pre irc-msg-info-post))
	  ((= num 369)			     ; RPL_ENDOFWHOWAS
	   )
	  ((= num 371)                       ; RPL_INFO
	   (irc-insert "* %s" txt))
	  ((= num 372)                       ; RPL_MOTD
	   (cond
	     ((string-match (concat "^ *: *\\([^: ]+\\)? +372 *\\([^: ]+\\)?"
				    " *: *Message-of-today +not +found +"
				    "in +server +\\([^: ]+\\) *$")
			    str)
	      (let* ((f (substring str (match-beginning 1) (match-end 1)))
		     (frm (if (string= (upcase f) (upcase irc-server)) "" f))
		     (at (substring str (match-beginning 3)
				    (match-end 3))))
		(if (not (string= (upcase origin) (upcase frm)))
		    (irc-remember frm 'irc-servernames))
		(if (not (string= (upcase origin) (upcase at)))
		    (irc-remember at 'irc-servernames))
		(irc-insert "%%%sNo message of the day at server %s."
			    (if (string= frm "")
				""
				(concat "(" frm ") "))
			    (upcase at))))
	     ((string-match (concat "^ *: *\\([^: ]+\\)? +372 *"
				    "\\([^: ]+\\)? *: *"
				    "\\(start\\|end\\)?"
				    "\\( +at +server +\\)?"
				    "\\([A-Za-z.-.---]*\\|.*\\)"
				    "\\( *:\\)? *$")
			    str)
	      (let* ((from-1 (irc-non-num-to-0 (match-beginning 1)))
		     (to-1 (irc-non-num-to-0 (match-end 1)))
		     (from-2 (irc-non-num-to-0 (match-beginning 2)))
		     (to-2 (irc-non-num-to-0 (match-end 2)))
		     (from-3 (irc-non-num-to-0 (match-beginning 3)))
		     (to-3 (irc-non-num-to-0 (match-end 3)))
		     (from-4 (irc-non-num-to-0 (match-beginning 4)))
		     (to-4 (irc-non-num-to-0 (match-end 4)))
		     (from-5 (irc-non-num-to-0 (match-beginning 5)))
		     (to-5 (irc-non-num-to-0 (match-end 5)))
		     (from-6 (irc-non-num-to-0 (match-beginning 6)))
		     (to-6 (irc-non-num-to-0 (match-end 6)))
		     (server (substring str from-1 to-1))
		     (user (substring str from-2 to-2))
		     (direction (substring str from-3 to-3))
		     (token (substring str from-4 to-4))
		     (message (substring str from-5 to-5))
		     (colon (substring str from-6 to-6)))
		(if (and (not (string= "" server))
			 (not (string= (upcase origin) (upcase server))))
		    (irc-remember server 'irc-servernames))
		(if (not (string= "" user))
		    (irc-remember user 'irc-nicknames))
		(cond ((and (or (string= (downcase direction) "start")
				(string= (downcase direction) "end"))
			    (string-match "^ *at +server *$" token)
			    (not (string= "" message))
			    (string-match "^ *:$" colon)
			    (not (string= (upcase origin)
					  (upcase message))))
		       (irc-remember message 'irc-servernames))
		      ((string-match "[^: ]" message)
		       (irc-insert ">>> %s" (concat direction
						    token
						    message
						    colon))))))
	     (t (irc-insert (concat "%%Unkown format on MOTD reply,"
				    " in function irc-parse-RPL-3xx,"
				    " num 372"))
		(irc-insert "%% \"%s\"." str))))
;;; 	  ((= num 373)                       ; RPL_VERSION
;;; 	   (irc-insert "DEBUG: RPL373 txt=\"%s\"." txt))
	  ((= num 374)			;RPL_INFO_END
	   (irc-insert "%sEnd of information about this IRC server%s"
		       irc-msg-info-pre irc-msg-info-post))
	  ((= num 375)			;RPL, start of MOTD
	   (irc-insert ">>> %s" txt))
	  ((= num 376)			;RPL, end of MOTD
	   (irc-insert "%sEnd of MOTD at %s%s"
		       irc-msg-info-pre origin irc-msg-info-post))
	  ((= num 381)                       ; RPL_YOUREOPER
	   (setq irc-operator " IOPR")
	   (set-buffer-modified-p (buffer-modified-p))
	   (irc-insert "%sOperator status for %s ENABLED%s"
		       irc-msg-info-pre
		       irc-nick-used
		       irc-msg-info-post))
	  ((= num 382)                       ; RPL_REHASHING
	   (irc-insert "%sReread local ircd configuration information%s"
		       irc-msg-info-pre
		       irc-msg-info-post))
;;; 	  ((= num 383)                       ; RPL_YOURESERVICE / Kim
;;; 	   (irc-insert "DEBUG: RPL383, txt=\"%s\"." txt))
;;; 	  ((= num 384)                       ; RPL_MYPORTIS / Kim
;;; 	   (irc-insert "DEBUG: RPL384 txt=\"%s\"." txt))
;;; 	  ((= num 385)			; deop reply?
;;; 	   (irc-insert "DEBUG: RPL385 txt=\"%s\"." txt))
	  ((= num 391)                       ; RPL_TIME
	   (setq irc-last-time (irc-get-time))
	   (let* ((pair
		   (if (string-match (concat
				      "^ *\\([^ :]+\\) *: *" ;srvr
				      "\\([A-Za-z][A-Za-z][A-Za-z]\\)"
				      "[A-Za-z]*day" ;weekday
				      " +\\(...\\)[^: ]* +" ;month
				      "\\([0-9]+\\) +" 	;monthday
				      "\\([0-9]+\\) +-* *") ;year
				     txt)
		       (cons (subfield txt 1) 
			     (format "%s %s %s %s %s"
				     (substring txt (match-end 0)) ;clock
				     (subfield txt 2) ;weekday
				     (subfield txt 4) ;monthday
				     (subfield txt 3) ;month
				     (subfield txt 5))) ;year
		       (progn (string-match "^\\([^: ]+\\) :" txt)
			      (cons (subfield txt 1)
				    (irc-nuke-whitespace
				     (substring txt (match-end 0)))))))
		  (server (car pair))
		  (date (irc-nuke-whitespace (cdr pair))))
	     (irc-insert "%sLocal time at %s is %s%s%s"
;;;			 "%s%s%s local time is %s%s%s"
			 irc-msg-info-pre
			 (upcase server)
;;;			 (if (= ?S (aref (upcase server) (1- (length server))))
;;;			     "'"
;;;			     "'s")
			 date
			 (if (string= "0" irc-channel)
			     ""
			     (concat " /" irc-channel))
			 irc-msg-info-post)))
	  ((or (= num 392)
	       (= num 393))
	   (irc-parse-notice (format ":%s NOTICE %s :%s"
				     origin user txt)))
	  ((= num 394)
	   (irc-insert "%sEnd of USERS at %s%s"
		       irc-msg-info-pre origin irc-msg-info-post))
	  ((= num 395)
	   (irc-insert "%%No one logged in on Internet host %s" origin))
	  (t                                 ; default
	   (irc-insert (concat "%%Unrecognized numeric RPL 3xx message; "
			       "please tell %s:")
		       irc-hacker)
	   (irc-insert "%% str=\"%s\"." str)
	   (irc-insert "%% Function irc-parse-RPL-3xx."))))
      ;; else
      (irc-insert (concat "%%Unrecognized nonnumeric RPL 3xx message follows; "
			  "please tell %s:")
		  irc-hacker)
      (irc-insert "%% \"%s\"." str)
      (irc-insert "%% Function irc-parse-RPL-3xx."))
  nil)


(defun irc-parse-topic (str)
  "Examine a TOPIC message from the IRC server.
TOPIC is sent to all of the users on a channel when someone changes the
topic of the channel.  Secret channels can not have the topic set.  TOPIC
messages are displayed as long as 'topic' is in irc-events, even if the user
changing the topic is being ignored.

This function returns t if a signal should be issued for the 'topic' event,
nil otherwise."
  (if (not (string-match "^:\\([^: ]+\\) +TOPIC +\\([^ ]*\\) *:" str))
      (progn (irc-insert "%%Unknown TOPIC command in irc-parse-topic:")
	     (irc-insert "%% \"%s\"." str)
	     (irc-insert "%% Please tell %s, it might be a bug." irc-hacker))
      (let* ((user (subfield str 1))
	     (chnl (subfield str 2))
	     (topic (substring str (match-end 0)))
	     (old-chnl (let ((l (irc-recall-all 'irc-subscribed-channels)))
			 (while (not (atom l))
			   (setq l (if (irc-is-multijoinable-channel (car l))
					 (cdr l)
					 (car l))))
			 (if (listp l) "" l)))
	     (channel (if (string< "" chnl) chnl old-chnl)))
	(irc-remember user 'irc-nicknames)
	(irc-remember channel 'irc-subscribed-channels)
	(irc-insert (concat "%s%s has changed the topic of channel %s to \""
			    (irc-clean-up-message topic)
			    "\"%s")
		    irc-msg-info-pre
		    user
		    channel
		    irc-msg-info-post)
	(if (memq 'topic irc-events)
	    (irc-signal user 'topic)))))


(defun irc-parse-wall (str)
  "Examine a WALL message from the IRC server.
WALL is sent by IRC operators to everyone on IRC.  A WALL message will
always be displayed even if the sender is being ignored.

This function returns t if a signal should be issued for the \"wall\" event,
nil otherwise."
  (if (not (string-match "^:? *\\([^: ]*\\) +WALL +:" str))
      (progn (irc-insert (concat "%%Internal error, function irc-parse-wall"
				 " called with a non-WALL message:"))
	     (irc-insert "%% \"%s\"." str)
	     (irc-insert "%%Please tell %s about it." irc-hacker)
	     (irc-signal "<internal-error in irc-parse-wall>" 'wall))
      (let ((user (substring str (match-beginning 1) (match-end 1)))
	    (msg (substring str (match-end 0))))
	(irc-remember user 'irc-nicknames)
	(irc-insert (concat (format irc-msg-wall
				    (concat " (" (irc-get-time) ") ")
				    user)
			    msg))
	(irc-signal user 'wall))))


(defun irc-parse-wallops (str)
  "Examine a WALLOPS message from the IRC server.
WALLOPS are sent by any user to all enabled operators on IRC. A WALLOPS will
allways be displayed, even if the sender is being ignores.

This function returns t if a signal should be issued for the \"wall\" event,
nil otherwise."
  (if (not (string-match "^: *\\([^: ]*\\) +WALLOPS +:" str))
      (progn (irc-insert (concat "%%Internal error, function irc-parse-wallops"
				 " called with a non-WALLOPS message:"))
	     (irc-insert "%% \"%s\"." str)
	     (irc-insert "%%Please tell %s about it." irc-hacker)
	     (irc-signal "<internal-error in irc-parse-wallops>" 'wall))
      (let* ((sender (substring str (match-beginning 1) (match-end 1)))
	     (msg (substring str (match-end 0))))
	(cond ((string-match (concat "^Remote 'CONNECT \\([^ ]+\\) +"
				     "\\([0-9]*\\)' from \\([^ ]+\\) *$")
			     msg)
	       (let ((nsrv (substring msg (match-beginning 1) (match-end 1)))
		     (port (substring msg (match-beginning 2) (match-end 2)))
		     (user (substring msg (match-beginning 3) (match-end 3))))
		 (cond ((irc-is-nickname user)
			(irc-remember user 'irc-nicknames))
		       ((irc-is-hostname user)
			(irc-remember user 'irc-servernames)))
		 (irc-insert (concat "%sTrying to establish a serverlink from"
				     " %s to %s on port %s on remote command"
				     " by user \"%s\"%s")
			     irc-msg-info-pre
			     (upcase sender)
			     (upcase nsrv)
			     port
			     user
			     irc-msg-info-post)))
	      ((string-match (concat "Received SQUIT \\([^ ]+\\) +from"
					 " +\\(.+\\) *$")
				 msg)
	       (let ((oded (substring msg (match-beginning 1) (match-end 1)))
		     (orig (substring msg (match-beginning 2) (match-end 2))))
		 (cond ((irc-is-nickname orig)
			(irc-remember orig 'irc-nicknames))
		       ((irc-is-hostname orig)
			(irc-remember orig 'irc-servernames)))
		 (irc-insert (concat "%sClosing the serverlink from %s to"
				     " server %s, by order of %s%s")
			     irc-msg-info-pre
			     (upcase sender)
			     (upcase oded) ;OD'ed server
			     (upcase orig)
			     irc-msg-info-post)))
	      (t (let* ((head (format "%s%s" (concat
					      (format (format irc-msg-priv 0) 
						      (concat " ("
							      (irc-get-time)
							      ") ")
						      (concat sender
							      " (WALLOPS)")))))
			(irc-msg-cont-used (make-string
					    (min
					     (length head)
					     (/ (window-width
						 (get-buffer-window
						  (current-buffer)))
						2))
					    ? )))
		   (cond ((and (irc-is-hostname sender)
			       (not (irc-is-nickname sender)))
			  (irc-remember sender 'irc-servernames))
			 ((irc-is-nickname sender)
			  (irc-remember sender 'irc-nicknames)))
		   (irc-insert (concat head (irc-clean-up-message msg))))))
	(if (irc-is-nickname sender)
	    (irc-signal sender 'wall)
	    (irc-later-execute-lusers)))))


(defun irc-parse-whoreply (str)
  "Examine a WHOREPLY message from the IRC server.
The message is formatted into a line that is more easily understood than
the raw data.  
The status of the users is shown as a four letter word (:-) according to
the combination of their attributes. The possible attributes being
an IRC operator, being a channel operator and being marked away.

                                          Status field
A normal user having no attributes set:     (blank)
A normal user marked as being away:          Away
An IRC operator with no other attribues:     Iopr
An IRC operator marked as being away:        IoAw
A channel operator with no other attributes: Copr
A channel operator marked as being away:     CoAw
User being both IRC- and channel operator:   ICop
As above but also marked as being away:      ICAw

Being ignored takes precedence over all
other attributes, always shown as:           IGNR


No signals are issued for lines from the WHOREPLY."
  (string-match "^WHOREPLY +" str)
  (setq str (substring str (match-end 0)))
  (let ((is-header (or (string-match (concat "^\\* +User +Host +Server"
					     " +Nickname +S +: *Name *$")
				     str)
		       (string-match (concat "^Channel +User +Host +Server"
					     " +Nickname +S +: *Name *$")
				     str)))
	(split))	      ; make this a list of strings of each data item.
    ;; the elements of 'split' are:
    ;; 0 - full name (with possible hop count)
    ;; 1 - status
    ;; 2 - nickname
    ;; 3 - hostname of server
    ;; 4 - hostname of client
    ;; 5 - login name
    ;; 6 - channel
    (if (or (string-match (concat "^\\([^ ]+\\) +\\([^ ]+\\) +\\([^ ]+\\)"
				  " +\\([^ ]+\\) +\\([^ ]+\\) +\\([^ ]+\\)"
				  " +:\\([0-9]+\\)? *\\(.+\\)$")
			  str))
	(let* ((channel (subfield str 1))
	       (x-login-name (subfield str 2))
	       (x-client-host (subfield str 3))
	       (x-server-host (subfield str 4))
	       (x-nickname (subfield str 5))
	       (x-status (subfield str 6))
	       (hop-count (subfield str 7))
	       (x-full-name (subfield str 8))
	       (name-col (string-match "-* *$" irc-who-stroke)) ;Last stroke
	       (chan-col (string-match "-* +-+ *$" irc-who-stroke)) ;2nd last
	       )
	  (if (not (string= x-status "S")) ;Header?
	      (progn
		;; if it isn't the bogus header
		(irc-remember x-nickname 'irc-nicknames) ; add nick
		(if (and (not is-header)
			 (irc-terminal-is-slow)
			 (irc-server-has-end-of-who)
			 (irc-nothing-remembered-p 'irc-whotree))
		    (let ((irc-msg-cont-used
			   (make-string name-col ? )))
		      (irc-insert irc-who-header)
		      (irc-insert irc-who-stroke)
		      ;;(sit-for 0)
		      (set-buffer-modified-p (buffer-modified-p))))
		(irc-remember x-server-host 'irc-servernames) ; hostnames.
		(irc-remember x-client-host 'irc-servernames)))
	  (let* ((full-name (irc-clean-up-message
			     x-full-name))
		 (nick-pad (make-string (max 0 (- (1- 10) (length x-nickname)))
					? ))
		 (vsts (cond ((irc-recall x-nickname 'irc-ignored-ppl) "IGNR")
			     ((string= "H"   x-status) "    ") ;Norm
			     ((string= "G"   x-status) "Away") ;Away
			     ((string= "H*"  x-status) "Iopr") ;Iopr
			     ((string= "G*"  x-status) "IoAw") ;IoAw
			     ((string= "H@"  x-status) "Copr") ;Copr
			     ((string= "G@"  x-status) "CoAw") ;CoAw
			     ((string= "H*@" x-status) "ICop") ;ICop
			     ((string= "G*@" x-status) "ICAw") ;ICAw
			     ((string= "S"   x-status) "    ") ;Norm
			     (t (concat "\"" x-status "\""))))
		 (vchnl (irc-format-channel
			 (cond ((string= "-1" channel) "-")
			       ((string= "0" channel) "")
			       ((string= "*" channel) "")
			       (t channel))))
		 (vusr (irc-clean-up-message x-login-name))
		 (vclt (irc-clean-up-message x-client-host))
		 (vsrv (irc-clean-up-message x-server-host))
 		 (line (format "%s%s %s %s <%s> %s@%s \"%s\" SERVER=%s"
			       x-nickname nick-pad vsts vchnl hop-count vusr
			       vclt full-name vsrv))
 		 (line (format "%s%s %s %s %s@%s \"%s\""
			       x-nickname nick-pad vsts vchnl vusr
			       vclt full-name))
;		 (line (format "%s%s %s %s@%s \"%s\""
;			       x-nickname nick-pad vsts vusr vclt full-name))
		 (irc-msg-cont-used (make-string 31 ? )))
	    (if (and (irc-terminal-is-slow) (irc-server-has-end-of-who))
		(if (not is-header)
		    (irc-remember line 'irc-whotree))
		(if is-header		;No RPL_ENDOFWHO
		    (let ((irc-msg-cont-used
			   (make-string (string-match "-* *$" irc-who-stroke)
					? )))
		      (irc-insert irc-who-header)
		      (irc-insert irc-who-stroke))
		    (irc-insert "%s" (irc-clean-up-message line))))))
	(irc-insert "%%Unkown WHOREPLY line: \"%s\"." str)))
    nil)


(defun irc-explain-channel-mode (mode &optional skip-direction)
  "Front end to irc-explain-mode."
  (irc-explain-mode mode 'channel skip-direction))


(defun irc-explain-user-mode (mode &optional skip-direction)
  "Front end to irc-explain-mode."
  (irc-explain-mode mode 'user skip-direction))
  

(defun irc-explain-mode (mode type &optional skip-direction)
  "Translate a channels MODE code into plain text. Use optional FLAG when
addition / removal information shouldn't be added."
  (let* ((known-modes '(((?a 0 "anonymous <A>")
			 (?b 1 "ban (from channel) <B>" " of " " of ")
			 (?i 0 "invite only <I>")
			 (?k 1 "channel key <K> ")
			 (?l 1 "limit number of users on channel <L>" " to ")
			 (?m 0 "moderated <M>")
			 (?n 0 "disallow nonlisteners to talk to channel <N>")
			 (?o 1 "channel operator privilege <O>"
			       " for " " for ")
			 (?p 0 "private <P>")
			 (?s 0 "secret <S>")
			 (?t 0 "topic lock <T>")
			 (?v 1 "voice capability <V>" " to " " to "))
			.
			((?i 0 "invisibility <I>")
			 (?o 0 "IRC-operator <O>")
			 (?s 0
			  "subscription to local server status messages <S>")
			 (?w 0 "subscription to WALLOPS <W>"))))
	 (modes (cond ((eq type 'channel) (car known-modes))
		      ((eq type 'user) (cdr known-modes))
		      (t nil)))
	 (cmds (if (string-match " +" mode) 
		   (substring mode 0 (match-beginning 0))
		   mode))
	 (args (substring mode (length cmds)))
	 (dir "added ")
	 (argl ())
	 (expl ""))
    (while (string-match "^ *\\([^: ]+\\)" args)
      (setq argl (append argl (list (subfield args 1)))
	    args (substring args (match-end 1))))
    (while (not (string= cmds ""))
      (let ((c (string-to-char (substring cmds 0 1))))
	(cond ((= ?+ c) (setq dir "added "))
	      ((= ?- c) (setq dir "removed "))
	      (t (let ((p (assoc c modes)))
		   (setq expl (concat expl
				      ", "
				      (if (not skip-direction) dir)
				      (if p
					  (nth 2 p)
					  (concat irc-msg-info-pre
						  "UNKNOWN '"
						  (char-to-string c)
						  "'"
						  irc-msg-info-post))
				      (cond ((and (string= dir "added ")
						  (nth 3 p))
					     (nth 3 p))
					    ((and (string= dir "removed ")
						  (nth 4 p))
					     (nth 4 p))
					    (t ""))
				      (let ((n (or (nth 1 p) 0))
					    (s ""))
					(while (and (> n 0) (car argl))
					  (setq s (concat s (car argl))
						argl (cdr argl)
						n (1- n)))
					s)))))))
      (setq cmds (substring cmds 1)))
    (concat (cond ((string-match "^\\(, *\\)[\000-\377]*\\(,\\) [^,]+" expl)
		   (concat (substring expl (match-end 1) (match-beginning 2))
			   " and"
			   (substring expl (match-end 2))))
		  ((string-match "^\\(, *\\)" expl)
		   (substring expl (match-end 1)))
		  ((string= expl "") (if skip-direction "normal" "none"))
		  (t expl))
	    (if (and (= (length argl) 1)
		     (string-match "^[0-9]+$" (car argl)))
		(concat "; user limit is " (car argl))
		(progn (if (not (equal argl ()))
			   (progn (irc-insert (concat "%%Warning: parts left"
						      " unparsed in function"
						      " irc-explain-mode."))
				  (irc-insert "%% \"%s\" (mode)." mode)
				  (irc-insert (concat "%% Please tell %s, it"
						      " might be a bug.")
					      irc-hacker))
			   ""))))))


(defun irc-ctcp-dequote (str)
  "Return STRING with quoted characters changed into the unquoted equivalents."
  (let ((new ""))
    (while (string-match "\\\\" str)	;Get to next backslash, if any
      (let ((pos (match-beginning 0)))
	(if (>= (- (length str) pos) 2)
	    (setq new (concat new
			      (substring str 0 pos)
			      (irc-ctcp-dequote-char (aref str (1+ pos))))
		  str (substring str (+ 2 pos)))
	    (irc-insert (concat "%%Received badly quoted CTCP string, ignored"
				" superfluous backslash."))
	    (setq new (concat new (substring str 0 pos))
		  str (substring str (1+ pos))))))
    (concat new str)))


(defun irc-ctcp-dequote-char (char)
  "Map a two character STRING to it's corresponding character."
  (char-to-string (cond ((= char ?a) ?\001)
			((= char ?\\) ?\\)
			(t char))))


(defun irc-ctcp-enquote (str)
  "Enquote a STRING, exchanging some characters to two character combinations."
  (let ((d "")
	(dirty "[\001\\]"))
    (while (string-match dirty str)
      (setq d (concat d
		      (substring str 0 (match-beginning 0))
		      (irc-ctcp-enquote-char (aref str (match-beginning 0))))
	    str (substring str (match-end 0))))
    (concat d str)))


(defun irc-ctcp-enquote-char (char)
  "Map a character to it's corresponding one or two character STRING."
  (cond ((= char ?\001) "\\a")
	((= char ?\\) "\\\\")
	(t (char-to-string char))))


(defun irc-lowlevel-dequote (str)
  "Return STRING with quoted characters changed into the unquoted equivalents."
  (let ((new ""))
    (while (string-match "\020" str)
      (let ((pos (match-beginning 0)))
	(if (< pos (1- (length str)))
	    (setq new (concat new
			      (substring str 0 pos)
			      (irc-lowlevel-dequote-char (aref str (1+ pos))))
		  str (substring str (+ 2 pos)))
	    (setq new (concat new
			      (substring str 0 pos)
			      "<<ERROR in senders client: unquoted CNTRL/P>>")
		  str "")
	    )))
    (concat new str)))


(defun irc-lowlevel-dequote-char (char)
  "Map the CHARACTER after a \020 to it's corresponding character."
  (char-to-string (cond ((= char ?0) ?\000)
			((= char ?n) ?\n)
			((= char ?r) ?\r)
			((= char ?\020) ?\020)
			(t char))))


(defun irc-lowlevel-enquote (str)
  "Enquote a STRING, exchanging some characters to two character combinations."
  (let ((d "")
	(dirty "[\000\n\r\020]"))
    (while (string-match dirty str)
      (setq d (concat
	       d
	       (substring str 0 (match-beginning 0))
	       (irc-lowlevel-enquote-char (aref str (match-beginning 0))))
	    str (substring str (match-end 0))))
    (concat d str)))


(defun irc-lowlevel-enquote-char (char)
  "Map a character to it's corresponding one or two character STRING."
  (cond ((= char ?\000) "\0200")
	((= char ?\n) "\020n")
	((= char ?\r) "\020r")
	((= char ?\020) "\020\020")
	(t (char-to-string char))))


(defun irc-show-subscribed-channels ()
  "Show which channel's user is listening to, and which one is talked to."
  (if (not irc-multiple-leave-in-progress)
      (let ((listen (irc-clean-up-message
		     (irc-listify
		      (irc-recall-all 'irc-subscribed-channels)
		      ", "
		      "and"))))
	(cond ((irc-nothing-remembered-p 'irc-subscribed-channels)
	       (cond ((string= "0" irc-channel)
		      (irc-insert (concat "%sYou're neither listening nor"
					  " talking to any channel%s")
				  irc-msg-info-pre
				  irc-msg-info-post))
		     (t (irc-insert (concat "%sYou're now talking and"
					    " listening to channel %s%s")
				    irc-msg-info-pre
				    irc-channel
				    irc-msg-info-post))))
	      ((string= "0" irc-channel)
	       (irc-insert (concat "%sYou're not talking to any channel, but"
				   " listening to %s%s")
			   irc-msg-info-pre
			   listen
			   irc-msg-info-post))
	      (t (let* ((part-1 (format "%sYou are now talking to channel "
					irc-msg-info-pre))
			(irc-msg-cont-used (make-string (length part-1) ? )))
		   (irc-insert "%s%s, while listening to %s%s"
			       part-1
			       irc-channel
			       listen
			       irc-msg-info-post)))))
      (set-buffer-modified-p (buffer-modified-p)))
  nil)


(defun irc-server-has-end-of-who ()
  "True if the current server end WHOREPLY with a \"end of list\" message."
  (or (> irc-major-version 2)
      (and (= irc-major-version 2)
	   (>= irc-minor-version 4))))
 

(defun irc-server-has-end-of-names ()
  "True if the current server end NAMREPLY with a \"end of list\" message."
  (or (> irc-major-version 2)
      (and (= irc-major-version 2)
	   (>= irc-minor-version 4))))


(defun irc-server-has-end-of-links ()
  "True if the current server end LINREPLY with a \"end of list\" message."
  (or (> irc-major-version 2)
      (and (= irc-major-version 2)
	   (>= irc-minor-version 4))))


(defun irc-server-has-non-numeric-channel-names ()
  "True if the current server supports non nummeric IDs for channels."
  (or (> irc-major-version 2)
      (and (= irc-major-version 2)
	   (or (> irc-minor-version 4)
	       (and (= irc-minor-version 4)
		    (>= irc-edit-version 2))))))


(defun irc-server-has-multijoinable-channels ()
  "True if the current server supports #-type channels and the commands JOIN
and PART. Probaly v2.6 or later."
  (or (> irc-major-version 2)
      (and (= irc-major-version 2)
	   (>= irc-minor-version 6))))


(defun irc-server-has-channelname-in-msgs ()
  "True if the server gives the channels name in a RPL_CHANNELMODEIS message."
  (or (> irc-major-version 2)
      (and (>= irc-major-version 2)
	   (or (> irc-minor-version 6)
	       (and (>= irc-minor-version 6)
		    (>= irc-edit-version 19))))))


(defun irc-server-has-end-of-whois ()
  "True if the server ends WHOIS replies with an enad marker."
  (or (> irc-major-version 2)
      (and (>= irc-major-version 2)
	   (or (> irc-minor-version 6)
	       (and (>= irc-minor-version 6)
		    (>= irc-edit-version 20))))))


(defun irc-server-has-settable-topic-on-multijoinable-channel ()
  "True if the server supports setting topic for a multijoinable channel."
  (or (> irc-major-version 2)
      (and (>= irc-major-version 2)
	   (>= irc-minor-version 7))))


(defun irc-active-servers ()
  "Return list of active IRC processes."
  (let ((lst (if (boundp 'irc-processes) irc-processes))
	(act nil))
    (while (consp lst)
      (if (and (memq (process-status (car lst)) '(open run))
	       (stringp (buffer-name (process-buffer (car lst)))))
	  (setq act (cons (car lst) act)))
      (setq lst (cdr lst)))
    (reverse act)))


(defun irc-host+port-to-buffer-name (host-name port-number)
  "Return a legal buffer name for a Kiwi session."
  (cond ((or (not (numberp port-number))
	     (> 0 port-number)
	     (> port-number 65535))
	 (error (format "Kiwi: \"%s\" illegal TCP-port number" port-number)))
	(t (format "*Kiwi-%s/%d*" host-name port-number))))


(defun irc-host+port-to-buffer (host-name port-number)
  "Return the buffer a HOST-NAME in a Kiwi session is associated with.
If no such buffer exist, create it.

See also: irc-host-to-buffer-name <f>"
  (get-buffer-create (irc-host+port-to-buffer-name host-name port-number)))


(defun irc-session-to-buffer (session)
  "Convert a Kiwi SESSION (ie host name) to its associated buffer.
The buffer must exist."
  (get-buffer session))


(defun irc-session-names (proc-list)
  "Convert a LIST of Kiwi processes to a list of the processes host names."
  (mapcar (function (lambda (p)
	     (buffer-name (process-buffer p))))
	  proc-list))


(defun irc-get-host-from-session-name (session-name)
  "Extract the host name associated with a SESSION-NAME."
  (if (string-match irc-legal-session-name session-name)
      (subfield session-name 1)
      (error (format "Illegal session-name: \"%s\"." session-name))))


(defun irc-get-port-from-session-name (session-name)
  "Extract the TCP/IP port number associated with a SESSION-NAME."
  (if (string-match irc-legal-session-name session-name)
      (string-to-int (subfield session-name 2))
      (error (format "Illegal session-name: \"%s\"." session-name))))


(defun irc-terminal-is-slow ()
  "True if the terminal's speed (baud-rate) is lower than the variable
\"search-slow-speed\" (the variable is defined in either file loaddefs.el or
isearch.el in the lisp subdirectory)."
  (if (not (boundp 'search-slow-speed))	;This should never happen.
      (progn (load "loaddefs"))
      (if (not (boundp 'search-slow-speed))
	  (load "isearch"))
      (if (not (boundp 'search-slow-speed))
	  (setq search-slow-speed 1200)))
  (<= (baud-rate) search-slow-speed)
  t)					;Patched.


(defun irc-non-num-to-0 (x)
  (if (numberp x)
      x
      0))


(defun irc-pong ()
  "Send a PONG message with the hostname as an argument.
This is usually used to answer a PING message."
  ;; it's interactive so it can be bound during testing.
  (interactive)
  (irc-send (concat "PONG :" (system-name)))
  (irc-who-is-on)
  nil)


(defun irc-new-scroll-step ()
  "Calculate a new scroll-step value based on the current windows height."
  (if (irc-terminal-is-slow)
      (let* ((h (- (window-height) 3))
	     (x (- h (/ h 6))))
	(if (> window-min-height x)
	    1
	    (if (> (* 2 window-min-height) x)
		(/ x 2)
		x)))
      ;; Else use old value.
      scroll-step))


(defun irc-last-found (str left-most char)
  "Search in STRING, position LEFT-MOST to end of string, for a CHARacter.
Return its position, or nil if not found."
  (let* ((n (length str))
	 (m (while (and (>= n left-most)
			(not (= char (string-to-char (substring str n nil)))))
	      (setq n (1- n)))))
    (if (>= n left-most) (1+ n) nil)))


(defun irc-choose-break-point (str left-most right-most delimiters)
  "Choose a nice point to break a string in. Break at a delimiting
character like space, bang or comma. Return a list of two strings,
the part before and the part after the breakpoint."
  (cond
    ((string-match "\n" str)
     (irc-split-string str (string-match "\n" str)))
    ((< (length str) right-most)
     (irc-split-string str (length str)))
    (t (let ((s (substring str 0 (1- right-most))))
	 (if (or (null delimiters)
		 (<= (min left-most (length str))
		     0))
	     (irc-split-string str (1- right-most))
	     (let ((possible (irc-last-found s left-most (car delimiters))))
	       (if possible
		   (irc-split-string str possible)
		   (irc-choose-break-point str
					   left-most
					   right-most
					   (cdr delimiters)))))))))


(defun irc-split-string (str n)
  "Split string STR into two at position N, trimming all white space
around the split point."
  (let* ((x (min n (length str)))
	 (first-half (substring str 0 x))
	 (second-half (substring str x))
	 (a (progn (string-match "[\n\t ]*$" first-half)
		   (match-beginning 0)))
	 (b (progn (string-match "^[\n\t ]*" second-half)
		   (match-end 0))))
    (cons (substring first-half 0 a)
	  (substring second-half (match-end 0)))))


(defun irc-insert (format &rest args)
  "Insert before irc-mark the string created by FORMAT with substituted ARGS.
Resets the global variable \"scroll-step\" each time. If the string is to long
to be inserted on one line, insert just first part and give the rest to
\"irc-insert-more\"."
  (setq scroll-step (irc-new-scroll-step))
  (let* ((str (apply 'format format args))
	 (strlen (length str))
	 (left-most (* (/ (window-width (get-buffer-window (current-buffer)))
			  3)
		       2))
	 (splitted (irc-choose-break-point
		    str
		    left-most
		    (window-width (get-buffer-window (current-buffer)))
		    '(?, ?  ?\( ?! ?? ?. ?;)))
	 (first-half (car splitted))
	 (second-half (cdr splitted)))
    (save-excursion
      (goto-char (if (and (boundp 'irc-mark) (markerp irc-mark))
		     irc-mark
		     (point-max)))
      (insert-before-markers first-half "\n")
      (if (not (string= "" second-half))
	  (irc-insert-more irc-msg-cont-used left-most second-half)))))


(defun irc-insert-more (continue left-most str)
  "Insert before irc-mark the line created by concatenating the CONTINUE string
and the MESSAGE string. If the line is to long, insert the first part and 
recurse with the rest."
  (let* ((cont (substring continue 0 (min (/ (* 3 (window-width
						   (get-buffer-window
						    (current-buffer))))
					     4)
					  (length continue))))
	 (splitted (irc-choose-break-point str ;;QUICK ugly bug fix.... bleah!
					   (max 0 (- left-most (length cont)))
					   (- (window-width (get-buffer-window
							     (current-buffer)))
					      (length cont))
					   '(?, ?  ?\( ?! ?? ?. ?;)))
	 (first-half (concat cont (car splitted)))
	 (second-half (cdr splitted)))
    (insert-before-markers first-half "\n")
    (if (not (string= "" second-half))
	(irc-insert-more cont left-most second-half))))


(defun irc-fix-wordwrap (line1 line2)
  "With arguments LINE1 and LINE2 apply some simple heuristics to see if the
line which they originally formed was broken in an acceptable place.  Returns
a dotted pair with LINE1 as the car and LINE2 as the cdr."
  (cond ((string-match "^ +" line2)
         ;; broke at whitespace; strip leading space from next line
         (setq line2 (substring line2 1)))
        ((string-match " +$" line1)
         ;; trailing whitespace on line.  might as well just nuke it all.
         (setq line1 (substring line1 0 (match-beginning 0))))
        ((string-match "\\( +\\)[^: ]+$" line1)
         ;; broke in a word, but it's wrappable.  just eat one space.
         (setq line2 (concat (substring line1 (1+ (match-beginning 1))) line2)
               line1 (substring line1 0 (match-beginning 0)))))
  (cons line1 line2))



;; simple key functions -- self-insert, tab, destructive backspace
(defun irc-self-insert (arg)
  "Normaly just inserts the typed character in the input region.
If point is in the output region, irc-spacebar-pages is non-nil and a space
is typed, scroll-up otherwise point moves to end of input region and inserts
the character.

If the character to be inserted is a colon or semi-colon and it is the first
non-white space character on the line then the input region is updated to
begin with the last explicit sendlist, irc-last-explicit.

Inserts the character ARG times if self-inserting.  An argument is not
passed to scroll-up if paging with the spacebar."
  (interactive "p")
  (let* ((in-region (>= (point) irc-mark))
         ;; it's times like this that i wish someone would tell me what
         ;; a good indentation style is for this expression
         (expand-colon
          (and
	   (or (= last-input-char ?\;) (= last-input-char ?:))
           (string-match "^ *$"
			 (buffer-substring
			  irc-mark
			  (if in-region (point) (point-max)))))))
    (if (not expand-colon)
        (if in-region (self-insert-command arg)
	    (if (and irc-spacebar-pages (= last-input-char 32))
		;; it's nice to be able to return to the input region just by
		;; pounding on the spacebar repeatedly.
		(condition-case EOB (scroll-up nil)
		  (end-of-buffer (goto-char (point-max))))
		(goto-char (point-max))
		(self-insert-command arg)))
	(or in-region (goto-char (point-max)))
	;; kill white space.  This also takes out previous lines in input region.
	(delete-region irc-mark (point))
	(insert (if (= last-input-char ?:) irc-last-private irc-last-explicit))
	;; put in the extra characters if need be.
	(if (> arg 1) (self-insert-command (1- arg))))))


(defun irc-del-backward-char (arg)
  "If in the input region, delete ARG characters before point, restricting
deletion to the input region.  If in the output region and irc-spacebar-pages
then scroll-down (aka window-back) otherwise do nothing."
  (interactive "p")
  (if (> (point) irc-mark)
      ;; only delete as far back as irc-mark at most
      (if (> arg (- (point) irc-mark)) (delete-region (point) irc-mark)
	  (delete-backward-char arg))
      (if (and (<= (point) irc-mark) irc-spacebar-pages) (scroll-down nil)
	  (ding))))


(defun irc-tab ()
  "If point is in the input region then tab-to-tab-stop.  If it is in the
output region, go to the previous line if irc-spacebar-pages; do nothing
otherwise."
  (interactive)
  (if (>= (point) irc-mark) (tab-to-tab-stop)
      (if irc-spacebar-pages (previous-line 1)
	  (ding))))



;; top-level -- entry, sentinel and mode
(defun irc (universal-argument)
  "Enter the Internet Relay Chat conferencing system.
If no connexion to an irc-server is open, then one is started.  If no buffer
*IRC* exists then it is created otherwise the existing buffer is used.  If
a connexion is already active then the most recently started IRC session
is switched to in the current window.  This makes binding 'irc' to a key
much more convenient.

With prefix argument NEW-BUFFER, another *IRC* buffer is created and a
new IRC session is started.  This is provided so that multiple IRC
sessions can co-exist in one Emacs, which is sometimes a useful thing."
  (interactive "P")
  (let* ((act (irc-active-servers))
	 (session-names (irc-session-names act))
	 (number-of-sessions (length session-names))
	 (ask-for-session (> number-of-sessions 1))
	 (session (let ((s (if ask-for-session
			       (irc-nuke-whitespace
				(irc-read-object
				 "Select which Kiwi session? (RET for new) "
				 ""
				 session-names))
			       "")))
		    (if (or (irc-member-general s
						session-names
						'string=)
			    (string= "" s))
			s
			(error (format "No such active session: %s" s)))))
	 (ask-for-host (or universal-argument
			   (= 0 number-of-sessions)
			   (and ask-for-session
				(string= "" session))))
	 (host (downcase
		(cond
		  (ask-for-host
		   (let ((h (irc-nuke-whitespace
			     (read-string
			      (concat "Connect new Kiwi session to which"
				      " internet host? (RET for "
				      irc-server
				      ") ")))))
		     (if (string= "" h) irc-server h)))
		  (ask-for-session
		   (irc-get-host-from-session-name session))
		  ((= 1 number-of-sessions)
		   (irc-get-host-from-session-name (car session-names)))
		  (t ""))))
	 (port (cond
		 (ask-for-host
		  (let* ((p (irc-nuke-whitespace
			     (read-string
			      (concat "Use which TCP port for new connection?"
				      " (RET for "
				      (int-to-string irc-port)
				      ", * for 194) "))))
			 (n (string-to-int p)))
		    (cond ((string= "*" p) 194)
			  ((string= "" p) irc-port)
			  ((and (> n 0) (> 65535 n)) n)
			  (t (error "Illegal TCP port")))))
		 (ask-for-session
		  (irc-get-port-from-session-name session))
		 ((= 1 number-of-sessions)
		  (irc-get-port-from-session-name (car session-names)))
		 (t nil)))
	 (buffer (irc-host+port-to-buffer host port)))
    (make-variable-buffer-local 'irc-port)
    (setq irc-port port)
    (switch-to-buffer buffer)
    (if (not (get-buffer-process (current-buffer)))
	(progn
	  (goto-char (point-max))
	  (irc-mode)
	  (irc-insert "")
	  (irc-insert "")
	  (irc-insert "%s for GNU Emacs v18.57 and around...." irc-version)
	  (irc-insert "Comments to %s." irc-hacker)
	  (irc-insert "")
	  (condition-case NOT-IRCED
	      (let ((proc (open-network-stream
			   "irc" buffer host port)))
		(set-process-filter proc 'irc-filter)
		(set-process-sentinel proc 'irc-sentinel)
		(setq irc-server host)
		(setq-default irc-server host)
		(irc-send (concat "NICK " irc-nick))
		(irc-send (format "USER %s %s %s :%s"
				  (user-login-name)
				  (system-name)
				  irc-server
				  (or (getenv "IRCNAME")
				      (getenv "NAME")
				      (user-full-name)
				      "<<USERNAME UNKNOWN>>")))
		;; a new process, so initialize the variables.  they aren't set
		;; in irc-mode so that irc-mode can be called at any time.
		;; (irc-remember irc-nick 'irc-nicknames)
		(let ((s (irc-recall-all 'irc-services)))
		  (while (and (listp s) s)
		    (irc-remember (car s) 'irc-nicknames)
		    (setq s (cdr s))))
		(irc-remember irc-server 'irc-servernames)
		(setq irc-nick-used ".Not.connected.yet."
		      irc-major-version 0
		      irc-minor-version 0
		      irc-edit-version 0
		      irc-motd-lines nil
		      irc-list-stats '(0 0)
		      irc-away nil
		      irc-channel "0"
		      irc-history-index -1
		      irc-operator nil
		      irc-scratch ""
		      irc-last-command  ""
		      irc-last-explicit "*;"
		      irc-last-private "*;"
		      irc-last-time (irc-get-time)
		      ;; this next bit of messiness just ups irc-last-stamp
		      ;; in an effort to make nice numbers out of the time
		      ;; stamps -- ie, if the time is now 13:53 with an
		      ;; interval of 15 minutes, this makes it 13:45
		      irc-last-stamp 0
		      irc-total-time (string-to-int
				      (substring irc-last-time 3))
		      irc-last-stamp (if (zerop irc-time-stamp)
					 0
					 (while (< (+ irc-last-stamp
						      irc-time-stamp)
						   irc-total-time)
					   (setq irc-last-stamp
						 (+ irc-last-stamp
						    irc-time-stamp)))
					 irc-last-stamp)
		      irc-last-notify irc-last-stamp
		      irc-processes (cons proc irc-processes)))
	    (error
	     (irc-insert (concat "%%Couldn't connect to the TCP/IP"
				 " port %s at the internet host %s --"
				 " sorry: %s")
			 port
			 host
			 NOT-IRCED)
	     ))))))


(defun irc-mode ()
  "To understand some documentation given with irc-mode variables and
functions, \"output region\" is defined as everything before the irc-mark.
irc-mark is a marker kept by irc-mode to know where to insert new text
from IRC.  Text in the output region cannot be modified by the most common
methods of typing a self-inserting character or pressing delete.

The input region is everything which follows irc-mark.  It is what
gets processed by irc-mode when you type LFD or RET.  If irc-spacebar-pages
is non-nil, the following keys are in effect when the cursor is in the
output region:

SPC             scroll-forward       DEL     scroll-backward
LFD or RET      next-line            TAB     previous-line

Local keys:
\\{irc-mode-map}"
  (interactive)
  (setq buffer-offer-save t)
  (kill-all-local-variables)
  (setq major-mode 'irc-mode
	mode-name "Kiwi"
	fill-column (- (window-width (get-buffer-window (current-buffer))) 5))
  (set (make-local-variable 'irc-away) nil) ; for the mode-line
  (set (make-local-variable 'irc-channel) nil) ; for sendlists and broken PRIVMSGs
  (set (make-local-variable 'irc-edit-version) nil) ; edit version number of server
  (set (make-local-variable 'irc-history-index) nil) ; for the message history
  (set (make-local-variable 'irc-last-command) nil) ; for the command history
  (set (make-local-variable 'irc-last-explicit) nil) ; for sendlist ; auto-expansion
  (set (make-local-variable 'irc-last-private) nil) ; for sendlist : auto-expansion
  (set (make-local-variable 'irc-last-stamp) nil) ; for time-sentinel
  (set (make-local-variable 'irc-last-time) nil) ;
  (set (make-local-variable 'irc-list-stats) nil) ; for RPL_LIST statistics (temp)
  (set (make-local-variable 'irc-major-version) nil) ; major version number of server
  (set (make-local-variable 'irc-minor-version) nil) ; major version number of server
  (set (make-local-variable 'irc-nick-used) nil) ; the nick used at the server
  (set (make-local-variable 'irc-operator) nil)	; for special privileges
  (set (make-local-variable 'irc-scratch) nil) ; for accumulating server messages
  (set (make-local-variable 'irc-total-time) nil) ;
  (set (make-local-variable 'irc-count-incoming-messages) 0)
  (set (make-local-variable 'irc-idle-last-sent) -1)
;;;  (set (make-local-variable 'irc-idle-scratch-file)
;;;       (expand-file-name
;;;	(concat (make-temp-name "/tmp/.Kiwi.")
;;;		"."
;;;		(user-login-name)
;;;		".may-safely-be-deleted-anytime")))
;;;  (if (file-exists-p irc-idle-scratch-file)
;;;      (delete-file irc-idle-scratch-file))
  (set (make-local-variable 'irc-ignored-ppl) ;Nicks actually ignored
       (irc-create-new-hash-table 7))
  (set (make-local-variable 'irc-last-NOTICE-rcv) "")
  (set (make-local-variable 'irc-last-NOTICE-src) "")
  (set (make-local-variable 'irc-links-header) "Known server links.")
  (set (make-local-variable 'irc-links-stroke) "-------------------")
  (set (make-local-variable 'irc-linksinfo) ; for LINREPLY & RPL364 data (temp)
       (irc-create-new-hash-table 149))	;2*(65-70 seen links, oct -91)
  (set (make-local-variable 'irc-list-header) "Name of channel  Users Topic")
  (set (make-local-variable 'irc-list-stroke) "---------------  ----- -----")
  (set (make-local-variable 'irc-listtree) ; for RPL_LIST 322 data (temp)
       (irc-create-new-hash-table 163))	;2*(50-80 channels, oct -91)
  (set (make-local-variable 'irc-msg-cont-used) irc-msg-cont)
  ;; Indentation for continued long lines when displaying NAMREPLY's.
  (set (make-local-variable 'irc-names-cont-msg) (make-string 24 ? ))
  (set (make-local-variable 'irc-notify-looked-for)
       (irc-create-new-hash-table 149))
  (set (make-local-variable 'irc-notify-detected)
       (irc-create-new-hash-table 149))
  (set (make-local-variable 'irc-namtree) ; for NAMREPLY data (temp)
       (irc-create-new-hash-table 853))	;2*(200-400 users, oct -91)
  (set 'irc-nicknames			; for sendlists
       (irc-create-new-hash-table 853)) 
  (set 'irc-servernames			; for server completion
       (irc-create-new-hash-table 997))	;2*((60-70 seen links)+(200-400usrs))
  (set (make-local-variable 'irc-subscribed-channels) ; channels we listen to
       (irc-create-new-hash-table 7))	;max 11
  (set (make-local-variable 'irc-services) ; for services like NICKSERV
       (irc-create-new-hash-table 17))
  (set (make-local-variable 'irc-who-header)
       (concat "Nickname  Stat Channel name   "
	       " <Hop count> Login@Client \"Real name\" SERVER=srvr"))
  (set (make-local-variable 'irc-who-header)
       (concat "Nickname  Stat Random channel  Whatever@clientmachine \"Real name\""))
  (set (make-local-variable 'irc-who-stroke)
       (concat "--------  ---- --------------  "
	       "------------------------------------------------"))
  (set (make-local-variable 'irc-whotree) ; for WHOREPLY data (temp)
       (irc-create-new-hash-table 853))	;(200-400 users, oct -91)
  (set (make-local-variable 'mode-line-format)
       (list (purecopy "--- %14b ")
	     'global-mode-string
	     (purecopy " %[(")
	     'mode-name 'minor-mode-alist 'irc-operator
	     (purecopy ")%] ")
	     'irc-nick-used ":" 'irc-channel
	     " " 'irc-away (purecopy "-%-")))
  ;; too many ways to get unbalanced parens (most notably ":-)")
  (set (make-local-variable 'blink-matching-paren) nil)
  ;; as closest we can come to "natural" terminal scrolling
  (set (make-local-variable 'scroll-step) 1) ; Reset in irc.
  (set-marker (set (make-local-variable 'irc-mark) (make-marker)) (point-max))
  (let ((nicks irc-ignores))
    (while (not (null nicks))
      (irc-remember (car nicks) 'irc-ignored-ppl)
      (setq nicks (cdr nicks))))
  (let ((services '(;;"AMIGASERV"
		    "ARSKA"		;TZoper of #42.
		    "CDSERV"
		    "CONVSERV"		;Convert from Celsius to Fahrenheit etc
		    "EU-OPER"		;TZoper of #Eu-opers.
		    "IRCIIHELP"		;Help automaton for ircII commands
		    ;;"JIRCC"		;Japanese Kanjii/ASCII translator
		    ;;"MSGSERV"		;Stores and forwards user-user messages
		    "NICKSERV"		;Register for nick names
		    ;;"NOTE-NO"
		    "NOTESERV"		;"Wait for user" and "store messages".
		    "SWEDESERV"
		    "TZOPER"		;Copr ioprs on #Twilight_Zone
		    ;;"WEBSERV"
		    )))
    (while services
      (irc-remember (car services) 'irc-services)
      (irc-remember (car services) 'irc-nicknames)
      (setq services (cdr services))))
  (buffer-enable-undo)
  (irc-wrap-display-time)
  (turn-on-auto-fill)
  ;; "invisible subwindows" or whatever you would like to call them would be
  ;; nice.  That way I could make the output-region read-only.  The two things
  ;; most likely to screw up the buffer are backward-kill-word and kill-region
  (use-local-map irc-mode-map)
  (run-hooks 'irc-mode-hook))


(defun irc-sentinel (proc stat)
  "The sentinel for the IRC connexion.
Takes the normal sentinel arguments PROCESS and STATUS."
  ;; ignore anything but finished; i don't know what to do with the others
  (cond ((or (string= stat "finished\n")
	     (string-match "^exit" stat))
         (save-excursion
           (set-buffer (cond ((processp proc) (process-buffer proc))
			     ((bufferp proc) proc)
			     (t (get-buffer
				 (irc-host+port-to-buffer irc-server
							  irc-port)))))
           (goto-char (point-max))
	   (ding nil)
	   (ding nil)
	   (ding nil)
	   (irc-insert "")
	   (irc-insert "")
	   (irc-insert (make-string (1- (window-width
					 (get-buffer-window (current-buffer))))
				    ?*))
	   (let ((msg (format "Your IRC session ended at %s." (irc-get-time))))
	     (irc-insert "%s%s"
			 (make-string (max (/ (- (window-width
						  (get-buffer-window
						   (current-buffer)))
						 (length msg))
					      2)
					   0)
				      ? )
			 msg))
	   (irc-insert (make-string (1- (window-width
					 (get-buffer-window (current-buffer))))
				    ?*))
	   (irc-insert ""))
         ;; all that needs to be done is a little maintenance ...
         (setq irc-processes (delq proc irc-processes)))))



;; processing input
(defun irc-process-input ()
  "If in the input region, parse it for messages and commands.
In the output region, next-line if irc-spacebar-pages, otherwise do nothing.

All of the lines in the input region are rejoined during processing to be
handled as one.  A command is any line starting with a / after leading
whitespace is stripped away; commands can not exceed 510 characters.  Messages
can be longer but they will be split into 510 character segments for IRC.  The
buffer will reflect how the message was sent if it needed to be broken; the
split(s) will be indicated by \" >>\" to mean that the message is continued."
  (interactive)
  ;; do the simple stuff for the output region
  (if (< (point) irc-mark)
      (if irc-spacebar-pages
	  (next-line 1)
	  (ding))
      (if (not (processp (get-buffer-process (current-buffer))))
	  (irc-insert (concat "%%Buffer not connected to any IRC-server, type"
			      " M-x irc RET to connect to a server."))
	  (irc-check-time)
	  ;; the input region is more work ...
	  ;; first, toast extraneous spaces, tabs and newlines
	  ;; at end of input region
	  (delete-region (goto-char (point-max))
			 (if (re-search-backward "[^ \n]" irc-mark t)
			     (1+ (point))
			     (point)))
	  ;; nuke the white space at the beginning of input region, too
	  (delete-region (goto-char irc-mark)
			 (progn (re-search-forward " *")
				(point)))
	  (setq irc-history-index -1)	; reset the history scroll location
	  (let ((txt (buffer-substring irc-mark (point-max)))
		(maxlen (- irc-max-server-message-length 2))
		send
		ass)
	    ;; check to see if the input region is empty
	    (if (string= "" txt)
		(message "%%Nothing sent to the irc-server.")
		(while (string-match "\n" txt)
		  (aset txt (match-beginning 0) ? ))
		(if (string-match "^/" txt)  ; it's a command
		    (if (< (length txt) maxlen)
			(progn
			  (goto-char (point-max))
			  (insert "\n")
			  (set-marker irc-mark (point))
			  (irc-execute-command
			   (setq irc-last-command (substring txt 1))))
			;; can't use error because that kills the function
			(ding)
			(message "IRC commands can't exceed %d characters."
				 maxlen))
		    ;; "a specified sendlist" -- was there one?
		    (setq ass (irc-find-to txt 'explicit))
		    (if (and ass
			     (string-match "^[^;:]" txt))
			;; a real sendlist was specified, so
			;; update irc-last-explicit
			(setq irc-last-explicit (irc-find-to txt)))
		    (irc-add-to-hist (concat (if (not ass)
						 irc-default-to)
					     (buffer-substring irc-mark
							       (point-max))))
		    (while (> (length txt) maxlen)
		      (setq send (substring txt 0 maxlen)
			    txt  (substring txt maxlen)
			    send (irc-fix-wordwrap send txt)
			    txt  (concat (if ass
					     irc-last-explicit
					     irc-default-to)
					 (cdr send))
			    send (concat (car send) " >>"))
		      (goto-char (+ irc-mark (- (length send) 3)))
		      (insert " >>\n" (if ass
					  irc-last-explicit
					  irc-default-to))
		      (if (looking-at " ")
			  (delete-char 1))
		      (beginning-of-line)
		      (set-marker irc-mark (point))
		      (irc-execute-msg send))
		    (goto-char (point-max))
		    (insert "\n")
		    (set-marker irc-mark (point))
		    (irc-execute-msg txt))))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-command (str)
  "Execute the \"/\" command of STR.  STR should not begin with a slash.
Commands are first looked up in the irc-alias-alist; if it is found there
then the alias gets passed recursively with any arguments the original
had.  The irc-command-alist is checked next and finally the irc-operator-alist.
A command is considered \"found\" when it matches either exactly or
unambiguously starting at the first character.  That is, J would match JOIN,
but OIN would not."
  (let* ((case-fold-search t)
         (cmd (substring str 0 (string-match "\\( +\\|$\\)" str)))
         (text (substring str (match-end 0)))
         (ambig (irc-check-list
                 (mapcar 'car (append irc-alias-alist irc-command-alist
                                      (if irc-operator irc-operator-alist)))
                 cmd 'start-only))
         (matches nil)
	 (irc-called-from-buffer t))
    ;; if no matches are found the command might still be a valid command
    ;; name hiding behind non-operator status.  i don't like messages that
    ;; lie and say "Unknown command '/REHASH'" so this should make it not lie.
    (if (and (not irc-operator) (null ambig))
        (setq ambig (irc-check-list (mapcar 'car irc-operator-alist) cmd t)))
    ;; first determine any ambiguities among the lists
    (if (null ambig)
        ;; no matches at all were found
        (irc-insert "%%Unknown command \"/%s\".  Type /HELP for help."
                    (upcase cmd))
	;; this is here for when a regular command gets aliased.  it shows up
	;; as being ambiguous but it really isn't later on.
	(if (irc-member-general (car ambig) (cdr ambig) 'string=)
	    (setq ambig (cdr ambig)))
	(if (> (length ambig) 1)
	    (irc-insert "%%Ambiguous command \"/%s\".  Could be %s."
			(upcase cmd)
			(irc-subst-comma
			 (mapconcat (function (lambda (arg)
				      (concat "/" arg)))
				    ambig
				    ", ")
			 "or"))
	    ;; alias list has highest priority
	    (setq matches (irc-check-list (mapcar 'car irc-alias-alist) cmd t))
	    ;; make sure matches is what we set out to looking for ...
	    (if (and matches (string= (car matches) (car ambig)))
		;; call this function again with the text as argument
		(irc-execute-command
		 (concat (cdr (assoc (car matches) irc-alias-alist))
			 ;; the servers won't grok trailing whitespace for some
			 ;; messages so only use it to separate an argument
			 (if (string< "" text) " ") text))
		;; next try the command alist
		(setq matches (irc-check-list (mapcar 'car irc-command-alist)
					      cmd
					      t))
		(if (and matches
			 (not (irc-check-list (mapcar 'car irc-operator-alist)
					      cmd
					      t)))
		    ;; call the appropriate irc-execute-* function
		    (funcall (intern-soft
			      (concat "irc-execute-"
				      (cdr (assoc (car matches)
						  irc-command-alist))))
			     text)
		    ;; no matches yet.  last resort is the operator alist
		    (setq matches (irc-check-list (mapcar 'car
							  irc-operator-alist)
						  cmd
						  t))
		    (if matches
			(if irc-operator
			    (funcall
			     (intern-soft (concat
					   "irc-execute-"
					   (cdr
					    (assoc (car matches)
						   irc-operator-alist))))
			     text)
			    (irc-insert (concat "%%Only enabled IRC Operators"
						" can use the /%s command. Use"
						" /OPER to enable yourself.")
					(upcase (car matches)))))))))))


(defun irc-send (str)
  "Send STR to process in the current buffer.
A CR-LFD pair is appended automatically as per the 'official' IRC protocol,
but it seems unnecessary.  Returns its argument STR."
  (setq str (irc-lowlevel-enquote str))
  (let* ((proc (get-buffer-process (current-buffer)))
	 (sent (if (processp proc)
		   (condition-case x
		       (send-string proc (concat str "\r\n"))
		     (error x))))
	 (ok (and (processp proc)
		  (eq sent nil))))
    (if (not ok)
	(let ((stars (make-string (1- (window-width (get-buffer-window
						     (current-buffer))))
				  ?*)))
	  (irc-insert "")
	  (irc-insert "")
	  (irc-insert "%s" stars)
	  (irc-insert "?Failed sending to server!")
	  (cond ((not (processp proc))
		 (irc-insert "?No \"process\" to send to."))
		((consp sent)
		 (irc-insert (concat "?Signaled condition is \"%s\" and"
				     " associated data is \"%s\".")
			     (car sent)
			     (cdr sent)))
		(t (irc-insert "?Unknown reason, proc=%s, sent=%s."
			       proc
			       sent)))
	  (irc-insert "?Your IRC session aborted at %s." (irc-get-time))
	  (irc-insert "%s" stars)
	  (if (processp proc) (delete-process proc))
	  (error "IRC aborted"))
	(irc-log-in-debug-buffer (concat "+ " str)))))



;; sending messages to people
(defun irc-execute-privmsg (str)
  "Usage: /MSG recipient(s) message

This command is provided simply for compatability with the C client.
It is preferable instead to just type the name of the user followed by
a semi-colon and then the message. That is, \"tale;hi!\" will send the
message \"hi!\" to the user with the nickname which starts with
\"tale\".  A semi-colon at the beginning of the line means to send to
the last recipient explicity specified; typing a semi-colon at the
beginning of a line expands it to the last recipient(s) specified. The
recipients for a message can be a comma separated list of users and/or
channels. Don't use any spaces left of the semi-colon.

You can send messages to users and channels, and if you are an enabled
IRC operator, you can also send broadcasts to server machines and
client machines.

Examples:
/msg wiz hi there           send \"hi there\" to user with nickname wiz
wiz;hi there                send to user wiz
msa,wiz;hi there            send to the users wiz and msa
42;hi there                 send into channel 42
42,wiz;hi there             channel 42 and user wiz

If you are an enabled IRC operator:
$minsk.docs.uu.se;...       send a message to all users using server minsk
$*.se;...                   send to all users using swedish servers
#cia.docs.uu.se;...         send to all users running their clients on cia
#*.se;...                   all user running their clients on swedish machines"
  (irc-add-to-hist
   (irc-execute-msg
    (concat
     (setq irc-last-explicit (concat (substring str 0 (string-match " +\\|$"
								    str))
				     ";"))
     (substring str (match-end 0))))))


(defun irc-execute-msg (str)
  "Send a message to a channel or another user.  Returns its argument STR,
munged slightly to indicate where it was attempted to be sent."
  ;; this really is an indirect function of the UI (ie, not through a /COMMAND)
  ;; so it isn't interactive
  (let ((tolist nil)
	(orig str)
	(icw nil)
	(confirm nil)
	(on-hash (= ?# (aref irc-channel 0))) ;New 2.6 #channel?
	(newsrvr (irc-server-has-multijoinable-channels)))
    (if (string-match "^[:;]" str)
        ;; a little bit of fill-in-the-blank
        (setq str (concat irc-last-explicit (substring str 1)))
	(if (not (irc-find-to str 'explicit))
	    ;; prepend an implicit sendlist if need be
	    (if irc-default-to
		(setq str (concat irc-default-to str))
		(irc-insert "%%You have no default sendlist."))))
    (if (irc-find-to str 'explicit)
        (setq icw (irc-find-to str)
              tolist (irc-burst-comma (substring icw 0 (1- (length icw))))
	      tolist (mapcar 'irc-clean-up-message tolist)
              str (irc-find-message str)
              ;; kill on leading space if it exists.  ie, "tale: hi" will
              ;; send "hi" as a message not " hi".
              str (if (string-match "^ *" str)
		      (substring str (match-end 0)))))
    (setq confirm (delq			; whee.  lisp indentation is fun.
		   nil
		   (mapcar
		    (function
		     (lambda (to)
		      (if (not (zerop (string-to-int to)))
			  (if (string= to irc-channel)
			      (progn
				(irc-send
				 (if (and newsrvr on-hash)
				     (concat "PRIVMSG " irc-channel " :" str)
				     (concat "MSG :" str)))
				to)
			      ;; new in 1.2 -- you _can_ send to a channel you
			      ;; are not on
			      (irc-send (concat "PRIVMSG " to " :" str))
			      to)
			  (if (not (or (string= to "*")
				       (string= to "0")))
			      (setq icw (irc-check-list (irc-recall-all
							 'irc-nicknames)
							to)))
			  (cond ((string= to "*")
				 (if (string= "0" irc-channel)
				     (progn
				       (irc-insert (concat "%%You are not"
							   " talking to any"
							   " channel."))
				       nil)
				     (irc-send
				      (if (and newsrvr on-hash)
					  (concat "PRIVMSG " irc-channel
						  " :" str)
					  (concat "MSG :" str)))
				     irc-channel))
				((string= to "0")
				 (irc-insert "%%You can't send to channel 0.")
				 nil)
				((= (length icw) 1)
				 (irc-send (concat "PRIVMSG "
						   (car icw)
						   " :"
						   str))
				 (car icw))
				((not icw)
				 ;; Wox! No one found, but we'll do a
				 ;; nonomatching. Try sending it anyway and
				 ;; let the server bitch if necessary.
				 ;; So don't remember this "nonmatch".
				 (irc-send (concat "PRIVMSG " to " :" str))
				 to)
				(t (irc-insert (concat "%%Ambiguous recipient"
						       " \"%s\"; could be %s.")
					       to
					       (irc-subst-comma
						(mapconcat
						 (function
						  (lambda (arg)
						   (concat "\"" arg "\"")))
						 icw
						 ", ")
						"or"))
				   nil)))))
		    tolist)))
    (setq confirm (let ((foo ())
			(bar confirm))
		    (while (> (length bar) 0)
		      (setq foo (cons
				 (concat
				  (cond
				    ((irc-is-nickname (car bar)) "user")
				    ((irc-is-channelname (car bar)) "channel")
				    ((irc-is-broadcastname (car bar)) "server")
				    (t "receiver"))
				  " "
				  (car bar))
				 foo)
			    bar (cdr bar)))
		    foo))
    (let* ((rcvr (irc-subst-comma (mapconcat 'eval confirm ", ")
				  "and"))
	   (data (irc-clean-up-message (format irc-msg-sent rcvr))))
      (if (and confirm irc-confirm)
	  (let ((c confirm))
	    (while c
	      (let ((r (irc-clean-up-message (format irc-msg-sent (car c)))))
		(irc-insert "%s%s" (make-string
				    (max 0 (- (window-width
					       (get-buffer-window
						(current-buffer)))
					      (length r)
					      1))
				    ? )
			    r)
		(setq c (cdr c)))))
	  (if (not confirm)
	      (irc-insert "%% Message not sent. Use /HELP for help."))))
    orig))


(defun irc-execute-oops (newto)
  "Usage: /OOPS intended-recipient
Send irc-oops to recipient(s) of last message and resend message to
'intended-recipient'.  This command is handy when you've just sent a message
to the wrong place and you want the person/people who saw it to know that they
should just disregard it.  The message which was originally sent then gets
forwarded to its proper destination."
  (interactive '(""))
  (if (not irc-called-from-buffer)
      (progn (irc-insert "")
	     (irc-insert "/OOPS")))  
  (let* ((prev (irc-find-to (car irc-history)))
	 (data (concat prev irc-oops)))
    ;; first do the oops message
    (irc-execute-msg data)
    ;; then resend the original
    (if (and (string= "" newto) irc-called-from-buffer)
	(irc-insert (concat "%%No new receiver given. Oops said to %s. Not"
			    " redirected.")
		    prev)
	(irc-execute-redirect newto)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-redirect (newto)
  "Usage: /REDIRECT additional-recipient

Send to 'additional-recipient' the last message which you sent.  This 
command can be fairly easily duplicated using the history mechanism by hand
but it is provided to make it even easier."
  (interactive '(""))
  (if (not irc-history)
      (irc-insert "%%No message sent yet, nothing to redirect.")
      (let* ((default (if irc-default-to 
			  (substring irc-default-to 0 (string-match
						       "[:;]" irc-default-to))
			  ""))
	     (prompt (if (string< "" default)
			 (concat "(RET for "
				 (irc-nuke-whitespace
				  (concat
				   default
				   (if (string-match "\\*" default)
				       (concat "; where *=\""
					       irc-channel
					       "\""))))
				 ") ")
			 ""))
	     (n (irc-nuke-whitespace
		 (if (and (string= "" newto) (not irc-called-from-buffer))
		     (irc-read-object
		      (format "Send copy of last message (%s) to whom? %s"
			      (let ((m (irc-find-message (car irc-history))))
				(if (> (length m) 5)
				    (concat (substring m 0 5) "...")
				    m))
			      prompt)
		      ""
		      (irc-recall-all 'irc-nicknames))
		     newto)))
	     (new (if (string= "" n) default n)))
	(if (not (irc-is-receiver new))
	    (irc-insert (concat "%%\"%s\" is not a valid receiver. Message not"
				" redirected.")
			new)
	    (let ((to (if (string= "" new)
			  default
			  (concat new ";"))))
	      (setq irc-last-explicit to)
	      (irc-add-to-hist
	       (irc-execute-msg
		(concat to (irc-find-message (car irc-history)))))))))
  (setq irc-idle-last-sent (irc-current-time)))



;; /commands for the server
(defun irc-execute-quote (msg)
  "Usage: /QUOTE string

This command is used to send 'string' directly to the IRC server without
any local processing.  Warning: this has the potential to screw up some
things in irc-mode, particularly if it is used to change your nickname or
to switch channels."
  (interactive '(""))
  (let ((m (if (and (string= "" msg) (not irc-called-from-buffer))
	       (read-string "String to send to server: ")
	       msg)))
    (cond ((string-match "^ *\\([^ ]+\\)" m)
	   (let ((cmd (subfield m 1)))
	     (irc-send m)
	     (irc-send (concat "CLIENT-SYNCH :QUOTE " cmd))))
	  (t (irc-insert "%%Nothing was sent to the IRC server."))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-who (channel)
  "Usage: /WHO [ channel | user ]

Get a list of the users on IRC.  Optional argument \"channel\" means to show
just the users on that channel, with * representing the current channel.
User can be any mask, ie *.se for current swedish IRCjunkies.

Each user is indicated on a separate line with their nickname, status,
channel, login name, host and real name. The second column, \"Stat\"
gives the status for the user, and is shown as a four letter word (:-)
according to the combination of their attributes. The possible
attributes are being an IRC operator, being a channel operator and
being marked as being away.

                                          Status field
A normal user having no attributes set:     (blank)
A normal user marked as being away:          Away
An IRC operator with no other attribues:     Iopr
An IRC operator marked as being away:        IoAw
A channel operator with no other attributes: Copr
A channel operator marked as being away:     CoAw
User being both IRC- and channel operator:   ICop
As above but also marked as being away:      ICAw

Being ignored takes precedence over all
other attributes, always shown as:           IGNR     (cf \"/HELP IGNORE\"). 

Users being on either no channel at all, or on channels with the mode
PRIVATE are appear with a blank \"Channel\" field. If you are
connected to a server of version 2.6, and are requesting a \"general\"
who listing (ie for users potentially on different channels), all
users \"Channel\" field will be blank.

An %-sign is appended to the name or number of your current
channel.

BE WARNED: on some servers, the operators CAN see who's on a negative channel.
On some of these servers, the channel number is disclosed, on others only
the fact that a user is on a negative channel, but not which one.
The same goes for \"private\" and \"secret\" channels.

If a single \"user\" is given as the argument, (ie a word starting with neither
a + nor a digit) it is taken to be the nickname of a user on IRC and more
information, if available, is given about the person.

If this function is called interactively then the prefix argument is used
as the channel to query.  No argument means all of them and an argument
of \"-\" or \"*\" means the current channel." 
  (interactive '(""))
  (let* ((c (if (and (string= "" channel) (not irc-called-from-buffer))
		(irc-read-object (concat
				  "Who? (Press return for ALL"
				  (if (string= "0" irc-channel)
				      ""
				      (concat ", * for " irc-channel))
				  ") ")
				 ""
				 (irc-get-channels-and-nicks-and-servers))
		channel))
	 (c2 (irc-nuke-whitespace c))
	 (chan (cond ((string= "" c2) "0")
		     ((string= "*" c2) irc-channel)
		     (t c2))))
    (if (string= "0" chan)
	(let ((s (irc-recall-all 'irc-services)))
	  (irc-forget-all 'irc-nicknames)
	  (irc-remember irc-nick-used 'irc-nicknames)
	  (while (and (listp s) s)
	    (irc-remember (car s) 'irc-nicknames)
	    (setq s (cdr s)))))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert "/WHO %s" chan)))
    (if (and irc-called-from-buffer (irc-is-nickname chan))
	(irc-execute-whois chan)
	(irc-send (concat "WHO " chan))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-whois (user)
  "Usage: /WHOIS user

Get a two line description of who and where \"user\" is.  If user is not
provided it is read from the minibuffer with a completing-read.
If * is given instead of a user name, you will be informed about ALL users.

BUG: The list of users used when * is given may be somewhat obsolete,
therefore first give a NAMES command."
  (interactive '(""))
  (let ((wholist (irc-recall-all 'irc-nicknames)))
    (if (and (string-match "^ *$" user) (not irc-called-from-buffer))
	(setq user (irc-read-object "Who is who? "
				    user
				    wholist)))
    (if (string< "" user)
	(progn (if (not irc-called-from-buffer)
		   (progn (irc-insert "")
			  (irc-insert "/WHOIS %s" user)))
	       (irc-send (concat "WHOIS " user)))
	(irc-insert "%%Who is who? No nick given.")))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-motd (server)
  "Usage: /MOTD [ server ]

Tells the message of the day at the server."
  (interactive '(""))
  (let ((host (if (string= "" server)
		  (if (not irc-called-from-buffer)
		      (irc-read-object
		       "Get message of the day for which host? "
		       ""
		       (irc-recall-all 'irc-servernames))
		      "")
		  server)))
    (if (string-match "^ *\\([^: ]*\\)" host)
	(let ((host (substring host (match-beginning 1) (match-end 1))))
	  (if (not irc-called-from-buffer)
	      (progn (irc-insert "")
		     (irc-insert "/MOTD %s" host)))
	  (irc-send (concat "MOTD " host)))
	(irc-insert (concat "%%Internal error in function irc-execute-motd,"
			    " please try another syntax and notify %s"
			    " by email.")
		    irc-hacker)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-version (server)
  "Usage: /VERSION [ server | user ]

Shows the version of the server, or of a user. If the argument looks
like a nickname, that user's client is queried using CTCP (client to
client protocol) about which client it is and which version of it it
is. If the queried client doesn't understand CTCP, no answer will
follow."
  (interactive '(""))
  (let* ((host (irc-nuke-whitespace
		(if (string= "" server)
		    (if (not irc-called-from-buffer)
			(irc-read-object
			 "Get version of which server/nick? "
			 ""
			 (irc-get-channels-and-nicks-and-servers))
			"")
		    server)))
	 (host (if (string= "" host) irc-server host))
	 (hst (if (string= "*" host) irc-channel host)))
    (if (string-match "^ *\\([^: ]*\\)" hst)
	(let ((h (substring hst (match-beginning 1) (match-end 1))))
	  (cond ((or (irc-is-nickname h)
		     (irc-is-channelname h))
		 (if (not irc-called-from-buffer)
		     (progn (irc-insert "")
			    (irc-insert (concat "/VERSION " h))))
		 (irc-insert (concat "%sQuerying client of user%s %s for"
				     " version of client%s")
			     irc-msg-info-pre
			     (if (irc-is-channelname h) "s on channel" "")
			     h
			     irc-msg-info-post)
		 (irc-send (concat "PRIVMSG " h " :\001"
				   (irc-ctcp-enquote "VERSION")
				   "\001")))
		(t (if (not irc-called-from-buffer)
		       (progn (irc-insert "")
			      (irc-insert "/VERSION %s" h)))
		   (irc-send (concat "VERSION " h)))))
	(irc-insert "%%Give only one word please. \"%s\" is not valid."
		    hst)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-list (channel)
  "Usage: /LIST [channel]

Get a list of the discussions that are on IRC. If a channel is given,
only the topic for that channel is shown. A * denotes your current
channel, if any. An %-sign is appended to your current channel's name
or number."
  (interactive '(""))
  (if (not irc-called-from-buffer)
      (progn (irc-insert "")
	     (irc-insert "/LIST %s" channel)))
  (irc-send (concat "LIST " channel))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-links (mask)
  "Usage: /LINKS [ mask ]

Show the names of all the servers which can communicate with your server.
The links can go down isolating different parts of the IRC-net, so this
is a good way to find out how extensive it is at the moment.

Given a mask like *.se, only server names matching the mask are displayed."
  (interactive '(""))
  (let ((m (if (and (string= "" mask) (not irc-called-from-buffer))
	       (irc-read-object (concat "Check for which servers? (RET for"
					" all, *.SE for swedish servers etc) ")
				""
				(irc-recall-all 'irc-servernames))
	       mask)))
    (cond ((string-match "^ *\\*? *$" m)
	   (irc-forget-all 'irc-servernames)
	   (irc-forget-all 'irc-linksinfo)))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert "/LINKS %s" m)))
    (irc-send (concat "LINKS " m)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-lusers (mask)
  "Usage: /LUSERS [servermask]

Get the number of users and servers on your IRC network. 

There is a optional argument you can use if you only want
to see the count of some part of the IRCnet. For instance,
to see how many swedish users are on, try /lusers *.se"
  (interactive '(""))
  (let* ((mask (irc-nuke-whitespace mask)))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert (format "/LUSERS %s" mask))))
    (irc-send (concat "LUSERS " mask))
    (setq irc-idle-last-sent (irc-current-time))))


(defun irc-execute-admin (server)
  "Usage: /ADMIN [ server ]

Get information about the IRC administrator for 'server'; if server is not
supplied just query for the server to which you are connected."
  (interactive '(""))
  (let ((s (if (and (string= "" server) (not irc-called-from-buffer))
	       (irc-read-object "Administrative info about which server? "
				""
				(irc-recall-all 'irc-servernames))
	       server)))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert "/ADMIN %s" s)))
    (irc-send (concat "ADMIN " s)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-time (&optional server)
  "Usage: /TIME [ server ]

Get the current time on 'server'; is no server is provided use the one to which
you are connected.  When called with a interactively with a prefix-argument
the server name is read using the minibuffer.

Querying other servers can be handy given that people on IRC are spread out
 from the west coast of the United States to Finland.  The question \"What
time is it in Finland?\" comes up so frequently that an alias -- /TF -- has
been provided by default to get the answer.  This alias should work as long
as tut.fi is connected to your IRC-net."
  (interactive '(""))
  (let ((s (if (and (stringp server)
		    (string= "" server)
		    (not irc-called-from-buffer))
	       (irc-read-object "Get the time at which server? "
				""
				(irc-recall-all 'irc-servernames))
	       server)))
    (if (and (stringp server) (not irc-called-from-buffer))
	(progn (irc-insert "")
	       (irc-insert "/TIME %s" s)))
    (irc-send (concat "TIME " s)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-join (channel)
  "Usage: /JOIN channel [mode]

Join \"channel\" on IRC.  If channel is not provided it is requested
in the minibuffer; when called interactively, the channel is prompted
for in the minibuffer.  Use /LEAVE to exit the channel.

You can supply an initial mode for the channel. If the channel didn't
exist before you joined it, the channel will be sat to the supplied mode.
If the channel already existed, the mode will be ignored by the server.

One can be listening on several channels at once. Up to 10 channels
can be listened to at any single time, either 10 #channels, or 1 old
style (either a +channel or a nummeric channel) and 9 #channels. Use
/JOIN to join (ie start listening and talkinging to) a channel and
/LEAVE to stop listening to a channel. If you /LEAVE the channel you
were talking to, you will end up not talking to any channel; just use
/JOIN again to remedy that situation.  There is no way yet to start
listening to a channel without choosing that channel as the one you
will talk to. Of course, another /JOIN will help..."

  (interactive '(""))
  (let* ((pair (cond
		 ((string-match "^ *\\([^ ]+\\) +\\([^ ]\\)" channel)
		  (cons (subfield channel 1)
			(concat (subfield channel 2)
				(substring channel (match-end 0)))))
		 ((string-match "^ *\\([^ ]+\\) *$" channel)
		  (cons (subfield channel 1) ""))
		 (t (cons "" ""))))
	 (c (car pair))
	 (m (cdr pair))
	 (subchnls (irc-recall-all 'irc-subscribed-channels))
	 (chn (irc-nuke-whitespace
	       (if (and (string= c "")
			(not irc-called-from-buffer))
		   (irc-read-object "Channel to join? "
				    ""
				    subchnls)
		   c)))
	 (mode (irc-nuke-whitespace
		(if (and (string= m "")
			 (not irc-called-from-buffer)
			 (not (irc-recall chn 'irc-subscribed-channels)))
		    (irc-read-object (format
				      "Initial mode for channel \"%s\"? "
				      chn)
				     ""
				     '("A combination of +-bilmnopst"))
		    m)))
	 (digit-start (string-match "^-?[0-9]" chn)))
    (if (string= "" chn) ; well, so much for that 
	(irc-insert "%%No channel given. continuing talking to %s."
		    irc-channel)
	(let* ((newsrvr (irc-server-has-multijoinable-channels)) 
	       (mtch (irc-recall chn 'irc-subscribed-channels)))
	  (cond ((string= "0" chn) 
		 (setq irc-channel "0")
		 (irc-show-subscribed-channels)) 
		((and (not mtch)
		      (not (irc-is-channelname chn))) 
		 (irc-insert (concat "%%Warning, that channel name (%s) looks" 
				     " strange, but will try to /join it"
				     " anyway. You might be in for a surprise,"
				     " thought."))
		 (if (not irc-called-from-buffer) 
		     (progn (irc-insert "")
			    (irc-insert "/JOIN %s %s" chn mode))) 
		 (irc-send (format "%s %s %s"
				   (if newsrvr "JOIN " "CHANNEL ")
				   chn
				   mode)))
		(mtch (setq irc-channel mtch)
		      (if (not irc-called-from-buffer) 
			  (progn (irc-insert "")
				 (irc-insert "/JOIN %s" mtch)))
		      (irc-show-subscribed-channels))
		(t (if (not irc-called-from-buffer)
		       (progn (irc-insert "")
			      (irc-insert "/JOIN %s %s" chn mode)))
		   (irc-send (format "%s %s %s"
				     (if newsrvr "JOIN " "CHANNEL ")
				     chn
				     mode))))))
    (setq irc-msg-cont-used
	  (if (string= "0" chn) ;On private?
	      irc-msg-cont 
	      (make-string (1- (length (concat irc-msg-cont chn)))
			   (string-to-char " ")))) )
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-leave (channel)
  "Usage: /LEAVE [channel]

Leave your current channel (or a selected one from the set of joined channels,
if you're using a v2.6 or later server). Don't join another channel.

Also see /HELP JOIN."
  (interactive '(""))
  (let* ((c (if (and (string-match "^ *$" channel)
		     (not irc-called-from-buffer))
		(irc-read-object (concat "Leave which channel? (0 for ALL"
					 (if (string= "0" irc-channel)
					     ") "
					     (concat ", RET for "
						     irc-channel
						     ") ")))
				 ""
				 (irc-recall-all 'irc-subscribed-channels))
		channel))
	 (chan (irc-nuke-whitespace
		(if (and (not (string= "0" irc-channel))
			 (or (string= "*" c) (string= "" c)))
		    irc-channel
		    c))))
    (if (string= "0" chan)
	(progn
	  (let ((lst (irc-recall-all 'irc-subscribed-channels))
		(irc-multiple-leave-in-progress t)
		(irc-called-from-buffer t))
	    (while (not (null lst))
	      (irc-send (concat "PART " (car lst)))
	      (setq lst (cdr lst))))
	  (irc-show-subscribed-channels))
	(if (not irc-called-from-buffer)
	    (progn (irc-insert "")
		   (irc-insert "/LEAVE %s" chan)))
	(if (string< "" chan)
	    (progn (irc-insert "%sLeaving channel %s%s"
			       irc-msg-info-pre chan irc-msg-info-post)
		   (if (irc-server-has-multijoinable-channels)
		       (irc-send (concat "PART " chan))
		       (irc-send "CHANNEL 0")))
	    (irc-insert (concat "%%Leave which channel? No channel given. Try"
				" /MEMBERSHIPS")))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-nick (name)
  "Usage: /NICKNAME name

Change your nickname in IRC.  A nickname can contain alphanumeric characters,
underscores (_), hyphens (-) or the special characters left brace ({), right
brace (}), vertical bar (|), left bracket ([), right bracket (]) and back
slash (\\). These special character are alphabetic characters in some
languages like the scandinavian ones.  The name cannot start with a hyphen or
number and only the first nine characters are used.

Unfortunately, due to the way confirmation from the servers work, it might be
falsely reported that your nickname was successfully changed when it was not.
The server will come back and say so and finally irc-mode will wise-up and
note that your nickname was not changed.

All the above things change from server version to server version, so they may
or may not work. Try it."
  (interactive '(""))
  (let ((newnick (if (and (string= "" name) (not irc-called-from-buffer))
		     (irc-nuke-whitespace (read-string "New nickname? "))
		     name)))
    (if (string= "" newnick)
	(irc-insert "%sNickname not changed%s"
		    irc-msg-info-pre
		    irc-msg-info-post)
	(progn
	  (if (not irc-called-from-buffer)
	      (progn (irc-insert "")
		     (irc-insert "/NICK %s" newnick)))
	  (irc-insert "%sTrying to change your nickname to \"%s\"%s"
		      irc-msg-info-pre
		      newnick
		      irc-msg-info-post)
	  (set-buffer-modified-p (buffer-modified-p))
	  (irc-send (concat "NICK " newnick)))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-quit (text)
  "Usage: /QUIT [reason]

Exit IRC.  The connection is closed but the buffer is left behind.
If you want, you can give a reason for quitting IRC."
  (interactive '(""))
  (if (not irc-called-from-buffer)
      (progn (if (string-match "^ *$" text)
		 (setq text (yow)))
	     (irc-insert "")
	     (irc-insert "/QUIT %s" text))) 
  (irc-send (concat "QUIT :" text))
  (sit-for 2)				;Give server a chance to react.
  (let ((proc (get-buffer-process (current-buffer))))
    (cond ((processp proc)
	   (irc-sentinel proc "finished\n")
	   (delete-process proc))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-away (text)
  "Usage: /AWAY [message]

Mark yourself as away, giving TEXT to people who send you private messages.
Without any arguments it will just insert a message about your current status."
  (interactive '(""))
  (let ((reason (if (and (string= "" text) (not irc-called-from-buffer))
		    (read-string "Reason to be away? ")
		    text)))
    (if (string= "" reason)
	(if irc-away
	    (let ((pre (concat irc-msg-info-pre "You are ")))
	      (irc-insert (concat "%smarked as being away: \"%s\", "
				  "use /HERE to remove the mark%s")
			  pre
			  (substring irc-away 2 -1)
			  irc-msg-info-post))
	    (irc-insert "%sYou are not currently marked as being away%s"
			irc-msg-info-pre
			irc-msg-info-post))
	(if (not irc-called-from-buffer)
	    (progn (irc-insert "")
		   (irc-insert "/AWAY %s" reason)))
	(irc-send (concat "AWAY :" reason))
	(setq irc-away (concat " [" reason "]")))
    (set-buffer-modified-p (buffer-modified-p)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-here (cruft)
  "Usage: /HERE

Mark yourself as present (ie, not \"away\") on IRC.  Any arguments to this
command are ignored."
  (interactive '(""))
  (if (not irc-called-from-buffer)
      (progn (irc-insert "")
	     (irc-insert "/HERE")))
  (irc-send "AWAY")
  (setq irc-away nil)
  (set-buffer-modified-p (buffer-modified-p))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-topic (topic)
  "Usage: /TOPIC channel [topic ...]
      or: /TOPIC [topic ...]

Make 'topic' the description of the named channel; * for channelname means
the current channel you're talking to.

With no topic, doesn't change the topic but only inspects it.

If no channel is given (and the first word of the topic doesn't looks like
a channel name), the command operates on the current channel you're talking to.
"
  (interactive '(""))
  (let* ((top1 (cond ((and (string= "" topic) (not irc-called-from-buffer))
		      (read-string (concat "Topic for channel "
					   irc-channel
					   "? (RET to check) ")))
		     (t topic)))
	 (top (if (string-match "^\\([#&][^ :]+\\) *" top1)
		  (substring top1 (match-end 0))
		  top1))
	 (chn (if (string-match "^\\([#&][^ :]+\\) *" top1)
		  (subfield top1 1)
		  irc-channel)))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert "/TOPIC %s %s" chn top)))
    (cond ((string= "0" chn)
	   (irc-insert "%%You aren't on any channel."))
	  ((and (irc-is-multijoinable-channel chn)
		(irc-server-has-settable-topic-on-multijoinable-channel))
	   (if (string-match "^ *$" top)
	       (irc-send (concat "LIST :" chn))
	       (irc-send (concat "TOPIC " chn " :" top))))
	  ((irc-is-multijoinable-channel irc-channel)
	   (irc-insert "%%Can't set the topic of a #channel."))
	  (t (irc-send (concat "TOPIC :" top)))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-oper (oper)
  "Usage: /OPER [ name [ password ]]

Attempt to become an IRC Operator.  Can take the name of the operator
and the password as arguments.  If name is missing then it will be read
from the minibuffer; if password is missing it will be read and hidden
in the minibuffer.

If you become an operator then the word \"operator\" will appear in the
minibuffer along with the mode name."
  (interactive '(""))
  (let* ((pair (cond
		 ((string-match "^ *\\([^: ]+\\) +" oper)
		  (cons (subfield oper 1) (substring oper (match-end 0))))
		 ((string-match "^ *\\([^: ]+\\) *$" oper)
		  (cons (subfield oper 1) ""))
		 (t (cons "" ""))))
	 (n (car pair))
	 (n1 (if (string= "" n)
		 (read-string (concat "Operator name at server "
				      (upcase irc-server)
				      "? "))
		 n))
	 (name (irc-nuke-whitespace n1))
	 (p (irc-nuke-whitespace (cdr pair)))
	 (p1 (if (and (string= "" p) (string< "" name))
		 (irc-read-passwd (concat "Password for operator "
					  name
					  " at server "
					  (upcase irc-server)
					  "? "))
		 p))
	 (password (irc-nuke-whitespace p1)))
    (cond ((string= "" name)
	   (irc-insert "%%No operator name given."))
	  ((string= "" password)
	   (irc-insert "%%No password given."))
	  (t (if (not irc-called-from-buffer)
		 (progn (irc-insert "")
			(irc-insert "/OPER ...")))
	     (irc-send (concat "OPER " name " " password)))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-summon (user)
  "Usage: /SUMMON login-name@server

Summon a user not on IRC to join IRC.  The argument provided may either be
a user name on the local machine or user@server, where server is another
machine on the IRC-net.  The user must be signed on to the specified server."
  (interactive '(""))
  (let* ((pair (cond ((string-match (concat "^ *\\([^ @]*\\) *@ *"
					    "\\([^ @]*\\) *$")
				    user)
		      (cons
		       (substring user (match-beginning 1) (match-end 1))
		       (substring user (match-beginning 2) (match-end 2))))
		     ((string-match "^ *\\([^ @]*\\) *$" user)
		      (cons
		       (substring user (match-beginning 1) (match-end 1))
		       ""))
		     (t (cons "" ""))))
	 (login (irc-nuke-whitespace
		 (if (and (string= "" (car pair)) (not irc-called-from-buffer))
		     (read-string "Login-name of person to summon? ")
		     (car pair))))
	 (s (irc-nuke-whitespace
	     (if (and (not (string= "" login))
		      (string= "" (cdr pair))
		      (not irc-called-from-buffer))
		 (irc-read-object (format "Server \"%s\" is on? " login)
				  ""
				  (irc-recall-all 'irc-servernames))
		 (cdr pair))))
	 (server (if (string= "" s) irc-server s)))
    (if (string= "" login)
	(irc-insert "%%No login name given.")
	(progn (if (not irc-called-from-buffer)
		   (progn (irc-insert "")
			  (irc-insert "/SUMMON %s@%s" login server)))
	       (irc-send (concat "SUMMON " login "@" server)))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-users (host)
  "Usage: /USERS [ server ]

Get a list of the users signed on to \"server\".  If no server name is provided
then the server to which you are connected is used.  When called interactively
a prefix argument means to prompt for the server to query."
  (interactive '(""))
  (let* ((h (irc-nuke-whitespace
	     (if (and (string= "" host) (not irc-called-from-buffer))
		 (irc-read-object (format (concat "List users on which host?"
						  " (RET for %s) ")
					  irc-server)
				  ""
				  (irc-recall-all 'irc-servernames))
		 host)))
	 (hst (if (string= "" h) irc-server h)))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert "/USERS %s" hst)))
    (irc-send (concat "USERS " hst)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-ignore (user)
  "Usage: /IGNORE username@host [event]

Used to ignore a user. The first argument is a specification of whose messages
to ignore. The second specifies which kind of messages should be ignored from
that particular user.

When specifying a user, you can give either just the user's login name at the
remote site (you can see what it is by doing a /whois), or you can give the
user's login name followed by a @ and the name of the user's and the host of
the user (i.e. the name of host the user's client is running on). You can give
a * instead of a username to ignore everyone at the given client machine.

One can either ignore all signs of the user, or just certain events. The events
include
    CTCP-ANSWER = Don't display any client to client protocol answers (they
                  *might* be spurios). Also, don't react on erronous answers.
    CTCP-QUERY  = Neither display nor answer any client to client protocoll
                  questions from the user.
    INVITE      = Don't display 
    JOIN
    NICK
    NOTICE
    PART
    PRIVMSG
    QUIT
    channel
    
THIS IS ALL NOT YET IMPLEMENTED.

"
  (interactive '(""))
  (let ((usr (irc-nuke-whitespace
	      (if (and (string= "" user) (not irc-called-from-buffer))
		  (irc-read-object "Ignore which user? (RET to view) "
				   user
				   (irc-recall-all 'irc-nicknames))
		  user))))
    (if (string= "" usr)
	(if (not (irc-nothing-remembered-p 'irc-ignored-ppl))
	    (irc-insert "%sYou are currently ignoring %s%s"
			irc-msg-info-pre
			(irc-subst-comma (mapconcat
					  'eval
					  (irc-recall-all 'irc-ignored-ppl)
					  ", ")
					 "and")
			irc-msg-info-post)
	    (irc-insert "%sYou are not ignoring anyone%s"
			irc-msg-info-pre
			irc-msg-info-post))
	(progn (irc-remember usr 'irc-ignored-ppl)
	       (irc-insert "%sYou are now ignoring %s%s"
			   irc-msg-info-pre
			   usr
			   irc-msg-info-post))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-info (nick)
  "Usage: /INFO nick

With * as the argument, show some information about who built IRC.
Else query a users client and show the information she/he has given
about her/himself. With no argument, show the information you have
given about yourself. You can set the information with the command
/USERINFO."
  (interactive '(""))
  (let* ((name (irc-nuke-whitespace
		(if (and (string= "" nick) (not irc-called-from-buffer))
		    (irc-read-object
		     (concat "Info for which user or server? (RET"
			     " for yourself, * for server program)"
			     " ")
		     ""
		     (irc-get-names-and-servers))
		    nick))))
    (cond ((and (string= "" name)
		(stringp irc-userinfo))
	   (let* ((pre (concat irc-msg-info-pre "You have "))
		  (irc-msg-cont-used (make-string (length pre) ? )))
	     (irc-insert (concat "%sgiven the information \"%s\" about"
				 " yourself. Use /USERINFO to change it."
				 " (Warning: /USERINFO with no argument will"
				 " clear the information)%s")
			 pre
			 irc-userinfo
			 irc-msg-info-post)))
	  ((string= "" name)
	   (let* ((pre (concat irc-msg-info-pre "You haven't "))
		  (irc-msg-cont-used (make-string (length pre) ? )))
	     (irc-insert (concat "%sgiven any information about"
				 " yourself yet. Use /USERINFO to do so%s")
			 pre
			 irc-msg-info-post)))
	  ((string= "*" name)
	   (if (not irc-called-from-buffer)
	       (progn (irc-insert "")
		      (irc-insert "/INFO *")))
	   (irc-send "INFO"))
	  ((irc-is-receiver name)
	   (if (not irc-called-from-buffer)
	       (progn (irc-insert "")
		      (irc-insert (concat "/INFO " name))))
	   (irc-insert (concat "%sQuerying client of user %s for user supplied"
			       " information%s")
		       irc-msg-info-pre name irc-msg-info-post)
	   (irc-send
	    (concat "PRIVMSG "
		    name
		    " :\001FINGER\001\001VERSION\001\001USERINFO\001")))
	  ((irc-is-hostname name)
	   (if (not irc-called-from-buffer)
	       (progn (irc-insert "")
		      (irc-insert (concat "/ADMIN " name))))
	   (irc-send (concat "ADMIN " name)))
	  (t (irc-insert "%%Can't be a users nickname: \"%s\"." name))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-userinfo (information)
  "Usage: /USERINFO [information]

Set some information (any text you want) to be given out to anybody
querying this client for it. With no argument, remove all information
you have given about yourself, making further queries \"draw a
blank\". You can use /INFO to query other clients. Be warned though,
other clients may or may not know of this query so you may sometimes
get back no answer or strange responses."
  (interactive '(""))
  (let ((info (if (and (string= "" information) (not irc-called-from-buffer))
		  (read-string (concat "What information do you wish to"
				       " disclose? (RET to remove) "))
		  information)))
    (let ((inf (irc-nuke-whitespace info)))
      (cond ((string= "" inf)
	     (setq irc-userinfo nil)
	     (irc-insert "%sCleared your information string%s"
			 irc-msg-info-pre irc-msg-info-post))
	    (t (setq irc-userinfo inf)
	       (irc-insert "%sThe information string is: \"%s\"%s"
			   irc-msg-info-pre
			   irc-userinfo
			   irc-msg-info-post)))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-kill (user-etc)
  "Usage: /KILL user comment

Forcibly remove a user from IRC. The mandotory comment will be displayed to
the victim and to all the IRC operators online. The topic should be in English,
which after all is the lingua franca of IRC.

This command is reserved for IRC operators."
  (interactive '(""))
  (let* ((pair (cond ((string-match "^ *\\([^ ]+\\) +\\(.\\|\n\\)+ *$"
				    user-etc)
		      (cons (subfield user-etc 1) (subfield user-etc 2)))
		     ((string-match "^ *\\([^ ]+\\) *$" user-etc)
		      (cons (subfield user-etc 1) ""))
		     (t (cons "" ""))))
	 (u (irc-nuke-whitespace (car pair)))
	 (user (irc-nuke-whitespace
		(if (and (string= "" u) (not irc-called-from-buffer))
		    (irc-read-object "Nuke which user? "
				     ""
				     (irc-recall-all 'irc-nicknames))
		    u)))
	 (c (irc-nuke-whitespace (cdr pair)))
	 (comm (irc-nuke-whitespace
		(if (and (string= "" c)
			 (string< "" user)
			 (not irc-called-from-buffer))
		    (read-string "Comment? (In English please) ")
		    c))))
    (if (string< "" user)
	(if (string< "" comm)
	    (progn (if (not irc-called-from-buffer)
		       (progn (irc-insert "")
			      (irc-insert "/KILL %s %s" user comm)))
		   (irc-send (concat "KILL " user " " comm)))
	    (irc-insert "%%No comment given to /KILL command."))
	(irc-insert "%%No user given, no killing take place.")))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-invite (user)
  "Usage: /INVITE user [ channel ]

Ask \"user\" on IRC to join \"channel\".  If channel is 0, * or not provided
then the invitation de faults to your current channel, ie the on you're talking
to. If you are not talking to any channel and channel is 0 or not provided then
no invitation is sent -- you can't invite someone to \"go private\". When
called interactively, channel is set to the prefix argument; with no argument
or - the current channel is assumed."
  (interactive '(""))
  (let* ((pair (cond ((string-match "^ *\\([^ ]+\\) +\\([^ ]+\\) *$"
				    user)
		      (cons
		       (substring user (match-beginning 1) (match-end 1))
		       (substring user (match-beginning 2) (match-end 2))))
		     ((string-match " *\\([^ ]+\\) *$" user)
		      (cons (substring user (match-beginning 1) (match-end 1))
			    ""))
		     (t (cons "" ""))))
	 (usr (irc-nuke-whitespace
	       (if (and (string= "" (car pair)) (not irc-called-from-buffer))
		   (irc-read-object "Invite which user? "
				    ""
				    (irc-recall-all 'irc-nicknames))
		   (car pair))))
	 (c1 (irc-nuke-whitespace
	      (if (and (string= "" (cdr pair))
		       (string< "" usr)
		       (not irc-called-from-buffer))
		  (irc-read-object
		   (if (string= "0" irc-channel)
		       (format "Invite \"%s\" to which channel? " usr)
		       (format "Invite \"%s\" %s \"%s\") "
			       usr
			       "to which channel? (RET for"
			       irc-channel))
		   ""
		   (irc-recall-all 'irc-subscribed-channels))
		  (cdr pair))))
	 (chn (if (string= "" c1) irc-channel c1)))
    (cond ((string= "" usr) (irc-insert "%%No users nick given."))
	  ((string= "0" chn) (irc-insert "%%Can't invite to \"channel\" 0."))
	  (t (if (not irc-called-from-buffer)
		 (progn (irc-insert "")
			(irc-insert "/INVITE %s %s"
				    usr
				    (irc-clean-up-message chn))))
	     (irc-send (format "INVITE %s %s" usr chn)))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-names (channel)
  "Usage: /NAMES [ channel ]

Show which channels everyone is on.  Optional argument \"channel\" means
to show just the users on that channel.  * means to show people on the
current channel.

Each line starts with a column for the channel number and is followed
by the nicknames of the people on that channel.  Users who are on
private channels or who are not on any channel are listed as
\"Private\".  Users who are on secret channels (channels less than 0)
are not shown at all.  A %-sign is appended to the current channel's
name or number."
  (interactive '(""))
  (let* ((c (if (and (string= "" channel) (not irc-called-from-buffer))
 		(irc-read-object (format
				  "Names of persons on which channel? (%s) "
				  (if (string= "0" irc-channel)
				      "RET for all"
				      (concat "RET for all, * for "
					      irc-channel)))
				 ""
				 (irc-recall-all 'irc-subscribed-channels))
		channel))
	 (chn (if (string= "*" c) irc-channel c)))
    (if (string= "" chn)
 	(let ((lst (irc-recall-all 'irc-services)))
	  (irc-forget-all 'irc-nicknames)
	  (while lst
	    (irc-remember (car lst) 'irc-nicknames)
	    (setq lst (cdr lst)))))
    (if (not irc-called-from-buffer)
 	(progn (irc-insert "")
	       (irc-insert "/NAMES %s" chn)))
    (irc-send (concat "NAMES " chn))
    (if (not (and (irc-terminal-is-slow)
		  (irc-server-has-end-of-names)))
	(progn (irc-insert "")
	       (irc-insert "Name of channel Users Nicknames")
	       (irc-insert "--------------- ----- ---------")
	       (set-buffer-modified-p (buffer-modified-p)))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-wall (message)
  "Usage: /WALL message

Send 'message' to everyone on IRC.  This can only be done by IRC Operators."
  (interactive '(""))
  (let ((msg (if (and (string= "" message) (not irc-called-from-buffer))
		 (read-string "Message to send to everyone? ")
		 message)))
    (if (string= "" msg)
	(irc-insert "%%No message.")
	(if (not irc-called-from-buffer)
	    (progn (irc-insert "")
		   (irc-insert "/WALL %s" msg)))
	(irc-send (concat "WALL " msg))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-wallops (message)
  "Usage: /WALLOPS message

Send 'message' to every enabled IRC-operator on IRC.
If you got any problems you think an enabled IRC operator might be the right
person to help you with, *use* /WALLOPS. (But please don't *ABuse* it)."
  (interactive '(""))
  (let ((msg (if (and (string= "" message) (not irc-called-from-buffer))
		 (read-string "Message to send to all enabled IRC operators? ")
		 message)))
    (if (string< "" msg)
	(let ((data (format irc-msg-sent "all ENABLED operators of IRC")))
	  (if (not irc-called-from-buffer)
	      (progn (irc-insert "")
		     (irc-insert "/WALLOPS %s" msg)))
	  (irc-send (concat "WALLOPS " msg))
	  (if irc-confirm
	      (irc-insert "%s%s" (make-string
				  (max 0 (- (window-width
					     (get-buffer-window
					      (current-buffer)))
					    (length data)
					    1))
				  ? )
			  data)))
	(irc-insert "%%No message given, no message sent.")))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-rehash (cruft)
  "Usage: /REHASH

Force the server to which you are connected to reread it's irc.conf file.
Arguments are ignored.  This command is only available to IRC Operators."
  (interactive '(""))
  (if (not irc-called-from-buffer)
      (progn (irc-insert "")
	     (irc-insert "/REHASH")))
  (irc-send "REHASH")
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-trace (server)
  "Usage: /TRACE [ server ]

Find the route from the server to which you are attached to 'server'; if the
server argument is not provided then the servers to which the current server
is directly connected are listed.  This command is only available to IRC
Operators."
  (interactive '(""))
  (let ((srvr (if (and (string= "" server) (not irc-called-from-buffer))
		  (irc-read-object (concat "Trace route to server/user?"
					   " (RET for " irc-server ") ")
				   ""
				   (irc-get-names-and-servers))
		  server)))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert "/TRACE %s" srvr)))
    (irc-send (concat "TRACE " srvr)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-squit (server)
  "Usage: /SQUIT [ server ]

Shut down a server-to-server link.
The link shut down is the the one next farest away from you, ie if you do a
/SQUIT BYE.EDU and the path to it is your.server.edu!a!b!bye.edu, then the
link between b and bye.edu will be disconnected.

You have to be an enabled IRC operator to issue this conmmand."
  (interactive '(""))
  (let ((srvr (if (and (string= "" server) (not irc-called-from-buffer))
		  (irc-read-object "Shut down link to which server? "
				   ""
				   (irc-recall-all 'irc-servernames))
		  server)))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert "/SQUIT %s" srvr)))
    (irc-send (concat "SQUIT " srvr))
    (irc-insert "%sClosing the link to server %s%s"
		irc-msg-info-pre srvr irc-msg-info-post))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-connect (new)
  "Usage: /CONNECT [ newhost [ port [remoteserver ] ] ]

Connect the local server to SERVER on tcp-port PORT.

You have to be an enabled IRC operator to issue this conmmand."
  (interactive '(""))
  (let* ((tri (cond ((string-match
		      (concat "^ *\\([^ ]+\\) +\\([^ ]+\\) +"
			      "\\([^ ]+\\) *$")
		      new)
		     (cons (subfield new 1)
			   (cons (subfield new 2)
				 (subfield new 3))))
		    ((string-match "^ *\\([^ ]+\\) +\\([^ ]+\\) *$"
				   new)
		     (cons (subfield new 1)
			   (cons (subfield new 2)
				 "")))
		    ((string-match "^ *\\([^ ]+\\) *$" new)
		     (cons (subfield new 1) (cons "" "")))
		    (t (cons "" (cons "" "")))))
	 (h1 (car tri))
	 (h2 (if (and (string= "" h1) (not irc-called-from-buffer))
		 (irc-read-object (concat "Connect server-link to which"
					  " internet host? ")
				  ""
				  (irc-recall-all 'irc-servernames))
		 h1))
	 (new-host (irc-nuke-whitespace h2))
	 (p1 (car (cdr tri)))
	 (p2 (if (and (string= "" p1)
		      (string< "" new-host)
		      (not irc-called-from-buffer))
		 (irc-read-object (concat "Use which TCP-port for"
					  " server-link? (RET for 6667) ")
				  ""
				  (list "194" "6667"))
		 p1))
	 (p3 (irc-nuke-whitespace p2))
	 (new-port (if (string= "" p3)
		       6667 
		       (string-to-int p3)))
	 (s1 (cdr (cdr tri)))
	 (s2 (if (and (string= "" s1)
		      (string< "" new-host)
		      (not irc-called-from-buffer))
		 (irc-read-object (concat "Execute connect command at which"
					  " (reachable) server? (RET for"
					  " local) ")
				  ""
				  (irc-recall-all 'irc-servernames))
		 s1))
	 (s3 (irc-nuke-whitespace s2))
	 (remote-server (if (string= "" s3) irc-server s3)))
    (cond ((string-match " " new-host)
	   (irc-insert "%%Spaces aren't allowed in hostnames (%s)." new-host))
	  ((string-match " " remote-server)
	   (irc-insert "%%Spaces aren't allowed in servername (%s)."
		       remote-server))
	  (t (if (not irc-called-from-buffer)
		 (progn (irc-insert "")
			(if (string= "" new-host)
			    (irc-insert "/CONNECT")
			    (irc-insert "/CONNECT %s %d %s"
					new-host new-port remote-server))))
	     (if (string= "" new-host)
		 (irc-insert "%%No host to connect too given.")
		 (irc-send (format "CONNECT %s %d %s"
				   new-host new-port remote-server))
		 (irc-insert (concat "%sConnecting a server-link to host %s on"
				     " port %d from server %s%s")
			     irc-msg-info-pre
			     (upcase new-host)
			     new-port
			     (upcase remote-server)
			     irc-msg-info-post)))
	  (setq irc-idle-last-sent (irc-current-time)))))

;; /command just for the client  (need /stamp /alias /unalias)
(defun irc-execute-send (slist)
  "Usage: /SEND [ sendlist | - ]

Set the default sendlist for IRC messages.  This is a comma separated list
of the intended recipient(s) of messages which do not have an explicit
sendlist.  '-' as an argument means to disable the default sendlist; every
message sent then must have an explicit recipient provided with the message.
Without any arguments this command just displays the current default sendlist.

Each item specified is checked to see whether you can send there; ambiguous
references to users are not allowed.

\"*\" is always allowed and means to send to the current channel, ie the
channel you are talking to.
If no item in the new list can be set then the sendlist is not changed."
  (interactive "sDefault recipient(s) for messages? ")
  ;; blast some whitespace
  (setq slist (irc-nuke-whitespace slist))
  (let (matches)
    ;; first the easiest case
    (if (string= "-" slist) (setq irc-default-to nil)
	(setq matches
	      (delq nil                   ; more indentation fun.  can someone
		    (mapcar               ; recommend a good style manual?
		     (function (lambda (arg)
		       (setq matches (irc-check-list
				      (irc-recall-all 'irc-nicknames)
				      arg))
		       (cond
			 ((string= arg "*") arg)
			 ((string= arg "0")
			  (irc-insert "%%You can't send to channel 0.")
			  nil)
			 ((= (length matches) 1) (car matches))
			 ((eq matches nil)
			  (irc-insert "%%No names found to match \"%s\"." arg)
			  nil)
			 (t (irc-insert (concat "%%Ambiguous recipient %s;"
						" could be %s.")
					arg
					(irc-subst-comma
					 (mapconcat (function (lambda (arg)
						      (concat "\"" arg "\"")))
						    matches
						    ", ")
					 "or"))
			    nil))))
		     (irc-burst-comma slist))))
	(if matches
	    (setq irc-default-to (concat (mapconcat 'eval matches ",") ";"))
	    (or (string= "" slist)  ; only print the error if tried to set it.
		(irc-insert "%%No matches -- sendlist not changed."))))
    (cond ((not irc-default-to)
	   (irc-insert (concat "%%Your default sendlist is disabled. (Ie you"
			       " are not sending to anyone when failing to"
			       " give an explicit receiver)")))
	  ((string= irc-default-to "*;")
	   (irc-insert (concat "%sYou are now using the default send list (ie"
			       " sending to the current channel (%s) when"
			       " failing to give an explicit receiver)%s")
		       irc-msg-info-pre
		       (if (string= "0" irc-channel) "none" irc-channel)
		       irc-msg-info-post))
	  (t (irc-insert (concat "%sYou are sending to %s, use /SEND * to"
				 " get back default sendlist%s")
			 irc-msg-info-pre
			 (irc-subst-comma
			  (mapconcat 'eval
				     (irc-burst-comma
				      (substring irc-default-to 0
						 (1- (length irc-default-to))))
				     ", ")
			  "and")
			 irc-msg-info-post))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-service (service)
  "Usage: /SERVICE [ [+]nick | -nick ]

Show or update list of services. A service is an automaton which looks
like a normal user, ie it has a nickname. It doesn't act as a normal
user though; normaly you can ask it for some service. For instance, at
NICKSERV you can register your nick name (do \"nickserv; help\" for
more information). Services often talk to you with so called NOTICE's.
When a service isn't marked as being one, these messages will be
displayed in a somewhat annoying format. Use this command to get a
nicer display.

With no argument, show which nicknames are assumed to be services."
  (interactive '(""))
  (let* ((pair (cond ((string-match (concat "^ *\\(+\\|-\\) *"
					    "\\([^ ]+\\) *$")
				    service)
		      (cons (subfield service 1) (subfield service 2)))
		     ((string-match "^ *\\([^ ]+\\) *$" service)
		      (cons "+" (subfield service 1)))
		     (t (cons "" ""))))
	 (a (car pair))
	 (a2 (irc-nuke-whitespace
	      (if (and (string= "" a) (not irc-called-from-buffer))
		  (irc-read-object (concat "Mark or unmark nick as"
					   " service? (RET to mark) ")
				   ""
				   '("mark" "unmark"))
		  a)))
	 (action (if (string-match "\\(unmark\\|-\\)" (downcase a2)) nil t))
	 (n (cdr pair))
	 (nick (irc-nuke-whitespace
		(if (and (string= "" n) (not irc-called-from-buffer))
		    (irc-read-object (format "%sark which %s as service? (%s) "
					     (if action "M" "Unm")
					     (if action "nick" "service")
					     "RET to view existing")
				     ""
				     (if action
					 (irc-recall-all 'irc-nicknames)
					 (irc-recall-all 'irc-services)))
		    n)))
	 (s-list (irc-recall-all 'irc-services))
	 (s-len (length s-list))
	 (plural (if (= 1 s-len) "" "s"))
	 (lst (irc-listify s-list ", " "and")))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert "/SERVICE %s%s" (if action "+" "-") nick)))
    (if (string= "" nick)
	(if (irc-nothing-remembered-p 'irc-services)
	    (irc-insert "%sNo users marked as automatons%s"
			irc-msg-info-pre irc-msg-info-post)
	    (irc-insert (concat "%s%d \"user%s\" marked as being"
				" automaton%s (which provide some service):"
				" %s%s")
			irc-msg-info-pre s-len plural plural lst
			irc-msg-info-post))
	(let ((irc-called-from-buffer t))
	  (irc-insert "%s%sing \"%s\" %s being treated as a service%s"
		      irc-msg-info-pre
		      (if action "Add" "Remov")
		      nick
		      (if action "as" "from")
		      irc-msg-info-post)
	  (if action
	      (irc-remember nick 'irc-services)
	      (irc-forget nick 'irc-services))
	  (irc-execute-service ""))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-event (events)
  "Usage: /EVENT [ [+]event | -event ] [...]

Set the list of events to notify you about with a message.  Notification
is a one-line message inserted when someone causes that event to occur.
Events are added with +event or simply event; they are removed with -event.
+ adds all supported events and - removes all supported events.  More than
one event can be specified in the arguments.  In case of conflict, the argument
which appears later overrides the argument with which it conflicts.

Currently supported by /EVENT are the \"ctcp\", \"join\", \"nick\", \"quit\"
and \"topic\" events.

CTCP happens whenever someone send you a CTCP query.
Join happens whenever someone enters or leaves a channel which you are on.
Nick occurs when someone changes nicknames; recognition of this event is
currently limited to when the person making the change is on the same channel
as you.
Quit happens when someone quits from IRC and you see it.
Topic happens when the topic of a channel you listen to is changed.


"
  (interactive "sNotify for events: ")
  ;; die scurvy whitespace
  (setq events (irc-nuke-whitespace events))
  (let ((recog '(ctcp join nick quit topic))
	(str events)
	sym
	off
	event
	(count 0))
    (while (string< "" events)
      ;; multiple args are okay.  we'll do one at a time.
      (setq str (substring events 0 (or (string-match " +" events)
					(string-match "$" events)))
	    events (substring events (match-end 0)))
      (string-match "^\\([---+]?\\)" str)
      (setq off (string= "-" (substring str (match-beginning 1) (match-end 1)))
	    event (substring str (match-end 0))
	    sym (if (string= "" event) nil
		    (car (delq nil	; do some minor pattern matching
			       (mapcar	; to find the intended event
				(function
				 (lambda (arg)
				  (if (string-match
				       (concat "^" (regexp-quote event))
				       (prin1-to-string arg))
				      arg))) recog)))))
      (cond
	((and (string= "" event) off) (setq irc-events nil))
	;; the only way for this to happen and not the above is str == "+"
	((string= "" event) (setq irc-events recog))
	((null sym)
	 (irc-insert "%sEvent: Unknown argument \"%s\"%s"
		     irc-msg-info-pre
		     event
		     irc-msg-info-post))
	(t (setq irc-events (if off (delq sym irc-events)
				  (if (not (memq sym irc-events)) ; avoid
				      (cons sym irc-events) ; redundancy
				      irc-events))))))
    (if irc-events
	(irc-insert "%sEvent notification is currently enabled for %s%s"
		    irc-msg-info-pre
		    (irc-subst-comma (mapconcat 'prin1-to-string irc-events
						", ") "and")
		    irc-msg-info-post)
	(irc-insert "%sNotification is currently disabled%s"
		    irc-msg-info-pre
		    irc-msg-info-post)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-confirm (str)
  "Usage: /CONFIRM [ + | - ]

Turn on message confirmation with + or off with -.  Any other arguments or no
arguments just gives a message about the current setting.

Message confirmation is a line indicating to whom a message was sent.
Occasionally this will say that a message has been sent to someone who
was not present but another message soon after will set the record straight."
  (interactive '(""))
  (let* ((c1 (irc-nuke-whitespace
	      (if (and (string= "" str) (not irc-called-from-buffer))
		  (irc-read-object "Turn confirm on or off? (RET to view) "
				   ""
				   '("+" "on" "-" "off"))
		  str)))
	 (conf (cond ((string-match "^\\(on\\|+\\)$" c1) "+")
		     ((string-match "^\\(off\\|-\\)$" c1) "-")
		     ((string= "" c1) "")
		     (t nil))))
    (if (not conf)
	(irc-insert "%Huh? Try /HELP CONFIRM.")
	(progn (if (string= "+" conf) (setq irc-confirm t))
	       (if (string= "-" conf) (setq irc-confirm nil))
	       (irc-insert "%sMessage confirmation is %s%s"
			   irc-msg-info-pre
			   (if irc-confirm "on" "off")
			   irc-msg-info-post))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-notify (users)
  "Usage: /NOTIFY [ + | [-]nick ... ]

Add and delete nicknames from your notifylist. When people on the list get
detected, you will be notified. When they quit IRC, you will be notified of
this fact too.

With no argument, show the people on list, no matter if they've been
detected or not.

With a list of nicknames as the argument (each optionally prepended by
a \"-\"), add those nicknames to the list, unless a \"-\" was
prepended, in which case the nickname is removed."
  (interactive '(""))
  (let* ((str (irc-nuke-whitespace
	       (if (and (string= "" users) (not irc-called-from-buffer))
		   (irc-read-object
		    "Notify when detecting whom? (RET to show list) "
		    ""
		    (irc-recall-all 'irc-nicknames))
		   users)))
	 (list (mapcar 'irc-nuke-whitespace (reverse (irc-burst-comma str))))
	 (added ())
	 (removed ())
	 (show-current (null list)))
    (while (not (null list))
      (let* ((subtract (= ?- (aref (car list) 0)))
	     (name (if subtract (substring (car list) 1) (car list)))
	     (is-in-list (irc-recall name 'irc-notify-looked-for)))
	(cond ((not (irc-is-nickname name))
	       (irc-insert "%%This doesn't look like a nickname: \"%s\"."
			   name))
	      (subtract
	       (if is-in-list
		   (setq removed (cons name removed)))
	       (irc-forget (substring (car list) 1) 'irc-notify-looked-for))
	      (t (if (not is-in-list)
		     (setq added (cons name added)))
		 (irc-remember (car list) 'irc-notify-looked-for))))
      (setq list (cdr list)))
    (irc-who-is-on (irc-recall-all 'irc-notify-looked-for))
    (if show-current
	(let* ((detected (irc-recall-all 'irc-notify-detected))
	       (looked-for (irc-recall-all 'irc-notify-looked-for))
	       (pre (format "%sOf the persons "
			    irc-msg-info-pre))
	       (irc-msg-cont-used (make-string (length pre) ? )))
	  (if (null looked-for)
	      (irc-insert "%sYou're not looking for anybody%s"
			  irc-msg-info-pre irc-msg-info-post)
	      (progn
		(irc-insert "%syou want notifications for (%s),"
			    pre
			    (irc-listify looked-for ", " "and"))
		(setq irc-msg-cont-used (substring
					 irc-msg-cont-used
					 0
					 (- (length "the persons "))))
		(irc-insert "%s%s on IRC at the moment%s"
			    irc-msg-cont-used
			    (if (null detected)
				"no one is"
				(concat
				 "the person"
				 (if (= 1 (length detected)) " " "s ")
				 (irc-listify detected ", " "and")
				 (if (= 1 (length detected)) " is" " are")))
			    irc-msg-info-post))))
	(let* ((a (if added
		      (format "Added %s to"
			      (irc-listify (reverse added) ", " "and"))))
	       (r (if removed
		      (format "%semoved %s from"
			      (if added ", and r" "R")
			      (irc-listify (reverse removed) ", " "and"))))
	       (str (cond ((and added removed) (format "%s%s" a r))
			  (added a)
			  (removed r)
			  (t "No change to")))
	       (indent (or (and (string-match " " str) (1+ (match-end 0)))
			   irc-msg-cont-used))
	       (irc-msg-cont-used (make-string indent ? )))
	  (irc-insert "%s%s the notification list%s"
		      irc-msg-info-pre str irc-msg-info-post)))))


(defun irc-execute-unignore (user)
  "Usage: /UNIGNORE user | + | -

Stop ignoring a user who has been /IGNOREd.  The special arguments + or -
mean to stop ignoring everyone who is being ignored."
  (interactive '(""))
  (let ((usr (irc-nuke-whitespace
	      (if (and (string= "" user) (not irc-called-from-buffer))
		  (irc-read-object "Stop ignoring whom? (RET to view) "
				   ""
				   (irc-recall-all 'irc-ignored-ppl))
		  user))))
    (if (string= "" usr)
	(if (irc-nothing-remembered-p 'irc-ignored-ppl)
	    (irc-insert "%sYou are not ignoring anyone%s"
			irc-msg-info-pre
			irc-msg-info-post)
	    (irc-insert "%sYou are currently ignoring %s%s"
			irc-msg-info-pre
			(irc-subst-comma
			 (mapconcat 'eval
				    (irc-recall-all 'irc-ignored-ppl)
				    ", ")
			 "and")
			irc-msg-info-post))
	(if (string-match "^[---+]$" usr)
	    (progn (irc-forget-all 'irc-ignored-ppl)
		   (irc-insert "%sYou are no longer ignoring anyone%s"
			       irc-msg-info-pre
			       irc-msg-info-post))
	    (if (string< "" user)
		(progn (irc-forget user 'irc-ignored-ppl)
		       (irc-insert "%sYou are no longer ignoring %s%s"
				   irc-msg-info-pre
				   user
				   irc-msg-info-post))))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-kick (kick)
  "Usage: /KICK user [channel]
   or: /KICK channel user

Kick user out from a channel. If no channel is given, defaults to the channel
you are currently talking to."
  (interactive)
  (let* ((c (if (string-match "^ *[^: ]+ +\\([^: ]+\\) *$" kick)
		(substring kick (match-beginning 1) (match-end 1))
		irc-channel))
	 (chn (if (string= c "*") irc-channel c))
	 (usr (if (string-match "^ *\\([^: ]+\\) +[^: ]+ *$" kick)
		   (substring kick (match-beginning 1) (match-end 1))
		   kick))
	 (reversed (and (or (irc-is-channelname usr)
			    (string= usr "*"))
			(irc-is-nickname chn)))
	 (chan (if reversed (if (string= "*" usr) irc-channel usr) chn))
	 (user (if reversed chn usr)))
    (if (string-match "^ *$" user)
	(irc-insert "%%No user given, use /HELP KICK for help.")
	(if (not irc-called-from-buffer)
	    (progn (irc-insert "")
		   (irc-insert "/KICK %s %s" user chan)))
	(irc-send (concat "KICK " chan " :" user))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-mail (m)
  "Usage: /MAIL [command [arguments]]

Give commands to the MAIL subsystem of IRC. This has nothing to do with
\"regular email\".
Do \"/MAIL HELP\" for seeing what commands are supported and how to use them.
Be warned though, the syntax in the help is somewhat ... nonstandard."
  (interactive)
  (if (not irc-called-from-buffer)
      (progn (irc-insert "")
	     (irc-insert "/MAIL %s" m)))
  (irc-send (concat "MAIL " m))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-stats (args)
  "Usage: /STATS [ type [ server ] ]

Get statistics from a server. There are different kind of statistics
one can obtain:
	C	tells which other servers etc a server will accept
		connections from and will connect to
	H	tells which servers are treated as HUBS and LEAVES
	I	tells which masks are used to allow client connections
		at a server
	K	tells which user isn't allowed to use a specific server
	L	tells how much data has passed over the currently
		active links
        M       tells how many times the different (server level) commands
                has been seen by the server
        N       same as C
	Q	tells which servers are in quarantine, ie for which servers
		a server disconnects its local link in that direction
        R       tells some internal resource statistics regarding traffic on
                the server
        S       tells which services are connected to a server
        U       tells the uptime of a server
	Y	tells the connection classes in effect at a server
        Z       tells some internal resource statistics regarding the servers
                memory usage

If no server is given, the current server (ie the one your client is
connected to at the time of the command) is used."
  (interactive '(""))
  (let* ((pair (cond ((string-match "^ *\\([^ ]+\\) +\\([^ ]+\\) *$" args)
		      (cons (subfield args 1) (subfield args 2)))
		     ((string-match "^ *\\([^ ]+\\) *$" args)
		      (cons (subfield args 1) ""))
		     (t (cons "" ""))))
	 (c (car pair))
	 (s (cdr pair))
	 (cmd (irc-nuke-whitespace
	       (if (and (string= "" c) (not irc-called-from-buffer))
		   (irc-read-object (concat "Which STATS command? (C, I, K, L,"
					    " M, Q, R, S, U, Y or Z) ")
				    ""
				    '("C" "I" "K" "L" "M" "Q" "R" "S" "U" "Y"
				      "Z"))
		   c)))
	 (server (irc-nuke-whitespace
		  (if (and (string= "" s) (not irc-called-from-buffer))
		      (irc-read-object (concat "STATS at which server? "
					       "(RET for "
					       irc-server
					       ") ")
				       ""
				       (irc-recall-all 'irc-servernames))
		      s))))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert "/STATS %s"
			   (concat (upcase cmd)
				   (if (not (string= "" cmd))
				       " "
				       "")
				   server))))
    (irc-send (concat "STATS " (concat cmd " " server)))
    (setq irc-idle-last-sent (irc-current-time))))


(defun irc-execute-mode (mode)
  "Usage: /MODE [channel [commands [arguments]]]
   or: /MODE nick [commands [arguments]]

Change or inspect the mode of a channel or a user.
The following modes exist for channels:
   a      Make channel anonymous (not supported by any servers yet)
   b      Manipulate a channels list of banned people
   i      Make channel joinable only with an invitation
   k      Add or remove channel key (password)
   l      Limit the number of users on the channel (argument: count)
   m      Make channel moderated
   n      Forbid /MSG's into channel
   o      Make someone a channel operator of this channel (argument: user)
   p      Make the channel private (ie channel invisible, users shown as being
          on some private channel)
   s      Make the channel secret (ie both channel and users invisible)
   t      Lock the topic of the channel (only settable by channel operators)
   v      Gives a user a voice on a moderated channel

The following modes exist for a user:
   i      Invisible
   o      Operator on IRC - can only be set with /OPER command
   s      Statusmessages, receive from server
   w      WALLOPS, receive from server
At the moment, only one self can be manipulated, and only i, s and w
can be set.

Prepend the mode command character with a + to turn the correspending
mode on, or with a - to turn it off.

Examples:
    /mode                    Show the mode of the current channel.
    /mode #foo               Show the mode of the channel named \"#foo\".
    /mode #foo +t            Locks the topic of channel \"#foo\".
    /mode * +i+o WiZ         Makes the current channel invite only and
                             gives user WiZ channel operator status.
    /mode * -o+l+o WiZ 3 `   Removes channel operator status for the
                             current channel from user WiZ, sets the
                             maximum number of users to 3 and makes
                             user ` a channel operator.

    /mode WiZ                Show the mode of user WiZ.
    /mode WiZ +s             Start receiving local status messages, such as
                             other servers connecting to this server etc.
    /mode WiZ +i-w           Become invisible and stop receiving WALLOPS.

"
  (interactive '(""))
  (let* ((tri (cond ((string-match "^ *\\([^ ]+\\) +\\([^ ]+\\) +\\([^ ]\\)"
				   mode)
		     (cons (subfield mode 1)
			   (cons (subfield mode 2)
				 (concat (subfield mode 3)
					 (substring mode (match-end 0))))))
		    ((string-match "^ *\\([^ ]+\\) +\\([^ ]\\)" mode)
		     (cons (subfield mode 1)
			   (cons (concat (subfield mode 2)
					 (substring mode (match-end 0)))
				 "")))
		    ((string-match "^ *[^ ]" mode)
		     (cons (irc-nuke-whitespace mode) (cons "" "")))
		    (t (cons "" (cons "" "")))))
	 (ch1 (irc-nuke-whitespace (car tri)))
	 (ch2 (irc-nuke-whitespace
	       (if (and (string= "" ch1) (not irc-called-from-buffer))
		   (irc-read-object (concat "Get/set mode of what channel/user"
					    "? "
					    (if (not (string= "0" irc-channel))
						(format "(RET for %s) "
							irc-channel)))
				    ""
				    (append
				     (irc-recall-all 'irc-subscribed-channels)
				     (list irc-nick)))
		   ch1)))
	 (channel (if (or (string= "" ch2) (string= "*" ch2)) irc-channel ch2))
	 (is-nick (irc-is-nickname channel))
	 (cmd1 (irc-nuke-whitespace (car (cdr tri))))
	 (command (irc-nuke-whitespace
		   (if (and (string= "" cmd1)
			    (string< "" channel)
			    (not irc-called-from-buffer))
		       (irc-read-object (format (concat "Mode commands for"
							" channel %s? (RET to"
							" view) ")
						channel)
					""
					(if is-nick
					    '("+" "-" "i" "o" "s" "w")
					    '("+" "-" "b" "i" "k" "l" "m" "n"
					      "o" "p" "s" "t" "v")))
		       cmd1)))
	 (a1 (irc-nuke-whitespace (cdr (cdr tri))))
	 (arg (irc-nuke-whitespace
	       (if (and (string= "" a1)
			(string< "" channel)
			(string-match "[LOlo]" command)	;Needs arguments?
			(not irc-called-from-buffer))
		   (irc-read-object (format "Arguments to \"/MODE %s %s\"? "
					    channel command)
				    ""
				    (irc-recall-all 'irc-nicknames))
		   a1))))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert "/MODE %s %s %s" channel command arg))) 
    (irc-send (format "MODE %s %s %s" channel command arg)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-die (&optional dummy)
  "Usage: /DIE

Tells your local server to go belly-up."
  (interactive)
  (if (not irc-called-from-buffer)
      (progn (irc-insert "")
	     (irc-insert "/DIE")))
  (irc-send (concat "DIE"))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-finger (nick)
  "Usage: /FINGER user

Query a users client to get users finger information. As some clients don't
understand this query, you might or might not get back a valid answer.
Sometime you'll get back a confused \"What?\" from user instead."
  (interactive '(""))
  (let* ((u (irc-nuke-whitespace nick))
	 (user (irc-nuke-whitespace
		(if (and (string= "" u) (not irc-called-from-buffer))
		    (irc-read-object "Get FINGER info from whom? "
				     ""
				     (irc-recall-all 'irc-nicknames))
		    u))))
    (if (string= "" user)
	(irc-insert "%%No nick name given.")
	(if (not irc-called-from-buffer)
	    (progn (irc-insert "")
		   (irc-insert "/FINGER %s" user)))
	(irc-send (concat "PRIVMSG " user " :\001FINGER\001"))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-describe (str)
   "Usage: /DESCRIBE user action

Send a CTCP ACTION message to a user. As some clients don't understand
this message, you might get back a confused \"What?\" from user instead."
   (interactive '(""))
   (let* ((nick (substring str 0 (string-match " +\\|$" str)))
	  (action (substring str (match-end 0)))
	  (u (irc-nuke-whitespace nick))
	  (user (irc-nuke-whitespace
 		(if (and (string= "" u) (not irc-called-from-buffer))
 		    (irc-read-object "Send a ACTION message to whom? "
 				     ""
 				     (irc-recall-all 'irc-nicknames))
 		    u)))
	  (m (irc-nuke-whitespace action))
	  (msg (irc-nuke-whitespace
 	       (if (and (string= "" m) (not irc-called-from-buffer))
 		   (irc-read-object "What is your action? "
 				    ""
				    nil)
		   m))))
     (if (string= "" user)
	 (irc-insert "%%No nick name given.")
	(if (string= "" msg)
	   (irc-insert "%%No action given.")
	  (if (not irc-called-from-buffer)
 	    (progn (irc-insert "")
 		   (irc-insert "/DESCRIBE %s %s" user msg)))
 	(irc-send (concat "PRIVMSG " user " :\001ACTION " msg "\001")))))
   (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-me (action)
   "Usage: /ME action

Send a CTCP ACTION message to current channel. As some clients don't
understand this message, you might get back a confused \"What?\" from
some users."
   (interactive '(""))
   (let* ((m (irc-nuke-whitespace action))
	  (msg (irc-nuke-whitespace
 	       (if (and (string= "" m) (not irc-called-from-buffer))
 		   (irc-read-object "What is your action? "
				    ""
				    nil)
		   m)))
	  (on-hash (= ?# (aref irc-channel 0))) ;New 2.6 #channel?
	  (newsrvr (irc-server-has-multijoinable-channels)))
     (if (string= "" msg)
	 (irc-insert "%%No action given.")
	(if (not irc-called-from-buffer)
	   (progn (irc-insert "")
 		 (irc-insert "/ME %s" msg)))
       (irc-send (concat (if (and newsrvr on-hash)
			     (concat "PRIVMSG " irc-channel)
			    "MSG")
			 " :\001ACTION " msg "\001"))
       (let* ((confirm (format "(Description sent to channel %s)" irc-channel))
	      (padding (make-string (max 0 (- (window-width
					       (get-buffer-window
						(current-buffer)))
					      (length confirm)
					      1))
				    ? )))
	 (irc-insert "%s%s" padding confirm))
       ))
   (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-ping (nick)
  "Usage: /PING nick

Query a user's client (using CTCP) and calculate the distance between
the two of you in tenth of seconds. As some clients don't understans this
query, you might or might not get back a valid answer. Sometimes you
might even get back a confused \"What?\" from the user of the client.

Due to some technical limits and sojges laziness, the measurment isn't
foolproof. If the result seems to be totally wrong, resubmit the query.

The granularity is 500 ms."
  (interactive '(""))
  (let* ((n (irc-nuke-whitespace nick))
	 (v (irc-nuke-whitespace
	     (if (and (string= "" n) (not irc-called-from-buffer))
		 (irc-read-object "Get PING distance to whom? "
				  ""
				  (irc-recall-all 'irc-nicknames))
		 n)))
	 (victim (if (string= "*" v) irc-channel v)))
    (if (string= "" victim)
	(irc-insert "%%No name given.")
	(if (not irc-called-from-buffer)
	    (progn (irc-insert "")
		   (irc-insert "/PING %s" victim)))
	(irc-send (format (concat "PRIVMSG %s :"
				  "\001ERRMSG PING RELAY %d\001"
				  "\001PING %d\001")
			  victim
			  (car (cdr (irc-current-time)))
			  (car (cdr (irc-current-time)))))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-memberships (&optional dummy)
  "Usage: /MEMBERSHIPS

Show which channels you're listening to, and which one you're talking to.

Use /JOIN to join more or other channels, /LEAVE to leave some."
  (interactive '(""))
  (if (not irc-called-from-buffer)
      (progn (irc-insert "")
	     (irc-insert "/MEMBERSHIPS")))
  (irc-show-subscribed-channels)
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-signal (sigs)
  "Usage: /SIGNAL [ + | - | [+]event | -event ] [...]

Set the events which will get signals (aks bells or dings) when they
occur.  Events supported are:

  private -- private messages      join     -- channel changes
  public  -- public messages       topic    -- channel topic changes
  wall    -- broadcast messages    nick     -- nickname changes
  invite  -- invitations           backtalk -- your nick is mentioned

Without any arguments /SIGNAL simply prints a message about what signals
are currently enabled.  With event or +event turn on all signalling for that
event.  Remove all signals for an event with -event.  /SIGNAL + or /SIGNAL -
adds or removes all signals respectively.  Multiple arguments are accepted;
later ones take precedence over the ones which came before them.  For example,
\"/SIGNAL - +w +i\" would turn off all signals and then turn on signalling only
for wall messages and invitations."
  (interactive "sSet signal: ")
  ;; blow some whitespace away.  curiously this doesn't work correctly in debug
  (setq sigs (irc-nuke-whitespace sigs))
  (let ((recog '(backtalk private public wall invite join nick topic))
	str
	sym
	on
	off
	event)
    (while (string< "" sigs)
      ;; take one argument at a time
      (setq str  (substring sigs 0 (string-match " +\\|$" sigs))
	    sigs (substring sigs (match-end 0)))
      (string-match "^\\([---+]?\\)" str)
      (setq off (string= "-" (substring str (match-beginning 1) (match-end 1)))
	    event (substring str (match-end 0))
	    sym (if (string= "" event) nil
		    (car (delq nil
			       (mapcar
				(function
				 (lambda (arg)
				  (if (string-match
				       (concat "^" (regexp-quote event))
				       (prin1-to-string arg))
				      arg))) recog)))))
      (cond
	((and (string= "" event) off)
	 (setq irc-signals (mapcar 'list recog)))
	((string= "" event)
	 (setq irc-signals (mapcar
			    (function (lambda (arg) (list arg t))) recog)))
	((null sym)
	 (irc-insert "%%Signal: Unknown argument %s."
		     irc-msg-info-pre
		     event
		     irc-msg-info-post))
	(t (if off (setcdr (assoc sym irc-signals) nil)
	       (setcdr (assoc sym irc-signals) '(t))))))
    (setq on (delq nil
		   (mapcar        ; test against t because I have plans
		    (function     ; to couple users and events
		     (lambda (arg)
		      (if (eq (nth 1 (assoc arg irc-signals)) t)
			  arg))) recog)))
    (if on
	(irc-insert "%sSignalling is enabled for %s%s"
		    irc-msg-info-pre
		    (irc-subst-comma
		     (mapconcat 'prin1-to-string on ", ") "and")
		    irc-msg-info-post)
	(irc-insert "%sAll signalling is currently disabled%s"
		    irc-msg-info-pre
		    irc-msg-info-post)))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-stamp (stamp)
  "Usage: /STAMP [ + | - | [+]event | -event | interval ] [...]

Set time-stamping for IRC.  + means to turn it on for all messages from users
and - means to turn it off for them.  +event or just event will turn it on for
that class of message and -event means to disable it for those messages.  An
integer interval means to insert a message indicating the time every N minutes,
where N is the interval.  With no arguments simply insert a message indicating
the current time-stamps.

The current time in HH:MM format can appear two different ways in IRC.  One is
to have it associate with 'event'; two events, 'private' and 'public' messages,
are supported this way.  The other is to have it as a stand-alone message
indicating the current time.  Both can be very useful in noting when someone
actually sent you a message or when another event happened if you happen to be
away for a while.  The accuracy of the interval timer is currently limited to
0-2 minutes beyond the interval if display-time is not running; accuracy is
greatly improved if it is.  It can be turned off by setting the interval
to 0."
  (interactive "sSet time-stamp: ")
  ;; whee.  napalm would feel particularly good here.
  (setq stamp (irc-nuke-whitespace stamp))
  (let (str sym event off)
    (while (string< "" stamp)
      ;; as the args go marching one by one the last one stopped ... <ahem>
      (setq str   (substring stamp 0 (string-match " +\\|$" stamp))
	    stamp (substring stamp (match-end 0)))
      (string-match "^\\([---+]?\\)" str)
      (setq off (string= "-" (substring str (match-beginning 1) (match-end 1)))
	    event (substring str (match-end 1))
	    sym (cond ((string= "" event) nil)
		      ((string-match (concat "^" (regexp-quote event))
				     "private") 'private)
		      ((string-match (concat "^" (regexp-quote event))
				     "public")  'public)
		      ((natnump (car (read-from-string event)))
		       (car (read-from-string event)))))
      ;; the following cond is really what sets eveything
      (cond ((and (string= "" event) off) (setq irc-message-stamp nil))
	    ((string= "" event) (setq irc-message-stamp t))
	    ((null sym) (irc-insert "%%Stamp: Unknown argument %s."
				    irc-msg-info-pre
				    event
				    irc-msg-info-post))
	    ((natnump sym) (setq irc-time-stamp sym))
	    (off (setq irc-message-stamp
		       (car (delq sym (if (eq irc-message-stamp t)
					  '(private public)
					  (list irc-message-stamp))))))
	    (t (setq irc-message-stamp
		     (cond ((null irc-message-stamp) sym)
			   ((or (eq irc-message-stamp t)
				(eq irc-message-stamp sym)) irc-message-stamp)
			   (t t)))))))
  (irc-insert "%s%s messages get time-stamps.%s%s"
	      irc-msg-info-pre
	      (cond ((eq irc-message-stamp t) "Private and public")
		    ((null irc-message-stamp) "No")
		    (t (capitalize (prin1-to-string irc-message-stamp))))
	      (if (zerop irc-time-stamp) ""
		  (format "  The time interval is %d minutes."
			  irc-time-stamp))
	      irc-msg-info-post)
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-alias (alias)
  "Usage: /ALIAS alias [command] [args for command]]]

Allow 'alias' to be equivalent to 'command'.
For example, \"/ALIAS tf time tut.fi\" will make typing \"/tf\" be equivalent
to having issued the command \"/time tut.fi\".  Aliases can only be made
to existing commands, not other aliases.  They are also only recognized when
in the command name position of a line.  If given with no arguments then
all aliases are displayed; if given with just an alias name then the alias
with that name will be shown.  Aliases can be removed with /UNALIAS."
  (interactive "sWhat name should the new alias have? (RET to view all) ")
  (if (and (interactive-p) (string-match "[^: ]+" alias))
      (setq alias (concat
		   alias
		   " "
		   (irc-read-object
		    (format (concat "Alias \"%s\" to which command? "
				    "(Including optional arguments) ")
			    alias)
		    ""
		    (mapcar (function (lambda (pair)
			      (upcase (cdr pair))))
			    (append irc-alias-alist
				    (append irc-command-alist
					    irc-operator-alist)))))))
  (setq alias (irc-nuke-whitespace alias))
  (string-match "^/?\\([^: ]*\\) */?\\([^: ]*\\) *" alias)
  (let ((new (upcase (subfield alias 1)))
	(cmd (upcase (subfield alias 2)))
	(arg (irc-nuke-whitespace (substring alias (match-end 2))))
	match)
    (cond
      ((string= "" new)
       (let ((aliases irc-alias-alist))
	 (cond ((> (length aliases) 0)
		(while aliases
		  (irc-insert "%s/%s is aliased to mean \"/%s\"%s"
			      irc-msg-info-pre
			      (car (car aliases))
			      (cdr (car aliases))
			      irc-msg-info-post)
		  (setq aliases (cdr aliases)))
		(irc-insert "%sListed all aliases%s"
			    irc-msg-info-pre
			    irc-msg-info-post))
	       (t (irc-insert "%sYou don't have any aliases%s"
			      irc-msg-info-pre
			      irc-msg-info-post)))))
      ((string= "" cmd)
       (let ((alias (assoc new irc-alias-alist)))
	 (if alias
	     (irc-insert "%s/%s is aliased to mean \"/%s\"%s"
			 irc-msg-info-pre
			 (car alias)
			 (cdr alias)
			 irc-msg-info-post)
	     ;; this could possibly have done some matching to see whether
	     ;; just an abbrev was being given, but we'll just take it as given
	     (irc-insert "%s\"/%s\" is not aliased%s"
			 irc-msg-info-pre
			 new
			 irc-msg-info-post))))
      (t
       ;; Okay, we've got at least a command.  let's try and make this as
       ;; painless as possible. 
       (setq match (irc-check-list
		    (mapcar 'car (append irc-command-alist
					 irc-operator-alist))
		    cmd
		    'start-only))
       (if (/= (length match) 1)
	   (if match
	       (irc-insert "%%Ambiguous command /%s; could be %s."
			   cmd
			   (irc-subst-comma
			    (mapconcat (function (lambda (arg)
					 (concat "\"" arg "\"")))
				       match
				       ", ")
			    "or"))
	       (irc-insert "%%Command not found: /%s." cmd))
	   (irc-change-alias new
			     (concat (downcase (car match))
				     ;; no trailing space if no arg
				     (if (string= "" arg)
					 ""
					 " ")
				     arg)
			     'add)
	   (irc-insert "%s\"/%s\" has been aliased to mean \"/%s\"%s"
		       irc-msg-info-pre
		       new 
		       (cdr (assoc new irc-alias-alist))
		       irc-msg-info-post)))))
  (setq irc-idle-last-sent (irc-current-time)))


(defun irc-execute-unalias (alias)
  "Usage: /UNALIAS alias

Remove the 'alias' for a command."
  (interactive '(""))
  (let* ((tmpalias (if (and (string= "" alias) (not irc-called-from-buffer))
		       (irc-read-object "Remove which alias? "
					""
					(mapcar (function (lambda (p)
						  (upcase (car p))))
						(irc-recall-all
						 'irc-alias-alist)))
		       alias))
	 (a (irc-nuke-whitespace tmpalias)) 
	 (match (irc-check-list (mapcar 'car irc-alias-alist) a t)))
    (if (/= (length match) 1)
	(if match (irc-insert "%%%s is an ambiguous alias. Could be %s."
			      (upcase alias)
			      (irc-subst-comma
			       (mapconcat (function (lambda (arg)
					    (concat "\"" arg "\"")))
					  match
					  ", ")
			       "or"))
	    (irc-insert "%%No alias found to match %s." (upcase alias)))
	(irc-change-alias (car match) nil 'remove)
	(irc-insert "%s\"%s\" is no longer aliased%s"
		    irc-msg-info-pre (car match) irc-msg-info-post)))
  (setq irc-idle-last-sent (irc-current-time)))


;;START of code by Per Starbäck (starback@Student.DoCS.UU.SE)
(defun irc-execute-help (topic)
  "Usage: /HELP topic

Get the documentation for \"command\".  If no command is given then a list
of the possible topics is shown.  Note that commands for IRC Operators will
not appear in the help topics when not an IRC Operator."
  (interactive '(""))
  (let* ((normal-commands (mapcar (function car) irc-command-alist))
	 (oper-commands (mapcar (function car) irc-operator-alist))
	 (alias-commands (mapcar (function car) irc-alias-alist))
	 (help-topics (mapcar (function car) irc-help-topic-alist))
	 (top (irc-nuke-whitespace
	       (if (and (string= "" topic) (not irc-called-from-buffer))
		   (irc-read-object
		    "Help for which command or topic? "
		    ""
		    (append normal-commands
			    (if irc-operator oper-commands '())
			    alias-commands
			    help-topics))
		 topic))))
    (if (not irc-called-from-buffer)
	(progn (irc-insert "")
	       (irc-insert "/HELP %s" top)))
    (if (string= top "")
	(let* ((str (concat "Help is available for the following IRC-mode"
			    " commands:\n"))
	       (topics (sort (append normal-commands
				     (if irc-operator oper-commands nil)
				     alias-commands
				     help-topics)
			     (function string<))))
	  (while topics
	    (setq str (concat str
			      (format "\n%14s%14s%14s%14s%14s"
				      (nth 0 topics)
				      (or (nth 1 topics) "")
				      (or (nth 2 topics) "")
				      (or (nth 3 topics) "")
				      (or (nth 4 topics) "")))
		  topics (nthcdr 5 topics)))
	  (with-output-to-temp-buffer "*Help*" (princ str)))
      (let* ((matches (irc-check-list
		       (append normal-commands
			       (if irc-operator oper-commands '())
			       alias-commands
			       help-topics)
		       top t))
	     ;;(matches-normal (irc-common-element matches normal-commands))
	     (matches-oper (irc-common-element matches oper-commands))
	     (matches-alias (irc-common-element matches alias-commands))
	     (matches-topic (irc-common-element matches help-topics))
	     (shadowingalias nil))
	;; If there is an alias with the same name as a command,
	;; the alias is preferred.  Other collisions shouldn't be possible.
	(if (and (cdr matches)		;== (> (length matches) 1)
		 (irc-all-true (mapcar (function (lambda (x)
						   (string= x (car matches))))
				       (cdr matches))))
	    (if matches-alias
		(setq matches (list (car matches))
		      shadowingalias (concat
				      "\n\nThis shadows the "
				      (cond (matches-topic
					     "general help on the topic")
					    (matches-oper "operator command")
					    (t "command"))
				      " with the same name."))
	      (with-output-to-temp-buffer "*Help*"
		(princ (format (concat "There are several things called %s, "
				       "and no way to know \nwhich one you "
				       "want help on.  This is a bug--please "
				       "contact \n%s.")
			       (car matches) irc-hacker)))))
	;; If nothing else matches even a non-oper gets to see
	;; help on oper commands.
	(if (and (null matches) (not irc-operator))
	    (setq matches (irc-check-list oper-commands top t)
		  matches-oper matches))
	(cond ((null matches)
	       (irc-insert "%%No help is available for \"%s\"."
			   (upcase top)))
	      ((cdr matches)		;== (> (length matches) 1)
	       (irc-insert "%%Ambiguous help topic %s; could be %s."
			   (upcase top)
			   (irc-subst-comma
			    (mapconcat (function (lambda (arg)
						   (concat "\"" arg "\"")))
				       matches
				       ", ")
			    "or")))
	      (t ;;Exactly one match:
	       (let ((match (car matches)))
		 (with-output-to-temp-buffer "*Help*"
		   (cond (matches-alias
			  (let ((aliasfor (cdr (assoc match irc-alias-alist))))
			    (princ (format "%s is an alias for \"/%s\"."
					   match aliasfor))
			    (if shadowingalias (princ shadowingalias))
			    (princ (format "\n\n%s"
					   (irc-help-for-command
					    (upcase
					     (substring 
					      aliasfor 0
					      (string-match " "
							    aliasfor))))))))
			 (matches-topic
			  (princ (cdr (assoc match irc-help-topic-alist))))
			 (t (princ (irc-help-for-command match))))))))))))


(defun irc-help-for-command (command)
  "Returns the documentation string for the irc command COMMAND."
  (documentation
   (or (intern-soft (concat "irc-execute-"
			    (or (cdr (assoc command irc-command-alist))
				(cdr (assoc command irc-operator-alist)))))
       'irc-internal-error-dummy)))


(defun irc-common-element (list1 list2)
  "True if two lists have at least one common element.
The predicate eq is used for the comparisions."
  (cond ((null list1) nil)
	((memq (car list1) list2) t)
	(t (irc-common-element (cdr list1) list2))))


(defun irc-all-true (list)
  "Is true if all args in LIST are true."
  ;; This is needed as special forms (like AND) can't be given to FUNCALL.
  (if (null list) t (and (car list) (irc-all-true (cdr list)))))
;;;END of code by Per Starbäck.


(defun irc-later-execute-lusers ()
  "Dummy function. Used when it's desired to do a irc-execute-lusers in a
little while. Not implemented yet."
  nil)


(defun irc-internal-error-dummy ()
  "Internal error.

No internal function associated with this command. This should never happen.
Please report this to the person mentioned in variable irc-hacker."
  nil)



;; miscellaneous irc-* commands
(defun irc-truncate-buffer (max min)
  "Remove as many lines from the beginning of the buffer as is
necessary to get it under MAX number of characters, downto MIN number
of characters. This function is used by irc-mode to prevent an
irc-session from consuming gross amounts of space.

See irc-filter about not truncating the Kiwi buffer at all."
  (if (>= (buffer-size) max)
      (save-excursion
	;; first go to the lowest point posssible that would do it
	(goto-char min)
	;; get to the end of this line
	(end-of-line)
	(if (< (point) irc-mark)
	    ;; just to make sure we don't toast pending input
	    (delete-region 1 (1+ (point)))
	    (message "Warning: %s exceeding %s characters.  Couldn't truncate."
		     (buffer-name (current-buffer)) max)))))


(defun irc-read-passwd (&optional prompt)
  "Allow user to type a string without it showing.  Returns string.
If optional PROMPT non-nil, use it as the prompt string in the minibuffer."
  ;; this is based on a similar function in telnet.el
  ;; the major drawback is that while being prompted for a password
  ;; it stays in this routine until C-g, RET or LFD is typed.
  (let ((passwd "") (echo-keystrokes 0) char)
    (if prompt (message prompt))
    (while (not (or (= (setq char (read-char)) 13) (= char 10)))
      ;; naughty bit.  take C-h to mean DEL.
      (if (or (= char 8) (= char 127))
	  (if (> (length passwd) 0)
	      (setq passwd (substring passwd 0 (1- (length passwd)))))
	  (setq passwd (concat passwd (char-to-string char))))
      (if prompt (message (concat prompt (make-string (length passwd) ?*)))))
    (if prompt (message ""))
    passwd))


(defun irc-read-object (prompt object list)
  "Prompting with PROMPT, read an IRC objects name from the minibuffer.
Second argument OBJECT is a string which is checked for a non-ambiguous match
before the minibuffer read is done.  Optional third argument LIST is a
list to use for checking rather than the irc-nicknames.

It returns either the name of a object or an empty string (\"\")."
  (if (not (string-match "^ *\\([^: ]*\\)" object)) ; just want one name
      "" 
      (let ((completion-ignore-case t)
	    (object (substring object (match-beginning 1) (match-end 1)))
	    (match nil))
	(if (or (string= "" object)
		(/= (length (setq match (irc-check-list list object))) 1))
	    (completing-read (format "%s%s"
				     (if (string= "" object)
					 ""
					 (format (if (zerop (length match))
						     "No names match %s.  "
						     "\"%s\" is ambiguous.  ")
						 object))
				     prompt)
			     ;; build the list for completing-read.  a
			     ;; null string is there so that it can exit
			     ;; without anything, since we require matches
			     (mapcar 'list (cons "" list))
			     nil
			     nil		;Also non exact matches are OK.
			     object)
	    (car match)))))

(defun irc-nuke-whitespace (str)
  "One string argument.  Returns it with surrounding whitespace removed."
  (let* ((tmp (and (string-match "^ *" str)
		   (substring str (match-end 0)))))
    (if (not tmp)
	str
	(and (string-match " *$" tmp)
	     (substring tmp 0 (match-beginning 0))))))


(defun irc-stringlist-to-string (list &optional sep)
  "Take a LIST of strings and concate all string into one single string.
Optionally takes as a second argument a string to use as a seperator."
  (mapconcat (function (lambda (arg) arg)) list sep))


(defun irc-subst-comma (str newsep)
  "Return the string formed by substituting for the last \", \" in STR
the string NEWSEP followed by a space.  For example:
  (irc-subst-comma \"1, 2, 3\" \"or\") => \"1, 2 or 3\"

This function is especially designed for making message from irc-mode
more grammatically correct and the strings which it operates on should
be carefully chosen so as to avoid possibly blowing away a comma that
really wasn't separating elements in a list."
  ;; did you know that example up there can't appear starting in column 0
  ;; without screwing up lisp-indent-line?
  (if (string-match ", [^,]*$" str)
      (concat (substring str 0 (match-beginning 0)) " " newsep
	      (substring str (1+ (match-beginning 0))))
      str))


(defun irc-listify (list sep conn)
  "Take a LIST of strings, and put them together into one single string, using
the SEPerator string between every pair of string, except the last pair where
the CONNector is used.
Example: (irc-listify '(\"a\" \"b\" \"c\") \", \" \"and\") returns
\"a, b and c\"."
  (irc-subst-comma (irc-stringlist-to-string list sep) conn))


(defun irc-get-time ()
  "Return the hour and minutes of the current time in the form \"HH:MM\"."
  (let ((time (current-time-string)))
    (substring time
	       (string-match "[0-2][0-9]:[0-5][0-9]" time)
	       (match-end 0))))


;;;(defun irc-current-time ()
;;;  "Return current time as number of seconds since 1-jan-1970 0:00:00.
;;;As this is a 32 bit number but GNU Emacs only handles 16 bit numbers, split
;;;it up in a cons with the car being the high order 16 bit numer and th cdr
;;;the low order 16 bit number."
;;;  (if (= 0 (buffer-size))
;;;      (irc-insert ""))
;;;  (write-region (point-max) (1- (point-max)) irc-idle-scratch-file nil 'silent)
;;;  (nth 6 (file-attributes irc-idle-scratch-file)))


(defun irc-current-time ()
  "Return current time as number of seconds since 1-jan-1970 0:00:00.
As this is a 32 bit number but GNU Emacs only handles 16 bit numbers, split
it up in a cons with the car being the high order 16 bit numer and th cdr
the low order 16 bit number.

Written by Stephen Ma <ma_s@maths.su.oz.au>"
  (irc-time-to-int (current-time-string)))


(defun irc-time-to-int (timestr)
  "Convert from time in string format as returned by current-time-string
to a double integer format, as returned by file-attributes.

Written by Stephen Ma <ma_s@maths.su.oz.au>"
  (let* ((norm+ '(lambda (num1 num2)
		  (let ((sumh (+ (car num1) (car num2)))
			(suml (+ (car (cdr num1)) (car (cdr num2)))))
		    (list (+ sumh (/ suml 65536)) (% suml 65536)))))
	 (norm* '(lambda (num1 num2)
		  (let ((prodh (* num1 (car num2)))
			(prodl (* num1 (car (cdr num2)))))
		    (list (+ prodh (/ prodl 65536)) (% prodl 65536)))))
	 (seconds (string-to-int (substring timestr 17 19)))
	 (minutes (string-to-int (substring timestr 14 16)))
	 (hours (string-to-int (substring timestr 11 13)))
	 (partdays (1- (string-to-int (substring timestr 8 10))))
	 (years (string-to-int (substring timestr 20 24)))
	 (days (+ partdays
		  (cond ((and (= (% years 4) 0)
			      (/= (% years 100) 0))
			 (cdr (assoc (substring timestr 4 7)
				     '(("Jan" . 0)
				       ("Feb" . 31)
				       ("Mar" . 60)
				       ("Apr" . 91)
				       ("May" . 121)
				       ("Jun" . 152)
				       ("Jul" . 182)
				       ("Aug" . 213)
				       ("Sep" . 244)
				       ("Oct" . 274)
				       ("Nov" . 305)
				       ("Dec" . 335)))))
			(t (cdr (assoc (substring timestr 4 7)
				       '(("Jan" . 0)
					 ("Feb" . 31)
					 ("Mar" . 59)
					 ("Apr" . 90)
					 ("May" . 120)
					 ("Jun" . 151)
					 ("Jul" . 181)
					 ("Aug" . 212)
					 ("Sep" . 243)
					 ("Oct" . 273)
					 ("Nov" . 304)
					 ("Dec" . 334))))))
		  (* (- years 1970) 365)
		  (/ (- years 1969) 4)
		  (- (/ (- years 1901) 100)))))
    (funcall norm+
	     (funcall norm*
		      60
		      (funcall norm+
			       (funcall norm*
					60
					(funcall norm+
						 (funcall norm*
							  24
							  (list 0 days))
						 (list 0 hours)))
			       (list 0 minutes)))
	     (list 0 seconds))))


(defun irc-time= (a b)
  "Compare two time, return true if they're equal."
  (and (= (nth 0 a) (nth 0 b))
       (= (nth 1 ) (nth 1 b))))


(defun irc-time< (a b)
  "Compare two times, return t if the first is earlier than the second."
  (or (< (nth 0 a) (nth 0 b))
      (and (= (nth 0 a) (nth 0 b))
	   (< (nth 1 a) (nth 1 b)))))


(defun irc-time-diff (a b)
  "Return the difference between two times. This functions requires
the first argument to be earlier in time than the second argument."
  (cond ((= (nth 0 a) (nth 0 b)) (list 0 (- (nth 1 a) (nth 1  b))))
	((> (nth 1 b) (nth 1 a)) (list (- (nth 0 a) (nth 0 b) 1)
				       (- (+ 65536 (nth 1 a)) (nth 1 b))))
	(t (list (- (nth 0 a) (nth 0 b))
		 (- (nth 1 a) (nth 1 b))))))


(defun irc-idle-time ()
  "Return a approximation of the idle time. The time is the number of seconds
which have passed since the last write to the server. If a valid idle-time
can't be returned, -1 is returned instead."
  (let ((now (irc-current-time))
	(then irc-idle-last-sent))
    (if (or (numberp irc-idle-last-sent)
	    (not (= (car now) (car then))))
	-1
	(- (car (cdr now))
	   (car (cdr then))))))


(defun irc-internal-time ()
  "Return a new value every time irc-internal-time is called. The new value is
larger than the latest returned, starting at 0."
  (if (not (boundp 'irc-internal-time))
      (set (make-local-variable 'irc-internal-time) nil))
  (if (not (integerp irc-internal-time))
      (setq irc-internal-time 0)
      (setq irc-internal-time (1+ irc-internal-time))))


(defun irc-check-time ()
  "Check to see whether it is time to insert a current-time message into
the *IRC* buffer."
  (if (null irc-last-time)
      (setq irc-last-time 0))
  (let* ((time (irc-get-time))
	 (last (if (and (boundp 'irc-last-time) (stringp irc-last-time))
		   irc-last-time
		   time))
 	 (old-minute (string-to-int (substring last 3)))
 	 (new-minute (string-to-int (substring time 3)))
 	 (total-time (if (numberp irc-total-time) irc-total-time 0)))
    (if (and (zerop irc-time-stamp) (zerop irc-notify-interval))
 	()
	;; check the time sentinel
	(if (string= irc-last-time time)
	    ()
	    ;; time has gone stomping on by ...
	    (setq new-minute (+ new-minute (if (< new-minute old-minute) 60 0))
		  irc-last-time time
		  irc-total-time (+ total-time (- new-minute old-minute)))
	    (if (not (zerop irc-time-stamp))
		(if (not (< (- irc-total-time irc-last-stamp) irc-time-stamp))
		    (progn (irc-wrap-display-time)
			   (irc-send "TIME")
			   (setq irc-last-stamp irc-total-time))))
	    (if (not (zerop irc-notify-interval))
		(if (not (< (- irc-total-time irc-last-notify)
			    irc-notify-interval))
		    (progn (irc-wrap-display-time)
			   (irc-who-is-on
			    (irc-recall-all 'irc-notify-looked-for))
			   (setq irc-last-notify irc-total-time))))))))


(defun irc-wrap-display-time ()
  "Set up a wrapper around the display-time-filter to hopefully provide a
little better accuracy for the time stamps."
  (if (and (fboundp 'display-time-filter)
           (not (fboundp 'original-display-time-filter)))
      (progn
        (fset 'original-display-time-filter
              (symbol-function 'display-time-filter))
        ;; a nested defun seems to do funny things to the byte-compiler, so
        ;; instead we find a way around it.
        (fset 'display-time-filter
              (function
               (lambda (proc str)
		"
The filter for the display-time-process.  This function has been modified
for IRC-mode to call irc-check-time before calling the original
display-time-filter."
		(save-excursion
		  (let ((procs (irc-active-servers)))
		    (while procs
		      (let ((buf (buffer-name (process-buffer (car procs)))))
			(if buf (progn (set-buffer buf) (irc-check-time)))
			(setq procs (cdr procs))))))
		(original-display-time-filter proc str)))))))


(defun irc-who-is-on (&optional list)
  (if (or (not (boundp 'irc-last-who-is-on))
	  (not (and (listp irc-last-who-is-on)
		    (numberp (nth 0 irc-last-who-is-on))
		    (numberp (nth 1 irc-last-who-is-on)))))
      (set (make-local-variable 'irc-last-who-is-on) '(0 0)))
  (let* ((now (irc-current-time))
	 (diff (irc-time-diff now irc-last-who-is-on)))
    (cond ((or (not (= 0 (nth 0 diff)))
	       (> (nth 1 diff) 60))
	   (setq irc-last-who-is-on (irc-current-time))
	   (let ((str "")
		 (namelist (if (null list)
			       (irc-recall-all 'irc-notify-looked-for)
			       list)))
	     (while (not (null namelist))
	       (setq str (concat str " " (car namelist))
		     namelist (cdr namelist)))
	     (irc-send (format "ISON :%s" str)))))))


(defun irc-change-alias (alias cmd add)
  "Modify ALIAS for CMD in the irc-alias-alist.  ADD non-nil means to put the
alias in the list, nil (or the symbol \"remove\") means to clear it.  This
function does no hand-holding like /ALIAS; its intended use is in
irc-mode-hook."
  (let ((entry (assoc (upcase alias) irc-alias-alist)))
    (if (or (null add) (eq add 'remove))
        (setq irc-alias-alist (delq entry irc-alias-alist))
	(if entry (setcdr entry cmd)
	    (setq irc-alias-alist
		  (cons (cons (upcase alias) cmd) irc-alias-alist))))))


(defun irc-signal (user event)
  "Return t if a ding should be issued for a USER/EVENT pair.
Currently only the event part of things is supported by /SIGNAL."
  (let ((signal (cdr (assoc event irc-signals))))
    (or (memq t signal)
	(irc-member-general user signal 'string=)
        (irc-member-general user (cdr (assoc 'user irc-signals)) 'string=))))


(defun irc-check-list (list item &optional start-only)
  "See if LIST has string ITEM.  Returns a list of possible matches.  The list
returned is based on the following precedence rules:  if there is an exact
match, it is returned.  If there are any strings in the list whose beginning
match the item, they are returned.  If that fails and optional argument
START-ONLY is missing or nil, strings which have the item match anywhere are
returned.  As a last resort, nil is returned.
This function is not case-sensitive."
  (let ((return nil)
	(case-fold-search t)
	(item (regexp-quote item)))
    (if (setq return
              (delq nil                         ; whole words
                    (mapcar (function   
                             (lambda (arg)
			      (if (string-match (concat "^" item "$") arg)
				  arg))) list)))
        return
	(if (setq return
		  (delq nil                       ; beginnings
			(mapcar (function
				 (lambda (arg)
				  (if (string-match (concat "^" item) arg)
				      arg))) list)))
	    return
	    (if start-only
		nil
		(delq nil
		      (mapcar (function               ; anywhere
			       (lambda (arg)        
				(if (string-match (concat "." item) arg) arg)))
			      list)))))))


(defun irc-list-remember (item list)
  "Add string ITEM to ordered LIST destructivly, returning the new list in
reversed order. The intended way to call this is like:
  (setq lst (irc-list-remember \"foo\" lst)).

This function is case insensitive."
  (let ((ui (upcase item)))
    (cond ((null list) (cons item nil))
	  ((string< (upcase (car list)) ui) (cons item list))
	  ((string= (upcase (car list)) ui)
	   (rplaca list item)
	   list)
	  (t (let ((ptr list))
	       (while (and (not (null (cdr ptr)))
			   (string< ui (upcase (car (cdr ptr)))))
		 (setq ptr (cdr ptr)))
	       (cond ((null (cdr ptr)) (rplacd ptr (cons item nil)))
		     ((string= (upcase (car (cdr ptr))) ui)
		      (rplaca (cdr ptr) item))
		     ((string< (upcase (car (cdr ptr))) ui)
		      (rplacd ptr (cons item (cdr ptr))))
		     (t (error
			 (format "NOT possible! item=%s, list=%s, ptr=%s."
				 item list ptr)))))
	     list))))
	     
	     
(defun irc-hash-value (str tbl-size)
  "Return a hash value (index into a hash table) for string ITEM according to
a table of size SIZE."
  (let ((h 0)
	(i 0)
	(l (min 4 (length str))))
    (while (< i l)
      (setq h (% (+ (* h 256) (upcase (aref str i))) tbl-size)
	    i (1+ i)))
    h))


(defconst irc-hash-index-size 0
  "Index of size field in a Kiwi hash table.")
(defconst irc-hash-index-timestamp 1
  "Index of timestamp field in a Kiwi hash table.")
(defconst irc-hash-index-cleanflag 2
  "Index of cleanstamp in a Kiwi hash table. Either a symbol or a list.")
(defconst irc-hash-index-bucketarray 3
  "Index of bucketarray in a Kiwi hash table.")


(defun irc-create-new-hash-table (size)
  "Create a empty hash table of size SIZE." 
  ;; [size write-date cleanflag bucketarray]
  ;; cleanflag is a list = non-dirty, valid sorted list representation of
  ;; the hashed data. Else only data to be found is in the hashtable. The hash
  ;; table is always clean.
  (let ((htbl (make-vector 4 nil)))
    (aset htbl irc-hash-index-size size)
    (aset htbl irc-hash-index-timestamp (irc-internal-time))
    (aset htbl irc-hash-index-cleanflag 'empty)
    (aset htbl irc-hash-index-bucketarray (make-vector size nil))
    htbl))


(defun irc-nothing-remembered-p (bag)
  "True if the BAG is empty."
  (let* ((htbl (symbol-value bag))
	 (flg (aref htbl irc-hash-index-cleanflag)))
    (cond ((and (symbolp flg) (eq 'empty flg)) flg)
	  (t (let* ((a (aref htbl irc-hash-index-bucketarray))
		    (size (aref htbl irc-hash-index-size))
		    (i size)
		    (empty t))
	       (while (and empty (> i 0))
		 (setq i (1- i)
		       empty (null (aref a i))))
	       (if empty
		   (irc-forget-all bag))
	       empty)))))


(defun irc-remember (item bag)
  "Store a string ITEM in the named BAG."
  (let* ((htbl (symbol-value bag))
	 (a (aref htbl irc-hash-index-bucketarray))
	 (fixed (cond ((eq bag 'irc-servernames)
		       (let ((i (irc-extract-hostname (upcase item))))
			 (cond ((not i) nil)
			       ((string-match "^ *$" i)
				(irc-insert (concat "%%Function irc-remember"
						    " found a space in"
						    " a hostname %s).")
					    item)
				(if debug-on-error
				    (error "SPC in hostname"))
				nil)
			       (t i))))
		      ((or (eq bag 'irc-linksinfo)
			   (eq bag 'irc-namtree)
			   (eq bag 'irc-whotree)
			   (eq bag 'irc-listtree))
		       item)
		      ((or (eq bag 'irc-nicknames)
			   (eq bag 'irc-notify-detected)
			   (eq bag 'irc-notify-looked-for))
		       (cond ((string= "" item)
			      (irc-insert "%%Skipped \"\" in irc-nicknames.")
			      nil)
			     ((= ?@ (aref item 0))
			      (substring item 1))
			     ((string-match " " item)
			      (irc-insert (concat "%%Function irc-remember"
						  " found a space in a"
						  " nickname (%s).")
					  item)
			      (if debug-on-error
				  (error "SPC in nickname"))
			      nil)
			     ((string-match "\\." item)
			      (irc-insert (concat "%%Function irc-remember"
						  " found a dot in a"
						  " nickname (%s).")
					  item)
			      (if debug-on-error
				  (error "Dot in nickname"))
			      nil)
			     (t item)))
		      ((string-match " " item)
		       (irc-insert (concat "%%Function irc-remember"
					   " found a space in an item"
					   " (%s).")
				   item)
		       (if debug-on-error
			   (error "SPC in item"))
		       nil)
		      (t item))))
    (cond (fixed
	   (let* ((idx (irc-hash-value fixed (aref htbl irc-hash-index-size)))
		  (oldlen (length (aref a idx))))
	     (aset a idx (irc-list-remember fixed (aref a idx)))
	     (cond ((not (= oldlen (length (aref a idx))))
		    (aset htbl irc-hash-index-timestamp (irc-internal-time))
		    (aset htbl irc-hash-index-cleanflag 'dirty)))
	     (if (and (eq bag 'irc-nicknames)
		      (not (irc-recall fixed 'irc-notify-detected))
		      (irc-recall fixed 'irc-notify-looked-for))
		 (irc-who-is-on (irc-recall-all 'irc-notify-looked-for))))))))


(defun irc-debug-check-all-hashtables ()
  ""
  (let ((n (irc-internal-time)))
    (setq irc-userinfo (format "DEBUGGING CLIENT -- %s." n))
    (mapcar (function (lambda (p)
	      (irc-debug-check-hashtable (car p) (cdr p) n)))
	    '((irc-nicknames . irc-is-nickname)
	      (irc-ignored-ppl . irc-is-nickname)
	      (irc-linksinfo . nil)
	      (irc-listtree . nil)
	      (irc-notify-looked-for . irc-is-nickname)
	      (irc-notify-detected . irc-is-nickname)
	      (irc-namtree . nil)
	      (irc-servernames . irc-is-hostname)
	      (irc-subscribed-channels . irc-is-channelname)
	      (irc-services . irc-is-nickname)
	      (irc-whotree . nil)))))


(defun irc-debug-check-hashtable (bag chkfcn &optional mark)
  ""
  (let* ((data (irc-recall-all bag))
	 (lst data)
	 (id (if (null mark) "" mark))
	 (consistent t)
	 (doublettes nil))
    (sit-for 0)
    (irc-insert "DEBUG: %s checking if hashtable %s is consistent using %s."
		id bag chkfcn)
    (irc-insert "DEBUG: %s = %s." bag data)
    (while (not (null lst))
      (let* ((matches (irc-check-list data (car lst)))
	     (l (length matches)))
	(cond ((= 1 l) (irc-insert "DEBUG: %s  OK, \"%s\" found once."
				   id (car lst)))
	      ((= 0 l)
	       (irc-insert "DEBUG: %s Huh? \"%s\" not found."
			   id (car lst))
	       (setq doublettes t))
	      (t (irc-insert "DEBUG: %s ERROR \"%s\" FOUND %d TIMES."
			     id (car lst) l)
		 (setq doublettes t)))
	(setq lst (cdr lst))))
    (irc-insert "DEBUG: %s checked for doublettes, done." id)
    (sit-for 0)
    (if (not (null chkfcn))
	(let ((lst data))
	  (while (not (null lst))
	    (cond ((funcall chkfcn (car lst))
		   (irc-insert "DEBUG: %s item \"%s\" of %s -- OK."
			       id (car lst) bag))
		  (t (irc-insert "DEBUG: %s item \"%s\" of %s FAILED CHECK."
				 id (car lst) bag)
		     (setq consistent nil)))
	    (setq lst (cdr lst)))))
    (irc-insert "DEBUG: %s done checking: doublettes %s, consistent %s"
		id (if doublettes "FAILED" "OK") (if consistent "OK" "FAILED"))
    (irc-insert "DEBUG: %s --------------" id)
    (sit-for 0)))


(defun irc-list-recall (item list)
  "Check if a string ITEM is in the LIST. The comparsion is not case
sensitive. If the item is found, return the stored spelling, else nil."
  (let ((ui (upcase item)))
    (while (and (not (null list))
		(string< ui (upcase (car list))))
      (setq list (cdr list)))
    (if (and (not (null list)) (string= (upcase (car list)) ui))
	(car list)
	nil)))


(defun irc-recall (item bag)
  "Check if a string ITEM is in the BAG. If so, return the stored spelling,
else ni. All checking is done without being case sensitive."
  (let* ((htbl (symbol-value bag))
	 (idx (irc-hash-value item (aref htbl irc-hash-index-size))))
    (irc-list-recall item (aref (aref htbl irc-hash-index-bucketarray)
		(irc-hash-value item (aref htbl irc-hash-index-size))))))


(defun irc-recall-all (bag)
  "Return a sorted list representation of all strings in the BAG."
  ;; [size write-date cleanflag bucketarray]
  (let* ((htbl (symbol-value bag))
	 (cached-data (aref htbl irc-hash-index-cleanflag))
	 (is-clean (listp cached-data))
	 (buckets (aref htbl irc-hash-index-bucketarray))
	 (size (aref htbl irc-hash-index-size)))
    (cond ((not (= size (length buckets)))
	   (irc-insert "%%Bad hash list %s, size != length bucketarray." bag)))
    (cond (is-clean cached-data)
	  (t (let ((r nil)
		   (i size)
		   (a nil))
	       (while (> i 0)
		 (setq i (1- i)
		       a (aref buckets i))
		 (while (not (null a))
		   (setq r (cons (car a) r)
			 a (cdr a))))
	       (let ((s (sort r '(lambda (a b)
				  (string< (upcase a) (upcase b))))))
		 (aset htbl irc-hash-index-cleanflag s)
		 s))))))


(defun irc-recall-all-and-display (bag ind &optional plur sing)
  "Enter the nodes in the BAG as seperate lines in the irc buffer. Entries
longer than 1 line are continued with an indentation of IND, a number.

If you supply the optional arguments PLUR and SING, then after the lines
a short message of the form \"[%d %s]\" is printed. If only PLUR is supplied,
it is always used. If both PLUS and SING is supplied, then SING is used if
exactly one line was printed, else PLUR is used."
  (let ((lst (irc-recall-all bag))
	(n 0)
	(irc-msg-cont-used (make-string ind ? )))
    (while lst
      (irc-insert "%s" (car lst))
      (setq n (1+ n)
	    lst (cdr lst)))
    (if (stringp plur)
	(if (zerop n)			;List empty?
	    (if (irc-terminal-is-slow) ;When on a slow terminal, no list
		(irc-insert "%%No %s." plur) ; is kept.
		(irc-insert "%sEnd of (unsorted) %s list%s"
			    irc-msg-info-pre
			    plur
			    irc-msg-info-post))
	    (irc-insert "%s%d %s%s"
			irc-msg-info-pre
			n
			(if (= n 1)
			    (if (stringp sing) sing plur)
			    plur)
			irc-msg-info-post))
	(irc-insert "%sEnd of (unsorted) list%s"
		    irc-msg-info-pre
		    irc-msg-info-post)))
  (irc-insert ""))


(defun irc-list-forget (item list)
  "Remove a string ITEM from a LIST of string, if it's found. The comparsions
are not case sensitive."
  (let ((ui (upcase item)))
    (cond ((null list) list)
	  ((string< (upcase (car list)) ui) list)
	  ((string= (upcase (car list)) ui) (cdr list))
	  (t (let ((ptr list))
	       (while (and (not (null (cdr ptr)))
			   (string< ui (upcase (car (cdr ptr)))))
		 (setq ptr (cdr ptr)))
	       (cond ((null (cdr ptr)) nil)
		     ((string< (upcase (car (cdr ptr))) ui) nil)
		     ((string= (upcase (car (cdr ptr))) ui)
		      (rplacd ptr (cdr (cdr ptr))))
		     (t (error (concat  "irc-list-forget: NOT possible!"
					" item=%s, list=%s, ptr=%s.")
			       item list ptr))))
	     list))))


(defun irc-forget (item bag)
  "Remove a string ITEM from the BAG, if the item was there. Not case
sensitive."
  (let* ((htbl (symbol-value bag))
	 (idx (irc-hash-value item (aref htbl irc-hash-index-size)))
	 (a (aref htbl irc-hash-index-bucketarray))
	 (oldlen (length (aref a idx))))
    (aset a idx (irc-list-forget item (aref a idx)))
    (if (and (eq bag 'irc-nicknames)
	     (irc-recall item 'irc-notify-detected)
	     (irc-recall item 'irc-notify-looked-for))
	(irc-who-is-on (irc-recall-all 'irc-notify-looked-for)))
    (cond ((not (= oldlen (length (aref a idx))))
	   (aset htbl irc-hash-index-timestamp (irc-internal-time))
	   (aset htbl irc-hash-index-cleanflag 'dirty)))))


(defun irc-forget-all (bag)
  "Empty BAG."
  (let* ((htbl (symbol-value bag))
	 (size (aref htbl irc-hash-index-size))
	 (a (aref htbl irc-hash-index-bucketarray))
	 (i 0))
    (while (< i size)
      (aset a i nil)
      (setq i (1+ i)))
    (aset htbl irc-hash-index-timestamp (irc-internal-time))
    (aset htbl irc-hash-index-cleanflag 'empty)))


(defun irc-get-names-and-servers ()
  "Return a alphabetically sorted list of all the nick- and servernames which
are known by the client."
  (if (not (boundp 'irc-cache-n+s))
      (set (make-local-variable 'irc-cache-n+s) nil))
  (if (or (null irc-cache-n+s)		;No data yet?
	  (< (car irc-cache-n+s)	;Nicknames updated?
	     (aref irc-nicknames irc-hash-index-timestamp))
	  (< (car irc-cache-n+s)	;Servernames updated?
	     (aref irc-servernames irc-hash-index-timestamp)))
      (let* ((servers (irc-recall-all 'irc-servernames))
	     (names (irc-recall-all 'irc-nicknames))
	     (data nil))
	(while (not (null servers))
	  (setq data (cons (car servers) data)
		servers (cdr servers)))
	(while (not (null names))
	  (setq data (cons (car names) data)
		names (cdr names)))
	(let ((s (sort data '(lambda (a b) (string< (upcase a) (upcase b))))))
	  (setq irc-cache-n+s (cons (irc-internal-time) s)))))
  (cdr irc-cache-n+s))


(defun irc-get-channels-and-nicks-and-servers ()
  "Return a alphabetically sorted list of all the channel-, nick- and server-
names which are known by the client."
  (if (not (boundp 'irc-cache-c+n+s))
      (set (make-local-variable 'irc-cache-c+n+s) nil))
  (if (or (null irc-cache-c+n+s)
	  (< (car irc-cache-c+n+s)
	     (aref irc-subscribed-channels irc-hash-index-timestamp))
	  (< (car irc-cache-c+n+s)
	     (aref irc-nicknames irc-hash-index-timestamp))
	  (< (car irc-cache-c+n+s)
	     (aref irc-servernames irc-hash-index-timestamp)))
      (let* ((channels (irc-recall-all 'irc-subscribed-channels))
	     (nicks (irc-recall-all 'irc-nicknames))
	     (servers (irc-recall-all 'irc-servernames))
	     (data nil))
	(while (not (null channels))
	  (setq data (cons (car channels) data)
		channels (cdr channels)))
	(while (not (null nicks))
	  (setq data (cons (car nicks) data)
		nicks (cdr nicks)))
	(while (not (null servers))
	  (setq data (cons (car servers) data)
		servers (cdr servers)))
	(let ((s (sort data '(lambda (a b) (string< (upcase a) (upcase b))))))
	  (setq irc-cache-c+n+s (cons (irc-internal-time) s)))))
  (cdr irc-cache-c+n+s))


(defun irc-sec-to-time (n)
  "Convert number of SECONDS into a time string of one of the following
formats \"SS seconds\" or \"MM minutes and SS seconds\"
or \"HH hours, MM minutes and SS seconds\"
or \"DD days, HH hourse, MM minutes and SS seconds\"."
  (let* ((one-minute 60)
	 (one-hour (* 60 one-minute))
	 (one-day (* 24 one-hour))
	 (days (/ n one-day))
	 (day-string (cond ((= 0 days) "")
			   ((= 1 days) "1 day")
			   (t (format "%d days" days))))
	 (m (% n one-day))
	 (hours (/ m one-hour))
	 (hour-string (cond ((= 0 hours) "")
			    ((= 1 hours) "1 hour")
			    (t (format "%d hours" hours))))
	 (l (% m one-hour))
	 (minutes (/ l one-minute))
	 (min-string (cond ((= 0 minutes) "")
			   ((= 1 minutes) "1 minute")
			   (t (format "%d minutes" minutes))))
	 (seconds (% l one-minute))
	 (sec-string (cond ((= 0 seconds) "")
			   ((= 1 seconds) "1 second")
			   (t (format "%d seconds" seconds)))))
    ;; Possible combinations:
    ;; D H M S
    ;; 0 0 0 0 = none
    ;; 0 0 0 1 = S
    ;; 0 0 1 0 = M
    ;; 0 0 1 1 = MS
    ;; 0 1 0 0 = H
    ;; 0 1 0 1 = HS
    ;; 0 1 1 0 = HM
    ;; 0 1 1 1 = HMS
    ;; 1 0 0 0 = D
    ;; 1 0 0 1 = DS
    ;; 1 0 1 0 = DM
    (let ((acc nil))
      (if (not (string= "" sec-string)) (setq acc (cons sec-string acc)))
      (if (not (string= "" min-string)) (setq acc (cons min-string acc)))
      (if (not (string= "" hour-string)) (setq acc (cons hour-string acc)))
      (if (not (string= "" day-string)) (setq acc (cons day-string acc)))
      (cond ((= 0 (length acc)) "0 seconds")
	    ((= 1 (length acc)) (car acc))
	    ((= 2 (length acc)) (format "%s and %s" (nth 0 acc) (nth 1 acc)))
	    ((= 3 (length acc))
	     (format "%s, %s and %s" (nth 0 acc) (nth 1 acc) (nth 2 acc)))
	    (t (format "%s, %s, %s and %s"
		       (nth 0 acc) (nth 1 acc) (nth 2 acc) (nth 3 acc)))))))


(defun irc-burst-comma (str)
  "Take a comma or space separated STR and turn it into a list of its elements.
Example: \"1, 2,3,4,  6  7\" becomes the list
(\"7\" \"6\" \"4\" \"3\" \"2\" \"1\")."
  (let (list sub (beg 0))
    (string-match "" str)
    (while (string-match ",+\\| +\\|,+ +" str beg)
      (if (not (string= (setq sub (substring str beg (match-beginning 0))) ""))
          (setq list (cons sub list)))
      (setq beg (match-end 0)))
    (if (/= (length str) beg) (cons (substring str beg) list) list)))



;; miscellaneous other commands (usually from other sources)

;; this makes up for not being able to provide a :test to memq.
;; irc-member-general by Bard Bloom <bard@theory.lcs.mit.com>
(defun irc-member-general (x l comparison)
  "Is X a member of L under COMPARISON?"
  (let ((not-found t))
    (while (and l not-found)
      (setq not-found (not (funcall comparison x (car l)))
            l         (cdr-safe l)))
    (not not-found)))


;; wish i could remember who I got this from; I had to patch it to work
;; with the minibuffer correctly but it is mostly untouched.
(defun irc-walk-windows (proc &optional no-mini)
  "Applies PROC to each visible window (after selecting it, for convenience).
Optional arg NO-MINI non-nil means don't apply PROC to the minibuffer
even if it is active."
  (let* ((real-start (selected-window))
	 (start (next-window real-start no-mini))
	 (current start) done)
    (while (not done)
      (select-window current)
      (funcall proc)
      (setq current (next-window current no-mini))
      (setq done (eq current start)))
    (select-window real-start)))


(defun irc-count-windows (&optional no-mini)
  "Returns the number of visible windows.
Optional arg NO-MINI non-nil means don't count the minibuffer
even if it is active."
  (let ((count 0))
    (irc-walk-windows (function (lambda () (setq count (1+ count)))) no-mini)
    count))


;; swiped from minibuf.el, but made exclusive to * Minibuf-n*.
(defun irc-minibuffer-message (format &rest args)
  "Print a temporary message at the end of the Minibuffer.
After 2 seconds or when a key is typed, erase it."
  (if (zerop (minibuffer-depth)) (apply 'message format args)
      (let (p)
	(save-excursion
	  (set-buffer (concat " *Minibuf-" (1- (minibuffer-depth)) "*"))
	  (unwind-protect
	       (progn
		 (setq p (goto-char (point-max)))
		 (insert (apply 'format format args))
		 (sit-for 2))
	    (delete-region p (point-max)))))))


(defun irc-find-to (str &optional explicit)
  "Find the part of STRING that IRC-mode will interpret as the sendlist.
If no explicit list is found, irc-default-to is returned.  The string returned
is either : or ; terminated.

If optional EXPLICIT is non-nil, then return t if a sendlist was explicitly
specified, nil if the sendlist was implicit."
  (let* ((part (if (string-match "^ *\\([^;:]*\\) *\\([:;]\\)" str)
		   (subfield str 1)
		   nil))
	 (retval (if (not part)
		     ""
		     (concat part (subfield str 2))))
	 (matched (and part
		       (or (string= "" part)
			   (let ((s part))
			     (while (string-match (concat "^ *\\([^ ,]+\\)"
							  " *, *")
						  s)
			       (let ((b1 (match-beginning 1))
				     (e0 (match-end 0))
				     (e1 (match-end 1)))
				 (if (irc-is-receiver (substring s b1 e1))
				     (setq s (substring s e0))
				     (setq s ":"))))
			     (irc-is-receiver s))))))
    (if explicit matched (if matched retval irc-default-to))))


(defun irc-find-message (string)
  "Find the message that IRC will see if STR were sent.  For messages
sent with explicit lists, this is everything following the colon or
semi-colon.  For everything else, it is just the string."
  (substring string (length (irc-find-to string))))



;; functions for the irc-history list
(defun irc-add-to-hist (str)
  "Put STRING at the head of the irc-history list."
  (if (string-match "^[;:]" str)
      (setq str
            (concat irc-last-explicit (substring str 1 (length str)))))
  (setq irc-history (append (list str) irc-history))
  (and (> (length irc-history) irc-max-history)
       (setq irc-history (reverse (cdr (reverse irc-history))))))


(defun irc-yank-prev-command ()
  "Put the last IRC /command in the input-region."
  (interactive)
  (delete-region irc-mark (goto-char (point-max)))
  (insert "/" irc-last-command)
  (goto-char (1+ irc-mark)))


(defun irc-history-prev (arg)
  "Select the previous message in the IRC history list.  ARG means
select that message out of the list (0 is the first)."
  (interactive "P")
  (let ((str (nth (or arg (1+ (or irc-history-index 0))) irc-history)))
    (if (not str)
        (message "No message %d in history." (or arg (1+ irc-history-index)))
	(delete-region irc-mark (goto-char (point-max)))
	(insert str)
	(goto-char irc-mark)
	(setq irc-history-index (or arg (1+ irc-history-index))))))


(defun irc-history-next (arg)
  "Select the next message in the IRC history list.  With prefix ARG
select that message out of the list (same as irc-history-prev if
called with a prefix arg)."
  (interactive "P")
  (if arg (irc-history-prev arg)
      (if (= irc-history-index -1)
	  (message "No next message in history.")
	  (delete-region irc-mark (goto-char (point-max)))
	  (insert (if (zerop irc-history-index) ""
		      (nth (1- irc-history-index) irc-history)))
	  (setq irc-history-index (1- irc-history-index)))))


(defun irc-kill-input ()
  "Delete the input region and start out fresh.  This function is recommended
over any other way of killing the input-region interactively because it
also resets the index for the history list."
  (interactive)
  (delete-region irc-mark (goto-char (point-max)))
  (setq irc-history-index -1))


(defun irc-complete-name ()
  "Not completed yet."
  (interactive)
  (let* ((end (point))
	 (beg (save-excursion
		(while
		    (and (not (= (point) (point-min)))
			 (irc-is-receiver (buffer-substring (1- (point)) end)))
		  (backward-char 1))
		(point)))
	 (pattern (downcase (buffer-substring beg end)))
	 (alist (mapcar (function (lambda (s)
			  (list (downcase s))))
			(irc-get-names-and-servers)))
	 (completion (try-completion pattern alist)))
    (cond ((eq completion t))		;Exact match.
	  ((null completion)		;No match at all.
	   (ding)
	   (message "Can't find completion for \"%s\"" pattern))
	  ((not (string= pattern completion)) ;Complete (but maybe not unique).
	   (delete-region beg end)
	   (insert completion)) 
	  (t (let* ((name (irc-nuke-whitespace
			   (completing-read "Who? "
					    alist
					    nil
					    nil
					    pattern))))
	       (delete-region beg end)
	       (insert name))))))


(defun irc-line-count (buf)
  "Returns the size of the buffer BUF."
  (save-excursion (set-buffer buf)
		  (goto-line 1)
		  (count-lines (point-min) (1+ (buffer-size)))))


(defun irc-log-in-debug-buffer (line)
  "Append a LINE to the debug buffer associated with a session, appending a
newline to the line. If the debug buffer doesn't exist, do nothing."
  (let ((debug-buffer (concat (buffer-name (current-buffer))
			      "-*debug*")))
    (if (get-buffer debug-buffer)
	(irc-append-string-to-buffer line (get-buffer debug-buffer)))))


(defun irc-append-string-to-buffer (str buf)
  "Append the string STR to the buffer BUF as a line."
  (save-excursion (set-buffer buf)
		  (goto-char (1+ (buffer-size)))
		  (insert str "\n")))


(defun irc-get-buffers-nth-line (n buf)
  "Returns the line number N in the buffer BUF as a string.
The first line inb the buffer is line number 1.
If the buffer is empty, returns \"\"."
  (if (>= (irc-line-count buf) n)
      (save-excursion (set-buffer buf)
		      (goto-line n)
		      (let ((p (point)))
			(search-forward "\n" (1+ (buffer-size)) 'non-nil-non-t)
			(buffer-substring p (1- (point)))))
	""))


(defun irc-sort-lines-in-buffer (buf &optional reverse)
  "Sort the lines in buffer BUF.
An optional argument REVERSE can be supplied as non-nil to sort the buffer
in reversed order."
  (save-excursion (set-buffer buf)
		  (sort-lines reverse (point-min) (1+ (buffer-size)))))


(defun irc-history-menu ()
  "List the history of messages kept by irc-mode in another buffer."
  (interactive)
  (let ((pop-up-windows t) (hist irc-history) (line 0))
    (save-excursion
      (set-buffer (get-buffer-create "*IRC History*"))
      (fundamental-mode)
      (erase-buffer)
      (while hist
        (insert (format "%2d: %s\n" line (car hist)))
        (setq hist (cdr hist))
        (setq line (1+ line)))
      (if (zerop line)
          (insert "No messages have been sent to IRC yet."))
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    (display-buffer "*IRC History*")))



;; stuff about irc-mode
(defun irc-version (&optional arg)
  "Print the current version of irc.el in the minibuffer.  With optional
ARG, insert it in the current buffer."
  (interactive "P")
  (if arg
      (insert irc-version)
      (princ irc-version)))


(defun irc-fatal (msg item)
  (irc-insert "FATAL ERROR: \"%s\" \"%s\"." msg item))


;;; Force GC to prevent GC bug in older (pre 18.57) GNU Emacs'es.
(garbage-collect)

(defun irc-execute-news (dummy)
  "Shows news about latest changes to this GNU Emacs client.
Even shows news about old changes -- what a wonderous function indeed.

Latest changes to IRC mode, oldest at bottom, newest at top:

*** NEEDS TO BE UPDATED ***"
  (interactive '(""))
  (save-excursion
    (set-buffer (get-buffer-create "*IRC-mode News*"))
    (erase-buffer)
    (insert (documentation 'irc-execute-news))
    (goto-char (point-min)))
  (display-buffer "*IRC-mode News*"))

