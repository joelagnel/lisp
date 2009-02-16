;;; Saved through ges-version 0.3.3dev at 2004-05-14 13:29
;;; ;;; From: Stephen Farrell <steve@farrell.org>
;;; ;;; Subject: Announce: susync
;;; ;;; Newsgroups: gnu.emacs.sources
;;; ;;; Date: Fri, 27 Feb 2004 08:18:21 -0800
;;; ;;; Organization: IBM Almaden Research Center

;;; [1. text/plain]

;;; Hi,

;;; This is to announce an experimental (pre-alpha) synchronization suite
;;; for emacs.  I wanted something that was written in elisp, wasn't
;;; specific to the palm pilot, and could be adapted easily to work with
;;; different emacs productivity applications.  I couldn't find it, so I
;;; have written it.

;;; I plan to write the calendary/diary client next.

;;;  >>>>This is experimental software.  Please back up your palm pilot,
;;; ~/.bbdb, and the rest of your computer while you're at it before
;;; trying it out!  I'm trying to "release early, release often".<<<<<

;;; (Oh, damn, this code is xemacs-lisp.  Simple matter to make emacs
;;; compatible... will do so ASAP).

;;; Here's the comments:

;;; susync.el --- An extensible synchronization suite for emacs
;;;
;;; Copyright (c) 2004 Stephen Farrell <steve@farrell.org> "smackman"
;;;
;;; Distributed under MPL-1.1 (Mozilla Public License), NO WARRANTY
;;
;; About susync:
;;
;; susync is an extensible synchronization suite for emacs
;; productivity apps.  For example, it might be used to keep your BBDB
;; and palm address book up-to-date.  It is very flexible, however, so
;; with the right client implementations it could sync your IMAP
;; mailbox and your Zaurus PDA (no emacs apps), or to sync your
;; calendar with minesweeper!
;;
;; The sync algorithm works very much like CVS.  The "checkpoint"
;; serves much like the CVS repository, and each client generates
;; deltas against the checkpoint.  The deltas are then applied both
;; ways, and then a new checkpoint is created.  Conflicts are handled
;; by the client implementation.
;;
;; Using eieio, plugin synchronization clients can be easily written
;; for different applications by overriding a few (e.g., 2) methods.
;; A tricky bit is assigning a unique key to each record.  Susync was
;; designed to work *without* having to embed identifiers in records,
;; however the client implementor can use such identifiers if he or
;; she chooses.
;;
;; The BBDB client uses either lastname+firstname, company, or email
;; address as the identifier.  This works most of the time, but if you
;; change the spelling of someone's name, e.g., then it will seem like
;; a record was deleted and a record was created.  If there was extra,
;; non-synchronized information in the record, then this is lost.
;; Some might consider this a bug.  A work around is to set a
;; "deleted" flag in BBDB, and the user could merge the records at
;; some later date.
;;
;; About clients:
;;
;; Susync ships with three clients:
;;
;; * BBDB: BBDB client that does not embed identifiers in BBDB records.
;; * PilotSync: http://www.osk.3web.ne.jp/~nyasu/palmsync/e.html
;; * SPF-Todo: My own todo mode (single flat file)
;;
;; TODO
;;
;; * Native Pilot client
;; * Calendar/Diary client
;;

The code is hosted on sourceforge: http://sourceforge.net/projects/susync/

