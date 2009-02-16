;;; nnir.el --- search mail with various search engines -*- coding: iso-8859-1 -*-
;; Copyright (C) 1998 Kai Groﬂjohann

;; Author: Kai Groﬂjohann <grossjohann@ls6.cs.uni-dortmund.de>
;; Keywords: news, mail, searching, ir, glimpse, wais, hyrex

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The most recent version of this can always be fetched from the Gnus
;; CVS repository.  See http://www.gnus.org/ for more information.

;; This code is still in the development stage but I'd like other
;; people to have a look at it.  Please do not hesitate to contact me
;; with your ideas.

;; What does it do?  Well, it allows you to index your mail using some
;; search engine (freeWAIS-sf, Glimpse and others -- see later),
;; then type `G G' in the Group buffer and issue a query to the search
;; engine.  You will then get a buffer which shows all articles
;; matching the query, sorted by Retrieval Status Value (score).

;; When looking at the retrieval result (in the Summary buffer) you
;; can type `G T' (aka M-x gnus-summary-nnir-goto-thread RET) on an
;; article.  You will be teleported into the group this article came
;; from, showing the thread this article is part of.  (See below for
;; restrictions.)

;; The Lisp installation is simple: just put this file on your
;; load-path, byte-compile it, and load it from ~/.gnus or something.
;; This will install a new command `G G' in your Group buffer for
;; searching your mail.  Note that you also need to configure a number
;; of variables, as described below.

;; Restrictions:
;;
;; * If you don't use HyREX as your search engine, this expects that
;;   you use nnml or another one-file-per-message backend, because the
;;   others doesn't support nnfolder.
;; * It can only search the mail backend's which are supported by one
;;   search engine, because of different query languages.
;; * There are restrictions to the Glimpse setup.
;; * There are restrictions to the Wais setup.
;; * There are restrictions to the imap setup.
;; * gnus-summary-nnir-goto-thread: Fetches whole group first, before
;;   limiting to the right articles.  This is much too slow, of
;;   course.  May issue a query for number of articles to fetch; you
;;   must accept the default of all articles at this point or things
;;   may break.

;; The Lisp setup involves setting a few variables and setting up the
;; search engine. You can define the variables in the server definition
;; like this :
;;   (setq gnus-secondary-select-methods '(
;;       (nnimap "" (nnimap-address "localhost")
;;                  (nnir-search-engine hyrex)
;;                  (nnir-hyrex-additional-switches ("-d" "ddl-nnimap.xml"))
;;       )))
;; Or you can define the global ones. The variables set in the mailer-
;; definition will be used first.
;; The variable to set is `nnir-search-engine'.  Choose one of the engines
;; listed in `nnir-engines'.  (Actually `nnir-engines' is an alist,
;; type `C-h v nnir-engines RET' for more information; this includes
;; examples for setting `nnir-search-engine', too.)
;;
;; The variable nnir-mail-backend isn't used anymore.
;;

;; You must also set up a search engine.  I'll tell you about the two
;; search engines currently supported:

;; 1. freeWAIS-sf
;;
;; As always with freeWAIS-sf, you need a so-called `format file'.  I
;; use the following file:
;;
;; ,-----
;; | # Kai's format file for freeWAIS-sf for indexing mails.
;; | # Each mail is in a file, much like the MH format.
;; |
;; | # Document separator should never match -- each file is a document.
;; | record-sep: /^@this regex should never match@$/
;; |
;; | # Searchable fields specification.
;; |
;; | region: /^[sS]ubject:/ /^[sS]ubject: */
;; |         subject "Subject header" stemming TEXT BOTH
;; | end: /^[^ \t]/
;; |
;; | region: /^([tT][oO]|[cC][cC]):/ /^([tT][oO]|[cC][cC]): */
;; |         to "To and Cc headers" SOUNDEX BOTH
;; | end: /^[^ \t]/
;; |
;; | region: /^[fF][rR][oO][mM]:/ /^[fF][rR][oO][mM]: */
;; |         from "From header" SOUNDEX BOTH
;; | end: /^[^ \t]/
;; |
;; | region: /^$/
;; |         stemming TEXT GLOBAL
;; | end: /^@this regex should never match@$/
;; `-----
;;
;; 1998-07-22: waisindex would dump core on me for large articles with
;; the above settings.  I used /^$/ as the end regex for the global
;; field.  That seemed to work okay.

;; There is a Perl module called `WAIS.pm' which is available from
;; CPAN as well as ls6-ftp.cs.uni-dortmund.de:/pub/wais/Perl.  This
;; module comes with a nifty tool called `makedb', which I use for
;; indexing.  Here's my `makedb.conf':
;;
;; ,-----
;; | # Config file for makedb
;; |
;; | # Global options
;; | waisindex = /usr/local/bin/waisindex
;; | wais_opt  = -stem -t fields
;; | # `-stem' option necessary when `stemming' is specified for the
;; | # global field in the *.fmt file
;; |
;; | # Own variables
;; | homedir = /home/kai
;; |
;; | # The mail database.
;; | database        = mail
;; | files           = `find $homedir/Mail -name \*[0-9] -print`
;; | dbdir           = $homedir/.wais
;; | limit           = 100
;; `-----
;;
;; The Lisp setup involves the `nnir-wais-*' variables.  The most
;; difficult to understand variable is probably
;; `nnir-wais-remove-prefix'.  Here's what it does: the output of
;; `waissearch' basically contains the file name and the (full)
;; directory name.  As Gnus works with group names rather than
;; directory names, the directory name is transformed into a group
;; name as follows: first, a prefix is removed from the (full)
;; directory name, then all `/' are replaced with `.'.  The variable
;; `nnir-wais-remove-prefix' should contain a regex matching exactly
;; this prefix.  It defaults to `$HOME/Mail/' (note the trailing
;; slash).

;; 2. Glimpse
;;
;; The code expects you to have one Glimpse index which contains all
;; your mail files.  The Lisp setup involves setting the
;; `nnir-glimpse-*' variables.  The most difficult to understand
;; variable is probably `nnir-glimpse-remove-prefix', it corresponds
;; to `nnir-wais-remove-prefix', see above.  The `nnir-glimpse-home'
;; variable should be set to the value of the `-H' option which allows
;; one to search this Glimpse index.  I have indexed my whole home
;; directory with Glimpse, so I assume a default of `$HOME'.

;; 3. Namazu
;;
;; The Namazu backend requires you to have one directory containing all
;; index files, this is controlled by the `nnir-namazu-index-directory'
;; variable.  To function the `nnir-namazu-remove-prefix' variable must
;; also be correct, see the documentation for `nnir-wais-remove-prefix'
;; above.
;;
;; It is particularly important not to pass any any switches to namazu
;; that will change the output format.  Good switches to use include
;; `--sort', `--ascending', `--early' and `--late'.  Refer to the Namazu
;; documentation for further information on valid switches.
;;
;; To index my mail with the `mknmz' program I use the following
;; configuration file:
;;
;; ,----
;; | package conf;  # Don't remove this line!
;; |
;; | # Paths which will not be indexed. Don't use `^' or `$' anchors.
;; | $EXCLUDE_PATH = "spam|sent";
;; |
;; | # Header fields which should be searchable. case-insensitive
;; | $REMAIN_HEADER = "from|date|message-id|subject";
;; |
;; | # Searchable fields. case-insensitive
;; | $SEARCH_FIELD = "from|date|message-id|subject";
;; |
;; | # The max length of a word.
;; | $WORD_LENG_MAX = 128;
;; |
;; | # The max length of a field.
;; | $MAX_FIELD_LENGTH = 256;
;; `----
;;
;; My mail is stored in the directories ~/Mail/mail/, ~/Mail/lists/ and
;; ~/Mail/archive/, so to index them I go to the directory set in
;; `nnir-namazu-index-directory' and issue the following command.
;;
;;      mknmz --mailnews ~/Mail/archive/ ~/Mail/mail/ ~/Mail/lists/
;;
;; For maximum searching efficiency I have a cron job set to run this
;; command every four hours.

;; 4. HyREX
;;
;; The HyREX backend requires you to have one directory from where all
;; your relative paths are to, if you use them. This directory must be
;; set in the `nnir-hyrex-index-directory' variable, which defaults to
;; your home directory. You must also pass the base, class and
;; directory options or simply your dll to the `nnir-hyrex-programm' by
;; setting the `nnir-hyrex-additional-switches' variable accordently.
;; To function the `nnir-hyrex-remove-prefix' variable must also be
;; correct, see the documentation for `nnir-wais-remove-prefix' above.

;; Developer information:

;; I have tried to make the code expandable.  Basically, it is divided
;; into two layers.  The upper layer is somewhat like the `nnvirtual'
;; or `nnkiboze' backends: given a specification of what articles to
;; show from another backend, it creates a group containing exactly
;; those articles.  The lower layer issues a query to a search engine
;; and produces such a specification of what articles to show from the
;; other backend.

;; The interface between the two layers consists of the single
;; function `nnir-run-query', which just selects the appropriate
;; function for the search engine one is using.  The input to
;; `nnir-run-query' is a string, representing the query as input by
;; the user.  The output of `nnir-run-query' is supposed to be a
;; vector, each element of which should in turn be a three-element
;; vector.  The first element should be full group name of the article,
;; the second element should be the article number, and the third
;; element should be the Retrieval Status Value (RSV) as returned from
;; the search engine.  An RSV is the score assigned to the document by
;; the search engine.  For Boolean search engines like Glimpse, the
;; RSV is always 1000 (or 1 or 100, or whatever you like).

;; The sorting order of the articles in the summary buffer created by
;; nnir is based on the order of the articles in the above mentioned
;; vector, so that's where you can do the sorting you'd like.  Maybe
;; it would be nice to have a way of displaying the search result
;; sorted differently?

;; So what do you need to do when you want to add another search
;; engine?  You write a function that executes the query.  Temporary
;; data from the search engine can be put in `nnir-tmp-buffer'.  This
;; function should return the list of articles as a vector, as
;; described above.  Then, you need to register this backend in
;; `nnir-engines'.  Then, users can choose the backend by setting
;; `nnir-search-engine'.

;; Todo, or future ideas:

;; * Make it so that Glimpse can also be called without `-F'.
;;
;; * It should be possible to restrict search to certain groups.
;;
;; * There is currently no error checking.
;;
;; * The summary buffer display is currently really ugly, with all the
;;   added information in the subjects.  How could I make this
;;   prettier?
;;
;; * A function which can be called from an nnir summary buffer which
;;   teleports you into the group the current article came from and
;;   shows you the whole thread this article is part of.
;;   Implementation suggestions?
;;   (1998-07-24: There is now a preliminary implementation, but
;;   it is much too slow and quite fragile.)
;;
;; * Support other mail backends.  In particular, probably quite a few
;;   people use nnfolder.  How would one go about searching nnfolders
;;   and producing the right data needed?  The group name and the RSV
;;   are simple, but what about the article number?
;;   - The article number is encoded in the `X-Gnus-Article-Number'
;;     header of each mail.
;;   - The HyREX engine supports nnfolder.
;;
;; * Support compressed mail files.  Probably, just stripping off the
;;   `.gz' or `.Z' file name extension is sufficient.
;;
;; * Support a find/grep combination.
;;
;; * At least for imap, the query is performed twice.
;;

;; Have you got other ideas?

;;; Setup Code:

(require 'cl)
(require 'nnoo)
(require 'gnus-group)
(require 'gnus-sum)
(eval-and-compile
  (require 'gnus-util))
(eval-when-compile
  (require 'nnimap)
  (autoload 'read-kbd-macro "edmacro" nil t))

(nnoo-declare nnir)
(nnoo-define-basics nnir)

(gnus-declare-backend "nnir" 'mail)

(defvar nnir-imap-search-field "TEXT"
  "The IMAP search item when doing an nnir search")

(defvar nnir-imap-search-arguments
  '(("Whole message" . "TEXT")
    ("Subject" . "SUBJECT")
    ("To" . "TO")
    ("From" . "FROM")
    (nil . "HEADER \"%s\""))
  "Mapping from user readable strings to IMAP search items for use in nnir")

(defvar nnir-imap-search-argument-history ()
  "The history for querying search options in nnir")

;;; Developer Extension Variable:

(defvar nnir-engines
  `((glimpse nnir-run-glimpse
             ((group . "Group spec: ")))
    (wais    nnir-run-waissearch
             ())
    (excite  nnir-run-excite-search
	     ())
    (imap    nnir-run-imap
             ((criteria 
	       "Search in: "                      ; Prompt
	       ,nnir-imap-search-arguments        ; alist for completing
	       nil                                ; no filtering
	       nil                                ; allow any user input
	       nil                                ; initial value
	       nnir-imap-search-argument-history  ; the history to use
	       ,nnir-imap-search-field            ; default
	       )))
    (swish++ nnir-run-swish++
             ((group . "Group spec: ")))
    (swish-e nnir-run-swish-e
             ((group . "Group spec: ")))
    (namazu  nnir-run-namazu
             ())
    (hyrex   nnir-run-hyrex
	     ((group . "Group spec: "))))
  "Alist of supported search engines.
Each element in the alist is a three-element list (ENGINE FUNCTION ARGS).
ENGINE is a symbol designating the searching engine.  FUNCTION is also
a symbol, giving the function that does the search.  The third element
ARGS is a list of cons pairs (PARAM . PROMPT).  When issuing a query,
the FUNCTION will issue a query for each of the PARAMs, using PROMPT.

The value of `nnir-search-engine' must be one of the ENGINE symbols.
For example, use the following line for searching using freeWAIS-sf:
    (setq nnir-search-engine 'wais)
Use the following line if you read your mail via IMAP and your IMAP
server supports searching:
    (setq nnir-search-engine 'imap)
Note that you have to set additional variables for most backends.  For
example, the `wais' backend needs the variables `nnir-wais-program',
`nnir-wais-database' and `nnir-wais-remove-prefix'.

Add an entry here when adding a new search engine.")

;;; User Customizable Variables:

(defgroup nnir nil
  "Search nnmh and nnml groups in Gnus with Glimpse, freeWAIS-sf, or EWS."
  :group 'gnus)

;; Mail backend.

;; TODO:
;; If `nil', use server parameters to find out which server to search. CCC
;;
(defcustom nnir-mail-backend '(nnml "")
  "*Specifies which backend should be searched.
More precisely, this is used to determine from which backend to fetch the
messages found.

This must be equal to an existing server, so maybe it is best to use
something like the following:
    (setq nnir-mail-backend (nth 0 gnus-secondary-select-methods))
The above line works fine if the mail backend you want to search is
the first element of gnus-secondary-select-methods (`nth' starts counting
at zero)."
  :type '(sexp)
  :group 'nnir)

;; Search engine to use.

(defcustom nnir-search-engine 'wais
  "*The search engine to use.  Must be a symbol.
See `nnir-engines' for a list of supported engines, and for example
settings of `nnir-search-engine'."
  :type '(sexp)
  :group 'nnir)

;; Glimpse engine.

(defcustom nnir-glimpse-program "glimpse"
  "*Name of Glimpse executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-glimpse-home (getenv "HOME")
  "*Value of `-H' glimpse option.
`~' and environment variables must be expanded, see the functions
`expand-file-name' and `substitute-in-file-name'."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-glimpse-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by Glimpse
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

For example, suppose that Glimpse returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-glimpse-remove-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\"."
  :type '(regexp)
  :group 'nnir)

(defcustom nnir-glimpse-additional-switches '("-i")
  "*A list of strings, to be given as additional arguments to glimpse.
The switches `-H', `-W', `-l' and `-y' are always used -- calling
glimpse without them does not make sense in our situation.
Suggested elements to put here are `-i' and `-w'.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-glimpse-additional-switches \"-i -w\") ; wrong!
Instead, use this:
    (setq nnir-glimpse-additional-switches '(\"-i\" \"-w\"))"
  :type '(repeat (string))
  :group 'nnir)

;; freeWAIS-sf.

(defcustom nnir-wais-program "waissearch"
  "*Name of waissearch executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-wais-database (expand-file-name "~/.wais/mail")
  "*Name of Wais database containing the mail.

Note that this should be a file name without extension.  For example,
if you have a file /home/john/.wais/mail.fmt, use this:
    (setq nnir-wais-database \"/home/john/.wais/mail\")
The string given here is passed to `waissearch -d' as-is."
  :type '(file)
  :group 'nnir)

(defcustom nnir-wais-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each directory name returned by waissearch
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable is similar to `nnir-glimpse-remove-prefix', only for Wais,
not Glimpse."
  :type '(regexp)
  :group 'nnir)

;; EWS (Excite for Web Servers) engine.

(defcustom nnir-excite-aquery-program "aquery.pl"
  "*Name of the EWS query program.  Should be `aquery.pl' or a path to same."
  :type '(string)
  :group 'nnir)

(defcustom nnir-excite-collection "Mail"
  "*Name of the EWS collection to search."
  :type '(string)
  :group 'nnir)

(defcustom nnir-excite-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by EWS
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable is very similar to `nnir-glimpse-remove-prefix', except
that it is for EWS, not Glimpse."
  :type '(regexp)
  :group 'nnir)

;; Swish++.  Next three variables Copyright (C) 2000, 2001 Christoph
;; Conrad <christoph.conrad@gmx.de>.
;; Swish++ home page: http://homepage.mac.com/pauljlucas/software/swish/

(defcustom nnir-swish++-configuration-file
  (expand-file-name "~/Mail/swish++.conf")
  "*Configuration file for swish++."
  :type '(file)
  :group 'nnir)

(defcustom nnir-swish++-program "search"
  "*Name of swish++ search executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-swish++-additional-switches '()
  "*A list of strings, to be given as additional arguments to swish++.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-swish++-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-swish++-additional-switches '(\"-i\" \"-w\"))"
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-swish++-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by swish++
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable is very similar to `nnir-glimpse-remove-prefix', except
that it is for swish++, not Glimpse."
  :type '(regexp)
  :group 'nnir)

;; Swish-E.  Next three variables Copyright (C) 2000 Christoph Conrad
;; <christoph.conrad@gmx.de>.
;; URL: http://sunsite.berkeley.edu/SWISH-E/
;; New version: http://www.boe.es/swish-e

(defcustom nnir-swish-e-index-file
  (expand-file-name "~/Mail/index.swish-e")
  "*Index file for swish-e.
This could be a server parameter."
  :type '(file)
  :group 'nnir)

(defcustom nnir-swish-e-program "swish-e"
  "*Name of swish-e search executable.
This cannot be a server parameter."
  :type '(string)
  :group 'nnir)

(defcustom nnir-swish-e-additional-switches '()
  "*A list of strings, to be given as additional arguments to swish-e.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-swish-e-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-swish-e-additional-switches '(\"-i\" \"-w\"))

This could be a server parameter."
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-swish-e-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by swish-e
in order to get a group name (albeit with / instead of .).  This is a
regular expression.

This variable is very similar to `nnir-glimpse-remove-prefix', except
that it is for swish-e, not Glimpse.

This could be a server parameter."
  :type '(regexp)
  :group 'nnir)

;; HyREX engine, see <URL:http://ls6-www.cs.uni-dortmund.de/>

(defcustom nnir-hyrex-program "nnir-search"
  "*Name of the nnir-search executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-hyrex-additional-switches '()
  "*A list of strings, to be given as additional arguments for nnir-search.
Note that this should be a list. Ie, do NOT use the following:
    (setq nnir-hyrex-additional-switches \"-ddl ddl.xml -c nnir\") ; wrong !
Instead, use this:
    (setq nnir-hyrex-additional-switches '(\"-ddl\" \"ddl.xml\" \"-c\" \"nnir\"))"
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-hyrex-index-directory (getenv "HOME")
  "*Index directory for HyREX."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-hyrex-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by HyREX
in order to get a group name (albeit with / instead of .).

For example, suppose that HyREX returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-hyrex-remove-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\"."
  :type '(directory)
  :group 'nnir)

;; Namazu engine, see <URL:http://ww.namazu.org/>

(defcustom nnir-namazu-program "namazu"
  "*Name of Namazu search executable."
  :type '(string)
  :group 'nnir)

(defcustom nnir-namazu-index-directory (expand-file-name "~/Mail/namazu/")
  "*Index directory for Namazu."
  :type '(directory)
  :group 'nnir)

(defcustom nnir-namazu-additional-switches '()
  "*A list of strings, to be given as additional arguments to namazu.
The switches `-q', `-a', and `-s' are always used, very few other switches
make any sense in this context.

Note that this should be a list.  Ie, do NOT use the following:
    (setq nnir-namazu-additional-switches \"-i -w\") ; wrong
Instead, use this:
    (setq nnir-namazu-additional-switches '(\"-i\" \"-w\"))"
  :type '(repeat (string))
  :group 'nnir)

(defcustom nnir-namazu-remove-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by Namazu
in order to get a group name (albeit with / instead of .).

This variable is very similar to `nnir-glimpse-remove-prefix', except
that it is for Namazu, not Glimpse."
  :type '(directory)
  :group 'nnir)

;;; Internal Variables:

(defvar nnir-current-query nil
  "Internal: stores current query (= group name).")

(defvar nnir-current-server nil
  "Internal: stores current server (does it ever change?).")

(defvar nnir-current-group-marked nil
  "Internal: stores current list of process-marked groups.")

(defvar nnir-artlist nil
  "Internal: stores search result.")

(defvar nnir-tmp-buffer " *nnir*"
  "Internal: temporary buffer.")

;;; Code:

;; Gnus glue.

(defun gnus-group-make-nnir-group (extra-parms query)
  "Create an nnir group.  Asks for query."
  (interactive "P\nsQuery: ")
  (setq nnir-current-query nil
	nnir-current-server nil
	nnir-current-group-marked nil
	nnir-artlist nil)
  (let ((parms nil))
    (if extra-parms
        (setq parms (nnir-read-parms query))
      (setq parms (list (cons 'query query))))
    (gnus-group-read-ephemeral-group
     (concat "nnir:" (prin1-to-string parms)) '(nnir "") t
     (cons (current-buffer)
           gnus-current-window-configuration)
     nil)))

;; Emacs 19 compatibility?
(or (fboundp 'kbd) (defalias 'kbd 'read-kbd-macro))

(defun nnir-group-mode-hook ()
  (define-key gnus-group-mode-map
    (if (fboundp 'read-kbd-macro)
        (kbd "G G")
      "GG")                             ; XEmacs 19 compat
    'gnus-group-make-nnir-group))
(add-hook 'gnus-group-mode-hook 'nnir-group-mode-hook)



;; Summary mode commands.

(defun gnus-summary-nnir-goto-thread ()
  "Only applies to nnir groups.  Go to group this article came from
and show thread that contains this article."
  (interactive)
  (unless (eq 'nnir (car (gnus-find-method-for-group gnus-newsgroup-name)))
    (error "Can't execute this command unless in nnir group."))
  (let* ((cur (gnus-summary-article-number))
         (group (nnir-artlist-artitem-group nnir-artlist cur))
         (backend-number (nnir-artlist-artitem-number nnir-artlist cur))
	 server backend-group)
    (setq server (nnir-group-server group))
    (setq backend-group (gnus-group-real-name group))
    (gnus-group-read-ephemeral-group
     backend-group
     (gnus-server-to-method server)
     t                                  ; activate
     (cons (current-buffer)
           'summary)                    ; window config
     nil
     (list backend-number))
    (gnus-summary-limit (list backend-number))
    (gnus-summary-refer-thread)))

(if (fboundp 'eval-after-load)
    (eval-after-load "gnus-sum"
      '(define-key gnus-summary-goto-map
         "T" 'gnus-summary-nnir-goto-thread))
  (add-hook 'gnus-summary-mode-hook
            (function (lambda ()
                        (define-key gnus-summary-goto-map
                          "T" 'gnus-summary-nnir-goto-thread)))))



;; Gnus backend interface functions.

(deffoo nnir-open-server (server &optional definitions)
  ;; Just set the server variables appropriately.
  (nnoo-change-server 'nnir server definitions))

(deffoo nnir-request-group (group &optional server fast)
  "GROUP is the query string."
  (nnir-possibly-change-server server)
  ;; Check for cache and return that if appropriate.
  (if (and (equal group nnir-current-query)
           (equal gnus-group-marked nnir-current-group-marked)
           (or (null server)
               (equal server nnir-current-server)))
      nnir-artlist
    ;; Cache miss.
    (setq nnir-artlist (nnir-run-query group)))
  (save-excursion
    (set-buffer nntp-server-buffer)
    (if (zerop (length nnir-artlist))
	(progn
	  (setq nnir-current-query nil
		nnir-current-server nil
		nnir-current-group-marked nil
		nnir-artlist nil)
	  (nnheader-report 'nnir "Search produced empty results."))
      ;; Remember data for cache.
      (setq nnir-current-query group)
      (when server (setq nnir-current-server server))
      (setq nnir-current-group-marked gnus-group-marked)
      (nnheader-insert "211 %d %d %d %s\n"
		       (nnir-artlist-length nnir-artlist) ; total #
		       1              ; first #
		       (nnir-artlist-length nnir-artlist) ; last #
		       group))))     ; group name

(deffoo nnir-retrieve-headers (articles &optional group server fetch-old)
  (save-excursion
    (let ((artlist (copy-sequence articles))
          (idx 1)
          (art nil)
          (artitem nil)
          (artgroup nil) (artno nil)
          (artrsv nil)
          (artfullgroup nil)
          (novitem nil)
          (novdata nil)
          (foo nil)
	  server)
      (while (not (null artlist))
        (setq art (car artlist))
        (or (numberp art)
            (nnheader-report
             'nnir
             "nnir-retrieve-headers doesn't grok message ids: %s"
             art))
        (setq artitem (nnir-artlist-article nnir-artlist art))
        (setq artrsv (nnir-artitem-rsv artitem))
        (setq artfullgroup (nnir-artitem-group artitem))
        (setq artno (nnir-artitem-number artitem))
        (setq artgroup (gnus-group-real-name artfullgroup))
	(setq server (nnir-group-server artfullgroup))
        ;; retrieve NOV or HEAD data for this article, transform into
        ;; NOV data and prepend to `novdata'
        (set-buffer nntp-server-buffer)
	(nnir-possibly-change-server server)
        (case (setq foo (gnus-retrieve-headers (list artno) artfullgroup nil))
          (nov
           (goto-char (point-min))
           (setq novitem (nnheader-parse-nov))
           (unless novitem
             (pop-to-buffer nntp-server-buffer)
             (error
              "nnheader-parse-nov returned nil for article %s in group %s"
              artno artfullgroup)))
          (headers
           (goto-char (point-min))
           (setq novitem (nnheader-parse-head))
           (unless novitem
             (pop-to-buffer nntp-server-buffer)
             (error
              "nnheader-parse-head returned nil for article %s in group %s"
              artno artfullgroup)))
          (t (nnheader-report 'nnir "Don't support header type %s." foo)))
       ;; replace article number in original group with article number
        ;; in nnir group
        (mail-header-set-number novitem idx)
        (mail-header-set-from novitem
                              (mail-header-from novitem))
        (mail-header-set-subject
         novitem
         (format "[%d: %s/%d] %s"
                 artrsv artgroup artno
                 (mail-header-subject novitem)))
        ;;-(mail-header-set-extra novitem nil)
        (push novitem novdata)
        (setq artlist (cdr artlist))
        (setq idx (1+ idx)))
      (setq novdata (nreverse novdata))
      (set-buffer nntp-server-buffer) (erase-buffer)
      (mapcar 'nnheader-insert-nov novdata)
      'nov)))

(deffoo nnir-request-article (article
                              &optional group server to-buffer)
  (save-excursion
    (let* ((artitem (nnir-artlist-article nnir-artlist
                                          article))
           (artfullgroup (nnir-artitem-group artitem))
           (artno (nnir-artitem-number artitem))
           ;; Bug?
           ;; Why must we bind nntp-server-buffer here?  It won't
           ;; work if `buf' is used, say.  (Of course, the set-buffer
           ;; line below must then be updated, too.)
           (nntp-server-buffer (or to-buffer nntp-server-buffer)))
      (set-buffer nntp-server-buffer)
      (erase-buffer)
      (message "Requesting article %d from group %s"
               artno artfullgroup)
      (gnus-request-article artno artfullgroup nntp-server-buffer)
      (cons artfullgroup artno))))


(nnoo-define-skeleton nnir)

;;; Search Engine Interfaces:

;; Glimpse interface.
(defun nnir-run-glimpse (query server &optional group)
  "Run given query against glimpse.  Returns a vector of (group name, file name)
pairs (also vectors, actually)."
  (save-excursion
    (let ((artlist nil)
          (groupspec (cdr (assq 'group query)))
          (qstring (cdr (assq 'query query)))
	  (prefix (nnir-read-server-parm 'nnir-glimps-remove-prefix server))
	  artno dirnam)
      (when (and group groupspec)
        (error (concat "It does not make sense to use a group spec"
                       " with process-marked groups.")))
      (when group
        (setq groupspec (gnus-group-real-name group)))
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (if groupspec
          (message "Doing glimpse query %s on %s..." query groupspec)
        (message "Doing glimpse query %s..." query))
      (let* ((cp-list
              `( ,nnir-glimpse-program
                 nil                    ; input from /dev/null
                 t                      ; output
                 nil                    ; don't redisplay
                 "-H" ,(nnir-read-server-parm 'nnir-glimpse-home server) ; search home dir
                 "-W"                   ; match pattern in file
                 "-l" "-y"              ; misc options
                 ,@(nnir-read-server-parm 'nnir-glimpse-additional-switches server)
                 "-F" ,prefix           ; restrict output to mail
                 ,qstring               ; the query, in glimpse format
		 ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-glimpse-program
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run glimpse: %s" exitstatus)
          ;; Glimpse failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))
      (when groupspec
        (keep-lines groupspec))
      (if groupspec
          (message "Doing glimpse query %s on %s...done" query groupspec)
        (message "Doing glimpse query %s...done" query))
      (sit-for 0)
      ;; remove superfluous stuff from glimpse output
      (goto-char (point-min))
      (delete-non-matching-lines "/[0-9]+$")
      ;;(delete-matching-lines "\\.overview~?$")
      (goto-char (point-min))
      (while (re-search-forward (concat "^" prefix "\\(.+\\)" "/\\([0-9]\\)+$") nil t)
	;; replace / with . in group names
        (setq dirnam (substitute ?. ?/ (match-string 1))
	      artno  (match-string 2))
	(push (vector (nnir-group-full-name dirnam server)
		      (string-to-int artno)) artlist))

      (sort* artlist
             (function (lambda (x y)
                         (if (string-lessp (nnir-artitem-group x)
                                           (nnir-artitem-group y))
                             t
                           (< (nnir-artitem-number x)
                              (nnir-artitem-number y))))))
      )))

;; freeWAIS-sf interface.
(defun nnir-run-waissearch (query server &optional group)
  "Run given query agains waissearch.  Returns vector of (group name, file name)
pairs (also vectors, actually)."
  (when group
    (error "The freeWAIS-sf backend cannot search specific groups."))
  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
	  (prefix (nnir-read-server-parm 'nnir-wais-remove-prefix server))
          (artlist nil)
          (score nil) (artno nil) (dirnam nil) (group nil))
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (message "Doing WAIS query %s..." query)
      (call-process nnir-wais-program
                    nil                 ; input from /dev/null
                    t                   ; output to current buffer
                    nil                 ; don't redisplay
                    "-d" (nnir-read-server-parm 'nnir-wais-database server) ; database to search
                    qstring)
      (message "Massaging waissearch output...")
      ;; remove superfluous lines
      (keep-lines "Score:")
      ;; extract data from result lines
      (goto-char (point-min))
      (while (re-search-forward
              "Score: +\\([0-9]+\\).*'\\([0-9]+\\) +\\([^']+\\)/'" nil t)
        (setq score (match-string 1)
              artno (match-string 2)
              dirnam (match-string 3))
        (unless (string-match prefix dirnam)
          (nnheader-report 'nnir "Dir name %s doesn't contain prefix %s"
                           dirnam prefix))
        (setq group (substitute ?. ?/ (replace-match "" t t dirnam)))
        (push (vector (nnir-group-full-name group server)
                      (string-to-int artno)
                      (string-to-int score))
              artlist))
      (message "Massaging waissearch output...done")
      (apply 'vector
             (sort* artlist
                    (function (lambda (x y)
                                (> (nnir-artitem-rsv x)
                                   (nnir-artitem-rsv y)))))))))

;; EWS (Excite for Web Servers) interface
(defun nnir-run-excite-search (query server &optional group)
  "Run a given query against EWS.  Returns vector of (group name, file name)
pairs (also vectors, actually)."
  (when group
    (error "Searching specific groups not implemented for EWS."))
  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
	  (prefix (nnir-read-server-parm 'nnir-excite-remove-prefix server))
	  artlist group article-num article)
      (setq nnir-current-query query)
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (message "Doing EWS query %s..." qstring)
      (call-process nnir-excite-aquery-program
		    nil			; input from /dev/null
		    t			; output to current buffer
		    nil			; don't redisplay
		    (nnir-read-server-parm 'nnir-excite-collection server)
		    (if (string= (substring qstring 0 1) "(")
			qstring
		      (format "(concept %s)" qstring)))
      (message "Gathering query output...")

      (goto-char (point-min))
      (while (re-search-forward
	      "^[0-9]+\\s-[0-9]+\\s-[0-9]+\\s-\\(\\S-*\\)" nil t)
	(setq article (match-string 1))
	(unless (string-match
		 (concat "^" (regexp-quote prefix)
			 "\\(.*\\)/\\([0-9]+\\)") article)
	  (nnheader-report 'nnir "Dir name %s doesn't contain prefix %s"
			   article prefix))
	(setq group (substitute ?. ?/ (match-string 1 article)))
	(setq group (nnir-group-full-name group server))
	(setq article-num (match-string 2 article))
	(setq artlist (vconcat artlist (vector (vector group
						       (string-to-int article-num)
						       1000)))))
      (message "Gathering query output...done")
      artlist)))

;; IMAP interface.  The following function is Copyright (C) 1998 Simon
;; Josefsson <jas@pdc.kth.se>.
;; todo:
;; nnir invokes this two (2) times???!
;; we should not use nnimap at all but open our own server connection
;; we should not LIST * but use nnimap-list-pattern from defs
;; send queries as literals
;; handle errors

(defun nnir-run-imap (query srv &optional group-option)
  (require 'imap)
  (require 'nnimap)
  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
	  (server (cadr (gnus-server-to-method srv)))
	  (group (or group-option (gnus-group-group-name)))
	  (defs (caddr (gnus-server-to-method srv)))
	  (criteria (or (cdr (assq 'criteria query))
			nnir-imap-search-field))
	  artlist buf)
      (message "Opening server %s" server)
      (condition-case ()
	  (when (nnimap-open-server server defs) ;; xxx
	    (setq buf nnimap-server-buffer) ;; xxx
	    (message "Searching %s..." group)
            (let ((arts 0)
                  (mbx (gnus-group-real-name group)))
              (when (imap-mailbox-select mbx nil buf)
                (mapcar
                 (lambda (artnum)
                   (push (vector group artnum 1) artlist)
                   (setq arts (1+ arts)))
                 (imap-search (concat criteria " \"" qstring "\"") buf))
                (message "Searching %s... %d matches" mbx arts)))
            (message "Searching %s...done" group))
        (quit nil))
      (reverse artlist))))

;; Swish++ interface.  The following function is Copyright (C) 2000,
;; 2001 Christoph Conrad <christoph.conrad@gmx.de>.
;; -cc- Todo
;; Search by
;; - group
;; Sort by
;; - rank (default)
;; - article number
;; - file size
;; - group
(defun nnir-run-swish++ (query server &optional group)
  "Run given query against swish++.
Returns a vector of (group name, file name) pairs (also vectors,
actually).

Tested with swish++ 4.7 on GNU/Linux and with with swish++ 5.0b2 on
Windows NT 4.0."

  (when group
    (error "The swish++ backend cannot search specific groups."))

  (save-excursion
    (let ( (qstring (cdr (assq 'query query)))
	   (groupspec (cdr (assq 'group query)))
	   (prefix (nnir-read-server-parm 'nnir-swish++-remove-prefix server))
           (artlist nil)
           (score nil) (artno nil) (dirnam nil) (group nil) )

      (when (equal "" qstring)
        (error "swish++: You didn't enter anything."))

      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)

      (if groupspec
          (message "Doing swish++ query %s on %s..." qstring groupspec)
        (message "Doing swish++ query %s..." qstring))

      (let* ((cp-list `( ,nnir-swish++-program
                         nil            ; input from /dev/null
                         t              ; output
                         nil            ; don't redisplay
                         "--config-file" ,(nnir-read-server-parm 'nnir-swish++-configuration-file server)
                         ,@(nnir-read-server-parm 'nnir-swish++-additional-switches server)
                         ,qstring       ; the query, in swish++ format
                         ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-swish++-program
                         (mapconcat 'identity (cddddr cp-list) " ")) ;; ???
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run swish++: %s" exitstatus)
          ;; swish++ failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; The results are output in the format of:
      ;; V 4.7 Linux
      ;; rank relative-path-name file-size file-title
      ;; V 5.0b2:
      ;; rank relative-path-name file-size topic??
      ;; where rank is an integer from 1 to 100.
      (goto-char (point-min))
      (while (re-search-forward
              "\\(^[0-9]+\\) \\([^ ]+\\) [0-9]+ \\(.*\\)$" nil t)
        (setq score (match-string 1)
              artno (file-name-nondirectory (match-string 2))
              dirnam (file-name-directory (match-string 2)))

        ;; don't match directories
        (when (string-match "^[0-9]+$" artno)
          (when (not (null dirnam))

	    ;; maybe limit results to matching groups.
	    (when (or (not groupspec)
		      (string-match groupspec dirnam))

	      ;; remove nnir-swish++-remove-prefix from beginning of dirname
	      (when (string-match (concat "^" prefix)
				  dirnam)
		(setq dirnam (replace-match "" t t dirnam)))

	      (setq dirnam (substring dirnam 0 -1))
	      ;; eliminate all ".", "/", "\" from beginning. Always matches.
	      (string-match "^[./\\]*\\(.*\\)$" dirnam)
	      ;; "/" -> "."
	      (setq group (substitute ?. ?/ (match-string 1 dirnam)))
	      ;; "\\" -> "."
	      (setq group (substitute ?. ?\\ group))

	      (push (vector (nnir-group-full-name group server)
			    (string-to-int artno)
			    (string-to-int score))
		    artlist)))))

      (message "Massaging swish++ output...done")

      ;; Sort by score
      (apply 'vector
             (sort* artlist
                    (function (lambda (x y)
                                (> (nnir-artitem-rsv x)
                                   (nnir-artitem-rsv y)))))))))

;; Swish-E interface.  The following function is Copyright (C) 2000,
;; 2001 by Christoph Conrad <christoph.conrad@gmx.de>.
(defun nnir-run-swish-e (query server &optional group)
  "Run given query against swish-e.
Returns a vector of (group name, file name) pairs (also vectors,
actually).

Tested with swish-e-2.0.1 on Windows NT 4.0."

  ;; swish-e crashes with empty parameter to "-w" on commandline...
  (when group
    (error "The swish-e backend cannot search specific groups."))

  (save-excursion
    (let ((qstring (cdr (assq 'query query)))
	  (prefix
	   (or (nnir-read-server-parm 'nnir-swish-e-remove-prefix server)
	       (error "Missing parameter `nnir-swish-e-remove-prefix'")))
	  (artlist nil)
	  (score nil) (artno nil) (dirnam nil) (group nil) )

      (when (equal "" qstring)
        (error "swish-e: You didn't enter anything."))

      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)

      (message "Doing swish-e query %s..." query)
      (let* ((index-file
	      (or (nnir-read-server-parm
		   'nnir-swish-e-index-file server)
		  (error "Missing parameter `nnir-swish-e-index-file'")))
	     (additional-switches
	      (nnir-read-server-parm
	       'nnir-swish++-additional-switches server))
	     (cp-list `(,nnir-swish-e-program
			nil		; input from /dev/null
			t		; output
			nil		; don't redisplay
			"-f" ,index-file
			,@additional-switches
			"-w"
			,qstring	; the query, in swish-e format
			))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-swish-e-program
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run swish-e: %s" exitstatus)
          ;; swish-e failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; The results are output in the format of:
      ;; rank path-name file-title file-size
      (goto-char (point-min))
      (while (re-search-forward
              "\\(^[0-9]+\\) \\([^ ]+\\) \"\\([^\"]+\\)\" [0-9]+$" nil t)
        (setq score (match-string 1)
              artno (match-string 3)
              dirnam (file-name-directory (match-string 2)))

        ;; don't match directories
        (when (string-match "^[0-9]+$" artno)
          (when (not (null dirnam))

	    ;; remove nnir-swish-e-remove-prefix from beginning of dirname
            (when (string-match (concat "^" prefix) dirnam)
              (setq dirnam (replace-match "" t t dirnam)))

            (setq dirnam (substring dirnam 0 -1))
	    ;; eliminate all ".", "/", "\" from beginning. Always matches.
            (string-match "^[./\\]*\\(.*\\)$" dirnam)
            ;; "/" -> "."
            (setq group (substitute ?. ?/ (match-string 1 dirnam)))
            ;; Windows "\\" -> "."
            (setq group (substitute ?. ?\\ group))

            (push (vector (nnir-group-full-name group server)
                          (string-to-int artno)
                          (string-to-int score))
                  artlist))))

      (message "Massaging swish-e output...done")

      ;; Sort by score
      (apply 'vector
             (sort* artlist
                    (function (lambda (x y)
                                (> (nnir-artitem-rsv x)
                                   (nnir-artitem-rsv y)))))))))

;; HyREX interface
(defun nnir-run-hyrex (query server &optional group)
  (save-excursion
    (let ((artlist nil)
          (groupspec (cdr (assq 'group query)))
          (qstring (cdr (assq 'query query)))
	  (prefix (nnir-read-server-parm 'nnir-hyrex-remove-prefix server))
	  score artno dirnam)
      (when (and group groupspec)
        (error (concat "It does not make sense to use a group spec"
                       " with process-marked groups.")))
      (when group
        (setq groupspec (gnus-group-real-name group)))
      (when (and group (not (equal group (nnir-group-full-name groupspec server))))
	(message "%s vs. %s" group (nnir-group-full-name groupspec server))
	(error "Server with groupspec doesn't match group !"))
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (if groupspec
          (message "Doing hyrex-search query %s on %s..." query groupspec)
        (message "Doing hyrex-search query %s..." query))
      (let* ((cp-list
	      `( ,nnir-hyrex-program
		 nil			; input from /dev/null
		 t			; output
		 nil			; don't redisplay
		 "-i",(nnir-read-server-parm 'nnir-hyrex-index-directory server) ; index directory
		 ,@(nnir-read-server-parm 'nnir-hyrex-additional-switches server)
		 ,qstring	   ; the query, in hyrex-search format
		 ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-hyrex-program
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run hyrex-search: %s" exitstatus)
          ;; nnir-search failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer)))) ;; FIXME: Dont clear buffer !
      (if groupspec
          (message "Doing hyrex-search query \"%s\" on %s...done" qstring groupspec)
        (message "Doing hyrex-search query \"%s\"...done" qstring))
      (sit-for 0)
      ;; nnir-search returns:
      ;;   for nnml/nnfolder: "filename mailid weigth"
      ;;   for nnimap:        "group mailid weigth"
      (goto-char (point-min))
      (delete-non-matching-lines "^\\S + [0-9]+ [0-9]+$")
      ;; HyREX couldn't search directly in groups -- so filter out here.
      (when groupspec
	(keep-lines groupspec))
      ;; extract data from result lines
      (goto-char (point-min))
      (while (re-search-forward
	      "\\(\\S +\\) \\([0-9]+\\) \\([0-9]+\\)" nil t)
	(setq dirnam (match-string 1)
	      artno (match-string 2)
	      score (match-string 3))
	(when (string-match prefix dirnam)
	  (setq dirnam (replace-match "" t t dirnam)))
	(push (vector (nnir-group-full-name (substitute ?. ?/ dirnam) server)
		      (string-to-int artno)
		      (string-to-int score))
	      artlist))
      (message "Massaging hyrex-search output...done.")
      (apply 'vector
	     (sort* artlist
		    (function (lambda (x y)
				(if (string-lessp (nnir-artitem-group x)
						  (nnir-artitem-group y))
				    t
				  (< (nnir-artitem-number x)
				     (nnir-artitem-number y)))))))
      )))

;; Namazu interface
(defun nnir-run-namazu (query server &optional group)
  "Run given query against Namazu.  Returns a vector of (group name, file name)
pairs (also vectors, actually).

Tested with Namazu 2.0.6 on a GNU/Linux system."
  (when group
    (error "The Namazu backend cannot search specific groups"))
  (save-excursion
    (let (
          (artlist nil)
          (qstring (cdr (assq 'query query)))
	  (prefix (nnir-read-server-parm 'nnir-namazu-remove-prefix server))
          (score nil)
          (group nil)
          (article nil)
	  (process-environment (copy-sequence process-environment))
          )
      (setenv "LC_MESSAGES" "C")
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (let* ((cp-list
              `( ,nnir-namazu-program
                 nil			; input from /dev/null
                 t			; output
                 nil			; don't redisplay
                 "-q"			; don't be verbose
                 "-a"			; show all matches
                 "-s"			; use short format
                 ,@(nnir-read-server-parm 'nnir-namazu-additional-switches server)
                 ,qstring		; the query, in namazu format
                 ,(nnir-read-server-parm 'nnir-namazu-index-directory server) ; index directory
                 ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-namazu-program
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run namazu: %s" exitstatus)
          ;; Namazu failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; Namazu output looks something like this:
      ;; 2. Re: Gnus agent expire broken (score: 55)
      ;; /home/henrik/Mail/mail/sent/1310 (4,138 bytes)

      (goto-char (point-min))
      (while (re-search-forward
              "^\\([0-9]+\\.\\).*\\((score: \\([0-9]+\\)\\))\n\\([^ ]+\\)"
              nil t)
        (setq score (match-string 3)
              group (file-name-directory (match-string 4))
              article (file-name-nondirectory (match-string 4)))

        ;; make sure article and group is sane
        (when (and (string-match "^[0-9]+$" article)
                   (not (null group)))
          (when (string-match (concat "^" prefix) group)
            (setq group (replace-match "" t t group)))

          ;; remove trailing slash from groupname
          (setq group (substring group 0 -1))

          ;; stuff results into artlist vector
          (push (vector (nnir-group-full-name (substitute ?. ?/ group) server)
                        (string-to-int article)
                        (string-to-int score)) artlist)))

      ;; sort artlist by score
      (apply 'vector
             (sort* artlist
                    (function (lambda (x y)
                                (> (nnir-artitem-rsv x)
                                   (nnir-artitem-rsv y)))))))))

;;; Util Code:

(defun nnir-read-parms (query)
  "Reads additional search parameters according to `nnir-engines'."
  (let ((parmspec (caddr (assoc nnir-search-engine nnir-engines))))
    (cons (cons 'query query)
          (mapcar 'nnir-read-parm parmspec))))

(defun nnir-read-parm (parmspec)
  "Reads a single search parameter.
`parmspec' is a cons cell, the car is a symbol, the cdr is a prompt."
  (let ((sym (car parmspec))
        (prompt (cdr parmspec)))
    (if (listp prompt)
	(let* ((result (apply 'completing-read prompt))
	       (mapping (or (assoc result nnir-imap-search-arguments)
			    (assoc nil nnir-imap-search-arguments))))
	  (cons sym (format (cdr mapping) result)))
      (cons sym (read-string prompt)))))

(defun nnir-run-query (query)
  "Invoke appropriate search engine function (see `nnir-engines').
If some groups were process-marked, run the query for each of the groups
and concat the results."
  (let ((q (car (read-from-string query))))
    (if gnus-group-marked
	(apply 'vconcat
	       (mapcar (lambda (x)
			 (let ((server (nnir-group-server x))
			       search-func)
			   (setq search-func (cadr
					      (assoc
					       (nnir-read-server-parm 'nnir-search-engine server) nnir-engines)))
			   (if search-func
			       (funcall search-func q server x)
			     nil)))
		       gnus-group-marked)
	       )
      (apply 'vconcat
	     (mapcar (lambda (x)
		       (if (and (equal (cadr x) 'ok) (not (equal (cadar x) "-ephemeral")))
			   (let ((server (format "%s:%s" (caar x) (cadar x)))
				 search-func)
			     (setq search-func (cadr
						(assoc
						 (nnir-read-server-parm 'nnir-search-engine server) nnir-engines)))
			     (if search-func
				 (funcall search-func q server nil)
			       nil))
			 nil))
		     gnus-opened-servers)
	     ))
    ))

(defun nnir-read-server-parm (key server)
  "Returns the parameter value of for the given server, where server is of
form 'backend:name'."
  (let ((method (gnus-server-to-method server)))
    (cond ((and method (assq key (cddr method)))
	   (nth 1 (assq key (cddr method))))
	  ((and nnir-mail-backend
		(gnus-method-equal method nnir-mail-backend))
	   (symbol-value key))
	  ((null nnir-mail-backend)
	   (symbol-value key))
	  (t nil))))
;;     (if method
;;       (if (assq key (cddr method))
;; 	  (nth 1 (assq key (cddr method)))
;; 	(symbol-value key))
;;       (symbol-value key))
;;     ))

(defmacro nnir-group-server (group)
  "Returns the server for a foreign newsgroup in the format as gnus-server-to-method needs it. Compare to gnus-group-real-prefix and gnus-group-real-name."
  `(let ((gname ,group))
    (if (string-match "^\\([^:]+\\):" gname)
	(setq gname (match-string 1 gname))
      nil)
    (if (string-match "^\\([^+]+\\)\\+\\(.+\\)$" gname)
	(format "%s:%s" (match-string 1 gname) (match-string 2 gname))
      (concat gname ":"))
    ))

(defun nnir-group-full-name (shortname server)
  "For the given group name, return a full Gnus group name.
The Gnus backend/server information is added."
  (gnus-group-prefixed-name shortname (gnus-server-to-method server)))

(defun nnir-possibly-change-server (server)
  (unless (and server (nnir-server-opened server))
    (nnir-open-server server)))


;; Data type article list.

(defun nnir-artlist-length (artlist)
  "Returns number of articles in artlist."
  (length artlist))

(defun nnir-artlist-article (artlist n)
  "Returns from ARTLIST the Nth artitem (counting starting at 1)."
  (elt artlist (1- n)))

(defun nnir-artitem-group (artitem)
  "Returns the group from the ARTITEM."
  (elt artitem 0))

(defun nnir-artlist-artitem-group (artlist n)
  "Returns from ARTLIST the group of the Nth artitem (counting from 1)."
  (nnir-artitem-group (nnir-artlist-article artlist n)))

(defun nnir-artitem-number (artitem)
  "Returns the number from the ARTITEM."
  (elt artitem 1))

(defun nnir-artlist-artitem-number (artlist n)
  "Returns from ARTLIST the number of the Nth artitem (counting from 1)."
  (nnir-artitem-number (nnir-artlist-article artlist n)))

(defun nnir-artitem-rsv (artitem)
  "Returns the Retrieval Status Value (RSV, score) from the ARTITEM."
  (elt artitem 2))

(defun nnir-artlist-artitem-rsv (artlist n)
  "Returns from ARTLIST the Retrieval Status Value of the Nth artitem
(counting from 1)."
  (nnir-artitem-rsv (nnir-artlist-article artlist n)))

;; unused?
(defun nnir-artlist-groups (artlist)
  "Returns a list of all groups in the given ARTLIST."
  (let ((res nil)
        (with-dups nil))
    ;; from each artitem, extract group component
    (setq with-dups (mapcar 'nnir-artitem-group artlist))
    ;; remove duplicates from above
    (mapcar (function (lambda (x) (add-to-list 'res x)))
            with-dups)
    res))


;; The end.
(provide 'nnir)

;;; arch-tag: 9b3fecf8-4397-4bbb-bf3c-6ac3cbbc6664
