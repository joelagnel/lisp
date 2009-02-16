;;  trac-wiki.el --- edit trac wiki pages in emacs via XML-RPC

;; Copyright (C) 2006  Shun-ichi GOTO

;; Author: Shun-ichi GOTO <shunichi.goto@gmail.com>
;; Keywords: trac, xml-rpc, wiki, wiki-rpc
;; Version: 1.4
;; URL: http://www.meadowy.org/~gotoh/projects/trac-wiki/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


;; Overview:

;; Features:
;;   * Multiple project access.
;;   * Retrieve page from remote site and edit it with highlighting.
;;   * Commit page with version check.
;;   * Diff / Ediff between editing text and original.
;;   * Revert local edit.
;;   * Merge with most recent version if it is modified by other user.
;;   * Show history of page (but not so informative)
;;   * Preview page on Emacs with w3m (textual).
;;   * Preview page with external browser with CSS.
;;   * Search words in trac site for all pages and view result.
;;   * Completion for macro name and wiki page name in buffer.
;;
;; Requirement:
;;   * Works on most recent Emacs 22.0.50.
;;     It may work on Emacs 21 but it is not tested yet.
;;   * need xml-rpc.el with small patch for I18N (non-ascii)
;;   * need small patch for XmlRpcPlugin for I18N (non-ascii)
;;        http://trac-hacks.org/ticket/845
;;   * w3m and emacs-w3m for preview.
;;
;; Restriction:
;;   * It is not well on error handling (auth fail, spam-filtered, etc.)
;;   * Cannot run on Emacs 21 or before.
;;   * Cannot delete page version.
;;   * Cannot operates tickets.


;; Requirements:

;;  * Emacs 22.0.50 or later.
;;  * xml-rpc.el or multi-byte patched xml-rpc.el
;;  * emacs-w3m and w3m program
;;  * (server side) Trac 0.10 or later
;;  * (server side) XmlRpcPlugin and small patch to use preview.

;; xml-rpc.el is required to use this program.
;; It can be get from following URL.  (now, newest is rev 1.6)
;; http://cvs.savannah.gnu.org/viewcvs/emacsweblogs/weblogger/lisp/xml-rpc.el
;;
;; If you want to edit wiki pages with non-ASCII characters,
;; you need a patch against xml-rpc.el rev 1.6.
;; The patch or patched xml-rpc.el can be get from here:
;;   http://www.meadowy.org/~gotoh/trac-wiki/xml-rpc.el-1.6-i18n.patch
;;   http://www.meadowy.org/~gotoh/trac-wiki/xml-rpc.el

;; XmlRpcPlugin also has a problem on previewing.  There is a patch:
;; http://trac-hacks.org/ticket/845 (against r1278 @TracHacks)
;; Without this patch, previewing cause error on server side.

;; emacs-w3m can be get from:
;; http://emacs-w3m.namazu.org/index.html
;; Of course you must have w3m program because emacs-w3m use it.

;;; Configuration:

;;  Step 1. Get and enable XmlRpcPlugin on your trac site.
;;
;;  Step 2. Set project information variable `trac-projects'
;;          in your .emacs.
;;
;;  Step 3. Set proxy information variable `url-proxy-services'
;;          in your .emacs if need

;; XmlRpcPlugin can be get from TracHacs site
;;   http://trac-hacks.org/wiki/XmlRpcPlugin
;; And don't forget patch for it.
;;   http://trac-hacks.org/ticket/845 (against r1278)
;; Install it with refering the page above.  Don't forget enabling
;; plugin in trac.ini and adding permission to allow using XML-RPC.
;; For example, you may assign XML_RPC to 'authenticated' to allow
;; only for relyable users.
;;
;; To use XML-RPC, you should know "end point" URL.
;; Here is an example for trac site http://www.some.host.org/bar/
;;
;;   Trac URL      : http://www.some.host.org/proj/
;;   without login : http://www.some.host.org/proj/xmlrpc
;;   with login    : http://www.some.host.org/proj/login/xmlrpc

;; Once you know end-point url, you can setup `trac-project' variable
;; in your .emacs or scratch buffer like this:
;; (setq '(("proj1"
;;          :endpoint "http://www.some.host.org/proj/login/xmlrpc")
;;         ("proj2"
;;          :endpoint "http://www.other.net/project/login/xmlrpc")))
;; Format of this variable is alist of project information element.
;; Project information is cons of project name and property list.
;; So 1st element is project name string.
;; 2nd is property key (:endpoint symbol) and 3rd is property
;; value (end-point string).

;; To use http proxy server, you need to set proxy information as url
;; package's way because trac-wiki.el uses url package.
;;
;; See info of url pakage for more detail.
;;   ... by evalueate (Info-goto-node "(url)Proxies")

;; Finaly, set autoload for editing function like this:
;; (autoload 'trac-wiki "trac-wiki" "Trac wiki editing entry-point." t)


;;; NOTICE:

;; There is a notice for authentication.  If your target trac site
;; provides multiple authentication scheme (ex. both NTLM and BASIC)
;; and first one is not supported by url package, authentication step
;; is ignored. It's bug of url-http.el. On this case, you may encount
;; endless user/pass query.  For example, this case will be occured
;; when trac site uses mod_auth_sspi for domain/ActiveDirectory
;; authentication and allowing fallback to basic authentication. This
;; setting generates tow WWW-Authenticate: line and first one is NTLM
;; auth and url package cannot recognize it. Thus fail.
;;
;; To avoid this:
;;  - Apply following patch
;;     http://www.meadowy.org/~gotoh/trac-wiki/url-http.el-multi-auth.patch
;;
;; or
;;
;;  - set auth information by your hand into url-basic-auth-storage
;;    (or url-digets-auth-storage) variable like this:
;;
;;    (let ((auth (base64-encode-string (format "%s:%s" user pass))))
;;      (set (symbolvalue 'url-basic-real-auth-storage)
;;           '(("www.some.org:80" (realm . auth))
;;             ("www.other.net:80" ....
;;

;;; Usage:

;; You can start editing by `M-x trac-wiki`.
;; Flow of editing is:

;;  1. M-x trac-wiki
;;  2. Specify project name.
;;  3. Specify page name.
;;  4. Edit page content.
;;  5. Check difference.
;;  6. Preview page output.
;;  6. Commit it.

;; After configuration above, you can start editing by M-x trac-wiki.
;; It ask you project name with completion then ask page name to edit.
;; If you want to specify end-point URL directly, simple ENTER wihout
;; any characters on asking project name, then program asks you
;; end-point URL. trac-wiki.el accesses to the site after project or
;; end-point is specified to retrieve page names in the site.  So you
;; can use completion on entering page name. If non existing name is
;; specified, it means new page.
;;
;; Edit page content, then commit by `trac-wiki-commit' (C-c C-c).
;; You cannot save editing buffer because the page is not assigned to
;; local file. You shoul hold as modified, then commit.

;; On editing page content, the buffer is `trac-wiki-mode' which is
;; based on `text-mode'. you can specify some mode specific commands:
;;
;;   C-c C-c ... `trac-wiki-commit'
;;       Commit current editing content.
;;       Same project (end-point) is used, or ask project with C-u.
;;   C-c C-o ... `trac-wiki-edit'
;;       Edit another page in new buffer.
;;   C-c C-p ... `trac-wiki-preview'
;;       Preview current content by w3m (text base).
;;       With C-u, preview by external browser (graphical).
;;   C-c = ... `trac-wiki-diff'
;;   C-c C-d ... `trac-wiki-diff'
;;       Make diff between current content and original content
;;       With C-u,  execute ediff instead of diff.
;;   C-c C-m ... `trac-wiki-merge'
;;       Merge with most recent page content using `ediff-merge'.
;;       If not modified, turn current buffer to newest version.
;;   C-c C-u ... `trac-wiki-revert'
;;       Revert to original content discarding current modification.
;;       It shows diff and confirm you before do it.
;;   M-C-i ... `trac-wiki-complete-at-point'
;;       Complete macro name or page name on current point.
;;       The macro names are collected from "WikiMacros" page on the
;;       site (and cached).
;;   C-c C-h ... `trac-wiki-history'
;;       Show page history in other buffer.
;;       History is information returned from xmlrpc plugin.
;;       On each revision entry, you can show diff on its revision
;;       by '=' key.
;;   C-c C-s ... `trac-wiki-search'
;;       Search on project site for specified keywords.
;;       You can specify keywords and filters. The result is shown
;;       in another buffer with highlighting.
;;

;;; References:

;; - JSPWiki: Wiki RPC Interface 2
;;   http://www.jspwiki.org/Wiki.jsp?page=WikiRPCInterface2
;;
;; - XmlRpcPlugin - Trac Hacks
;;   http://trac-hacks.org/wiki/XmlRpcPlugin
;;

;;; Code:

(require 'xml-rpc)
(require 'url)
(require 'url-http)

(eval-when-compile
  (require 'cl)
  (require 'w3m nil t)			; optional
  (require 'ediff)
  (require 'hi-lock))


(defvar trac-projects
  '(("easycat-wiki"			; annonymous (read-only)
     :endpoint "http://anubhav/projects/easycat/login/xmlrpc"
     :login-name "isaac")
    ("deeproot-catalyst-wiki"			
     :endpoint "http://wiki.deeproot.in/Catalyst/login/xmlrpc"
     :login-name "isaac")
    ("trac-hacks"			; TracHacs site (need login)
     :endpoint "http://trac-hacks.org/login/xmlrpc"))
  "*List of project definitions.
The value is alist of project name and information plist.
For example:

  '((\"meadow\"
     :endpoint \"http://www.meadowy.org/meadow/login/xmlrpc\"
     :login-name \"gotoh\")
    (\"local-test\"
     :endpoint \"http://localhost/project/test/xmlrpc\"
     :login-name \"gotoh\"
     :name \"Shun-ichi Goto\")))")

(defvar trac-wiki-hidden-pages
  `("Trac\\w+"			; system pages
    ,(regexp-opt '("WikiMacros"
		   "WikiDeletePage"
		   "WikiNewPage"
		   "WikiHtml"
		   "WikiPageNames"
		   "WikiProcessors"
		   "WikiFormatting"
		   "WikiRestructuredText"
		   "WikiRestructuredTextLinks"
		   "RecentChanges"))
    "CamelCase"
    "TitleIndex"
    "Graphviz.*")			; for Graphviz plugin
  "*List of regexp to be hidden in completion of page name selection.
The purpose of this variable is to hide system help
pages (ex. CamelCase, WikiProcessors) and auto generate
pages (ex. RecentChanges, WikiMacros) which will not be edited
usualy.  On the other hand, some pages provided by default are
not hidden.  For example, WikiStart, SandBox InterWiki, etc.
Although hidden page is not shown in completion list,
you can edit existing page by specifying the exact page name.")


(defvar trac-wiki-search-default-filters '("wiki")
  "*List of search filter name to use as default.
Available filter names are:
  wiki      : search in all the wiki pages.
  ticket    : search description and comment of all the tickets.
  changeset : search commit log of all changesets.")


(defvar trac-wiki-max-history 100
  "*Maximum number of wiki page history to fetch.
See `trac-wiki-history'.")

(defvar trac-wiki-history-count 10
  "*Usual number of wiki page history to fetch.
See `trac-wiki-history'.")


;;; internal variables

(defconst trac-wiki-diff-buffer-name
  (if (<= 22 emacs-major-version)
      "*Diff*"
    "*diff*")
  "Buffer name of `diff' output.")

(defvar trac-wiki-macro-name-cache nil
  "Alist of endpoint and macro name list.")

(defvar trac-wiki-page-name-cache nil
  "Alist of endpoint and page name list.")

(defvar trac-rpc-endpoint nil)
(make-variable-buffer-local 'trac-rpc-endpoint)

(defvar trac-wiki-page-info nil)
(make-variable-buffer-local 'trac-wiki-page-info)

(defvar trac-wiki-search-keyword-hist nil)
(defvar trac-wiki-search-filter-hist nil)

(defvar trac-wiki-search-filter-cache nil
  "Alist of end-point and list of filter names supported in site.
This value is made automaticaly on first search access.")

;; font-lock

(defconst trac-wiki-link-type-keywords
  '("ticket" "comment" "report" "changeset" "log" "diff" "wiki"
    "milestone" "attachment" "source")
  "Trac link type keywords to be used in font-lock.")

(defun trac-wiki-link-face (face)
  "Evaluate BODY if not escaped, or return 'normal."
  (if (eq (char-before (match-beginning 0)) ?!)
      'shadow
    face))

;; for trac-wiki mode, simple
(defvar trac-wiki-font-lock-keywords
  `(("^\\(\\(=+\\) \\(.*\\) \\(=+\\)\\)\\(.*\\)" ; section heading
     (1 (if (string= (match-string 2) (match-string 4))
	    'bold
	  ;; Warn if starting/ending '='  count is not ballanced.
	  'font-lock-warning-face))
     (5 'shadow))
    ("^=.*" . font-lock-warning-face)	   ; invalid section heading
    ("`[^`\n]*`" . 'shadow)		   ; inline quote
    ("\\(''+\\)[^'\n]*\\(''+\\)"	   ; bold and italic
     (0 (let ((b (match-string 1))
	      (e (match-string 2)))
	  (if (not (string= b e))
	      font-lock-warning-face
	    (cond
	     ((string= b "''") 'italic)
	     ((string= b "'''") 'bold)
	     ((string= b "''''") 'bold-italic))))))
    ("\\[\\[[^]\n]+\\]\\]\\|{[0-9]+}"	; macro, {1}
     ;; font-lock-preprocessor-face is not defined in emacs 21
     (0 (trac-wiki-link-face font-lock-type-face)))
    ("\\[\\(\\w+:\\)?\\([^] #\n]*\\)[^]\n]*\\]" ; bracket trac link
     (0 (let ((whole (match-string 0))
	      (scheme (match-string 1))
	      (name (match-string 2)))
	  (trac-wiki-link-face
	   (cond
	    ((save-match-data	 ; [1], [1/trunk], [../file], [/trunk]
	       (string-match "^\\[[1-9./]" whole))
	     font-lock-function-name-face)
	    ((or (string= scheme "wiki:")
		 (null scheme))
	     (if (trac-wiki-page-exist-p name)
		 font-lock-function-name-face
	       font-lock-warning-face))
	    (t
	     font-lock-function-name-face))))))
    (,(format "\\(?:%s\\):\\(?:\"[^\"\n]*\"\\|[^ \t\n]\\)+" ; types
	      (regexp-opt trac-wiki-link-type-keywords))
     (0 (trac-wiki-link-face font-lock-function-name-face)))
    ("\\w+://[^ \t]+" . font-lock-function-name-face)	  ; raw url
    ("\\(?:#\\|\\br\\)[0-9]+\\(?::[0-9a-z]+\\)?\\b" ; r123 or #123
     (0 (trac-wiki-link-face font-lock-function-name-face)))
    ("\\([A-Z][a-z0-9]+\\(?:[A-Z][a-z0-9]+\\)+\\)\\(?:#[^] ]+\\)?\\b" ; camel case
     (0 (trac-wiki-link-face
	 (if (trac-wiki-page-exist-p (match-string 1))
	     font-lock-function-name-face
	   font-lock-warning-face))))
    ("||" . 'shadow)			; table delimiter
    )
  "For `trac-wiki-mode'.")

;; for history buffer
(defvar trac-wiki-history-font-lock-keywords
  '(("^\\([^:]+:\\) +\\(.*\\)"
     (1 'bold)
     (2 'shadow)))
  "For history buffer.")

;; for search result
(defvar trac-wiki-search-result-font-lock-keywords
  '(("^\\([^ \n]+\\):.*"
     (1 'bold))
    ("^  \\(http://.*\\)"
     (1 font-lock-function-name-face))
    ("^\\(By \\w+ -- [-: 0-9]+\\)\n\n"
     (1 font-lock-type-face)))
  "For search result buffer.")


;; history holder
(defvar trac-rpc-project-history nil)
(defvar trac-rpc-endpoint-history nil)
(defvar trac-wiki-page-history nil)
(defvar trac-wiki-comment-history nil)

;; key map

(defvar trac-wiki-mode-map (make-sparse-keymap))

(define-key trac-wiki-mode-map "\C-c\C-c" 'trac-wiki-commit)
(define-key trac-wiki-mode-map "\C-c=" 'trac-wiki-diff)
(define-key trac-wiki-mode-map "\C-c\C-d" 'trac-wiki-diff)
(define-key trac-wiki-mode-map "\C-c\C-u" 'trac-wiki-revert)
(define-key trac-wiki-mode-map "\C-c\C-m" 'trac-wiki-merge)
(define-key trac-wiki-mode-map "\C-c\C-p" 'trac-wiki-preview)
(define-key trac-wiki-mode-map "\C-c\C-l" 'trac-wiki-history)
(define-key trac-wiki-mode-map "\C-c\C-o" 'trac-wiki-edit)
(define-key trac-wiki-mode-map "\C-c\C-s" 'trac-wiki-search)
(define-key trac-wiki-mode-map "\C-\M-i" 'trac-wiki-complete-at-point)

(define-key trac-wiki-mode-map "\C-x\C-s" 'trac-wiki-save)


(defvar trac-wiki-search-result-mode-map nil)
(let ((map (make-sparse-keymap)))
  (define-key map "n" 'trac-wiki-search-result-next)
  (define-key map "p" 'trac-wiki-search-result-prev)
  (define-key map "e" 'trac-wiki-search-result-edit)
  (define-key map "o" 'trac-wiki-search-result-edit)
  (define-key map "\C-c\C-s" 'trac-wiki-search)
  (define-key map "s" 'trac-wiki-search)
  (define-key map "q" 'trac-wiki-delete-window-or-bury-buffer)
  (setq trac-wiki-search-result-mode-map map))

;; accessor

(defsubst trac-wiki-page-version ()
  "Get page version of current page in buffer."
  (if (null trac-wiki-page-info)
      (error "Page information is not exist!"))
  (cdr (assoc "version" trac-wiki-page-info)))

(defsubst trac-wiki-page-name ()
  "Get page name of current page in buffer."
  (if (null trac-wiki-page-info)
      (error "Page information is not exist!"))
  (cdr (assoc "name" trac-wiki-page-info)))

(defsubst trac-wiki-page-hash ()
  "Get hash value of original page content in buffer."
  (if (null trac-wiki-page-info)
      (error "Page information is not exist!"))
  (cdr (assoc "hash" trac-wiki-page-info)))

;; predicate
(defun trac-wiki-page-exist-p (page)
  "Return non-nil if PAGE exists in page name cache or no cache.
Note that if buffer has end-point information, return also non-nil
because we cannot get cache data.  In other word, \"I don't know\"
is non-nil."
  (let ((pages (and (boundp 'trac-rpc-endpoint)
		    (cdr (assoc trac-rpc-endpoint
				trac-wiki-page-name-cache)))))
    (or (null pages)
	(member page pages))))

;; cache macro
(defmacro trac-wiki-with-cache (cache-name ep no-cache &rest body)
  "Update CACHE-NAME for EP regarding NO-CACHE with result of BODY.
CACHE-NAME is symbol of variable which is cache data storage formated
as alist of end-point and cache data.
EP is end-point string and works as key of cache data to select.
If NO-CACHE is nil, return data in cache if exist without executing
BODY.  If NO-CACHE is non-nil, always run BODY and update cache with its
result data."
  `(let* ((entry (assoc ,ep (symbol-value ,cache-name)))
	  (data (if (and (not ,no-cache) entry)
		    (cdr entry)
		  ,@body)))
     (if (null entry)
	 (set ,cache-name (cons (cons ,ep data)
				(symbol-value ,cache-name))))
     data))
(put 'trac-wiki-with-cache 'lisp-indent-function 3)


;; mode

(define-derived-mode trac-wiki-mode text-mode "Trac[Wiki]"
  "Trac Wiki authorizing mode with XML-RPC access."
  (set (make-local-variable 'font-lock-defaults)
       '(trac-wiki-font-lock-keywords t))
  (require 'font-lock)
  (if font-lock-mode
      (font-lock-fontify-buffer)))


;; XML-RPC functions

(when (eq emacs-major-version 21)
  (defadvice url-retrieve (before trac-wiki
				  (url &optional callback args) activate)
    "Bug workaround advice for Emacs 21 and w3-url-e21(2005.10.23-5).
Url package (debian) for Emacs 21.4 has a bug. The function
url-retrieve-synchronously<f> does not pass callback argument but
url-http-handle-authentication<f> expects url is 1st element of
callback argument. This advice fakes for this."
    (if (null args)
	(setq args (list url)))))


(defun trac-rpc-call (method &rest args)
  "Call METHOD with ARGS via XML-RPC and return response data.
WARNING: This functionis not use because synchronous
`xml-rpc-method-call' has strange behavour on authentication
retrying.  Use `trac-rpc-call-async' instead."
  (let ((result (apply 'xml-rpc-method-call
		       trac-rpc-endpoint method args)))
    (if (and (numberp result) (= result 0))
	nil
      result)))

(defvar trac-rpc-working-buffers nil
  "Internal list to hold working buffers to be deleted later")

(defvar trac-rpc-callback-flag nil
  "Internal flag variable set when callbacked from xml-rpc call.")

(defun trac-rpc-get-page (page &optional version)
  "Get content of PAGE in VERSION invoking XML-RPC call.
If VERSION is omitted, most recent version is selected."
  (if version
      (trac-rpc-call 'wiki.getPageVersion page version)
    (trac-rpc-call 'wiki.getPage page)))

(defun trac-rpc-get-page-info (page &optional version)
  "Get information of PAGE in VERSION invoking XML-RPC call.
If VERSION is omitted, most recent version is selected."
  (if version
      (trac-rpc-call 'wiki.getPageInfoVersion page version)
    (trac-rpc-call 'wiki.getPageInfo page)))

(defun trac-rpc-get-page-html (page &optional version)
  "Get rendered content of PAGE in VERSION invoking XML-RPC call.
If VERSION is omitted, most recent version is selected."
  (if version
      (trac-rpc-call 'wiki.getPageHTMLVersion page version)
    (trac-rpc-call 'wiki.getPageHTML page)))

(defun trac-rpc-get-all-pages (&optional endpoint)
  "Get list of page names available in remote site of ENDPOINT.
If optional argument EP is nil, use `trac-rpc-endpoint' is used."
  (let ((trac-rpc-endpoint (or endpoint trac-rpc-endpoint)))
    (trac-rpc-call 'wiki.getAllPages)))

(defun trac-rpc-put-page (page content comment)
  "Update PAGE as CONTENT with COMMENT.
COMMENT can be nil."
  (let ((attributes `(("comment" . ,(or comment "")))))
    (trac-rpc-call 'wiki.putPage page content attributes)))

(defun trac-rpc-wiki-to-html (content)
  "Covnert wiki CONTENT into html via XML-RPC method call."
  (trac-rpc-call 'wiki.wikiToHtml content))


(defun trac-rpc-get-page-version (&optional page)
  "Get latest version of PAGE in remote."
  (if (or (null trac-wiki-page-info)
	  (null trac-rpc-endpoint))
      (error "Page information is not exist!"))
  (let ((info (trac-rpc-get-page-info
	       (or page (trac-wiki-page-name)))))
    (if (null info)
	0				; no page, return version 0
      (cdr-safe (assoc "version" info)))))


(defun trac-wiki-read-page-name ()
  "Enter page name with competion."
  (let ((all (trac-wiki-with-cache
		 'trac-wiki-page-name-cache
		 trac-rpc-endpoint 'update
	       (trac-rpc-get-all-pages)))
	(re (concat "^\\(?:"
		    (mapconcat 'identity trac-wiki-hidden-pages "\\|")
		    "\\)\\(?:\\.[a-z]\\{2\\}\\)?$"))
	pages page)
    (dolist (page all)
      (unless (string-match re page)
	(add-to-list 'pages page)))
    (while (null page)
      (setq page (completing-read "Page name: "
				  (mapcar 'list pages)
				  nil nil nil 'trac-wiki-page-history)))
    page))

(defun trac-wiki-save ()
  "Alternative function to avoid usual file save function."
  (interactive)
  (message "You cannot save this buffer to file. Use %s to commit."
	   (substitute-command-keys "\\[trac-wiki-commit]")))

(defun trac-wiki-ask-endpoint ()
  "Prompts to enter project name and return its endpoint.
If simply hit enter on asking project name, ask again for
raw end-point url."
  (let* ((project (completing-read "Select project: "
				   trac-projects
				   nil t nil
				   'trac-rpc-project-history))
	 (pinfo (and project
		     (cdr (assoc project trac-projects)))))
    (or (and pinfo (plist-get pinfo :endpoint))
	(read-string "Endpoint URL: "
		     trac-rpc-endpoint
		     'trac-rpc-endpoint-history))))

(defun trac-wiki ()
  "Initial interface to edit trac wiki page.
You can select trac project by name which is pre-defined,
or enter raw URL of XML-RPC endpoint."
  (interactive)
  (let ((ep (trac-wiki-ask-endpoint)))
    (if (not (string-match "\\`https?://[^ ]+/\\(?:.*/\\)?xmlrpc" ep))
	(message "Not valid trac XML-RPC endpoint! (canceled)")
      (trac-wiki-visit ep))))

(defun trac-wiki-edit (&optional ask-endpoint)
  "Retreive wiki page content with new buffer.
If with prefix argument ASK-ENDPOINT, force asking end-point instead
of current buffer's one."
  (interactive "P")
  (let ((ep (or (and (not ask-endpoint) trac-rpc-endpoint)
		(trac-wiki-ask-endpoint))))
    (if (or (null ep)
	    (string= ep ""))
	(error "Invalid endpoint: %s" trac-rpc-endpoint))
    (trac-wiki-visit ep)))

(defun trac-wiki-visit (endpoint &optional page)
  "By accessing to ENDPOINT, visit to PAGE to edit.
If PAGE is nil, you can input page name interactively with completion.
Page names for completion are retrieved by fetching list of all the
pages in remote site via XML-RPC call."
  ;; this is workaround for bug of url-generic-parse-url<f>
  ;; (bug is found in url-parse.el rev 1.13)
  (url-generic-parse-url endpoint)
  ;; clean up wasted buffer named as " *http:www.xxx.xxx .....*"
  (dolist (buf (buffer-list))
    (if (and (string-match "^ \\*http" (buffer-name buf))
	     (get-buffer-process buf)
	     (not (member (process-status (get-buffer-process buf))
			  '(open connect run))))
	(kill-buffer buf)))
  ;; main
  (let ((ep endpoint))
    (if (null page)
	(let ((trac-rpc-endpoint ep))
	  (setq page (trac-wiki-read-page-name))
	  (if (or (null page) (string= page ""))
	      (error "Page name should be specified"))))
    ;; FIXME: check existing buffer which is for same page.
    (switch-to-buffer (generate-new-buffer (format "%s" page)))
    (erase-buffer)
    (trac-wiki-mode)
    (setq trac-rpc-endpoint ep)
    (let ((info (trac-rpc-get-page-info page)))
      (if (null info)
	  (progn
	    ;; make dummy information
	    (setq trac-wiki-page-info `(("version" . 0)
					("name" . ,page)
					("lastModified" .
					 ,(format-time-string "%Y%m%dT%T"))
					("hash" . ,(md5 ""))))
	    (message "new page"))
	(insert (trac-rpc-get-page page))
	(goto-char (point-min))
	(trac-wiki-update-page-info page info))
      (set-buffer-modified-p nil)
      (message "Page is retrieved (version = %s)"
	       (trac-wiki-page-version)))))

(defun trac-wiki-update-page-info (page &optional info)
  "Update information of PAGE as INFO.
If INFO is not specified, information is retrieved via XML-RPC call.
This information is page specific data holded as buffer local variable."
  (setq info (or info (trac-rpc-get-page-info page)))
  (when (eq info 0)
    ;; case of new page (no page information exists).
    (setq info `(("name" . ,page)
		 ("version" . 0))))
  (add-to-list 'info `("hash" . ,(md5 (buffer-string) nil nil 'utf-8)))
  (setq trac-wiki-page-info info))

(defun trac-wiki-commit ()
  "Commit current content to remote site.
Before commit, check the version of this page in remote site is match
with local version.  If not matched, show warning and do merging.
If local content is not changed, confirm doing."
  (interactive)
  (if (null trac-rpc-endpoint)
      (error "This buffer is not managed as trac wiki mode")
  (cond
   ((not (buffer-modified-p))
    (message "Nothing changed."))
   ((and (string= (trac-wiki-page-hash)
		  (md5 (buffer-string) nil nil 'utf-8))
	 (not (y-or-n-p "Buffer seems to be same.  Commit it? ")))
    (message "canceled."))
   ((/= (trac-rpc-get-page-version)
	(trac-wiki-page-version))
    (if (y-or-n-p (format "Remote page is updated (version: local=%s, remote=%s).  Merge with it? "
			  (trac-wiki-page-version)
			  (trac-rpc-get-page-version)))
	(trac-wiki-merge)
      (message "commit canceled.")))
   (t
    ;; do it
    (let ((comment (read-string "Comment: " nil 'trac-wiki-comment-history))
	  (page (trac-wiki-page-name)))
      (trac-rpc-put-page page (buffer-string) comment)
      ;; update new info
      (trac-wiki-update-page-info page)
      (set-buffer-modified-p nil)
      ;; TODO: add this page name into cache?
      (message "Committed as version %s" (trac-wiki-page-version)))))))

(defun trac-wiki-modified-p ()
  "Return non-nil if buffer content is modified.
Jadgement of 'modified' is done by `buffer-modified-p' flag
and comparation of MD5 hash with current and original content.
Note that return nil if MD5 is equal althogh `buffer-modified-p' is non-nil."
  (let ((modified (and (buffer-modified-p)
		       (not (string= (trac-wiki-page-hash)
				     (md5 (buffer-string) nil nil 'utf-8))))))
    modified))
	
(defun trac-wiki-revert ()
  "Revert to original content with discarding local change."
  (interactive)
  (if (not (trac-wiki-modified-p))
      (message "Nothing changed.")
    (let ((config (current-window-configuration)))
      (trac-wiki-diff nil)
      (if (not (y-or-n-p "Really revert these changes? "))
	  (message "canceled.")
	(erase-buffer)
	(let ((page (trac-wiki-page-name))
	      (ver (trac-wiki-page-version)))
	  (insert (trac-rpc-get-page page ver))
	  (set-buffer-modified-p nil)
	  (goto-char 1)
	  (message "Reverted to original (version=%s)" ver)))
      (set-window-configuration config)
      ;; erase diff buffer
      (let ((win (get-buffer-window trac-wiki-diff-buffer-name)))
	(if win
	    (delete-window win))))))


(defun trac-wiki-diff (arg)
  "Diff with original version in remote.
If with prefix ARG, invoke `ediff' instead of `diff'."
  ;; WISH: If content is not so big, it is better that the original
  ;;  text is holded in local variable, and we don't need access to
  ;;  server for diff.
  (interactive "P")
  (if arg
      (trac-wiki-ediff)
    ;; clean up diff output window
    (let ((win (get-buffer-window trac-wiki-diff-buffer-name)))
      (if win
	  (delete-window win)))
    ;; check and confirm unmodified guess.
    (if (not (trac-wiki-modified-p))
	(message "Nothing changed.")
      (let* ((page (trac-wiki-page-name))
	     (version (trac-wiki-page-version)))
	(if (= 0 version)
	    (message "No need to diff. This is initial version.")
	  (let ((orig (trac-rpc-get-page page version)))
	    (if (null orig)
		(error "Error on fetching page: %s, version %s" page version))
	    (trac-wiki-diff-internal (buffer-substring-no-properties
				      (point-min) (point-max)) orig)))))))

(defun trac-wiki-diff-internal (str1 str2)
  "Show diff of two content STR1 and STR2 in popup buffer."
  (let* ((trac-rpc-endpoint trac-rpc-endpoint)
	 (page (trac-wiki-page-name))
	 (tmpa (make-temp-file "wiki"))
	 (tmpb (make-temp-file "wiki")))
    (unwind-protect
	(progn
	  (with-temp-file tmpa
	    (insert str2))
	  (with-temp-file tmpb
	    (insert str1))
	  (require 'diff)
	  (condition-case nil
	      (diff tmpa tmpb nil 'no-async) ; for emacs 22.50 or later
	    (error
	     ;; for emacs 21 or before
	     (diff tmpa tmpb)))
	  (with-current-buffer (get-buffer trac-wiki-diff-buffer-name)
	    ;; wait for process completion, required for Emacs 21
	    (let ((proc (get-buffer-process (current-buffer))))
	      (while (and proc
			  (member (process-status proc)
				  '(run open connect)))
		(accept-process-output proc)))
	    (local-set-key "q" 'trac-wiki-delete-window-or-bury-buffer)
	    (let ((win (get-buffer-window (current-buffer))))
	      (if (not (re-search-forward "^Diff finished (no differences)."
					  nil t))
		  (shrink-window-if-larger-than-buffer win)
		(delete-window win)
		(message "No difference.")))))
      (delete-file tmpa)
      (delete-file tmpb))))
    
(defun trac-wiki-ediff ()
  "Invoke `ediff' with original content."
  (interactive)
  (let* ((mode major-mode)
	 (page (trac-wiki-page-name))
	 (version (trac-wiki-page-version)))
    (if (= version 0)
	(message "No need to ediff. This is initial version.")
      (let ((content (trac-rpc-get-page page version))
	    (buf (generate-new-buffer (format "*%s BASE*" page))))
	(when (null buf)
	  (kill-buffer buf)
	  (error "Error on fetching page: %s, version %s" page version))
	(with-current-buffer buf
	  (erase-buffer)
	  (insert content)
	  (funcall mode)
	  (set-buffer-modified-p nil))
	(ediff-buffers (current-buffer) buf)))))


(defvar trac-wiki-merge-windows nil)
(defvar trac-wiki-merge-buffer nil)
(defvar trac-wiki-merge-page-info nil)

(defun trac-wiki-merge ()
  "Merge with most recent version if exist."
  (interactive)
  (let* ((page (trac-wiki-page-name))
	 (page-info (trac-rpc-get-page-info page)) ; of recent one
	 (rver (cdr (assoc "version" page-info)))
	 (lver (trac-wiki-page-version)))
    (cond
     ((= lver rver)
      (message "This page is up-to-date. No need to merge."))
     ((< rver lver)
      (error "Remote version is smaller than me.  Page might be deleted"))
     (t
      (let* ((mode major-mode)
	     (config (current-window-configuration))
	     (cur (current-buffer))
	     (ver lver)
	     (their (or (trac-rpc-get-page page)
			(error "Cannot get page content: %s (newest:ver=%s)"
			       page rver)))
	     (base (or (trac-rpc-get-page page ver)
		       (error "Cannot get page content: %s (base:ver=%s)"
			      page lver)))
	     (mine-buf (generate-new-buffer (format "*%s MINE*" page)))
	     (their-buf (generate-new-buffer (format "*%s OTHER*" page)))
	     (base-buf (generate-new-buffer (format "*%s BASE*" page))))
	(require 'ediff)
	(with-current-buffer mine-buf
	  (insert-buffer-substring cur)
	  (funcall mode))
	(with-current-buffer their-buf
	  (insert their)
	  (funcall mode))
	(with-current-buffer base-buf
	  (insert base)
	  (funcall mode))
	;; start merging
	(set-buffer
	 (ediff-merge-buffers-with-ancestor mine-buf their-buf base-buf))
	;; prepare for sentinel action
	(set (make-local-variable 'trac-wiki-merge-windows) config)
	(set (make-local-variable 'trac-wiki-merge-buffer) cur)
	(set (make-local-variable 'trac-wiki-merge-page-info)
	     (append page-info `("hash" (md5 (buffer-string)))))
	(set (make-local-variable 'ediff-quit-hook) 'trac-wiki-merge-sentinel)
	(message "Please merge with recent version."))))))
	       

(defun trac-wiki-merge-sentinel ()
  "Called by `ediff-quit-hook' for cleanup and aplying merge result."
  (let ((buffers (list ediff-buffer-A ediff-buffer-B ediff-buffer-C
		       ediff-ancestor-buffer))
	(merged ediff-buffer-C)
	(windows trac-wiki-merge-windows)
	(page-info trac-wiki-merge-page-info)
	(buf trac-wiki-merge-buffer))
    (ediff-cleanup-mess)
    (if (not (y-or-n-p "Accept merge result? "))
	(message "Discarded merge result and stay on version %s."
		 (trac-wiki-page-version))
      (with-current-buffer buf
	(let ((buffer-read-only nil))
	  (erase-buffer)
	  (insert-buffer-substring merged)
	  (setq trac-wiki-page-info page-info))
	(message "Merged with version %s and now on it."
		 (cdr (assoc "version" page-info)))))
    ;; kill all the buffuers
    (dolist (buf buffers)
      (if (bufferp buf)
	  (kill-buffer buf)))
    (set-window-configuration windows)))


(defun trac-wiki-strip-url-trailer (url trailers)
  "Return modified URL removing trailing words specfied in TRAILERS.
TRAILERS is list of string to be removed."
  (let ((re (concat "\\(?:/\\(?:" (regexp-opt trailers) "\\)\\)+$")))
    (if (string-match re url)
	(substring url 0 (match-beginning 0))
      url)))

(defun trac-wiki-preview (arg)
  "Preview current wiki content as html page.
Usualy this function requests conversion to html via XML-RPC
then render in Emacs buffer with `w3m' feature (if available)
With prefix ARG, execute browser using `browse-url' to preview
html.  It supports styles sheet."
  (interactive "P")
  (let ((html (trac-rpc-wiki-to-html (buffer-string)))
	(buf (get-buffer-create (if arg " *html-preview-tmp*" "*preview*")))
	(name (trac-wiki-page-name))
	;; this depen on trac url structure
	(base-url (trac-wiki-strip-url-trailer trac-rpc-endpoint
					       '("xmlrpc" "login" "wiki"))))
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only t)
      (let ((buffer-read-only nil)
	    (css (mapconcat
		  (lambda (x)
		    (format "<link rel='stylesheet' href='%s%s%s' type='text/css' />"
			    base-url "/chrome/common/css/" x))
		  '("trac.css" "wiki.css" "site_common.css")
		  "\n")))
	(erase-buffer)
	;; add some supplements as valid html content
	(insert (format "<html><head><title>%s (preview)</title>" name)
		"\n"
		css
		"\n</head><body>\n"
		"<div id='content' class='wiki'><div class='wikipage'>"
		html
		"</div></div></body>")
	;; replace links
	(goto-char (point-min))
	(while (re-search-forward "\\(?:href\\|src\\)=\"/" nil t)
	  (backward-char 1)
	  (insert base-url))
	(if arg
	    (progn
	      (require 'browse-url)
	      (browse-url-of-buffer)
	      (message "Previewing with external browser."))
	  (require 'w3m)
	  (w3m-region (point-min) (point-max))
	  (pop-to-buffer buf)
	  ;; define 'q' key to close preview buffer
	  (local-set-key "q" 'trac-wiki-delete-window-or-bury-buffer)
	  (message "Hit 'q' to quit preview window"))))))

(defun trac-wiki-html2text-string (str)
  "Return plain text string convert from html markup'ed STR."
  (require 'w3m)
  (with-temp-buffer
    (insert str)
    (require 'w3m)
    (w3m-region (point-min) (point-max))
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (buffer-substring (point-min) (point))))

(defun trac-wiki-search (query &optional filters)
  "Search QUERY keywords on remote trac.
Keywords and filters can be specified.
FILTERS is interactively selected if not specified."
  (interactive
   (list
    ;; enter query string
    (let ((str (read-string "Query string: " nil
			    'trac-wiki-search-keyword-hist)))
      (if (or (null str) (string= str ""))
	  (error "Query string must be specified"))
      str)
    ;; select filters
    (let ((filters (trac-wiki-with-cache
		       'trac-wiki-search-filter-cache
		       trac-rpc-endpoint nil
		     (mapcar 'car
			     (trac-rpc-call 'search.getSearchFilters)))))
      (completing-read-multiple "Select filters: "
				(mapcar 'list filters)
				nil t
				(mapconcat 'identity
					   trac-wiki-search-default-filters
					   ",")
				'trac-wiki-search-filter-hist
				"wiki"))))
  ;; args are prepared,
  ;; try search request
  (let ((result (trac-rpc-call 'search.performSearch query filters))
	(ep trac-rpc-endpoint)
	(buf (get-buffer-create "*search result*")))
    ;; close last result first
    (if (get-buffer-window buf)
	(delete-window (get-buffer-window buf)))
    (if (null result)
	(message "No match.")
      (with-current-buffer buf
	(let ((buffer-read-only nil))
	  (erase-buffer)
	  (require 'hi-lock)
	  (if hi-lock-mode
	      (hi-lock-mode 0))
	  (dolist (elem result)
	    (setq elem (mapcar
			(lambda (x) (replace-regexp-in-string
				     "[ \t\r\n]+" " " x))
			elem))
	    ;; elem := (href title date author excerpt)
	    (require 'url-util)
	    (let ((url (nth 0 elem))
		  (title (nth 1 elem))
		  (date (nth 2 elem))
		  (author (nth 3 elem))
		  (excerpt (nth 4 elem)))
	      (if (string-match "<[a-z]+.*>" title)
		  (setq title (trac-wiki-html2text-string title)))
	      (insert title "\n"	; title
		      "  "
		      (decode-coding-string
		       (url-unhex-string (string-as-unibyte url))
		       'utf-8)
		      "\n"		; href
		      excerpt "\n"
		      (format "By %s -- %s\n"
			      author
			      (format-time-string
			       "%Y-%m-%d %H:%M:%S"
			       (seconds-to-time (string-to-number date))))
		      "\n"))))
	(goto-char (point-min))
	;; setup font-lock
	(set (make-local-variable 'font-lock-defaults)
	     '(trac-wiki-search-result-font-lock-keywords t t))
	(if font-lock-mode
	    (font-lock-default-fontify-buffer)) ; immediately
	;; highlight search keywords
	(let ((colors '(hi-yellow hi-blue hi-green hi-pink)))
	  (dolist (q (split-string (downcase query)))
	    (highlight-regexp q (or (car-safe colors)
				    'highlight))
	    (setq colors (cdr-safe colors))))
	(setq trac-rpc-endpoint ep)
	;; local keys
	(use-local-map trac-wiki-search-result-mode-map)
	(setq buffer-read-only t))
      (pop-to-buffer buf))))

(defconst trac-wiki-search-result-top-regexp
  "\\(?:\n\\|\\`\\)\\([^ :\n]+\\):")

(defun trac-wiki-search-result-next ()
  "Move to next entry."
  (interactive)
  (end-of-line)
  (if (re-search-forward trac-wiki-search-result-top-regexp nil t)
      (goto-char (match-beginning 1))
    (message "No more entry")))

(defun trac-wiki-search-result-prev ()
  "Move to prev entry."
  (interactive)
  (beginning-of-line)
  (if (re-search-backward trac-wiki-search-result-top-regexp nil t)
      (goto-char (match-beginning 1))
    (message "No more entry")))
  
(defun trac-wiki-search-result-get-page-name ()
  "Get page name of entry at point."
  (interactive)
  (save-excursion
    (end-of-line)
    (if (re-search-backward trac-wiki-search-result-top-regexp nil t)
	(match-string-no-properties 1))))
  
(defun trac-wiki-search-result-edit ()
  "Edit page of entry at point."
  (interactive)
  (let ((page (trac-wiki-search-result-get-page-name)))
    (cond
     ((string-match "^\\`#[1-9]+\\'" page)
      (message "Sorry, ticket cannot be edited."))
     ((y-or-n-p (format "Edit this page [%s]? " page))
      (trac-wiki-visit trac-rpc-endpoint page))
     (t (message "")))))

(defun trac-wiki-complete-at-point (no-cache)
  "Do completion for the wiki page or wiki macro at point.
Completion candidates are collected from remote site and cached
localy.  So second completion works fast with cache if exist.
With prefix arg NO-CACHE, it means canceling current cache and update
with data retrieved from remote site again.

NOTE: Wiki macro names are retrieved from HTML contents of
WikiMacros page on remote site."
  (interactive "P")
  (let ((ep (or trac-rpc-endpoint
		(error "XML-RPC endpoint is not known")))
	kind candidates part)
    (cond
     ;; macro completion
     ((trac-wiki-looking-back "\\[\\[\\(\\w*\\)")
      (setq kind "macro"
	    part (match-string 1)
	    candidates (append (list "BR")
			       (trac-wiki-with-cache
				   'trac-wiki-macro-name-cache
				   ep no-cache
				 (trac-wiki-collect-macro-names)))))
     ((trac-wiki-looking-back "\\[wiki:\\(\\(?:\\w\\|[/.]\\)*\\)")
      ;; wiki link
      (setq kind "page name"
	    part (match-string 1)
	    candidates (trac-wiki-with-cache
			   'trac-wiki-page-name-cache
			   ep no-cache
			 (trac-rpc-get-all-pages))))
     ((trac-wiki-looking-back "\\(^\\|\\W\\)\\([A-Z]\\(\\w\\|[/.]\\)*\\)")
      ;; camel case wiki name
      (setq kind "wiki name"
	    part (match-string 2)
	    candidates (trac-wiki-with-cache
			   'trac-wiki-page-name-cache
			   ep no-cache
			 (trac-rpc-get-all-pages)))))
    
    (when (and kind candidates part)
      (let* ((pos (point))
	     (beg (- pos (length part))))
	;; try completion
	(let ((cmpl (try-completion part (mapcar 'list candidates))))
	  (cond
	   ((null cmpl)
	    (message "no matching %s" kind))
	   ((eq cmpl t)
	    (message "Sole completion"))
	   (t
	    (let ((repl (if (string= cmpl part)
			    (completing-read (format "[%s] " kind)
					     (mapcar 'list candidates)
					     nil t part)
			  cmpl)))
	      (kill-region beg pos)
	      (insert repl)
	      (if (eq t (try-completion repl (mapcar 'list candidates)))
		  (message "Sole completion")
		(message "Complete, but not uniq"))))))))))

(defun trac-wiki-history (arg)
  "Show history of visiting page in popup'ed buffer.
In history buffer, you can get diff of each versions.
Maximum count of history is limited by `trac-wiki-max-history'.
If with prefix ARG, all the history is displayed but it might slow
if too many version exists."
  (interactive "P")
  (let* ((page-info trac-wiki-page-info)
	 (page (trac-wiki-page-name))
	 (current (trac-wiki-page-version))
	 (ver (trac-rpc-get-page-version))
	 (buf (get-buffer-create (format " *page history*")))
	 (ep trac-rpc-endpoint)
	 (keyhelp (substitute-command-keys "\\[trac-wiki-history]"))
	 (rest (if arg trac-wiki-max-history
		 trac-wiki-history-count))
	 info)
    (save-excursion
      (set-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (message "fetching version info...")
      (let ((trac-rpc-endpoint ep)
	    info)
	(insert "--- History of " page " ---")
	(while (and (< 0 rest) (< 0 ver))
	  (setq info (condition-case nil
			 (trac-rpc-get-page-info page ver)
		       (error nil)))
	  (when (and info (listp info))
	    (setq rest (1- rest))
	    (insert (format "\nversion:  %s" (cdr (assoc "version" info))))
	    (insert "\nmodified: "
		    (trac-wiki-convert-to-readable-time-string
		     (cdr (assoc "lastModified" info))))
	    (insert "\nauthor:   " (cdr (assoc "author" info)))
	    (if (assoc "comment" info)
		(insert "\ncomment:  " (or (cdr-safe (assoc "comment" info))
					   "")))
	    (insert "\n"))
	  (setq ver (1- ver))))
      (message "fetching version info...done")
      (if (and (null arg) (< 1 ver))
	  (insert "\n... to show more versions, use C-u " keyhelp "\n"))
      (setq buffer-read-only t)
      (re-search-backward (format "version: +%s" current) nil t)
      (trac-wiki-history-mode)
      (setq trac-rpc-endpoint ep
	    trac-wiki-page-info page-info)
      (pop-to-buffer buf)
      (shrink-window-if-larger-than-buffer))))

(define-derived-mode trac-wiki-history-mode text-mode "trac-wiki-history"
  "History operation mode"
  (setq buffer-read-only t)
  (set (make-local-variable 'font-lock-defaults)
       '(trac-wiki-history-font-lock-keywords t))
  (if font-lock-mode
      (font-lock-fontify-buffer))
  (local-set-key "?" 'trac-wiki-history-help)
  (local-set-key "q" 'trac-wiki-delete-window-or-bury-buffer)
  (local-set-key "=" 'trac-wiki-history-diff)
  (local-set-key "n" 'trac-wiki-history-next)
  (local-set-key "p" 'trac-wiki-history-prev)
  (trac-wiki-history-help))

(defun trac-wiki-history-help ()
  "Show small help in echo area."
  (interactive)
  (message "[help] =:diff, n:next, p:prev, q:quit, ?:help"))

(defun trac-wiki-history-next ()
  "Move to next entry."
  (interactive)
  (if (not (re-search-forward "version: +" nil t))
      (message "No more older version.")))

(defun trac-wiki-history-prev ()
  "Move to previous entry."
  (interactive)
  (let ((pos (point)))
    (beginning-of-line)
    (while (looking-at "\\w+:")
      (forward-line -1))
    (if (not (re-search-backward "version: +" nil t))
	(message "No more newer version.")
      (goto-char (match-end 0)))))

(defun trac-wiki-history-diff ()
  "Show change of this version as diff output in popup buffer."
  (interactive)
  (let (ver1 ver2)
    (save-excursion
      (end-of-line)
      (if (not (re-search-backward "version: +\\([0-9]+\\)" nil t))
	  (message "No version")
	(setq ver1 (string-to-number (match-string 1)))
	(end-of-line)
	(if (not (re-search-forward "version: +\\([0-9]+\\)" nil t))
	    (message "This is initial version.")
	  (setq ver2 (string-to-number (match-string 1))))))
    (when (and ver1 ver2)
      (let* ((page (trac-wiki-page-name))
	     (str1 (trac-rpc-get-page page ver1))
	     (str2 (trac-rpc-get-page page ver2)))
	(if (null str1)
	    (error "Cannot fetch version %s" ver1))
	(if (null str2)
	    (error "Cannot fetch version %s" ver2))
	(trac-wiki-diff-internal str1 str2)))))

;; utilities
;; FIXME: alter
(if (fboundp 'looking-back)
    (defalias 'trac-wiki-looking-back 'looking-back)
  ;; for Emacs 21 or before
  (defun trac-wiki-looking-back (regex)
    "Easy implementation of `looking-back' of Emacs 22."
    (let ((pos (point)))
      (and (save-excursion
	     (re-search-backward regex nil t))
	   (eq (match-end 0) pos)))))

(defun trac-wiki-delete-window-or-bury-buffer (&optional buf)
  "Close window of BUF if displayed or bury if it is solo window."
  (interactive)
  (setq buf (or buf (current-buffer)))
  (condition-case nil
      (let ((win (get-buffer-window buf)))
	(and win (window-live-p win)
	     (delete-window win)))
    (error (bury-buffer buf))))

(defun trac-wiki-convert-to-readable-time-string (str)
  "Parse STR as ISO format time and return encoded time value."
  (if (not (string-match (concat "\\`"
				 "\\([0-9][0-9][0-9][0-9]\\)"
				 "\\([0-9][0-9]\\)"
				 "\\([0-9][0-9]\\)"
				 "T"
				 "\\([0-9][0-9]?\\)"
				 ":"
				 "\\([0-9][0-9]?\\)"
				 ":"
				 "\\([0-9][0-9]?\\)"
				 "\\([-+][0-9][0-9][0-9][0-9]\\)?"
				 "\\'")
			 str))
      (error "Invalid time format: %s" str)
    (apply 'format "%s-%s-%s %s:%s:%s%s"
	   (append (mapcar (lambda (n)
			     (or (match-string n str) ""))
			   '(1 2 3 4 5 6 7))))))

      
(defun trac-wiki-collect-macro-names ()
  "Collect available macro names from WikiMacro page."
  (let ((html (trac-rpc-get-page-html "WikiMacros"))
	names)
    (with-temp-buffer
      (insert html)
      (goto-char (point-min))
      (while (re-search-forward "<code>\\[\\[\\(\\w+\\)\\]\\]</code>" nil t)
	(let ((name (match-string 1)))
	  (if (not (member name names))
	      (setq names (cons name names))))))
    (sort names 'string<)))		; return sorted names


(provide 'trac)

;;; trac-wiki.el ends here
