;;; elserv.el -- Yet another HTTP server on Emacsen

;; Copyright (C) 2001 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>
;; Keywords: HTTP

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;;; Commentary:
;;

;; API for server handling

;; elserv-start
;; elserv-find-process
;; elserv-stop
;; elserv-publish
;; elserv-unpublish

;; API for content making

;; elserv-make-result
;; elserv-make-redirect

;; Example:
;;
;; (require 'elserv)
;; (elserv-start 8080)
;; (elserv-publish (elserv-find-process 8080) "/"
;;                 :string "Hello World."
;;                 :content-type "text/plain")
;;
;; or write following lines in your .emacs.
;;
;; (autoload elserv-start "elserv" nil t)
;; (add-hook 'elserv-start-hook
;;           '(lambda ()
;;              (elserv-publish (elserv-find-process) "/"
;;                              :string "Hello World."
;;                              :content-type "text/plain")))

;;; History:
;;
;; Part of the codes are originally in an HTTP server embedded in Emacs
;; available from <URL:http://www.chez.com/emarsden/downloads/>.

;;; Code:

(require 'product)
(require 'pces)
(require 'poem)
(require 'std11)

(eval-when-compile
  (require 'cl)
  (require 'static))

(eval-and-compile
  (autoload 'elserv-autoindex "elserv-autoindex" nil t))

(product-provide 'elserv
  (product-define "Elserv" nil
		  '(0 4 0)
		  "Never Surrender"))

(defgroup elserv nil
  "Elserv -- Yet another HTTP server on Emacsen."
  :group 'hypermedia)

(defcustom elserv-default-port 8000
  "*Default port number for Elserv."
  :type 'integer
  :group 'elserv)

(defcustom elserv-program-name nil
  "*If non-nil, it is invoked as a command.
`elserv-daemon-name' is passed as first argument."
  :type '(choice (symbol :tag "Direct" nil)
		 (string :tag "Program Name"))
  :group 'elserv)

(defcustom elserv-daemon-name (if (fboundp 'locate-data-directory)
				  (expand-file-name
				   "elservd"
				   (locate-data-directory "elserv"))
				"elservd")
  "*Program name for Elserv daemon process."
  :type 'string
  :group 'elserv)

(defcustom elserv-publish-hash-length 31
  "*Length of publish hash."
  :type 'integer
  :group 'elserv)

(defcustom elserv-debug nil
  "*If non-nil, request string is inserted to the debug buffer."
  :type 'boolean
  :group 'elserv)

(defcustom elserv-directory-index-file "index.html"
  "*Index file name for the directory."
  :type 'string
  :group 'elserv)

(defcustom elserv-directory-autoindex t
  "*If Non-nil and directory has no index file, generate html index in the
directory."
  :type 'boolean
  :group 'elserv)

(defcustom elserv-keep-alive t
  "*Non-nil enable persistent connections.
\(more than one request per connection\)."
  :type 'boolean
  :group 'elserv)

(defcustom elserv-max-keep-alive-requests 100
  "*The maximum number of requests to allow during a persistent connection.
Set to nil to allow an unlimited amount.
We recommend you leave this number high, for maximum performance."
  :type 'integer
  :group 'elserv)

(defcustom elserv-keep-alive-timeout 15
  "*Number of seconds to wait for the next request on the same connection."
  :type 'integer
  :group 'elserv)

(defcustom elserv-identity-check nil
  "*Non-nil enables RFC1413-compliant logging.
\(logging of the remote user name for each connection\)"
  :type 'boolean
  :group 'elserv)

(defcustom elserv-max-clients 20
  "*Non-nil limits the number of clients who can simultaneously connect.
If this limit is ever reached, clients will be LOCKED OUT."
  :type 'integer
  :group 'elserv)

(defcustom elserv-access-log-file nil
  "*If file name is specified, access log is saved to the file."
  :type 'file
  :group 'elserv)

(defcustom elserv-access-log-max-size 50000
  "*Max size of access log file."
  :type 'integer
  :group 'elserv)

(defcustom elserv-icon-path (if (fboundp 'locate-data-directory)
				(locate-data-directory "elserv")
			      (let ((icons (expand-file-name "elserv/icons/"
							     data-directory)))
				(if (file-directory-p icons)
				    icons)))
  "*Icon directory path."
  :type 'directory
  :group 'elserv)

(defcustom elserv-icon-publish-path "/icons"
  "*Path to publish an icon directory specified by `elserv-icon-path'."
  :type 'string
  :group 'elserv)

(defconst elserv-url-unreserved-chars
  '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
       ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
       ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
       ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
       ?$ ?- ?_ ?. ?! ?~ ?* ?' ?( ?) ?,))

(defconst elserv-http-version "HTTP/1.1")

(defconst elserv-server-eol "\r\n"
  "The end-of-line string sent from the server.")

(defconst elserv-client-eor "\r\n\r\n"
  "The end-of-request string sent from the elservd.")

(defvar elserv-buffer-publish-hash nil)
(make-variable-buffer-local 'elserv-buffer-publish-hash)
(defvar elserv-buffer-request-handler nil)
(make-variable-buffer-local 'elserv-buffer-request-handler)
(defvar elserv-buffer-port nil)
(make-variable-buffer-local 'elserv-buffer-port)
(defvar elserv-buffer-client-process nil)
(make-variable-buffer-local 'elserv-buffer-client-process)
(defvar elserv-buffer-client-port nil)
(make-variable-buffer-local 'elserv-buffer-client-port)

(defvar elserv-mime-types-alist
  '(("html" . "text/html")
    ("txt"  . "text/plain")
    ("jpg"  . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif"  . "image/gif")
    ("png"  . "image/png")
    ("tif"  . "image/tiff")
    ("tiff" . "image/tiff")
    ("css"  . "text/css")
    ("gz"   . "application/octet-stream")
    ("ps"   . "application/postscript")
    ("pdf"  . "application/pdf")
    ("eps"  . "application/postscript")
    ("tar"  . "application/x-tar")
    ("rpm"  . "application/x-rpm")
    ("zip"  . "application/zip")
    ("mp3"  . "audio/mpeg")
    ("mp2"  . "audio/mpeg")
    ("mid"  . "audio/midi")
    ("midi" . "audio/midi")
    ("wav"  . "audio/x-wav")
    ("au"   . "audio/basic")
    ("ram"  . "audio/pn-realaudio")
    ("ra"   . "audio/x-realaudio")
    ("mpg"  . "video/mpeg")
    ("mpeg" . "video/mpeg")
    ("qt"   . "video/quicktime")
    ("mov"  . "video/quicktime")
    ("avi"  . "video/x-msvideo"))
  "Alist of (SUFFIX .CONTENT-TYPE).")

(defsubst elserv-bytes (string)
  "Return the byte length of the STRING."
  (length (string-as-unibyte string)))

(defun elserv-mime-type (filename)
  "Return content-type for FILENAME."
  (or (cdr (assoc (file-name-extension filename) elserv-mime-types-alist))
      "text/plain"))

(put 'elserv-exception 'error-conditions
     '(elserv-exception error))

(defmacro elserv-define-status-code (name code msg)
  "Define status code with NAME, CODE, and MSG."
  `(progn
     (put ',name 'error-conditions '(,name elserv-exception error))
     (put ',name 'elserv-code ,code)
     (put ',name 'elserv-msg ,msg)))

(elserv-define-status-code elserv-ok                 200 "OK")
(elserv-define-status-code elserv-moved-permanently  301 "Moved permanently")
(elserv-define-status-code elserv-found              302 "Found")
(elserv-define-status-code elserv-see-other          303 "See Other")
(elserv-define-status-code elserv-not-modified       304 "Not Modified")
(elserv-define-status-code elserv-bad-request        400 "Bad request")
(elserv-define-status-code elserv-unauthorized       401 "Unauthorized")
(elserv-define-status-code elserv-forbidden          403 "Forbidden")
(elserv-define-status-code elserv-file-not-found     404 "Not found")
(elserv-define-status-code elserv-method-not-allowed 405 "Method not allowed")
(elserv-define-status-code elserv-internal-error   500 "Internal server error")
(elserv-define-status-code elserv-unimplemented    501 "Not implemented")
(elserv-define-status-code elserv-unavailable      503 "Service unavailable")

;;; Result
(defmacro elserv-make-result (&optional code header body
					user content-length)
  "Make a result structure.
CODE is the status code.
HEADER is the plist for header structure.
BODY is the body string.
USER is the user who is authenticated.
CONTENT-LENGTH is the length of the content."
  `(vector ,code ,header ,body ,user ,content-length))

(defmacro elserv-result-code (result)
  "Return code of RESULT."
  `(aref ,result 0))

(defmacro elserv-set-result-code (result code)
  "Set code of RESULT as CODE."
  `(aset ,result 0 ,code))

(defmacro elserv-result-header (result)
  "Return header of RESULT."
  `(aref ,result 1))

(defmacro elserv-set-result-header (result header)
  "Set header of RESULT as HEADER."
  `(aset ,result 1 ,header))

(defmacro elserv-result-body (result)
  "Return body of RESULT."
  `(aref ,result 2))

(defmacro elserv-set-result-body (result body)
  "Set body of RESULT as BODY."
  `(aset ,result 2 ,body))

(defmacro elserv-result-user (result)
  "Return user of RESULT."
  `(aref ,result 3))

(defmacro elserv-set-result-user (result user)
  "Set user of RESULT as USER."
  `(aset ,result 3 ,user))

(defmacro elserv-result-content-length (result)
  "Return content-length of RESULT."
  `(aref ,result 4))

(defmacro elserv-set-result-content-length (result content-length)
  "Set content-length of RESULT as CONTENT-LENGTH."
  `(aset ,result 4 ,content-length))

;;; Error
(defun elserv-error (why &optional msg)
  "Make a error response from WHY.
If optional MSG is specified, it is used as response body."
  (elserv-make-result
   (car why)
   '(content-type "text/html")
   (concat "<html><head><title>Error</title></head>\n"
	   "<body><h1>"
	   (get (car why) 'elserv-msg)
	   "</h1>\n<p>"
	   (or msg (cdr why))
	   "\n</body></html>\n")))

(put 'with-elserv-error-handler 'edebug-form-spec '(body))
(defmacro with-elserv-error-handler (&rest forms)
  "Evaluate FORMS like progn with elserv error handler."
  `(condition-case why
       (progn ,@forms)
     (elserv-exception (elserv-error why))
     (error (elserv-error (cons 'elserv-internal-error nil)
			  (format "Emacs Lisp error: %s\n" why)))))

(defun elserv-host-member (host list)
  "Return t if HOST is matched to any of the regexp in the LIST."
  (let ((case-fold-search t)
	match)
    (while list
      (if (or (string-match (car list) (nth 0 host))
	      (string-match (car list) (nth 1 host)))
	  (setq match t
		list nil)
	(setq list (cdr list))))
    match))

(defun elserv-make-predicate-from-plist (plist)
  "Make a check predicate from PLIST."
  (let (second pred)
    (while plist
      (when (eq (car plist) :allow)
	(setq pred
	      (list 'and (list 'elserv-host-member 'host
			       (append (list 'list) (cadr plist)))
		    (if (setq second (cadr (memq :deny (cdr plist))))
			(list 'not (list 'elserv-host-member 'host
					 (append (list 'list) second)))
		      t)))
	(setq plist nil))
      (when (eq (car plist) :deny)
	(setq pred
	      (list 'or (list 'not (list 'elserv-host-member
					 'host (append (list 'list
							     (cadr plist)))))
		    (if (setq second (cadr (memq :deny (cdr plist))))
			(list 'elserv-host-member 'host
			      (append (list 'list second))))))
	(setq plist nil))
      (setq plist (cdr plist)))
    (or pred t)))

(defun elserv-make-unauthorized-basic (request realm)
  "Make unauthorized RESULT for REQUEST.
Basic authorization response with REALM is created."
  (let ((result (elserv-make-result)))
    (elserv-set-result-code result 'elserv-unauthorized)
    (elserv-set-result-header result
			      `(www-authenticate
				,(concat "Basic realm=\"" realm "\"")
				content-type "text/html"))
    (elserv-set-result-body
     result
     (concat
      "<html><head><title>Authorization required</title></head>
<body><h1>Authorization Required</h1>This server could not verify that you are authorized to access the document requested. Either you supplied the wrong
credentials (e.g., bad password), or your browser doesn't understand how to supply the credentials required.
<hr>"
      (elserv-version) "</body></html>"))
    result))

(defun elserv-make-redirect (result where)
  "Make RESULT as a redirect to new location WHERE."
  (elserv-set-result-code result 'elserv-moved-permanently)
  (elserv-set-result-header result
			    (list 'location where
				  'content-type "text/html"
				  'uri where))
  (elserv-set-result-body result
			  "<html><head><title>Moved permanently</title></head>
<body><h1>Moved permanently</h1>This Page is moved permanently.</body>")
  result)

(defun elserv-version (&optional arg)
  "Return Elserv version.
If it is called interactively, version string is appeared on minibuffer.
If ARG is specified, don't display code name."
  (interactive "P")
  (let ((product-info (product-string-1 'elserv (not arg))))
    (if (interactive-p)
	(message "%s" product-info)
      product-info)))

;;; URL decode: original codes are cgi.el
(defun elserv-url-hex-char-p (ch)
  "Return non-nil if CH is hex char."
  (declare (character ch))
  (let ((hexchars '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
                    ?A ?B ?C ?D ?E ?F)))
    (member (upcase ch) hexchars)))

(defun elserv-url-decode-string (str)
  "Decode STR as URL string.
It replaces %xx to the corresponding character and + to ' '."
  (do ((i 0)
       (len (length str))
       (decoded '()))
      ((>= i len) (concat (nreverse decoded)))
    (let ((ch (aref str i)))
      (cond ((eq ?+ ch)
             (push ?\ decoded)
             (incf i))
            ((and (eq ?% ch)
                  (< (+ i 2) len)
                  (elserv-url-hex-char-p (aref str (+ i 1)))
                  (elserv-url-hex-char-p (aref str (+ i 2))))
             (let ((hex (string-to-number (substring str (+ i 1) (+ i 3)) 16)))
               (push (int-char hex) decoded)
               (incf i 3)))
            (t (push ch decoded)
               (incf i))))))

(defsubst elserv-position (char str)
  "Find the first occurrence of CHAR in STR."
  (let ((end (length str))
	(i 0)
	pos)
    (while (< i end)
      (if (eq (aref str i) char)
	  (setq pos i
		i end))
      (incf i))
    pos))

(defun elserv-url-decode (q)
  "Parse string Q as URL query.
\"foo=x&bar=y+re\" into ((\"foo\" .  \"x\") (\"bar\" \.  \"y re\"))
Substrings are plus-decoded and then URL-decoded."
  (when q
    (flet ((split-= (str)
            (let ((pos (or (elserv-position ?= str) 0)))
              (cons (elserv-url-decode-string (substring str 0 pos))
                    (elserv-url-decode-string (substring str (+ pos 1)))))))
      (mapcar #'split-= (split-string q "&")))))


;;; Object loading and saving.
(defun elserv-load (filename &optional coding)
  "Load OBJECT from the file specified by FILENAME.
File content is decoded with CODING."
  (if (not (file-readable-p filename))
      nil
    (with-temp-buffer
      (insert-file-contents-as-binary filename)
      (when coding
	(set-buffer-multibyte t)
	(decode-coding-region (point-min) (point-max) coding))
      (ignore-errors (read (current-buffer))))))

(defun elserv-make-directory (path)
  "Create directory on PATH recursively."
  (let ((parent (directory-file-name (file-name-directory path))))
    (if (null (file-directory-p parent))
	(elserv-make-directory parent))
    (make-directory path)))

(defsubst elserv-save-buffer (filename &optional coding)
  "Save current buffer to the file specified by FILENAME.
Directory of the file is created if it doesn't exist.
File content is encoded with CODING."
  (let ((dir (directory-file-name (file-name-directory filename))))
    (if (file-directory-p dir)
	() ; ok.
      (unless (file-exists-p dir) (elserv-make-directory dir)))
    (when coding
      (encode-coding-region (point-min) (point-max) coding))
    (write-region-as-binary (point-min) (point-max)
			    filename nil 'no-msg)))

(defun elserv-save (filename object &optional coding)
  "Save object.
FILENAME is the name of the saved file.
OBJECT is the object to be saved.
Directory of the file is created if it doesn't exist.
File content is encoded with CODING before saving."
  (with-temp-buffer
    (prin1 object (current-buffer))
    (elserv-save-buffer filename coding)
    object))

;;; Debug
(defvar elserv-debug-buffer nil)
(defun elserv-debug (string)
  "Insert STRING to the debug buffer."
  (when elserv-debug
    (if (or (null elserv-debug-buffer)
	    (not (bufferp elserv-debug-buffer))
	    (not (buffer-live-p elserv-debug-buffer)))
	(setq elserv-debug-buffer (get-buffer-create "*Debug elserv*")))
    (with-current-buffer elserv-debug-buffer
      (goto-char (point-max))
      (insert string))))

(defun elserv-process-filter (process string)
  "Process filter elserv.  PROCESS, STRING are argument for process filter."
  (elserv-debug string)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward elserv-client-eor nil t)
	(elserv-process-request process
				(elserv-parse-request
				 (buffer-substring (point-min) (point))))
	(delete-region (point-min) (point))))))

(defsubst elserv-client-start (port process)
  "Start client process for elservd.
PORT is the elservd client port.
PROCESS is the server process."
  (with-current-buffer (get-buffer-create (concat "*elserv client*"
						  (number-to-string
						   (elserv-process-port
						    process))))
    (set-buffer-multibyte nil)
    (open-network-stream-as-binary "_elserv"
				   (current-buffer)
				   "localhost" port)))

(defsubst elserv-process-request-internal (request client-process
						   process handler)
  "Process request.
REQUEST, CLIENT-PROCESS, PROCESS, HANDLER are used."
  (let (result header connection string)
    (setq result
	  (with-elserv-error-handler
	   (funcall handler process request)))
    (setq connection (elserv-decide-connection result request))
    (setq header (elserv-make-header result request connection))
    (setq string (concat (plist-get request 'key)
			 (if (string= connection "close")
			     ";" ":")
			 (number-to-string (+ (elserv-bytes header)
					      ;; redundant process.
					      (if (elserv-result-body result)
						  (elserv-bytes
						   (elserv-result-body result))
						0)))
			 "\r\n"))
    (process-send-string client-process string)
    (process-send-string client-process header)
    (elserv-debug string)
    (elserv-debug header)
    (when (elserv-result-body result)
      (process-send-string client-process (elserv-result-body result))
      (elserv-debug (elserv-result-body result))
      (elserv-debug "\r\n"))
    (process-send-string client-process "\r\n")
    (elserv-log process request result)))

(defun elserv-process-request (process request)
  "Process request string on the current buffer.
PROCESS is elserv process.
REQUEST is the request plist."
  ;; current buffer is process buffer.
  (let ((client-process elserv-buffer-client-process)
	(handler elserv-buffer-request-handler))
    (if elserv-buffer-client-port
	(progn
	  (unless (memq (process-status elserv-buffer-client-process)
			'(open run))
	    (delete-process elserv-buffer-client-process)
	    ;; restart.
	    (setq elserv-buffer-client-process
		  (elserv-client-start elserv-buffer-client-port process)
		  client-process elserv-buffer-client-process))
	  (with-current-buffer (process-buffer elserv-buffer-client-process)
	    (elserv-process-request-internal
	     request client-process process handler)))
      ;; Process greeting.
      (setq elserv-buffer-client-port
	    (string-to-number (plist-get request 'port))
	    elserv-buffer-client-process
	    (elserv-client-start elserv-buffer-client-port process)))))

(defsubst elserv-delete-cr-buffer ()
  "Delete CR from buffer."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t)
      (replace-match "\n")) ))

(defun elserv-parse-request (request)
  "Parse REQUEST string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert request)
    (elserv-delete-cr-buffer)
    (goto-char (point-min))
    (let ((regexp (concat "\\(" std11-field-head-regexp "\\)[ \t]*"))
	  name body dest end)
      (while (re-search-forward regexp nil t)
	(setq name (downcase (buffer-substring
			      (match-beginning 1)(1- (match-end 1))))
	      end  (match-end 0)
	      name (intern (if (string-match "^elserv-" name)
			       (setq name (substring name (match-end 0)))
			     name))
	      body (buffer-substring end (std11-field-end)))
	(if (eq name 'client)
	    (setq body (split-string body)))
	(when (eq name 'content)
	  (setq name 'body)
	  (setq body (ignore-errors (base64-decode-string body))))
	(setq dest (nconc (list name body) dest)))
      dest)))

(defun elserv-decide-connection (result request)
  "Decide connection type by RESULT and REQUEST."
  (if (and elserv-keep-alive
	   (string-match "keep-alive"
			 (or (plist-get request 'connection) ""))
	   (eq (get (elserv-result-code result)
		    'elserv-code)
	       200))
      "keep-alive"
    "close"))

(defun elserv-make-header (result request connection)
  "Make an HTTP header string from RESULT, REQUEST, and CONNECTION."
  (concat elserv-http-version " "
	  (number-to-string (get (elserv-result-code result) 'elserv-code))
	  " "
	  (get (elserv-result-code result) 'elserv-msg)
	  "\r\nServer: " (elserv-version 'simple)
	  "\r\nAccept-Ranges: none"
	  "\r\nDate: " (let ((system-time-locale "C"))
			 (format-time-string "%a, %e %b %Y %T %Z"))
	  "\r\nConnection: " connection
	  (if (string= connection "keep-alive")
	      (concat
	       "\r\nKeep-Alive: timeout=" (number-to-string
					   elserv-keep-alive-timeout)
	       ", max=" (number-to-string
			 elserv-max-keep-alive-requests)))
	  "\r\n"
	  (let ((header (elserv-result-header result))
		str)
	    (while header
	      (setq str (concat str (capitalize (symbol-name (nth 0 header))) ": "
				(nth 1 header) "\r\n"))
	      (setq header (nthcdr 2 header)))
	    str)
	  "Content-Length: " (number-to-string
			      (+ 2 (or 
				    (elserv-result-content-length
				     result)
				    (if (elserv-result-body result)
					(elserv-bytes (elserv-result-body
						       result))
				      0))))
	  "\r\n"
	  "MIME-Version: 1.0\r\n\r\n"))

(defun elserv-process-sentinel (process string)
  "A sentinel for elserv process.  PROCESS, STRING are arguments for sentinel."
  (elserv-debug string)
  (delete-process process))

;;; Commands

;;;###autoload
(defun elserv-start (&optional port)
  "Start elserv server process.
Optional PORT is port number for the server process.
If PORT is not specified, `elserv-default-port' is used.
Return server process object."
  (interactive)
  (let (process args)
    (setq port (or port elserv-default-port))
    (setq args (list (number-to-string port)
		     (if elserv-identity-check "log" "nolog")
		     (number-to-string (or elserv-max-clients 0))
		     (number-to-string (or elserv-max-keep-alive-requests 0))
		     (number-to-string (or elserv-keep-alive-timeout 0))))
    (if elserv-program-name (setq args (cons elserv-daemon-name args)))
    (setq process (as-binary-process
		   (apply
		    'start-process
		    "elserv"
		    (get-buffer-create (concat "*elserv*"
					       (number-to-string port)))
		    (or elserv-program-name elserv-daemon-name)
		    args)))
    (with-current-buffer (process-buffer process)
      (set-buffer-multibyte nil)
      (erase-buffer)
      (setq elserv-buffer-request-handler 'elserv-request-handler)
      (setq elserv-buffer-publish-hash
	    (make-vector elserv-publish-hash-length 0))
      (setq elserv-buffer-port port))
    (set-process-filter process 'elserv-process-filter)
    (set-process-sentinel process 'elserv-process-sentinel)
    (elserv-publish-default process)
    (get-buffer-create (concat "*Log of elserv*"
			       (number-to-string
				(elserv-process-port process))))
    (run-hooks 'elserv-start-hook)
    process))

(defun elserv-process-port (process)
  "Get port number of the Elserv server PROCESS."
  (with-current-buffer (process-buffer process)
    elserv-buffer-port))

(defun elserv-find-process (&optional port)
  "Find running Elserv server process.
If optional PORT is specified, find process with the specified port number.
Otherwise, an Elserv process last invoked is returned."
  (catch 'found
    (dolist (process (process-list))
      (if (string-match "^elserv" (process-name process))
	  (if port
	      (if (eq port (elserv-process-port process))
		  (throw 'found process))
	    (throw 'found process))))))

(defun elserv-stop (&optional port)
  "Stop running Elserv server process.
If optional PORT is specified, kill process with the specified port number.
Otherwise, an Elserv process last invoked is killed."
  (interactive)
  (let ((process (elserv-find-process port)))
    (if process
	(progn
	  (kill-buffer (process-buffer process))
	  (delete-process process)
	  (message "Elserv stopped."))
      (message "Elserv process not found."))))

;;; Access log
(defun elserv-log (process request result)
  "Record a server access log.
PROCESS is the Elserv server process.
REQUEST is the request structure.
RESULT is the result structure."
  (with-current-buffer (get-buffer-create
			(concat "*Log of elserv*"
				(number-to-string
				 (elserv-process-port process))))
    (let (point)
      (goto-char (point-max))
      (setq point (point))
      (insert
       (car (plist-get request 'client))
       " "
       (if elserv-identity-check
	   (or (plist-get request 'ident) "unknown")
	 "-")
       " "
       (or (elserv-result-user result) "-") ; remote user (auth)
       " "
       (let ((system-time-locale "C"))
	 (format-time-string "[%a, %d %b %Y %T %z] "))
       "\"" (plist-get request 'request) "\""
       " " (number-to-string (get (elserv-result-code result)
				  'elserv-code))
       " "
       (if (elserv-result-body result)
	   (number-to-string (elserv-bytes (elserv-result-body result)))
	 "0")
       " \"" (or (plist-get request 'referer) "-") "\" \""
       (or (plist-get request 'user-agent) "no agent info") "\"\n")
      (if elserv-access-log-file
	  (if (file-writable-p elserv-access-log-file)
	      (progn
		(if (> (nth 7 (file-attributes elserv-access-log-file))
		       elserv-access-log-max-size)
		    (ignore-errors
		      (rename-file elserv-access-log-file
				   (concat elserv-access-log-file
					   ".0") t)))
		(write-region point (point) elserv-access-log-file t 'no-msg))
	    (elserv-debug (concat elserv-access-log-file
				  " is not writable!!\n")))))))

;;; Process request.
(defun elserv-request-handler (process request)
  "Request handler.  PROCESS, REQUEST are arguments for request handler."
  (let ((req (plist-get request 'request))
	method func)
    (if (and (string-match "HTTP/1\\.1" req)
	     (null (plist-get request 'host)))
	(signal 'elserv-bad-request
		"HTTP 1.1 client must send a Host: field."))
    (if (string-match "\\`\\([^ ]+\\)\\s-\\([^ \t\r\n]*\\)" req)
	(progn
	  (setq method (match-string 1 req)
		func (intern (concat "elserv-handle-"
				     (downcase method))))
	  (if (fboundp func)
	      (funcall func process (match-string 2 req) request)
	    (signal 'elserv-not-implemented (concat 
					     method
					     " is not implemented"))))
      (signal 'elserv-bad-request req))))

(defun elserv-handle-get (process path request)
  "Handle GET request.
PROCESS is elserv process.
PATH is the requested path string.
REQUEST is the request structure."
  (elserv-service process path request))

(defun elserv-handle-head (process path request)
  "Handle HEAD request.
PROCESS is elserv process.
PATH is the requested path string.
REQUEST is the request structure."
  (let ((result (elserv-service process path request)))
    (elserv-set-result-content-length result (elserv-bytes
					      (elserv-result-body result)))
    (elserv-set-result-body result nil)
    result))

(defun elserv-handle-post (process path request)
  "Handle POST request.
PROCESS is elserv process.
PATH is the requested path string.
REQUEST is the request structure."
  (elserv-service process path request))

(defun elserv-authenticate-basic (result value password-alist)
  "Implementation of basic authenticate type.
RESULT is the result structure.
VALUE is authorization value from client.
PASSWORD-ALIST is the alist of cons cell like: (USER . PASSWORD)."
  (when (string-match "\\([^:]*\\):\\(.*\\)" value)
    (let (user passwd)
      (setq user (substring value (match-beginning 1)(match-end 1)))
      (setq passwd (substring value (match-beginning 2)(match-end 2)))
      (when (string= (cdr (assoc user password-alist)) passwd)
	(elserv-set-result-user result user)
	t))))

(defun elserv-authenticate (request auth result)
  "Return unauthorized result.
REQUEST is the request structure.
AUTH is the auth structure.
Return RESULT if REQUEST is not authorized by AUTH.
Otherwise, RESULT is set as authenticated and return nil."
  (let ((authorization (plist-get request 'authorization)))
    (if (plist-get auth :realm) ; authentication required.
	(if (null authorization)
	    (funcall
	     (intern
	      (concat "elserv-make-unauthorized-" (plist-get auth :type)))
	     request
	     (plist-get auth :realm))
	  (setq authorization (nth 1 (split-string authorization)))
	  (if (funcall
	       (intern (concat "elserv-authenticate-" (plist-get auth :type)))
	       result
	       (base64-decode-string authorization)
	       (plist-get auth :users))
	      ;; OK.
	      nil
	    ;; Try again.
	    (funcall
	     (intern
	      (concat "elserv-make-unauthorized-" (plist-get auth :type)))
	     request
	     (plist-get auth :realm)))))))

(defun elserv-check-predicate (request predicate)
  "Return forbidden result if REQUEST does not satisfy PREDICATE."
  (let ((host (plist-get request 'client)))
    (unless (eval predicate)
      (signal 'elserv-forbidden (concat (car host) " is not allowed.")))))

;; Publish & Service
(defun elserv-publish (process path &rest args)
  "Publish a document.
PROCESS is the server process of Elserv.
PATH is the requested path.
Rest of arguments ARGS are plist of the form (:ATTR1 VAL1 :ATTR2 VAL2 ...)."
  (let (data set-auth auth predicate host doc)
    ;; Virtual host.
    (if (setq host (plist-get args :host))
	(setq path (concat host path)))
    (with-current-buffer (process-buffer process)
      (when (setq set-auth (plist-get args :authenticate))
	(setq auth
	      (list :type (or (plist-get set-auth :type) "basic")
		    :realm (plist-get set-auth :realm)
		    :users (plist-get set-auth :users))))
      (setq predicate (elserv-make-predicate-from-plist args))
      (setq doc (plist-get args :description))
      (cond
       ((setq data (plist-get args :directory)) ; directory is set.
	(set (intern path elserv-buffer-publish-hash)
	     (list 'elserv-service-directory
		   doc auth predicate data)))
       ((setq data (plist-get args :string))    ; string is set.
	(set (intern path elserv-buffer-publish-hash)
	     (list 'elserv-service-string
		   doc auth predicate data
		   (plist-get args :content-type))))
       ((setq data (plist-get args :function))   ; handler is set.
	(set (intern path elserv-buffer-publish-hash)
	     (nconc (list 'elserv-service-function
			  doc auth predicate data
			  (plist-get args :content-type)))))))))

(defun elserv-unpublish (process path)
  "Unpublish a published document.
PROCESS is the server process of Elserv.
PATH is the requested path."
  (with-current-buffer (process-buffer process)
    (unintern path elserv-buffer-publish-hash)))

(defsubst elserv-execute-service-maybe (ppath path host request)
  "Call service function for PPATH, PATH, HOST and REQUEST, if registered.
Return result structure. If function is not registered, return nil."
  (let (sym func)
    (when (and (or (setq sym (intern-soft 
			      (concat host ppath)
			      elserv-buffer-publish-hash))
		   (setq sym (intern-soft
			      ppath
			      elserv-buffer-publish-hash)))
	       (boundp sym)
	       (setq func (append (symbol-value sym)
				  (list path ppath request))))
      (apply (car func) (cdr func)))))

(defun elserv-parse-path (path)
  "Return a reversed list of substrings of PATH which are separated by '/'."
  (let ((start 0) parts)
    (while (string-match "/" path start)
      (setq parts (cons (substring path start (match-beginning 0)) parts)
	    start (match-end 0)))
    (cons (substring path start) parts)))

(defun elserv-service (process path request)
  "Provide a service.
PROCESS is the server process of Elserv.
PATH is the requested path string.
REQUEST is the request structure."
  (let ((host (plist-get request 'host))
	path-list ppath rpath result)
    ;; absolute URI.
    (when (string-match "^http://\\([^/]+\\)\\(/\\)" path)
      (setq host (substring path (match-beginning 1) (match-end 1))
	    path (substring path (match-beginning 2))))
    (setq path-list (elserv-parse-path path))
    (with-current-buffer (process-buffer process)
      (while path-list
	(setq ppath (concat (mapconcat 'identity
				       (reverse path-list) "/"))
	      rpath (substring path (length ppath)))
	(when (eq (length ppath) 0)
	  (setq ppath "/"))
	(when (string= ppath "/")
	  (setq rpath path))
	(if (setq result (elserv-execute-service-maybe
			  ppath rpath
			  host request))
	    (setq path-list nil))
	(setq path-list (cdr path-list)))
      (or result
	  (signal 'elserv-file-not-found path)))))

(defun elserv-parse-accept-language (string)
  "Parse Accept-Language field body and return language candidate list."
  (let (candidates)
    (while (string-match "^\\([A-Za-z-]+\\)\\(\\(; *q=[0-9.]+\\)?, *\\)?"
			 string)
      (setq candidates (cons 
			(substring string (match-beginning 1)(match-end 1))
			candidates))
      (setq string (substring string (match-end 0))))
    (nreverse candidates)))

(defun elserv-find-file (filename language)
  "Return a filename which matches to FILENAME and LANGUAGE exists."
  (if language
      (let ((langs (elserv-parse-accept-language language))
	    file)
	(catch 'done
	  (dolist (lang langs)
	    (if (file-readable-p (setq file (concat filename "." lang)))
		(throw 'done file)))
	  (if (file-readable-p filename)
	      filename)))
    filename))

(defun elserv-service-directory (doc auth predicate root path ppath request)
  "Service a directory.
DOC is the documentation of the service.
AUTH is the autenticator plist.
PREDICATE is the predicate to check a request.
ROOT is the top directory recorded by `elserv-publish'.
PATH is the path string relative from published path.
PPATH is the path string published by `elserv-publish'.
REQUEST is the request structure (plist)."
  (let ((result (elserv-make-result)))
    (or (elserv-check-predicate request predicate)
	(elserv-authenticate request auth result)
	(let (filename realfile attr mime-type)
	  (setq filename (concat root path))
	  (setq path (elserv-url-decode-string path))
	  (when (string-match "\\.\\." path)
	    (signal 'elserv-forbidden (concat root path)))
	  (if (zerop (length (file-name-nondirectory filename)))
	      (setq filename (expand-file-name
 			      elserv-directory-index-file
			      filename)))
	  (cond ((file-directory-p filename)
		 (elserv-make-redirect
		  result
		  (concat "http://" (plist-get request 'host)
			  (unless (string= ppath "/") ppath)
			  path "/")))
		((setq realfile
		       (elserv-find-file filename (plist-get 
						   request
						   'accept-language)))
		 (setq mime-type (elserv-mime-type filename))
		 (setq attr (file-attributes realfile))
		 ;; Trace symbolic link.
		 (when (stringp (car attr))
		   (setq realfile (expand-file-name (car attr) root))
		   (setq attr (file-attributes realfile)))
		 (elserv-set-result-code result 'elserv-ok)
		 (elserv-set-result-header result
					   `(content-type ,mime-type))
		 (elserv-set-result-body result
					 (with-temp-buffer
					   (insert-file-contents-as-binary
					    realfile)
					   (buffer-string)))
		 result)
		((and elserv-directory-autoindex
		      (string= elserv-directory-index-file
			       (file-name-nondirectory filename)))
		 (elserv-autoindex
		  result
		  (plist-get request 'host) (concat
					     (unless (string= ppath "/") ppath)
					     path)
		  (file-name-directory filename)))
		(t (signal 'elserv-file-not-found
			   (concat (unless (string= ppath "/") ppath)
				   path))))))))

(defun elserv-service-string (doc auth predicate string content-type path ppath
				  request)
  "Service a string.
DOC is the documentation of the service.
AUTH is the autenticator plist.
PREDICATE is the predicate to check a request.
STRING is the content string recorded by `elserv-publish'.
CONTENT-TYPE is the content-type string recorded by `elserv-publish'.
PATH is the path string relative from published path.
PPATH is the path string published by `elserv-publish'.
REQUEST is the request structure (plist)."
  (let ((result (elserv-make-result)))
    (or (elserv-check-predicate request predicate)
	(elserv-authenticate request auth result)
	(progn
	  (elserv-set-result-code result 'elserv-ok)
	  (elserv-set-result-header result
				    `(content-type ,content-type))
	  (elserv-set-result-body result string)
	  result))))

(defun elserv-service-function (doc auth predicate function
				     content-type path ppath request)
  "Service by a function.
DOC is the documentation of the service.
AUTH is the autenticator plist.
PREDICATE is the predicate to check a request.
FUNCTION is the symbol of the function registered.
CONTENT-TYPE is the content-type string registered.
PATH is the path string relative from published path.
PPATH is the published path string.
REQUEST is the request structure (plist)."
  (let ((result (elserv-make-result)))
    (or (elserv-check-predicate request predicate)
	(elserv-authenticate request auth result)
	(progn
	  (funcall function result
		   (elserv-url-decode-string path)
		   ppath request)
	  (unless (elserv-result-code result)
	    (elserv-set-result-code result 'elserv-ok)
	    (unless (plist-get (elserv-result-header result) 'content-type)
	      (elserv-set-result-header result
					(append
					 (elserv-result-header result)
					 `(content-type ,(or content-type
							     "text/plain"))))))
	  result))))

(defun elserv-package-publish (process path name)
  "Publish package.
PROCESS is the server process of Elserv.
PATH is the path to publish.
NAME is the name of the package to publish."
  (require (intern (concat "es-" name)))
  (let ((sym (intern (concat "elserv-" name "-publish"))))
    (if (fboundp sym)
	(funcall sym process path)
      (error "Cannot publish as package: %s." name))))

(defun elserv-publish-default (process)
  "Publish default pages for PROCESS."
  ;; Publish monitor.
  (elserv-package-publish process "/" "monitor")
  (elserv-package-publish process "/monitor" "monitor")
  ;; Publish icons.
  (if (and elserv-icon-path
	   (file-directory-p elserv-icon-path))
      (elserv-publish process elserv-icon-publish-path
		      :directory elserv-icon-path)))

;;; Utils

(defun elserv-replace-in-string (str regexp newtext &optional literal)
  "Replace all matches in STR for REGEXP with NEWTEXT string.
And returns the new string.
Optional LITERAL non-nil means do a literal replacement.
Otherwise treat \\ in NEWTEXT string as special:
  \\& means substitute original matched text,
  \\N means substitute match for \(...\) number N,
  \\\\ means insert one \\."
  (let ((rtn-str "")
	(start 0)
	(special)
	match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
	    start (match-end 0)
	    rtn-str
	    (concat
	     rtn-str
	     (substring str prev-start match)
	     (cond (literal newtext)
		   (t (mapconcat
		       (function
			(lambda (c)
			  (if special
			      (progn
				(setq special nil)
				(cond ((eq c ?\\) "\\")
				      ((eq c ?&)
				       (substring str (match-beginning 0)
						  (match-end 0)))
				      ((and (>= c ?0) (<= c ?9))
				       (if (> c (+ ?0 (length
						       (match-data))))
					   ;; Invalid match num
					   (error "Invalid match num: %c" c)
					 (setq c (- c ?0))
					 (substring str (match-beginning c)
						    (match-end c))))
				      (t (char-to-string c))))
			    (if (eq c ?\\) (progn (setq special t) nil)
			      (char-to-string c)))))
		       newtext ""))))))
    (concat rtn-str (substring str start))))

(provide 'elserv)

;;; elserv.el ends here
