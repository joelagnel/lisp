;;; blogmax.el - maintain a weblog <pre>

;; Copyright (C) 2001-2005 Bill St. Clair
;; email: bill@billstclair.com
;; Web: http://billstclair.com/blogmax/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Mod History
;;
;; 051101 wws  weblog-latest-month-before works correctly when there are
;;             future files in the directory.
;; 050605 wws  Make it work to store the day files in year directories
;;             (two digits matching the first two digits of the file name).
;; 050124 wws  weblog-set-buffer-mode no longer searches for weblog.ini
;;             unless the extension of the buffer's file name is "txt".
;;             This stops long pauses on ange-ftp connections looking
;;             for source files.
;; 041208 wws  Add "GUID" to RSS so that it will work correctly with
;;             http://minutillo.com/steve/feedonfeeds/
;;             This is a kludge that generates new GUIDs every time
;;             you upload the RSS file, but it works for me.
;;             weblog-upload-rss invoked interactively now takes a prefix
;;             argument. If 1 (the default), doesn't upload the RSS
;;             file to the FTP server.
;;             Add Shane Simmons' weblog-maco-wikipedia
;; 040914 wws  {bumper back top-fore top-back top-msg bot-fore bot-back bot-msg}
;;             Generates a bumper sticker with a top and bottom section.
;;             back is the background color for the border around the
;;             whole thing.
;;             top-fore, top-back, & top-msg are the foreground color,
;;             background color, and message for the top half.
;;             bot-fore, bot-back, & bot-msg are the foreground color,
;;             background color, and message for the bottom half.
;; 040904 wws  {pl "..."} expansion links to day page in rss file
;;             C-0 C-X C-I now properly uploads the previous month
;;             and the current file, even if the "previous" month
;;             is a long time back.
;; 040903 wws  *weblog-bugmenot-auto-list*
;;             Domains in the "bugmenot-auto-list" get {bugmenot "..."}
;;             links auto-inserted after links to them.
;; 040903 wws  {bugmenot "www.washingtonpost.com"}
;;             generates a link to the Post's BugMeNot login information
;;             using {blogToplevel}bugmenot.png.
;; 040328 wws  weblog-make-index always generates *weblog-index-files*
;;             entries, independent of how many future days are
;;             in the directory.
;; 040208 m3m  Make weblog-rss-format-time RFC 822 compliant
;; 040125 wws  Add optional link-text param to weblog-macro-dailylink
;;             Other small changes to help CSS templates work.
;; 040103 wws  {pl "name"} (permalink macro)
;;             Make weblog-insert-day-index-entries use the last line
;;             of a multi-line header, so that it won't include
;;             a permalink macro on the line before the story link.
;; 031210 wws  line-beginning-position definition for later xemacs versions
;; 031208 wws  weblog-file-mdy works for month files, e.g. "0312.txt"
;; 031105 wws  C-x i -> weblog-italicize-word
;; 031019 wws  Peter L. DeWolf's fixes for weblog-map-all-files and
;;             weblog-find-or-visit.
;; 030618 wws  m-p inserts <br>. I found I didn't use c-m-b
;; 030602 wws  Make weblog-month-index work correctly if the file-name
;;             arg has no directory component. This happens when it's
;;             called from weblog-maybe-upload-previous-month-file,
;;             which caused the previous month's index to have no links
;;             to days or other months.
;; 030514 wws  updated weblog-macro-jargon for ESR's new directory structure
;; 030131 wws  Remove title and link from RSS items
;; 030124 wws  weblog-macro-jargon: www.jargon.org -> www.catb.org/jargon
;; 021222 wws  C-M-R inserts "<br>"
;; 021103 wws  *weblog-char-map* - automatically fix common 8-bit chars
;; 021009 wws  Fix non-local return in
;;             weblog-first-day-file-in-next-month.
;;             Do the same in weblog-last-day-file-in-previous-month.
;;             Add descending parameter to weblog-map-directory.
;; 021005 wws  Tony Sidaway's changes to make the generated HTML
;;             pass weblint.
;;             Make weblog-month-index link to the proper next and
;;             previous month in the presence of missing months.
;; 021003 wws  Change GIFs to PNGs here and in shortcuts.el
;;             Regenerate all the html files to use the new PNG files.
;; 020731 wws  James Thornton's fix to weblog-macro-dailyLink:
;;             only include the link on the index page.
;;             Included his directions for using SSH via Tramp
;;             as a comment to the ftp-directory spec in weblog.ini
;; 020725 wws  weblog-month-index
;;             Fix weblog-first-day-file-in-next-month and
;;             weblog-last-day-file-in-previous-month to work
;;             correctly for missing months.
;; 011130 wws  weblog-insert-ellipsis bound to c-x m-.
;; 011119 wws  Enable starting the calendar on any day of the week.
;; 010919 wws  Better error message if shortcuts.el file doesn't parse.
;;             Create shortcuts.el if it's not there.
;; 010821 wws  Make it work in XEmacs 21.4 on Windoze.
;; 010803 wws  Make it work in XEmacs
;; 010708 wws  weblog-macro-jargon
;; 010702 wws  weblog-file-in-base-dir
;;             weblog-upload doesn't call weblog-make-rss unless the
;;             file is in the base directory.
;; 010612 wws  weblog-make-rss now generates RSS that Feedreader can
;;             grok. This includes <title> & <link> tags in each <item> and
;;             spaces before newlines in the <description>.
;; 010607 wws  New prefix-arg values for weblog-upload-index
;;             1 - Make index. Don't upload (this is a change).
;;             0 - Make & upload index. Regen and upload this month.
;;             2 - Make & upload index. Regen and upload current directory's
;;                 (and subdirectories') text files.
;;             4 - Make and upload index only.
;; 010606 wws  Test on Linux in Emacs 20.4.1.
;;             Add missing </td> in calendar.
;; 010605 wws  First release 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; To do:
;;    Why doesn't C-0 C-x C-i work when there is only a single
;;    file (the first day)?
;;    Send XML-RPC message to rpc.weblogs.com after uploading index.
;;    See http://www.xmlrpc.com/weblogsCom
;;      start-process  (ange-ftp-get-process)
;;      process-send-string  (send-string, ange-ftp-raw-send-cmd)
;;      kill-buffer  (ange-ftp-kill-ftp-process)
;;
;;    Do the next and previous month links properly in the calendar if
;;    either contains no entries but an earlier or later month does.
;;
;;    Figure out how to upload a binary file (copy-file doesn't work)
;;    Choose template in header comment
;;    Record upload time in header comment
;;      {!--Title--}
;;      {!-- template template-name--}
;;      {!-- saved time--}
;;    content-template.tmpl only for yymmdd.txt files in
;;      top-level directory. story-template.tmpl for all else.
;;    Properly save an empty buffer
;;    Don't do text-only operations on non ".txt" buffers.
;;    Dependencies, so that generating and/or uploading one file
;;      will cause files that depend on it to be generated and/or
;;      uploaded.
;;    Eliminate infinite recursion in macros that include files
;;      This may require changing to depth first instead of
;;      breadth first macro expansion.

;; This file aids in the maintenance of a Weblog.
;; It runs text files containing the weblog content through templates
;; to create HTML files, and uploads them via FTP to a web site. It is
;; similar to Dave Winer's Manila <http://manila.userland.com/>.

;; Expects the daily files to be named yymmdd.txt, where yy is the
;; last two digits of the year, mm is the month, and dd is the
;; day. These files should be at the top level of the
;; *weblog-directory*.
;;
;; weblog-save saves the current buffer to html by running it through
;; the *weblog-page-template-file*. Within that file and its included
;; templates, {contentTemplate} includes the
;; *weblog-content-template-file*, {dayTemplate} includes the
;; *weblog-day-template-file*, {storyContent} includes the current
;; buffer's text. There are other macros, but I haven't documented
;; them yet. Macro names all begin with "weblog-macro-".
;;
;; Manila used double quotes to delimit shortcuts. I'm tired of
;; escaping them, so I changed a shortcut to {=shortcut}, i.e
;; a macro beginning with "=".
;;
;; If the current buffer is not at the top-level of the
;; *weblog-directory*, then {contentTemplate} includes
;; *weblog-story-template-file* instead of
;; *weblog-content-template-file*. This allows stories to have a
;; different appearance than daily blog entries.
;;
;; weblog-make-index creates an index.html page using the last
;; *weblog-index-days* of blog entries.
;;
;; A weblog-mode, derived from html-mode, is defined near the end of
;; this file. Any ".txt" file in the *weblog-directory* or a
;; sub-directory will be opened in weblog-mode. Key bindings are
;; at the end of the file.
;;

(provide 'blogmax)

;; Use the Common Lisp library
(require 'cl)

;; Use the ange-ftp library to upload files
(if (featurep 'efs-autoloads)
    (require 'efs)
  (require 'ange-ftp))

;; The {calendar} macro needs some functions from the calendar library
(require 'calendar)

;; Stop xemacs from filling in a blank buffer
(set-variable 'html-helper-build-new-buffer nil)

;; The name of the parameter file containing bindings for many of
;; the variables below.
(defconst *weblog-init-file* "weblog.ini")

;; The directory containing all the files. Should end with a slash.
;; This is bound during processing to the directory containing
;; the *weblog-init-file*.
;; This initial value is designed to cause an error if any code
;; that needs it runs without some caller binding it to a directory name.
(defvar *weblog-directory* t)

;; The name of the weblog
;; Bound to the "site-name" parameter in the *weblog-init-file*.
;; Available via the {siteName} macro.
(defvar *weblog-site-name* nil)

;; The by-line that goes under the site name in the default page template.
;; Available via the {byline} macro
(defvar *weblog-byline* nil)

;; The author of the site.
;; Available via the {author} macro
(defvar *weblog-author* nil)

;; The author's email address
;; Available via the {email} macro
(defvar *weblog-email* nil)

;; The FTP directory to which to upload. Should end with a slash.
;; Of the form "/username@host:/dir-path.../" for ange-ftp upload
;; Bound to the "ftp-directory" parameter in the *weblog-init-file*.
(defvar *weblog-ftp-directory* nil)

;; The URL where the ftp directory is available vai HTTP
;; Available via the {url} macro
(defvar *weblog-url* nil)

;; The number of days to put in the index.html file
;; Bound to the "index-days" parameter in the *weblog-init-file*.
(defvar *weblog-index-days* 7)

;; This file defines "shortcuts". It contains a list of two-element
;; lists. If the first element of any list is in the text between
;; double quotes, it is replaced by the second element of the list.
;; weblog-add-shortcuts can be used to add elements to this list.
;; Bound to the "shortcuts-file" parameter in the *weblog-init-file*.
(defvar *weblog-shortcuts-file* nil)

;; True if we should generate month indices and link them into the
;; calendar.
;; Bound to the "month-index" parameter in the *weblog-init-file*
(defvar *weblog-generate-month-index-p*)

;; The text of the link generated by the {pl "name"} macro
;; Bound to the "pl-macro-text" parameter in the *weblog-init-file*
(defvar *weblog-pl-macro-text* "#")

;; A list of domains links to which will be followed by a {BugMeNot "..."} 
;; link.
(defvar *weblog-bugmenot-auto-list* nil)

;; The parsed shortcuts
(defvar *weblog-shortcuts* nil)

;; Bound to the list of file names while generating the index page
(defvar *weblog-index-files* nil)

;; Bound to true while generating RSS
(defvar *weblog-generating-rss* nil)

;; This variable is bound to the story text during macro expansion
;; or to 'generate-index during index page generation.
(defvar *weblog-story-content* "")

;; Bound to the file being generated
(defvar *weblog-story-file* nil)

;; The mod time of the generated file
(defvar *weblog-story-modtime* nil)

;; True if saving a story, not a day page (or the index.html file)
(defvar *weblog-saving-story* nil)

;; The template file for one day's page
;; This file's content is expanded for each generated html page.
(defconst *weblog-page-template-file* "page-template.tmpl")

;; The template for the content section of a page
;; The {contentTemplate} macro expands into the contents of this file.
(defconst *weblog-content-template-file* "content-template.tmpl")

;; The template for the content section of a non top-level page
;; The {contentTemplate} macro expands into the contents of this file.
(defconst *weblog-story-template-file* "story-template.tmpl")

;; The template file for a single day's weblog entry.
;; The {dayTemplate} macro expands into the contents of this file.
(defconst *weblog-day-template-file* "day-template.tmpl")

(defconst *weblog-escape-string* "\\")
(defconst *weblog-escape-char* (string-to-char *weblog-escape-string*))
(defconst *weblog-equal-sign-char* (string-to-char "="))
(defconst *weblog-at-sign-char* (string-to-char "@"))
(defconst *weblog-newline-char* (string-to-char "\n"))
(defconst *weblog-lt-char* (string-to-char "<"))
(defconst *weblog-sharp-sign-char* (string-to-char "\#"))
(defconst *weblog-char-map*
  '(("\226" "--")
    ("\227" "--")
    ("\222" "'")
    ("\223" "\"")
    ("\224" "\"")
    ("\221" "'")
    ("\205" "...")
    ("\240" "")
    ))

;; Append the *weblog-directory* to the given filename
(defun weblog-file (file)
  (let ((res (concat *weblog-directory* file)))
    (if (file-exists-p res)
        res
      (let ((first-two (and (>= (length file) 2) (substring file 0 2))))
        (if (not first-two)
            res
          (let ((other-res (concat *weblog-directory* first-two "/" file)))
            (if (file-exists-p other-res)
                other-res
              res)))))))

(defmacro weblog-while-visiting-file (buf-var file &rest body)
  "Execute BODY with BUF-VAR bound to a buffer containing FILE."
  (let ((exists-var (gensym)))
    `(save-excursion
      (let* ((,exists-var (weblog-find-or-visit ,file))
             (,buf-var (current-buffer)))
        (unwind-protect
            (progn ,@body)
          (unless ,exists-var (kill-buffer ,buf-var)))))))

(defmacro weblog-while-visiting-weblog-file (buf-var file &rest body)
  "Execute BODY with BUF-VAR bound to a buffer containing (weblog-file FILE)."
  `(weblog-while-visiting-file ,buf-var (weblog-file ,file) ,@body))

(defconst *weblog-init-param-descs*
  '(("site-name" *weblog-site-name*)
    ("byline" *weblog-byline*)
    ("author" *weblog-author*)
    ("email" *weblog-email*)
    ("url" *weblog-url*)
    ("ftp-directory" *weblog-ftp-directory*)
    ("index-days" *weblog-index-days* 
     (lambda (x) (car (read-from-string x))) integerp)
    ("shortcuts-file" *weblog-shortcuts-file*)
    ("month-index" *weblog-generate-month-index-p*
     (lambda (x) (equalp x "true")))
    ("pl-macro-text" *weblog-pl-macro-text*)
    ("bugmenot-auto-list" *weblog-bugmenot-auto-list*
     weblog-parse-space-separated-string))
  "A description for each parameter in the *weblog-init-file*
Each entry is of the form (PARAM VAR COERCE PRED), where
PARAM is the name of the parameter (before the equal sign),
VAR is the name of the variable to set to the parameter's value,
COERCE is a function of one argument to convert the value from a
string to what it needs to be, and
PRED is a predicate that is called on the coerced value. If the
value is nil, then an error is signalled.")

(defmacro weblog-with-init-params (file &rest body)
  "Execute BODY with the init param variables bound to values
found in DIRECTORY or one of its parent directories."
  `(weblog-funcall-with-init-params ,file (function (lambda () ,@body))))

(defun weblog-funcall-with-init-params (file thunk)
  "Call THUNK with the init param variables bound to values
found in FILE's directory or one of its parent directories."
  (let ((dir (weblog-seek-base-dir (file-name-directory file))))
    (when (null dir)
      (error "There is no %s in any directory leading to: %s"
             *weblog-init-file* (file-name-directory file)))
    (if (equal dir *weblog-directory*)
        (funcall thunk)
      (let ((*weblog-directory* dir)
            *weblog-site-name*
            *weblog-ftp-directory*
            *weblog-index-days*
            *weblog-shortcuts-file*
            *weblog-generate-month-index-p*
            *weblog-pl-macro-text*
            *weblog-bugmenot-auto-list*
            *weblog-shortcuts*)
        (weblog-parse-parameter-file)
        (funcall thunk)))))

(defun weblog-seek-base-dir (directory)
  "Find the *weblog-init-file* in DIRECTORY or one of its parents.
Return the name of that directory or nil if not found."
  (let ((file (weblog-seek-file directory *weblog-init-file*)))
    (and file (file-name-directory file))))

(defun weblog-seek-file (directory file-name)
  "Find FILE-NAME in DIRECTORY or one of its parents.
Return the full path or nil if not found."
  (loop
   (let ((file (concat directory file-name)))
     (when (file-exists-p file)
       (return (expand-file-name file)))
     (let ((parent (file-name-directory (directory-file-name directory))))
       (when (equal parent directory) (return nil))
       (setq directory parent)))))

(defun weblog-file-in-base-dir (file-name)
  (let ((dir (file-name-directory file-name)))
    (equalp dir (weblog-seek-base-dir dir))))

(defun weblog-parse-parameter-file (&optional dir)
  "Parse the *weblog-init-file* in directory DIR.
Set the variables associated with the parameters.
Return t if the file was found and all the parameters were OK.
Return nil if the file was not found.
Error if an unknown parameter is found in the file."
  (when (null dir) (setq dir *weblog-directory*))
  (let ((file (concat dir *weblog-init-file*)))
    (when (file-exists-p file)
      (weblog-while-visiting-file buf file
         (weblog-parse-init-file-buffer)))))

(defun weblog-parse-init-file-buffer ()
  "Parse the init file in the current buffer.
Set the variables associated with the parameters.
Return t if the file was found and all the parameters were OK.
Return nil if the file was not found.
Error if an unknown parameter is found in the file."
  (goto-char (point-min))
  (loop
   (let* ((point (point))
          (line-end (or (line-end-position) (point-max))))
     (unless (eql (char-after) *weblog-sharp-sign-char*)
       (let* ((equal-pos (search-forward "=" line-end t)))
         (when equal-pos
           (let* ((param (buffer-substring point (1- equal-pos)))
                  (value (buffer-substring equal-pos line-end))
                  (desc (assoc (downcase param) *weblog-init-param-descs*)))
             (unless desc
               (error "Unknown init parameter: %s" param))
             (multiple-value-bind (var coerce pred) (cdr desc)
               (let ((coerced-value value))
                 (when coerce
                   (setq coerced-value (funcall coerce value)))
                 (unless (or (null pred) (funcall pred coerced-value))
                   (error "Init parameter \"%s\" has illegal value: %s"
                          param value))
                 (set var coerced-value)))))))
       (when (eql line-end (point-max)) (return))
       (goto-char (1+ line-end))))
  (weblog-get-shortcuts)
  nil)

;; line-end-position doesn't exist in some versions of XEmacs
(unless (fboundp 'line-end-position)
(defun line-end-position ()
  (let ((point (point)))
    (end-of-line)
    (prog1 (point) (goto-char point))))
)

(defvar *weblog-shortcuts-alist* nil
  "Map directories to shortcuts values")

(defun weblog-get-shortcuts ()
  "Set *weblog-shortcuts* from either the *weblog-shortcuts-alist*
or by loading *weblog-shortcuts=file* from *weblog-directory*."
  (let ((elt (assoc *weblog-directory* *weblog-shortcuts-alist*)))
    (if elt
        (setq *weblog-shortcuts* (cdr elt))
      (weblog-load-shortcuts))))  

;; Load the *weblog-shortcuts-file* and put the result in *weblog-shortcuts*
(defun weblog-load-shortcuts ()
  "Load the *weblog-shortcuts* from file in *weblog-directory*.
File defaults to *weblog-shortcuts-file*"
  (let (s)
    (when (file-exists-p (weblog-file *weblog-shortcuts-file*))
      (weblog-while-visiting-weblog-file buf *weblog-shortcuts-file*
	(goto-char (point-min))
	(setq s (weblog-safe-read
		 (concat "Error parsing " *weblog-shortcuts-file*)
		 buf))))
    (setq *weblog-shortcuts*
	  (mapcar '(lambda (x) (cons (downcase (car x)) (cdr x))) s))
    (let ((elt (assoc *weblog-directory* *weblog-shortcuts-alist*)))
      (if elt
	  (setf (cdr elt) *weblog-shortcuts*)
	(push (cons *weblog-directory* *weblog-shortcuts*)
	      *weblog-shortcuts-alist*)))
    (length *weblog-shortcuts*)))

(defun weblog-safe-read (error-string &optional stream)
  "(read stream), but (error error-string) if an error happens"
  (condition-case errvar (read stream) (error (error error-string))))

(defun weblog-reload-shortcuts ()
  "Reload the shortcuts for the file of the current buffer"
  (interactive)
  (weblog-with-init-params (buffer-file-name)
     (weblog-load-shortcuts)))

;; Expand a buffer
;; Replace double newlines with <p>
;; Replace {fun args...} with the result of evaluating (fun args...)
;; Replace double quoted substrings with their entry in *weblog-shortcuts*
(defun weblog-expand-buffer (&optional leave-escapes file)
  "Expand the macros & shortcuts in the current buffer."
  (interactive)
  (weblog-with-init-params (or file (buffer-file-name))
    (loop
     (let ((cnt (weblog-expand-macros)))
       (if (eql 0 cnt) (return))))
    (weblog-insert-bugmenot-macros)
    (weblog-add-paragraphs)
    (unless leave-escapes (weblog-remove-escapes))))
  
(defun search-forward-non-escaped (string &optional limit)
  (loop
   (let  ((pos (search-forward string limit t)))
     (if (null pos) (return nil))
     (setq pos (- pos (length string)))
     (unless (eq *weblog-escape-char* (char-before pos))
       (return pos)))))

(defun weblog-do-replacement (f start-delim end-delim &optional keep-delims)
  "Find all one-line strings between START-DELIM and END-DELIM.
Call F on each one. If F returns non-NIL, replace the string and
the delimiters with the returned value. If KEEP-DELIMS is true,
replace only the string."
  (goto-char (point-min))
  (let* ((cnt 0)
         (start-len (length start-delim))
         (end-len (if (null end-delim) 0 (length end-delim)))
         (total-len (+ start-len end-len))
         pos end)
    (loop
     (setq pos (search-forward-non-escaped start-delim nil))
     (if (null pos) (return cnt))
     (if (null end-delim)
         (setq end (point))
       (setq end (search-forward-non-escaped end-delim (line-end-position))))
     (unless (null end)
       (incf pos start-len)
       (let* ((s (buffer-substring pos end))
              (new-s (funcall f s)))
         (unless (null new-s)
           (unless (stringp new-s)
             (setq new-s (format "%s" new-s)))
           (incf cnt)
           (backward-delete-char (+ total-len (- end pos)))
           (unless (stringp new-s)
             (setq new-s (with-output-to-string (princ new-s))))
           (when keep-delims (setq new-s (concat start-delim new-s end-delim)))
           (insert new-s)))))))

(defun weblog-convert-shortcuts ()
  "Convert shortcuts from double-quote delimited to {=....} delimited"
  (interactive)
  (weblog-with-init-params (buffer-file-name)
    (if (null *weblog-shortcuts*) (weblog-load-shortcuts))
    (weblog-do-replacement
     '(lambda (s)
        (if (weblog-lookup-shortcut s) (concat "{=" s "}")))
     "\"" "\"")))

(defun weblog-directory-files (dir &optional full pattern descending year-dirs)
  "Like directory-files, but matches pattern, sorted descending if true, and includes a list of sub-directories in year-dirs"
  (let* ((pat (or pattern ".*\.txt"))
         (files (directory-files dir full pat t))
         (pred (if descending 'weblog-string-greaterp 'string-lessp))
         (need-key nil))
    (dolist (yd year-dirs)
      (let ((yf (directory-files (concat dir yd "/") full pat t)))
        (when yf
          (setq files (nconc files yf)
                need-key t))))
    (if (and full need-key)
        (sort* files pred :key 'file-name-nondirectory)
      (sort files pred))))

(defun weblog-map-directory (dir f &optional pred pattern descending year-dirs)
  "(funcall F) for every file name in DIR that satisifies PRED.
Consider only files that match the regexp PATTERN, which defaults
to all text files.
Opens each file in a buffer before doing (funcall F)."
  (let* ((files (weblog-directory-files dir t pattern descending year-dirs)))
    (dolist (file files)
      (when (or (null pred) (funcall pred file))
        (weblog-while-visiting-file buf file
          (goto-char (point-min))
          (funcall f)
          (set-buffer buf)
          (save-buffer))))))

;; Strangely, emacs doesn't implement string-greaterp
(defun weblog-string-greaterp (s1 s2)
  (string-lessp s2 s1))

(defun weblog-map-all-files (dir f &optional pred pattern)
  "(funcall F file-name) for each file in DIR and its sub-directories
that satisfies pred and matches the regexp PATTERN, which defaults to
all text files."
  (dolist (file (directory-files dir t (or pattern ".*\.txt")))
    (when (or (null pred) (funcall pred file))
      (funcall f file)))
  (dolist (file (directory-files dir t))
    (when (file-directory-p file)
      (let ((name (file-name-nondirectory file)))
        (unless (or (equal name ".") (equal name ".."))
          (weblog-map-all-files file f pred pattern))))))

(defun dir-convert-shortcuts (dir)
  "This is a one-timer to convert from the old to the new shortcut syntax"
  (weblog-map-directory dir 'weblog-convert-shortcuts))

(defun dir-convert-macro-shortcuts (dir)
  "A one-timer to change {= to {@ in every text file in a directory"
  (weblog-map-directory
   dir
   '(lambda ()
      (replace-string "{=" "{@"))))

(defun weblog-lookup-shortcut (name)
  "Look up NAME in the *weblog-shortcuts*. Return its value or nil."
  (cadr (assoc (downcase name) *weblog-shortcuts*)))

(defun weblog-add-paragraphs ()
  (weblog-do-replacement
   '(lambda (s) "\n<p>\n")
   "\n\n" nil))

(defun weblog-expand-macros ()
  (weblog-do-replacement
   '(lambda (s)
      (ignore-errors
        (if (eq 0 (length s))
            nil
          (cond ((eq *weblog-at-sign-char* (elt s 0))
                 ;; {@shortcut} looks up shortcut
                 (weblog-lookup-shortcut (substring s 1)))
                ((eq *weblog-equal-sign-char* (elt s 0))
                 ;; {=forms...} evals (forms...}
                 (let ((form (car (read-from-string
                                   (concat "(" (substring s 1) ")")))))
                   (eval form)))
                (t
                 ;; {forms...} evals (weblog-macro-forms...)
                 (let ((form (car (read-from-string
                                   (concat "(weblog-macro-" s ")")))))
                   (eval form)))))))
   "{" "}"))

(defun weblog-remove-escapes ()
  "Remove escape characters (\"\\\") from the current buffer"
  (goto-char (point-min))
  (loop
   (let ((pos (search-forward *weblog-escape-string* nil t)))
     (if (null pos) (return))
     (backward-delete-char 1)
     (forward-char))))

(defun weblog-buffer-contents ()
  "Return the contents of the current buffer"
  (buffer-substring (point-min) (point-max)))

(defun weblog-save-story ()
  "Save the current buffer as a weblog story
using the *weblog-story-template-file*"
  (interactive)
  (let ((*weblog-content-template-file* *weblog-story-template-file*)
        (*weblog-saving-story* t))
    (weblog-save)))

(defun weblog-story-file-p (file-name)
  (let* ((y (third (weblog-file-mdy file-name)))
         (dir (downcase (file-name-directory file-name)))
         (file (file-name-nondirectory file-name))
         (file-year (substring file 0 (min 2 (length file))))
         (sep-char (substring dir (max 0 (1- (length dir))) (length dir))))
    (and (not (equal dir (downcase *weblog-directory*)))
         (not (equal dir
                     (downcase
                      (concat *weblog-directory* file-year sep-char)))))))

(defun weblog-save (&optional template)
  "Save the current buffer to an html file with the same name
using the *weblog-page-template-file*."
  (interactive)
  (let ((file-name (buffer-file-name))
        (*weblog-content-template-file* *weblog-content-template-file*)
        (*weblog-saving-story* *weblog-saving-story*))
    (weblog-with-init-params file-name
      (if (null file-name)
        (message "No file for current buffer")
        (message "%s" (concat "Saving " file-name "..."))
        (when (weblog-story-file-p file-name)
          (setq *weblog-content-template-file* *weblog-story-template-file*
                *weblog-saving-story* t))
        (let* ((content (weblog-buffer-contents))
               (ext (file-name-extension file-name))
               (ext-len (if (null ext) 
                            (progn (setq file-name (concat file-name ".")) 0)
                          (length ext)))
               (html-file (concat
                           (substring file-name
                                      0 (- (length file-name) ext-len))
                           "html")))
          (weblog-save-internal file-name html-file content template))))))

(defun weblog-save-internal (file-name html-file content &optional template)
  (if (null template) (setq template *weblog-page-template-file*))
  (weblog-while-visiting-file buf html-file
    (let ((*weblog-story-content* content)
          (*weblog-story-file* file-name)
          (*weblog-story-modtime* (nth 5 (file-attributes file-name))))
      (erase-buffer)
      (insert-file-contents (weblog-find-template template) nil nil nil t)
      (weblog-expand-buffer)
      (set-buffer buf)
      (save-buffer))))

(defun weblog-find-template (template)
  "Search for a file named TEMPLATE in the directory
of the *weblog-story-file* or one of its parents."
  (weblog-seek-file (file-name-directory *weblog-story-file*) template))

(defun weblog-find-or-visit (file)
   "Switch to a file's buffer.
If it existed already return true. Otherwise, return false."
   (or  (let ((buf (find-buffer-visiting file)))
          (when buf
            (set-buffer buf)
            t))
        (let ((buf (find-file-noselect file t)))
          (set-buffer buf)
          nil)))

(defun weblog-upload (&optional dont-upload-source file-name)
  "Upload the current buffer to the FTP directory.
Upload only the HTML file for a text file if dont-upload-source is true.
If FILE-NAME is non-nil, upload that file and don't generate html."
  (interactive)
  (let ((file (or file-name (buffer-file-name))))
    (weblog-with-init-params file
      (let* ((buf (current-buffer))
             (textp (equalp (file-name-extension file) "txt"))
             (html-name (if textp
                            (concat (file-name-sans-extension file) ".html")
                          file))
             (name (weblog-file-relative-name html-name *weblog-directory*)))
        (unless file-name
          (if textp
              (weblog-save-both)
            (when (buffer-modified-p) (save-buffer))))
        (if (eq name file)
            (message "Buffer not in *weblog-directory*")
          (let ((ftp-name (concat *weblog-ftp-directory* name))
                (source (if textp html-name file)))
            ;; Don't use copy-file here. It doesn't work on my FTP server.
            (weblog-write-text-to-file (weblog-absolute-file-contents source)
                                       ftp-name)
            (when (and textp (not dont-upload-source))
              (setq ftp-name (concat (file-name-sans-extension ftp-name) ".txt"))
              (weblog-write-text-to-file (weblog-absolute-file-contents file)
                                         ftp-name))))
        (when textp
          (set-buffer buf)
          (let ((latest-text-file (weblog-latest-text-file)))
            (when (and latest-text-file
		       (weblog-file-in-base-dir file)
                       (equal (weblog-file-mdy file)
                              (weblog-file-mdy (weblog-file latest-text-file))))
              (weblog-make-rss latest-text-file))))))))

;; file-relative-name can include "\" characters in XEmacs.
;; Change them to the canonical "/".
(defun weblog-file-relative-name (filename directory)
  (weblog-replace-strings (file-relative-name filename directory) "\\" "/"))

(defun weblog-write-text-to-file (text file-name)
  "Write text to file-name"
  (let ((buf (create-file-buffer file-name)))
    (set-buffer buf)
    (insert text)
    (unwind-protect
        (write-file file-name)
      (set-buffer-modified-p nil)
      (kill-buffer buf))))

;; Regular expression to match the weblog files
(defconst *weblog-file-regexp* "^[0-9][0-9][0-9][0-9][0-9][0-9]\.txt$")

(defun weblog-year-dirs (&optional dir)
  (when (null dir)
    (setq dir *weblog-directory*))
  (mapcan '(lambda (f)
             (and (file-directory-p f) (list (file-name-nondirectory f))))
          (directory-files dir t "^[0-9][0-9]$")))

(defun weblog-make-index ()
  "Make the index.html file with *weblog-index-days* days of data"
  (interactive)
  (weblog-with-init-params (buffer-file-name)
    (let* ((*weblog-index-files*
            (nreverse (last
              (delete-if '(lambda (file)
                            (apply 'weblog-mdy-in-future-p
                                   (weblog-file-mdy file)))
                (weblog-directory-files
                 *weblog-directory*
                 nil
                 *weblog-file-regexp*
                 nil
                 (weblog-year-dirs)))
              *weblog-index-days*)))
           (first-file (car *weblog-index-files*)))
           (weblog-save-internal
            first-file (weblog-file "index.html") 'generate-index))))

(defun weblog-upload-index (&optional index-only)
  "Create and upload the index file.
If prefix arg is 0, generate and upload this month's files
to regenerate their calendars.
If prefix arg is 1, don't upload anything, just generate the index.
If prefix arg is 2, upload all text files in the current directory and its sub-directories"
  ;; Don't use 4 for index-only. This should mean to upload the index only
  (interactive "p")
  (weblog-with-init-params (buffer-file-name)
    (weblog-make-index)
    (let ((month-file (and *weblog-generate-month-index-p*
                           (weblog-month-index))))
      (unless (eql 1 index-only)
        (weblog-while-visiting-weblog-file buf "index.html"
          (weblog-upload))
        (when month-file
          (weblog-while-visiting-weblog-file buf month-file
            (weblog-upload))
          (weblog-maybe-upload-previous-month-file))))
    (cond ((eql 0 index-only) (weblog-upload-month))
          ((eql 2 index-only) (weblog-upload-directory-text)))))

(defun weblog-upload-directory-text (&optional file)
  "Regenerate html for and upload every \".txt\" file
in the current directory and all of its sub-directories"
  (unless file (setq file (buffer-file-name)))
  (weblog-map-all-files
   (file-name-directory file)
   '(lambda (file)
      (weblog-while-visiting-file buf file (weblog-upload)))))

(defun weblog-upload-last-month ()
  "Upload last month's entries.
Useful on the first day of a month to update the calendars in last
month's entries."
  (interactive)
  (weblog-with-init-params (buffer-file-name)
    (weblog-upload-month (nth 4 (decode-time (current-time))))))

(defun weblog-todays-mdy ()
  "Return (month day year) for today"
  (let* ((time (decode-time (current-time))))
    (list (nth 4 time)(nth 3 time) (nth 5 time))))

(defun weblog-upload-month (&optional month year)
  "Convert to HTML and upload all the weblog files for a month.
You need to do this to update the calendars to have days later than
when each day was initially generated. At the beginning of the next
month, you need to do it for the previous month to make the next month
link below the calendar point to the first day in this month.
MONTH and YEAR default to today's month and year."
  (interactive)
  (weblog-with-init-params (buffer-file-name)
    (let* ((time (decode-time (current-time)))
           (cur-day (nth 3 time))
           (cur-month (nth 4 time))
           (cur-year (nth 5 time))
           (cur-mdy (list cur-month cur-day cur-year))
           (mdy nil)
           (latest-text-file (weblog-latest-text-file)))
      (when (null year) (setq year cur-year))
      (when (null month)
        (if (and (eq year cur-year)
                 (equal (weblog-first-text-file-this-month)
                        latest-text-file))
            (setq month (weblog-latest-month-before cur-month cur-year))
         (setq month cur-month)))
      (weblog-map-directory
       *weblog-directory*
       (function (lambda ()
                   (let ((buf (current-buffer)))
                     (weblog-save-both)
                     (set-buffer buf)
                     (weblog-upload (not (equal mdy cur-mdy))))))
       (function (lambda (file-name)
                   (setq mdy (weblog-file-mdy file-name))
                   (or (equal (file-name-nondirectory file-name)
                              latest-text-file)
                       (and (eq month (extract-calendar-month mdy))
                            (eq year (extract-calendar-year mdy))
                            (not (apply 'weblog-mdy-in-future-p mdy))))))
       *weblog-file-regexp*)
      nil
      (list (weblog-format-2d month)))))

(defun weblog-latest-month-before (month year)
  (let ((files (nreverse (weblog-directory-files
                          *weblog-directory* nil *weblog-file-regexp* nil
                          (weblog-year-dirs)))))
    (dolist (file files)
      (let ((mdy (weblog-file-mdy file)))
        (when (or (< (third mdy) year)
                  (and (eql (third mdy) year)
                       (< (first mdy) month)))
          (return (first mdy)))))))

(defun weblog-zero-pad (width string)
  "Pad STRING on the left with "0"'s to make it WIDTH chars wide"
  (let ((cnt (- width (length string))))
    (dotimes (i cnt)
      (setq string (concat "0" string))))
  string)

(defun weblog-rss-format-time (time &optional gmt-p)
  "Format a TIME as returned by decode-time for inclusion in an RSS file.
Translate to GMT if GMT-P is true."
  (let* ((second (first time))
         (minute (second time))
         (hour (third time))
         (date (fourth time))
         (month (fifth time))
         (year (sixth time))
         (dow (seventh time))
         ;(dst (eighth time))
         (gmt-offset (ninth time)))
    (when gmt-p
      (decf second gmt-offset)
      (multiple-value-setq (second minute hour date month year dow)
        (decode-time (encode-time second minute hour date month year))))
    (concat (calendar-day-name dow 3 t)
;;m3m changes to make date RFC 822 compliant
;;            (format ", %d " date)
	    ", "
            (weblog-zero-pad 2 (format "%d" date))
	    " "
;;            (calendar-month-name month)
            (calendar-month-name month 3)
;;m3m end changes
            (format " %d " year)
            (weblog-zero-pad 2 (format "%d" hour))
            ":"
            (weblog-zero-pad 2 (format "%d" minute))
            ":"
            (weblog-zero-pad 2 (format "%d" second)))))

(defun weblog-latest-text-file ()
  "Return the latest text file that is not in the future."
  (let ((files (weblog-directory-files
                *weblog-directory* nil
                *weblog-file-regexp* t
                (weblog-year-dirs))))
    (dolist (file files)
      (unless (apply 'weblog-mdy-in-future-p (weblog-file-mdy file))
        (return file)))))

(defun weblog-first-text-file-this-month ()
  "Return the latest text file that is not in the future."
  (let* ((mdy (weblog-todays-mdy))
	 (month (first mdy))
	 ;(day (second mdy))
	 (year (third mdy))
	 (regexp (concat "^"
			 (weblog-zero-pad 2 (format "%d" (% year 100)))
			 (weblog-zero-pad 2 (format "%d" month))
			 "[0-9][0-9]\.txt$")))
    (car (weblog-directory-files *weblog-directory* nil regexp nil (weblog-year-dirs)))))

(defun weblog-replace-xml-tag-text (start-tag end-tag new-text)
  "Replace the text between START-TAG and END-TAG with NEW-TEXT.
Delete the entire tag if NEW-TEXT is null."
  (goto-char (point-min))
  (let* ((start (search-forward start-tag))
         (end (- (search-forward end-tag) (length end-tag))))
    (if (null new-text) (decf start (length start-tag)))
    (goto-char start)
    (delete-char (- end start))
    (if new-text
        (insert new-text)
      (delete-char (length end-tag)))))

(defun weblog-replace-strings (text from to)
  "Replace each instance of FROM with TO in TEXT"
  (let ((res "")
        (pos 0)
        (len (length text))
        (from-len (length from))
        end)
    (loop
     (setq end (search from text :start2 pos))
     (unless end
       (return (if (eq pos 0) text (concat res (substring text pos)))))
     (setq res (concat res (substring text pos end) to))
     (setq pos (+ end from-len))
     (if (>= pos len) (return res)))))

(defun weblog-neuter-tags (text &optional macros-too)
  "In TEXT, change \"&\", \"<\", and \">\" to their HTML entities.
If MACROS-TOO is true, escape \"{\" with \"\\\"."
  (setq text (weblog-replace-strings text "&" "&amp;"))
  (setq text (weblog-replace-strings text "<" "&lt;"))
  (setq text (weblog-replace-strings text ">" "&gt;"))
  (when macros-too
    (setq text (weblog-replace-strings text "\\" "\\\\"))
    (setq text (weblog-replace-strings text "{" "\\{")))
  text)

(defun weblog-neuter-blank-lines (text)
  "In TEXT, add a space to blank lines"
  (weblog-replace-strings text "\n\n" "\n \n"))

;; This isn't used any more. I'm not parsing out links.
;; I just put everything in the <description>
(defun weblog-parse-out-links (start end)
  "Parse out the links between START and END in the current buffer.
Return (text (url1 . text1) (url2 . text2) ...)"
  (save-excursion
    (goto-char start)
    (let* ((res "")
           (tags nil)
           (pos start)
           (start-tag "<a href=\"")
           (start-tag-len (length start-tag))
           (end-tag "</a>")
           (end-tag-len (length end-tag)))
      (loop
       (let ((tag-pos (search-forward start-tag end t)))
         (when (null tag-pos)
           (setq res (concat res (weblog-neuter-tags (buffer-substring pos end))))
           (return))
         (setq res (concat res
                           (weblog-neuter-tags
                            (buffer-substring pos (- tag-pos start-tag-len)))))
         (setq pos tag-pos)
         (setq tag-pos (search-forward "\"" end t))
         (if (null tag-pos) (return))
         (let ((url (weblog-neuter-tags (buffer-substring pos (1- tag-pos))))
               url-text)
           (setq pos (search-forward ">" end t))
           (if (null pos) (return))
           (when (eq (char-after) *weblog-newline-char*)
             (incf pos))
           (setq tag-pos (search-forward end-tag end t))
           (if (null tag-pos) (return))
           (setq url-text (weblog-neuter-tags
                           (buffer-substring pos (- tag-pos end-tag-len))))
           (push (cons url url-text) tags)
           (setq res (concat res url-text)))
         (setq pos tag-pos)))
      (cons res (nreverse tags)))))

(defun weblog-map-urls (f)
  "For each url in the current buffer replace the url text
with the result of (funcall F url-text).
Currently, a url is the string after \"href=\" or \"img=\"."
  (weblog-do-replacement f "href=\"" "\"" t)
  (weblog-do-replacement f "src=\"" "\"" t))

(defun weblog-make-urls-absolute ()
  "Make all the URLs absolute in the current buffer.
Do this by prepending the *weblog-url* to relative URLs."
  (weblog-map-urls
   '(lambda (url)
      (if (weblog-url-absolute-p url)
          url
        (concat *weblog-url* url)))))

(defun weblog-url-absolute-p (url)
  "Return true if the url string is relative.
Currently this means it contains \"http://\" or \"ftp://\""
  (or (search "http://" (downcase url))
      (search "ftp://" (downcase url))))

(defun weblog-make-rss (&optional text-file dont-upload)
  "Create rss.xml from rss-template.xml and the newest html file.
Upload it to the FTP server."
  (interactive "i\np")
  (when (integerp dont-upload)
    (setq dont-upload (eql dont-upload 1)))
  (weblog-with-init-params (buffer-file-name)
    (unless text-file
      (setq text-file (weblog-latest-text-file)))
    (weblog-while-visiting-weblog-file rss-buf "rss.xml"
      (let* ((*weblog-generating-rss* t)
             (template (weblog-file-contents "rss-template.xml"))
             (text (weblog-file-contents text-file))
             (time (decode-time (current-time)))
             ;(pub-time (append '(0 0 0) (cdddr time)))
             (gmt-time (weblog-rss-format-time time t))
             (time-string (concat gmt-time " GMT"))
             ;(pub-time-string (concat (weblog-rss-format-time pub-time t)
             ;                         " GMT"))                            
             (html-buf (create-file-buffer text-file))
             (idx 0))
        (set-buffer rss-buf)
        (erase-buffer)
        (insert template)
        (weblog-replace-xml-tag-text "<pubDate>" "</pubDate>" time-string) ;;pub-time-string)
        (weblog-replace-xml-tag-text "<lastBuildDate>" "</lastBuildDate>" time-string)
        (weblog-replace-xml-tag-text "<item>" "</item>" nil)
        (backward-delete-char 1)
        (set-buffer html-buf)
        (insert text)
        (let ((*weblog-story-file* text-file)
              (*weblog-story-modtime* (nth 5 (file-attributes text-file))))
          (weblog-expand-buffer nil (weblog-file text-file)))
        (set-buffer html-buf)
        (weblog-make-urls-absolute)
        (goto-char (point-min))
        (loop
         (let* ((start (point))
                (end (search-forward "<p>" nil t))
                (real-end (if end (- end 3) (point-max)))
                (text (buffer-substring start real-end))
                (link (cadr (weblog-parse-out-links start real-end))))
           (setq text (weblog-neuter-tags text))
	   ;; This is for the Feedreader browser
	   (setq text (weblog-replace-strings text "\n" " \n"))
           (set-buffer rss-buf)
           ;; Insert the link and title
           (insert "\n    <item>\n")
           ;;(insert "      <title>")
           ;;(if link (insert (cdr link)) (insert "No link"))
           ;;(insert "</title>\n")
           ;;(insert "      <link>")
           ;;(if link (insert (car link)) (insert *weblog-url*))
           ;;(insert "</link>\n")
           ;; Insert the description
           (insert "      <description>")
           (insert text)
           (insert "      </description>\n")
           (insert "      <guid>")
           (insert gmt-time)
           (insert "-")
           (insert (with-output-to-string (princ (incf idx))))
           (insert "</guid>\n")
           (insert "    </item>")
           (set-buffer html-buf)
           (unless end (return))
           (if (eql (point) (point-max)) (return))
           (goto-char end)))
        (kill-buffer html-buf)
        (set-buffer rss-buf)
        (save-buffer)
        (unless dont-upload
          (weblog-upload))))))

(defun weblog-parse-space-separated-string (x)
  (let ((s 0)
        (space (elt " " 0))
        e res)
    (loop
     (setq e (position space x :start s))
     (when (null e)
       (setq e (length x))
       (if (<= e s) (return (nreverse res))))
     (push (substring x s e) res)
     (setq s (1+ e)))))

(defun weblog-insert-bugmenot-macros ()
  (unless (null *weblog-bugmenot-auto-list*)
    (save-excursion
      (let ((s 0) e domain)
        (goto-char (point-min))
        (loop
         (setq s (search-forward "<a href=\"http://" nil t))
         (if (null s) (return))
         (setq e (search-forward "/" nil t))
         (if (null e) (return))
         (if (null (search-forward "</a>" nil t)) (return))
         (let* ((link " <a href=\"http://www.bugmenot.com/")
                (link-len (length link))
                (end (+ (point) link-len)))
           ;; This condition is necessary in case this function is run
           ;; more than once on the same buffer, which happens when the
           ;; index page is generated.
           (unless (and (>= (point-max) end)
                        (equal link (buffer-substring (point) end)))
             (setq domain (buffer-substring s (1- e)))
             (dolist (d *weblog-bugmenot-auto-list*)
               (let ((p (search d domain)))
                 (when (and p (eql (+ p (length d)) (length domain)))
                   (insert " " (weblog-macro-bugmenot domain))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macros
;;;
;;; {macroName args...} evals (weblog-macro-macroName args...)
;;; The result replaces the macro call, unless it's null or it errors
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {contentTemplate} macro
(defun weblog-macro-contentTemplate ()
  (weblog-file-contents *weblog-content-template-file*))

;; {dayTemplate} macro
(defun weblog-macro-dayTemplate ()
  (cond ((eq 'generate-index *weblog-story-content*)
         (weblog-index-content "{dayTemplate}"))
        (t (weblog-file-contents *weblog-day-template-file*))))

(defun weblog-file-contents (file)
  "Read and return the contents of a file. Prepend the *weblog-directory*."
  (weblog-while-visiting-weblog-file buf file
     (weblog-buffer-contents)))

(defun weblog-absolute-file-contents (file)
  "Read and return the contents of a file."
  (weblog-while-visiting-file buf file
    (weblog-buffer-contents)))

(defun weblog-index-content (template)
  "Expand the template for each file in *weblog-index-files*"
  (save-excursion  ;save-window-excursion
    (let ((res "")
          (buf (set-buffer (create-file-buffer "*weblog-index-content*"))))
      (unwind-protect
          (progn
            (dolist (file *weblog-index-files*)
              (erase-buffer)
              (insert template)
              (let ((*weblog-story-content*
                     (weblog-file-contents file))
                    (*weblog-story-file* file))
                (weblog-expand-buffer t file)
                (setq res (concat res (weblog-buffer-contents)))))
            res)
        (kill-buffer buf)))))

;; {storyContent} macro
(defun weblog-macro-storyContent ()
  (let ((content *weblog-story-content*))
    (cond ((eq 'generate-index content)
           (weblog-index-content "{storyContent}"))
          (t content))))

;; {title} macro
;; This parses <a name="Story title"></a> into "Story title"
;; That's how I denote my story titles in a way that can pass
;; into the generated html
(defun weblog-macro-title ()
  (let* ((content *weblog-story-content*)
         (prefix "<a name=\"")   ;;;></a>
         (prefix2 "<!--")
         (prefix-len (length prefix))
         (prefix2-len (length prefix2)))
    (unless (stringp content)
      (setq content (weblog-file-contents *weblog-story-file*)))
    (let ((substring (substring content 0 prefix-len))
          (substring2 (substring content 0 prefix2-len)))
      (or
       (and (equal (downcase substring) prefix)
            (let ((pos (position (string-to-char "\"") content
                                 :start prefix-len)))
              (and pos (substring content prefix-len pos))))
       (and (equal substring2 prefix2)
            (let ((pos (search "-->" content)))
              (and pos (substring content prefix2-len pos))))
       "Untitled"))))

;; {siteName} macro. Return the weblog's name.
(defun weblog-macro-siteName ()
  *weblog-site-name*)

;; {byline} macro.
(defun weblog-macro-byline ()
  *weblog-byline*)

;; {author} macro
(defun weblog-macro-author ()
  *weblog-author*)

;; {email} macro
(defun weblog-macro-email ()
  *weblog-email*)

;; {url} macro
(defun weblog-macro-url ()
  *weblog-url*)

;; {lastUpdate} macro
(defun weblog-macro-lastUpdate ()
  (let ((modtime *weblog-story-modtime*))
    (if (and (listp modtime) (eq 2 (length modtime)))
        (format-time-string "%m/%d/%Y;&nbsp;%H:%M:%S" *weblog-story-modtime*)
      "unknown")))

;; {calendar} macro
;; Uses functions from 'calendar library:
;;   (calendar-last-day-of-month month year)
;;   (calendar-month-name month)
;;      1 = January
;;     12 = December
;;   (calendar-day-of-week (list month day year))
;;     0 = sunday
;;     6 = saturday
;; "file" is the file for the current day, default: the current story file
;; "start-day" is the day of the week for the first calendar column.
;; The default is 0, Sunday.
(defun weblog-macro-calendar (&optional file start-day)
  (if (null file) (setq file *weblog-story-file*))
  (if (null start-day) (setq start-day 0))
  (when (stringp file)
    (let ((mdy (weblog-file-mdy file)))
      (when mdy
	(let* ((month (extract-calendar-month mdy))
	       (year (extract-calendar-year mdy))
	       (date (extract-calendar-day mdy))
	       (day (calendar-day-of-week (list month 1 year)))
	       (last-day-of-month (calendar-last-day-of-month month year))
               (month-file (weblog-month-file-name month year ".html"))
               (month-name (calendar-month-name month)))
	  (with-output-to-string
	    (princ "<table border=\"0\" cellspacing=\"0\" cellpadding=\"1\">\n")
	    (princ "<tr><td colspan=\"7\"><center>")
            (if *weblog-generate-month-index-p*
                (princ (concat
                        "<a href=\""
                        month-file
                        "\">"
                        month-name
                        "</a>"))
              (princ month-name))
	    (princ " ")
            (princ year)
            (princ "</center></td></tr>\n<tr>\n")
            (dotimes (i 7)
              (princ "<td>")
              (let ((column-day (mod (+ i start-day) 7)))
                (weblog-princ-dayname (calendar-day-name column-day 3 t)))
              (princ "</td>\n"))          
            (princ "</tr><tr>\n")
            (setq day (mod (- day start-day) 7))
            (when (> day 0)
              (princ "<td colspan=\"")
              (princ day)
              (princ "\"></td>\n"))
            (dotimes (i last-day-of-month)
              (when (>= day 7)
                (princ "</tr><tr>\n")
                (setq day 0))
              (princ "<td align=\"center\">")
              (let* ((todayp (eq date (1+ i)))
                     (exists (and (not todayp)
				  (not (weblog-mdy-in-future-p
					month (1+ i) year))
                                  (weblog-day-file-exists month (1+ i) year))))
                (when exists
                  (princ "<a href=\"")
                  (princ exists)
                  (princ "\">"))
                (weblog-princ-day (1+ i) todayp)
                (when exists
                  (princ "</a>"))
                (princ "</td>\n")
                (incf day)))
            (princ "</tr><tr>\n<td align=\"center\" colspan=\"7\">")
            (let ((last-month (calendar-month-name (1+ (% (+ month 10) 12)) 3))
                  (next-month (calendar-month-name (1+ (% (+ month 12) 12)) 3))
                  (last-month-file (weblog-last-day-file-in-previous-month month year))
                  (next-month-file (weblog-first-day-file-in-next-month month year)))
              (when last-month-file
                (setq last-month
                      (concat "<a href=\""
                              last-month-file "\">"
                              last-month
                              "</a>")))
              (when next-month-file
                (setq next-month
                      (concat "<a href=\""
                              next-month-file "\">"
                              next-month
                              "</a>")))
              (weblog-princ-day (concat last-month "&nbsp;&nbsp;" next-month)))
            (princ "\n</td></tr>\n</table>\n")))))))

(defun weblog-mdy-lessp (mdy1 mdy2)
  (let ((m1 (first mdy1))
	(d1 (second mdy1))
	(y1 (third mdy1))
	(m2 (first mdy2))
	(d2 (second mdy2))
	(y2 (third mdy2)))
    (or (< y1 y2)
	(and (eq y1 y2)
	     (or (< m1 m2)
		 (and (eq m1 m2)
		      (< d1 d2)))))))

(defun weblog-mdy-in-future-p (month day year)
  "Say whether a (month day year) list is in the future"
  (let* ((time (decode-time (current-time)))
	 (cur-day (nth 3 time))
	 (cur-month (nth 4 time))
	 (cur-year (nth 5 time)))
    (weblog-mdy-lessp
     (list cur-month cur-day cur-year) (list month day year))))

(defun weblog-file-mdy (file-name)
  "Return (list month day year) for a filename of \"yymmdd.txt\""
  (ignore-errors
    (let* ((name (file-name-sans-extension
                  (file-name-nondirectory file-name)))
           (yy (weblog-integer-substring name 0 2 0))
           (mm (weblog-integer-substring name 2 4 1))
           (dd (weblog-integer-substring name 4 6 1)))
      (setq yy (if (< yy 70) (+ 2000 yy) (+ 1900 yy)))
      (list mm dd yy))))

(defun weblog-integer-substring (str start end default)
  (let ((res (ignore-errors
               (car (read-from-string (substring str start end))))))
    (if (integerp res) res default)))
        
(defun weblog-princ-dayname (dayname)
  "Print a single day name for weblog-macro-calendar"
  (princ "<font size=\"-2\" color=\"green\">")
  (princ dayname)
  (princ "</font>"))

(defun weblog-princ-day (day &optional todayp)
  "Print a single day for weblog-macro-calendar"
  (princ "<font size=\"-2\"")
  (if todayp (princ " color=\"red\""))
  (princ ">")
  (princ day)
  (princ "</font>"))

(defun weblog-day-file (month day year)
  "Return the name of the html file for the given day"
  (concat (weblog-format-2d (% year 100))
          (weblog-format-2d month)
          (weblog-format-2d day)
          ".html"))

(defun weblog-day-text-file (month day year)
  "Return the name of the html file for the given day"
  (concat (weblog-format-2d (% year 100))
          (weblog-format-2d month)
          (weblog-format-2d day)
          ".txt"))

(defun weblog-format-2d (n)
  "Format an integer as two characters with a leading zero"
  (let* ((s (format "%d" n)))
    (if (eq 1 (length s)) (setq s (concat "0" s)))
    s))

(defun weblog-day-file-exists (month day year)
  "Return true if the html file exists for a particular day"
  (let* ((file (weblog-day-file month day year))
         (other-file (concat (file-name-sans-extension file) ".txt")))
    (and (or (file-attributes (weblog-file file))
             (file-attributes (weblog-file other-file)))
         file)))

(defun weblog-last-day-file-in-previous-month (month year)
  "Find the last html file in the previous month"
  (decf month)
  (when (eq month 0)
    (setq month 12 year (1- year)))
  (dotimes (i 31)
    (let ((file (weblog-day-file-exists month (- 31 i) year)))
      (if file (return file))))
  (block map
    (weblog-map-directory
     *weblog-directory*
     nil;; mapping function. Won't ever be called
     (function (lambda (file-name)
                 (let* ((mdy (weblog-file-mdy file-name))
                        (m (extract-calendar-month mdy))
                        (y (extract-calendar-year mdy)))
                   (when (or (and (eql y year) (<= m month))
                             (< y year))
                     (return-from map (apply 'weblog-day-file mdy))))
                 nil))
     *weblog-file-regexp*
     t
     (weblog-year-dirs))))

(defun weblog-first-day-file-in-next-month (month year)
  "Find the first html file in the next month"
  (incf month)
  (when (> month 12)
    (setq month 1 year (1+ year)))
  (dotimes (i 31)
    (let ((file (weblog-day-file month (1+ i) year)))
      (if (file-attributes (weblog-file file))
          (return file))))
  (block map
    (weblog-map-directory
     *weblog-directory*
     nil;; mapping function. Won't ever be called
     (function (lambda (file-name)
                 (let* ((mdy (weblog-file-mdy file-name))
                        (m (extract-calendar-month mdy))
                        (y (extract-calendar-year mdy)))
                   (when (or (and (eql y year) (>= m month))
                             (> y year))
                     (return-from map (apply 'weblog-day-file mdy))))
                 nil))
     *weblog-file-regexp*
     nil
     (weblog-year-dirs))))

;; {storyDate} macro
(defun weblog-macro-storyDate ()
  (let ((mdy (weblog-file-mdy *weblog-story-file*)))
    (when mdy
      (let ((month (extract-calendar-month mdy))
            (year (extract-calendar-year mdy))
            (date (extract-calendar-day mdy)))
        (format "%s, %s %d, %d"
                (calendar-day-name mdy)
                (calendar-month-name month)
                date
                year)))))

;; {blogToplevel} macro
;; Example: {blogToplevel "stories/"}
;; A path from here to the top-level of the blog plus the optional
;; sub-dir path. This lets you generate relative links.
(defun weblog-macro-blogToplevel (&optional dir)
  (let* ((story-dir (file-name-directory *weblog-story-file*))
         (weblog-dir (if dir (concat *weblog-directory* dir)
                       *weblog-directory*))
         (rel (weblog-file-relative-name weblog-dir story-dir)))
    (if (equal rel "./") (setq rel ""))
    rel))

;;; </pre>

;; {dailyLink} macro
(defun weblog-macro-dailyLink (&optional link-text)
  (if (null *weblog-index-files*)
      ""           ;; Regular day page. Don't include link.
    ;; Creating index page. Include link.
    (let* ((file (concat
                  (file-name-sans-extension
                   (file-name-nondirectory *weblog-story-file*))
                  ".html")))
      (when (null link-text)
        (setq link-text
              "<img src=\"{blogToplevel}dailyLinkIcon.png\" alt=\"Daily\">"))
      (concat "<a href=\"{blogToplevel}" file
              "\" title=\"Permanent link to this day: "
              file "\">"
              link-text "</a>"))))

(defun weblog-add-shortcut (&optional name url)
  "Add a shortcut to the table.
If the name begins with \"=\", don't wrap an anchor tag around the url."
  (interactive "s Add Shortcut Named: 
s URL: ")
  (weblog-with-init-params (buffer-file-name)
    (let (entry)
      (if (and (> (length url) 0)
               (eql *weblog-equal-sign-char* (elt url 0)))
          (setq entry (list (downcase name) (substring url 1)))
        (setq entry (list (downcase name)
                          (concat "<a href=\"" url "\">" name "</a>"))))
      (if (null *weblog-shortcuts*)
          (setq *weblog-shortcuts* (list entry))
        (let ((old-entry (assoc (car entry) *weblog-shortcuts*)))
          (if old-entry
              (setf (cdr old-entry) (cdr entry))
            (setf (cdr (last *weblog-shortcuts*)) (list entry))))))
    (weblog-write-shortcuts)))

(defun weblog-write-shortcuts ()
  "Write the *weblog-shortcuts* to the *weblog-shortcuts-file*"
  (let* ((file (weblog-file *weblog-shortcuts-file*))
         (bak (concat file ".bak")))
    (when (file-exists-p file)
      (copy-file file bak t t))
    (weblog-while-visiting-file buf file
      (erase-buffer)
      (insert "(\n")
      (dolist (entry *weblog-shortcuts*)
        (prin1 entry buf)
        (insert "\n"))
      (insert ")\n")
      (save-buffer))))

;; {year} macro
(defun weblog-macro-year ()
  (with-output-to-string (princ (nth 5 (decode-time *weblog-story-modtime*)))))

;; {img "name" &optional alignment} macro
(defun weblog-macro-img (name &optional alignment)
  (let ((img (weblog-lookup-shortcut name)))
    (when img
      (when alignment
	(let ((pos (search "<img" img)))
	  (when pos
	    (let ((pos2 (search ">" img :start2 pos)))
	      (when pos2
		(setq img (concat (substring img 0 pos2)
				  " align=\"" alignment "\""
                                  " alt=\"" name "\""
				  (substring img pos2))))))))
      img)))

(defun weblog-in-story-directory (file)
  "Concatenates FILE with the directory of the current story"
  (concat (file-name-directory *weblog-story-file*) file))

;; {include} macro
(defun weblog-macro-include (file &optional neuter pre)
  (let ((real-file (weblog-in-story-directory file)))
    (let ((text (weblog-absolute-file-contents real-file)))
      (if neuter (setq text (weblog-neuter-tags text t)))
      (if pre
          (setq text
                (concat "<pre>" (weblog-neuter-blank-lines text) "</pre>")))
      text)))

;; {txtLink} macro
;; Insert a link to the ".txt" file for this page
(defun weblog-macro-txtLink (&optional link-text)
  (let* ((txt-file (concat (file-name-sans-extension *weblog-story-file*)
                           ".txt")))
    (if (file-exists-p txt-file)
        (let ((name (file-name-nondirectory txt-file)))
          (concat "<a href=\"" name "\">" (or link-text name) "</a>"))
      "")))

;; {jargon} macro
;; Insert a link to the Hacker's Dictionary entry for the argument.
;; e.g. {jargon "grok"} becomes
;;      <a href="http://www.catb.org/jargon/html/entry/grok.html">grok</a>
(defun weblog-macro-jargon (&optional entry link)
  (let ((first-letter nil))
    (if (stringp entry)
        (progn
          (setq first-letter (upcase (substring entry 0 1)))
          (unless (search first-letter "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
            (setq first-letter "0")))
    (setq entry nil))
  (if (null entry)
      "<a href=\"http://catb.org/esr/jargon/html/frames.html\">Hacker's Dictionary</a>"
    (concat "<a href=\"http://catb.org/esr/jargon/html/"
            first-letter "/" entry ".html\">"
            (or link entry)
            "</a>"))))

(defun weblog-macro-pl (name)
  (let* ((day-page-p (and (null *weblog-index-files*)
                          (not *weblog-generating-rss*)))
         (base-file (file-name-sans-extension
                     (file-name-nondirectory *weblog-story-file*))))
    (concat "<a "
            (if day-page-p (concat "name=\"" name "\" "))
            "href=\""
            (if day-page-p "" (concat base-file ".html"))
            "#"
            name
            "\" title=\"Permalink\">"
            (or *weblog-pl-macro-text* "#")
            "</a>")))

(defun weblog-insert-permalink ()
  (interactive)
  (insert "{pl \"\"}")
  (backward-char 2))
  
(defun weblog-macro-bugmenot (url)
  (concat "<a href=\"http://www.bugmenot.com/view.php?url="
          url
          "\">"
          "<img src=\""
          (weblog-macro-blogToplevel)
          "bugmenot.png\" border=\"0\" alt=\"BugMeNot\" width=\"16\" height=\"16\">"
          "</a>"))

(defun weblog-macro-bumper (back top-fore top-back top-msg bot-fore bot-back bot-msg)
  (concat "<span style=\"font-size:250%; font-weight: bold; font-family: sans-serif\">"
          "<table cellpadding=\"5\">"
          "<tr><td bgcolor=\"" back "\">"
          "<table cellpadding=\"5\" cellspacing=\"0\">"
          "<tr><td bgcolor=\"" top-back "\">"
          "<span style=\"color: " top-fore "\">"
          "<center>" top-msg "</center>"
          "</span></td></tr>"
          "<tr><td bgcolor=\"" bot-back "\">"
          "<span style=\"color: " bot-fore "\">"
          "<center>" bot-msg "</center>"
          "</span></td></tr>"
          "</table></span>"
          "</tr></td>"
          "</table>"
          "</span>"))

(defun weblog-macro-wikipedia (&optional entry link)
  (if (null entry)
      "<a href=\"http://www.wikipedia.org\">Wikipedia</a>"
    (concat "<a href=\"http://www.wikipedia.org/"
             entry "\">"
            (or link entry)
            "</a>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Month and Year indices
;;;

(defun weblog-month-file-name (month year &optional file-type)
  (concat (weblog-format-2d (% year 100))
          (weblog-format-2d month)
          (or file-type ".txt")))

(defun weblog-month-index (&optional file-name)
  "Create a month index file for the current day file.
Return the file name of the month file"
  (interactive)
  (let ((file (or file-name (buffer-file-name))))
    (when (null (file-name-directory file))
      (setq file (concat (file-name-directory (buffer-file-name)) file-name)))
    (weblog-with-init-params file
      (let* ((mdy (weblog-file-mdy file))
             (month (extract-calendar-month mdy))
             (month-name (calendar-month-name month))
             (year (extract-calendar-year mdy))
             (month-file (weblog-month-file-name month year))
             (dir (file-name-directory file))
             (month-path (concat dir month-file))
             (next-month (+ 1 (% month 12)))
             (next-year (if (eq next-month 1) (+ year 1) year))
             (prev-month (if (eq month 1) 12 (- month 1)))
             (prev-year (if (eq prev-month 12) (- year 1) year))
             (last-prev-month-file
              (weblog-last-day-file-in-previous-month month year))
             (first-next-month-file
              (weblog-first-day-file-in-next-month month year))
             (prev-file (weblog-month-file-for-day-file last-prev-month-file))
             (next-file (weblog-month-file-for-day-file first-next-month-file))
             (year-file (concat (weblog-format-2d (% year 100))
                                ".html"))
             (navigation-thunk
              (function
               (lambda()
                 (weblog-insert-same-dir-link
                  dir prev-file (concat (calendar-month-name prev-month)
                                        (if (eq prev-year year)
                                            ""
                                          (format " %d" prev-year))))
                 (insert (concat " | <b>" month-name " " (format "%d" year)))
                 (insert "</b> | ")
                 (weblog-insert-same-dir-link
                  dir next-file (concat (calendar-month-name next-month)
                                        (if (eq next-year year)
                                            ""
                                          (format " %d" next-year))))))))
        (weblog-while-visiting-file buf month-path
          (goto-char 0)
          (delete-char (buffer-size))
          (insert (concat "<!--" month-name (format " %d" year) "-->\n"))
          (funcall navigation-thunk)
          (insert "\n<p>\n")
          (weblog-insert-month-index-entries dir year month 1 year month 31)
          (insert "\n<p>\n")
          (funcall navigation-thunk)
          (save-buffer)
          (weblog-save-story)
          (weblog-month-file-name month year ".html"))))))

(defun weblog-month-file-for-day-file (day-file)
  "Return the month file that goes with a day file"
  (and day-file
       (concat (substring day-file 0 4) (substring day-file 6))))

;; This tends to make very large files.
;; So I haven't hooked it to any keys.
(defun weblog-year-index (&optional file-name)
  "Create a year index file for the current day file"
  (interactive)
  (let ((file (or file-name (buffer-file-name))))
    (weblog-with-init-params file
      (let* ((mdy (weblog-file-mdy file))
             (year (extract-calendar-year mdy))
             (year-name (format "%d" year))
             (year-file (concat (weblog-format-2d (% year 100))
                                 ".txt"))
             (dir (file-name-directory file))
             (year-path (concat dir year-file))
             (next-year (+ 1 year))
             (prev-year (- year 1))
             (prev-file (concat (weblog-format-2d (% prev-year 100))
                                ".html"))
             (next-file (concat (weblog-format-2d (% next-year 100))
                                ".html"))
             (navigation-thunk
              (function
               (lambda()
                 (weblog-insert-same-dir-link
                  dir prev-file (format " %d" prev-year))
                 (insert (concat " | <b>" year-name " "))
                 (insert "</b> | ")
                 (weblog-insert-same-dir-link
                  dir next-file (concat (format " %d" next-year)))))))
        (weblog-while-visiting-file buf year-path
          (goto-char 0)
          (delete-char (buffer-size))
          (insert (concat "<!--" year-name "-->\n"))
          (funcall navigation-thunk)
          (insert "\n<p>\n")
          (weblog-insert-month-index-entries dir year 1 1 year 12 31)
          (insert "\n<p>\n")
          (funcall navigation-thunk)
          (save-buffer)
          (weblog-save-story))))))

(defun weblog-insert-same-dir-link (dir file text)
  "Insert a link in the buffer to 'file', with 'text' as the link text.
Just insert 'text' if the 'file' does not exist in directory 'dir'"
  (let ((linkp (and dir file (file-exists-p (concat dir file)))))
    (when linkp
      (insert "<a href=\"")
      (insert file)
      (insert "\">"))
    (insert text)
    (when linkp (insert "</a>"))))

(defun weblog-insert-month-index-entries (dir start-year start-month start-day
                                              end-year end-month end-day)
  
  (do ((year start-year (1+ year)))
      ((> year end-year))
    (do ((month start-month (1+ month)))
        ((> month end-month))
      (do ((day start-day (1+ day)))
          ((> day end-day))
        (weblog-insert-day-index-entries dir month day year)))))

(defun weblog-insert-day-index-entries (dir month day year)
  (let* ((day-file (weblog-day-text-file month day year))
         (day-html (weblog-day-file month day year))
         (day-path (concat dir day-file))
         (buf (current-buffer))
         (buf-pos (point))
         (blank-line-pos 1)
         (title "Untitled")
         pos)
    (when (file-exists-p day-path)
      (weblog-insert-same-dir-link
       dir
       day-html
       (concat (calendar-day-name (list month day year))
               (format ", %d " day)
               (calendar-month-name month)))
      (insert ": ")
      (weblog-while-visiting-file day-buf day-path
        (set-buffer day-buf)
        (goto-char 0)
        (if (eql 5 (search-forward "<!--" nil t))
            (progn
              (setq blank-line-pos (search-forward "-->\n" nil t))
              (when blank-line-pos
                (setq title (buffer-substring 5 (- blank-line-pos 4)))))
          (goto-char 0))
        (set-buffer buf)
        (insert title)
        (insert "\n<ul>\n")
        (loop
         (set-buffer day-buf)
         (let* ((pos (search-forward "<a href=" nil t))
                (start-pos pos))
           (unless pos (return))
           (when (and blank-line-pos (< blank-line-pos pos))
             (let (next-blank-pos)
               (goto-char blank-line-pos)
               (loop
                (setq next-blank-pos (search-forward "\n\n" nil t))
                (when (or (null next-blank-pos) (> next-blank-pos pos))
                  (goto-char pos)
                  (return))
                (setq blank-line-pos next-blank-pos)))
              (set-buffer buf)
              (insert "<li>")
              (setq start-pos blank-line-pos)
              (setq blank-line-pos nil)
              (set-buffer day-buf))
           (unless blank-line-pos
             (setq blank-line-pos (search-forward "\n\n" nil t)))
           (goto-char pos)
           (let ((end-pos (search-forward "</a>" nil t)))
           (unless end-pos (return))
           (let ((string (buffer-substring (- pos 8) end-pos)))
             (when (< start-pos pos)
               ;; This is the first link after a double-newline
               (let ((header (buffer-substring start-pos (- pos 8))))
                 (setq string (concat (weblog-last-line header) string))))
             (set-buffer buf)
             (insert string)
             (insert "\n")))))
        (set-buffer buf)
        (insert "</ul>\n")
        (setq pos (point)))
      (goto-char pos))))

(defun weblog-last-line (string)
  "Return the last line of a string, not counting a trailing newline"
  (let ((len (length string)))
    (when (and (> len 0) (eq (elt "\n" 0) (elt string (1- len))))
      (setq string (substring string 0 (1- len)))))
  (let ((nl-pos (search "\n" string :from-end t)))
    (if nl-pos
        (substring string (1+ nl-pos))
      string)))

(defun weblog-make-all-month-indices  (&optional file-name)
  "Create month index files for all months that have a day file."
  ;;(interactive)
  (let* ((file (or file-name (buffer-file-name)))
         (dir (file-name-directory file))
         (files (weblog-directory-files *weblog-directory* nil *weblog-file-regexp* nil (weblog-year-dirs))))
    ;; Incomplete
    ))
    
(defun weblog-maybe-upload-previous-month-file (&optional file-name)
  "Create and upload last month's index if today is the first day of this month"
  (let* ((file (or file-name (buffer-file-name)))
         (mdy (weblog-file-mdy file))
         (month (extract-calendar-month mdy))
         (day (extract-calendar-day mdy))
         (year (extract-calendar-year mdy))
         (first-day-file-this-month
          (weblog-first-day-file-in-next-month (1- month) year))
         (this-day-file (weblog-day-file month day year)))
    (when (equal first-day-file-this-month this-day-file)
      (let ((last-file-prev-month
             (weblog-last-day-file-in-previous-month month year)))
        (when last-file-prev-month
          (let ((month-file (weblog-month-index last-file-prev-month)))
            (weblog-while-visiting-weblog-file buf month-file
               (weblog-upload))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mode supprt
;;;
;;; weblog-mode inherits from html mode.
;;; Any text file in the *weblog-directory* is opened in weblog-mode
;;;

(define-derived-mode weblog-mode html-mode "Weblog"
  nil
  (auto-fill-mode)
  (make-local-variable 'sgml-indent-step)
  (set-variable 'sgml-indent-step nil))

(defun weblog-insert-shortcut ()
  "Insert the shortcut delimiters: \"{@}\""
  (interactive)
  (insert "{@}")
  (backward-char))

(defun weblog-insert-source-key ()
  "Insert a source key: \"[{@}]\""
  (interactive)
  (insert "[{@}]")
  (backward-char 2))

(defun weblog-insert-comment ()
  "Insert an HTML comment: \"<!---->\""
  (interactive)
  (insert "<!---->")
  (backward-char 3))

(defun weblog-save-both ()
  "Save the current buffer and create an html page"
  (interactive)
  (prog1
      (when (buffer-modified-p)
        (weblog-process-charmap)
        (save-buffer))
    (weblog-save)))

(defun weblog-process-charmap ()
  (save-excursion
    (dolist (pair *weblog-char-map*)
      (goto-char 0)
      (let ((from (car pair))
            (to (cadr pair)))
        (loop
         (unless (search-forward from nil t) (return))
         (replace-match to t t))))))

(defun weblog-set-buffer-mode ()
  "Set the mode to weblog-mode for text files in the *weblog-directory*"
  (interactive)
  (let* ((file (buffer-file-name))
         (ext (file-name-extension file))
         (dir (file-name-directory file))
         (weblog-dir nil))
    (when (and (equalp ext "txt")
               (setq weblog-dir (weblog-seek-base-dir dir))
               (let ((len (length weblog-dir)))
                 (and (>= (length dir) len)
                      (equalp weblog-dir
                              (substring dir 0 len)))))
      (weblog-mode))))

(defun weblog-yank-link ()
  "Yank a link tag"
  (interactive)
  (insert "<a href=\"")
  (yank)
  (insert "\">\n</a>")
  (backward-char 4))

(unless (fboundp 'line-beginning-position)
  (defalias 'line-beginning-position 'point-at-bol))

(defun weblog-yank-blockquote ()
  "Yank a blockquote section"
  (interactive)
  (insert "<blockquote>\n")
  (yank)
  (unless (eq (line-beginning-position) (point))
    (insert "\n"))
  (insert "</blockquote>"))

(defun weblog-insert-ellipsis ()
  (interactive)
  (insert "<br>
<br>
...<br>
<br>
"))

(defun weblog-insert-break ()
  (interactive)
  (insert "<br>"))

(defun weblog-italicize-word ()
  (interactive)
  (insert "<i>")
  (forward-word 1)
  (insert "</i>"))

;; Set weblog-file on file open if appropriate
(pushnew 'weblog-set-buffer-mode find-file-hooks)
(define-key weblog-mode-map "\M-]" 'weblog-insert-source-key)
(define-key weblog-mode-map "\M-}" 'weblog-insert-shortcut)
(define-key weblog-mode-map "\M-!" 'weblog-insert-comment)
(define-key weblog-mode-map "\C-x\C-s" 'weblog-save-both)
(define-key weblog-mode-map "\C-x\C-a" 'weblog-add-shortcut)
(define-key weblog-mode-map "\C-x\C-i" 'weblog-upload-index)
(define-key weblog-mode-map "\C-\M-a" 'weblog-yank-link)
(define-key weblog-mode-map "\C-\M-u" 'weblog-yank-blockquote)
(define-key weblog-mode-map "\C-x\M-s" 'weblog-upload)
(define-key weblog-mode-map "\C-x\M-." 'weblog-insert-ellipsis)
(define-key weblog-mode-map "\C-\M-r" 'weblog-insert-break)
(define-key weblog-mode-map "\M-p" 'weblog-insert-break)
(define-key weblog-mode-map "\C-xi" 'weblog-italicize-word)
(define-key weblog-mode-map "\C-\M-l" 'weblog-insert-permalink)
