;; mt.el - Elisp package for posting to an MT blog via XML-RPC
;; $Id: mt.el,v 1.20 2002/09/12 02:30:32 was Exp $

;; Copyright (C) 2002 Bill Stilwell

;; Author: Bill Stilwell <bill@marginalia.org>
;; Version: 0.99
;; Created: August 7 2002
;; 
;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; this software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; This packages enables you to create new posts and edit old posts on
;; your Movable Type Weblog. It may be expanded so that any weblog
;; tool that supports the metaWeblog API can be used.

;;; THANKS & CREDIT:
;; mt.el is based heavily on the blogger.el package by Mark
;; A. Hershberger (http://mah.everybody.org/hacks/emacs/blogger.el.txt)
;; and Jamie Zawinski's LiveJournal package
;; (http://www.jwz.org/hacks/jwz-lj.el). All the good parts here are
;; probably from them; the parts that screw up are mine.


;;; INSTALLATION:
;;
;; To install, put mt.el somewhere in your load path:
;; e.g.: (add-to-list 'load-path "~/elisp")

;; and add these lines to your .emacs or ~/.xemacs/init.el:
;; (require 'mt)
;; (global-set-key "\C-cwc" 'weblog-create-post)
;; (global-set-key "\C-cwr" 'weblog-retrieve-recent-posts)
;; (global-set-key "\C-cwg" 'weblog-retrieve-post)
;;
;; This will load mt.el and bind C-c w c to the post creation command.
;;
;; You will also need to set a few variables, either in your .emacs or
;; via the customization system.

;; With customize:

;; Type M-x customize-group <RET> weblog <RET>. You will need to set:
;;
;; Id - This is the id of your weblog. You can get this from the query
;; string when you go into the movable type main menu for your
;; weblog. You should have something like:
;; 'mt.cgi?__mode=menu&blog_id=1'. In this example, the Id would be 1.
;;
;; Username - username
;;
;; Password - password
;;
;; Url - complete URL to access the mt xmlrpc service. This is likely
;; to be something like:
;; http://www.example.com/PATH/TO/MT/mt-xmlrpc.cgi
;;
;; You can also set Post Count to something other than the
;; default. This value is used when no count is provided to the
;; retrieve recent posts command.
;;
;; If you wish, you can set these values directly in your .emacs:
;; (setq weblog-id "1")
;; (setq weblog-username "My Username")
;; (setq weblog-password "easy")
;; (setq weblog-url "http://www.example.com/mt/mt-xmlrpc.cgi")
;; (setq weblog-post-count 5)
;; (setq weblog-publish-on-save t)
;;
;;; USING mt.el
;; C-c w c -- create a new buffer for a new post

;; C-c w r -- retrieve a set of recent posts. If a prefix argument is
;;            supplied (e.g. C-u 3 C-c w r), that number of posts will
;;            be fetched; otherwise the value of weblog-post-count is
;;            used. A new buffer will open with a list of posts, hit
;;            return on any of the posts to edit that post.

;; C-c w g -- get a particular post. You will be prompted for the post
;;            id.

;; C-c C-c -- save it! Will publish if weblog-publish-on-save is
;;            true.

;; C-c C-s -- save without publish, regardless of
;;            weblog-publish-on-save setting.

;; C-c C-p -- save and publish, regardless weblog-publish-on-save setting.

;; C-c C-e -- in post list, edit post at point

;; C-c C-r -- refresh current post buffer

;;; Requirements:

;; requires xml-rpc.el 
;; http://www.marginalia.org/code/xml-rpc.el
;; and xml.el
;; ftp://ftp.codefactory.se/pub/people/daniel/elisp/xml.el

;; elib is required (http://www.gnu.org/software/elib/elib.html)

;; if you're using emacs, you may need to put (require 'cl) in your
;; .emacs (thanks to Shawn Ledbetter for pointing this out).

;; mt.el uses the metaweblog API (see http://www.xmlrpc.com/metaWeblogApi
;; and movabletype's support for it:
;; http://www.movabletype.org/docs/mtmanual_programmatic.html#xmlrpc%20api.
;;
;; If your weblog does not support these tools, this package will NOT
;; work. To see if your movable type weblog can use XML-RPC, load
;; mt-check.cgi. If you see the following lines, you should be ok:
;;    LWP::UserAgent...
;;        Your server has LWP::UserAgent installed.

;;    SOAP::Lite...
;;        Your server has SOAP::Lite installed.
;; 
;; If not, you will have to install them; see MT's installation manual
;; for instructions.
;;
;; There are currently issues with line breaks. If you set up MT to
;; automatically convert linebreaks and you have auto-fill on in your
;; entry buffer (or fill before submitting), you will end up breaking
;; entries where auto fill wraps tags in the middle. My current
;; recommendation is to turn this off in your MT preferences and have
;; mt.el wrap your paragraphs in <p> tags. However, if you really
;; don't like this, turn off weblog-wrap-paras in customize



;; TODO 

;; BUGS

;; Paragraph wrapping breaks on the last paragraph if there isn't a
;; newline at the end. Current hack fix is to add newline at the end.

;;; CODE

(require 'xml-rpc)

(defgroup weblog nil
  "Emacs interface to the metaWeblogAPI."
  :group 'emacs)

(defcustom weblog-id nil
  "Id for your weblog."
  :group 'weblog
  :type 'string)

(defcustom weblog-username nil
  "Username for this weblog."
  :group 'weblog
  :type 'string)

(defcustom weblog-password nil
  "Password for your weblog."
  :group 'weblog
  :type 'string)

(defcustom weblog-url nil
  "Complete url to your XMLRPC service."
  :group 'weblog
  :type 'string)

(defcustom weblog-post-count 5
  "Default number of posts to retrieve if no count supplied."
  :group 'weblog
  :type 'integer)

(defcustom weblog-publish-on-save t
  "If t, publish on save."
  :group 'weblog
  :type 'boolean)

(defcustom weblog-wrap-paras t
  "If t, mt.el will wrap paragraphs in <p> tags, and you should turn this
  functionality off in MT. If nil, no wrapping will be done, but you
  should turn off auto-fill in your post buffers."
  :group 'weblog
  :type 'boolean)

(defcustom weblog-local-save-dir "~/.mt"
  "Used to cache weblog name and local copies of posts"
  :group 'weblog
  :type 'directory)

(defcustom weblog-auto-categories nil
  "If t, mt will automatically prompt you to set categories on new posts."
  :group 'weblog
  :type 'boolean)


(defvar mt-mode-map
  (let ((map (make-sparse-keymap)))
    (if (functionp 'set-keymap-name)
	(set-keymap-name map 'mt-mode-map))
    (define-key map "\C-c\C-c" 'weblog-submit-post)
    (define-key map "\C-c\C-s" 'weblog-submit-publish)
    (define-key map "\C-c\C-p" 'weblog-submit-no-publish)
    (define-key map "\C-c\C-e" 'weblog-edit-post-at-point)
    (define-key map "\C-c\C-r" 'weblog-refresh-buffer)
    (define-key map "\C-c\C-d" 'mt-cat-create-buffer)
    map))

(defvar mt-cat-mode-map
  (let ((map (make-sparse-keymap)))
    (if (functionp 'set-keymap-name)
	(set-keymap-name map 'mt-cat-mode-map))
    (suppress-keymap map)
    (define-key map "p" 'mt-cat-toggle-primary)
    (define-key map "s" 'mt-cat-toggle-secondary)
    (define-key map "\C-c\C-c" 'mt-cat-update)
    map))

(defvar weblog-mode-hook nil
  "Hook run after starting up weblog mode.")

(defvar weblog-new-post-hook nil
  "Hook to run when visiting a new post buffer")

(defun weblog-mode ()
  "Major mode for editing posts for a weblog."
  (interactive)
  (text-mode)
  (use-local-map mt-mode-map)
  (setq mode-name "weblog")
  (setq major-mode 'weblog-mode)
  ;; what would be nice for hooks is to have it prompt for
  ;; title/category or not depending on user preference
  (run-hooks 'weblog-mode-hook))

(defun weblog-cat-mode ()
  "Major mode for editing categories on a post."
  (interactive)
  (text-mode)
  (use-local-map mt-cat-mode-map)
  (setq mode-name "weblog-cat")
  (setq major-mode 'weblog-cat-mode)
  (setq buffer-read-only t))

(defun weblog-create-post ()
  "*Compose a weblog post."
  (interactive)
  (switch-to-buffer (generate-new-buffer (get-post-buffer-name)))
  (erase-buffer)
  (goto-char (point-min))
  (insert "Title: \n"
	  "\n")
  (goto-char (point-min))
  (end-of-line)
  (weblog-mode)
  (run-hooks 'weblog-new-post-hook))

(defun weblog-submit-post (&optional publish)
  "*Publish current message."
  (interactive)
  (setq new-post (new-postp))
  (weblog-refresh-buffer
  (if new-post
      (mwa-new-post publish)
    (mwa-edit-post publish)))
  (if new-post
      (if (y-or-n-p "Set categories on this post? ")
	  (mt-cat-create-buffer))))

(defun weblog-retrieve-post (&optional post-id)
  "*Retrieves a post for a given weblog post id."
  (interactive)
  (create-post-buffer 
   (mwa-get-post (or post-id (read-from-minibuffer "Post Id: ")))))

(defun weblog-retrieve-recent-posts (&optional num)
  "*Retrieve a list of recent posts, puts in a buffer for further editing."
  (interactive "P")
  (create-posts-buffer (mwa-get-recent-posts (or num weblog-post-count))))

(defun weblog-edit-post-at-point ()
  "Edit the post at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "\\(^[0-9]+\\)[ \t]+.*$" nil t)
	(create-post-buffer (mwa-get-post (match-string 1)))
      (error "Couldn't determine post number"))))

(defun weblog-submit-publish ()
  "Save and publish post regardless of weblog-publish-on-save setting"
  (interactive)
  (weblog-submit-post '1))

(defun weblog-submit-no-publish ()
  "Save without publish regardless of weblog-publish-on-save setting"
  (interactive)
  (weblog-submit-post '0))

(defun weblog-switch-weblog (&optional id)
  "Convenient way to switch what weblog is posted to"
  (interactive)
  (setq weblog-id (or id (read-from-minibuffer "Blog Id: ")))
  (setq weblog-name (mt-get-weblog-name weblog-id)))

(defun create-posts-buffer (posts)
  "Creates a listing of retrieved posts"
  ; Needs to add a key binding for return to edit a post maybe@
  (switch-to-buffer (generate-new-buffer "*weblog-posts*"))
  (insert "Recent posts:\n\n")
  (mapcar 
   (lambda (post)
     (setq post-id (cdr (assoc "postid" post)))
     (setq post-title (cdr (assoc "title" post)))
     (setq post-body (cdr (assoc "description" post)))
     (insert post-id)
     (insert " ")
     (insert post-title)
     (insert "\n"))
   posts)
  (weblog-mode)
  (goto-char (point-min)))
	  
(defun create-post-buffer (post)
  "Creates buffer filled with post info"
  ;; so we should have a struct in post
  (setq post-id (cdr (assoc "postid" post)))
  (setq post-title (cdr (assoc "title" post)))
  (setq post-body (cdr (assoc "description" post)))
  (switch-to-buffer (generate-new-buffer (get-post-buffer-name post-id)))
  (erase-buffer)
  (goto-char (point-min))
  (insert "Post Id: ")
  (insert post-id)
  (insert " [erase this line to create a new post]\n")
  (insert "Title: ")
  (insert post-title)
  (insert "\n\n")
  (insert (mt-clean-body post-body))
  (insert "\n")
  (goto-char (point-max))
  (end-of-line)
  (weblog-mode))

(defun get-post-buffer-name (&optional post-id)
  (setq log-name (mt-get-weblog-name weblog-id))
  (if post-id
      (concat "*weblog post " post-id " (" log-name ")*") ; need to check if numberp?
    (concat "*weblog-post (" log-name ")*"))) ; stupid

;; metaweblogapi implementations
(defun mwa-new-post (&optional publish)
  "Makes a new post via metaWeblogAPI newPost command"
  (xml-rpc-method-call 
   weblog-url 
   'metaWeblog.newPost 
   weblog-id 
   weblog-username
   weblog-password
   (list (cons "description" (weblog-post-body))
	 (cons "title" (weblog-post-title)))
   (or publish (publishp))))

(defun mwa-edit-post (&optional publish)
  "Edits a post via metaWeblogAPI editPost command"
  (xml-rpc-method-call
   weblog-url
   'metaWeblog.editPost
   (weblog-post-id)
   weblog-username
   weblog-password
   (list (cons "description" (weblog-post-body))
	 (cons "title" (weblog-post-title)))
   (or publish (publishp))))

(defun mwa-get-post (post-id)
  "Gets a single post via metaWeblogAPI getPost command"
  (xml-rpc-method-call
   weblog-url
   'metaWeblog.getPost
   post-id
   weblog-username
   weblog-password))

(defun mwa-get-recent-posts (number-of-posts)
  "Gets specified number of recent posts via metaWeblogAPI getRecentPosts command"
  (xml-rpc-method-call
   weblog-url
   'metaWeblog.getRecentPosts
   weblog-id
   weblog-username
   weblog-password
   number-of-posts))

;; MovableType XMLRPC implementation - category stuff

(defun mt-get-category-list ()
  "Gets list of categories for a weblog via mt API getCategoryList command"
  (xml-rpc-method-call
   weblog-url
   'mt.getCategoryList
   weblog-id
   weblog-username
   weblog-password))

(defun mt-get-post-categories (&optional post-id)
  "Gets categories for a post via mt API getPostCategories command"
  (xml-rpc-method-call
   weblog-url
   'mt.getPostCategories
   (or post-id (weblog-post-id))
   weblog-username
   weblog-password))

(defun mt-set-post-categories (post-id post-categories)
  "Sets categories for a post via mt API setPostCategories command"
  (xml-rpc-method-call
   weblog-url
   'mt.setPostCategories
   post-id
   weblog-username
   weblog-password
   post-categories))

(defun mt-get-trackback-pings ()
  "Retrieves trackback pings for an entry, if any."
  (xml-rpc-method-call
   weblog-url
   'mt.getTrackbackPings
   (weblog-post-id)))

;; blogger api implementation
(defun blogger-get-users-blogs ()
  "Retrieves info about current users blogs"
  (xml-rpc-method-call
   weblog-url
   'blogger.getUsersBlogs
   '""
   weblog-username
   weblog-password))

;; support utility functions
(defun weblog-post-title ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (search-forward "\n\n")
      (narrow-to-region (point-min) (point))
      
      (goto-char (point-min))
      (if (re-search-forward "^Title:[ \t]*\\(.*\\)$" nil t)
	  (setq post-title (match-string 1))
	(read-from-minibuffer "Post Title: " post-title))
      (xml-rpc-encode post-title))))

(defun weblog-post-body ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (search-forward "\n\n")
      (if weblog-wrap-paras
	  (xml-rpc-encode (wrap-paragraphs (buffer-substring (point) (point-max))))
	(xml-rpc-encode (buffer-substring (point) (point-max)))))))

(defun xml-rpc-encode (string)
  "Replaces < with &lt; and & with &amp;"
  (let (buf)
    (save-excursion
      (unwind-protect
	  (progn
	    (setq buf (get-buffer-create " *xml-rpc-escape*"))
	    (set-buffer buf)
	    (erase-buffer)
	    (insert-string string)
	    
	    ;; This whole section is a massive massive hack
	    (goto-char (point-min))
	    (while (re-search-forward "&" nil t)
	      (replace-match "&amp;" nil nil)) ;; can't have bare &
	    (goto-char (point-min))
	    (while (re-search-forward "<" nil t)
	      (replace-match "&lt;" nil nil))  ;; can't have bare <
	    (buffer-string))
	(if buf (kill-buffer buf))))))

(defun wrap-paragraphs (string)
    (save-excursion
      (unwind-protect
	  (progn
	    (with-temp-buffer
	      (insert-string string)
	      ;; spot the hack
	      (goto-char (point-max))
	      (while (mt-looking-back-at "\n")
		(delete-backward-char 1))
	      (insert "\n")
	      (goto-char (point-min))
	      (while (not (eq (point) (point-max)))
		(start-of-paragraph-text)
		(if (not (looking-at "<p>"))  
		    (insert "<p>")) 
		(end-of-paragraph-text) 
		(if (not (mt-looking-back-at "</p>"))  
		    (insert "</p>"))
		(end-of-paragraph-text))
	      (buffer-string))))))
  

(defun weblog-post-id ()
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (search-forward "\n\n")
      (narrow-to-region (point-min) (point))
      
      (goto-char (point-min))
      (if (re-search-forward "^Post Id:[ \t]*\\([0-9]*\\).*$" nil t)
	  (setq post-id (match-string 1))
	(setq post-id nil)))))

(defun weblog-insert-post-contents (post)
  (setq post-id (cdr (assoc "postid" post)))
  (setq post-title (cdr (assoc "title" post)))
  (setq post-body (cdr (assoc "description" post)))
  (insert "Post Id: ")
  (insert post-id)
  (insert " [erase this line to create a new post]\n")
  (insert "Title: ")
  (insert post-title)
  (insert "\n\n")
  (insert post-body)
  (insert "\n"))
  
(defun weblog-refresh-buffer (&optional post-id)
  "Refreshes content of current buffer with whatever mt server has."
  (interactive)
  (setq pm (point-max))
  (goto-char (point-max))
  (if (stringp post-id)
      (setq id post-id)
    (setq id (weblog-post-id)))
  (weblog-insert-post-contents
   (mwa-get-post id))
  (delete-region (point-min) pm))

(defun new-postp ()
  "Return t if buffer is a new post."
  (not (weblog-post-id)))

(defun publishp ()
  "Return 1 if publish, 0 otherwise."
  (if weblog-publish-on-save
      (setq publish '1)
    (setq publish '0)))

(defun mt-looking-back-at (regexp &optional bound)
  "Return t if text before point matches REGEXP.
Modifies the match data. If supplied, BOUND means not to look farther
back that that many characters before point. Otherwise, it defaults to
\(length REGEXP), which is good enough when REGEXP is a simple
string."
  ;; taken from mmm-mode - mmm-utils.el
  (eq (point)
      (save-excursion
        (and (re-search-backward regexp
               (- (point) (or bound (length regexp)))
               t)
             (match-end 0)))))

(defun mt-clean-body (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "&#xd;" nil t)
      (replace-match "" nil nil))
    (buffer-string)))

(defun mt-get-weblog-name (&optional log-id)
  "retrieves name of a weblog given an id, either from local file or net"
  (if (mt-weblog-file-exists-p (or log-id weblog-id))
      (mt-read-weblog-name (or log-id weblog-id))
    (mt-store-weblog-name (mt-fetch-weblog-name (or log-id weblog-id)))))

(defun mt-weblog-file-exists-p (log-id)
  "Tests to see if the weblog name file exists"
  (setq weblog-file-name (concat weblog-local-save-dir "/" "weblog." log-id))
  (file-exists-p weblog-file-name))

(defun mt-read-weblog-name (log-id)
  "Reads the weblog name from a file"
  (with-temp-buffer
    (insert-file-contents (concat weblog-local-save-dir "/" "weblog." log-id))
    (goto-char (point-min))
    (while (re-search-forward "\n" nil t)
      (replace-match "" nil nil))
    (buffer-string)))

(defun mt-store-weblog-name (weblog-name)
  "Stores the weblog name in a file"
  (setq weblog-file-name (concat weblog-local-save-dir "/" "weblog." log-id))
  (if (not (file-directory-p weblog-local-save-dir))
      (make-directory weblog-local-save-dir))
  (with-temp-buffer
    (insert weblog-name)
    (insert "\n")
    (append-to-file (point-min) (point-max) weblog-file-name))
  weblog-name)

(defun mt-fetch-weblog-name (log-id)
  "Fetches weblog name from server using XML-RPC"
  (setq weblog-name nil)
  (mapc
   (lambda (log-info)
     (if (string= (or log-id weblog-id) (cdr (assoc "blogid" log-info)))
	 (setq weblog-name (cdr (assoc "blogName" log-info)))))
   (blogger-get-users-blogs))
  weblog-name)


;; category handling stuff

;; Here's the basics of what I'd like to do:
;; - have a buffer created (probably split the window) that gives the
;;   categories available
;; - allow user to mark primary and secondary categories. Only one
;;   primary category is allowed, as many secondary as necessary.
;; - an update command that sets the categories on a post.
(require 'cookie)

(defvar mt-cat-col nil)

(defvar mt-cat-buf "*Category Selection*")

(defun mt-cat-create-buffer (&optional post-id)
  "Creates a buffer filled with categories."
  (interactive)
  (setq post-id (or post-id (or (weblog-get-post-id-at-point) (weblog-post-id))))
  (split-window)
  (save-excursion
    (set-buffer (get-buffer-create mt-cat-buf))
    (setq buffer-read-only nil)
    (erase-buffer))
  (setq mt-cat-col (mt-cat-create-cookie-collection post-id))
  (mt-cat-enter-cookies post-id (mt-get-post-categories post-id))
  (other-window 1)
  (switch-to-buffer mt-cat-buf)
  
  (weblog-cat-mode))

(defun weblog-get-post-id-at-point ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "\\(^[0-9]+\\)[ \t]+.*$" nil t)
	(match-string 1))))


(defun mt-cat-create-cookie-collection (post-id)
  (collection-create
   mt-cat-buf
  (function mt-cat-pp)
  (concat "Post " post-id " category selection\n") 
  "\n+ Primary category\n- Secondary category" 0))

(defun mt-cat-enter-cookies (post-id post-categories)
  (collection-append-cookies 
   mt-cat-col
   (mapcar 
    (lambda (category)
      (setq cat-id 
	    (cdr (assoc "categoryId" category)))
      (setq cat-primary 
	    (mt-post-primary post-categories))
      (setq cat-secondary 
	    (mt-post-secondary post-categories))
      (list (cons "cat-id" cat-id)
	    (cons "cat-name" 
		  (cdr (assoc "categoryName" category)))
	    (cons "primary" 
		  (is-primary-p cat-id cat-primary))
	    (cons "secondary" 
		  (is-secondary-p cat-id cat-secondary))))
    (mt-get-category-list))))

(defun is-primary-p (cat-id cat-primary)
  (if (string= cat-id cat-primary)
      t
    nil))

(defun is-secondary-p (cat-id cat-secondary)
  (if (member cat-id cat-secondary)
      t
    nil))

(defun mt-post-primary (post-categories)
  "Returns the category id of the primary category for a post"
  ;; this is a bit hackish and inefficient, as we'll continue looping
  ;; over the list of categories even after we've got our primary
  (let (primary)
    (mapc
     (lambda (category)
       (if (cdr (assoc "isPrimary" category))
	   (setq primary (cdr (assoc "categoryId" category)))))
     post-categories)
    primary))
       
(defun mt-post-secondary (post-categories)
  "Returns a list of the category id(s) of the secondary categories
  for a post"
  (interactive)
  (mapcar
   (lambda (category)
     (if (not (cdr (assoc "isPrimary" category)))
	 (cdr (assoc "categoryId" category))
       nil))
   post-categories))

(defun mt-cat-get-tin (pos)
  (interactive "d")
  (mt-cat-mark-primary cookie))

(defun mt-cat-toggle-primary (pos)
  "toggles primary setting of this category"
  (interactive "d")
  (let* ((tin (tin-locate mt-cat-col pos))
	 (cookie (tin-cookie mt-cat-col tin)))
  (cookie-map
   (function (lambda (cat)
	       ;; if this is our primary cat, toggle it
	       ;; othewise, mark the primary field nil
	       ;; this should enforce only one primary cat at a time,
	       ;; also sets secondary to nil
	       (if (string= (cdr (assoc "cat-id" cookie)) (cdr (assoc "cat-id" cat)))
		   (progn
		     (setcdr (assoc "primary" cat) (mt-cat-toggle (cdr (assoc "primary" cat))))
		     (setcdr (assoc "secondary" cat) nil))
		 (setcdr (assoc "primary" cat) nil))
	       t))
   mt-cat-col)))

(defun mt-cat-toggle-secondary (pos)
  (interactive "d")
  (let* ((tin (tin-locate mt-cat-col pos))
	(cookie (tin-cookie mt-cat-col tin)))

  (setcdr (assoc "secondary" cookie) (mt-cat-toggle (cdr (assoc "secondary" cookie))))
  (setcdr (assoc "primary" cookie) nil)
  (collection-refresh mt-cat-col)
  (goto-char pos))) ; lazy, but why test?


(defun mt-cat-toggle (bool)
  (if bool
      nil
    t))

(defun mt-cat-update ()
  "Sets the category information on the post on the server"
  (interactive)
  (mt-set-post-categories (mt-cat-post-id) (mt-cats-as-array))
  (bury-buffer)
  (delete-window))

(defun mt-cat-post-id ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Post \\([0-9]*\\).*" nil t)
	(match-string 1))))

(defun mt-cats-as-array ()
  (interactive)
  (mt-cat-get-struct (append (get-primary-cat) (get-secondary-cats))))

(defun mt-cat-get-struct (cats)
  (mapcar 
   (lambda (cat)
     (list (cons "categoryId" (cdr (assoc "cat-id" cat)))))
   cats))
  
(defun get-primary-cat ()
  (interactive)
  (collection-collect-cookie mt-cat-col (function is-marked-primary-p)))

(defun get-secondary-cats ()
  (interactive)
  (collection-collect-cookie mt-cat-col (function is-marked-secondary-p)))

(defun is-marked-primary-p (cat)
  (if (cdr (assoc "primary" cat))
      t
    nil))

(defun is-marked-secondary-p (cat)
  (if (cdr (assoc "secondary" cat))
      t
    nil))
  
(defun mt-cat-pp (mt-cat-info)
  "pretty printer for cookie collection"
  (insert "[")
  (cond 
   ((cat-info-primary-p mt-cat-info)
    (insert "+"))
   ((cat-info-secondary-p mt-cat-info)
    (insert "-"))
   (t (insert " ")))
  (insert (concat (cdr (assoc "cat-id" mt-cat-info)) " ]"))
  (insert " ")
  (insert (cdr (assoc "cat-name" mt-cat-info))))

(defun cat-info-primary-p (mt-cat-info)
  (if (cdr (assoc "primary" mt-cat-info))
      t
    nil))

(defun cat-info-secondary-p (mt-cat-info)
  (if (cdr (assoc "secondary" mt-cat-info))
      t
    nil))

(provide 'mt)

;;; mt.el ends here
