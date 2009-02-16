;;; mew-func.el --- Basic functions for Mew

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Mar 23, 1997

;;; Code:

(require 'mew)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Mode
;;;

(defsubst mew-summary-p ()
  (eq major-mode 'mew-summary-mode))

(defsubst mew-virtual-p ()
  (eq major-mode 'mew-virtual-mode))

(defsubst mew-message-p ()
  (eq major-mode 'mew-message-mode))

(defsubst mew-draft-p ()
  (eq major-mode 'mew-draft-mode))

(defsubst mew-header-p ()
  (eq major-mode 'mew-header-mode))

(defsubst mew-draft-or-header-p ()
  (memq major-mode '(mew-draft-mode mew-header-mode)))

(defsubst mew-thread-p ()
  (mew-vinfo-get-parent-folder))

(defsubst mew-summary-or-virtual-p ()
  (or (mew-summary-p) (mew-virtual-p)))

(defsubst mew-virtual-for-one-summary ()
  (and (mew-virtual-p) (= (length (mew-vinfo-get-flds)) 1)))

(defsubst mew-pickable-p ()
  (or (mew-summary-p)
      (mew-thread-p)
      (mew-virtual-for-one-summary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; List functions
;;;

(defsubst mew-case-equal (str1 str2)
  (string= (downcase str1) (downcase str2)))

(defun mew-folder-recursive-match (key folder)
  "Initial substring match for folders.
If FOLDER is a sub-folder of KEY or KEY itself, t is returned."
  ;; file-name-as-directory should be first
  ;; because the path separator may be regex-non-safe.
  (let ((regex (if (string-match "^[-+%$]$" key)
		   (mew-folder-regex key)
		 (mew-folder-regex (file-name-as-directory key)))))
    (string-match regex (file-name-as-directory folder))))

(defun mew-member-case-equal (str list)
  "Return the position equal to STR in LIST. Case is ignored."
  (let ((n 0))
    (catch 'member
      (while list
	(if (string= (downcase (car list)) (downcase str))
	    (throw 'member n))
	(setq list (cdr list))
	(setq n (1+ n))))))

(defun mew-member* (x list)
  "Member in a nested list."
  (catch 'found
    (while list
      (if (consp (car list))
	  (if (mew-member* x (car list))
	      (throw 'found t))
	(if (equal x (car list))
	    (throw 'found t)))
      (setq list (cdr list)))))

(defun mew-member-match (str list &optional ignore-case)
  "Return the position matched to STR in LIST. If
IGNORE-CASE is t, matching is performed by ignoring case."
  (let ((n 0) (case-fold-search ignore-case))
    (catch 'member
      (while list
	(if (string-match (car list) str)
	    (throw 'member n))
	(setq list (cdr list))
	(setq n (1+ n))))))

(defun mew-member-match2 (regex list &optional ignore-case)
  (let ((case-fold-search ignore-case))
    (catch 'member
      (while list
	(if (string-match regex (car list))
	    (throw 'member (car list)))
	(setq list (cdr list))))))

(defun mew-uniq-list (lst)
  "Destructively uniqfy elements of LST.
This is O(N^2). So, do not use this function with a large LST."
  (let ((tmp lst))
    (while tmp (setq tmp (setcdr tmp (delete (car tmp) (cdr tmp))))))
  lst)

(defun mew-uniq-alist (alst)
  "Destructively uniqfy elements of ALST."
  (let ((vec (make-vector 511 0)) ;; hash
	ent str ret)
    (while alst
      (setq ent (car alst))
      (setq str (car ent))
      (setq alst (cdr alst))
      (cond
       ((not (stringp str))
	(setq ret (cons ent ret)))
       ((intern-soft str vec)
	())
       (t
	(setq ret (cons ent ret))
	(intern str vec))))
    (nreverse ret)))

(defun mew-delete (key alist)
  "Destructively delete elements whose first member is equal to key"
  (if (null key)
      alist
    (let (ret)
      (while (and (listp (nth 0 alist)) (equal (car (nth 0 alist)) key))
	(setq alist (cdr alist)))
      (setq ret alist)
      (while alist
	(if (and (listp (nth 1 alist)) (equal (car (nth 1 alist)) key))
	    (setcdr alist (cdr (cdr alist)))
	  (setq alist (cdr alist))))
      ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Associative list functions
;;;

(defun mew-assoc-equal (key alist nth)
  (let (a n)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(setq n (nth nth a))
	(if (or (equal n key) (eq n t))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun mew-assoc-case-equal (key alist nth)
  (let ((skey (downcase key)) a n)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(setq n (nth nth a))
	(if (or (and (stringp n) (string= (downcase n) skey))
		(eq n t))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun mew-assoc-match (key alist nth)
  "Return list in ALIST that KEY regex is matched to its NTH element.
Case is ignored. Note that the NTH element is 't', 
the list is always selected."
  (let ((case-fold-search t) a n)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(setq n (nth nth a))
	(if (or (and (stringp n) (string-match key n))
		(equal n key) (eq n t))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun mew-assoc-match2 (key alist nth)
  "Return list in ALIST whose NTH regex is matched to KEY.
Case is ignored. Note that the NTH element is 't', 
the list is always selected."
  (let ((case-fold-search t) a n)
    (catch 'loop
      (while alist
	(setq a (car alist))
	(setq n (nth nth a))
	(if (or (and (stringp n) (string-match n key))
		(equal n key) (eq n t))
	    (throw 'loop a))
	(setq alist (cdr alist))))))

(defun mew-assoc-match3 (key alist nth)
  "Return list in ALIST whose NTH regex is matched to KEY.
Case is ignored. Note that the NTH element is 't', 
the list is always selected. The deference from mew-assoc-match2
is that this returns the position of a selected list in addition
to the list itself."
  (let ((case-fold-search t) (i 0) a n )
    (catch 'loop
      (while alist
	(setq a (car alist))
	(setq n (nth nth a))
	(if (or (and (stringp n) (string-match n key))
		(equal n key) (eq n t))
	    (throw 'loop (cons i a)))
	(setq i (1+ i))
	(setq alist (cdr alist))))))

(defsubst mew-assoc-member (key lol nth)
  "Return a list member of LoL whose NTH list contains 
a member equal to KEY."
  (mew-assoc-member-base key lol nth 'member))

(defsubst mew-assoc-member-case-equal (key lol nth)
  "Return a list member of LoL whose NTH list contains 
a member equal to KEY ignoring case."
  (mew-assoc-member-base key lol nth 'mew-member-case-equal))

(defun mew-assoc-member-base (key lol nth func)
  "Return a list member of LoL whose NTH list contains KEY
in the context of FUNC."
  (let (l)
    (catch 'loop
      (while lol
	(setq l (car lol))
	(setq lol (cdr lol))
	(if (and (listp (nth nth l)) (funcall func key (nth nth l)))
	    (throw 'loop l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; getting next
;;;

(defun mew-get-next (LIST MEM)
  "Return of a member in LIST which is the next member of MEM."
  (let (frst next crnt)
    (setq frst (car LIST))
    (setq LIST (cdr LIST))
    (setq next (car LIST))
    (if (equal frst MEM)
	(if next next frst)
    (catch 'loop
      (while LIST
	(setq crnt next)
	(setq LIST (cdr LIST))
	(setq next (car LIST))
	(if (equal crnt MEM)
	    (throw 'loop (if next next frst))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Modifying list
;;;

(defmacro mew-add-first (variable value)
  `(setq ,variable (cons ,value ,variable)))

(defmacro mew-addq (variable value)
  `(if (and ,value (not (member ,value ,variable)))
       (setq ,variable (cons ,value ,variable))))

(defmacro mew-insert-after (variable value key)
  `(let ((var ,variable))
     (catch 'loop
       (while var
	 (if (equal (nth 0 (car var)) ,key)
	     (throw 'loop (setcdr var (cons ,value (cdr var)))))
	 (setq var (cdr var))))))

(defmacro mew-replace-with (variable value key)
  `(let ((var ,variable))
     (catch 'loop
       (while var
	 (if (equal (nth 0 (car var)) ,key)
	     (throw 'loop (setcar var ,value)))
	 (setq var (cdr var))))))

(defmacro mew-remove-entry (variable key)
  `(let ((crn ,variable) prv)
     (if (equal (nth 0 (car crn)) ,key)
	 (setq ,variable (cdr crn))
       (setq prv crn)
       (setq crn (cdr crn))
       (catch 'loop
	 (while crn
	   (if (equal (nth 0 (car crn)) ,key)
	       (throw 'loop (setcdr prv (cdr crn))))
	   (setq prv crn)
	   (setq crn (cdr crn)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; String
;;;

(defun mew-replace-character (string from to)
  "Replace characters equal to FROM to TO in STRING."
  (let ((len (length string))
	(cnt 0))
    (while (< cnt len)
      (if (char-equal (aref string cnt) from)
	  (aset string cnt to))
      (setq cnt (1+ cnt)))
    string))

(defun mew-replace-white-space (string)
  "Replace white characters to a space."
  (while (string-match "[\n\t]+" string)
    (setq string (replace-match " " nil t string)))
  (while (string-match "  +" string)
    (setq string (replace-match " " nil t string)))
  string)

(defun mew-replace-white-space2 (string)
  "Replace white characters to under score."
  (while (string-match "[\n\t\r ]+" string)
    (setq string (replace-match "_" nil t string)))
  string)

(defsubst mew-capitalize (ostr)
  "Syntax table independent version of capitalize.
Words are separated by '/' and '-'."
  (let* ((len (length ostr))
	 (nstr (make-string len ?a))
	 (i 0) (topp t) c)
    (while (< i len)
      (setq c (aref ostr i))
      (cond
       (topp
	(aset nstr i (upcase c))
	(setq topp nil))
       ((or (char-equal c ?/) (char-equal c ?-))
	(aset nstr i c)
	(setq topp t))
       (t
	(aset nstr i (downcase c))))
      (setq i (1+ i)))
    nstr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Insertion
;;;

(defsubst mew-insert (form str)
  (when str
    (if form
	(insert (format form str))
      (insert str))))

(defsubst mew-insert-message (fld msg rcs size)
  (let ((file (mew-expand-folder fld msg)))
    (cond
     ((not (file-readable-p file))
      (error "%s does not exist" (mew-concat-folder fld msg)))
     ((= (mew-file-get-size file) 0)
      (error "The size of %s is 0" (mew-concat-folder fld msg)))
     ((file-readable-p file)
      (let ((old-cs (and (boundp 'buffer-file-coding-system)
			 buffer-file-coding-system)))
	(mew-frwlet
	 rcs mew-cs-dummy
	 (mew-insert-file-contents file nil 0 size))
	(if (boundp 'buffer-file-coding-system)
	    (setq buffer-file-coding-system old-cs)))
      ;; return physical size
      (cons (mew-file-get-time file) (mew-file-get-size file))))))

(defsubst mew-insert-manual (&rest args)
  (insert (substitute-command-keys (apply 'concat args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Stolen form Perl
;;;

(defsubst mew-join (separator string-list)
  (mapconcat 'identity string-list separator))

(defun mew-split (str sepchar)
  "Return a list of strings splitting STR with SEPCHAR."
  (let ((len (length str)) (start 0) (i 0) ret)
    (while (< i len)
      (when (char-equal (aref str i) sepchar)
	(setq ret (cons (substring str start i) ret))
	(setq start (1+ i)))
      (setq i (1+ i)))
    (if (/= start len)
	(setq ret (cons (substring str start) ret)))
    (nreverse ret)))

(defun mew-split-quoted (str sepchar &optional qopen qclose)
  "Return a list of strings splitting STR with SEPCHAR.
SEPCHARs in double-quoted strings are ignored.
If QUOTEDCHAR is provided, SEPCHARs between QOPEN and QCLOSE are
also ignored."
 (let ((qlevel 0) (len (length str)) (start 0) (i 0) dblq ret c)
   (if (and qopen (not qclose)) (setq qclose qopen))
   (while (< i len)
     (setq c (aref str i))
     (cond 
      ((char-equal ?\\ c)
       (setq i (1+ i)))
      ((char-equal ?\" c)
       (setq dblq (not dblq)))
      ((and qopen (char-equal c qopen))
       (setq qlevel (1+ qlevel)))
      ((and qclose (char-equal c qclose))
       (setq qlevel (1- qlevel)))
      ((char-equal c sepchar)
       (if (or dblq (>= qlevel 1))
	   ()
	 (setq ret (cons (substring str start i) ret))
	 (setq start (1+ i)))))
     (setq i (1+ i)))
   (if (/= start len)
       (setq ret (cons (substring str start) ret)))
   (nreverse ret)))

(defun mew-chop (str)
  "Split off preceding and following white spaces."
  (let ((i 0) (j (length str)) c)
    (catch 'loop
      (while (< i j)
	(setq c (aref str i))
	(if (or (char-equal c mew-sp) (char-equal c ?\t))
	    (setq i (1+ i))
	  (throw 'loop nil))))
    (setq j (1- j))
    (catch 'loop
      (while (< i j)
	(setq c (aref str j))
	(if (or (char-equal c mew-sp) (char-equal c ?\t))
	    (setq j (1- j))
	  (throw 'loop nil))))
    (substring str i (1+ j))))

(defun mew-quote-string (str qchar targets)
  (let* ((len (length str))
	(ret (make-string (* len 2) mew-sp))
	(i 0) (j 0) c)
    (while (< i len)
      (setq c (aref str i))
      (when (member c targets)
	(aset ret j qchar)
	(setq j (1+ j)))
      (aset ret j c)
      (setq i (1+ i) j (1+ j)))
    (substring ret 0 j)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder
;;;

(defsubst mew-folder-regex (folder)
  (concat "^" (regexp-quote folder)))

;;

(defun mew-case:folder-case (case:folder)
  "Extract case from case:folder. If case does not exist, nil is returned."
  ;; "case" must be distinguished from drive
  (if (string-match mew-regex-case1 case:folder)
      (mew-match-string 1 case:folder)))

(defun mew-case:folder-folder (case:folder)
  "Extract folder from case:folder."
  ;; "case" must be distinguished from drive
  (if (string-match mew-regex-case1 case:folder)
      (mew-match-string 3 case:folder)
    case:folder))

(defun mew-case-folder (case folder)
  "Concat case and folder to produce case:folder.
If case is \"default\", it is not prepended."
  (if (and case
	   (not (string= case mew-case-default))
	   (mew-folder-remotep folder))
      (concat case ":" folder)
    folder))

;;

(defsubst mew-folder-absolutep (folder)
  (or (string-match mew-regex-file-absolute folder)
      (and (featurep 'mew-win32)
	   (string-match mew-regex-drive-letter folder))))

(defsubst mew-folder-localp (folder)
  (string-match "^\\+" folder))

(defsubst mew-folder-remotep (folder)
  (string-match "^[-%$]" folder))

(defsubst mew-folder-popp (folder)
  (string-match "^\\$" folder))

(defsubst mew-folder-nntpp (folder)
  (string-match "^-" folder))

(defsubst mew-folder-imapp (folder)
  (string-match "^%" folder))

(defsubst mew-folder-virtualp (folder)
  (string-match "^\\*" folder))

(defsubst mew-virtual-thread-p (vfolder)
  (save-excursion  
    (set-buffer vfolder)
    (mew-thread-p)))

;;

(defsubst mew-folder-prefix (folder)
  (substring folder 0 1))

(defsubst mew-folder-string (folder)
  (substring folder 1))

(defsubst mew-string-to-local (folder)
  (concat mew-folder-local folder))

;;

(defsubst mew-folder-to-virtual (folder)
  (concat mew-folder-virtual folder mew-folder-virtual))

(defsubst mew-folder-to-thread (folder)
  (concat mew-folder-virtual folder))

(defsubst mew-physical-folder (folder)
  ;; Upper functions may use string-match.
  ;; So, string-match must not be used here.
  (if (eq (aref folder 0) ?*)
      (setq folder (substring folder 1)))
  (if (eq (aref folder (1- (length folder))) ?*)
      (setq folder (substring folder 0 -1)))
  folder)

;;

(defun mew-summary-physical-folder ()
  (cond
   ((mew-thread-p)
    (mew-vinfo-get-parent-folder))
   ((mew-virtual-for-one-summary)
    (car (mew-vinfo-get-flds)))
   (t
    (mew-summary-folder-name 'ext))))

;;

(defsubst mew-local-to-dir (folder)
  (if (string-match "^[+]" folder)
      (substring folder 1)
    folder))

;;

(defsubst mew-folder-inboxp (folder)
  (member folder mew-inbox-folders))

(defsubst mew-folder-queuep (folder)
  (member folder mew-queue-folders))

(defsubst mew-folder-postqp (folder)
  (member folder mew-postq-folders))

(defsubst mew-folder-draftp (folder)
  (equal folder mew-draft-folder))

;;

(defsubst mew-folder-imap-queuep ()
  (string= (mew-sinfo-get-folder)
	   (mew-imap-queue-folder (mew-sinfo-get-case))))

;;

(defun mew-canonicalize-folder (folder)
  (cond
   ((mew-folder-localp folder)
    folder)
   ((mew-folder-imapp folder)
    folder)
   ((file-name-absolute-p folder)
    folder)
   (t
    (mew-string-to-local folder))))

(defun mew-path-to-folder (path)
  (let ((regex (concat "^" (regexp-quote (file-name-as-directory (expand-file-name mew-mail-path))))))
    (if (string-match regex path)
	(mew-string-to-local (substring path (match-end 0)))
      path)))

(defun mew-canonicalize-case-folder (case:folder)
  (let ((case (mew-case:folder-case case:folder))
	(folder (mew-case:folder-folder case:folder))
	len)
    (cond
     ((mew-folder-localp folder)   (directory-file-name folder))
     ((mew-folder-absolutep folder)(directory-file-name folder))
     ((mew-folder-virtualp folder) folder)
     (t
      (if (null case)
	  (setq folder case:folder)
	(if (string-equal case mew-case-default)
	    (progn
	      (setq case nil)
	      (setq case:folder folder))))
      (cond
       ((mew-folder-popp folder) case:folder)
       ((mew-folder-imapp folder)
	(mew-imap-directory-file-name
	 case:folder (or case ;; visit
			 mew-inherit-case))) ;; refile
       ((mew-folder-nntpp folder)
	(setq len (1- (length case:folder)))
	(if (char-equal (aref case:folder len) ?.)
	    (substring case:folder 0 len)
	  case:folder)))))))

;;

(defun mew-expand-folder2 (case:folder)
  "Expanding FOLDER to a relative path to '+'"
  (let ((case (mew-case:folder-case case:folder))
	(folder (mew-case:folder-folder case:folder))
	subdir)
    ;; The length of "case" must be longer than or equal to 2.
    (setq subdir (mew-folder-string folder))
    (cond
     ((mew-folder-popp folder)
      (mew-concat-folder (mew-pop-folder case) subdir))
     ((mew-folder-nntpp folder)
      (mew-concat-folder (mew-nntp-folder case) subdir))
     ((mew-folder-imapp folder)
      (mew-concat-folder (mew-imap-folder case)
			 (mew-imap-expand-folder
			  case
			  (mew-imap-utf-7-encode-string subdir))))
     (t folder))))

(defun mew-expand-folder (folder &optional message)
  "Expanding FOLDER to its absolute path"
  (when (stringp folder)
    (let (dir)
      (setq folder (mew-expand-folder2 folder))
      (if (mew-folder-localp folder)
	  (setq dir (expand-file-name (mew-folder-string folder) mew-mail-path))
	;; absolute path
	;; "C:/" -> t, "C:" -> nil , "CC:/" -> nil
	(setq dir (expand-file-name folder)))
      (if message
	  (expand-file-name message dir)
	dir))))

(defun mew-concat-folder (folder subfolder)
  (concat (file-name-as-directory folder) subfolder))

(defun mew-concat-folder2 (folder subfolder case)
  (if (mew-folder-imapp folder)
      (concat (mew-imap-file-name-as-directory folder case) subfolder)
    (concat (file-name-as-directory folder) subfolder)))

(defsubst mew-dir-messages (dir &optional regex)
  ;; directory_files uses ENCODE_FILE() for DIR and
  ;;                      DECODE_FILE() for results(FILES).
  ;; Both ENCODE_FILE() and DECODE_FILE() are quite slow.
  ;; Since DECODE_FILE() is used many times, directory_files is
  ;; too slow when {default-,}file-name-coding-system are non-nil.
  ;; If {default-,}file-name-coding-system are bound to nil, 
  ;; ENCODE_FILE() and DECODE_FILE() are skipped, resulting speed up.
  ;; So, we need to encode DIR by ourselves.
  (let ((edir (expand-file-name dir))
	;; XEmacs 21.1.14 does not have default-file-name-coding-system
	(cs (or (and (boundp 'default-file-name-coding-system)
		     default-file-name-coding-system)
		(and (boundp 'file-name-coding-system)
		     file-name-coding-system))))
    (setq edir (mew-cs-encode-string edir cs))
    (or regex (setq regex mew-regex-message-files))
    (mew-alet
     (directory-files edir nil regex 'no-sort))))

(defun mew-folder-messages (folder)
  (let* ((dir (mew-expand-folder folder))
	 msgs nums)
    (when (file-directory-p dir)
      (setq msgs (mew-dir-messages dir))
      (setq nums (sort (mapcar 'string-to-int msgs) '<))
      (mapcar 'int-to-string nums))))

(defun mew-folder-new-message (folder &optional num-only cache)
  (let* ((dir (mew-expand-folder folder))
	 (regex (if cache mew-regex-message-files3 mew-regex-message-files))
	 (max 0)
	 cur maxfile maxpath msgs)
    ;; xxx create if there is no directory?
    (when (file-directory-p dir)
      (setq msgs (mew-dir-messages dir regex))
      (while msgs
	(setq cur (string-to-int (car msgs)))
	(if (> cur max) (setq max cur))
	(setq msgs (cdr msgs)))
      (setq max (1+ max))
      (setq maxfile (int-to-string max))
      (setq maxpath (mew-expand-folder folder maxfile))
      (while (file-exists-p maxpath) ;; xxx
	;; If NFS is used, readdir() may fail. Emacs 20 does not
	;; rescan the directory again. So, we need to rescan in
	;; the Emacs level.
	(mew-touch-folder folder) ;; can clear cache?
	(setq msgs (mew-dir-messages dir regex))
	(setq max 0)
	(while msgs
	  (setq cur (string-to-int (car msgs)))
	  (if (> cur max) (setq max cur))
	  (setq msgs (cdr msgs)))
	(setq max (1+ max))
	(setq maxfile (int-to-string max))
	(setq maxpath (mew-expand-folder folder maxfile)))
      (while (get-file-buffer maxpath) ;; xxx
	;; file not exist but there is a buffer.
	(setq max (1+ max))
	(setq maxfile (int-to-string max))
	(setq maxpath (mew-expand-folder folder maxfile)))
      (while (file-exists-p maxpath)
	(setq maxfile (read-string (format "%s/%s exists. Input a message number: " max folder)))
	(while (not (string-match mew-regex-message-files maxfile))
	  (setq maxfile (read-string "Input NUMBER: ")))
	(setq maxpath (mew-expand-folder folder maxfile)))
      (if num-only
	  maxfile
	maxpath))))

(defun mew-touch-folder (fld)
  (when (stringp mew-summary-touch-file)
    (let ((file (mew-expand-folder fld mew-summary-touch-file)))
      (when (file-writable-p file)
	(write-region "touched by Mew." nil file nil 'no-msg)
	(mew-set-file-modes file)))))

;; this kind of defmacro can't recurse
;; if +foo exists and we insert +foo/bar,
;; +foo/ should be inserted, too. But +foo/ does not match to +foo...
(defmacro mew-folder-insert (folder lst subdir)
  `(unless (mew-assoc-equal ,folder ,lst 0)
     (let ((case-fold-search nil)
	   (max (1- (length ,lst)))
	   (pair (mew-folder-func ,folder ,subdir))
	   (min 0)
	   mid crr prv)
       (while (> (- max min) 20) ;; 20 is enough?
	 (setq mid (/ (+ min max) 2))
	 (if (string< (car (nth mid ,lst)) ,folder)
	     (setq min mid)
	   (setq max mid)))
       (setq crr (nthcdr min ,lst))
       (while (and crr (string< (car (car crr)) ,folder))
	 (setq prv crr)
	 (setq crr (cdr crr)))
       (if prv
	   (setcdr prv (cons pair crr))
	 (setq ,lst (cons pair crr))))))

(defmacro mew-folder-delete (folder lst)
  `(setq ,lst (delete (assoc ,folder ,lst) ,lst)))

(defun mew-folder-node-p (folder &optional case)
  (cond
   ((mew-folder-localp folder)
    (let* ((dir (file-name-as-directory folder))
	   (regex (concat "^" (regexp-quote dir))))
      (mew-assoc-match regex (mew-local-folder-alist) 0)))
   ((mew-folder-imapp folder)
    (let* ((dir (mew-imap-file-name-as-directory folder case))
	   (regex (concat "^" (regexp-quote dir))))
      (mew-assoc-match regex (mew-imap-folder-alist case) 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Folder spec
;;;

(defconst mew-folder-spec-type
  '((regex     . string-match)
    (recursive . mew-folder-recursive-match)
    (string    . string=)))

(defsubst mew-folder-spec-func (type)
  (or (cdr (assq type mew-folder-spec-type)) 'string=))

(defun mew-folder-spec (folder lst str-type lst-type)
  (let ((str-func (mew-folder-spec-func str-type))
	(lst-func (mew-folder-spec-func lst-type))
	key keys values ret)
    (setq folder (mew-case:folder-folder folder))
    (catch 'loop
      (while lst
	(setq keys (car (car lst)))
	(setq values (cdr (car lst)))
	(setq lst (cdr lst))
	(cond
	 ((eq keys t)
	  (throw 'loop (setq ret values)))
	 ((stringp keys)
	  (if (funcall str-func keys folder)
	      (throw 'loop (setq ret values))))
	 ((listp keys)
	  (while keys
	    (setq key (car keys))
	    (setq keys (cdr keys))
	    (if (funcall lst-func key folder)
		(throw 'loop (setq ret values))))))))
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Directory
;;;

(defun mew-make-directory (path)
  (let ((parent (directory-file-name (file-name-directory path))))
    (unless (file-directory-p parent)
      (mew-make-directory parent))
    (if (and (file-exists-p path) (not (file-directory-p path)))
	(delete-file path))
    (make-directory path)
    (if (/= mew-folder-mode (mew-file-get-mode path))
	(set-file-modes path mew-folder-mode))))

(defun mew-delete-directory-recursively (dir)
  (when (file-directory-p dir)
    (let ((files (directory-files dir 'full mew-regex-files 'no-sort)))
      (while files
	(cond
	 ((file-symlink-p (car files))
	  ;; never chase symlink which points a directory
	  (delete-file (car files)))
	 ((file-directory-p (car files))
	  (mew-delete-directory-recursively (car files)))
	 (t
	  (delete-file (car files))))
	(setq files (cdr files))))
    (unless (directory-files dir 'full mew-regex-files 'no-sort)
      (delete-directory dir))))

(defun mew-check-directory (dir)
  (unless (file-directory-p dir)
    (unless (file-exists-p dir)
      (mew-make-directory dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File existence
;;;

(defun mew-which (file path)
  (catch 'loop
    (while path
      (if (file-exists-p (expand-file-name file (car path)))
	  (throw 'loop (expand-file-name file (car path)))
	(setq path (cdr path))))))

(defun mew-which-el (elfile)
  (or (mew-which (concat elfile ".el") load-path)
      (mew-which (concat elfile ".elc") load-path)))

(defvar mew-which-exec-suffixes '(""))

(defun mew-which-exec (execfile)
  (let ((suffixes mew-which-exec-suffixes)
	file)
    (catch 'detect
      (while suffixes
	(when (setq file (mew-which (concat execfile (car suffixes)) exec-path))
	  (throw 'detect file))
	(setq suffixes (cdr suffixes))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File attribute
;;;

;; Functions to get other attributes are implemented in C level.

(defsubst mew-file-get-links (file)
  (let ((w32-get-true-file-link-count t)) ;; for Meadow
    (nth 1 (file-attributes file))))

(defsubst mew-file-get-time (file)
  (nth 5 (file-attributes file)))

(defsubst mew-file-get-size (file)
  (nth 7 (file-attributes file)))

(defun mew-file-get-mode (file)
  (let* ((mode (nth 8 (file-attributes file)))
	 (len (length mode))
	 (i 1)
	 (dec 0))
    (while (< i len)
      (setq dec (* dec 2))
      (unless (char-equal (aref mode i) ?-)
	(setq dec (1+ dec)))
      (setq i (1+ i)))
    dec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; File
;;;

(defsubst mew-file-chase-links (file)
  "Chase links in FILENAME until a name that is not a link.
Does not examine containing directories for links."
  (let ((ret file) exp)
    (while (setq exp (file-symlink-p ret))
      (setq ret (expand-file-name exp (file-name-directory ret))))
    ret))

(defun mew-file-from-home (str)
  (if (string-match (expand-file-name mew-home) str)
      (concat mew-home (substring str (match-end 0)))
    str))

(defun mew-prepend-prefix (file prefix)
  (if (file-name-absolute-p file)
      (concat (file-name-directory file) prefix (file-name-nondirectory file))
    (concat prefix file)))

(defun mew-rotate-log-files (file-name)
  (let ((i 8) (file (expand-file-name file-name mew-conf-path)))
    (when (and (file-exists-p file)
	       (>= (mew-file-get-size file) mew-log-max-size))
      (while (>= i 0)
	(if (file-exists-p (format "%s.%d" file i))
	    (rename-file (format "%s.%d" file i)
			 (format "%s.%d" file (1+ i)) t))
	(setq i (1- i)))
      (rename-file file (format "%s.0" file)))))

(defun mew-remove-drive-letter (file)
  (if (string-match mew-regex-drive-letter file)
      (substring file 2)
    file))

(defun mew-get-file-modes (path)
  (let* ((dir (file-name-directory path))
	 (dirmode (mew-file-get-mode dir)))
    (logand dirmode mew-file-mode-mask)))

(defun mew-set-file-modes (path)
  (set-file-modes path (mew-get-file-modes path)))

(defun mew-delete-file (file)
  (if (and (stringp file) (file-exists-p file)) (delete-file file)))

(defun mew-insert-file-contents (&rest args)
  "A safe version of insert-file-contents.
This checks -*-coding:ctext;-*- internally when including a file."
  (let ((after-insert-file-functions nil))
    ;; preventing after-insert-file-set-buffer-file-coding-system
    (apply 'mew-insert-file-contents2 args)))

(defun mew-insert-file-contents2 (&rest args)
  "Mew version of insert-file-contents.
This sets  buffer-file-coding-system."
  (let ((auto-image-file-mode nil))
    (apply 'insert-file-contents args)))

(defun mew-find-file-noselect (&rest args)
  "A safe version of find-file-noselect.
This checks -*-coding:ctext;-*- internally when including a file.
But this does not set buffer-file-coding-system."
  (let ((after-insert-file-functions nil))
    ;; preventing after-insert-file-set-buffer-file-coding-system
    (apply 'mew-find-file-noselect2 args)))

(defun mew-find-file-noselect2 (&rest args)
  "A safe version of find-file-noselect.
This checks -*-coding:ctext;-*- internally when including a file
and sets buffer-file-coding-system."
  (let ((auto-image-file-mode nil)
	(format-alist nil)
	(auto-mode-alist nil)
        (enable-local-variables nil)
	(find-file-hook nil)
        (find-file-hooks nil))
    (apply 'find-file-noselect args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; temp name
;;;

(defun mew-make-temp-name (&optional fname)
  (unless (file-exists-p mew-temp-dir)
    (mew-make-directory mew-temp-dir)) ;; just in case
  (if fname
      ;; File name of a temporary file should be ASCII only.
      (if (and (string-match "^[ -~]+$" fname)
	       (not (file-exists-p (expand-file-name fname mew-temp-dir))))
	  (expand-file-name fname mew-temp-dir)	
	(if (string-match "\\.[ -~]+$" fname)
	    (concat (make-temp-name mew-temp-file) (mew-match-string 0 fname))
	  (make-temp-name mew-temp-file)))
    (make-temp-name mew-temp-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Random
;;;

(defvar mew-random-last nil)
(defvar mew-random-base-09 "0123456789")
(defvar mew-random-base-az "abcdefghijklmnopqrstuvwxwz")

(defun mew-random ()
  (let* ((vec (recent-keys))
	 (len (length vec))
	 (i 0) (ran (+ (random) (emacs-pid)))
	 c)
    (while (< i len)
      (setq c (aref vec i))
      (cond
       ((and mew-temacs-p (numberp c))
	(setq ran (+ ran c)))
       ((and mew-xemacs-p (eventp c))
	(if (characterp (event-to-character c))
	    (setq ran (+ ran (char-to-int (event-to-character c)))))))
      (setq i (1+ i)))
    (abs ran)))

(defun mew-random-string (len nump)
  (let* ((base (if nump mew-random-base-09 mew-random-base-az))
	 (baselen (length base))
	 (ret (make-string len ?a))
	 (i 0))
    (while (< i len)
      (aset ret i (aref base (% (mew-random) baselen)))
      (setq i (1+ i)))
    (while (string= ret mew-random-last)
      (setq i 0)
      (while (< i len)
	(aset ret i (aref base (% (mew-random) baselen)))
	(setq i (1+ i))))
    ret))

(defun mew-random-filename (dir len nump &optional suffix)
  (let ((cnt 0) (max 20) ;; ad hoc
	file filepath)
    (setq file (concat (mew-random-string len nump) suffix))
    (setq filepath (expand-file-name file dir))
    (while (and (file-exists-p filepath) (< cnt max))
      (setq file (concat (mew-random-string len nump) suffix))
      (setq filepath (expand-file-name file dir))
      (setq cnt (1+ cnt)))
    (if (file-exists-p filepath) 
	nil
      filepath)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer
;;;

(defsubst mew-erase-buffer ()
  (mew-elet
   (widen)
   (erase-buffer)
   (buffer-disable-undo)))

(defun mew-remove-buffer (buf)
  (if (and buf (get-buffer buf)) (kill-buffer buf)))

(defmacro mew-elet (&rest body)
  `(let ((buffer-read-only nil)
	 (inhibit-read-only t)
	 (after-change-functions nil)
	 (mark-active nil) ;; for Emacs
	 (zmacs-regions nil)) ;; for XEmacs
     ,@body))

(defun mew-push-mark ()
  ;; for C-x C-x
  (let ((mark-active nil) ;; for Emacs
	(zmacs-regions nil)) ;; for XEmacs
    (push-mark (point) t t)))

(defsubst mew-region-bytes (beg end buf)
  ;; string-bytes() acts differently on each Emacs.
  ;; set-buffer-multibyte is also buggy.
  ;; So, use this way.
  (save-excursion
    (set-buffer buf)
    (if (fboundp 'string-as-unibyte)
	(length (string-as-unibyte (mew-buffer-substring beg end)))
      (- end beg))))

(defun mew-count-lines (beg end)
  "Return number of lines between BEG and END."
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (setq beg (point))
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (if (or (not (mew-decode-syntax-p))
	    (not (equal (mew-decode-syntax-buffer) (current-buffer))))
	(mew-count-lines1 beg end)
      (let ((mbeg (mew-decode-syntax-begin))
	    (mend (mew-decode-syntax-end)))
	(if (or (<= end mbeg) (>= beg mend))
	    (mew-count-lines1 beg end)
	  (+ (mew-count-lines1 beg mbeg) (mew-count-lines1 mend end)))))))

(defun mew-count-lines1 (beg end)
  (if (>= beg end) 
      0
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (if (not (and (mew-thread-p) mew-use-thread-separator))
	  (- (buffer-size) (forward-line (buffer-size)))
	(let ((regex mew-regex-thread-separator)
	      (lines 0))
	  (while (not (eobp))
	    (unless (looking-at regex)
	      (setq lines (1+ lines)))
	    (forward-line))
	  lines)))))

(defun mew-buffer-list (regex &optional listp mode)
  (let ((bufs (mapcar 'buffer-name (buffer-list)))
	buf ret)
    (while bufs
      (setq buf (car bufs))
      (setq bufs (cdr bufs))
      (when (and (string-match regex buf)
		 (or (not mode)
		     (and mode (get-buffer buf)
			  (save-excursion
			    (set-buffer buf)
			    (eq major-mode mode)))))
	(if listp
	    (setq ret (cons (list buf) ret))
	  (setq ret (cons buf ret)))))
    (nreverse ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; process
;;;

(defmacro mew-filter (&rest body)
  `(let ((pbuf (process-buffer process)) ;; MUST use 'process'
	 (obuf (buffer-name)))
     (if (and (bufferp pbuf)
	      (buffer-name pbuf)) ;; check a killed buffer
	 ;; must use buffer-name instead of current-buffer
	 ;; so that get-buffer can detect killed buffer.
	 (unwind-protect
	     (progn
	       ;; buffer surely exists.
	       (set-buffer (process-buffer process)) ;; necessary
	       ,@body)
	   (if (get-buffer obuf)
	       ;; the body sometimes kills obuf.
	       (set-buffer obuf))))))

(defun mew-start-process-disp (name buffer program &rest program-args)
  (let ((disp (if (and mew-xemacs-p (eq (device-type) 'x))
                  (device-connection)
		(cdr (assq 'display (frame-parameters)))))
        (process-environment (copy-sequence process-environment)))
    (if disp (setenv "DISPLAY" disp))
    (apply 'start-process name buffer program program-args)))

(defun mew-start-process-lang (name buffer program &rest program-args)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "LANGUAGE" "C")
    (setenv "LC_ALL" "C")
    (setenv "LANG" "C")
    (apply 'start-process name buffer program program-args)))

(defun mew-call-process-lang (prog &optional infile buffer display &rest args)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "LANGUAGE" "C")
    (setenv "LC_ALL" "C")
    (setenv "LANG" "C")
    (apply 'call-process prog infile buffer display args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global info
;;;

(defsubst mew-info (name)
  (cond
   ((vectorp name) name) ;; just for .mqi
   ((stringp name)
    (if (or (not (intern-soft name))
	    (not (boundp (intern name))))
	(if (string-match "^mew-[^-]+-info-" name)
	    (let* ((sym (intern (concat (mew-match-string 0 name) "list")))
		   (lst (symbol-value sym))
		   (len (length lst)))
	      (set (intern name) (make-vector len nil)))))
    (symbol-value (intern-soft name)))))

(defun mew-info-defun (prefix lst)
  (let ((i 0) ent)
    (while lst
      (setq ent (car lst))
      (setq lst (cdr lst))
      (fset (intern (concat prefix "get-" ent))
	    `(lambda (arg)
	       (cond
		((stringp arg) (aref (mew-info arg) ,i))
		((vectorp arg) (aref arg ,i)))))
      (fset (intern (concat prefix "set-" ent))
	    `(lambda (arg value)
	       (cond
		((stringp arg) (aset (mew-info arg) ,i value))
		((vectorp arg) (aset arg ,i value)))))
      (setq i (1+ i)))))

(defun mew-info-clean-up (arg &optional start)
  (let ((i (or start 0)) vec len)
    (cond
     ((stringp arg) (setq vec (mew-info arg)))
     ((vectorp arg) (setq vec arg)))
    (setq len (length vec))
    (while (< i len)
      (aset vec i nil)
      (setq i (1+ i)))))

(defsubst mew-net-get-ssh-process (pnm)
  (aref (mew-info pnm) 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer local info
;;;

(defsubst mew-blinfo (sym)
  (let* ((name (symbol-name sym))
	 (lname (concat name "-list"))
	 (lsym (intern lname))
	 (lst (symbol-value lsym))
	 (len (length lst)))
    (set sym (make-vector len nil))
    (symbol-value sym)))

(defun mew-blinfo-defun (blv-sym lst)
  (let ((i 0) ent)
    (while lst
      (setq ent (car lst))
      (setq lst (cdr lst))
      (fset (intern (concat (symbol-name blv-sym) "-get-" ent))
	    `(lambda ()
	       (cond
		((null ,blv-sym) (aref (mew-blinfo (quote ,blv-sym)) ,i))
		((vectorp ,blv-sym) (aref ,blv-sym ,i)))))
      (fset (intern (concat (symbol-name blv-sym) "-set-" ent))
	    `(lambda (value)
	       (cond
		((null ,blv-sym) (aset (mew-blinfo (quote ,blv-sym)) ,i value))
		((vectorp ,blv-sym) (aset ,blv-sym ,i value)))))
      (setq i (1+ i)))))


(defvar mew-ainfo-list '("icon" "win-cfg"))
(mew-blinfo-defun 'mew-ainfo mew-ainfo-list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Address
;;;

(defsubst mew-get-my-address ()
  (or (mew-header-parse-address mew-from:) (mew-mail-address)))

;;

(defsubst mew-get-my-address-regex-list ()
  "This creates a list of regular expression used to tell
whether or not a given address is mine. The list is created
from (mew-user), (mew-mail-address), and 'mew-mail-address-list'."
  (cons (concat "^" (regexp-quote (mew-user)) "$")
	(cons (concat "^" (regexp-quote (mew-mail-address)) "$")
	      mew-mail-address-list)))
  
(defsubst mew-is-my-address (addrs from)
  (and from
       (let ((case-fold-search t))
	 (catch (quote match)
	   (car (mapcar (lambda (arg) (and (string-match arg from)
					   (throw (quote match) t)))
			addrs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Lisp load/save
;;;

(defun mew-lisp-load (filename)
  "Load lisp from FILENAME"
  (let* ((fullname (if (file-name-absolute-p filename)
		       filename
		     (expand-file-name filename mew-conf-path)))
	 (tmp-buf (create-file-buffer fullname))
	 lisp)
    (if (file-readable-p fullname)
	(save-excursion
	  (set-buffer tmp-buf)
	  (mew-erase-buffer)
	  (mew-frwlet
	   mew-cs-m17n mew-cs-dummy
	   (mew-insert-file-contents fullname))
	  (setq lisp 
		(condition-case nil
		    (read (current-buffer))
		  (error ())))))
    (mew-remove-buffer tmp-buf)
    lisp))

(defun mew-lisp-save (filename lisp &optional nobackup unlimit)
  "Save LISP to FILENAME. LISP is truncated to mew-lisp-max-length
by side-effect."
  (let* ((fullname (if (file-name-absolute-p filename)
		       filename
		     (expand-file-name filename mew-conf-path)))
	 (backname (concat fullname mew-backup-suffix))
	 (tmp-buf (create-file-buffer fullname))
	 print-length print-level) ;; for Emacs 21
    (if (file-writable-p fullname)
	(save-excursion
	  (if nobackup
	      (mew-delete-file fullname)
	    (if (file-exists-p fullname)
		(rename-file fullname backname 'override)))
	  (set-buffer tmp-buf)
	  (mew-erase-buffer)
	  (when (and (not unlimit) (> (length lisp) mew-lisp-max-length))
	    (setq lisp (copy-sequence lisp)) ;; no side effect
	    (setcdr (nthcdr (1- mew-lisp-max-length) lisp) nil))
	  (if (> (length lisp) mew-lisp-max-length)
	      (print lisp tmp-buf)
	    (pp lisp tmp-buf))
	  (mew-frwlet
	   mew-cs-dummy mew-cs-m17n
	   (write-region (point-min) (point-max) fullname nil 'no-msg))
	  (mew-set-file-modes fullname)))
    (mew-remove-buffer tmp-buf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Month
;;;

(defvar mew-time-mon-alist
  '(("Jan" .  1) ("Feb" .  2) ("Mar" .  3) ("Apr" .  4)
    ("May" .  5) ("Jun" .  6) ("Jul" .  7) ("Aug" .  8)
    ("Sep" .  9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defsubst mew-time-mon-str-to-int (str)
  (or (cdr (assoc (capitalize str) mew-time-mon-alist)) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Time Zone
;;;

;; Define symbol time zone defined in RFC822 only.
(defvar mew-time-tmzn-alist
  '(("UT"  .  0) ("GMT" .  0)
    ("EST" . -5) ("EDT" . -4)
    ("CST" . -6) ("CDT" . -5) 
    ("MST" . -7) ("MDT" . -6)
    ("PST" . -8) ("PDT" . -7)))

(defsubst mew-time-tmzn-str-to-int (str)
  (cdr (assoc (upcase str) mew-time-tmzn-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RFC 822
;;; Date: Wed, 26 Jul 2000 21:18:35 +0900 (JST)
;;;

(defvar mew-time-rfc-regex
  "\\([0-9]+\\)[ \t]+\\([a-zA-Z]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?\\([ \t]+\\([-+0-9a-zA-Z]+\\)\\)?")

(defmacro mew-time-rfc-day  ()
  '(string-to-int (substring s (match-beginning 1) (match-end 1))))

(defmacro mew-time-rfc-mon  ()
  '(substring s (match-beginning 2) (match-end 2)))

(defmacro mew-time-rfc-year ()
  '(string-to-int (substring s (match-beginning 3) (match-end 3))))

(defmacro mew-time-rfc-hour ()
  '(string-to-int (substring s (match-beginning 4) (match-end 4))))

(defmacro mew-time-rfc-min  ()
  '(string-to-int (substring s (match-beginning 5) (match-end 5))))

(defmacro mew-time-rfc-sec  ()
  '(if (match-beginning 7)
       (string-to-int (substring s (match-beginning 7) (match-end 7)))
     0))

;; returns seconds
(defmacro mew-time-rfc-tmzn ()
  '(if (match-beginning 9)
       (let ((tmzn (substring s (match-beginning 9) (match-end 9)))
	     int)
	 (cond
	  ((string-match "^[-+][0-9]+$" tmzn)
	   (setq int (string-to-int tmzn))
	   (+ (* (/ int 100) 3600) (* (% int 100) 60)))
	  ((setq int (mew-time-tmzn-str-to-int tmzn))
	   (* int 3600))
	  (t 0)))
     0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sort key for Date:
;;;

;; "20000726121835"
(defsubst mew-time-ctz-to-sortkey (time)
  (let ((system-time-locale "C"))
    (format-time-string "%Y%m%d%H%M%S" time)))

(defsubst mew-time-ctz-to-sortkey-invalid (sec min hour day mon year)
  (format "%04d%02d%02d%02d%02d%02d" year mon day hour min sec))

;; "20000726121835"
(defsubst mew-time-rfc-to-sortkey (s &optional tzadj)
  (if (string-match mew-time-rfc-regex s)
      (let ((year (mew-time-rfc-year))
	    (mon  (mew-time-mon-str-to-int (mew-time-rfc-mon)))
	    (day  (mew-time-rfc-day))
	    (hour (mew-time-rfc-hour))
	    (min  (mew-time-rfc-min))
	    (sec  (mew-time-rfc-sec))
	    (tmzn (mew-time-rfc-tmzn)))
	(cond
	 ((< year 50)
	  (setq year (+ year 2000)))
	 ((< year 100)
	  (setq year (+ year 1900))))
	(if (or (< year 1970) (>= year 2038))
	    ;; invalid data
	    (mew-time-ctz-to-sortkey-invalid sec min hour day mon year)
	  (setq sec (- sec tmzn))
	  (if tzadj (setq sec (+ sec (car (current-time-zone)))))
	  (mew-time-ctz-to-sortkey (encode-time sec min hour day mon year))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User friendly time
;;;

(defsubst mew-time-tmzn-int ()
  (let ((tz (car (current-time-zone))))
    (if (< tz 0)
	(format "-%02d%02d" (/ (- tz) 3600) (/ (% (- tz) 3600) 60))
      (format "+%02d%02d" (/ tz 3600) (/ (% tz 3600) 60)))))

;; Wed, 26 Jul 2000 21:18:35 +0900 (JST)
(defsubst mew-time-ctz-to-rfc (time)
  (let ((system-time-locale "C")
	(tmzn-int (mew-time-tmzn-int)))
    ;; XEmacs does not have %z
    (format (format-time-string "%a, %d %b %Y %T %%s (%Z)" time) tmzn-int)))

;; 2000/07/12 16:22:30
(defsubst mew-time-ctz-to-logtime (time)
  (let ((system-time-locale "C"))
    (format-time-string "%Y/%m/%d %H:%M:%S" time)))

;; 20000712.155559
(defsubst mew-time-ctz-to-msgid (time)
  (let ((system-time-locale "C"))
    (format-time-string "%Y%m%d.%H%M%S" time)))

;;

(defsubst mew-time-calc (new old)
  (let ((million 1000000)
	(sec (+ (* 65536 (- (nth 0 new) (nth 0 old)))
		(- (nth 1 new) (nth 1 old))))
	(usec (- (nth 2 new) (nth 2 old))))
    (if (< usec 0)
        (setq sec (1- sec)
              usec (+ usec million))
      (if (>= usec million)
          (setq sec (1+ sec)
                usec (- usec million))))
    (+ sec (/ usec (float million)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Multibyte
;;;

(defsubst mew-set-buffer-multibyte (arg)
  (if (fboundp 'set-buffer-multibyte)
      (set-buffer-multibyte arg)))

(defsubst mew-set-string-multibyte (str)
  (if (fboundp 'string-as-multibyte)
      (string-as-multibyte str)
    str))

(defsubst mew-multibyte-string-p (str)
  (if (fboundp 'multibyte-string-p)
      (multibyte-string-p str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Network
;;;

(defsubst mew-port-sanity-check (port)
  (if (string-match "^[0-9]+$" port)
      (string-to-int port)
    port))

(defsubst mew-dot-insert ()
  (goto-char (point-min))
  (while (re-search-forward "^\\." nil t)
    (insert ".")
    (forward-line)))

(defsubst mew-dot-delete ()
  (goto-char (point-max))
  (forward-line -1)
  (delete-region (point) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "^\\." nil t)
    (delete-char -1)
    (forward-line)))

(defsubst mew-crlf-to-lf ()
  (while (search-forward "\r\n" nil t) (replace-match "\n" nil t)))

(defsubst mew-lf-to-crlf ()
  (while (search-forward "\n" nil t) (replace-match "\r\n" nil t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; sit-for
;;;

(defsubst mew-redraw (&optional time)
  (sit-for (or time 0)))

(defmacro mew-rendezvous (who)
  ;; Wait for the termination of WHO.
  ;; Emacs does not provide synchronize mechanism with
  ;; an asynchronous process. So, take this way. 
  `(while ,who
     (if mew-xemacs-p
	 (accept-process-output)
       (sit-for 0.1)
       ;; accept-process-output or sleep-for is not enough
       (discard-input))))

(defsubst mew-let-user-read ()
  (sit-for 1.5))

(defsubst mew-warn (&rest msg)
  (apply 'message msg)
  (ding)
  (mew-let-user-read))

(defsubst mew-timing ()
  (sit-for 0.01))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mew-set (vars vals)
  (while vars
    (when (car vars)
      (set (car vars) (car vals)))
    (setq vars (cdr vars))
    (setq vals (cdr vals))))

(provide 'mew-func)

;;; Copyright Notice:

;; Copyright (C) 1997-2005 Mew developing team.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mew-func.el ends here
