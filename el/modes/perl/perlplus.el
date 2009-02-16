;;; -*- Emacs-Lisp -*-
;;; Perl mode plus
;;; (c) 1995-2000, 2005 by HIROSE Yuuji [yuuji@gentei.org]
;;; $Id: perlplus.el,v 1.2 2000/02/01 07:40:17 yuuji Exp yuuji $
;;; Last modified Fri Sep 16 00:56:10 2005 on firestorm

;;; [Commentary]
;;; 
;;; This package enables function/variable name completion on cperl-mode.
;;; (Use this package with your favorite perl-editing mode.)
;;; 
;;; [How to use]
;;; 
;;; Put the next lines into your ~/.emacs
;;; 
;;; 	(setq cperl-mode-hook
;;;	  '(lambda ()
;;; 	     (require 'perlplus)
;;;	     (define-key cperl-mode-map "\M-\t" 'perlplus-complete-symbol)
;;;	     (perlplus-setup)))
;;; 
;;; If you  are using cperl-mode,  replace  all `cperl-mode's above  with
;;; ccperl-mode.
;;; 

;;; Does this work?
;;; [Description]
;;; 
;;; Once you have set  up as above, you  can use M-TAB to complete  perl
;;; symbol (built-in functions,  user  defined functions  and variables)
;;; including  require-ed  perl libraries.   If  you modify the required
;;; library (by current source text), call M-x  cperl-mode (or other mode
;;; you are using) again from referring perl buffer.
;;; 
;;; If the  file `foo.pl' is required  and found  in system perl library
;;; directory which  is specified  by  the  second  list's part of  each
;;; element of the variable perlplus-inc-path-alist (see the description
;;; of   the  variable), or `foo.pl'  is    set to read-only  attribute,
;;; `foo.pl' is read only once  even if the function `perlplus-setup' is
;;; read more than  once.  This causes the  symbol collection  speed up,
;;; but inconsistency  of   symbol  completion  table when  you   modify
;;; `foo.pl'.  If you need to synchronize completion table, call;
;;; 
;;;	M-x perlplus-reset
;;; 
;;; このプログラムを使うことにより、Perl のシンボル({内部,外部}{関数,変
;;; 数})を ESC TAB で補完できるようになります。カレントファイルで参照し
;;; ているライブラリファイルを変更した場合は、カレントバッファで再度
;;; 
;;;	M-x cperl-mode
;;;	(あるいはあなたの使っている Perl 編集モードコマンド)
;;; 
;;; とすることで、外部シンボルの変更をカレントファイルに反映させることが
;;; できます。 ただし、変数 perlplus-inc-path-alist で system 属性が付け
;;; られているディレクトリにあるperlライブラリ、または readonly のライブ
;;; ラリファイル中のシンボルはファイル読み込みと同時に perlplus の内部変
;;; 数に格納され再度読み込まれることはありません。もし、このようなライブ
;;; ラリファイルに修正を加えて補完テーブルに食い違いが生じた場合は
;;; 
;;;	M-x perlplus-reset
;;; 
;;; として、内部補完テーブルの初期化を行って下さい。
;;; 
;;; [NO WARRANTY]
;;; 
;;; This  program  is distributed    as a   free   software.   You   can
;;; redistribute this software  freely but with  NO warranty to anything
;;; as   a result  of  using this   software.   However, any reports and
;;; suggestions   are welcome as   long  as I  feel   interests  in this
;;; software.
;;; 
;;; このプログラムはフリーソフトウェアとして配布致します。本プログラムを
;;; 利用することにより生じた結果に関して、作者は一切の責任を負いかねます
;;; のでご注意下さい。バグレポート/コメント等は歓迎致します。連絡は以下
;;; のアドレスまでお願い致します。
;;; 
;;; 							   HIROSE, Yuuji
;;; 							yuuji@gentei.org

(defvar perlplus-inc-path-alist
  '(("/usr/local/lib/perl" . system)
    ("/usr/local/lib/perl5" . system)
    ("/usr/share/perl" . system)
    ("." . user))
  "* @INC path alist of perl;
ex.
'((PATH1 . system)
  (PATH2 . user))
The car of a list is path name of @INC directory, and the cdr of a list should
one of 'system or 'user, which specifies if that perl library is system
library or not.  When system library, perlplus won't read library files
twice.  When user library, perlplus reads them everytime you call perl mode.")
(defvar perlplus-perl5 nil "If the perl is version 5")
(defvar perlplus-function-open "("
  "*Function's argument starting character.
If you don't like parenthesizing function's arguments, set \" \".")

(defvar perlplus-system-function-alist nil)
(defvar perlplus-system-variable-alist nil)
(defvar perlplus-perl-symbol-regexp "[A-Za-z_][A-Za-z_0-9\':]*"
  "Regexp of perl symbol names")
(defvar perlplus-perl-symbol-regexp2 "[A-Za-z_0-9\':]*"
  "Regexp of the second character or later of perl symbol names")
(defvar perlplus-external-functions nil "External function list")
(defvar perlplus-external-variables nil "External variable list")

(defvar perlplus-builtin-function-alist
  '(
    ("accept") ("alarm") ("atan2") ("bind") ("binmode") ("caller") ("chdir")
    ("chmod") ("chop") ("chown") ("chroot") ("close") ("closedir") ("cmp")
    ("connect") ("cos") ("crypt") ("dbmclose") ("dbmopen") ("defined")
    ("delete") ("die") ("do") ("dump") ("each") ("endgrent") ("endhostent")
    ("endnetent") ("endprotoent") ("endpwent") ("endservent") ("eof") ("eq")
    ("eval") ("exec") ("exit") ("exp") ("fcntl") ("fileno") ("flock")
    ("for") ("foreach") ("fork") ("ge") ("getc") ("getgrent") ("getgrgid")
    ("getgrnam") ("gethostbyaddr") ("gethostbyname") ("gethostent")
    ("getlogin") ("getnetbyaddr") ("getnetbyname") ("getnetent")
    ("getpeername") ("getpgrp") ("getppid") ("getpriority")
    ("getprotobyname") ("getprotobynumber") ("getprotoent") ("getpwent")
    ("getpwnam") ("getpwuid") ("getservbyname") ("getservbyport")
    ("getservent") ("getsockname") ("getsockopt") ("gmtime") ("goto")
    ("grep") ("gt") ("hex") ("if") ("index") ("int") ("ioctl") ("join")
    ("keys") ("kill") ("last") ("le") ("length") ("link") ("listen")
    ("local") ("localtime") ("log") ("lstat") ("lt") ("mkdir") ("msgctl")
    ("msgget") ("msgrcv") ("msgsnd") ("ne") ("next") ("oct") ("open")
    ("opendir") ("ord") ("pack") ("package") ("pipe") ("pop") ("print")
    ("printf") ("push") ("rand") ("read") ("readdir") ("readlink") ("recv")
    ("redo") ("rename") ("require") ("reset") ("return") ("reverse")
    ("rewinddir") ("rindex") ("rmdir") ("scalar") ("seek") ("seekdir")
    ("select") ("semctl") ("semget") ("semop") ("send") ("setgrent")
    ("sethostent") ("setnetent") ("setpgrp") ("setpriority") ("setprotoent")
    ("setpwent") ("setservent") ("setsockopt") ("shift") ("shmctl")
    ("shmget") ("shmread") ("shmwrite") ("shutdown") ("sin") ("sleep")
    ("socket") ("socketpair") ("sort") ("splice") ("split") ("sprintf")
    ("sqrt") ("srand") ("stat") ("study") ("substr") ("symlink") ("syscall")
    ("sysread") ("system") ("syswrite") ("tell") ("telldir") ("time")
    ("times") ("truncate") ("umask") ("undef") ("unless") ("unlink")
    ("unpack") ("unshift") ("until") ("utime") ("values") ("vec") ("wait")
    ("waitpid") ("wantarray") ("warn") ("while") ("write")
    )
  "Alist of Perl's built-in functions")

(defvar perlplus-control-structure-alist
  '(("if") ("while") ("unless") ("foreach"))
  "Alist of control structure of perl")

(defun perlplus-get-package-name ()
  (save-excursion
    (if (progn (goto-char (point-min))
	       (re-search-forward "package +\\([^;]*\\);" nil t))
	(let ((p (buffer-substring (match-beginning 1) (match-end 1))))
	  (if (equal p "main")
	      ""
	    (concat p (if perlplus-perl5 "::" "'"))))
      "")))

(defun perlplus-matching-functions-internal (initial &optional packagename)
  "Return the function names as a list."
  (save-excursion
    (setq packagename (if packagename (perlplus-get-package-name) ""))
    (let ((subrx (concat "^sub *\\(" perlplus-perl-symbol-regexp "\\)"))
	  list ini w)
      (setq ini (concat "^" (regexp-quote initial)))
      (goto-char (point-min))
      (while (re-search-forward subrx nil t)
	(if (string-match
	     ini (setq w (buffer-substring (match-beginning 1) (match-end 1))))
	    (progn
	      (or (string-match ".+\\('\\|::\\).+" w)
		  (setq w (concat packagename w)))
	      (setq list (cons (list w) list)))))
      list
      ;;(try-completion initial list)
      )))

(defun perlplus-match-list (initial alist)
  (let ((l alist) ml)
    (setq initial (concat "^" (regexp-quote initial)))
    (while l
      (if (string-match initial (car (car l)))
	  (setq ml (cons (car l) ml)))
      (setq l (cdr l)))
    ml))

(defun perlplus-matching-functions (initial)
  (append (perlplus-matching-functions-internal initial)
	  (perlplus-match-list initial perlplus-external-functions)))

(defun perlplus-function-completion (initial)
  (try-completion initial
		  (append
		   (perlplus-matching-functions initial)
		   perlplus-external-functions)))

(defun perlplus-matching-variables-local (initial)
  "Return the local variable names as a list."
  (let ((p (point)) e vlist word (case-fold-search nil))
    (save-excursion
      ;;(perl-beginning-of-function)
      (re-search-backward "^sub .*" nil 1)
      (if (= (point) (point-min))
	  nil
	(while (re-search-forward "\\<\\(local\\|my\\)\\s *(" p t)
	  (goto-char (1- (match-end 0)))
	  (setq e (save-excursion (forward-list 1) (point)))
	  (while (re-search-forward
		  (concat
		   "[\$@%\*]{?\\("
		   initial perlplus-perl-symbol-regexp2 "\\)") e t)
	    (setq word (buffer-substring
			(match-beginning 1) (match-end 1)))
	    (setq vlist (cons (list word) vlist)))
	  (goto-char (1+ e)))))
    vlist))

(defun perlplus-re-search-active-forward (regexp &optional bound noerror count rv)
  "Search regular expression in non-commented line."
  (let ((func (if rv 're-search-backward 're-search-forward)))
    (catch 'found
      (while (funcall func regexp bound noerror count)
	(if (save-excursion
	      (beginning-of-line)
	      (re-search-forward "[^$\\]?#" (match-beginning 0) t))
	    nil
	  (throw 'found t))))))


(defun perlplus-matching-variables-global (initial &optional packagename)
  "Return the global variable names as a list.
This function only investigates top level's variables.
Optional second argument PACKAGENAME append the package name"
  (let (vlist op word (case-fold-search nil) (p (point)))
    (save-excursion
      (setq packagename (if packagename (perlplus-get-package-name) ""))
      (goto-char (point-min))
      (while (not (eobp))
	(save-excursion
	  (if (perlplus-re-search-active-forward "^[ \t]*sub .* {" nil t)
	      (setq op (1- (match-end 0)))
	    (setq op (point-max))))
	(while (perlplus-re-search-active-forward
		;; "[\$@]\\([A-z_][A-Z_0-9\':]\\)*"
		(concat "[\$%@]\\("
			initial perlplus-perl-symbol-regexp2 "\\)") op t)
	  (setq word (buffer-substring (match-beginning 1) (match-end 1)))
	  (if packagename (setq word (concat packagename word)))
	  (or (assoc word vlist)
	      (string-match ".*\\('\\|::\\).*\\('\\|::\\)" word)
	      (= p (match-beginning 1))
	      (= p (match-end 0))
	      (setq vlist (cons (list word) vlist))))

	(goto-char op)
	;;(forward-list 1)
	(cond
	 ((condition-case err
	      (forward-list 1)
	    (error nil)))
	 ((re-search-forward "^sub .*{\\|^} *$" nil t)))
	))
    vlist))

(defun perlplus-matching-variables (initial)
  "Return the variable names as a list."
  (append (perlplus-matching-variables-local initial)
	  (perlplus-matching-variables-global initial)
	  (perlplus-match-list initial perlplus-external-variables)))
(defun perlplus-variable-completion (initial)
  (try-completion initial (perlplus-matching-variables initial)))
(defun perlplus-collect-imported-symbols1 ()
  (let ((cb (current-buffer))
	reqfiles rlist file plist dir vlist flist vl fl rl sys)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      ;"^[^#]*\\s \\(require\\|use\\) +\\([\"']\\)\\([^\";']+\\)\\2;"
	      "[\n\t ]\\(require\\|use\\) +\\([\"']\\)\\([^\";']+\\)\\2;"
	      ;"\\<\\(require\\|use\\) +\"\\(.+\\)\";"
	      nil t)
	(if (save-excursion
	      (save-match-data
		(let ((b (match-beginning 0)))
		  (beginning-of-line)
		  (search-forward "#" b t))))
	    (goto-char (match-end 0))
	  (setq reqfiles
		(cons (buffer-substring (match-beginning 3) (match-end 3))
		      reqfiles)))))
    (while reqfiles
      (setq file (car reqfiles))
      (setq plist perlplus-inc-path-alist)
      (if (assoc file perlplus-system-variable-alist) (setq plist nil))
      (while plist
	(setq dir (car (car plist))
	      sys (eq 'system (cdr (car plist))))
	(if (file-directory-p dir)
	    (if (file-exists-p (expand-file-name file dir))
		(let ((hilit-auto-highlight nil ))
		  (message "Collecting symbols in %s..." file)
		  (set-buffer (find-file-noselect (expand-file-name file dir)))
		  (setq rl (perlplus-collect-imported-symbols)
			vl (or (cdr-safe
				(assoc file perlplus-system-variable-alist))
			       (perlplus-matching-variables-global "" t))
			fl (or (cdr-safe
				(assoc file perlplus-system-function-alist))
			       (perlplus-matching-functions-internal "" t))
			vlist (append vlist vl (car rl))
			flist (append flist fl (car (cdr rl))))
		  (if (and (or sys buffer-read-only)
			   (null (assoc file perlplus-system-function-alist)))
		      (progn
			(setq perlplus-system-function-alist
			      (cons
			       (cons file fl) perlplus-system-function-alist)
			      perlplus-system-variable-alist
			      (cons
			       (cons file vl) perlplus-system-variable-alist))
			(kill-buffer nil)))
		  (set-buffer cb)
		  (message "Collecting symbols in %s...Done" file)
		  (setq plist nil))
	      ))
	(setq plist (cdr plist)))
      (setq reqfiles (cdr reqfiles)))
    (list vlist flist)))

(defun perlplus-collect-imported-symbols ()
  (let (perlplus:visited)
    (perlplus-collect-imported-symbols1)))
;;;
;; flash-string from rp-describe-function.el
;;;
(defun perlplus-flash-string (STRING)
  "Momentarily display STRING in the buffer; erase it on the next keystroke.
The string is displayed starting on the line after point.  The window is
recentered if necessary to make the whole string visible.  If the window isn't
large enough, at least you get to read the beginning."
  (let ((buffer-read-only nil)
        (modified (buffer-modified-p))
        (name buffer-file-name)
        insert-start
        insert-end)
    (unwind-protect
        (progn
          (save-excursion
            ;; defeat file locking... don't try this at home, kids!
            (setq buffer-file-name nil)
            (forward-line 1)
            (setq insert-start (point))
            (insert STRING)
            (setq insert-end (point)))
          ; make sure the whole string is visible
          (if (not (pos-visible-in-window-p insert-end))
            (recenter (max 0
                           (- (window-height)
                              (count-lines insert-start insert-end)
                              2))))
          (message "Type %s to continue editing..."
                   (single-key-description ?\ ))
          (let ((char (read-char)))
            (or (eq char ?\ )
                (setq unread-command-char char))))
      (if insert-end
          (save-excursion
            (delete-region insert-start insert-end)))
      (setq buffer-file-name name)
      (set-buffer-modified-p modified))))


(defun perlplus-reset ()
  "Reset perlplus internal symbol table."
  (interactive)
  (setq perlplus-system-function-alist nil
	perlplus-system-variable-alist nil)
  (let ((blist (buffer-list)))
    (while blist
      (set-buffer (car blist))
      (and (boundp 'perlplus-external-variables)
	   perlplus-external-variables
	   (message "Resetting %s..." (buffer-name))
	   (funcall major-mode)
	   (message "Resetting %s...Done" (buffer-name)))
      (setq blist (cdr blist)))))

(defun perlplus-complete-symbol ()
  "Complete perl symbol dynamically."
  (interactive)
  (let ((p (point)) b abr word listfunc ps w (ww (window-width)) (len 0))
    (save-excursion
      (or (string-match
	   "[A-Z_0-9':]" (char-to-string (char-after (1- (point)))))
	  (error "Cannot complete here!"))
      ;;(skip-chars-backward "^ \"(\t\n")
      (skip-chars-backward "A-Za-z_0-9\':")
      (setq abr (buffer-substring (point) p))
      (cond
       ((and (not (bolp)) (= (char-after (1- (point))) ?&))	;function
	(setq b (point)
	      listfunc 'perlplus-matching-functions
	      word (perlplus-function-completion abr)
	      ps perlplus-function-open))
       ((and (not (bolp))					;variable
	     (or
	      (string-match
	       "[\$%@]" (char-to-string (char-after (1- (point)))))
	      (save-excursion
		(forward-char -2)
		(and (looking-at "[\$%@]{") (setq ps "}")))))
	(setq b (point)
	      listfunc 'perlplus-matching-variables
	      word (perlplus-variable-completion abr)))
       ((looking-at "[A-Za-z_]")				;keyword
	(setq b (point))
	(setq listfunc
	      '(lambda (x)
		 (perlplus-match-list x perlplus-builtin-function-alist))
	      word (try-completion abr perlplus-builtin-function-alist)
	      ps perlplus-function-open))
       (t (setq word 1))))
    (cond
     ((stringp word)
      (if (string= word abr)
	  (let ((cand "") (list (funcall listfunc abr)))
	    (while list
	      (setq w (car (car list)))
	      (if (> (+ len (length w) 2) ww)
		  (setq w (concat "\n" w "  ") len (+ (length w) 2))
		(setq w (concat w "  ") len (+ len (+ (length w) 2))))
	      (setq cand (concat cand w)
		    list (cdr list)))
	    (perlplus-flash-string
	     (format "== Matches with %s\n%s\n%s"
		     (make-string (- (window-width) 16) ?=) cand
		     (make-string (1- (window-width)) ?=))))
	(delete-region b p)
	(insert word)
	(setq abr word)
	(setq word (try-completion word (funcall listfunc abr)))
	(if (eq t word)
	    (if (eolp)
		(cond
		 ((assoc abr perlplus-control-structure-alist)
		  (insert " ("))
		 (ps (insert ps)))))
	))
     ((null word)
      (error "No matches found for `%s'" abr))
     ((eq word t)
      (message "Sole completion")
      (cond
       ((assoc abr perlplus-control-structure-alist)
	(insert " ("))
       (ps (insert ps)))))))

(defun perlplus-setup ()
  (make-local-variable 'perlplus-external-variables)
  (make-local-variable 'perlplus-external-functions)
  (make-local-variable 'perlplus-inc-path-alist)
  (make-local-variable 'perlplus-perl5)
  (let ((symlist (perlplus-collect-imported-symbols))
	(st (syntax-table)))
    (set-syntax-table (standard-syntax-table))
    (unwind-protect
	(setq perlplus-external-variables (car symlist)
	      perlplus-external-functions (car (cdr symlist)))
      (set-syntax-table st)))
  )
(provide 'perlplus)

;;; Local variables:
;;; fill-prefix: ";;; "
;;; End:
