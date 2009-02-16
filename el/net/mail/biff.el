;;; -*- Emacs-Lisp -*-
;;; (Hope to be) All-mighty biff program in Emacs-Lisp
;;; (c)2000, 2003 by HIROSE Yuuji [yuuji@gentei.org]
;;; $Id: biff.el,v 1.4 2003/11/14 15:56:28 yuuji Exp $
;;; Last modified Sat Nov 15 00:52:58 2003 on firestorm
;;; Update count: 224

;;[Commentary]
;;
;;	This  package provides  mail  arrival check,  so called  `biff',
;;	feature via POP3(and APOP),  IMAP, mbox, maildir.  Both one-time
;;	mode and repeat-in-background mode are available.
;;	
;;	mbox, maildir, POP3(APOP), IMAP4 全対応の `biff' プログラム。
;;
;;[Requirements]
;;	
;;	This program uses Gareth Rees's  `md5.el' package if you want to
;;	use APOP, and Kyle E. Jones's `base64.el' package if you want to
;;	use  IMAP/AUTH.  Maybe  you  have already  gotten  it with  this
;;	package.  If  not yet, it can  be found in  w3-mode package.  Or
;;	you'll be find it in the following URL;
;;	
;;	http://www.gentei.org/~yuuji/software/biff-el.html
;;	
;;	APOP でのチェックをするには md5.el が、IMAP/AUTHでのチェックをす
;;	るには base64.el がそれぞれ必要です。見付からない場合は上記のURL
;;	の分かる場所に置いときます。
;;	
;;[Installation]
;;
;;	Make sure  biff.el and  md5.el are in  a directory  which is
;;	contained in load-path.  Then  put the lisp expressions below in
;;	your ~/.emacs,  where `ACCOUNT' is a nickname  of associated POP
;;	account, `SERVER' is  its POP3 server host name,  `USER' is user
;;	name on  the server, `PROTO'  is protocol; 'pop or  'apop, which
;;	defaults to pop.
;;	
;;		(load "biff")
;;		(setq biff-account-alist
;;		  '(("ACCOUNT"
;;		       (server . "SERVER") (user . "USER") (proto . apop))
;;		    ("ACCOUNT2"
;;		       (server . "SERVER2") (user . "USER2") (proto . pop))))
;;		(biff-background-all) ;(if you want to check all server)
;;	
;;	biff.el, md5.el を load-path の通ったディレクトリに置いて上
;;	記のEmacs-Lisp式を ~/.emacs に追加します。
;;	
;;[Run]
;;
;;	The first time you use this, or to create a new mail account,
;;	type simply;
;;	
;;		M-x biff
;;	
;;	It asks you account name(any string) and some server
;;	information.  Once you answer to all the question, the account
;;	name will be choosable in the further `M-x biff' command.  You
;;	can register any number of accounts. You may want to save your
;;	account information in your ~/.emacs.  Then, type
;;	
;;		M-x biff-save-accounts
;;	
;;	to save all account information.
;;	The following commands are useful.
;;	
;;	* M-x biff-background
;;	  Check mail on one account
;;	* M-x biff-background-list
;;	  Check mail on specified(maybe multiple) accounts
;;	* M-x biff-background-all
;;	  Check mail on all accounts
;;	* M-x biff-cancel-background
;;	  Stop background-check
;;	
;;	とりあえず一発目は M-x biff とすると、アカウント名を聞いて来るの
;;	で調べたいサーバを表す適当な名前を入れ、その後に聞いて来るサーバ
;;	情報に答えます。全てに答えたら次回の M-x biff で利用できます。ア
;;	カウントは何個でも登録でき、もしそれらを ~/.emacs にセーブしたい
;;	ときは
;;
;;		M-x biff-save-accounts
;;
;;	します。その他良く利用しそうなコマンドは以下の通りです。
;;
;;	* M-x biff-background
;;	  一つのアカウントをバックグラウンドで繰り返しチェック
;;	* M-x biff-background-list
;;	  任意個のアカウントをバックグラウンドで繰り返しチェック
;;	* M-x biff-background-all
;;	  登録してある全てのアカウントをバックグラウンドで繰り返しチェック
;;	* M-x biff-cancel-background
;;	  バックグラウンドでのチェックをキャンセル
;;
;;[Configuration]
;;
;;	Here are the configuration variables.
;;
;;	[Variable]			[Default value / Meaning]
;;	biff-pop-port			110 / Port number of POP3 service
;;	biff-imap-port			143 / Port number of IMAP service
;;	biff-account-alist		nil / See above
;;	biff-check-interval		600 / Mail check interval in second
;;	biff-use-beep			t   / Beep when some mails arrived
;;	biff-use-beep-visible		nil / When use-beep, need visible-bell?
;;
;;	上記の変数によってカスタマイズ可能です。
;;
;;[Bugs]
;;
;;	When  your mails are  arrived on  multiple servers,  because the
;;	arrival notifications except the last one immediately disappear,
;;	you cannot take your eyes off :)
;;
;;	And more...
;;
;;	複数のアカウントに新着メイルがあるときは最後の通知以外はすぐに見
;;	えなくなるのでよそ見ができません。
;;
;;	その他バグ出しはまだまだ…
;;
;;[Acknowledgements]
;;
;;	Thanks to the people who sent me patches and/or advices as bellow;
;;	
;;	* Daiji KANEMATSU <kanematu@sra.co.jp>
;;		Patch for ding
;;	* Jun'ichiro KITA <kita@coe.nttdata.co.jp>
;;		Patch for XEmacs (using itimer package)
;;	* MIHIRA Sanpei Yoshiro <sanpei@sanpei.org>
;;		Reported CR/CR/LF problem on Meadow
;;
;;[No Warranty]
;;
;;	This  program is  free  software and  comes  with absolutely  NO
;;	WARRANTY.   The  author  is  not responsible  for  any  possible
;;	defects  caused by this  software.  You  can freely  modify this
;;	program  for  your convenience.   But  if  you  want to  publish
;;	modified program,  please tell me before  announcement.  Take it
;;	easy to write me comments, bug-reports.
;;							yuuji@gentei.org
;;
;;	このプログラムはフリーソフトウェアとして配布します。このプログラ
;;	ムの利用によって生じたいかなる結果に対しても作者は責任を負いませ
;;	ん。コメントやバグレポートはおおいに歓迎しますので御気軽に御連絡
;;	ください。またプログラムに対する個人的な修正は自由にして頂いて構
;;	いませんが、それを公開したい場合は私まで御連絡ください。連絡は以
;;	下のアドレスまでお願いします(2003/11現在)。
;;							yuuji@gentei.org

(if (featurep 'xemacs)
    (require 'itimer)
  (require 'timer))

(defvar biff-j (or (featurep 'mule) (boundp 'mule) (boundp 'nemacs))
  "Japanese")

(defvar biff-pop-port	110 "POP3のポート番号")
(defvar biff-imap-port	143 "IMAPのポート番号")
(defvar biff-account-alist nil "アカウント情報を持ったalist
'((アカウント名 (server . サーバ) (user . ユーザ名) (proto . プロトコル))
   …)
「プロトコル」は pop, apop, imap, imap/auth, mbox, maildir の
どれか(いずれもシンボル)")
(defvar biff-check-interval 600
  "バックグラウンドモードの場合の新着チェックする間隔")
(defvar biff-mbox-check-threshold 100000
  "mbox形式のときの到達数をチェックする上限バイト数")
(defvar biff-autoraise t "ミニバッファを表に出すか")
(defvar biff-use-modeline t "モードラインに出すか")
(defvar biff-use-beep t "Beep音を鳴らすか")
(defvar biff-use-beep-visible nil "Beep音を視覚ベルにするか")

;;;
; Variables for Internal Work
(defvar biff-process nil "POP3接続を保持するプロセス")
(defvar biff-buffer " *biff*" "POP3接続を保持するバッファ")
(defvar biff-user nil "ユーザ名")
(defvar biff-pass1 nil "今打ったパスワード")

(defvar biff-current-account nil "現在のアカウント名")
(defvar biff-current-user nil "現在の接続のユーザ名")
(defvar biff-current-proto nil "現在の接続のプロトコル")
(defvar biff-current-status nil "現在の状態
'start	開始直後
'user	ユーザ送信前
'pass	パスワード送信前
'cmd	コマンド入力待ち
'stat	stat結果待ち")
(defvar biff-check-queue nil "キュー")
(defvar biff-modeline-string nil)
(defvar biff-modeline-arrival-list nil)
(defvar biff-debug nil)
(defvar biff-debug-buffer "*biff-debug*")

(if biff-use-modeline
    (progn
      (or global-mode-string (setq global-mode-string '("")))
      (or (memq 'biff-modeline-string global-mode-string)
	  (setq global-mode-string
		(append global-mode-string '(biff-modeline-string))))))

(defun biff-debug-output (string)
  (let ((cb (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer (get-buffer-create biff-debug-buffer))
	  (goto-char (point-max))
	  (insert
	   (format "%s.%06d %s\n"
		   (substring (current-time-string)  11 19)
		   (nth 2 (current-time)) string)))
      (set-buffer cb))))
   
(defun biff-send-command (mess)
  "POP3サーバにコマンドを送信する(行末をCRLFで)"
  (if (eq (process-status biff-process) 'open) ;ソケットが開いているなら
      (let ((cr (if (memq system-type '(ms-dos windows-nt OS/2)) "" "\r")))
	(process-send-string biff-process (format "%s%s\n" cr mess))
	(if biff-debug (biff-debug-output (concat "SEND: " mess))))))

(defun biff-notify (num &optional size)
  (if (stringp num) (setq num (string-to-int num)))
  (if (stringp size) (setq size (string-to-int size)))
  (cond
   ((and window-system (fboundp 'make-frame) nil biff-use-frame)
    )
   (t
    (cond
     ((and (stringp (current-message))
	   (string-equal (current-message) (get 'biff-notify 'lastmsg)))
      ;;(message "さっきのメイルは読んじゃったみたい")
      )
     ((> num 0)
      (if biff-use-modeline
	  (setq biff-modeline-arrival-list
		(cons (cons biff-current-account num)
		      biff-modeline-arrival-list)))
      (put 'biff-notify 'lastmsg
	   (format (cond
		    (biff-j "%s に %d 通%sのメイルが届いています")
		    (t "%s: %d message(s) %s"))
		   biff-current-account
		   num
		   (if size (format " %d bytes" size) "")))
      (and biff-autoraise window-system (fboundp 'raise-frame)
	   (raise-frame (car (minibuffer-frame-list))))
      (biff-ding)
      (message (get 'biff-notify 'lastmsg))
      (if (featurep 'xemacs)
	  (sit-for 3 nil)
	(sit-for 3 nil t)))
     (t )))))

(defun biff-close-end ()
  "メッセージ出力後. 接続を閉じて終了"
  (if (and biff-process		;ソケットが生きていることの確認
	   (eq (process-status biff-process) 'open))
      (delete-process biff-process));closeは delete-process にて行う
  (setq biff-process nil		;変数をクリア
	biff-current-status nil
	biff-current-account nil
	biff-current-proto nil)
  (if biff-check-queue
      (let ((next (car biff-check-queue)))
	(setq biff-check-queue (cdr biff-check-queue))
	(biff next))
    ;;the end of list
    (if biff-modeline-arrival-list
	(setq biff-modeline-string
	      (concat
	       "("
	       (mapconcat
		  (function
		   (lambda (s) (concat (car s) ":"
				       (int-to-string (cdr s)))))
		  biff-modeline-arrival-list
		  ",")
	       ")"))
      ;(setq biff-modeline-string "(NoMail)")
      )
    (set-buffer-modified-p (buffer-modified-p))
    (sit-for 0)
    (unwind-protect
	(sit-for (/ biff-check-interval 2))
      (setq biff-modeline-string nil
	    biff-modeline-arrival-list nil)
      ;(message (current-message))
      (sit-for 0))
    ))

(defun biff-ding ()
  "Beep音を鳴らす"
  (if biff-use-beep
      (let ((visible-bell biff-use-beep-visible))
	(ding t))))

(defun biff-get-pass ()
  "カレントアカウント用のパスワード取得"
  (or (get 'biff-get-pass 'ary)
      (put 'biff-get-pass 'ary (make-vector 127 nil)))
  (let*((ar (get 'biff-get-pass 'ary))
	(f (function (lambda (s)
		       (if (boundp (intern s ar))
			   (symbol-value (intern s ar)))))))
    (if (and biff-process (eq (process-status biff-process) 'open))
	(or (funcall f biff-current-account)
	    (read-passwd (format "Password(%s): " biff-current-account)))
      (read-passwd "Password: "))))

(defun biff-set-pass (str &optional account)
  "カレントアカウント用のパスワード設定"
  (or (get 'biff-get-pass 'ary)
      (put 'biff-get-pass 'ary (make-vector 127 nil)))
  (let ((ac (or account biff-current-account)) (i 0))
    (if (eq				;eq is correct
	 (and (boundp (intern ac (get 'biff-get-pass 'ary)))
	      (symbol-value (intern ac (get 'biff-get-pass 'ary))))
	 str) nil
      (set (intern ac (get 'biff-get-pass 'ary))
	   (copy-sequence str))
      (and str (stringp str) (string< "" str)
	   (while (< i (length str))
	     (aset str i ?.)
	     (setq i (1+ i)))))))

(defun biff-warn (mesg)
  "Put warning message"
  (message "%s%s"
	   mesg
	   (if biff-current-account (format "(%s)" biff-current-account) "")))

(defvar biff-fail-count 0)

(defun biff-fail ()
  "Count failure and stop background check at 3 times failure."
  (if (> (setq biff-fail-count (1+ biff-fail-count)) 2)
      (biff-cancel-background))
  (biff-close-end))

(defun biff-filter (proc message)
  "POP3サーバからのメッセージを受け取る"
  (let ((stat biff-current-status))
    (if biff-debug
	(biff-debug-output
	 (format "RECV(%s): %s" biff-current-account message)))
    (cond
     ((null stat)
      ;(biff-close-end "")
      (and biff-process (eq (process-status biff-process) 'open)
	   (progn
	     (delete-process biff-process)
	     (setq biff-process nil))))
     ((eq stat 'start)			;開始直後なら
      (if (string-match "OK" message)
	  (cond
	   ((eq biff-current-proto 'apop)
	    (if (string-match "<.+>" message)
		(let ((clg (substring message
				      (match-beginning 0) (match-end 0))))
		  (require 'md5)
		  (setq biff-pass1 (biff-get-pass))
		  (biff-send-command
		   (format "APOP %s %s"
			     biff-current-user
			     (md5 (concat clg biff-pass1))))
		  (setq biff-current-status 'cmd))
	      (biff-send-command "quit")
	      (biff-warn
	       (cond
		(biff-j "サーバはAPOPに対応していません")
		(t "POP Server doesn't accept APOP")))
	      (biff-fail)))
	   ((eq biff-current-proto 'pop);;普通のPOP
	    (biff-send-command (concat "user " biff-current-user))
	    (setq biff-current-status 'pass))
	   ((eq biff-current-proto 'imap)
	    (setq biff-pass1 (biff-get-pass))
	    (biff-send-command (format "biff.el LOGIN %s %s"
				       biff-current-user biff-pass1))
	    (setq biff-current-status 'cmd))
	   ((eq biff-current-proto 'imap/auth)
	    (require 'base64)
	    (biff-send-command "biff.el AUTHENTICATE LOGIN")
	    (setq biff-current-status 'user)
	   ))
	(biff-warn
	 (cond
	  (biff-j "サーバへの接続に失敗しました")
	  (t "Could not establish connecton to server")))
	(biff-fail)))
     ((eq stat 'user)			;imap4/auth
      (if (and (string-match "^\\+ \\(\\S +\\)" message)
	       (string-match
		"User"
		(base64-decode
		 (substring message (match-beginning 1) (match-end 1)))))
	    (progn
	      (biff-send-command (base64-encode biff-current-user))
	      (setq biff-current-status 'pass))))
     ((eq stat 'pass)			;パスワード待ちなら
      (cond
       ((eq biff-current-proto 'imap/auth)
	(if (and (string-match "^\\+ \\(\\S +\\)" message)
		 (string-match
		  "Pass"
		  (base64-decode
		   (substring message (match-beginning 1) (match-end 1)))))
	    (progn
	      (setq biff-pass1 (biff-get-pass))
	      (biff-send-command (base64-encode biff-pass1))
	      (setq biff-current-status 'cmd))))
       (t
	(if (string-match "OK" message)
	    (progn
	      (setq biff-pass1 (biff-get-pass))
	      (biff-send-command
	       (concat "pass " biff-pass1))
	      (setq biff-current-status 'cmd))
	  (biff-warn (cond
		      (biff-j "ユーザ名が違います")
		      (t "Invalid login name")))
	  (biff-fail)))))
     ((eq stat 'cmd)			;コマンド待ちなら
      (cond
       ((memq biff-current-proto '(imap imap/auth))
	(if (string-match "OK" message)
	    (progn
	      (biff-send-command "biff.el SELECT inbox")
	      (setq biff-current-status 'stat))
	  (biff-warn (cond
		      (biff-j "パスワードが違います")
		      (t "Password incorrect")))
	  (biff-set-pass nil)
	  (biff-fail)))
       ((memq biff-current-proto '(pop apop))
	(if (string-match "^\\+OK" message)
	    (progn
	      (biff-send-command "stat")
	      (setq biff-current-status 'stat))
	  ;;else, some error occurs
	  (cond
	   ((string-match "lock" message)
	    (biff-warn
	     (cond (biff-j "他のPOPクライアントによってlockされています")
		   (t "POP locked by other pop-client"))))
	   (t
	    (biff-warn (cond (biff-j "パスワードが違います")
			     (t "Password incorrect")))
	    (biff-set-pass nil)))
	  (biff-fail)))))
     ((eq stat 'stat)			;stat結果待ちなら
      (set-process-filter biff-process nil) ;もうフィルタは不要
      (let*((unknown (cond (biff-j "不明") (t "unknown")))
	    (m message) (n "不明") (s ))
	(cond
	 ((memq biff-current-proto '(imap imap/auth))
	  (if (string-match "^\\* \\([0-9]+\\)\\s EXISTS" m)
	      (put 'biff-filter 'auth/exists
		   (string-to-int
		    (substring m (match-beginning 1) (match-end 1)))))
	  (if (string-match "^\\* \\([0-9]+\\)\\s RECENT" m)
	      (put 'biff-filter 'auth/recent
		   (string-to-int
		    (substring m (match-beginning 1) (match-end 1)))))
	  (if (and (get 'biff-filter 'auth/exists)
		   (get 'biff-filter 'auth/recent))
	      (progn
		(setq n (+ (get 'biff-filter 'auth/exists)
			   (get 'biff-filter 'auth/recent))
		      biff-current-status nil)
		(biff-send-command "biff.el LOGOUT")
		(biff-set-pass biff-pass1)
		(setq biff-pass1 nil)
		(biff-notify n)
		(put 'biff-filter 'auth/exists nil)
		(put 'biff-filter 'auth/recent nil)
		(biff-close-end)))
	  )
	 ((and (memq biff-current-proto '(pop apop))
	       (string-match "^\\+OK" m))
	  (cond
	   ((string-match "OK\\s \\([0-9]+\\) \\([0-9]+\\)" m);ipop3d
	    (setq n (substring m (match-beginning 1) (match-end 1))
		  s (substring m (match-beginning 2) (match-end 2))))
	   ((string-match "OK.*\\([0-9]+\\)\\s *message.*\\([0-9]+\\)" m)
	    ;;QPOP
	    (setq n (substring m (match-beginning 1) (match-end 1))
		  s (substring m (match-beginning 2) (match-end 2)))))
	  (biff-send-command "quit")
	  (setq biff-current-status nil)
	  (biff-set-pass biff-pass1)
	  (setq biff-pass1 nil)
	  (biff-notify n s)
	  (biff-close-end))
	 (t (biff-warn (cond (biff-j "STATできませんでした")
			     (t "Couldn't STAT")))
	    (biff-fail)))))
     (t
      (biff-warn (cond (biff-j "良く分からないエラーが出ました.")
		       (t "Unexpedted error occured")))
      (biff-fail)))))

(defvar biff-maildir (or (getenv "MAILDIR") "~/maildir"))

(defun biff-check-maildir (dir)
  (let ((flist (append (directory-files (concat dir "/cur") 'fullpath)
		       (directory-files (concat dir "/new") 'fullpath)))
	(n 0) (s 0))
    (while flist
      (if (file-regular-p (car flist))
	  (setq n (1+ n)
		s (nth 7 (file-attributes (car flist)))))
      (setq flist (cdr flist)))
    (if (> n 0)
	(biff-notify n s))
    (biff-close-end)))

(defun biff-check-mbox (file)
  (let ((tb (get-buffer-create " *bifftmp* "))
	(n 0) (s 0) (case-fold-search nil))
    (save-excursion
      (set-buffer tb)
      (erase-buffer)
      (setq s (nth 7 (file-attributes file)))
      (if (> s biff-mbox-check-threshold)
	  (setq n "たくさん")
	(unwind-protect
	    (progn
	      (insert-file-contents file)
	      (while (re-search-forward "^From " nil t)
		(setq n (1+ n))))
	  (kill-buffer tb)))
      (if (> s 0)
	  (biff-notify n s)))
    (biff-close-end)))

; (defun biff (host user)
;   "HOST にPOP3接続して USER 宛のメイルがあるか調べる"
;   (interactive "sPOP3 server: \nsUser: ")
;   (setq biff-user user)
;   ;;最初はユーザ入力待ち状態
;   (setq biff-current-status 'start)
;   ;;ソケット接続プロセスの開始
;   (setq biff-process
; 	(open-network-stream "biff" biff-buffer host biff-port))
;   ;;接続
;   (set-process-filter biff-process 'biff-filter))
(defun biff (account)
  (interactive
   (list (completing-read "Account: " biff-account-alist)))
  (let* ((account-info (cdr-safe (assoc account biff-account-alist)))
	 (server (cdr-safe (assq 'server account-info)))
	 (user (cdr-safe (assq 'user account-info)))
	 (proto (cdr-safe (assq 'proto account-info))))
    (or proto (setq proto
		    (intern (completing-read
			     "Protocol: "
			     '(("pop") ("apop") ("imap") ("imap/auth")
			       ("maildir") ("mbox"))
			     nil t))))
    (or server
	(cond
	 ((eq proto 'maildir)
	  (while (not (file-directory-p
		       (setq server (read-file-name "Maildir: "))))))
	 ((eq proto 'mbox)
	  (setq server (read-file-name "Mbox: " "" nil t (getenv "MAIL"))))
	 (t
	  (setq server (read-string "Server Host Name: ")))))
    (or user
	(memq proto '(mbox maildir))
	(setq user (read-string "User Name: " (user-login-name))))
    (setq biff-account-alist
	  (delq (assoc account biff-account-alist) biff-account-alist)
	  biff-account-alist
	  (cons (cons account (list (cons 'server server)
				    (cons 'user user)
				    (cons 'proto proto)))
		biff-account-alist))
    (setq biff-current-account account
	  biff-current-status 'start
	  biff-current-user user
	  biff-current-proto proto)
    (cond
     ((memq proto '(pop apop imap imap/auth))
      (setq biff-process
	    (open-network-stream
	     "biff" biff-buffer server
	     (if (memq proto '(pop apop))
		       biff-pop-port
	       biff-imap-port)))
      (set-process-filter biff-process 'biff-filter))
     ((eq proto 'maildir)
      (biff-check-maildir server))
     ((eq proto 'mbox)
      (biff-check-mbox server)))))

(defun biff-do-list (account-list)
  "複数のアカウントを連続的にチェックする"
  (setq biff-check-queue (cdr account-list))
  (biff (car account-list)))

(defun biff-all ()
  "全部チェック"
  (interactive)
  (biff-do-list (mapcar 'car biff-account-alist)))

(defun biff-background-all ()
  "全てのリストのチェック"
  (interactive)
  (biff-cancel-background)
  (if (featurep 'itimer)
      (start-itimer "biff" 'biff-all 1 biff-check-interval)
    (run-with-timer 1 biff-check-interval 'biff-all)))

(defun biff-background (account)
  "一箇所のチェック"
  (interactive
   (list (completing-read "Account: " biff-account-alist)))
  (biff-cancel-background)
  (if (featurep 'itimer)
      (start-itimer "biff" 'biff 1 biff-check-interval nil t account)
    (run-with-timer 1 biff-check-interval 'biff account)))

(defun biff-background-list (list)
  "LISTで指定した箇所のチェック"
  (interactive
   (let (l s)
     (while (string< "" (setq s (completing-read
				 "Account (RET Only to quit): "
				 biff-account-alist
				 (function
				  (lambda (s)
				    (not (member (car s) l))))
				 t)))
       (setq l (cons s l)))
     (list l)))
  (biff-cancel-background)
  (if (featurep 'itimer)
      (start-itimer "biff" 'biff-do-list 1 biff-check-interval nil t list)
    (run-with-timer 1 biff-check-interval 'biff-do-list list)))

(defun biff-cancel-background ()
  "バックグラウンドチェックのキャンセル"
  (interactive)
  (setq biff-fail-count 0)
  (cond ((featurep 'itimer)
	 (let ((timer (get-itimer "biff")))
	   (and timer
		(delete-itimer timer))))
	(t
	 (if (string< "19.34" emacs-version)
	     (progn
	       (cancel-function-timers 'biff)
	       (cancel-function-timers 'biff-do-list)
	       (cancel-function-timers 'biff-all))))))

(defun biff-change-interval (secs)
  (interactive "nCheck Interval: ")
  (setq biff-check-interval secs)
  (cond ((featurep 'itimer)
	 (let ((timer (get-itimer "biff")))
	   (and timer
		(set-itimer-restart timer biff-check-interval))))
	(t
	 (let ((list timer-list))		;timer-list is in the timer.el
	   (while list
	     (if (memq (aref (car list) 5) '(biff biff-do-list biff-all))
		 (progn
		   (timer-set-idle-time (car list) 1 biff-check-interval)
		   (message "Running %s's interval reset to %d"
			    (aref (car list) 5) biff-check-interval)
		   (sit-for 1)))
	     (setq list (cdr list)))))))

(defun biff-save-accounts (arg)
  "現在のアカウントリストを ~/.emacs に書き出す.
Universal argument をつけると別のファイルに出す."
  (interactive "P")
  (let ((file (if arg (read-file-name "Account Output file: ") "~/.emacs"))
	(bal biff-account-alist))
    (find-file file)
    (goto-char (point-min))
    (if (search-forward "(setq biff-account-alist" nil t)
	(delete-region
	 (progn (goto-char (match-beginning 0)) (point))
	 (progn (forward-list 1) (point)))
      (goto-char (point-max))
      (or (= (current-column) 0) (insert ?\n)))
    (insert "(setq biff-account-alist\n  '(")
    (while bal
      (lisp-indent-line)
      (insert (format "%s\n" (prin1-to-string (car bal))))
      (setq bal (cdr bal)))
    (delete-backward-char 1)
    (insert "))\n")))

(eval-when-compile
  (load "md5" t)
  (load "base64" t)
  (if (featurep 'xemacs)
      (require 'itimer)
    (require 'timer)))

(provide 'biff)

; Local variables:
; fill-prefix: ";;	"
; paragraph-start: "^$\\|\\|;;$"
; paragraph-separate: "^$\\|\\|;;$"
; End:
