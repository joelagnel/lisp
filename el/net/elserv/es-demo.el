;;; es-demo.el -- Elserv demo.

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

;;; History:
;;

;;; Code

(require 'elserv)
(require 'calendar)
(eval-and-compile
  (require 'cl)
  (ignore-errors (require 'w3m)))

;; calendar (original was in the httpd.el)
(defun elserv-demo-calendar (result path ppath request)
  (let (year month prev-year prev-month next-year next-month)
    (if (string-match "^/\\([0-9]+\\)/\\([0-9]+\\)" path)
	(progn
	  (setq year (string-to-number (match-string 1 path)))
	  (setq month (string-to-number (match-string 2 path))))
      (setq year (string-to-number (format-time-string "%Y")))
      (setq month (string-to-number (format-time-string "%m"))))
    (if (eq month 1)
	(progn
	  (setq prev-year (- year 1))
	  (setq prev-month 12))
      (setq prev-year year)
      (setq prev-month (- month 1)))
    (if (eq month 12)
	(progn
	  (setq next-year (+ year 1))
	  (setq next-month 1))
      (setq next-year year)
      (setq next-month (+ month 1)))
    (elserv-set-result-code result 200)
    (elserv-set-result-header result '(content-type "text/html"))
    (elserv-set-result-body
     result
     (concat 
      "<html><head><title>Emacs calendar</title></head>\n"
      "<body> <h1>Calendar for "
      (number-to-string year)
      "/"
      (number-to-string month)
      "</h1>\n"
      "<a href=\"/calendar/"
      (number-to-string prev-year) "/"
      (number-to-string prev-month) "\">prev</a>\n "
      "<a href=\"/calendar/"
      (number-to-string next-year) "/"
      (number-to-string next-month) "\">next</a> "
      "<pre>\n"
      (with-temp-buffer
	(apply 'generate-calendar (list month year))
	(buffer-string))
      "</pre>\n</body></html>\n"))))

;; list-buffers
(defun elserv-demo-buffers (result path ppath request)
  (let ((buf (and (not (string= path ""))
		  (get-buffer (substring path 1)))))
    (if buf
	(progn
	  (elserv-set-result-header result '(content-type "text/plain"))
	  (elserv-set-result-body
	   result
	   (with-current-buffer buf
	     (encode-coding-string (buffer-string) 'iso-2022-jp))))
      (elserv-set-result-header result '(content-type "text/html"))
      (elserv-set-result-body
       result
       (concat
	"<ul>\n"
	(mapconcat
	 (function
	  (lambda (buf)
	    (let ((name (buffer-name buf)))
	      (unless (string= (substring name 0 1) " ")
		(concat "<li><a href=\""
			ppath
			"/" 
			(mapconcat 'identity (split-string
					      name " ") "+")
			"\">"
			name "</a>\n")))))
	 (buffer-list)
	 "")
	"</ul>\n")))))

;; POST (apropos)
(defun elserv-demo-post-apropos (result path ppath request)
  (elserv-set-result-header result '(content-type "text/plain"))
  (elserv-set-result-body
   result
   (save-window-excursion
     (if (apropos (nth 1 (split-string (plist-get request 'body) "=")))
	 (with-current-buffer (get-buffer "*Apropos*")
	   (buffer-string))
       (concat "No apropos matching for `"
	       (nth 1 (split-string (plist-get request 'body) "=")) "'")))))

;; A counter
(defvar elserv-counter-file-base "/tmp/elserv-counter")
(defun elserv-counter (name)
  "Return count for NAME."
  (let ((file (concat elserv-counter-file-base "-" name)))
    (elserv-save file (+ (or (elserv-load file) 0) 1))))

(defun elserv-demo-counter (result path ppath request)
  (elserv-set-result-header result '(content-type "text/plain"))
  (elserv-set-result-body result (format "You are %sth user."
					 (elserv-counter "demo"))))

;; POST (upload file)
(defun elserv-demo-upload (result path ppath request)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "Content-Type: " (plist-get request 'content-type) "\r\n\r\n"
	    (plist-get request 'body))
    (elserv-set-result-header result '(content-type "text/plain"))
    (elserv-set-result-body
     result (buffer-string))))

;; antenna (requires w3m)
(defun elserv-demo-antenna (result path ppath request)
  (elserv-set-result-header result
			    '(content-type "text/html; charset=iso-2022-jp"))
  (elserv-set-result-body
   result
   (encode-coding-string
    (save-window-excursion
      (with-temp-buffer
	(w3m-antenna)
	(w3m-with-work-buffer
	  (buffer-string))))
    'iso-2022-jp)))

;; history (requires w3m)
(defun elserv-demo-history (result path ppath request)
  (elserv-set-result-header result
			    '(content-type "text/html; charset=iso-2022-jp"))
  (elserv-set-result-body
   result
   (encode-coding-string
    (save-window-excursion
      (with-temp-buffer
	(w3m-db-history)
	(w3m-with-work-buffer
	  (buffer-string))))
    'iso-2022-jp)))

;; weather (requires w3m)
(defun elserv-demo-weather (result path ppath request)
  (elserv-set-result-header result
			    '(content-type "text/html; charset=iso-2022-jp"))
  (elserv-set-result-body
   result  
   (encode-coding-string
    (save-window-excursion
      (with-temp-buffer
	(call-interactively 'w3m-weather)
	(w3m-with-work-buffer
	  (buffer-string))))
    'iso-2022-jp)))

;; describe-function
(defun elserv-demo-describe-function (result path ppath request)
  (elserv-set-result-header result
			    '(content-type "text/plain"))
  (elserv-set-result-body
   result  
   (concat (save-window-excursion
	     (prog1 (describe-function (intern-soft (substring path 1)))
	       (message ""))))))

(defun elserv-demo-publish (process path)
  "Publish DEMO service.
PROCESS is the elserv server process.
PATH is the path to publish DEMO content."
  (elserv-publish process
		  path
		  :string
		  (encode-coding-string
		   (concat
		    "<html><head><title>Elserv</title></head>\
<body bgcolor=\"white\"> <img src=\"" path "/logo.gif\"><h1> Elserv: Yet another HTTP server on Emacsen </h1>\
もしこのページが読めたのであれば、Elserv ウェブサーバのインストールがこの計算機で無事に終了したことを意味します。あなたは、関数 <a href=\"/function/elserv-publish\">elserv-publish</a> によって文書を
加えたり、このページを置きかえることができます。
<h2>デモ</h2><a href=\"" path "/calendar\">カレンダー</a> ... calendar を表示します。<br>
<a href=\"" path "/buffers\">バッファ一覧</a> ... 現在 Emacs 上にあるバッファの一覧を表示します。<br>
<a href=\"" path "/buffers-local\">バッファ一覧</a> ... 同上(ただし localhost 以外は拒否。)<br>
<a href=\""path "/auth.txt\">認証テスト</a> ... 認証のテスト<br>
<a href=\"" path "/apropos.html\">キーワードをしらべる</a> ... POST を使う例です。<br>
<a href=\"" path "/upload.html\">ファイルをアップロードする</a> ... POST を使う例、その 2。<br>
<a href=\"" path "/counter\">カウンタ</a> ... いわゆるカウンタ。<br>
<a href=\"a\">あんてな(要 w3m)</a> ... <a href=\"http://namazu.org/~tsuchiya/emacs-w3m\">emacs-w3m </a> のアンテナ機能を中継します。<br>
<a href=\"" path "/h\">ヒストリ(要 w3m)</a> ... 同じく emacs-w3m のDBヒストリを中継します。<br>
<a href=\"" path "/w\">天気予報(要 w3m)</a> ... 同じく emacs-w3m の天気予報を中継します。<br>
<hr> Powered by <a href=\"http://www.gohome.org/elserv\">" (elserv-version) "</a>
</body></html>")
		   'iso-2022-jp)
		  :content-type "text/html; charset=ISO-2022-JP"
		  :description "Elserv demonstration.")
  (elserv-publish (elserv-find-process)
		  (concat path "/calendar")
		  :function 'elserv-demo-calendar)
  (elserv-publish (elserv-find-process)
		  (concat path "/buffers")
		  :function 'elserv-demo-buffers)
  (elserv-publish (elserv-find-process)
		  (concat path "/buffers-local")
		  :function 'elserv-demo-buffers
		  :allow '("localhost"))
  (elserv-publish (elserv-find-process)
		  (concat path "/auth.txt")
		  :string "Hello World."
		  :content-type "text/plain"
		  :authenticate '(:realm "HelloWorld"
					 :users (("foo" . "bar")
						 ("hoge" . "fuga"))))
  (elserv-publish (elserv-find-process)
		  (concat path "/apropos.html")
		  :string
		  (encode-coding-string
		   (concat
		    "<html><head><title>describe-function</title></head>
<body bgcolor=\"white\">
<H1>Emacs のキーワードをしらべる (apropos)</H1>
<form action=\"" path "/apropos\" method=\"POST\">
<input type=\"text\" name=\"value\"><br>
<input type=\"submit\" value=\"しらべる\">
<input type=\"reset\" value=\"クリア\">
</form></html>") 'iso-2022-jp)
		  :content-type "text/html; charset=ISO-2022-JP")
  (elserv-publish (elserv-find-process)
		  (concat path "/function")
		  :function 'elserv-demo-describe-function)
  (elserv-publish (elserv-find-process)
		  (concat path "/apropos")
		  :function 'elserv-demo-post-apropos)
  (elserv-publish (elserv-find-process)
		  (concat path "/a")
		  :function 'elserv-demo-antenna)
  (elserv-publish (elserv-find-process)
		  (concat path "/h")
		  :function 'elserv-demo-history)
  (elserv-publish (elserv-find-process)
		  (concat path "/w")
		  :function 'elserv-demo-weather)
  (elserv-publish (elserv-find-process)
		  (concat path "/counter")
		  :function 'elserv-demo-counter)
  (elserv-publish (elserv-find-process)
		  (concat path "/upload")
		  :function 'elserv-demo-upload)
  (elserv-publish (elserv-find-process)
		  (concat path "/upload.html")
		  :string (encode-coding-string
			   (concat "<html><body>ファイルのアップロードじゃ<FORM ENCTYPE=\"multipart/form-data\" ACTION=\"" path "/upload\" METHOD=\"POST\">
<INPUT TYPE=\"file\" NAME=\"upload\"><br>
<input type=\"submit\" value=\"送る\">
</FORM></body></html>") 'iso-2022-jp)
		  :content-type "text/html; charset=iso-2022-jp")
  (elserv-publish (elserv-find-process)
		  (concat path "/data")
		  :directory "/usr/local/www/data")
  (elserv-publish (elserv-find-process) 
		  (concat path "/logo.gif")
		  :string
		  (base64-decode-string
"R0lGODdhvQBEAPcAAAAAAICAgEBEQLjA0FhgaFhgoEBEgHiAuNjg6CAgIKiwuDAwMFBQUH
BwcMjQ2GhwoJCQkJig0FhggFBUiDhAYKiw2CgwWLjA4CAgQGhwwOjw+EhQaNDY4IiQ2HiA
qKCosFBUoEBIYGhwiFhkuLC4yGBoiAgQGICIsCAoMDg8OODo8HB4qGhoaJiYmMDI4FBYcM
DIyDA4YICIoBAYKJCYwKCoyFhYgJigwGBoqEBMiMjQ4LC44NDY6EBIeEBIWKiwwJCY0HB4
kODg4DAwUHB4gFhgkDhAeCAoSKCouICI0MjI4DA4aIiIiGBgcGBkqBggOGhwuEhUkHB4yF
BQaFBcqLi4uGBomCgoKKCo4EhMkMDAwKCgoHiAyPj8+ICAoGBowBgYGDA4UHB4sJiYsDhA
WNjY2EhIYOjo6GhseFhYaMjIyDg4YIiIoBgcMNDQ4Li40JicyHh4kODk8CgsUMjM6Li8yC
gsOMDE0KCksICEkFhgsLC0sDAwQAgMCICEsJCUuKCk0LC00MDE2JCUyHB0kIiMuEhISGhs
uFhcqGBkYNDU0HB0oGBkiEBEaDA0YPD08KisqGhsgHh8qBgYMKis0KCkwEhMiNjY8EhMeL
C0yHh8eJCQoGBkkEBEeCgsSKiswIiM0Dg8cCAgOFhcWHB0uFBUmGhskKis2ICE0Dg8UHh8
sEBEWFBQYHBwgGBkuJicqFhckHB40FBUeICEqJicwOjs+FhceLi84Jic2Hh8mLC04MDA6H
B0mBAUIIiMwNjg8IiQwEhQeMDI6JCY2GBksAgMGKCk2GhsyFhcsICEgFhkqEBIiHiEuCAk
IDA0MFBUUHB0cJCUkFBYgCAkQGh0yFBYoGhsaJicmFhcgGBssODk4DA0UICMyMjM4IiMiG
BkeGh0sHB8yFBUcLi8uCgsKEhQkMDEwKCkoICEmGBsyBgcGHB8uJicsNjc2EhMaOjs6Fhc
cMjMyDg8WIiMoNDU6Li82Hh8iAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAACwAAAAAvQ
BEAAAI/gC7CBxIsKDBgwgTKlzIsKHDhxAjSpxIsaLFixgzatzIsaPHjyBDinT4qJacS3IG
PlrJcqTLlzA1PlJx6VKtRwRr8ri08xJOnDGDCh1K8FFNmwJ3YRnkClRPXboqQFXy7t1Kol
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")
		  :content-type "image/gif"))

(defun elserv-demo-start (&optional port)
  "Start a demo server."
  (interactive (if current-prefix-arg
		   (list (string-to-number (read-from-minibuffer "Port: ")))))
  (elserv-demo-publish (elserv-start port) "/"))

(require 'product)
(product-provide (provide 'es-demo) (require 'elserv))

;;; es-demo.el ends here
