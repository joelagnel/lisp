;;; Author:        Alastair Burt (burt@dfki.de)
;;; File:          xmlrpc.el
;;; Purpose:       XMLRPC client
;;; Created:       Apr 26, 2001
;;; Version:       $Id: xmlrpc.el,v 1.2 2001/06/20 14:40:36 burt Exp burt $
;;; Entry Points:  

;;; Commentary

;;; Todo

;;; - Handle &lt; etc in stgrings.  

;;; The file xml-parse.el from John Wiegley can be obtained from
;;; http://www.gci-net.com/users/j/johnw/Emacs/xml-parse.el

(require 'xml-parse)

(defvar xmlrpc-use-i4-tag nil
  "*Whether to use 'i4' or 'int' as the XMLRPC tag for integers")

(defconst xmlrpc-buffer " *XMLRPC*")
(defconst xmlrpc-version "0.1")
(defconst xmlrpc-user-agent (concat "Emacs XMLRPC/" xmlrpc-version))

(defvar xmlrpc-no-depth nil)

(defun xmlrpc-to-xml-value (term)
  (list  "value" (xmlrpc-to-xml term)))

(defun xmlrpc-to-xml (term)
  (let ((func (intern-soft
	       (concat "xmlrpc-to-xml-" (symbol-name (type-of term))))))
    (if func
	(funcall func term)
      (error 'invalid-argument
	     "Cannot convert Elisp:" term))))

(defun xmlrpc-to-xml-integer (int)
  (list (if xmlrpc-use-i4-tag "i4" "int") (number-to-string int))) 

(defun xmlrpc-to-xml-string (str)
  (list "string" str))

(defun xmlrpc-to-xml-float (float)
  (list "double" (format "%2f" float)))

(defun xmlrpc-to-xml-symbol (sym)
  (when (eq sym nil)
    (list "array" (list "data"))))

(defun xmlrpc-to-xml-cons (list)
  (cond 
   ((xmlrpc-alistp list)
    (cons "struct" (mapcar 'xmlrpc-to-xml-alist list)))
   (t
    (list "array" (cons "data" (mapcar 'xmlrpc-to-xml-value list))))))

(defun xmlrpc-to-xml-alist (el)
  (list "member"
	(list "name" (symbol-name (car el)))
	(xmlrpc-to-xml-value (cdr el))))
	  
(defun xmlrpc-alistp (list)
  (every '(lambda (el)
	    (and (eq (type-of el) 'cons)
		 (eq (type-of (car el)) 'symbol)))
	 list))

(defun xmlrpc-to-xml-vector (vec)
  (cond
   ((xmlrpc-datetime-p vec)
    (list "dateTime.iso8601"
	  (format "%04d%02d%02dT%02d:%02d:%02d"
		  (xmlrpc-datetime-year vec)
		  (xmlrpc-datetime-month vec)
		  (xmlrpc-datetime-day vec)
		  (xmlrpc-datetime-hour vec)
		  (xmlrpc-datetime-minute vec)
		  (xmlrpc-datetime-second vec))))
   ((xmlrpc-base64-p vec)
    (list "base64" (xmlrpc-base64-data vec)))
   ((xmlrpc-boolean-p vec)
    (list "boolean" (number-to-string (xmlrpc-boolean-value vec))))
   (t
    (error 'invalid-argument "Cannot convert Elisp:" vec))))

(defstruct xmlrpc-datetime year month day hour minute second)

(defstruct xmlrpc-base64 data)

(defstruct xmlrpc-boolean value)

(defun xmlrpc-true (bool) (= (xmlrpc-boolean-value bool) 1))

(defun xmlrpc-false (bool) (= (xmlrpc-boolean-value bool) 0))

(defun xmlrpc-make-boolean (val)
  (if val
      (make-xmlrpc-boolean :value 1)
    (make-xmlrpc-boolean :value 0)))

(defun xmlrpc-make-call (method args)
  (let ((str "")
	(xml (list "methodCall" 
		   (list "methodName" method)
		   (cons "params" 
			 (mapcar 
			  '(lambda (arg)
			     (list "param" (xmlrpc-to-xml-value arg)))
			  args))))
	(xmlrpc-no-depth t))
    (with-string-as-buffer-contents str 
      (insert "<?xml version=\"1.0\"?>\n")
      (insert-xml xml))))

(defadvice insert-xml (around  xmlrpc-no-depth activate)
  "A hack to stop the indenting of XML.
Some XMLRPC servers object to whitespace between tags."
  (cond 
   (xmlrpc-no-depth
    (setq depth nil)
    ad-do-it)
   (t ad-do-it)))
    
(defstruct xmlrpc-server host (port "80") (url "/RPC2") (user nil)
  (password nil))

(defun xmlrpc-method (server method &rest args)
  (let* ((url-working-buffer (get-buffer-create xmlrpc-buffer))
	 (url-request-method "POST")
	 (url-request-data (xmlrpc-make-call method args))
	 (url-package-name "Emacs XMLRPC")
	 (url-package-version "0.1")
	 (url-request-extra-headers
	  (list (cons "Content-Type" "text/xml"))))
    (when (and (xmlrpc-server-user server)
	       (xmlrpc-server-password server))
      (push (cons "Authorization"
		  (concat (xmlrpc-server-password server)))
	    url-request-extra-headers))
    (save-excursion
      (xmlrpc-handle-retrieve
       (url-retrieve 
	(concat "http://"
		(xmlrpc-server-host server)
		":"
		(xmlrpc-server-port server)
		(xmlrpc-server-url server)) 'no-cache)))))

(define-error 'xmlrpc-error "Remote procedure returned error")

(defun xmlrpc-handle-retrieve (response)
  (let ((xml))
    (save-excursion
      (set-buffer (cdr response))
      (setq xml (read-xml))
      (condition-case nil
	(progn
	  (let* ((methresp-name (first xml)) 
		 (resptype (second xml))
		 (resptype-name (car resptype))
		 (resptype-value (cdr resptype))
		 (rlist nil)
		 (params)
		 (param))
	    (unless (equal methresp-name "methodResponse")
	      (error 'invalid-argument))
	    (cond 
	     ((equal resptype-name "params")
	      (setq params resptype-value)
	      (while params
		(setq param (pop params))
		(unless (equal (first param) "param")
		  (error 'invalid-argument))
		(push (xmlrpc-to-elisp (second param)) rlist))
	      (if (= (length rlist) 1)
		  (car rlist)
		rlist))
	     ((equal resptype-name "fault")
	      (error 'xmlrpc-error (xmlrpc-to-elisp (car resptype-value))))
	     (t
	      (error 'invalid-argument)))))
      (invalid-argument
       (error 'invalid-argument "Cannot parse response." (buffer-string)))))))

(defun xmlrpc-to-elisp (val)
  (let (p pname func pval)
    (unless (equal (first val) "value")
      (error 'invalid-argument))
    (setq p (second val))
    (cond 
     ((consp p)
      (setq pname (car p))
      (setq pval (cdr p)))
     (t
      (setq pname "string")
      (setq pval p)))
    (setq func (intern-soft
		(concat "xmlrpc-to-elisp-" pname)))
    (if func
	(if (= (length pval) 1)
	    (funcall func (car pval))
	  (funcall func pval))
      (error 'invalid-argument))))
     
(defalias 'xmlrpc-to-elisp-int 'xmlrpc-to-elisp-i4)

(defun xmlrpc-to-elisp-i4 (p)
  (string-to-number p))

(defun xmlrpc-to-elisp-string (p)
  p)

(defun xmlrpc-to-elisp-boolean (p)
  (make-xmlrpc-boolean :value (string-to-number p)))

(defun xmlrpc-to-elisp-double (p)
  (float (string-to-number p)))

(defconst xmlrpc-datetime-regexp 
  (concat "\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)" 
	  "T"
	  "\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)"))

(defun xmlrpc-to-elisp-dateTime.iso8601 (dt)
  (string-match xmlrpc-datetime-regexp dt)
  (make-xmlrpc-datetime 
   :year (string-to-number (match-string 1 dt))
   :month (string-to-number (match-string 2 dt))
   :day (string-to-number (match-string 3 dt))
   :hour (string-to-number (match-string 4 dt))
   :minute (string-to-number (match-string 5 dt))
   :second(string-to-number (match-string 6 dt))))

(defun xmlrpc-to-elisp-base64 (p)
  (make-xmlrpc-base64 :data p))

(defun xmlrpc-to-elisp-array (d)
  (unless (equal (car d) "data")
    (error 'invalid-argument))
  (mapcar 'xmlrpc-to-elisp (cdr d)))

(defun xmlrpc-to-elisp-struct (s)
  (let (el mem key val (alist nil))
    (while s
      (setq el (pop s)
	    mem (first el)
	    key (second el)
	    val (third el))
      (unless (equal mem "member")
	(error 'invalid-argument))
      (unless (equal (first key) "name")
	(error 'invalid-argument))
      (push (cons (intern (second key))
		  (xmlrpc-to-elisp val)) alist))
    alist))

(provide 'xmlrpc)

