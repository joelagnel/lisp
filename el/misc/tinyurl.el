;; The following two functions grab the URL at point, feed it to tinyurl.com or metamark.net and retrieve a smaller URL redirection, which is placed in the kill-ring to conveniently yank it where you need it.

;; Since the two versions at the bottom of this page didn't work for me and my GNU Emacs 22, I refactored a bit and these two functions work quite good. If the two older versions below are broken on older emacsen, too, feel free to remove them. (TassiloHorn)

;; MarkAHershberger improved the cruft catching and tested get-metamark in emacs22 and emacs21.   


;;;;;;;;;;;;;
;; Tinyurl ;;
;;;;;;;;;;;;;
(require 'mm-url)
(defun get-tinyurl ()
"Grabs the url at point and echos the equivalent tinyurl in the
minibuffer to ease cutting and pasting."
  (interactive)
  (let* ((long-url (thing-at-point 'url))
         (tinyurl
          (save-excursion
            (with-temp-buffer
              (mm-url-insert
               (concat "http://tinyurl.com/api-create.php?url=" long-url))
              (kill-ring-save (point-min) (point-max))
              (buffer-string)))))
    (message tinyurl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metamark - Free Short URL redirection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'mm-url)
(defun get-metamark ()
  "Grabs the url at point and echos the equivalent metamark url
in the minibuffer to ease cutting and pasting."
  (interactive)
  (let* ((long-url (or (thing-at-point 'url)
                       (read-string "URL to shorten: ")))
         (url (concat "http://metamark.net/api/rest/simple?"
                      (mm-url-encode-www-form-urlencoded
                       (list (cons "long_url" long-url)))))
         (short-url
          (save-excursion
            (with-temp-buffer
              (mm-url-insert url)
              (goto-char (point-max))
              (goto-char (re-search-backward "[^[:cntrl:][:space:]]"))
              (delete-region (+ 1 (point)) (point-max))
              (kill-ring-save (point-min) (point-max))
              (buffer-string)))))
    (message "shortened: %s" short-url)))
</pre>

----

Tinyurl.el grabs the url at point, feeds it through tinyurl.com and spits the result out in the minibuffer.

If you use this with emacs running in a GUI you may want to extend the code to make the tinyurl yank-able.

(seems broken on GNU Emacs 22, so use the two functions on top.)

<pre>
(require 'url)                                                                  
                                                                                
(defun get-tinyurl ()                                                           
"Grabs the url at point and echos the equivalent tinyurl in the                 
minibuffer to ease cutting and pasting."                                        
  (interactive)                                                                 
  (let* ((beg-end (bounds-of-thing-at-point 'url))                              
         (aurl (buffer-substring (car beg-end)                                  
                                 (cdr beg-end)))                                
         (tinyurl                                                               
          (save-excursion                                                       
            (set-buffer                                                         
             (url-retrieve-synchronously                                        
              (concat "http://tinyurl.com/api-create.php?url=" aurl)))          
            (let* ((beg1 (- (search-forward-regexp "http:") 5))                 
                   (end1 (- (search-forward-regexp "^M") 1)))                   
              (buffer-substring beg1 end1)))))                                  
    (message tinyurl)))
</pre>

----

Here is a modified version using Ask Bjrn Hansen's metamark service for even shorter urls.

(seems broken on GNU Emacs 22, so use the two functions on top.)

<pre>
(require 'mm-url)

(defun get-metamark ()
  "Grabs the url at point and echos the equivalent metamark url in the
minibuffer to ease cutting and pasting."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'url))
	 (longurl (buffer-substring (car bounds)
				    (cdr bounds)))
	 (url (concat "http://metamark.net/api/rest/simple?"
		      (mm-url-encode-www-form-urlencoded
		       (list (cons "long_url" longurl)))))
	 
	 (short-url
	  (save-excursion
	    (with-temp-buffer
	      (mm-url-insert url)
	      (kill-ring-save (point-min) (point-max))
	      (buffer-string)))))
    (message "shortened: %s" short-url)))
</pre>
