;;; Saved through ges-version 0.3.3dev at 2004-11-20 18:31
;;; ;;; From: Yoni Rabkin Katzenell <yoni-r@actcom.com>
;;; ;;; Subject: search-list.el
;;; ;;; Newsgroups: gmane.emacs.sources
;;; ;;; Date: Sat, 07 Aug 2004 23:21:32 +0300
;;; ;;; Organization: NTT/VERIO


;;; This tiny piece of code does something that I keep finding myself
;;; wanting. Given a list of regular expressions, search-list returns a list
;;; containing the matches with their beginning and ending positions in the
;;; buffer.

;;; I would very much like to know if something like this already exists. If
;;; so, my apologies.

;;; Begin Code------------------------------------------------------------

(defun search-list (l)
  (flet ((search-list-make-res (beg end str)
			       (list (cons beg end) str)))
  (let (res)
    (mapcar (lambda (l)
	      (save-excursion
		(goto-char (point-min))
		(while (re-search-forward l (point-max) t)
		  (push (search-list-make-res (match-beginning 0)
					      (match-end 0)
					      (match-string-no-properties 0))
			res))))
	    l)
    res)))

;;End Code--------------------------------------------------------------

;;; -- 
;;; "Cut your own wood and it will warm you twice"
;;; 	Regards, Yoni Rabkin Katzenell

