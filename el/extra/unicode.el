  (defun unicode-symbol (name)
    "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW
  or GREATER-THAN into an actual Unicode character code. "
    (decode-char 'ucs (case name
                        ;; arrows
                        ('left-arrow 8592)
                        ('up-arrow 8593)
                        ('right-arrow 8594)
                        ('down-arrow 8595)
  
                        ;; boxes
                        ('double-vertical-bar #X2551)
                        
                        ;; relational operators
                        ('equal #X003d)
                        ('not-equal #X2260)
                        ('identical #X2261)
                        ('not-identical #X2262)
                        ('less-than #X003c)
                        ('greater-than #X003e)
                        ('less-than-or-equal-to #X2264)
                        ('greater-than-or-equal-to #X2265)
  
                        ;; logical operators
                        ('logical-and #X2227)
                        ('logical-or #X2228)
                        ('logical-neg #X00AC)
  
                        ;; misc
                        ('nil #X2205)
                        ('horizontal-ellipsis #X2026)
                        ('double-exclamation #X203C)
                        ('prime #X2032)
                        ('double-prime #X2033)
                        ('for-all #X2200)
                        ('there-exists #X2203)
                        ('element-of #X2208)
  
                        ;; mathematical operators
                        ('square-root #X221A)
                        ('squared #X00B2)
                        ('cubed #X00B3)
  
                        ;; letters
                        ('lambda #X03BB)
                        ('alpha #X03B1)
                        ('beta #X03B2)
                        ('gamma #X03B3)
                        ('delta #X03B4))))
                        
  (defun substitute-pattern-with-unicode (pattern symbol)
    "Add a font lock hook to replace the matched part of PATTERN with the 
  Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
    (interactive)
    (font-lock-add-keywords
     nil `((,pattern (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                              ,(unicode-symbol symbol))
                              nil))))))
  
  (defun substitute-patterns-with-unicode (patterns)
    "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
    (mapcar #'(lambda (x)
                (substitute-pattern-with-unicode (car x)
                                                 (cdr x)))
            patterns))

