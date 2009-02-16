; -*- coding: utf-8; mode: emacs-lisp; mode: pretty-symbols; -*-
(require 'cl)
(define-minor-mode pretty-symbols-mode
  "A font-lock extension to draw multi-character codes in programming buffers
as Unicode glyphs.  For example, in C \"!=\" would be drawn as the not-equals
symbol."
  nil " Î»" nil
  (if pretty-symbols-mode
      (pretty-symbols-enable)
      (pretty-symbols-disable)))

(defcustom pretty-symbol-patterns nil
  "A structure ((pattern symbol major-modes*)*)."
  ;; only one variable, so we won't bother creating a whole new group.
  :group 'font-lock
  :type '(repeat (list char string list)))

;; This should really be in the defcustom block, but putting it here makes
;; debugging much easier for me.  So fuck you all.
(setq
 pretty-symbol-patterns
 (let ((lispen '(emacs-lisp-mode
		 ilisp-mode
		 inferior-lisp-mode
		 lisp-interaction-mode
		 lisp-mode))
       (mlen	'(sml-mode
		  inferior-sml-mode))
       (texen	'(latex-mode))
       (c-like '(c-mode
		 c++-mode
		 perl-mode
		 sh-mode)))
   `((?\\ "\\textbackslash"	nil)
     (?| "\\textbar"		nil)
     (?Â¡ "!!"			nil)
     (?Â¢ "cents?"		nil)
     (?Â£ "pounds?"		nil)
     (?Â¥ "yen"			nil)
     (?Â© "&copy;"		nil)
     (?Â¬ "\\<!\\>"		(,@c-like))
     (?Â¬ "\\<not\\>"		(,@lispen tuareg-mode haskell-mode ,@mlen))
     (?Â± "plus-minus"		nil)
     (?Â² "square"		nil)
     (?Â³ "cube"			nil)
     (?Â· "dot-product"		nil)
     (?Â¼ "one-quarter"		nil)
     (?Â½ "one-half"		nil)
     (?Â¾ "three-quarters"	nil)
     (?Ã "*"			(,@mlen))
     (?Ã· "/"			nil)
     (?â¹ ":-("			(erc-mode))
     (?âº ":-)"			(erc-mode))
     (?Î± "\\<alpha\\>"		(tuareg-mode haskell-mode))
     (?Î² "\\<beta\\>"		(tuareg-mode haskell-mode))
     (?Î³ "\\<gamma\\>"		(tuareg-mode haskell-mode))
     (?Î´ "\\<delta\\>"		(tuareg-mode haskell-mode))
     (?Î» "\\<lambda\\>"		(,@lispen))
     (?Î» "\\<fn\\>"		(,@mlen))
     (?â "--"			(texen))
     (?â "^ +\\(|\\)"		(tuareg-mode))
     (?â "`"			nil)
     (?â "'"			nil)
     (?â "``"			nil)
     (?â "''"			nil)
     (?â¥ "[^.]\\(\\.\\.\\)[^.]" (perl-mode))
     (?â¦ "[^.]\\(\\.\\.\\)[^.]" (haskell-mode))
     (?â¦ "[^.]\\(\\.\\.\\.\\)[^.]" (perl-mode))
     (?â° "/1000"		nil)
     (?â± "/10000"		nil)

     ;; I think these should be Ê¹ and Êº... :-/
     (?â² "[^']'[^']"		(tuareg-mode haskell-mode))
     (?â³ "''"			(tuareg-mode haskell-mode))
     (?â´ "'''"			nil)
     (?âµ "`"			nil)
     (?âµ "\\s \\('\\)[a-zA-Z]"		(inferior-sml-mode))
     (?â¶ "\\s \\(''\\)[a-zA-Z]"		(inferior-sml-mode))
     (?â· "\\s \\('''\\)[a-zA-Z]"	(inferior-sml-mode))
     (?â² "[a-zA-Z0-9_]\\('\\)\\s "	(,@mlen))
     (?â³ "[a-zA-Z0-9_]\\(''\\)\\s "	(,@mlen))
     (?â´ "[a-zA-Z0-9_]\\('''\\)\\s "	(,@mlen))
     (?â¶ "``"			nil)
     (?â· "```"			nil)
     (?â¹ "<"			nil)
     (?âº ">"			nil)
     (?â¼ "!!"			(haskell-mode))
     (?â½ "!?"			nil)
     (?â ":="			(smalltalk-mode))
     (?â "<-"			(tuareg-mode haskell-mode))
     (?â "\\^"			(tuareg-mode))
     (?â "->"			(c-mode c++-mode perl-mode tuareg-mode haskell-mode ,@mlen))
     (?â "=>"			(perl-mode ,@mlen))
     (?â "\\<List.for_all\\>"	(tuareg-mode))
     (?â "\\<\\(for\\)\\s +[^\\s ]+\\s +in\\>" (sh-mode))
     (?â "\\<forall\\>"		(perl-mode))
     (?â "\\<List.exists\\>"	(tuareg-mode))
     (?â "\\<thereexists\\>"	nil)
     (?â "\\<NULL\\>"		(c-mode))
     (?â "\\<nil\\>"		(,@lispen tuareg-mode))
     (?â "\\[\\]"		(haskell-mode))
     (?â "\\<List.mem\\>"	(tuareg-mode))
     (?â "\\<for\\s +[^\\s ]+\\s +\\(in\\)\\>" (sh-mode))
     (?â "\\<member\\>"		(,@lispen))
     (?â "\\<product\\>"	nil)
     (?â "\\<sum\\>"		nil)
     (?â "minus-plus"		nil)
     (?â "\\<sqrt\\>"		(tuareg-mode))
     (?â "\\<infinity\\>"	nil)
     (?â§ "&&"			(,@c-like tuareg-mode))
     (?â§ "\\<and\\>"		(,@lispen))
     (?â§ "\\<andalso\\>"	(,@mlen))
     (?â¨ "\\<or\\>"		(,@lispen))
     (?â¨ "\\<orelse\\>"		(,@mlen))
     (?â¨ "||"			(,@c-like tuareg-mode))
     (?â "~="			(perl-mode))
     (?â  "/="			(,@lispen haskell-mode))
     (?â  "<>"			(tuareg-mode ,@mlen))
     (?â  "\\!="			(,@c-like tuareg-mode))
     (?â¡ "=="			(,@c-like tuareg-mode haskell-mode joy-mode))
     (?â¡ "\\<eql\\>"		(,@lispen))
     (?â£ "\\<equal\\>"		(,@lispen))
     (?â» "~"			(,@mlen)) ; unary negation
     (?â¤ "<="			t)
     (?â¥ ">="			t)
     (?âª "<<"			(,@c-like shell-mode))
     (?â« ">>"			(,@c-like shell-mode))
     (?â "\\<set-difference\\>" (,@lispen))
     (?â "\\<intersection\\>"	(,@lispen))
     (?â "\\<union\\>"		(,@lispen))

     ;; I make the assumption that you write "... *pointer", but
     ;; "...*..." or "... * ..." for multiplication.
     ;; This stuff isn't running so well, so I've switched it off.

     ;; (?â "\\(?:[	 (*]\\)\\(\\*\\)\\(?:[a-zA-Z0-9()*\n]\\)"			(c-mode))
     ;; (?â "\\(?:[a-zA-Z0-9()*]\\)\\(\\*\\)\\(?:[*)	 ]\\)"			 	(c-mode))
     ;; (?Ã "\\(?:[a-zA-Z0-9)]\\)\\(\\*\\)\\(?:[a-zA-Z0-9(]\\)"				(c-mode))
     ;; (?Ã "\\(?:[a-zA-Z0-9)]\\)[	 ]+\\(\\*\\)[	 ]+\\(?:[a-zA-Z0-9(]\\)"	(c-mode))
     ;; (?Ã· "\\(?:[a-zA-Z0-9)]\\)\\(\\/\\)\\(?:[a-zA-Z0-9(]\\)"				(c-mode))
     ;; (?Ã· "\\(?:[a-zA-Z0-9)]\\)[	 ]+\\(\\/\\)[	 ]+\\(?:[a-zA-Z0-9(]\\)"	(c-mode))

     ;; LaTeX text symbols.  From http:/www.tug.org/tex-archive/info/symbols/comprehensive/symbols-a4.pdf
     ,@(map 'list (lambda (c p) (list c (format "\\(\\\\%s\\({}\\)?\\)" p) '(,@texen)))
	    "$%_}&#{â â¡Â¶Â©Â§â¦Â£^~*\\|{}â¢Â©â â¡$â¦ââÂ¡><ÂªÂºÂ¶Â·Â¿ââââÂ®Â§Â£â¢_â©âª"
	    '("$" "%" "_" "}" "&" "#" "{" "dag" "ddag" "P" "copyright" "S" "dots" "pounds" "textasciicircum" "textasciitilde" "textasteriskcentered" "textbackslash" "textbar" "textbraceleft" "textbraceright" "textbullet" "textcopyright" "textdagger" "textdaggerdbl" "textdollar" "textellipsis" "textemdash" "textendash" "textexclamdown" "textgreater" "textless" "textordfeminine" "textordmasculine" "textparagraph" "textperiodcentered" "textquestiondown" "textquotedblleft" "textquotedblright" "textquoteleft" "textquoteright" "textregistered" "textsection" "textsterling" "texttrademark" "textunderscore" "textlangle" "textrangle"))

     ;; LaTeX math symbols.
   ,@(map 'list (lambda (c p) (list c (format "\\(\\\\%s\\(?:{}\\)?\\|\\$\\\\%s\\(?:{}\\)?\\$\\)" p p) '(,@texen)))
	  "ÎÎÎÎÎÎÎÎÎÎÎÎÎÎÎÎ Î¡Î£Î¤Î¥Î¦Î§Î¨Î©Î±Î²Î³Î´ÎµÎ¶Î·Î¸ÏÎ¹ÎºÎ»Î¼Î½Î¾Î¿ÏÏÏÏÏÏÏÏÏÏÏÏ"
	  '("Alpha" "Beta" "Gamma" "Delta" "Epsilon" "Zeta" "Eta" "Theta"
	    "Iota" "Kappa" "Lambda" "Mu" "Nu" "Xi" "Omicron" "Pi" "Rho"
	    "Sigma" "Tau" "Upsilon" "Phi" "Chi" "Psi" "Omega" "alpha" "beta"
	    "gamma" "delta" "epsilon" "zeta" "eta" "theta" "thetasym" "iota"
	    "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "piv" "rho"
	    "sigmaf" "sigma" "tau" "upsilon" "upsih" "phi" "chi" "psi"
	    "omega"))

     ;; This list is taken from the HTML4 spec.
   ,@(map 'list (lambda (c p) (list c (format "&%s;" p) '(html-mode)))
	  "Â¡Â¢Â£Â¤Â¥Â¦Â§Â¨Â©ÂªÂ«Â¬Â­Â®Â¯Â°Â±Â²Â³Â´ÂµÂ¶Â·Â¸Â¹ÂºÂ»Â¼Â½Â¾Â¿ÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃÃ Ã¡Ã¢Ã£Ã¤Ã¥Ã¦Ã§Ã¨Ã©ÃªÃ«Ã¬Ã­Ã®Ã¯Ã°Ã±Ã²Ã³Ã´ÃµÃ¶Ã·Ã¸Ã¹ÃºÃ»Ã¼Ã½Ã¾Ã¿ÆÎÎÎÎÎÎÎÎÎÎÎÎÎÎÎÎ Î¡Î£Î¤Î¥Î¦Î§Î¨Î©Î±Î²Î³Î´ÎµÎ¶Î·Î¸ÏÎ¹ÎºÎ»Î¼Î½Î¾Î¿ÏÏÏÏÏÏÏÏÏÏÏÏâ¢â¦â²â³â¾âââââ¢âµââââââµâââââââââââââââââââââ â§â¨â©âªâ«â´â¼âââ â¡â¤â¥ââââââââ¥ââââââ©âªââ â£â¥â¦\"&<>ÅÅÅ Å¡Å¸^~ââââââââââââââââ â¡â°â¹âºâ¬"
	  '("iexcl" "cent" "pound" "curren" "yen" "brvbar" "sect" "uml"
	    "copy" "ordf" "laquo" "not" "shy" "reg" "macr" "deg" "plusmn"
	    "sup2" "sup3" "acute" "micro" "para" "middot" "cedil" "sup1"
	    "ordm" "raquo" "frac14" "frac12" "frac34" "iquest" "Agrave"
	    "Aacute" "Acirc" "Atilde" "Auml" "Aring" "AElig" "Ccedil"
	    "Egrave" "Eacute" "Ecirc" "Euml" "Igrave" "Iacute" "Icirc"
	    "Iuml" "ETH" "Ntilde" "Ograve" "Oacute" "Ocirc" "Otilde" "Ouml"
	    "times" "Oslash" "Ugrave" "Uacute" "Ucirc" "Uuml" "Yacute"
	    "THORN" "szlig" "agrave" "aacute" "acirc" "atilde" "auml"
	    "aring" "aelig" "ccedil" "egrave" "eacute" "ecirc" "euml"
	    "igrave" "iacute" "icirc" "iuml" "eth" "ntilde" "ograve"
	    "oacute" "ocirc" "otilde" "ouml" "divide" "oslash" "ugrave"
	    "uacute" "ucirc" "uuml" "yacute" "thorn" "yuml" "fnof" "Alpha"
	    "Beta" "Gamma" "Delta" "Epsilon" "Zeta" "Eta" "Theta" "Iota"
	    "Kappa" "Lambda" "Mu" "Nu" "Xi" "Omicron" "Pi" "Rho" "Sigma"
	    "Tau" "Upsilon" "Phi" "Chi" "Psi" "Omega" "alpha" "beta" "gamma"
	    "delta" "epsilon" "zeta" "eta" "theta" "thetasym" "iota" "kappa"
	    "lambda" "mu" "nu" "xi" "omicron" "pi" "piv" "rho" "sigmaf"
	    "sigma" "tau" "upsilon" "upsih" "phi" "chi" "psi" "omega" "bull"
	    "hellip" "prime" "Prime" "oline" "frasl" "weierp" "image" "real"
	    "trade" "alefsym" "larr" "uarr" "rarr" "darr" "harr" "crarr"
	    "lArr" "uArr" "rArr" "dArr" "hArr" "forall" "part"
	    "exist" "empty" "nabla" "isin" "notin" "ni" "prod" "sum" "minus"
	    "lowast" "radic" "prop" "infin" "ang" "and" "or" "cap" "cup"
	    "int" "there4" "sim" "cong" "asymp" "ne" "equiv" "le" "ge" "sub"
	    "sup" "nsub" "sube" "supe" "oplus" "otimes" "perp" "sdot"
	    "lceil" "rceil" "lfloor" "rfloor" "lang" "rang" "loz" "spades"
	    "clubs" "hearts" "diams" "quot" "amp" "lt" "gt" "OElig" "oelig"
	    "Scaron" "scaron" "Yuml" "circ" "tilde" "ensp" "emsp"
	    "thinsp" "zwnj" "zwj" "lrm" "rlm" "ndash" "mdash" "lsquo"
	    "rsquo" "sbquo" "ldquo" "rdquo" "bdquo" "dagger" "Dagger"
	    "permil" "lsaquo" "rsaquo" "euro")))))

(defun pretty-symbols-enable/disable (font-lock-add/remove-keywords)
  (dolist (x pretty-symbol-patterns)
    (if (or (eql t (third x))
	    (find major-mode (third x)))
	(funcall font-lock-add/remove-keywords
		 nil
		 `((,(second x)
		     ;; if there is grouping, match the first group, else match the whole pattern.
		    (0
		     (prog1 nil
			 (compose-region (match-beginning ,(if (string-match "\\\\(.*\\\\)" (second x)) 1 0))
					 (match-end       ,(if (string-match "\\\\(.*\\\\)" (second x)) 1 0))
					 ,(first x))))))))))

;      <bojohan> twb: anyway, here's how to fix unprettification:
;		(compose-region foo bar baz 'decompose-region)

(defun pretty-symbols-enable ()
  (pretty-symbols-enable/disable #'font-lock-add-keywords))

(defun pretty-symbols-disable ()
  (pretty-symbols-enable/disable #'font-lock-remove-keywords))

(provide 'pretty-symbols)
