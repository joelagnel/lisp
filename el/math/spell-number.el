;;; spell-number.el --- Spell out an integer or currency in words.

;; Copyright (C) 1999-2000 Vinicius Jose Latorre <vinicius@cpqd.com.br>

;; Author:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Maintainer:	Vinicius Jose Latorre <vinicius@cpqd.com.br>
;; Time-stamp:	<2000/02/27 13:53:47 vinicius>
;; Version:	2.0
;; Keywords:	spell, local
;; X-URL:	http://www.cpqd.com.br/~vinicius/emacs/

;; This file is NOT (yet?) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:

;; Introduction
;; ------------
;;
;; To use spell-number, insert in your ~/.emacs:
;;
;;        (require 'spell-number)
;;
;; `spell-integer-in-words' spells out an integer in words in the language
;; specified by `spell-language'.
;;
;; `spell-currency-in-words' spells out a currency in words in the language
;; specified by `spell-language' and in the country specified by
;; `spell-country'.
;;
;; `spell-language-database' and `spell-currency-database' contains language
;; information for spelling.
;;
;; `spell-numeric-string-in-words' and `spell-currency-string-in-words' accept
;; numeric string as parameter.
;;
;; `spell-zero-cents' indicates if " and zero cents" should be spelled.
;;
;; `spell-gender-default' specifies the default gender to be used when there is
;; no neuter gender.
;;
;; `spell-period-character' specifies the character to separate periods.
;;
;; `spell-decimal-character' specifies the decimal point character.
;;
;; `spell-number-customize' customizes spell-number options.
;;
;;
;; Examples
;; --------
;;
;; For:
;;   (setq spell-language 'english-informal-us)
;;   (spell-integer-in-words 121121)
;;
;; The result is:
;; "one hundred twenty-one thousand, one hundred twenty-one"
;;
;; And for:
;;   (setq spell-language 'english-informal-us)
;;   (setq spell-country 'united-states)
;;   (spell-currency-in-words 1121.21)
;;
;; The result is:
;; "one thousand, one hundred twenty-one dollars and twenty-one cents"
;;
;; You can also use numeric strings:
;;
;;   (setq spell-language 'english-informal-us)
;;   (setq spell-period-character ?,)
;;   (spell-numeric-string-in-words "121,121")
;;    ==> "one hundred twenty-one thousand, one hundred twenty-one"
;;
;;   (setq spell-language 'english-informal-us)
;;   (setq spell-country 'united-states)
;;   (setq spell-period-character ?,)
;;   (setq spell-decimal-character ?.)
;;   (spell-currency-string-in-words "1,121.21")
;;    ==> "one thousand, one hundred twenty-one dollars and twenty-one cents"
;;
;; The maximum numeric string that spell-number gets to spell out in words is
;; "999,999,999,999,999".  Below is the answer given by spell-number to numeric
;; strings above the maximum value.
;;
;;   (setq spell-language 'english-informal-us)
;;   (setq spell-period-character ?,)
;;   (spell-numeric-string-in-words "2,000,000,000,121,121")
;;    ==> "two ??? , one hundred twenty-one thousand, one hundred twenty-one"
;;
;;
;; Gender Engine
;; -------------
;;
;; The gender engine is designed for spelling out number and currency.  It's
;; beyond the scope of gender engine to handle general gender in all languages.
;;
;; See the following examples of gender usage:
;;
;; . In english (US): (only `neuter')
;;    (spell-integer-in-words 101)
;;	 ==> "one hundred one"
;;			  `neuter'
;;    (spell-currency-in-words 101.01)
;;	 ==> "one hundred one dollars and one cent"
;;			  `neuter'	  `neuter'
;;
;; . In german (DE): (`neuter' is used to express numeral only)
;;    (spell-integer-in-words 101)
;;	 ==> "einhundertundeins"
;;			   `neuter'
;;    (spell-currency-in-words 101.01)
;;	 ==> "einhundertundeine Mark und ein Pfennig"
;;			   `feminine'	 `masculine'
;;
;; . In portuguese (BR): (no `neuter')
;;    (spell-integer-in-words 101)
;;	 ==> "cento e um"
;;		      `masculine'
;;    (spell-currency-in-words 101.01)
;;	 ==> "cento e um reais e um centavo"
;;		      `masculine' `masculine'
;;
;; . In spanish (ES): (no `neuter', but it's used to express numeral only)
;;    (spell-integer-in-words 101)
;;	 ==> "ciento uno"
;;		     `neuter'
;;    (spell-currency-in-words 101.01)
;;	 ==> "ciento una pesetas y un céntimo"
;;		     `feminine'	   `masculine'
;;
;; As you can note from the examples above, there are cases where the `neuter'
;; gender used by spell-number differs from the usual way that `neuter' gender
;; is used in a language.  This is a trick used to spell out numbers correctly.
;;
;;
;; Languages & Countries
;; ---------------------
;;
;; The following languages are supported:
;;
;; catalan, danish, dutch, english-formal-gb, english-informal-gb,
;; english-formal-us, english-informal-us, esperanto, finnish, french-fr,
;; french-ch, german, italian, japanese, norwegian, portuguese-br,
;; portuguese-pt, spanish and swedish.
;;
;; The following countries are supported:
;;
;; andorra-french, andorra-spanish, antigua-and-barbuda, argentina, australia,
;; austria, bahamas, barbados, belgium, belize, benin, bolivia, brazil, brunei,
;; burkina-faso, burundi, cameroon, canada, cape-verde central-african-republic,
;; chad, chile, colombia, comoros, congo, costa-rica, cuba, cyprus, denmark,
;; djibouti, dominica, dominican-republic, ecuador, el-salvador,
;; equatorial-guinea, fiji, finland, france, gabon, germany, grenada, guatemala,
;; guinea, guinea-bissau, guyana, haiti, honduras, ireland, italy, ivory-coast,
;; jamaica, japan, kenya, kiribati, liberia, liechtenstein, luxembourg,
;; madagascar, mali, mexico, monaco, mozambique, namibia, nauru, netherlands,
;; new-zealand, nicaragua, niger, norway, panama, paraguay, peru, philippines,
;; portugal, rwanda, sao-tome-and-principe, senegal, singapore, solomon-islands,
;; somalia, south-africa, spain, st-kitts-and-nevis, st-lucia,
;; st-vicent-and-grenadines, sweden, switzerland, taiwan, tanzania, togo,
;; trinidad-and-tobago, tuvalu, uganda, united-kingdom, united-states, uruguay,
;; venezuela and zimbabwe.
;;
;;
;; Number in Several Language
;; --------------------------
;;
;; To see example of numbers (until million) in several languages, see the URL:
;;
;;    http://www.travlang.com/languages/
;;
;;
;; American and British English
;; ----------------------------
;;
;; For numbers in english above million, see the URLs:
;;
;;    http://www.m-w.com/mw/table/number.htm
;;    http://www.cs.unb.ca/~alopez-o/math-faq/mathtext/node25.html
;;
;;
;; How to Use the Functions From a Shell
;; -------------------------------------
;;
;; Create a script file (let's say currency.sh) containing the following code:
;;
;;    #! /bin/bash
;;
;;    # spell-number.el path
;;    LISPDIR=/usr/local/lib/emacs/site-lisp
;;
;;    cat >$$.data <<EOF
;;    (setq spell-language 'english-informal-us)
;;    (setq spell-country 'united-states)
;;    (message (spell-currency-in-words $*))
;;    EOF
;;
;;    emacs -batch -load $LISPDIR/spell-number.el -load $$.data
;;
;;    rm $$.data
;;
;; So, when you type:
;;
;;    currency.sh 1121.21
;;
;; It's displayed:
;;
;;    one thousand, one hundred twenty-one dollars and twenty-one cents
;;
;;
;; Acknowledgements
;; ----------------
;;
;; Thanks to Juanma Barranquero <lektu@teleline.es> for spanish corrections, and
;; for zero cents and gender suggestions.
;;
;; Thanks to Florian Weimer <fw@s.netic.de> for german gender corrections.
;;
;; Thanks to Eberhard Burr <Eberhard.Burr@gmx.de> for german corrections.
;;
;; Thanks to Petra Stempfle for german contribution.
;;
;; Thanks to Antonio Orlando Faro da Silva <ao@cpqd.com.br> for portuguese (PT)
;; corrections.
;;
;; Thanks to Ailton Bauer Paschoal <abauer@sti.com.br> for italian contribution.
;;
;; Thanks to Emmanuel Michon <emmanuel_michon@sdesigns.com>, Christophe Cuq
;; <ccuq@teaser.fr> and John S. Yates Jr <john@everfile.com> for french
;; corrections.
;;
;; Thanks to Luciene Mastrandrea <tucunlu@dglnet.com.br> for french
;; contribution.
;;
;; Thanks to Don Provan <dprovan@ra.lucent.com> for American and British
;; English.
;;
;; Thanks to Franz Zahaurek <fzk@gams.at> for How to Use the Functions From a
;; Shell.
;;
;; Thanks to all who emailed comments and contributions.
;;
;;
;;
;; Fell free to send contributions, suggestions, corrections, new languages,
;; etc. to maintainer.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Options:


(defgroup spell-number nil
  "Spell out an integer or currency in words"
  :link '(emacs-library-link :tag "Source Lisp File" "spell-number.el")
  :prefix "spell-"
  :group 'local)


(defcustom spell-language 'portuguese-br
  "*Specify the language to spell out a number in words.

See also `spell-language-database'."
  :type '(radio
	  :tag "Language"
	  (const catalan)       (const danish)    (const dutch)
	  (const english-formal-gb) (const english-informal-gb)
	  (const english-formal-us) (const english-informal-us)
	  (const esperanto)     (const finnish)   (const french-fr)
	  (const french-ch)     (const german)    (const italian)
	  (const japanese)      (const norwegian) (const portuguese-br)
	  (const portuguese-pt) (const spanish)   (const swedish))
  :group 'spell-number)


(defcustom spell-country 'brazil
  "*Specify the country to spell out a currency in words.

See also `spell-country-database'."
  :type '(radio
	  :tag "Country"
	  (const andorra-french) (const andorra-spanish)
	  (const antigua-and-barbuda)
	  (const argentina)    (const australia)       (const austria)
	  (const bahamas)      (const barbados)        (const belgium)
	  (const belize)       (const benin)           (const bolivia)
	  (const brazil)       (const brunei)
	  (const burkina-faso) (const burundi)         (const cameroon)
	  (const canada)       (const cape-verde)
	  (const central-african-republic)             (const chad)
	  (const chile)        (const colombia)        (const comoros)
	  (const congo)        (const costa-rica)      (const cuba)
	  (const cyprus)       (const denmark)         (const djibouti)
	  (const dominica)     (const dominican-republic)
	  (const ecuador)      (const el-salvador)
	  (const equatorial-guinea)                    (const fiji)
	  (const finland)      (const france)          (const gabon)
	  (const germany)      (const grenada)         (const guatemala)
	  (const guinea)       (const guinea-bissau)   (const guyana)
	  (const haiti)        (const honduras)        (const ireland)
	  (const italy)        (const ivory-coast)     (const jamaica)
	  (const japan)        (const kenya)           (const kiribati)
	  (const liberia)      (const liechtenstein)   (const luxembourg)
	  (const madagascar)   (const mali)            (const mexico)
	  (const monaco)       (const mozambique)      (const namibia)
	  (const nauru)        (const netherlands)     (const new-zealand)
	  (const nicaragua)    (const niger)           (const norway)
	  (const panama)       (const paraguay)        (const peru)
	  (const philippines)  (const portugal)        (const rwanda)
	  (const sao-tome-and-principe)                (const senegal)
	  (const singapore)    (const solomon-islands) (const somalia)
	  (const south-africa) (const spain)
	  (const st-kitts-and-nevis)                   (const st-lucia)
	  (const st-vicent-and-grenadines)             (const sweden)
	  (const switzerland)  (const taiwan)          (const tanzania)
	  (const togo)         (const trinidad-and-tobago)
	  (const tuvalu)       (const uganda)          (const united-kingdom)
	  (const united-states)                        (const uruguay)
	  (const venezuela)    (const zimbabwe))
  :group 'spell-number)


(defcustom spell-zero-cents t
  "*Non-nil means that \" and zero cents\" is spelled out.

If cent part is different than zero, it's always spelled out.

It's used in `spell-currency-in-words' and `spell-currency-string-in-words'."
  :type 'boolean
  :group 'spell-number)


(defcustom spell-gender-default 'masculine
  "*Specify the default gender to be used when there is no neuter gender.

It's used in `spell-int' and `spell-str'."
  :type '(radio :tag "Gender"
		(const feminine) (const masculine))
  :group 'spell-number)


(defcustom spell-period-character ?.
  "*Specify the character to separate periods.

For example, in the numeric string \"1.000,00\" this variable should be set to
character `.'.

It's used in `spell-currency-string-in-words' and
`spell-numeric-string-in-words'."
  :type 'character
  :group 'spell-number)


(defcustom spell-decimal-character ?,
  "*Specify the decimal point character.

For example, in the numeric string \"1.000,00\" this variable should be set to
character `,'.

It's used in `spell-currency-string-in-words'."
  :type 'character
  :group 'spell-number)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Variables and Functions:


;; to avoid compilation gripes
(defvar spell-zero            nil)
(defvar spell-comma           nil)
(defvar spell-minus           nil)
(defvar spell-and             nil)
(defvar spell-tens            nil)
(defvar spell-hundreds        nil)
(defvar spell-hundreds-tens   nil)
(defvar spell-singular-period nil)
(defvar spell-plural-period   nil)
(defvar spell-gender          1)


(defsubst spell-aref-gender (number gender)
  (if (vectorp number)
      (or (aref number gender)
	  (aref number spell-gender))
    number))


(defsubst spell-aref-period (vector period)
  (if (> period 4)
      " ??? "
    (aref vector period)))


(defmacro spell-engine (separator strings integer period gender)
  `(progn
     (setq ,separator
	   (cond
	    ((= ,separator 0)		; null "and"
	     1)
	    ((= ,separator 1)		; "and"
	     (setq ,strings
		   (concat (if (vectorp spell-and)
			       (and (> ,integer 0)
				    (aref spell-and
					  (if (> ,period 1) 1 0)))
			     spell-and)
			   ,strings))
	     (if (and (vectorp spell-and) (<= ,integer 0))
		 1
	       2))
	    ((= ,separator 2)		; ","
	     (setq ,strings (concat spell-comma ,strings))
	     2)
	    ((= ,separator 3)		; null ","
	     2)))
     (cond
      ((= ,integer 0)			; period "000"
       (setq ,separator (if (= ,separator 1)
			    (if (vectorp spell-and)
				1
			      0)	; "000" initial
			  3)))		; "000" intermediate

      ((= ,integer 1)			; period "001"
       (setq ,strings
	     (concat (spell-aref-gender
		      (spell-aref-period spell-singular-period ,period)
		      ,gender)
		     ,strings)))
      (t				; any period  (2..999)
       (let ((tens (mod ,integer 100)))	; current tens
	 (setq ,integer (/ ,integer 100)
	       ,strings
	       (concat (spell-aref-gender
			(aref spell-tens tens)
			,gender)
		       (spell-aref-gender
			(spell-aref-period spell-plural-period ,period)
			,gender)
		       ,strings))
	 (and (> ,integer 0)		; hundreds (100..999)
	      (setq ,strings
		    (concat (spell-aref-gender
			     (aref (if (zerop tens)
				       spell-hundreds
				     (setq ,separator 2)
				     spell-hundreds-tens)
				   ,integer)
			     ,gender)
			    ,strings))))))
     (setq ,period (1+ ,period))))	; next period


(defconst spell-gender-alist
  '((feminine  . 0)
    (masculine . 1)
    (neuter    . 2))
  "Alist for gender index used by `spell-int' and `spell-str'.")


(defun spell-int (int gender-sym)
  (let ((negative (< int 0))
	(integer (abs int))
	(period 0)
	(separator 0)
	(gender (or (cdr (assoc gender-sym spell-gender-alist))
		    2))			; neuter
	strings rest rest-tens rest-units tens units)
    (if (or (null spell-tens) (zerop integer))
	spell-zero			; zero
      ;; set default gender index
      (setq spell-gender (or (cdr (assoc spell-gender-default
					 spell-gender-alist))
			     1))	; masculine
      ;; spell out number in words
      (while (> integer 0)
	(setq rest (mod integer 1000))	; current period
	(spell-engine separator strings rest period gender)
	(setq integer (/ integer 1000)))
      ;; indicate sign number and trim spaces
      (spell-sign-and-trim-spaces negative strings))))


(defun spell-str (str gender-sym &optional places)
  (let ((len (length str))
	(period 0)
	(separator 0)
	(gender (or (cdr (assoc gender-sym spell-gender-alist))
		    2))			; neuter
	(power10 [1 10 100 1000])
	(number 0)
	negative start stri strings integer power)
    (if (or (null spell-tens) (zerop len))
	nil				; invalid state
      ;; set default gender index
      (setq spell-gender (or (cdr (assoc spell-gender-default
					 spell-gender-alist))
			     1)		; masculine
	    start (cond ((= (aref str 0) ?+) 1)
			((= (aref str 0) ?-) (setq negative t) 1)
			(t                   0)
			))
      ;; skip initial zeros
      (or places
	  (while (and (< start len)
		      (or (= (aref str start) ?0)
			  (= (aref str start) spell-period-character)))
	    (setq start (1+ start))))
      (if (= start len)
	  (cons 0 spell-zero)		; zero
	;; set index to end of string
	(if (not places)
	    (setq stri  (1- len)
		  power 1)
	  (setq stri start)
	  (while (and (> places 0) (< stri len))
	    ;; skip period separator
	    (while (and (< stri len)
			(= (aref str stri) spell-period-character))
	      (setq stri (1+ stri)))
	    (setq stri   (1+ stri)
		  places (1- places)))
	  (setq stri  (1- stri)
		power (aref power10 places)))
	;; spell out numeric string in words
	(while (>= stri start)
	  ;; get current period
	  (let ((i 0))
	    (setq integer 0)
	    (while (and (>= stri start) (< i 3))
	      ;; skip period separator
	      (while (and (>= stri start)
			  (= (aref str stri) spell-period-character))
		(setq stri (1- stri)))
	      (and (>= stri start)
		   (setq integer (+ (* (aref power10 i)
				       (- (aref str stri) ?0))
				    integer)))
	      (setq i    (1+ i)
		    stri (1- stri))))
	  (setq integer (* integer power))
	  ;; spell out current period
	  (spell-engine separator strings integer period gender))
	(cons (cond ((> period 1)  2)	; above one
		    ((= integer 0) 0)	; zero
		    ((= integer 1) 1)	; one
		    (t             2))	; above one
	      ;; indicate sign number and trim spaces
	      (spell-sign-and-trim-spaces negative strings))))))


(defun spell-sign-and-trim-spaces (negative strings)
  (if (or (null strings) (string= strings ""))
      spell-zero
    ;; indicate sign number
    (and negative
	 (setq strings (concat spell-minus strings)))
    (let ((from 0)
	  (to   (1- (length strings))))
      ;; trim initial spaces
      (while (= (aref strings from) ?\ )
	(setq from (1+ from)))
      ;; trim trailing spaces
      (while (= (aref strings to) ?\ )
	(setq to (1- to)))
      (substring strings from (1+ to)))))


(defconst spell-language-database
  '(
    (catalan				; LANGUAGE
     "zero"				; 0 - zero
     ""					; 1 - comma
     "menos"				; 2 - minus
     " i"				; 3 - and
     [""				; 4 - tens [0-99]
      [" una" " un" nil]    [" dues" " dos" nil] " tres"
      " quatre"             " cinc"              " sis"
      " set"                " vuit"              " nou"
      " deu"
      " onze"               " dotze"             " tretze"
      " catorze"            " quinze"            " setze"
      " disset"             " divuit"            " dinou"
      " vint"
      [" vint-i-una"  " vint-i-un"  nil]
      [" vint-i-dues" " vint-i-dos" nil]         " vint-i-tres"
      " vint-i-quatre"      " vint-i-cinc"       " vint-i-sis"
      " vint-i-set"         " vint-i-vuit"       " vint-i-nou"
      " trenta"
      [" trenta-i-una"  " trenta-i-un"  nil]
      [" trenta-i-dues" " trenta-i-dos" nil]     " trenta-i-tres"
      " trenta-i-cuatro"    " trenta-i-cinco"    " trenta-i-seis"
      " trenta-i-siete"     " trenta-i-ocho"     " trenta-i-nueve"
      " quarenta"
      [" quarenta-i-una"  " quarenta-i-un"  nil]
      [" quarenta-i-dues" " quarenta-i-dos" nil] " quarenta-i-tres"
      " quarenta-i-cuatro"  " quarenta-i-cinco"  " quarenta-i-seis"
      " quarenta-i-siete"   " quarenta-i-ocho"   " quarenta-i-nueve"
      " cinquanta"
      [" cinqanta-i-una"  " cinqanta-i-un"  nil]
      [" cinqanta-i-dues" " cinqanta-i-dos" nil] " cinqanta-i-tres"
      " cinqanta-i-cuatro"  " cinqanta-i-cinco"  " cinqanta-i-seis"
      " cinqanta-i-siete"   " cinqanta-i-ocho"   " cinqanta-i-nueve"
      " seixanta"
      [" seixanta-i-una"  " seixanta-i-un"  nil]
      [" seixanta-i-dues" " seixanta-i-dos" nil] " seixanta-i-tres"
      " seixanta-i-cuatro"  " seixanta-i-cinco"  " seixanta-i-seis"
      " seixanta-i-siete"   " seixanta-i-ocho"   " seixanta-i-nueve"
      " setanta"
      [" setanta-i-una"  " setanta-i-un"  nil]
      [" setanta-i-dues" " setanta-i-dos" nil]   " setanta-i-tres"
      " setanta-i-cuatro"   " setanta-i-cinco"   " setanta-i-seis"
      " setanta-i-siete"    " setanta-i-ocho"    " setanta-i-nueve"
      " vuitanta"
      [" vuitanta-i-una"  " vuitanta-i-un"  nil]
      [" vuitanta-i-dues" " vuitanta-i-dos" nil] " vuitanta-i-tres"
      " vuitanta-i-cuatro"  " vuitanta-i-cinco"  " vuitanta-i-seis"
      " vuitanta-i-siete"   " vuitanta-i-ocho"   " vuitanta-i-nueve"
      " noranta"
      [" noranta-i-una"  " noranta-i-un"  nil]
      [" noranta-i-dues" " noranta-i-dos" nil]   " noranta-i-tres"
      " noranta-i-cuatro"   " noranta-i-cinco"   " noranta-i-seis"
      " noranta-i-siete"    " noranta-i-ocho"    " noranta-i-nueve"]
     [""				; 5 - hundreds [0-9]
      " cent"
      " doscent"
      " trescent"
      " quatrecent"
      " cinccent"
      " siscent"
      " setcent"
      " vuitcent"
      " noucent"]
     [""				; 6 - hundreds-tens [0-9]
      " cent"
      " doscent"
      " trescent"
      " quatrecent"
      " cinccent"
      " siscent"
      " setcent"
      " vuitcent"
      " noucent"]
     [[" una" " un" nil]		; 7 - singular period [0-4]
      " mil"
      " un millón"
      " un mil millón"
      " un billón"]
     [""				; 8 - plural period [0-4]
      " mil"
      " millones"
      " mil millones"
      " billones"]
     )
    (danish				; LANGUAGE
     "nul"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     " og"				; 3 - and
     [""				; 4 - tens [0-99]
      " en"                 " to"                 " tre"
      " fire"               " fem"                " seks"
      " syv"                " otte"               " ni"
      " ti"
      " elleve"             " tolv"               " tretten"
      " fjorten"            " femten"             " seksten"
      " sytten"             " atten"              " nitten"
      " tyve"
      " en og tyve"         " to og tyve"         " tre og tyve"
      " fire og tyve"       " fem og tyve"        " seks og tyve"
      " syv og tyve"        " otte og tyve"       " ni og tyve"
      " tredive"
      " en og tredive"      " to og tredive"      " tre og tredive"
      " fire og tredive"    " fem og tredive"     " seks og tredive"
      " syv og tredive"     " otte og tredive"    " ni og tredive"
      " fyrre"
      " en og fyrre"        " to og fyrre"        " tre og fyrre"
      " fire og fyrre"      " fem og fyrre"       " seks og fyrre"
      " syv og fyrre"       " otte og fyrre"      " ni og fyrre"
      " halvtreds"
      " en og halvtreds"    " to og halvtreds"    " tre og halvtreds"
      " fire og halvtreds"  " fem og halvtreds"   " seks og halvtreds"
      " syv og halvtreds"   " otte og halvtreds"  " ni og halvtreds"
      " tres"
      " en og tres"         " to og tres"         " tre og tres"
      " fire og tres"       " fem og tres"        " seks og tres"
      " syv og tres"        " otte og tres"       " ni og tres"
      " halvfjerds"
      " en og halvfjerds"   " to og halvfjerds"   " tre og halvfjerds"
      " fire og halvfjerds" " fem og halvfjerds"  " seks og halvfjerds"
      " syv og halvfjerds"  " otte og halvfjerds" " ni og halvfjerds"
      " firs"
      " en og firs"         " to og firs"         " tre og firs"
      " fire og firs"       " fem og firs"        " seks og firs"
      " syv og firs"        " otte og firs"       " ni og firs"
      " halvfems"
      " en og halvfems"     " to og halvfems"     " tre og halvfems"
      " fire og halvfems"   " fem og halvfems"    " seks og halvfems"
      " syv og halvfems"    " otte og halvfems"   " ni og halvfems"]
     [""				; 5 - hundreds [0-9]
      " et hundred"
      " to hundred"
      " tre hundred"
      " fire hundred"
      " fem hundred"
      " seks hundred"
      " syv hundred"
      " otte hundred"
      " ni hundred"]
     [""				; 6 - hundreds-tens [0-9]
      " et hundred og"
      " to hundred og"
      " tre hundred og"
      " fire hundred og"
      " fem hundred og"
      " seks hundred og"
      " syv hundred og"
      " otte hundred og"
      " ni hundred og"]
     [" en"				; 7 - singular period [0-4]
      " en tusind"
      " en million"
      " en milliard"
      " en billion"]
     [""				; 8 - plural period [0-4]
      " tusind"
      " millioner"
      " milliarder"
      " billioner"]
     )
    (dutch				; LANGUAGE
     "nul"				; 0 - zero
     ""					; 1 - comma
     "minus "				; 2 - minus
     "en"				; 3 - and
     [""				; 4 - tens [0-99]
      ["eene" "een" nil] "twee"           "drie"
      "vier"             "vijf"           "zes"
      "zeven"            "acht"           "negen"
      "tien"
      "elf"              "twaalf"         "dertien"
      "veertien"         "vijftien"       "zestien"
      "zeventien"        "achttien"       "negentien"
      "twintig"
      "eenentwintig"     "tweeentwintig"  "drieentwintig"
      "vierentwintig"    "vijfentwintig"  "zesentwintig"
      "zevenentwintig"   "achtentwintig"  "negenentwintig"
      "dertig"
      "eenendertig"      "tweeendertig"   "drieendertig"
      "vierendertig"     "vijfendertig"   "zesendertig"
      "zevenendertig"    "achtendertig"   "negenendertig"
      "veertig"
      "eenenveertig"     "tweeenveertig"  "drieenveertig"
      "vierenveertig"    "vijfenveertig"  "zesenveertig"
      "zevenenveertig"   "achtenveertig"  "negenenveertig"
      "vijftig"
      "eenenvijftig"     "tweeenvijftig"  "drieenvijftig"
      "vierenvijftig"    "vijfenvijftig"  "zesenvijftig"
      "zevenenvijftig"   "achtenvijftig"  "negenenvijftig"
      "zestig"
      "eenenzestig"      "tweeenzestig"   "drieenzestig"
      "vierenzestig"     "vijfenzestig"   "zesenzestig"
      "zevenenzestig"    "achtenzestig"   "negenenzestig"
      "zeventig"
      "eenenzeventig"    "tweeenzeventig" "drieenzeventig"
      "vierenzeventig"   "vijfenzeventig" "zesenzeventig"
      "zevenenzeventig"  "achtenzeventig" "negenenzeventig"
      "tachtig"
      "eenentachtig"     "tweeentachtig"  "drieentachtig"
      "vierentachtig"    "vijfentachtig"  "zesentachtig"
      "zevenentachtig"   "achtentachtig"  "negenentachtig"
      "negentig"
      "eenennegentig"    "tweeennegentig" "drieennegentig"
      "vierennegentig"   "vijfennegentig" "zesennegentig"
      "zevenennegentig"  "achtennegentig" "negenennegentig"]
     [""				; 5 - hundreds [0-9]
      "eenhonderd"
      "tweehonderd"
      "driehonderd"
      "vierhonderd"
      "vijfhonderd"
      "zeshonderd"
      "zevenhonderd"
      "achthonderd"
      "negenhonderd"]
     [""				; 6 - hundreds-tens [0-9]
      "eenhonderd"
      "tweehonderd"
      "driehonderd"
      "vierhonderd"
      "vijfhonderd"
      "zeshonderd"
      "zevenhonderd"
      "achthonderd"
      "negenhonderd"]
     [["eene" "een" nil]		; 7 - singular period [0-4]
      "eenduizend"
      "een miljoen "
      "een ?? "
      "een ?? "]
     [""				; 8 - plural period [0-4]
      "duizend"
      " miljoen "
      " ?? "
      " ?? "]
     )
    (english-formal-gb			; LANGUAGE
     "zero"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     " and"				; 3 - and
     [""				; 4 - tens [0-99]
      " one"           " two"           " three"
      " four"          " five"          " six"
      " seven"         " eight"         " nine"
      " ten"
      " eleven"        " twelve"        " thirteen"
      " fourteen"      " fifteen"       " sixteen"
      " seventeen"     " eighteen"      " nineteen"
      " twenty"
      " twenty one"    " twenty two"    " twenty three"
      " twenty four"   " twenty five"   " twenty six"
      " twenty seven"  " twenty eight"  " twenty nine"
      " thirty"
      " thirty one"    " thirty two"    " thirty three"
      " thirty four"   " thirty five"   " thirty six"
      " thirty seven"  " thirty eight"  " thirty nine"
      " fourty"
      " fourty one"    " fourty two"    " fourty three"
      " fourty four"   " fourty five"   " fourty six"
      " fourty seven"  " fourty eight"  " fourty nine"
      " fifty"
      " fifty one"     " fifty two"     " fifty three"
      " fifty four"    " fifty five"    " fifty six"
      " fifty seven"   " fifty eight"   " fifty nine"
      " sixty"
      " sixty one"     " sixty two"     " sixty three"
      " sixty four"    " sixty five"    " sixty six"
      " sixty seven"   " sixty eight"   " sixty nine"
      " seventy"
      " seventy one"   " seventy two"   " seventy three"
      " seventy four"  " seventy five"  " seventy six"
      " seventy seven" " seventy eight" " seventy nine"
      " eighty"
      " eighty one"    " eighty two"    " eighty three"
      " eighty four"   " eighty five"   " eighty six"
      " eighty seven"  " eighty eight"  " eighty nine"
      " ninety"
      " ninety one"    " ninety two"    " ninety three"
      " ninety four"   " ninety five"   " ninety six"
      " ninety seven"  " ninety eight"  " ninety nine"]
     [""				; 5 - hundreds [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [""				; 6 - hundreds-tens [0-9]
      " one hundred and"
      " two hundred and"
      " three hundred and"
      " four hundred and"
      " five hundred and"
      " six hundred and"
      " seven hundred and"
      " eight hundred and"
      " nine hundred and"]
     [" one"				; 7 - singular period [0-4]
      " one thousand"
      " one million"
      " one milliard"
      " one billion"]
     [""				; 8 - plural period [0-4]
      " thousand"
      " million"
      " milliard"
      " billion"]
     )
    (english-informal-gb		; LANGUAGE
     "zero"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     ""					; 3 - and
     [""				; 4 - tens [0-99]
      " one"           " two"           " three"
      " four"          " five"          " six"
      " seven"         " eight"         " nine"
      " ten"
      " eleven"        " twelve"        " thirteen"
      " fourteen"      " fifteen"       " sixteen"
      " seventeen"     " eighteen"      " nineteen"
      " twenty"
      " twenty one"    " twenty two"    " twenty three"
      " twenty four"   " twenty five"   " twenty six"
      " twenty seven"  " twenty eight"  " twenty nine"
      " thirty"
      " thirty one"    " thirty two"    " thirty three"
      " thirty four"   " thirty five"   " thirty six"
      " thirty seven"  " thirty eight"  " thirty nine"
      " fourty"
      " fourty one"    " fourty two"    " fourty three"
      " fourty four"   " fourty five"   " fourty six"
      " fourty seven"  " fourty eight"  " fourty nine"
      " fifty"
      " fifty one"     " fifty two"     " fifty three"
      " fifty four"    " fifty five"    " fifty six"
      " fifty seven"   " fifty eight"   " fifty nine"
      " sixty"
      " sixty one"     " sixty two"     " sixty three"
      " sixty four"    " sixty five"    " sixty six"
      " sixty seven"   " sixty eight"   " sixty nine"
      " seventy"
      " seventy one"   " seventy two"   " seventy three"
      " seventy four"  " seventy five"  " seventy six"
      " seventy seven" " seventy eight" " seventy nine"
      " eighty"
      " eighty one"    " eighty two"    " eighty three"
      " eighty four"   " eighty five"   " eighty six"
      " eighty seven"  " eighty eight"  " eighty nine"
      " ninety"
      " ninety one"    " ninety two"    " ninety three"
      " ninety four"   " ninety five"   " ninety six"
      " ninety seven"  " ninety eight"  " ninety nine"]
     [""				; 5 - hundreds [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [""				; 6 - hundreds-tens [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [" one"				; 7 - singular period [0-4]
      " one thousand"
      " one million"
      " one milliard"
      " one billion"]
     [""				; 8 - plural period [0-4]
      " thousand"
      " million"
      " milliard"
      " billion"]
     )
    (english-formal-us			; LANGUAGE
     "zero"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     " and"				; 3 - and
     [""				; 4 - tens [0-99]
      " one"           " two"           " three"
      " four"          " five"          " six"
      " seven"         " eight"         " nine"
      " ten"
      " eleven"        " twelve"        " thirteen"
      " fourteen"      " fifteen"       " sixteen"
      " seventeen"     " eighteen"      " nineteen"
      " twenty"
      " twenty-one"    " twenty-two"    " twenty-three"
      " twenty-four"   " twenty-five"   " twenty-six"
      " twenty-seven"  " twenty-eight"  " twenty-nine"
      " thirty"
      " thirty-one"    " thirty-two"    " thirty-three"
      " thirty-four"   " thirty-five"   " thirty-six"
      " thirty-seven"  " thirty-eight"  " thirty-nine"
      " fourty"
      " fourty-one"    " fourty-two"    " fourty-three"
      " fourty-four"   " fourty-five"   " fourty-six"
      " fourty-seven"  " fourty-eight"  " fourty-nine"
      " fifty"
      " fifty-one"     " fifty-two"     " fifty-three"
      " fifty-four"    " fifty-five"    " fifty-six"
      " fifty-seven"   " fifty-eight"   " fifty-nine"
      " sixty"
      " sixty-one"     " sixty-two"     " sixty-three"
      " sixty-four"    " sixty-five"    " sixty-six"
      " sixty-seven"   " sixty-eight"   " sixty-nine"
      " seventy"
      " seventy-one"   " seventy-two"   " seventy-three"
      " seventy-four"  " seventy-five"  " seventy-six"
      " seventy-seven" " seventy-eight" " seventy-nine"
      " eighty"
      " eighty-one"    " eighty-two"    " eighty-three"
      " eighty-four"   " eighty-five"   " eighty-six"
      " eighty-seven"  " eighty-eight"  " eighty-nine"
      " ninety"
      " ninety-one"    " ninety-two"    " ninety-three"
      " ninety-four"   " ninety-five"   " ninety-six"
      " ninety-seven"  " ninety-eight"  " ninety-nine"]
     [""				; 5 - hundreds [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [""				; 6 - hundreds-tens [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [" one"				; 7 - singular period [0-4]
      " one thousand"
      " one million"
      " one billion"
      " one trillion"]
     [""				; 8 - plural period [0-4]
      " thousand"
      " million"
      " billion"
      " trillion"]
     )
    (english-informal-us		; LANGUAGE
     "zero"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     ""					; 3 - and
     [""				; 4 - tens [0-99]
      " one"           " two"           " three"
      " four"          " five"          " six"
      " seven"         " eight"         " nine"
      " ten"
      " eleven"        " twelve"        " thirteen"
      " fourteen"      " fifteen"       " sixteen"
      " seventeen"     " eighteen"      " nineteen"
      " twenty"
      " twenty-one"    " twenty-two"    " twenty-three"
      " twenty-four"   " twenty-five"   " twenty-six"
      " twenty-seven"  " twenty-eight"  " twenty-nine"
      " thirty"
      " thirty-one"    " thirty-two"    " thirty-three"
      " thirty-four"   " thirty-five"   " thirty-six"
      " thirty-seven"  " thirty-eight"  " thirty-nine"
      " fourty"
      " fourty-one"    " fourty-two"    " fourty-three"
      " fourty-four"   " fourty-five"   " fourty-six"
      " fourty-seven"  " fourty-eight"  " fourty-nine"
      " fifty"
      " fifty-one"     " fifty-two"     " fifty-three"
      " fifty-four"    " fifty-five"    " fifty-six"
      " fifty-seven"   " fifty-eight"   " fifty-nine"
      " sixty"
      " sixty-one"     " sixty-two"     " sixty-three"
      " sixty-four"    " sixty-five"    " sixty-six"
      " sixty-seven"   " sixty-eight"   " sixty-nine"
      " seventy"
      " seventy-one"   " seventy-two"   " seventy-three"
      " seventy-four"  " seventy-five"  " seventy-six"
      " seventy-seven" " seventy-eight" " seventy-nine"
      " eighty"
      " eighty-one"    " eighty-two"    " eighty-three"
      " eighty-four"   " eighty-five"   " eighty-six"
      " eighty-seven"  " eighty-eight"  " eighty-nine"
      " ninety"
      " ninety-one"    " ninety-two"    " ninety-three"
      " ninety-four"   " ninety-five"   " ninety-six"
      " ninety-seven"  " ninety-eight"  " ninety-nine"]
     [""				; 5 - hundreds [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [""				; 6 - hundreds-tens [0-9]
      " one hundred"
      " two hundred"
      " three hundred"
      " four hundred"
      " five hundred"
      " six hundred"
      " seven hundred"
      " eight hundred"
      " nine hundred"]
     [" one"				; 7 - singular period [0-4]
      " one thousand"
      " one million"
      " one billion"
      " one trillion"]
     [""				; 8 - plural period [0-4]
      " thousand"
      " million"
      " billion"
      " trillion"]
     )
    ;; it's used char `ü' in `naü' (9) because there is no proper character
    ;; representation.
    (esperanto				; LANGUAGE
     "nulo"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     ""					; 3 - and
     [""				; 4 - tens [0-99]
      " unu"          " du"           " tri"
      " kvar"         " kvin"         " ses"
      " sep"          " ok"           " naü"
      " dek"
      " dek unu"      " dek du"       " dek tri"
      " dek kvar"     " dek kvin"     " dek ses"
      " dek sep"      " dek ok"       " dek naü"
      " dudek"
      " dudek unu"    " dudek du"     " dudek tri"
      " dudek kvar"   " dudek kvin"   " dudek ses"
      " dudek sep"    " dudek ok"     " dudek naü"
      " tridek"
      " tridek unu"   " tridek du"    " tridek tri"
      " tridek kvar"  " tridek kvin"  " tridek ses"
      " tridek sep"   " tridek ok"    " tridek naü"
      " kvardek"
      " kvardek unu"  " kvardek du"   " kvardek tri"
      " kvardek kvar" " kvardek kvin" " kvardek ses"
      " kvardek sep"  " kvardek ok"   " kvardek naü"
      " kvindek"
      " kvindek unu"  " kvindek du"   " kvindek tri"
      " kvindek kvar" " kvindek kvin" " kvindek ses"
      " kvindek sep"  " kvindek ok"   " kvindek naü"
      " sesdek"
      " sesdek unu"   " sesdek du"    " sesdek tri"
      " sesdek kvar"  " sesdek kvin"  " sesdek ses"
      " sesdek sep"   " sesdek ok"    " sesdek naü"
      " sepdek"
      " sepdek unu"   " sepdek du"    " sepdek tri"
      " sepdek kvar"  " sepdek kvin"  " sepdek ses"
      " sepdek sep"   " sepdek ok"    " sepdek naü"
      " okdek"
      " okdek unu"    " okdek du"     " okdek tri"
      " okdek kvar"   " okdek kvin"   " okdek ses"
      " okdek sep"    " okdek ok"     " okdek naü"
      " naüdek"
      " naüdek unu"   " naüdek du"    " naüdek tri"
      " naüdek kvar"  " naüdek kvin"  " naüdek ses"
      " naüdek sep"   " naüdek ok"    " naüdek naü"]
     [""				; 5 - hundreds [0-9]
      " cent"
      " ducent"
      " tricent"
      " kvarcent"
      " kvincent"
      " sescent"
      " sepcent"
      " okcent"
      " naücent"]
     [""				; 6 - hundreds-tens [0-9]
      " cent"
      " ducent"
      " tricent"
      " kvarcent"
      " kvincent"
      " sescent"
      " sepcent"
      " okcent"
      " naücent"]
     [" unu"				; 7 - singular period [0-4]
      " mil"
      " miliono"
      " miliardo"
      " duiliono"]
     [""				; 8 - plural period [0-4]
      " mil"
      " milionoj"
      " miliardoj"
      " duilionoj"]
     )
    (finnish				; LANGUAGE
     "nolla"				; 0 - zero
     ","				; 1 - comma
     "??"				; 2 - minus
     ""					; 3 - and
     [""				; 4 - tens [0-99]
      " yksi"                   " kaksi"                  " kolme"
      " neljä"                  " viisi"                  " kuusi"
      " seitsemän"              " kahdesan"               " yhdeksän"
      " kymmenen"
      " yksitoista"             " kaksitoista"            " kolmetoista"
      " neljätoista"            " viisitoista"            " kuusitoista"
      " seitsemäntoista"        " kahdeksantoista"        " yhdeksäntoista"
      " kaksikymmentä"
      " kaksikymmentäyksi"      " kaksikymmentäkaksi"     " kaksikymmentäkolme"
      " kaksikymmentäneljä"     " kaksikymmentäviisi"     " kaksikymmentäkuusi"
      " kaksikymmentäseitsemän" " kaksikymmentäkahdesan"
      " kaksikymmentäyhdeksän"
      " kolmekymmentä"
      " kolmekymmentäyksi"      " kolmekymmentäkaksi"     " kolmekymmentäkolme"
      " kolmekymmentäneljä"     " kolmekymmentäviisi"     " kolmekymmentäkuusi"
      " kolmekymmentäseitsemän" " kolmekymmentäkahdesan"
      " kolmekymmentäyhdeksän"
      " neljäkymmentä"
      " neljäkymmentäyksi"      " neljäkymmentäkaksi"     " neljäkymmentäkolme"
      " neljäkymmentäneljä"     " neljäkymmentäviisi"     " neljäkymmentäkuusi"
      " neljäkymmentäseitsemän" " neljäkymmentäkahdesan"
      " neljäkymmentäyhdeksän"
      " viisikymmentä"
      " viisikymmentäyksi"      " viisikymmentäkaksi"     " viisikymmentäkolme"
      " viisikymmentäneljä"     " viisikymmentäviisi"     " viisikymmentäkuusi"
      " viisikymmentäseitsemän" " viisikymmentäkahdesan"
      " viisikymmentäyhdeksän"
      " kuusikymmentä"
      " kuusikymmentäyksi"      " kuusikymmentäkaksi"     " kuusikymmentäkolme"
      " kuusikymmentäneljä"     " kuusikymmentäviisi"     " kuusikymmentäkuusi"
      " kuusikymmentäseitsemän" " kuusikymmentäkahdesan"
      " kuusikymmentäyhdeksän"
      " seitsemänkymmentä"
      " seitsemänkymmentäyksi"  " seitsemänkymmentäkaksi"
      " seitsemänkymmentäkolme"
      " seitsemänkymmentäneljä" " seitsemänkymmentäviisi"
      " seitsemänkymmentäkuusi"
      " seitsemänkymmentäseitsemän" " seitsemänkymmentäkahdesan"
      " seitsemänkymmentäyhdeksän"
      " kahdesankymmentä"
      " kahdesankymmentäyksi"   " kahdesankymmentäkaksi"
      " kahdesankymmentäkolme"
      " kahdesankymmentäneljä"  " kahdesankymmentäviisi"
      " kahdesankymmentäkuusi"
      " kahdesankymmentäseitsemän" " kahdesankymmentäkahdesan"
      " kahdesankymmentäyhdeksän"
      " yhdeksänkymmentä"
      " yhdeksänkymmentäyksi"  " yhdeksänkymmentäkaksi" " yhdeksänkymmentäkolme"
      " yhdeksänkymmentäneljä" " yhdeksänkymmentäviisi" " yhdeksänkymmentäkuusi"
      " yhdeksänkymmentäseitsemän" " yhdeksänkymmentäkahdesan"
      " yhdeksänkymmentäyhdeksän"]
     [""				; 5 - hundreds [0-9]
      " sata"
      " kaksisata"
      " kolmesata"
      " neljäsata"
      " viisisata"
      " kuusisata"
      " seitsemänsata"
      " kahdesansata"
      " yhdeksänsata"]
     [""				; 6 - hundreds-tens [0-9]
      " sata"
      " kaksisata"
      " kolmesata"
      " neljäsata"
      " viisisata"
      " kuusisata"
      " seitsemänsata"
      " kahdesansata"
      " yhdeksänsata"]
     [" yksi"				; 7 - singular period [0-4]
      " tuhat"
      " miljoona"
      " ??"
      " ??"]
     [""				; 8 - plural period [0-4]
      " tuhat"
      " miljoona"
      " ??"
      " ??"]
     )
    (french-fr				; LANGUAGE
     "zéro"				; 0 - zero
     ","				; 1 - comma
     "moins"				; 2 - minus
     ""					; 3 - and
     [""				; 4 - tens [0-99]
      " un"                    " deux"                  " trois"
      " quatre"                " cinq"                  " six"
      " sept"                  " huit"                  " neuf"
      " dix"
      " onze"                  " douze"                 " treize"
      " quatorze"              " quinze"                " seize"
      " dix-sept"              " dix-huit"              " dix-neuf"
      " vingt"
      " vingt et un"           " vingt-deux"            " vingt-trois"
      " vingt-quatre"          " vingt-cinq"            " vingt-six"
      " vingt-sept"            " vingt-huit"            " vingt-neuf"
      " trente"
      " trente et un"          " trente-deux"           " trente-trois"
      " trente-quatre"         " trente-cinq"           " trente-six"
      " trente-sept"           " trente-huit"           " trente-neuf"
      " quarante"
      " quarante et un"        " quarante-deux"         " quarante-trois"
      " quarante-quatre"       " quarante-cinq"         " quarante-six"
      " quarante-sept"         " quarante-huit"         " quarante-neuf"
      " cinquente"
      " cinquante et un"       " cinquante-deux"        " cinquante-trois"
      " cinquante-quatre"      " cinquante-cinq"        " cinquante-six"
      " cinquante-sept"        " cinquante-huit"        " cinquante-neuf"
      " soixante"
      " soixante et un"        " soixante-deux"         " soixante-trois"
      " soixante-quatre"       " soixante-cinq"         " soixante-six"
      " soixante-sept"         " soixante-huit"         " soixante-neuf"
      " soixante-dix"
      " soixante et onze"      " soixante-douze"        " soixante-treize"
      " soixante-quatorze"     " soixante-quinze"       " soixante-seize"
      " soixante-dix-sept"     " soixante-dix-huit"     " soixante-dix-neuf"
      " quatre-vingts"
      " quatre-vingt-un"       " quatre-vingt-deux"     " quatre-vingt-trois"
      " quatre-vingt-quatre"   " quatre-vingt-cinq"     " quatre-vingt-six"
      " quatre-vingt-sept"     " quatre-vingt-huit"     " quatre-vingt-neuf"
      " quatre-vingt-dix"
      " quatre-vingt-onze"     " quatre-vingt-douze"    " quatre-vingt-treize"
      " quatre-vingt-quatorze" " quatre-vingt-quinze"   " quatre-vingt-seize"
      " quatre-vingt-dix-sept" " quatre-vingt-dix-huit"
      " quatre-vingt-dix-neuf"]
     [""				; 5 - hundreds [0-9]
      " cent"
      " deux cents"
      " trois cents"
      " quatre cents"
      " cinq cents"
      " six cents"
      " sept cents"
      " huit cents"
      " neuf cents"]
     [""				; 6 - hundreds-tens [0-9]
      " cent"
      " deux cent"
      " trois cent"
      " quatre cent"
      " cinq cent"
      " six cent"
      " sept cent"
      " huit cent"
      " neuf cent"]
     [" un"				; 7 - singular period [0-4]
      " mille"
      " un million"
      " un milliard"
      " un billion"]
     [""				; 8 - plural period [0-4]
      " mille"
      " millions"
      " milliards"
      " billions"]
     )
    (french-ch				; LANGUAGE
     "zéro"				; 0 - zero
     ","				; 1 - comma
     "moins"				; 2 - minus
     ""					; 3 - and
     [""				; 4 - tens [0-99]
      " un"               " deux"           " trois"
      " quatre"           " cinq"           " six"
      " sept"             " huit"           " neuf"
      " dix"
      " onze"             " douze"          " treize"
      " quatorze"         " quinze"         " seize"
      " dix-sept"         " dix-huit"       " dix-neuf"
      " vingt"
      " vingt et un"      " vingt-deux"     " vingt-trois"
      " vingt-quatre"     " vingt-cinq"     " vingt-six"
      " vingt-sept"       " vingt-huit"     " vingt-neuf"
      " trente"
      " trente et un"     " trente-deux"    " trente-trois"
      " trente-quatre"    " trente-cinq"    " trente-six"
      " trente-sept"      " trente-huit"    " trente-neuf"
      " quarante"
      " quarante et un"   " quarante-deux"  " quarante-trois"
      " quarante-quatre"  " quarante-cinq"  " quarante-six"
      " quarante-sept"    " quarante-huit"  " quarante-neuf"
      " cinquente"
      " cinquante et un"  " cinquante-deux" " cinquante-trois"
      " cinquante-quatre" " cinquante-cinq" " cinquante-six"
      " cinquante-sept"   " cinquante-huit" " cinquante-neuf"
      " soixante"
      " soixante et un"   " soixante-deux"  " soixante-trois"
      " soixante-quatre"  " soixante-cinq"  " soixante-six"
      " soixante-sept"    " soixante-huit"  " soixante-neuf"
      " septante"
      " septante et un"   " septante-deux"  " septante-trois"
      " septante-quatre"  " septante-cinq"  " septante-six"
      " septante-sept"    " septante-huit"  " septante-neuf"
      " octante"
      " octante et un"    " octante-deux"   " octante-trois"
      " octante-quatre"   " octante-cinq"   " octante-six"
      " octante-sept"     " octante-huit"   " octante-neuf"
      " nonante"
      " nonante et un"    " nonante-deux"   " nonante-trois"
      " nonante-quatre"   " nonante-cinq"   " nonante-six"
      " nonante-sept"     " nonante-huit"   " nonante-neuf"]
     [""				; 5 - hundreds [0-9]
      " cent"
      " deux cents"
      " trois cents"
      " quatre cents"
      " cinq cents"
      " six cents"
      " sept cents"
      " huit cents"
      " neuf cents"]
     [""				; 6 - hundreds-tens [0-9]
      " cent"
      " deux cent"
      " trois cent"
      " quatre cent"
      " cinq cent"
      " six cent"
      " sept cent"
      " huit cent"
      " neuf cent"]
     [" un"				; 7 - singular period [0-4]
      " mille"
      " un million"
      " un milliard"
      " un billion"]
     [""				; 8 - plural period [0-4]
      " mille"
      " millions"
      " milliards"
      " billions"]
     )
    (german				; LANGUAGE
     "null"				; 0 - zero
     ""					; 1 - comma
     "minus "				; 2 - minus
     ["und" "und "]			; 3 - and
     [""				; 4 - tens [0-99]
      ["eine" "ein" "eins"] "zwei"            "drei"
      "vier"                "fünf"            "sechs"
      "sieben"              "acht"            "neun"
      "zehn"
      "elf"                 "zwölf"           "dreizehn"
      "vierzehn"            "fünfzehn"        "sechzehn"
      "siebzehn"            "achtzehn"        "neunzehn"
      "zwanzig"
      "einundzwanzig"       "zweiundzwanzig"  "dreiundzwanzig"
      "vierundzwanzig"      "fünfundzwanzig"  "sechsundzwanzig"
      "siebenundzwanzig"    "achtundzwanzig"  "neunundzwanzig"
      "dreissig"
      "einunddreissig"      "zweiunddreissig" "dreiunddreissig"
      "vierunddreissig"     "fünfunddreissig" "sechsunddreissig"
      "siebenunddreissig"   "achtunddreissig" "neununddreissig"
      "vierzig"
      "einundvierzig"       "zweiundvierzig"  "dreiundvierzig"
      "vierundvierzig"      "fünfundvierzig"  "sechsundvierzig"
      "siebenundvierzig"    "achtundvierzig"  "neunundvierzig"
      "fünfzig"
      "einundfünfzig"       "zweiundfünfzig"  "dreiundfünfzig"
      "vierundfünfzig"      "fünfundfünfzig"  "sechsundfünfzig"
      "siebenundfünfzig"    "achtundfünfzig"  "neunundfünfzig"
      "sechzig"
      "einundsechzig"       "zweiundsechzig"  "dreiundsechzig"
      "vierundsechzig"      "fünfundsechzig"  "sechsundsechzig"
      "siebenundsechzig"    "achtundsechzig"  "neunundsechzig"
      "siebzig"
      "einundsiebzig"       "zweiundsiebzig"  "dreiundsiebzig"
      "vierundsiebzig"      "fünfundsiebzig"  "sechsundsiebzig"
      "siebenundsiebzig"    "achtundsiebzig"  "neunundsiebzig"
      "achtzig"
      "einundachtzig"       "zweiundachtzig"  "dreiundachtzig"
      "vierundachtzig"      "fünfundachtzig"  "sechsundachtzig"
      "siebenundachtzig"    "achtundachtzig"  "neunundachtzig"
      "neunzig"
      "einundneunzig"       "zweiundneunzig"  "dreiundneunzig"
      "vierundneunzig"      "fünfundneunzig"  "sechsundneunzig"
      "siebenundneunzig"    "achtundneunzig"  "neunundneunzig"]
     [""				; 5 - hundreds [0-9]
      "einhundert"
      "zweihundert"
      "dreihundert"
      "vierhundert"
      "fünfhundert"
      "sechshundert"
      "siebenhundert"
      "achthundert"
      "neunhundert"]
     [""				; 6 - hundreds-tens [0-9]
      "einhundertund"
      "zweihundertund"
      "dreihundertund"
      "vierhundertund"
      "fünfhundertund"
      "sechshundertund"
      "siebenhundertund"
      "achthundertund"
      "neunhundertund"]
     [["eine" "ein" "eins"]		; 7 - singular period [0-4]
      "eintausend"
      "eine Million "
      "eine Milliarde "
      "eine Billion "]
     [""				; 8 - plural period [0-4]
      "tausend"
      " Millionen "
      " Milliarden "
      " Billionen "]
     )
    (italian				; LANGUAGE
     "zero"				; 0 - zero
     ""					; 1 - comma
     "meno "				; 2 - minus
     ""					; 3 - and
     [""				; 4 - tens [0-99]
      "uno"              "due"             "tre"
      "quattro"          "cinque"          "sei"
      "sette"            "otto"            "nove"
      "diece"
      "undici"           "dodici"          "tredici"
      "quattordici"      "quindici"        "sedici"
      "diciasette"       "diciotto"        "dicianove"
      "venti"
      "ventuno"          "ventidue"        "ventitre"
      "ventiquattro"     "venticinque"     "ventisei"
      "ventisette"       "ventotto"        "ventinove"
      "trenta"
      "trentuno"         "trentadue"       "trentatre"
      "trentaquattro"    "trentacinque"    "trentasei"
      "trentasette"      "trentotto"       "trentanove"
      "quaranta"
      "quarantuno"       "quarantadue"     "quarantatre"
      "quarantaquattro"  "quarantacinque"  "quarantasei"
      "quarantasette"    "quarantotto"     "quarantanove"
      "cinquanta"
      "cinquantuno"      "cinquantadue"    "cinquantatre"
      "cinquantaquattro" "cinquantacinque" "cinquantasei"
      "cinquantasette"   "cinquantotto"    "cinquantanove"
      "sessanta"
      "sessantuno"       "sessantadue"     "sessantatre"
      "sessantaquattro"  "sessantacinque"  "sessantasei"
      "sessantasette"    "sessantotto"     "sessantanove"
      "settanta"
      "settantuno"       "settantadue"     "settantatre"
      "settantaquattro"  "settantacinque"  "settantasei"
      "settantasette"    "settantotto"     "settantanove"
      "ottanta"
      "ottantuno"        "ottantadue"      "ottantatre"
      "ottantaquattro"   "ottantacinque"   "ottantasei"
      "ottantasette"     "ottantotto"      "ottantanove"
      "novanta"
      "novantuno"        "novantadue"      "novantatre"
      "novantaquattro"   "novantacinque"   "novantasei"
      "novantasette"     "novantotto"      "novantanove"]
     [""				; 5 - hundreds [0-9]
      "cento"
      "duecento"
      "trecento"
      "quattrocento"
      "cinquecento"
      "seicento"
      "settecento"
      "ottocento"
      "novecento"]
     [""				; 6 - hundreds-tens [0-9]
      "cento"
      "duecento"
      "trecento"
      "quattrocento"
      "cinquecento"
      "seicento"
      "settecento"
      "ottocento"
      "novecento"]
     ["uno"				; 7 - singular period [0-4]
      "mille"
      "un milione "
      "un miliardo "
      "un billón "]
     [""				; 8 - plural period [0-4]
      "mila"
      " milione "
      " miliardo "
      " billón "]
     )
    (japanese				; LANGUAGE
     "zero"				; 0 - zero
     ""					; 1 - comma
     "??"				; 2 - minus
     ""					; 3 - and
     [""				; 4 - tens [0-99]
      " ichi"           " ni"              " san"
      " yon"            " go"              " roku"
      " nana"           " hachi"           " kyuu"
      " juu"
      " juu ichi"       " juu ni"          " juu san"
      " juu yon"        " juu go"          " juu roku"
      " juu nana"       " juu hachi"       " juu kyuu"
      " ni juu"
      " ni juu ichi"    " ni juu ni"       " ni juu san"
      " ni juu yon"     " ni juu go"       " ni juu roku"
      " ni juu nana"    " ni juu hachi"    " ni juu kyuu"
      " san juu"
      " san juu ichi"   " san juu ni"      " san juu san"
      " san juu yon"    " san juu go"      " san juu roku"
      " san juu nana"   " san juu hachi"   " san juu kyuu"
      " yon juu"
      " yon juu ichi"   " yon juu ni"      " yon juu san"
      " yon juu yon"    " yon juu go"      " yon juu roku"
      " yon juu nana"   " yon juu hachi"   " yon juu kyuu"
      " go juu"
      " go juu ichi"    " go juu ni"       " go juu san"
      " go juu yon"     " go juu go"       " go juu roku"
      " go juu nana"    " go juu hachi"    " go juu kyuu"
      " roku juu"
      " roku juu ichi"  " roku juu ni"     " roku juu san"
      " roku juu yon"   " roku juu go"     " roku juu roku"
      " roku juu nana"  " roku juu hachi"  " roku juu kyuu"
      " nana juu"
      " nana juu ichi"  " nana juu ni"     " nana juu san"
      " nana juu yon"   " nana juu go"     " nana juu roku"
      " nana juu nana"  " nana juu hachi"  " nana juu kyuu"
      " hachi juu"
      " hachi juu ichi" " hachi juu ni"    " hachi juu san"
      " hachi juu yon"  " hachi juu go"    " hachi juu roku"
      " hachi juu nana" " hachi juu hachi" " hachi juu kyuu"
      " kyuu juu"
      " kyuu juu ichi"  " kyuu juu ni"     " kyuu juu san"
      " kyuu juu yon"   " kyuu juu go"     " kyuu juu roku"
      " kyuu juu nana"  " kyuu juu hachi"  " kyuu juu kyuu"]
     [""				; 5 - hundreds [0-9]
      " hyaku"
      " ni hyaku"
      " san hyaku"
      " yon hyaku"
      " go hyaku"
      " roku hyaku"
      " nana hyaku"
      " hachi hyaku"
      " kyuu hyaku"]
     [""				; 6 - hundreds-tens [0-9]
      " hyaku"
      " ni hyaku"
      " san hyaku"
      " yon hyaku"
      " go hyaku"
      " roku hyaku"
      " nana hyaku"
      " hachi hyaku"
      " kyuu hyaku"]
     [" ichi"				; 7 - singular period [0-4]
      " sen"
      " hyaku man"
      " ??"
      " ??"]
     [""				; 8 - plural period [0-4]
      " sen"
      " hyaku man"
      " ??"
      " ??"]
     )
    (norwegian				; LANGUAGE
     "null"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     " og"				; 3 - and
     [""				; 4 - tens [0-99]
      " en"         " to"         " tre"
      " fire"       " fem"        " seks"
      " syv"        " åtte"       " ni"
      " ti"
      " elleve"     " tolv"       " tretten"
      " fjorten"    " femten"     " seksten"
      " søtten"     " atten"      " nitten"
      " tyve"
      " tjueen"     " tjueto"     " tjuetre"
      " tjuefire"   " tjuefem"    " tjueseks"
      " tjuesyv"    " tjueåtte"   " tjueni"
      " tretti"
      " trettien"   " trettito"   " trettitre"
      " trettifire" " trettifem"  " trettiseks"
      " trettisyv"  " trettiåtte" " trettini"
      " førti"
      " førtien"    " førtito"    " førtitre"
      " førtifire"  " førtifem"   " førtiseks"
      " førtisyv"   " førtiåtte"  " førtini"
      " femti"
      " femtien"    " femtito"    " femtitre"
      " femtifire"  " femtifem"   " femtiseks"
      " femtisyv"   " femtiåtte"  " femtini"
      " seksti"
      " sekstien"   " sekstito"   " sekstitre"
      " sekstifire" " sekstifem"  " sekstiseks"
      " sekstisyv"  " sekstiåtte" " sekstini"
      " sytti"
      " syttien"    " syttito"    " syttitre"
      " syttifire"  " syttifem"   " syttiseks"
      " syttisyv"   " syttiåtte"  " syttini"
      " åtti"
      " åttien"     " åttito"     " åttitre"
      " åttifire"   " åttifem"    " åttiseks"
      " åttisyv"    " åttiåtte"   " åttini"
      " nitti"
      " nittien"    " nittito"    " nittitre"
      " nittifire"  " nittifem"   " nittiseks"
      " nittisyv"   " nittiåtte"  " nittini"]
     [""				; 5 - hundreds [0-9]
      " ett hundre"
      " to hundre"
      " tre hundre"
      " fire hundre"
      " fem hundre"
      " seks hundre"
      " syv hundre"
      " åtte hundre"
      " ni hundre"]
     [""				; 6 - hundreds-tens [0-9]
      " ett hundre og"
      " to hundre og"
      " tre hundre og"
      " fire hundre og"
      " fem hundre og"
      " seks hundre og"
      " syv hundre og"
      " åtte hundre og"
      " ni hundre og"]
     [" ett"				; 7 - singular period [0-4]
      " ett tusen"
      " en million"
      " en milliard"
      " en billion"]
     [""				; 8 - plural period [0-4]
      " tusen"
      " millioner"
      " milliarder"
      " billioner"]
     )
    (portuguese-br			; LANGUAGE
     "zero"				; 0 - zero
     ","				; 1 - comma
     "menos"				; 2 - minus
     " e"				; 3 - and
     [""				; 4 - tens [0-99]
      [" uma" " um" nil]    [" duas" " dois" nil]   " três"
      " quatro"             " cinco"                " seis"
      " sete"               " oito"                 " nove"
      " dez"
      " onze"               " doze"                 " treze"
      " quatorze"           " quinze"               " dezesseis"
      " dezessete"          " dezoito"              " dezenove"
      " vinte"
      [" vinte e uma"  " vinte e um"   nil]
      [" vinte e duas" " vinte e dois" nil]         " vinte e três"
      " vinte e quatro"     " vinte e cinco"        " vinte e seis"
      " vinte e sete"       " vinte e oito"         " vinte e nove"
      " trinta"
      [" trinta e uma"  " trinta e um"   nil]
      [" trinta e duas" " trinta e dois" nil]       " trinta e três"
      " trinta e quatro"    " trinta e cinco"       " trinta e seis"
      " trinta e sete"      " trinta e oito"        " trinta e nove"
      " quarenta"
      [" quarenta e uma"  " quarenta e um"   nil]
      [" quarenta e duas" " quarenta e dois" nil]   " quarenta e três"
      " quarenta e quatro"  " quarenta e cinco"     " quarenta e seis"
      " quarenta e sete"    " quarenta e oito"      " quarenta e nove"
      " cinqüenta"
      [" cinqüenta e uma"  " cinqüenta e um"   nil]
      [" cinqüenta e duas" " cinqüenta e dois" nil] " cinqüenta e três"
      " cinqüenta e quatro" " cinqüenta e cinco"    " cinqüenta e seis"
      " cinqüenta e sete"   " cinqüenta e oito"     " cinqüenta e nove"
      " sessenta"
      [" sessenta e uma" " sessenta e um" nil]
      [" sessenta e duas" " sessenta e dois" nil]   " sessenta e três"
      " sessenta e quatro"  " sessenta e cinco"     " sessenta e seis"
      " sessenta e sete"    " sessenta e oito"      " sessenta e nove"
      " setenta"
      [" setenta e uma"  " setenta e um"   nil]
      [" setenta e duas" " setenta e dois" nil]     " setenta e três"
      " setenta e quatro"   " setenta e cinco"      " setenta e seis"
      " setenta e sete"     " setenta e oito"       " setenta e nove"
      " oitenta"
      [" oitenta e uma"  " oitenta e um"   nil]
      [" oitenta e duas" " oitenta e dois" nil]     " oitenta e três"
      " oitenta e quatro"   " oitenta e cinco"      " oitenta e seis"
      " oitenta e sete"     " oitenta e oito"       " oitenta e nove"
      " noventa"
      [" noventa e uma"  " noventa e um"   nil]
      [" noventa e duas" " noventa e dois" nil]     " noventa e três"
      " noventa e quatro"   " noventa e cinco"      " noventa e seis"
      " noventa e sete"     " noventa e oito"       " noventa e nove"]
     [""				; 5 - hundreds [0-9]
      " cem"
      [" duzentas"     " duzentos"     nil]
      [" trezentas"    " trezentos"    nil]
      [" quatrocentas" " quatrocentos" nil]
      [" quinhentas"   " quinhentos"   nil]
      [" seiscentas"   " seiscentos"   nil]
      [" setecentas"   " setecentos"   nil]
      [" oitocentas"   " oitocentos"   nil]
      [" novecentas"   " novecentos"   nil]]
     [""				; 6 - hundreds-tens [0-9]
      " cento e"
      [" duzentas e"     " duzentos e"     nil]
      [" trezentas e"    " trezentos e"    nil]
      [" quatrocentas e" " quatrocentos e" nil]
      [" quinhentas e"   " quinhentos e"   nil]
      [" seiscentas e"   " seiscentos e"   nil]
      [" setecentas e"   " setecentos e"   nil]
      [" oitocentas e"   " oitocentos e"   nil]
      [" novecentas e"   " novecentos e"   nil]]
     [[" uma" " um" nil]		; 7 - singular period [0-4]
      " um mil"
      " um milhão"
      " um bilhão"
      " um trilhão"]
     [""				; 8 - plural period [0-4]
      " mil"
      " milhões"
      " bilhões"
      " trilhões"]
     )
    (portuguese-pt			; LANGUAGE
     "zero"				; 0 - zero
     ","				; 1 - comma
     "menos"				; 2 - minus
     " e"				; 3 - and
     [""				; 4 - tens [0-99]
      [" uma" " um" nil]    [" duas" " dois" nil]   " três"
      " quatro"             " cinco"                " seis"
      " sete"               " oito"                 " nove"
      " dez"
      " onze"               " doze"                 " treze"
      " catorze"            " quinze"               " dezasseis"
      " dezassete"          " dezoito"              " dezanove"
      " vinte"
      [" vinte e uma"  " vinte e um"   nil]
      [" vinte e duas" " vinte e dois" nil]         " vinte e três"
      " vinte e quatro"     " vinte e cinco"        " vinte e seis"
      " vinte e sete"       " vinte e oito"         " vinte e nove"
      " trinta"
      [" trinta e uma"  " trinta e um"   nil]
      [" trinta e duas" " trinta e dois" nil]       " trinta e três"
      " trinta e quatro"    " trinta e cinco"       " trinta e seis"
      " trinta e sete"      " trinta e oito"        " trinta e nove"
      " quarenta"
      [" quarenta e uma"  " quarenta e um"   nil]
      [" quarenta e duas" " quarenta e dois" nil]   " quarenta e três"
      " quarenta e quatro"  " quarenta e cinco"     " quarenta e seis"
      " quarenta e sete"    " quarenta e oito"      " quarenta e nove"
      " cinqüenta"
      [" cinqüenta e uma"  " cinqüenta e um"   nil]
      [" cinqüenta e duas" " cinqüenta e dois" nil] " cinqüenta e três"
      " cinqüenta e quatro" " cinqüenta e cinco"    " cinqüenta e seis"
      " cinqüenta e sete"   " cinqüenta e oito"     " cinqüenta e nove"
      " sessenta"
      [" sessenta e uma" " sessenta e um" nil]
      [" sessenta e duas" " sessenta e dois" nil]   " sessenta e três"
      " sessenta e quatro"  " sessenta e cinco"     " sessenta e seis"
      " sessenta e sete"    " sessenta e oito"      " sessenta e nove"
      " setenta"
      [" setenta e uma"  " setenta e um"   nil]
      [" setenta e duas" " setenta e dois" nil]     " setenta e três"
      " setenta e quatro"   " setenta e cinco"      " setenta e seis"
      " setenta e sete"     " setenta e oito"       " setenta e nove"
      " oitenta"
      [" oitenta e uma"  " oitenta e um"   nil]
      [" oitenta e duas" " oitenta e dois" nil]     " oitenta e três"
      " oitenta e quatro"   " oitenta e cinco"      " oitenta e seis"
      " oitenta e sete"     " oitenta e oito"       " oitenta e nove"
      " noventa"
      [" noventa e uma"  " noventa e um"   nil]
      [" noventa e duas" " noventa e dois" nil]     " noventa e três"
      " noventa e quatro"   " noventa e cinco"      " noventa e seis"
      " noventa e sete"     " noventa e oito"       " noventa e nove"]
     [""				; 5 - hundreds [0-9]
      " cem"
      [" duzentas"     " duzentos"     nil]
      [" trezentas"    " trezentos"    nil]
      [" quatrocentas" " quatrocentos" nil]
      [" quinhentas"   " quinhentos"   nil]
      [" seiscentas"   " seiscentos"   nil]
      [" setecentas"   " setecentos"   nil]
      [" oitocentas"   " oitocentos"   nil]
      [" novecentas"   " novecentos"   nil]]
     [""				; 6 - hundreds-tens [0-9]
      " cento e"
      [" duzentas e"     " duzentos e"     nil]
      [" trezentas e"    " trezentos e"    nil]
      [" quatrocentas e" " quatrocentos e" nil]
      [" quinhentas e"   " quinhentos e"   nil]
      [" seiscentas e"   " seiscentos e"   nil]
      [" setecentas e"   " setecentos e"   nil]
      [" oitocentas e"   " oitocentos e"   nil]
      [" novecentas e"   " novecentos e"   nil]]
     [[" uma" " um" nil]		; 7 - singular period [0-4]
      " um mil"
      " um milhão"
      " um milhar de milhão"
      " um bilhão"]
     [""				; 8 - plural period [0-4]
      " mil"
      " milhões"
      " milhares de milhão"
      " bilhões"]
     )
    (spanish				; LANGUAGE
     "cero"				; 0 - zero
     ""					; 1 - comma
     "menos"				; 2 - minus
     " y"				; 3 - and
     [""				; 4 - tens [0-99]
      [" una" " un" "uno"]  " dos"               " tres"
      " cuatro"             " cinco"             " seis"
      " siete"              " ocho"              " nueve"
      " diez"
      " once"               " doce"              " trece"
      " catorce"            " quince"            " dieciséis"
      " diecisiete"         " dieciocho"         " diecinueve"
      " veinte"
      [" veintiuna" " veintiun" " veintiuno"]
      " veintidós"          " veintitrés"
      " veinticuatro"       " veinticinco"       " veintiseis"
      " veintisiete"        " veintiocho"        " veintinueve"
      " treinta"
      [" treinta y una" " treinta y un" " treinta y uno"]
      " treinta y dos"      " treinta y tres"
      " treinta y cuatro"   " treinta y cinco"   " treinta y seis"
      " treinta y siete"    " treinta y ocho"    " treinta y nueve"
      " cuarenta"
      [" cuarenta y una" " cuarenta y un" " cuarenta y uno"]
      " cuarenta y dos"     " cuarenta y tres"
      " cuarenta y cuatro"  " cuarenta y cinco"  " cuarenta y seis"
      " cuarenta y siete"   " cuarenta y ocho"   " cuarenta y nueve"
      " cincuenta"
      [" cincuenta y una" " cincuenta y un" " cincuenta y uno"]
      " cincuenta y dos"    " cincuenta y tres"
      " cincuenta y cuatro" " cincuenta y cinco" " cincuenta y seis"
      " cincuenta y siete"  " cincuenta y ocho"  " cincuenta y nueve"
      " sesenta"
      [" sesenta y una" " sesenta y un" " sesenta y uno"]
      " sesenta y dos"      " sesenta y tres"
      " sesenta y cuatro"   " sesenta y cinco"   " sesenta y seis"
      " sesenta y siete"    " sesenta y ocho"    " sesenta y nueve"
      " setenta"
      [" setenta y una" " setenta y un" " setenta y uno"]
      " setenta y dos"      " setenta y tres"
      " setenta y cuatro"   " setenta y cinco"   " setenta y seis"
      " setenta y siete"    " setenta y ocho"    " setenta y nueve"
      " ochenta"
      [" ochenta y una" " ochenta y un" " ochenta y uno"]
      " ochenta y dos"      " ochenta y tres"
      " ochenta y cuatro"   " ochenta y cinco"   " ochenta y seis"
      " ochenta y siete"    " ochenta y ocho"    " ochenta y nueve"
      " noventa"
      [" noventa y una" " noventa y un" " noventa y uno"]
      " noventa y dos"      " noventa y tres"
      " noventa y cuatro"   " noventa y cinco"   " noventa y seis"
      " noventa y siete"    " noventa y ocho"    " noventa y nueve"]
     [""				; 5 - hundreds [0-9]
      " cien"
      [" doscientas"    " doscientos"    nil]
      [" trescientas"   " trescientos"   nil]
      [" cuatrocientas" " cuatrocientos" nil]
      [" quinientas"    " quinientos"    nil]
      [" seiscientas"   " seiscientos"   nil]
      [" setecientas"   " setecientos"   nil]
      [" ochocientas"   " ochocientos"   nil]
      [" novecientas"   " novecientos"   nil]]
     [""				; 6 - hundreds-tens [0-9]
      " ciento"
      [" doscientas"    " doscientos"    nil]
      [" trescientas"   " trescientos"   nil]
      [" cuatrocientas" " cuatrocientos" nil]
      [" quinientas"    " quinientos"    nil]
      [" seiscientas"   " seiscientos"   nil]
      [" setecientas"   " setecientos"   nil]
      [" ochocientas"   " ochocientos"   nil]
      [" novecientas"   " novecientos"   nil]]
     [[" una" " un" " uno"]		; 7 - singular period [0-4]
      " mil"
      " un millón"
      " un mil millón"
      " un billón"]
     [""				; 8 - plural period [0-4]
      " mil"
      " millones"
      " mil millones"
      " billones"]
     )
    (swedish				; LANGUAGE
     "null"				; 0 - zero
     ","				; 1 - comma
     "minus"				; 2 - minus
     " och"				; 3 - and
     [""				; 4 - tens [0-99]
      " ett"        " två"        " tre"
      " fyra"       " fem"        " sex"
      " sju"        " åtta"       " nio"
      " tio"
      " elva"       " tolv"       " tretton"
      " fjorton"    " femton"     " sexton"
      " sjutton"    " arton"      " nitton"
      " tjugo"
      " tjugoett"   " tjugotvå"   " tjugotre"
      " tjugofyra"  " tjugofem"   " tjugosex"
      " tjugosju"   " tjugoåtta"  " tjugonio"
      " trettio"
      " trettiett"  " trettitvå"  " trettitre"
      " trettifyra" " trettifem"  " trettisex"
      " trettisju"  " trettiåtta" " trettinio"
      " fyrtio"
      " fyrtiett"   " fyrtitvå"   " fyrtitre"
      " fyrtifyra"  " fyrtifem"   " fyrtisex"
      " fyrtisju"   " fyrtiåtta"  " fyrtinio"
      " femtio"
      " femtiett"   " femtitvå"   " femtitre"
      " femtifyra"  " femtifem"   " femtisex"
      " femtisju"   " femtiåtta"  " femtinio"
      " sextio"
      " sextiett"   " sextitvå"   " sextitre"
      " sextifyra"  " sextifem"   " sextisex"
      " sextisju"   " sextiåtta"  " sextinio"
      " sjuttio"
      " sjuttiett"  " sjuttitvå"  " sjuttitre"
      " sjuttifyra" " sjuttifem"  " sjuttisex"
      " sjuttisju"  " sjuttiåtta" " sjuttinio"
      " åttio"
      " åttiett"    " åttitvå"    " åttitre"
      " åttifyra"   " åttifem"    " åttisex"
      " åttisju"    " åttiåtta"   " åttinio"
      " nittio"
      " nittiett"   " nittitvå"   " nittitre"
      " nittifyra"  " nittifem"   " nittisex"
      " nittisju"   " nittiåtta"  " nittinio"]
     [""				; 5 - hundreds [0-9]
      " ett hundra"
      " två hundra"
      " tre hundra"
      " fyra hundra"
      " fem hundra"
      " sex hundra"
      " sju hundra"
      " åtta hundra"
      " nio hundra"]
     [""				; 6 - hundreds-tens [0-9]
      " ett hundra och"
      " två hundra och"
      " tre hundra och"
      " fyra hundra och"
      " fem hundra och"
      " sex hundra och"
      " sju hundra och"
      " åtta hundra och"
      " nio hundra och"]
     [" ett"				; 7 - singular period [0-4]
      " ett tusen"
      " en milljon"
      " en milljard"
      " en billjon"]
     [""				; 8 - plural period [0-4]
      " tusen"
      " milljoner"
      " milljarder"
      " billjoner"]
     )
    )
  "Alist where each element has the following form:

    (LANGUAGE
     ;; 0 - zero
     0
     ;; 1 - comma
     \",\"
     ;; 2 - minus (negative numbers)
     \"minus\"
     ;; 3 - and (for example: one thousand AND one)
     ;;         (special case in some languages, see text below)
     \"\"
     ;; 4 - tens [0-99]
     [\"\"
      1  2  3
      4  5  6
      7  8  9
      10
      11 12 13
      14 15 16
      17 18 19
      20
      ...
      97 98 99]
     ;; 5 - hundreds [0-9] (for example: one hundred)
     [\"\" 100 200 .. 900]
     ;; 6 - hundreds-tens [0-9] (for example: one hundred AND ten)
     ;;                         (special case in some languages)
     [\"\" 100 200 .. 900]
     ;; 7 - singular period (like one, one thousand, one million, etc.)
     [1
      1,000
      1,000,000
      1,000,000,000
      1,000,000,000,000]
     ;; 8 - plural period (like thousands, millions, etc.)
     [\"\"
      1,000
      1,000,000
      1,000,000,000
      1,000,000,000,000]
     )

The numbers should be spelled out in a string or in a vector like:

   [FEMININE MASCULINE NEUTER]

Where FEMININE, MASCULINE and NEUTER are strings representing number in
feminine, masculine and neuter gender, respectively.  If there is no neuter, it
should be nil.

The string representation is used when all gender (feminine, masculine and
neuter) has the same spelling.

Some language may have two ways to express \"and\" in `;; 3 - and', one for
numbers until million (like one thousand AND one) and other for numbers above
million (like one million AND one), in this case, instead of a string, use a
vector like:

   [\"one and\" \"other and\"]

For an example, see `german' entry.")


(defconst spell-currency-database
  '(
    (andorra-french			; COUNTRY (AD)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (andorra-spanish			; COUNTRY (AD)
     [" peseta" " pesetas" " y " feminine] ; 0 - currency
     [" céntimo" " céntimos" 100 masculine] ; 1 - fractional
     )
    (antigua-and-barbuda		; COUNTRY (AG)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (argentina				; COUNTRY (AR)
     [" peso" " pesos" " y " masculine]	; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (australia				; COUNTRY (AU)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (austria				; COUNTRY (AT)
     [" Schilling" " Schilling" " und " neuter] ; 0 - currency
     [" Groschen" " Groschen" 100 neuter] ; 1 - fractional
     )
    (bahamas				; COUNTRY (BS)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (barbados				; COUNTRY (BB)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (belgium				; COUNTRY (BE)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (belize				; COUNTRY (BZ)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (benin				; COUNTRY (BJ)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (bolivia				; COUNTRY (BO)
     [" boliviano" " bolivianos" " y " masculine] ; 0 - currency
     [" peso" " pesos" 1000 masculine]	; 1 - fractional
     )
    (brazil				; COUNTRY (BR)
     [" real" " reais" " e " masculine]	; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (brunei				; COUNTRY (BN)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (burkina-faso			; COUNTRY (BF)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (burundi				; COUNTRY (BI)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (cameroon				; COUNTRY (CM)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (canada				; COUNTRY (CA)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (cape-verde				; COUNTRY (CV)
     [" escudo" " escudos" " e " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (central-african-republic		; COUNTRY (CF)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (chad				; COUNTRY (TD)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (chile				; COUNTRY (CL)
     [" peso" " pesos" " y " masculine]	; 0 - currency
     [" centésimo" " centésimos" 100 masculine] ; 1 - fractional
     )
    (colombia				; COUNTRY (CO)
     [" peso" " pesos" " y " masculine]	; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (comoros				; COUNTRY (KM)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (congo				; COUNTRY (CG)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (costa-rica				; COUNTRY (CR)
     [" colón" " colones" " y " masculine] ; 0 - currency
     [" céntimo" " céntimos" 100 masculine] ; 1 - fractional
     )
    (cuba				; COUNTRY (CU)
     [" peso" " pesos" " y " masculine]	; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (cyprus				; COUNTRY (CY)
     [" pound" " pounds" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (denmark				; COUNTRY (DK)
     [" krone" " kroner" " og " neuter] ; 0 - currency
     [" øre" " ører" 100 neuter]	; 1 - fractional
     )
    (djibouti				; COUNTRY (DJ)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (dominica				; COUNTRY (DM)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (dominican-republic			; COUNTRY (DO)
     [" peso" " pesos" " y " masculine]	; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (ecuador				; COUNTRY (EC)
     [" sucre" " sucres" " y " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (el-salvador			; COUNTRY (SV)
     [" colón" " colones" " y " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (equatorial-guinea			; COUNTRY (GQ)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (fiji				; COUNTRY (FJ)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (finland				; COUNTRY (FI)
     [" markka" " markka" " ?? " neuter] ; 0 - currency
     [" penni" " penni" 100 neuter]	; 1 - fractional
     )
    (france				; COUNTRY (FR)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (gabon				; COUNTRY (GA)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (germany				; COUNTRY (DE)
     [" Mark" " Mark" " und " feminine]	; 0 - currency
     [" Pfennig" " Pfennig" 100 masculine] ; 1 - fractional
     )
    (grenada				; COUNTRY (GD)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (guatemala				; COUNTRY (GT)
     [" quetzal" " quetzals" " y " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (guinea				; COUNTRY (GN)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (guinea-bissau			; COUNTRY (GW)
     [" peso" " pesos" " y " masculine]	; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (guyana				; COUNTRY (GY)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (haiti				; COUNTRY (HT)
     [" gourde" " gourdes" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (honduras				; COUNTRY (HN)
     [" lempira" " lempiras" " y " feminine] ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (ireland				; COUNTRY (IE)
     [" pound" " pounds" " and " neuter] ; 0 - currency
     [" penny" " pennies" 100 neuter]	; 1 - fractional
     )
    (italy				; COUNTRY (IT)
     [" lira" " lire" " e " neuter]	; 0 - currency
     [" centesimo" " centesimo" 100 neuter] ; 1 - fractional
     )
    (ivory-coast			; COUNTRY (CI) Cote d'Ivoire
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (jamaica				; COUNTRY (JM)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (japan				; COUNTRY (JP)
     [" yen" " yen" " ?? " neuter]	; 0 - currency
     [" sen" " sen" 100 neuter]		; 1 - fractional
     )
    (kenya				; COUNTRY (KE)
     [" shilling" " shillings" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (kiribati				; COUNTRY (KI)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (liberia				; COUNTRY (LR)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (liechtenstein			; COUNTRY (LI)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (luxembourg				; COUNTRY (LU)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (madagascar				; COUNTRY (MG)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (mali				; COUNTRY (ML)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (mexico				; COUNTRY (MX)
     [" peso" " pesos" " y " masculine]	; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (monaco				; COUNTRY (MC)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (mozambique				; COUNTRY (MZ)
     [" metical" " meticals" " e " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (namibia				; COUNTRY (NA)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (nauru				; COUNTRY (??)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (netherlands			; COUNTRY (NL)
     [" guilder" " guilder" " en " masculine] ; 0 - currency
     [" cent" " cent" 100 masculine]	; 1 - fractional
     )
    (new-zealand			; COUNTRY (NZ)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (nicaragua				; COUNTRY (NI)
     [" córdoba" " córdobas" " y " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (niger				; COUNTRY (NE)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (norway				; COUNTRY (NO)
     [" krone" " kroner" " og " neuter] ; 0 - currency
     [" øre" " ører" 100 neuter]	; 1 - fractional
     )
    (panama				; COUNTRY (PP)
     [" balboa" " balboas" " y " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (paraguay				; COUNTRY (PY)
     [" guaraní" " guaranís" " y " neuter] ; 0 - currency
     [" céntimo" " céntimos" 100 masculine] ; 1 - fractional
     )
    (peru				; COUNTRY (PE)
     [" sol" " soles" " y " neuter]	; 0 - currency
     [" céntimo" " céntimos" 100 masculine] ; 1 - fractional
     )
    (philippines			; COUNTRY (PH)
     [" peso" " pesos" " y " masculine]	; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (portugal				; COUNTRY (PT)
     [" escudo" " escudos" " e " masculine] ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (rwanda				; COUNTRY (RW)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (sao-tome-and-principe		; COUNTRY (ST)
     [" dobra" " dobras" " y " feminine] ; 0 - currency
     [" centavo" " centavos" 100 masculine] ; 1 - fractional
     )
    (senegal				; COUNTRY (SN)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (singapore				; COUNTRY (SG)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (solomon-islands			; COUNTRY (SB)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (somalia				; COUNTRY (SO)
     [" shilling" " shillings" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (south-africa			; COUNTRY (ZA)
     [" rand" " rands" " and " neuter]	; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (spain				; COUNTRY (ES)
     [" peseta" " pesetas" " y " feminine] ; 0 - currency
     [" céntimo" " céntimos" 100 masculine] ; 1 - fractional
     )
    (st-kitts-and-nevis			; COUNTRY (KN)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (st-lucia				; COUNTRY (LC)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (st-vicent-and-grenadines		; COUNTRY (VC)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (sweden				; COUNTRY (SE)
     [" krona" " kronor" " och " neuter] ; 0 - currency
     [" öre" " ören" 100 neuter]	; 1 - cent
     )
    (switzerland			; COUNTRY (CH)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (taiwan				; COUNTRY (TW)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (tanzania				; COUNTRY (TZ)
     [" shilling" " shillings" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (togo				; COUNTRY (TG)
     [" franc" " francs" " et " neuter] ; 0 - currency
     [" centime" " centimes" 100 neuter] ; 1 - fractional
     )
    (trinidad-and-tobago		; COUNTRY (TT)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (tuvalu				; COUNTRY (TV)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (uganda				; COUNTRY (UG)
     [" shilling" " shillings" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (united-kingdom			; COUNTRY (GB)
     [" pound" " pounds" " and " neuter] ; 0 - currency
     [" penny" " pennies" 100 neuter]	; 1 - fractional
     )
    (united-states			; COUNTRY (US)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    (uruguay				; COUNTRY (UY)
     [" peso" " pesos" " y " masculine]	; 0 - currency
     [" centésimo" " centésimos" 100 masculine] ; 1 - fractional
     )
    (venezuela				; COUNTRY (VE)
     [" bolívar" " bolívares" " y " masculine] ; 0 - currency
     [" céntimo" " céntimos" 100 masculine] ; 1 - fractional
     )
    (zimbabwe				; COUNTRY (ZW)
     [" dollar" " dollars" " and " neuter] ; 0 - currency
     [" cent" " cents" 100 neuter]	; 1 - fractional
     )
    )
  "Alist where each element has the following form:

    (COUNTRY
     ;; 0 - currency
     [\" dollar\" \" dollars\" \" and \" GENDER]
     ;; 1 - fractional
     [\" cent\" \" cents\" 100 GENDER]
     )

Where GENDER indicates the word gender.  Valid values are `feminine',
`masculine' or `neuter'.  Any other value is treated as `neuter'.")


(defun spell-string-match (match str)
  (let (case-fold-search)
    (or (and (string-match match str)
	     (= (match-beginning 0) 0)
	     (= (match-end 0) (length str)))
	(error "Invalid numeric string for spelling out: %S" str))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization


;;;###autoload
(defun spell-number-customize ()
  "Customize spell-number options."
  (interactive)
  (customize-group 'spell-number))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Commands:


;;;###autoload
(defun spell-integer-in-words (int &optional gender-sym)
  "Return the spelling of integer INT in words.

Optionally, GENDER-SYM specifies the spelling gender.
Valid values for GENDER-SYM are: 'feminine, 'masculine or 'neuter.
Any other value is treated as 'neuter."
  (interactive "nInteger to spell out in words: ")
  (let* ((integer-base (cdr (assq spell-language spell-language-database)))
	 (spell-zero            (nth 0 integer-base))
	 (spell-comma           (nth 1 integer-base))
	 (spell-minus           (nth 2 integer-base))
	 (spell-and             (nth 3 integer-base))
	 (spell-tens            (nth 4 integer-base))
	 (spell-hundreds        (nth 5 integer-base))
	 (spell-hundreds-tens   (nth 6 integer-base))
	 (spell-singular-period (nth 7 integer-base))
	 (spell-plural-period   (nth 8 integer-base))
	 (spell                 (and integer-base
				     (spell-int int gender-sym))))
    (and spell
	 (interactive-p)
	 (message spell))
    spell))


;;;###autoload
(defun spell-currency-in-words (value)
  "Return the spelling of number VALUE as a currency in words."
  (interactive "nCurrency to spell out in words: ")
  (let* ((integer-base (cdr (assq spell-language spell-language-database)))
	 (spell-zero            (nth 0 integer-base))
	 (spell-comma           (nth 1 integer-base))
	 (spell-minus           (nth 2 integer-base))
	 (spell-and             (nth 3 integer-base))
	 (spell-tens            (nth 4 integer-base))
	 (spell-hundreds        (nth 5 integer-base))
	 (spell-hundreds-tens   (nth 6 integer-base))
	 (spell-singular-period (nth 7 integer-base))
	 (spell-plural-period   (nth 8 integer-base))
	 (country-base (cdr (assq spell-country spell-currency-database)))
	 (spell-currency        (nth 0 country-base))
	 (spell-fractional      (nth 1 country-base))
	 (money (truncate value))
	 (frac  (abs (round (* (- value money) (aref spell-fractional 2)))))
	 (spell (and integer-base country-base
		     (concat
		      (spell-int money (aref spell-currency 3))	; value
		      (aref spell-currency ; currency
			    (if (or (= money 1) (= money -1))
				0
			      1))
		      (and (or (/= frac 0) ; fractional spelling
			       spell-zero-cents)
			   (concat (aref spell-currency 2) ; and
				   (spell-int frac ; value
					      (aref spell-fractional 3))
				   (aref spell-fractional ; fractional
					 (if (= frac 1)
					     0
					   1))))))))
    (and spell
	 (interactive-p)
	 (message spell))
    spell))


;;;###autoload
(defun spell-numeric-string-in-words (str &optional gender-sym)
  "Return the spelling of a numeric string STR in words.

STR should match the regexp \"[-+]?[0-9P]+\", where P is the value of variable
`spell-period-character'.

For: (setq spell-period-character ?,)
A valid numeric string is \"+1,234,567\" or \"1234567\".

Optionally, GENDER-SYM specifies the spelling gender.
Valid values for GENDER-SYM are: 'feminine, 'masculine or 'neuter.
Any other value is treated as 'neuter."
  (interactive "sNumeric string to spell out in words: ")
  (let* ((integer-base (cdr (assq spell-language spell-language-database)))
	 (spell-zero            (nth 0 integer-base))
	 (spell-comma           (nth 1 integer-base))
	 (spell-minus           (nth 2 integer-base))
	 (spell-and             (nth 3 integer-base))
	 (spell-tens            (nth 4 integer-base))
	 (spell-hundreds        (nth 5 integer-base))
	 (spell-hundreds-tens   (nth 6 integer-base))
	 (spell-singular-period (nth 7 integer-base))
	 (spell-plural-period   (nth 8 integer-base))
	 (spell (and (save-match-data
		       (spell-string-match
			(concat "[-+]?[0-9"
				(regexp-quote
				 (char-to-string spell-period-character))
				"]+")
			str))
		     (cdr (spell-str str gender-sym)))))
    (and spell
	 (interactive-p)
	 (message spell))
    spell))


;;;###autoload
(defun spell-currency-string-in-words (value)
  "Return the spelling of numeric string VALUE as a currency in words.

VALUE should match the regexp \"[-+]?[0-9P]+\\(D[0-9]*\\)?\", where P is the
value of variable `spell-period-character' and D is the value of variable
`spell-decimal-character'.

For: (setq spell-period-character ?,
	   spell-decimal-character ?.)
A valid numeric string is \"+1,234,567.89\" or \"1234567.89\"."
  (interactive "sCurrency string to spell out in words: ")
  (let* ((integer-base (cdr (assq spell-language spell-language-database)))
	 (spell-zero            (nth 0 integer-base))
	 (spell-comma           (nth 1 integer-base))
	 (spell-minus           (nth 2 integer-base))
	 (spell-and             (nth 3 integer-base))
	 (spell-tens            (nth 4 integer-base))
	 (spell-hundreds        (nth 5 integer-base))
	 (spell-hundreds-tens   (nth 6 integer-base))
	 (spell-singular-period (nth 7 integer-base))
	 (spell-plural-period   (nth 8 integer-base))
	 (country-base (cdr (assq spell-country spell-currency-database)))
	 (spell-currency        (nth 0 country-base))
	 (spell-fractional      (nth 1 country-base))
	 decimal
	 (spell
	  (and integer-base country-base
	       (save-match-data
		 (and (spell-string-match
		       (concat "[-+]?[0-9"
			       (regexp-quote
				(char-to-string spell-period-character))
			       "]+\\("
			       (regexp-quote
				(char-to-string spell-decimal-character))
			       "[0-9]*\\)?")
		       value)
		      (progn
			(setq decimal (match-beginning 1)) ; decimal point
			t)))
	       (let ((money (spell-str (if decimal
					   (substring value 0 decimal)
					 value)
				       (aref spell-currency 3)))
		     (frac  (spell-str (if decimal
					   (substring value (1+ decimal))
					 "0")
				       (aref spell-fractional 3)
				       (truncate
					(log10 (aref spell-fractional 2))))))
		 (concat (cdr money)	; value
			 (aref spell-currency ; currency
			       (if (= (car money) 1)
				   0
				 1))
			 (and (or (/= (car frac) 0) ; fractional spelling
				  spell-zero-cents)
			      (concat (aref spell-currency 2) ; and
				      (cdr frac) ; value
				      (aref spell-fractional ; fractional
					    (if (= (car frac) 1)
						0
					      1)))))))))
    (and spell
	 (interactive-p)
	 (message spell))
    spell))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'spell-number)


;;; spell-number.el ends here
