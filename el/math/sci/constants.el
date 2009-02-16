;;; constants.el --- enter definition of constants into source code
;; Copyright (c) 2003, 2004, 2005 Carsten Dominik

;; Author: Carsten Dominik <dominik@science.uva.nl>
;; Version: 2.0
;; Keywords: programming, languages

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;---------------------------------------------------------------------------
;;
;;; Commentary:
;;
;; When I write small programs to calculate something, I often need
;; the values of some physical constants and units.  I could of course
;; alway link a big module with all those definitions.  But often I
;; want the program to run stand-allone, so I prefer to define
;; variables for these constants directly.  This package provides the
;; command `constants-insert'.  It prompts for one or more variable
;; names and inserts definition statements for numerical constants
;; into source code.  It does this in the appropriate syntax for many
;; different programming languages.
;; There are also the commands `constants-get' and `constants-replace'
;; which just dieplay the value of a constant in the echo area, or replace
;; the name of a constant in a buffer with its value.
;;
;; The package knows many constants and units, both in the SI and in
;; the cgs unit system.  It also understands the usual unit prefixes
;; (like "M" for Mega=10^6 or "m" for milli=10^-3).  So the built-in
;; constant "pc" (parsec, an astronomical distance), can also be used
;; in kpc (1000 pc), Mpc (10^6 pc) etc.  For a full list of all
;; available constants, units, and prefixes, use `M-x constants-help',
;; or press "?" while the command is prompting for names.
;;
;; The unit system (SI or cgs) can be selected using the option
;; `constants-unit-system'.  Additional constants or units can be
;; defined by customizing `constants-user-defined'.  You can tell the
;; package to use different names for some of the constants (option
;; `constants-rename'), and you may also specify a different variable
;; name on the fly (see "Mlunar" in the example below).
;;
;; The code inserted into the buffer is mode dependent.  Constants.el
;; has defaults for some programming languages: FORTRAN, C, IDL,
;; MATLAB, OCTAVE, PERL, EMACS-LISP, GP.  You can change these
;; defaults and add definitions for other languages with the variable
;; `constants-languages'.
;;
;; INSTALLATION
;; ------------
;; Put this file on your load path, byte compile it, and copy the
;; following code into your .emacs file.  Change the key definitions,
;; variable name aliasing and the unit system to your liking.
;;
;;   (autoload 'constants-insert "constants" "Insert constants into source." t)
;;   (autoload 'constants-get "constants" "Get the value of a constant." t)
;;   (autoload 'constants-replace "constants" "Replace name of a constant." t)
;;   (define-key global-map "\C-cci" 'constants-insert)
;;   (define-key global-map "\C-ccg" 'constants-get)
;;   (define-key global-map "\C-ccr" 'constants-replace)
;;   (setq constants-unit-system 'SI)   ;  this is the default
;;
;;   ;; Use "cc" as the standard variable name for speed of light,
;;   ;; "bk" for Boltzmann's constant, and "hp" for Planck's constant
;;   (setq constants-rename '(("cc" . "c") ("bk" . "k") ("hp" . "h")))
;;
;;   ;; A default list of constants to insert when none are specified
;;   (setq constants-default-list "cc,bk,hp")
;;
;; USAGE
;; -----
;; In a programming mode, call the function and at the prompt enter
;; for example
;;
;;    Name[, Name2...}: cc,k,Mlunar=Mmoon,Mpc
;;
;; In a FORTRAN buffer, this would insert
;;
;;      doubleprecision cc=2.99792458d8     ! Speed of light [SI]
;;      doubleprecision k=1.3806503d-23     ! Boltzmann's constant [SI]
;;      doubleprecision Mlunar=7.35d22      ! Moon mass [SI]
;;      doubleprecision Mpc=3.085677582d+22 ! Mega-Parsec [SI]
;;
;; while in a C buffer you would get
;;
;;      double cc=2.99792458e8;           /* Speed of light [SI] */
;;      double k=1.3806503e-23;           /* Boltzmann's constant [SI] */
;;      double Mlunar=7.35e22;            /* Moon mass [SI] */
;;      double Mpc=3.085677582e+22;       /* Mega-Parsec [SI] */
;;
;; When entering the names, you can optionally precede a name with
;; "varname=", in order to change the variable name that will be used
;; for the definition.  While entering the name of a constant, you can
;; use completion.  Press `?' during completion to display a detailed
;; list of all available constants.  You can scroll the Help window
;; with S-TAB while entering text in the minibuffer.
;;
;;
;; CUSTOMIZATION
;; -------------
;; The following customization variables are available:
;;
;; constants-unit-system
;;   The unit system to be used for the constants (`cgs' or `SI').
;;
;; constants-rename
;;   Alist with additional names for some existing constants.
;;
;; constants-user-defined
;;   User defined constants.
;;
;; constants-default-list
;;   Default constants to insert if none are specified.
;;
;; constants-languages
;;   Format descriptions for different major programming modes.
;;
;; constants-indent-code
;;   Non-nil means, indent the newly inserted code according to mode.
;;
;; constants-allow-prefixes
;;   Non-nil means, interpret prefixes like M (mega) etc.
;;
;; constants-prefixes
;;   Allowed prefixes for constants and units.
;;
;; CONTEXT SENSITIVITY
;; -------------------
;; For some languages, it might be usefull to adapt the inserted code
;; to context.  For example, in Emacs Lisp mode, the default settings
;; insert "(VARIABLE VALUE)" with surrounding parenthesis for a `let'
;; form.  However, if you'd like to use this in a `setq' form, the
;; parenthesis are incorrect.  To customize for such a case, set the
;; variable `constants-language-function' to a function which returns,
;; after checking the context, the correct language entry of the form
;; (MAJOR-MODE FORMAT EXP-STRING PREFIX-EXPRESSION).  For the above
;; example, you could do this:
;;
;;   (defun my-constants-elisp-function ()
;;     "Check context for constants insertion."
;;     (save-excursion
;;       (condition-case nil
;;           (progn (up-list -1)
;;                  (if (looking-at "(setq\\>")
;;                      '(emacs-lisp-mode "%n %v%t; %d %u" "e" "(* %p %v)")
;;                    '(emacs-lisp-mode "(%n %v)%t; %d %u" "e" "(* %p %v)")))
;;         (error nil))))     ; return value nil means use default
;;
;;   ;; The variable is buffer-local and must be set in the mode hook.
;;   (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (setq constants-language-function
;;                     'my-constants-elisp-function)))
;;
;; BUGS
;; ----
;; - Completion does not consider prefixes.  For example, you cannot
;;   complete "MGa" to "MGauss" (meaning "Mega-Gauss").  This was not
;;   implemented because it would cause too many matches during
;;   completion.  But you can still use completion for this by separating
;;   the prefix from the unit with a star:  After typing "M*Ga", completion 
;;   will work and result in "M*Gauss".  Both "MGauss" and "M*Gauss" will
;;   result in a variable "MGauss" being defined.
;; - When using cgs units, be very careful with the electric constants
;;   and units.  This library uses E.S.U., not E.M.U.  Note that the
;;   *equations* involving charges, currents and magnetism are all
;;   different for SI, CGS/ESU and CGS/EMU.  So when switching to a
;;   different system, you must make sure to use the right equations.
;; - I have tried to implement the cgs units correctly, but I have
;;   some doubt about the electrical and radiation units.
;;   Double-check before blindly using these.
;;
;; AUTHOR
;; ------
;; Carsten Dominik <dominik@science.uva.nl>
;;
;; Let me know if you are missing a constant in the default setup, if
;; you notice that a value of a constant is not correct, or if you
;; would like to see support for another language mode.
;;
;; ACKNOWLEDGEMENTS
;; ----------------
;; Thanks to Kees Dullemond.  Watching him writing programs has
;; inspired this package.
;;
;; Thanks to the following people for reporting bugs and/or suggesting
;; features/constants/languages:
;; Bruce Ignalis, Dave Pearson, Jacques L'helgoualc'h
;;
;; CHANGES
;; -------
;; Version 2.0
;; - New commands `constants-get' and `constants-replace'.
;;
;; Version 1.8
;; - Completion now preserves up/downcase as typed.
;; - Completion with prefixes can be done, by adding a star as in "M*Gauss".
;; - A different variable name may be specified directly at the prompt with
;;   the syntax "varname=const".
;; - Support for shell-like modes like idlwave-shell-mode.
;; - XEmacs support (fixed a small bug)
;;
;; TO DO
;; -----
;; - Support more programming languages.
;; - Add expression values for matlab, octave, python, tcl and others.
;; - Add calc mode.
;; - add these units?
;;   - g[u]age, 
;;   - (circular) mill
;;   - ampere-turn
;; - add a command to get info about a certain variable.  Only useful if
;;   the variable name really is the constant name.  Not sure if this will
;;   be used at all, so until someones asks for it, this will not be done.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defgroup constants nil 
  "Customization group for inserting constants into programs."
  :tag "Constants"
  :prefix "constants-"
  :group 'tools)

(defcustom constants-unit-system 'SI
  "The unit system to be used for the constants.
Legal values are `cgs' and `SI'."
  :group 'constants
  :type '(choice
	  (const cgs)
	  (const SI)))

(defcustom constants-rename
  '(("kk" . "k") ("bk" . "k")
    ("cc" . "c") ("cl" . "c")
    ("hh" . "h") ("hp" . "h")
    )
  "Alist with additional names for some existing constants.  
Sometime it is better in a program to use different names for
constants, for exapmle \"cc\" instead of \"c\" for the speed of light,
in order be able to use single letter variables for other purposes.
Here you can specify a list of new names.  The cdr of each item must
be the name of a user-defined or default constant.  See the variables
`constants-defaults' and `constants-user-defined'."
  :group 'constants
  :type
  '(repeat
    (cons (string "Variable name")
	  (string "Constant name"))))

(defcustom constants-user-defined nil
  "User defined constants for programs.
For each constant there is a list of 5 items: The long and a short
variable name, a brief description, and the values of the constant
in SI and cgs units, as strings.  For examples, see `constants-defaults'.
The description should be short, because it is placed into a comment
after the variable assignment.

Besides these 5 items, the list can contain additional items of the form

 (major-mode si-value cgs-value)

These can be used to set special values for different programming
languages.  For example, for mathematical constants it may be useful
to use capabilities of the language to get a value with higher
precision (e.g. \"4*atan(1)\" versus \"3.14\").  But this only makes
sense if the constant can be computed exactly.  If it is just a
measured physical constant multiplied by a mathematical constant, the
high accuracy is usually not meaningful.  As a special case, if the
SI-value is a symbol like `fortran-mode', the expressions for the
corresponding mode will be looked up.  If the SI-value is the empty
string or the symbol `default', settings for a virtual mode `default'
are used.  Again, `constants-defaults' contains examples."
  :group 'constants
  :type '(repeat
	  (list
	   (string :tag "Long name")
	   (string :tag "Short name")
	   (string :tag "Description")
	   (string :tag "SI  value")
	   (string :tag "cgs value")
           (repeat 
            :inline t
            (list (symbol :tag "mode")
                  (string :tag "SI  value")
                  (string :tag "cgs value"))))))


(defcustom constants-default-list "hp,kk,cc,AU,Msun,Lsun,Grav"
  "Default constants to insert if none are specified."
  :group 'constants
  :type 'string)

(defcustom constants-languages
  '(
    ;; (fortran-mode "doubleprecision %n\nparameter(%n=%v)%t! %d %u" "d")
    (fortran-mode "doubleprecision %n=%v%t! %d %u" "d" "%p*%v" nil)
    (c-mode "double %n=%v;%t/* %d %u */" "e" "%p*%v" nil)
    (idlwave-mode "%n = %v%t;; %d %u" "d" "%p*%v" nil)
    (text-mode    "%n = %v%t(%d %u)" "d" "%p*%v" nil)
    (idlwave-shell-mode "%n = %v%t;; %d %u" "d" "%p*%v" 
                        idlwave-shell-send-command)
    (matlab-mode "%n = %v%t% %d %u" "e" "%p*%v" nil)
    (octave-mode "%n = %v%t# %d %u" "e" "%p*%v" nil)
    (perl-mode  "$%n = %v;%t# %d %u" "e" "%p*%v" nil)
    (cperl-mode . perl-mode)
    (emacs-lisp-mode "(%n %v)%t; %d %u" "d" "(* %p %v)" nil) ; for a let form
    (lisp-interaction-mode . emacs-lisp-mode)
    (gp-mode "%n = %v /* %d %u */" "e" "%p*%v" nil)
    (gp-script-mode "%n = %v; /* %d %u */" "e" "%p*%v" nil)
    (t "%n = %v%t; %d %u" "e" "%p*%v" nil))
    "Format descriptions for different major modes.
This is an alist with major mode symbols as keys.  If a key is `t', that
entry specifies a default format.  The second item is the format string
to insert for each constant.  In the format, several %-escapes
have special meaning:
%n   the variable name
%v   the value of the constant
%p   the value of the numeric prefix
%d   the descriptive text of the constant
%u   the unit sytem for which this value is applicable
%t   Text after this is indented to `comment-column'

The third element in the list is the string to use for starting an
exponent (a power of 10).  By default, \"e\" is used, but you can
change it here, most likely to \"d\", which indicates double precision
values in some languanges.

The forth item indicates how to implement a numeric prefix in the case
of an expression constant.  Normally, the numerical constant is worked
directly into the constant - but if the value is an expression, than
this is not possible.  In this case, the string given here will
replace %v in the main insertion format.

The final entry is a function to call in order to process the variable
definition.  The default nil means to simply insert it into the
buffer, as source code.  If the buffer is interacting with a process,
the definition may have to be processed in another way.  See for
example the entry for `idlwave-shell-mode'.

An entry in this variable may also be a cons cell
   (major-mode . other-major-mode)
In this case, the language settings are copied from other-major-mode.
This is mainly useful for modes which have several incarnations, like
`emacs-lisp-mode' and `lisp-interaction-mode', or `perl-mode' and
`cperl-mode'."
  :group 'constants
  :type
  '(repeat
    (choice
     :value (c-mode "" "e" "%p*%v")
     (cons :tag "Like another major mode" :value (nil . nil)
           (symbol :tag "Major mode")
           (symbol :tag "Copy from other mode"))
     (list :tag "Specification" :value (c-mode "" "e" "%p*%v" nil)
           (symbol :tag "Major mode")
           (string :tag "Format")
           (string :tag "Exponent key")
           (string :tag "Expression format")
           (symbol :tag "Function" :value nil)))))

(defcustom constants-indent-code t
  "Non-nil means, indent the newly inserted code according to mode."
  :group 'constants
  :type 'boolean)

(defconst constants-defaults
  '(
    "Natural constants"

    ;; Note: electrical charge given here is e.s.u., not e.m.u.
    ("echarge"       "e"      "Elementary charge"      "1.602176462e-19 [C]"    "4.8032e-10 [esu]")
    ("clight"        "c"      "Speed of light"         "2.99792458e8 [m/s]"     "2.99792458e10 [cm/s]")
    ("hplanck"       "h"      "Planck's constant"      "6.62606876e-34 [J s]"   "6.62606876e-27 [erg s]")
    ("hplanckbar"    "hbar"   "Planck's constant"      "1.054571596e-34 [J s]"  "1.054571596e-27 [erg s]")
    ("Grav"          "GG"     "Gravitational constant" "6.673e-11 [N m^2/g^2]"  "6.673e-8 [dyn cm^2/g^2]")
    ("Nav"           "NA"     "Avogadro's constant"    "6.02214199e23 [1/mol]"  "6.02214199e23 [1/mol]")
    ("melektron"     "me"     "Electron rest mass"     "9.10938188e-31 [kg]"    "9.10938188e-28 [g]")
    ("mproton"       "mp"     "Proton rest mass"       "1.67262158e-27 [kg]"    "1.67262158e-24 [g]")
    ("mneutron"      "mn"     "Neutron rest mass"      "1.67492716e-27 [kg]"    "1.67492716e-24 [g]")
    ("mmuon"         "mu"     "Muon rest mass"         "1.88353109e-28 [kg]"    "1.88353109e-25 [g]")
    ("atomicmass"    "amu"    "Atomic mass unit"       "1.66053873e-27 [kg]"    "1.66053873e-24 [g]")
    ("Rydberg"       "Ryd"    "Rydberg's constant"     "1.09737315685e7 [1/m]"  "1.09737315685e5 [1/cm]")
    ("finestructure" "fsc"    "Fine structure const"   "7.297352533e-3 []"      "7.297352533e-3 []")
    ("kboltzmann"    "k"      "Boltzmann's constant"   "1.3806503e-23 [J/K]"    "1.3806503e-16 [erg/K]")
    ("Rgas"          "R0"     "Molar gas constant"     "8.314472e0 [J/K mol]"   "8.314472e7 [erg/K mol]")
    ("Vgas"          "V0"     "Ideal gas volume"       "2.2710981e-2 [m^3/mol]" "2.2710981e4 [cm^3/mol]")
    ("sigthompson"   "sth"    "Thompson crosssection"  "6.6524e-29 [m^2]"       "6.6524e-25 [cm^2]")  ;; FIXME?
    ("sigma"         "sig"    "Stefan-Boltzman const"  "5.6703e-8 [W/m^2 K^4]"  "5.6703e-5 [erg/s cm^2 K^4]")
    ("arad"          "a"      "Radiation constant"     "7.5657e-15 [J/m^3 K^4]" "7.5657e-15 [erg/cm^3 K^4]")

    "Math constants"

    ("pi"            ""       "Pi"                     "3.1415926535897932385e0 []" "3.1415926535897932385e0 []"
     (idlmode "!DPI")
     (emacs-lisp-mode "pi")
     (default "4.e0*atan(1.e0)")
     (fortran-mode default) (c-mode default)
     (idlwave-mode default) (matlab-mode default) (octave-mode default)
     (perl-mode "4*atan2(1,1)")
     )
    ("exp1"          ""       "e (base of ln)"         "2.7182818284590452354e0 []" "2.7182818284590452354e0 []")

    "Length units"

    ("meter"         "m"      "Meter"                  "1.0e0 [m]"           "1.0e2 [cm]")
    ;; 1 lyr = c * 365.2425 *24*60^2 
    ("Angstroem"     "Ang"    "Angstroem"              "1e-10 [m]"           "1e-8 [cm]")
    ("micron"        "mum"    "Micrometer"             "1e-6 [m]"            "1e-4 [cm]")
    ;; Just a few more commonly used english units - completeness is not attempted
    ("inch"          "in"     "Inch"                   "2.54e-2 [m]"         "2.54e0 [cm]") 
    ("foot"          "ft"     "Foot"                   "3.048e-1 [m]"        "3.048e1 [cm]")
    ("yard"          "yd"     "Yard"                   "9.144e-1 [m]"        "9.144e1 [cm]")
    ("mile"          "mi"     "Mile"                   "1.609344e3 [m]"      "1.609344e5 [cm]")
    ("nauticmile"    "nmi"    "Nautical Mile"          "1.852e3 [m]"         "1.852e5 [cm]")
    ("point"         "pt"     "Point (1/72 in)"        "3.527777778e-4 [m]"  "3.527777778e-2 [cm]")

    "Area units"

    ("Hectar"        "hect"   "Hectar"                 "1e4 [m^2]"             "1e8 [cm^2]")
    ("Acre"          ""       "Acre"                   "4.04685642241e3 [m^2]" "4.04685642241e7 [cm^2]")
    ("barn"          "ba"     "Barn"                   "1e-28 [m^2]"           "1e-24 [cm^2]")
    
    "Time units"

    ("second"        "s"      "Seconds"                "1.0e0 [s]"           "1.0e0 [s]")
    ("minute"        "min"    "Minutes"                "60e0 [s]"            "60e0 [s]")
    ("hour"          "hr"     "Hours"                  "3600e0 [s]"          "3600e0 [s]")
    ("day"           "d"      "Days"                   "8.64e4 [s]"          "8.64e4 [s]")
    ("week"          "wk"     "Weeks"                  "6.048e5 [s]"         "6.048e5 [s]")
    ("year"          "yr"     "Years"                  "3.15576e7 [s]"       "3.15576e7 [s]")
    ("Hertz"         "Hz"     "Hertz"                  "1.0e0 [s]"           "1.0e0 [s]")

    "Velocity Units"
    ("kmh"           ""       "Kilometers per  hour"   "2.7777777778e-1 [m/s]" "2.7777777778e1 [cm/s]"
     (default "1e3/3600" "1e5/3600")
     (fortran-mode default) (c-mode default)
     (idlwave-mode default) (matlab-mode default) (octave-mode default)
     (perl-mode default) (emacs-lisp-mode  "(/ 1e3 3600)" "(/ 1.e5 3600)")
     ;; (gp-mode default)
     )
    
    ("mph"           ""       "Miles per  hour"        "4.4704e-1 [m/s]"       "4.4704e1 [cm/s]")
    ;; knot = nmi / hour
    ("knot"          ""       "Knot"                   "5.144444444e1 [m/s]"   "5.144444444e-1 [cm/s]")

    "Mass units"

    ("gram"          "g"      "Grams"                  "1.0e-3 [kg]"           "1.0e0 [g]")
    ("pound"         "lb"     "Pound"                  "4.5359237e-1 [kg]"     "4.5359237e2 [g]")
    ("Ounce"         "oz"     "Ounce"                  "2.8349523125e-2 [kg]"  "2.8349523125e1 [g]")
    ("ton_metric"    "t"      "Metric ton"             "1e3 [kg]"              "1e6 [g]")
    ("carat"         "ct"     "Carat"                  "2e-4 [kg]"             "2e-1 [g]")

    "Force units"

    ("Newton"        "N"      "Newton (force)"         "1e0 [kg m/s^2]"        "1e5 [g cm/s^2]")
    ("dyne"          "dyn"    "Dyne (force)"           "1e-5 [kg m/s^2]"       "1e0 [g cm/s^2]")

    "Energy units"

    ("Joule"         "J"      "Joule (energy)"         "1e0 [J]"             "1e7 [erg]")
    ("erg"           ""       "Erg (energy)"           "1e-7 [J]"            "1e0 [erg]")
    ("Calories"      "cal"    "Calories (energy)"      "4.1868e0 [J]"        "4.1868e7 [erg]")
    ("eVolt"         "eV"     "Electron Volt (energy)" "1.602176462e-19 [J]" "1.602176462e-12 [erg]")
    ("Kayser"        "invcm"  "Energy in cm^-1"        "1.986445e-23 [J]"    "1.986445e-16 [erg]")
    ("WattHour"      "Wh"     "Watt*Hour"              "3.6e3 [J]"           "3.6e10 [erg]")
    ("Horse"         "hp"     "Horse power"            "7.457e2 [J]"         "7.457e9 [erg]")
    ("BritTherm"     "Btu"    "British Thermal Unit"   "1.055056e10 [J]"     "1.055056e3 [erg]")

    "Power units"

    ("Watt"          "W"      "Watt"                   "1e0 [J/s]"           "1e7 [erg/s]")

    "Pressure units"

    ("Pascal"        "Pa"     "Pascal (pressure)"      "1e0 [N/m^2]"             "10e0 [dyn/cm^2]")
    ("bar"           ""       "Bar (pressure)"         "1e5 [N/m^2]"             "1e6 [dyn/cm^2]")
    ("atmospheres"   "atm"    "Atmospheres (pressure)" "1.01325e5 [N/m^2]"       "1.01325e6 [dyn/cm^2]")
    ("torr"          ""       "Torr (pressure)"        "1.333224e2 [N/m^2]"      "1.333224e3 [dyn/cm^2]")
    ("psi"           ""       "Pounds/in^2"            "6.89475729317e3 [N/m^2]" "6.89475729317e4 [dyn/cm^2]")
    ("mHg"           ""       "Meter of Mercury"       "1.333224e5 [N/m^2]"      "1.333224e6 [dyn/cm^2]")

    "Temperature units" ;; FIXME: dimensionless factors for transformations

    ("Kelvin"        "degK"   "Kelvin"                 "1.0e0 [K]"           "1.0e0 [K]")
    ("Celsius"       "degC"   "Celsius"                "1.0e0 [K]"           "1.0e0 [K]")
    ("Fahrenheit"    "degF"   "Fahrenheit"             "0.55555555556e0 [K]" "0.55555555556e0 [K]"
     (default "5.e0/9")
     (fortran-mode default) (c-mode default)
     (idlwave-mode default) (matlab-mode default) (octave-mode default)
     (perl-mode default) (emacs-lisp-mode "(/ 5. 9.)") (gp-mode default))

    "Light units"  ;; FIXME: I am not sure if these are right...

    ("Candela"       "cd"     "Candela"                "1e0 [cd]"            "1e0 [cd]")
    ("Stilb"         "sb"     "Stilb"                  "1e4 [cd/m^2]"        "1e0 [cd/cm^2]")
    ("Lumen"         "lm"     "Lumen"                  "1e0 [cd sr]"         "1e0 [cd sr]")
    ("Lux"           "lx"     "Lux"                    "1e0 [cd sr/m^2]"     "1e-4 [cd sr/cm^2]")
    ("Phot"          "ph"     "Phot"                   "1e4 [lx]"            "1e0 [lx]")
    ("Lambert"       "lam"    "Lambert"                "3.18309886184e3 [cd/m^2]" "3.18309886184e-1 [cd/cm^2]")

    "Radiation units"

    ("Becquerel"     "Bq"     "Becquerel"              "1.0e0 [1/s]"         "1.0e0 [1/s]")
    ("Curie"         "Ci"     "Curie"                  "3.7e10 [1/s]"        "3.7e10 [1/s]")
    ("Gray"          "Gy"     "Gray"                   "1.0e0 [J/kg]"        "1.0e4 [erg/g]")
    ("Sievert"       "Sv"     "Sievert"                "1.0e0 [J/kg]"        "1.0e4 [erg/g]")
    ("Roentgen"      "R"      "Roentgen"               "2.58e-4 [C/kg]"      "7.7346e2 [?]")
    ("Radrad"        "rd"     "Rad (radiation)"        "1.0e-2 [J/kg]"       "1.0e2 [erg/g]")
    ("Rem"           "rem"    "Rem"                    "1.0e-2 [J/kg]"       "1.0e2 [erg/g]")

    "Amount of matter units"
    ("Mol"           "Mol"    "Mol (SI base unit)"     "1.0e0 [mol]"         "1.0e0 [mol]")

    "Friction units" 

    ("Poise"         "Poi"    "Poise"                  "1.0e-1 [kg/m s]"     "1.0e0 [g/cm s]")
    ("Stokes"        "St"     "Stokes"                 "1.0e-4 [m^2/s]"      "1.0e0 [cm^2/s]")

    "Electrical units" ;; FIXME; I am not sure if the cgs versions are right.

    ;; Note: units refer to esu, not emu units.... 
    ("Ampere"        "Amp"    "Ampere"                 "1.0e0 [A]"           "2.99792458e9 [?]")
    ("Coulomb"       "C"      "Coulomb"                "1.0e0 [C]"           "2.99792458e9 [?]")
    ("Faraday"       "Fdy"    "Faraday"                "9.6485341472e4 [C]"  "2.892555240e14 [?]")
    ("Volt"          ""       "Volt"                   "1.0e0 [W/A]"         "3.335640952e-3 [?]")
    ("Ohm"           ""       "Ohm"                    "1.0e0 [V/A]"         "1.112650056e-12 [?]")
    ("Mho"           ""       "Mho"                    "1.0e0 [A/V]"         "8.987551787e11 [?]")
    ("Siemens"       ""       "Siemens"                "1.0e0 [A/V]"         "8.987551787e11 [?]")
    ("Farad"         ""       "Farad"                  "1.0e0 [C/V]"         "8.987551787e11 [?]")
    ("Henry"         ""       "Henry"                  "1.0e0 [Wb/A]"        "1.112650056e-12 [?]")
    ("Tesla"         "T"      "Tesla"                  "1.0e0 [Wb/m^2]"      "2.99792458e14 [?]")
    ("Gauss"         ""       "Gauss"                  "1.0e-4 [Wb/m^2]"     "2.99792458e10 [?]")
    ("Weber"         "Wb"     "Weber"                  "1.0e0 [V s]"         "3.335640952e-3 [?]")

    "Angular units"
    
    ("Radian"        "rad"    "Radian"                 "1.0e0 [rad]"          "1.0e0 [rad]")
    ("Steradian"     "sr"     "Steradian"              "1.0e0 [sr]"           "1.0e0 [sr]")
    ("Degrees"       "deg"    "Degrees"                "1.745329252e-2 [rad]" "1.745329252e-2 [rad]"
     (default "atan(1.e0)/45e0")
     (fortran-mode default) (c-mode default)
     (idlwave-mode "!DPI/180") ; (matlab-mode "") (octave-mode "")
     ;; (perl-mode "")
     (emacs-lisp-mode "(/ pi 180)")
     ;; (gp-mode "")
     )

    ("Grad"          "grad"   "Grad"                   "1.570796327e-2 [rad]"  "1.570796327e-2 [rad]"
     (default "atan(1.e0)/50e0")
     (fortran-mode default) (c-mode default)
     (idlwave-mode "!DPI/200") ; (matlab-mode "") (octave-mode "")
     ;; (perl-mode "")
     (emacs-lisp-mode "(/ pi 200)")
     ;; (gp-mode "")
     )
    ("Arcminute"     "arcmin" "Arcminutes"             "2.908882087e-4 [rad]"  "2.908882087e-4 [rad]"
     (default "atan(1.e0)/27e2")
     (fortran-mode default) (c-mode default)
     (idlwave-mode "!DPI/180/60") ; (matlab-mode "") (octave-mode "")
     ;; (perl-mode "")
     (emacs-lisp-mode "(/ pi 180 60)")
     ;; (gp-mode "")
     )
    ("Arcsecond"     "arcsec" "Arcseconds"             "4.848136812e-6 [rad]"  "4.848136812e-6 [rad]"
     (default "atan(1.e0)/162e3")
     (fortran-mode default) (c-mode default)
     (idlwave-mode "!DPI/180/3600") ; (matlab-mode "") (octave-mode "")
     ;; (perl-mode "")
     (emacs-lisp-mode "(/ pi 180 3600)")
     ;; (gp-mode "")
     )

    ("Degrees2"       "deg2"    "Square Degrees"       "3.04617419786e-4 [sr]" "3.04617419786e-4 [sr]"
     (default "atan(1.e0)*atan(1.e0)/2025e0")
     (fortran-mode default) (c-mode default)
     (idlwave-mode "!DPI^2/32400") ; (matlab-mode "") (octave-mode "")
     ;; (perl-mode "")
     (emacs-lisp-mode "(/ (* pi pi) 32400)")
     ;; (gp-mode "")
     )

    ("Arcminute2"     "arcmin2" "Square Arcminutes"      "8.46159499406e-8 [sr]"  "8.46159499406e-8 [sr]"
     (default "atan(1.e0)*atan(1.e0)/729e4")
     (fortran-mode default) (c-mode default)
     (idlwave-mode "!DPI^2/1.1664e8") ; (matlab-mode "") (octave-mode "")
     ;; (perl-mode "")
     (emacs-lisp-mode "(/ (* pi pi) 180 180 3600)")
     ;; (gp-mode "")
     )
    ("Arcsecond2"     "arcsec2" "Square Arcseconds"      "2.35044305389e-11 [sr]"  "2.35044305389e-11 [sr]"
     (default "atan(1.e0)*atan(1.e0)/2.6244e10")
     (fortran-mode default) (c-mode default)
     (idlwave-mode "!DPI^2/4.19904e11") ; (matlab-mode "") (octave-mode "")
     ;; (perl-mode "")
     (emacs-lisp-mode "(/ (* pi pi) 4.19904e11)")
     ;; (gp-mode "")
     )

    "Astronomical Units"

    ("lightyear"     "lyr"    "Lightyear"              "9.460536207e15 [m]"   "9.460536207e17 [cm]")
    ;; 1 pc = AU / arcsec
    ("parsec"        "pc"     "Parsec"                 "3.085677582e16 [m]"   "3.085677582e18 [cm]")
    ("Lsun"          ""       "Solar Luminosity"       "3.82e26 [W]"          "3.82e33 [erg/s]")
    ("Msun"          ""       "Solar Mass"             "1.989e30 [kg]"        "1.989e33 [g]")
    ("Mjupiter"      "Mjup"   "Jupiter mass"           "1.8986e27 [kg]"       "1.8986e30 [g]")
    ("Mearth"        "MEa"    "Earth Mass"             "5.976e24 [kg]"        "5.976e27 [g]")
    ("Mmoon"         "Mmn"    "Moon mass"              "7.35e22 [kg]"         "7.35e25 [g]")
    ("Rsun"          ""       "Solar radius"           "6.96e8 [m]"           "6.96e10 [cm]")
    ("Rearth"        ""       "Earth radius"           "6.378e6 [m]"          "6.378e8 [cm]")
    ("AstronUnit"    "AU"     "Astronomical unit"      "1.49597870691e11 [m]" "1.49597870691e13 [cm]")
    ("Jansky"        "Jy"     "Jansky"                 "1e-26 [W / m^2 Hz]"   "1e-23 [erg/cm^2 s Hz]")
    ("gEarth"        "ga"     "Earth acceleration"     "9.80665e0 [m/s^2]"    "9.80665e2 [cm/s^2]")

    "Special Units"

    ;; Planck units:  These definitions use h, not hbar
    ("lPlanck"       "lpl"    "Planck length (h)"       "4.05083e-35 [m]"     "4.05083e-33 [cm]")
    ("mPlanck"       "mpl"    "Planck mass (h)"         "5.45621e-8 [kg]"     "5.45621e-5 [g]")
    ("tPlanck"       "tpl"    "Planck time (h)"         "1.35121e-43 [s]"     "1.35121e-43 [s]")
    ;; Planck units:  These definitions use hbar, not h
    ("lPlanckBar"    "lplb"   "Planck length (hbar)"    "1.61605e-35 [m]"     "1.61605e-33 [cm]")
    ("mPlanckBar"    "mplb"   "Planck mass (hbar)"      "2.17671e-8 [kg]"     "2.17671e-5 [g]")
    ("tPlanckBar"    "tplb"   "Planck time (hbar)"      "5.39056e-44 [s]"     "5.39056e-44 [s]")
    )
  "Built-in constants and units")

(defcustom constants-allow-prefixes t
  "Non-nil means, non-matching names are tried again with the first character
interpreted as unit prefix.  See `constants-prefixes' for a list of allowed
prefiexes."
  :group 'constants
  :type 'boolean)

(defcustom constants-prefixes
  '((?E "1e18"  "Exa")
    (?P "1e15"  "Peta")
    (?T "1e12"  "Tera")
    (?G "1e9"   "Giga")
    (?M "1e6"   "Mega")
    (?k "1e3"   "Kilo")
    (?h "1e2"   "Hecto")
    (?D "1e1"   "Deka")
    (?d "1e-1"  "Deci")
    (?c "1e-2"  "Centi")
    (?m "1e-3"  "Milli")
    (?u "1e-6"  "Micro")
    (?n "1e-9"  "Nano")
    (?p "1e-12" "Pico")
    (?f "1e-15" "Femto")
    (?a "1e-18" "Atto"))
  "Allowed prefixes for constants and units"
  :group 'constants
  :type  '(repeat
           (list (character :tag "Prefix char")
                 (number    :tag "Numeric value")
                 (string    :tag "Prefix name"))))

(defvar constants-language-function nil
  "Function for returning a special format entry.
The value of this variable must be a function which returns a list
\((MAJOR-MODE FORMAT EXP-STRING), similar to the entries in
`constants-languages'.")
(make-variable-buffer-local 'constants-language-function)

(defvar constants-major-mode)

(eval-when-compile (defvar ctable))

;;;###autoload
(defun constants-insert (&optional unit-system names)
  "Insert one or more natural constant definitions in source code.
Prompts for a constant name and inserts a variable definition and
assignment into the code.  The code produced is different for
different programming languages.  The available constants are defined
in `constants-defaults' and `constants-user-defined'.  Also names
specified in `constants-rename' can be given here.  For speed, you can
enter a comma-separated list of several names, and completion will be
available for each name.  The variables will be defined in the
upcase-downcase spelling you typed, but completion lookup is
case-insensitive.  While entering the variable names, you can press
`?' for a complete list of all available constants.

When called with prefix argument UNIT-SYSTEM, the \"other\" unit
system will be used.  I.e., if your default is `SI', then a prefix arg
will switch to `cgs' and vice versa.

`constants-insert' may also be called from a lisp program - in this
case the comma-separated list of names should be given as argument
NAMES.  UNIT-SYSTEM may be nil to use the default, but also `SI' or
`cgs' may be specified directly.  For example

     (constants-insert 'SI \"hplanck,c,M*pc\")"
  (interactive "P")
  (let* ((constants-unit-system
          (cond ((and unit-system (symbolp unit-system)) unit-system)
                (unit-system (if (eq constants-unit-system 'SI) 'cgs 'SI))
                (t constants-unit-system)))
         (all-constants (append constants-user-defined constants-defaults))
	 (atable (append constants-rename all-constants))
         (ctable (constants-make-completion-table constants-rename
                                                  all-constants))
         (constants-major-mode major-mode)
	 (req1 (or names
                   (constants-completing-read "Name1[,name2...]: " ctable)))
         (req (if (string= "" req1) constants-default-list req1))
	 (clist (split-string req "[ ,]+"))
         (mode major-mode)
	 (fentry (or (and constants-language-function
                          (fboundp constants-language-function)
                          (funcall constants-language-function))
                     (assq mode constants-languages)
                     (assq t constants-languages)))
	 format exp-string
         pmatch factor prefix-name rpl prefix-exp force-prefix process-func
	 const prefix entry entry1 desc value ins beg linelist line vname)
        ;; Check for fentry aliasing
    (while (and fentry
                (symbolp (cdr fentry)))
      (setq mode (cdr fentry))
      (setq fentry (or (assq mode constants-languages)
                       (assq t constants-languages))))
            
    (unless fentry
      (error "No format definition for constants in %s" major-mode))
    ;; extract format specifications
    (setq format (nth 1 fentry) exp-string (nth 2 fentry)
          prefix-exp (nth 3 fentry) process-func (nth 4 fentry))
    (while (setq const (pop clist))
      (setq prefix nil factor nil prefix-name "" force-prefix nil vname nil)
      (if (string-match "\\(.*\\)=\\(.*\\)" const)
          (setq vname (match-string 1 const) const (match-string 2 const)))
      (if (string-match "\\(.*\\)\\*\\(.*\\)" const)
          (setq const (concat (match-string 1 const) (match-string 2 const))
                force-prefix t))
      (setq entry (constants-assoc const atable 'follow)
	    name (car entry)
            pmatch (assoc (string-to-char const) constants-prefixes))
      (if (and (or force-prefix
                   (not entry))
               constants-allow-prefixes
               pmatch
               (setq entry1 (constants-assoc (substring const 1) atable)))
          (progn
            (setq entry entry1
                  name (car entry)
                  factor (nth 1 pmatch)
                  prefix (string-to-char const)
                  prefix-name (nth 2 pmatch))))
      (if (not entry)
          (error "No such constant: %s" const))
      (unless entry
	(error "No such constant: %s" const))
      (setq name (car entry)
	    desc (nth 2 entry)
	    value (constants-get-value entry mode))
      (if (or (not value) (not (stringp value))
	      (not (string-match "\\S-" value)))
	  (error "No value for constant %s (%s)" const desc))
      (if (and prefix                                   ; prefix
               (not (string-match "[^-+0-9e.]" value))) ; no expression
          (progn
            ;; Implement prefix directly in the number
            ;; We need to hack around accuracy problems here.
            (if (string-match "\\([eEdD]\\)\\([-+]?[0-9]+\\)" value)
                (setq value (replace-match (concat "\\1" 
                   (format "%d" (floor
                    (+ 0.1 (string-to-number (match-string 2 value))
                       (log (string-to-number factor) 10.))))) nil nil value))
              (setq value (format "%.12e"
                                  (* (string-to-number factor)
                                     (string-to-number value))))
              (if (string-match "0+[eE]" value)
                  (setq value (replace-match "e" t t value))))))

      ;; Insert and indent
      (setq ins format)
      (if (and (string-match "[^-+0-9e.]" value)  ; value is an expression
               (string-match "%p" prefix-exp)     ; prefix format is there
               prefix
               (string-match "%v" ins))
          (setq ins (replace-match prefix-exp t t ins)))                             
      (while (string-match "%p" ins)
	(setq ins (replace-match factor t t ins)))
      (while (string-match "%v" ins)
	(setq ins (replace-match value t t ins)))
      (let ((start -1) (rpl (concat exp-string "\\2")))
        (while (and (string-match "\\S-" exp-string)
                    (setq start (string-match "\\(e\\)\\([-+]?[0-9]+\\)"
                                              ins (1+ start))))
	  (setq ins (replace-match rpl t nil ins))))
      (while (string-match "%n" ins)
	(setq ins (replace-match (or vname const) t t ins)))
      (while (string-match "%d" ins)
	(setq ins (replace-match 
                   (if prefix (concat prefix-name "-" desc) desc)
                   t t ins)))
      (while (string-match "%u" ins)
	(setq ins (replace-match
                   (concat "[" (symbol-name constants-unit-system) "]")
                   t t ins)))
      (if process-func
          ;; Special treatment!
          (progn
            (while (string-match "%t" ins)
              (setq ins (replace-match 
                         (make-string (max 2 (- 38 (match-beginning 0))) ?\ )
                         t t ins)))
            (funcall process-func ins))
        ;; Here comes the insertion stuff for source code editing modes.
        ;; First make sure we start a new line
        (if (string-match
             "\\S-" (buffer-substring (point-at-bol) (point-at-eol)))
            ;; non-empty line, insert after this line
            (progn 
              (end-of-line 1) 
              (if constants-indent-code (newline-and-indent) (newline)))
          ;; Empty line, simply insert into this line
          (if constants-indent-code
              (indent-according-to-mode)
            (beginning-of-line 1)))
        (setq linelist (split-string ins "\n"))
        (while (setq line (pop linelist))
          (if (string-match "\\(.*\\)%t\\(.*\\)" line)
              (let ((comment-column 42))
                (insert (match-string 1 line))
                (indent-to comment-column)
                (insert (match-string 2 line)))
            (insert line)))
        (if constants-indent-code
            (newline-and-indent)
          (newline))))))

;;;###autoload
(defun constants-get (&optional const message)
  "Return the value of CONST as defined in the constants package.
The will interpret the name of a constant, possible prefix notation
like Ms for Mega-seconds etc, just like in the constants package.  The
result also depends on the selected unit system, see `constants-unit-system'.
Interactive calls to this routine prompt for the constant name and place
the value into the kill ring."
;; FIXME: There is a lot of code duplication with constants-insert here,
;;        maybe we should restructure this at some point.  For now, it works.
  (interactive)
  (let* ((all-constants (append constants-user-defined constants-defaults))
         (atable (append constants-rename all-constants))
         entry prefix prefix-name pmatch unit factor value ok)
    (if (interactive-p)
        ;; Read a constant name
        (let* ((ctable (constants-make-completion-table constants-rename
                                                        all-constants))
               (constants-major-mode major-mode)
               (req1 (constants-completing-read "Constant: " ctable)))
          (setq const req1))
      (or const 
          (error "Non-interactive use must supply the name of a constant")))
    (setq entry (constants-assoc const atable 'follow)
          pmatch (assoc (string-to-char const) constants-prefixes))
    (and (not entry)
         constants-allow-prefixes
         pmatch
         (setq entry (constants-assoc (substring const 1) atable)
               factor (nth 1 pmatch)
               prefix (string-to-char const)
               prefix-name (nth 2 pmatch)))
    (catch 'exit
      (if (not entry)
          (throw 'exit nil))  ; return nothing
      (setq value (constants-get-value entry nil)
            unit (constants-get-unit entry))
      (if (or (not value) (not (stringp value))
	      (not (string-match "\\S-" value)))
          (throw 'exit nil)) ; return nothing
      (setq ok t)   ; we seem to have something...
      (if (and prefix
               (not (string-match "[^-+0-9e.]" value)))
          (progn
            ;; Implement prefix directly in the number
            ;; We need to hack around accuracy problems here.
            (if (string-match "\\([eEdD]\\)\\([-+]?[0-9]+\\)" value)
                (setq value (replace-match (concat "\\1" 
                   (format "%d" (floor
                    (+ 0.1 (string-to-number (match-string 2 value))
                       (log (string-to-number factor) 10.))))) nil nil value))
              (setq value (format "%.12e"
                                  (* (string-to-number factor)
                                     (string-to-number value))))
              (if (string-match "0+[eE]" value)
                  (setq value (replace-match "e" t t value)))))))
    (if (not ok)
        (and (interactive-p) (error "No such constant: %s" const))
      (if (or (interactive-p) message)
          (progn
            (kill-new value)
            (message "Value of `%s'%s is %s %s"
                     const
                     (if prefix (concat " (=" prefix-name "-" 
                                        (substring const 1) ")") "")
                     value
                     (if (and unit (string-match "\\S-" unit))
                         unit
                       "[no units]"))))
      value)))

;;;###autoload
(defun constants-replace ()
  "Replace the name of a constant at point with its value.
For example \"pi\" would be replaced by \"3.1415926535897932385\"."
  (interactive)
  (let (value)
    (save-excursion
      (skip-chars-backward "[a-zA-Z0-9]")
      (if (and (looking-at "[a-zA-Z][a-zA-Z0-9]*")
               (setq value (save-match-data
                             (constants-get (match-string 0) t))))
           (replace-match value t t)
        (error "No such constant: %s" (match-string 0))))))

(defun constants-get-value (entry mode)
  "Extract the correct value string from the entry."
  (let (ee val)
    (if (and mode (setq ee (assq mode entry)))
        (if (or (eq (nth 1 ee) 'default)
                (equal (nth 1 ee) ""))
            (setq ee (assq 'default entry))))
    (setq val 
          (if ee
              (cond ((eq constants-unit-system 'SI) (nth 1 ee))
                    ((eq constants-unit-system 'cgs) (or (nth 2 ee) (nth 1 ee)))
                    (t nil))
            (cond ((eq constants-unit-system 'SI) (nth 3 entry))
                  ((eq constants-unit-system 'cgs) (nth 4 entry))
                  (t nil))))
    (if (and val (stringp val) (string-match "\\(.*?\\) *\\[\\(.*\\)\\]" val))
        (match-string 1 val)
      val)))

(defun constants-get-unit (entry &optional mode)
  "Extract the unit string from the entry.  MODE will be ignored."
  (let ((val (cond ((eq constants-unit-system 'SI) (nth 3 entry))
                   ((eq constants-unit-system 'cgs) (nth 4 entry))
                   (t nil))))
    (if (and val (stringp val) (string-match "\\(.*?\\) *\\[\\(.*\\)\\]" val))
        (match-string 2 val)
      "")))

(defun constants-assoc (key table &optional follow)
  "Case-insensitive assoc on first and second list element.
When FOLLOW is non-nil, check if the match is a rename cell
and follow it up."
  (catch 'exit
    (let ((key1 (downcase key)) entry)
      (while (setq entry (car table))
	(if (and (consp entry)
                 (or (equal key1 (downcase (car entry)))
                     (and (consp (cdr entry))
                          (equal key1 (downcase (nth 1 entry))))))
            (if (stringp (cdr (car table)))
                (throw 'exit (constants-assoc (cdr (car table)) table))
              (throw 'exit (car table))))
        (setq table (cdr table)))
      nil)))

(defun constants-completing-read (&rest args)
  "Completing read, case insensitive."
  (let ((old-value (default-value 'completion-ignore-case))
        (minibuffer-local-completion-map 
         (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map "?" 
      (lambda () 
        (interactive)
        (let ((major-mode constants-major-mode))
          (constants-help nil 'completing))))
    (define-key minibuffer-local-completion-map [(shift tab)] 'constants-scroll-help)
    (unwind-protect
	(progn
	  (setq-default completion-ignore-case t)
	  (apply 'completing-read (car args)
                 'constants-completion-function
                 (cdr (cdr args))))
      (setq-default completion-ignore-case old-value))))

(defun constants-scroll-help ()
  (interactive)
  (let ((cw (selected-window))
        (hw (get-buffer-window "*Help*")))
    (if hw
        (progn
          (select-window hw)
          (condition-case nil
              (scroll-up)
            (error (goto-char (point-min))))
          (select-window cw)))))

(defun constants-make-completion-table (varnames constants)
  "Make completion table containing all allowed names."
  (let ((all 
         (delq nil
               (append
                (mapcar 'car varnames)
                (mapcar (lambda(x) (if (consp x) (car x)))
                        constants)
                (mapcar (lambda(x) (if (consp x) (nth 1 x)))
                        constants))))
        (seen '(""))
        rtn dc)
    (while all
      (setq dc (downcase (car all)))
      (if (not (member dc seen))
          (setq rtn (cons (list (car all)) rtn)
                seen (cons dc seen)))
      (setq all (cdr all)))
    rtn))

(defun constants-completion-function (string predicate &optional flag)
  (let (s1 s2 rtn)
    (if (string-match "^\\(.*[*,=]\\)\\([^*,]*\\)$" string)
        (setq s1 (match-string 1 string)
              s2 (match-string 2 string))
      (setq s1 "" s2 string)) 
    (cond
     ((eq flag nil)
      ;; try completion
      (setq rtn (try-completion s2 ctable))
      (if (stringp rtn) (concat s1 s2 (substring rtn (length s2))))
      )
     ((eq flag t)
      ;; all-completions
      (all-completions s2 ctable)
      )
     ((eq flag 'lambda)
      ;; exact match?
      (assoc s2 ctable)))
    ))

;;;###autoload
(defun constants-help (&optional unit-system completing)
  "List all available constants.
The values are for the currently selected unit system.  When called
with prefix argument UNIT-SYSTEM, the \"other\" unit system will be
used.  I.e., if your default is `SI', then a prefix arg will switch to
`cgs' and vice versa."

  (interactive "P")
  (with-output-to-temp-buffer "*Help*"
    (let* ((constants-unit-system
            (cond ((and unit-system (symbolp unit-system)) unit-system)
                  (unit-system (if (eq constants-unit-system 'SI) 'cgs 'SI))
                  (t constants-unit-system)))
           (all (append constants-user-defined constants-defaults))
           (us (symbol-name constants-unit-system))
           (mode major-mode)
           fentry entry)

      ;; Check for aliasing
      (while (and (setq fentry (assq mode constants-languages))
                  (and (cdr fentry) (symbolp (cdr fentry))))
        (setq mode (cdr fentry)))
      
      (if constants-user-defined (setq all (cons "User defined entries" all)))
      (princ (format
"            List of constants: %s
Description                    Short      Long name       Value [%s]
-------------------------------------------------------------------------------
" 
(if completing "Use Shift-<TAB> to scroll" "") us))
      (while (setq entry (pop all))
        (if (stringp entry)
            (progn
              (princ "\n")
              (princ (make-string (/ (- 79 (length entry)) 2) ?.))
              (princ entry)
              (princ (make-string (/ (- 79 (length entry)) 2) ?.))
              (princ "\n"))
          (princ (format "%-30s %-10s %-15s %s %s\n"
                         (nth 2 entry) (nth 1 entry) (nth 0 entry)
                         (constants-get-value entry mode)
                         (constants-get-unit entry mode))))))
(let ((all constants-rename) entry)
      (princ "\nRenaming\n--------\n")
      (while (setq entry (pop all))
        (princ (format "%-15s refers to `%s'\n" (car entry) (cdr entry)))))
    (let ((all constants-prefixes) entry)
      (princ "\nUnit Prefixes\n-------------\n")
      (while (setq entry (pop all))
        (princ (format "%c  %-6s  %s\n"
                       (nth 0 entry) (nth 2 entry) (nth 1 entry)))))
    (let* ((all-constants (append constants-user-defined constants-defaults))
           (atable (append constants-rename all-constants))
           (ctable (constants-make-completion-table constants-rename
                                                    all-constants))
           const c1ass c1)
      (princ "
The following ambiguities are resolved by ignoring the unit prefix
------------------------------------------------------------------
")
      (while (setq const (car (pop ctable)))
        (if (and (assoc (string-to-char const) constants-prefixes)
                 (> (length const) 1)
                 (setq c1 (downcase (substring const 1)))
                 (setq c1ass (constants-assoc c1 atable 'follow)))
            (princ (format "%-15s refers to %-15s and not to %s-%s\n"
                           const 
                           (car (constants-assoc const atable))
                           (nth 2 (assoc (string-to-char const) constants-prefixes))
                           (if (or t (string= c1 (downcase (car c1ass))))
                               (car c1ass)
                             (nth 1 c1ass))))))))
  (save-window-excursion
    (select-window (get-buffer-window "*Help*"))
    (setq truncate-lines t)
    (goto-char (point-min))))

(defun constants-test (names)
  "Test constants-insert for several different modes.
To try it out, type '(constants-test)' into a buffer, put the cursor after
the closing parenthesis and execute \\[eval-last-sexp]."
  (let ((modes '(fortran-mode c-mode emacs-lisp-mode lisp-interaction-mode
                              idlwave-mode perl-mode cperl-mode))
        mode)
    (while (setq mode (pop modes))
      (insert "\n>>>>> " (symbol-name mode) "\n")
      (funcall mode)
      (constants-insert 'SI names)
      (constants-insert 'cgs names))
    (normal-mode)))

(provide 'constants)

;;; constants.el ends here

