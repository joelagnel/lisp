;;; etask-tex.el --- part of EtaskMode (main file: etask.el)

;; Copyright (C) 2004 René Weichselbaum

;; Author: Rene Weichselbaum

;; $Id: etask-tex.el,v 1.17 2004/10/28 21:26:09 rene Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


;;; Commentary:

;; This software component implements the creation of LaTeX bar
;; charts, frequently also called Gantt charts, for EtaskMode.


;;_________________


;;; Code:

(require 'calendar)

(defconst etask-tex-buffer "*etaskCHART*"
  "Name of the buffer used for Gantt chart tex file (in a new frame).")

(defcustom etask-tex-papersize "a4paper"
  "Specify paper size.

Examples: a4paper, letterpaper"
  :type 'string
  :group 'latex)

(defcustom etask-tex-processing-script ""
  "Script name to generate ps or pdf out of LaTeX.

etask starts the subprocess `etask-chart-gen' for the specified script
with one argument - the generated LaTeX file name without the .tex
ending - and shows its output in buffer '*etask-chart-gen*'.

This is a very simple sample script:

#!/bin/sh
file=`basename $1`
cd /path/to/your/chart/
latex $file.tex
dvips -Ppdf -t landscape $file.dvi
ps2pdf $file.ps
xpdf $file.pdf"
  :type 'string
  :group 'latex)

(defcustom etask-tex-header-color1 "\\definecolor{headercolor1}{cmyk}{0, 0.70, 0.70, 0.2} %logored"
  "1st color for Gantt chart header.  This color can be used in
`etask-tex-custom-header'.

Example:
   \\makebox[9.6cm][c]\{\\huge \\sc \\color\{
   etask-tex-header-color1-name
   \} Ren\\'e Weichselbaum\}

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-header-color1-name (substring 
                                     etask-tex-header-color1
                                     (1+ (string-match 
                                          "{" 
                                          etask-tex-header-color1))
                                     (string-match 
                                      "}" 
                                      etask-tex-header-color1))
  "String identifier for color1 in Gantt chart.")

(defcustom etask-tex-header-color2 "\\definecolor{headercolor2}{rgb}{0.12, 0.12, 0.6} %sloganblue"
  "2nd color for Gantt chart header.  This color can be used in
`etask-tex-custom-header'.  See also `etask-tex-header-color1'.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-header-color2-name (substring 
                                          etask-tex-header-color2
                                          (1+ (string-match 
                                               "{" 
                                               etask-tex-header-color2))
                                          (string-match 
                                           "}" 
                                           etask-tex-header-color2))
  "String identifier for color2 in Gantt chart.")

(defcustom etask-tex-custom-header ""
  "Header as LaTeX statement; you can, for example, design or include
your Company logo here.  Ensure that you do not exceed the header
height defined in `etask-tex-header-height'.

If you do not define this variable etask generates a header for you that consists of `etask-organization' and `etask-organization-slogan'.

Example:

\\makebox[9.6cm][c]\{\\huge \\sc \\color\{headercolor1\} Ren\\'e Weichselbaum\}
\\makebox[14.4cm][r]\{\\footnotesize \\sc \\color\{headercolor2\} Sound Advice. Reliable Software.\}\\\\ [-3mm]
\\makebox[9.6cm][c]\{\\color\{headercolor1\} \\rule[0mm]\{9.6cm\}\{0.3pt\}\}\\\\ [1mm]
\\makebox[9.6cm][c]\{\\large \\sc \\color\{headercolor1\} Software Engineering \{\\normalsize \\&\} Consulting\}\\\\ [10mm]"
  :type 'string
  :group 'latex)

(defcustom etask-tex-header-height 34
  "Height of your Gantt chart header in mm.  See `etask-tex-header'."
  :type 'integer
  :group 'latex)

(defcustom etask-tex-text-width 250
  "Maximum LaTeX Gantt chart width in millimeter.

See also the LaTeX variable \\textwidth."
  :type 'integer
  :group 'latex)

(defcustom etask-tex-text-height 180
  "Maximum LaTeX Gantt chart height including
`etask-tex-header-height' in millimeter.

See also the LaTeX variable \\textheight."
  :type 'integer
  :group 'latex)

(defcustom etask-tex-custom-latex-lengths
  (concat
   "\\setlength{\\parindent}{0mm}\n"
   "\\setlength{\\footskip}{20mm}\n"
   "\\setlength{\\oddsidemargin}{0mm}\n"
   "\\setlength{\\topmargin}{-30mm}\n"
   "\\setlength{\\parskip}{1.2ex}\n"
   "\\setlength{\\parindent}{0mm}\n")
  "Customize page layout for LaTeX file generation."
  :type 'string
  :group 'latex)

(defcustom etask-tex-color-tasknames-p t
  "If non-nil the tasknames in the Gantt chart are colored according
to their fill colors: Go to `etask-tex-color-expended-*' to change
these colors."
  :type 'boolean
  :group 'latex)

(defcustom etask-tex-taskbar-invisible-border-p nil
  "If non-nil the task bar borders and their fill colors are the same.
Otherwise, the borders are black."
  :type 'boolean
  :group 'latex)

(defcustom etask-tex-taskbar-linewidth "0.05mm"
  "Width of the task bar borders."
  :type 'string
  :group 'latex)

(defcustom etask-tex-taskbar-fillstyle "solid"
  "In {none, solid, vlines, vlines*, hlines, hlines*, crosshatch,
crosshatch*}

The * versions also fill the background.  'none' is the default value."
  :type 'string
  :group 'latex)

(defcustom etask-tex-color-expended-normal "\\definecolor{expendednormal}{rgb}{0.26,0.56,0.26} %darkgreen"
  "Color for expended effort of normal tasks in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-color-expended-normal-name "expendednormal" 
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-color-expended-highrisk "\\definecolor{expendedhighrisk}{rgb}{0.26,0.26,0.56} %darkblue"
  "Color for expended effort of high risk tasks in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-color-expended-highrisk-name "expendedhighrisk" 
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-color-expended-critical "\\definecolor{expendedcritical}{rgb}{0.56,0.26,0.26} %darkred"
  "Color for expended effort of critical path tasks in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-color-expended-critical-name "expendedcritical" 
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-color-open-normal "\\definecolor{opennormal}{rgb}{0.86,1,0.86} %lightgreen"
  "Color for open effort of normal tasks in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-color-open-normal-name "opennormal" 
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-color-open-highrisk "\\definecolor{openhighrisk}{rgb}{0.86,0.86,1} %lightblue"
  "Color for open effort of highrisk tasks in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-color-open-highrisk-name "openhighrisk" 
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-color-open-critical "\\definecolor{opencritical}{rgb}{1,0.86,0.86} %lightred"
  "Color for open effort of critical path tasks in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-color-open-critical-name "opencritical" 
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-today-vertical-line-p t
  "If non-nil vertical today line is inserted in Gantt chart."
  :type 'boolean
  :group 'latex)

(defcustom etask-tex-color-today-vertical-line "\\definecolor{todaycolor}{rgb}{0.65,0,0} %darkred"
  "Color for vertical line at current date in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-today-vertical-line-color-name "todaycolor"
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-today-vertical-line-width "0.2mm"
  "Width of vertical today line in Gantt chart."
  :type 'string
  :group 'latex)

(defcustom etask-tex-today-vertical-line-style "solid"
  "Line style of vertical today line in Gantt chart.

Values: solid, dotted, dashed, none; solid is the default value"
  :type 'string
  :group 'latex)

(defcustom etask-tex-year-vertical-line-p t
  "If non-nil vertical year line is inserted in Gantt chart."
  :type 'boolean
  :group 'latex)

(defcustom etask-tex-color-year-vertical-line "\\definecolor{yearcolor}{rgb}{0,0,0} %black"
  "Color for vertical line at beginning of a year in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-year-vertical-line-color-name "yearcolor"
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-year-vertical-line-width "0.2mm"
  "Width of vertical year line in Gantt chart."
  :type 'string
  :group 'latex)

(defcustom etask-tex-year-vertical-line-style "solid"
  "Line style of vertical year line in Gantt chart.

Values: solid, dotted, dashed, none; solid is the default value"
  :type 'string
  :group 'latex)

(defcustom etask-tex-month-vertical-line-p t
  "If non-nil vertical month line is inserted in Gantt chart according
to `etask-tex-month-filter-values'."
  :type 'boolean
  :group 'latex)

(defcustom etask-tex-color-month-vertical-line "\\definecolor{monthcolor}{rgb}{0,0,0} %black"
  "Color for vertical line at beginning of a month in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-month-vertical-line-color-name "monthcolor"
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-month-vertical-line-width "0.2mm"
  "Width of vertical month line in Gantt chart."
  :type 'string
  :group 'latex)

(defcustom etask-tex-month-vertical-line-style "dashed"
  "Line style of vertical month line in Gantt chart.

Values: solid, dotted, dashed, none; solid is the default value"
  :type 'string
  :group 'latex)

(defcustom etask-tex-week-vertical-line-p t
  "If non-nil vertical week lines are inserted in Gantt chart
according to `etask-tex-week-filter-values' ."
  :type 'boolean
  :group 'latex)

(defcustom etask-tex-insert-iso-week-p t
  "If non-nil the ISO week is inserted."
  :type 'boolean
  :group 'latex)

(defcustom etask-tex-color-week-vertical-line "\\definecolor{weekcolor}{rgb}{0,0,0} %black"
  "Color for vertical line at beginning of a week in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-week-vertical-line-color-name "weekcolor"
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-week-vertical-line-width "0.2mm"
  "Width of vertical week line in Gantt chart."
  :type 'string
  :group 'latex)

(defcustom etask-tex-week-vertical-line-style "dotted"
  "Line style of vertical week line in Gantt chart.

Values: solid, dotted, dashed, none"
  :type 'string
  :group 'latex)

(defcustom etask-tex-day-vertical-line-p t
  "If non-nil and one day scales to more than 1mm then vertical day
lines are inserted at the top of the Gantt chart."
  :type 'boolean
  :group 'latex)

(defcustom etask-tex-color-day-vertical-line "\\definecolor{daycolor}{rgb}{0,0,0} %black"
  "Color for vertical line at beginning of a day in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defcustom etask-tex-color-holiday-line "\\definecolor{holidaycolor}{rgb}{1,0.66,0.66} %lightred"
  "Color for horizontal and vertical line at beginning of a holiday in
Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-day-vertical-line-normalcolor-name "daycolor"
  "String identifier for color in Gantt chart.")

(defconst etask-tex-day-holidaycolor-name "holidaycolor"
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-day-vertical-line-width "0.2mm"
  "Width of vertical day line in Gantt chart."
  :type 'string
  :group 'latex)

(defcustom etask-tex-day-vertical-line-style "solid"
  "Line style of vertical day line in Gantt chart.

Values: solid, dotted, dashed, none"
  :type 'string
  :group 'latex)

(defcustom etask-tex-taskline-p t
  "If non-nil a grid is inserted in Gantt chart."
  :type 'boolean
  :group 'latex)

(defcustom etask-tex-color-taskline "\\definecolor{taskcolor}{rgb}{0.86,0.86,0.86} %grey"
  "Color for horizontal and vertical task lines in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-taskline-color-name "taskcolor"
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-taskline-width "0.02mm"
  "Width of task lines in Gantt chart."
  :type 'string
  :group 'latex)

(defcustom etask-tex-taskline-style "solid"
  "Line style of task lines in Gantt chart.

Values: solid, dotted, dashed, none"
  :type 'string
  :group 'latex)

(defcustom etask-tex-color-milestones-p t
  "If non-nil milestone colors are set to their criticality in Gantt
chart."
  :type 'boolean
  :group 'latex)

(defcustom etask-tex-linecolor-milestone "\\definecolor{lcmilestone}{rgb}{0,0,0} %black"
  "Color for milestone border in Gantt chart.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-linecolor-milestone-name "lcmilestone"
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-fillcolor-milestone "\\definecolor{fcmilestone}{rgb}{0,0,0} %black"
  "Color for milestone in Gantt chart.  Used if
`etask-tex-color-milestones-p' is nil.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-fillcolor-milestone-name "fcmilestone"
  "String identifier for color in Gantt chart.")

(defcustom etask-tex-milestone-dotstyle "diamond"
  "Style of a milestone.

Values according to PSTricks dot styles:
* 
+ 
| 
o 
x 
asterisk 
diamond* 
diamond 
oplus
otimes
pentagon*
pentagon
square* 
square 
triangle* 
triangle"
  :type 'string
  :group 'latex)

(defcustom etask-tex-milestone-scale 3
  "Scale of a milestone."
  :type 'number
  :group 'latex)

(defcustom etask-tex-default-label ""
  "Default label in the footer for Gantt chart."
  :type 'string
  :group 'latex)

(defcustom etask-tex-color-tasknames-header "\\definecolor{tasknamesheadercolor}{rgb}{0,0,0} %black"
  "Color for task names header in Gantt chart -- not used.

Syntax:
\\definecolor{name}{model}{color specification} %comment.

Do not change the prefix \\definecolor{name}.

How to define your own color:
-----------------------------
model ... in {rgb, cmyk, gray}

color specification for rgb
  comma-separated list of 3 real numbers 
  between 0 and 1 (color components: red-green-blue)

color specification for cmyk
  comma-separated list of 4 real numbers 
  between 0 and 1 (cyan magenta yellow black)

color specification for gray
  grayscale, a single real number 
  between 0 (black) and 1 (white)"
  :type 'string
  :group 'latex)

(defconst etask-tex-color-tasknames-header-name "tasknamesheadercolor"
  "String identifier for color in Gantt chart.")

(defconst etask-tex-timeline-height 12
  "Height of timeline at the top of the Gantt chart in mm.")

(defconst etask-tex-holiday-width 2
  "Width of Gantt chart holiday marker in mm.")

(defconst etask-tex-letter-width 2
  "Magic number: Width of one upper case letter in mm in LaTeX
chart.")

(defconst etask-tex-subitemoffset 2
  "Subitem indentation in mm.")

(defconst etask-tex-all-month-letters-width 20
  "Minimum month width in LaTeX Gantt chart in mm to display all its letters.")

(defconst etask-tex-3-month-letters-width 11
  "Minimum month width in LaTeX Gantt chart in mm to display all its letters.")

(defconst etask-tex-2-month-letters-width 8
  "Minimum month width in LaTeX Gantt chart in mm to display all its letters.")

(defconst etask-tex-1-month-letter-width 5
  "Minimum month width in LaTeX Gantt chart in mm to display all its letters.")

(defconst etask-tex-min-taskbar-space 2
  "Minimum space between task name and its surrounding frame in LaTeX
chart in mm.")

(defcustom etask-tex-taskbar-height 6
  "Height of taskbar in LaTeX chart in mm."
  :type 'integer
  :group 'latex)

(defcustom etask-tex-task-height 8
  "Height of a task name at the left side of the Gantt chart in mm.

A task name is inserted via \\rput(x,y){name} - see PSTricks package.
This variable is y1-y2, where y1 belongs to the task that is above the
task with y value y2."
  :type 'integer
  :group 'latex)

(defcustom etask-tex-space-between-delimiter 5
  "Space between delimiter line and content in mm.

   Task 01|     |            |        ==--------
   Task 02|     |     C      |    ======-----
   Task 03|     |     C      |        ==-------

task names|____|||___| |___|||___|task bars
            ^      ^task ^     ^
            |      |type |     |
            - - - - - - - - - - - - - customize this width"
  :type 'integer
  :group 'latex)

(defcustom etask-tex-week-filter-values
  '((1 1)
    (0.7 2)
    (0.5 5)
    (0 10))
  "List of (FACTOR MODULO) value pairs to determine if vertical week
line and its corresponding ISO week are displayed.  FACTOR is the
width of a day in mm.  Factors must be sorted descending, i.e. the
pair with the greater FACTOR comes first.

FACTOR ... number, >= 0
MODULO ... whole number

For example, assume the following expression:

\'((1 1) (0.7 2) (0 10))

This expression specifies that if one day in your Gantt chart scales
to more than 1mm then all weeks are displayed, but if one day is only
in the interval ]0.7 1] then every 2nd week is shown.  If FACTOR
equals 0 then its MODULO value is used for all other day lengths.  If
MODULO is zero nothing is displayed.  For example, in '((1 1) (0.7 2)
\(0.6 0) (0 10)) nothing is displayed if one day is greater than 0.6mm
and not greater than 0.7mm."
  :type 'sexp
  :group 'latex)

(defcustom etask-tex-month-filter-values
  '((0.5 1)
    (0.7 2)
    (0.5 5)
    (0 10))
  "(FACTOR MODULO) value pairs to determine if vertical month line is
displayed.  For details see `etask-tex-week-filter-values'."
  :type 'sexp
  :group 'latex)

(defcustom etask-tex-inputencoding "latin1"
  "Specify input encoding."
  :type 'string
  :group 'latex)

(defconst etask-tex-bookmark-tasknames "TASKNAMES COLUMN"
  "Bookmark in LaTeX source file.")

(defconst etask-tex-bookmark-tasktypes "TASKTYPES COLUMN"
  "Bookmark in LaTeX source file.")

(defconst etask-tex-bookmark-timeline "TIMELINE"
  "Bookmark in LaTeX source file.")

(defconst etask-tex-bookmark-taskbars "TASKBARS"
  "Bookmark in LaTeX source file.")

(defcustom etask-tex-custom-commands ""
  "Add LaTeX commands not covered so far.

Format:
\(concat \"your_first_command\" \"your_next_command_string\")"
  :type 'string
  :group 'latex)


; <------------- etask-tex-text-width ------------------------------------>
; ^                                                   ^                   |
; |    HEADER                                         |                   |
; |                                                   |                   |
; etask-tex-text-height                     etask-tex-header-height       |
; |                                                   |                   |
; |                                                   V                   |
; |- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - --
; |                                                   ^                   ^
; |    EMPTY                            | YEARS       |                   |
; |                                                   |                   |
; |                                     | -    etask-tex-timeline-height  |
; |                                                   |                   |
; |                                     | MONTHS      V                   |
; |- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -|
; |                  ^          |       | DAYS                            |
; |    EMPTY         |          |                                         |
; |                  |          |       |                                 |
; |       etask-tex-task-height |                                         |
; |                  V          |  etask-tex-space-between-delimiter   cheight
; |- - - - - - - - - - - - --   |     | |                                 |
; |        ^                ^ |---|---|                                   |
; |        |                | V | V   V -----------------------           |
; |        |     TASKNAME   |<->|<->C<->|   TASKBAR           |           |
; | etask-tex-task-height   |   |       -----------------------           |
; |        V                    |   T                                     |
; |- - - - - - - - - - - - -|   |   A   |                                 |
; |                             |   S                                     |
; |                         |   |   K   |   ----------------------        |
; |              TASKNAME       |           |  TASKBAR           |        |
; |                         |   |   I   |   ----------------------        |
; |                             |   N                                     |
; |- - - - - - - - - - - - -|   |   F   |                                 |
; |                             |   O                                     |
; |                         |   |   R   |       ---------------------     |
; |              TASKNAME       |   M           |  TASKBAR          |     |
; |                         |   |   A   |       ---------------------     |
; |                             |   T                                     |
; |- - - - - - - - - - - - -|   |   I   |                                 |
; |                             |   O                                     |
; |                         |   |   N   |            -------------------  |
; |              TASKNAME       |                    | TASKBAR         |  |
; |                         |   |   Z   |            -------------------  |
; |                             |   O                                     |
; |- - - - - - - - - - - - -|   |   N   |                                 |
; |                             |   E                                     |
; |               taskbarheight |       |                                 |
; |                         |   |                                         |
; V                         V   |       |                                 V
; <------- ctwidth -------->------------<----- cwidth -------------------->

;;; Interactive Functions

(defun etask-generate-latex-gantt-chart()
  "Generate a Gantt chart (LaTeX file) for all (marked) tasks.

If there are marked tasks, only these tasks are included."
  (interactive)
  (let ((tasklist (etask-cat-get-current-elts)))
    (if tasklist
        (let* ((markedtasks (etask-cat-get-marked-elements))
               (tasksinchart (if markedtasks
                                 markedtasks
                               tasklist))
               (num 0)
               (task)
               (texlabel
                (read-from-minibuffer 
                 (concat
                  (etask-lang-msg 320 etask-language)
                  ": "
                  nil
                  nil 
                  nil 
                  nil 
                  etask-tex-default-label))))
          (if (and (etask-multi-frame-p) etask-allow-popup-frames-p)
              (let ((frame-alist))
                (setq frame-alist
                      (cons (cons 'name
                                  (concat "GANTT CHART --- "
                                          (calendar-date-string 
                                           (calendar-current-date))))
                            frame-alist))
                (select-frame (make-frame frame-alist))
                (set-buffer (get-buffer-create etask-tex-buffer))
                (find-file (etask-get-chartfile))
                (goto-char (point-min))
                (erase-buffer)
                (etask-tex-generate-barchart tasksinchart texlabel)
                (run-hooks 'etask-tex-hook)
                (save-buffer))
            (progn
              (save-current-buffer
                (set-buffer (get-buffer-create etask-tex-buffer))
                (find-file (etask-get-chartfile))
                (goto-char (point-min))
                (erase-buffer)
                (etask-tex-generate-barchart tasksinchart texlabel)
                (run-hooks 'etask-tex-hook)
                (save-buffer))))
          (when (and (stringp etask-tex-processing-script)
                     (> (length etask-tex-processing-script) 0))
            (start-process "etask-chart-gen"
                           "*etask-chart-gen*" 
                           etask-tex-processing-script
                           (substring
                            (etask-get-chartfile)
                            (- 0 (- (length (etask-get-chartfile))
                                    (length etask-working-dir)))
                            -4))))      ;cut ".tex"
      (error "Gantt chart contains no data"))))


;;; LaTeX File Generation

(defun etask-tex-insert-header()
  "Insert chart header in LaTeX file."
  (let ((header
         (if (> (length etask-tex-custom-header) 0)
             etask-tex-custom-header
           (concat
            "\\makebox[\\width][c]\{\\huge \\sc \\color\{"
            etask-tex-header-color1-name
            "\} "
            etask-organization
            "\}\n"
            "\\hfill\n"
            "\\makebox[\\width][r]\{\\footnotesize \\sc \\color\{"
            etask-tex-header-color2-name
            "\} "
            etask-organization-slogan
            "\}"
            "\\\\[10mm]"))))
    (insert header)))

(defun etask-tex-insert-preamble(texlabel)
  "Insert preamble in LaTeX file."
  (insert "\\documentclass[landscape,12pt,")
  (insert etask-tex-papersize)
  (insert "]{article}\n")
  (insert "\\usepackage{ae}\n")
  (insert "\\usepackage[")
  (insert (downcase etask-language))
  (insert "]{babel}\n")
  (insert "\\usepackage[")
  (insert (downcase etask-tex-inputencoding))
  (insert "]{inputenc}\n")
  (insert "\\usepackage[T1]{fontenc}\n")
  (insert "\\usepackage[official]{eurosym}\n")
  (insert "\\usepackage{amssymb}\n")
  (insert "\\usepackage{pst-all}\n")
  (insert (concat etask-tex-header-color1 "\n"))
  (insert (concat etask-tex-header-color2 "\n"))
  (insert (concat etask-tex-color-expended-normal "\n"))
  (insert (concat etask-tex-color-expended-highrisk "\n"))
  (insert (concat etask-tex-color-expended-critical "\n"))
  (insert (concat etask-tex-color-open-normal "\n"))
  (insert (concat etask-tex-color-open-highrisk "\n"))
  (insert (concat etask-tex-color-open-critical "\n"))
  (insert (concat etask-tex-color-today-vertical-line "\n"))
  (insert (concat etask-tex-color-year-vertical-line "\n"))
  (insert (concat etask-tex-color-month-vertical-line "\n"))
  (insert (concat etask-tex-color-week-vertical-line "\n"))
  (insert (concat etask-tex-color-day-vertical-line "\n"))
  (insert (concat etask-tex-color-holiday-line "\n"))
  (insert (concat etask-tex-color-taskline "\n"))
  (insert (concat etask-tex-linecolor-milestone "\n"))
  (insert (concat etask-tex-fillcolor-milestone "\n"))
  (insert (concat etask-tex-color-tasknames-header "\n"))
  (insert "\\usepackage{fancyhdr}\n")
  (insert "\\special{! TeXDict begin /landplus90{true}store end }\n")
  (insert "\\setlength{\\textwidth}{")
  (insert (number-to-string etask-tex-text-width))
  (insert "mm}\n")
  (insert "\\setlength{\\textheight}{")
  (insert (number-to-string etask-tex-text-height))
  (insert "mm}\n")
  (insert etask-tex-custom-latex-lengths)
  (insert etask-tex-custom-commands)
  (insert "\\pagestyle{fancy}\n")
  (insert "\\fancyhf{}\n")
  (insert "\\renewcommand{\\headrulewidth}{0pt}\n")
  (insert "\\fancyfoot[C]{\n")
  (insert "\\makebox[\\textwidth][c]{\\footnotesize\\sf\%\n")
  (insert (concat " {\\sc "
                  (if (string= texlabel "p")
                      (car (cdr (etask-cat-get-current-item)))
                    texlabel)
                  "\\hspace{\\stretch{1}}\n"))
  (insert "  ---\\hspace{1cm}\\thepage\\hspace{1cm}---\n")
  (insert "   \\hspace{\\stretch{1}}\\today}}}\n\n")
  (insert "\%----------------------------------------\n")
  (insert "\\begin{document}\n")
  (insert "\%----------------------------------------\n\n"))

(defun etask-tex-insert-begin-picture(cheight)
  "Insert pspicture specification for chart page in LaTeX file
with width= `etask-tex-text-width' and height=CHEIGHT."
  (insert "\n\n")
  (insert "\\begin{pspicture}(0,0)(")
  (insert (number-to-string (etask-tex-num-cm etask-tex-text-width)))
  (insert ",")
  (insert (number-to-string (etask-tex-num-cm cheight)))
  (insert ")\n"))

(defun etask-tex-insert-end-picture()
  "Insert end statement for pspicture in LaTeX file."
  (insert "\n\\end{pspicture}\n"))

(defun etask-tex-generate-barchart(tasklist texlabel)
  "Generate Gantt chart of TASKLIST.  TEXLABEL is a string printed on
the left side of the footer - e.g the project name.  Configure the
variable `etask-tex-default-label' to set the default value for the
chart label promt."
  (let* ((projbegin (etask-calculate-earliestbegin tasklist))
         (projend (etask-calculate-lastend tasklist))
         (tasknum (length tasklist))
         (ctwidth (etask-tex-get-maxtaskwidth tasklist))
         (sp etask-tex-space-between-delimiter)
         (cwidth (- etask-tex-text-width 
                    ctwidth 
                    (* 3 sp) 
                    etask-tex-letter-width)) ;task type
         (cheight (- etask-tex-text-height
                     etask-tex-header-height))
         (taskbarheight (- cheight
                           etask-tex-timeline-height
                           ;; 1 line between timeline and rest
                           etask-tex-task-height))
         (taskbarx
          (+ ctwidth 
             (* 3 sp)
             etask-tex-letter-width))
         (taskbarcurrheight
          (* etask-tex-task-height tasknum))
         (vliney                           
          (- cheight etask-tex-timeline-height))
         (pages (ceiling (/ (float taskbarcurrheight)
                            taskbarheight)))
         (alreadyprinted 0)
         tasksonpage elements vlinelen contentstr)
    (etask-tex-insert-preamble texlabel)
    (while (> pages 0)
      (setq vlinelen
            (- vliney
               (if (> taskbarheight taskbarcurrheight)
                   (- taskbarheight taskbarcurrheight)
                 0)))

      (setq tasksonpage (/ (min taskbarheight taskbarcurrheight)
                           etask-tex-task-height))
      (setq elements (etask-get-sublist tasklist
                                        (1+ alreadyprinted) 
                                        (+ alreadyprinted tasksonpage)))
      (etask-tex-insert-header)

      ;; ---------- begin: picture ----------
      (etask-tex-insert-begin-picture cheight)
      (insert "\n\%" etask-tex-bookmark-tasknames "\n")

      ;; Insert something above the task names
      ;; (etask-tex-insert-tasknames-header)

      (etask-tex-insert-tasknames elements 
                                  ctwidth 
                                  taskbarheight)
      (insert (concat "\n\%" etask-tex-bookmark-tasktypes "\n"))
      (etask-tex-insert-vertical-line (+ ctwidth sp) vliney vlinelen)
      (etask-tex-insert-tasktypes elements
                                  (+ ctwidth (* 2 sp))
                                  taskbarheight)
      (insert (concat "\n\%" etask-tex-bookmark-timeline "\n"))
      (etask-tex-insert-timeline elements
                                 taskbarx
                                 cheight
                                 (/ etask-tex-timeline-height 2)
                                 cwidth)
      (insert "\n\%" etask-tex-bookmark-taskbars "\n")
      (etask-tex-insert-taskbars elements
                                 taskbarx
                                 taskbarheight
                                 cwidth
                                 projbegin
                                 projend)
      (insert "\n\% HIGHLIGHT SPECIAL DATES\n")
      (etask-tex-insert-special-date-lines taskbarx
                                           vliney
                                           vlinelen
                                           projbegin
                                           projend
                                           cwidth)
      (etask-tex-insert-end-picture)
      ;; ---------- end: picture ----------

      (setq alreadyprinted (+ alreadyprinted tasksonpage))
      (setq taskbarcurrheight
            (* etask-tex-task-height (- tasknum alreadyprinted)))
      (setq pages (1- pages))
      (when (> pages 0)
        (insert "\n\\newpage\n")))
    (insert "\\end{document}")))

(defun etask-tex-generate-tasknames-header(xpos ypos label)
  "Insert Gantt chart's label above tasknames in LaTeX file."
  (if label
      (progn
        (insert "\\rput[tr](")
        (insert (number-to-string (etask-tex-num-cm xpos)))
        (insert ",")
        (insert (number-to-string (etask-tex-num-cm ypos)))
        (insert "){\\color{")
        (insert etask-tex-color-tasknames-header-name)
        (insert "}")
        (insert label)
        (insert "}\n"))))

(defun etask-tex-insert-rputstring(align xpos ypos str)
  "Insert STR at position \(XPOS,YPOS\) using rput.  The string ALIGN
is in l,r,c"
  (insert "\\rput[")
  (insert align)
  (insert "](")
  (insert (number-to-string xpos))
  (insert ",")
  (insert (number-to-string ypos))
  (insert "){")
  (if etask-tex-color-tasknames-p
      (progn
        (insert "\\color{")
        (insert (etask-tex-taskbar-get-fillcolor 
                 (car tasklist) 
                 'expended))
        (insert "}")))
  (insert str)
  (insert "}\n"))

(defun etask-tex-insert-tasknames(tasklist xpos ypos)
  "Insert Gantt chart's task name column for the tasks in TASKLIST in
LaTeX file.

XPOS and YPOS specify the point where the first task name
is to be inserted.  Alignment: top, right"
  (if tasklist
      (let* ((sublevel (1- (length
                            (etask-cat-get-elementindex
                             (car tasklist)))))
             (currypos (- ypos (/ (float etask-tex-task-height) 2)))
             (tmp sublevel))
        (when (> sublevel 0)
          (while (> tmp 0)
            (etask-tex-insert-rputstring
             "l"
             (etask-tex-num-cm (+ etask-tex-subitemoffset 
                                  (* etask-tex-subitemoffset tmp)))
             (etask-tex-num-cm currypos)
             "-")
            (setq tmp (1- tmp)))
          (setq tmp sublevel))

        (etask-tex-insert-rputstring
         "l"
         (if (> sublevel 0)
             (etask-tex-num-cm (+ etask-tex-subitemoffset
                                  (* etask-tex-subitemoffset (1+ sublevel))))
           (etask-tex-num-cm (+ etask-tex-subitemoffset
                                (* etask-tex-subitemoffset sublevel))))
         (etask-tex-num-cm currypos)
         (etask-db-get (car tasklist) etask-db-attr-taskname))

        (if etask-tex-taskline-p
            (etask-tex-insert-horizontal-line
             0
             ypos
             etask-tex-text-width
             etask-tex-taskline-color-name
             etask-tex-taskline-width
             etask-tex-taskline-style))
        (if (and etask-tex-taskline-p
                 (not (cdr tasklist)))
            (etask-tex-insert-horizontal-line
             0
             (- ypos etask-tex-task-height)
             etask-tex-text-width
             etask-tex-taskline-color-name
             etask-tex-taskline-width
             etask-tex-taskline-style))
        (if (cdr tasklist)
            (etask-tex-insert-tasknames (cdr tasklist) 
                                        xpos 
                                        (- ypos
                                           etask-tex-task-height))))))

(defun etask-tex-insert-horizontal-line(xpos ypos len &optional linecolor linewidth linestyle)
  "Insert horizontal line (XPOS,YPOS) (XPOS+len,YPOS) in LaTeX file.

LINECOLOR, LINEWIDTH, and LINESTYLE are strings."
  (insert "\\psline")
  (if (or linecolor linewidth linestyle)
      (insert
       (concat
        "["
        (if linecolor
            (concat
             "linecolor="
             linecolor))
        (if linewidth
            (concat
             (if linecolor
                 ",")
             "linewidth="
             linewidth))
        (if linestyle
            (concat
             (if (or linewidth
                     (and linecolor (not linewidth)))
                 ",")
             "linestyle="
             linestyle))
        "]")))
  (insert "(")
  (insert (number-to-string (etask-tex-num-cm xpos)))
  (insert ",")
  (insert (number-to-string (etask-tex-num-cm ypos)))
  (insert ")(")
  (insert (number-to-string (etask-tex-num-cm (+ xpos len))))
  (insert ",")
  (insert (number-to-string (etask-tex-num-cm ypos)))
  (insert ")\n"))

(defun etask-tex-insert-vertical-line(xpos ypos len &optional linecolor linewidth linestyle)
  "Insert vertical line (XPOS,YPOS) (XPOS,YPOS-len) in LaTeX file.

LINECOLOR, LINEWIDTH, and LINESTYLE are strings."
  (insert "\\psline")
  (if (or linecolor linewidth linestyle)
      (insert 
       (concat
        "["
        (if linecolor
            (concat
             "linecolor="
             linecolor))
        (if linewidth
            (concat
             (if linecolor
                 ",")
             "linewidth="
             linewidth))
        (if linestyle
            (concat
             (if (or linewidth
                     (and linecolor (not linewidth)))
                 ",")
             "linestyle="
             linestyle))
        "]")))
  (insert "(")
  (insert (number-to-string (etask-tex-num-cm xpos)))
  (insert ",")
  (insert (number-to-string (etask-tex-num-cm ypos)))
  (insert ")(")
  (insert (number-to-string (etask-tex-num-cm xpos)))
  (insert ",")
  (insert (number-to-string (etask-tex-num-cm (- ypos len))))
  (insert ")\n"))

(defun etask-tex-insert-special-date-lines(xpos ypos height projbegin
projend projlen)
  "Insert vertical lines that highlight special days in LaTeX file.

\(XPOS,YPOS) is the upper left corner of the task bar region.  HEIGHT
is the task bar region's height.  PROJBEGIN and PROJEND are dates.
PROJLEN is the project length scaled to mm to fit into the task bar
region."
  (let* ((projdays (1+ (etask-days-between 
                        projbegin
                        projend)))
         (date projbegin)
         (days 0)
         (factor (/ (float projlen) projdays))
         (isoweek)
         (weekfilter 
          (etask-tex-filter 
           factor
           etask-tex-week-filter-values))
         (monthfilter
          (etask-tex-filter 
           factor
           etask-tex-month-filter-values))
         (printweekp)
         (str))
    (while (< days projdays)
      (setq isoweek
            (extract-calendar-month 
             (calendar-iso-from-absolute 
              (calendar-absolute-from-gregorian date))))
      (setq printweekp
            (cond ((or (not etask-tex-week-vertical-line-p)
                       (not weekfilter))
                   nil)
                  (t
                   (and (= (calendar-day-of-week date) 1)
                        (= (% isoweek
                              weekfilter)
                           0)))))
      (cond ((and etask-tex-today-vertical-line-p
                  (calendar-date-equal (calendar-current-date) date))
             (insert "\n\% Today:\n")
             (etask-tex-insert-vertical-line 
              (+ xpos
                 (* factor days))
              ypos
              height
              etask-tex-today-vertical-line-color-name
              etask-tex-today-vertical-line-width
              etask-tex-today-vertical-line-style)
             (if printweekp
                 (progn
                   (etask-tex-insert-iso-week (+ xpos
                                                 (* factor days))
                                              ypos
                                              date)
                   (insert "\n"))))
            
            ((and etask-tex-year-vertical-line-p
                  (= (extract-calendar-month date) 1)
                  (= (extract-calendar-day date) 1))
             (insert "\n\% Year (")
             (insert (number-to-string (extract-calendar-year date)))
             (insert "):\n")
             (etask-tex-insert-vertical-line 
              (+ xpos
                 (* factor days))
              ypos
              height
              etask-tex-year-vertical-line-color-name
              etask-tex-year-vertical-line-width
              etask-tex-year-vertical-line-style)
             (if (and printweekp
                      etask-tex-insert-iso-week-p)
                 (progn
                   (etask-tex-insert-iso-week (+ xpos
                                                 (* factor days))
                                              ypos
                                              date)
                   (insert "\n"))))

            ((and etask-tex-month-vertical-line-p
                  monthfilter
                  (= (extract-calendar-day date) 1)
                  (= (% (extract-calendar-month date)
                        monthfilter)
                     0))
             (insert "\n\% Month (")
             (insert (number-to-string (extract-calendar-month date)))
             (insert "):\n")
             (etask-tex-insert-vertical-line 
              (+ xpos
                 (* factor days))
              ypos
              height
              etask-tex-month-vertical-line-color-name
              etask-tex-month-vertical-line-width
              etask-tex-month-vertical-line-style)
             (if (and printweekp
                      etask-tex-insert-iso-week-p)
                 (progn
                   (etask-tex-insert-iso-week 
                    (+ xpos
                       (* factor days))
                    ypos
                    date)
                   (insert "\n"))))

            (printweekp
             (insert "\n\% Week:\n")
             (etask-tex-insert-vertical-line 
              (+ xpos
                 (* factor days))
              ypos
              height
              etask-tex-week-vertical-line-color-name
              etask-tex-week-vertical-line-width
              etask-tex-week-vertical-line-style)
             (if etask-tex-insert-iso-week-p
                 (etask-tex-insert-iso-week 
                  (+ xpos
                     (* factor days))
                  ypos
                  date))
             (insert "\n")))

      (if (and etask-tex-day-vertical-line-p
               (> factor 1))
          (etask-tex-insert-vertical-line 
           (+ xpos
              (* factor days))
           ypos
           etask-tex-holiday-width
           (if (etask-businessday-p date)
               etask-tex-day-vertical-line-normalcolor-name
             etask-tex-day-holidaycolor-name)
           etask-tex-day-vertical-line-width
           etask-tex-day-vertical-line-style))

      (if (and (> factor 1)
               (not (etask-businessday-p date)))
          (etask-tex-insert-horizontal-line
           (+ xpos
              (* factor days))
           (- ypos (/ (float etask-tex-holiday-width) 4))
           factor
           etask-tex-day-holidaycolor-name
           (concat
            (number-to-string (/ (float etask-tex-holiday-width) 2))
            "mm")))
      (setq date (etask-add-days-to-date date 1))
      (setq days (1+ days)))))

(defun etask-tex-insert-iso-week(xpos ypos date)
  "Insert ISO week number of DATE at (XPOS,YPOS) in LaTeX file.  See
also `calendar-iso-date-string'.  Alignment: top, left"
  (insert "\\rput[tl](")
  (insert (number-to-string
           (etask-tex-num-cm (1+ xpos))))
  (insert ",")
  (insert (number-to-string
           (etask-tex-num-cm (- ypos etask-tex-holiday-width 1))))
  (insert "){\\scriptsize ")
  (insert (number-to-string 
           (extract-calendar-month
            (calendar-iso-from-absolute 
             (calendar-absolute-from-gregorian date)))))
  (insert "}\n"))

(defun etask-tex-insert-tasktypes(tasklist xpos ypos)
  "Insert Gantt chart's task type column for the tasks in TASKLIST in
LaTeX file.

XPOS and YPOS specify the point where the first task type is to be
inserted."
  (if tasklist
      (let* ((type (etask-db-get (car tasklist) etask-db-attr-tasktype)))
        (insert "\\rput[c](")
        (insert (number-to-string (etask-tex-num-cm xpos)))
        (insert ",")
        (insert (number-to-string 
                 (etask-tex-num-cm 
                  (- ypos 
                     (/ (float etask-tex-task-height) 2)))))
        (insert "){")
        (cond ((string= type etask-normaltask-string)
               (insert "~}\\\\\n"))
              ((string= type etask-highrisktask-string)
               (insert "H}\\\\\n"))
              ((string= type etask-criticaltask-string)
               (insert "C}\\\\\n"))
              (t
               (insert "")))))
        (if (cdr tasklist)
            (etask-tex-insert-tasktypes (cdr tasklist)
                                        xpos
                                        (- ypos
                                           etask-tex-task-height))))

(defun etask-tex-insert-timeline(tasklist xpos ypos height width)
  "Insert Gantt chart's timeline in LaTeX file."
  (insert "\n\% YEARS\n")
  (etask-tex-insert-years tasklist xpos ypos height width)
  (insert "\n\% MONTHS\n")
  (etask-tex-insert-months tasklist 
                           xpos 
                           (- ypos 
                              (/ etask-tex-timeline-height 2))
                           height 
                           width))

(defun etask-tex-insert-years(tasklist xpos ypos height width)
  "Insert the Gantt chart's year timeline in LaTeX file.

XPOS and YPOS specify the point (top, left) where the year timeline
should start.  This point and its HEIGHT and WIDTH in millimeter
define the year-frame to be generated."
  (let* ((projbegin (etask-calculate-earliestbegin tasklist))
         (projend (etask-calculate-lastend tasklist))
         (projyearbegin (extract-calendar-year projbegin))
         (projyears (1+ (- (extract-calendar-year projend)
                           projyearbegin)))
         (yearnum projyears)
         (projdays (1+ (etask-days-between projbegin projend)))
         (factor (/ (float width) projdays))
         (str)
         (year)
         (len))
    (insert "\n% Length of all years in mm = ")
    (insert (number-to-string
             (etask-simplify-number 
              (* factor projdays))))
    (insert "\n")
    (while (> yearnum 0)
      (setq year (+ projyearbegin (- projyears yearnum)))
      (setq len (etask-simplify-number 
                 (* factor 
                    (etask-projdays-in-year projbegin projend year))))
      (insert "\n% ")
      (insert (number-to-string year))
      (insert ": Length in mm = ")
      (insert (number-to-string len))
      (insert "  (1 day = ")
      (insert (number-to-string (etask-simplify-number factor)))
      (insert "mm)")
      (insert "\n")
      (insert "\\psframe(")
      (insert (number-to-string (etask-tex-num-cm xpos)))
      (insert ",")
      (insert (number-to-string (etask-tex-num-cm (- ypos height))))
      (insert ")(")
      (insert (number-to-string (etask-tex-num-cm (+ xpos len))))
      (insert ",")
      (insert (number-to-string (etask-tex-num-cm ypos)))
      (insert ")\n")
      (insert "\\rput[t](")
      (insert (number-to-string 
               (etask-tex-num-cm 
                (+ xpos (/ (float len) 2)))))
      (insert ",")
      (insert (number-to-string
               (etask-tex-num-cm (- ypos 1.5))))
      (insert "){")
      (cond ((> len 10)
             (insert (number-to-string year)))
            ((> len 5)
             (insert (substring (number-to-string year) 2)))
            (t
             (insert "")))
      (insert "}\n")
      (setq xpos (+ xpos len))
      (setq yearnum (1- yearnum)))))

(defun etask-tex-insert-months(tasklist xpos ypos height width)
  "Insert the Gantt chart's month timeline in LaTeX file.

XPOS and YPOS specify the point (top, left) where the month timeline
should start.  This point and its HEIGHT and WIDTH in millimeter
define the month-frame to be generated."
  (let* ((projbegin (etask-calculate-earliestbegin tasklist))
         (projend (etask-calculate-lastend tasklist))
         (projmonbegin (extract-calendar-month projbegin))
         (projyearbegin (extract-calendar-year projbegin))
         (projmonend (extract-calendar-month projend))
         (projyearend (extract-calendar-year projend))
         (projyears (1+ (- (extract-calendar-year projend)
                           projyearbegin)))
         (projdays (1+ (etask-days-between projbegin projend)))
         (factor (/ (float width) projdays))
         (projmonths (cond ((= projyearbegin projyearend)
                            (1+ (- projmonend projmonbegin)))
                           (t
                            (+
                             (1+ (- 12 projmonbegin))
                             (* (- projyears 2) 12)
                             projmonend))))
         (monnum 1)
         (month (extract-calendar-month projbegin))
         (year projyearbegin)
         (str)
         (days)
         (len))
    (insert "\n% Length of all months in mm = ")
    (insert (number-to-string
             (etask-simplify-number 
              (* factor projdays))))
    (insert "\n")
    (while (<= monnum projmonths)
      (setq days (etask-projdays-in-month monnum
                                          month 
                                          year 
                                          projbegin
                                          projend
                                          projmonths))
      (setq len (etask-simplify-number 
                 (* factor days)))
      (insert "\n% ")
      (insert (number-to-string month))
      (insert ": Length in mm = ")
      (insert (number-to-string len))
      (insert " Number of days = ")
      (insert (number-to-string days))
      (insert "\n")
      (insert "\\psframe(")
      (insert (number-to-string (etask-tex-num-cm xpos)))
      (insert ",")
      (insert (number-to-string (etask-tex-num-cm (- ypos height))))
      (insert ")(")
      (insert (number-to-string (etask-tex-num-cm (+ xpos len))))
      (insert ",")
      (insert (number-to-string (etask-tex-num-cm ypos)))
      (insert ")\n")
      (insert "\\rput[t](")
      (insert (number-to-string
               (etask-tex-num-cm 
                (+ xpos (/ (float len) 2)))))
      (insert ",")
      (insert (number-to-string
               (etask-tex-num-cm (- ypos 1.5))))
      (insert "){")
      (cond ((> len etask-tex-all-month-letters-width)
             (insert (calendar-month-name month)))
            ((> len etask-tex-3-month-letters-width)
             (insert (calendar-month-name month 3)))
            ((> len etask-tex-2-month-letters-width)
             (insert (calendar-month-name month 2)))
            ((> len etask-tex-1-month-letter-width)
             (insert (calendar-month-name month 1)))
            (t
             (insert "")))
      (insert "}\n")
      (if (< month 12)
          (setq month (1+ month))
        (setq month 1)
        (setq year (1+ year)))
      (setq xpos (+ xpos len))
      (setq monnum (1+ monnum)))))
    
(defun etask-tex-insert-taskbars(tasklist xpos ypos projlen projbegin
projend)
  "Insert Gantt chart's task bars for the tasks in LaTeX file.

XPOS and YPOS specify the point where a task bar is to be inserted.
Alignment: left, top

PROJLEN is the project length scaled to mm to fit in chart."
  (if tasklist
      (progn
        (if (etask-is-milestone-p (car tasklist))
            (etask-tex-insert-milestone (car tasklist) 
                                        xpos 
                                        ypos
                                        projlen
                                        projbegin
                                        projend)
          (etask-tex-insert-taskbar (car tasklist) 
                                    xpos 
                                    ypos 
                                    projlen
                                    projbegin
                                    projend))
        (if (cdr tasklist)
            (etask-tex-insert-taskbars (cdr tasklist) 
                                       xpos 
                                       (- ypos
                                          etask-tex-task-height)
                                       projlen
                                       projbegin
                                       projend)))))

(defun etask-tex-insert-milestone(task xpos ypos projlen projbegin projend)
  "Insert milestone in LaTeX file.  Project start is at (XPOS, YPOS,
alignment: left, top).  PROJLEN is the project length scaled to
millimeter."
  (let* ((projdays (1+ (etask-days-between 
                        projbegin
                        projend)))
         (factor (/ (float projlen) projdays))
         (daysbefore (etask-days-between
                      projbegin
                      (etask-db-get task etask-db-attr-taskbegin)))
         (taskstartpos
          (etask-simplify-number
           (* factor daysbefore))))
    (insert "\\psdots[linecolor=")
    (insert etask-tex-linecolor-milestone-name)
    (insert ",fillcolor=")
    (if etask-tex-color-milestones-p
        (insert (etask-tex-taskbar-get-fillcolor task 'expended))
      (insert etask-tex-fillcolor-milestone-name))
    (insert ",dotstyle=")
    (insert etask-tex-milestone-dotstyle)
    (insert ",dotscale=")
    (insert (number-to-string etask-tex-milestone-scale))
    (insert "](")
    (insert (number-to-string (etask-tex-num-cm
                               (+ xpos taskstartpos))))
    (insert ",")
    (insert (number-to-string (etask-tex-num-cm 
                               (- ypos 
                                  (/ (float etask-tex-task-height) 2)))))
    (insert ")\n")))

(defun etask-tex-insert-taskbar(task xpos ypos projlen projbegin projend)
  "Insert task bar of TASK in LaTeX file.

Project start is at (XPOS, YPOS, alignment: left, top).  PROJLEN is
the project length scaled to millimeter."
  (let* ((projdays (1+ (etask-days-between 
                        projbegin
                        projend)))
         (factor (/ (float projlen) projdays))
         (daysbefore (etask-days-between
                      projbegin
                      (etask-db-get task etask-db-attr-taskbegin)))
         (taskstartpos
          (etask-simplify-number
           (* factor daysbefore)))
         (taskdays (1+ (etask-days-between 
                        (etask-db-get task etask-db-attr-taskbegin)
                        (etask-db-get task etask-db-attr-taskend))))
         (tasklen 
          (etask-simplify-number
           (* factor taskdays)))
         (expendedpercent               ; [0,1]
          (etask-simplify-number
           (/ (float (etask-db-get task etask-db-attr-eeffort))
              (etask-db-get task etask-db-attr-peffort))))
         (expendedlen
          (etask-simplify-number
           (* tasklen expendedpercent)))
         (openlen
          (etask-simplify-number
           (* tasklen (- 1 expendedpercent)))))

    (insert (concat "\n%TASK " 
                    (etask-db-get task etask-db-attr-taskname)
                    ": \n"))

    (if(> expendedpercent 0)            ;already expended 
        (progn
          (insert "\\psframe[fillstyle=")
          (insert etask-tex-taskbar-fillstyle)
          (insert ",linewidth=")
          (insert etask-tex-taskbar-linewidth)
          (insert ",fillcolor=")
          (insert (etask-tex-taskbar-get-fillcolor task 'expended))
          (if etask-tex-taskbar-invisible-border-p
              (insert 
               (concat
                ",linecolor="
                (etask-tex-taskbar-get-fillcolor task 'expended))))
          (insert "](")
          (insert (number-to-string
                   (etask-tex-num-cm (+ xpos taskstartpos))))
          (insert ",")
          (insert (number-to-string
                   (etask-tex-num-cm 
                    (- ypos 
                       (- etask-tex-task-height
                          (etask-tex-calculate-taskbar-space))))))
          (insert ")(")
          (insert (number-to-string
                   (etask-tex-num-cm (+ xpos taskstartpos expendedlen))))
          (insert ",")
          (insert (number-to-string
                   (etask-tex-num-cm 
                    (- ypos 
                       (etask-tex-calculate-taskbar-space)))))
          (insert ")\n")))

     (if (< expendedpercent 1)          ;still open
         (progn
           (insert "\\psframe[fillstyle=")
           (insert etask-tex-taskbar-fillstyle)
           (insert ",linewidth=")
           (insert etask-tex-taskbar-linewidth)
           (insert ",fillcolor=")
           (insert (etask-tex-taskbar-get-fillcolor task))
           (if etask-tex-taskbar-invisible-border-p
               (insert 
                (concat
                 ",linecolor="
                 (etask-tex-taskbar-get-fillcolor task))))
           (insert "](")
           (insert (number-to-string
                    (etask-tex-num-cm (+ xpos taskstartpos expendedlen))))
           (insert ",")
           (insert (number-to-string
                    (etask-tex-num-cm 
                     (- ypos 
                        (- etask-tex-task-height
                           (etask-tex-calculate-taskbar-space))))))
           (insert ")(")
           (insert (number-to-string
                    (etask-tex-num-cm 
                     (+ xpos taskstartpos expendedlen openlen))))
           (insert ",")
           (insert (number-to-string
                    (etask-tex-num-cm 
                     (- ypos 
                        (etask-tex-calculate-taskbar-space)))))
           (insert ")\n")))))


;;; Utilities Functions

(defun etask-get-chartfile()
  "Return full path name of requested LaTeX Gantt chart file."
  (concat
   etask-working-dir
   etask-tex-filename-prefix
   "."
   (number-to-string (extract-calendar-day (calendar-current-date)))
   "."
   (calendar-month-name (extract-calendar-month (calendar-current-date)) 3)
   "."
   (number-to-string (extract-calendar-year (calendar-current-date)))
   ".tex"))

(defun etask-tex-get-maxtaskwidth(tasklist)
  "Return length of longest taskname in mm for LaTeX output."
  (let ((list))
    (while tasklist
      (setq sublevel
            (1- (length
                 (etask-cat-get-elementindex
                  (car tasklist)))))
      (setq list
            (cons 
             (make-string
              (+ (length (etask-db-get (car tasklist) etask-db-attr-taskname))
                 sublevel
                 etask-tex-subitemoffset)
              70)
             list))
      (setq tasklist (cdr tasklist)))
    (* etask-tex-letter-width (etask-longest-string-in-list list))))

(defun etask-tex-num-cm(num)
  "Return NUM, given in mm, in cm and simplify if necessary."
  (etask-simplify-number (/ (float num) 10)))

(defun etask-tex-filter(factor filter)
  "Return modulo value for FACTOR that determines which time labels
are displayed or nil if an assertion fails.

FACTOR is the width of a day in mm.  FILTER is a list of (FACTOR
MODULO) value pairs.  For details see `etask-tex-week-filter-values'."
  (if (and (> factor 0)
           filter)
      (let ((fac (car (car filter)))
            (mod (car (cdr (car filter)))))
        (if (and
             (numberp fac)
             (>= fac 0)
             (natnump mod))
            (cond ((> factor fac)
                   mod)
                  ((zerop fac) 
                   mod)
                  (t
                   (etask-tex-filter factor 
                                     (cdr filter))))
          nil))
    nil))

(defun etask-tex-taskbar-get-fillcolor(task &optional expended)
  "Return color string for TASK's bar.

If EXPENDED is nil, OPEN is assumed."
  (let ((type (etask-db-get task etask-db-attr-tasktype)))
    (if expended
        (cond ((string= type etask-normaltask-string)
               etask-tex-color-expended-normal-name)
              ((string= type etask-highrisktask-string)
               etask-tex-color-expended-highrisk-name)
              ((string= type etask-criticaltask-string)
               etask-tex-color-expended-critical-name)
              (t
               nil))
      (cond ((string= type etask-normaltask-string)
               etask-tex-color-open-normal-name)
            ((string= type etask-highrisktask-string)
             etask-tex-color-open-highrisk-name)
            ((string= type etask-criticaltask-string)
             etask-tex-color-open-critical-name)
            (t
             nil)))))

(defun etask-tex-calculate-taskbar-space()
  "Return taskbar space in mm.

Taskbar's y-center = taskname's y-center:

------------------------------------------
|      <--- taskbar space --->  
|----------------   |          -----------------
|     (1)       |   |          |             |
| TASKNAME     (2)  |   TYPE   | TASKBAR    (3)
|     (1)       |   |          |             |
|----------------   |          -----------------
|      <-|          |      -->
-        |          |      |
|      <--- taskbar space --->
|----------------   |                 ---------------
|               |   |                 |
| NEXT TASKNAME |   | NEXT T.         | NEXT TASKBAR
|               |   |                 |
-----------------   |                 ---------------
|                   |

   (1) ... etask-min-taskbar-space
   (2) ... etask-tex-task-height
   (3) ... etask-tex-taskbar-height"

  (max etask-tex-min-taskbar-space
       (/ (- (float etask-tex-task-height) 
             etask-tex-taskbar-height)
          2)))


;;; Initialization

(setq etask-tex-loaded-p t)
(provide 'etask-tex)


;;; etask-tex.el  end of file