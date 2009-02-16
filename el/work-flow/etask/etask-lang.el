;;; etask-lang.el --- part of EtaskMode (main file: etask.el)

;; Copyright (C) 2004 René Weichselbaum

;; Author: Rene Weichselbaum

;; $Id: etask-lang.el,v 1.34 2004/10/31 22:27:58 rene Exp $

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

;; This software component implements EtaskMode's multilingual
;; feature.


;; _________________


;;; Code:

;; Add other languages as you need but do not change the order
(defconst etasklang '("english" "german"))

(defconst msglist 
  '((100 . 
         (("Number of new tasks")
          ("Anzahl neuer Tasks")))
    (110 . 
         (("Completion in percent")
          ("Fertig in Prozent")))
    (120 . 
         (("Planned effort")
          ("Geplanter Aufwand")))
    (121 . 
         (("Planned")
          ("Geplant")))
    (122 . 
         (("Expended")
          ("Bisher")))
    (130 . 
         (("Full-time equivalents (FTE)")
          ("Vollzeitmitarbeiter (FTE)")))
    (140 . 
         (("Effort already expended")
          ("Bisheriger Aufwand")))
    (150 . 
         (("Add expended effort")
          ("Aufwand verbuchen")))
    (160 . 
         (("Subtract expended effort")
          ("Aufwand zuruecknehmen")))
    (170 . 
         (("Planned begin")
          ("Geplanter Beginn")))
    (180 . 
         (("Business days between")
          ("Arbeitstage zwischen")))
    (181 . 
         (("Calendar days between")
          ("Kalendertage zwischen")))
    (190 . 
         (("Bring begin forward")
          ("Beginn vorverlegen")))
    (200 . 
         (("Postpone begin")
          ("Beginn verschieben")))
    (210 . 
         (("Bring due date forward")
          ("Ende vorverlegen")))
    (220 . 
         (("Postpone due date")
          ("Ende verschieben")))
    (230 . 
         (("Bring forward")
          ("Vorverlegen")))
    (240 . 
         (("Postpone")
          ("Verschieben")))
    (250 . 
         (("Tracking: Algorithm (TAB for list)")
          ("Tracking: Algorithmus (TAB fuer Liste)")))
    (260 . 
         (("Confirm  deletion")
          ("Loeschen bestaetigen")))
    (270 . 
         (("Not deleted")
          ("Nicht geloescht")))
    (280 . 
         (("Go to")
          ("Gehe zu")))
    (290 . 
         (("New chart begin date")
          ("Neues Chart-Anfangsdatum")))
    (299 . 
         (("Rename")
          ("Umbenennen")))
    (300 . 
         (("New")
          ("Neu")))
    (301 . 
         (("Delete")
          ("Loeschen")))
    (302 . 
         (("New task name")
          ("Neuer Taskname")))
    (303 . 
         (("or RET for")
          ("oder RET für")))
    (304 . 
         (("Task name")
          ("Taskname")))
    (305 . 
         (("Planned begin")
          ("Geplanter Beginn")))
    (306 . 
         (("Planned end")
          ("Geplantes Ende")))
    (307 . 
         (("")
          ("")))
    (308 . 
         (("all")
          ("Alles")))
    (309 . 
         (("day")
          ("Tag")))
    (310 . 
         (("planner")
          ("Tagesplan")))
    (311 . 
         (("week")
          ("Woche")))
    (312 . 
         (("Zoom")
          ("Zoom")))
    (320 . 
         (("Chart Label (RET = no label, p = project name)")
          ("Chart Label (RET = kein Label, p = Projektname)")))
    (375 . 
         (("Need more data: ")
          ("Benoetige mehr Daten: ")))
    (376 . 
         (("Welcome! ")
          ("Willkommen! ")))
    (380 . 
         (("Choose: ")
          ("Waehlen Sie: ")))
    (381 . 
         (("")
          ("")))
    (390 . 
         (("")
          ("")))
    (400 . 
         (("Project")
          ("Projekt")))
    (401 . 
         (("ToDo")
          ("ToDo")))
    (402 . 
         (("Event")
          ("Event")))
    (410 . 
         (("New project name ")
          ("Neuer Projektname ")))
    (411 . 
         (("New todo category ")
          ("Neue Todo-Kategorie ")))
    (412 . 
         (("New event category ")
          ("Neue Event-Kategorie ")))
    (420 . 
         (("Project name")
          ("Projektname")))
    (421 . 
         (("Todo category")
          ("Todo-Kategorie")))
    (422 . 
         (("Event category")
          ("Event-Kategorie")))
    (430 . 
         (("Task")
          ("Task")))
    (431 . 
         (("Todo")
          ("Todo")))
    (432 . 
         (("Event")
          ("Event")))
    (450 . 
         (("Priority")
          ("Prioritaet")))
    (451 . 
         (("Due at")
          ("Bis")))

    ;; do not change these values
    (500 . 
         (("h")                         ;abbreviation for 'hours'
          ("h")))
    (502 . 
         (("min")                       ;abbreviation for 'minutes'
          ("min")))
    (504 . 
         (("d")
          ("d")))

    (506 . 
         (("")
          ("")))
    (508 . 
         (("")
          ("")))
    (510 . 
         (("")
          ("")))
    (511 . 
         (("More")
          ("Mehr")))
    (512 . 
         (("Other")
          ("Anderer Wert")))
    (514 . 
         (("")
          ("")))
    (516 . 
         (("")
          ("")))
    (518 . 
         (("")
          ("")))
    (520 . 
         (("")
          ("")))
    (522 . 
         (("")
          ("")))
    (524 . 
         (("")
          ("")))
    (540 . 
         (("Today")
          ("Heute")))
    (542 . 
         (("Tomorrow")
          ("Morgen")))
    (544 . 
         (("After Tomorrow")
          ("Uebermorgen")))
    (546 . 
         (("In a Week")
          ("In einer Woche")))
    (560 . 
         (("Due at (hh[:mm] [am/pm])")
          ("Bis (hh[:mm] [am/pm])")))
    (562 . 
         (("Start time (hh[:mm] [am/pm])")
          ("Beginnzeit (hh[:mm] [am/pm])")))
    (564 . 
         (("")
          ("")))
    (566 . 
         (("")
          ("")))
    (568 . 
         (("")
          ("")))
    (570 . 
         (("")
          ("")))
    (572 . 
         (("")
          ("")))
    (600 . 
         (("[Tracking: S-Shape-65]")
          ("[Tracking: S-Shape-65]")))
    (601 . 
         (("[Tracking: S-Shape-70]")
          ("[Tracking: S-Shape-70]")))
    (602 . 
         (("[Tracking: Linear]")
          ("[Tracking: Linear]")))
    (640 . 
         (("Sort: ")
          ("Sortieren: ")))
    (650 . 
         (("Prio/Date")
          ("Prio/Datum")))
    (655 . 
         (("Date/Prio")
          ("Datum/Prio")))
    (660 . 
         (("Category/Prio")
          ("Kategorie/Prio")))
    (665 . 
         (("Category/Date")
          ("Kategorie/Datum")))
    (700 . 
         (("REPORT")
          ("BERICHT")))
    (701 . 
         (("S T A T U S    R E P O R T")
          ("S T A T U S    B E R I C H T")))
    (702 . 
         (("==========================")
          ("============================")))
    (703 . 
         ((" (today) ")
          (" (heute) ")))
    (704 . 
         (("and")
          ("und")))
    (705 . 
         (("business days")
          ("Arbeitstage")))
    (706 . 
         (("days")
          ("Tage")))
    (707 . 
         (("tasks")
          ("Tasks")))
    (708 . 
         (("calendar days")
          ("Kalendertage")))
    (710 . 
         (("BEHIND SCHEDULE")
          ("HINTER PLAN")))
    (711 . 
         (("ON-TIME")
          ("IM PLAN")))
    (712 . 
         (("MILESTONE")
          ("MEILENSTEIN")))
    (713 . 
         (("COMPLETED")
          ("ABGESCHLOSSEN")))
    (714 . 
         (("NOT STARTED YET")
          ("NOCH NICHT GESTARTET")))
    (715 . 
         (("IN PROGRESS")
          ("IN ARBEIT")))
    (720 . 
         (("day")
          ("Tag")))
    (721 . 
         (("business day")
          ("Arbeitstag")))
    (730 . 
         (("Target")
          ("Vorgabe")))
    (731 . 
         (("actual")
          ("aktuell")))
    (740 . 
         (("(normal)")
          ("(normal)")))
    (741 . 
         (("(high risk)")
          ("(risikoreich)")))
    (742 . 
         (("(critical)")
          ("(kritisch)")))
    (760 . 
         (("Effort planned")
          ("Geplanter Aufwand")))
    (761 . 
         (("person hour")
          ("Personenstunde")))
    (762 . 
         (("person hours")
          ("Personenstunden")))
    (763 . 
         (("person day")
          ("Personentag")))
    (764 . 
         (("person days")
          ("Personentage")))
    (765 . 
         (("Effort expended")
          ("Angefallener Aufwand")))
    (766 . 
         (("or")
          ("oder")))
    (767 . 
         (("Remaining Work")
          ("Offen")))
    (768 . 
         (("till task deadline")
          ("bis zur Task-Deadline")))
    (769 . 
         (("after task deadline")
          ("nach Task-Deadline")))
    (770 . 
         (("till milestone")
          ("bis zum Meilenstein")))
    (771 . 
         (("after milestone")
          ("nach Meilenstein")))
    (772 . 
         (("DAY")
          ("TAG")))
    (773 . 
         (("")
          ("")))
    (774 . 
         (("Project")
          ("Projekt")))
    (775 . 
         (("STATUS")
          ("STATUS")))
    (776 . 
         (("Year")
          ("Jahr")))
    (777 . 
         (("Month name")
          ("Monatsname")))
    (778 . 
         (("Task")
          ("Task")))
    (779 . 
         (("Help")
          ("Hilfe")))
    (780 . 
         (("Quit Help")
          ("Hilfe ausblenden")))
    (781 . 
         (("Enter name")
          ("Name")))
    (782 . 
         (("Refresh/Execute")
          ("Aktualisieren/Ausfuehren")))
    (783 . 
         (("Same Flag Again")
          ("Flag nochmals setzen")))
    (790 . 
         (("Tasks file name (full path): ")
          ("Task-Dateiname (inkl. Pfad): ")))


    ;; Online Help

    (800 . 
         (("   GENERAL   :       DATA I      :       DATA II      :   MOVING/SCROLLING")
          ("  ALLGEMEIN  :      DATEN I      :      DATEN II      :   BEWEGEN/SCROLLEN")))
    (801 . 
         (("?  More Help : C-ca Add/Del/Ren  : I        Notes     : C-[np],n,p,t     Move^v")
          ("?  Mehr Hilfe: C-ca Neu/Entf/Umb : I        Notizen   : C-[np],n,p,t   Auf/Ab^v")))
    (802 . 
         (("q  Quit      : C-cs Switch to    : %,+,-    Effort    : g,1-9,N,P,M-[np] Jump^v")
          ("q  Beenden   : C-cs Wechseln zu  : %,+,-    Aufwand   : g,1-9,N,P,M-[np] Gehe^v")))
    (803 . 
         (("v  Version   : i    Insert       : m,M,SPC  Mark      : C-[fb]   Scroll Days")
          ("v  Version   : i    Einfuegen    : m,M,SPC  Markieren : C-[fb]   Scrollen Tage")))
    (804 .                                                  
         (("R  Report    : C-ui    Subitem   : u        Unmark    : M-[fb]   Scroll Weeks")
          ("R  Bericht   : C-ui    Subelement: u    Mark. aufheben: M-[fb]   Scrollen Wo.")))
    (805 .                                                 
         (("C  Chart     : E    Edit         : C-cft Element <<   : C-M-[fb] Scroll Months")
          ("C  Chart     : E    Edit         : C-cft Element <<   : C-M-[fb] Scrollen Mon.")))
    (806 .                                                                         
         (("w  Warranty  : C-xu Undo         : C-cpt Element >>   : <   Scroll to Begin")
          ("w  Garantie  : C-xu Undo         : C-cpt Element >>   : <   Scrollen zum Anf.")))
    (807 .                                                                                          
         (("L  Language  : C-k/C-y  Kill/Yank: C-cfb Begin <<  (p): >   Scroll to End")
          ("L  Sprache   : C-k/C-y  Entf/Einf: C-cfb Beginn << (p): >   Scrollen zum Ende")))
    (808 .                                                                            
         (("C-cd  Biz/Cal: e    Set Effort   : C-cpb Begin >>  (p): z   Zoom Chart")     
          ("C-cd  Biz/Kal: e    Aufwand      : C-cpb Beginn >> (p): z   Chart zoomen"))) 
    (809 .                                                                                               
         (("C-cC-d B or C: f    Set FTE   (p): C-cfd DueDate <<(p): B   Set Chart Begin")
          ("C-cC-d B od C: f    FTE       (p): C-cfd Ende <<   (p): B   Chart-Beginn"))) 
    (810 .                                                                    
         (("C-cC-b Bug   : c,h  Criticality  : C-cpd DueDate >>(p): j,k,J,K  Scroll Hours")
          ("C-cC-b Bug-  : c,h  Kritikalitaet: C-cpd Ende >>   (p): j,k,J,K  Scrollen Std"))) 
    (811 .                                                                                         
         (("       Report: T    Tracking  (p): C-M-[ud] Element ^v: ,    El's first day")
          ("       report: T    Tracking  (p): C-M-[ud] Element ^v: ,    Erster El.-Tag")))
    (812 .                                                                                                                              
         (("C-cC-f Feed- : b    Begin        : C-u<level>X   Level: C-u, El's last day")
          ("C-cC-f Feed- : b    Beginn       : C-u<level>X   Level: C-u, Letzter El.-Tag")))
    (813 .                                                                                                                               
         (("       back  : l    Link      (p): d/a,A   Del/Archive: .    Current (Day/Hour)")
          ("       back  : l    Verlinken (p): d/a,A  Entf, Archiv: .    Aktuell (Tag/Std)")))


    ;; General Information

    (900 .
         (("Test Release `")
          ("Test Release `")))
    (901 .
         (("', Copyright (C) 2004 René Weichselbaum\nThis software comes with ABSOLUTELY NO WARRANTY -- for details type `w'.")
          ("', Copyright (C) 2004 René Weichselbaum\nDie Veroeffentlichung dieses Programms erfolgt OHNE IRGENDEINE GARANTIE -- zu den Details mit `w'.")))
    (905 .
         (("NO WARRANTY

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software 
Foundation; either version 2 of the License, or (at your option) any later 
version.

BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE
PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE
STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE
PROGRAM \"AS IS\" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND
PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU
ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY
COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE
PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR
INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA
BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER
OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.")

          ("KEINE GEWAEHRLEISTUNG

Dieses Programm ist freie Software. Sie koennen es unter den
Bedingungen der GNU General Public License, wie von der Free Software
Foundation veroeffentlicht, weitergeben und/oder modifizieren, entweder
gemaess Version 2 der Lizenz oder (nach Ihrer Option) jeder spaeteren
Version.

DA DAS PROGRAMM OHNE JEGLICHE KOSTEN LIZENZIERT WIRD, BESTEHT
KEINERLEI GEWAEHRLEISTUNG FUER DAS PROGRAMM, SOWEIT DIES GESETZLICH
ZULAESSIG IST. SOFERN NICHT ANDERWEITIG SCHRIFTLICH BESTAETIGT, STELLEN
DIE COPYRIGHT-INHABER UND/ODER DRITTE DAS PROGRAMM SO ZUR VERFUEGUNG,
,,WIE ES IST``, OHNE IRGENDEINE GEWAEHRLEISTUNG, WEDER AUSDRUECKLICH
NOCH IMPLIZIT, EINSCHLIESSLICH - ABER NICHT BEGRENZT AUF - MARKTREIFE
ODER VERWENDBARKEIT FUER EINEN BESTIMMTEN ZWECK. DAS VOLLE RISIKO
BEZUEGLICH QUALITAET UND LEISTUNGSFAEHIGKEIT DES PROGRAMMS LIEGT BEI
IHNEN. SOLLTE SICH DAS PROGRAMM ALS FEHLERHAFT HERAUSSTELLEN, LIEGEN
DIE KOSTEN FUER NOTWENDIGEN SERVICE, REPARATUR ODER KORREKTUR BEI
IHNEN.

IN KEINEM FALL, AUSSER WENN DURCH GELTENDES RECHT GEFORDERT ODER
SCHRIFTLICH ZUGESICHERT, IST IRGENDEIN COPYRIGHT-INHABER ODER
IRGENDEIN DRITTER, DER DAS PROGRAMM WIE OBEN ERLAUBT MODIFIZIERT ODER
VERBREITET HAT, IHNEN GEGENUEBER FUER IRGENDWELCHE SCHAEDEN HAFTBAR,
EINSCHLIESSLICH JEGLICHER ALLGEMEINER ODER SPEZIELLER SCHAEDEN, SCHAEDEN
DURCH SEITENEFFEKTE (NEBENWIRKUNGEN) ODER FOLGESCHAEDEN, DIE AUS DER
BENUTZUNG DES PROGRAMMS ODER DER UNBENUTZBARKEIT DES PROGRAMMS FOLGEN
\(EINSCHLIESSLICH - ABER NICHT BESCHRAENKT AUF - DATENVERLUSTE,
FEHLERHAFTE VERARBEITUNG VON DATEN, VERLUSTE, DIE VON IHNEN ODER
ANDEREN GETRAGEN WERDEN MUESSEN, ODER DEM UNVERMOEGEN DES PROGRAMMS, MIT
IRGENDEINEM ANDEREN PROGRAMM ZUSAMMENZUARBEITEN), SELBST WENN EIN
COPYRIGHT-INHABER ODER DRITTER UEBER DIE MOEGLICHKEIT SOLCHER SCHAEDEN
UNTERRICHTET WORDEN WAR.")))
    (910 . 
         (("\nEtaskMode - Managing project tasks and todo lists within Emacs:")
          ("\nEtaskMode - Projektaufgaben und Todo-Listen mit Emacs verwalten:")))
    (911 . 
         (("\n===============================================================")
          ("\n================================================================")))
    (912 . 
         (("\n\nCopyright (C) 2004 René Weichselbaum")
          ("\n\nCopyright (C) 2004 René Weichselbaum")))
    (913 . 
         (("\n\nURL: http://members.chello.at/rene.weichselbaum/etask.html")
          ("\n\nURL: http://members.chello.at/rene.weichselbaum/etask.html")))
    (914 . 
         (("\n\nOnline Help: `C-h m' from within EtaskMode Window")
          ("\n\nOnline-Hilfe: `C-h m' im EtaskMode-Fenster")))
    (915 . 
         (("\n\nBug Report: `C-cC-b' from within EtaskMode Window")
          ("\n\nFehlerbericht: `C-cC-b' im EtaskMode-Fenster")))
    (990 . 
         (("* Describe the behavior you observe
  that you believe is incorrect.")
          ("* Beschreiben Sie das von Ihnen beobachtete
  Fehlverhalten.")))

    (994 . 
         (("* Describe the features that you think are missing in
  EtaskMode, suggest better ways of doing things, make 
  technical comments, ...")
          ("* Beschreiben Sie die Funktionen, die Sie gerne in
  EtaskMode haetten, machen Sie Verbesserungsvorschlaege,
  senden Sie technische Kommentare, ...")))


    ;; Error Messages

    (1000 . 
         (("No planned effort")
          ("Keine geplanten Aufwaende")))
    (1001 . 
         (("No day between start and end")
          ("Kein Tag zwischen Beginn und Ende")))
    (1002 . 
         (("No business day between start and end")
          ("Kein Arbeitstag zwischen Beginn und Ende")))
    (1003 . 
         (("Planned end is before planned begin")
          ("Geplantes Ende ist vor geplantem Beginn")))
    (1004 . 
         (("Key disabled")
          ("Taste bzw. Tastenkombination deaktiviert")))
    (1005 . 
         (("Switching failed")
          ("Wechsel schlug fehl")))
    (1006 . 
         (("Invalid new item")
          ("Ungueltiger neuer Eintrag")))
    (1007 . 
         ((" not deleted")
          (" nicht geloescht")))
    (1008 . 
         (("Inserting failed")
          ("Einfuegen schlug fehl")))
    (1009 . 
         (("Already showing the last project day")
          ("Zeige bereits den letzten Projekttag")))
    (1010 . 
         (("Already showing the first project day")
          ("Zeige bereits den ersten Projekttag")))
    (1011 . 
         ((" not renamend")
          (" nicht umbenannt")))
    (1012 . 
         (("Not available in this test release")
          ("In dieser Testversion nicht verfuegbar")))
    (1013 . 
         (("File not readable")
          ("Datei nicht lesbar")))
    (1014 . 
         (("Migration failed")
          ("Migration fehlgeschlagen")))
    (1015 . 
         (("Name not unique")
          ("Name schon vergeben")))
    (1016 . 
         (("")
          ("")))))

(defvar etask-current-language-index nil "Current language index cache")

(defun etask-set-calendar-language(lang)
  ""
  (when etask-set-calendar-language-p
    (let ((ix (etask-lang-ix lang 'init)))
      (cond ((or (= ix 0) (> ix 1))     ;English
             (setq european-calendar-style nil)
             (setq calendar-day-name-array
                   ["Sunday" "Monday" "Tuesday" "Wednesday"
                    "Thursday" "Friday" "Saturday"])
             (setq calendar-month-name-array 
                   ["January" "February" "March" "April" "May" 
                    "June" "July"    "August"   "September"
                    "October" "November" "December"])
             (setq solar-n-hemi-seasons
                   '("Beginning of Spring" "Beginning of Summer" 
                     "Beginning of Autumn" "Beginning of Winter")))
            ((= ix 1)                   ;German
             (setq european-calendar-style t)
             (setq calendar-day-name-array
                   ["Sonntag" "Montag" "Dienstag" "Mittwoch"
                    "Donnerstag" "Freitag" "Samstag"])
             (setq calendar-month-name-array
                   ["Jaenner" "Februar" "Maerz" "April" "Mai"
                    "Juni" "Juli" "August" "September"
                    "Oktober" "November" "Dezember"])
             (setq solar-n-hemi-seasons
                   '("Fruehlingsanfang" "Sommeranfang" 
                     "Herbstanfang" "Winteranfang")))))))

(defun etask-lang-ix(lang &optional init)
  "Return index of LANG in `etasklang' or nil if LANG not found.
Normally a cached value is returned.  If optional INIT is non-nil then
the index is calculated again."
  (if init
      (let ((ix)
            (count 0)
            (langlist etasklang))
        (if (stringp lang)
            (while (and (not ix) langlist)
              (if (or 
                   (string= (car langlist) (downcase lang))
                   (string= (car langlist) (upcase lang))
                   (string= (car langlist) (capitalize (downcase lang))))
                  (setq ix count)
                (setq count (1+ count))
                (setq langlist (cdr langlist)))))
        (setq etask-current-language-index ix)
        ix)
    etask-current-language-index))

(defun etask-lang-msg(key lang &optional init)
  "Return a string specified by KEY in language LANG or empty string
\"\" if no string found.  Optional parameter INIT is passed to
`etask-lang-ix'."
  (let ((ix (etask-lang-ix lang init)))
    (if ix
        (car (nth ix (cdr (assoc key msglist))))
      "")))


;;; Initialization

(setq etask-lang-loaded-p t)
(provide 'etask-lang)


;;; etask-lang.el  end of file