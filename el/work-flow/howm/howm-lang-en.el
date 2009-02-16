;;; howm-lang-en.el --- Wiki-like note-taking tool
;;; Copyright (c) 2005, 2006
;;;   by HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; $Id: howm-lang-en.el,v 1.2 2006/01/06 15:35:46 hira Exp $
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

(provide 'howm-lang-en)

(defvar howm-day-of-week-en "SMTWtFs")

(defvar howm-menu-command-table-en
  `(
    ("[New]" (lambda () (howm-create ,howm-menu-action-arg)))
    ("[Add]" (lambda () (howm-create-here ,howm-menu-action-arg)))
    ("[Dup]" howm-dup)
    ("[Update]" howm-initialize-buffer previous)
    ("[Regexp]" howm-list-grep)
    ("[String]" howm-list-grep-fixed)
    ;;         ("[roma]" howm-list-migemo)
    ("[Today]" howm-find-today)
    ("[Yesterday]" howm-find-yesterday)
    ("[All]" howm-list-all)
    ("[Recent]" howm-list-recent)
    ("[Around]" howm-list-around)
    ("[Schedule]" howm-list-schedule)
    ("[History]" howm-history)
    ("[<Title]" howm-keyword-to-kill-ring)
    ("[<Name]" (lambda () (howm-keyword-to-kill-ring t)))
    ("[Key>]" howm-insert-keyword previous)
    ("[Date>]" howm-insert-date previous)
    ("[DTime>]" howm-insert-dtime previous)
    ("[Todo]" howm-list-todo)
    ("[Killall]" howm-kill-all)
    ("[Force Killall]" (lambda () (interactive) (howm-kill-all t)))
    ("[Edit Menu]" howm-menu-edit current)
    ("[Update Menu]" howm-menu-refresh current)
    ("[Preference]" (lambda () (customize-group 'howm)))
    ("[Random Walk]" howm-random-walk previous)
    ))

;;; howm-lang-en.el ends here
