#!/usr/bin/awk -f
# This is the `rcfiles/change.awk' file of GNU eev.
# Copyright (C) 2004 Free Software Foundation, Inc.
#
# This file is part of GNU eev.
#
# GNU eev is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 2, or (at your option) any
# later version.
#
# GNU eev is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU eev; see the file COPYING. If not, write to the Free
# Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.
#
# Author:     Eduardo Ochs <edrx@mat.puc-rio.br>
# Maintainer: Eduardo Ochs <edrx@mat.puc-rio.br>
# Version:    2005jan07

# Latest version: <http://angg.twu.net/eev-current/rcfiles/change.awk>
#       Htmlized: <http://angg.twu.net/eev-current/rcfiles/change.awk.html>
#       See also: <http://angg.twu.net/eev-current/README.html>
#            and: <http://angg.twu.net/eev-current/INSTALL.html>
#            and: <http://angg.twu.net/eev-current/rcfiles/change.html>.
#
# change.awk: an awk script that is used by "change" to modify
# initialization files.
#
# Usage:
#   awk -f change.awk PATCHFILE < OLDFILE > NEWFILE
#
# This script will read PATCHFILE a first time to remember its first
# and last lines, which will typically be something like
#
#         "# Beginning of the eev block:"
#   and   "# End of the eev block." ,
#
# without the quotes, obviously. Then the script will read OLDFILE and
# try to find a block of lines there that start with PATCHFILE's first
# line and ends with PATCHFILE's last line; if it finds such a block
# it will replace it by the contents of PATCHFILE and write the
# modified version of OLDFILE to stdout (i.e., to NEWFILE). If it
# doesn't find a block like that it will write to stdout (i.e., to
# NEWFILE) the full contents of OLDFILE, then a blank line (as a
# separator), then PATCHFILE. In this process PATCHFILE is read two
# times and so it needs to be a regular file - e.g. not a pipe.

# (find-eevrc "change")
# (find-eev "Makefile" "dotfiles")
# (find-eev "INSTALL" "eevblock-file-format")

# If you have never seen an Awk program before, the following links
# should take you to the pages in the Gawk Manual that describe all
# the features used in this script (and a few that aren't used anymore).
#
# (find-node "(gawk)Executable Scripts" "#! /bin/awk -f")
# (find-node "(gawk)Running gawk" "awk -f PROGRAM-FILE INPUT-FILE1")
# (find-node "(gawk)Assignment Options" "-v VARIABLE=TEXT")
# (find-node "(gawk)Options" "`-v VAR=VAL'")
# (find-node "(gawk)Very Simple")
# (find-node "(gawk)Pattern Overview" "`BEGIN'")
# (find-node "(gawk)Using BEGIN/END")
# (find-node "(gawk)Getline/Variable/File" "`getline VAR < FILE'")
# (find-node "(gawk)Getline/File")
# (find-node "(gawk)Typing and Comparison" "`X == Y'")
# (find-node "(gawk)While Statement")
# (find-node "(gawk)Using Variables" "initialized to")
# (find-node "(gawk)Fields" "`$0'")
#
# (find-node "(gawk)Index")

# (find-node "(gawk)Definition Syntax" "function NAME(PARAMETER-LIST)")
# (find-node "(gawk)Redirection" "`print ITEMS > OUTPUT-FILE'")
# (find-node "(gawk)Special FD" "`/dev/stderr'")
# (find-node "(gawk)Exit Statement")

function myerror (str) {
  print str > "/dev/stderr";
  exit 1;
}
function cat_patch_file () {
  close(patch_fname); while (getline < patch_fname) print
}
BEGIN {
  if (ARGC!=2)
    myerror("Usage: " ARGV[0] " PATCHFILE < OLDFILE > NEWFILE");
  patch_fname = ARGV[--ARGC];
  getline patch_first_line < patch_fname;
  while (getline patch_last_line < patch_fname) {}
  if (patch_last_line=="")
    myerror("PATCHFILE too short or last line empty");
}

state == 0 {
  if ($0 == patch_first_line) {
    state = 1;
  } else
    print;
}
state == 1 {
  if ($0 == patch_last_line) {
    cat_patch_file();
    state = 2;
    next;
  }
}
state == 2 {			
  print				
}				

END {			# on end of input,
  if (state == 0) {	# if we have never met a patch_last_line,
    print "";		# print a blank line, as a separator,
    cat_patch_file();   # and then cat the new block
  }
  if (state == 1) {
    myerror("OLDFILE ends in the middle of the patch block")
  }
  if (state == 2) {
  }
}
