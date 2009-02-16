# This is the `rcfiles/.zshrc' file of GNU eev.
# This file is in the public domain.
# Author and version: Eduardo Ochs, 2005jan07

# The installation scripts of eev add a few lines to your ~/.zshrc to
# make zsh read this file on startup. Note that this file is intended
# to be _read_ by your ~/.zshrc, not to _replace_ your ~/.zshrc!
#
# (find-eev "rcfiles/change" "eev_chunk_for_dotzshrc")
# (find-eev "eev-dev.el" "ee-setenv")

# (find-node "(zsh)Parameter Expansion" "${NAME:=WORD}")
# (find-node "(zsh)Parameter Expansion" "${NAME:-WORD}")
# (find-node "(zsh)Shell Builtin Commands" ": [ ARG ... ]")
# (find-node "(zsh)Shell Builtin Commands" "export [ NAME[=VALUE] ... ]")

export EEVDIR    ;: ${EEVDIR:=~/eev-current}
export EEVRCDIR  ;: ${EEVRCDIR:=$EEVDIR/rcfiles}
export EEVTMPDIR ;: ${EEVTMPDIR:=$EEVDIR/tmp}
export EE        ;: ${EE:=$EEVTMPDIR/ee.sh}
export EEG       ;: ${EEG:=$EEVTMPDIR/ee.eeg}
export EEGDB     ;: ${EEGDB:=$EEVTMPDIR/ee.gdb}
export EETEX     ;: ${EETEX:=$EEVTMPDIR/ee.tex}

function ee () { set -v; . $EE$*; set +v; }

export EEC       ;: ${EEC:=$EEVTMPDIR/ee.c}
export EETMPC    ;: ${EETMPC:=$EEVTMPDIR/tmp.c}
export EEAOUT    ;: ${EEAOUT:=$EEVTMPDIR/ee.aout}


# Maybe I should move these - eegcc and eeflex - to a file
# ".zshrc-langs"...
#
function eegcc () {
  cat $EETMPC - > $EEC
  gcc $* -o $EEAOUT $EEC
}
alias eec=$EEAOUT

# (find-node "(flex)Options for Specifing Filenames" "lex.yy.c")
# (find-node "(make)Catalogue of Rules" "Lex for C programs" "N.l")
#
function eeflex () {
  cat > $EEVTMPDIR/ee.l
  flex -o $EEVTMPDIR/ee.yy.c $EEVTMPDIR/ee.l
  gcc $* -o $EEAOUT $EEVTMPDIR/ee.yy.c
}
