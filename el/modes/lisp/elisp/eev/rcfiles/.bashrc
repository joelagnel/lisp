# This is the `rcfiles/.bashrc' file of GNU eev.
# This file is in the public domain.
# Author and version: Eduardo Ochs, 2005jan07

# The installation scripts of eev add a few lines to your ~/.bashrc to
# make bash read this file on startup. Note that this file is intended
# to be _read_ by your ~/.bashrc, not to _replace_ your ~/.bashrc!
#
# (find-eev "rcfiles/change" "eev_chunk_for_dotbashrc")
# (find-eev "eev-dev.el" "ee-setenv")

# (find-node "(bashref)Shell Parameter Expansion")
# (find-node "(bashref)Bourne Shell Builtins" "`:")

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

function eegcc () {
  cat $EETMPC - > $EEC
  gcc $* -o $EEAOUT $EEC
}
alias eec=$EEAOUT
