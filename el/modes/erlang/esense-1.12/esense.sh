#!/bin/sh

scriptdir=`dirname $0`

erl -boot start_clean -emu-args -noshell -pz $scriptdir/compat -pz $scriptdir -run esense start -run erlang halt -extra "$@"

