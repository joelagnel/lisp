

# Features common to sh, bash and zsh.
# Edrx, 2004


# «.basic-substitutions»	(to "basic-substitutions")
# «.redirections»		(to "redirections")
# «.exit-status»		(to "exit-status")
# «.setting-variables»		(to "setting-variables")

# (find-es "bash")
# (find-es "zsh")





# Write:
# Here-documents, here-strings
# <(), >(); =() is a zshism
# echo12







#####
#
# basic substitutions
# 2004oct16
#
#####

# «basic-substitutions»  (to ".basic-substitutions")
#
echo hello
echo hello  goodbye
echo hello $(echo hmm) goodbye
echo
echo $[1+2*3]

#
cd /tmp
echo $PWD
cd
echo $PWD

#
echo "stuff
  taking
    several
      lines, with
  substitutions: \$PWD becomes $PWD"
echo 'no substitutions: $PWD'

#





#####
#
# redirections
# 2004oct15
#
#####

# «redirections»  (to ".redirections")
# (find-bashnode "Redirections")
#
function echo12 () {
  echo $1
  echo $2 1>&2
}
echo12 stdout sterr
echo12 stdout sterr > /dev/null
echo12 stdout sterr > /dev/null 2>&1

#




#####
#
# program exit status and conditionals
# 2004oct15
#
#####

# «exit-status»  (to ".exit-status")
# (find-man "1 true")
# (find-man "1 false")
# (find-node "(coreutils)true invocation")
# (find-node "(coreutils)false invocation")
# (find-bashnode "Special Parameters" "`?'")
# (find-zshnode "Parameters Set By The Shell" "? <S>")

#
true;  echo $?
false; echo $?
function foo () { return $1 }
foo 99; echo $?

if true;   then echo y; else echo n; fi
if foo 0;  then echo y; else echo n; fi

if false;  then echo y; else echo n; fi
if foo 1;  then echo y; else echo n; fi
if foo 99; then echo y; else echo n; fi

#




#####
#
# setting variables
# 2004oct15
#
#####

# «setting-variables»  (to ".setting-variables")
# (find-bashnode "Shell Parameter Expansion" "`${PARAMETER:-WORD}'")
# (find-zshnode  "Parameter Expansion" "${NAME:-WORD}")
#
VSET=original
VVOID=
unset VUNSET

echo ${VSET:-new}    ;# -> original
echo ${VVOID:-new}   ;# -> new
echo ${VUNSET:-new}  ;# -> new

#
# (find-bashnode "Shell Parameter Expansion" "`${PARAMETER:=WORD}'")
# (find-zshnode  "Parameter Expansion" "${NAME:=WORD}")





#####
#
# arg0
# 2004oct15
#
#####

#
rm -Rv   ~/tmp/arg0/
mkdir -p ~/tmp/arg0/
cd       ~/tmp/arg0/
mkdir -p dir1/dir2/

cat > dir1/showarg0 <<'%%%'
#!/bin/sh
echo "$0 -> $(readlink -f $0)"
%%%
chmod 755 dir1/showarg0

ln -s showarg0 dir1/linktosamedir
ln -s dir1/showarg0 linktosubdir
ln -s ../showarg0 dir1/dir2/linktoparentdir

./linktosubdir
dir1/showarg0
dir1/linktosamedir
dir1/dir2/linktoparentdir

(cd dir1
 ../linktosubdir
 ./showarg0
 ./linktosamedir
 dir2/linktoparentdir
)

#




#  Local Variables:
#  coding:               raw-text-unix
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
