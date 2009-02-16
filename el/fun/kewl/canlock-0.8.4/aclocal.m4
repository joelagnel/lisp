m4_define([CANLOCK_PRE_INIT],
 [m4_define([_CANLOCK_VERSION],
  m4_esyscmd([sed -n 's/^(defconst canlock-version "\(.*\)")/\1/p'\
  canlock.el]))
  m4_define([CANLOCK_VERSION],
   m4_substr(_CANLOCK_VERSION, 0, decr(len(_CANLOCK_VERSION))))
  m4_undefine([_CANLOCK_VERSION])])

AC_DEFUN(AC_CHECK_EMACS,
 [dnl Check for Emacsen.

  dnl Apparently, if you run a shell window in Emacs, it sets the EMACS
  dnl environment variable to 't'.  Lets undo the damage.
  test "$EMACS" = t && EMACS=

  dnl Ignore cache.
  unset ac_cv_prog_EMACS;

  AC_ARG_WITH(emacs,
   [  --with-emacs[[=EMACS]]    compile with EMACS  [[EMACS=emacs]]],
   [if test "$withval" = yes -o -z "$withval"; then
      AC_CHECK_PROGS(EMACS, emacs xemacs mule, emacs)
    else
      AC_CHECK_PROG(EMACS, $withval, $withval, emacs)
    fi])
  AC_ARG_WITH(xemacs,
   [  --with-xemacs[[=XEMACS]]  compile with XEMACS [[XEMACS=xemacs]]],
   [if test "$withval" = yes -o -z "$withval"; then
      AC_CHECK_PROG(EMACS, xemacs, xemacs, xemacs)
    else
      AC_CHECK_PROG(EMACS, $withval, $withval, xemacs)
    fi])
  test -z "$EMACS" && AC_CHECK_PROGS(EMACS, emacs xemacs mule, emacs)
  AC_SUBST(EMACS)])

AC_DEFUN(AC_EMACS_LISP,
 [elisp="$2"
  if test -z "$3"; then
    AC_MSG_CHECKING(for $1)
  fi
  AC_CACHE_VAL(EMACS_cv_SYS_$1,
   [OUTPUT=./conftest-$$
    echo ${EMACS}' -batch -eval '\''(let ((x '${elisp}')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
    eval ${EMACS}' -batch -eval '\''(let ((x '${elisp}')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
    retval=`cat ${OUTPUT}`
    echo "=> ${retval}" >& AC_FD_CC 2>&1
    rm -f ${OUTPUT}
    EMACS_cv_SYS_$1=$retval])
  $1=${EMACS_cv_SYS_$1}
  if test -z "$3"; then
    AC_MSG_RESULT($$1)
  fi])

AC_DEFUN(AC_CHECK_EMACS_FLAVOR,
 [AC_MSG_CHECKING([what a flavor does $EMACS have])

  dnl Ignore cache.
  unset EMACS_cv_SYS_flavor;

  AC_EMACS_LISP(flavor,
    (cond ((featurep (quote xemacs)) \"XEmacs\")\
          ((boundp (quote MULE)) \"MULE\")\
          (t \"FSF Emacs\")),
    "noecho")
  case $EMACS_cv_SYS_flavor in
  XEmacs)
    EMACS_FLAVOR=xemacs;;
  MULE)
    EMACS_FLAVOR=mule;;
  *)
    EMACS_FLAVOR=emacs;;
  esac
  AC_MSG_RESULT($EMACS_cv_SYS_flavor)])

AC_DEFUN(AC_PATH_LISPDIR,
 [AC_CHECK_EMACS_FLAVOR
  if test "$prefix" = NONE; then
    AC_MSG_CHECKING([for the installation prefix])
    AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),"noecho")
    prefix=${EMACS_cv_SYS_prefix}
    AC_MSG_RESULT($prefix)
  fi
  AC_ARG_WITH(LISPDIR,
    [  --with-lispdir=DIR      where lisp files should be installed],
    LISPDIR=${withval})
  AC_MSG_CHECKING([where lisp files should go])
  if test -z "$LISPDIR"; then
    dnl Set the default value
    if test "$prefix" = NONE; then
      prefix=$ac_default_prefix
    fi
    LISPDIR="${datadir}/${EMACS_FLAVOR}/site-lisp/"
    for thedir in share lib; do
      potential=
        if test -d ${prefix}/${thedir}/${EMACS_FLAVOR}/site-lisp; then
          LISPDIR="\${prefix}/${thedir}/${EMACS_FLAVOR}/site-lisp/"
          break
        fi
    done
  fi
  AC_MSG_RESULT([$LISPDIR])
  AC_SUBST(LISPDIR)])

AC_DEFUN(AC_ADD_LOAD_PATH,
 [dnl Check for the additional load-path.
  AC_ARG_WITH(addpath,
   [  --with-addpath=PATH     add PATH to the value of load-path
                          use colons to separate directory names],
   [if test "$withval" != yes -a -n "$withval"; then
      AC_MSG_CHECKING([where to find the additional elisp libraries])
      ADDITIONAL_LOAD_PATH=$withval
      AC_MSG_RESULT($ADDITIONAL_LOAD_PATH)
    fi],
    ADDITIONAL_LOAD_PATH=)
  AC_SUBST(ADDITIONAL_LOAD_PATH)])
