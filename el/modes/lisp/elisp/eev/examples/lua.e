

# «.debian-packages»	(to "debian-packages")
# «.quick-presentation»	(to "quick-presentation")
# «.qp-libraryfuns»	(to "qp-libraryfuns")
# «.qp-metatables»	(to "qp-metatables")
# «.install-5.0.2»	(to "install-5.0.2")
# «.compile-5.0.2»	(to "compile-5.0.2")
# «.manual2-5.0.2»	(to "manual2-5.0.2")
# «.eeluaw»		(to "eeluaw")
# «.lua-gdb-1»		(to "lua-gdb-1")
# «.luaL_openlib»	(to "luaL_openlib")
# «.closures-C»		(to "closures-C")
# «.luagtk»		(to "luagtk")

(find-es "lua5")
(find-es "04lua")

(find-eevex "")




#####
#
# The debian packages for Lua
# 2004oct19
#
#####

# «debian-packages»  (to ".debian-packages")
#
apt-get install \
  liblua50 liblua50-dev liblualib50 liblualib50-dev lua50 lua50-doc
#

liblua50
liblua50-dev
liblualib50
liblualib50-dev
lua50
lua50-doc

# (find-sh "dpkg -l")

# (find-vldifile "liblua50.list")
# (find-vldifile "liblua50-dev.list")
# (find-vldifile "liblualib50.list")
# (find-vldifile "liblualib50-dev.list")

# (find-status   "lua50")
# (find-vldifile "lua50.list")
# (find-udfile   "lua50/")
# (find-status   "lua50-doc")
# (find-vldifile "lua50-doc.list")
# (find-udfile   "lua50-doc/")





#####
#
# Quick presentation
# 2005oct05
#
#####

# «quick-presentation»  (to ".quick-presentation")
#
# Basic datatypes
# (find-luamanualw3m "TypesSec")
# (find-luamanualw3m+ "TypesSec")
# (find-luamanualw3m+ "pdf-type")
lua50 -e '
  print(1,     type(1))          --> 1       number		    
  print(1.0,   type(1.0))	 --> 1       number		    
  print("abc", type("abc"))	 --> abc     string		    
  print(nil,   type(nil))	 --> nil     nil			    
  print(true,  type(true))	 --> true    boolean		    
  print(false, type(false))	 --> false   boolean		    
  print(print, type(print))	 --> function: 0x804d218     function
  print({2,3,5}, type({}))	 --> table: 0x8053ab0        table   
'
#
# Functions are values
# (find-luamanualw3m+ "func-def" "f = function () ... end")
lua50 -e '
  function square(a)     return a*a  end
  square = function (a)  return a*a  end
  print(square, square(2))
'
#
# Functions and expressions can return several values
# (find-luamanualw3m+ "expressions")
# (find-luamanualw3m+ "assignment")
lua50 -e '
  function foo()  return 1, 2, 3  end
  print(1, 2, 3)                         --> 1   2  3                  
  print(foo())                           --> 1   2  3                  
  print(99, foo())                       --> 99  1  2   3          
  print(99, foo(), 200)                  --> 99  1  200            
  print(99, (foo()))                     --> 99  1                 
  zero, one, two, three, four = 0, foo()
  print(zero, one, two, three, four)     --> 0   1  2   3  nil
'
#
# Coercion
# (find-luamanualw3m+ "coercion")
lua50 -e '
  print(1+"2")				--> 3     
  print("<".. 11 .. 22 ..">")		--> <1122>
'
#
# String literals
# (find-luamanualw3m+ "lexical" "Literal strings")
# (find-luamanualw3m+ "lexical" "double square brackets")
#
# Syntax for comments
# (find-luamanualw3m+ "lexical" "comments")
# (find-luamanualw3m+ "lexical" "long comment")
#
# PP
# (find-luamanualw3m+ "print")
# (find-luamanualw3m+ "tostring")
# (find-luamanualw3m+ "lua-sa")
# (find-angg ".zshrc" "LUA_INIT")
# (find-angg "LUA/lua50init.lua" "PP")
# (find-angg "LUA/lua50init.lua" "mytostring")
echo $LUA_INIT
lua50 -e '
  print(22, "abc", print)	--> 22	abc	function: 0x804d218
  PP   (22, "abc", print)	-->  22 "abc" <function: 0x804d218>    
'
#
# Tables and PP
# (find-luamanualw3m+ "TypesSec" "references")
lua50 -e '
  a = {10, 20, 30}
  b = {10, 20, 30}
  print(a); PP(a)	--> table: 0x8051c38; {1=10, 2=20, 3=30}
  print(b); PP(b)	--> table: 0x8052ef0; {1=10, 2=20, 3=30}
  a[2] = 40		
  print(a); PP(a)	--> table: 0x8051c38; {1=10, 2=40, 3=30}
  print(b); PP(b)	--> table: 0x8052ef0; {1=10, 2=20, 3=30}
'
#
# Tables and PP
# (find-luamanualw3m+ "TypesSec" "heterogeneous")
lua50 -e '
  a = {10, 20, 30}
  print(200, "some string", a)  --> 200	some string	table: 0x8051c38		       
  PP   (200, "some string", a)  -->  200 "some string" {1=10, 2=20, 3=30}			       
  b = {11, a, "foo", print}	    							       
  PP(b)	   --> {1=11, 2={1=10, 2=20, 3=30}, 3="foo", 4=<function: 0x804d218>}
'
#
# Tables (2)
# (find-luamanualw3m+ "tableconstructor")
# (find-luamanualw3m+ "TypesSec" "except nil")
# (find-luamanualw3m+ "next" "value nil")
lua50 -e '
  c = {11, 22, 33}; PP(c)   --> {1=11, 2=22, 3=33}
  c[2] = c[2]+c[3]; PP(c)   --> {1=11, 2=55, 3=33}
  c[5] = 55       ; PP(c)   --> {1=11, 2=55, 3=33, 5=55}
  c["foo"] = "FOO"; PP(c)   --> {1=11, 2=55, 3=33, 5=55, "foo"="FOO"}
  c[3] = nil      ; PP(c)   --> {1=11, 2=55, 5=55, "foo"="FOO"}
  d = {11, 22, 33, [5]=555,
       ["bar"]="BAR", [c]="!"}; PP(d)
 --> {1=11, 2=22, 3=33, 5=555, "bar"="BAR", {1=11, 2=55, 5=55, "foo"="FOO"}="!"}
  d[2] = nil                  ; PP(d)
 --> {1=11, 3=33, 5=555, "bar"="BAR", {1=11, 2=55, 5=55, "foo"="FOO"}="!"}
  d[c] = nil                  ; PP(d)
 --> {1=11, 3=33, 5=555, "bar"="BAR"}
'
#
# Global variables
# (find-luamanualw3m+ "_G")
# (find-luamanualw3m+ "TypesSec" "a.name")
# (find-sh "lua50 -e 'for key,val in _G do print(key, val) end'")
lua50 -e '
  print(print)          --> function: 0x804d218
  print(_G["print"])	--> function: 0x804d218
  print(_G.print)	--> function: 0x804d218
  print(_G)		--> table: 0x804c678   
  print(_G._G)		--> table: 0x804c678   
'
#
# Local variables
# (find-luamanualw3m+ "visibility")
lua50 -e '
  a = 22
  do print(a)		--> 22
     local a = 33
     print(a)		--> 33
  end
  print(a)		--> 22
'
#
# Capture of local variables
# (find-luamanualw3m+ "func-def" "closure")
# (find-es "lua5" "captured-variables")
lua50 -e '
  foo = function ()
    local storage
    return
      function () return storage end,
      function (x) storage = x; return x end
  end
  get1, set1 = foo()
  get2, set2 = foo()
  print(set1(22), get1())          --> 22 22
  print(set2(33), get1(), get2())  --> 33 22 33
'
#
# loadstring
# (find-luamanualw3m+ "loadstring")
lua50 -e '
  print(loadstring([[ print("hi") ]]))
  print(loadstring([[ print("hi") ]])())                        -- execute
  print(loadstring([[ print("hi"); return "foo", "bar" ]])())   -- execute
  print(loadstring([[ print("hi   ]]))
  print(loadstring([[ print("hi   ]], "name of the block"))
'
#
# assert
# (find-luamanualw3m+ "assert")
lua50 -e '
  print(       "foo" )
  print(assert("foo"))
  print(assert("foo", nil))
  print(assert("foo", "ignored"))
  print(assert(  nil, "errmsg"))
'
#





#####
#
# quick presentation: library functions
# 2005nov06
#
#####

# «qp-libraryfuns»  (to ".qp-libraryfuns")
#
# (find-luamanualw3m+ "string.gsub")
# (find-luamanualw3m+ "pm")
lua50 -e '
  print(string.gsub("Hello world", "l",  "<>"))
  print(string.gsub("Hello world", "l+", "<>"))
  print(string.gsub("Hello world", "([A-Z]+)([a-z]+)", "<%1,%2>"))
  print(string.gsub("Hello world", "([A-Z]+)()([a-z]+)", "<%1,%2,%3>"))
'
#



#####
#
# quick presentation: metatables
# 2005oct18
#
#####

# «qp-metatables»  (to ".qp-metatables")
#
# Metatables
# (find-luamanualw3m+ "metatable")
# (find-luamanualw3m+ "setmetatable")
lua50 -e '
  a = {}
  mt = {}
  setmetatable(a, mt)
  mtset = function (mtfield)
      mt[mtfield] = function (...) P(mtfield, unpack(arg)) end
    end
  mtset("__add"); b = a + 1
  mtset("__sub"); b = a - 2
  mtset("__mul"); b = a * 3
  mtset("__div"); b = a / 4
  mtset("__pow"); b = a ^ 5
  mtset("__unm"); b =   - a
  mtset("__concat"); b = a .. 6
  -- mtset("__eq");  b = a == a
  -- mtset("__lt");  b = a <  9
  -- mtset("__le");  b = a <= 10
'
#
# Default metatables: files are not primitive types
# (find-angg "LUA/lua50init.lua" "readfile")
echo foo > ~/o; cd
lua50 -e '
  readfile = function (fname)
      local f = assert(io.open(fname, "r"))
      local bigstr = f:read("*a")
      f:close()
      return bigstr
    end
  print(readfile("o"))
  f = io.open("o", "r")
  mt = getmetatable(f)
  print(f)
  for key,val in mt do print(key, val) end
  print(f:__tostring())
'
#
lua50 -e '
  a = {}
  mt = {__tostring = function () return "foo!" end}
  setmetatable(a, mt)
  print(a)
  PP(a)    -- hmm, broken
'
#
# (find-luamanualw3m+ "pdf-getmetatable")
# (find-luamanualw3m+ "tostring")


  PP(getmetatable({}))
  f = io.open("~/tmp/o", "w")
  P(getmetatable(f))




#####
#
# Recompiling from source with -g
# 2004oct19
#
#####

# «install-5.0.2»  (to ".install-5.0.2")
# (find-es "lua5" "install-5.0.2")
# (code-c-d "lua5" "~/usrc/lua-5.0.2/")
# (find-lua5file "")
#
# (find-fline "$S/http/www.lua.org/ftp/lua-5.0.2.tar.gz")
psne http://www.lua.org/ftp/lua-5.0.2.tar.gz

#
# «compile-5.0.2»  (to ".compile-5.0.2")
mkdir  ~/usrc/
rm -Rv ~/usrc/lua-5.0.2/
mkdir  ~/usrc/lua-5.0.2/
tar -xvzf $S/http/www.lua.org/ftp/lua-5.0.2.tar.gz -C ~/usrc/
cd     ~/usrc/lua-5.0.2/

find * -name '*.[ch]' | sort > .files.ch
etags $(<.files.ch)

# (find-lua5file "INSTALL")
# (find-lua5file "config")
# (find-lua5file "etc/README")
# (find-lua5file "src/lib/loadlib.c")
cat >> config <<'%%%'

# --- Edrx's changes
# (find-lua5file "config" "dynamic loading on Unix systems")
LOADLIB= -DUSE_DLOPEN=1
DLLIB= -ldl
MYLDFLAGS= -Wl,-E
MYCFLAGS=-g
#
# (find-lua5file "config" "\n#USERCONF=")
USERCONF=-DLUA_USERCONFIG='"$(LUA)/etc/saconfig.c"' -DUSE_READLINE
EXTRA_LIBS= -lm -lreadline -ldl

INSTALL_EXEC= cp -v
INSTALL_DATA= cp -v
STRIP=echo NOT stripping:
INSTALL_BIN=$(HOME)/bin
INSTALL_INC=$(HOME)/include/lua5
INSTALL_LIB=$(HOME)/ulocal/lib
INSTALL_MAN=$(HOME)/ulocal/man/man1
%%%

make test       2>&1 | tee omt
ldd  bin/lua    2>&1 | tee ol
make so         2>&1 | tee oms

mkdir -p ~/bin/ ~/lib/
mkdir -p ~/include/lua5/
mkdir -p ~/ulocal/lib/ ~/ulocal/man/man1/
make install    2>&1 | tee omi
make soinstall  2>&1 | tee omsi
# (find-lua5file "om")
# (find-lua5file "omi")
# (find-lua5file "omsi")

#
# «manual2-5.0.2»  (to ".manual2-5.0.2")
# (find-angg ".emacs" "luamanual")
# (find-luash "grep '<p><h3><code>' doc/manual.html")
# (find-luash "grep 'name='         doc/manual.html")
# (find-luash "grep 'name='         doc/manual2.html")
# (find-luash "cat doc/manual2.html | lua50 -e 'string.gsub(io.read(\"*a\"), \"<p><a name=\\\"([A-Za-z0-9_.:]+)\\\">\", print)'")
# (eev "cd ~/usrc/lua-5.0.2/doc/; tkdiff manual.html manual2.html")

cd ~/usrc/lua-5.0.2/doc/
cat manual.html \
  | lua50 -e 'print((string.gsub(io.read("*a"),
                "<p><h3><code>(([A-Za-z0-9_.:]+).-)</code></h3>",
		"<p><a name=\"%2\"><h3><code>%1</code></h3></a>")))' \
  > manual2.html

#
# A fossil?
mkdir -p ~/bin ~/lib

cd ~/usrc/lua-5.0.2/
cp -v bin/lua  ~/bin/lua50
cp -v bin/luac ~/bin/luac50
cp -v lib/liblua.so.5.0    ~/lib/liblua.so.5.0
cp -v lib/liblualib.so.5.0 ~/lib/liblualib.so.5.0

#
# (find-luafile "src/lib/lmathlib.c")
# (find-efunction 'eegdb)
# (find-efunction 'ee-gdb-start)
# (find-efunction 'eeb-gdb-start)
# (eeb-gdb-start t ee-luadir "bin/lua")
br luaopen_math
run

#





#####
#
# eeluaw
# 2004oct19
#
#####

# «eeluaw»  (to ".eeluaw")
# (find-eev "eev-langs.el" "eeb-luaw")
# (eeluaw "for i=1,20 do print(i) end")
--
-- (ee-once (eeb-luaw))
for i=1,20 do print(i) end
--




#####
#
# lua and gdb (1)
# 2005oct16
#
#####

#
# «lua-gdb-1»  (to ".lua-gdb-1")
cd /tmp/
export LUA50SRC=$HOME/usrc/lua-5.0.2

cat > so.c <<'%%%'
#include <lauxlib.h>
#include <stdio.h>
static int my_foo(lua_State* L) {
  int n = lua_gettop(L);
  int isnum = lua_isnumber(L, 1);
  int m = lua_tonumber(L, 1);
  printf("Hi hi!\n");
  printf("%d %d %d!\n", n, isnum, m);
  return 0;
}

LUALIB_API int my_init(lua_State *L) {
  lua_register(L, "foo", my_foo);
  return 0;
}
%%%
gcc -g -Wall -shared -I$LUA50SRC/include -o so.so so.c

# (find-lua50ref "Defining C Functions" "lua_register")

cat > loadso.lua <<'%%%'
assert(loadlib("/tmp/so.so", "my_init"))()
foo(22)
%%%

$LUA50SRC/bin/lua /tmp/loadso.lua

#
# (ee-once (eeb-luagdb-start "bin/lua"))
set args /tmp/loadso.lua
# br main
br loadlib
run
n
n
n
br my_foo
cont

#




#####
#
# Lua no GDB, parte 2: PP
# 2005out05
#
#####

# «gdb-PP»  (to ".gdb-PP")
# (find-es "lua5" "luastackPP")
#
export LUA50SRC=$HOME/usrc/lua-5.0.2

rm -Rv   ~/tmp/luastackPP/
mkdir -p ~/tmp/luastackPP/
cd       ~/tmp/luastackPP/
cat > luastackPP.c <<'%%%'
#include <stdio.h>
#include <stdlib.h>
#include <lua.h>
#include <lauxlib.h>
void PP(lua_State *L, int index) {
  lua_pushstring(L, "PP");
  lua_gettable(L, LUA_GLOBALSINDEX);
  lua_pushvalue(L, index<0?index-1:index);
  lua_call(L, 1, 0);
}
LUALIB_API int luastackPP_init(lua_State *L) {
  /* nothing to do on the Lua side; we're only adding a C function */
  return 0;
}
%%%
gcc -g -Wall -shared -I$LUA50SRC/include -o luastackPP.so luastackPP.c
mkdir -p ~/lib/lua5
cp -v ~/tmp/luastackPP/luastackPP.so \
            ~/lib/lua5/luastackPP.so

#
cat > /tmp/testPP.lua <<'%%%'
  LIBDIR = os.getenv("HOME").."/lib/lua5"
  assert(loadlib(LIBDIR.."/luastackPP.so", "luastackPP_init"))()
  math.sin(0)
  -- print(1, 2+"3", string.sub)
  print("aa", "bb", "cc", "dd", "ee")
%%%
cat /tmp/testPP.lua        >  /tmp/testPP.lst
luac -p -l /tmp/testPP.lua >> /tmp/testPP.lst
lua50 /tmp/testPP.lua

#
# (ee-once (eeb-luagdb-start nil "bin/lua"))
# (find-fline "/tmp/testPP.lst")
# (find-node "(gdb)Calling")
# (find-node "(gdb)Define")

define depth
  p lua_gettop(L)
end
define PP
  call PP(L, $arg0)
end

set args /tmp/testPP.lua
# br main
br math_sin
run

#





#####
#
# luaL_openlib - for registering an array of functions
# 2004jan06
#
#####

# «luaL_openlib»  (to ".luaL_openlib")
# (find-lua50file "src/lib/loadlib.c")
# (find-lua50file "include/")
# (find-fline "/tmp/luabit/lbitlib.c")
#
rm -Rv ~/usrc/lual-openlib/
mkdir  ~/usrc/lual-openlib/
cd     ~/usrc/lual-openlib/

cat > foo.c <<'%%%'
#include "lauxlib.h"
#include <stdio.h>
/*
 * (find-luamanualw3m+ "API")
 * (find-luamanualw3m+ "pushing")
*/
static int my_foo(lua_State* L) {
  lua_pushnumber(L, 33);
  lua_pushnumber(L, 333);
  return 2;
}

/*
 * (find-luamanualw3m+ "LuacallC")
 * (find-luamanualw3m+ "LuacallC" "#define lua_register(L,n,f)")
 * (find-lua50file "src/lib/lbaselib.c" "luaL_reg base_funcs[] =")
 * (find-lua50file "src/lib/lbaselib.c" "luaL_openlib(L, NULL, base_funcs, 0)")
 * (find-lua50file "include/lauxlib.h" "luaL_openlib")
 * (find-lua50tag "luaL_openlib")
*/
static const struct luaL_reg bitlib[] = {
  {"foo", my_foo},
  {NULL,  NULL}
};
LUALIB_API int my_init(lua_State *L) {
  lua_pushvalue(L, LUA_GLOBALSINDEX);
  luaL_openlib(L, NULL, bitlib, 0);
  return 0;
}
%%%

# (find-luamanualw3m+ "loadlib")

export LUA50SRC=$HOME/usrc/lua-5.0.2
gcc -g -Wall -shared -I$LUA50SRC/include -o foo.so foo.c

cat > testlib.lua <<'%%%'
assert(os.getenv("HOME").."/usrc/lual-openlib/foo.so", "my_init"))()
print(foo(44))
print("ok")
%%%

lua50 ~/usrc/lual-openlib/testlib.lua

#





#####
#
# understanding closures (from the C side)
# 2005nov14
#
#####

# «closures-C»  (to ".closures-C")
;;
;; (ee-once (eeeval-bounded))
;; (find-eev "eev-langs.el" "eelua")
(defun eelual (s &optional e)
  (interactive "r")
  (ee-write-with-nl s e "" "" ee-file-lua)
  (find-sh (format "awk '{print NR\": \"$0}' %s; luac50 -l %s"
		   ee-file-lua ee-file-lua)))

(eeb-define 'eelual-bounded  'eelual  "\n--\n" nil t t)

;;
--
-- (ee-once (eelua-bounded))
-- (ee-once (eelual-bounded))
function foo (a, b, c)
    return function () return a, b, c end
  end
load_PP(); math.sin(0)
f = foo(10, 20, 30)

--
#
# (ee-once (eeb-lua50gdb-start "bin/lua"))
source ~/.lua50/PP.gdb
set args $EEVTMPDIR/ee.lua
br math_sin
run

#
# (find-angg ".lua50/PP.c")
# (find-lua50file "src/lvm.c" "case OP_RETURN:")
# (find-lua50file "src/lopcodes.c" "/* OP_RETURN */")
# (find-luamanualw3m+ "API")
# (find-luamanualw3m+ "c-closure")
# (find-luamanualw3m+ "debugI")





#####
#
# luagtk
# 2005sep10
#
#####

# «luagtk»  (to ".luagtk")
# http://luaforge.net/projects/lua-gtk/
# http://luaforge.net/frs/download.php/989/lua-gtk2-0.3.tar.gz
#
rm -Rv ~/usrc/lua-gtk2-0.3/
mkdir  ~/usrc/lua-gtk2-0.3/
tar -C ~/usrc/ -xvzf \
      $S/http/luaforge.net/frs/download.php/989/lua-gtk2-0.3.tar.gz
cd ~/usrc/lua-gtk2-0.3/
./configure   2>&1 | tee oc
make          2>&1 | tee om

mkdir ~/.lua50/
cp -v build-linux/libluagtk2.so ~/.lua50/

#
cd ~/usrc/lua-gtk2-0.3/; lua50 examples/button.lua
cd ~/usrc/lua-gtk2-0.3/; lua50 examples/calculator.lua
cd ~/usrc/lua-gtk2-0.3/; lua50 examples/loadlib.lua
cd ~/usrc/lua-gtk2-0.3/; lua50 examples/memory.lua
cd ~/usrc/lua-gtk2-0.3/; lua50 examples/notebook.lua
cd ~/usrc/lua-gtk2-0.3/; lua50 examples/pixmap.lua

#
# (code-c-d "luagtk" "~/usrc/lua-gtk2-0.3/")
# (find-luagtkfile "")
# (find-luagtkfile "doc/")
# (find-luagtkfile "doc/README")
# (find-luagtkfile "gtk2.lua")
# (find-luagtkfile "examples/")
# (find-luagtkfile "examples/")
# (find-luagtkfile "data/gtkdata.funcs" "gdk_draw_rectangle")
# (find-luagtkfile "doc/README" "Perl script")
# (find-luagtkfile "script/gen-list.pl")







#  Local Variables:
#  coding:               raw-text-unix
#  modes:                (fundamental-mode sh-mode emacs-lisp-mode)
#  ee-delimiter-hash:    "\n#\n"
#  ee-anchor-format:     "«%s»"
#  End:
