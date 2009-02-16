#!/usr/bin/env lua50
-- -*- coding: raw-text-unix -*-
-- Edrx, 2004sep08
-- Current version: 2004dec22
-- (find-angg "LUA/debs.lua")
-- (find-es "debrepository" "var-www-vcarchives")
--
-- debs.lua: find the .debs corresponding to installed packages.
--
-- To create a new Debian system from an existing one (say, in another
-- partition) we usually need a repository of .debs; and we usually
-- want the new system to have a subset of the packages that are
-- installed in the current one, with exactly the same versions, and,
-- for dozens of reasons, we want to access the net as little as
-- possible in the process. The best solution is then to create a
-- local repository with the right selection of .debs, but how do we
-- do that?
--
-- This library provides some functions to help in that task. It is
-- easy to obtain the names of the packages currently installed, each
-- with its version string:
--
--   # (find-man "1 grep-status")
--   # (find-sh "grep-status -s Package,Version -F Status ' installed'")
--   grep-status --show-field=Package,Version --field=Status ' installed'
--
-- Usually _all_ the original .debs corresponding to those packages
-- are available somewhere in the system:
--
--   * the ones that were downloaded by apt are at
--     (find-fline "/var/cache/apt/archives/")
--
--   * the ones coming from CD installs are inside the CD images;
--     we can either mount the real CD or its .iso image
--
--   * the ones coming from local repositories, are, huh, well, just
--     files inside these local repositories
--
--   * the ones that have been installed with "dpkg -i" from
--     nonstandard .debs, like things that we have compiled locally or
--     that we have downloaded from very nonstandard places, are in
--     directories that we know; we are always very organized when we
--     put our root hats on :)
--
-- So we just need to list these .debs, typically with
--
--   find dir1 dir2 ... dirn -name '*.deb'
--
-- and then cross the output with the output of "grep-status" to
-- obtain the first listed .deb corresponding to each installed
-- package, plus a listing of the installed packages for which no
-- corresponding .deb was found.
--
-- After that we need a few other tricks to convert the output to a
-- series of "cp"s or "ln"s. Check the examples at the end of the
-- file. (But they're not well-documented, sorry).


strfind = string.find
format  = string.format

splitdebversion = function (str)
    local _, __, epoch, colon, version
    _, __, epoch, colon, version = strfind(str, "^(.*)(%%3a)(.*)$")
    if _ then return epoch, colon, version end
    _, __, epoch, colon, version = strfind(str, "^(.*)(:)(.*)$")
    if _ then return epoch, colon, version end
    return nil, nil, str
  end
splitdeb = function (str)
    local _, __, dir, name, epoch, colon, version, arch
    _, __, dir, rest = strfind(str, "^(.*/)([^/]*)$")
    if _ then str = rest end
    _, __, name, v, arch = strfind(str, "^([^_]*)_([^_]*)_([^_.]*).deb$")
    if not _ then return end
    epoch, colon, version = splitdebversion(v)
    return {dir=dir, name=name, epoch=epoch, colon=colon,
            version=version, arch=arch}
  end
readinstalleddebs = function (fname)
    for str in io.lines(fname) do
      local _, __, field, contents = strfind(str, "^(.*): (.*)$")
      if field == "Package" then name = contents end
      if field == "Version" then
        epoch, colon, version = splitdebversion(contents)
        installed[name] = {name=name, epoch=epoch, colon=colon,
                           version=version}
      end
    end
  end

readdeblist = function (fname)
    for pathname in io.lines(fname) do
      local struct = splitdeb(pathname)
      if (struct and
          installed[struct.name] and
          not installed[struct.name].pathname and
          installed[struct.name].version == struct.version) then
        installed[struct.name].pathname = pathname
        installed[struct.name].arch = struct.arch
        installed[struct.name].epoch = struct.epoch
        installed[struct.name].colon = struct.colon
      end
    end
  end



-- (find-fline "/tmp/obigdebiandebs")
-- (find-luamanw3m "manual.html" "string.format")
printcps = function (prefix)
    for name,s in installed do
      if s.pathname then
        print(format("%s %s %s_%s_%s.deb",
                     prefix,
                     installed[name].pathname,
                     name, s.version, s.arch))
      end
    end
  end
printnotfound = function (prefix)
    for name,s in installed do
      if not s.pathname then
        print(format("%s %s %s", prefix, name, installed[name].version))
      end
    end
  end


-- (find-luamanw3m "manual.html")
-- (find-luamanw3m "manual.html" "lua -la.lua b.lua t1 t2")
-- (find-luamanw3m "contents.html")

i = 1
installed = {}
while i <= arg.n do
  local a, b = arg[i], arg[i+1]
  if     a == "-i"  then readinstalleddebs(b); i = i+2
  elseif a == "-d"  then readdeblist(b); i = i+2
  elseif a == "-cp" then printcps(b); i = i+2
  elseif a == "-nf" then printnotfound(b); i = i+2
  elseif a == "-e"  then assert(loadstring(b))(); i = i+2
  else print("Unrecognized option: " .. a); os.exit(1)
  end
end

--[[
#
# «preparation»

find /big/debian/pool/        -type f -and -name '*.deb' > /tmp/obigdebiandebs
find /var/cache/apt/archives/ -type f -and -name '*.deb' > /tmp/ovcaadebs
find $S/                      -type f -and -name '*.deb' > /tmp/osnarfdebs
grep-status -s Package,Version -F Status ' installed'    > /tmp/oinstalleddebs

# (find-fline "/tmp/obigdebiandebs")
# (find-fline "/tmp/")

#
# A small test:
# (find-fline "/tmp/o")
cd /tmp/
~/LUA/debs2.lua \
  -i /tmp/oinstalleddebs \
  -d /tmp/ovcaadebs \
  -cp 'cp -s' \
  -nf '# Not found:' \
  -e 'for name,s in installed do PP(name, s) end' \
  |& tee /tmp/o

#
# «ocp»
# The real thing:
# (find-fline "/tmp/ocp")
cd /tmp/
~/LUA/debs2.lua \
  -i /tmp/oinstalleddebs \
  -d /tmp/obigdebiandebs \
  -d /tmp/ovcaadebs \
  -d /tmp/osnarfdebs \
  -cp 'cp -s' \
  -nf '# Not found:' \
  |& tee /tmp/ocp

#
# (find-es "debrepository" "var-www-vcarchives")

--]]
