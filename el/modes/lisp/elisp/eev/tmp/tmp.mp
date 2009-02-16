% This is the `tmp/tmp.mp' file of GNU eev.
% This file is in the Public Domain.
% Author and version: Eduardo Ochs, 2005jan07
% If you run `eemp' in a block in Emacs and then go to a shell and
% type `ee' you'll notice that this is the file that will be MetaPost
% will process (unless you've changed the defaults). This is a wrapper
% around ee.mp.

% (find-fline "/usr/share/doc/texmf/metapost/base/")
% (code-ps "mpman"   "/usr/share/doc/texmf/metapost/base/mpman.ps.gz")
% (code-ps "mpintro" "/usr/share/doc/texmf/metapost/base/mpintro.ps.gz")
% (find-mpmanpage   1)
% (find-mpintropage 1)
%
beginfig(0);
input ee.mp;
endfig;
end
