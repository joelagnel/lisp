" This is the `rcfiles/.vimrc' file of GNU eev.
" This file is in the Public Domain.
" By Igor Morgado, 2004oct24.
" This is a proof-of-concept implementation of the action of eev's <f3>
" key (`eev-bounded') for vim.
"
map <F3> ?^#<char-15><CR>j<S-v>/^#<char-15><CR>k:w! $EE<CR>
