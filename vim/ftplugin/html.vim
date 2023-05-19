vim9script

if &ft != "html" | finish | endif
setlocal foldmethod=indent

import "ftext.vim"
ftext.CmdEngine.new("Run", (...args) => {
    w
    exec "!" .. (has("win32") ? "start" : "xdg-open") .. " " .. expand("%") .. " " .. args->join(" ")
}).Do()
