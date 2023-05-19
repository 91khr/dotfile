vim9script
import "ftext.vim"
setlocal foldmethod=marker

ftext.CmdEngine.new("Compile", (...args) => {
    w
    exec "AsyncRun gvim -c 'so %' -c " .. args->join(" ")->shellescape()
}).Do()
ftext.CmdEngine.new("Run", (...args) => {
    w | so %
}).Do()

