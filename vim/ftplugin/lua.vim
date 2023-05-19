vim9script
import "ftext.vim"

ftext.CmdEngine.new("Compile", (...args) => {
    w
    ftext.TermRun([ "lua", "-i", "-e",
                \       printf('self = require("%s")', expand("%:r:gs?/?.?")) ],
                \ { persist: v:false, unique: v:true })
}).Do()
ftext.CmdEngine.new("Run", (...args) => {
    w
    exec "botright term lua " .. expand("%") .. " " .. args->join(" ")
}).Do()
