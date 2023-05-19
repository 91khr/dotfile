vim9script
import "ftext.vim"

ftext.CmdEngine.new("Run", (...args) => {
    w
    botright ftext.TermRun(
                \     [ "python", expand("%") ] + args,
                \ { persist: true, unique: false })
}).Do()

