vim9script
import "ftext.vim"

const compile_cmd = "w | AsyncRun cd %:h; xelatex %:t"

ftext.CmdEngine.new("Compile", (...args) => {
    w
    exec compile_cmd .. " " .. args->join(" ")
}).Do()
ftext.CmdEngine.new("Run", (...args) => {
    w | exec compile_cmd .. " " .. args->join(" ") .. " && " .. (has("win") ? "start" : "xdg-open") .. " %:r.pdf"
}).Do()

