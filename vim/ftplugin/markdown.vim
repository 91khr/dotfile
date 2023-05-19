vim9script
import "ftext.vim"

var compile_flags: string
def RenewCompiler()
    if getline(1) == "---" && getline(2) =~ "^compile_flags:"
        compile_flags = getline(2)["compile_flags:"->len() : ]
    else
        compile_flags = "-s --mathml -o %:h/%:t:r.html "
    endif
enddef
RenewCompiler()
autocmd BufWrite <buffer> RenewCompiler()

ftext.CmdEngine.new("Compile", (...args) => {
    w
    exec "AsyncRun pandoc % " .. compile_flags .. args->join(" ")
}).Do()
ftext.CmdEngine.new("Run", (...args) => {
    exec "!" .. (has("win32") ? "start" : "xdg-open") .. " %:h/%:t:r.html " .. args->join(" ")
}).Do()

b:coc_suggest_disable = 1

command! -buffer Debug {
    echom "flags: " compile_flags
}
