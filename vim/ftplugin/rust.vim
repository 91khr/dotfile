vim9script

import "ftext.vim"

if filereadable("Cargo.toml")  # Project
    ftext.CmdEngine.new("Compile", "AsyncRun cargo build").Do()
    ftext.CmdEngine.new("Run", (...args) => {
        ftext.TermRun(["cargo", "run"] + args, { persist: v:true, unique: v:false })
        doautocmd WinEnter !.
    }).WaitAsyncCmd().Do()
    compiler cargo
else  # Single file
    ftext.CmdEngine.new("Compile", "AsyncRun rustc %").Do()
    ftext.CmdEngine.new("Run", (...args) => {
        ftext.TermRun([expand("%:p:r")] + args, { persist: v:true, unique: v:false })
        doautocmd WinEnter !.
    }).WaitAsyncCmd().Do()
    compiler rustc
endif
