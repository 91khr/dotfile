vim9script
import "ftext.vim"

exec ftext.MakeRun("haskell", "Compile",
            \ "'ghc -dynamic %' .. {args}")

def Run(args: any)
    ftext.TermRun(["ghci", expand("%"), args], { persist: true, unique: false })
    doautocmd WinEnter !.
enddef
exec ftext.MakeRun("haskell", "Run", "call Run({args})")

