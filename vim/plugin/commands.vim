vim9script

import autoload "repl.vim"

command -nargs=* Repl repl.MkRepl(<q-args> ?? &ft ?? &shell)
