import "ftext.vim"

if s:ftext.CanCmd("Compile")
    command! -buffer -bar Compile w | eval s:ftext.TermRun([ "lua", "-i", "-e",
                \ printf('self = require("%s")', expand("%:r:gs?/?.?")) ],
                \     #{ persist: v:false, unique: v:true })
    let b:compile_overridable = 0
endif

if s:ftext.CanCmd("Run")
    command! -buffer -bar Run w | botright term lua %
    let b:run_overridable = 0
endif

