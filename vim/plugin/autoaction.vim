vim9script

# {{{ Run local rc when it's trusted
const db_path = expand("~/.local/share")
const trustdb_file = db_path .. "/trusted_dirs.txt"
const forbiddendb_file = db_path .. "/forbidden_dirs.txt"
var loaded_trustdb = false
final [ trusted_dirs, forbidden_dirs ] = [ {}, {} ]
def ExecuteExrc(needconfirm = true): bool
    const cwd = getcwd()
    # Undo the exrc if needed
    if exists("g:Undo_exrc")
        if type(g:Undo_exrc) == v:t_string
            exec g:Undo_exrc
        elseif type(g:Undo_exrc) == v:t_func
            call g:Undo_exrc()
        endif
        g:Undo_exrc = null
    endif
    # Check if needed to load exrc
    if index([ expand('$HOME'), expand('$HOME/.vim'), expand('$VIM') ], cwd) != -1
                \ || !filereadable(".vimrc")
        return false
    endif
    # Load trustdb
    if !loaded_trustdb
        var ctnt: list<string>
        ctnt = filereadable(trustdb_file) ? readfile(trustdb_file) : []
        for key in ctnt | trusted_dirs[key] = null | endfor
        ctnt = filereadable(forbiddendb_file) ? readfile(forbiddendb_file) : []
        for key in ctnt | forbidden_dirs[key] = null | endfor
        loaded_trustdb = true
    endif
    # Confirm whether to load the exrc and load it
    if forbidden_dirs->has_key(cwd) | return false | endif
    const answer = trusted_dirs->has_key(cwd) ? "yes" :
                \ needconfirm ?
                #\ Not using confirm() because it would confirm with only one type
                \ input("Found .vimrc in current directory, trust and execute it? (yes/N[O]/ban) ")->tolower() :
                \ "no"
    if answer == "yes"
        unsilent so .vimrc
        if !trusted_dirs->has_key(cwd)
            trusted_dirs[cwd] = null
            writefile([cwd], trustdb_file, "a")
        endif
        return true
    elseif answer =~# '\v^b(an?)?$' && !forbidden_dirs->has_key(cwd)
        forbidden_dirs[cwd] = null
        writefile([cwd], forbiddendb_file, "a")
    endif
    return false
enddef

autocmd DirChanged * call ExecuteExrc()
autocmd BufWritePost .vimrc call ExecuteExrc(false)
autocmd VimEnter * if ExecuteExrc() && exists("#vimrc#BufRead") | silent doautocmd vimrc BufRead | endif
# }}} End local exrc loading

# {{{ Default filetype settings
def OutputUnableToRun(name: string)
    echohl Error
    echom name .. " is not defined for this file > <"
    echohl Normal
enddef
def DefaultLanguageSettings()
    if !exists(":Compile") || b:compile_overridable
        command! -buffer Compile OutputUnableToRun("Compile")
        b:compile_overridable = 1
    endif
    if !exists(":Run") || b:run_overridable
        command! -buffer Run OutputUnableToRun("Run")
        b:run_overridable = 1
    endif
enddef
augroup filetypeplugin
    # All previously defined commands can be overrided
    autocmd FileType * {
        b:compile_overridable = 1
        b:run_overridable = 1
    }
augroup END
filetype plugin indent on
augroup filetypeplugin
    autocmd FileType * DefaultLanguageSettings()
augroup END
# }}} End default filetype settings

# Report asyncrun status after finish
autocmd User AsyncRunStop {
    if g:asyncrun_code != 0 | echohl WarningMsg | endif
    unsilent echo "(AsyncRun finished with code " .. g:asyncrun_code .. ")"
    echohl Normal
}

