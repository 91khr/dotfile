vim9script
# Run local rc when it's trusted
const db_path = expand("~/.local/share")
const trustdb_file = db_path .. "/trusted_dirs.txt"
const forbiddendb_file = db_path .. "/forbidden_dirs.txt"
var loaded_trustdb = false
final [ trusted_dirs, forbidden_dirs ] = [ {}, {} ]
def ExecuteExrc(needconfirm = true): bool
    const cwd = getcwd()
    if index([ expand('$HOME'), expand('$HOME/.vim'), expand('$VIM') ], cwd) != -1
                \ || !filereadable(".vimrc")
        return false
    endif
    if !loaded_trustdb
        var ctnt: list<string>
        ctnt = filereadable(trustdb_file) ? readfile(trustdb_file) : []
        for key in ctnt | trusted_dirs[key] = null | endfor
        ctnt = filereadable(forbiddendb_file) ? readfile(forbiddendb_file) : []
        for key in ctnt | forbidden_dirs[key] = null | endfor
        loaded_trustdb = true
    endif
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
autocmd BufWritePost .vimrc call ExecuteExrc(v:false)
autocmd VimEnter * if ExecuteExrc() | silent doautocmd <nomodeline> vimrc BufRead | endif

# Report asyncrun status after finish
autocmd User AsyncRunStop {
    if g:asyncrun_code != 0 | echohl WarningMsg | endif
    unsilent echo "(AsyncRun finished with code " .. g:asyncrun_code .. ")"
    echohl Normal
}

