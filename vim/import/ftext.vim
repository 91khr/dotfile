vim9script

export def TermRun(cmd: list<string>, args: dict<bool> = { persist: false, unique: false }): number
    var realcmd = args->get("shell", false) ? ["zsh", "-c", join(cmd, " ")] : cmd
    def Post(nr: number): void
        if nr != bufnr() | return | endif
        set modifiable bufhidden=wipe
        normal i 
        set bt=nowrite
    enddef
    def Exec(): number
        var nr: number
        belowright nr = term_start(realcmd, {
                          term_rows: float2nr(0.35 * winheight(0)),
                          term_finish: args.persist ? "open" : "close",
                          exit_cb: (_, _) => timer_start(10, (_) => Post(nr)),
                      })
        return nr
    enddef
    if args.unique
        if exists("b:replbuf") && bufexists(b:replbuf)
            exec ":" .. string(b:replbuf) .. "bw!"
        endif
        var curbuf = bufnr()
        var execbuf = Exec()
        setbufvar(curbuf, "replbuf", execbuf)
        return execbuf
    else
        return Exec()
    endif
enddef

export def MakeRun(ft: string, cmd: string, pat: string, options: dict<bool> = {}): string
    if !!exists(":" .. cmd) && !options->get("force", false)
        return ""
    endif
    var flagname = tolower(cmd) .. "_flags"
    var varlist = {
        args: "[get(g:, '" .. flagname .. "_" .. ft .. "') ?? "
            .. "get(b:, '" .. flagname .. "') ?? [], trim(<q-args>) ?? []]->flattennew()->join(' ')",
    }
    def Var(name: list<string>): string
        var v = name[1][1 : -2]
        if !varlist->has_key(v)
            throw "No such variable in compile pattern"
        endif
        return varlist[v]
    enddef
    return "command! -buffer " .. (options->get("bar", true) ? "-bar " : "") .. "-nargs=? " ..
                \ (cmd == "Compile" ? "Compile exec 'AsyncRun ' .. " :
                \     "Run Compile | autocmd User AsyncRunStop ++once ") ..
                \ substitute(pat, '\v(\{\w+\})', Var, 'g')
enddef

