vim9script

export def TermRun(cmd: list<string>, args_: dict<any> = v:none): number
    const args = extend({ persist: false, unique: false, termfn: function("term_start") }, args_ ?? {})
    var realcmd = args->get("shell", false) ? ["zsh", "-c", join(cmd, " ")] : cmd
    def Post(nr: number): void
        if nr != bufnr() | return | endif
        set modifiable bufhidden=wipe
        normal i 
        set bt=nowrite
    enddef
    def Exec(): number
        var nr: number
        const Fn = args->get("termfn", function("term_start"))
        belowright nr = Fn(realcmd, {
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

export def CanCmd(cmd: string): bool
    return !exists(":" .. cmd) || get(b:, tolower(cmd) .. "_overridable", true)
enddef

export class CmdEngine
    var cmd: string
    var ft: string
    public var exec: func(...list<string>)
    var options: dict<any> = {}

    def new(this.cmd, exec: any, this.options = v:none, ft: string = "")
        this.ft = ft == "" ? &ft : ft
        if type(exec) == v:t_string
            this.exec = this.ExpandVar(exec)
            if !this.options->has_key("compl")
                this.options.compl = (prev, line, _) =>
                    prev[0] == '+' ? ["+clear"] :
                    line =~ "+clear" ? [] : ["+clear"] + getcompletion(prev, "file")
            endif
        elseif type(exec) == v:t_func
            this.exec = exec
        else
            throw "E1272: Expected string or func, but got " .. type(exec)
        endif
    enddef
    def ExpandVar(cmdpat: string): func(...list<string>)
        const flagname = this.cmd->tolower() .. "_flags"
        def Work(...args: list<string>)
            var b = b:
            if args->get(0, "") == "+clear"
                b[flagname .. "_save"] = []
            elseif !args->empty()
                b[flagname .. "_save"] = args
            endif
            const varlist = {
                args: [get(g:, $"{flagname}_{this.ft}") ?? get(b:, flagname) ?? [],
                    b->get(flagname .. "_save", [])]->flattennew()->join(" "),
            }
            def Var(name: list<string>): string
                if !varlist->has_key(name[1]) | throw "No such variable in compile pattern" | endif
                return varlist[name[1]]
            enddef
            const cmdctnt = substitute(cmdpat, '\v\{(\w+)\}', Var, 'g')
            exec cmdctnt
        enddef
        return Work
    enddef

    def Opt(opts: dict<any>): any
        this.options->extend(opts)
        return this
    enddef
    def Exec(Fn: func(...list<string>)): any
        this.exec = Fn
        return this
    enddef
    def WrapExec(Fn: func(func(...list<string>), ...list<string>)): any
        const PreExec: func(...list<string>) = this.exec
        this.exec = (...args) => {
            call(Fn, [PreExec] + args)
        }
        return this
    enddef

    def Do()
        if !CanCmd(this.cmd) && !this.options->get("force", false) | return | endif
        final b = b:
        b[tolower(this.cmd) .. "_overridable"] = 0
        b[this.cmd] = this.exec
        const bar = this.options->get("bar", true) ? "-bar" : ""
        b[this.cmd .. "Compl"] = this.options->get("compl", (prev, _, _) => getcompletion(prev, "cmdline"))
        exec $'command! -buffer -nargs=* -complete=customlist,b:{this.cmd}Compl {bar} {this.cmd} b:{this.cmd}(<f-args>)'
    enddef

    def AddFlags(): any
        const flagname = this.cmd->tolower() .. "_flags"
        return this.WrapExec((F, ...args) => {
        })
    enddef

    def WaitAsyncCmd(waitcmd: string = "Compile"): any
        return this.WrapExec((F, ...args) => {
            exec waitcmd
            b:after_asyncrun = () => call(F, args)
            autocmd User AsyncRunStop ++once if g:asyncrun_code == 0 | call(b:after_asyncrun, []) | endif
        })
    enddef
endclass
