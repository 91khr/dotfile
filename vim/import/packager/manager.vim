vim9script
import "packager/info.vim" as info

const statusmap = {
    missing: { text: "Not installed", status: "oper_exit" },
    diverged: { text: "Diverged with upstream", status: "error_exit" },
    latest: { text: "Up to date", status: "ok_exit" },
    outdated: { text: "Need to update", status: "oper_exit" },
    }

# [ { name: string, path: string, url: string, branch: string,
#   status: "latest" | "outdated" | "diverged" | "missing" } ]
var packconf: list<dict<string>>
var hasinit = false
var max_job = 3
var cur_job = 0
var pending_jobs: list<func()> = []
var packpath = expand(has("win32") ? "$USERPROFILE" : "$HOME") .. "/.vim/pack/packager/"
# for all opts arg: { force: bool?, silent: bool? }

# {{{ InitPack
def InitPack(opts: dict<any>)
    if hasinit && !opts->get("force", false)
        return
    endif
    # Init options
    max_job = g:packager_config->get("max_job", max_job)
    packpath = g:packager_config->get("packpath", packpath)
    if packpath[-1] != "/"
        packpath ..= "/"
    endif
    # Process packages
    for it in g:packager_config.packs
        var pack = type(it) == type("") ? { name: it } : it
        var path = packpath .. pack->get("location", "opt") .. "/" .. matchstr(pack.name, '\v\/\zs[^/]+$')
        var conf = {
            name: pack.name,
            path: path,
            path_sh: shellescape(path),
            branch: pack->get("branch", ""),
            }
        if pack.name =~# '\v^(http|https)\:\/\/'
            conf.url = pack.name
        elseif pack.name =~# '\v^[^/]+\/[^/]+$'
            conf.url = "https://github.com/" .. pack.name
        elseif pack.name =~# '\v^\w+\@[^:/]+\:'
            conf.url = pack.name
        endif
        if !isdirectory(conf.path)
            conf.status = "missing"
        elseif !empty(conf.branch) && !opts->get("force", false)
            && system("git -C " .. conf.path .. " branch --show-current")->trim() != conf.branch
            conf.status = "diverged"
        endif
        packconf->add(conf)
    endfor
    hasinit = true
enddef
# }}} End InitPack

# post return: non empty list for execute that command, empty list for head to next post
#   1 for exit and status was set, 0 for succeed exit, -1 for error exit
#   (due to vim limitations, it is wrapped in a list)
def ChainJob(name: string, cmd: list<string>, post: list<func(job, number): any>)
    if cur_job > max_job
        pending_jobs->add(function(ChainJob, [name, cmd, post]))
        return
    endif
    var hasexit = false
    # {{{ Helpers
    def OnOutput(_ch: channel, ctnt: string)
        var status = { text: ctnt->split("\r")[-1]->split("\n")[-1] }
        if !hasexit
            status.status = "running"
        endif
        info.Update(name, status)
    enddef
    def ExecPost(Fn: func(job, number): number, job: job, res: number): bool
        var state = Fn(job, res)
        if state < 1
            info.Update(name, { status: state == 0 ? "ok_exit" : "error_exit" })
            return false
        else
            return state == 2
        endif
    enddef
    # }}} End helpers
    cur_job += 1
    job_start(cmd, {
        out_mode: "raw", err_mode: "raw",
        out_cb: OnOutput, err_cb: OnOutput,
        exit_cb: (job, res) => {
            for id in range(len(post))
                var next = post[id](job, res)[0]
                if type(next) == type(0)
                    if next != 1
                        info.Update(name, { status: next == 0 ? "ok_exit" : "error_exit" })
                    endif
                    break
                elseif type(next) == type([]) && !empty(next)
                    ChainJob(name, next, post[id + 1 : ])
                    return
                endif
            endfor
            if len(post) == 0
                info.Update(name, { status: res == 0 ? "ok_exit" : "error_exit" })
            endif
            cur_job -= 1
            hasexit = true
            if !empty(pending_jobs)
                remove(pending_jobs, len(pending_jobs) - 1)()
            endif
            },
        })
enddef

def Status_Callback(pack: dict<string>, opts: dict<any>, job: job, res: number): list<any>
    if res != 0
        return [-1]
    endif
    if !empty(pack.branch)
        && system("git -C " .. pack.path_sh .. " branch --show-current")->trim() != pack.branch
        if opts->get("force", false)
            system("git -C " .. pack.path_sh .. " checkout " .. pack.branch)
            if v:shell_error != 0
                return [-1]
            endif
        else
            pack.status = "diverged"
            return [-1]
        endif
    endif
    var local = system("git -C " .. pack.path_sh .. " rev-parse @")
    var remote = system("git -C " .. pack.path_sh .. " rev-parse @{u}")
    var base = system("git -C " .. pack.path_sh .. " merge-base @ @{u}")
    if local == remote
        pack.status = "latest"
    elseif local == base
        pack.status = "outdated"
    else
        pack.status = "diverged"
    endif
    return [[]]
enddef

export def Status(opts = { force: false, silent: false })
    InitPack(opts)
    if !opts->get("silent", false)
        info.Show(packconf->mapnew("v:val.name"))
    endif
    for pack in packconf
        if pack->has_key("status")
            info.Update(pack.name, statusmap[pack.status])
            continue
        endif
        ChainJob(pack.name, ["git", "-C", pack.path, "remote", "update"],
            [function(Status_Callback, [pack, opts]), function((curpack, job, res) => {
                info.Update(curpack.name, statusmap[curpack.status])
                return [1]
            }, [pack])])
    endfor
enddef

export def Sync(opts = { force: false, silent: false })
    InitPack(opts)
    if !opts->get("silent", false)
        info.Show(packconf->mapnew("v:val.name"))
    endif
    const procfn = {
        outdated: (pack) => ["pull", "--ff-only", "--rebase=false", "--progress"],
        missing: (pack) => ["clone", pack.url],
        }
    for pack in packconf
        if pack->has_key("status") && index(["diverged", "latest"], pack.status) != -1
            info.Update(pack.name, statusmap[pack.status])
            continue
        endif
        ChainJob(pack.name, ["git", "-C", pack.path, "remote", "update"],
            [function(Status_Callback, [pack, opts]), function((curpack, job, res): list<any> => {
                if index(["latest", "diverged"], curpack.status) != -1
                    info.Update(curpack.name, statusmap[curpack.status])
                    return [1]
                else
                    return [["git", "-C", curpack.path] + procfn[curpack.status](curpack)]
                endif
            }, [pack])])
    endfor
enddef

export def Clean(opts = { force: false, silent: false })
enddef

# vim: fdm=marker
