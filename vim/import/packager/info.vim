vim9script

var infobuf = -1
# name: string -> { lnum: int, status: string, lasthl: (match id), }
var packlist: dict<dict<any>>
var hlprops = {
    name: "Identifier",
    pending: "Comment",
    running: "Normal",
    error_exit: "WarningMsg",
    oper_exit: "Label",
    ok_exit: "String",
    }

export def Show(packages: list<string>)
    if infobuf != -1 && bufexists(infobuf)
        return
    endif
    # Init the buf
    vsplit [Package Status]
    setlocal nonu bt=nofile bufhidden=delete noswapfile nobuflisted
    infobuf = bufnr()
    # Fill in the content
    append("$", repeat([''], packages->len()))
    for id in range(len(packages))
        var name = packages[id]
        packlist[name] = { lnum: id + 1, status: "", lasthl: -1 }
        Update(name, { text: "Pending", status: "pending" })
        matchaddpos(hlprops['name'], [[id + 1, 1, len(name)]])
    endfor
enddef

export def Hide()
    if infobuf == -1 || !bufexists(infobuf)
        return
    endif
    exec ":" .. infobuf .. "bw"
enddef

# status: { text: string, status: ("pending" | "running" | "error_exit" | "ok_exit")? }
export def Update(name: string, status: dict<string>)
    if infobuf == -1 || !bufexists(infobuf)
        return
    endif
    var lnum = packlist[name].lnum
    var proplen: number
    if status->has_key("text")
        setbufline(infobuf, lnum, name .. ": " .. status.text)
        proplen = len(status.text)
    else
        proplen = getbufline(infobuf, lnum)[0]->len() - len(name) - 2
    endif
    packlist[name].status = get(status, "status", packlist[name].status)
    if packlist[name].lasthl != -1
        matchdelete(packlist[name].lasthl, bufwinid(infobuf))
    endif
    packlist[name].lasthl = matchaddpos(hlprops[packlist[name].status],
        [[packlist[name].lnum, len(name) + 3, proplen]],
        10, -1, { window: bufwinid(infobuf) })
enddef

# vim:fdm=marker
