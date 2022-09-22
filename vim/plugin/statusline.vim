vim9script

import "lines9.vim"
import "lines9/color.vim"
import "lines9/utils.vim"
import "lines9/components.vim"

final conf = lines9.GetPreset("default")

var cached_val = {}
def UpdateChangedCache(_, pat: string)
    final bvar = b:
    for [name, Update] in cached_val->items()
        if b:changedtick > bvar->get("cache_" .. name, [0, ""])[0]
            bvar["cache_" .. name] = [b:changedtick, Update()]
        endif
    endfor
enddef

# {{{ Listeners and dispatcher
var leaving = false
def LeavingRefresh(..._)
    leaving = true
    lines9.Refresh({ scope: "window" })
enddef

conf.autocmds += ["FileType", "TextChanged", "InsertLeave"]
conf.listeners->extend({
    "autocmd:WinLeave": { 0: [LeavingRefresh] },
    "autocmd:FileType": {
        0: [(..._) => lines9.Refresh({ scope: "window" })],
        1: [(..._) => lines9.Update({ type: "statusline" })],
    },
    "autocmd:CocNvimInit": {
        0: [(..._) => lines9.Refresh({ scope: "window" })],
        1: [(..._) => lines9.Update({ type: "statusline" })]
    },
    "autocmd:TextChanged": { 2: [UpdateChangedCache] },
    "autocmd:InsertLeave": { 2: [UpdateChangedCache] },
})

def DispatchWin(loc: any): string
    if loc.type == "tabline"
        return "tabline"
    else
        const prefix = (leaving || win_getid() != loc.winid) ? "inactive" : "active"
        const ft = getwinvar(loc.winid, "&ft")
        const ftspec = (ft == "nerdtree" || ft == "coc-explorer") ? "_tree" : ""
        leaving = false
        return prefix .. ftspec
    endif
enddef
conf.dispatch = DispatchWin
# }}} End listeners and dispatcher

# {{{ Wordcount
const wordcounted_filetypes = ["markdown", "mail", "text"]
perl <<EOF
sub WordCount {
    my $ctnt = join ' ', $curbuf->Get(1..$curbuf->Count());
    utf8::decode($ctnt);
    my $hanzi = 0 + $ctnt =~ s/\p{Han}/ i /g;
    my $count = () = $ctnt =~ /\b\w[\w'.]*\b/g;
    my $res = "$count WD";
    if ($hanzi != 0) {
        $res = $res . ", $hanzi HZ";
    }
    return $res;
}
EOF
cached_val.wordcount = () => perleval("WordCount")
conf.components.wordcount = utils.MakeComponent((win) => {
    const buf = winbufnr(win)
    if index(wordcounted_filetypes, buf->getbufvar("&ft")) == -1 || buf->getbufvar("wordcount_disabled", false)
        return ""
    else
        setbufvar(buf, "cache_wordcount", [0, perleval("WordCount")])
        return color.Highlight("StatusLine") .. " %{b:cache_wordcount[1]} "
    endif
})
# }}} End wordcount

# {{{ Space status
def UpdateSpace(): string
    var trailing = search('\s\+$', 'ncw')
    if trailing != 0
        return " Trailing: " .. trailing .. " "
    endif
    var mixing = search('^ \+', 'ncw') != 0 ? search('^\s*\t', 'ncw') : 0
    if mixing != 0
        return " Mixed indent: " .. mixing .. " "
    endif
    return ""
enddef
cached_val.spacestatus = UpdateSpace
conf.components.spacestatus = utils.MakeComponent((win) => {
    final bvar = winbufnr(win)->getbufvar("")
    if bvar->get("spacecheck_disabled", false) || bvar->get("&bt", "") == "terminal"
        return ""
    else
        bvar.cache_spacestatus = [0, ""]
        return color.Highlight("ErrorMsg") .. "%{get(b:, 'cache_spacestatus', [0, ''])[1]}"
    endif
})
# }}} End space status

conf.components.asyncrun_status = utils.MakeComponent(
            \ color.Highlight("Keyword") .. "%{get(g:, 'asyncrun_status', '') == 'running' ? '(async running)' : ''}")

conf.components.cocstatus = utils.MakeComponent((win) => {
    if exists(":CocInfo") == 2
        return color.Highlight("CursorLine") .. "%{coc#status()}"
    else
        return ""
    endif
})

conf.schemes.inactive_tree = ["fname_inactive", "sep", "index_inactive"]
conf.schemes.active_tree = ["fname", "sep", "index"]
conf.schemes.active = ["mode", "fname", "modified", "asyncrun_status", "cocstatus",
            \ "sep", "fileinfo", "spacestatus", "wordcount", "percentage", "index"]

lines9.Init(conf)
lines9.Enable()

