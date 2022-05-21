vim9script

# {{{ Helpers
def IsWide(): bool
    return winwidth(0) > 70
enddef
def Func(name: string): string
    return '<SNR>' .. matchstr(expand('<stack>'), '<SNR>\zs\d\+\ze_Func') .. '_' .. name
enddef
def CacheChange(name: string, Update: func(): string): string
    var b = b:  # Cache the dict, or it would be a syntax error (?)
    if get(b, name, [0, ''])[0] != b:changedtick
        b[name] = [b:changedtick, Update()]
    endif
    return b[name][1]
enddef
# }}} End helpers

# {{{ Components
def FileName(win: number, wide: bool): string
    var buf = winbufnr(win)
    if getbufvar(buf, '&ft') == 'help' && getbufvar(buf, '&ro') && !getbufvar(buf, '&modifiable')
        return expand('%:t')
    endif
    var fname = bufname(buf)
    if empty(fname)
        var bt = getbufvar(buf, '&bt')
        if bt == 'quickfix'
            return getwininfo(win)[0].loclist ? '[Location List]' : '[Quickfix List]'
        endif
        var btlist = {
                    \     nofile: '[Scratch]',
                    \     prompt: '[Prompt]',
                    \     popup: '[Popup]',
                    \ }
        return btlist->has_key(bt) ? btlist[bt] : '[No Name]'
    endif
    var relpath = fnamemodify(fname, ':.')
    return wide ? relpath :
                \ substitute(relpath, '\v([^/])([^/]*)' .. '/', '\1' .. '/', 'g')
enddef

def StatusFileName(): string
    return FileName(winnr(), IsWide())
enddef

def TabFileName(n: number): string
    return FileName(win_getid(tabpagewinnr(n), n), false)
enddef

def CocStatus(): string
    if !exists(":CocInfo") || !IsWide() | return '' | endif
    return coc#status()
enddef

def SpaceStatus(): string
    return CacheChange("spacestatus", () => {
        if !IsWide() || get(b:, "spacecheck_disabled", false) || &bt == 'terminal'
            return ''
        endif
        var trailing = search('\s\+$', 'ncw')
        if trailing != 0
            return 'Trailing: ' .. trailing
        endif
        var mixing = search('^ \+', 'ncw') != 0 ? search('^\s*\t', 'ncw') : 0
        if mixing != 0
            return 'Mixed indent: ' .. mixing
        endif
        return ''
    })
enddef

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
def WordCount(): string
    return CacheChange("wordcount", () => {
        if !IsWide() || index(wordcounted_filetypes, &ft) == -1 || get(b:, "wordcount_disabled", false)
            return ''
        endif
        return perleval("WordCount")
    })
enddef
# }}} End components

g:lightline = {
            \     colorscheme: 'solarized',
            \     component_function: {
            \         status_filename: Func('StatusFileName'),
            \         cocstatus: Func('CocStatus'),
            \     },
            \     tab_component_function: {
            \         tab_filename: Func('TabFileName'),
            \     },
            \     component_expand: {
            \         spacestatus: Func('SpaceStatus'),
            \         wordcount: Func('WordCount'),
            \     },
            \     component: { },
            \     component_type: {
            \         spacestatus: 'error',
            \         wordcount: '',
            \     },
            \     component_visible_condition: {
            \         spell: '&spell',
            \     },
            \     active: {
            \         left: [ [ 'mode', 'paste', 'spell' ],
            \                 [ 'readonly', 'status_filename', 'modified' ],
            \                 [ 'cocstatus' ],
            \         ],
            \         right: [ [ 'lineinfo' ],
            \                  [ 'percent' ],
            \                  [ 'spacestatus', 'wordcount' ],
            \                  [ 'fileformat', 'fileencoding', 'filetype' ],
            \         ],
            \     },
            \     tab: {
            \         active: [ 'tabnum', 'tab_filename', 'modified' ],
            \         inactive: [ 'tabnum', 'tab_filename', 'modified' ],
            \     },
            \ }

var wideonly_component = {
            \     fileformat: '&ff',
            \     fileencoding: '&fenc!=#""?&fenc:&enc',
            \     filetype: '&ft!=#""?&ft:"no ft"',
            \     spell: '&spell?&spelllang:""',
            \ }
var wide_condition = "winwidth(0) > 70"
for [name, exp] in items(wideonly_component)
    g:lightline.component[name] = printf("%%{%s ? (%s) : ''}", wide_condition, exp)
    var cond = g:lightline.component_visible_condition
    if cond->has_key(name)
        cond[name] = printf("(%s)&&(%s)", cond[name], wide_condition)
    else
        cond[name] = wide_condition
    endif
endfor

