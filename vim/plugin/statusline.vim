vim9script

# {{{ Helpers
def IsWide(): bool
    return winwidth(0) > 70
enddef
def Func(name: string): string
    return '<SNR>' .. matchstr(expand('<stack>'), '<SNR>\zs\d\+\ze_Func') .. '_' .. name
enddef
# }}} End helpers

# {{{ Components
def FileName(): string
    if &ft == 'help' | return expand('%:t') | endif
    if expand('%:t') == ''
        if &buftype == 'quickfix'
            return getwininfo()[winnr() - 1].loclist ? '[Location List]' : '[Quickfix List]'
        endif
        var btlist = {
                    \     nofile: '[Scratch]',
                    \     prompt: '[Prompt]',
                    \     popup: '[Popup]',
                    \ }
        return btlist->has_key(&bt) ? btlist[&bt] : '[No Name]'
    endif
    var relpath = fnamemodify(expand('%'), ':.')
    var pathsep = has("win32") ? '\' : '\/'
    return IsWide() ? relpath :
                \ substitute(relpath, '\v([^/])([^/]*)' .. pathsep, '\1' .. pathsep, 'g')
enddef

def CocStatus(): string
    if !exists(":CocInfo") || !IsWide() | return '' | endif
    return coc#status()
enddef

def SpaceStatus(): string
    if !IsWide() || get(b:, "spacecheck_disabled", false) || &bt == 'terminal'
        return ''
    endif
    var pos = getcurpos()
    cursor(1, 1)
    var trailing = search('\s\+$', 'ncw')
    if trailing != 0
        cursor(pos[1:])
        return 'Trailing: ' .. trailing
    endif
    var mixing = search('^ \+', 'ncw') ? search('^\s*\t', 'ncw') : 0
    if mixing != 0
        cursor(pos[1:])
        return 'Mixed indent: ' .. mixing
    endif
    cursor(pos[1:])
    return ''
enddef

const wordcounted_filetypes = ["markdown", "mail", "text"]
perl <<EOF
my $tick = 0;
my $last = "";
sub WordCount {
    if (@_[0] == $tick) {
        return $last;
    }
    $tick = @_[0];
    my $ctnt = join ' ', $curbuf->Get(1..$curbuf->Count());
    utf8::decode($ctnt);
    my $hanzi = 0 + $ctnt =~ s/\p{Han}/ i /g;
    my $count = () = $ctnt =~ /\b\w[\w'.]*\b/g;
    $last = "$count WD";
    if ($hanzi != 0) {
        $last = $last . ", $hanzi HZ";
    }
    return $last;
}
EOF
def WordCount(): string
    if !IsWide() || index(wordcounted_filetypes, &ft) == -1 || get(b:, "wordcount_disabled", false)
        return ''
    endif
    return perleval("WordCount " .. b:changedtick)
enddef
# }}} End components

g:lightline = {
            \     colorscheme: 'solarized',
            \     component_function: {
            \         filename: Func('FileName'),
            \         cocstatus: Func('CocStatus'),
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
            \                 [ 'readonly', 'filename', 'modified' ],
            \                 [ 'cocstatus' ],
            \         ],
            \         right: [ [ 'lineinfo' ],
            \                  [ 'percent' ],
            \                  [ 'spacestatus', 'wordcount' ],
            \                  [ 'fileformat', 'fileencoding', 'filetype' ],
            \         ],
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

