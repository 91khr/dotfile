vim9script

for env in ["multline", "equation", "align"]
    exec 'syn region mkdMath start=/\\begin{\z(' .. env .. '\*\?\)}/ end=/\\end{\z1}/ keepend contains=@tex'
endfor

syn region mkdTexComment start=/^\\begin{comment}/ end=/\\end{comment}/
syn cluster mkdListItem add=mkdTexComment
syn cluster mkdNonListItem add=mkdTexComment
hi link mkdTexComment Comment

syn region mkdLink matchgroup=mkdDelimiter start=/\[@\ze[^]\n]*\]/ end=/]/ concealends skipwhite
syn match mkdLink /\v\@%(\w|[-])+/
