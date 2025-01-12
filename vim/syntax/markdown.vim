vim9script

for env in ["multline", "equation", "align"]
    exec 'syn region mkdMath start=/\V\\begin{\z('..env..'*\?\)}/ end=/\V\\end{\z1}/ keepend contains=@tex'
endfor

syn region texComment start=/\V\\begin{comment}/ end=/\V\\end{comment}/ keepend
