vim9script

const config_dir = expand("<sfile>:p:h:h:h")

# Senioria would often type chars triggering foldings qwq><
setlocal nofoldenable
# Make text flow and be prettier~ w
setlocal formatoptions+=aw
# No space check: flow will have trailing spaces as continuation ><
b:spacecheck_disabled = true

# Preview composed HTML
import "ftext.vim"

ftext.CmdEngine.new("Compile", (...args) => {
    w
    exec "botright term ++shell " .. config_dir .. "/mutt/gen_multipart_alternative.sh < %" .. args->join(" ")
    set ft=html
}).Do()
ftext.CmdEngine.new("Run", (...args) => {
    w
    const f = tempname() .. ".html"
    job_start([&shell, &shcf, config_dir .. "/mutt/gen_multipart_alternative.sh | tail -n +3"], {
                \   in_io: "file", in_name: expand("%"),
                \   out_io: "file", out_name: f,
                \   exit_cb: (_, _) => execute("!" .. (has("win32") ? "start" : "xdg-open") .. " " .. f),
                \ })
}).Do()

