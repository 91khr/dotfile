local utils = require("pandoc.utils")

local punct_map = {
  { pat = ",%s*", repl = "，" },
  { pat = "%.%s*", repl = "。"},
  { pat = "!%s*", repl = "！" },
  { pat = "%?%s*", repl = "？" },
  { pat = ";%s*", repl = "；" },
  { pat = ":%s*", repl = "：" }
}

local filter = {
    traverse = "topdown",
}

function filter.Str(elem)
    local text = elem.text
    for _, m in pairs(punct_map) do
        text = text:gsub(m.pat, m.repl)
    end
    return pandoc.Str(text)
end

function filter.Link(elem)
    return elem, false
end

return {
    {
        Pandoc = function(doc)
            for _, k in pairs(doc.meta.transpunct or {}) do
                local mk = utils.stringify(k)
                doc.meta[mk] = doc.meta[mk]:walk(filter)
            end
            for k, blk in pairs(doc.blocks) do
                doc.blocks[k] = blk:walk(filter)
            end
            return doc
        end,
    }
}
