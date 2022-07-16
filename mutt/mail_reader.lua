-- Pandoc reader for pure text mails

function Reader(input, opts)
    -- Turn flow into line break
    local src = tostring(input)
    src = src:gsub(" \n", " \\\n")
    -- Split out the responsing line
    src = src:gsub("\n\nOn.+,.+wrote:\n", "%0\n")
    -- Flip soft break and line break :) x
    local filter = {
        SoftBreak = function(s)
            return pandoc.LineBreak {}
        end,
        LineBreak = function(s)
            return pandoc.SoftBreak {}
        end,
    }
    setmetatable(filter, {
        __index = function(k)
            return function(s)
                return s
            end
        end,
    })
    return pandoc.read(src, "gfm+tex_math_dollars"):walk(filter)
end

