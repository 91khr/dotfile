local chain_nil = { fn = {}, var = {} }
setmetatable(chain_nil.fn, {
    __index = function()
        return function()
        end
    end
})
setmetatable(chain_nil.var, {
    __index = function()
    end
})

local filters = {}

filters.linkinclude = {
    Block = function(elem)
        if elem.tag ~= "Para" and elem.tag ~= "Figure" then return end
        local sub = elem.content[1]
        if sub.tag ~= "Link" then return end
        if sub.attributes.code ~= nil then
            local src = io.open(sub.target)
            if not src then error(("Not found source file %s"):format(sub.target)) end
            src = src:read('a')
            if not src then error(("Can't read source file %s"):format(sub.target)) end
            -- Clean up src
            local pos = 1
            local res = ""
            while true do
                local next = src:find("%f[^\0\n][^\0\n]-{{{ included: invisible.-\n", pos)
                if not next then break end
                res = res .. src:sub(pos, next - 1)
                local _, fin = src:find("%f[^\0\n][^\0\n]-}}} end invisible.-\n", next)
                pos = fin + 1
            end
            -- Make the block
            res = pandoc.CodeBlock(res .. src:sub(pos))
            res.classes[1] = sub.attributes.code
            return res
        end
    end,
}

--- @param attr string
local function trans_link(attr)
    return function(elem)
        if elem[attr]:sub(1, 2) == "@/" then
            print(pandoc.path.directory(PANDOC_STATE.input_files[1]), elem[attr]:sub(2))
            elem[attr] = pandoc.path.join({ pandoc.path.directory(PANDOC_STATE.input_files[1]), elem[attr]:sub(3) })
        end
        return elem
    end
end

return {
    -- Apply filters in meta
    {
        Pandoc = function(doc)
            for _, v in pairs(doc.meta.filter or {}) do
                doc = doc:walk(filters[v[1].text])
            end
            return doc
        end
    },
    -- Translate the paths in links
    {
        Link = trans_link('target'),
        Image = trans_link('src'),
    },
}
