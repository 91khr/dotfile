#!/bin/env lua

function split_str(str)
    return function(str, pos)
        str = str:sub(pos)
        if str:len() == 0 then return nil end
        -- BBCode tag or link
        local prefix = str:match("^!?%[.-%]")
        if prefix then
            local peek = str:match("^%(.-%)", prefix:len() + 1)
            if peek then prefix = prefix .. peek end
            return pos + prefix:len(), prefix, "tag/link"
        end
        -- link definition
        prefix = str:match("^\n%[[^\n]-%]:[^\n]*\n")
        if prefix then
            return pos + prefix:len(), prefix, "linkdef"
        end
        -- Code (inline or block)
        prefix = str:match("^`+")
        if prefix then
            local _1, peek = str:find(string.rep("`", prefix:len()), prefix:len() + 1, true)
            if not peek then peek = str:len() end
            return pos + peek, str:sub(1, peek), "code"
        end
        -- Ordered lists
        prefix = str:match("^\n[ \t]-%d+%.")
        if prefix then
            return pos + prefix:len(), prefix, "ordlist"
        end
        -- Normal text
        local peek = str:find("[%[!`\n\\]", str:match("^\\") and 3 or (str:match("^[!\n]") and 2 or 1))
        if not peek then peek = str:len() + 1 end
        return pos + peek - 1, str:sub(1, peek - 1), "ordinary"
    end, str, 1
end

lastlang = "zh"
function replace_punct(str)
    local quotes = {}
    local res = str:gsub("([^%p%d%s]?)([().,:;?!\"' \t]+)", function(lead, punct)
        if lead:match("%a") or (lead == "" and lastlang == "en") then
            lastlang = "en"
            return lead .. punct
        elseif lead:len() > 0 then
            lastlang = "zh"
        end
        if punct:match("^%s+$") then return lead .. punct end
        return lead .. punct:gsub(".", function(punct)
            if punct:match("%s") then
                return ''
            elseif punct ~= '"' and punct ~= "'" then
                return ({
                    ["("] = "（",
                    [")"] = "）",
                    ["."] = "。",
                    [","] = "，",
                    [":"] = "：",
                    [";"] = "；",
                    ["?"] = "？",
                    ["!"] = "！",
                })[punct]
            elseif punct == quotes[#quotes] then
                table.remove(quotes)
                return "」"
            else
                table.insert(quotes, punct)
                return "「"
            end
        end)
    end)
    return res
end

src = io.stdin:read("*all")
for pos, ctnt, needy in split_str(src) do
    if needy == "ordinary" then
        ctnt = replace_punct(ctnt)
    end
    io.stdout:write(ctnt)
end
