#!/bin/env lua
quotes = {}
res = io.stdin:read("*all"):gsub("([().,:?!\"'])%s*", function(punct)
    if punct ~= '"' and punct ~= "'" then
        return ({
            ["("] = "（",
            [")"] = "）",
            ["."] = "。",
            [","] = "，",
            [":"] = "：",
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
print(res)
