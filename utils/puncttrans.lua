#!/bin/env lua

local all_zh = os.getenv("ALL_ZH") == "1"

local function split_str(str)
    return function(cur, pos)
        cur = cur:sub(pos)
        if cur:len() == 0 then return nil end
        -- BBCode tag or link
        local prefix = cur:match("^!?%[.-%]")
        if prefix then
            local peek = cur:match("^%(.-%)", prefix:len() + 1)
            if peek then prefix = prefix .. peek end
            return pos + prefix:len(), prefix, "tag/link"
        end
        -- link definition
        prefix = cur:match("^\n%[[^\n]-%]:[^\n]*\n")
        if prefix then
            return pos + prefix:len(), prefix, "linkdef"
        end
        -- Code (inline or block)
        prefix = cur:match("^`+")
        if prefix then
            local _1, peek = cur:find(string.rep("`", prefix:len()), prefix:len() + 1, true)
            if not peek then peek = cur:len() end
            return pos + peek, cur:sub(1, peek), "code"
        end
        -- Ordered lists
        prefix = cur:match("^\n[ \t]-%d+%.")
        if prefix then
            return pos + prefix:len(), prefix, "ordlist"
        end
        -- Normal text
        local peek = cur:find("[%[!`\n\\]", cur:match("^\\") and 3 or (cur:match("^[!\n]") and 2 or 1))
        if not peek then peek = cur:len() + 1 end
        return pos + peek - 1, cur:sub(1, peek - 1), "ordinary"
    end, str, 1
end

local lastlang = "zh"
local quotes = {}
local function replace_punct(str)
    local res = str:gsub("([^%p%d%s]?)([().,:;?!\"' \t]+)", function(lead, punct)
        if not all_zh and (lead:match("%a") or (lead == "" and lastlang == "en")) then
            lastlang = "en"
        elseif lead:len() > 0 then
            lastlang = "zh"
        end
        if punct:match("^%s+$") then return lead .. punct end
        local function mkquote(punct)
            if #quotes == 0 or punct ~= quotes[#quotes].value then
                table.insert(quotes, { value = punct == "(" and ")" or punct, lang = lastlang })
                return lastlang == "zh" and
                        ({ ["\""] = "「", ["'"] = "「", ["("] =  "（" })[punct] or
                        punct
            else
                local res = quotes[#quotes].lang == "zh" and
                        ({ ["\""] = "」", ["'"] = "」", [")"] = "）" })[punct] or
                        quotes[#quotes].value
                lastlang = quotes[#quotes].lang
                table.remove(quotes)
                return res
            end
        end
        local function entrans(punct)
            if punct:match("[\"'()]") then
                return mkquote(punct)
            else
                return punct
            end
        end
        local function zhtrans(punct)
            if punct:match("%s") then
                return ""
            elseif not punct:match("[\"'()]") then
                return ({
                    ["."] = "。",
                    [","] = "，",
                    [":"] = "：",
                    [";"] = "；",
                    ["?"] = "？",
                    ["!"] = "！",
                })[punct]
            else
                return mkquote(punct)
            end
        end
        return lead .. punct:gsub(".", function(punct)
            return lastlang == "zh" and zhtrans(punct) or entrans(punct)
        end)
    end)
    return res
end

local src = io.stdin:read("*all")
for pos, ctnt, needy in split_str(src) do
    if needy == "ordinary" then
        ctnt = replace_punct(ctnt)
    end
    io.stdout:write(ctnt)
end
