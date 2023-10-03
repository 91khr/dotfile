#!/bin/env lua

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

local ZhPunctReplacer = {
    lastlang = "zh",
    quotes = {},
}
function ZhPunctReplacer:new()
    local res = {}
    setmetatable(res, self)
    self.__index = self
    return res
end

function ZhPunctReplacer:replace_punct(str)
    local res = str:gsub("([^%p%d%s]?)([().,:;?!\"' \t]+)", function(lead, punct)
        if not all_zh and (lead:match("%a") or (lead == "" and self.lastlang == "en")) then
            self.lastlang = "en"
        elseif lead:len() > 0 then
            self.lastlang = "zh"
        end
        if punct:match("^%s+$") then return lead .. punct end
        local function mkquote(punct)
            if #self.quotes == 0 or punct ~= self.quotes[#self.quotes].value then
                table.insert(self.quotes, { value = punct == "(" and ")" or punct, lang = self.lastlang })
                return self.lastlang == "zh" and
                        ({ ["\""] = "「", ["'"] = "「", ["("] =  "（" })[punct] or
                        punct
            else
                local res = self.quotes[#self.quotes].lang == "zh" and
                        ({ ["\""] = "」", ["'"] = "」", [")"] = "）" })[punct] or
                        self.quotes[#self.quotes].value
                self.lastlang = self.quotes[#self.quotes].lang
                table.remove(self.quotes)
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
                    ["."] = "。", [","] = "，",
                    [":"] = "：", [";"] = "；",
                    ["?"] = "？", ["!"] = "！",
                })[punct]
            else
                return mkquote(punct)
            end
        end
        return lead .. punct:gsub(".", function(punct)
            return self.lastlang == "zh" and zhtrans(punct) or entrans(punct)
        end)
    end)
    return res
end

local function to_enpunct(str)
    local wantspc = false
    local res = ""
    for p, ch in utf8.codes(str) do
        ch = utf8.char(ch)
        if ('。，：；？！、“”‘’（）…'):find(ch, 1, true) then
            io.stdout:flush()
            ch = ({
                ["。"] = ".", ["，"] = ",",
                ["："] = ":", ["；"] = ";",
                ["？"] = "?", ["！"] = "!",
                ["、"] = ",", ["…"] = "...",
                ["“"] = "\"", ["”"] = "\"",
                ["‘"] = "'", ["’"] = "'",
                ["（"] = "(", ["）"] = ")",
            })[ch]
            if ("\"'()"):find(ch, 1, true) then
                ch = (wantspc and " " or "") .. ch
                wantspc = false
            else
                wantspc = true
            end
            res = res .. ch
        else
            if wantspc and not ch:find("%s") then res = res .. " " end
            wantspc = false
            res = res .. ch
        end
    end
    return res
end

local modname = ...
local all_zh = os.getenv("ALL_ZH")
local detrans = os.getenv("DETRANS") == "1"

if modname == nil then
    local src = io.stdin:read("*all")
    if detrans then
        io.stdout:write(to_enpunct(src))
    else
        local repl = ZhPunctReplacer:new()
        for pos, ctnt, needy in split_str(src) do
            if needy == "ordinary" then
                ctnt = repl:replace_punct(ctnt)
            end
            io.stdout:write(ctnt)
        end
    end
else
    return {
        split_str = split_str,
        ZhPunctReplacer = ZhPunctReplacer,
        to_enpunct = to_enpunct,
    }
end
