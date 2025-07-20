-- Get the complete list in awesome environment in the given position
local function wm_lua_completion(cmd, pos)
    local pre, final = cmd:sub(1, pos):match("([%w_.:]-)([%w_]*)$")
    if not pre and not final then return {} end
    local env = _ENV
    for cur in (pre or ""):gmatch("([%w_]+)[.:]") do
        if type(env[cur]) ~= "table" then
            return {}
        else
            env = env[cur]
        end
    end
    local res = {}
    for k, _ in pairs(env) do
        if type(k) == "string" and k:sub(1, final:len()) == final then
            res[#res + 1] = k
        end
    end
    table.sort(res, function (a, b) return a < b end)
    return pos - final:len() - 1, res
end

-- Perform completion at a point in the source code
-- Used to assist vim completion
local function vim_env_completion(pos)
end

return {
    wm_lua_completion = wm_lua_completion,
    vim_env_completion = vim_env_completion,
}
