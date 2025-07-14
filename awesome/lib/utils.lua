local Gio = require("lgi").Gio

local res = {}

-- Wrap the function with pcall_notify and Gio.Async.start
function res.wrap_async_pcall(func)
    return function(...) Gio.Async.start(function(...) pcall_notify(func, ...) end)(...) end
end

return res
