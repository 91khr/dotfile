-- Media controls
local lgi = require("lgi")
local Gio = lgi.Gio
local utils = require("lib.utils")

local bus
local players = {
    stack = {},
    loc = {},
    avail = 0,
}

local res = {
}

function players:compact_avail()
    if self.avail * 2 >= #self.stack then return end
    local fr = 1
    for i = 1, #self.stack do
        if self.stack[i] then
            self.stack[fr] = self.stack[i]
            fr = fr + 1
        end
    end
    for i = fr, #self.stack do
        self.stack[i] = nil
    end
    for i, v in ipairs(self.stack) do
        self.loc[v] = i
    end
    self.avail = #self.stack
end

function players:push(p)
    local loc = self.loc[p]
    if loc then
        if loc == #self.stack then return end
        self.stack[loc] = false
        self.avail = self.avail - 1
        self:compact_avail()
    end
    table.insert(self.stack, p)
    self.loc[p] = #self.stack
    self.avail = self.avail + 1
end

function players:remove(p)
    self.stack[self.loc[p]] = false
    self.loc[p] = nil
    self.avail = self.avail - 1
    self:compact_avail()
end

function players:top()
    return self.stack[#self.stack]
end

local function is_mpris_name(name)
    return name:match("^org%.mpris%.MediaPlayer2%.")
end

local function on_mpris_player_appeared(name)
    if not is_mpris_name(name) then return end
    local proxy = Gio.DBusProxy.async_new(
        bus,
        Gio.DBusProxyFlags.NONE,
        nil,
        name,
        "/org/mpris/MediaPlayer2",
        "org.mpris.MediaPlayer2.Player"
    )

    function proxy:on_g_properties_changed()
        players:push(name)
    end

    players:push(name)
end

local function on_mpris_player_vanished(name)
    players:remove(name)
end

local function on_name_owner_changed(conn, sender, objpath, ifname, signame, param)
    local name, oldowner, newowner = param.value[1], param.value[2], param.value[3]
    if not is_mpris_name(name) then return end
    if oldowner == "" and newowner ~= "" then
        on_mpris_player_appeared(name)
    elseif oldowner ~= "" and newowner == "" then
        on_mpris_player_vanished(name)
    end
end

local function do_init()
    bus = Gio.async_bus_get(Gio.BusType.SESSION)
    local proxy = Gio.DBusProxy.async_new(bus, Gio.DBusProxyFlags.NONE, nil, "org.freedesktop.DBus", "/org/freedesktop/DBus", "org.freedesktop.DBus")
    local names = proxy:async_call("org.freedesktop.DBus.ListNames", nil, Gio.DBusCallFlags.NONE, -1)
    for k, v in names[1]:pairs() do
        on_mpris_player_appeared(v)
    end
    -- Watch the players
    bus:signal_subscribe(
        nil, "org.freedesktop.DBus", "NameOwnerChanged", nil, nil, Gio.DBusSignalFlags.NONE,
        utils.wrap_async_pcall(on_name_owner_changed)
    )
end
function res.init()
    Gio.Async.start(function() pcall_notify(do_init) end)()
end

local function do_call(func)
    if not bus then return end
    local name = players:top()
    if not name then return end
    local proxy = Gio.DBusProxy.async_new(
        bus,
        Gio.DBusProxyFlags.NONE,
        nil,
        name,
        "/org/mpris/MediaPlayer2",
        "org.mpris.MediaPlayer2.Player"
    )
    proxy:async_call("org.mpris.MediaPlayer2.Player." .. func, nil, Gio.DBusCallFlags.NONE, -1)
end
function res.call(func)
    Gio.Async.start(function() pcall_notify(do_call, func) end)()
end

function res.debug()
    return { bus, players }
end

return res
