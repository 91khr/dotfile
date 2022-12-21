local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local naughty = require("naughty")
local wibox = require("wibox")

local prompt_widget = {}

local default_modes = {
    shell = {
        exec = function (args)
            local res = awful.spawn(args)
            if type(res) == type("") then
                naughty.notify({
                    preset = naughty.config.presets.critical,
                    title = "Command execution error",
                    timeout = 4,
                    text = res
                })
            end
        end,
        completion_callback = function (cmd, pos, ncomp)
            local res, newpos, matches = awful.completion.shell(cmd, pos, ncomp)
            prompt_widget:set_complist(matches, ncomp)
            return res, newpos, matches
        end,
        prompt = "Run: ",
        history_path = gears.filesystem.get_cache_dir() .. "/history",
    },
    lua = {
        exec = function (args)
            awful.util.eval(args)
        end,
        completion_callback = function (cmd, pos, ncomp)
            local pre, final = cmd:sub(1, pos):match("([%w_.:]-)([%w_]*)$")
            if not pre and not final then return cmd, pos, {} end
            local env = _ENV
            for cur in (pre or ""):gmatch("([%w_]+)[.:]") do
                if type(env[cur]) ~= "table" then
                    return cmd, pos, {}
                else
                    env = env[cur]
                end
            end
            local res = {}
            for k, _ in pairs(env) do
                if k:sub(1, final:len()) == final then
                    res[#res + 1] = k
                end
            end
            table.sort(res, function (a, b) return a < b end)
            prompt_widget:set_complist(res, ncomp)
            if #res > 0 then
                ncomp = (ncomp - 1) % #res + 1
                return cmd:sub(1, pos - final:len() - 1) .. res[ncomp] .. cmd:sub(pos + 1),
                    pos + res[ncomp]:len(), res
            else
                return cmd, pos, res
            end
        end,
        prompt = "Lua exec: ",
        history_path = gears.filesystem.get_cache_dir() .. "/history_eval",
    },
}

local function worker(widget_args)
    prompt_widget.widget = awful.popup {
        widget = {
            -- TODO: Refactor the whole component to support better completion
            {
                {
                    id = "mypromptbox",
                    widget = wibox.widget.textbox,
                },
                {
                    {
                        markup = '(Completion textbox, initial)',
                        --width = ?,
                        id = "mycompl",
                        widget = wibox.widget.textbox,
                    },
                    direction = "v",
                    fps = 0.1,
                    step_function = function() return 0 end,
                    id = "mycompl_cont",
                    layout = wibox.container.scroll.horizontal,
                },
                spacing = 1,
                spacing_widget = {
                    color = beautiful.border_marked,
                    widget = wibox.widget.separator,
                },
                id = "mymainlayout",
                layout = wibox.layout.fixed.vertical,
            },
            margins = 3,
            widget  = wibox.container.margin
        },
        border_color = beautiful.border_marked,
        border_width = beautiful.border_width * 2 / 3,
        ontop        = true,
        opacity      = 0.9,
        visible      = false,
        --placement  = ?,
        shape        = gears.shape.rounded_rect
    }

    prompt_widget.modes = widget_args.modes or default_modes
    for name, info in pairs(widget_args.modes or default_modes) do
        local patch = info
        function patch.done_callback() prompt_widget.widget.visible = false end
        function patch.exe_callback(args)
            prompt_widget.widget.visible = false
            info.exec(args)
        end
        patch.textbox = prompt_widget.widget.widget.mymainlayout.mypromptbox
        prompt_widget.modes[name] = patch
    end

    function prompt_widget:run(scr, mode)
        self:set_complist(nil)
        self.widget.widget.mymainlayout.mycompl_cont:set_max_size(scr.workarea.height * 0.6)
        self.widget.placement = function(d)
            local f = awful.placement.center_horizontal + awful.placement.top
            f(d, { offset = { y = scr.workarea.height * 0.3 } })
        end
        self.widget.minimum_width = scr.workarea.width * 0.8
        self.widget.visible = true
        awful.prompt.run(self.modes[mode])
    end

    function prompt_widget:set_complist(matches, raw_ncomp)
        local markup = "(This message means an error)"
        if not matches or #matches == 0 then
            markup = '<span foreground="cyan">(No completion)</span>'
        else
            markup = ""
            local ncomp = (raw_ncomp - 1) % #matches + 1
            local width = 0
            for i, v in ipairs(matches) do
                if width == 7 then
                    width = 0
                    markup = markup .. "\n"
                end
                if i == ncomp then
                    markup = markup .. '<span background="' .. beautiful.bg_focus .. '">' .. v .. "</span>"
                else
                    markup = markup .. v
                end
                markup = markup .. " "
                width = width + 1
            end
        end
        self.widget.widget.mymainlayout.mycompl_cont.mycompl.markup = markup
    end

    return prompt_widget.widget
end

return setmetatable(prompt_widget, {
    __call = function(_, ...)
        return worker(...)
    end,
})
