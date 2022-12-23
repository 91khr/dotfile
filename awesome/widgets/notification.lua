local wibox = require("wibox")
local beautiful = require("beautiful")
local gears = require("gears")
local awful = require("awful")

local notification_widget = {}

local function worker(args)
    local icon_dir = args.icon_dir or ""
    local scroll_step = args.scroll_step or awful.screen.focused().workarea.height * 0.04

    notification_widget.systray = wibox.widget {
        {
            {
                image = icon_dir .. "notifications.svg",
                resize = true,
                forced_width = 24,
                forced_height = 18,
                widget = wibox.widget.imagebox,
            },
            {
                id = "mycount",
                text = "0",
                widget = wibox.widget.textbox,
            },
            spacing = 2,
            widget = wibox.layout.fixed.horizontal,
        },
        widget = wibox.container.place,
    }

    notification_widget.popup = awful.popup {
        widget = wibox.widget {
            {
                {
                    {
                        id = "myclearbut",
                        image = icon_dir .. "trash.svg",
                        resize = true,
                        forced_width = 23,
                        forced_height = 23,
                        widget = wibox.widget.imagebox,
                    },
                    halign = "right",
                    widget = wibox.container.place,
                },
                {
                    {
                        id = "mynotilist",
                        spacing = 1,
                        spacing_widget = {
                            color = beautiful.border_marked,
                            widget = wibox.widget.separator,
                        },
                        layout = wibox.layout.fixed.vertical,
                    },
                    id = "mynotiview",
                    fps = 0.00001,
                    layout = wibox.container.scroll.vertical,
                },
                spacing = 1,
                spacing_widget = {
                    color = beautiful.border_marked,
                    widget = wibox.widget.separator,
                },
                layout = wibox.layout.fixed.vertical,
            },
            left = 2, right = 2, top = 2, bottom = 2,
            widget = wibox.container.margin,
        },
        ontop = true,
        visible = false,
        maximum_height = awful.screen.focused().workarea.height * 0.8,
        minimum_width = awful.screen.focused().workarea.width * 0.2,
        maximum_width = awful.screen.focused().workarea.width * 0.3,
        placement = function(c)
            awful.placement.top_right(c, { honor_workarea = true })
        end,
        shape = gears.shape.rounded_rect,
    }

    notification_widget.count = 0
    notification_widget.cached = {}
    function notification_widget:add_notification(noti)
        table.insert(self.cached, { noti, id = #self.cached })
        local widget
        if noti.box then
            widget = noti.box
        elseif noti.icon then
            widget = wibox.widget {
                {
                    image = noti.icon,
                    resize = true,
                    widget = wibox.widget.imagebox,
                },
                {
                    markup = (noti.title and "<b>" .. noti.title .. "</b>\n" or "") .. (noti.text or ""),
                    widget = wibox.widget.textbox,
                },
                forced_width = awful.screen.focused().workarea.height * 0.05,
                forced_height = awful.screen.focused().workarea.width * 0.05,
                layout = wibox.layout.fixed.horizontal
            }
        else
            widget = wibox.widget {
                markup = (noti.title and "<b>" .. noti.title .. "</b>\n" or "") .. (noti.text or ""),
                widget = wibox.widget.textbox,
            }
        end
        self.popup.widget:get_children_by_id("mynotilist")[1]:add(widget)
        self.count = self.count + 1
        self.systray:get_children_by_id("mycount")[1].text = tostring(self.count)
    end
    function notification_widget:clear_notification()
        self.scrollvol = 0
        self.count = 0
        self.cached = {}
        self.systray:get_children_by_id("mycount")[1].text = "0"
        self.popup.widget:get_children_by_id("mynotilist")[1]:set_children({})
        collectgarbage()
    end

    notification_widget.scrollvol = 0
    notification_widget.notiview = notification_widget.popup.widget:get_children_by_id("mynotiview")[1]
    notification_widget.notiview:pause()
    function notification_widget.notiview.step_function(_, maxsize, viewsize)
        notification_widget.scrollvol = math.max(0, math.min(maxsize - viewsize, notification_widget.scrollvol))
        return notification_widget.scrollvol
    end

    function notification_widget:scroll_list(vol)
        self.scrollvol = self.scrollvol + vol
        self.notiview:emit_signal("widget::redraw_needed")
    end
    function notification_widget:toggle_list()
        self.popup.visible = not notification_widget.popup.visible
    end

    notification_widget.systray:buttons(
        gears.table.join(
            awful.button({}, 1, function() notification_widget:toggle_list() end)
        )
    )
    notification_widget.popup:buttons(
        gears.table.join(
            awful.button({}, 4, function() notification_widget:scroll_list(-scroll_step) end),
            awful.button({}, 5, function() notification_widget:scroll_list(scroll_step) end)
        )
    )
    notification_widget.popup.widget:get_children_by_id("myclearbut")[1]:buttons(
        gears.table.join(
            awful.button({}, 1, function()
                notification_widget:clear_notification()
                notification_widget:toggle_list()
            end)
        )
    )

    function notification_widget:on_notification(noti)
        if noti.freedesktop_hints then
            self:add_notification(noti)
        end
        return noti
    end

    return notification_widget.systray
end

return setmetatable(notification_widget, {
    __call = function(_, ...)
        return worker(...)
    end,
})
