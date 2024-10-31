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
                    widget = wibox.container.scroll.vertical,
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
        placement = function (c)
            local wa = awful.screen.focused().workarea
            local geo = c:geometry()
            geo.width = math.min(wa.width * 0.3, math.max(wa.width * 0.2, geo.width))
            geo.height = math.min(wa.height * 0.8, geo.height)
            c:geometry(geo)
            awful.placement.top_right(c, { honor_workarea = true })
        end,
        shape = gears.shape.rounded_rect,
    }

    notification_widget.count = 0
    notification_widget.cached = {}
    function notification_widget:add_notification(noti)
        local elem = { noti }
        if noti.box then
            elem.widget = noti.box
        elseif noti.icon then
            elem.widget = wibox.widget {
                {
                    image = noti.icon,
                    resize = true,
                    auto_dpi = true,
                    widget = wibox.widget.imagebox,
                },
                {
                    markup = (noti.title and "<b>" .. noti.title .. "</b>\n" or "") .. (noti.text or ""),
                    widget = wibox.widget.textbox,
                },
                forced_width = awful.screen.focused().workarea.height * 0.05,
                forced_height = awful.screen.focused().workarea.width * 0.05,
                layout = wibox.layout.fixed.horizontal,
            }
        else
            elem.widget = wibox.widget {
                markup = (noti.title and "<b>" .. noti.title .. "</b>\n" or "") .. (noti.text or ""),
                widget = wibox.widget.textbox,
            }
        end
        self.popup.widget:get_children_by_id("mynotilist")[1]:add(elem.widget)
        table.insert(self.cached, elem)
        self.count = self.count + 1
        self.systray:get_children_by_id("mycount")[1].text = tostring(self.count)
    end
    function notification_widget:clear_notification()
        self.scrollvol = 0
        self.cached = {}
        self.count = 0
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

    notification_widget.traffic = 0
    notification_widget.traffic_timer = gears.timer {
        timeout = 1,
        callback = function ()
            notification_widget.traffic = math.max(0, notification_widget.traffic * 0.7 - 3)
            if notification_widget.traffic <= 0 then
                notification_widget.traffic_timer:stop()
            end
        end
    }
    function notification_widget:on_notification(noti)
        if noti.freedesktop_hints then
            self:add_notification(noti)
            self.traffic = self.traffic + 1
            if not self.traffic_timer.started then self.traffic_timer:start() end
            if self.traffic >= 3 then
                return nil
            end
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
