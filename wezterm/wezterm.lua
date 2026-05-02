-- Pull in the wezterm API
local wezterm = require("wezterm")
local config = wezterm.config_builder()

-- wezterm.gui is not available to the mux server, so take care to
-- do something reasonable when this config is evaluated by the mux
local function get_appearance()
	if wezterm.gui then
		return wezterm.gui.get_appearance()
	end
	return "Dark"
end

local function scheme_for_appearance(appearance)
	if appearance:find("Light") then
		-- return "Tokyo Night Light (Gogh)"
		return "tokyonight-day"
		-- return "Gruvbox Material (Gogh)"
		-- return "Gruvbox Light"
		-- return "Catppuccin Latte"
	else
		-- return "Tokyo Night"
		return "tokyonight"
	end
end

config.color_scheme = scheme_for_appearance(get_appearance())

-- config.font = wezterm.font 'FiraCode Nerd Font'
config.font = wezterm.font("FiraCode Nerd Font Propo")
config.font_size = 14

config.keys = {
	{ key = "Enter", mods = "SHIFT", action = wezterm.action({ SendString = "\x1b\r" }) },
}

-- and finally, return the configuration to wezterm
return config
