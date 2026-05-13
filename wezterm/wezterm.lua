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
		-- return "tokyonight-day"
		-- return "Gruvbox Material (Gogh)"
		-- return "Gruvbox Light"
		-- return "Catppuccin Latte"
		return "Kanagawa Lotus (Gogh)"
	else
		-- return "Tokyo Night"
		-- return "tokyonight"
		return "Kanagawa (Gogh)"
		-- return "Kanagawa Dragon (Gogh)"
	end
end

config.color_scheme = scheme_for_appearance(get_appearance())

-- config.font = wezterm.font 'FiraCode Nerd Font'
-- config.font = wezterm.font("FiraCode Nerd Font Propo")
config.font = wezterm.font("JetBrains Mono")
config.font_size = 13
-- config.font_size = 12
config.line_height = 1.0 -- may need to be 1 to prevent claude code bugs
-- config.line_height = 1.0
-- config.line_height = 0.9
config.max_fps = 144

config.harfbuzz_features = {"calt=1", "clig=1", "liga=1"}
-- config.freetype_load_target = "Light"
config.freetype_load_target = "Normal"

-- Per-screen tuning. Static config above is the GNV32DB-tuned default and
-- the fallback if the event handler fails. Overrides are applied at runtime
-- from inside window-config-reloaded, which is the only safe place to call
-- wezterm.gui.screens() — calling it at top-level can hang on startup.
-- Reload config (CMD+SHIFT+R) after moving a window between displays.
local function tuning_for(name)
	if name:find("[Bb]uilt[%- ]?[Ii]n") or name:find("[Ll]iquid [Rr]etina") then
		-- MBP 16" built-in: ~20" viewing distance, 254 PPI
		return { font_size = 12, freetype_load_target = "Light", max_fps = 120 }
	elseif name:find("U4025") then
		-- Dell U4025QW: ~30" viewing distance, 139 PPI HiDPI
		return { font_size = 14, freetype_load_target = "Light", max_fps = 120 }
	end
	-- Viotek GNV32DB / low-DPI external fallback: ~30" viewing distance, 92 PPI
	return { font_size = 13, freetype_load_target = "Normal", max_fps = 144 }
end

local function apply_screen_tuning(window)
	local ok, screens = pcall(wezterm.gui.screens)
	if not ok or not screens or not screens.active then
		return
	end
	local name = screens.active.name or ""
	local tuned = tuning_for(name)

	local overrides = window:get_config_overrides() or {}
	-- Equality guard prevents an infinite reload cycle: set_config_overrides
	-- triggers another window-config-reloaded firing.
	if overrides.font_size == tuned.font_size
		and overrides.freetype_load_target == tuned.freetype_load_target
		and overrides.max_fps == tuned.max_fps then
		return
	end

	overrides.font_size = tuned.font_size
	overrides.freetype_load_target = tuned.freetype_load_target
	overrides.max_fps = tuned.max_fps
	window:set_config_overrides(overrides)
end

-- Top-level color_scheme is evaluated once at config load; wezterm does not
-- reload the config on a macOS Dark↔Light toggle, so we push the live scheme
-- via overrides from the window-config-reloaded / window-resized handlers.
-- Same equality-guard pattern as apply_screen_tuning to avoid reload loops.
local function apply_color_scheme(window)
	local appearance = wezterm.gui and wezterm.gui.get_appearance() or "Dark"
	local scheme = scheme_for_appearance(appearance)
	local overrides = window:get_config_overrides() or {}
	if overrides.color_scheme == scheme then
		return
	end
	overrides.color_scheme = scheme
	window:set_config_overrides(overrides)
end

wezterm.on("window-config-reloaded", function(window, pane)
	apply_screen_tuning(window)
	apply_color_scheme(window)
end)

-- Dragging between displays of different DPIs fires window-resized, so this
-- catches cross-display moves without the user needing to reload config.
wezterm.on("window-resized", function(window, pane)
	apply_screen_tuning(window)
	apply_color_scheme(window)
end)

config.keys = {
	{ key = "Enter", mods = "SHIFT", action = wezterm.action({ SendString = "\x1b\r" }) },
}

-- and finally, return the configuration to wezterm
return config
