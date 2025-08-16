import os
import tempfile
import subprocess
import threading
import time
import shutil
# import shlex
from os.path import expanduser
from libqtile import bar, layout, qtile, widget, hook
from libqtile.backend.base import Window
from libqtile.config import Click, Drag, Group, Key, Match, Screen, ScratchPad, DropDown
from libqtile.layout import Columns, MonadTall, MonadWide, Tile
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal
from libqtile.command.base import expose_command
from libqtile.log_utils import logger
# Requires Qtile Extras
from qtile_extras import widget as extra_widget 

# === Global Variables ===
mod = "mod4"
terminal = guess_terminal()
previous_layouts = {}
group_screen_map = {}

layout_display_names = {
    "monadtall": "[]=",
    "columns": "[]|",
    "max": "[M]",
    "bsp": "|||",
    "floating": "><>",
    "treetab": "⎇|",
}


# Other Desktop Apps
# LightDM, Gnome Keyring (handled by LightDM), BetterLockScreen
# Kvantum, LxAppearance, Thunar, Pavuaudio Control, NetworkManager-UI,
# libinput-gestures, dbus-broker, python-dbus-fast, playerctl
# brightnessctl2

@hook.subscribe.startup_once
def autostart():
    wallpaper = expanduser("~/Pictures/base.png")
    picom_conf = expanduser("~/.config/picom/picom.conf")
    # subprocess.Popen(["/bin/sh", "-c", "xmodmap ~/.Xmodmap"])
    subprocess.Popen(["feh", "--bg-scale", wallpaper])
    subprocess.Popen(["picom", "--config", picom_conf, "-b"])
    subprocess.Popen(["/usr/lib/xfce4/notifyd/xfce4-notifyd"])
    def start_emacs_daemon():
        time.sleep(1)
        subprocess.Popen(["emacs", "--daemon"])
    threading.Thread(target=start_emacs_daemon).start()

# LOGIC FOR FLOATING TOP LAYER
PIP_WID = None
_PIP_ACTIVE = False

PIP_SIZE = 0.30
PIP_MARGIN = 16
PIP_CORNER = "br" # "br", "bl", "tr", "tl"

PIP_VISIBLE = True
PIP_OFFSET = None
PIP_FIXED_SIZE = None

def _place_pip(q, w):
    if not _PIP_ACTIVE or not PIP_VISIBLE:
        return

    try:
        w.info()
    except Exception:
        _disable_pip()
        return
    
    s = q.current_screen
    sw, sh = s.width, s.height

    if PIP_FIXED_SIZE:
        ww, wh = PIP_FIXED_SIZE
    else:
        ww, wh = int(sw * PIP_SIZE), int(sh * PIP_SIZE)

    if PIP_OFFSET is None:
        offx, offy = PIP_MARGIN, PIP_MARGIN
    else:
        offx, offy = PIP_OFFSET

    if PIP_CORNER == "br":
        x, y = sw - ww - offx, sh - wh - offy
    elif PIP_CORNER == "bl":
        x, y = offx, sh - wh - offy
    elif PIP_CORNER == "tr":
        x, y = sw - ww - offx, offy
    elif PIP_CORNER == "tl":
        x, y = offx, offy
    

    w.enable_floating()
    w.set_size_floating(ww, wh)
    w.set_position_floating(x, y)
    w.bring_to_front()

    # q.call_later(0.02, lambda: (w.cmd_set_position_floating(x, y), w.cmd_bring_to_front()))

    
def _force_move_to_current_group(q):
    global PIP_WID
    if PIP_WID is None:
        return
    w = q.windows_map.get(PIP_WID)
    if not w:
        return

    tgt = q.current_group.name

    # move to current group
    try:
        w.togroup(tgt, switch_group=False)
    except Exception:
        try:
            q.groups_map[tgt].add(w)
        except Exception:
            return

    # ensure it's on the focused screen, then place
    try:
        w.toscreen(q.current_screen.index)
    except Exception:
        pass
    if PIP_VISIBLE:
        _place_pip(q, w)

    try:
        q.current_group.focus_back()
    except Exception:
        pass

def _pip_corner_after_move(corner, direction):
    """Check current corner + direction to get appropriate destination"""
    table = {
        "up":    {"bl": "tl", "br": "tr", "tl": "tl", "tr": "tr"},
        "down":  {"tl": "bl", "tr": "br", "bl": "bl", "br": "br"},
        "left":  {"tr": "tl", "br": "bl", "tl": "tl", "bl": "bl"},
        "right": {"tl": "tr", "bl": "br", "tr": "tr", "br": "br"},
    }
    return table[direction][corner]

def _disable_pip():
    """Stop tracking + return window to tiling (if it still exists)."""
    global PIP_WID, _PIP_ACTIVE
    w = qtile.windows_map.get(PIP_WID) if PIP_WID is not None else None
    _PIP_ACTIVE = False
    PIP_WID = None
    if w:
        try:
            w.disable_floating()
        except Exception:
            pass

def toggle_pip(q):
    """Toggles PIP"""
    global PIP_WID, _PIP_ACTIVE
    w = q.current_window
    if not w:
        return

    # Toggle Off
    if _PIP_ACTIVE and w.wid == PIP_WID:
        _disable_pip()
        return

    if w.info().get("floating", False):
        return
    
    # Toggle On
    PIP_WID = w.wid
    _PIP_ACTIVE = True
    _force_move_to_current_group(q)  # start in current group/screen

def pip_toggle_visible(q):
    global PIP_VISIBLE
    PIP_VISIBLE = not PIP_VISIBLE
    w = q.windows_map.get(PIP_WID) if PIP_WID is not None else None
    if not w:
        return

    if PIP_VISIBLE:
        _place_pip(q, w)
    else:
        try:
            w.minimized = True
        except Exception:
            try:
                pip_remember_pos(q)
                w.set_position_floating(-10000, -10000)
            except Exception:
                pass

def pip_remember_pos(q):
    global PIP_OFFSET, PIP_FIXED_SIZE
    w = q.windows_map.get(PIP_WID) if PIP_WID is not None else None
    if not w:
        return
    info = w.info()
    x, y, ww, wh = info.get("x"), info.get("y"), info.get("width"), info.get("height")
    if None in (x, y, ww, wh):
        return

    sw, sh = q.current_screen.width, q.current_screen.height
    if PIP_CORNER == "br":
        offx, offy = sw - (x + ww), sh - (y + wh)
    elif PIP_CORNER == "bl":
        offx, offy = x, sh - (y + wh)
    elif PIP_CORNER == "tr":
        offx, offy = sw - (x + ww), y    
    elif PIP_CORNER == "tl":
        offx, offy = x, y
    else:
        # PIP_CORNER isn't working
        offx, offy = x, y

    offx = max(0, int(offx));
    offy = max(0, int(offy));
    PIP_OFFSET = (offx, offy)
    PIP_FIXED_SIZE = (int(ww), int(wh))

def pip_corner(q, corner):
    global PIP_CORNER
    PIP_CORNER = corner

    PIP_OFFSET = None
    PIP_FIXED_SIZE = None
    
    w = q.windows_map.get(PIP_WID) if PIP_WID is not None else None
    if w and _PIP_ACTIVE and PIP_VISIBLE:
        _place_pip(q, w)

def pip_resize(q, direction, amount=50):
    """Resize PIP window. Direction: grow and shrink"""
    w = q.windows_map.get(PIP_WID) if PIP_WID is not None else None
    if not w or not _PIP_ACTIVE:
        return

    try:
        info = w.info()
        current_w, current_h = info.get("width", 400), info.get("height", 300)

        if direction == "grow":
            new_w, new_h = current_w + amount, current_h + amount
        else:
            new_w, new_h = max(100, current_w - amount), max(100, current_h - amount)

        w.set_size_floating(new_w, new_h)
        pip_remember_pos(q)
    except Exception:
        pass
        
@hook.subscribe.setgroup
def _follow_group():
    # Defer so the new group is fully active before we move/raise
    if not _PIP_ACTIVE or not PIP_VISIBLE:
        return

    w = qtile.windows_map.get(PIP_WID) if PIP_WID is not None else None
    # Window disappeared 
    if not w:
        _disable_pip()
        return

    try:
        info = w.info()
        # Window was tiled at any point
        if not info.get("floating", False):
            _disable_pip()
            return
    except Exception:
        _disable_pip()
        return

    if not PIP_VISIBLE:
        return

    qtile.call_later(0.02, lambda: _force_move_to_current_group(qtile))

@hook.subscribe.current_screen_change
def _follow_screen():
    if not _PIP_ACTIVE or not PIP_VISIBLE:
        return
    w = qtile.windows_map.get(PIP_WID)
    if w:
        _place_pip(qtile, w)

@hook.subscribe.client_mouse_enter
def _auto_save_pip_position(window):
    global PIP_WID
    if not _PIP_ACTIVE or window.wid != PIP_WID:
        return

    qtile.call_later(0.1, lambda: pip_remember_pos(qtile))

@hook.subscribe.focus_change
def _check_pip_winodw_state():
    global PIP_WID, _PIP_ACTIVE
    if not _PIP_ACTIVE or PIP_WID is None:
        return

    w = qtile.windows_map.get(PIP_WID)
    if not w:
        _disable_pip()
        return

    try:
        if not w.info().get("floating", False):
            _disable_pip()
    except Exception:
        _disable_pip()
        
@hook.subscribe.client_killed
def _cleanup_pip(window):
    global PIP_WID, _PIP_ACTIVE
    if window.wid == PIP_WID:
        PIP_WID = None
        _PIP_ACTIVE = False

# Universal Switch Layout Function
@lazy.function
def set_layout_to(layout_name):
    def _inner(qtile):
        group = qtile.current_group
        for i, layout in enumerate(group.layouts):
            if layout.name == layout_name:
                group.layout = layout
                group.layout_index = i
                group.layout_all()
                break
    return _inner

        
@lazy.function
def switch_to_monad_tall(qtile):
    global monad_orientation

    group = qtile.current_group
    group_name = group.name
    current_layout = group.layout.name

    if current_layout != layout_display_names["monadtall"]:
        group_screen_map[group_name] = current_layout
        group.setlayout(layout_display_names["monadtall"])

        screen_index = next((i for i, screen in enumerate(qtile.screens) if screen.group == group), None)

        if screen_index is None or not hasattr(group.layout, "flip"):
            return

        group.layout.align = 1 if screen_index == 0 else 0
        group.layout_all()

    else:
        prev = group_screen_map.get(group_name)
        if prev and prev != layout_display_names["monadtall"]:
            group.setlayout(prev)


@lazy.function
def switch_to_max(qtile):
    group = qtile.current_group
    group_name = group.name
    current_layout = group.layout.name

    if current_layout != layout_display_names["max"]:
        previous_layouts[group_name] = current_layout
        group.setlayout(layout_display_names["max"])
    else:
        prev = previous_layouts.get(group_name)
        if prev and prev != layout_display_names["max"]:
            group.setlayout(prev)


@lazy.function
def universal_move(qtile, direction):
    """Move function that works for all different modes"""
    global PIP_CORNER

    w = qtile.current_window
    layout = qtile.current_layout
    layout_name = layout.name.lower()

    # PiP Window
    if(_PIP_ACTIVE and w is not None and PIP_WID is not None and w.wid == PIP_WID):
        try:
            PIP_CORNER = _pip_corner_after_move(PIP_CORNER, direction)
        except KeyError:
            pass

        if PIP_VISIBLE:
            _place_pip(qtile, w)
        return
        
    # TreeTab
    if layout_name == layout_display_names["treetab"] and direction in {"up", "down"}:
        if (method := getattr(layout, f"move_{direction}", None)): 
            method()
    # Everything Else
    elif (method := getattr(layout, f"shuffle_{direction}", None)): 
        method()
    elif (method := getattr(layout, direction, None)): 
        method()

@lazy.function
def universal_focus(qtile):
    """Focus function that works for all different normal modes"""
    layout = qtile.current_layout
    layout_name = layout.name.lower()

    if layout_name == layout_display_names["columns"]:
        # Toggle split mode for columns layout
        layout.toggle_split()

    elif layout_name in [layout_display_names["monadtall"], "monadwide"]:
        # Check if the focused window is already maximized
        win = qtile.current_window
        if win and getattr(win, 'maximized', False):
            layout.normalize()
        else:
            layout.toggle_auto_maximize()


def get_action(axis, position, direction, is_flipped):
    opposites = {"left": "right", "right": "left", "up": "down", "down": "up"}
    grow_rules = {
        ("vertical", "master"): "right",
        ("vertical", "first"): "down",
        ("vertical", "last"): "up",
        ("vertical", "middle"): "left",

        ("horizontal", "master"): "down",
        ("horizontal", "first"): "right",
        ("horizontal", "last"): "left",
        ("horizontal", "middle"): "up",
    }

    grow_dir = grow_rules.get((axis, position))
    shrink_dir = opposites[grow_dir]

    if is_flipped:
        temp = grow_dir
        grow_dir = shrink_dir
        shrink_dir = temp

    if direction == grow_dir:
        return "grow"
    elif direction == shrink_dir:
        return "shrink"


@lazy.function
def universal_resize(qtile, direction):
    layout = qtile.current_layout
    layout_name = layout.name.lower()
    win = qtile.current_window
    axis = "vertical" if layout_name == layout_display_names["monadtall"] else "horizontal"

    if layout_name in [layout_display_names["monadtall"], "monadwide"]:
        stack = layout.clients
        if win in stack:
            index = stack.index(win)
            position = (
                "master" if index == 0 else
                "first" if index == 1 else
                "last" if index == len(stack) -1 else
                "middle"
            )

            is_flipped = False
            if layout_name == layout_display_names["monadtall"] and position in ["master", "middle"]:
                is_flipped = layout.align == MonadTall._right
            elif layout_name == "monadwide" and position in ["master", "middle"]:
                is_flipped = layout.align == MonadWide._bottom

            if (action := get_action(axis, position, direction, is_flipped)):
               getattr(layout, action)()
    elif layout_name == layout_display_names["columns"]:
        if (method := getattr(layout, f"grow_{direction}", None)):
            method()
    else:
        if (method := getattr(layout, f"move_{direction}", None)):
            method()


def backup_config():
    config = os.path.expanduser("~/.config/qtile/config.py")
    backup = os.path.expanduser("~/.config/qtile/config.py.bak")
    shutil.copyfile(config, backup)

    
@lazy.function
def reload_with_backup(qtile):
    backup_config()
    qtile.reload_config()
    

@lazy.function
def check_config(qtile):
    result = subprocess.run(["qtile", "check"], capture_output=True, text=True)
    if result.returncode == 0:
        subprocess.run(["notify-send", "Qtile Config", "Config OK"])
    else:
        # subprocess.run(["notify-send", "Qtile Config Error", result.stderr])
         # Combine stdout and stderr for max humiliation
        error_msg = result.stderr.strip() or result.stdout.strip() or "Unknown error"
        subprocess.run(["notify-send", "Qtile Config Error", error_msg])

        
# Helper Functions
def check_net():
    try:
        # Ping 1.1.1.1 once, wait max 2 seconds
        result = subprocess.run(
            ["ping", "-c", "1", "-W", "2", "1.1.1.1"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )
        return "^ Net" if result.returncode == 0 else "X Net"
    except Exception:
        return "X Net"

    
def get_meminfo():
    meminfo = {}
    with open("/proc/meminfo") as f:
        for line in f:
            key, value = line.split(':')
            meminfo[key] = int(value.split()[0])

    total = meminfo["MemTotal"]
    available = meminfo.get("MemAvailable", meminfo["MemFree"])
    used = total - available
    # Convert kB to GiB
    return used / (1024 ** 2), total / (1024 ** 2)


# Maybe use instead of layout_display_name
@lazy.function
def layout_display_name(qtile):
    layout = qtile.current_layout
    layout_name = layout.name.lower()
    
    return layout_display_names.get(layout_name, layout_name)


def show_rofi_popup(qtile):
    # Create temp file and write the text
    with tempfile.NamedTemporaryFile(mode="w", delete=False) as f:
        f.write(popup_text)
        temp_path = f.name

        # Use shell to redirect input (Rofi needs STDIN for -dmenu)
        qtile.spawn(
            f"/bin/sh -c \"rofi -dmenu -p Shortcuts -scrollbar-width 8 -lines 20 "
            f"-theme-str 'window {{ height: 39em; }} scrollbar {{ handle-width: 0; width: 0; }}' < {temp_path}\"")

def now_playing():
    try:
        out = subprocess.run(
            ["playerctl", "-p", "playerctld", "metadata",
             "--format", "<{{status}}: {{title}} — {{artist}}>"],
            capture_output=True, text=True, timeout=0.5
        )
        s = out.stdout.strip()
        return s if s and "No players found" not in s else "<>"
    except Exception:
        return "<X>"

keys_arr = [
    # Key List: https://docs.qtile.org/en/latest/manual/config/lazy.html

    # Switch between windows
    ([mod], "left", lazy.layout.left(), "Move focus to left"),
    ([mod], "right", lazy.layout.right(), "Move focus to right"),
    ([mod], "down", lazy.layout.down(), "Move focus down"),
    ([mod], "up", lazy.layout.up(), "Move focus up"),
    ([mod], "space", lazy.layout.next(), "Move window focus to other window"),
    # Move Windows
    ([mod, "shift"], "left", universal_move("left"), "Move window to the left"),
    ([mod, "shift"], "right", universal_move("right"), "Move window to the right"),
    ([mod, "shift"], "down", universal_move("down"), "Move window down"),
    ([mod, "shift"], "up", universal_move("up"), "Move window up"),
    # Sizing
    ([mod, "control"], "left", universal_resize("left"), "Grow window to the left"),
    ([mod, "control"], "right", universal_resize("right"), "Grow window to the right"),
    ([mod, "control"], "down", universal_resize("down"), "Grow window down"),
    ([mod, "control"], "up", universal_resize("up"), "Grow window up"),
    # Next Workspace
    ([mod, "mod1"], "right", lazy.screen.next_group(), "Move to next workspace"),
    ([mod, "mod1"], "left",  lazy.screen.prev_group(), "Move to prev workspace"),
    # Normalize
    ([mod], "n", lazy.layout.normalize(), "Reset all window sizes"),
    # Toggle between split and unsplit sid (original mod, shift, return)
    ([mod], "Tab", universal_focus, "Toggle between split and unsplit sides of stack"),
    ([mod], "Return", lazy.spawn(terminal), "Launch terminal"),
    # Custom Max Function
    ([mod], "f", switch_to_max, "Toggle max on the focused window"),
    # Switch Layouts
    ([mod], "grave", switch_to_monad_tall,   "Switch to Monad Tall"),
    ([mod], "c",     lazy.next_layout(),     "Cycle through layouts"),
    # Fullscreen/Floating
    ([mod, "shift"], "f", lazy.window.toggle_fullscreen(), "Toggle fullscreen on the focused window"),
    ([mod],          "t", lazy.window.toggle_floating(),   "Toggle floating on the focused window"),
    # Core
    ([mod],            "r", lazy.spawncmd(),    "Spawn a command using a prompt widget"),
    ([mod, "shift"],   "r", check_config,       "Check is config will break"),
    ([mod, "control"], "r", reload_with_backup, "Reload the config"),
    ([mod],            "q", lazy.window.kill(), "Kill focused window"),
    ([mod, "control"], "q", lazy.shutdown(),    "Shutdown Qtile"),
    # Custom
    ([mod], "p", lazy.spawn("rofi -show drun"), "powermenu"),
    ([mod], "l", lazy.spawn("betterlockscreen -l"), "lockscreen"),
    ([mod], "m", lazy.window.togroup("scratchpad", switch_group=False), "Minimize with scratchpad"),
    (
        [mod, "shift"], "m",
        lazy.function(lambda qtile: [
            w.togroup(qtile.current_group.name) for w in qtile.groups_map["scratchpad"].windows[-1:]
        ]),     
        "Unminimize last hidden window"
    ),
    ([mod], "period", lazy.next_screen(), "Focus lex screen"),
    ([mod], "comma", lazy.prev_screen(), "Focus previous screen"),
    ([mod], "i", lazy.layout.flip().when(layout=[layout_display_names["monadtall"]]), "Flips master and stack"),
    ([mod, "shift"], "x", lazy.spawn("xkill"), "Force Kill"),
    # Need to modify to display
    ([mod, "shift"], "n", lazy.spawn("bash -c 'info=$(xprop | grep -E \"WM_CLASS|WM_NAME\"); notify-send \"Window Info\" \"$info\"'"), "Get Name"),
    ([mod, "shift"], "Return", lazy.group["scratchpad"].dropdown_toggle("term"), "Drops down terminal"),
    ([mod, "shift"], "p", lazy.function(toggle_pip), "Toggle sticky PiP for focused window"),
    ([mod, "control"], "p", lazy.function(pip_toggle_visible), "PiP hide/show"),
    ([mod, "shift"], "s", lazy.function(pip_remember_pos), "PiP remember position/size"),
    # Media
    ([], "XF86AudioPlay",        lazy.spawn("playerctl play-pause"), "Play/Pause"),
    ([], "XF86AudioNext",        lazy.spawn("playerctl next"),       "Next"),
    ([], "XF86AudioPrev",        lazy.spawn("playerctl previous"),   "Previous"),
    ([], "XF86AudioStop",        lazy.spawn("playerctl stop"),       "Stop"),
    ([], "XF86AudioMute",        lazy.spawn("pactl set-sink-mute @DEFAULT_SINK@ toggle"), "Mute"),
    ([], "XF86AudioLowerVolume", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ -5%"), "Volume Down"),
    ([], "XF86AudioRaiseVolume", lazy.spawn("pactl set-sink-volume @DEFAULT_SINK@ +5%"), "Volume Up"),
    ([], "XF86MonBrightnessUp",  lazy.spawn("brightnessctl set +10%"), "Brightness Up"),
    ([], "XF86MonBrightnessDown",lazy.spawn("brightnessctl set 10%-"), "Brightness Down"),
    ([], "XF86Search",           lazy.spawn("rofi -show drun"),      "Search/Launcher"),
    ([], "XF86Calculator",       lazy.spawn("gnome-calculator"),     "Calculator"),
    ([], "XF86Mail",             lazy.spawn("thunderbird"),          "Mail"),
    ([], "XF86WWW",              lazy.spawn("firefox"),              "Web Browser"),
    ([], "XF86HomePage",         lazy.spawn("firefox"),              "Home Page"),
    ([], "XF86Favorites",        lazy.spawn("rofi -show drun"),      "Favorites Menu"),

]

keys = []
popup_lines = []

for mods, key, action, desc in keys_arr:
    keys.append(Key(mods, key, action, desc=desc))

    modifier_names = ["Mod" if k == mod else k.capitalize() for k in mods]
    key_combo = " + ".join(modifier_names + [key.capitalize()])
    popup_lines.append(f"{key_combo} = {desc}")

popup_text = "\n".join(popup_lines)

# Add key bindings to switch VTs in Wayland.
for vt in range(1, 8):
    keys.append(
        Key(
            ["control", "mod1"],
            f"f{vt}",
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == "wayland"),
            desc=f"Switch to VT{vt}",
        )
    )

# groups = [Group(i, label=i, persist=True, init=True) for i in "123456789"]
# Define your real groups
group_names = "123456789"
groups = [Group(name, label=name, persist=True, init=True) for name in group_names]

# Add scratchpad at the end
scratchpad = ScratchPad("scratchpad", [
        DropDown("term", "alacritty", width=0.6, height=0.6, x=0.2, y=0.2),
])
groups.append(scratchpad)

for name in group_names:
    keys.extend(
        [
            Key(
                [mod], name, lazy.group[name].toscreen(), 
                desc=f"Switch to group {name}",
            ),
            Key(
                [mod, "control"], name, lazy.window.togroup(name, switch_group=True),
                desc=f"Switch to & move focused window to group {name}",
            ),
            Key(
                [mod, "shift"], name, lazy.window.togroup(name),
                desc="move focused window to group {}".format(name),
            ),
        ]
    )

# Dracula Theme
colors = {
    "black": "#262934",
    "dark_gray": "#282a36",
    "gray":"#44475a",
    "white": "#f8f8f2",
    "lavender":"#6272a4", 
    "cyan": "#8be9fd",
    "green": "#50fa7b",
    "orange": "#ffb86c",
    "pink": "#ff79c6",
    "purple": "#bd93f9",
    "red": "#ff5555",
    "yellow": "#f1fa8c",
    "default_red": "#d75f5f",
    "default_black": "#000000"
}

# == Layout Themes ==
layout_theme = {
    "border_width": 2,
    "margin": 0,
    "border_focus": colors["lavender"],
    "border_normal": colors["gray"],
    "border_focus_stack": colors["red"],
    "font": "JetBrains Mono",
    "fontsize": 12,
    "padding": 3,
}

max_theme = {
    "name": layout_display_names["max"],
    "border_focus": colors["purple"],
    **layout_theme
}

tile_theme = {
    "ratio": 0.70,
    "masterWindows": 1,
    **layout_theme
}

tree_tab_theme = {
    "bg_color": colors["gray"],
    "inactive_bg": colors["lavender"],
    "active_bg": colors["purple"],
    "urgent_bg": colors["red"],
    "place_right": True,
    "margin_left": 2,
    "vspace": 2,
    "panel_width": 95,
    "sections": ["Main", "Misc"],
    "name": layout_display_names["treetab"]
}

floating_theme = {
    "float_rules": [
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
        # Match(title="SDL2 Image Sprite"),
        Match(title="Network Connections"),
        Match(title="Volume Control"),
    ],
    "border_normal": colors["gray"],
    "border_focus": colors["lavender"],
    "name": layout_display_names["floating"]
}

# == Widget Configs ==
group_box_config = {
    "highlight_method": "line",
    "active": colors["purple"],
    "inactive": colors["lavender"],
    "this_current_screen_border": colors["purple"],
    "this_screen_border": colors["lavender"],
    "other_current_screen_border": colors["purple"],
    "other_screen_border": colors["lavender"],
}

spacer_line_config = {
    "text": '|',
    "font": "Ubuntu Mono",
    "foreground": colors["lavender"],
    "padding": 2,
    "fontsize": 14
}

media_config = {
    "name":"nowplaying",
    "update_interval":1,
    "func":now_playing,
    "fmt":"{}",
    "mouse_callbacks":{
        "Button1": lambda: subprocess.run(["playerctl", "-p", "playerctld", "play-pause"]),
        "Button4": lambda: subprocess.run(["playerctl", "-p", "playerctld", "next"]),
        "Button5": lambda: subprocess.run(["playerctl", "-p", "playerctld", "previous"]),
    },
    # "width":300,
}

chord_config = {
    "chords_colors":{
        "launch": ("#6272A4", "#44475A"),
    },
    "name_transform": lambda name: name.upper(),
}

volume_config = {
    "fmt": 'Vol: {}',
    "mute_format": '<span foreground="red"> X </span>',
    "markup": True,
    "mute_command": 'pactl set-sink-mute @DEFAULT_SINK@ toggle',
    "volume_up_command": 'pactl set-sink-volume @DEFAULT_SINK@ +5%',
    "volume_down_command": 'pactl set-sink-volume @DEFAULT_SINK@ -5%',
    "mouse_callbacks": {"Button1": lambda: qtile.spawn("pavucontrol")}
}

network_config = {
    "update_interval": 5,
    "func": check_net,
    "mouse_callbacks": {
        "Button1": lambda: qtile.spawn("nm-connection-editor")
    }
}
# "format": lambda: f"{get_meminfo()[0]:.1f}/{get_meminfo()[1]:.1f} GiB",
#     "format": '{MemUsed:.1f}/{MemTotal:.1f} GiB',
memory_config = {
    "func": lambda: f"{get_meminfo()[0]:.1f}/{get_meminfo()[1]:.1f} GiB",
    "update_interval": 3,
    # "format": lambda: f"{get_meminfo()[0]:.1f}/{get_meminfo()[1]:.1f} GiB",
    # "measure_mem": 'G',
    # "update_interval": 1,
    "foreground": '#ffffff',
    "background": '#000000',
    "mouse_callbacks": {
        'Button1': lambda: qtile.cmd_spawn(f'{terminal} -e btop')
    }
}

exit_config = {
    "default_text" : "[ X ]",
    "countdown_format": "[ {} ]",
    "foreground": colors["red"]
}

clock_config = {
    "format": "%Y-%m-%d %a %H:%M",
    "mouse_callbacks": {
        "Button1": lambda: qtile.spawn("gsimplecal"),
    }
}

settings_config = {
    "text": "[?]",
    "mouse_callbacks": {
        # "Button1": lambda: qtile.spawn(f"rofi -e {shlex.quote(popup_text)}"),
        "Button1": lazy.function(show_rofi_popup),

        "Button2": lambda: qtile.spawn(f"{terminal} -e emacsclient -t {expanduser('~/.config/qtile/config.py')}"), 
        "Button3": lambda: qtile.spawn(f"{terminal} -e sh -c 'tail -n 50 -f ~/.local/share/qtile/qtile.log'")
    }
}

battery_config = {
    "format":'+{char} {percent:2.0%}',
    "charge_char":'^',
    "discharge_char":'',
    "empty_char":'X',
    "full_char":'[100%]',
    "unknown_char":' ? ',
    "low_percentage":0.15,
    "low_foreground":'#ff0000',
    "update_interval":10,
}

# Cyclable Layouts
layouts = [
    layout.Columns(**layout_theme, name=layout_display_names["columns"]),
    # layout.Max(**max_theme),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Matrix(**layout_theme),
    layout.MonadTall(**layout_theme, ratio=0.7, name=layout_display_names["monadtall"]),
    # layout.MonadTall(**layout_theme, ratio=0.7, align=layout.MonadTall._left),
    # Monad(**layout_theme),
    # layout.MonadWide(**layout_theme),
    # layout.RatioTile(**layout_theme),
    # layout.Tile(**tile_theme),
    layout.TreeTab(**tree_tab_theme),
    layout.Max(**max_theme),
    layout.Bsp(**layout_theme, name=layout_display_names["bsp"]),
    # layout.VerticalTile(**layout_theme),
    # layout.Zoomy(**layout_theme),
]

# Keybinding Only Layouts
floating_layout = layout.Floating(**floating_theme)


## WIDGETS
widget_defaults = dict(
    font="JetBrains Mono",
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

def create_bar_laptop():
    spacer_widget = widget.Spacer(length = 8)
    text_box_widget = (widget.TextBox, spacer_line_config)

    widget_dict = {
        "GroupBox": (widget.GroupBox, group_box_config), 
        "CurrentLayout": (widget.CurrentLayout, None), 
        "Prompt": (widget.Prompt, None), 
        "WindowName": (widget.WindowName, None), 
        "Chord": (widget.Chord, chord_config),
        "Media": (widget.GenPollText, media_config),
        "Systray": (widget.Systray, None), 
        "Volume": (widget.Volume, volume_config),
        "Battery": (widget.Battery, battery_config),
        "Network": (widget.GenPollText, network_config),
        "Memory": (widget.GenPollText, memory_config),
        "Clock": (widget.Clock, clock_config),
        "Settings": (widget.TextBox, settings_config),
        "QuickExit": (widget.QuickExit, exit_config)
    }

    widget_keys = widget_dict.keys()
    exclude_keys = {"Prompt", "WindowName", "Chord"}
    # Starting Spacer
    window_1 = [spacer_widget]

    for i, key in enumerate(widget_keys):
        widget_cls, config = widget_dict[key]

        window_1.append(widget_cls(**config) if config else widget_cls())

        # Separators
        if i < len(widget_keys) - 1 and key not in exclude_keys:
            window_1.append(text_box_widget[0](**text_box_widget[1]))

    # Ending spacer
    window_1.append(spacer_widget)

    return window_1, None

def create_bar():
    spacer_widget = widget.Spacer(length = 8)
    text_box_widget = (widget.TextBox, spacer_line_config)

    widget_dict = {
        "CurrentLayout": (widget.CurrentLayout, None), 
        "GroupBox": (widget.GroupBox, group_box_config), 
        "Prompt": (widget.Prompt, None), 
        "WindowName": (widget.WindowName, None), 
        "Chord": (widget.Chord, chord_config), 
        "Systray": (widget.Systray, None), 
        "Volume": (widget.Volume, volume_config), 
        "Clock": (widget.Clock, {"format": "%Y-%m-%d %a %H:%M"}), 
        "QuickExit": (widget.QuickExit, exit_config)
    }

    screen_2_replacements = {
        "Systray": (widget.TextBox, {"text": "Press &lt;M-r&gt; to spawn", "foreground": colors["lavender"]}),
        "Volume": (widget.GenPollText, network_config),
        "QuickExit": (widget.TextBox, settings_config)
    }

    widget_keys = widget_dict.keys()
    exclude_keys = {"Prompt", "WindowName", "Chord"}
    # Starting Spacers
    window_1 = [spacer_widget]
    window_2 = [spacer_widget]

    for i, key in enumerate(widget_keys):
        widget_cls, config = widget_dict[key]

        # Screen 1
        window_1.append(widget_cls(**config) if config else widget_cls())

        # Screen 2
        if key in screen_2_replacements:
            alt_widget_cls, alt_config = screen_2_replacements[key]
            window_2.append(alt_widget_cls(**alt_config))
        else:
            window_2.append(widget_cls(**config) if config else widget_cls())

        # Separators
        if i < len(widget_keys) - 1 and key not in exclude_keys:
            window_1.append(text_box_widget[0](**text_box_widget[1]))
            window_2.append(text_box_widget[0](**text_box_widget[1]))

    # Ending spacers
    window_1.append(spacer_widget)
    window_2.append(spacer_widget)

    return window_1, window_2

# def get_screen_count():
#     return int(os.environ.get("SCREENS", "1"))
# 
# count = get_screen_count()
# if count > 1:
#     bar1, bar2 = create_bar()
# else:
#     bar1, bar2 = create_bar_laptop()  # returns (bar1, None)
bar1, bar2 = create_bar_laptop()
screens = [Screen(top=bar.Bar(bar1, 24))]
# if bar2 is not None:
#     screens.append(Screen(top=bar.Bar(bar2, 24)))

# # SCREENS
# screens = [
#     Screen(top=bar.Bar(bar1, 24)),
#     Screen(top=bar.Bar(bar2, 24)),
# ]

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # Launches apps to specific groups
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False

auto_fullscreen = True
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None
# xcursor theme (string or None) and size (integer) for Wayland backend
wl_xcursor_theme = None
wl_xcursor_size = 24

# Not actual WM name. Nobody really uses or cares about this string besides java UI toolkits
# LG3D is a 3D non-reparenting WM written in java that happens to be on java's whitelist.
wmname = "LG3D"
