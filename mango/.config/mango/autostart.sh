#! /bin/bash

# waybar -c ~/.config/mango/config.jsonc -s ~/.config/mango/style.css >/dev/null 2>&1 &
# # swaybg -i ~/Pictures/base.png >/dev/null 2>&1 &

# swww init &
# swww img ~/Pictures/base.png &

# mako &

# cliphist store &

# gnome-keyring-daemon --start --components=secrets &

# /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1
#
##! /bin/bash

set +e

# obs
dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=wlroots

# notify
swaync &mangoctl list-windowsswaync &mangoctl list-windows


# night light
# wlsunset -T 3501 -t 3500 &

# wallpaper
swaybg -i ~/.config/mango/base.png &

# top bar
waybar -c ~/.config/mango/waybar/config -s ~/.config/mango/waybar/style.css &

# keep clipboard content
wl-clip-persist --clipboard regular --reconnect-tries 0 &

# clipboard content manager
wl-paste --type text --watch cliphist store &

# xwayland dpi scale
echo "Xft.dpi: 140" | xrdb -merge #dpi
gsettings set org.gnome.desktop.interface text-scaling-factor 1.4

# Permission authentication
/usr/lib/xfce-polkit/xfce-polkit &




# # cursor size
# cursor_size=24
# env=XCURSOR_SIZE,24

# # fcitx5 im
# env=GTK_IM_MODULE,fcitx
# env=QT_IM_MODULE,fcitx
# env=SDL_IM_MODULE,fcitx
# env=XMODIFIERS,@im=fcitx
# env=GLFW_IM_MODULE,ibus

# # scale factor about qt (herr is 1.4)
# env=QT_QPA_PLATFORMTHEME,qt5ct
# env=QT_AUTO_SCREEN_SCALE_FACTOR,1
# env=QT_QPA_PLATFORM,Wayland;xcb
# env=QT_WAYLAND_FORCE_DPI,140
