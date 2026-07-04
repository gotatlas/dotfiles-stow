#!/usr/bin/env sh

rootfs="/"

disk="$(df -h "$rootfs" 2>/dev/null | awk 'NR==2 {print $4}')"

ram="$(free -m 2>/dev/null | awk '/^Mem:/ {printf "%d%%", ($3/$2)*100}')"

load="$(cut -d ' ' -f1 /proc/loadavg 2>/dev/null)"

temp="$(
    for f in /sys/class/thermal/thermal_zone*/temp /sys/class/hwmon/hwmon*/temp*_input; do
        [ -r "$f" ] || continue
        v="$(cat "$f" 2>/dev/null)"
        [ "$v" -gt 10000 ] 2>/dev/null || continue
        c=$((v / 1000))
        [ "$c" -gt 0 ] && [ "$c" -lt 120 ] && {
            printf "%s°C" "$c"
            break
        }
    done
)"

[ -n "$temp" ] || temp="--°C"

if ip route get 1.1.1.1 >/dev/null 2>&1; then
    net="on"
else
    net="off"
fi

bat="$(
    for b in /sys/class/power_supply/BAT*; do
        [ -r "$b/capacity" ] || continue
        cap="$(cat "$b/capacity" 2>/dev/null)"
        stat="$(cat "$b/status" 2>/dev/null)"
        case $stat in
            Charging) sym="+" ;;
            Discharging) sym="-" ;;
            Full) sym="=" ;;
            *) sym="" ;;
        esac
        printf "%s%s%%" "$sym" "$cap"
        break
    done
)"

[ -n "$bat" ] || bat="AC"

vol="$(
    wpctl get-volume @DEFAULT_AUDIO_SINK@ 2>/dev/null |
        awk '{
            v = int($2 * 100);
            if ($0 ~ /MUTED/) printf "mut";
            else printf "%d%%", v;
        }'
)"

[ -n "$vol" ] || vol="--"

mic="$(
    wpctl get-volume @DEFAULT_AUDIO_SOURCE@ 2>/dev/null |
        awk '{
            v = int($2 * 100);
            if ($0 ~ /MUTED/) printf "mut";
            else printf "%d%%", v;
        }'
)"

[ -n "$mic" ] || mic="--"

printf "| mic %s | vol %s | bat %s | net %s | tmp %s | cpu %s | ram %s | disk %s |\n" \
  "$mic" "$vol" "$bat" "$net" "$temp" "$load" "$ram" "$disk"
