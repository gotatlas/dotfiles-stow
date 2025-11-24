#!/usr/bin/env python3
import json, sys, time, shutil, os, subprocess, re, select
from datetime import datetime

INTERVAL = 1  # seconds

def fmt_bytes(n): return f"{n/1024/1024/1024:.1f}G"

def disk_avail(path):
    try:
        u = shutil.disk_usage(path)
        return fmt_bytes(u.free)
    except Exception:
        return "?"

def cpu_usage():
    try:
        import psutil
        return psutil.cpu_percent(interval=None)
    except Exception:
        return 0.0

def load_1min():
    try:
        return os.getloadavg()[0]
    except Exception:
        return 0.0

def mem_used_total():
    try:
        import psutil
        v = psutil.virtual_memory()
        return fmt_bytes(v.used), fmt_bytes(v.total)
    except Exception:
        return "?", "?"

def wifi_quality():
    try:
        with open("/proc/net/wireless") as f:
            lines = f.read().strip().splitlines()[2:]
        for ln in lines:
            parts = ln.split()
            if len(parts) >= 3:
                raw = parts[2].strip(".")
                val = float(raw)
                return round(max(0, min(100, (val/70.0)*100.0)))
    except Exception:
        pass
    return None

def power_profile_get():
    """Return current power profile: 'performance' | 'balanced' | 'power-saver' | None"""
    try:
        out = subprocess.check_output(["powerprofilesctl", "get"], text=True).strip()
        return out if out in ("performance", "balanced", "power-saver") else None
    except Exception:
        return None

def power_profile_cycle():
    """Cycle: balanced -> power-saver -> performance -> balanced"""
    cur = power_profile_get()
    nxt = "power-saver" if cur == "balanced" else ("performance" if cur == "power-saver" else "balanced")
    try:
        subprocess.check_call(["powerprofilesctl", "set", nxt])
        return nxt
    except Exception:
        return cur

def battery_percent():
    try:
        import psutil
        b = psutil.sensors_battery()
        return None if not b else int(round(b.percent))
    except Exception:
        return None

_VOL_RE = re.compile(r'(\d+)%')

def volume_status():
    """Return (muted: bool, percent: int|None)."""
    muted = False
    try:
        out = subprocess.check_output(
            ["pactl", "get-sink-mute", "@DEFAULT_SINK@"], text=True
        ).lower()
        muted = "yes" in out
    except Exception:
        pass
    pct = None
    try:
        out = subprocess.check_output(
            ["pactl", "get-sink-volume", "@DEFAULT_SINK@"], text=True
        )
        vals = [int(m.group(1)) for m in _VOL_RE.finditer(out)]
        if vals:
            pct = sum(vals) // len(vals)
    except Exception:
        pass
    return muted, pct

def mic_status():
    """Return (muted: bool, percent: int|None)."""
    # Find default source (mic)
    try:
        src = subprocess.check_output(["pactl","get-default-source"], text=True).strip()
    except Exception:
        src = None
    if not src:
        # fallback first non-monitor
        try:
            out = subprocess.check_output(["pactl","list","short","sources"], text=True)
            for ln in out.splitlines():
                cols = ln.split()
                if cols and "monitor" not in cols[1]:
                    src = cols[1]; break
        except Exception:
            src = None
    if not src:
        return True, None  # treat as muted/unavailable

    muted = True
    try:
        out = subprocess.check_output(["pactl","get-source-mute",src], text=True).lower()
        muted = "yes" in out
    except Exception:
        pass

    pct = None
    try:
        out = subprocess.check_output(["pactl","get-source-volume",src], text=True)
        vals = [int(m.group(1)) for m in _VOL_RE.finditer(out)]
        if vals:
            pct = sum(vals)//len(vals)
    except Exception:
        pass

    return muted, pct

def toggle_mic():
    subprocess.call(["pactl", "set-source-mute", "@DEFAULT_SOURCE@", "toggle"])

def toggle_sink_mute():
    subprocess.call(["pactl", "set-sink-mute", "@DEFAULT_SINK@", "toggle"])

def player_status():
    """Return (ok, status, artist, title). status in {'Playing','Paused','Stopped'}."""
    try:
        st = subprocess.check_output(["playerctl", "status"], text=True).strip()
    except Exception:
        return False, None, None, None
    if st not in ("Playing","Paused","Stopped"):
        return False, None, None, None
    artist = title = None
    try:
        artist = subprocess.check_output(["playerctl","metadata","artist"], text=True).strip()
    except Exception:
        pass
    try:
        title  = subprocess.check_output(["playerctl","metadata","title"], text=True).strip()
    except Exception:
        pass
    return True, st, artist, title

def player_cmd(cmd):
    # Return True if command likely succeeded
    try:
        subprocess.check_call(["playerctl", cmd])
        return True
    except Exception:
        return False

def blocks_now():
    blocks = []
    blocks.append({"name":"diskhome", "full_text": f" /home {disk_avail('/home')}"})
    blocks.append({"name":"cpu", "full_text": f" {cpu_usage():.0f}%"})
    blocks.append({"name":"load", "full_text": f" {load_1min():.2f}"})
    used, total = mem_used_total()
    blocks.append({"name":"mem", "full_text": f"{used}/{total}"})

    q = wifi_quality()
    blocks.append({"name":"wifi", "full_text": f" {q}%" if q is not None else " down"})

    bp = battery_percent()
    profile = power_profile_get()
    # small icon for power profile
    prof_icon = {"power-saver":"", "balanced":"", "performance":""}.get(profile, "")
    if bp is None:
        blocks.append({"name":"bat", "full_text": f" — {prof_icon}".rstrip()})
    else:
        blk = {"name":"bat", "full_text": f" {bp}% {prof_icon}".rstrip()}
        if bp <= 15:
            blk["color"] = "#ff5555"
        blocks.append(blk)

    muted, vol = volume_status()
    blocks.append({
        "name":"vol",
        "full_text": (" muted" if muted else f" {vol if vol is not None else '—'}%"),
        "color": "#f38ba8" if muted else None
    })

    mic_muted, mic_volpct = mic_status()
    blocks.append({
        "name":"mic",
        "full_text": ("" if mic_muted else f" {mic_volpct if mic_volpct is not None else '—'}%"),
        "color": "#f38ba8" if mic_muted else "#a6e3a1"
    })

    blocks.append({
        "name":"time",
        "full_text": datetime.now().strftime("%m/%d/%y %H:%M"),
        "separator": True
    })
    return blocks

def print_header():
    # request click events
    print(json.dumps({"version": 1, "click_events": True}))
    print("[")
    print("[],")

def click_events(evt):
    try:
        name = evt.get("name")
        btn  = evt.get("button")
        if name == "vol":
            if btn == 1:
                subprocess.call(["pactl","set-sink-mute","@DEFAULT_SINK@","toggle"])
            elif btn in (4,5):
                delta = "+5%" if btn == 4 else "-5%"
                subprocess.call(["pactl","set-sink-volume","@DEFAULT_SINK@", delta])
            elif btn == 2:
                subprocess.Popen(["pavucontrol"])
        elif name == "mic":
            if btn == 1:
                subprocess.call(["pactl","set-source-mute","@DEFAULT_SOURCE@","toggle"])
            elif btn in (4,5):
                delta = "+5%" if btn == 4 else "-5%"
                # best effort: change default source volume
                try:
                    src = subprocess.check_output(["pactl","get-default-source"], text=True).strip()
                except Exception:
                    src = None
                if src:
                    subprocess.call(["pactl","set-source-volume",src, delta])
            elif btn == 2:
                subprocess.Popen(["pavucontrol","--tab=4"])
        elif name == "bat":
            if btn == 1:
                power_profile_cycle()
            elif btn == 3:
                prof = power_profile_get() or "unknown"
                # requires 'notify-send' (libnotify)
                subprocess.Popen(["notify-send", "Power profile", prof])
        elif name == "wifi" and btn == 1:
            subprocess.Popen(["nm-connection-editor"])
        elif name == "mem" and btn == 1:
            subprocess.Popen(["alacritty","-e","btop"])
                
    except Exception:
        # swallow click errors; never block the status loop
        pass

def event_loop():
    last = 0.0
    while True:
        # poll stdin for click events
        r, _, _ = select.select([sys.stdin], [], [], 0)
        if r:
            line = sys.stdin.readline()
            if not line:
                break  # bar closed
            line = line.strip()
            if not line:
                pass
            else:
                # i3bar click events arrive as ",{json}"
                if line.startswith(","):
                    line = line[1:].lstrip()
                try:
                    evt = json.loads(line)
                    click_events(evt)
                    # after handling a click, refresh immediately
                    print(json.dumps(blocks_now()) + ",")
                    sys.stdout.flush()
                except Exception:
                    pass

        now = time.time()
        if now - last >= INTERVAL:
            last = now
            try:
                print(json.dumps(blocks_now()) + ",")
                sys.stdout.flush()
            except Exception:
                print(json.dumps([{"full_text":"status: error"}]) + ",")
                sys.stdout.flush()

        time.sleep(0.05)

def main():
    print_header()
    event_loop()

if __name__ == "__main__":
    main()
