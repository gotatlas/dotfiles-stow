#!/usr/bin/env python3
import json
import subprocess
import sys

try:
    raw = subprocess.check_output(
        ["pw-dump"],
        text=True,
        stderr=subprocess.DEVNULL,
    )
    data = json.loads(raw)
except Exception:
    print("")
    sys.exit(0)

mic = False
cam = False
screen = False
audio_out = False

def propstr(props, key):
    val = props.get(key, "")
    return str(val) if val is not None else ""

for obj in data:
    info = obj.get("info") or {}
    props = info.get("props") or {}

    media_class = propstr(props, "media.class")
    stream_monitor = propstr(props, "stream.monitor").lower()

    if stream_monitor in ("true", "1", "yes"):
        continue

    name_blob = " ".join([
        propstr(props, "application.name"),
        propstr(props, "application.process.binary"),
        propstr(props, "node.name"),
        propstr(props, "node.description"),
        propstr(props, "media.name"),
        propstr(props, "media.role"),
    ]).lower()

    if media_class == "Stream/Input/Audio":
        mic = True
    elif media_class == "Stream/Output/Audio":
        audio_out = True
    elif media_class == "Stream/Input/Video":
        if (
            "screen" in name_blob
            or "screencast" in name_blob
            or "webrtc-consume-stream" in name_blob
            or "xdg-desktop-portal" in name_blob
            or "portal" in name_blob
            or "capture" in name_blob
        ):
            screen = True
        else:
            cam = True

items = []

if screen:
    items.append("SCR")
if cam:
    items.append("CAM")
if mic:
    items.append("MIC")

print(" ".join(items))
