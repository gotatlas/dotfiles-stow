#!/usr/bin/env bash
set -u

window_ids=()
window_entries=()
window_icons=()

while IFS=$'\t' read -r id app_id title workspace_id; do
	window_ids+=("$id")
	window_entries+=("W${workspace_id} | ${title}")
	window_icons+=("$app_id")
done < <(
	niri msg --json windows | 
		jq -r '
			sort_by(.workspace_id // 9999, .app_id // "unknown", .title // "untitled")
			| .[]
			| [
				(.id | tostring),
				(.app_id // "unknown"),
				(.title // "untitled"),
				(.workspace_id // "?")
			]
			| @tsv
		'
)

result="$(
	for i in "${!window_entries[@]}"; do
		printf '%s\0icon\x1f%s\n' "${window_entries[$i]}" "${window_icons[$i]}"
	done |
		fuzzel \
			--counter \
			--dmenu \
			--index \
			--prompt="Window: " \
			--width=100 \
			--lines=18
)"

if [[ "$result" =~ ^[0-9]+$ ]] && [[ -n "${window_ids[$result]:-}" ]]; then
	niri msg action focus-window --id "${window_ids[$result]}"
fi
