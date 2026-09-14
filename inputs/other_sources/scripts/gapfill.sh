#!/bin/bash
A="$(cd "$(dirname "$0")" && pwd)"; D=$A/sources/gapfill; BUDGET=${1:-36}
UA='Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15'
T=$A/logs/gapfill_targets.tsv; L=$A/logs/gapfill_results.tsv
START=$SECONDS
while IFS=$'\t' read -r name url ref; do
  [ -z "$name" ] && continue
  d="$D/$name"
  [ -s "$d" ] && continue
  [ $((SECONDS-START)) -ge "$BUDGET" ] && break
  curl -sSL --max-time 90 -A "$UA" \
    -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
    -H 'Accept-Language: en-US,en;q=0.9' ${ref:+-H "Referer: $ref"} \
    -o "$d.part" -w '%{http_code}\t%{size_download}\t%{content_type}\n' "$url" > /tmp/w.txt 2>/dev/null
  read -r code size ctype < /tmp/w.txt
  magic=$(head -c4 "$d.part" 2>/dev/null)
  if [ "$code" = "200" ] && [ "${size:-0}" -gt 3000 ]; then mv "$d.part" "$d"; st=OK; else rm -f "$d.part"; st=FAIL; fi
  printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$st" "$name" "$code" "$size" "$ctype" "$magic" >> "$L"
  printf '%-5s %-42s %s %s\n' "$st" "$name" "$code" "$size"
  sleep 1
done < "$T"
