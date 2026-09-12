#!/bin/bash
A="$(cd "$(dirname "$0")" && pwd)"; BUDGET=${1:-36}
UA='Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15'
WL=$A/logs/worklist_287gMOA.tsv; START=$SECONDS; got=0
while IFS=$'\t' read -r u d; do
  [ -z "$u" ] && continue; [ -s "$d" ] && continue
  [ $((SECONDS-START)) -ge "$BUDGET" ] && break
  curl -sSL --max-time 60 -A "$UA" -o "$d.part" "$u"; rc=$?
  n=$(stat -c%s "$d.part" 2>/dev/null || echo 0); m=$(head -c4 "$d.part" 2>/dev/null)
  if [ $rc -eq 0 ] && [ "$n" -ge 3000 ] && [ "$m" = "%PDF" ]; then mv "$d.part" "$d"; got=$((got+1));
  elif [ $rc -eq 7 ]; then limited=1; break; fi
  sleep 1
done < "$WL"
REM=$(while IFS=$'\t' read -r u d; do [ -z "$u" ] && continue; [ -s "$d" ] || echo x; done < "$WL" | grep -c .)
echo "got $got  rate_limited=${limited:-0}  REMAINING $REM"
