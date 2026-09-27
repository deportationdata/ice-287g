#!/bin/bash
# Mirror every distinct version of every ina287.org page the Wayback Machine
# holds, then every document those pages link to.
#
# One file per (URL, content digest): an unchanged page captured many times
# is fetched once, and a page whose content changed is fetched once per
# version. Theme files, scripts, trackbacks and xmlrpc are skipped. Files are
# fetched with the id_ flag so they arrive as the site served them, without
# the Wayback toolbar or rewritten links.
#
# Output: inputs/other_sources/ina287/
#   site/<path>/<timestamp>.<ext>   every version of every page
#   docs/<host>/<path>/<timestamp>_<file>  every archived linked document
#   captures.tsv   the site captures fetched
#   links.tsv      every outbound link, with the pages and dates it appeared on
#   docs.tsv       each linked document and whether Wayback holds it

set -u
OUT="$(cd "$(dirname "$0")/.." && pwd)/ina287"
mkdir -p "$OUT/site" "$OUT/docs"
CDX="https://web.archive.org/cdx/search/cdx"

cdx() { # query string; retries when the CDX server throttles or errors
  for i in 1 2 3 4 5 6; do
    out=$(curl -sf --max-time 120 "$CDX?$1") && { printf '%s\n' "$out"; return 0; }
    sleep $((i * 15))
  done
  echo "CDX FAILED $1" >&2
}

fetch() { # url timestamp dest
  [ -s "$3" ] && return 0
  mkdir -p "$(dirname "$3")"
  for i in 1 2 3 4; do
    curl -sfL --max-time 120 "https://web.archive.org/web/${2}id_/${1}" -o "$3" && return 0
    sleep $((i * 5))
  done
  echo "FAILED $2 $1" >&2
  rm -f "$3"
}

# 1. Site captures, one per distinct digest.
cdx "url=ina287.org&matchType=domain&fl=original,mimetype,statuscode,timestamp,digest&collapse=digest" |
  grep -Ev 'wp-content/themes|wp-includes|trackback|xmlrpc|favicon' |
  awk '!seen[$1" "$5]++' > "$OUT/captures.tsv"

while read -r url mime status ts digest; do
  path=$(echo "$url" | sed -E 's#^https?://(www\.)?ina287\.org/?##; s#/$##; s#[?&=:]#_#g')
  [ -z "$path" ] && path=_home
  case "$mime" in
    text/html) ext=html ;; text/xml) ext=xml ;; text/plain) ext=txt ;;
    image/jpeg) ext=jpg ;; application/pdf) ext=pdf ;; *) ext=bin ;;
  esac
  fetch "$url" "$ts" "$OUT/site/$path/${ts}_${status}.$ext"
  sleep 1
done < "$OUT/captures.tsv"

# 2. Outbound links from every page version.
: > "$OUT/links.raw"
find "$OUT/site" -name '*.html' -o -name '*.xml' | while read -r f; do
  ts=$(basename "$f" | cut -d_ -f1)
  page=$(dirname "${f#$OUT/site/}")
  grep -oiE '(href|src)="[^"]+"' "$f" | sed -E 's/^[^"]*"//; s/"$//; s/&amp;/\&/g' |
    while read -r l; do printf '%s\t%s\t%s\n' "$l" "$page" "$ts"; done
done >> "$OUT/links.raw"
sort -u "$OUT/links.raw" |
  awk -F'\t' '{k=$1; first[k]=(k in first && first[k]<$3)?first[k]:$3; last[k]=(last[k]>$3)?last[k]:$3; pages[k]=pages[k] (index(pages[k],$2)?"":(pages[k]?"; ":"") $2)}
    END{for(k in first) print k "\t" first[k] "\t" last[k] "\t" pages[k]}' |
  sort > "$OUT/links.tsv"
rm "$OUT/links.raw"

# 3. Linked documents (PDF, Word, Excel, uploads) wherever they were hosted.
printf 'url\tstatus\tversions\n' > "$OUT/docs.tsv"
cut -f1 "$OUT/links.tsv" | grep -iE '\.(pdf|docx?|xlsx?|pptx?|txt|csv|zip)([?#]|$)|wp-content/uploads' |
  grep -Ev 'wp-content/themes' | sed -E 's#^/#http://ina287.org/#' | sort -u |
  while read -r u; do
    caps=$(cdx "url=$(printf %s "$u" | sed 's/ /%20/g')&fl=original,timestamp,statuscode,digest&collapse=digest&filter=statuscode:200")
    n=$(printf '%s' "$caps" | grep -c . || true)
    if [ "$n" -eq 0 ]; then printf '%s\tnot archived\t0\n' "$u" >> "$OUT/docs.tsv"; continue; fi
    printf '%s\tarchived\t%s\n' "$u" "$n" >> "$OUT/docs.tsv"
    printf '%s\n' "$caps" | while read -r orig ts st dg; do
      rel=$(echo "$orig" | sed -E 's#^https?://##; s#\?.*$##')
      fetch "$orig" "$ts" "$OUT/docs/$(dirname "$rel")/${ts}_$(basename "$rel")"
      sleep 1
    done
  done

# 4. Scribd documents. The site hosted nothing itself: its documents were
# Scribd uploads, which the uploader replaced in place (the "12-11-09" MOA
# list is now "5-8-10"). Keep every distinct archived copy of each doc page,
# under both the old /doc/ and current /document/ URL forms, plus the page as
# it stands today, and the text of each.
printf 'scribd_id\ttitle_linked\twayback_versions\tlive_url\n' > "$OUT/scribd.tsv"
cut -f1 "$OUT/links.tsv" | grep -oE 'scribd\.com/doc/[0-9]+[^ ]*' | sort -u |
  while read -r u; do
    id=$(echo "$u" | cut -d/ -f3)
    dir="$OUT/scribd/$id"
    caps=$(for form in doc document; do
        cdx "url=scribd.com/$form/$id&matchType=prefix&fl=original,timestamp,statuscode,digest&filter=statuscode:200"
      done | grep -E "/$id(/|$)" | awk '!seen[$4]++')
    n=$(printf '%s' "$caps" | grep -c . || true)
    printf '%s\n' "$caps" | while read -r orig ts st dg; do
      [ -n "$orig" ] && fetch "$orig" "$ts" "$dir/wayback_${ts}.html" && sleep 1
    done
    mkdir -p "$dir"
    live=$(curl -sL -o "$dir/live_$(date +%Y%m%d).html" -w '%{http_code} %{url_effective}' "https://www.scribd.com/document/$id")
    [ "${live%% *}" = 200 ] || rm -f "$dir/live_$(date +%Y%m%d).html"
    printf '%s\t%s\t%s\t%s\n' "$id" "$(echo "$u" | cut -d/ -f4)" "$n" "$live" >> "$OUT/scribd.tsv"
  done

# Plain text of every Scribd copy, for reading and diffing.
python3 - "$OUT/scribd" <<'PY'
import sys, re, html, pathlib
for f in pathlib.Path(sys.argv[1]).rglob('*.html'):
    t = f.read_text(errors='ignore')
    t = re.sub(r'(?s)<script.*?</script>|<style.*?</style>', '', t)
    t = re.sub(r'<(br|p|div|tr|li|h\d)[^>]*>', '\n', t)
    t = html.unescape(re.sub(r'<[^>]+>', ' ', t))
    t = '\n'.join(re.sub(r'[ \t]+', ' ', l).strip() for l in t.splitlines())
    f.with_suffix('.txt').write_text(re.sub(r'\n{3,}', '\n\n', t))
PY
