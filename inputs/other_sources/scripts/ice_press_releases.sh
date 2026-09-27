#!/bin/bash
# Search every ICE news release for 287(g), on ice.gov today and in the
# Wayback Machine, and keep the ones that mention it.
#
# Live: every /news/releases/ page in ice.gov's sitemap.
# Wayback: every release url the Wayback Machine holds under ICE's release
# paths since 2003 (pi/news/newsreleases, pi/nr, news/releases/YYMM and the
# current slugs), except slugs still live, one capture per url. Wayback
# files are fetched with the id_ flag so they arrive as ICE served them.
#
# Every release is fetched once into a cache outside the repo; rerunning
# the script only fetches what the cache lacks. Only matching releases are
# copied into the repo.
#
# Output: inputs/other_sources/ice_press_releases/
#   live/<slug>.html            live releases that mention 287(g)
#   wayback/<timestamp>_<path>  archived releases that mention 287(g)
#   scanned.tsv                 every release checked: source, timestamp, url, hit

set -u
OUT="$(cd "$(dirname "$0")/.." && pwd)/ice_press_releases"
CACHE="${CACHE:-$HOME/.cache/ice-press-releases}"
mkdir -p "$OUT/live" "$OUT/wayback" "$CACHE/live" "$CACHE/wayback"
UA="Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128 Safari/537.36"
CDX="https://web.archive.org/cdx/search/cdx"
PATTERN='287 ?\(g\)|287 ?g\b|287\(g\)'

get() { # url dest; waits longer after each failure, including 429s
  [ -s "$2" ] && return 0
  for i in 1 2 3 4 5 6; do
    code=$(curl -sL --max-time 120 -A "$UA" -o "$2.part" -w '%{http_code}' "$1")
    [ "$code" = 200 ] && [ -s "$2.part" ] && { mv "$2.part" "$2"; return 0; }
    rm -f "$2.part"
    [ "$code" = 404 ] && break
    echo "retry $i $code $1" >&2
    sleep $((i * i * 10))
  done
  echo "FAILED $code $1" >&2
  return 1
}

body() { # file; the release's own text: the <main> element when there is one, never
         # the menus, header or footer, whose 287(g) program links are on every page
  perl -0777 -ne 's/<(script|style|nav|header|footer)\b.*?<\/\1>//gis; $_ = $1 if /<main\b(.*)<\/main>/is;
                  s/<[^>]*>/ /g; s/&nbsp;|&#160;/ /g; s/&#40;/(/g; s/&#41;/)/g; s/\s+/ /g; print' "$1"
}

mentions() { body "$1" | grep -qiE "$PATTERN"; }

# ---- live releases -------------------------------------------------------
: > "$CACHE/live_urls.txt"
for i in $(seq 1 20); do
  curl -sf -A "$UA" "https://www.ice.gov/sitemap.xml?page=$i" | grep -oE '<loc>[^<]+' | sed 's/<loc>//' > "$CACHE/sitemap.part" || break
  [ -s "$CACHE/sitemap.part" ] || break
  grep '^https://www.ice.gov/news/releases/' "$CACHE/sitemap.part" >> "$CACHE/live_urls.txt"
done
sort -u -o "$CACHE/live_urls.txt" "$CACHE/live_urls.txt"
echo "live releases: $(wc -l < "$CACHE/live_urls.txt")" >&2

export -f get; export UA CACHE
sed 's|.*/news/releases/||' "$CACHE/live_urls.txt" |
  xargs -P 4 -n 1 bash -c 'get "https://www.ice.gov/news/releases/$1" "$CACHE/live/$1.html"; sleep 0.5' _

# ---- Wayback releases ----------------------------------------------------
if [ ! -s "$CACHE/wayback_cdx.txt" ]; then
  for prefix in ice.gov/pi/news/newsreleases/ ice.gov/pi/nr/ ice.gov/news/releases/; do
    for i in 1 2 3 4 5 6; do
      curl -sf --max-time 600 "$CDX?url=$prefix&matchType=prefix&collapse=urlkey&fl=timestamp,original&filter=statuscode:200&filter=mimetype:text/html" \
        >> "$CACHE/wayback_cdx.txt" && break
      sleep $((i * 60))
    done
  done
fi
# one capture per path; drop query strings, index pages and slugs still live
sed 's|.*/news/releases/||' "$CACHE/live_urls.txt" | sort -u > "$CACHE/live_slugs.txt"
awk '{ u = $2; sub(/[?#].*/, "", u); sub(/^https?:\/\/(www\.)?/, "", u); sub(/:80\//, "/", u)
       if (u ~ /\/$/ || u ~ /index\.html?$/) next
       if (!(tolower(u) in seen)) { seen[tolower(u)] = 1; print $1 "\t" u } }' "$CACHE/wayback_cdx.txt" |
  awk -F'\t' 'NR == FNR { live[$1] = 1; next }
              { slug = $2; sub(/.*\/news\/releases\//, "", slug) }
              !(slug in live)' "$CACHE/live_slugs.txt" - > "$CACHE/wayback_todo.tsv"
echo "wayback releases not live: $(wc -l < "$CACHE/wayback_todo.tsv")" >&2

while IFS=$'\t' read -r ts u; do
  f="$CACHE/wayback/${ts}_$(echo "$u" | sed 's|^ice.gov/||; s|/|_|g')"
  [ -s "$f" ] && continue
  get "https://web.archive.org/web/${ts}id_/http://www.$u" "$f"
  sleep 1
done < "$CACHE/wayback_todo.tsv"

# ---- keep the matches ----------------------------------------------------
printf 'source\ttimestamp\turl\thit\n' > "$OUT/scanned.tsv"
while read -r url; do
  slug="${url##*/news/releases/}"; f="$CACHE/live/$slug.html"
  [ -s "$f" ] || { printf 'live\t\t%s\tNA\n' "$url" >> "$OUT/scanned.tsv"; continue; }
  if mentions "$f"; then cp "$f" "$OUT/live/"; hit=1; else hit=0; fi
  printf 'live\t\t%s\t%s\n' "$url" "$hit" >> "$OUT/scanned.tsv"
done < "$CACHE/live_urls.txt"
while IFS=$'\t' read -r ts u; do
  name="${ts}_$(echo "$u" | sed 's|^ice.gov/||; s|/|_|g')"; f="$CACHE/wayback/$name"
  [ -s "$f" ] || { printf 'wayback\t%s\thttp://www.%s\tNA\n' "$ts" "$u" >> "$OUT/scanned.tsv"; continue; }
  if mentions "$f"; then cp "$f" "$OUT/wayback/"; hit=1; else hit=0; fi
  printf 'wayback\t%s\thttp://www.%s\t%s\n' "$ts" "$u" "$hit" >> "$OUT/scanned.tsv"
done < "$CACHE/wayback_todo.tsv"
awk -F'\t' 'NR > 1 { n[$1]++; h[$1] += ($4 == 1); na[$1] += ($4 == "NA") }
            END { for (s in n) printf "%s: %d scanned, %d mention 287(g), %d not fetched\n", s, n[s], h[s], na[s] }' \
  "$OUT/scanned.tsv" >&2
