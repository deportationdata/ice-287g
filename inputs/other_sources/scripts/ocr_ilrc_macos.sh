#!/bin/bash
# ---------------------------------------------------------------------------
# OCR the ILRC FOIA compilation — run this natively on macOS, not through the
# Claude bridge. The bridge's Linux VM only sees 4 cores; this box has ~18, so
# native is roughly 4-5x faster (a couple of minutes rather than ~20).
#
#   brew install tesseract poppler        # if not already present
#   cd ~/projects/287g_archive && ./ocr_ilrc_macos.sh
#
# Resumable and safe to interrupt: pages already done are skipped.
# Output: derived/ilrc_ocr/page_NNNN.txt  +  derived/ilrc_ocr_all.txt
# ---------------------------------------------------------------------------
set -u
A="$(cd "$(dirname "$0")" && pwd)"
F="$A/sources/gapfill/ILRC_287g_FOIA_excerpts.pdf"
O="$A/derived/ilrc_ocr"; W="${TMPDIR:-/tmp}/ilrc_work"
mkdir -p "$O" "$W"

command -v tesseract >/dev/null || { echo "need: brew install tesseract"; exit 1; }
command -v pdftoppm  >/dev/null || { echo "need: brew install poppler";   exit 1; }
[ -f "$F" ] || { echo "missing $F"; exit 1; }

N=$(pdfinfo "$F" | awk '/^Pages/{print $2}')
J=$(sysctl -n hw.ncpu)
echo "$N pages, $J cores"

# Work in blocks: render a block with parallel pdftoppm (it is single threaded,
# so split the range), then OCR the block with one single threaded tesseract per
# core. PNGs are deleted per block to keep the working set small.
BLOCK=$((J * 12))
for ((s=1; s<=N; s+=BLOCK)); do
  e=$((s+BLOCK-1)); ((e>N)) && e=$N
  need=0; for ((p=s; p<=e; p++)); do [ -s "$O/page_$(printf %04d $p).txt" ] || need=1; done
  ((need==0)) && { echo "block $s-$e already done"; continue; }
  rm -f "$W"/s-*.png
  q=$(( (e-s+1+J-1)/J ))
  for ((k=0; k<J; k++)); do
    a=$((s+k*q)); b=$((a+q-1)); ((b>e)) && b=$e; ((a>e)) && break
    pdftoppm -r 300 -png -f $a -l $b "$F" "$W/s" 2>/dev/null &
  done
  wait
  ls "$W"/s-*.png 2>/dev/null | OMP_THREAD_LIMIT=1 xargs -P "$J" -I{} bash -c '
    f="$1"; O="$2"; n=$(basename "$f" .png); n=${n#s-}
    [ -s "$O/page_$n.txt" ] || tesseract "$f" "$O/page_$n" --psm 6 -l eng 2>/dev/null' _ {} "$O"
  rm -f "$W"/s-*.png
  echo "block $s-$e done ($(ls "$O"/page_*.txt | wc -l | tr -d ' ')/$N)"
done

cat "$O"/page_*.txt > "$A/derived/ilrc_ocr_all.txt"
echo "wrote derived/ilrc_ocr_all.txt ($(wc -c < "$A/derived/ilrc_ocr_all.txt") bytes)"

echo
echo "=== pages that look like 287(g) rosters ==="
grep -lEi "jail enforcement|task force officer|287\(g\) (agreements|participating)" "$O"/page_*.txt 2>/dev/null |
  while read -r f; do
    hits=$(grep -ciE "sheriff|police department|department of correction" "$f")
    [ "$hits" -ge 5 ] && echo "  $(basename "$f")  ($hits agency-like lines)"
  done
