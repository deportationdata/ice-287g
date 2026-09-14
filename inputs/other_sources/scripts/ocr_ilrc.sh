#!/bin/bash
# Resumable OCR of the ILRC FOIA compilation. Pages 2-713 are scans with no text
# layer; page 1 is ILRC's own cover note and already has text.
# Re-run until it reports REMAINING 0. Safe to interrupt.
A="$(cd "$(dirname "$0")" && pwd)"
F=$A/sources/gapfill/ILRC_287g_FOIA_excerpts.pdf
O=$A/derived/ilrc_ocr; mkdir -p "$O" /tmp/ocrwork
COUNT=${1:-60}; JOBS=${2:-6}   # pages per invocation, not seconds
N=$(pdfinfo "$F" | awk '/^Pages/{print $2}')
todo=(); for p in $(seq 1 "$N"); do [ -s "$O/page_$(printf %04d $p).txt" ] || todo+=("$p"); done
todo=("${todo[@]:0:$COUNT}")
one() {
  p=$1; A=$2; F=$3; O=$4
  n=$(printf %04d "$p"); w=/tmp/ocrwork/pg$n
  pdftoppm -r 200 -png -f "$p" -l "$p" "$F" "$w" 2>/dev/null
  img=$(ls ${w}*.png 2>/dev/null | head -1); [ -z "$img" ] && return
  tesseract "$img" "${w}" --psm 6 -l eng 2>/dev/null
  [ -s "${w}.txt" ] && mv "${w}.txt" "$O/page_$n.txt"
  rm -f ${w}*.png
}
export -f one
printf '%s\n' "${todo[@]}" | xargs -P "$JOBS" -I{} bash -c 'one "$@"' _ {} "$A" "$F" "$O"
done_n=$(ls "$O"/page_*.txt 2>/dev/null | wc -l)
echo "pages OCR'd: $done_n / $N   REMAINING $((N-done_n))"
