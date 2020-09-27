#!/bin/bash

# Uploads one or more source files to a code.world installation
# Also remembers their hashes

set -e

url=https://code.world/
mode=haskell
for file in "$@"
do
    echo -n "Uploading $file... "
    perl -0777 -n -p -e 's/\n$//; s/\n/\r\n/g' "$file" > "$file.dos"
    json="$(curl -S -s -F "mode=$mode" -F "source=<$file.dos" ${url}compile)"
    rm -f "$file.dos"
    echo "$json" | jq -j .hash > "$file.hash"
    echo "$json" | jq -j .dhash > "$file.dhash"
    echo "${url}$mode#$(cat "$file.hash")"
    ! curl -S -s -F "mode=$mode" -F "hash=<$file.hash" ${url}runMsg | grep .
done

