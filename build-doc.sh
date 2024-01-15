#!/usr/bin/env bash

pandoc --standalone \
       --from gfm+smart+yaml_metadata_block \
       --output doc/index.html \
       ReadMe.md

sed -i -e '15d' doc/index.html

cp ipfs-explorer.png doc/

ipfs add --recursive --pin=false doc
