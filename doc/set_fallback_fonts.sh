#!/usr/bin/env bash

if [ $# -lt 1 ]; then
    echo "Need to specify a file"
    exit
fi

SVG_FILE="${1}"

sed -i ${SVG_FILE} \
    -e 's/font-family:'"'"'Liberation Sans'"'"';/font-family:'"'"'Liberation Sans'"'"',Helvetica,Arial,sans-serif;/g' \
    -e 's/-inkscape-font-specification:'"'"'Liberation Sans, Normal'"'"';/-inkscape-font-specification:'"'"'Liberation Sans, Normal'"'"','"'"'Helvetica, Normal'"'"','"'"'Arial, Normal'"'"','"'"'sans-serif, Normal'"'"';/g'


