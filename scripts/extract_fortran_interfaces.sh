#!/bin/bash
set -x

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

FILE=${1:-UnknownFile.c}
FILE_NAME_ONLY=$(basename ${FILE})
FILE_BASE=${FILE_NAME_ONLY%.*}

sed -e 's/^\(.*\)$/! &/' < ${SCRIPT_DIR}/common_header.txt
echo -e "\n! This file has been generated from ${FILE_NAME_ONLY}"
sed -n '/F_StArT/,/F_EnD/p' ${1:-UnknownFile} | sed -e 's:^[/!]*[ ]*::' -e 's/.*F_EnD.*//' -e 's/.*F_StArT.*//' | grep -v '^$'

