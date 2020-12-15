#!/bin/bash
set -x

FILE=${1:-UnknownFile.c}
FILE_NAME_ONLY=$(basename ${FILE})
FILE_BASE=${FILE_NAME_ONLY%.*}

#echo interface
#sed -n '/[/!]*F_StArT$/,/[/!]*F_EnD$/p' ${1:-UnknownFile}  | grep -v '^[/!]*F_' | sed -e 's:^[/!]*::' -e s'/[ !]*InTf.*//'
echo "! This file has been generated from ${FILE_NAME_ONLY}"
sed -n '/F_StArT/,/F_EnD/p' ${1:-UnknownFile} | sed -e 's:^[/!]*[ ]*::' -e 's/.*F_EnD.*//' -e 's/.*F_StArT.*//'
#echo end interface

