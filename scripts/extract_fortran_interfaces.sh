#!/bin/bash
set -x
#echo interface
#sed -n '/[/!]*F_StArT$/,/[/!]*F_EnD$/p' ${1:-UnknownFile}  | grep -v '^[/!]*F_' | sed -e 's:^[/!]*::' -e s'/[ !]*InTf.*//'
sed -n '/F_StArT/,/F_EnD/p' ${1:-UnknownFile} | sed -e 's:^[/!]*[ ]*::' -e 's/.*F_EnD.*//' -e 's/.*F_StArT.*//'
#echo end interface

