#!/bin/bash
set -x
sed -n '/[/!]*C_StArT$/,/[/!]*C_EnD$/p' ${1:-UnknownFile}  | grep -v C_StArT | grep -v C_EnD | sed -e 's:){:);:' -e 's:^// !>::'

