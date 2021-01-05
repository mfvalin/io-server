#!/bin/bash
set -x

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

FILE=${1:-UnknownFile.c}
FILE_NAME_ONLY=$(basename ${FILE})
FILE_BASE=${FILE_NAME_ONLY%.*}
GUARD="IO_SERVER_${FILE_BASE}_GEN_H"


echo "/*"
sed -e 's/^\(.*\)$/ * &/' < ${SCRIPT_DIR}/common_header.txt
echo " */"
echo -e "// This file has been generated from ${FILE_NAME_ONLY}"
echo -e "#ifndef ${GUARD}"
echo -e "#define ${GUARD}\n"
sed -n '/[/! ]*C_StArT$/,/[/! ]*C_EnD$/p' ${FILE}  | grep -v C_StArT | grep -v C_EnD | sed -e 's|^[[:space:]]*)[[:space:]]*$|);|' -e 's:^// !>::'
echo -e "\n#endif // ${GUARD}"

