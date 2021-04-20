#!/bin/bash
#set -x

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

cat << EOT
/******************************************************************************
         INSTRUCTIONS FOR PROPERLY GENERATING THE HEADER FROM A .C FILE
   --------------------------------------------------------------------------
 We use the '//C_StArT' and '//C_EnD' tags to indicate the beginning and end of
 extraction.
 To extract the entire function (for inline functions, for example), you must
 put the begin/end tags around the entire function code, and **MAKE SURE TO
 LEAVE A SPACE** between the closing parenthesis of the function header and the
 opening bracket of the function body. They have to be on the same line.

 For example:
     //C_StArT
     inline int my_function(type1 arg1, type2* arg2) {
         [function body]
     }
     //C_EnD

 or also:
     //C_StArT
     inline int my_function(type1 arg1, //!< Doxygen doc
                            type2* arg2 //!< More doc
       ) {
         [function body]
     }
     //C_EnD


 To extract the function interface only, you must put the begin/end tags around
 the header. The placement of the closing parenthesis/opening bracket does not
 matter, as long as they are not on the same line with a space between them

 For example:
     //C_StArT
     int my_function(type1 arg1, type2* arg2)
     //C_EnD
     {
         [function body]
     }

 or also:
     //C_StArT
     int my_function(type1 arg1, type2* arg2){
     //C_EnD
         [function body]
     }
 ******************************************************************************/

EOT

sed -n '/[/! ]*C_StArT$/,/[/! ]*C_EnD$/p' ${FILE}  | grep -v C_StArT | grep -v C_EnD | sed -e 's|^[[:space:]]*)[[:space:]]*$|);|' -e 's:^// !>::' -e 's/){$/);/'

echo -e "\n#endif // ${GUARD}"

