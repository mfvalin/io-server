cmake_minimum_required(VERSION 3.16)

set(PSEUDO_MODEL_LIB ioserver_pseudo_model)

# What to compile
file(GLOB PROJECT_C_FILES *.c)
file(GLOB PROJECT_F_FILES *.F90 *.F *.f *.f90)
file(GLOB PROJECT_INCLUDE_FILES *.h *.inc *.hf)

# The library itself
add_library(${PSEUDO_MODEL_LIB} STATIC ${PROJECT_C_FILES} ${PROJECT_F_FILES} ${PROJECT_INCLUDE_FILES})
# target_include_directories(${PSEUDO_MODEL_LIB} PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
# target_include_directories(${PSEUDO_MODEL_LIB} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(${PSEUDO_MODEL_LIB} PRIVATE ioserver_pseudo_rmn)

# Where to put modules
get_target_property(LIBRARY_DIR ${PSEUDO_MODEL_LIB} BINARY_DIR)
set(MODULE_DIR ${LIBRARY_DIR}/include)
set_target_properties(${PSEUDO_MODEL_LIB} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIR})
target_include_directories(${PSEUDO_MODEL_LIB} INTERFACE ${MODULE_DIR})

set_target_properties(${PSEUDO_MODEL_LIB} PROPERTIES VERSION ${PROJECT_VERSION})
# target_compile_definitions(${PSEUDO_MODEL_LIB} PRIVATE _POSIX_C_SOURCE=199309L)

target_compile_options(
   ${PSEUDO_MODEL_LIB}
   PRIVATE
   $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wall -Wextra>
   $<$<COMPILE_LANG_AND_ID:C,GNU>:-Werror -Wno-error=pedantic -Wno-error=unused-function>
   $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Wall -Wextra -fmax-errors=8>
   $<$<COMPILE_LANG_AND_ID:Fortran,GNU>:-Werror -Wno-error=unused-function -Wno-error=maybe-uninitialized -Wno-error=unused-variable -Wno-error=unused-dummy-argument>
   $<$<COMPILE_LANG_AND_ID:C,Intel>:-Wall -Werror>
   $<$<COMPILE_LANG_AND_ID:Fortran,Intel>:-error-limit 8 -warn all -diag-disable 5268,7025,7416>
   # -g -O0
)
