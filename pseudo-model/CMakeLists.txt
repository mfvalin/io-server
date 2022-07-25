cmake_minimum_required(VERSION 3.16)

set(PSEUDO_MODEL_LIB ioserver_pseudo_model)

# What to compile

set(PROJECT_F_FILES
   process_command.F90
)

set(PROJECT_INCLUDE_FILES
   process_command.inc
)

# The library itself
add_library(${PSEUDO_MODEL_LIB} STATIC ${PROJECT_C_FILES} ${PROJECT_F_FILES} ${PROJECT_INCLUDE_FILES})
target_link_libraries(${PSEUDO_MODEL_LIB} PUBLIC rmn)

# Where to put modules
get_target_property(LIBRARY_DIR ${PSEUDO_MODEL_LIB} BINARY_DIR)
set(MODULE_DIR ${LIBRARY_DIR}/include)
set_target_properties(${PSEUDO_MODEL_LIB} PROPERTIES Fortran_MODULE_DIRECTORY ${MODULE_DIR})
target_include_directories(${PSEUDO_MODEL_LIB} INTERFACE ${MODULE_DIR})

set_target_properties(${PSEUDO_MODEL_LIB} PROPERTIES VERSION ${PROJECT_VERSION})
