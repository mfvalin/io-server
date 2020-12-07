
function(gen_test_target)
   set(options IS_ACTIVATED)
   set(oneValueArgs NAME)
   set(multiValueArgs INPUT_FILES)
   cmake_parse_arguments(GEN_TEST_TARGET "${options}"
                        "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

   # Param checking
   if(NOT DEFINED GEN_TEST_TARGET_INPUT_FILES)
      message(FATAL_ERROR "Missing INPUT_FILES, please add at least one")
   endif()

   if (NOT DEFINED GEN_TEST_TARGET_NAME)
      message(FATAL_ERROR "Your test needs a NAME")
   endif()

   list(LENGTH GEN_TEST_TARGET_INPUT_FILES NUMBER_OF_PARAM_IN_ARRAY)

   set(TARGET_NAME ${PROJECT_NAME}_${GEN_TEST_TARGET_NAME})
   add_executable(${TARGET_NAME} ${GEN_TEST_TARGET_INPUT_FILES})
   set_target_properties(${TARGET_NAME} PROPERTIES VERSION ${PROJECT_VERSION})
   target_link_libraries(${TARGET_NAME} io-server)
   target_include_directories(
     ${TARGET_NAME}
      PRIVATE
      "$<TARGET_PROPERTY:io-server,INCLUDE_DIRECTORIES>"
      "$<TARGET_PROPERTY:io-server,BINARY_DIR>"
   )
endfunction()
