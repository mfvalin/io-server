
foreach(C_FILE ${C_TO_INC_FILES})
    get_filename_component(FILE_NAME ${C_FILE} NAME_WLE)
    set(INC_FILE "${CMAKE_CURRENT_SOURCE_DIR}/${FILE_NAME}.inc")
    add_custom_command(
        OUTPUT ${INC_FILE}
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/extract_fortran_interfaces.sh ${CMAKE_CURRENT_SOURCE_DIR}/${C_FILE} > ${INC_FILE}
        DEPENDS ${C_FILE})
    list(APPEND INC_FILES ${INC_FILE})
endforeach(C_FILE)

add_custom_target(
    ${PROJECT_NAME}_inc_files
    DEPENDS ${INC_FILES}
    )

add_dependencies(${PROJECT_NAME} ${PROJECT_NAME}_inc_files)


