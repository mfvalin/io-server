
foreach(C_FILE ${C_TO_H_FILES})
    get_filename_component(FILE_NAME ${C_FILE} NAME_WLE)
    set(H_FILE "${CMAKE_CURRENT_SOURCE_DIR}/${FILE_NAME}.h")
    add_custom_command(
        OUTPUT ${H_FILE}
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/extract_c_interfaces.sh ${CMAKE_CURRENT_SOURCE_DIR}/${C_FILE} > ${H_FILE}
        DEPENDS ${C_FILE})
    list(APPEND H_FILES ${H_FILE})
endforeach(C_FILE)

add_custom_target(
    ${PROJECT_NAME}_h_files
    DEPENDS ${H_FILES}
    )

add_dependencies(${PROJECT_NAME} ${PROJECT_NAME}_h_files)


