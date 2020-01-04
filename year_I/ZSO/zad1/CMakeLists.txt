cmake_minimum_required(VERSION 3.1)
project(crossld C ASM)

macro (add_sources)
    file (RELATIVE_PATH _relPath "${PROJECT_SOURCE_DIR}" "${CMAKE_CURRENT_SOURCE_DIR}")
    foreach (_src ${ARGN})
        if (_relPath)
            list (APPEND SRCS "${_relPath}/${_src}")
        else()
            list (APPEND SRCS "${_src}")
        endif()
    endforeach()
    if (_relPath)
        # propagate SRCS to parent directory
        set (SRCS ${SRCS} PARENT_SCOPE)
    endif()
endmacro()

set(CMAKE_C_STANDARD 99)

add_sources(
        crossld.c crossld.h
        elf_loader.c elf_loader.h
        cross_call.c cross_call.h
        dynamic_load.c dynamic_load.h
        crossld_helper.c crossld_helper.h
        utils.h utils.c asm.s asm.h)

add_library(crossld SHARED ${SRCS})
