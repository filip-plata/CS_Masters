cmake_minimum_required(VERSION 3.0)

set(CMAKE_SYSTEM_NAME Generic)
set(CMAKE_SYSTEM_PROCESSOR arm)

project(LibConfig C)

add_library(config STATIC config.c gpio.c)

target_compile_options(config PUBLIC -nostartfiles -Wl,--gc-sections -Wall -O2 -ffunction-sections -fdata-sections)
target_compile_definitions(config PUBLIC -DSTM32F411xE)
target_include_directories(config PRIVATE inc)

install(TARGETS config DESTINATION .)
