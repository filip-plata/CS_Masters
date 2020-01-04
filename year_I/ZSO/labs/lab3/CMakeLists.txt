cmake_minimum_required(VERSION 3.9)
project(lab3 C)

set(CMAKE_C_STANDARD 11)

include_directories(.)

add_executable(lab3
        main.c
        make_formatter.c
        make_formatter.h)
