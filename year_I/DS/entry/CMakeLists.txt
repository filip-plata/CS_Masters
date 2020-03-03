cmake_minimum_required(VERSION 3.5)
project(entry)

set(CMAKE_CXX_STANDARD 11)

find_package (Threads)

add_executable(client client.cpp common.h common.cpp)
add_executable(server server.cpp semaphore.cpp semaphore.h threadpool.cpp threadpool.h bigint.cpp bigint.h)

add_library(common STATIC common.cpp)

target_link_libraries (server ${CMAKE_THREAD_LIBS_INIT} common)
target_link_libraries (client common)
