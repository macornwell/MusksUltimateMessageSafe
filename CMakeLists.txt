cmake_minimum_required(VERSION 3.5)
#project(MusksUltimateMessageSafe)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

set(CMAKE_CXX_STANDARD 11)

#set(SOURCE_FILES cpp/src/musks_ultimate_message_safe.cpp)
#set(CMAKE_EXE_LINKER_FLAGS "-static-libgcc -static-libstdc++")
#set(CMAKE_C_FLAGS "-lgmp")
#include_directories(dep/gmp-6.1.2)
#add_executable(my_ultimate_safe cpp/src/main.cpp ${SOURCE_FILES})
#target_link_libraries(my_ultimate_safe gmp z)

# Add Google Test
#add_subdirectory(cpp/dep/googletest)
#include_directories(cpp/dep/googletest/googletest/include)
#set(TEST_FILES "cpp/test/musks_ultimate_message_safe_tests.cpp")
#add_executable(run_tests cpp/test/tests.cpp ${SOURCE_FILES} ${TEST_FILES})
#target_link_libraries(run_tests gtest pthread gmp z)

project(MusksUltimateMessageSafe)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0 --coverage")

include_directories(cpp/src)

add_subdirectory(cpp/dep/googletest)
add_subdirectory(cpp/src)
add_subdirectory(cpp/test)

