cmake_minimum_required(VERSION 3.7)
project(MusksUltimateMessageSafe)

set(CMAKE_CXX_STANDARD 11)

set(SOURCE_FILES cpp/src/musks_ultimate_message_safe.cpp)
set(CMAKE_EXE_LINKER_FLAGS "-static-libgcc -static-libstdc++")
set(CMAKE_C_FLAGS "-lgmp")
include_directories(dep/gmp-6.1.2)
add_executable(my_ultimate_safe cpp/src/main.cpp ${SOURCE_FILES})
target_link_libraries(my_ultimate_safe gmp z)

# Add Google Test
add_subdirectory(cpp/dep/googletest)
include_directories(cpp/dep/googletest/googletest/include)
set(TEST_FILES "cpp/test/musks_ultimate_message_safe_tests.cpp")
add_executable(run_tests cpp/test/tests.cpp ${SOURCE_FILES} ${TEST_FILES})
target_link_libraries(run_tests gtest pthread gmp z)



