project(SafeTests)

set(SOURCE_FILES ../src/musks_ultimate_message_safe.cpp)


add_executable(run_tests musks_ultimate_message_safe_tests.cpp tests.cpp ${SOURCE_FILES})
target_link_libraries(run_tests gtest z gmp)
