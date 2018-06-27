project(MusksUltimateMessageSafe)

set(SOURCE_FILES musks_ultimate_message_safe.cpp)

add_library(MusksUltimateMessageSafe ${SOURCE_FILES})
add_executable(open_safe main.cpp ${SOURCE_FILES})
target_link_libraries(open_safe z gmp)
