cmake_minimum_required(VERSION 3.0.2)

project(ffss C CXX)

# Add skyutils flags
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DSU_ENABLE_ANSI_CODE -D_REENTRANT -DSU_USE_DL -DSU_USE_ARCH")

# Flags to add on Windows
if(WIN32)
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -D_CRT_SECURE_NO_DEPRECATE -D_CRT_SECURE_NO_WARNINGS -D_WINSOCK_DEPRECATED_NO_WARNINGS")
endif()

# Flags to add for DEBUG
set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -DDEBUG")

# Replace MSVCRT by using static libc on windows (release only)
string( REGEX REPLACE "/MD" "/MT" CMAKE_C_FLAGS_RELEASE ${CMAKE_C_FLAGS_RELEASE} )
string( REGEX REPLACE "/MD" "/MT" CMAKE_CXX_FLAGS_RELEASE ${CMAKE_CXX_FLAGS_RELEASE} )

# Temp: Disable compression right now
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DDISABLE_BZLIB -DDISABLE_ZLIB")


add_subdirectory(skyutils/src)
add_subdirectory(libffss)
add_subdirectory(appli)
