# Library CMake file

#include_directories(${CMAKE_SOURCE_DIR}/include)

set (HEADER_FILES_COMMON
	../include/ffss/ffss.h
	common.h
	transfer.h
	utils.h
)

set (HEADER_FILES_WINDOWS
)

set (HEADER_FILES_LINUX
)


set (SOURCE_FILES_COMMON
	client.c
	common.c
	master.c
	message.c
	server.c
	transfer.c
	utils.c
	filter.c
	qos.c
	driver.c
)

set (SOURCE_FILES_WINDOWS
)

set (SOURCE_FILES_LINUX
)

if(WIN32)
	set (SOURCE_FILES ${SOURCE_FILES_COMMON} ${SOURCE_FILES_WINDOWS})
	set (HEADER_FILES ${HEADER_FILES_COMMON} ${HEADER_FILES_WINDOWS})
else()
	set (SOURCE_FILES ${SOURCE_FILES_COMMON} ${SOURCE_FILES_LINUX})
	set (HEADER_FILES ${HEADER_FILES_COMMON} ${HEADER_FILES_LINUX})
endif()

add_library(ffss ${HEADER_FILES} ${SOURCE_FILES})

if(CMAKE_COMPILER_IS_GNUCC)
	set_property(TARGET ffss APPEND_STRING PROPERTY COMPILE_FLAGS -Wall)
endif()
if(MSVC)
	set_property(TARGET ffss APPEND_STRING PROPERTY COMPILE_FLAGS /WX)
endif()

