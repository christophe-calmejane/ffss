# Master CMake file

set (HEADER_FILES_COMMON
	${CMAKE_SOURCE_DIR}/libffss/include/ffss/ffss.h
	index.h
	master.h
)

set (HEADER_FILES_WINDOWS
)

set (HEADER_FILES_LINUX
)


set (SOURCE_FILES_COMMON
	build.c
	config.c
	hosts.c
	index.c
	master.c
	search.c
	threads.c
)

set (SOURCE_FILES_WINDOWS
)

set (SOURCE_FILES_LINUX
)

set(LINK_LIBRARIES skyutils ffss)
if(WIN32)
	set (SOURCE_FILES ${SOURCE_FILES_COMMON} ${SOURCE_FILES_WINDOWS})
	set (HEADER_FILES ${HEADER_FILES_COMMON} ${HEADER_FILES_WINDOWS})
	add_executable(FFSS_Master WIN32 ${HEADER_FILES} ${SOURCE_FILES})
	add_executable(FFSS_Master_Console ${HEADER_FILES} ${SOURCE_FILES})
	set_property(TARGET FFSS_Master_Console APPEND_STRING PROPERTY COMPILE_FLAGS -D_CONSOLE)
	if(MSVC)
		set_property(TARGET FFSS_Master APPEND_STRING PROPERTY COMPILE_FLAGS /WX)
	endif()
	set(LINK_LIBRARIES ${LINK_LIBRARIES} ws2_32)
	target_link_libraries(FFSS_Master ${LINK_LIBRARIES})
	target_link_libraries(FFSS_Master_Console ${LINK_LIBRARIES})
else()
	set (SOURCE_FILES ${SOURCE_FILES_COMMON} ${SOURCE_FILES_LINUX})
	set (HEADER_FILES ${HEADER_FILES_COMMON} ${HEADER_FILES_LINUX})
	add_executable(FFSS_Master ${HEADER_FILES} ${SOURCE_FILES})
	if(CMAKE_COMPILER_IS_GNUCC)
		set_property(TARGET FFSS_Master APPEND_STRING PROPERTY COMPILE_FLAGS -Wall)
	endif()
	set(LINK_LIBRARIES ${LINK_LIBRARIES} pthread dl m)
	target_link_libraries(FFSS_Master ${LINK_LIBRARIES})
endif()
