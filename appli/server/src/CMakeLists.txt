# Server CMake file

if("${CMAKE_SYSTEM}" MATCHES "Linux")
	find_package(BISON)
	find_package(FLEX)
	BISON_TARGET(ServerParser parser.y ${CMAKE_CURRENT_BINARY_DIR}/parser.c)
	FLEX_TARGET(ServerScanner lexer.l ${CMAKE_CURRENT_BINARY_DIR}/lexer.c)
	ADD_FLEX_BISON_DEPENDENCY(ServerScanner ServerParser)
	include_directories(${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR})
endif()

set (HEADER_FILES_COMMON
	${CMAKE_SOURCE_DIR}/libffss/include/ffss/ffss.h
	confapi.h
	plugin.h
	server.h
)

set (HEADER_FILES_WINDOWS
)

set (HEADER_FILES_LINUX
)


set (SOURCE_FILES_COMMON
	conf.c
	confapi.c
	index.c
	plugin.c
	server.c
	transfer.c
)

set (SOURCE_FILES_WINDOWS
	arch_dep_win.c
)

set (SOURCE_FILES_LINUX
	arch_dep_unix.c
	${BISON_ServerParser_OUTPUTS}
	${FLEX_ServerScanner_OUTPUTS}
)

set(LINK_LIBRARIES skyutils ffss)
if(WIN32)
	set (SOURCE_FILES ${SOURCE_FILES_COMMON} ${SOURCE_FILES_WINDOWS})
	set (HEADER_FILES ${HEADER_FILES_COMMON} ${HEADER_FILES_WINDOWS})
	set( CMAKE_EXE_LINKER_FLAGS_DEBUG "${CMAKE_EXE_LINKER_FLAGS_DEBUG} /SUBSYSTEM:CONSOLE" )
	add_executable(FFSS_Server WIN32 ${HEADER_FILES} ${SOURCE_FILES})
	if(MSVC)
		set_property(TARGET FFSS_Server APPEND_STRING PROPERTY COMPILE_FLAGS /WX)
	endif()
	set(LINK_LIBRARIES ${LINK_LIBRARIES} ws2_32)
	target_link_libraries(FFSS_Server ${LINK_LIBRARIES})
else()
	set (SOURCE_FILES ${SOURCE_FILES_COMMON} ${SOURCE_FILES_LINUX})
	set (HEADER_FILES ${HEADER_FILES_COMMON} ${HEADER_FILES_LINUX})
	add_executable(ffssd ${HEADER_FILES} ${SOURCE_FILES})
	if(CMAKE_COMPILER_IS_GNUCC)
		set_property(TARGET ffssd APPEND_STRING PROPERTY COMPILE_FLAGS -Wall)
	endif()
	set(LINK_LIBRARIES ${LINK_LIBRARIES} pthread dl m)
	target_link_libraries(ffssd ${LINK_LIBRARIES})
endif()
