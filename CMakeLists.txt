# CMakeLists for easyMp3Gain

project(easymp3gain)

cmake_minimum_required(VERSION 2.8.0 FATAL_ERROR)

#UNSET THIS VARIABLE AT RELEASE TIME
set(VERSION_SUFFIX "-dev")

set(PROJECT_NAME  "easymp3gain")
set(EMP3GAIN_VERSION_MAJOR  "0")
set(EMP3GAIN_VERSION_MINOR  "5")
set(EMP3GAIN_VERSION_PATCH  "0")
set(EMP3GAIN_VERSION "${EMP3GAIN_VERSION_MAJOR}.${EMP3GAIN_VERSION_MINOR}.${EMP3GAIN_VERSION_PATCH}")

# Forbid in-tree building & force "build" directory name
if (${CMAKE_SOURCE_DIR} MATCHES ${CMAKE_BINARY_DIR})
      message(STATUS "Please do an out-of-tree build:")
      message(STATUS "rm -f CMakeCache.txt && mkdir build && cd build; cmake .. && make")
      message(FATAL_ERROR "In-tree-build detected!")
endif (${CMAKE_SOURCE_DIR} MATCHES ${CMAKE_BINARY_DIR})
if (NOT ${CMAKE_SOURCE_DIR}/build MATCHES ${CMAKE_BINARY_DIR})
      message("Please build only in 'build' subfolder!")
      message(FATAL_ERROR "Not in /build!")
endif ()

#
# Options
#
########################################
option (GTK2 "Build GTK+2 user interface" ON)
option (QT4 "Build Qt4 user interface" OFF)
option (I18N "Enable locale" OFF)
########################################

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX
    "/usr" CACHE PATH "Default install prefix" FORCE)
endif(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

set(XDG_APPS_INSTALL_DIR share/applications)

add_definitions(-DCMAKE_INSTALL_PREFIX="\\\"${CMAKE_INSTALL_PREFIX}\\\"")
add_definitions(-DCMAKE_INSTALL_LIBDIR="\\\"${CMAKE_INSTALL_LIBDIR}\\\"")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/data/cmake/)
include(${CMAKE_SOURCE_DIR}/data/cmake/lcl_build_gui.cmake)

message(STATUS "Compiling easyMp3Gain version ${EMP3GAIN_VERSION}")

#
# l18n
#
find_package (Gettext REQUIRED)
if (${GETTEXT_FOUND} STREQUAL "TRUE" AND (I18N))
	add_custom_command (OUTPUT ${CMAKE_SOURCE_DIR}/po/${PROJECT_NAME}.pot
			COMMAND xgettext -i -c: -s --files-from ${CMAKE_SOURCE_DIR}/po/POTFILES.in -k_
				-o ${CMAKE_SOURCE_DIR}/po/${PROJECT_NAME}.pot
				--package-name=${PROJECT_NAME}
				--package-version="${LISTALLER_VERSION}"
				--no-wrap --no-location
			WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
	)
	add_custom_target(
	update_pot
	DEPENDS ${CMAKE_SOURCE_DIR}/po/${PROJECT_NAME}.pot
	)
        file (GLOB _translations ${CMAKE_SOURCE_DIR}/po/*.po)
        gettext_create_translations (${CMAKE_SOURCE_DIR}/po/${PROJECT_NAME}.pot
				     ALL ${_translations}
	)
endif (${GETTEXT_FOUND} STREQUAL "TRUE" AND (I18N))

#
# Validate FPC version
#
find_package(PkgConfig REQUIRED)
find_package(FPC REQUIRED)

add_subdirectory(src)
add_subdirectory(data)
