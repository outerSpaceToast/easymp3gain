# - Try to find Lazbuild
# Once done this will define
#
#  LAZBUILD_EXE - The FPC binary
#  LCL_VERSION  - Active version of the LCL
#
# Copyright (c) 2010, Matthias Klumpp <matthias@nlinux.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

if (LAZBUILD_EXE AND LCL_VERSION)
else()

set(lazbuild_tryexe lazbuild)

find_program(LAZBUILD_EXE ${lazbuild_tryexe})

message(STATUS "Check for Lazbuild: ${LAZBUILD_EXE}")

exec_program(${LAZBUILD_EXE}
	ARGS --version
	OUTPUT_VARIABLE lazbuild_out
)

string(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+" lcl_ver "${lazbuild_out}")

if (lcl_ver)
	string(REGEX REPLACE "([0-9]+)\\.[0-9]+\\.[0-9]+" "\\1" lcl_vers_major "${lcl_ver}")
	string(REGEX REPLACE "[0-9]+\\.([0-9]+)\\.[0-9]+" "\\1" lcl_vers_minor "${lcl_ver}")
	string(REGEX REPLACE "[0-9]+\\.[0-9]+\\.([0-9]+)" "\\1" lcl_vers_patch "${lcl_ver}")

	message(STATUS "LCL version is: ${lcl_vers_major}.${lcl_vers_minor}.${lcl_vers_patch}")
	math(EXPR LCL_VERSION "${lcl_vers_major}*10000 + ${lcl_vers_minor}*100 + ${lcl_vers_patch}")
else()
	message(FATAL_ERROR "Could not check LCL version (Lazbuild found?)")
endif()

mark_as_advanced(LAZBUILD_EXE LCL_VERSION LAZBUILD_FOUND)

endif()
