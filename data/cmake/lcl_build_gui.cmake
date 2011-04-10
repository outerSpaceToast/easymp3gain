# - Macro to build Pascal LCL GUI projects
#
# Copyright (c) 2010, Matthias Klumpp, <matthias@nlinux.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

macro (lcl_set_build_project project_bin widget)
  if(${widget} MATCHES qt)
    set(lcgui_build_args_gtk "-B" "-r" "--ws=qt")
    set(widget_f qt4)
  else()
    if(${widget} MATCHES gtk)
      set(lcgui_build_args_gtk "-B" "-r" "--ws=gtk2")
      set(widget_f gtk2)
    else()
      message(FATAL_ERROR "Invalid widgetset passed!")
    endif()
  endif()

  set(${project_bin}_${widget}_OUT "${CMAKE_BINARY_DIR}/${widget_f}/${project_bin}")
  add_custom_command(OUTPUT ${${project_bin}_${widget}_OUT}
	  COMMAND ${LAZBUILD_EXE}
	  ARGS ${lcgui_build_args_gtk} "${project_bin}.lpr"

	  COMMAND ${CMAKE_COMMAND}
	  ARGS -E rename ${CMAKE_BINARY_DIR}/${project_bin} ${CMAKE_BINARY_DIR}/${widget_f}/${project_bin}
	  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}

	  MAIN_DEPENDENCY "${CMAKE_CURRENT_SOURCE_DIR}/${project_bin}.lpr"
	  DEPENDS ${${project_bin}_base_sources}
  )
endmacro (lcl_set_build_project)
