# CMakeLists.txt for easyMp3Gain GUI

find_package(Lazbuild REQUIRED)

if(LCL_VERSION LESS "930")
	message(FATAL_ERROR "LCL should be at least version 0.9.30!")
endif()

set(emp3gain_project easymp3gain)

set(emp3gain_base_sources
	${emp3gain_project}.lpr
)

##########> Build GTK+ UI <##########
if(GTK2)

lcl_set_build_project(${emp3gain_project} gtk)

add_custom_command(OUTPUT "${CMAKE_BINARY_DIR}/${listallmgr_project}-gtk"
	  COMMAND ln
	  ARGS -f -s ${CMAKE_INSTALL_PREFIX}/${EMP3GAIN_INSTALL_DIR}/gtk2/${emp3gain_project}
		     ${listallmgr_project}-gtk
	  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
	  MAIN_DEPENDENCY ${easymp3gain_gtk_OUT}
)

add_custom_target(emp3gain-GTK2 ALL DEPENDS ${easymp3gain_gtk_OUT})

endif(GTK2)
