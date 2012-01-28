# CMakeLists.txt for easyMp3Gain GUI

find_package (Lazbuild REQUIRED)

if (LCL_VERSION LESS "930")
	message(FATAL_ERROR "LCL should be at least version 0.9.30!")
endif()

set (easymp3gain_project easymp3gain)

set (easymp3gain_FORMS
		unitconsoleoutput.lfm
		unitgainconstant.lfm
		unitinfo.lfm
		unitmain.lfm
		unitoptions.lfm
)

set (easymp3gain_SRC ${easymp3gain_project}.lpr
		${easymp3gain_FORMS}
		${CMAKE_BINARY_DIR}/appversion.inc
		unitconsoleoutput.pas
		unitgainconstant.pas
		unitinfo.pas
		unitmain.pas
		unitmediagain.pas
		unitmp3gain.pas
		unitoptions.pas
		unittranslate.pas
		unitvorbisgain.pas
		unitvorbiscomment.pas
)

configure_file(appversion.inc.in ${CMAKE_BINARY_DIR}/appversion.inc)

##########> Build GTK+ UI <##########
if (GTK2)

pkg_check_modules(GTK2 REQUIRED gtk+-2.0)

lcl_set_build_project (${easymp3gain_project} gtk)

add_custom_command (OUTPUT ${CMAKE_BINARY_DIR}/${easymp3gain_project}-gtk
	COMMAND ${CMAKE_COMMAND}
	ARGS -E copy ${CMAKE_BINARY_DIR}/gtk2/${easymp3gain_project}
		${CMAKE_BINARY_DIR}/${easymp3gain_project}-gtk
	WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
	MAIN_DEPENDENCY ${easymp3gain_gtk_OUT}
	DEPENDS ${easymp3gain_SRC}
)

add_custom_target (easyMp3Gain_GTK2 ALL DEPENDS ${CMAKE_BINARY_DIR}/${easymp3gain_project}-gtk)

# Install the GTK+ version
install (PROGRAMS ${CMAKE_BINARY_DIR}/${easymp3gain_project}-gtk
	DESTINATION bin COMPONENT binary
)

endif (GTK2)

##########> Build Qt UI <##########
if (QT4)

find_package (Qt4Pas)

lcl_set_build_project (${easymp3gain_project} qt)

add_custom_command (OUTPUT ${CMAKE_BINARY_DIR}/${easymp3gain_project}-qt
	COMMAND ${CMAKE_COMMAND}
	ARGS -E copy ${CMAKE_BINARY_DIR}/qt4/${easymp3gain_project}
		${CMAKE_BINARY_DIR}/${easymp3gain_project}-qt
	WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
	MAIN_DEPENDENCY ${easymp3gain_qt_OUT}
	DEPENDS ${easymp3gain_SRC}
)

add_custom_target (easyMp3Gain_Qt4 ALL DEPENDS ${CMAKE_BINARY_DIR}/${easymp3gain_project}-qt)

# Install the Qt version
install (PROGRAMS ${CMAKE_BINARY_DIR}/${easymp3gain_project}-qt
	DESTINATION bin COMPONENT binary
)

endif (QT4)
