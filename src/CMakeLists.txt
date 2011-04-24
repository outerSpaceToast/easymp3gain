# CMakeLists.txt for easyMp3Gain GUI

find_package(Lazbuild REQUIRED)

if (LCL_VERSION LESS "930")
	message(FATAL_ERROR "LCL should be at least version 0.9.30!")
endif()

set (easymp3gain_project easymp3gain)

set (easymp3gain_FORMS unitconsoleoutput.lfm
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
		vorbiscomment.pas
)

configure_file(appversion.inc.in ${CMAKE_BINARY_DIR}/appversion.inc)

set (EMP3GAIN_INSTALL_DIR lib/easymp3gain)

##########> Build GTK+ UI <##########
if (GTK2)

lcl_set_build_project (${easymp3gain_project} gtk)

add_custom_command (OUTPUT "${CMAKE_BINARY_DIR}/${easymp3gain_project}-gtk"
	  COMMAND ln
	  ARGS -f -s ${CMAKE_INSTALL_PREFIX}/${EMP3GAIN_INSTALL_DIR}/gtk2/${easymp3gain_project}
		     ${easymp3gain_project}-gtk
	  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
	  MAIN_DEPENDENCY ${easymp3gain_gtk_OUT}
)

add_custom_target (easyMp3Gain_GTK2 ALL DEPENDS ${CMAKE_BINARY_DIR}/${easymp3gain_project}-gtk)

endif (GTK2)

##########> Build Qt UI <##########
if (QT4)

# TODO

endif (QT4)
