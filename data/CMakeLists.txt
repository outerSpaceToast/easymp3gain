# CMakeLists for easyMp3Gain generic data

set (XDG_APPS_INSTALL_DIR share/applications)

if (QT4)

set(show_desktop KDE)
set(notshow_desktop GNOME)
set(emp3gain_current_exec "easymp3gain-qt")
configure_file("easymp3gain.desktop.in"
  "${CMAKE_CURRENT_BINARY_DIR}/easymp3gain-kde.desktop"
  @ONLY
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/easymp3gain-kde.desktop
	  DESTINATION ${XDG_APPS_INSTALL_DIR}
	  COMPONENT data
)

endif (QT4)

if (GTK2)

set(show_desktop GNOME)
set(notshow_desktop KDE)
set(emp3gain_current_exec "easymp3gain-gtk")
configure_file("easymp3gain.desktop.in"
  "${CMAKE_CURRENT_BINARY_DIR}/easymp3gain-gnome.desktop"
  @ONLY
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/easymp3gain-gnome.desktop
	  DESTINATION ${XDG_APPS_INSTALL_DIR}
	  COMPONENT data
)

endif (GTK2)

add_subdirectory (icons)
