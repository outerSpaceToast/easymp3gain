# CMakeLists for help documents

set (EMP3GAIN_HELP_DOCS
	index.html
	install.html
	index.de.html
	install.de.html
)

install (FILES ${EMP3GAIN_HELP_DOCS} DESTINATION ${EMP3GAIN_DATA_DIR}/help COMPONENT data)
