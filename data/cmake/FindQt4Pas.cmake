# - Try to find Qt4Pas
# Once done this will define
#
#  Qt4PAS_FOUND - System has Qt4Pas
#  QT4PAS_INCLUDE_DIR - Set if system has extra qt4.pas file
#
# Copyright (c) 2010, Matthias Klumpp, <matthias@nlinux.org>
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.

if ( QT4PAS_LIBRARY )
   # in cache already
   SET(Qt4Pas_FIND_QUIETLY TRUE)
endif ( QT4PAS_LIBRARY )

FIND_PATH(QT4PAS_INCLUDE_DIR qt4.pas /usr/share/pascal/qt4 /usr/local/share/pascal/qt4)

FIND_LIBRARY(QT4PAS_LIBRARY NAMES Qt4Pas PATH /usr/lib /usr/local/lib /usr/lib64)

IF (QT4PAS_LIBRARY)
   SET(QT4PAS_FOUND TRUE)
ENDIF (QT4PAS_LIBRARY)


IF (QT4PAS_FOUND)
   IF (NOT Qt4Pas_FIND_QUIETLY)
      MESSAGE(STATUS "Found Qt4Pas: ${QT4PAS_LIBRARY}")
   ENDIF (NOT Qt4Pas_FIND_QUIETLY)
ELSE (QT4PAS_FOUND)
   IF (Qt4Pas_FIND_REQUIRED)
      MESSAGE(FATAL_ERROR "Could not find Qt4Pas")
   ENDIF (Qt4Pas_FIND_REQUIRED)
ENDIF (QT4PAS_FOUND)
