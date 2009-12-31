#!/bin/bash
#
# Currently supported options are:
#
# DESTDIR		Destination root directory

set -e

for arg; do
  case $arg in
    DESTDIR=*) DESTDIR=${arg#DESTDIR=};;
    WIDGET=*) WIDGET=${arg#WIDGET=};;
  esac;
done


echo "Installation started."

# Does the install

mkdir -p $DESTDIR/usr/share/easymp3gain
mkdir -p $DESTDIR/usr/bin

if [ "$WIDGET" == "qt4" ]; then
cp ./bin/qt4/easymp3gain $DESTDIR/usr/bin/
else
cp ./bin/gtk2/easymp3gain $DESTDIR/usr/bin/
fi

mkdir -p $DESTDIR/usr/share/applications
mkdir -p $DESTDIR/usr/share/easymp3gain/lang
mkdir -p $DESTDIR/usr/share/easymp3gain/help
# mkdir -p $DESTDIR/usr/share/easymp3gain/icons
mkdir -p $DESTDIR/usr/share/icons/hicolor/16x16/apps
mkdir -p $DESTDIR/usr/share/icons/hicolor/22x22/apps
mkdir -p $DESTDIR/usr/share/icons/hicolor/24x24/apps
mkdir -p $DESTDIR/usr/share/icons/hicolor/32x32/apps
mkdir -p $DESTDIR/usr/share/icons/hicolor/48x48/apps
mkdir -p $DESTDIR/usr/share/icons/hicolor/64x64/apps
mkdir -p $DESTDIR/usr/share/icons/hicolor/128x128/apps
mkdir -p $DESTDIR/usr/share/icons/hicolor/192x192/apps


cp ./applications/easymp3gain.desktop $DESTDIR/usr/share/applications
# cp ./doc/* $DESTDIR/usr/share/doc/easymp3gain
cp ./lang/* $DESTDIR/usr/share/easymp3gain/lang
cp ./help/* $DESTDIR/usr/share/easymp3gain/help
# cp ./icons/*.png $DESTDIR/usr/share/easymp3gain/icons
cp ./icons/easymp3gain-16.png $DESTDIR/usr/share/icons/hicolor/16x16/apps/easymp3gain.png
cp ./icons/easymp3gain-22.png $DESTDIR/usr/share/icons/hicolor/22x22/apps/easymp3gain.png
cp ./icons/easymp3gain-24.png $DESTDIR/usr/share/icons/hicolor/24x24/apps/easymp3gain.png
cp ./icons/easymp3gain-32.png $DESTDIR/usr/share/icons/hicolor/32x32/apps/easymp3gain.png
cp ./icons/easymp3gain-48.png $DESTDIR/usr/share/icons/hicolor/48x48/apps/easymp3gain.png
cp ./icons/easymp3gain-64.png $DESTDIR/usr/share/icons/hicolor/64x64/apps/easymp3gain.png
cp ./icons/easymp3gain-128.png $DESTDIR/usr/share/icons/hicolor/128x128/apps/easymp3gain.png
cp ./icons/easymp3gain-192.png $DESTDIR/usr/share/icons/hicolor/192x192/apps/easymp3gain.png


#Create symlink
# mkdir -p $DESTDIR/usr/bin
# ln -s $DESTDIR/usr/share/easymp3gain/easymp3gain $DESTDIR/usr/bin/easymp3gain
# ln -s $DESTDIR/usr/share/easymp3gain/icons/easymp3gain-128.png $DESTDIR/usr/share/pixmaps/easymp3gain.png

echo "Installation done."
