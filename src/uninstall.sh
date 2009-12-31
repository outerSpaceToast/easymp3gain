#!/bin/sh
echo "Uninstallation started."
rm -f /usr/share/applications/easyMP3Gain.desktop
rm -f /usr/share/lang/*
rm -f /usr/share/help/*
# rm -f /usr/share/icons/*
# rm -f /usr/share/pixmaps/easymp3gain.png
rm -f /usr/share/icons/hicolor/16x16/easymp3gain.png
rm -f /usr/share/icons/hicolor/22x22/easymp3gain.png
rm -f /usr/share/icons/hicolor/24x24/easymp3gain.png
rm -f /usr/share/icons/hicolor/32x32/easymp3gain.png
rm -f /usr/share/icons/hicolor/48x48/easymp3gain.png
rm -f /usr/share/icons/hicolor/64x64/easymp3gain.png
rm -f /usr/share/icons/hicolor/128x128/easymp3gain.png
rm -f /usr/share/icons/hicolor/192x192/easymp3gain.png

rm -f /usr/bin/easymp3gain

rmdir /usr/share/easymp3gain/lang
rmdir /usr/share/easymp3gain/help
# rmdir /usr/share/easymp3gain/icons
rmdir /usr/share/easymp3gain

echo "Uninstalled."
