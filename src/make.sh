#!/bin/bash

set -e

for arg; do
  case $arg in
    WIDGET=*) WIDGET=${arg#WIDGET=};;
  esac;
done

# Detects and parses the architecture
ARCH=$(uname -m)
case "$ARCH" in
 "i686") ARCH="i386";;
 "i586") ARCH="i386";;
 "i486") ARCH="i386";;
esac
 
echo "Target architecture: $ARCH"
 
OS="linux"
echo "Target operating system: $OS"
 
echo "Compiler version: $(fpc -iW)"

# Command line to build the sofware 
echo "Building easyMP3Gain ..."

mkdir -p ./bin
mkdir -p ./bin/qt4
mkdir -p ./bin/gtk2
if [ "$WIDGET" == "qt4" ]; then
lazbuild -B --ws=qt easymp3gain.lpr
mv ./bin/easymp3gain ./bin/qt4/
else
lazbuild -B --ws=gtk2 easymp3gain.lpr
mv ./bin/easymp3gain ./bin/gtk2/
fi

echo "Build finished."
