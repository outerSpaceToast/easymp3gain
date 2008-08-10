 
REQUIREMENTS:
easyMP3Gain requires a Linux based OS with X11 and the programs mp3gain and/or vorbisgain and/or aacgain installed.
If your OS doesn't provide a mp3gain-package install it manually and download the easymp3gain-tar.gz-package.
Make sure that the mp3gain/vorbisgain/aacgain-binaries or a link is in the "/bin"-directory of your OS.

RUNNING (.deb):
1. Install the package easymp3gain_x.deb
2. Run the program by clicking on the accordingly menu entry in your desktop environment

RUNNING (.tar.gz):
1. Extract the contents of the package file in your desired directory.
2. Change to the 'easyMP3Gain' directory where the files were extracted.
3. Start easyMP3Gain by running "./easymp3gain"

MP3/AAC-Files vs. Ogg-Vorbis-Files:
-----------------------------------
MP3-Files are handled in an other way than Ogg-Vorbis-Files.
The handling depends on the backends mp3gain and vorbisgain.

Mp3-Files can be analyzed and "hard-gained". In the analalyze-process volume-tags are written in the mp3-file.
The Column "Track Gain" shows, how much the file will be gained when clicking on "Gain".
In this process the volume of the files is changed (hard-gained).

Vorbis-files can't be "hard-gained". If you click on "Gain" the file is analyzed and the differece to 89dB is written in the file (as a tag).
Some media-players read this tag and adjust the volume. In contrast to "mp3gain" "vorbisgain" has a fixed reference at 89dB.
Example: easyMP3Gain shows: Volume: 98.8; Gain: (-9.8).
         This means, the file has a real volume of 98.8, but the player reads the gain-tag and adjusts the volume to: 98.8-9.8 = 89 dB!

Why is there a "?" in the "Album Volume" column?
There is no information about the album volume in the ogg-vorbis files. Only the album-gain tags can be read.

NOTES:
The easymp3gain-packages have been created on a Kubuntu 7.10 (gutsy) and tested under Kubuntu 7.10 and Ubuntu 6.10

easyMP3Gain can be downloaded at http://easymp3gain.sourceforge.net/
mp3gain can be downloaded at http://mp3gain.sourceforge.net/download.php

