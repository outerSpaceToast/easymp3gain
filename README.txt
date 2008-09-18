
                                  easyMP3Gain
                                  -----------

REQUIREMENTS:
easyMP3Gain requires a Linux based OS with X11 and the programs mp3gain and/or vorbisgain and/or aacgain installed.
If your OS doesn't provide a mp3gain-package install it manually from the mp3gain-site.
Make sure that the mp3gain/vorbisgain/aacgain-binaries or a link is in the "/bin"-directory of your OS.

INSTALLING (Debian/xUbuntu/LinuxMint)
1.) download the ".deb" package of easyMP3Gain
2.) sudo dpkg -i <package name including path>

INSTALLING (openSuSE/Fedora/RedHat)
1.) download the ".rpm" package of easyMP3Gain
2.) sudo rpm -i <package name including path>

INSTALLING (.tar.gz)
1.) download the ".tar.gz" package of easyMP3Gain
1.) Extract the contents of the package file in your desired directory.

RUNNING (.deb / .rpm):
1.) Run the program by clicking on the accordingly menu entry in your desktop environment

RUNNING (.tar.gz):
1.) Change to the 'easyMP3Gain' directory where the files were extracted.
2.) Start easyMP3Gain by running "./easymp3gain"

Adding files to the list:
-------------------------
There are 3 possibilities to add files to the list.
1.) Select "Add files" or "Add files recursively" in the menu "File".
2.) Drag & Drop: Drag files from a file manager and drop them into the easyMP3Gain window.
3.) start easyMP3Gain with parameters. Some examples: 
     >easymp3gain '/home/song1.mp3' '/home/mymusic/'   --> adds song1.mp3 and all songs in /home/mymusic
     >easymp3gain -r '/home/mymusic'                   --> adds all songs in /home/mymusic recursively

MP3/AAC-Files vs. Ogg-Vorbis-Files:
-----------------------------------
MP3-Files are handled in an other way than Ogg-Vorbis-Files.
The handling depends on the backends mp3gain and vorbisgain.

Mp3-Files can be analyzed and "hard-gained". In the analyze-process volume-tags are written in the mp3-file.
The Column "Track Gain" shows, how much the file will be gained when clicking on "Gain".
In this process the volume of the files is changed (hard-gained).

Vorbis-files can't be "hard-gained". If you click on "Gain" the file is analyzed and the difference to 89dB is written in the file (only as a tag).
Some media-players read this tag and adjust the volume. In contrast to "mp3gain" "vorbisgain" has a fixed reference at 89dB.
Example: easyMP3Gain shows: Volume: 98.8; Gain: (-9.8).
         This means, the file has a real volume of 98.8, but the player reads the gain-tag and adjusts the volume to: 98.8-9.8 = 89 dB!

Why is there a "?" in the "Album Volume" column?
There is no information about the album volume in ogg-vorbis files. Only the album-gain tags can be read.

NOTES:
------
The easymp3gain-deb-packages have been created on a Debian Lenny and tested under Debian Lenny and Ubuntu 8.04

easyMP3Gain can be downloaded at http://easymp3gain.sourceforge.net/
mp3gain can be downloaded at http://mp3gain.sourceforge.net/download.php
vorbisgain can be downloaded at http://sjeng.org/vorbisgain.html
aacgain can be downloaded at http://altosdesign.com/aacgain/

