SoftROM EEPROM tool
===================

The SoftROM is a ROM replacement for Commodore CBM/PET computers as
described at http://petsd.net/softrom.php


Please note: current state is alpha and partially a TODO-list.

This tool writes ROM images to the EEPROM non-volatile memory.

The current version is for BASIC 4 and 80 column display.
Future versions will support BASIC 2 and 40 columns too.

It shows the directory of a drive with a file selection bar and lets the
user select an image to write into the EEPROM with cursor up/down.
If the directory exceeds the size of the screen, it scrolls up and down.

A filter is available to show only files with appropiate size and start
address that qualify the file as an extension ROM image file.

ENTER starts the write process:

- buffer file into RAM
- EEPROM write protection is disabled
- buffered file content is written in chunks of 64 bytes
- EEPROM write protection is enabled again
- Verify EEPROM against file buffer

![ScreenShot](https://raw.githubusercontent.com/nils-eilers/SoftROM/master/shot.jpg)
