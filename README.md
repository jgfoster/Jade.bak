Jadeite
====

Jadeite is a graphical user interface/IDE for GemStone/S 64 Bit development. Jadeite is a Dolphin-based Smalltalk application that runs on Microsoft Windows. It allows login to a GemStone Smalltalk Repository in which the Rowan tools are loaded, to allow project and package management, code development, and debugging.

Jadeite is based on the Jade Smalltalk project developed by James Foster, built in (and inspired by) Dolphin Smalltalk from [Object-Arts](https://github.com/dolphinsmalltalk/Dolphin).

The "Oscar" versions of Jadeite are the development line for Release, which is separate from and not compatible with the Jadeite Alpha.  

### Runtime Installation
To install Jadeite runtime, download the zip file with the executable and supporting libraries from [here](https://github.com/GemTalk/Jadeite/releases).  Once you have the environment, you can generally download just the latest jadeite.exe executable.

Within the zip file, the runtime directory contains the jadeite.exe, README.md, and /bin and /icons directories that compose the Jadeite runtime environment. 

On Windows, create a directory for Jadeite, and copy the contents of the runtime directory to this directory. You can now execute jadeite.exe.  No updates to the OS path or %GEMSTONE% environment variable are required. 

Jadeite can only log into a GemStone/S 64 Bit GemStone repository that has Rowan installed. Note that the GemStone server is not supported on Windows; server platforms other than Linux have not been tested. 

To download Rowan, see the [Rowan Releases](https://github.com/GemTalk/Rowan/releases), and installation is described in the [README](https://github.com/GemTalk/Rowan/blob/master/README.md).
