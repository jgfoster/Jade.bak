Jade
====

Jade is an Alternative Development Environment (IDE) for GemStone/S that runs on Microsoft Windows. This [screencast](https://www.youtube.com/watch?v=dnRB5rBbkiI) gives a brief demo from 2013. 

Jade is built in (and inspired by) Dolphin Smalltalk from [Object-Arts](https://github.com/dolphinsmalltalk/Dolphin).

### Runtime Installation
The [Releases](https://github.com/jgfoster/Jade/releases) page contains links to a zip file with the executable and supporting libraries.

Older versions are available using the instructions [here](https://github.com/jgfoster/Jade/issues/56).

Jade runs pretty well under [Wine](https://www.winehq.org/) and a pre-built Mac app is available [here](http://seaside.gemtalksystems.com/jade/Jade.app.zip). It may be out-of-date, so you can use “Show Package Contents” and then update "drive_c/Program Files/Jade” with items obtained from [here](https://github.com/jgfoster/Jade/raw/master/runtime/Jade.zip).

### Development Installation

To install Jade in Dolphin 7, follow these steps:

1. Install a Github client such as [SourceTree](http://www.sourcetreeapp.com/).
2. Clone [Dolphin](https://github.com/jgfoster/Dolphin) to Documents\Dolphin.
3. Run Documents\Dolphin\BootPRO.cmd to build the DPRO image.
4. Clone [Jade](https://github.com/jgfoster/Jade) to Documents\Dolphin\Jade.
4. Run Documents\Dolphin\Jade\BootJade.cmd to build the JadeDev image.
5. Finally, from the Additional Tools folder in the System Shell open the Jade Login.
