Jade
====

Jade is an Alternative Development Environment (IDE) for GemStone/S that runs on Microsoft Windows.

Jade is built in (and inspired by) Dolphin Smalltalk from [Object-Arts](http://www.object-arts.com/).

### Runtime Installation
To install Jade runtime, download a 15 MB zip file with the executable and supporting libraries from [here](https://github.com/jgfoster/Jade/raw/master/runtime/Jade.zip). Once you have the environment, you can get just the latest executable (about 1.5 MB) from [here](https://github.com/jgfoster/Jade/raw/master/runtime/Jade.exe).

### Development Installation

To install Jade in Dolphin to do development, follow these steps:

1. Install a Github client such as [SourceTree](http://www.sourcetreeapp.com/)
2. Clone https://github.com/jgfoster/Jade to Documents\Dolphin\Jade.
3. Do a Fresh Install of Dolphin into Documents\Dolphin\Jade as 'Jade.img'.
4. Run the Package Install Script (below).
5. Finally, open a login window:
```
    JadeLoginShell show.
```

#### Package Installation Script
```
PackageManager current
	install: 'Object Arts\Dolphin\Internal Bitmaps and Icons.pac';
	install: 'Solutions Software\Widgets\SSW ListView Extensions.pac';
	install: 'Solutions Software\Widgets\SSW Widget Enhancements.pac';
	install: 'Solutions Software\Widgets\SSW EditableListView.pac';
	install: 'JGF\JGF Dolphin.pac';
	install: 'JGF\JGF Dolphin MVP Base.pac';
	install: 'JGF\JGF Fading Dialog.pac';
	install: 'Jade\GemStone C Interface.pac';
	install: 'Jade\GemStone Objects.pac';
	install: 'Jade\GemStone Session.pac';
	install: 'Jade\GemStone Services.pac';
	install: 'Jade\VisualWorks Component.pac';
	install: 'Jade\Jade UI Base.pac';
	install: 'Jade\Jade Autocompletation.pac';
	install: 'Jade\Jade Inspector.pac';
	install: 'Jade\Jade Class Browser.pac';
	install: 'Jade\Jade Method Browser.pac';
	install: 'Jade\Jade User Browser.pac';
	install: 'Jade\Monticello.pac';
	install: 'Jade\Jade UI.pac';
	install: 'Jade\Jade System Browser.pac';
	install: 'Jade\Jade Transcript.pac';
	install: 'Jade\Jade Process Browser.pac';
	install: 'Jade\Object Log Browser.pac';
	install: 'Jade\Jade Login.pac';
	yourself.
[PackageManager current install: 'Jade\Jade from Dolphin.pac']
	on: Package clashSignal  do: [:ex | ex resume].

"If you want to integrate Jade in Dolphin Professional"
PackageManager current
	install: 'Jade\Jade in Dolphin.pac';
	install: 'Jade\Jade Deployment.pac';
	yourself.
```
#### Dolphin Community Edition

Dolphin CE is on version 6.0 while the main development work
has been done on Dolphin Pro 6.1b2. This means that things don't
really work. You can, however, load things without errors and
look at the code if you start with the following:
```
PackageManager current
    install: 'Jade\Jade in Dolphin CE.pac';
    install: 'Steve Waring\YAXO Base.pac';
    install: 'Steve Waring\YAXO DOM.pac';
    yourself.
```
