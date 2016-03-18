@ECHO OFF
del JadeDev.*
cd ..
Dolphin7 DPRO.img7 -q -i Jade\JadeDev -d . -x
timeout /T 1 >nul
cd Jade
start ..\Dolphin7 JadeDev.img7 -q -f BootJade.st
