@ECHO OFF
cd ..
del DPRO.*
call BootDPRO
start Dolphin7 DPRO.img7 -q -f Jade\BootDPRO.st
