@ECHO OFF
cd ..
Dolphin7 DBOOT.img7 Jade "Jade\Jade Development.pax"
IF %ERRORLEVEL% NEQ 0 (
  ECHO Boot failed, Code=%ERRORLEVEL%
  PAUSE
)
move Jade.* Jade\
cd Jade
