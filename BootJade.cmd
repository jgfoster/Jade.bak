@ECHO OFF
cd ..
Dolphin7 DBOOT.img7 Jade "Jade\sources\Jade Development.pax"
IF %ERRORLEVEL% NEQ 0 (
  ECHO Boot failed, Code=%ERRORLEVEL%
)
cd Jade
