@ECHO OFF
for %%I in (..\Boot.st ..\DBOOT.img7 ..\DBOOT.sml) do (copy %%I .)
..\Dolphin7 DBOOT.img7 Jade "Jade\sources\Jade Development.pax"
IF %ERRORLEVEL% NEQ 0 (
  ECHO Boot failed, Code=%ERRORLEVEL%
)
del Boot.st DBOOT.*
