@echo off
if exist ..\bin\mne.exe (del ..\bin\mne.exe)
if exist ..\bin\mne32.exe (del ..\bin\mne32.exe)
if exist ..\bin\mne64.exe (del ..\bin\mne64.exe)
 
lazbuild --build-mode=Win32 gui\mne.lpr -r -B
if errorlevel 1 goto erroroccurred

lazbuild --build-mode=Win64 gui\mne.lpr -r -B
if errorlevel 1 goto erroroccurred

rem strip ..\bin\mne.exe
rem if errorlevel 1 goto erroroccurred

goto noerrors

:erroroccurred
echo ???????????????????
echo    Error compile
echo ???????????????????
pause
goto :EOF
:noerrors
echo #######################
echo    Compile completed
echo #######################
pause
