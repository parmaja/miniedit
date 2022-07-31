@echo off

rem call build-windows.cmd

7z a "..\release\miniedit-%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%.7z" ..\bin\mne32.exe ..\bin\mne64.exe ..\bin\*.mne-theme ..\bin\lslint.exe ..\bin\*.dll ..\bin\*.template ..\readme.md changed.txt copying.txt
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
