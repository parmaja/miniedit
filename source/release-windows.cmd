@echo off

call build-windows.cmd

7z a "../release/miniedit-%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%.7z" ../bin/mne32.exe ../bin/mne64.exe ../readme.md ../bin/*.mne-theme
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
