## Mini Edit 

MiniEdit is an open source desktop application for editing files like PHP, HTML, CSS, Python, D, C and Pascal using UTF8 encoding.
Also it can compile or run script of this files, if your already installed its compilers in your system, without need to open a project for it, 
if you are editing multiple files, e.g. Python, PHP it can run this files.

### Target

 * Simple
 * Portable
 * Fast 

### Features

 * UTF8 encoding
 * Search in files
 * Search for file
 * Portable, one executable file
 * Projects files, each project have special type (PHP/Pascal/...)
 * PHP XDebug
 * Until now it is very Fast
 * Tortoise SVN/GIT integrated in Windows 


###Rules

For contributors keep it as simple as possible.

###Build

Miniedit is FreePascal/Lazarus project, so if you want to compile it need to install all packages that needed for the project

 * Use source code from github.com in branch "release", "master" branch is my upstream work.
 * FreePascal FPC 3.0 or later
 * Lazarus last update from svn repo, it is recomended, or use version 1.7
 * MiniLib http://sourceforge.net/projects/minilib
 * CMDBox http://wiki.freepascal.org/CmdLine

CMDBox package

    CMDLine\cmdbox.dpk

####minilib packages

    minilib\lib\MiniCommons.lpk
    minilib\xml\source\MiniXML.lpk
    minilib\socket\source\MiniSockets.lpk
    minilib\connection\source\MiniConnections.lpk
    minilib\comm\source\MiniComm.lpk    
    minilib\lazarus\lib\MiniLib.lpk
    minilib\lazarus\components\native\NativeLib.lpk

####Build minilib project
