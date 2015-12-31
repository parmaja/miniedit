## Mini Edit 

MiniEdit is an open source desktop application for edit files like PHP, HTML and Pascal using UTF8 encoding.

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
 * Tortoise SVN/GIT integrated

### Compile

 * Use source code from github.com in branch "release", "master" branch is my upstream work.
 * FreePascal FPC 2.6.4 or later
 * Lazarus last update from svn repo
 * MiniLib from http://sourceforge.net/projects/minilib
 * CMDBox http://wiki.freepascal.org/CmdLine

 ###Build

 You need to install all packages that needed for minilib project

CMDBox package

    CMDLine\cmdbox.dpk

minilib pachages

    minilib\lib\MiniCommons.lpk
    minilib\xml\source\MiniXML.lpk
    minilib\socket\source\MiniSockets.lpk
    minilib\connection\source\MiniConnections.lpk
    minilib\comm\source\MiniComm.lpk
    minilib\connection\db\MiniDB.lpk
    minilib\lazarus\lib\MiniLib.lpk
    minilib\lazarus\components\native\NativeLib.lpk

Build minilib project
