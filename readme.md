## Mini Edit 

MiniEdit is an open source desktop application for editing files like PHP, HTML, CSS, Python, D, C and Pascal using UTF8 encoding.
Also it can compile or run script of this files, if your already installed its compilers in your system, without need to open a project for it, 
if you are editing multiple files, e.g. Python, PHP it can run this files into console(cmd) or terminal(xterm).

### Target

 * Simple (especially for beginners or kids)
 * Portable
 * Fast   

### Features

 * Portable, one executable file, take it any where.
 * UTF8, Ansi, UC16LE, UC16BE encoding
 * Projects files, each project have special type/tendency (PHP, Lua, Python, Pascal, D)
 * Until now it is very Fast and very small
 * Search in files
 * Search for file
 * PHP XDebug
 * Tortoise SVN/GIT integrated in Windows, [TODO] RabbitVCS in Linux
 * [TODO] GDB debugging
 * Works in Windows and Linux
 * Compile and run Pascal, D, Go, Cpp (cpp todo not yet)
 * Run PHP, CMD, SH, BAT, Lua, Py
 
### Disadvantages
  
 * Not good in auto complete
 * Can't open unkown files
 * Can't open a huge file like logs

### Contributing

Keep it as simple as possible.
There is an addons to add menu items to file or main menu tools.

### Build

MiniEdit is FreePascal/Lazarus project, to compile it you need to install all packages that needed

 * Use source code from github.com in branch "release", or use last tag,  branch "master" is my upstream work, it is not stable.
 * FreePascal FPC 3.0 or later
 * Lazarus last update from subversion repo, it is recommended, or use version 2.1
 * MiniLib http://sourceforge.net/projects/minilib
 * CMDBox http://wiki.freepascal.org/CmdLine

Open each package in Lazarus and compile it in order.

#### Required Packages

    CMDLine\cmdbox.dpk
    minilib\lib\MiniCommons.lpk
    minilib\xml\source\MiniXML.lpk
    minilib\socket\source\MiniSockets.lpk
    minilib\connection\source\MiniConnections.lpk
    minilib\comm\source\MiniComm.lpk    
    minilib\lazarus\lib\MiniLib.lpk
    minilib\lazarus\components\native\NativeLib.lpk


#### Competitions
    Notepad++ https://notepad-plus-plus.org/
    Geany https://www.geany.org/
    Textadept https://foicica.com/textadept/
