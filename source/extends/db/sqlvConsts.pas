unit sqlvConsts;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  SysUtils;

const
  sqlvVersion = '0.2.1';
  sSqliteFilter = 'Sqlite (*.sqlite)|*.sqlite|FirebirdSQL (*.fdb)|*.fdb';
  sAllFilesFilter = 'All files (*.*)|*.*';
  sFileNameFilter = '*.sqlite; *.fdb';
  sFileExtFilter = 'sqlite';
  sqlvConfig = 'sqlviewer.config';
  sqlvRecents = 'sqlviewer.recents';

implementation
end.

