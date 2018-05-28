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
  sqlvVersion = '0.1.6';
  {$ifdef FIREBIRD}
  sSqliteFilter = 'FirebirdSQL (*.fdb)|*.fdb';
  sAllFilesFilter = 'All files (*.*)|*.*';
  sFileNameFilter = '*.fdb';
  sFileExtFilter = 'fdb';
  sqlvConfig = 'fbsql.viewer.config';
  sqlvRecents = 'fbsql.viewer.recents';
  {$else}
  sSqliteFilter = 'Sqlite (*.sqlite)|*.sqlite';
  sAllFilesFilter = 'All files (*.*)|*.*';
  sFileNameFilter = '*.sqlite';
  sFileExtFilter = 'sqlite';
  sqlvConfig = 'sqlite.viewer.config';
  sqlvRecents = 'sqlite.viewer.recents';
  {$endif}

implementation
end.

