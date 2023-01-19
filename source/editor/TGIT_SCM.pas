unit TGIT_SCM;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}
{$ifndef WINDOWS}
{$ERROR 'This unit only for Windows'}
{ you can build one for Linux with same name to work}
{$endif}

interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  FileUtil, SynEdit, EditorEngine,
  Windows;

type

  { TTGIT_SCM }

  TTGIT_SCM = class(TEditorSCM)
  private
  protected
    procedure Execute(App, Cmd: string); override;
    function GetTortoiseProc: string;
    function GetTortoiseMerge: string;
  public
    constructor Create; override;
    procedure CommitDirectory(Directory: string); override;
    procedure CommitFile(FileName: string); override;
    procedure UpdateDirectory(Directory: string); override;
    procedure UpdateFile(FileName: string); override;
    procedure RevertDirectory(Directory: string); override;
    procedure RevertFile(FileName: string); override;
    procedure DiffFile(FileName: string); override;
    procedure AddFile(FileName: string); override;
    procedure DiffToFile(FileName, ToFileName: string); override;
    property TortoiseProc: string read GetTortoiseProc;
    property TortoiseMerge: string read GetTortoiseMerge;
  end;

implementation

{ TTGIT_SCM }

procedure TTGIT_SCM.CommitDirectory(Directory: string);
begin
  Execute(TortoiseProc, '/command:commit /path:"' + Directory + '" /notempfile /closeonend');
end;

procedure TTGIT_SCM.CommitFile(FileName: string);
begin
  Execute(TortoiseProc, '/command:commit /path:"' + FileName + '" /notempfile /closeonend');
end;

procedure TTGIT_SCM.UpdateDirectory(Directory: string);
begin
  Execute(TortoiseProc, '/command:update /path:"' + Directory + '" /notempfile /closeonend');
end;

procedure TTGIT_SCM.UpdateFile(FileName: string);
begin
  Execute(TortoiseProc, '/command:update /path:"' + FileName + '" /notempfile /closeonend');
end;

procedure TTGIT_SCM.RevertDirectory(Directory: string);
begin
  Execute(TortoiseProc, '/command:revert /path:"' + Directory + '" /notempfile /closeonend');
end;

procedure TTGIT_SCM.RevertFile(FileName: string);
begin
end;

procedure TTGIT_SCM.AddFile(FileName: string);
begin
  Execute(TortoiseProc, '/command:add /path:"' + FileName + '" /notempfile /closeonend');
end;

procedure TTGIT_SCM.DiffFile(FileName: string);
begin
  Execute(TortoiseProc, '/command:diff /path:"' + FileName + '" /notempfile /closeonend');
end;

procedure TTGIT_SCM.DiffToFile(FileName, ToFileName: string);
begin
  Execute(TortoiseMerge, '/base:"' + FileName + '" /mine:"' + ToFileName+'"');
end;

procedure TTGIT_SCM.Execute(App, Cmd: string);
begin
  if ShellExecute(0, nil, PChar(App), PChar(Cmd), PChar(ExtractFilePath(App)), 1) < 32 then
    RaiseLastOSError;
end;

function TTGIT_SCM.GetTortoiseProc: string;
var
  s: string;
begin
  s := '';
  if (s = '') and DirectoryExists('C:\Program Files\TortoiseGit') then
    s := 'C:\Program Files\TortoiseGit';
  if s <> '' then
    s := IncludeTrailingPathDelimiter(s);
  if s = '' then
    Result := 'TortoiseGitProc.exe'
  else if (s <> '') and SameText(RightStr(s, 4), 'bin\') then
    Result := s + 'TortoiseGitProc.exe'
  else
    Result := s + 'bin\TortoiseGitProc.exe';
end;

function TTGIT_SCM.GetTortoiseMerge: string;
var
  s: string;
begin
  s := '';
  if (s = '') and DirectoryExists('C:\Program Files\TortoiseGit') then
    s := 'C:\Program Files\TortoiseGit';
  if s <> '' then
    s := IncludeTrailingPathDelimiter(s);
  if s = '' then
    Result := 'TortoiseGitMerge.exe'
  else if (s <> '') and SameText(RightStr(s, 4), 'bin\') then
    Result := '"' + s + 'TortoiseGitMerge.exe"'
  else
    Result := '"' + s + 'bin\TortoiseGitMerge.exe"';
end;

constructor TTGIT_SCM.Create;
begin
  inherited Create;
  FName := 'GIT';
  FTitle := 'Tortoise GIT';
  FDescription := 'Tortoise GIT for windows';
end;

initialization
  with Engine do
  begin
    SourceManagements.Add(TTGIT_SCM);
  end;
end.

