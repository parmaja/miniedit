unit mneTyroClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, mneClasses,
  Contnrs, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  gdbClasses, EditorRun, EditorClasses, mneCompilerProjectFrames, mneRunFrames,
  LazFileUtils, SynHighlighterPas, SynHighlighterLFM,
  mnePASClasses, mneLuaClasses;

type
  TPSFileCategory = class(TPASFileCategory)
  public
  end;

  TLSFileCategory = class(TLuaFileCategory)
  public
  end;

  { TTyroProjectOptions }

  TTyroProjectOptions = class(TEditorProjectOptions)
  private
  public
    constructor Create; override;
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
  end;

  { TTyroTendency }

  TTyroTendency = class(TEditorTendency)
  private
    FUseCFG: Boolean;
    FCompiler: string;
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Created; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack); override;
    function CreateProjectOptions: TEditorProjectOptions; override;
    property Compiler: string read FCompiler write FCompiler;
    property UseCFG: Boolean read FUseCFG write FUseCFG default True;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, mneTyroProjectFrames;

{ TTyroProjectOptions }

constructor TTyroProjectOptions.Create;
begin
  inherited Create;
end;

procedure TTyroProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TTyroProjectFrame.Create(AOwner);
  (aFrame as TTyroProjectFrame).Project := AProject;
  aFrame.Caption := 'Tyro Options';
  AddFrame(aFrame);
end;

{ TTyroTendency }

function TTyroTendency.CreateDebugger: TEditorDebugger;
begin
  Result := TGDBDebug.Create;
end;

function TTyroTendency.CreateProjectOptions: TEditorProjectOptions;
begin
  Result := TTyroProjectOptions.Create;;
end;

procedure TTyroTendency.Created;
begin
  FCapabilities := [capExecute, capDebug, capTrace, capCompile, capLink];
  FHaveOptions := True;
  FName := 'Tyro';
  FTitle := 'Tyro project';
  FDescription := 'Tyro Files, *.lua, *.ps *.ls';
  FUseCFG := True;
  FImageIndex := -1;
  AddGroup('lua', TLuaFileCategory);
end;

procedure TTyroTendency.DoRun(Info: TmneRunInfo);
var
  aRunItem: TmneRunItem;
begin
  Engine.Session.Run.Clear;
  Engine.SendAction(eaClearOutput);

  if rnaExecute in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Run.Console := Info.Console;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.StatusMessage := 'Runing ' + Info.MainFile;
    if RunOptions.Require <> '' then
        aRunItem.Info.Run.AddParam('-l '+ RunOptions.Require);

    aRunItem.Info.Run.Command := Info.Command;
    if Info.Command = '' then
    begin
      {$ifdef windows}
        aRunItem.Info.Run.Command := 'tyro.exe';
      {$else}
        aRunItem.Info.Run.Command := 'tyro';
      {$endif}
    end;
    aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
  end
  else if (rnaLint in Info.Actions) or (rnaCompile in Info.Actions) then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Run.Silent := True;
    aRunItem.Info.Run.Console := False;

    if (rnaCompile in Info.Actions) then
    begin
      aRunItem.Info.StatusMessage := 'Compiling ' + Info.MainFile;
      if Info.OutputFile <> '' then
        aRunItem.Info.Run.AddParam('-o "'+ Info.OutputFile + '"');
    end
    else
    begin
      aRunItem.Info.StatusMessage := 'Linting ' + Info.MainFile;
      aRunItem.Info.Run.AddParam('-p ');
    end;

    //aRunItem.Info.Run.Command := Info.LintCommand;
    //if Info.Command = '' then
    //begin
    {$ifdef windows}
      aRunItem.Info.Run.Command := 'tyro.exe';
    {$else}
      aRunItem.Info.Run.Command := 'tyro';
    {$endif}
      aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
    //end
  end;
end;

procedure TTyroTendency.CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := RunOptions;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TTyroTendency);
    Categories.Add(TPSFileCategory.Create(TTyroTendency, 'ps', 'Pascal Script'));
    Groups.Add(TPASFile, 'ps', 'Pascal Script', TPSFileCategory, ['.rops', '.ps', '.pascal'], [fgkAssociated, fgkFolding, fgkBrowsable], [capExecute, capDebug]);

    Categories.Add(TLSFileCategory.Create(TTyroTendency, 'ls', 'Lua Script'));
    Groups.Add(TLuaFile, 'ls', 'Lua Script', TLSFileCategory, ['.ls'], [fgkAssociated, fgkFolding, fgkBrowsable], [capExecute, capDebug]);
  end;
end.
