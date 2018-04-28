unit mneSARDClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, Dialogs,
  LCLintf, LCLType, FileUtil,
  EditorOptions, EditorDebugger, EditorRun, EditorClasses, mneRunFrames,
  SynEditHighlighter, SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  mnSynHighlighterSARD, SynHighlighterLFM;

type

  { TmneSynSARDSyn }

  TmneSynSARDSyn = class(TSynSARDSyn)
  public
  end;

  { TSARDFile }

  TSARDFile = class(TSourceEditorFile)
  protected
    procedure NewContent; override;
  public
  end;

  { TSARDFileCategory }

  TSARDFileCategory = class(TTextFileCategory)
  private
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  TSardEditorDebugger = class(TEditorDebugger)
  end;

  { TSARDTendency }

  TSARDTendency = class(TEditorTendency)
  protected
    procedure Init; override;
    function CreateDebugger: TEditorDebugger; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils;

{ TSARDTendency }

procedure TSARDTendency.Init;
begin
  FCapabilities := [capRun, capOptions];
  FName := 'SARD';
  FTitle := 'SARD project';
  FDescription := 'SARD Files, *.sard';
  FImageIndex := -1;
end;

function TSARDTendency.CreateDebugger: TEditorDebugger;
begin
  //Result := TSardEditorDebugger.Create;
  Result := inherited CreateDebugger;
end;

procedure TSARDTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := ATendency.RunOptions;
  EnumRunCommands(aFrame.CommandEdit.Items);
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

procedure TSARDTendency.DoRun(Info: TmneRunInfo);
var
  aRunItem: TmneRunItem;
begin
  Engine.Session.Run.Clear;

  if rnaExecute in Info.Actions then
  begin
    Engine.SendAction(eaClearOutput);

    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Run.Console := Info.Console;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.StatusMessage := 'Runing ' + Info.MainFile;
    {
    if RunOptions.Require <> '' then
      RunItem.Info.Run.AddParam('-l '+ RunOptions.Require);
    if rnaDebug in Info.Actions then
      aRunItem.Info.Run.AddParam('-e '+ '"require(''mobdebug'').start()"'); //using mobdebug
    }
    aRunItem.Info.Run.Command := Info.Command;
    if Info.Command = '' then
    begin
      {$ifdef windows}
        aRunItem.Info.Run.Command := 'sard.exe';
      {$else}
        aRunItem.Info.Run.Command := 'sard';
      {$endif}
    end;
    aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
  end
  else if (rnaLint in Info.Actions) then
  begin
    Engine.SendAction(eaClearOutput);

    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Run.Silent := True;
    aRunItem.Info.Run.Console := False;

    aRunItem.Info.StatusMessage := 'Linting ' + Info.MainFile;
    aRunItem.Info.Run.AddParam('-l ');

    {$ifdef windows}
      aRunItem.Info.Run.Command := 'sard.exe';
    {$else}
      aRunItem.Info.Run.Command := 'sard';
    {$endif}
    aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
  end;

  if Engine.Session.Run.Active then //if there is items ready to run
    Engine.Session.Run.Start(Self);
end;

{ TSARDFileCategory }

function TSARDFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmneSynSARDSyn.Create(nil);
end;

procedure TSARDFileCategory.InitMappers;
begin
  with Highlighter as TSynSARDSyn do
  begin
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(ObjectAttri, attDataName);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
  end
end;

{ TSARDFile }

procedure TSARDFile.NewContent;
begin
  inherited NewContent;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TSARDTendency);
    Categories.Add(TSARDFileCategory.Create(TSARDTendency, 'Sard'));
    Groups.Add(TSARDFile, 'sard', 'SARD', TSARDFileCategory, ['sard'], [fgkAssociated, fgkExecutable, fgkBrowsable], [fgsFolding]);
  end;
end.
