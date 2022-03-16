unit mneSardClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}

interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, Dialogs,
  LCLintf, LCLType, LazFileUtils,
  EditorOptions, EditorRun, EditorClasses, mneRunFrames,
  SynEditHighlighter, SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  mnSynHighlighterSARD, SynHighlighterLFM;

type

  { TmneSynSardSyn }

  TmneSynSardSyn = class(TSynSardSyn)
  public
  end;

  { TSardFile }

  TSardFile = class(TSourceEditorFile)
  protected
    procedure NewContent; override;
  public
  end;

  { TSardFileCategory }

  TSardFileCategory = class(TTextFileCategory)
  private
  protected
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitMappers; override;
  public
  end;

  TSardEditorDebugger = class(TEditorDebugger)
  end;

  { TSardTendency }

  TSardTendency = class(TEditorTendency)
  protected
    procedure Created; override;
    function CreateDebugger: TEditorDebugger; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils;

{ TSardTendency }

procedure TSardTendency.Created;
begin
  FCapabilities := [capExecute];
  FHaveOptions := True;
  FName := 'Sard';
  FTitle := 'Sard project';
  FDescription := 'Sard Files, *.sard';
  FImageIndex := -1;
end;

function TSardTendency.CreateDebugger: TEditorDebugger;
begin
  //Result := TSardEditorDebugger.Create;
  Result := inherited CreateDebugger;
end;

procedure TSardTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := ATendency.RunOptions;
  EnumRunCommands(aFrame.CommandEdit.Items);
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

procedure TSardTendency.DoRun(Info: TmneRunInfo);
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
    Engine.Session.Run.Start(Debugger);
end;

{ TSardFileCategory }

function TSardFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmneSynSardSyn.Create(nil);
end;

procedure TSardFileCategory.InitMappers;
begin
  with Highlighter as TSynSardSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeywordAttri, attKeyword);
    Mapper.Add(DocumentAttri, attDocument);
    Mapper.Add(TypeAttri, attDataType);
    Mapper.Add(FunctionAttri, attCommon);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(ProcessorAttri, attDirective);
  end
end;

{ TSardFile }

procedure TSardFile.NewContent;
begin
  inherited NewContent;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TSardTendency);
    Categories.Add(TSardFileCategory.Create(TSardTendency, 'Sard', 'Sard lang'));
    Groups.Add(TSardFile, 'sard', 'Sard', TSardFileCategory, ['.sard'], [fgkAssociated, fgkFolding, fgkBrowsable], [capExecute, capDebug]);
  end;
end.
