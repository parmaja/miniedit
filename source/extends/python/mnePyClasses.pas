unit mnePyClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}
{*
TODO
  add keywords
    With
    Self
*}
interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics,
  Contnrs, LazFileUtils, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterPython, EditorClasses, mneClasses,
  mneCompilerProjectFrames, EditorRun, dbgpServers,
  mneRunFrames;

type

  { TPyFile }

  TPyFile = class(TSourceEditorFile)
  protected
  public
    procedure NewContent; override;
    procedure OpenInclude; override;
    function CanOpenInclude: Boolean; override;
  end;

  { TPyFileCategory }

  TPyFileCategory = class(TCodeFileCategory)
  private
  protected
    procedure DoFixTabsSpaces(Sender: TObject);
    procedure InitMappers; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;
  public
    function CreateHighlighter: TSynCustomHighlighter; override;
    procedure EnumMenuItems(AddItems: TAddClickCallBack); override;
  end;

  { TPyProjectOptions }

  TPyProjectOptions = class(TEditorProjectOptions)
  private
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
  end;

  { TPyTendency }

  TPyTendency = class(TEditorTendency)
  private
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Created; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
    function CreateOptions: TEditorProjectOptions; override;
  published
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, mnSynHighlighterMultiProc, SynEditStrConst, mnePyProjectFrames;

{ TDProject }

procedure TPyProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectFrame.Create(AOwner);
  (aFrame as TCompilerProjectFrame).Project := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);
  aFrame := TPyProjectFrame.Create(AOwner);
  (aFrame as TPyProjectFrame).Project := AProject;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

{ TPyFile }

procedure TPyFile.NewContent;
begin
  //SynEdit.Text := cPySample;
end;

{ TPyFile }

procedure TPyFile.OpenInclude;
var
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aToken: string;
  aTokenType: integer;
  aStart: integer;

  function TryOpen: boolean;
  begin
    if (aToken[1] = '/') or (aToken[1] = '\') then
      aToken := RightStr(aToken, Length(aToken) - 1);
    aToken := Engine.ExpandFile(aToken);
    Result := FileExists(aToken);
    if Result then
      Engine.Files.OpenFile(aToken);
  end;

begin
  inherited;
  if Engine.Files.Current <> nil then
  begin
    if Engine.Files.Current.Group.Category is TPyFileCategory then
    begin
      P := SynEdit.CaretXY;
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      aToken := DequoteStr(aToken);
      //if (aToken <> '') and (TtkTokenKind(aTokenType) = tkString) then
      begin
        aToken := StringReplace(aToken, '/', '\', [rfReplaceAll, rfIgnoreCase]);
        if not TryOpen then
        begin
          aToken := ExtractFileName(aToken);
          TryOpen;
        end;
      end;
    end;
  end;
end;

function TPyFile.CanOpenInclude: Boolean;
var
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aToken: string;
  aTokenType: integer;
  aStart: integer;
begin
  Result := False;
  if (Group <> nil) then
  begin
    if Group.Category is TPyFileCategory then
    begin
      P := SynEdit.CaretXY;
      aToken := '';
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      //Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

{ TPyTendency }

procedure TPyTendency.DoRun(Info: TmneRunInfo);
var
  aRunItem: TmneRunItem;
begin
  Engine.SendAction(eaClearOutput);
  Engine.Session.Run.Clear;

  if rnaExecute in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Run.Console := Info.Console;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.OutputFile);;
    aRunItem.Info.Run.Command := Info.Command;
    if Info.Command = '' then
    begin
      {$ifdef windows}
        aRunItem.Info.Run.Command := 'python.exe';
      {$else}
        aRunItem.Info.Run.Command := 'python';
      {$endif}
    end;
    aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
    aRunItem.Info.Run.AddParam(RunOptions.Params);
    aRunItem.Info.StatusMessage := 'Running ' + Info.OutputFile;
  end
  else if rnaCompile in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;

    aRunItem.Info.Run.Command := Info.Command;
    if aRunItem.Info.Run.Command = '' then
    begin
      {$ifdef windows}
      aRunItem.Info.Run.Command := 'python.exe';
      {$else}
      aRunItem.Info.Run.Command := 'python';
      {$endif}
    end;

    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aRunItem.Info.StatusMessage := 'Runing ' + Info.MainFile;
    //{'-m pyxdebug ' + }
    aRunItem.Info.Run.AddParam(Info.MainFile);
  end;

  Engine.Session.Run.Start(Debugger);
end;

constructor TPyTendency.Create;
begin
  inherited Create;
end;

procedure TPyTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := ATendency.RunOptions;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

function TPyTendency.CreateDebugger: TEditorDebugger;
begin
  Result := TdbgpDebugger.Create;
end;

function TPyTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TPyProjectOptions.Create;
end;

procedure TPyTendency.Created;
begin
  FCapabilities := [capDebug, capTrace, capDebugServer, capExecute, capCompile, capLink];
  FHaveOptions := True;
  FTitle := 'Python Lang';
  FDescription := 'Python Files, *.py';
  FName := 'Python';
  FImageIndex := -1;
  AddGroup('cfg', 'cfg');
  AddGroup('ini', 'ini');
  AddGroup('txt', 'txt');
end;

{ TPyFileCategory }

function TPyFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynPythonSyn.Create(nil);
end;

procedure TPyFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
end;

procedure TPyFileCategory.DoAddKeywords;
begin
  //this a hack to lazarus source, just make GetKeywordIdentifiers public
  EnumerateKeywords(Ord(attKeyword), (Highlighter as TSynPythonSyn).GetKeywordIdentifiers, Highlighter.IdentChars, @AddKeyword);
end;

procedure TPyFileCategory.DoFixTabsSpaces(Sender: TObject);
var
  S: string;
begin
  with (Engine.Files.Current as TSourceEditorFile) do
  begin
    SynEdit.BeginUndoBlock;
    try
      S := SynEdit.TextBetweenPoints[Point(1,1), Point(length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1, SynEdit.Lines.Count)];
      SynEdit.TextBetweenPoints[Point(1,1), Point(length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1, SynEdit.Lines.Count)] := ConvertIndents(S, SynEdit.TabWidth, Tendency.IndentMode);
      //SynEdit.Text := ConvertIndents(SynEdit.Text, SynEdit.TabWidth, Tendency.IndentMode);
    finally
      SynEdit.EndUndoBlock;
    end;
  end;
end;

procedure TPyFileCategory.EnumMenuItems(AddItems: TAddClickCallBack);
begin
  AddItems('FixTabsSpaces', 'Fix Tabs/Spaces', 'Edit', @DoFixTabsSpaces);
end;

procedure TPyFileCategory.InitMappers;
begin
  with Highlighter as TSynPythonSyn do
  begin
    Mapper.Add(CommentAttri, attComment, ord(tkComment));
    Mapper.Add(IdentifierAttri, attIdentifier, ord(tkIdentifier));
    Mapper.Add(KeyAttri, attKeyword, ord(tkKeyword));
    Mapper.Add(NonKeyAttri, attDefault);
    Mapper.Add(SystemAttri, attDefault);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(HexAttri, attQuotedString);
    Mapper.Add(OctalAttri, attNumber);
    Mapper.Add(FloatAttri, attNumber);
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(DocStringAttri, attDocument, ord(tkDocument));
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(ErrorAttri, attDefault);
  end;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TPyTendency);
    Categories.Add(TPyFileCategory.Create(TPyTendency, 'Python', 'Python', [fckPublish]));
    Groups.Add(TPyFile, 'py', 'Python', TPyFileCategory, ['.py'], [fgkAssociated, fgkBrowsable, fgkMain], [capExecute, capDebug]);
  end;
end.
