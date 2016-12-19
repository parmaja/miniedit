unit mnePyClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
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
  Contnrs, FileUtil, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  SynHighlighterPython, EditorDebugger, EditorClasses, mneClasses,
  mneCompilerProjectFrames, EditorRun, mneConsoleClasses, dbgpServers,
  mneConsoleForms;

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
    function DoCreateHighlighter: TSynCustomHighlighter; override;
    procedure InitCompletion(vSynEdit: TCustomSynEdit); override;
    procedure DoAddKeywords; override;
  public
    procedure EnumMenuItems(AddItems: TAddClickCallBack); override;
  end;

  { TPyProjectOptions }

  TPyProjectOptions = class(TCompilerProjectOptions)
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
    procedure Init; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
    function CreateOptions: TEditorProjectOptions; override;
  published
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, mnSynHighlighterMultiProc, SynEditStrConst, mneCompilerTendencyFrames, mnePyProjectFrames;

{ TDProject }

procedure TPyProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectFrame.Create(AOwner);
  (aFrame as TCompilerProjectFrame).FProject := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);
  aFrame := TPyProjectFrame.Create(AOwner);
  (aFrame as TPyProjectFrame).FProject := AProject;
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
  Options: TPyProjectOptions;
  aRunItem: TmneRunItem;
begin
  if (Engine.Session.Project.Options is TPyProjectOptions) then
    Options := (Engine.Session.Project.Options as TPyProjectOptions)
  else
    Options := TPyProjectOptions.Create;//Default options

  Engine.Session.Run.Clear;

  if rnaCompile in Info.Actions then
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

    aRunItem.Info.Run.Mode := Info.Mode;
    aRunItem.Info.Run.Pause := Info.Pause;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    aRunItem.Info.Message := 'Runing ' + Info.MainFile;
    aRunItem.Info.Run.Params := {'-m pyxdebug ' + }Info.MainFile + #13;
  end
  else if rnaExecute in Info.Actions then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Message := 'Running ' + Info.OutputFile;
    aRunItem.Info.Run.Mode := Info.Mode;
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Run.Pause := Options.RunPause;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.OutputFile);;
    aRunItem.Info.Run.Command := ChangeFileExt(Info.OutputFile, '.exe');
    if Options.RunParams <> '' then
      aRunItem.Info.Run.Params := aRunItem.Info.Run.Params + Options.RunParams + #13;
  end;

  Engine.Session.Run.Start;
end;

constructor TPyTendency.Create;
begin
  inherited Create;
end;

procedure TPyTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TCompilerTendencyFrame;
begin
  aFrame := TCompilerTendencyFrame.Create(AOwner);
  aFrame.FTendency := ATendency;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

function TPyTendency.CreateDebugger: TEditorDebugger;
begin
  Result := TdbgpDebug.Create;
end;

function TPyTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TPyProjectOptions.Create;
end;

procedure TPyTendency.Init;
begin
  FCapabilities := [capDebug, capTrace, capDebugServer, capRun, capCompile, capLink, capOptions];
  FTitle := 'Python Lang';
  FDescription := 'Python Files, *.py';
  FName := 'Python';
  FImageIndex := -1;
  AddGroup('cfg', 'cfg');
  AddGroup('ini', 'ini');
  AddGroup('txt', 'txt');
end;

{ TPyFileCategory }

function TPyFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TSynPythonSyn.Create(nil);
end;

procedure TPyFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
  IdentifierID := ord(SynHighlighterPython.tkIdentifier);
end;

procedure TPyFileCategory.DoAddKeywords;
begin
  //this a hack to lazarus source, just make GetKeywordIdentifiers public
  Completion.ItemList.Assign((Highlighter as TSynPythonSyn).GetKeywordIdentifiers);
  //EnumerateKeywords(Ord(tkKeyword), sDKeywords, Highlighter.IdentChars, @DoAddCompletion);
  //EnumerateKeywords(Ord(tkFunction), sDFunctions, Highlighter.IdentChars, @DoAddCompletion);
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
  AddItems('FixTabsSpaces', 'Fix Tabs/Spaces', @DoFixTabsSpaces);
end;

procedure TPyFileCategory.InitMappers;
begin
  with Highlighter as TSynPythonSyn do
  begin
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(KeyAttri, attKeyword);
    Mapper.Add(NonKeyAttri, attDefault);
    Mapper.Add(SystemAttri, attDefault);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(HexAttri, attQuotedString);
    Mapper.Add(OctalAttri, attNumber);
    Mapper.Add(FloatAttri, attNumber);
    Mapper.Add(SpaceAttri, attDefault);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(DocStringAttri, attDocument);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(ErrorAttri, attDefault);
  end;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TPyTendency);
    Categories.Add(TPyFileCategory.Create(TPyTendency, 'Python', [fckPublish]));
    Groups.Add(TPyFile, 'py', 'Python', TPyFileCategory, ['py'], [fgkAssociated, fgkExecutable, fgkBrowsable, fgkMain]);
  end;
end.
