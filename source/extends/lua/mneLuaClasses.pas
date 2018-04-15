unit mneLuaClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics,
  Contnrs, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  FileUtil, mnSynHighlighterLua, EditorDebugger, EditorClasses, mneClasses,
  mneCompilerProjectFrames, EditorRun, mneConsoleClasses, LuaDBGServers,
  mneConsoleForms, mneRunFrames;

type

  { TLuaFile }

  TLuaFile = class(TSourceEditorFile)
  protected
  public
    procedure NewContent; override;
    procedure OpenInclude; override;
    function CanOpenInclude: Boolean; override;
  end;

  { TLuaFileCategory }

  TLuaFileCategory = class(TCodeFileCategory)
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

  { TLuaProjectOptions }

  TLuaProjectOptions = class(TEditorProjectOptions)
  private
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
  end;

  { TLuaTendency }

  TLuaTendency = class(TEditorTendency)
  private
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Init; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    procedure CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack); override;
    function CreateOptions: TEditorProjectOptions; override;
    procedure EnumRunCommands(Items: TStrings); override;
    function GetDefaultGroup: TFileGroup; override;
  published

  end;

implementation

uses
  {lua53,}IniFiles, mnStreams, mnUtils, mnSynHighlighterMultiProc, SynEditStrConst, mneLuaProjectFrames;

{ TDProject }

procedure TLuaProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  (aFrame as TRunFrameOptions).Options := AProject.RunOptions;
  AProject.Tendency.EnumRunCommands((aFrame as TRunFrameOptions).CommandEdit.Items);
  aFrame.Caption := 'Run';
  AddFrame(aFrame);
  aFrame := TLuaProjectFrame.Create(AOwner);
  (aFrame as TLuaProjectFrame).Project := AProject;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

{ TLuaFile }

procedure TLuaFile.NewContent;
begin
  //SynEdit.Text := cLuaSample;
end;

{ TLuaFile }

procedure TLuaFile.OpenInclude;
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
    if Engine.Files.Current.Group.Category is TLuaFileCategory then
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

function TLuaFile.CanOpenInclude: Boolean;
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
    if Group.Category is TLuaFileCategory then
    begin
      P := SynEdit.CaretXY;
      aToken := '';
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      //Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

{ TLuaTendency }

procedure TLuaTendency.DoRun(Info: TmneRunInfo);
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
    if RunOptions.Require <> '' then
        aRunItem.Info.Run.AddParam('-l '+ RunOptions.Require);
    if rnaDebug in Info.Actions then
        aRunItem.Info.Run.AddParam('-e '+ '"require(''mobdebug'').start()"'); //using mobdebug

    aRunItem.Info.Run.Command := Info.Command;
    if Info.Command = '' then
    begin
      {$ifdef windows}
        aRunItem.Info.Run.Command := 'lua.exe';
      {$else}
        aRunItem.Info.Run.Command := 'lua';
      {$endif}
      aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
    end
  end
  else if (rnaLint in Info.Actions) or (rnaCompile in Info.Actions) then
  begin
    Engine.SendAction(eaClearOutput);

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
        aRunItem.Info.Run.Command := 'luac.exe';
      {$else}
        aRunItem.Info.Run.Command := 'luac';
      {$endif}
      aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
    //end
  end;

  if Engine.Session.Run.Active then //if there is items ready to run
    Engine.Session.Run.Start(Self);
end;

function TLuaTendency.GetDefaultGroup: TFileGroup;
begin
  Result :=Groups.Find('lua');
end;

constructor TLuaTendency.Create;
begin
  inherited Create;
end;

procedure TLuaTendency.CreateOptionsFrame(AOwner: TComponent; ATendency: TEditorTendency; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := ATendency.RunOptions;
  EnumRunCommands(aFrame.CommandEdit.Items);
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

function TLuaTendency.CreateDebugger: TEditorDebugger;
begin
  Result := TLuaDBGDebug.Create;
end;

function TLuaTendency.CreateOptions: TEditorProjectOptions;
begin
  Result := TLuaProjectOptions.Create;
end;

procedure TLuaTendency.EnumRunCommands(Items: TStrings);
begin
  inherited;
  Items.Add('lua "?file"');
  Items.Add('lua -l "tyro" "?file"');
  Items.Add('love ?mainpath "?mainfile"');
  Items.Add('lovec ?mainpath "?mainfile"');
  Items.Add('lovec "?root" "?mainfile"');
end;

procedure TLuaTendency.Init;
begin
  FCapabilities := [capDebug, capTrace, capDebugServer, capRun, capCompile, capLint, capOptions];
  Title := 'Lua Lang';
  FDescription := 'Lua Files, *.lua';
  FName := 'Lua';
  OutputExtension := '.luac';
  FImageIndex := -1;
  AddGroup('cfg', 'cfg');
  AddGroup('ini', 'ini');
  AddGroup('txt', 'txt');
end;

{ TLuaFileCategory }

function TLuaFileCategory.DoCreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmnSynLuaSyn.Create(nil);
end;

procedure TLuaFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
  IdentifierID := ord(tkIdentifier);
end;

procedure TLuaFileCategory.DoAddKeywords;
begin
  EnumerateKeywords(Ord(tkKeyword), sLuaKeywords, Highlighter.IdentChars, @DoAddCompletion);
  EnumerateKeywords(Ord(tkFunction), sLuaFunctions, Highlighter.IdentChars, @DoAddCompletion);
end;

procedure TLuaFileCategory.DoFixTabsSpaces(Sender: TObject);
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

procedure TLuaFileCategory.EnumMenuItems(AddItems: TAddClickCallBack);
begin
  //AddItems('FixTabsSpaces', 'Fix Tabs/Spaces', @DoFixTabsSpaces);
end;

procedure TLuaFileCategory.InitMappers;
begin
  with Highlighter as TmnSynLuaSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment);
    Mapper.Add(KeywordAttri, attKeyword);
    Mapper.Add(TypeAttri, attDataType);
    Mapper.Add(DocumentAttri, attDocument);
    Mapper.Add(FunctionAttri, attStandard);
    Mapper.Add(IdentifierAttri, attIdentifier);
    Mapper.Add(TextAttri, attText);
    Mapper.Add(NumberAttri, attNumber);
    Mapper.Add(StringAttri, attQuotedString);
    Mapper.Add(SymbolAttri, attSymbol);
    Mapper.Add(VariableAttri, attVariable);
    Mapper.Add(ProcessorAttri, attDirective);
  end;
end;

initialization
  with Engine do
  begin
    Tendencies.Add(TLuaTendency);
    Categories.Add(TLuaFileCategory.Create(TLuaTendency, 'Lua', [fckPublish]));
    Groups.Add(TLuaFile, 'lua', 'Lua', TLuaFileCategory, ['lua'], [fgkAssociated, fgkExecutable, fgkBrowsable, fgkMain]);
  end;
end.
