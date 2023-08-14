unit mneNimClasses;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}
{*
  @Ref:
	  https://nim-lang.org/docs/nimc.html
*}
interface

uses
  Messages, Forms, SysUtils, StrUtils, Variants, Classes, Controls, Graphics,
  Contnrs, LazFileUtils, LCLintf, LCLType, Dialogs, EditorOptions, SynEditHighlighter,
  SynEditSearch, SynEdit, Registry, EditorEngine, mnXMLRttiProfile, mnXMLUtils,
  SynEditTypes, SynCompletion, SynHighlighterHashEntries, EditorProfiles,
  mnSynUtils, mnSynHighlighterNim, EditorClasses, mneClasses,
  mneCompilerProjectFrames, EditorRun, mneResources,
  mneRunFrames, mneNimTendencyFrames;

type

  { TNimFile }

  TNimFile = class(TSourceEditorFile)
  protected
  public
    procedure NewContent; override;
    procedure OpenInclude; override;
    function CanOpenInclude: Boolean; override;
  end;

  { TNimFileCategory }

  TNimFileCategory = class(TCodeFileCategory)
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

  { TNimProjectOptions }

  TNimProjectOptions = class(TEditorProjectOptions)
  private
  public
    procedure CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack); override;
  published
  end;

  { TNimTendency }

  TNimTendency = class(TEditorTendency)
  private
    FRunWithCompileCommand: Boolean;
    FUseCfg: Boolean;
  protected
    function CreateDebugger: TEditorDebugger; override;
    procedure Created; override;
    procedure DoRun(Info: TmneRunInfo); override;
  public
    constructor Create; override;
    procedure SendMessage(S: string; vMessageType: TNotifyMessageType); override;
    procedure PrepareSynEdit(SynEdit: TSynEdit); override;
    procedure CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack); override;
    function CreateProjectOptions: TEditorProjectOptions; override;
  published
    property RunWithCompileCommand: Boolean read FRunWithCompileCommand write FRunWithCompileCommand default True;
    property UseCfg: Boolean read FUseCfg write FUseCfg default False;
  end;

implementation

uses
  IniFiles, mnStreams, mnUtils, mnSynHighlighterMultiProc, SynEditStrConst, mneNimProjectFrames;

{ TDProject }

procedure TNimProjectOptions.CreateOptionsFrame(AOwner: TComponent; AProject: TEditorProject; AddFrame: TAddFrameCallBack);
var
  aFrame: TFrame;
begin
  aFrame := TCompilerProjectFrame.Create(AOwner);
  (aFrame as TCompilerProjectFrame).Project := AProject;
  aFrame.Caption := 'Compiler';
  AddFrame(aFrame);

  aFrame := TNimProjectFrame.Create(AOwner);
  (aFrame as TNimProjectFrame).Project := AProject;
  aFrame.Caption := 'Nim Options';
  AddFrame(aFrame);
end;

{ TNimFile }

procedure TNimFile.NewContent;
begin
  SynEdit.Text := cNimSample;
end;

{ TNimFile }

procedure TNimFile.OpenInclude;
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
    if Engine.Files.Current.Group.Category is TNimFileCategory then
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

function TNimFile.CanOpenInclude: Boolean;
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
    if Group.Category is TNimFileCategory then
    begin
      P := SynEdit.CaretXY;
      aToken := '';
      SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      //Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

{ TNimTendency }

procedure TNimTendency.DoRun(Info: TmneRunInfo);
var
  i: Integer;
  aPath, aMainFile: string;
  aRunItem: TmneRunItem;
begin
  Engine.Session.Run.Clear;

  if (rnaCompile in Info.Actions) or (rnaLint in Info.Actions) then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.Info.Run.Silent := True;
    aRunItem.MessageType := msgtInteractive;

    aRunItem.Info.Run.Command := Info.Command;
    if aRunItem.Info.Run.Command = '' then
    begin
      {$ifdef windows}
        aRunItem.Info.Run.Command := 'nim.exe';
      {$else}
        aRunItem.Info.Run.Command := 'nim';
      {$endif}
    end;

    if (rnaLint in Info.Actions) then
    begin
      aRunItem.Info.Run.AddParam('check');
      aRunItem.Info.Run.Silent := True;
      aRunItem.Info.StatusMessage := 'Linting ' + Info.OutputFile;
    end
    else
    begin
      aRunItem.Info.Run.AddParam('c');
      if RunWithCompileCommand and (rnaExecute in Info.Actions) then
      begin
        aRunItem.Info.Run.AddParam('-r');
        aRunItem.Info.Run.Silent := False;
        aRunItem.Info.StatusMessage := 'Executing ' + Info.OutputFile;
      end
      else
      begin
        aRunItem.Info.Run.Silent := True;
        aRunItem.Info.StatusMessage := 'Compiling ' + Info.OutputFile;
      end;
    end;

    aRunItem.MessageType := msgtInteractive;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;

    if RunOptions.ConfigFile <> '' then
      aRunItem.Info.Run.AddParam('--config:' + ReplaceVariables(RunOptions.ConfigFile, []))
    else if UseCfg then
    begin
      if FileExists(ChangeFileExt(Info.MainFile, '.cfg')) then
        aRunItem.Info.Run.AddParam('--config:' + ExtractFileNameWithoutExt(ExtractFileName(Info.MainFile))+'.cfg');
    end;


    { TODO
		  if Info.OutputFile <> '' then
        aRunItem.Info.Run.AddParam('-o:' + Info.OutputFile);}

    if rnaDebug in Info.Actions then
    begin
      //aRunItem.Info.Run.AddParam('-d:Debug');
    end;

    for i := 0 to RunOptions.Paths.Count - 1 do
    begin
      aPath := Trim(RunOptions.Paths[i]);
      if aPath <>'' then
      begin
        if RunOptions.ExpandPaths then
          aPath := Engine.ExpandFile(aPath);
        if not DirectoryExists(aPath) then
          raise EEditorException.Create('Path not exists: ' + aPath);

        aRunItem.Info.Run.AddParam('-p:' + aPath);
      end;
    end;

    aMainFile := Info.MainFile;
    if RunOptions.ExpandPaths then
      aMainFile := Engine.ExpandFile(aMainFile);
    if not FileExists(aMainFile) then
      raise EEditorException.Create('File not exists: ' + aMainFile);

    aRunItem.Info.Run.AddParam('--verbosity:0');
    aRunItem.Info.Run.AddParam('--spellSuggest:0');

    aRunItem.Info.Run.AddParam(aMainFile);

    if rnaExecute in Info.Actions then
      aRunItem.Info.Run.AddParam(Engine.Session.Project.RunOptions.Params);
  end;

  if (rnaExecute in Info.Actions) then
  begin
    if not (RunWithCompileCommand and (rnaCompile in Info.Actions)) then
    begin
      aRunItem := Engine.Session.Run.Add;
      aRunItem.Info.Run.Pause := Info.Pause;
      aRunItem.Info.Run.Console := Info.Console;
      aRunItem.Info.CurrentDirectory := Info.Root;
      aRunItem.Info.StartDebug := rnaDebug in Info.Actions;
      aRunItem.Info.Title := ExtractFileName(Info.OutputFile);
      aRunItem.Info.Run.Command := Info.RunFile;
      aRunItem.Info.Run.AddParam(RunOptions.Params);
      aRunItem.Info.Run.AddParam(Engine.Session.Project.RunOptions.Params);
      aRunItem.Info.StatusMessage := 'Running ' + Info.OutputFile;
    end;
  end;
end;

procedure TNimTendency.SendMessage(S: string; vMessageType: TNotifyMessageType);
var
  aMsg: TMessageInfo;
  p: Integer;
  m, t : string;
  list: TStringList;
  function GetStr(Index: Integer): string;
  begin
    if Index >= list.Count then
      Result := ''
    else
      Result := List[Index];
  end;

begin
  //--msg-template='{abspath}::{category}::{msg_id}::{line:5d},{column}::{obj}::{msg}'
  if (S <> '') and (vMessageType = msgtInteractive) then
  begin
    aMsg := Default(TMessageInfo);
    list := TStringList.Create;
    try
      StrToStringsEx(Trim(S), List, ['::']);
      t := GetStr(3);
      if t <> '' then
      begin
        aMsg.Processed := True;
        aMsg.MessageType := vMessageType;
        aMsg.FileName := GetStr(0);
        aMsg.Name := GetStr(1);
        aMsg.Message := GetStr(6);

        t := GetStr(3);
        if LeftStr(t, 1) = '(' then
          t := Trim(MidStr(t, 2, Length(t) - 1));
        if RightStr(t, 1) = ')' then
          t := Trim(MidStr(t, 1, Length(t) - 1));

        p := Pos(',', t);
        if p > 0 then
        begin
          m := Trim(MidStr(t, 1, p - 1));
          t := Trim(MidStr(t, p + 1, MaxInt));
        end
        else
          m := t;
        aMsg.Line := StrToIntDef(m, 0);
        aMsg.Column := StrToIntDef(t, 0);

        if SameText(aMsg.Name, 'error') then
          aMsg.Kind := mskError
        else if SameText(aMsg.Name, 'convention') then
          aMsg.Kind := mskHint
        else if SameText(aMsg.Name, 'warning') then
          aMsg.Kind := mskWarning;
        Engine.SendMessage(S, aMsg);
      end
      else
        inherited;
    finally
      list.Free;
    end;
  end
  else
    inherited;
end;

procedure TNimTendency.PrepareSynEdit(SynEdit: TSynEdit);
begin
  inherited;
  SynEdit.Options := SynEdit.Options + [eoSpacesToTabs];
  SynEdit.TabWidth := 2; //*hmmmm
end;

constructor TNimTendency.Create;
begin
  inherited Create;
end;

procedure TNimTendency.CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
  aNimFrame: TNimTendencyFrame;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := RunOptions;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);

  aNimFrame := TNimTendencyFrame.Create(AOwner);
  aNimFrame.Tendency := Self;
  aNimFrame.Caption := 'Nim Options';
  AddFrame(aNimFrame);
end;

function TNimTendency.CreateDebugger: TEditorDebugger;
begin
  Result := nil;
end;

function TNimTendency.CreateProjectOptions: TEditorProjectOptions;
begin
  Result := TNimProjectOptions.Create;
end;

procedure TNimTendency.Created;
begin
  FCapabilities := [capDebug, capTrace, capDebugServer, capExecute, capCompile, capLink];
  FHaveOptions := True;
  FTitle := 'Nim Lang';
  FDescription := 'Nim Files, *.nim';
  FName := 'Nim';
  //AddGroup('cfg', 'cfg');
end;

{ TNimFileCategory }

function TNimFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmnSynNimSyn.Create(nil);
end;

procedure TNimFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
end;

procedure TNimFileCategory.DoAddKeywords;
begin
  SynHighlighterHashEntries.EnumerateKeywords(Ord(attKeyword), sNimKeywords, Highlighter.IdentChars, @AddKeyword);
  SynHighlighterHashEntries.EnumerateKeywords(Ord(attDataType), sNimTypes, Highlighter.IdentChars, @AddKeyword);
  SynHighlighterHashEntries.EnumerateKeywords(Ord(attDataValue), sNimValues, Highlighter.IdentChars, @AddKeyword);
  SynHighlighterHashEntries.EnumerateKeywords(Ord(attCommon), sNimFunctions, Highlighter.IdentChars, @AddKeyword);
end;

procedure TNimFileCategory.DoFixTabsSpaces(Sender: TObject);
var
  S: string;
begin
  with (Engine.Files.Current as TSourceEditorFile) do
  begin
    SynEdit.BeginUndoBlock;
    try
      S := SynEdit.TextBetweenPoints[Point(1,1), Point(length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1, SynEdit.Lines.Count)];
      SynEdit.TextBetweenPoints[Point(1,1), Point(length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1, SynEdit.Lines.Count)] := ConvertLineIndents(S, SynEdit.TabWidth, Tendency.IndentMode);
      //SynEdit.Text := ConvertIndents(SynEdit.Text, SynEdit.TabWidth, Tendency.IndentMode);
    finally
      SynEdit.EndUndoBlock;
    end;
  end;
end;

procedure TNimFileCategory.EnumMenuItems(AddItems: TAddClickCallBack);
begin
  AddItems('FixTabsSpaces', 'Fix Tabs/Spaces', 'Edit', @DoFixTabsSpaces);
end;

procedure TNimFileCategory.InitMappers;
begin
  with Highlighter as TmnSynNimSyn do
  begin
    Mapper.Add(WhitespaceAttri, attDefault);
    Mapper.Add(CommentAttri, attComment, ord(tkComment));
    Mapper.Add(KeywordAttri, attKeyword, ord(tkKeyword));
    Mapper.Add(TypeAttri, attDataType);
    Mapper.Add(ValueAttri, attDataValue);
    Mapper.Add(DocumentAttri, attDocument, ord(tkDocument));
    Mapper.Add(FunctionAttri, attCommon, ord(tkFunction));
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
    Tendencies.Add(TNimTendency);
    Categories.Add(TNimFileCategory.Create(TNimTendency, 'Nim', 'Nim', [fckPublish]));
    Groups.Add(TNimFile, 'nim', 'Nim', TNimFileCategory, ['.nim'], [fgkAssociated, fgkBrowsable, fgkMain], [capExecute, capLint, capDebug]);
  end;
end.
