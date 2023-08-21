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
  mnSynHighlighterPy, EditorClasses,
	mneClasses, mnStreams, mnSynUtils, mnServers, mnConnections,
  mneCompilerProjectFrames, EditorRun,
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
    procedure SendMessage(S: string; vMessageType: TNotifyMessageType); override;
    procedure PrepareSynEdit(SynEdit: TSynEdit); override;
    procedure CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack); override;
    function CreateProjectOptions: TEditorProjectOptions; override;
  published
  end;

  TmnDebugPyListener = class;

  { TDebugPyConnection }

  TDebugPyConnection = class(TmnServerConnection)
  private
  protected
    function Listener: TmnDebugPyListener;
    procedure Process; override;
  public
    constructor Create(vConnector: TmnConnections; vStream: TmnConnectionStream);
    destructor Destroy; override;
  end;

  { TmnDebugPyListener }

  TmnDebugPyListener = class(TmnListener)
  private
  protected
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TDebugPyServer }

  TDebugPyServer = class(TmnServer)
  private
    FKey: string;
    function GetIsRuning: Boolean;
  protected
    function DoCreateListener: TmnListener; override;
  public
    property IsRuning: Boolean read GetIsRuning;

  end;

  { TDebugPyDebugger }

  TDebugPyDebugger = class(TEditorDebugger)
  private
    FServer: TDebugPyServer;
  protected
    function CreateBreakPoints: TEditorBreakPoints; override;
    function CreateWatches: TEditorWatches; override;

    procedure Reset;
    procedure Resume;
    procedure StepOver;
    procedure StepInto;
    procedure StepOut;
    procedure Run;
    property Server: TDebugPyServer read FServer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure Action(AAction: TDebugAction); override;
    function GetState: TDebugStates; override;
    function GetKey: string; override;
  end;

implementation

uses
  IniFiles, mnUtils, mnSynHighlighterMultiProc, SynEditStrConst, mnePyProjectFrames, debugpyServers;

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
    if rnaDebug in Info.Actions then
    begin
      aRunItem.Info.Run.AddParam('-m debugpy --connect localhost:9001');
      aRunItem.Environment.Add('PYDEVD_DISABLE_FILE_VALIDATION=1');
    end;
    aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
    aRunItem.Info.Run.AddParam(RunOptions.Params);
    aRunItem.Info.StatusMessage := 'Running ' + Info.OutputFile;
  end
	else if (rnaLint in Info.Actions) then
  begin
    aRunItem := Engine.Session.Run.Add;
    aRunItem.MessageType := msgtInteractive;
    aRunItem.Info.Title := ExtractFileNameWithoutExt(Info.MainFile);
    aRunItem.Info.CurrentDirectory := Info.Root;
    aRunItem.Info.Run.Silent := True;
    aRunItem.Info.Run.Console := False;

    aRunItem.Info.StatusMessage := 'Linting ' + Info.MainFile;
    aRunItem.Info.Run.AddParam(' --msg-template=''{abspath}::{category}::{msg_id}::{line:5d},{column}::{obj}::{msg}''');

    {$ifdef windows}
      aRunItem.Info.Run.Command := 'pylint';
    {$else}
      aRunItem.Info.Run.Command := 'lslint';
    {$endif}
    aRunItem.Info.Run.AddParam(' "' + Info.MainFile + '"');
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
end;

procedure TPyTendency.SendMessage(S: string; vMessageType: TNotifyMessageType);
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

procedure TPyTendency.PrepareSynEdit(SynEdit: TSynEdit);
begin
  inherited;
  SynEdit.Options := SynEdit.Options + [eoSpacesToTabs];
end;

constructor TPyTendency.Create;
begin
  inherited Create;
end;

procedure TPyTendency.CreateOptionsFrame(AOwner: TComponent; AddFrame: TAddFrameCallBack);
var
  aFrame: TRunFrameOptions;
begin
  aFrame := TRunFrameOptions.Create(AOwner);
  aFrame.Options := RunOptions;
  aFrame.Caption := 'Options';
  AddFrame(aFrame);
end;

function TPyTendency.CreateDebugger: TEditorDebugger;
begin
  Result := TDebugPyDebugger.Create;
end;

function TPyTendency.CreateProjectOptions: TEditorProjectOptions;
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

//https://github.com/JetBrains/intellij-community/blob/master/python/helpers/pydev/_pydevd_bundle/pydevd_comm.py

const
  DEBUGBY_RUN = 101;
  DEBUGBY_LIST_THREADS = 102;
  DEBUGBY_THREAD_CREATE = 103;
  DEBUGBY_THREAD_KILL = 104;
  DEBUGBY_THREAD_SUSPEND = 105;
  DEBUGBY_CMD_THREAD_RUN = 106;
  DEBUGBY_STEP_INTO = 107;
  DEBUGBY_STEP_OVER = 108;
  DEBUGBY_STEP_RETURN = 109;
  DEBUGBY_GET_VARIABLE = 110;
  DEBUGBY_SET_BREAK = 111;
  DEBUGBY_REMOVE_BREAK = 112;
  DEBUGBY_CMD_EVALUATE_EXPRESSION = 113;
  DEBUGBY_CMD_GET_FRAME = 114;
  DEBUGBY_CMD_EXEC_EXPRESSION = 115;
  DEBUGBY_CMD_WRITE_TO_CONSOLE = 116;
  DEBUGBY_CMD_CHANGE_VARIABLE = 117;
  DEBUGBY_CMD_RUN_TO_LINE = 118;
  DEBUGBY_CMD_RELOAD_CODE = 119;
  DEBUGBY_CMD_GET_COMPLETIONS = 120;
  DEBUGBY_CMD_REDIRECT_OUTPUT = 200;

function FormatCommand(CMD, SEQ: Integer; Msg: string = ''): string;
begin
  Result := CMD.ToString+#9+SEQ.ToString;
  Result := Result + #9 + Msg;
end;

{ TDebugPyConnection }

function TDebugPyConnection.Listener: TmnDebugPyListener;
begin
  Result := inherited Listener as TmnDebugPyListener;
end;

procedure TDebugPyConnection.Process;
var
  line: string;
begin
  Stream.WriteLineUTF8(FormatCommand(DEBUGBY_LIST_THREADS,1));
  if Stream.WaitToRead then
  begin
    try
      while Stream.Connected do
      begin
        Stream.ReadLineUTF8(line);
{        if line = '' then
          break;}
      end;
    finally
    end;
  end;
end;

constructor TDebugPyConnection.Create(vConnector: TmnConnections; vStream: TmnConnectionStream);
begin
  inherited;
end;

destructor TDebugPyConnection.Destroy;
begin
  inherited;
end;

{ TmnDebugPyListener }

function TmnDebugPyListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TDebugPyConnection.Create(Self, vStream);
  vStream.EndOfLine := sWinEndOfLine;
end;

constructor TmnDebugPyListener.Create;
begin
  inherited;
end;

destructor TmnDebugPyListener.Destroy;
begin
  inherited;
end;

{ TDebugPyServer }

function TDebugPyServer.GetIsRuning: Boolean;
begin
  Result := Count > 0;
end;

function TDebugPyServer.DoCreateListener: TmnListener;
begin
  Result := TmnDebugPyListener.Create;
end;

{ TDebugPyDebugger }

function TDebugPyDebugger.CreateBreakPoints: TEditorBreakPoints;
begin
  Result := nil;
end;

function TDebugPyDebugger.CreateWatches: TEditorWatches;
begin
  Result := nil;
end;

procedure TDebugPyDebugger.Reset;
begin

end;

procedure TDebugPyDebugger.Resume;
begin

end;

procedure TDebugPyDebugger.StepOver;
begin

end;

procedure TDebugPyDebugger.StepInto;
begin

end;

procedure TDebugPyDebugger.StepOut;
begin

end;

procedure TDebugPyDebugger.Run;
begin

end;

constructor TDebugPyDebugger.Create;
begin
  inherited Create;
  FServer := TDebugPyServer.Create;
  FServer.Port := '9001';
end;

destructor TDebugPyDebugger.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

procedure TDebugPyDebugger.Start;
begin
  FServer.Start;
end;

procedure TDebugPyDebugger.Stop;
begin
  inherited;
  if FServer.Active then
  begin
  end;
  FServer.Stop;
end;

procedure TDebugPyDebugger.Action(AAction: TDebugAction);
begin
  case AAction of
    dbaActivate: Start;
    dbaDeactivate: Stop;
    dbaReset: Reset;
    dbaResume: Resume;
    dbaStepInto: StepInto;
    dbaStepOver: StepOver;
    dbaStepOut: StepOut;
    dbaRun: Run;
  end;
end;

function TDebugPyDebugger.GetState: TDebugStates;
begin
  Result := [];
  if FServer.Active then
    Result := Result + [dbsActive];
  if FServer.IsRuning then
    Result := Result + [dbsRunning, dbsDebugging];
end;

function TDebugPyDebugger.GetKey: string;
begin
  Result :=inherited GetKey;
end;

{ TPyFileCategory }

function TPyFileCategory.CreateHighlighter: TSynCustomHighlighter;
begin
  Result := TmnSynPySyn.Create(nil);
end;

procedure TPyFileCategory.InitCompletion(vSynEdit: TCustomSynEdit);
begin
  inherited;
  FCompletion.EndOfTokenChr := '${}()[].<>/\:!&*+-=%;';
end;

procedure TPyFileCategory.DoAddKeywords;
begin
  SynHighlighterHashEntries.EnumerateKeywords(Ord(attKeyword), sPyKeywords, Highlighter.IdentChars, @AddKeyword);
  SynHighlighterHashEntries.EnumerateKeywords(Ord(attDataType), sPyTypes, Highlighter.IdentChars, @AddKeyword);
  SynHighlighterHashEntries.EnumerateKeywords(Ord(attDataValue), sPyValues, Highlighter.IdentChars, @AddKeyword);
  SynHighlighterHashEntries.EnumerateKeywords(Ord(attCommon), sPyFunctions, Highlighter.IdentChars, @AddKeyword);
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
      SynEdit.TextBetweenPoints[Point(1,1), Point(length(SynEdit.Lines[SynEdit.Lines.Count - 1]) + 1, SynEdit.Lines.Count)] := ConvertLineIndents(S, SynEdit.TabWidth, Tendency.IndentMode);
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
  with Highlighter as TmnSynPySyn do
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
    Tendencies.Add(TPyTendency);
    Categories.Add(TPyFileCategory.Create(TPyTendency, 'Python', 'Python', [fckPublish]));
    Groups.Add(TPyFile, 'py', 'Python', TPyFileCategory, ['.py'], [fgkAssociated, fgkBrowsable, fgkMain], [capExecute, capLint, capDebug]);
  end;
end.
