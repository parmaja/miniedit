unit debugpyServers;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license   GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey
 * @ref       http://xdebug.org/docs-debugpy.php#id1
 *}
{

  https://stackoverflow.com/questions/41169047/no-pydevd-protocol-specification
  https://pypi.org/project/trepan3k/

}

{$ifdef WINDOWS}
{.$DEFINE SAVELOG}
{$endif}

interface

uses
  SysUtils, StrUtils, Classes, Contnrs, Dialogs, Variants,
  SyncObjs, IniFiles, Base64,
  EditorRun,
  mnClasses, mnSockets, mnStreams, mnConnections, mnServers,
  mnXMLUtils, mnXMLRttiProfile, mnXMLNodes;

type
  TdebugpyServer = class;
  TdebugpyConnection = class;
  TdebugpyConnectionClass = class of TdebugpyConnection;

  TdebugpyXMLRespond = class(TmnXMLNodes)
  public
    Source: string;
  end;

  { TdebugpyAction }

  TdebugpyAction = class(TDebugServerAction)
  private
  protected
    procedure DoExecute; virtual; abstract;
    procedure Execute;
    function GetCommand: Integer; virtual; abstract;
    function SendCommand(Command: Integer; Data: string): integer;
    function ReadRespond: Boolean; virtual;
  public
    procedure Process; override;
  end;

  TdebugpyXMLAction = class(TdebugpyAction)
  protected
    Respond: TdebugpyXMLRespond;
  public
    function ReadRespond: Boolean; override;
  end;

  TdebugpyActionClass = class of TdebugpyAction;

  { TdebugpyInit }

  TdebugpyInit = class(TdebugpyXMLAction)
  protected
    procedure Created; override;
  public
    function GetCommand: Integer; override;
    procedure DoExecute; override;
  end;

  { TdebugpyStepOver }

  TdebugpyStepOver = class(TdebugpyAction)
  public
    function GetCommand: Integer; override;
    procedure DoExecute; override;
  end;

  { TdebugpyRun }

  TdebugpyRun = class(TdebugpyAction)
  public
    function GetCommand: Integer; override;
    procedure DoExecute; override;
  end;

  { TdebugpyConnection }

  TdebugpyConnection = class(TDebugConnection)
  private
  public
  protected
    procedure Prepare; override;
  public
  published
  end;

  { TdebugpyListener }

  TdebugpyListener = class(TDebugListener)
  private
  protected
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; override;
  public
  end;

  { TdebugpyServer }

  TdebugpyServer = class(TDebugServer)
  private
  protected
    function GetIsRuning: Boolean; override;
    function CreateListener: TmnListener; override;
    procedure WatchAdded; override;
    procedure BreakPointAdded; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  TdebugpyDebugger = class;

  { TdebugpyDebuggerBreakPoints }

  TdebugpyDebuggerBreakPoints = class(TEditorBreakPoints)
  protected
    FDebug: TdebugpyDebugger;
    FBreakpoints: TDebugBreakpoints; //Cache of undeleted breakpoints
    function GetCount: integer; override;
    function GetItems(Index: integer): TDebugBreakpointInfo; override;
    procedure UpdateBreakpoints;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Toggle(FileName: string; LineNo: integer); override;
    function IsExists(FileName: string; LineNo: integer): boolean; override;
    procedure Add(FileName: string; LineNo: integer); override;
    procedure Remove(Handle: integer); override; overload;
    procedure Remove(FileName: string; Line: integer); override; overload;
  end;

  { TdebugpyDebuggerWatches }

  TdebugpyDebuggerWatches = class(TEditorWatches)
  protected
    FDebug: TdebugpyDebugger;
    function GetCount: integer; override;
    function GetItems(Index: integer): TDebugWatchInfo; override;
  public
    procedure Clear; override;
    procedure Add(vName: string); override;
    procedure Remove(vName: string); override;
    //function GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean; override;
  end;

  { TdebugpyDebugger }

  TdebugpyDebugger = class(TEditorDebugger)
  private
    FServer: TdebugpyServer;
  protected
    function CreateBreakPoints: TEditorBreakPoints; override;
    function CreateWatches: TEditorWatches; override;

    procedure Reset;
    procedure Resume;
    procedure StepOver;
    procedure StepInto;
    procedure StepOut;
    procedure Run;
    property Server: TdebugpyServer read FServer;
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
  EditorEngine;

//https://github.com/JetBrains/intellij-community/blob/master/python/helpers/pydev/_pydevd_bundle/pydevd_comm.py

const
  CMD_RUN = 101;
  CMD_LIST_THREADS = 102;
  CMD_THREAD_CREATE = 103;
  CMD_THREAD_KILL = 104;
  CMD_THREAD_SUSPEND = 105;
  CMD_THREAD_RUN = 106;
  CMD_STEP_INTO = 107;
  CMD_STEP_OVER = 108;
  CMD_STEP_RETURN = 109;
  CMD_GET_VARIABLE = 110;
  CMD_SET_BREAK = 111;
  CMD_REMOVE_BREAK = 112;
  CMD_EVALUATE_EXPRESSION = 113;
  CMD_GET_FRAME = 114;
  CMD_EXEC_EXPRESSION = 115;
  CMD_WRITE_TO_CONSOLE = 116;
  CMD_CHANGE_VARIABLE = 117;
  CMD_RUN_TO_LINE = 118;
  CMD_RELOAD_CODE = 119;
  CMD_GET_COMPLETIONS = 120;

  // Note: renumbered (conflicted on merge)
  CMD_CONSOLE_EXEC = 121;
  CMD_ADD_EXCEPTION_BREAK = 122;
  CMD_REMOVE_EXCEPTION_BREAK = 123;
  CMD_LOAD_SOURCE = 124;
  CMD_ADD_DJANGO_EXCEPTION_BREAK = 125;
  CMD_REMOVE_DJANGO_EXCEPTION_BREAK = 126;
  CMD_SET_NEXT_STATEMENT = 127;
  CMD_SMART_STEP_INTO = 128;
  CMD_EXIT = 129;
  CMD_SIGNATURE_CALL_TRACE = 130;

  CMD_SET_PY_EXCEPTION = 131;
  CMD_GET_FILE_CONTENTS = 132;
  CMD_SET_PROPERTY_TRACE = 133;
  // Pydev debug console commands
  CMD_EVALUATE_CONSOLE_EXPRESSION = 134;
  CMD_RUN_CUSTOM_OPERATION = 135;
  CMD_GET_BREAKPOINT_EXCEPTION = 136;
  CMD_STEP_CAUGHT_EXCEPTION = 137;
  CMD_SEND_CURR_EXCEPTION_TRACE = 138;
  CMD_SEND_CURR_EXCEPTION_TRACE_PROCEEDED = 139;
  CMD_IGNORE_THROWN_EXCEPTION_AT = 140;
  CMD_ENABLE_DONT_TRACE = 141;
  CMD_SHOW_CONSOLE = 142;

  CMD_GET_ARRAY = 143;
  CMD_STEP_INTO_MY_CODE = 144;
  CMD_GET_CONCURRENCY_EVENT = 145;
  CMD_SHOW_RETURN_VALUES = 146;

  CMD_GET_THREAD_STACK = 152;
  CMD_THREAD_DUMP_TO_STDERR = 153;  // This is mostly for unit-tests to diagnose errors on ci.
  CMD_STOP_ON_START = 154;
  CMD_GET_EXCEPTION_DETAILS = 155;
  CMD_PYDEVD_JSON_CONFIG = 156;

  CMD_THREAD_SUSPEND_SINGLE_NOTIFICATION = 157;
  CMD_THREAD_RESUME_SINGLE_NOTIFICATION = 158;

  CMD_STEP_OVER_MY_CODE = 159;
  CMD_STEP_RETURN_MY_CODE = 160;

  //CMD_SET_PY_EXCEPTION = 161;
  CMD_SET_PATH_MAPPING_JSON = 162;

  CMD_GET_SMART_STEP_INTO_VARIANTS = 163;  // XXX: PyCharm has 160 for this (we're currently incompatible anyways).

  CMD_REDIRECT_OUTPUT = 200;
  CMD_GET_NEXT_STATEMENT_TARGETS = 201;
  CMD_SET_PROJECT_ROOTS = 202;

  CMD_AUTHENTICATE = 205;

  CMD_VERSION = 501;
  CMD_RETURN = 502;
  CMD_SET_PROTOCOL = 503;
  CMD_ERROR = 901;

  REASON_CAUGHT_EXCEPTION = CMD_STEP_CAUGHT_EXCEPTION;
  REASON_UNCAUGHT_EXCEPTION = CMD_ADD_EXCEPTION_BREAK;
  REASON_STOP_ON_BREAKPOINT = CMD_SET_BREAK;
  REASON_THREAD_SUSPEND = CMD_THREAD_SUSPEND;
  REASON_STEP_INTO = CMD_STEP_INTO;
  REASON_STEP_INTO_MY_CODE = CMD_STEP_INTO_MY_CODE;
  REASON_STOP_ON_START = CMD_STOP_ON_START;
  REASON_STEP_RETURN = CMD_STEP_RETURN;
  REASON_STEP_RETURN_MY_CODE = CMD_STEP_RETURN_MY_CODE;
  REASON_STEP_OVER = CMD_STEP_OVER;
  REASON_STEP_OVER_MY_CODE = CMD_STEP_OVER_MY_CODE;

  // Always True (because otherwise when we do have an error, it's hard to diagnose).
  SHOW_WRITES_AND_READS = True;
  SHOW_OTHER_DEBUG_INFO = True;
  SHOW_STDOUT = True;

function FormatCommand(CMD, SEQ: Integer; Msg: string = ''): string;
begin
  Result := CMD.ToString+#9+SEQ.ToString;
  Result := Result + #9 + Msg;
end;

{ TdebugpyAction }

procedure TdebugpyAction.Execute;
begin
  DoExecute;
end;

function TdebugpyAction.SendCommand(Command: Integer; Data: string): integer;
var
  s: string;
begin
  Result := Connection.NewTransactionID;
  s := FormatCommand(Command,Result,Data);
  Connection.Stream.WriteUTF8Line(s);
{$IFDEF SAVELOG}
  SaveLog(s);
{$ENDIF}
end;

function TdebugpyAction.ReadRespond: Boolean;
var
  header: string;
  line: string;
begin
  Connection.Stream.ReadLineUTF8(header);
  Connection.Stream.ReadLineUTF8(line);
end;

procedure TdebugpyAction.Process;
var
  aCommand: Integer;
begin
  aCommand := GetCommand;
  if (dafSend in Flags) and (aCommand <> 0) then
    FTransactionID := SendCommand(aCommand, GetData);
  if Accept and Connection.Connected then
  begin
    if dafRespond in Flags then
    begin
      try
        if ReadRespond then
        begin
          if Connection.Connected then
            Execute;
        end
        else
          if Connection.Connected then
            Execute;
      finally
      end;
    end;
  end;
end;

{TdebugpyXMLAction}

function TdebugpyXMLAction.ReadRespond: Boolean;
var
  Reader: TmnXMLNodeReader;
  s: string;
  aMatched: boolean;
begin
  Result := False;
  Connection.Stream.ReadLineUTF8(s);
  if Connection.Connected and aMatched and (S <> '') then
  begin
    Respond := TdebugpyXMLRespond.Create;
    Connection.Stream.ReadUntil(#0, true, s, aMatched);
    s := Trim(s);
    {$IFDEF SAVELOG}
    SaveLog(s);
    {$ENDIF}
    Respond.Source := s;
    Reader := TmnXMLNodeReader.Create;
    try
      Reader.Start;
      Reader.Nodes := Respond;
      Reader.Parse(s);
    finally
      Reader.Free;
    end;
    Result := True;
  end;
end;

{ TdebugpyCommandSet }

constructor TdebugpyServer.Create;
begin
  inherited;
  Port := '5678';
end;

destructor TdebugpyServer.Destroy;
begin
  inherited;
end;

function TdebugpyServer.GetIsRuning: Boolean;
begin
  Result := RunCount > 0;
end;

{ TdebugpyGetCurrent }

{ TdebugpySocketServer }

function TdebugpyServer.CreateListener: TmnListener;
begin
  Result := TdebugpyListener.Create;
end;

procedure TdebugpyServer.WatchAdded;
begin
  inherited;
//  Queue.Add(TdebugpyGetWatches.Create);
//  Queue.Add(TdebugpyGetCurrent.Create);
end;

procedure TdebugpyServer.BreakPointAdded;
begin
  inherited;
//  Queue.Add(TdebugpyGetWatches.Create);
//  Queue.Add(TdebugpyGetCurrent.Create);
end;


{$IFDEF SAVELOG}
procedure TdebugpyConnection.SaveLog(s: string);
var
  aStrings: TStringList;
  aStream: TFileStream;
  i: integer;
const
  sFile = 'c:\xdebug_server.log';
begin
  aStrings := TStringList.Create;
  aStrings.Text := s;
  if FileExists(sFile) then
  begin
    aStream := TFileStream.Create(sFile, fmOpenWrite);
    aStream.Seek(0, soFromEnd);
  end
  else
    aStream := TFileStream.Create(sFile, fmCreate);

  try
    for i := 0 to aStrings.Count - 1 do
    begin
      s := aStrings[i] + #13;
      aStream.Write(s[1], Length(s));
    end;
  finally
    aStream.Free;
    aStrings.Free;
  end;
end;

{$ENDIF}

procedure TdebugpyConnection.Prepare;
begin
  inherited;
  Server.Breakpoints.Clean;

  Queue.Add(TdebugpyInit.Create);

//  Queue.Add(TdebugpySetBreakpoints.Create);
{
  if Server.BreakOnFirstLine then
  begin
    Queue.Add(TdebugpyStepInto.Create);
    Queue.Add(TdebugpyGetCurrent.Create);
  end
  else
  begin
    Queue.Add(TdebugpyRun.Create);
    Queue.Add(TdebugpyGetWatches.Create);
    Queue.Add(TdebugpyGetCurrent.Create);
  end;}
end;

{ TdebugpyListener }

function TdebugpyListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TdebugpyConnection.Create(Self, vStream);
end;

{ TdebugpyStepOver }

function TdebugpyStepOver.GetCommand: Integer;
begin
  Result := CMD_STEP_OVER;
end;

procedure TdebugpyStepOver.DoExecute;
begin
end;

{ TdebugpyInit }

procedure TdebugpyInit.Created;
begin
  inherited;
  Flags := Flags - [dafRespond];
end;

function TdebugpyInit.GetCommand: Integer;
begin
  Result := CMD_VERSION;
end;

procedure TdebugpyInit.DoExecute;
begin
  DebugManager.Enter;
  try
    Connection.Server.Watches.Clean;
    //Connection.Key := Respond.Root.Attributes['idekey'];
  finally
    DebugManager.Leave;
  end;
end;

{ TdebugpyRun }

function TdebugpyRun.GetCommand: Integer;
begin
  Result := CMD_RUN;
end;

procedure TdebugpyRun.DoExecute;
begin
end;

{$IFDEF SAVELOG}
procedure SaveLog(s: string);
var
  aStrings: TStringList;
  aStream: TFileStream;
  i: integer;
const
  sFile = 'c:\lock_server.log';
begin
  aStrings := TStringList.Create;
  aStrings.Text := s;
  if FileExists(sFile) then
  begin
    aStream := TFileStream.Create(sFile, fmOpenWrite);
    aStream.Seek(0, soFromEnd);
  end
  else
    aStream := TFileStream.Create(sFile, fmCreate);

  try
    for i := 0 to aStrings.Count - 1 do
    begin
      s := aStrings[i] + #13;
      aStream.Write(s[1], Length(s));
    end;
  finally
    aStream.Free;
    aStrings.Free;
  end;
end;
{$ENDIF}

{ TdebugpyDebuggerBreakPoints }

function TdebugpyDebuggerBreakPoints.GetCount: integer;
begin
  Result := FBreakpoints.Count;
end;

function TdebugpyDebuggerBreakPoints.GetItems(Index: integer): TDebugBreakpointInfo;
var
  aBP: TDebugBreakpoint;
begin
  aBP := FBreakpoints[Index];
  Result.FileName := aBP.FileName;
  Result.Handle := aBP.Handle;
  Result.Line := aBP.Line;
end;

procedure TdebugpyDebuggerBreakPoints.UpdateBreakpoints;
var
  aItem: TDebugBreakpoint;
begin
  FBreakpoints.Clear;
  for aItem in FDebug.FServer.Breakpoints do
    if not aItem.Deleted then
      FBreakpoints.Add(aItem);
end;

constructor TdebugpyDebuggerBreakPoints.Create;
begin
  inherited Create;
  FBreakpoints := TDebugBreakpoints.Create(False);
end;

destructor TdebugpyDebuggerBreakPoints.Destroy;
begin
  FreeAndNil(FBreakpoints);
  inherited Destroy;
end;

procedure TdebugpyDebuggerBreakPoints.Clear;
begin
  with FDebug.FServer do
    Breakpoints.Clear;
  UpdateBreakpoints;
end;

procedure TdebugpyDebuggerBreakPoints.Toggle(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Toggle(FileName, LineNo);
  UpdateBreakpoints;
end;

function TdebugpyDebuggerBreakPoints.IsExists(FileName: string; LineNo: integer): boolean;
begin
  Result := FBreakpoints.Find(FileName, LineNo) <> nil;
end;

procedure TdebugpyDebuggerBreakPoints.Add(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Add(FileName, LineNo);
  UpdateBreakpoints;
end;

procedure TdebugpyDebuggerBreakPoints.Remove(FileName: string; Line: integer);
var
  aBP: TDebugBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints.Find(FileName, Line);

  if aBP <> nil then
      Remove(aBP.Handle);
end;

procedure TdebugpyDebuggerBreakPoints.Remove(Handle: integer);
begin
  with FDebug.FServer do
    Breakpoints.Remove(Handle);
  UpdateBreakpoints;
end;

{ TdebugpyDebuggerWatches }

function TdebugpyDebuggerWatches.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Watches.Count;
end;

function TdebugpyDebuggerWatches.GetItems(Index: integer): TDebugWatchInfo;
var
  aWt: TDebugWatch;
begin
  with FDebug.FServer do
    aWt := Watches[Index];
  Result:= aWt.Info;
end;

procedure TdebugpyDebuggerWatches.Clear;
begin
  with FDebug.FServer do
    Watches.Clear;
end;

procedure TdebugpyDebuggerWatches.Add(vName: string);
begin
  with FDebug.FServer do
    Watches.AddWatch(vName);
end;

procedure TdebugpyDebuggerWatches.Remove(vName: string);
begin
  with FDebug.FServer do
    Watches.RemoveWatch(vName);
end;

{function TdebugpyDebuggerWatches.GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean;
var
  aAction: TdebugpyCustomGet;
begin
  Result := False;
  if dbsRunning in FDebug.GetState then   //there is a connection from XDebug
  begin
    if EvalIt then
      aAction := TdebugpyEval.Create
    else
      aAction := TdebugpyGetWatchInstance.Create;
    aAction.CreateEvent;
    aAction.Info.Name := vName;
    with FDebug.FServer do
    begin
      AddAction(aAction);

      Resume;

      aAction.Event.WaitFor(30000);
      vValue := aAction.Info.Value;
      vType := aAction.Info.VarType;

      ExtractAction(aAction);
      aAction.Free;

      Result := True;
    end;
  end;
end;}

{ TdebugpyDebugger }

function TdebugpyDebugger.CreateBreakPoints: TEditorBreakPoints;
begin
  Result := TdebugpyDebuggerBreakPoints.Create;
  (Result as TdebugpyDebuggerBreakPoints).FDebug := Self;
end;

function TdebugpyDebugger.CreateWatches: TEditorWatches;
begin
  Result := TdebugpyDebuggerWatches.Create;
  (Result as TdebugpyDebuggerWatches).FDebug := Self;
end;

constructor TdebugpyDebugger.Create;
begin
  inherited Create;
  FServer := TdebugpyServer.Create;
  //FServer.FDebug := Self;
end;

destructor TdebugpyDebugger.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

procedure TdebugpyDebugger.Action(AAction: TDebugAction);
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

function TdebugpyDebugger.GetState: TDebugStates;
begin
  Result := [];
  if FServer.Active then
    Result := Result + [dbsActive];
  if FServer.IsRuning then
    Result := Result + [dbsRunning, dbsDebugging];
end;

procedure TdebugpyDebugger.Start;
begin
  inherited;
  FServer.Start;
end;

procedure TdebugpyDebugger.Stop;
{var
  aAction: TdebugpyDetach;}
begin
  inherited;
  if FServer.IsRuning then
  begin
    FServer.Clear;
    //aAction := TdebugpyDetach.Create;
    //aAction.CreateEvent;
    //FServer.AddAction(aAction);
    //FServer.Resume;
    //aAction.Event.WaitFor(30000);
    //FServer.ExtractAction(aAction);
    //aAction.Free;
  end;
  FServer.Stop;
end;

procedure TdebugpyDebugger.Reset;
begin
  FServer.Clear; //no need to any exists actions
  //FServer.AddAction(TdebugpyStop.Create);
  //FServer.AddAction(TdebugpyGetCurrent.Create);
  FServer.Resume;
end;

procedure TdebugpyDebugger.Resume;
begin
  //FServer.AddAction(TdebugpyDetach.Create);
  //FServer.AddAction(TdebugpyGetCurrent.Create);
  FServer.Resume;
end;

procedure TdebugpyDebugger.StepInto;
begin
  //FServer.AddAction(TdebugpySetBreakpoints.Create);
  //FServer.AddAction(TdebugpyStepInto.Create);
  //FServer.AddAction(TdebugpyGetWatches.Create);
  //FServer.AddAction(TdebugpyGetCurrent.Create);
  FServer.Resume;
end;

procedure TdebugpyDebugger.StepOver;
begin
  //FServer.AddAction(TdebugpySetBreakpoints.Create);
  //FServer.AddAction(TdebugpyStepOver.Create);
  //FServer.AddAction(TdebugpyGetWatches.Create);
  //FServer.AddAction(TdebugpyGetCurrent.Create);
  FServer.Resume;
end;

procedure TdebugpyDebugger.StepOut;
begin
  //FServer.AddAction(TdebugpySetBreakpoints.Create);
  //FServer.AddAction(TdebugpyStepOut.Create);
  //FServer.AddAction(TdebugpyGetWatches.Create);
  //FServer.AddAction(TdebugpyGetCurrent.Create);
  FServer.Resume;
end;

procedure TdebugpyDebugger.Run;
begin
  //FServer.AddAction(TdebugpySetBreakpoints.Create);
  FServer.AddAction(TdebugpyRun.Create);
  //FServer.AddAction(TdebugpyGetWatches.Create);
  //FServer.AddAction(TdebugpyGetCurrent.Create);
  FServer.Resume;
end;

function TdebugpyDebugger.GetKey: string;
begin
  Result := FServer.Key;
end;

end.

