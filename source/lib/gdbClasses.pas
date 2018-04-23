unit gdbClasses;

{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
{**
TODO show gdb log
//→→M:\home\
set prompt >>>>>>gdb:

set new-console on
directory .
//set breakpoint pending on
//set unwindonsignal on
//set print elements 0
//catch throw

break "test.d:1"
run
next


http://wiki.freepascal.org/Debugging_-_GDB_tricks
ftp://ftp.gnu.org/old-gnu/Manuals/gdb/html_node/gdb_8.html
ftp://ftp.gnu.org/old-gnu/Manuals/gdb/html_node/gdb_211.html#SEC216
http://dirac.org/linux/gdb/06-Debugging_A_Running_Process.php
}
interface

uses
  Windows, Classes, SysUtils, StrUtils,
  mnClasses, syncobjs, process,
  mnUtils, mnStreams, ConsoleProcess, EditorEngine, EditorDebugger;

type
  TGDBDebug = class;

  TGDBBreakPointItem = class(TObject)
  public
    Info: TDebugBreakpointInfo;
  end;

  { TGDBBreakPointList }

  TGDBBreakPointList = class(specialize TmnObjectList<TGDBBreakPointItem>)
  private
  protected
  public
    function Add(FileName: String; Line: Integer): Integer; overload;
    function IndexOf(FileName: String; LineNo: Integer): Integer;
  end;

  { TGDBBreakPoints }

  TGDBBreakPoints = class(TEditorBreakPoints)
  protected
    FDebug: TGDBDebug;
    BreakPoints: TGDBBreakPointList;
    function GetCount: Integer; override;
    function GetItems(Index: Integer): TDebugBreakpointInfo; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Toggle(FileName: String; LineNo: Integer); override;
    function IsExists(FileName: String; LineNo: Integer): Boolean; override;
    procedure Add(FileName: String; LineNo: Integer); override;
    procedure Remove(FileName: String; Line: Integer); override; overload;
    procedure Remove(Handle: Integer); override; overload;
  end;

  { TGDBWatchItem }

  TGDBWatchItem = class(TObject)
  public
    Info: TDebugWatchInfo;
    property Name: String read Info.Name write Info.Name;
    property VarType: String read Info.VarType write Info.VarType;
    property Value: Variant read Info.Value write Info.Value;
  end;

  { TGDBWatchList }

  TGDBWatchList = class(specialize TmnNamedObjectList<TGDBWatchItem>)
  private
  protected
  public
    function Add(Name: String; VarType: String): Integer; overload;
  end;

  { TGDBWatches }

  TGDBWatches = class(TEditorWatches)
  protected
    FDebug: TGDBDebug;
    Watches: TGDBWatchList;
    function GetCount: Integer; override;
    function GetItems(Index: Integer): TDebugWatchInfo; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Add(vName: String); override;
    procedure Remove(vName: String); override;
    function GetValue(vName: String; out vValue: Variant; out vType: String; EvalIt: Boolean): Boolean; override;
  end;


  { TgdbAction }

  TgdbAction = class(TDebugCommand)
  private
  protected
    FTransactionID: Integer;
    procedure CheckError(ARespond: TStringList);
    procedure DoExecute(ARespond: TStringList); virtual; abstract;
    procedure Execute(ARespond: TStringList);
  public
  end;

  TgdbActionClass = class of TgdbAction;

  { TgdbSpool }

  TgdbSpool = class(specialize TmnObjectList<TgdbAction>)
  private
  public
    procedure Added(Action: TgdbAction); override;
    procedure Add(Action: TgdbAction);
  end;

  { TgdbInit }

  TgdbInit = class(TgdbAction)
  protected
    procedure Created; override;
  public
    function GetCommand: String; override;
    procedure DoExecute(ARespond: TStringList); override;
  end;

  { TgdbSet }

  TgdbSet = class(TgdbAction)
  protected
    FName: String;
    FValue: String;
    procedure DoExecute(ARespond: TStringList); override;
  public
    constructor CreateBy(vName, vValue: String);
    function GetCommand: String; override;
  end;

  { TgdbCommand }

  TgdbCommand = class(TgdbAction)
  protected
    FName: String;
    FValue: String;
    procedure DoExecute(ARespond: TStringList); override;
  public
    constructor CreateBy(vName: String; vValue: String = '');
    function GetCommand: String; override;
    function GetData: String; override;
  end;

  { TgdbRun }

  TgdbRun = class(TgdbAction)
  public
    function GetCommand: String; override;
    procedure DoExecute(ARespond: TStringList); override;
  end;

  { TgdbContinue }

  TgdbContinue = class(TgdbAction)
  public
    function GetCommand: String; override;
    procedure DoExecute(ARespond: TStringList); override;
  end;

  { TmnSpoolThread }

  TmnSpoolThread = class(TThread)
  private
  protected
    FDebug: TGDBDebug;
    FProcess: TProcess;
    FSpool: TgdbSpool;
    FTransactionID: Integer;
    ReadStream: TmnWrapperStream;
    function ReadRespond: TStringList;
    function NewTransactionID: Integer;
    function SendCommand(Command: String; Data: String): Integer;
    function PopAction: TgdbAction;
    procedure CreateProcess;
    procedure DoExecute;
    property Spool: TgdbSpool read FSpool;
  public
    constructor Create(ADebug: TGDBDebug);
    destructor Destroy; override;
    procedure Execute; override;

    property Debug: TGDBDebug read FDebug;
    property Process: TProcess read FProcess;
  end;

  { TGDBDebug }

  TGDBDebug = class(TEditorDebugger)
  protected
    FRunCount: Integer;
    FActive: Boolean;
    FSpoolThread: TmnSpoolThread;
    FQueue: TgdbSpool;
    function CreateBreakPoints: TEditorBreakPoints; override;
    function CreateWatches: TEditorWatches; override;

    procedure Resume;
    procedure AddAction(vAction: TgdbAction); overload;
    procedure AddAction(vActionClass: TgdbActionClass); overload;
    procedure RemoveAction(vAction: TgdbAction);
    procedure ExtractAction(vAction: TgdbAction);
    procedure Clear;
    property Queue: TgdbSpool read FQueue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start; override;
    procedure Attach(SubProcess: TProcess); override;
    procedure Stop; override;
    procedure Next;
    procedure Run;
    procedure Step;
    function GetState: TDebugStates; override;
    procedure Action(AAction: TDebugAction); override;
    function GetKey: String; override;
    property RunCount: Integer read FRunCount; //count of waiting action
  end;

implementation

uses
  EditorClasses;

{ TmnSpoolThread }

constructor TmnSpoolThread.Create(ADebug: TGDBDebug);
begin
  inherited Create(True);
  FDebug := ADebug;
  FSpool := TgdbSpool.Create;
end;

destructor TmnSpoolThread.Destroy;
begin
  FreeAndNil(FSpool);
  FreeAndNil(ReadStream);
  FDebug := nil;
  inherited Destroy;
end;

procedure TmnSpoolThread.Execute;
begin
  CreateProcess;
  while not Terminated and Process.Active do
    DoExecute;
  if Process.Active then
    Process.Terminate(0);
  FreeAndNil(FProcess);
end;

{ TgdbContinue }

function TgdbContinue.GetCommand: String;
begin
  Result := 'continue';
end;

procedure TgdbContinue.DoExecute(ARespond: TStringList);
begin
end;

{ TgdbCommand }

procedure TgdbCommand.DoExecute(ARespond: TStringList);
begin
end;

constructor TgdbCommand.CreateBy(vName, vValue: String);
begin
  Create;
  FName := vName;
  FValue := vValue;
end;

function TgdbCommand.GetCommand: String;
begin
  Result := FName;
end;

function TgdbCommand.GetData: String;
begin
  Result := FValue;
end;

{ TgdbSpool }

procedure TgdbSpool.Added(Action: TgdbAction);
begin
  inherited Added(Action);
end;

procedure TgdbSpool.Add(Action: TgdbAction);
begin
  inherited Add(Action);
end;

{ TgdbRun }

function TgdbRun.GetCommand: String;
begin
  Result := 'run';
end;

procedure TgdbRun.DoExecute(ARespond: TStringList);
begin
end;

{ TgdbSet }

procedure TgdbSet.DoExecute(ARespond: TStringList);
begin
end;

constructor TgdbSet.CreateBy(vName, vValue: String);
begin
  Create;
  FName := vName;
  FValue := vValue;
end;

function TgdbSet.GetCommand: String;
begin
  // 'set prompt gdb:';
  Result := 'set ' + FName + ' ' + FValue;
end;

{ TgdbInit }

procedure TgdbInit.Created;
begin
  inherited Created;
  Flags := Flags - [dafSend];
end;

function TgdbInit.GetCommand: String;
begin
  Result := '';
end;

procedure TgdbInit.DoExecute(ARespond: TStringList);
begin
end;

{ TgdbAction }

procedure TgdbAction.CheckError(ARespond: TStringList);
begin
end;

procedure TgdbAction.Execute(ARespond: TStringList);
begin
end;

{ TGDBWatchList }

function TGDBWatchList.Add(Name: String; VarType: String): Integer;
var
  aItem: TGDBWatchItem;
begin
  aItem := TGDBWatchItem.Create;
  aItem.Info.Name := Name;
  aItem.Info.VarType := VarType;
  Result := inherited Add(aItem);
end;

{ TGDBBreakPointList }

function TGDBBreakPointList.Add(FileName: String; Line: Integer): Integer;
var
  aItem: TGDBBreakPointItem;
begin
  aItem := TGDBBreakPointItem.Create;
  aItem.Info.FileName := FileName;
  aItem.Info.Line := Line;
  Result := inherited Add(aItem);
end;

function TGDBBreakPointList.IndexOf(FileName: String; LineNo: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  if FileName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Info.FileName, FileName) and (Items[i].Info.Line = LineNo) then
      begin
        Result := i;
        break;
      end;
    end;
end;

{ TGDBWatches }

function TGDBWatches.GetCount: Integer;
begin
  Result := Watches.Count;
end;

function TGDBWatches.GetItems(Index: Integer): TDebugWatchInfo;
begin
  Result := Watches.Items[Index].Info;
end;

constructor TGDBWatches.Create;
begin
  inherited;
  Watches := TGDBWatchList.Create;
end;

destructor TGDBWatches.Destroy;
begin
  FreeAndNil(Watches);
  inherited Destroy;
end;

procedure TGDBWatches.Clear;
begin
  Watches.Clear;
end;

procedure TGDBWatches.Add(vName: String);
begin
  Watches.Add(vName, '');
end;

procedure TGDBWatches.Remove(vName: String);
var
  i: Integer;
begin
  i := Watches.IndexOfName(vName);
  if i >= 0 then
    Watches.Delete(i);
end;

function TGDBWatches.GetValue(vName: String; out vValue: Variant; out vType: String; EvalIt: Boolean): Boolean;
var
  aItem: TGDBWatchItem;
begin
  Result := False;
  aItem := Watches.Find(vName);
  if aItem <> nil then
  begin
    vValue := aItem.Value;
    vType := aItem.VarType;
    Result := True;
  end
  else
  begin
    vValue := Null;
    vType := '';
  end;
end;

{ TGDBBreakPoints }

function TGDBBreakPoints.GetCount: Integer;
begin
  Result := BreakPoints.Count;
end;

function TGDBBreakPoints.GetItems(Index: Integer): TDebugBreakpointInfo;
begin
  Result := BreakPoints[Index].Info;
end;

constructor TGDBBreakPoints.Create;
begin
  inherited;
  BreakPoints := TGDBBreakPointList.Create;
end;

destructor TGDBBreakPoints.Destroy;
begin
  FreeAndNil(BreakPoints);
  inherited Destroy;
end;

procedure TGDBBreakPoints.Clear;
begin
  BreakPoints.Clear;
end;

procedure TGDBBreakPoints.Toggle(FileName: String; LineNo: Integer);
var
  i: Integer;
begin
  i := BreakPoints.IndexOf(FileName, LineNo);
  if i >= 0 then
    BreakPoints.Delete(i)
  else
    BreakPoints.Add(FileName, LineNo);
end;

function TGDBBreakPoints.IsExists(FileName: String; LineNo: Integer): Boolean;
begin
  Result := BreakPoints.IndexOf(FileName, LineNo) >= 0;
end;

procedure TGDBBreakPoints.Add(FileName: String; LineNo: Integer);
begin
  if IsExists(FileName, LineNo) then
    raise Exception.Create('Breakpoint Already exists');
  BreakPoints.Add(FileName, LineNo);
end;

procedure TGDBBreakPoints.Remove(FileName: String; Line: Integer);
var
  i: Integer;
begin
  i := BreakPoints.IndexOf(FileName, Line);
  if i >= 0 then
    BreakPoints.Delete(i);
end;

procedure TGDBBreakPoints.Remove(Handle: Integer);
var
  i, f: Integer;
begin
  f := -1;
  for i := 0 to Count - 1 do
  begin
    if BreakPoints[i].Info.Handle = Handle then
    begin
      f := i;
      break;
    end;
  end;
  if f >= 0 then
    BreakPoints.Delete(i);
end;

{ TGDBDebug }

function TGDBDebug.CreateBreakPoints: TEditorBreakPoints;
begin
  Result := TGDBBreakPoints.Create;
  (Result as TGDBBreakPoints).FDebug := Self;
end;

function TGDBDebug.CreateWatches: TEditorWatches;
begin
  Result := TGDBWatches.Create;
  (Result as TGDBWatches).FDebug := Self;
end;

procedure TGDBDebug.Resume;
begin
  DebugManager.Event.SetEvent;
end;

procedure TGDBDebug.AddAction(vAction: TgdbAction);
begin
  DebugManager.Lock.Enter;
  try
    Queue.Add(vAction);
  finally
    DebugManager.Lock.Leave;
  end;
end;

procedure TGDBDebug.AddAction(vActionClass: TgdbActionClass);
begin
  AddAction(vActionClass.Create);
end;

procedure TGDBDebug.RemoveAction(vAction: TgdbAction);
begin
  DebugManager.Lock.Enter;
  try
    Queue.Remove(vAction);
  finally
    DebugManager.Lock.Leave;
  end;
end;

procedure TGDBDebug.ExtractAction(vAction: TgdbAction);
begin
  DebugManager.Lock.Enter;
  try
    Queue.Extract(vAction);
  finally
    DebugManager.Lock.Leave;
  end;
end;

procedure TGDBDebug.Clear;
begin
  DebugManager.Lock.Enter;
  try
    Queue.Clear;
  finally
    DebugManager.Lock.Leave;
  end;
end;

constructor TGDBDebug.Create;
begin
  inherited;
  FQueue := TgdbSpool.Create(True);
end;

destructor TGDBDebug.Destroy;
begin
  FreeAndNil(FQueue);
  inherited Destroy;
end;

{ TmnSpoolThread }

function TmnSpoolThread.ReadRespond: TStringList;

  function ParseMI(const S: String): String;
  var
    c: String;
  begin
    if s = '' then
      Result := ''
    else
    begin
      c := S[1];
      Result := MidStr(S, 2, MaxInt);
      MidStr(S, 2, MaxInt);
      if S[1] = '^' then
        Result := Result
      else if c = '&' then
        Result := DescapeStringC(DequoteStr(Result))
      else if c = '*' then
        Result := DescapeStringC(DequoteStr(Result))
      else if c = '~' then
        Result := DescapeStringC(DequoteStr(Result))
      else if c = '=' then
        Result := DescapeStringC(DequoteStr(Result))
      else
        Result := S;
    end;
  end;

var
  s: String;
begin
  Result := TStringList.Create;
  while ReadStream.ReadLine(S, True, #13#10) do
  begin
    S := ParseMI(S);
    Result.Add(s);
    //Engine.SendLog(S);
    OutputDebugString(PChar('<[MNE]' + S));
    if trim(s) = '(gdb)' then
      break;
  end;
end;

function TmnSpoolThread.NewTransactionID: Integer;
begin
  Inc(FTransactionID);
  Result := FTransactionID;
end;

function TmnSpoolThread.SendCommand(Command: String; Data: String): Integer;
var
  s: String;
begin
  Result := NewTransactionID;
  s := Command;
  if Data <> '' then
    s := s + ' ' + Data;
  //Engine.SendLog(S);
  OutputDebugString(PChar('>[MNE]' + S));
  s := s + #13#10;
  Process.Input.WriteBuffer(S[1], Length(S));
end;

function TmnSpoolThread.PopAction: TgdbAction;
var
  aAction: TgdbAction;
begin
  if FSpool.Count = 0 then
  begin
    InterLockedIncrement(Debug.FRunCount);
    DebugManager.Event.WaitFor(INFINITE); //wait the ide to make resume
    InterLockedDecrement(Debug.FRunCount);

    DebugManager.Lock.Enter;
    try
      while Debug.Queue.Count > 0 do
      begin
        aAction := Debug.Queue.Extract(Debug.Queue[0]);
        Spool.Add(aAction);
      end;
    finally
      DebugManager.Lock.Leave;
    end;
  end;
  Result := nil;
  while not Terminated and ((FSpool.Count > 0) and (Result = nil)) do
  begin
    Result := FSpool[0];
    Result.Prepare;
  end;
end;

procedure TmnSpoolThread.CreateProcess;
begin
  if FProcess = nil then
  begin
    FProcess := TProcess.Create(nil);
    FProcess.ConsoleTitle := 'GDB';
    {$ifdef windows}
    FProcess.Executable := 'gdb.exe';
    {$else}
    FProcess.Executable := 'gdb';
    {$endif}
    FProcess.Parameters.Add('-q');//"Quiet". Do not print the introductory and copyright messages.
    FProcess.Parameters.Add('-n');//Do not execute commands found in any initialization files.
    FProcess.Parameters.Add('-f');//Full name GDB output the full file name and line number in a standard.
    FProcess.Parameters.Add('--interpreter=mi2');//GDB/MI
    //FProcess.Parameters.Add('-annotate 1');
    //-nw not work with mi
    //FProcess.Parameters.Add('-nw');//"No windows".
    //FProcess.Parameters.Add('-noasync');//Disable the asynchronous event loop for the command-line interface.


    FProcess.CurrentDirectory := ExtractFilePath(ParamStr(0));
    FProcess.Options := [poUsePipes, poStderrToOutPut];
    FProcess.ShowWindow := swoHIDE;
    FProcess.PipeBufferSize := 0;

    FProcess.Execute;
  end;
end;

procedure TmnSpoolThread.DoExecute;
var
  aAction: TgdbAction;
  aRespond: TStringList;
  aCommand: String;
  aKeep: Boolean;
begin
  ReadStream := TmnWrapperStream.Create(Process.Output, False);
  aAction := PopAction;
  if aAction <> nil then
  begin
    if not aAction.Enabled then
      FSpool.Remove(aAction)
    else
      try
        aCommand := aAction.GetCommand;
        if (dafSend in aAction.Flags) and (aCommand <> '') then
          aAction.FTransactionID := SendCommand(aCommand, aAction.GetData);
        if aAction.Accept and not Terminated then
        begin
          aRespond := ReadRespond;
          try
            aAction.Execute(aRespond);
          finally
            FreeAndNil(aRespond);
          end;
        end;
      finally
        if not aAction.Stay then
        begin
          aKeep := (aAction.Event <> nil) or aAction.KeepAlive;
          if aAction.Event <> nil then
            aAction.Event.SetEvent;
          if aKeep then
            FSpool.Extract(aAction)
          else
            FSpool.Remove(aAction);
        end;
      end;
  end;
end;

procedure TGDBDebug.Start;
begin
  if FSpoolThread = nil then
  begin
    FSpoolThread := TmnSpoolThread.Create(Self);
    FSpoolThread.FreeOnTerminate := False;
    FSpoolThread.Start;
    //AddAction(TgdbSet.CreateBy('prompt', '(gdb)'#13));
    AddAction(TgdbInit.Create);
    AddAction(TgdbSet.CreateBy('language', 'pascal'));
    AddAction(TgdbSet.CreateBy('confirm', 'off'));
    AddAction(TgdbSet.CreateBy('annotate', '3'));
    //AddAction((TgdbSet.CreateBy('new-console', 'on'));
    AddAction(TgdbSet.CreateBy('verbose', 'off'));
    //AddAction(TgdbSet.CreateBy('print', 'symbol-filename off'));
    Resume;
  end;
end;

procedure TGDBDebug.Attach(SubProcess: TProcess);
var
  i: Integer;
begin
  //TgdbSet.CreateBy('new-console', 'on');
  AddAction(TgdbCommand.CreateBy('cd', SubProcess.CurrentDirectory));
  AddAction(TgdbCommand.CreateBy('directory', SubProcess.CurrentDirectory));
  AddAction(TgdbCommand.CreateBy('attach', IntToStr(SubProcess.ProcessID)));
  Resume;
  SubProcess.Resume;
  for i := 0 to Breakpoints.Count - 1 do
    AddAction(TgdbCommand.CreateBy('break', '"' + StringReplace(Breakpoints[i].FileName, '\', '/', [rfReplaceAll]) + '":' + IntToStr(Breakpoints[i].Line)));
  AddAction(TgdbContinue.Create);
  Resume;
end;

procedure TGDBDebug.Stop;
begin
  if FSpoolThread <> nil then
  begin
    AddAction(TgdbCommand.CreateBy('kill'));
    AddAction(TgdbCommand.CreateBy('q'));
    Resume;
    if not FSpoolThread.Finished then
      FSpoolThread.Terminate;
    FSpoolThread.WaitFor;
    FreeAndNil(FSpoolThread);
  end;
end;

procedure TGDBDebug.Next;
begin
  AddAction(TgdbCommand.CreateBy('next'));
  Resume;
end;

procedure TGDBDebug.Run;
begin
  AddAction(TgdbCommand.CreateBy('run'));
  Resume;
end;

procedure TGDBDebug.Step;
begin
  AddAction(TgdbCommand.CreateBy('step'));
  Resume;
end;

function TGDBDebug.GetState: TDebugStates;
begin
  Result := [];
  if FActive then
    Result := Result + [dbsActive];
  if FSpoolThread <> nil then
    Result := Result + [dbsDebugging, dbsRunning];
end;

procedure TGDBDebug.Action(AAction: TDebugAction);
begin
  case AAction of
    dbaActivate:
      FActive := True;
    //Start;
    dbaDeactivate:
    begin
      Stop;
      FActive := False;
    end;
    dbaReset:
      Stop;
    dbaRun:
      Run;
    dbaStepInto:
      Step;
    dbaStepOver:
      Next;
  end;
end;

function TGDBDebug.GetKey: String;
begin
  Result := 'GDB'; //Return version of debugger
end;

end.
