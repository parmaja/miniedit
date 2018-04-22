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
}
interface

uses
  Classes, SysUtils, mnClasses, syncobjs, process,
  mnStreams, ConsoleProcess, EditorEngine, EditorDebugger;

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
    function Add(FileName: string; Line: integer): Integer; overload;
    function IndexOf(FileName: string; LineNo: integer): Integer;
  end;

  { TGDBBreakPoints }

  TGDBBreakPoints = class(TEditorBreakPoints)
  protected
    FDebug: TGDBDebug;
    BreakPoints: TGDBBreakPointList;
    function GetCount: integer; override;
    function GetItems(Index: integer): TDebugBreakpointInfo; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Toggle(FileName: string; LineNo: integer); override;
    function IsExists(FileName: string; LineNo: integer): boolean; override;
    procedure Add(FileName: string; LineNo: integer); override;
    procedure Remove(FileName: string; Line: integer); override; overload;
    procedure Remove(Handle: integer); override; overload;
  end;

  { TGDBWatchItem }

  TGDBWatchItem = class(TObject)
  public
    Info: TDebugWatchInfo;
    property Name: string read Info.Name write Info.Name;
    property VarType: string read Info.VarType write Info.VarType;
    property Value: Variant read Info.Value write Info.Value;
  end;

  { TGDBWatchList }

  TGDBWatchList = class(specialize TmnNamedObjectList<TGDBWatchItem>)
  private
  protected
  public
    function Add(Name: string; VarType: string): Integer; overload;
  end;

  { TGDBWatches }

  TGDBWatches = class(TEditorWatches)
  protected
    FDebug: TGDBDebug;
    Watches: TGDBWatchList;
    function GetCount: integer; override;
    function GetItems(Index: integer): TDebugWatchInfo; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Add(vName: string); override;
    procedure Remove(vName: string); override;
    function GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean; override;
  end;


  { TgdbAction }

  TgdbAction = class(TDebugCommand)
  private
  protected
    FTransactionID: integer;
    procedure CheckError(ARespond: TStringList);
    procedure Execute(ARespond: TStringList); virtual; abstract;
  public
  end;

  TdbgActionClass = class of TgdbAction;

  { TgdbSpool }

  TgdbSpool = class(specialize TmnObjectList<TgdbAction>)
  private
  public
    procedure Added(Action: TgdbAction); override;
  end;

  { TgdbInit }

  TgdbInit = class(TgdbAction)
  protected
    procedure Created; override;
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TStringList); override;
  end;

  { TgdbFeatureSet }

  TgdbFeatureSet = class(TgdbAction)
  protected
    FName: string;
    FValue: string;
    procedure Execute(Respond: TStringList); override;
  public
    constructor CreateBy(vName, vValue: string);
    function GetCommand: string; override;
  end;

  { TgdbCommandSet }

  TgdbCommand = class(TgdbAction)
  protected
    FName: string;
    FValue: string;
    procedure Execute(Respond: TStringList); override;
  public
    constructor CreateBy(vName: string; vValue: string = '');
    function GetCommand: string; override;
    function GetData: string; override;
  end;

  { TgdbRun }

  TgdbRun = class(TgdbAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TStringList); override;
  end;

  { TgdbContinue }

  TgdbContinue = class(TgdbAction)
  public
    function GetCommand: string; override;
    procedure Execute(Respond: TStringList); override;
  end;

  { TGDBDebug }

  TGDBDebug = class(TEditorDebugger)
  protected
    FActive: Boolean;
    FGDBProcess: TProcess;
    FRunCount: Integer;
    FSpool: TgdbSpool;
    //FConsoleThread: TmnConsoleThread;
    FTransactionID: integer;
    ReadStream: TmnWrapperStream;
    function CreateBreakPoints: TEditorBreakPoints; override;
    function CreateWatches: TEditorWatches; override;
    procedure ReceiveProcess(S: ansistring);
    procedure WriteProcess(S: ansistring);
    property GDBProcess: TProcess read FGDBProcess;

    function ReadRespond: TStringList;
    function NewTransactionID: integer;
    function SendCommand(Command: string; Data: string): integer;
    function PopAction: TgdbAction;
    procedure DoExecute;
    property Spool: TgdbSpool read FSpool;
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start; override;
    procedure Attach(SubProcess: TProcess; Resume: Boolean); override;
    procedure Stop; override;
    procedure Next;
    procedure Run;
    procedure Step;
    procedure Lock; override;
    procedure Unlock; override;
    function GetState: TDebugStates; override;
    procedure Action(AAction: TDebugAction); override;
    function GetKey: string; override;
  end;

implementation

uses
  EditorClasses;

{ TgdbContinue }

function TgdbContinue.GetCommand: string;
begin
  Result := 'continue';
end;

procedure TgdbContinue.Execute(Respond: TStringList);
begin

end;

{ TgdbCommand }

procedure TgdbCommand.Execute(Respond: TStringList);
begin

end;

constructor TgdbCommand.CreateBy(vName, vValue: string);
begin
  Create;
  FName := vName;
  FValue:= vValue;
end;

function TgdbCommand.GetCommand: string;
begin
  Result :=  FName;
end;

function TgdbCommand.GetData: string;
begin
  Result := FValue;
end;

{ TgdbSpool }

procedure TgdbSpool.Added(Action: TgdbAction);
begin
  inherited Added(Action);
end;

{ TgdbRun }

function TgdbRun.GetCommand: string;
begin

end;

procedure TgdbRun.Execute(Respond: TStringList);
begin

end;

{ TgdbFeatureSet }

procedure TgdbFeatureSet.Execute(Respond: TStringList);
begin

end;

constructor TgdbFeatureSet.CreateBy(vName, vValue: string);
begin
  Create;
  FName := vName;
  FValue:= vValue;
end;

function TgdbFeatureSet.GetCommand: string;
begin
  // 'set prompt gdb:';
  Result := 'set ' + FName + ' '+ FValue;
end;

{ TgdbInit }

procedure TgdbInit.Created;
begin
  inherited Created;
  Flags := Flags - [dafSend];
end;

function TgdbInit.GetCommand: string;
begin
  Result := '';
end;

procedure TgdbInit.Execute(Respond: TStringList);
begin
end;

{ TgdbAction }

procedure TgdbAction.CheckError(ARespond: TStringList);
begin
end;

{ TGDBWatchList }

function TGDBWatchList.Add(Name: string; VarType: string): Integer;
var
  aItem: TGDBWatchItem;
begin
  aItem := TGDBWatchItem.Create;
  aItem.Info.Name := Name;
  aItem.Info.VarType := VarType;
  Result := inherited Add(aItem);
end;

{ TGDBBreakPointList }

function TGDBBreakPointList.Add(FileName: string; Line: integer): Integer;
var
  aItem: TGDBBreakPointItem;
begin
  aItem := TGDBBreakPointItem.Create;
  aItem.Info.FileName := FileName;
  aItem.Info.Line := Line;
  Result := inherited Add(aItem);
end;

function TGDBBreakPointList.IndexOf(FileName: string; LineNo: integer): Integer;
var
  i: integer;
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

function TGDBWatches.GetCount: integer;
begin
  Result := Watches.Count;
end;

function TGDBWatches.GetItems(Index: integer): TDebugWatchInfo;
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

procedure TGDBWatches.Add(vName: string);
begin
  Watches.Add(vName, '');
end;

procedure TGDBWatches.Remove(vName: string);
var
  i: Integer;
begin
  i := Watches.IndexOfName(vName);
  if i >= 0 then
    Watches.Delete(i);
end;

function TGDBWatches.GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean;
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

function TGDBBreakPoints.GetCount: integer;
begin
  Result := BreakPoints.Count;
end;

function TGDBBreakPoints.GetItems(Index: integer): TDebugBreakpointInfo;
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

procedure TGDBBreakPoints.Toggle(FileName: string; LineNo: integer);
var
  i: Integer;
begin
  i := BreakPoints.IndexOf(FileName, LineNo);
  if i >= 0 then
    BreakPoints.Delete(i)
  else
    BreakPoints.Add(FileName, LineNo);
end;

function TGDBBreakPoints.IsExists(FileName: string; LineNo: integer): boolean;
begin
  Result := BreakPoints.IndexOf(FileName, LineNo) >= 0;
end;

procedure TGDBBreakPoints.Add(FileName: string; LineNo: integer);
begin
  if IsExists(FileName, LineNo) then
    raise Exception.Create('Breakpoint Already exists');
  BreakPoints.Add(FileName, LineNo);
end;

procedure TGDBBreakPoints.Remove(FileName: string; Line: integer);
var
  i: Integer;
begin
  i := BreakPoints.IndexOf(FileName, Line);
  if i >= 0 then
    BreakPoints.Delete(i);
end;

procedure TGDBBreakPoints.Remove(Handle: integer);
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
    BreakPoints.Delete(i)
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

procedure TGDBDebug.ReceiveProcess(S: ansistring);
begin
  //if LeftStr(S, 2) = '→→' then
    //Engine.SendOutout('Set line to :');
  Engine.SendMessage(S, msgtLog);
end;

procedure TGDBDebug.WriteProcess(S: ansistring);
begin
  S := S + #13#10;
  //Engine.SendMessage(S, msgtLog);
  FGDBProcess.Input.WriteBuffer(S[1], Length(S));
end;

function TGDBDebug.ReadRespond: TStringList;
var
  s: string;
begin
  Result := TStringList.Create;
  while ReadStream.ReadLine(S, true, #13#10) do
  begin
    Result.Add(s);
    if trim(s) = '(gdb)' then
      break;
  end;
end;

function TGDBDebug.NewTransactionID: integer;
begin
  Inc(FTransactionID);
  Result := FTransactionID;
end;

function TGDBDebug.SendCommand(Command: string; Data: string): integer;
var
  s: string;
begin
  Result := NewTransactionID;
  s := Command;
  if Data <> '' then
    s := s + ' ' + Data;
  WriteProcess(s);
{$IFDEF SAVELOG}
  SaveLog(s);
{$ENDIF}
end;

function TGDBDebug.PopAction: TgdbAction;
//var
//  aAction: TgdbAction;
//  i: integer;
begin
  if FSpool.Count = 0 then
  begin
    InterLockedIncrement(FRunCount);
//    DebugManager.Event.WaitFor(INFINITE); //wait the ide to make resume
    InterLockedDecrement(FRunCount);

    {DebugManager.Lock.Enter;
    try
      i := 0;
      while i < Server.Spool.Count do
      begin
        aAction := Server.Spool.Extract(Server.Spool[i]) as TgdbAction;
        //        if aAction.Key = Key then
        FLocalSpool.Add(aAction);
        //        else
        //        inc(i);
      end;
    finally
      DebugManager.Lock.Leave;
    end;}
  end;
  Result := nil;
  while {not Terminated and }((FSpool.Count > 0) and (Result = nil)) do
  begin
    Result := FSpool[0];
    Result.Prepare;
  end;
end;

procedure TGDBDebug.DoExecute;
var
  aAction: TgdbAction;
  aRespond: TStringList;
  aCommand: string;
  aKeep: Boolean;
begin
  ReadStream := TmnWrapperStream.Create(GDBProcess.Output, False);
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
        if aAction.Accept {and not Terminated} then
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

procedure TGDBDebug.Execute;
begin
  DoExecute;
end;

constructor TGDBDebug.Create;
begin
  inherited;
  FSpool := TgdbSpool.Create;
end;

destructor TGDBDebug.Destroy;
begin
  FreeAndNil(FSpool);
  FreeAndNil(ReadStream);
  inherited Destroy;
end;

procedure TGDBDebug.Start;
begin
  if FGDBProcess = nil then
  begin
    FGDBProcess := TProcess.Create(nil);
    FGDBProcess.ConsoleTitle := 'GDB';
    {$ifdef windows}
    FGDBProcess.Executable := 'gdb.exe';
    {$else}
    FGDBProcess.Executable := 'gdb';
    {$endif}
    //FGDBProcess.Parameters.Add('-q');//"Quiet". Do not print the introductory and copyright messages.
    FGDBProcess.Parameters.Add('-n');//Do not execute commands found in any initialization files.
    FGDBProcess.Parameters.Add('-f');//Full name GDB output the full file name and line number in a standard.
    //FGDBProcess.Parameters.Add('-annotate 1');
    FGDBProcess.Parameters.Add('--interpreter=mi2');//GDB/MI
    //-nw not work with mi
    //FGDBProcess.Parameters.Add('-nw');//"No windows".
    //FGDBProcess.Parameters.Add('-noasync');//Disable the asynchronous event loop for the command-line interface.


    FGDBProcess.CurrentDirectory := ExtractFilePath(ParamStr(0));
    FGDBProcess.Options :=  [poUsePipes, poStderrToOutPut];
    FGDBProcess.ShowWindow := swoHIDE;
    FGDBProcess.PipeBufferSize := 0;

    //FConsoleThread := TmnConsoleThread.Create(FGDBProcess, @ReceiveProcess);
    //FConsoleThread.FreeOnTerminate := False;

    FGDBProcess.Execute;
    //FConsoleThread.Start;
//    Spool.Add(TgdbFeatureSet.CreateBy('prompt', 'gdb:'#13));
    Spool.Add(TgdbInit.Create);
    Execute;
    Spool.Add(TgdbFeatureSet.CreateBy('language', 'pascal'));
    Spool.Add(TgdbFeatureSet.CreateBy('confirm', 'off'));
    Execute;
    //WriteProcess('set prompt gdb:');
    //WriteProcess('set confirm off');

    //WriteProcess('set new-console on');
    //WriteProcess('set verbose off');
  end;
end;

procedure TGDBDebug.Attach(SubProcess: TProcess; Resume: Boolean);
var
  i: Integer;
begin
  ////WriteProcess('help');
  ////WriteProcess('set new-console on
  Spool.Add(TgdbCommand.CreateBy('cd', SubProcess.CurrentDirectory));
  Spool.Add(TgdbCommand.CreateBy('attach', IntToStr(SubProcess.ProcessID)));
  Spool.Add(TgdbCommand.CreateBy('directory', SubProcess.CurrentDirectory));
  //WriteProcess('cd '+ SubProcess.CurrentDirectory);
  //WriteProcess('attach '+ IntToStr(SubProcess.ProcessID));
  //WriteProcess('directory '+ SubProcess.CurrentDirectory);
  //for i := 0 to Breakpoints.Count - 1 do
    //WriteProcess('break "' + StringReplace(Breakpoints[i].FileName, '\', '/', [rfReplaceAll]) + '":' + IntToStr(Breakpoints[i].Line));
  Spool.Add(TgdbContinue.Create);
  Execute;

  //WriteProcess('continue');
 ////WriteProcess('run');
end;

procedure TGDBDebug.Stop;
begin
  if GDBProcess <> nil then
  begin
    WriteProcess('kill');
    WriteProcess('q');
    GDBProcess.WaitOnExit;
    FreeAndNil(FGDBProcess);
    //FConsoleThread.Terminate;
    //FreeAndNil(FConsoleThread);
  end;
end;

procedure TGDBDebug.Next;
begin
  WriteProcess('next');
end;

procedure TGDBDebug.Run;
begin
  WriteProcess('run');
end;

procedure TGDBDebug.Step;
begin
  WriteProcess('step');
end;

procedure TGDBDebug.Lock;
begin
end;

procedure TGDBDebug.Unlock;
begin
end;

function TGDBDebug.GetState: TDebugStates;
begin
  if GDBProcess <> nil then
    Result := [dbsActive, dbsDebugging, dbsRunning]
  else if FActive then
    Result := [dbsActive]
  else
    Result := [];
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

function TGDBDebug.GetKey: string;
begin
  Result := 'GDB'; //Return version of debugger
end;
//→→M:\home\
end.
