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
}
interface

uses
  Classes, SysUtils, mnClasses, process, ConsoleProcess, EditorEngine, EditorDebugger;

type
  TGDBDebug = class;

  TGDBBreakPointItem = class(TObject)
  public
    Info: TDebugBreakpointInfo;
  end;

  { TGDBBreakPointList }

  TGDBBreakPointList = class(specialize GItems<TGDBBreakPointItem>)
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

  TGDBWatchList = class(specialize GNamedItems<TGDBWatchItem>)
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

  { TGDBDebug }

  TGDBDebug = class(TEditorDebugger)
  protected
    FGDBProcess: TProcess;
    FConsoleThread: TmnConsoleThread;
    function CreateBreakPoints: TEditorBreakPoints; override;
    function CreateWatches: TEditorWatches; override;
    procedure ReceiveProcess(S: ansistring);
    procedure WriteProcess(S: ansistring);
    property GDBProcess: TProcess read FGDBProcess;
  public
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
  Engine.SendOutout(S);
end;

procedure TGDBDebug.WriteProcess(S: ansistring);
begin
  S := S+#13#10;
  Engine.SendOutout(S);
  FGDBProcess.Input.WriteBuffer(S[1], Length(S));
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
    FGDBProcess.Parameters.Add('-q');//"Quiet". Do not print the introductory and copyright messages.
    FGDBProcess.Parameters.Add('-n');//Do not execute commands found in any initialization files.
    FGDBProcess.Parameters.Add('-f');//Full name GDB output the full file name and line number in a standard.
    //FGDBProcess.Parameters.Add('-annotate 1');
    //FGDBProcess.Parameters.Add('-interpreter mi');//GDB/MI
    //-nw not work with mi
    //FGDBProcess.Parameters.Add('-nw');//"No windows".
    //FGDBProcess.Parameters.Add('-noasync');//Disable the asynchronous event loop for the command-line interface.

    FGDBProcess.CurrentDirectory := ExtractFilePath(ParamStr(0));
    FGDBProcess.Options :=  [poUsePipes, poStderrToOutPut];
    FGDBProcess.ShowWindow := swoHIDE;
    FGDBProcess.PipeBufferSize := 80;

    FConsoleThread := TmnConsoleThread.Create(FGDBProcess, @ReceiveProcess);
    FConsoleThread.FreeOnTerminate := False;

    FGDBProcess.Execute;
    FConsoleThread.Start;

    WriteProcess('set prompt gdb:');
    WriteProcess('set confirm off');

    //WriteProcess('set new-console on');
    //WriteProcess('set verbose off');
  end;
end;

procedure TGDBDebug.Attach(SubProcess: TProcess; Resume: Boolean);
var
  i: Integer;
begin
  //WriteProcess('help');
  //WriteProcess('set new-console on
  WriteProcess('cd '+ SubProcess.CurrentDirectory);
  WriteProcess('attach '+ IntToStr(SubProcess.ProcessID));
  WriteProcess('directory '+ SubProcess.CurrentDirectory);
  for i := 0 to Breakpoints.Count - 1 do
    WriteProcess('break "'+ Breakpoints[i].FileName+':'+IntToStr(Breakpoints[i].Line)+'"');
  WriteProcess('continue');
  //WriteProcess('run');
end;

procedure TGDBDebug.Stop;
begin
  if GDBProcess <> nil then
  begin
    WriteProcess('kill');
    WriteProcess('q');
    GDBProcess.WaitOnExit;
    FreeAndNil(FGDBProcess);
    FConsoleThread.Terminate;
    FreeAndNil(FConsoleThread);
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
    Result := [dbsDebugging, dbsRunning]
  else
    Result := [];
end;

procedure TGDBDebug.Action(AAction: TDebugAction);
begin
  case AAction of
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
