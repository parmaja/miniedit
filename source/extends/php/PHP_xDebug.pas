unit PHP_xDebug;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license   GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{
#Put it in php.ini

zend_extension = ext/php_xdebug.dll
xdebug.remote_handler = dbgp
xdebug.remote_mode = req
xdebug.remote_enable = On


xdebug.remote_autostart = On

xdebug.remote_port = 9000
xdebug.remote_connect_back = Off
xdebug.idekey = "XDEBUG"
#xdebug.extended_info = On
}
interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, syncobjs,
  mnServers,
  dbgpServers,
  SynEdit,
  EditorDebugger;

type
  TPHPDebug = class;

  TPHPDebugServer = class(TdbgpServer)
  private
    FKey: string;
  protected
    FDebug: TPHPDebug;
    procedure DoChanged(vListener: TmnListener); override;
  public
    destructor Destroy; override;
    property Key: string read FKey;
  end;

  { TPHPDebugBreakPoints }

  TPHPDebugBreakPoints = class(TEditorBreakPoints)
  protected
    FDebug: TPHPDebug;
    function GetCount: integer; override;
    function GetItems(Index: integer): TDebugBreakpointInfo; override;
  public
    procedure Clear; override;
    procedure Toggle(FileName: string; LineNo: integer); override;
    function IsExists(FileName: string; LineNo: integer): boolean; override;
    procedure Add(FileName: string; LineNo: integer); override;
    procedure Remove(FileName: string; Line: integer); override; overload;
    procedure Remove(Handle: integer); override; overload;
  end;

  { TPHPDebugWatches }

  TPHPDebugWatches = class(TEditorWatches)
  protected
    FDebug: TPHPDebug;
    function GetCount: integer; override;
    function GetItems(Index: integer): TDebugWatchInfo; override;
  public
    procedure Clear; override;
    procedure Add(vName: string); override;
    procedure Remove(vName: string); override;
    function GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean; override;
  end;

  { TPHPDebug }

  TPHPDebug = class(TEditorDebugger)
  private
    FServer: TPHPDebugServer;
  protected
    function CreateBreakPoints: TEditorBreakPoints; override;
    function CreateWatches: TEditorWatches; override;
    procedure DoShowFile(const Key, FileName: string; Line: integer; vCallStack: TCallStackItems); deprecated;

    procedure Reset;
    procedure Resume;
    procedure StepInto;
    procedure StepOver;
    procedure StepOut;
    procedure Run;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure Action(AAction: TDebugAction); override;
    function GetState: TDebugStates; override;
    procedure Lock; override;
    procedure Unlock; override;
    function GetKey: string; override;
  end;

implementation

{ TPHPDebugWatches }

function TPHPDebugWatches.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Watches.Count;
end;

function TPHPDebugWatches.GetItems(Index: integer): TDebugWatchInfo;
var
  aWt: TdbgpWatch;
begin
  with FDebug.FServer do
    aWt := Watches[Index];
  Result:= aWt.Info;
end;

procedure TPHPDebugWatches.Clear;
begin
  with FDebug.FServer do
    Watches.Clear;
end;

procedure TPHPDebugWatches.Add(vName: string);
begin
  with FDebug.FServer do
    Watches.AddWatch(vName);
end;

procedure TPHPDebugWatches.Remove(vName: string);
begin
  with FDebug.FServer do
    Watches.RemoveWatch(vName);
end;

function TPHPDebugWatches.GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean;
var
  aAction: TdbgpCustomGet;
begin
  Result := False;
  if dbsRunning in FDebug.GetState then   //there is a connection from XDebug
  begin
    if EvalIt then
      aAction := TdbgpEval.Create
    else
      aAction := TdbgpGetWatchInstance.Create;
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
end;

{ TPHPDebugBreakPoints }

function TPHPDebugBreakPoints.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Breakpoints.Count;
end;

function TPHPDebugBreakPoints.GetItems(Index: integer): TDebugBreakpointInfo;
var
  aBP: TdbgpBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints[Index];
  Result.FileName := aBP.FileName;
  Result.Handle := aBP.Handle;
  Result.Line := aBP.Line;
end;

procedure TPHPDebugBreakPoints.Clear;
begin
  with FDebug.FServer do
    Breakpoints.Clear;
end;

procedure TPHPDebugBreakPoints.Toggle(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Toggle(FileName, LineNo);
end;

function TPHPDebugBreakPoints.IsExists(FileName: string; LineNo: integer): boolean;
begin
  with FDebug.FServer do
    Result := Breakpoints.Find(FileName, LineNo) <> nil;
end;

procedure TPHPDebugBreakPoints.Add(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Add(FileName, LineNo);
end;

procedure TPHPDebugBreakPoints.Remove(FileName: string; Line: integer);
var
  aBP: TdbgpBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints.Find(FileName, Line);
  if aBP <> nil then
    with FDebug.FServer do
      Breakpoints.Remove(aBP);
end;

procedure TPHPDebugBreakPoints.Remove(Handle: integer);
begin
  with FDebug.FServer do
    Breakpoints.Remove(Handle);
end;

{ TPHPDebug }

function TPHPDebug.CreateBreakPoints: TEditorBreakPoints;
begin
  Result := TPHPDebugBreakPoints.Create;
  (Result as TPHPDebugBreakPoints).FDebug := Self;
end;

function TPHPDebug.CreateWatches: TEditorWatches;
begin
  Result := TPHPDebugWatches.Create;
  (Result as TPHPDebugWatches).FDebug := Self;
end;

procedure TPHPDebug.DoShowFile(const Key, FileName: string; Line: integer; vCallStack: TCallStackItems);
begin
  SetExecutedLine(Key, FileName, Line, vCallStack);
end;

constructor TPHPDebug.Create;
begin
  inherited Create;
  FServer := TPHPDebugServer.Create;
  FServer.FDebug := Self;
  DBGP.OnShowFile := @DoShowFile;
end;

destructor TPHPDebug.Destroy;
begin
  DBGP.OnShowFile := nil;
  FreeAndNil(FServer);
  inherited;
end;

procedure TPHPDebug.Action(AAction: TDebugAction);
begin
    case AAction of
    dbaStartServer: Start;
    dbaStopServer: Stop;
    dbaReset: Reset;
    dbaResume: Resume;
    dbaStepInto: StepInto;
    dbaStepOver: StepOver;
    dbaStepOut: StepOut;
    dbaRun: Run;
  end;
end;

function TPHPDebug.GetState: TDebugStates;
begin
  Result := [];
  if FServer.Active then
    Result := Result + [dbsActive];
  if FServer.IsRuning then
    Result := Result + [dbsRunning];
end;

procedure TPHPDebug.Start;
begin
  inherited;
  FServer.Start;
end;

procedure TPHPDebug.Stop;
var
  aAction: TdbgpDetach;
begin
  inherited;
  if FServer.IsRuning then
  begin
    FServer.Clear;
    aAction := TdbgpDetach.Create;
    aAction.CreateEvent;
    FServer.AddAction(aAction);
    FServer.Resume;
    aAction.Event.WaitFor(30000);
    FServer.ExtractAction(aAction);
    aAction.Free;
  end;
  FServer.Stop;
end;

procedure TPHPDebug.Reset;
begin
  FServer.Clear; //no need to any exists actions
  FServer.AddAction(TdbgpStop.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHPDebug.Resume;
begin
  FServer.AddAction(TdbgpDetach.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHPDebug.StepInto;
begin
  FServer.AddAction(TdbgpStepInto.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHPDebug.StepOver;
begin
  FServer.AddAction(TdbgpStepOver.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHPDebug.StepOut;
begin
  FServer.AddAction(TdbgpStepOut.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHPDebug.Run;
begin
  FServer.AddAction(TdbgpRun.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHPDebug.Lock;
begin
  DBGP.Lock.Enter;
end;

procedure TPHPDebug.Unlock;
begin
  DBGP.Lock.Leave;
end;

function TPHPDebug.GetKey: string;
begin
  Result := FServer.Key;
end;

procedure TPHPDebugServer.DoChanged(vListener: TmnListener);
begin
  inherited;
end;

destructor TPHPDebugServer.Destroy;
begin
  inherited;
end;

end.
