unit PHP_xDebug;

{$mode delphi}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  mnServers, dbgpServers,
  SynEdit, IAddons,
  EditorDebugger;

type
  TPHP_xDebug = class;

  TPHP_xDebugServer = class(TdbgpServer)
  private
    FKey: string;
  protected
    FDebug: TPHP_xDebug;
    procedure DoChanged(Listener: TmnListener); override;
    procedure DebugFile(Socket: TdbgpConnection; const FileName: string; Line: integer); override;
  public
    destructor Destroy; override;
    procedure Reset;
    procedure Detach;
    property Key: string read FKey;
  end;

  { TPHP_xDebugBreakPoints }

  TPHP_xDebugBreakPoints = class(TEditorBreakPoints)
  protected
    FDebug: TPHP_xDebug;
    function GetCount: integer; override;
    function GetItems(Index: integer): TEditBreakpoint; virtual;
  public
    procedure Clear; override;
    procedure Toggle(FileName: string; LineNo: integer); override;
    function Found(FileName: string; LineNo: integer): boolean; override;
    procedure Add(FileName: string; LineNo: integer); override;
    procedure Remove(FileName: string; Line: integer); override; overload;
    procedure Remove(Handle: integer); override; overload;
  end;

  { TPHP_xDebugWatches }

  TPHP_xDebugWatches = class(TEditorWatches)
  protected
    FDebug: TPHP_xDebug;
    function GetCount: integer; override;
    function GetItems(Index: integer): TEditWatch; virtual;
  public
    procedure Clear; override;
    procedure Add(vName: string); override;
    procedure Remove(vName: string); override;
    function GetWatchValue(vName: string; var vValue: string): boolean; override;
  end;

  { TPHP_xDebug }

  TPHP_xDebug = class(TEditorDebugger)
  private
    FServer: TPHP_xDebugServer;
  protected
    function CreateBreakPoints: TEditorBreakPoints; override;
    function CreateWatches: TEditorWatches; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure Reset; override;
    procedure StepInto; override;
    procedure StepOver; override;
    procedure StepOut; override;
    procedure Run; override;
    procedure Lock; override;
    procedure Unlock; override;
    function IsRuning: boolean; override;
    procedure RunToCursor(FileName: string; LineNo: integer); override;
    function GetKey: string; override;
  end;

implementation

{ TPHP_xDebugWatches }

function TPHP_xDebugWatches.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Watches.Count;
end;

function TPHP_xDebugWatches.GetItems(Index: integer): TEditWatch;
var
  aWt: TdbgpWatch;
begin
  with FDebug.FServer do
    aWt := Watches[Index];
  Result.Name := aWt.VariableName;
  Result.Value := aWt.Value;
  Result.VarType := aWt.VariableType;
end;

procedure TPHP_xDebugWatches.Clear;
begin
  inherited Clear;
end;

procedure TPHP_xDebugWatches.Add(vName: string);
begin
  inherited Add(vName);
end;

procedure TPHP_xDebugWatches.Remove(vName: string);
begin
  inherited Remove(vName);
end;

function TPHP_xDebugWatches.GetWatchValue(vName: string; var vValue: string): boolean;
var
  aAction: TdbgpGetWatchInstance;
begin
  aAction := TdbgpGetWatchInstance.Create;
  aAction.VariableName := vName;
  with FDebug.FServer do
  begin
    AddAction(aAction);
    Resume;
  end;
end;

{ TPHP_xDebugBreakPoints }

function TPHP_xDebugBreakPoints.GetCount: integer;
begin
  with FDebug.FServer do
    Result := Breakpoints.Count;
end;

function TPHP_xDebugBreakPoints.GetItems(Index: integer): TEditBreakpoint;
var
  aBP: TdbgpBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints[Index];
  Result.FileName := aBP.FileName;
  Result.Handle := aBP.Handle;
  Result.Line := aBP.Line;
end;

procedure TPHP_xDebugBreakPoints.Clear;
begin
  with FDebug.FServer do
    Breakpoints.Clear;
end;

procedure TPHP_xDebugBreakPoints.Toggle(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Toggle(FileName, LineNo);
end;

function TPHP_xDebugBreakPoints.Found(FileName: string; LineNo: integer): boolean;
begin
  with FDebug.FServer do
    Result := Breakpoints.Find(FileName, LineNo) <> nil;
end;

procedure TPHP_xDebugBreakPoints.Add(FileName: string; LineNo: integer);
begin
  with FDebug.FServer do
    Breakpoints.Add(FileName, LineNo);
end;

procedure TPHP_xDebugBreakPoints.Remove(FileName: string; Line: integer);
var
  aBP: TdbgpBreakpoint;
begin
  with FDebug.FServer do
    aBP := Breakpoints.Find(FileName, Line);
  if aBP <> nil then
    with FDebug.FServer do
      Breakpoints.Remove(aBP);
end;

procedure TPHP_xDebugBreakPoints.Remove(Handle: integer);
begin
  with FDebug.FServer do
    Breakpoints.Remove(Handle);
end;

{ TPHP_xDebug }

function TPHP_xDebug.CreateBreakPoints: TEditorBreakPoints;
begin
  Result := TPHP_xDebugBreakPoints.Create;
  (Result as TPHP_xDebugBreakPoints).FDebug := Self;
end;

function TPHP_xDebug.CreateWatches: TEditorWatches;
begin
  Result := TPHP_xDebugWatches.Create;
  (Result as TPHP_xDebugWatches).FDebug := Self;
end;

constructor TPHP_xDebug.Create;
begin
  inherited Create;
  FServer := TPHP_xDebugServer.Create(nil);
  FServer.FDebug := Self;
end;

destructor TPHP_xDebug.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

procedure TPHP_xDebug.Start;
begin
  FServer.Start;
end;

procedure TPHP_xDebug.Stop;
begin
  FServer.AddAction(TdbgpDetach.Create);
  FServer.AddAction(TdbgpGetCurrent.Create); //usefull to detect the disconnected
  FServer.Resume;
end;

procedure TPHP_xDebug.Reset;
begin
  FServer.AddAction(TdbgpStop.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHP_xDebug.StepInto;
begin
  FServer.AddAction(TdbgpStepInto.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHP_xDebug.StepOver;
begin
  FServer.AddAction(TdbgpStepOver.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHP_xDebug.StepOut;
begin
  FServer.AddAction(TdbgpStepOut.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHP_xDebug.Run;
begin
  FServer.AddAction(TdbgpRun.Create);
  FServer.AddAction(TdbgpGetWatches.Create);
  FServer.AddAction(TdbgpGetCurrent.Create);
  FServer.Resume;
end;

procedure TPHP_xDebug.Lock;
begin
  DBGLock.Lock;
end;

procedure TPHP_xDebug.Unlock;
begin
  DBGLock.Unlock;
end;

function TPHP_xDebug.IsRuning: boolean;
begin
  Result := FServer.Count > 0;
end;

procedure TPHP_xDebug.RunToCursor(FileName: string; LineNo: integer);
begin
end;

function TPHP_xDebug.GetKey: string;
begin
  Result := FServer.Key;
end;

procedure TPHP_xDebugServer.Reset;
begin
  //  Respond(dbgpReset);
end;

procedure TPHP_xDebugServer.Detach;
begin
  //  Respond(dbgpDetach);
end;

procedure TPHP_xDebugServer.DebugFile(Socket: TdbgpConnection; const FileName: string; Line: integer);
begin
  FDebug.SetExecuted(Socket.Key, FileName, Line);
end;

procedure TPHP_xDebugServer.DoChanged(Listener: TmnListener);
begin
  inherited;
  if Listener.Count = 0 then
    FDebug.SetExecuted('', nil, -1);
  //Engine.UpdateState([ecsDebug]);
end;

destructor TPHP_xDebugServer.Destroy;
begin
  FDebug.SetExecuted('', nil, -1);
  Stop;
  inherited;
end;

initialization
  Addons.Add('Debug', 'XDebug', TPHP_xDebug);
end.

