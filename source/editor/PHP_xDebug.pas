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
  SynEdit,
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

  { TPHP_xDebug }

  TPHP_xDebug = class(TEditorDebugger)
  private
    FServer: TPHP_xDebugServer;
    FBreakpoints: TdbgpBreakpoints;
    FWatches: TdbgpWatches;
  protected
    property Breakpoints: TdbgpBreakpoints read FBreakpoints;
    property Watches: TdbgpWatches read FWatches;
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
    function IsRuning: Boolean; override;
    procedure RunToCursor(FileName: string; LineNo: integer); override;
    function GetKey: string; override;

    procedure ToggleBreakpoint(FileName:string; LineNo: Integer); override;
    function IsBreakpoint(FileName:string; LineNo: Integer): Boolean; override;
    procedure AddBreakpoint(FileName:string; LineNo: Integer); override;
    procedure RemoveBreakpoint(FileName:string; Line: Integer); override;
    procedure RemoveBreakpoint(Handle: Integer); override;

    function GetWatchValue(vName:string; var vValue: string): Boolean; override;
end;

implementation

{ TPHP_xDebug }

constructor TPHP_xDebug.Create;
begin
  inherited Create;
  FServer := TPHP_xDebugServer.Create(nil);
  FServer.FDebug := Self;
  FBreakpoints := TdbgpBreakpoints.Create;
  FWatches := TdbgpWatches.Create;
end;

destructor TPHP_xDebug.Destroy;
begin
  FreeAndNil(FBreakpoints);
  FreeAndNil(FWatches);
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

function TPHP_xDebug.IsRuning: Boolean;
begin
  Result := FServer.Count > 0;
end;

procedure TPHP_xDebug.ToggleBreakpoint(FileName: string; LineNo: Integer);
begin
  FServer.Breakpoints.Toggle(FileName, LineNo);
end;

function TPHP_xDebug.IsBreakpoint(FileName: string; LineNo: Integer): Boolean;
begin

end;

procedure TPHP_xDebug.RunToCursor(FileName: string; LineNo: integer);
begin
end;

procedure TPHP_xDebug.AddBreakpoint(FileName: string; LineNo: integer);
begin
end;

procedure TPHP_xDebug.RemoveBreakpoint(FileName: string; Line: integer);
begin
end;

procedure TPHP_xDebug.RemoveBreakpoint(Handle: Integer);
begin
  inherited RemoveBreakpoint(Handle);
end;

function TPHP_xDebug.GetKey: string;
begin
  Result := FServer.Key;
end;

function TPHP_xDebug.GetWatchValue(vName: string; var vValue: string): Boolean;
var
  aAction: TdbgpGetWatchInstance;
begin
  aAction := TdbgpGetWatchInstance.Create;
  aAction.VariableName := vName;
  FServer.AddAction(aAction);
  FServer.Resume;
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
  RegisterClasses([TdbgpBreakpoint, TdbgpWatch]);
end.

