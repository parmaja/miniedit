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
  dbgpServers, EditorDebugger;

type

  TPHP_xDebugServer = class(TdbgpServer)
  private
    FSessionName: string;
  protected
    procedure DoChanged(Listener: TmnListener); override;
    procedure DebugFile(Socket: TdbgpConnection; const FileName: string; Line: integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetExecuted(SessionName: string; Edit: TCustomSynEdit; const Line: integer);
  public
    destructor Destroy; override;
    procedure Reset;
    procedure Detach;
    property SessionName: string read FSessionName;
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
    function IsRuning: Boolean;
    procedure ToggleBreakpoint(FileName:string; LineNo: Integer); virtual;
    function IsBreakpoint(FileName:string; LineNo: Integer): Boolean; virtual;
    procedure RunToCursor(FileName: string; LineNo: integer); override;
    procedure AddBreakpoint(FileName: string; LineNo: integer); override;
    procedure RemoveBreakpoint(FileName: string; LineNo: integer); override;
    function GetSessionName: string; override;

    procedure GetBreakPoints(vBreakpoints: TStringList); override;
    procedure GetWatches(vBreakpoints: TStringList); override;
    procedure SetBreakPoints(vBreakpoints: TStringList); override;
    procedure SetWatches(vBreakpoints: TStringList); override;
    function GetWatchValue(vName:string; var vValue: string): Boolean; override;
  end;

implementation

{ TPHP_xDebug }

constructor TPHP_xDebug.Create;
begin
  inherited Create;
  FBreakpoints := TdbgpBreakpoints.Create;
  FWatches := TdbgpWatches.Create;
end;

destructor TPHP_xDebug.Destroy;
begin
  FBreakpoints.Free;
  FWatches.Free;
  inherited;
end;

procedure TPHP_xDebug.Start;
begin
end;

procedure TPHP_xDebug.Stop;
begin
  FServer.Debug.AddAction(TdbgpDetach.Create);
  FServer.Debug.AddAction(TdbgpGetCurrent.Create); //usefull to detect the disconnected
  FServer.Debug.Resume;
end;

procedure TPHP_xDebug.Reset;
begin
  FServer.Debug.AddAction(TdbgpStop.Create);
  FServer.Debug.AddAction(TdbgpGetCurrent.Create);
  FServer.Debug.Resume;
end;

procedure TPHP_xDebug.StepInto;
begin
  FServer.Debug.AddAction(TdbgpStepInto.Create);
  FServer.Debug.AddAction(TdbgpGetWatches.Create);
  FServer.Debug.AddAction(TdbgpGetCurrent.Create);
  FServer.Debug.Resume;
end;

procedure TPHP_xDebug.StepOver;
begin
  FServer.Debug.AddAction(TdbgpStepOver.Create);
  FServer.Debug.AddAction(TdbgpGetWatches.Create);
  FServer.Debug.AddAction(TdbgpGetCurrent.Create);
  FServer.Debug.Resume;
end;

procedure TPHP_xDebug.StepOut;
begin
  FServer.Debug.AddAction(TdbgpStepOut.Create);
  FServer.Debug.AddAction(TdbgpGetWatches.Create);
  FServer.Debug.AddAction(TdbgpGetCurrent.Create);
  FServer.Debug.Resume;
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

procedure TPHP_xDebug.RemoveBreakpoint(FileName: string; LineNo: integer);
begin
end;

function TPHP_xDebug.GetSessionName: string;
begin
  Result := Server.SessionName;
end;

procedure TPHP_xDebug.GetBreakPoints(vBreakpoints: TStringList);
begin
  inherited GetBreakPoints(vBreakpoints);
end;

procedure TPHP_xDebug.GetWatches(vBreakpoints: TStringList);
begin
  inherited GetWatches(vBreakpoints);
end;

procedure TPHP_xDebug.SetBreakPoints(vBreakpoints: TStringList);
begin
  inherited SetBreakPoints(vBreakpoints);
end;

procedure TPHP_xDebug.SetWatches(vBreakpoints: TStringList);
begin
  inherited SetWatches(vBreakpoints);
end;

function TPHP_xDebug.GetWatchValue(vName: string; var vValue: string): Boolean;
var
  aAction: TdbgpGetWatchInstance;
begin
  aAction := TdbgpGetWatchInstance.Create;
  aAction.VariableName := vName;
  FServer.Debug.AddAction(aAction);
  FServer.Debug.Resume;
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
var
  aFile: TEditorFile;
begin
  inherited;
  aFile := FServer.Files.ShowFile(FileName);
  SetExecuted(Socket.SessionName, aFile.SynEdit, Line);
  FServer.UpdateState([ecsDebug]);
  SetForegroundWindow(Application.MainForm.Handle);
end;

procedure TPHP_xDebugServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = ExecuteEdit) then
    FExecuteEdit := nil;
end;

procedure TPHP_xDebugServer.DoChanged(Listener: TmnListener);
begin
  inherited;
  if Listener.Count = 0 then
    SetExecuted('', nil, -1);
  FServer.UpdateState([ecsDebug]);
end;

procedure TPHP_xDebugServer.SetExecuted(SessionName: string; Edit: TCustomSynEdit; const Line: integer);
var
  OldLine: integer;
  OldEdit: TCustomSynEdit;
begin
  FSessionName := SessionName;
  if (FExecuteEdit <> Edit) or (FExecuteLine <> Line) then
  begin
    OldLine := FExecuteLine;
    OldEdit := FExecuteEdit;

    FExecuteLine := Line;

    if ExecuteEdit <> nil then
      RemoveFreeNotification(ExecuteEdit);
    FExecuteEdit := Edit;
    if ExecuteEdit <> nil then
      FreeNotification(ExecuteEdit);

    if OldEdit <> nil then
    begin
      OldEdit.InvalidateLine(OldLine);
    end;

    if ExecuteEdit <> nil then
    begin
      ExecuteEdit.CaretY := FExecuteLine;
      ExecuteEdit.CaretX := 1;
      ExecuteEdit.InvalidateLine(FExecuteLine);
    end;
  end;
end;

destructor TPHP_xDebugServer.Destroy;
begin
  SetExecuted('', nil, -1);
  Stop;
  inherited;
end;

initialization
  RegisterClasses([TdbgpBreakpoint, TdbgpWatch]);
end.

