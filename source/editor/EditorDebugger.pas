unit EditorDebugger;
{$mode delphi}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs, IAddons, SynEdit;

type
  TEditBreakpoint = record
    Handle: Integer;
    FileName: string;
    Line: Integer;
  end;

  TEditWatch = record
    Name: string;
    VarType: string;
    Value: Variant;
  end;

  { TEditorDebugger }

  TEditorDebugger = class(TAddon, IDebugAddon, ICheckAddon)
  private
    FExecutedLine: integer;
    FExecutedEdit: TCustomSynEdit;
    FKey: string;
    function GetActive: Boolean;
    procedure SetActive(const AValue: Boolean);
  protected
    //procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetBreakpoints(Index: Integer): TEditBreakpoint; virtual;
    function GetWatches(Index: Integer): TEditWatch; virtual;
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
  public
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure Reset; virtual;
    procedure StepInto; virtual;
    procedure StepOver; virtual;
    procedure StepOut; virtual;
    procedure Run; virtual;
    procedure Resume; virtual;
    procedure Lock; virtual;
    procedure Unlock; virtual;
    function IsRuning: Boolean; virtual;

    function IsConnected: Boolean; virtual;

    procedure RunToCursor(FileName:string; LineNo: Integer); virtual;
    procedure ToggleBreakpoint(FileName:string; LineNo: Integer); virtual;
    function IsBreakpoint(FileName:string; LineNo: Integer): Boolean; virtual;
    procedure AddBreakpoint(FileName:string; LineNo: Integer); virtual;
    procedure RemoveBreakpoint(FileName:string; Line: Integer); virtual; overload;
    procedure RemoveBreakpoint(Handle: Integer); virtual; overload;

    property ExecutedLine: integer read FExecutedLine;
    property ExecutedEdit: TCustomSynEdit read FExecutedEdit write FExecutedEdit;
    function BreakpointsCount: Integer; virtual;
    procedure BreakpointsClear; virtual;
    property Breakpoints[Index: Integer]: TEditBreakpoint read GetBreakpoints;

    procedure AddWatch(vName:string); virtual;
    procedure RemoveWatch(vName:string); virtual;
    function WatchesCount: Integer; virtual;
    procedure WatchesClear; virtual;
    property Watches[Index: Integer]: TEditWatch read GetWatches;
    function GetWatchValue(vName:string; var vValue: string): Boolean; virtual;

    function GetKey: string; virtual;
    property Active: Boolean read GetActive write SetActive;

    procedure SetExecuted(Key: string; Edit: TCustomSynEdit; const Line: integer); overload;
    procedure SetExecuted(Key: string; FileName: string; const Line: integer); overload;
  end;

implementation

uses
  EditorEngine;

{ TEditorDebugger }

function TEditorDebugger.GetBreakpoints(Index: Integer): TEditBreakpoint;
begin
  raise Exception.Create('Out of index');
end;

function TEditorDebugger.GetWatches(Index: Integer): TEditWatch;
begin
  raise Exception.Create('Out of index');
end;

function TEditorDebugger.GetChecked: Boolean;
begin
  Result := Active;
end;

procedure TEditorDebugger.SetChecked(AValue: Boolean);
begin
  Active := AValue;
end;

function TEditorDebugger.GetActive: Boolean;
begin
  Result := False;
end;

procedure TEditorDebugger.SetActive(const AValue: Boolean);
begin

end;

procedure TEditorDebugger.Start;
begin

end;

procedure TEditorDebugger.Stop;
begin

end;

procedure TEditorDebugger.Reset;
begin

end;

procedure TEditorDebugger.StepInto;
begin

end;

procedure TEditorDebugger.StepOver;
begin

end;

procedure TEditorDebugger.StepOut;
begin
end;

procedure TEditorDebugger.Run;
begin

end;

procedure TEditorDebugger.Resume;
begin
end;

procedure TEditorDebugger.Lock;
begin
end;

procedure TEditorDebugger.Unlock;
begin
end;

function TEditorDebugger.IsRuning: Boolean;
begin
  Result := False;
end;

function TEditorDebugger.IsConnected: Boolean;
begin
  Result := False;
end;

procedure TEditorDebugger.RunToCursor(FileName: string; LineNo: Integer);
begin
end;

procedure TEditorDebugger.ToggleBreakpoint(FileName: string; LineNo: Integer);
begin
end;

function TEditorDebugger.IsBreakpoint(FileName: string; LineNo: Integer): Boolean;
begin
  Result := False;
end;

procedure TEditorDebugger.AddBreakpoint(FileName: string; LineNo: Integer);
begin
end;

procedure TEditorDebugger.RemoveBreakpoint(FileName: string; Line: Integer);
begin
end;

procedure TEditorDebugger.RemoveBreakpoint(Handle: Integer);
begin
end;

function TEditorDebugger.BreakpointsCount: Integer;
begin
  Result := 0;
end;

procedure TEditorDebugger.BreakpointsClear;
begin
end;

procedure TEditorDebugger.AddWatch(vName: string);
begin
end;

procedure TEditorDebugger.RemoveWatch(vName: string);
begin
end;

function TEditorDebugger.WatchesCount: Integer;
begin
  Result := 0;
end;

procedure TEditorDebugger.WatchesClear;
begin
end;

function TEditorDebugger.GetWatchValue(vName: string; var vValue: string): Boolean;
begin
  Result := False;
end;

function TEditorDebugger.GetKey: string;
begin
  Result := FKey;
end;

procedure TEditorDebugger.SetExecuted(Key: string; Edit: TCustomSynEdit; const Line: integer);
var
  OldLine: integer;
  OldEdit: TCustomSynEdit;
begin
  FKey := Key;
  if (FExecutedEdit <> Edit) or (FExecutedLine <> Line) then
  begin
    OldLine := FExecutedLine;
    OldEdit := FExecutedEdit;

    FExecutedLine := Line;

{    if ExecutedEdit <> nil then
      RemoveFreeNotification(FExecutedEdit);
    FExecutedEdit := Edit;
    if ExecutedEdit <> nil then
      FreeNotification(FExecutedEdit);}

    if OldEdit <> nil then
      OldEdit.InvalidateLine(OldLine);

    if ExecutedEdit <> nil then
    begin
      ExecutedEdit.CaretY := FExecutedLine;
      ExecutedEdit.CaretX := 1;
      ExecutedEdit.InvalidateLine(FExecutedLine);
    end;
  end;
  Engine.UpdateState([ecsDebug]);
  //SetForegroundWindow(Application.MainForm.Handle);
end;

procedure TEditorDebugger.SetExecuted(Key: string; FileName: string; const Line: integer);
var
  aFile: TEditorFile;
begin
  inherited;
  aFile := Engine.Files.ShowFile(FileName);
  SetExecuted(Key, aFile.SynEdit, Line);
end;

{procedure TEditorDebugger.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDebug.ExecutedEdit) then
    FDebug.ExecutedEdit := nil;
end;}

end.

