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
    Handle: integer;
    FileName: string;
    Line: integer;
  end;

  TEditWatch = record
    Name: string;
    VarType: string;
    Value: variant;
  end;

  { TEditorBreakPoints }

  TEditorBreakPoints = class(TObject)
  private
  protected
    function GetCount: integer; virtual;
    function GetItems(Index: integer): TEditBreakpoint; virtual;
  public
    procedure Clear; virtual;
    procedure Toggle(FileName: string; LineNo: integer); virtual;
    function Found(FileName: string; LineNo: integer): boolean; virtual;
    procedure Add(FileName: string; LineNo: integer); virtual;
    procedure Remove(FileName: string; Line: integer); virtual; overload;
    procedure Remove(Handle: integer); virtual; overload;

    property Count: integer read GetCount;
    property Items[Index: integer]: TEditBreakpoint read GetItems; default;
  end;

  { TEditorWatches }

  TEditorWatches = class(TObject)
  private
  protected
    function GetCount: integer; virtual;
    function GetItems(Index: integer): TEditWatch; virtual;
  public
    procedure Clear; virtual;
    procedure Add(vName: string); virtual;
    procedure Remove(vName: string); virtual;
    function GetWatchValue(vName: string; var vValue: string): boolean; virtual;
    property Count: integer read GetCount;
    property Items[Index: integer]: TEditWatch read GetItems; default;
  end;

  { TEditorDebugger }

  TEditorDebugger = class(TAddon, IMenuAddon, IClickAddon, IDebugAddon, ICheckAddon)
  private
    FBreakpoints: TEditorBreakPoints;
    FWatches: TEditorWatches;
    FExecutedLine: integer;
    FExecutedEdit: TCustomSynEdit;
    FKey: string;
    function GetActive: boolean;
    procedure SetActive(const AValue: boolean);
  protected
    //procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetChecked: boolean;
    procedure SetChecked(AValue: boolean);

    function GetCaption: string; virtual;
    procedure Click(Sender: TObject); virtual;
    function CreateBreakPoints: TEditorBreakPoints; virtual;
    function CreateWatches: TEditorWatches; virtual;
  public
    constructor Create;
    destructor Destroy; override;

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
    function IsRuning: boolean; virtual;

    function IsConnected: boolean; virtual;

    procedure RunToCursor(FileName: string; LineNo: integer); virtual;

    property ExecutedLine: integer read FExecutedLine;
    property ExecutedEdit: TCustomSynEdit read FExecutedEdit write FExecutedEdit;

    function GetKey: string; virtual;
    property Active: boolean read GetActive write SetActive;

    procedure SetExecuted(Key: string; Edit: TCustomSynEdit; const Line: integer); overload;
    procedure SetExecuted(Key: string; FileName: string; const Line: integer); overload;
    property Breakpoints: TEditorBreakPoints read FBreakpoints;
    property Watches: TEditorWatches read FWatches;
  end;

implementation

uses
  EditorEngine;

{ TEditorWatches }

function TEditorWatches.GetCount: integer;
begin
  Result := 0;
end;

function TEditorWatches.GetItems(Index: integer): TEditWatch;
begin

end;

procedure TEditorWatches.Clear;
begin
end;

procedure TEditorWatches.Add(vName: string);
begin
end;

procedure TEditorWatches.Remove(vName: string);
begin

end;

function TEditorWatches.GetWatchValue(vName: string; var vValue: string): boolean;
begin
end;

{ TEditorBreakPoints }

function TEditorBreakPoints.GetCount: integer;
begin
  Result := 0;
end;

function TEditorBreakPoints.GetItems(Index: integer): TEditBreakpoint;
begin
end;

procedure TEditorBreakPoints.Clear;
begin
end;

procedure TEditorBreakPoints.Toggle(FileName: string; LineNo: integer);
begin
end;

function TEditorBreakPoints.Found(FileName: string; LineNo: integer): boolean;
begin
end;

procedure TEditorBreakPoints.Add(FileName: string; LineNo: integer);
begin
end;

procedure TEditorBreakPoints.Remove(FileName: string; Line: integer);
begin
end;

procedure TEditorBreakPoints.Remove(Handle: integer);
begin
end;

{ TEditorDebugger }

function TEditorDebugger.GetChecked: boolean;
begin
  Result := Active;
end;

procedure TEditorDebugger.SetChecked(AValue: boolean);
begin
  Active := AValue;
end;

function TEditorDebugger.GetCaption: string;
begin
  Result := 'Debug';
end;

procedure TEditorDebugger.Click(Sender: TObject);
begin
  Active := not Active;
end;

function TEditorDebugger.CreateBreakPoints: TEditorBreakPoints;
begin
  Result := TEditorBreakPoints.Create;
end;

function TEditorDebugger.CreateWatches: TEditorWatches;
begin
  Result := TEditorWatches.Create;
end;

constructor TEditorDebugger.Create;
begin
  inherited;
  FBreakpoints := CreateBreakPoints;
  FWatches := CreateWatches;
end;

destructor TEditorDebugger.Destroy;
begin
  FreeAndNil(FBreakpoints);
  FreeAndNil(FWatches);
  inherited;
end;

function TEditorDebugger.GetActive: boolean;
begin
  Result := False;
end;

procedure TEditorDebugger.SetActive(const AValue: boolean);
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

function TEditorDebugger.IsRuning: boolean;
begin
  Result := False;
end;

function TEditorDebugger.IsConnected: boolean;
begin
  Result := False;
end;

procedure TEditorDebugger.RunToCursor(FileName: string; LineNo: integer);
begin
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

