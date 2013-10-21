unit EditorDebugger;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license   GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  SysUtils, Forms, StrUtils, Variants, Classes, Controls, Graphics, Contnrs,
  SynEdit,
  EditorClasses;

type
  TDebugAction = (
    dbaStart,
    dbaStop,
    dbaReset, //stop debug and stop the run
    dbaResume, //run and do not stop at breakpoints, or run without debug
    dbaStepInto,
    dbaStepOver,
    dbaStepOut,
    dbaRun
   );

   TDebugState = (
    dbsActive, //Server is active
    dbsRunning, //There is program in execute
    dbsDebugging //There is program in execute and debugged
   );

   TDebugStates = set of TDebugState;

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

  { TEditorElements }

  TEditorItem = class(TObject)
  protected
    function GetCount: integer; virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    property Count: integer read GetCount;
  end;

  { TEditorBreakPoints }

  TEditorBreakPoints = class(TEditorItem)
  protected
    function GetItems(Index: integer): TEditBreakpoint; virtual; abstract;
  public
    procedure Toggle(FileName: string; LineNo: integer); virtual; abstract;
    function Found(FileName: string; LineNo: integer): boolean; virtual; abstract;
    procedure Add(FileName: string; LineNo: integer); virtual; abstract;
    procedure Remove(FileName: string; Line: integer); virtual; overload; abstract;
    procedure Remove(Handle: integer); virtual; overload; abstract;

    property Items[Index: integer]: TEditBreakpoint read GetItems; default;
  end;

  { TEditorWatches }

  TEditorWatches = class(TEditorItem)
  private
  protected
    function GetItems(Index: integer): TEditWatch; virtual; abstract;
  public
    procedure Add(vName: string); virtual; abstract;
    procedure Remove(vName: string); virtual; abstract;
    function GetValue(vName: string; var vValue: Variant; var vType: string; EvalIt: Boolean): boolean; virtual; abstract;
    property Items[Index: integer]: TEditWatch read GetItems; default;
  end;

  { TEditorDebugLink }

  TEditorDebugLink = class(TComponent) //to use Notification :P
  private
    procedure SetExecutedExit(const AValue: TCustomSynEdit);
  public
    FExecutedLine: integer;
    FExecutedControl: TCustomSynEdit;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ExecutedLine: Integer read FExecutedLine write FExecutedLine;
    property ExecutedControl: TCustomSynEdit read FExecutedControl write SetExecutedExit;
  end;

  { TEditorDebugger }

  TEditorDebugger = class(TObject)
  private
    FBreakpoints: TEditorBreakPoints;
    FWatches: TEditorWatches;
    FKey: string;
    FLink: TEditorDebugLink;
    function GetExecutedControl: TCustomSynEdit;
    function GetExecutedLine: Integer;
    function GetActive: Boolean;
    function GetRunning: boolean;
    procedure SetActive(AValue: boolean);
    procedure SetExecutedControl(const AValue: TCustomSynEdit);
  protected
    function GetCaption: string; virtual;
    function CreateBreakPoints: TEditorBreakPoints; virtual; abstract;
    function CreateWatches: TEditorWatches; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Lock; virtual; abstract;
    procedure Unlock; virtual; abstract;
    function GetState: TDebugStates; virtual; abstract;
    procedure Action(AAction: TDebugAction); virtual; abstract;
    function GetKey: string; virtual;

    property ExecutedLine: Integer read GetExecutedLine;
    property ExecutedControl: TCustomSynEdit read GetExecutedControl write SetExecutedControl;

    property Active: boolean read GetActive write SetActive;
    property Running: boolean read GetRunning;

    procedure SetExecutedLine(Key: string; Edit: TCustomSynEdit; const Line: integer); overload;
    procedure SetExecutedLine(Key: string; FileName: string; const Line: integer); overload;
    property Breakpoints: TEditorBreakPoints read FBreakpoints;
    property Watches: TEditorWatches read FWatches;
  end;

implementation

uses
  EditorEngine;

{ TEditorDebugger }

function TEditorDebugger.GetCaption: string;
begin
  Result := 'Debug';
end;

function TEditorDebugger.GetRunning: boolean;
begin
  Result := dbsRunning in GetState;
end;

procedure TEditorDebugger.SetActive(AValue: boolean);
begin
  if not Active and AValue then
    Action(dbaStart)
  else if Active and not AValue then
    Action(dbaStop);
end;

procedure TEditorDebugger.SetExecutedControl(const AValue: TCustomSynEdit);
begin
  FLink.ExecutedControl := AValue;
end;

constructor TEditorDebugger.Create;
begin
  inherited;
  FLink := TEditorDebugLink.Create(nil);
  FBreakpoints := CreateBreakPoints;
  FWatches := CreateWatches;
end;

destructor TEditorDebugger.Destroy;
begin
  FLink.ExecutedControl := nil;//just for safe free
  FreeAndNil(FBreakpoints);
  FreeAndNil(FWatches);
  FreeAndNil(FLink);
  inherited;
end;

function TEditorDebugger.GetActive: Boolean;
begin
  Result := dbsActive in GetState;
end;

function TEditorDebugger.GetExecutedControl: TCustomSynEdit;
begin
  Result := FLink.ExecutedControl;
end;

function TEditorDebugger.GetExecutedLine: Integer;
begin
  Result := FLink.ExecutedLine;
end;

function TEditorDebugger.GetKey: string;
begin
  Result := FKey;
end;

procedure TEditorDebugger.SetExecutedLine(Key: string; Edit: TCustomSynEdit; const Line: integer);
var
  OldLine: integer;
  OldEdit: TCustomSynEdit;
begin
  FKey := Key;
  if (FLink.ExecutedControl <> Edit) or (FLink.ExecutedLine <> Line) then
  begin
    OldLine := FLink.ExecutedLine;
    OldEdit := FLink.ExecutedControl;

    FLink.ExecutedLine := Line;
    FLink.ExecutedControl := Edit;

    if OldEdit <> nil then
      OldEdit.InvalidateLine(OldLine);

    if ExecutedControl <> nil then
    begin
      ExecutedControl.CaretY := FLink.ExecutedLine;
      ExecutedControl.CaretX := 1;
      ExecutedControl.InvalidateLine(FLink.ExecutedLine);
    end;
    Engine.UpdateState([ecsDebug, ecsShow]);
  end;
end;

procedure TEditorDebugger.SetExecutedLine(Key: string; FileName: string; const Line: integer);
var
  aFile: TEditorFile;
begin
  if FileName <> '' then
  begin
    aFile := Engine.Files.ShowFile(FileName);
    if (aFile is ISourceEditor) then //{$warning 'bad beavor, this class must be outside the engine'}
      SetExecutedLine(Key, (aFile as TTextEditorFile).SynEdit, Line);
  end
  else
    SetExecutedLine(Key, nil, -1);
end;

procedure TEditorDebugLink.SetExecutedExit(const AValue: TCustomSynEdit);
begin
  if FExecutedControl <> AValue then
  begin
    if FExecutedControl <> nil then
      RemoveFreeNotification(FExecutedControl);
    FExecutedControl :=AValue;
    if FExecutedControl <> nil then
      FreeNotification(FExecutedControl)
  end;
end;

procedure TEditorDebugLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FExecutedControl) then
  begin
    FExecutedControl := nil;
    FExecutedLine := 0;
  end;
end;

constructor TEditorDebugLink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FExecutedLine := -1;
end;

end.

