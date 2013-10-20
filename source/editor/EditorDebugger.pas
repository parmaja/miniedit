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
    function GetCaption: string; virtual;
    procedure Click(Sender: TObject); virtual;
    procedure SetExecutedControl(const AValue: TCustomSynEdit);
  protected
    function GetActive: Boolean; virtual;
    procedure SetActive(const AValue: Boolean); virtual;
    function CreateBreakPoints: TEditorBreakPoints; virtual; abstract;
    function CreateWatches: TEditorWatches; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start; virtual;
    procedure Stop; virtual;

    procedure Reset; virtual; //stop debug and stop the run
    procedure Resume; virtual; //run and do not stop at breakpoints, or run without debug
    procedure StepInto; virtual;
    procedure StepOver; virtual;
    procedure StepOut; virtual;
    procedure Run; virtual;
    procedure Lock; virtual;
    procedure Unlock; virtual;
    function IsRuning: boolean; virtual;
    procedure RunTo(FileName: string; LineNo: integer); virtual;//todo runto

    property ExecutedLine: Integer read GetExecutedLine;
    property ExecutedControl: TCustomSynEdit read GetExecutedControl write SetExecutedControl;

    function GetKey: string; virtual;
    property Active: boolean read GetActive write SetActive;

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

procedure TEditorDebugger.Click(Sender: TObject);
begin
  Active := not Active;
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

function TEditorDebugger.GetActive: boolean;
begin
  Result := False;
end;

function TEditorDebugger.GetExecutedControl: TCustomSynEdit;
begin
  Result := FLink.ExecutedControl;
end;

function TEditorDebugger.GetExecutedLine: Integer;
begin
  Result := FLink.ExecutedLine;
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

procedure TEditorDebugger.RunTo(FileName: string; LineNo: integer);
begin
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

