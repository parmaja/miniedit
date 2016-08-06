unit EditorDebugger;
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$INTERFACES CORBA}
{**
 * Mini Edit
 *
 * @license   GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  SysUtils, Forms, Variants, Classes, Controls, Graphics, Contnrs, SyncObjs,
  SynEdit, process, mnClasses,
  EditorClasses;

type
  TmneRunAction = (rnaCompile, rnaExecute, rnaDebug, rnaLink);
  TmneRunActions = set of TmneRunAction;
  TmneRunMode = (runConsole, runProcess, runEmbedded, runOutput, runBrowser);

  { TmneRunInfo }

  TmneRunInfo = record
    Actions: TmneRunActions;
    Mode: TmneRunMode;
    Root: string; //cur dir for the project
    Pause: Boolean;
    Link: string; //URL to show in browser
    Command: string;
    MainFile: string; //file to compile
    OutputFile: string; //file to generate
    RunFile: string; //file to run
  end;

  TmneCommandInfo = record
    Title: string; //Console title
    Message: string; //Message send to output before execute it
    Mode: TmneRunMode;
    Command: string;
    Params: string;
    CurrentDirectory: string;
    Pause: Boolean;
    Link: string; //URL to show in browser
    Suspended: Boolean; //For attach with debugger
    DebugIt: Boolean;
    function GetCommandLine: string;
  end;

  TDebugWatchInfo = record
    Name: string;
    Value: variant;
    VarType: string;
  end;

  TDebugBreakpointInfo = record
    Handle: Integer;
    FileName: string;
    Line: Integer;
  end;

  { TCallStackItem }

  TCallStackItem = class(TObject)
  private
    FLine: Integer;
    FFileName: string;
  public
  published
    property FileName: string read FFileName write FFileName;
    property Line: integer read FLine write FLine;
  end;

  { TCallStackItems }

  TCallStackItems = class(specialize GItems<TCallStackItem>)
  private
  protected
  public
    function Add(FileName: string; Line: integer): integer; overload;
    procedure AssignFrom(vItems: TCallStackItems);
  end;

  TDebugAction = (
    dbaStartServer,
    dbaStopServer,
    dbaReset, //stop debug and stop the run
    dbaResume, //run and do not stop at breakpoints, or run without debug
    dbaStepOver,
    dbaStepInto,
    dbaStepOut,
    dbaRun
   );

   TDebugState = (
    dbsActive, //Server is active
    dbsRunning, //There is program in execute
    dbsDebugging //There is program in execute and debugged
   );

   TDebugStates = set of TDebugState;


  { TEditorItem }

  TEditorItem = class(TObject)
  protected
    function GetCount: Integer; virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    property Count: Integer read GetCount;
  end;

  { TEditorBreakPoints }

  TEditorBreakPoints = class(TEditorItem)
  protected
    function GetItems(Index: Integer): TDebugBreakpointInfo; virtual; abstract;
  public
    procedure Toggle(FileName: string; LineNo: Integer); virtual; abstract;
    function IsExists(FileName: string; LineNo: Integer): boolean; virtual; abstract;
    procedure Add(FileName: string; LineNo: Integer); virtual; abstract;
    procedure Remove(FileName: string; Line: Integer); virtual; overload; abstract;
    procedure Remove(Handle: Integer); virtual; overload; abstract;

    property Items[Index: Integer]: TDebugBreakpointInfo read GetItems; default;
  end;

  { TEditorWatches }

  TEditorWatches = class(TEditorItem)
  private
  protected
    function GetItems(Index: Integer): TDebugWatchInfo; virtual; abstract;
  public
    procedure Add(vName: string); virtual; abstract;
    procedure Remove(vName: string); virtual; abstract;
    function GetValue(vName: string; out vValue: Variant; out vType: string; EvalIt: Boolean): boolean; virtual; abstract;
    property Items[Index: Integer]: TDebugWatchInfo read GetItems; default;
  end;

  { TEditorDebugLink }

  TEditorDebugLink = class(TComponent) //to use Notification :P
  private
    procedure SetExecutedControl(const AValue: TCustomSynEdit);
  public
    FExecutedLine: Integer;
    FExecutedControl: TCustomSynEdit;
    FCallStack: TCallStackItems;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetExecutedLine(Key: string; Edit: TCustomSynEdit; const Line: Integer; vCallStack: TCallStackItems = nil);
    procedure SetExecutedLine(Key: string; FileName: string; const Line: Integer; vCallStack: TCallStackItems = nil);
    property ExecutedLine: Integer read FExecutedLine;// write FExecutedLine;
    property ExecutedControl: TCustomSynEdit read FExecutedControl write SetExecutedControl;
    property CallStack: TCallStackItems read FCallStack;
  end;

  { TEditorDebugger }

  TEditorDebugger = class abstract(TObject)
  private
    FBreakpoints: TEditorBreakPoints;
    FWatches: TEditorWatches;
    FKey: string;
    //FLink: TEditorDebugLink;
    function GetActive: Boolean;
    function GetRunning: boolean;
    procedure SetActive(AValue: boolean);
  protected
    function GetCaption: string; virtual;
    function CreateBreakPoints: TEditorBreakPoints; virtual; abstract;
    function CreateWatches: TEditorWatches; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start; virtual;
    procedure Attach(SubProcess: TProcess; Resume: Boolean); virtual;
    procedure Stop; virtual;

    procedure Lock; virtual;
    procedure Unlock; virtual;
    function GetState: TDebugStates; virtual; abstract;
    procedure Action(AAction: TDebugAction); virtual; abstract;
    function GetKey: string; virtual;

    property Active: boolean read GetActive write SetActive;
    property Running: boolean read GetRunning;

    property Breakpoints: TEditorBreakPoints read FBreakpoints;
    property Watches: TEditorWatches read FWatches;
  end;


  { TDebugManager }

  TDebugManager = class(TObject)
  private
   public
    Lock: TCriticalSection;
    Event: TEvent;
    constructor Create;
    destructor Destroy; override;
  end;

function DebugManager: TDebugManager;

implementation

uses
  EditorEngine;

var
  FDebugManager: TDebugManager = nil;

function DebugManager: TDebugManager;
begin
  if FDebugManager = nil then
    FDebugManager := TDebugManager.Create;
  Result := FDebugManager;
end;

{ TDebugManager }

constructor TDebugManager.Create;
begin
  inherited;
  Lock := TCriticalSection.Create;
  Event := TEvent.Create(nil, False, False, '');
end;

destructor TDebugManager.Destroy;
begin
  FreeAndNil(Event);
  FreeAndNil(Lock);
  inherited;
end;

{ TmneRunInfo }

function TmneCommandInfo.GetCommandLine: string;
begin
  Result := Command;
  if Params <> '' then
    Result := Result + ' ' + StringReplace(Params, #13, ' ', [rfReplaceAll]);
end;

{ TCallStackItems }

function TCallStackItems.Add(FileName: string; Line: integer): integer;
var
  aItem: TCallStackItem;
begin
  aItem := TCallStackItem.Create;
  aItem.FileName := FileName;
  aItem.Line := Line;
  Result := Add(aItem);
end;

procedure TCallStackItems.AssignFrom(vItems: TCallStackItems);
var
  i: Integer;
begin
  Clear;
  if vItems <> nil then
    for i := 0 to vItems.Count -1 do
    begin
       Add(vItems[i].FileName, vItems[i].Line);
    end;
end;

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
    Action(dbaStartServer)
  else if Active and not AValue then
    Action(dbaStopServer);
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

procedure TEditorDebugger.Start;
begin

end;

procedure TEditorDebugger.Attach(SubProcess: TProcess; Resume: Boolean);
begin

end;

procedure TEditorDebugger.Stop;
begin

end;

procedure TEditorDebugger.Lock;
begin
  DebugManager.Lock.Enter;
end;

procedure TEditorDebugger.Unlock;
begin
  DebugManager.Lock.Leave
end;

function TEditorDebugger.GetActive: Boolean;
begin
  Result := dbsActive in GetState;
end;

function TEditorDebugger.GetKey: string;
begin
  Result := FKey;
end;

procedure TEditorDebugLink.SetExecutedLine(Key: string; Edit: TCustomSynEdit; const Line: Integer; vCallStack: TCallStackItems);
var
  OldLine: Integer;
  OldEdit: TCustomSynEdit;
begin
//  FKey := Key;
  if (ExecutedControl <> Edit) or (ExecutedLine <> Line) then
  begin
    OldLine := ExecutedLine;
    OldEdit := ExecutedControl;

    FExecutedLine := Line;
    ExecutedControl := Edit;
    CallStack.AssignFrom(vCallStack);

    if OldEdit <> nil then
      OldEdit.InvalidateLine(OldLine);

    if ExecutedControl <> nil then
    begin
      ExecutedControl.CaretY := ExecutedLine;
      ExecutedControl.CaretX := 1;
      ExecutedControl.InvalidateLine(ExecutedLine);
    end;
    if Line >= 0 then
      Engine.UpdateState([ecsDebug, ecsShow])
    else
    begin
      Engine.UpdateState([ecsDebug]); //Do not show mainform if there is no line to set
      Engine.Session.Run.Show;
    end;
  end
  else
  begin
    Engine.UpdateState([ecsDebug]);//needed for update watches
    Engine.Session.Run.Show;
  end;
end;

procedure TEditorDebugLink.SetExecutedLine(Key: string; FileName: string; const Line: Integer; vCallStack: TCallStackItems);
var
  aFile: TEditorFile;
begin
  if FileName <> '' then
  begin
    aFile := Engine.Files.ShowFile(FileName);
    if (aFile is ITextEditor) then //{$warning 'bad beavor, this class must be outside the engine'}
      SetExecutedLine(Key, (aFile as TTextEditorFile).SynEdit, Line, vCallStack);
  end
  else
    SetExecutedLine(Key, nil, -1, vCallStack);
end;

procedure TEditorDebugLink.SetExecutedControl(const AValue: TCustomSynEdit);
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
  FCallStack := TCallStackItems.Create;
end;

destructor TEditorDebugLink.Destroy;
begin
  FreeAndNil(FCallStack);
  inherited Destroy;
end;


initialization
finalization
  FreeAndNil(FDebugManager);
end.

