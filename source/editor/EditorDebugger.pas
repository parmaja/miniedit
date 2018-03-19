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
  EDebugException = class(Exception);

  TmneRunAction = (rnaCompile, rnaLint, rnaExecute, rnaDebug, rnaLink);
  TmneRunActions = set of TmneRunAction;
  TmneRunMode = (runConsole, runOutput, runBox, runBrowser);

  { TmneRunInfo }

  TmneRunInfo = record
    Actions: TmneRunActions;

    Mode: TmneRunMode;
    Pause: Boolean;
    Command: string;
    MainFile: string; //file to compile

    Root: string; //cur dir for the project
    Link: string; //URL to show in browser
    OutputFile: string; //file to generate
    RunFile: string; //file to run
  end;

  TRunCommand = record
    Command: string;
    Params: string;
    Mode: TmneRunMode;
    Pause: Boolean;
  end;

  TmneCommandInfo = record
    Run: TRunCommand;
    Title: string; //Console title
    Message: string; //Message send to output before execute it
    CurrentDirectory: string;
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

  TCallStackItems = class(specialize TmnObjectList<TCallStackItem>)
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


  TDebugCommandFlag = (dafSend, dafCheckError, dafStopOnError);
  TDebugCommandFlags = set of TDebugCommandFlag;

  TDebugCommand = class(TObject)
  private
    FKeepAlive: Boolean;
    FKey: string;
    FFlags: TDebugCommandFlags;
    FEvent: TEvent; //must be nil until we need one
  protected
    function GetCommand: string; virtual;
    function GetData: string; virtual;
    function Stay: boolean; virtual;
    function Enabled: boolean; virtual;
    function Accept: boolean; virtual;
    procedure Created; virtual; //after create it
    procedure Prepare; virtual; //after pop from spool
    property Key: string read FKey;
    property Flags: TDebugCommandFlags read FFlags write FFlags;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CreateEvent; virtual;
    procedure FreeEvent; virtual;
    property Event: TEvent read FEvent;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive; //do no free it
  end;

  {TDebugCommandSpool = class(specialize GItems<TDebugCommand>)
  private
  public
  end;}

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
  Result := Run.Command;
  if Run.Params <> '' then
    Result := Result + ' ' + StringReplace(Run.Params, #13, ' ', [rfReplaceAll]);
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

{ TDebugCommand }

function TDebugCommand.GetCommand: string;
begin
  Result := '';
end;

function TDebugCommand.GetData: string;
begin
  Result := '';
end;

function TDebugCommand.Stay: boolean;
begin
  Result := False;
end;

function TDebugCommand.Accept: boolean;
begin
  Result := True;
end;

procedure TDebugCommand.CreateEvent;
begin
  if FEvent <> nil then
    raise EDebugException.Create('Event already exists');
  FEvent := TEvent.Create(nil, True, False, '');
end;

procedure TDebugCommand.FreeEvent;
begin
  FreeAndNil(FEvent);
end;

function TDebugCommand.Enabled: boolean;
begin
  Result := True;
end;

procedure TDebugCommand.Prepare;
begin
end;

constructor TDebugCommand.Create;
begin
  inherited Create;
  Created;
end;

destructor TDebugCommand.Destroy;
begin
  FreeEvent;
  inherited;
end;

procedure TDebugCommand.Created;
begin
  Flags := [dafSend];
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

