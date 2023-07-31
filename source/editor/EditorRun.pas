unit EditorRun;
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey 
 *}

interface

uses
  Forms, SysUtils, StrUtils, Classes, contnrs, SynEdit,
  mnUtils, ConsoleProcess, process, SyncObjs,
  mnStreams, EditorClasses, mnClasses, mnXMLUtils;

{$i '..\lib\mne.inc'}

type
  { TRunCommand }

  TRunCommand = record
    Command: string;
    Params: string;
    Pause: Boolean;
    Silent: Boolean;
    Console: Boolean;
    procedure AddParam(AParam: string);
  end;

  TmneRunAction = (
    rnaCompile, //Build
    rnaExecute, //Execute , Build + Execute = Run
    rnaLint,
    rnaLink,
    rnaDebug,
    rnaShow,
    rnaKill
  );

  TmneRunActions = set of TmneRunAction;

  TmneRunInfo = record
    Actions: TmneRunActions;

    Console: Boolean;
    Pause: Boolean;
    Command: string;

    Root: string; //cur dir for the project

    MainFile: string; //file to compile
    OutputFile: string; //file to generate
    RunFile: string; //file to run
  end;

  TmneCommandInfo = record
    Run: TRunCommand;
    Title: string; //Console title
    StatusMessage: string; //Message send to output before execute it
    CurrentDirectory: string;
    Suspended: Boolean; //For attach with debugger
    StartDebug: Boolean;
    function GetCommandLine: string;
  end;

  TmneRunErrorType = (
    errError,
    errWarning,
    errParse,
    errNotice
  );

  TmneRunLog = record
    Error: Integer;
    Caption: string;
    Msg: string;
    FileName: string;
    LineNo: Integer;
  end;

  TmneRun = class;
  TmneRunItem = class;
  TmneRunPool = class;

  { TmneRunItem }

  TmneRunItem = class(TObject)
  private
    FBreakOnFail: Boolean;
    FMessageType: TNotifyMessageType;
  protected
    FProcess: TProcess;
    FPool: TmneRunPool;
  protected
    FCatchOutput: Boolean;
    InternalString: string;
    InternalMessageType: TNotifyMessageType;
    procedure InternalMessage; //This for sync it, it will send to FWriteString
    procedure WriteOutput(S: string); //This assign it to consoles
    procedure WriteMessage(S: string; vMessageType: TNotifyMessageType = msgtLog); //This assign it to consoles
  protected
    //procedure CreateControl;
    procedure CreateConsole(AInfo: TmneCommandInfo);
    procedure Attach; //To Sync
    procedure Launch; //To Sync
  public
    Info: TmneCommandInfo;
    Status: integer;
    procedure Execute; virtual;
    procedure Stop; virtual;
    constructor Create(APool: TmneRunPool);
    property BreakOnFail: Boolean read FBreakOnFail write FBreakOnFail;
    property Process: TProcess read FProcess;
    property MessageType: TNotifyMessageType read FMessageType write FMessageType;
  end;

  TmneRunItemClass = class of TmneRunItem;

  { TmneRunItems }

  TmneRunItems = class(specialize TmnObjectList<TmneRunItem>);

  { TmneRunPool }

  TmneRunPool = class(TThread)
  protected
    FRun: TmneRun;
    FItems: TmneRunItems;
    FCurrent: TmneRunItem;
    procedure Update;
  public
    constructor Create(ARun: TmneRun);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Stop;
    procedure Show;
    property Items: TmneRunItems read FItems;
    property Current: TmneRunItem read FCurrent;
  end;

  TEditorDebugger = class;

  { TmneRun }

  TmneRun = class(TObject)
  private
    FPool: TmneRunPool;
    FDebugger: TEditorDebugger;
    FCurrentDirectory: string;
    function GetActive: Boolean;
  protected
    procedure PoolTerminated(Sender: TObject);
    procedure SendMessage(S: string; vMessageType: TNotifyMessageType);
    procedure Finish;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItemClass: TmneRunItemClass = nil): TmneRunItem; //Return same as parameter
    procedure Clear;
    procedure Show;
    procedure Start(ADebugger: TEditorDebugger; vCurrentDirectory: string = ''); //move vCurrentDirectory to RunItem
    procedure Stop;
    property Active: Boolean read GetActive;
    property Pool: TmneRunPool read FPool;
    property Debugger: TEditorDebugger read FDebugger;
    property CurrentDirectory: string read FCurrentDirectory write FCurrentDirectory;
  end;

  EDebugException = class(Exception);

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
    dbaActivate,
    dbaDeactivate,
    dbaReset, //stop debug and stop the run
    dbaResume, //run and do not stop at breakpoints, or run without debug
    dbaStepOver,
    dbaStepInto,
    dbaStepOut,
    dbaRun //run and stop on next breakpoint
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

  { TDebugCommand }

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
    procedure Attach(SubProcess: TProcess); virtual;
    procedure Launch(vFileName: string); virtual;
    procedure Stop; virtual;

    procedure SendMessage(S: string; vMessageType: TNotifyMessageType); virtual;

    function GetState: TDebugStates; virtual; abstract;
    procedure Action(AAction: TDebugAction); virtual; abstract;
    function GetKey: string; virtual;

    property Active: boolean read GetActive write SetActive;
    property Running: boolean read GetRunning;

    property Breakpoints: TEditorBreakPoints read FBreakpoints;
    property Watches: TEditorWatches read FWatches;
  end;

  { TTendency }

  TTendency = class(TEditorElement)
  private
  protected
    FDebugger: TEditorDebugger;
    procedure SetDebugger(AValue: TEditorDebugger);
  public
    procedure SendMessage(S: string; vMessageType: TNotifyMessageType); virtual; abstract;

    property Debugger: TEditorDebugger read FDebugger write SetDebugger;
  end;

  { TDebugManager }

  TDebugManager = class(TObject)
  strict private
    FLockCount: Integer;
    Lock: TCriticalSection;
  public
    Event: TEvent;
    procedure Enter;
    procedure Leave;
    constructor Create;
    destructor Destroy; override;
  end;

function DebugManager: TDebugManager;

implementation

uses
  {$ifdef windows}
  Windows,
  {$endif}
  EditorEngine, gdbClasses, lclintf;

{ TmneRunPool }

procedure TmneRunPool.Execute;
begin
  while not Terminated and (Items.Count > 0) do
  begin
    FCurrent := Items[0];
    Items.Extract(Current);
    Current.Execute;
    if Current.BreakOnFail and (Current.Status > 0) then
      Items.Clear;
    FreeAndNil(FCurrent);
    //Synchronize(@Update); //not yet
  end
end;

procedure TmneRunPool.Stop;
begin
  if FCurrent <> nil then
    FCurrent.Stop;
  Terminate;
end;
                            
{$ifdef windows}
function WindowsProc(windowHandle: HWND; lParam: LPARAM): Bool; stdcall;
var
  aProcessID: DWORD;
begin
  aProcessID := 0;
  GetWindowThreadProcessId(windowHandle, aProcessID);
  if (THANDLE(lParam) = aProcessID) then
  begin
    SetForegroundWindow(windowHandle);
    Result := False;
    exit;
  end;
  Result := True;
end;
{$endif}

procedure ShowProcess(ID: THandle);
begin  
  {$ifdef windows}
  EnumWindows(@WindowsProc, LPARAM(ID));       
  {$endif}
end;

procedure TmneRunPool.Show;
begin
  //TODO lock.Enter and Leave
  {$ifdef windows}
  if (Current.Process <> nil) and Current.Process.Active then
  begin
    ShowProcess(Current.Process.ProcessID);
  end;
  {$endif}
end;

procedure TmneRunPool.Update;
begin
  Engine.Update([ecsDebug]);
end;

constructor TmneRunPool.Create(ARun: TmneRun);
begin
  inherited Create(True);
  FItems := TmneRunItems.Create(True);
  FRun := ARun;
end;

destructor TmneRunPool.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

{ TmneRun }

function TmneRun.GetActive: Boolean;
begin
  Result := FPool <> nil;
end;

procedure TmneRun.PoolTerminated(Sender: TObject);
begin
  FPool := nil;
  Engine.Update([ecsDebug]);
  Finish;
end;

procedure TmneRun.SendMessage(S: string; vMessageType: TNotifyMessageType);
begin
{  if FDebug <> nil then
    FDebug.SendMessage(S, vMessageType)
  else}
    Engine.CurrentTendency.SendMessage(S, vMessageType); //TODO wrong, send it using Debugger
end;

procedure TmneRun.Finish;
begin
  FDebugger := nil;
  FCurrentDirectory := '';
  Engine.SendAction(eaEnd);
end;

constructor TmneRun.Create;
begin
  inherited Create;
end;

destructor TmneRun.Destroy;
begin
  Stop;
  inherited Destroy;
end;

procedure TmneRun.Start(ADebugger: TEditorDebugger; vCurrentDirectory: string);
begin
  if FPool <> nil then
  begin
    FCurrentDirectory := vCurrentDirectory;
    FDebugger := ADebugger;
    FPool.Start;
  end;
  //raise Exception.Create('There is no thread Pool');
end;

procedure TmneRun.Show;
begin
  if FPool <> nil then
    FPool.Show;
end;

procedure TmneRun.Stop;
begin
  if FPool <> nil then
  begin
    FPool.Stop;
    FPool.WaitFor;
    //Finish; //on terminate with call it
  end;
end;

function TmneRun.Add(AItemClass: TmneRunItemClass): TmneRunItem;
begin
  if FPool = nil then
  begin
    FPool := TmneRunPool.Create(Self);
    FPool.FreeOnTerminate := True;
    FPool.OnTerminate := @PoolTerminated;
  end;

  if AItemClass = nil then
    Result := TmneRunItem.Create(FPool)
  else
    Result := AItemClass.Create(FPool);
  FPool.Items.Add(Result);
end;

procedure TmneRun.Clear;
begin
  Stop;
  FreeAndNil(FPool);
end;

procedure TmneRunItem.Attach;
begin
  if Engine.Tendency.Debugger <> nil then
  begin
    Engine.Tendency.Debugger.Start;
    Engine.Tendency.Debugger.Attach(Process);
  end;
end;

procedure TmneRunItem.Launch;
begin
  if Engine.Tendency.Debugger <> nil then
  begin
    Engine.Tendency.Debugger.Start;
    Engine.Tendency.Debugger.Launch(Info.Run.Command);
  end;
end;

procedure TmneRunItem.WriteOutput(S: string);
begin
  InternalString := S;
  InternalMessageType := MessageType;
  FPool.Synchronize(@InternalMessage);
  InternalString := '';
end;

procedure TmneRunItem.InternalMessage;
begin
  //if not Engine.IsShutdown then //not safe to ingore it
  FPool.FRun.SendMessage(InternalString, InternalMessageType);
end;

procedure TmneRunItem.WriteMessage(S: string; vMessageType: TNotifyMessageType = msgtLog);
begin
  InternalString := S;
  InternalMessageType := vMessageType;
  FPool.Synchronize(@InternalMessage);
  InternalString := '';
end;

procedure TmneRunItem.Stop;
begin
  if FProcess <> nil then
    FProcess.Terminate(1);
end;

constructor TmneRunItem.Create(APool: TmneRunPool);
begin
  inherited Create;
  BreakOnFail := True;
  FPool := APool;
  FMessageType := msgtOutput;
end;

procedure TmneRunItem.CreateConsole(AInfo: TmneCommandInfo);
var
  ProcessObject: TmnProcessObject;
  aOptions: TProcessOptions;
begin
  if (AInfo.StatusMessage <> '') then
  begin
    WriteMessage(AInfo.StatusMessage, msgtStatus);
    WriteMessage(AInfo.StatusMessage + #13#10);
  end;

  FProcess := TProcess.Create(nil);
  FProcess.ConsoleTitle := Info.Title;
  FProcess.InheritHandles := True;
  FProcess.CurrentDirectory := ReplaceStr(AInfo.CurrentDirectory, '\', '/');
  WriteMessage('Directory: ' + FProcess.CurrentDirectory);
  FProcess.StartupOptions := [suoUseShowWindow]; //<- need it in linux to show window

  FProcess.Executable := ReplaceStr(AInfo.Run.Command, '\', '/');
  WriteMessage('Executable: ' + FProcess.Executable);
  WriteMessage('Params: ' + ReplaceStr(AInfo.Run.Params, #13, ' '));
  CommandToList(AInfo.Run.Params, FProcess.Parameters);

  aOptions := [];
  if Info.Suspended then
    aOptions := [poRunSuspended];

  if FCatchOutput then
  begin
    if Info.Run.Silent then
    begin
      FProcess.ShowWindow := swoHide;
      //FProcess.CloseInput;
    end
    else
      FProcess.ShowWindow := swoShow;
    FProcess.Options :=  aOptions + [poUsePipes, poStderrToOutPut, poNewProcessGroup];
    ProcessObject := TmnProcessObject.Create(FProcess, FPool, @WriteOutput);
    //FProcess.PipeBufferSize := 0; //80 char in line
    try
      try
        FProcess.Execute;
        Status := ProcessObject.ReadStream(FProcess.Output);
        except
          on E: Exception do
            WriteMessage(E.Message, msgtLog);
        end;
    finally
      FreeAndNil(FProcess);
      FreeAndNil(ProcessObject);
    end;
  end
  else
  begin
    FProcess.Options :=  aOptions + [poWaitOnExit];
    FProcess.ShowWindow := swoShow;
    //FProcess.CloseInput;
    FProcess.Execute;
  end;
  WriteMessage(#13#10'Exit: ' + IntToStr(Status), msgtLog);
  WriteMessage('Done', msgtStatus);
end;

procedure TmneRunItem.Execute;
var
  s: string;
{$ifdef windows}
{$else}
  term: string;
{$endif}
begin
  if Info.StartDebug then
  begin
    //Info.Run.Command := Info.GetCommandLine;
    //Info.Run.Params := '';
    //Info.Suspended := True;
    //CreateConsole(Info);
    //FPool.Synchronize(@Attach);
    //Process.Resume;
    FPool.Synchronize(@Launch);
  end
  else if Info.Run.Console then
  begin
    {$ifdef windows}
    s := '/c "'+ Info.GetCommandLine + '"';
    if Info.Run.Pause then
      s := s + ' & pause';
    Info.Run.Params := s;
    Info.Run.Command := 'cmd';
    {$else}
    //s := GetEnvironmentVariable('SHELL');
    term := GetEnvironmentVariable('COLORTERM');
    if term = '' then
       term := GetEnvironmentVariable('TERM');
    if term = '' then
       term := 'xterm';
    //xterm -e "lua lua-test.lua && bash"
    //xterm -e "lua lua-test.lua && read -rsp $''Press any key to continue'' -n1 key"
    if Info.Title <> '' then
        s := '-title "' + Info.Title + '"'
    else
        s := '';
    if term = 'xterm' then
        s := s + ' -fa "' + Engine.Options.Profile.Attributes.FontName+  '" -fs ' + IntToStr(Engine.Options.Profile.Attributes.FontSize);
    s := s + ' -e "'+Info.GetCommandLine;
    if Info.Run.Pause then
      s := s + '; read -rsp $''Press any key to continue'' -n1 key';
    s := s + '"';
    Info.Run.Params := s;
    Info.Run.Command := term;
    {$endif}
    if Info.Run.Silent then
      FCatchOutput := True;
    CreateConsole(Info);
  end
  else
  begin
    //Info.Run.Command := Info.GetCommandLine;
    if Info.Run.Silent then
      FCatchOutput := True;
    CreateConsole(Info);
  end
end;

{ ------------------------------------------------------------------------------- }

var
  FDebugManager: TDebugManager = nil;

function DebugManager: TDebugManager;
begin
  if FDebugManager = nil then
    FDebugManager := TDebugManager.Create;
  Result := FDebugManager;
end;

{ TRunCommand }

procedure TRunCommand.AddParam(AParam: string);
begin
  if AParam <> '' then
  begin
    if Params <> '' then
      Params := Params + #13;
    Params := Params + AParam;
  end;
end;

{ TDebugManager }

procedure TDebugManager.Enter;
begin
  {$ifdef DEBUG}
  Inc(FLockCount);
  //Debug.Write('--->DebugManager Enter', FLockCount);
  {$endif}
  Lock.Enter;
end;

procedure TDebugManager.Leave;
begin
  Lock.Leave;
  {$ifdef DEBUG}
  //Debug.Write('DebugManager Leave <---', FLockCount);
  Dec(FLockCount);
  {$endif}
end;

constructor TDebugManager.Create;
begin
  inherited;
  Lock := SyncObjs.TCriticalSection.Create;
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
    Action(dbaActivate)
  else if Active and not AValue then
    Action(dbaDeactivate);
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

procedure TEditorDebugger.Attach(SubProcess: TProcess);
begin
end;

procedure TEditorDebugger.Launch(vFileName: string);
begin
end;

procedure TEditorDebugger.Stop;
begin

end;

procedure TEditorDebugger.SendMessage(S: string; vMessageType: TNotifyMessageType);
begin
end;

function TEditorDebugger.GetActive: Boolean;
begin
  Result := (Self <> nil) and (dbsActive in GetState);
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
      Engine.Update([ecsDebug, ecsShow])
    else
    begin
      Engine.Update([ecsDebug]); //Do not show mainform if there is no line to set
      Engine.Session.Run.Show;
    end;
  end
  else
  begin
    Engine.Update([ecsDebug]);//needed for update watches
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

procedure TTendency.SetDebugger(AValue: TEditorDebugger);
begin
  if FDebugger =AValue then
    Exit;
  FDebugger :=AValue;
end;

initialization
finalization
  FreeAndNil(FDebugManager);
end.
