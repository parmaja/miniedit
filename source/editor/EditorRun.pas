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
  mnConnections, mnServers, Variants,
  mnStreams, EditorClasses, mnClasses, mnXMLUtils;

{$i '..\lib\mne.inc'}

type
  { TRunCommand }

  TRunCommand = record
    Command: string;
    Params: string;
    Pause: Boolean;
    Silent: Boolean;
    CatchOutput: Boolean;
    Console: Boolean;
    procedure AddParam(AParam: string);
  end;

  TmneRunAction = (
    rnaCompile, //Build
    rnaLint,
    rnaLink,
    rnaExecute, //Execute , Build + Execute = Run
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
    FEnvironment: TStringList;
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
    destructor Destroy; override;
    property BreakOnFail: Boolean read FBreakOnFail write FBreakOnFail;
    property Process: TProcess read FProcess;
    property MessageType: TNotifyMessageType read FMessageType write FMessageType;
    property Environment: TStringList read FEnvironment;
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

  TDebugCommandFlag = (dafSend, dafRespond, dafCheckError, dafStopOnError);
  TDebugCommandFlags = set of TDebugCommandFlag;

  { TDebugCommand }

  TDebugCommand = class(TObject)
  private
    FKeepAlive: Boolean;
    FKey: string;
    FFlags: TDebugCommandFlags;
    FEvent: TEvent; //must be nil until we need one
  protected
    FTransactionID: integer;
    function GetData: string; virtual;
    function Stay: boolean; virtual;
    function Enabled: boolean; virtual;
    function Accept: boolean; virtual;
    procedure Created; virtual; //after create it
    procedure Prepare; virtual; //after pop from Queue
    property Key: string read FKey;
    property Flags: TDebugCommandFlags read FFlags write FFlags;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Process; virtual;
    procedure CreateEvent; virtual;
    procedure FreeEvent; virtual;
    property Event: TEvent read FEvent;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive; //do no free it
  end;

  TDebugConnection = class;

  TDebugServerAction = class(TDebugCommand)
  private
    FConnection: TDebugConnection;
  protected
    property Connection: TDebugConnection read FConnection;
  public
  end;

  TDebugServerActionClass = class of TDebugServerAction;

  { TDebugQueue }

  TDebugQueue = class(specialize TmnObjectList<TDebugServerAction>)
  private
  public
  end;

  TDebugServer = class;

  //* Watches

  { TDebugWatch }

  TDebugWatch = class(TObject)
  private
    FHandle: integer;
  public
    Info: TDebugWatchInfo;
    property Handle: integer read FHandle write FHandle;
  published
  end;

  { TDebugWatches }

  TDebugWatches = class(specialize TmnObjectList<TDebugWatch>)
  private
    FServer: TDebugServer;
    CurrentHandle: integer;
    function GetValues(Name: string): variant;
    procedure SetValues(Name: string; const Value: variant);
  protected
    property Server: TDebugServer read FServer;
  public
    function Find(Name: string): TDebugWatch;
    function Add(Name: string; Value: variant): integer; overload;
    procedure AddWatch(Name: string);
    procedure RemoveWatch(Name: string);
    procedure Clean;
    property Values[Name: string]: variant read GetValues write SetValues;
  end;

//* Breakpoints

  { TDebugBreakpoint }

  TDebugBreakpoint = class(TObject)
  private
    FDeleted: Boolean;
    FID: integer;
    FLine: integer;
    FHandle: Integer;
    FFileName: string;
  public
    property Handle: Integer read FHandle write FHandle;
    property ID: integer read FID write FID;
    property Deleted: Boolean read FDeleted write FDeleted;
  published
    property FileName: string read FFileName write FFileName;
    property Line: integer read FLine write FLine;
  end;

  { TDebugBreakpoints }

  TDebugBreakpoints = class(specialize TmnObjectList<TDebugBreakpoint>)
  private
    CurrentHandle: Integer;
    FServer: TDebugServer;
  protected
    property Server: TDebugServer read FServer;
  public
    function Add(FileName: string; Line: integer): integer; overload;
    procedure Remove(Handle: Integer); overload;
    procedure ForceRemove(Handle: Integer); overload;
    function Find(Name: string; Line: integer; WithDeleted: Boolean = False): TDebugBreakpoint; overload;
    procedure Toggle(FileName: string; Line: integer);
    procedure Clean;
  end;

  TDebugOnServerEvent = procedure(Sender: TObject; Socket: TDebugConnection) of object;

  { TDebugServerQueue }

  TDebugServerQueue = class(TDebugQueue)
  private
    FConnection: TDebugConnection;
  public
    procedure Added(Action: TDebugServerAction); override;
  end;

  { TDebugConnection }

  TDebugConnection = class(TmnServerConnection)
  private
    FQueue: TDebugServerQueue;
    FKey: string;
    function GetServer: TDebugServer;
  public
    FTransactionID: integer;
  protected
{$IFDEF SAVELOG}
    procedure SaveLog(s: string);
{$ENDIF}
    function PopAction: TDebugServerAction;
    procedure Prepare; override;
    procedure DoExecute;
    procedure Process; override;
    procedure TerminatedSet; override;
  public
    constructor Create(vOwner: TmnConnections; vStream: TmnConnectionStream);
    destructor Destroy; override;
    function NewTransactionID: Integer;
    property Key: string read FKey write FKey;
    property Server: TDebugServer read GetServer;
    property Queue: TDebugServerQueue read FQueue;
  published
  end;

  { TDebugListener }

  TDebugListener = class(TmnListener)
  private
  protected
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; override;
  public
  end;

  TDebugServer = class(TmnServer)
  private
    FWatches: TDebugWatches;
    FBreakpoints: TDebugBreakpoints;
    FBreakOnFirstLine: Boolean;
    FQueue: TDebugServerQueue;
    FStackDepth: Integer;
    FRunCount: Integer;
    FKey: string;
  protected
    function GetIsRuning: Boolean; virtual;
    function CreateListener: TmnListener; override;
    procedure DoChanged(vListener: TmnListener); override;
    procedure DoStart; override;
    procedure DoBeforeOpen; override;
    procedure DoStop; override;
    procedure WatchAdded; virtual;
    procedure BreakPointAdded; virtual;
    property Queue: TDebugServerQueue read FQueue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Resume;
    procedure AddAction(vAction: TDebugServerAction); overload;
    procedure AddAction(vActionClass: TDebugServerActionClass); overload;
    procedure RemoveAction(vAction: TDebugServerAction);
    procedure ExtractAction(vAction: TDebugServerAction);
    procedure Clear;
    property IsRuning: Boolean read GetIsRuning;
    property StackDepth: Integer read FStackDepth write FStackDepth;
    property BreakOnFirstLine: Boolean read FBreakOnFirstLine write FBreakOnFirstLine default False;
    property Key: string read FKey;
    property RunCount: Integer read FRunCount; //count of waiting action
    property Watches: TDebugWatches read FWatches;
    property Breakpoints: TDebugBreakpoints read FBreakpoints;
  published
  end;

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
  FEnvironment := TStringList.Create;
end;

destructor TmneRunItem.Destroy;
begin
  FreeAndNil(FEnvironment);
  inherited Destroy;
end;

procedure TmneRunItem.CreateConsole(AInfo: TmneCommandInfo);
var
  ProcessObject: TmnProcessObject;
  aOptions: TProcessOptions;
  i: Integer;
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
  for i := 0 to GetEnvironmentVariableCount -1 do
     FProcess.Environment.Add(GetEnvironmentString(i));
  FProcess.Environment.AddStrings(Environment);

  aOptions := [];
  if Info.Suspended then
    aOptions := [poRunSuspended];

  if Info.Run.Silent then
  begin
    FProcess.ShowWindow := swoHide;
    //FProcess.CloseInput;
  end
  else
    FProcess.ShowWindow := swoShow;

  if FCatchOutput then
  begin
    FProcess.Options :=  aOptions + [poUsePipes, poStderrToOutPut, poNewProcessGroup];
    ProcessObject := TmnProcessObject.Create(FProcess, FPool, @WriteOutput);
    //FProcess.PipeBufferSize := 0; //80 char in line
    try
      try
        FProcess.Execute;
        Status := ProcessObject.ReadStream(FProcess.Output);
        except
          on E: Exception do
          begin
            Status := Process.ExitStatus;
            WriteMessage(FProcess.Executable + ' ' + E.Message, msgtOutput);
            if Status = 0 then
              Status := $1000; //$1000? idk just random number
          end
        end;
    finally
      FreeAndNil(FProcess);
      FreeAndNil(ProcessObject);
    end;
  end
  else
  begin
    FProcess.Options :=  aOptions + [poWaitOnExit];
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
      FCatchOutput := Info.Run.CatchOutput;
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

procedure TDebugCommand.Process;
begin
end;

procedure TDebugCommand.Created;
begin
  Flags := [dafSend, dafRespond];
end;

{ TDebugWatches }

function TDebugWatches.Add(Name: string; Value: variant): integer;
var
  aWatch: TDebugWatch;
begin
  Inc(CurrentHandle);
  aWatch := TDebugWatch.Create;
  aWatch.Handle := CurrentHandle;
  aWatch.Info.Name := Name;
  aWatch.Info.VarType := '';
  aWatch.Info.Value := Value;
  Result := Add(aWatch);
end;

procedure TDebugWatches.AddWatch(Name: string);
begin
  DebugManager.Enter;
  try
    Add(Name, '');
  finally
    DebugManager.Leave;
  end;
  if Server.IsRuning then
  begin
    DebugManager.Enter;
    try
      Server.WatchAdded;
    finally
      DebugManager.Leave;
    end;
    Server.Resume;
  end;
end;

procedure TDebugWatches.Clean;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    VarClear(Items[i].Info.Value);
  end;
end;

function TDebugWatches.Find(Name: string): TDebugWatch;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].Info.Name = Name then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TDebugWatches.GetValues(Name: string): variant;
var
  aWatch: TDebugWatch;
begin
  aWatch := Find(Name);
  if aWatch <> nil then
    Result := aWatch.Info.Value
  else
    VarClear(Result);
end;

procedure TDebugWatches.RemoveWatch(Name: string);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Info.Name = Name then
    begin
      Delete(i);
      break;
    end;
  end;
  if Server.IsRuning then
  begin
    DebugManager.Enter;
    try
      Server.BreakPointAdded;
    finally
      DebugManager.Leave;
    end;
    Server.Resume;
  end;
end;

procedure TDebugWatches.SetValues(Name: string; const Value: variant);
begin
end;

{ TDebugBreakpoints }

function TDebugBreakpoints.Add(FileName: string; Line: integer): integer;
var
  aBreakpoint: TDebugBreakpoint;
begin
  Result := -1;
  Inc(CurrentHandle);
  aBreakpoint := Find(FileName, Line, True);
  if aBreakpoint = nil then
  begin
    aBreakpoint := TDebugBreakpoint.Create;
    aBreakpoint.Handle := CurrentHandle;
    aBreakpoint.FileName := FileName;
    aBreakpoint.Line := Line;
    Result := Add(aBreakpoint);
  end
  else if aBreakpoint.Deleted then
    aBreakpoint.Deleted := False
  else
    Raise Exception.Create('Break point already exists');
end;

function TDebugBreakpoints.Find(Name: string; Line: integer; WithDeleted: Boolean): TDebugBreakpoint;
var
  aItem: TDebugBreakpoint;
begin
  Result := nil;
  for aItem in Self do
  begin
    if (not aItem.Deleted or WithDeleted) and (aItem.line = Line) and SameText(aItem.FileName, Name) then
    begin
      Result := aItem;
      break;
    end;
  end;
end;

procedure TDebugBreakpoints.Remove(Handle: Integer);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Handle = Handle then
    begin
      if Items[i].ID > 0 then
        Items[i].Deleted := True
      else
        Delete(i);
      break;
    end;
  end;
end;

procedure TDebugBreakpoints.ForceRemove(Handle: Integer);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Handle = Handle then
    begin
      Delete(i);
      break;
    end;
  end;
end;

procedure TDebugBreakpoints.Toggle(FileName: string; Line: integer);
var
  aBreakpoint: TDebugBreakpoint;
  //aSetBreakpoint: TDebugSetBreakpoint;
  //aRemoveBreakpoint: TDebugRemoveBreakpoint;
begin
  aBreakpoint := Find(FileName, Line, True); //lookup it as normal
  if (aBreakpoint <> nil) and (not aBreakpoint.Deleted) then
  begin
    if aBreakpoint.ID > 0 then
      aBreakpoint.Deleted := True //Will be removed by action
    else
      Remove(ABreakpoint);
  end
  else
  begin
    if aBreakpoint <> nil then
      aBreakpoint.Deleted := False
    else
      Add(FileName, Line);
  end;
end;

procedure TDebugBreakpoints.Clean;
var
  i: Integer;
begin
  i := 0;
  while i < Count do
  begin
    if Items[i].Deleted then
      Delete(i)
    else
    begin
      Items[i].ID := 0;
      Inc(i);
    end;
  end;
end;

{ TDebugServerQueue }

procedure TDebugServerQueue.Added(Action: TDebugServerAction);
begin
  inherited Added(Action);
  Action.FConnection := FConnection;
end;

{ TDebugConnection }

function TDebugConnection.GetServer: TDebugServer;
begin
  Result := (Listener.Server as TDebugServer);
end;

function TDebugConnection.NewTransactionID: integer;
begin
  Inc(FTransactionID);
  Result := FTransactionID;
end;

function TDebugConnection.PopAction: TDebugServerAction;
var
  aAction: TDebugServerAction;
  i: integer;
begin
  if FQueue.Count = 0 then
  begin
    InterLockedIncrement(Server.FRunCount);
    DebugManager.Event.WaitFor(INFINITE); //wait the ide to make resume
    InterLockedDecrement(Server.FRunCount);

    DebugManager.Enter;
    try
      i := 0;
      while i < Server.Queue.Count do
      begin
        aAction := Server.Queue.Extract(Server.Queue[i]);
        //        if aAction.Key = Key then
        FQueue.Add(aAction);
        //        else
        //        inc(i);
      end;
    finally
      DebugManager.Leave;
    end;
  end;
  Result := nil;
  while not Terminated and ((FQueue.Count > 0) and (Result = nil)) do
  begin
    Result := FQueue[0];
    Result.Prepare;
  end;
end;

procedure TDebugConnection.Prepare;
begin
  inherited Prepare;
end;

procedure TDebugConnection.DoExecute;
begin
end;

procedure TDebugConnection.Process;
var
  aAction: TDebugServerAction;
  aKeep: Boolean;
begin
  inherited;
  //Allow one connection to Execute
  //Listener.Enter;
  try
    DoExecute;
    aAction := PopAction;
    if aAction <> nil then
    begin
      if not aAction.Enabled then
        FQueue.Remove(aAction)
      else
        try
          if Connected then
            aAction.Process;
        finally
          if not aAction.Stay then
          begin
            aKeep := (aAction.Event <> nil) or aAction.KeepAlive;
            if aAction.Event <> nil then
              aAction.Event.SetEvent;
            if aKeep then
              FQueue.Extract(aAction)
            else
              FQueue.Remove(aAction);
          end;
        end;
    end;
  finally
    //Listener.Leave;
  end;
end;

procedure TDebugConnection.TerminatedSet;
begin
  inherited TerminatedSet;
  DebugManager.Event.SetEvent;
end;

constructor TDebugConnection.Create(vOwner: TmnConnections; vStream: TmnConnectionStream);
begin
  inherited;
  FQueue := TDebugServerQueue.Create;
  FQueue.FConnection := Self;
  //KeepAlive := True;
  Stream.ReadTimeout := 5000;
end;

destructor TDebugConnection.Destroy;
begin
  FreeAndNil(FQueue);
  inherited;
end;

{ TDebugListener }

function TDebugListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TDebugConnection.Create(Self, vStream);
end;

function TDebugServer.GetIsRuning: Boolean;
begin
  Result := RunCount > 0;
end;

function TDebugServer.CreateListener: TmnListener;
begin
  Result := TDebugListener.Create;
end;

procedure TDebugServer.DoChanged(vListener: TmnListener);
begin
  inherited;
  if vListener.Count = 0 then //TODO: i am not sure in Linux
    Engine.DebugLink.SetExecutedLine('', '', 0);
end;

procedure TDebugServer.DoStart;
begin
  inherited DoStart;
end;

procedure TDebugServer.DoBeforeOpen;
begin
  Queue.Clear;
  inherited;
end;

procedure TDebugServer.DoStop;
begin
  inherited;
  if FQueue <> nil then //DoStop class when Server free
    FQueue.Clear;
end;

procedure TDebugServer.WatchAdded;
begin
end;

procedure TDebugServer.BreakPointAdded;
begin
end;

constructor TDebugServer.Create;
begin
  inherited Create;
  FQueue := TDebugServerQueue.Create(True);
  FStackDepth := 10;
  FBreakOnFirstLine := False;
  FWatches := TDebugWatches.Create;
  FWatches.FServer := Self;
  FBreakpoints := TDebugBreakpoints.Create;
  FBreakpoints.FServer := Self;
  FBreakpoints.FServer := Self;
end;

destructor TDebugServer.Destroy;
begin
  FreeAndNil(FQueue);
  FreeAndNil(FWatches);
  FreeAndNil(FBreakpoints);
  inherited;
end;

procedure TDebugServer.Resume;
begin
  DebugManager.Event.SetEvent;
end;

procedure TDebugServer.AddAction(vAction: TDebugServerAction);
begin
  DebugManager.Enter;
  try
    Queue.Add(vAction);
  finally
    DebugManager.Leave;
  end;
end;

procedure TDebugServer.AddAction(vActionClass: TDebugServerActionClass);
begin
  AddAction(vActionClass.Create);
end;

procedure TDebugServer.RemoveAction(vAction: TDebugServerAction);
begin
  DebugManager.Enter;
  try
    Queue.Remove(vAction);
  finally
    DebugManager.Leave;
  end;
end;

procedure TDebugServer.ExtractAction(vAction: TDebugServerAction);
begin
  DebugManager.Enter;
  try
    Queue.Extract(vAction);
  finally
    DebugManager.Leave;
  end;
end;

procedure TDebugServer.Clear;
begin
  DebugManager.Enter;
  try
    Queue.Clear;
  finally
    DebugManager.Leave;
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
  inherited;
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
