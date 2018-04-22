unit EditorRun;
{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  Forms, SysUtils, StrUtils, Classes, SyncObjs, contnrs,
  mnUtils, ConsoleProcess, process,
  mnStreams, mneConsoleForms, EditorClasses, EditorDebugger, mnClasses, mnXMLUtils;

{$i '..\lib\mne.inc'}

type
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
    FControl: TConsoleForm;
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
    procedure UpdateState;
  public
    constructor Create(ARun: TmneRun);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Stop;
    procedure Show;
    property Items: TmneRunItems read FItems;
    property Current: TmneRunItem read FCurrent;
  end;

  { TmneRun }

  TmneRun = class(TObject)
  private
    FPool: TmneRunPool;
    FTendency: TEditorDebugTendency;
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
    procedure Start(ATendency: TEditorDebugTendency; vCurrentDirectory: string = ''); //move vCurrentDirectory to RunItem
    procedure Show;
    procedure Stop;
    property Active: Boolean read GetActive;
    property Pool: TmneRunPool read FPool;
    property Tendency: TEditorDebugTendency read FTendency;
    property CurrentDirectory: string read FCurrentDirectory write FCurrentDirectory;
  end;

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
    //Synchronize(@UpdateState); //not yet
  end
end;

procedure TmneRunPool.Stop;
begin
  if FCurrent <> nil then
    FCurrent.Stop;
  Terminate;
  WaitFor;
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

procedure TmneRunPool.UpdateState;
begin
  Engine.UpdateState([ecsDebug]);
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
  Engine.UpdateState([ecsDebug]);
  Finish;
end;

procedure TmneRun.SendMessage(S: string; vMessageType: TNotifyMessageType);
begin
  if FTendency <> nil then
    FTendency.SendMessage(S, vMessageType)
  else
    Engine.SendMessage(S, vMessageType);
end;

procedure TmneRun.Finish;
begin
  FTendency := nil;
  FCurrentDirectory := '';
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

procedure TmneRun.Start(ATendency: TEditorDebugTendency; vCurrentDirectory: string);
begin
  FCurrentDirectory := vCurrentDirectory;
  FTendency := ATendency;
  if FPool = nil then
    raise Exception.Create('There is no thread Pool');
  FPool.Start;
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
  end;
  Finish;
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

{procedure TmneRunItem.CreateControl;
begin
  FControl := TConsoleForm.Create(Application);
  FControl.Parent := Engine.Container;
  Engine.Files.New('CMD: ' + Info.Title, FControl);

  FControl.CMDBox.Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
  FControl.CMDBox.BackGroundColor := Engine.Options.Profile.Attributes.Default.Background;
  FControl.ContentPanel.Color := FControl.CMDBox.BackGroundColor;

  FControl.CMDBox.Font.Name := Engine.Options.Profile.Attributes.FontName;
  FControl.CMDBox.Font.Size := Engine.Options.Profile.Attributes.FontSize;
  //FControl.CMDBox.GraphicalCharacterWidth := 14;

  FControl.CMDBox.TextColor(Engine.Options.Profile.Attributes.Default.Foreground);
  FControl.CMDBox.TextBackground(Engine.Options.Profile.Attributes.Default.Background);
  FControl.CMDBox.Write('Ready!'+#13#10);
  //FOnWrite := @FControl.WriteText;
  Engine.UpdateState([ecsRefresh]);
end;}

procedure TmneRunItem.Attach;
begin
  if Engine.Tendency.Debug <> nil then
  begin
    Engine.Tendency.Debug.Start;
    Engine.Tendency.Debug.Attach(Process);
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
    FProcess.Options :=  aOptions + [poUsePipes, poStderrToOutPut];
    ProcessObject := TmnProcessObject.Create(FProcess, FPool, @WriteOutput);
    //FProcess.PipeBufferSize := 0; //80 char in line
    try
      FProcess.Execute;
      Status := ProcessObject.ReadStream(FProcess.Output);
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
  WriteMessage('', msgtLog);
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
    Info.Run.Command := Info.GetCommandLine;
    Info.Run.Params := '';
    Info.Suspended := True;
    CreateConsole(Info);
    FPool.Synchronize(@Attach);
    Process.Resume;
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

end.
