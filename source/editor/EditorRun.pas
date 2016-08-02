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
  mnStreams, mneConsoleForms, DebugClasses, mnClasses, mnXMLUtils;

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
  protected
    FProcess: TProcess;
    FControl: TConsoleForm;
    FPool: TmneRunPool;
  protected
    FOnWrite: TmnOnWrite;
    InternalString: string;
    InternalTemporary: Boolean;
    procedure InternalWrite; //This for sync it, it will send to FOnWriteString
    procedure WriteString(S: string); //This assign it to consoles
    procedure InternalMessage; //This for sync it, it will send to FWriteString
    procedure WriteMessage(S: string; Temporary: Boolean = False); //This assign it to consoles
  protected
    procedure CreateControl;
    procedure CreateConsole(AInfo: TmneCommandInfo);
  public
    Info: TmneCommandInfo;
    Status: integer;
    procedure Execute; virtual;
    procedure Stop; virtual;
    constructor Create(APool: TmneRunPool);
    property BreakOnFail: Boolean read FBreakOnFail write FBreakOnFail;
    property Process: TProcess read FProcess;
  end;

  TmneRunItemClass = class of TmneRunItem;

  { TmneRunItems }

  TmneRunItems = class(specialize GItems<TmneRunItem>);

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
    function GetActive: Boolean;
  protected
    procedure PoolTerminated(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItemClass: TmneRunItemClass = nil): TmneRunItem; //Return same as parameter
    procedure Clear;
    procedure Start;
    procedure Show;
    procedure Stop;
    property Active: Boolean read GetActive;
    property Pool: TmneRunPool read FPool;
  end;

implementation

uses
  {$ifdef windows}
  Windows,
  {$endif}
  EditorEngine, lclintf;

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

procedure TmneRun.Start;
begin
  if FPool = nil then
    raise Exception.Create('There is no thread Pool');
  FPool.Start;
end;

procedure TmneRun.Show;
begin
  if FPool <> nil then
    FPool.Show;
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

procedure TmneRunItem.CreateControl;
begin
  if Info.Mode = runEmbedded then
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
    FOnWrite := @FControl.WriteText;
    Engine.UpdateState([ecsRefresh]);
  end
  else
    FOnWrite := @Engine.SendOutout;
end;

procedure TmneRunItem.CreateConsole(AInfo: TmneCommandInfo);
var
  ProcessObject: TmnProcessObject;
begin
  if Assigned(FOnWrite) and (AInfo.Message <> '') then
    WriteString(AInfo.Message + #13#10);
  WriteMessage(AInfo.Message + #13#10);
  FProcess := TProcess.Create(nil);
  FProcess.ConsoleTitle := Info.Title;
  FProcess.Executable := AInfo.Command;
  FProcess.Parameters.Text := AInfo.Params;
  FProcess.CurrentDirectory := AInfo.CurrentDirectory;
  FProcess.InheritHandles := True;

  //WriteString(AInfo.Command + ' ' +FProcess.Parameters.Text);

  if Assigned(FOnWrite) then
  begin
    FProcess.Options :=  [poUsePipes, poStderrToOutPut];
    FProcess.ShowWindow := swoHIDE;
    FProcess.PipeBufferSize := 40; //80 char in line
    ProcessObject := TmnProcessObject.Create(FProcess, FPool, @WriteString);
    try
      Status := ProcessObject.Read;
    finally
      FreeAndNil(FProcess);
      FreeAndNil(ProcessObject);
    end;
  end
  else
  begin
    FProcess.Options :=  [poWaitOnExit];
    FProcess.ShowWindow := swoShow;
    FProcess.Execute;
  end;
  if Assigned(FOnWrite) then
    WriteString(#13#10'End Status: ' + IntToStr(Status)+#13#10);
  WriteMessage('Done', True);
end;

procedure TmneRunItem.InternalWrite;
begin
  if Assigned(FOnWrite) then
    FOnWrite(InternalString);
end;

procedure TmneRunItem.WriteString(S: string);
begin
  InternalString := S;
  FPool.Synchronize(@InternalWrite);
  InternalString := '';
end;

procedure TmneRunItem.InternalMessage;
begin
  Engine.SendMessage(InternalString, InternalTemporary);
end;

procedure TmneRunItem.WriteMessage(S: string; Temporary: Boolean);
begin
  InternalString := S;
  InternalTemporary:= Temporary;
  FPool.Synchronize(@InternalMessage);
  InternalString := '';
  InternalTemporary := False;
end;

procedure TmneRunItem.Execute;
var
  s: string;
  p: TProcess;
begin
  case Info.Mode of
    runConsole:
    begin
      s := '/c "'+ Info.GetCommandLine + '"';
      if Info.Pause then
        s := s + ' & pause';
      Info.Command := 'cmd';
      Info.Params := s;
      CreateConsole(Info);
      //Sync this function to make it modal
      {
      Status := ExecuteProcess('cmd ', s, [ExecInheritsHandles]);}
    end;
    runOutput:
    begin
      FPool.Synchronize(FPool, @CreateControl);
      CreateConsole(Info);
    end;
    runEmbedded:
    begin
      FPool.Synchronize(FPool, @CreateControl);
      CreateConsole(Info);
      //not free myself the thread will do
    end;
    runBrowser:
    begin
      OpenURL(Info.Link);
    end;
  end;
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
end;

procedure TmneRun.Stop;
begin
  if FPool <> nil then
  begin
    FPool.Stop;
  end;
end;

end.
