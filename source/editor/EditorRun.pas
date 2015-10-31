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
  windows, Forms, SysUtils, StrUtils, Classes, SyncObjs, contnrs,
  process, mnUtils,
  ConsoleProcess,
  mnStreams, mneConsoleForms, uTerminal, DebugClasses, mnXMLUtils;

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
    FOnWrite: TmnOnWrite;
    FPool: TmneRunPool;
    procedure CreateControl;
    procedure CreateConsole(AInfo: TmneCommandInfo);
  public
    Info: TmneCommandInfo;
    Status: integer;
    procedure Execute; virtual;
    procedure Stop; virtual;
    constructor Create(APool: TmneRunPool);
    property BreakOnFail: Boolean read FBreakOnFail write FBreakOnFail;
  end;

  TmneRunItemClass = class of TmneRunItem;

  { TmneRunItems }

  TmneRunItems = class(TObjectList)
  private
    function GetItem(Index: integer): TmneRunItem;
  public
    property Items[Index: integer]: TmneRunItem read GetItem; default;
  end;

  { TmneRunPool }

  TmneRunPool = class(TThread)
  protected
    FRun: TmneRun;
    FItems: TmneRunItems;
    FCurrent: TmneRunItem;
  public
    constructor Create(ARun: TmneRun);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Stop;
    property Items: TmneRunItems read FItems;
    property Current: TmneRunItem read FCurrent;
  end;

  { TmneRunProject }

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
    procedure Stop;
    property Active: Boolean read GetActive;
    property Pool: TmneRunPool read FPool;
  end;

implementation

uses
  EditorEngine;

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
  end
end;

procedure TmneRunPool.Stop;
begin
  if FCurrent <> nil then
    FCurrent.Stop;
  Terminate;
  WaitFor;
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

{ TmneConsoleThread }

{ TmneRunItems }

function TmneRunItems.GetItem(Index: integer): TmneRunItem;
begin
  Result := inherited Items[Index] as TmneRunItem;
end;

{ TmneRun }

function TmneRun.GetActive: Boolean;
begin
  Result := FPool <> nil;
end;

procedure TmneRun.PoolTerminated(Sender: TObject);
begin
  FPool := nil;
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
    Exception.Create('There is no thread Pool');
  FPool.Start;
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
  if Info.Mode = runTerminal then
  begin
    FControl := TConsoleForm.Create(Application);
    FControl.Parent := Engine.Container;
    Engine.Files.New('CMD', FControl);

    FControl.CMDBox.Font.Color := Engine.Options.Profile.Attributes.Whitespace.Foreground;
    FControl.CMDBox.BackGroundColor := Engine.Options.Profile.Attributes.Whitespace.Background;
    FControl.ContentPanel.Color := FControl.CMDBox.BackGroundColor;

    FControl.CMDBox.Font.Name := Engine.Options.Profile.FontName;
    FControl.CMDBox.Font.Size := Engine.Options.Profile.FontSize;

    FControl.CMDBox.TextColor(Engine.Options.Profile.Attributes.Whitespace.Foreground);
    FControl.CMDBox.TextBackground(Engine.Options.Profile.Attributes.Whitespace.Background);
    FControl.CMDBox.Write('Ready!'+#13#10);
    FOnWrite := @FControl.WriteText;
    Engine.UpdateState([ecsRefresh]);
  end
  else
    FOnWrite := Engine.OnLog;
end;

procedure TmneRunItem.CreateConsole(AInfo: TmneCommandInfo);
var
  ProcessObject: TmnProcessObject;
begin
  if Assigned(FOnWrite) then
    FOnWrite('Starting ' + Info.Title);
  FProcess := TProcess.Create(nil);
  FProcess.ConsoleTitle := Info.Title;
  FProcess.Executable := AInfo.Command;
  FProcess.Parameters.Text := AInfo.Params;
  FProcess.CurrentDirectory := AInfo.CurrentDirectory;
  FProcess.InheritHandles := True;
  FProcess.Options :=  [poUsePipes, poStderrToOutPut];
  FProcess.ShowWindow := swoHIDE;
  FProcess.PipeBufferSize := 80; //80 char in line

  ProcessObject := TmnProcessObject.Create(FProcess, FPool, FOnWrite);
  try
    Status := ProcessObject.Read;
  finally
    FreeAndNil(FProcess);
    FreeAndNil(ProcessObject);
  end;
  if Assigned(FOnWrite) then
    FOnWrite('Finished ' + Info.Title + ' with status: ' + IntToStr(Status));
end;

procedure TmneRunItem.Execute;
var
  s: string;
  p: TProcess;
begin
  case Info.Mode of
    runLog:
    begin
      FPool.Synchronize(FPool, @CreateControl);
      CreateConsole(Info);
    end;
    runTerminal:
    begin
      FPool.Synchronize(FPool, @CreateControl);
      CreateConsole(Info);
      //not free myself the thread will do
    end;
    runConsole:
    begin
      //Sync this function to make it modal
      SetCurrentDir(Info.CurrentDirectory);
      s := '/c "'+ Info.GetCommandLine + '"';
      if Info.Pause then
        s := s + ' & pause';
      Status := ExecuteProcess('cmd ', s, [ExecInheritsHandles]);
    end;
    runUrl:
    begin
      if Engine.Session.IsOpened then
      begin
        if SameText((MidStr(Info.Command, 1, Length(Info.CurrentDirectory))), Info.CurrentDirectory) then
        begin
          //Info.RunFile := MidStr(Info.Command, Length(Info.CurrentDirectory) + 1, MaxInt);
          //Info.RunFile := IncludeSlash(Info.Url) + Info.RunFile;
        end;
      end;
//      Run.Next;
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

