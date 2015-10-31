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
    FNextOnFail: Boolean;
  protected
    FProcess: TProcess;
    FControl: TConsoleForm;
    FOnWrite: TmnOnWrite;
    FPool: TmneRunPool;
    procedure CreateControl;
    procedure CreateConsole(AInfo: TmneCommandInfo);
  public
    Info: TmneCommandInfo;
    procedure Execute; virtual;
    procedure Stop; virtual;
    constructor Create(APool: TmneRunPool);
    property NextOnFail: Boolean read FNextOnFail write FNextOnFail;
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
    procedure DoTerminate; override;
  public
    constructor Create(ARun: TmneRun);
    destructor Destroy; override;
    procedure Execute; override;
    property Items: TmneRunItems read FItems;
    property Current: TmneRunItem read FCurrent;
  end;

  { TmneRunProject }

  TmneRun = class(TObject)
  private
    FPool: TmneRunPool;
    function GetActive: Boolean;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AItemClass: TmneRunItemClass = nil): TmneRunItem; //Return same as parameter
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
    Current.Free;
  end
end;

procedure TmneRunPool.DoTerminate;
begin

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
    FPool := TmneRunPool.Create(Self);
  if AItemClass = nil then
    Result := TmneRunItem.Create(FPool)
  else
    Result := AItemClass.Create(FPool);
  FPool.Items.Add(Result);
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
  FProcess := TProcess.Create(nil);
  FProcess.Options :=  [poUsePipes];
  FProcess.ShowWindow := swoHIDE;

  //FProcess.PipeBufferSize := 10;
  //FProcess.ConsoleTitle := Info.RunFile;
  FProcess.Executable := AInfo.Command;
  FProcess.Parameters.Text := AInfo.Params;
  FProcess.CurrentDirectory := AInfo.CurrentDirectory;
  FProcess.InheritHandles := True;

  ProcessObject := TmnProcessObject.Create(FProcess, FPool, FOnWrite);
  try
    ProcessObject.Read;
  finally
    FreeAndNil(FProcess);
    FreeAndNil(ProcessObject);
  end;
end;

procedure TmneRunItem.Execute;
var
  s: string;
  p: TProcess;
  Status: integer;
begin
  case Info.Mode of
    runShell:
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
      SetCurrentDir(Info.CurrentDirectory);
      s := '/c "'+ Info.GetCommandLine + '"';
      if Info.Pause then
        s := s + ' & pause';
      Status := ExecuteProcess('cmd ', s, [ExecInheritsHandles]);

{      if Status = 0 then
        aRun.Next;}
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
  FProcess.Terminate(1);
  FProcess.WaitOnExit;
  FProcess.Free;
end;

constructor TmneRunItem.Create(APool: TmneRunPool);
begin
  inherited Create;
  FPool := APool;
end;

procedure TmneRun.Stop;
begin
  if FPool <> nil then
  begin
    FPool.Terminate;
    FPool.WaitFor;
    FreeAndNil(FPool);
  end;
end;

end.

