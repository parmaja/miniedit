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
  process,
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
  TmneCapsule = class;

  { TmneConsoleThread }

  TmneConsoleThread = class(TmnConsoleThread)
  protected
    procedure Finished(Status: integer); override;
  public
    Capsule: TmneCapsule;
    constructor Create(ACapsule: TmneCapsule);
    destructor Destroy; override;
  end;

  { TmneCapsule }

  TmneCapsule = class(TObject)
  private
    FNextOnFail: Boolean;
  protected
    Run: TmneRun;
    function CreateConsole(AInfo: TmneRunInfo; NewTab: Boolean): TConsoleForm;
  public
    Info: TmneRunInfo;
    function Execute: Boolean; virtual;
    constructor Create(ARun: TmneRun);
    property NextOnFail: Boolean read FNextOnFail write FNextOnFail;
  end;

  { TmneCapsules }

  TmneCapsules = class(TObjectList)
  private
    function GetItem(Index: integer): TmneCapsule;
  public
    property Items[Index: integer]: TmneCapsule read GetItem; default;
  end;

  { TmneRunProject }

  TmneRun = class(TObject)
  private
    FActive: Boolean;
    FCapsules: TmneCapsules;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Next;
    procedure Stop;
    property Capsules: TmneCapsules read FCapsules;
    property Active: Boolean read FActive;
  end;

implementation

uses
  EditorEngine;

{ TmneConsoleThread }

procedure TmneConsoleThread.Finished(Status: integer);
begin
  inherited;
  if (Status = 0) or (Capsule.NextOnFail) then
    Capsule.Run.Next;
end;

constructor TmneConsoleThread.Create(ACapsule: TmneCapsule);
begin
  inherited Create;
  Capsule := ACapsule;
end;

destructor TmneConsoleThread.Destroy;
begin
  FreeAndNil(Capsule);
  inherited Destroy;
end;

{ TmneCapsule }

{ TmneCapsules }

function TmneCapsules.GetItem(Index: integer): TmneCapsule;
begin
  Result := inherited Items[Index] as TmneCapsule;
end;

{ TmneCapsule }


{ TmneRun }

constructor TmneRun.Create;
begin
  inherited Create;
  FCapsules := TmneCapsules.Create(True);
end;

destructor TmneRun.Destroy;
begin
  FreeAndNil(FCapsules);
  inherited Destroy;
end;

procedure TmneRun.Start;
begin
  Next;
end;

procedure TmneRun.Next;
var
  aCapsule: TmneCapsule;
begin
  if Capsules.Count > 0 then
  begin
    FActive := True;
    aCapsule := Capsules[0];
    Capsules.Extract(aCapsule);
    aCapsule.Execute; //the thread will free it, or it self
  end
  else
    FActive := False;
end;

function TmneCapsule.CreateConsole(AInfo: TmneRunInfo; NewTab: Boolean): TConsoleForm;
var
  aControl: TConsoleForm;
  thread: TmnConsoleThread;
  //thread: TConsoleThread;
begin
  thread := TmnConsoleThread.Create;
  thread.Executable := AInfo.Command;
  thread.Parameters.Text := AInfo.Params;
  thread.CurrentDirectory := AInfo.Root;

  if NewTab then
  begin
    aControl := TConsoleForm.Create(Application);
    aControl.Parent := Engine.Container;
    Engine.Files.New('CMD', aControl);

    aControl.CMDBox.Font.Color := Engine.Options.Profile.Attributes.Whitespace.Foreground;
    aControl.CMDBox.BackGroundColor := Engine.Options.Profile.Attributes.Whitespace.Background;
    aControl.ContentPanel.Color := aControl.CMDBox.BackGroundColor;

    aControl.CMDBox.Font.Name := Engine.Options.Profile.FontName;
    aControl.CMDBox.Font.Size := Engine.Options.Profile.FontSize;

    aControl.CMDBox.TextColor(Engine.Options.Profile.Attributes.Whitespace.Foreground);
    aControl.CMDBox.TextBackground(Engine.Options.Profile.Attributes.Whitespace.Background);
    //aControl.CMDBox.Write(Info.GetCommandLine+#13#10);
    thread.OnWrite := @aControl.WriteText;
    Engine.UpdateState([ecsRefresh]);
  end
  else
    thread.OnWrite := Engine.OnLog;

  thread.Start;

{  //aControl.CMDBox.InputSelColor
  thread := CreateConsoleThread;
  thread.CmdBox := aControl.CmdBox;
  thread.Shell := AInfo.GetCommandLine;
  thread.Start;}

  Result := aControl;
end;

function TmneCapsule.Execute: Boolean;
var
  s: string;
  p: TProcess;
  Status: integer;
begin
  SetCurrentDir(Info.Root);
  case Info.Mode of
    runShell:
    begin
      s := Info.GetCommandLine;
      p := TProcess.create(nil);
      p.CommandLine:= s;
      p.Execute;
      p.Free;
      Free; //Free myself
      Run.Next;
    end;
    runTerminal:
    begin
      CreateConsole(Info, true);
      //not free myself the thread will do
    end;
    runConsole:
    begin
      s := '/c "'+ Info.GetCommandLine;
      if Info.Pause then
        s := s + '" & pause';
      Status := ExecuteProcess('cmd ', s, [ExecInheritsHandles]);
      Free; //Free myself
      if Status = 0 then
        Run.Next;
    end;
    runUrl:
    begin
      if Engine.Session.IsOpened then
      begin
        if SameText((MidStr(Info.RunFile, 1, Length(Info.Root))), Info.Root) then
        begin
          Info.RunFile := MidStr(Info.RunFile, Length(Info.Root) + 1, MaxInt);
          Info.RunFile := IncludeSlash(Info.Url) + Info.RunFile;
        end;
      end;
      Free; //Free myself
      Run.Next;
    end;
  end;
end;

constructor TmneCapsule.Create(ARun: TmneRun);
begin
  inherited Create;
  Run := ARun;
end;

procedure TmneRun.Stop;
begin
  FActive := False; //TODO
end;

initialization
finalization
end.

