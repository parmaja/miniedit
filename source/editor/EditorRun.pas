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

  { TmneConsoleThread }

  TmneConsoleThread = class(TmnConsoleThread)
  protected
    procedure DoTerminate; override;
  public
    Capsule: TmneRunItem;
    constructor Create(ACapsule: TmneRunItem);
    destructor Destroy; override;
  end;

  { TmneRunItem }

  TmneRunItem = class(TObject)
  private
    FNextOnFail: Boolean;
  protected
    aControl: TConsoleForm;
    FThread: TmneConsoleThread;
    Run: TmneRun;
    procedure CreateConsole(AInfo: TmneCommandInfo; NewTab: Boolean);
  public
    Info: TmneCommandInfo;
    procedure Execute; virtual;
    procedure Stop; virtual;
    constructor Create(ARun: TmneRun);
    property NextOnFail: Boolean read FNextOnFail write FNextOnFail;
  end;

  { TmneRunItems }

  TmneRunItems = class(TObjectList)
  private
    function GetItem(Index: integer): TmneRunItem;
  public
    property Items[Index: integer]: TmneRunItem read GetItem; default;
  end;

  { TmneRunProject }

  TmneRun = class(TObject)
  private
    FActive: Boolean;
    FItems: TmneRunItems;
  protected
  public
    Current: TmneRunItem;
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Next;
    procedure Stop;
    property Items: TmneRunItems read FItems;
    property Active: Boolean read FActive;
  end;

implementation

uses
  EditorEngine;

{ TmneConsoleThread }

procedure TmneConsoleThread.DoTerminate;
begin
  Process.Terminate(2);
  inherited;
end;

constructor TmneConsoleThread.Create(ACapsule: TmneRunItem);
begin
  inherited Create;
  Capsule := ACapsule;
end;

destructor TmneConsoleThread.Destroy;
begin
//  FreeAndNil(Capsule);
  inherited Destroy;
end;

{ TmneRunItems }

function TmneRunItems.GetItem(Index: integer): TmneRunItem;
begin
  Result := inherited Items[Index] as TmneRunItem;
end;

{ TmneRun }

constructor TmneRun.Create;
begin
  inherited Create;
  FItems := TmneRunItems.Create(True);
end;

destructor TmneRun.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TmneRun.Start;
begin
  Next;
end;

procedure TmneRun.Next;
begin
  Application.ProcessMessages;
  if Current <> nil then
  begin
    Current.Stop;
    Current.Free;
  end;
  if Items.Count > 0 then
  begin
    FActive := True;
    Current := Items[0];
    Items.Extract(Current);
    Current.Execute; //the thread will free it, or it self
  end
  else
    FActive := False;
end;

procedure TmneRunItem.CreateConsole(AInfo: TmneCommandInfo; NewTab: Boolean);
begin
  FThread := TmneConsoleThread.Create(Self);
  FThread.FreeOnTerminate := False;
  FThread.Executable := AInfo.Command;
  FThread.Parameters.Text := AInfo.Params;
  FThread.CurrentDirectory := AInfo.CurrentDirectory;

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
    FThread.OnWrite := @aControl.WriteText;
    Engine.UpdateState([ecsRefresh]);
  end
  else
    FThread.OnWrite := Engine.OnLog;

  FThread.Start;
end;

procedure TmneRunItem.Execute;
var
  s: string;
  p: TProcess;
  Status: integer;
  aRun: TmneRun;
begin
  case Info.Mode of
    runShell:
    begin
      CreateConsole(Info, false);
    end;
    runTerminal:
    begin
      CreateConsole(Info, true);
      //not free myself the thread will do
    end;
    runConsole:
    begin
      SetCurrentDir(Info.CurrentDirectory);
      s := '/c "'+ Info.GetCommandLine + '"';
      if Info.Pause then
        s := s + ' & pause';
      Status := ExecuteProcess('cmd ', s, [ExecInheritsHandles]);

      aRun := Run;
      Free; //Free myself
      if Status = 0 then
        aRun.Next;
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
      Free; //Free myself
      Run.Next;
    end;
  end;
end;

procedure TmneRunItem.Stop;
begin
  FThread.Terminate;
  FThread.WaitFor;
  FThread.Free;
end;

constructor TmneRunItem.Create(ARun: TmneRun);
begin
  inherited Create;
  Run := ARun;
end;

procedure TmneRun.Stop;
begin
  if Current <> nil then
  begin
    Current.Stop;
    Current.Free;
  end;
  FActive := False; //TODO
end;

end.

