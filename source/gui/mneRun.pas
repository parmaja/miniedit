unit mneRun;
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
  Forms, SysUtils, StrUtils, Classes, SyncObjs,
  mnStreams, EditorEngine, mneConsoleForms, uTerminal, mnXMLUtils;

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

  { TmneRunProject }

  TmneRun = class(TObject)
  private
    function CreateInernalConsole(AInfo: TmneRunInfo): TConsoleForm;
  protected
  public
    Info: TmneRunInfo;
    constructor Create(AInfo: TmneRunInfo);
    destructor Destroy; override;
    procedure Execute;
  end;

implementation

uses
  process;

{ TmneRun }

constructor TmneRun.Create(AInfo: TmneRunInfo);
begin
  inherited Create;
  Info := AInfo;
end;

destructor TmneRun.Destroy;
begin
  inherited Destroy;
end;

function TmneRun.CreateInernalConsole(AInfo: TmneRunInfo): TConsoleForm;
var
  aControl: TConsoleForm;
  thread: TConsoleThread;
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
  aControl.CMDBox.Write('Ready!'#13#10);
  //aControl.CMDBox.InputSelColor
  thread := CreateConsoleThread;
  thread.CmdBox := aControl.CmdBox;
  thread.Shell := AInfo.GetCommandLine;
  thread.Start;

  Result := aControl;
  Engine.UpdateState([ecsRefresh]);
end;

procedure TmneRun.Execute;
var
  s: string;
  p: TProcess;
begin
  SetCurrentDir(Info.Root);
  case Info.Mode of
    runUrl:
    begin
      if Engine.Session.IsOpened then
      begin
        if SameText((MidStr(Info.RunFile, 1, Length(Info.Root))), Info.Root) then
        begin
          Info.RunFile := MidStr(Info.RunFile, Length(Info.Root) + 1, MaxInt);
          Info.RunFile := IncludeSlash(Info.Url) + Info.RunFile;
          //ShellExecute(0, 'open', PChar(aFile), '', PChar(ExtractFilePath(aFile)), SW_SHOWNOACTIVATE);
        end;
      end;
    end;
    runShell:
    begin
      {$ifdef windows}
      s := Info.GetCommandLine;
      p := TProcess.create(nil);
      p.CommandLine:= s;
      p.Execute;
      p.Free;
      {$endif}
    end;
    runTerminal:
    begin
      CreateInernalConsole(Info)
    end;
    runConsole:
    begin
      {$ifdef windows}
      s := '/c "'+ Info.GetCommandLine;
      if Info.Pause then
        s := s + '" & pause';
      ExecuteProcess('cmd ', s, []);
      {$endif}

      {$ifdef linux}
      {$endif}

      {$ifdef macos}
      {$endif}
    end;
  end;
end;

initialization
finalization
end.

