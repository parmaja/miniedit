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
  windows, Forms, SysUtils, StrUtils, Classes, SyncObjs, process,
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

  TmneOnWrite = procedure(S: string) of object;

  { TmneConsoleThread }

  TmneConsoleThread = class(TThread)
  private
    FOnWrite: TmneOnWrite;
    FBuffer: string;
  public
    Result: Integer;
    Info: TmneRunInfo;
    Process: TProcess;
    procedure FlushBuffer;
    procedure Execute; override;
    property OnWrite: TmneOnWrite read FOnWrite write FOnWrite;
  end;

  { TmneRunProject }

  TmneConsole = class(TObject)
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

{ TmneConsoleThread }

procedure TmneConsoleThread.FlushBuffer;
begin
  if Assigned(FOnWrite) then
  begin
    FOnWrite(FBuffer);
    FBuffer := '';
  end;
end;

procedure TmneConsoleThread.Execute;
const
  READ_BYTES = 1024;
var
  C, Count, L: DWORD;
  Status: integer;
  FirstTime: Boolean;
begin
  Process := TProcess.Create(nil);
  Result := -1;
  FirstTime := True;
  Count := 0;
  C := 0;
  Process.Options :=  [poUsePipes];
  Process.ShowWindow := swoHIDE;

  //Process.PipeBufferSize := 10;
  //Process.InheritHandles := True;
  //Process.ConsoleTitle := Info.RunFile;
  Process.CommandLine := Info.GetCommandLine;
  if Info.Root <> '' then
    Process.CurrentDirectory := Info.Root;
  try
    try
      Process.Execute;
      while not Terminated and (FirstTime or Process.Running or (C > 0)) do
      begin
        //Process.Input.WriteAnsiString('Zaher'#13#10);
        L := Length(FBuffer);
        if not PeekNamedPipe(Process.Output.Handle, nil, 0, nil, @C, nil) then
          C := 0;

        C := Process.Output.NumBytesAvailable;

        Setlength(FBuffer, L + READ_BYTES);
        C := Process.Output.Read(FBuffer[1 + L], READ_BYTES);
        if Assigned(FOnWrite) then
        begin
          SetLength(FBuffer, L + C);
          Synchronize(@FlushBuffer);
        end;
        if C > 0 then
          Inc(Count, C)
        else
          Sleep(100);
        FirstTime := False;
      end;
      if Terminated and Process.Running then
        Process.Terminate(0);
      if not Assigned(FOnWrite) then
        SetLength(FBuffer, Count);
      Status := Process.ExitStatus;
      Result := 0;
    except
      on e : Exception do
      begin
        if Process.Running and Terminated then
          Process.Terminate(0);
        if not Assigned(FOnWrite) then
          SetLength(FBuffer, Count);
      end;
    end;
  finally
    FreeAndNil(Process);
  end;
end;

{ TmneConsole }

constructor TmneConsole.Create(AInfo: TmneRunInfo);
begin
  inherited Create;
  Info := AInfo;
end;

destructor TmneConsole.Destroy;
begin
  inherited Destroy;
end;

function TmneConsole.CreateInernalConsole(AInfo: TmneRunInfo): TConsoleForm;
var
  aControl: TConsoleForm;
  thread: TmneConsoleThread;
  //thread: TConsoleThread;
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
  aControl.CMDBox.Write(Info.GetCommandLine+#13#10);

  thread := TmneConsoleThread.Create(true);
  thread.Info := AInfo;
  thread.OnWrite := @aControl.WriteText;
  thread.Start;

{  //aControl.CMDBox.InputSelColor
  thread := CreateConsoleThread;
  thread.CmdBox := aControl.CmdBox;
  thread.Shell := AInfo.GetCommandLine;
  thread.Start;}

  Result := aControl;
  Engine.UpdateState([ecsRefresh]);
end;

procedure TmneConsole.Execute;
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

