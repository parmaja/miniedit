program mne;

{$mode objfpc}

{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @url       http://sourceforge.net/projects/minilib
 *}

{
  rev: 50962

  good fonts
  http://www.lowing.org/fonts/
}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Dialogs,
  Controls,
  Forms,
  Registry,
  SysUtils,
  LCLVersion,
  simpleipc,
  Themes, cmdbox, Interfaces,
  MainUnit in 'MainUnit.pas' {MainForm},
  mneProjectOptions in 'mneProjectOptions.pas' {ProjectForm},
  SearchForms in '..\editor\SearchForms.pas' {SearchForm},
  EditorOptions in '..\editor\EditorOptions.pas' {EditorOptionsForm},
  EditorProfiles in '..\editor\EditorProfiles.pas',
  EditorEngine in '..\editor\EditorEngine.pas',
  mneResources in 'mneResources.pas' {EditorResource: TDataModule},
  SelectFiles in '..\editor\SelectFiles.pas' {SelectFileForm},
  mneConsts in 'mneConsts.pas',
  AboutForms in 'AboutForms.pas' {AboutForm},
  GotoForms in '..\editor\GotoForms.pas' {GotoLineForm},
  EditorRun in '..\editor\EditorRun.pas',
  mneBreakpoints in 'mneBreakpoints.pas' {BreakpointsForm},
  SearchInFilesForms in '..\editor\SearchInFilesForms.pas' {SearchInFilesForm},
  SearchProgressForms, EditorDebugger, SelectList, IniFiles, mneAssociateForm,
  mneExtends, mneCppClasses, mneVerilogClasses, SynHighlighterVerilog,
  mneCustomClasses, mnePHPProjectFrames, mnePasProjectFrames,
  mnePasTendencyFrames, mnePASClasses, mneCSVClasses, mneDClasses,
  mneLuaClasses, LuaDBGServers, mneDTendencyFrames, MsgBox, GUIMsgBox, Classes,
  PHPUtils, ntvThemes, mneSetups, mneSettings, EditorClasses, gdbClasses,
  mneCompileProjectOptions, mneManageRecentsForms, mneTendencyOptions,
  mneCSVForms, mnUtils, mnConnections;

{$R *.res}

{$i '..\lib\mne.inc'}

function CheckSetup: Boolean;
var
  aIniFile: TIniFile;
  aIni, aPath: string;
begin
  Result := False;
  aIni := ExtractFilePath(Application.ExeName) + 'setting.ini';
  if FileExists(aIni) then
  begin
    aIniFile := TIniFile.Create(aIni);
    try
      aPath := aIniFile.ReadString(SysPlatform, 'Workspace', '');
      //aPath := ExpandToPath(aPath, Application.Location);
      //Result := DirectoryExists(aPath);
      Result := aPath <> '';
    finally
      aIniFile.Free;
    end;
  end;
  if not Result then
  begin
    with TEditorSetupForm.Create(Application) do
    begin
      Result := ShowModal = mrOK;
      Free;
    end;
  end;
end;

function AnotherInstance: Boolean;
var
  aClient: TSimpleIPCClient;
begin
  if (ParamCount > 0) and not (SameText(ParamStr(1), '/dde')) then
  begin
    aClient := TSimpleIPCClient.Create(nil);
    try
      aClient.ServerID := sApplicationID;
      Result := aClient.ServerRunning;//There is another instance
      if Result then
      begin
        aClient.Connect;
        try
          aClient.SendStringMessage(1, ParamStr(1));
        finally
          aClient.Disconnect;
        end;
      end
    finally
      aClient.Free;
    end;
  end
  else
  begin
    Result := False;
  end
end;

procedure Run;
begin
  {$IFDEF DEBUG}
  //* http://wiki.freepascal.org/leakview
  if FileExists(Application.Location+'heap.trc') then
    DeleteFile(Application.Location+'heap.trc');
  //SetHeapTraceOutput(Application.Location+'heap.trc');
  {$ENDIF DEBUG}
  Application.CreateForm(TEditorResource, EditorResource);
  Application.CreateForm(TMainForm, MainForm);
end;

begin
  if not AnotherInstance then
  begin
    Application.Initialize;
    {$ifdef trunk} //version 1.7 or later
    {$else}
    Application.BidiMode := bdLeftToRight;
    {$endif trunk}
    if CheckSetup then
    begin
      Run;
      Application.Run;
    end;
  end;
end.

