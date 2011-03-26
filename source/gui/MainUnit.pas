unit MainUnit;

{$mode delphi}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
{

SynEdit:
    - Make PaintTransient in TCustomerHighlighter as virtual and called from DoOnPaintTransientEx like calling OnPaintTransient
    - Need a retrive the Range when call GetHighlighterAttriAtRowColEx
    - "Reset" method in TSynHighlighterAttributes to reassign property to default such as fBackground := fBackgroundDefault it is usfull to load and reload properties from file (XML one)

  MessageList TabStop must be False, can not Maximize window in startup the application (When it is take the focus)

  //i must move Macro Recorder to Engine not in Category
}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Math, ComCtrls, ExtCtrls, ImgList, Menus, ShlObj, ToolWin,
  Buttons, FileCtrl, ShellCtrls, ActnList, EditorEngine, mneClasses, StdActns,
  PairSplitter, SynEditHighlighter, SynHighlighterPHP, SynHighlighterApache,
  ntvTabSets, mneRun, Registry, SynEdit, CommCtrl, SynEditPlugins,
  DividerBevel, IniFiles, simpleipc, mnUtils, ntvTabs, ntvPageControls;

{$i '..\lib\mne.inc'}

type
  TTabSetDragObject = class(TDragObject)
  public
    TabIndex: integer;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    file1: TMenuItem;
    FileSet: TntvTabSet;
    MessageList: TListView;
    MessagesTabs: TntvPageControl;
    SearchList: TListView;
    IPCServer: TSimpleIPCServer;
    veiw1: TMenuItem;
    Help1: TMenuItem;
    NewMnu: TMenuItem;
    OpenMnu: TMenuItem;
    SaveMnu: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    About1: TMenuItem;
    OptionsMnu: TMenuItem;
    ShowTree1: TMenuItem;
    ReopenMnu: TMenuItem;
    TopCoolBar: TToolBar;
    MainBar: TToolBar;
    BackBtn: TToolButton;
    ForwardBtn: TToolButton;
    StopBtn: TToolButton;
    ToolButton7: TToolButton;
    FoldersBtn: TToolButton;
    ActionList: TActionList;
    FoldersAct: TAction;
    SaveAct: TAction;
    OpenAct: TAction;
    SaveAsAct: TAction;
    NewAct: TAction;
    Saveas1: TMenuItem;
    SettingAct: TAction;
    N6: TMenuItem;
    NextAct: TAction;
    PriorAct: TAction;
    CloseAct: TAction;
    New1: TMenuItem;
    Prior1: TMenuItem;
    N2: TMenuItem;
    ExitAct: TFileExit;
    ToolButton1: TToolButton;
    SaveAllAct: TAction;
    AboutAct: TAction;
    KeywordAct: TAction;
    PHPKeyword1: TMenuItem;
    About2: TMenuItem;
    RunAct: TAction;
    CheckAct: TAction;
    ProjectMnu: TMenuItem;
    Run2: TMenuItem;
    Check1: TMenuItem;
    AssociateAct: TAction;
    ools1: TMenuItem;
    Associate1: TMenuItem;
    FolderMenu: TPopupMenu;
    FolderOpenAllAct: TAction;
    OpenAll1: TMenuItem;
    FolderOpenAct: TAction;
    FolderOpenAct1: TMenuItem;
    FindAct: TAction;
    FindNextAct: TAction;
    Edit1: TMenuItem;
    Find1: TMenuItem;
    Findnext1: TMenuItem;
    HelpIndexAct: TAction;
    HelpIndex1: TMenuItem;
    EditorOptionsAct: TAction;
    N3: TMenuItem;
    EditorOptions1: TMenuItem;
    N4: TMenuItem;
    CloseAllAct: TAction;
    Close1: TMenuItem;
    ProjectOptionsMnu: TMenuItem;
    ProjectOptionsAct: TAction;
    New2: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs2: TMenuItem;
    NewProjectAct: TAction;
    OpenProjectAct: TAction;
    SaveProjectAct: TAction;
    SaveAsProjectAct: TAction;
    SelectFileAct: TAction;
    AddFileToProjectAct: TAction;
    N7: TMenuItem;
    AddFile1: TMenuItem;
    SelectFile1: TMenuItem;
    ReopenProjectMnu: TMenuItem;
    CloseProjectAct: TAction;
    CloseProject1: TMenuItem;
    OpenFolderAct: TAction;
    OpenFolder1: TMenuItem;
    MessagesSpl: TSplitter;
    ToolButton2: TToolButton;
    MessagesAct: TAction;
    Messages1: TMenuItem;
    FileModeMenu: TPopupMenu;
    UnixMnu: TMenuItem;
    WatchList: TListView;
    WindowsMnu: TMenuItem;
    MacMnu: TMenuItem;
    FoldersPnl: TPanel;
    FolderPanel: TPanel;
    FolderCloseBtn: TSpeedButton;
    FileList: TListView;
    FoldersSpl: TSplitter;
    Extractkeywords: TMenuItem;
    ManageAct: TAction;
    Manage1: TMenuItem;
    ToolButton3: TToolButton;
    OpenFolder2: TMenuItem;
    ProjectOpenFolderAct: TAction;
    OpenIncludeAct: TAction;
    OpenInclude1: TMenuItem;
    CopyAct: TAction;
    CutAct: TAction;
    PasteAct: TAction;
    SelectAllAct: TAction;
    Copy1: TMenuItem;
    N8: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    SelectAll1: TMenuItem;
    EditorPopupMenu: TPopupMenu;
    OpenInclude2: TMenuItem;
    Find2: TMenuItem;
    Findnext2: TMenuItem;
    N9: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    SelectAll2: TMenuItem;
    N10: TMenuItem;
    EditorOptions2: TMenuItem;
    GotoLineAct: TAction;
    GotoLine1: TMenuItem;
    GotoFolder1: TMenuItem;
    ReplaceAct: TAction;
    Replace1: TMenuItem;
    RevertAct: TAction;
    Revert1: TMenuItem;
    FileFolder1: TMenuItem;
    OpenColorAct: TAction;
    Open2: TMenuItem;
    OpenColor1: TMenuItem;
    N11: TMenuItem;
    SVN1: TMenuItem;
    Commit1: TMenuItem;
    Diff1: TMenuItem;
    CommitFIle1: TMenuItem;
    SVNCommitAct: TAction;
    SVNDiffFileAct: TAction;
    SVNCommitFileAct: TAction;
    SVNUpdateFileAct: TAction;
    SVNRevertAct: TAction;
    SVNUpdateAct: TAction;
    Update1: TMenuItem;
    UpdateFile1: TMenuItem;
    Revert2: TMenuItem;
    DBGStartServerAct: TAction;
    DBGStopServerAct: TAction;
    About3: TMenuItem;
    StartServer1: TMenuItem;
    DBGStopServerAct1: TMenuItem;
    DBGStepIntoAct: TAction;
    DBGStepOverAct: TAction;
    DBGResetAct: TAction;
    N12: TMenuItem;
    StepInto1: TMenuItem;
    StepOver1: TMenuItem;
    Reset2: TMenuItem;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    DBGActiveServerAct: TAction;
    DBGDetachAct: TAction;
    Detach1: TMenuItem;
    DBGStepOutAct: TAction;
    DBGStepOutAct1: TMenuItem;
    ToolButton6: TToolButton;
    PHPIniConfigAct: TAction;
    InstallXDebug1: TMenuItem;
    N5: TMenuItem;
    AddWatch1: TMenuItem;
    WatchesPopupMenu: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    N13: TMenuItem;
    Refresh1: TMenuItem;
    ShowValue1: TMenuItem;
    DBGToggleBreakpoint: TAction;
    N14: TMenuItem;
    DBGToggleBreakpoint1: TMenuItem;
    oggleBreakpoint1: TMenuItem;
    ToolButton8: TToolButton;
    OutputAct: TAction;
    ClientPnl: TPanel;
    EditorsPnl: TPanel;
    FileHeaderPanel: TPanel;
    FileCloseBtn: TSpeedButton;
    FileNameLbl: TLabel;
    OutputEdit: TSynEdit;
    OutputSpl: TSplitter;
    Output1: TMenuItem;
    DBGRunToCursor: TAction;
    RunToCursor1: TMenuItem;
    DBGBreakpointsAct: TAction;
    Breakpoints1: TMenuItem;
    FilePopupMenu: TPopupMenu;
    OpenFolder3: TMenuItem;
    CopyFileNameAct: TAction;
    Copyfilename1: TMenuItem;
    DBGAddWatchAct: TAction;
    AddWatch2: TMenuItem;
    StatusPanel: TPanel;
    MessagePnl: TPanel;
    DebugPnl: TPanel;
    CursorPnl: TPanel;
    FolderHomeAct: TAction;
    Home1: TMenuItem;
    StatusTimer: TTimer;
    MessagesPopup: TPopupMenu;
    Clear1: TMenuItem;
    FindInFilesAct: TAction;
    FindinFiles1: TMenuItem;
    NextMessageAct: TAction;
    PriorMessageAct: TAction;
    NextMessage1: TMenuItem;
    PriorMessage1: TMenuItem;
    N15: TMenuItem;
    OpenPHPiniAct: TAction;
    OpenPHPiniAct1: TMenuItem;
    OutputPopup: TPopupMenu;
    MenuItem1: TMenuItem;
    StatePnl: TPanel;
    SVNCompareToAct: TAction;
    CompareTo1: TMenuItem;
    SwitchFocusAct: TAction;
    SwitchFocus1: TMenuItem;
    N16: TMenuItem;
    Label1: TLabel;
    FolderBtn: TSpeedButton;
    QuickFindPnl: TPanel;
    Label2: TLabel;
    CloseQuickSearchBtn: TSpeedButton;
    QuickSearchPrevBtn: TBitBtn;
    QuickSearchNextBtn: TBitBtn;
    QuickSearchEdit: TEdit;
    QuickFindAct: TAction;
    QuickSearch1: TMenuItem;
    FileModeBtn: TSpeedButton;
    procedure FileSetSelectTab(Sender: TObject; OldTab, NewTab: TntvTabItem; var CanSelect: boolean);
    procedure FileSetTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
    procedure FolderCloseBtnClick(Sender: TObject);
    procedure FoldersActExecute(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure IPCServerMessage(Sender: TObject);
    procedure OpenActExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileCloseBtnClick(Sender: TObject);
    procedure FileSetClick(Sender: TObject);
    procedure NextActExecute(Sender: TObject);
    procedure PriorActExecute(Sender: TObject);
    procedure CloseActExecute(Sender: TObject);
    procedure FileListDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure SaveActExecute(Sender: TObject);
    procedure SaveAllActExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure NewActExecute(Sender: TObject);
    procedure AssociateActExecute(Sender: TObject);
    procedure FolderOpenAllActExecute(Sender: TObject);
    procedure FolderOpenActExecute(Sender: TObject);
    procedure FindActExecute(Sender: TObject);
    procedure FindNextActExecute(Sender: TObject);
    procedure FileListKeyPress(Sender: TObject; var Key: char);
    procedure KeywordActExecute(Sender: TObject);
    procedure HelpIndexActExecute(Sender: TObject);
    procedure EditorOptionsActExecute(Sender: TObject);
    procedure SettingActExecute(Sender: TObject);
    procedure RunActExecute(Sender: TObject);
    procedure ProjectOptionsActExecute(Sender: TObject);
    procedure NewProjectActExecute(Sender: TObject);
    procedure OpenProjectActExecute(Sender: TObject);
    procedure SaveProjectActExecute(Sender: TObject);
    procedure SelectFileActExecute(Sender: TObject);
    procedure CloseProjectActExecute(Sender: TObject);
    procedure OpenFolderActExecute(Sender: TObject);
    procedure MessagesActExecute(Sender: TObject);
    procedure psvPHPBeforeExecute(Sender: TObject);
    procedure UnixMnuClick(Sender: TObject);
    procedure WindowsMnuClick(Sender: TObject);
    procedure MacMnuClick(Sender: TObject);
    procedure ExtractkeywordsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckActExecute(Sender: TObject);
    procedure AboutActExecute(Sender: TObject);
    procedure SaveAsProjectActExecute(Sender: TObject);
    procedure ProjectOpenFolderActExecute(Sender: TObject);
    procedure ManageActExecute(Sender: TObject);
    procedure CloseAllActExecute(Sender: TObject);
    procedure OpenIncludeActExecute(Sender: TObject);
    procedure CopyActUpdate(Sender: TObject);
    procedure PasteActUpdate(Sender: TObject);
    procedure CutActUpdate(Sender: TObject);
    procedure SelectAllActUpdate(Sender: TObject);
    procedure PasteActExecute(Sender: TObject);
    procedure CopyActExecute(Sender: TObject);
    procedure CutActExecute(Sender: TObject);
    procedure SelectAllActExecute(Sender: TObject);
    procedure OpenIncludeActUpdate(Sender: TObject);
    procedure GotoLineActUpdate(Sender: TObject);
    procedure GotoLineActExecute(Sender: TObject);
    procedure GotoFolder1Click(Sender: TObject);
    procedure ReplaceActExecute(Sender: TObject);
    procedure RevertActExecute(Sender: TObject);
    procedure FileListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FileFolder1Click(Sender: TObject);
    procedure OpenColorActUpdate(Sender: TObject);
    procedure OpenColorActExecute(Sender: TObject);
    procedure FileModeBtnClick(Sender: TObject);
    procedure FolderBtnClick(Sender: TObject);
    procedure SVNCommitActExecute(Sender: TObject);
    procedure SVNDiffFileActExecute(Sender: TObject);
    procedure SVNCommitFileActExecute(Sender: TObject);
    procedure SVNUpdateFileActExecute(Sender: TObject);
    procedure SVNUpdateActExecute(Sender: TObject);
    procedure SVNRevertActExecute(Sender: TObject);
    procedure DBGStopServerActUpdate(Sender: TObject);
    procedure DBGStartServerActUpdate(Sender: TObject);
    procedure DBGStartServerActExecute(Sender: TObject);
    procedure DBGStopServerActExecute(Sender: TObject);
    procedure DBGStepOverActExecute(Sender: TObject);
    procedure DBGStepIntoActExecute(Sender: TObject);
    procedure DBGActiveServerActUpdate(Sender: TObject);
    procedure DBGActiveServerActExecute(Sender: TObject);
    procedure DBGResetActExecute(Sender: TObject);
    procedure DBGDetachActExecute(Sender: TObject);
    procedure DBGStepOutActExecute(Sender: TObject);
    procedure SaveAsActExecute(Sender: TObject);
    procedure PHPIniConfigActExecute(Sender: TObject);
    procedure MessagesTabChange(Sender: TObject; NewTab: integer; var AllowChange: boolean);
    procedure ShowValue1Click(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure DBGToggleBreakpointExecute(Sender: TObject);
    procedure OutputActExecute(Sender: TObject);
    procedure DBGBreakpointsActExecute(Sender: TObject);
    procedure CopyFileNameActExecute(Sender: TObject);
    procedure DBGAddWatchActExecute(Sender: TObject);
    procedure WatchListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FolderHomeActExecute(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure SearchListDblClick(Sender: TObject);
    procedure MessageListDblClick(Sender: TObject);
    procedure SearchListCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: integer; State: TCustomDrawState; var DefaultDraw: boolean);
    procedure FindInFilesActExecute(Sender: TObject);
    procedure NextMessageActExecute(Sender: TObject);
    procedure PriorMessageActExecute(Sender: TObject);
    procedure OpenPHPiniActExecute(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure SVNCompareToActExecute(Sender: TObject);
    procedure SwitchFocusActExecute(Sender: TObject);
    procedure QuickSearchNextBtnClick(Sender: TObject);
    procedure QuickSearchPrevBtnClick(Sender: TObject);
    procedure QuickSearchEditChange(Sender: TObject);
    procedure QuickSearchEditKeyPress(Sender: TObject; var Key: char);
    procedure CloseQuickSearchBtnClick(Sender: TObject);
    procedure QuickFindActExecute(Sender: TObject);
    procedure QuickFindActUpdate(Sender: TObject);
  private
    //ApplicationEvents: TApplicationEvents;
    FMessages: TEditorMessages;
    //    OnActivate = ApplicationEventsActivate
    //    OnHint = ApplicationEventsHint
    procedure ApplicationEventsActivate(Sender: TObject);
    procedure ApplicationEventsHint(Sender: TObject);

    function CanOpenInclude: boolean;
    function DoHtmlHelp: boolean;
    procedure WMHelp(var Message: TWMHelp); message WM_HELP;
    procedure UpdateFileHeaderPanel;
    procedure EditorChangeState(State: TEditorChangeState);
    procedure EngineChanged;
    procedure UpdateWatches;
    procedure EngineDebug;
    procedure EngineRefresh;
    procedure EngineEdited;
    procedure EngineState;
    procedure EngineProjectLoaded;
    procedure UpdateFolder;
    procedure SetFolder(const Value: string);
    procedure ReopenClick(Sender: TObject);
    procedure ReopenProjectClick(Sender: TObject);
    procedure AddWatch(s: string);
    procedure DeleteWatch(s: string);
    procedure EnumRecentFile;
    procedure EnumRecentProjects;
    procedure UpdateProject;
    function GetCurrentColorText: string;
    function GetFolder: string;
    procedure DeleteCurrentWatch;
    procedure MoveListIndex(vForward: boolean);
    procedure OnSynEditReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var Action: TSynReplaceAction);
  protected
    FRunProject: TRunProject;
    LastGotoLine: integer;
    FControlStructures: TStringList;
    //

    procedure PHPScriptError(Sender: TObject; AText: string; AType: TRunErrorType; AFileName: string; ALineNo: integer);
    procedure PHPLogMessage(Sender: TObject; AText: string);

    procedure RunTerminated(Sender: TObject);
    procedure RunScript;
    //
    procedure CreateWnd; override;
    procedure ReceiveBuffer(const Buffer: string);
    procedure Log(Error: integer; Caption, Msg, FileName: string; LineNo: integer); overload;
    procedure Log(Caption, Msg: string); overload;
    procedure FollowFolder(vFolder: string);
    procedure ShowMessagesList;
    procedure ShowWatchesList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Folder: string read GetFolder write SetFolder;
    procedure UpdateFoldersPnl;
    procedure UpdateMessagesPnl;
    procedure UpdateOutputPnl;
    procedure StartServer;
  end;

var
  MainForm: TMainForm;

implementation

uses
  mnXMLUtils, hh, StrUtils, ShellApi, SearchForms, mneProjectOptions, EditorOptions,
  EditorProfiles, mneResources, mneSetups, hh_funcs, Clipbrd,
  SelectFiles, mneSettings, mneConsts,
  SynEditTypes, AboutForms, mneProjectForms, GotoForms, Types, dbgpServers,
  mnePHPIniForm, mnXMLBase64, mneBreakpoints, mneAssociateForm,
  SearchInFilesForms, SynHighlighterHTMLPHP;

{$R *.lfm}

constructor TMainForm.Create(AOwner: TComponent);
var
  FileInfo: TSHFileInfo;
  aIniFile: TIniFile;
  aEngineFile: string;
  aWorkspace: string;
begin
  inherited;
  aIniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'setting.ini');
  try
    aWorkspace := IncludeTrailingPathDelimiter(aIniFile.ReadString('Options', 'Workspace', ''));
  finally
    aIniFile.Free;
  end;
  Engine := TmneEngine.Create;
  Engine.Workspace := aWorkspace;
  Engine.Window := EditorsPnl;
  //FileSet.Align := alClient;
  Engine.OnChangedState := EditorChangeState;
  //  Engine.OnSynEditReplaceText:= OnSynEditReplaceText;
  if (aWorkspace <> '') then
  begin
    aEngineFile := aWorkspace + 'mne-options.xml';
    Engine.Options.Load(aEngineFile);
  end;
  FoldersAct.Checked := Engine.Options.ShowFolder;
  MessagesAct.Checked := Engine.Options.ShowMessages;
  OutputAct.Checked := Engine.Options.ShowOutput;
  OutputEdit.Height := Engine.Options.OutputHeight;
  FoldersPnl.Width := Engine.Options.FoldersWidth;
  MessagesTabs.Height := Engine.Options.MessagesHeight;
  MessagesTabs.Visible := False;
  MessagesSpl.Visible := False;
  UpdateFoldersPnl;
  UpdateMessagesPnl;
  UpdateOutputPnl;
  if Engine.Options.WindowMaxmized then
    WindowState := wsMaximized;
{  else
    BoundsRect := Engine.Options.BoundRect;}
  FillChar(FileInfo, SizeOf(FileInfo), #0);
  if (ParamCount > 0) and not (SameText(ParamStr(1), '/dde')) then
  begin
    Engine.Files.OpenFile(DequoteStr(ParamStr(1)));
    Folder := ExtractFilePath(DequoteStr(ParamStr(1)));
  end;
  FControlStructures := TStringList.Create;
  FControlStructures.Add('if');
  FControlStructures.Add('else');
  FControlStructures.Add('elseif');
  FControlStructures.Add('while');
  FControlStructures.Add('do');
  FControlStructures.Add('for');
  FControlStructures.Add('foreach');
  FControlStructures.Add('break');
  FControlStructures.Add('continue');
  FControlStructures.Add('switch');
  FControlStructures.Add('declare');
  FControlStructures.Add('return');
  FControlStructures.Add('require');
  FControlStructures.Add('include');
  FControlStructures.Add('require_once');
  FControlStructures.Add('include_once');
  if Engine.Options.AutoStartDebugServer then
    DBGStartServerAct.Execute;
end;

procedure TMainForm.UpdateFoldersPnl;
begin
  FoldersSpl.Visible := FoldersAct.Checked;
  FoldersPnl.Visible := FoldersAct.Checked;
  if FoldersAct.Checked then
    UpdateFolder;
end;

procedure TMainForm.FolderCloseBtnClick(Sender: TObject);
begin
  FoldersAct.Execute;
end;

procedure TMainForm.FileSetSelectTab(Sender: TObject; OldTab, NewTab: TntvTabItem; var CanSelect: boolean);
begin
  //
end;

procedure TMainForm.FileSetTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
begin
  Engine.Files.SetCurrentIndex(FileSet.ItemIndex, True);
end;

procedure TMainForm.FoldersActExecute(Sender: TObject);
begin
  UpdateFoldersPnl;
end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
  i: integer;
  aFolder: string;
  A: string;
begin
  Engine.BeginUpdate;
  try
    aFolder := '';
    for i := 0 to Length(FileNames) -1 do
    begin
      A := FileNames[i];
      if DirectoryExists(A) and (i = 0) then
        aFolder := A
      else if FileExists(A) then
        Engine.Files.OpenFile(A);
    end;
    if aFolder <> '' then
      Folder := aFolder;
  finally
    Engine.EndUpdate;
  end;
end;

procedure TMainForm.IPCServerMessage(Sender: TObject);
var
  c: Integer;
begin
  c := IPCServer.MsgType;
  case c of
    0: BringToFront;
    1:
    begin
      Engine.Files.OpenFile(IPCServer.StringMessage);
      BringToFront;
      Activate;
    end;
  end;
end;

procedure TMainForm.OpenActExecute(Sender: TObject);
begin
  Engine.Files.Open;
end;

procedure TMainForm.EngineChanged;
var
  i: integer;
begin
  FileSet.Items.BeginUpdate;
  FileSet.Items.Clear;
  try
    for i := 0 to Engine.Files.Count - 1 do
    begin
      if Engine.Files[i].Name = '' then
        FileSet.Items.AddItem('No name', 'No Name')
      else
        FileSet.Items.AddItem(ExtractFileName(Engine.Files[i].Name), ExtractFileName(Engine.Files[i].Name));
    end;
  finally
    FileSet.Items.EndUpdate;
  end;
  FileSet.Visible := FileSet.Items.Count > 0;
  if Engine.Files.Current = nil then
    QuickFindPnl.Visible := False;
  if (Engine.Projects.IsOpened) and (Engine.Projects.Current.Name <> '') then
    Caption := Engine.Projects.Current.Name + ' - ' + sApplicationTitle
  else
    Caption := sApplicationTitle;
  Application.Title := Caption;
  EnumRecentFile;
  EnumRecentProjects;
  UpdateProject;
end;

procedure TMainForm.EngineRefresh;
begin
  if Engine.Files.Current <> nil then
  begin
    Engine.Files.Current.SynEdit.PopupMenu := EditorPopupMenu;
    FileNameLbl.Caption := Engine.Files.Current.Name;
    FileModeBtn.Caption := Engine.Files.Current.ModeAsText;
    FileSet.ItemIndex := Engine.Files.Current.Index;
    if Engine.Files.Current.Name <> '' then
      FileSet.Items[FileSet.ItemIndex].Caption := ExtractFileName(Engine.Files.Current.Name);
    if Folder = '' then
      Folder := ExtractFilePath(Engine.Files.Current.Name);
    SaveAct.Enabled := Engine.Files.Current.Edited;
    SaveAllAct.Enabled := Engine.Files.GetEditedCount > 0;
  end
  else
  begin
    FileNameLbl.Caption := '';
    FileModeBtn.Caption := '';
    SaveAct.Enabled := False;
    SaveAllAct.Enabled := False;
  end;
  //  DebugPnl.Visible := DebugPnl.Caption <> '';
  UpdateFileHeaderPanel;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  IPCServer.StopServer;
  Engine.Free;
end;

procedure TMainForm.FileCloseBtnClick(Sender: TObject);
begin
  CloseAct.Execute;
end;

procedure TMainForm.FileSetClick(Sender: TObject);
begin
  Engine.Files.SetCurrentIndex(FileSet.ItemIndex, True);
end;

procedure TMainForm.NextActExecute(Sender: TObject);
begin
  Engine.Files.Next;
end;

procedure TMainForm.PriorActExecute(Sender: TObject);
begin
  Engine.Files.Prior;
end;

procedure TMainForm.CloseActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.Files.Current.Close;
end;

procedure TMainForm.SetFolder(const Value: string);
begin
  if Engine.BrowseFolder <> Value then
  begin
    if Value = '..' then
      Engine.BrowseFolder := ExtractFilePath(IncludeTrailingPathDelimiter(Value))
    else
      Engine.BrowseFolder := IncludeTrailingPathDelimiter(Value);
    if FoldersAct.Checked then
      UpdateFolder;
    FolderPanel.Hint := Engine.BrowseFolder;
    FolderBtn.Hint := FolderPanel.Hint;
  end;
end;

procedure TMainForm.FileListDblClick(Sender: TObject);
begin
  FolderOpenAct.Execute;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  mr: integer;
begin
  if Engine.Files.GetEditedCount > 0 then
  begin
    mr := Application.MessageBox('There a files changed but not saved'#13'Save it all?', 'Save before exit', MB_YESNOCANCEL);
    if mr = mrCancel then
      CanClose := False
    else if mr = mrYes then
      Engine.Files.SaveAll;
  end;

  if (Engine.Projects.IsOpened) then
    if (Engine.Projects.Current.FileName = '') then
    begin
      mr := Application.MessageBox(PChar('Save project ' + Engine.Projects.Current.Name + ' before close?'), 'Save', MB_YESNOCANCEL);
      if mr = mrCancel then
        CanClose := False
      else if mr = mrYes then
        Engine.Projects.Current.Save;
    end;
end;

procedure TMainForm.SaveActExecute(Sender: TObject);
begin
  Engine.Files.Save;
end;

procedure TMainForm.EngineEdited;
begin
  if Engine.Files.Current <> nil then
  begin
    SaveAct.Enabled := Engine.Files.Current.Edited;
    if Engine.Files.Current.Edited then
      SaveAllAct.Enabled := True;
  end
  else
  begin
    SaveAct.Enabled := False;
    StatePnl.Caption := '';
  end;
end;

procedure TMainForm.SaveAllActExecute(Sender: TObject);
begin
  Engine.Files.SaveAll;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Engine.Options.ShowFolder := FoldersAct.Checked;
  Engine.Options.ShowMessages := MessagesAct.Checked;
  Engine.Options.ShowOutput := OutputAct.Checked;

  Engine.Options.OutputHeight := OutputEdit.Height;
  Engine.Options.MessagesHeight := MessagesTabs.Height;
  Engine.Options.FoldersWidth := FoldersPnl.Width;

  Engine.Options.WindowMaxmized := WindowState = wsMaximized;
  Engine.Options.BoundRect := BoundsRect;
  Engine.Projects.Close;
  Engine.Options.Save;
  if FRunProject <> nil then
    FRunProject.Terminate;
  //HtmlHelp(Application.Handle, nil, HH_CLOSE_ALL, 0);
end;

procedure TMainForm.NewActExecute(Sender: TObject);
begin
  Engine.Files.New;
end;

procedure TMainForm.CreateWnd;
begin
  inherited;
  DragAcceptFiles(Handle, True);
end;

procedure TMainForm.AssociateActExecute(Sender: TObject);
begin
  with TAssociateForm.Create(Application) do
    ShowModal;
end;

procedure TMainForm.FolderOpenAllActExecute(Sender: TObject);
var
  i: integer;
begin
  Engine.BeginUpdate;
  try
    for i := 0 to FileList.Items.Count - 1 do
    begin
      if PtrUInt(FileList.Items[i].Data) = 1 then
        Engine.Files.OpenFile(Folder + FileList.Items[i].Caption);
    end;
  finally
    Engine.EndUpdate;
  end;
end;

procedure TMainForm.FolderOpenActExecute(Sender: TObject);
begin
  if FileList.Selected <> nil then
  begin
    if PtrUInt(FileList.Selected.Data) = 0 then
      FollowFolder(FileList.Selected.Caption)
    else
      Engine.Files.OpenFile(Folder + FileList.Selected.Caption);
  end;
end;

procedure TMainForm.FindActExecute(Sender: TObject);
begin
  Engine.Files.Find;
end;

procedure TMainForm.FindNextActExecute(Sender: TObject);
begin
  Engine.Files.FindNext;
end;

procedure TMainForm.ApplicationEventsActivate(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Engine.Files.CheckChanged;
end;

procedure TMainForm.FileListKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    FolderOpenAct.Execute;
end;

procedure TMainForm.WMHelp(var Message: TWMHelp);
begin
  if not DoHtmlHelp then
    inherited;
end;

function TMainForm.DoHtmlHelp: boolean;
var
  aToken, aHelp: string;
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aTokenType: integer;
  aRange: Pointer;
  aStart: integer;
  HHAKLink: THHAKLink;
begin
  //we not use Application.Handle for easy to return to main form
  Result := False;
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.SynEdit.Highlighter is TSynHTMLPHPSyn) then
  begin
    P := Engine.Files.Current.SynEdit.CaretXY;
    if GetHighlighterAttriAtRowColEx2(Engine.Files.Current.SynEdit, P, aToken, aTokenType, aStart, Attri, aRange) then
    begin
      if (TtkTokenKind(aTokenType) in [tkValue, tkIdentifier, tkHTML, tkKeyword, tkFunction]) then
      begin
        aHelp := (Engine.Files.Current.SynEdit.Highlighter as TSynHTMLPHPSyn).Processors[RangeToProcessor(aRange)].Name;
        if (aToken <> '') then
        begin
          if (Engine.Options.HelpFiles.Values[aHelp] <> '') then
          begin
            if aHelp = 'html' then
            begin
              HHAKLink.cbStruct := SizeOf(HHAKLink);
              HHAKLink.fReserved := False;
              HHAKLink.pszKeywords := PChar(aToken);
              HHAKLink.pszWindow := nil;
              HHAKLink.fIndexOnFail := True;
              HHAKLink.pszUrl := nil;
              HHAKLink.pszMsgText := nil;
              HHAKLink.pszMsgTitle := nil;
              HtmlHelp(0, PChar(Engine.Options.HelpFiles.Values[aHelp]), HH_KEYWORD_LOOKUP, cardinal(@HHAKLink));
              Result := True;
            end
            else
            begin
              HHAKLink.cbStruct := SizeOf(HHAKLink);
              HHAKLink.fReserved := False;
              HHAKLink.pszKeywords := PChar(aToken);
              HHAKLink.pszWindow := nil;
              HHAKLink.fIndexOnFail := True;
              HHAKLink.pszUrl := nil;
              HHAKLink.pszMsgText := nil;
              HHAKLink.pszMsgTitle := nil;
              HtmlHelp(0, PChar(Engine.Options.HelpFiles.Values[aHelp]), HH_KEYWORD_LOOKUP, cardinal(@HHAKLink));
              Result := True;
{              if FControlStructures.IndexOf(aToken) >= 0 then
                aToken := 'control-structures.' + StringReplace(aToken, '_', '-', [rfReplaceAll]) + '.html'
              else
                aToken := 'function.' + StringReplace(aToken, '_', '-', [rfReplaceAll]) + '.html';
              HtmlHelp(0, pchar(Engine.Options.HelpFiles.Values[aHelp] + '::/' + aToken), HH_DISPLAY_TOPIC, 0);
              Result := True;}
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.KeywordActExecute(Sender: TObject);
begin
  DoHtmlHelp;
end;

procedure TMainForm.HelpIndexActExecute(Sender: TObject);
begin
  if (Engine.Options.HelpFiles.Values['php'] <> '') then
    HtmlHelp(0, PChar(Engine.Options.HelpFiles.Values['php'] + '::/' + 'index.html'), HH_DISPLAY_TOPIC, 0);
end;

procedure TMainForm.EditorOptionsActExecute(Sender: TObject);
begin
  Engine.Options.Show;
end;

procedure TMainForm.SettingActExecute(Sender: TObject);
begin
  ShowSettingForm(Engine);
end;

procedure TMainForm.RunActExecute(Sender: TObject);
begin
  if Engine.Debug.Count > 0 then
  begin
    Engine.Debug.AddAction(TdbgpRun.Create);
    Engine.Debug.AddAction(TdbgpGetWatches.Create);
    Engine.Debug.AddAction(TdbgpGetCurrent.Create);
    Engine.Debug.Resume;
  end
  else
  begin
    Engine.Debug.Clear;
    RunScript;
  end;
end;

procedure TMainForm.ProjectOptionsActExecute(Sender: TObject);
begin
  if Engine.Projects.IsOpened then
  begin
    ShowProjectForm(Engine.Projects.Current);
    Engine.UpdateState([ecsChanged]);
  end;
end;

procedure TMainForm.NewProjectActExecute(Sender: TObject);
var
  aProject: TEditorProject;
begin
  Engine.Projects.Close;
  aProject := Engine.Projects.New;
  if ShowProjectForm(aProject) then
    Engine.Projects.Current := aProject
  else
    aProject.Free;
end;

procedure TMainForm.OpenProjectActExecute(Sender: TObject);
begin
  Engine.Projects.Open;
end;

procedure TMainForm.SaveProjectActExecute(Sender: TObject);
begin
  Engine.Projects.Current.Save;
end;

procedure TMainForm.SelectFileActExecute(Sender: TObject);
begin
  if Engine.Projects.IsOpened then
    ShowSelectFile(Engine.Projects.Current.RootDir)
  else
    ShowSelectFile(Folder);
end;

procedure TMainForm.EnumRecentFile;
var
  i, c: integer;
  aMenuItem: TMenuItem;
begin
  ReopenMnu.Clear;
  c := Engine.Options.RecentFiles.Count;
  if c > 10 then
    c := 10;
  for i := 0 to c - 1 do
  begin
    aMenuItem := TMenuItem.Create(Self);
    aMenuItem.Caption := Engine.Options.RecentFiles[i];
    aMenuItem.Hint := aMenuItem.Caption;
    aMenuItem.OnClick := ReopenClick;
    ReopenMnu.Add(aMenuItem);
  end;
end;

procedure TMainForm.ReopenClick(Sender: TObject);
var
  aFile: string;
begin
  if Sender is TMenuItem then
  begin
    aFile := (Sender as TMenuItem).Caption;
    if Engine.Projects.IsOpened then
      aFile := ExpandToPath(aFile, Engine.Projects.Current.RootDir);
    Engine.Files.OpenFile(aFile);
  end;
end;

procedure TMainForm.ReopenProjectClick(Sender: TObject);
var
  aFile: string;
begin
  if Sender is TMenuItem then
  begin
    aFile := (Sender as TMenuItem).Caption;
    Engine.Projects.Load(aFile);
  end;
end;

procedure TMainForm.EnumRecentProjects;
var
  i, c: integer;
  aMenuItem: TMenuItem;
begin
  ReopenProjectMnu.Clear;
  c := Engine.Options.RecentProjects.Count;
  if c > 10 then
    c := 10;
  for i := 0 to c - 1 do
  begin
    aMenuItem := TMenuItem.Create(Self);
    aMenuItem.Caption := Engine.Options.RecentProjects[i];
    aMenuItem.Hint := aMenuItem.Caption;
    aMenuItem.OnClick := ReopenProjectClick;
    ReopenProjectMnu.Add(aMenuItem);
  end;
end;

procedure TMainForm.CloseProjectActExecute(Sender: TObject);
begin
  Engine.Projects.Close;
end;

procedure TMainForm.OpenFolderActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    ShellExecute(0, 'open', 'explorer.exe', PChar('/select,"' + Engine.Files.Current.Name + '"'), nil, SW_SHOW);
end;

procedure TMainForm.ApplicationEventsHint(Sender: TObject);
begin
  StatusTimer.Enabled := False;
  if Application.Hint <> '' then
    MessagePnl.Caption := Application.Hint;
  StatusTimer.Enabled := True;
end;

procedure TMainForm.UpdateMessagesPnl;
begin
  MessagesTabs.Visible := MessagesAct.Checked;
  MessagesSpl.Visible := MessagesAct.Checked;
end;

procedure TMainForm.MessagesActExecute(Sender: TObject);
begin
  UpdateMessagesPnl;
end;

procedure TMainForm.psvPHPBeforeExecute(Sender: TObject);
begin
  MessageList.Items.Clear;
end;

procedure TMainForm.UnixMnuClick(Sender: TObject);
begin
  Engine.Files.Current.Mode := efmUnix;
end;

procedure TMainForm.WindowsMnuClick(Sender: TObject);
begin
  Engine.Files.Current.Mode := efmWindows;
end;

procedure TMainForm.MacMnuClick(Sender: TObject);
begin
  Engine.Files.Current.Mode := efmMac;
end;

procedure TMainForm.ExtractkeywordsClick(Sender: TObject);
const
  aPath = 'D:\work\delphi\projects\LightPHPEdit\source\lib\';
var
  aList: TStringList;

  procedure ExtractKeywords(const Name, Keywords: string);
  var
    aStrings: TStringList;
    i: integer;
    s: string;
  begin
    aStrings := TStringList.Create;
    try
      ExtractStrings([','], [], PChar(Keywords), aStrings);
      aStrings.Sort;
      for i := 0 to aStrings.Count - 1 do
      begin
        s := '    ''' + aStrings[i];
        if i < aStrings.Count - 1 then
          s := s + ',';
        s := s + '''';
        if i >= aStrings.Count - 1 then
          s := s + ';'
        else
          s := s + '+';
        aStrings[i] := s;
      end;
      aStrings.Insert(0, '  ' + Name + ' =');
      aStrings.Add('');
      aList.AddStrings(aStrings);
    finally
      aStrings.Free;
    end;
  end;

begin
  aList := TStringList.Create;
  ExtractKeywords('sPHPControls', sPHPControls);
  ExtractKeywords('sPHPKeywords', sPHPKeywords);
  ExtractKeywords('sPHPFunctions', sPHPFunctions);
  ExtractKeywords('sPHPConstants', sPHPConstants);
  ExtractKeywords('sPHPVariables', sPHPVariables);
  ExtractKeywords('sHTMLKeywords', sHTMLKeywords);
  ExtractKeywords('sSQLKeywords', sSQLKeywords);
  aList.SaveToFile(aPath + 'PHPKeywords.inc.new');
  aList.Free;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  //  ThemeServices.us
  //  ThemeServices.ThemesEnabled := False;
  FMessages := Engine.MessagesList.GetMessages('Messages');
{$IFOPT D+}
  Extractkeywords.Visible := True;
{$ENDIF}
  EngineChanged;
  EngineRefresh;
  EngineDebug;
  FileSet.ShowBorder := False;
  IPCServer.ServerID := sApplicationID;
  IPCServer.StartServer;
end;

procedure TMainForm.CheckActExecute(Sender: TObject);
begin
  if (Engine.Files.Current <> nil) and (fgkExecutable in Engine.Files.Current.Group.Kind) then
  begin
    SaveAllAct.Execute;
//    RunInternal(True);
  end;
end;

procedure TMainForm.Log(Error: integer; Caption, Msg, FileName: string; LineNo: integer);
var
  aItem: TListItem;
begin
  aItem := MessageList.Items.Add;
  aItem.Caption := Caption;
  aItem.ImageIndex := 34;
  aItem.SubItems.Add(Msg);
  aItem.SubItems.Add(FileName);
  if LineNo > 0 then
    aItem.SubItems.Add(IntToStr(LineNo));
end;

procedure TMainForm.AboutActExecute(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.UpdateProject;
var
  b: boolean;
begin
  b := Engine.Projects.IsOpened;
  ProjectOptionsAct.Enabled := b;
  SaveProjectAct.Enabled := b;
  SaveAsProjectAct.Enabled := b;
  AddFileToProjectAct.Enabled := b;
  CloseProjectAct.Enabled := b;
  ProjectOpenFolderAct.Enabled := b;
end;

procedure TMainForm.SaveAsProjectActExecute(Sender: TObject);
begin
  if Engine.Projects.IsOpened then
    Engine.Projects.Current.SaveAs;
end;

procedure TMainForm.ProjectOpenFolderActExecute(Sender: TObject);
begin
  if Engine.Projects.IsOpened then
    ShellExecute(0, 'open', 'explorer.exe', PChar('/select,"' + Engine.Projects.Current.FileName + '"'), nil, SW_SHOW);
end;

procedure TMainForm.ManageActExecute(Sender: TObject);
begin
  with TManageProjectsForm.Create(Application) do
  begin
    ShowModal;
  end;
end;

procedure TMainForm.CloseAllActExecute(Sender: TObject);
begin
  Engine.Files.CloseAll;
end;

procedure TMainForm.OpenIncludeActExecute(Sender: TObject);
var
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aToken: string;
  aTokenType: integer;
  aStart: integer;

  function TryOpen: boolean;
  begin
    aToken := Engine.ExpandFileName(aToken);
    Result := FileExists(aToken);
    if Result then
      Engine.Files.OpenFile(aToken);
  end;

begin
  if Engine.Files.Current <> nil then
  begin
    if Engine.Files.Current.Group.Category.Name = 'HTML/PHP' then
    begin
      P := Engine.Files.Current.SynEdit.CaretXY;
      Engine.Files.Current.SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      aToken := DequoteStr(aToken);
      if (aToken <> '') and (TtkTokenKind(aTokenType) = tkString) then
      begin
        aToken := StringReplace(aToken, '/', '\', [rfReplaceAll, rfIgnoreCase]);
        if not TryOpen then
        begin
          aToken := ExtractFileName(aToken);
          TryOpen;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.CopyActUpdate(Sender: TObject);
begin
  CopyAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.SynEdit.SelAvail;
end;

procedure TMainForm.PasteActUpdate(Sender: TObject);
begin
  PasteAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.SynEdit.CanPaste;
end;

procedure TMainForm.CutActUpdate(Sender: TObject);
begin
  CutAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.SynEdit.SelAvail;
end;

procedure TMainForm.SelectAllActUpdate(Sender: TObject);
begin
  SelectAllAct.Enabled := (Engine.Files.Current <> nil);
end;

procedure TMainForm.PasteActExecute(Sender: TObject);
begin
  Engine.Files.Current.SynEdit.PasteFromClipboard;
end;

procedure TMainForm.CopyActExecute(Sender: TObject);
begin
  Engine.Files.Current.SynEdit.CopyToClipboard;
end;

procedure TMainForm.CutActExecute(Sender: TObject);
begin
  Engine.Files.Current.SynEdit.CutToClipboard;
end;

procedure TMainForm.SelectAllActExecute(Sender: TObject);
begin
  Engine.Files.Current.SynEdit.SelectAll;
end;

function TMainForm.CanOpenInclude: boolean;
var
  P: TPoint;
  Attri: TSynHighlighterAttributes;
  aToken: string;
  aTokenType: integer;
  aStart: integer;
begin
  Result := False;
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.Group <> nil) then
  begin
    if Engine.Files.Current.Group.Category.Name = 'HTML/PHP' then
    begin
      P := Engine.Files.Current.SynEdit.CaretXY;
      Engine.Files.Current.SynEdit.GetHighlighterAttriAtRowColEx(P, aToken, aTokenType, aStart, Attri);
      Result := (aToken <> '') and (TtkTokenKind(aTokenType) = tkString);
    end;
  end;
end;

procedure TMainForm.OpenIncludeActUpdate(Sender: TObject);
begin
  OpenIncludeAct.Enabled := CanOpenInclude;
end;

procedure TMainForm.GotoLineActUpdate(Sender: TObject);
begin
  GotoLineAct.Enabled := (Engine.Files.Current <> nil);
end;

procedure TMainForm.GotoLineActExecute(Sender: TObject);
begin
  with TGotoLineForm.Create(Application) do
  begin
    NumberEdit.Text := IntToStr(LastGotoLine);
    if ShowModal = mrOk then
    begin
      if NumberEdit.Text <> '' then
      begin
        LastGotoLine := StrToIntDef(NumberEdit.Text, 0);
        //Engine.Files.Current.SynEdit.GotoLineAndCenter(LastGotoLine);
      end;
    end;
    Free;
  end;
end;

procedure TMainForm.UpdateFolder;
var
  r: integer;
  SearchRec: TSearchRec;
  aItem: TListItem;
  AExtensions: TStringList;

  function FindExtension(vExtension: string): boolean;
  begin
    if LeftStr(vExtension, 1) = '.' then
      vExtension := Copy(vExtension, 2, MaxInt);
    Result := AExtensions.IndexOf(vExtension) >= 0;
  end;

begin
  FileList.Items.BeginUpdate;
  try
    AExtensions := TStringList.Create;
    try
      Engine.Groups.EnumExtensions(AExtensions);

      FileList.Clear;
      if (Folder <> '') and DirectoryExists(Folder) then
      begin
        r := FindFirst(Folder + '*.*', faAnyFile or faDirectory, SearchRec);
        while r = 0 do
        begin
          if (SearchRec.Name <> '.') then
          begin
            if (SearchRec.Attr and faDirectory) <> 0 then
            begin
              aItem := FileList.Items.Add;
              aItem.Caption := SearchRec.Name;
              aItem.Data := nil;
              aItem.ImageIndex := 0;
            end;
          end;
          r := FindNext(SearchRec);
        end;
        FindClose(SearchRec);

        r := FindFirst(Folder + '*.*', faAnyFile, SearchRec);
        while r = 0 do
        begin
          if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') and (not SameText(SearchRec.Name, '.svn')) then
          begin
            if FindExtension(ExtractFileExt(SearchRec.Name)) then
            begin
              aItem := FileList.Items.Add;
              aItem.Caption := SearchRec.Name;
              aItem.Data := Pointer(1);
              aItem.ImageIndex := GetFileImageIndex(SearchRec.Name);
            end;
          end;
          r := FindNext(SearchRec);
        end;
        FindClose(SearchRec);
      end;
    finally
      AExtensions.Free;
    end;
  finally
    FileList.Items.EndUpdate;
  end;
end;

procedure TMainForm.GotoFolder1Click(Sender: TObject);
var
  aFolder: string;
begin
  aFolder := Folder;
  if SelectFolder('Select root directory for you Engine', '', aFolder) then
  begin
    Folder := aFolder;
  end;
end;

procedure TMainForm.EditorChangeState(State: TEditorChangeState);
begin
  if ecsFolder in State then
    UpdateFolder;
  if ecsChanged in State then
    EngineChanged;
  if ecsRefresh in State then
    EngineRefresh;
  if ecsDebug in State then
    EngineDebug;
  if ecsEdit in State then
    EngineEdited;
  if ecsProjectLoaded in State then
    EngineProjectLoaded;
  if ecsState in State then
    EngineState;
end;

procedure TMainForm.EngineProjectLoaded;
begin
  if (Engine.Projects.IsOpened) and (Engine.Projects.Current.RootDir <> '') then
    Folder := Engine.Projects.Current.RootDir;
end;

procedure TMainForm.ReplaceActExecute(Sender: TObject);
begin
  Engine.Files.Replace;
end;

destructor TMainForm.Destroy;
begin
  FControlStructures.Free;
  inherited;
end;

procedure TMainForm.RevertActExecute(Sender: TObject);
begin
  Engine.Files.Revert;
end;

procedure TMainForm.FileListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_F5:
      begin
        UpdateFolder;
      end;
      VK_BACK:
      begin
        FollowFolder('..');
      end;
    end;
  end;
end;

procedure TMainForm.FollowFolder(vFolder: string);
var
  OldFolder: string;
  i, n: integer;
begin
  if vFolder = '..' then
    OldFolder := ExtractFileName(ExcludeTrailingPathDelimiter(Folder))
  else
    OldFolder := '';
  Folder := Folder + vFolder;
  n := 0; // if old folder not found or go inside it
  if (OldFolder <> '') then
  begin
    for i := 0 to FileList.Items.Count - 1 do
    begin
      if FileList.Items[i].Caption = OldFolder then
      begin
        n := i;
        break;
      end;
    end;
  end;
  if n >= 0 then
  begin
    FileList.ItemIndex := n;
    FileList.Items[FileList.ItemIndex].Focused := True;
  end;
end;

procedure TMainForm.FileFolder1Click(Sender: TObject);
begin
  if FileList.Selected <> nil then
  begin
    ShellExecute(0, 'open', 'explorer.exe', PChar('/select,"' + Folder + FileList.Selected.Caption + '"'), nil, SW_SHOW);
  end;
end;

procedure TMainForm.OpenColorActUpdate(Sender: TObject);
begin
  OpenColorAct.Enabled := GetCurrentColorText <> '';
end;

function TMainForm.GetCurrentColorText: string;
begin
  Result := '';
  if (Engine.Files.Current <> nil) then
  begin
    Result := Trim(GetWordAtRowColEx(Engine.Files.Current.SynEdit, Engine.Files.Current.SynEdit.CaretXY, TSynValidStringChars + ['#'], False));
    if Result <> '' then
    begin
      if Result[1] <> '#' then
        Result := '';
    end;
  end;
end;

procedure TMainForm.OpenColorActExecute(Sender: TObject);
var
  aWord: string;
  aColor: TColor;
  aDialog: TColorDialog;
  aIsUpper: boolean;

  procedure CheckIsUpper;
  var
    i: integer;
  begin
    aIsUpper := False;
    for i := 1 to Length(aWord) do
    begin
      if aWord[i] in ['A'..'Z', 'a'..'z'] then
      begin
        aIsUpper := IsCharUpper(aWord[i]);
        break;
      end;
    end;
  end;

begin
  aWord := GetCurrentColorText;
  if (aWord <> '') and (Length(aWord) > 1) then
  begin
    CheckIsUpper;
    aColor := RGBHexToColor(aWord);
    aDialog := TColorDialog.Create(Self);
    //aDialog.Options := aDialog.Options + [cdFullOpen];
    try
      aDialog.Color := aColor;
      if aDialog.Execute then
      begin
        aWord := ColorToRGBHex(aDialog.Color);
        GetWordAtRowColEx(Engine.Files.Current.SynEdit, Engine.Files.Current.SynEdit.CaretXY, TSynValidStringChars + ['#'], True);
        if aIsUpper then
          aWord := UpperCase(aWord)
        else
          aWord := LowerCase(aWord);
        Engine.Files.Current.SynEdit.SelText := aWord;
      end;
    finally
      aDialog.Free;
    end;
  end;
end;

procedure TMainForm.FileModeBtnClick(Sender: TObject);
var
  Pt: TPoint;
begin
  Pt.X := FileModeBtn.BoundsRect.Left;
  Pt.Y := FileModeBtn.BoundsRect.Bottom;
  Pt := FileModeBtn.ClientToScreen(Pt);
  FileModeBtn.PopupMenu.Popup(Pt.X, Pt.Y);
end;

procedure TMainForm.StartServer;
begin
end;

procedure TMainForm.FolderBtnClick(Sender: TObject);
var
  Pt: TPoint;
begin
  Pt.X := FolderBtn.BoundsRect.Left;
  Pt.Y := FolderBtn.BoundsRect.Bottom;
  Pt := FolderPanel.ClientToScreen(Pt);
  FolderBtn.PopupMenu.Popup(Pt.X, Pt.Y);
end;

procedure TMainForm.SVNCommitActExecute(Sender: TObject);
begin
  ShellExec(Engine.Options.TortoiseProc, '/command:commit /path:"' + Folder + '" /notempfile /closeonend');
end;

procedure TMainForm.SVNDiffFileActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    ShellExec(Engine.Options.TortoiseProc, '/command:diff /path:"' + Engine.Files.Current.Name + '" /notempfile /closeonend');
end;

procedure TMainForm.SVNCommitFileActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    ShellExec(Engine.Options.TortoiseProc, '/command:commit /path:"' + Engine.Files.Current.Name + '" /notempfile /closeonend');
end;

procedure TMainForm.SVNUpdateFileActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    ShellExec(Engine.Options.TortoiseProc, '/command:update /path:"' + Engine.Files.Current.Name + '" /notempfile /closeonend');
end;

procedure TMainForm.SVNUpdateActExecute(Sender: TObject);
begin
  ShellExec(Engine.Options.TortoiseProc, '/command:update /path:"' + Folder + '" /notempfile /closeonend');
end;

procedure TMainForm.SVNRevertActExecute(Sender: TObject);
begin
  ShellExec(Engine.Options.TortoiseProc, '/command:revert /path:"' + Folder + '" /notempfile /closeonend');
end;

procedure TMainForm.DBGStopServerActUpdate(Sender: TObject);
begin
  DBGStopServerAct.Enabled := Engine.Debug.Active;
end;

procedure TMainForm.DBGStartServerActUpdate(Sender: TObject);
begin
  DBGStartServerAct.Enabled := not Engine.Debug.Active;
end;

procedure TMainForm.DBGStartServerActExecute(Sender: TObject);
begin
  Engine.Debug.Start;
end;

procedure TMainForm.DBGStopServerActExecute(Sender: TObject);
begin
  Engine.Debug.Stop;
end;

procedure TMainForm.DBGStepOverActExecute(Sender: TObject);
begin
  if Engine.Debug.Count > 0 then
  begin
    Engine.Debug.AddAction(TdbgpStepOver.Create);
    Engine.Debug.AddAction(TdbgpGetWatches.Create);
    Engine.Debug.AddAction(TdbgpGetCurrent.Create);
    Engine.Debug.Resume;
  end;
end;

procedure TMainForm.DBGStepIntoActExecute(Sender: TObject);
begin
  Engine.Debug.AddAction(TdbgpStepInto.Create);
  Engine.Debug.AddAction(TdbgpGetWatches.Create);
  Engine.Debug.AddAction(TdbgpGetCurrent.Create);
  Engine.Debug.Resume;
end;

procedure TMainForm.DBGActiveServerActUpdate(Sender: TObject);
begin
  DBGActiveServerAct.Checked := Engine.Debug.Active;
end;

procedure TMainForm.DBGActiveServerActExecute(Sender: TObject);
begin
  Engine.Debug.Active := not DBGActiveServerAct.Checked;
end;

procedure TMainForm.DBGResetActExecute(Sender: TObject);
begin
  Engine.Debug.AddAction(TdbgpStop.Create);
  Engine.Debug.AddAction(TdbgpGetCurrent.Create);
  Engine.Debug.Resume;
end;

procedure TMainForm.DBGDetachActExecute(Sender: TObject);
begin
  Engine.Debug.AddAction(TdbgpDetach.Create);
  Engine.Debug.AddAction(TdbgpGetCurrent.Create); //usfull to detect the disconnected
  Engine.Debug.Resume;
end;

procedure TMainForm.DBGStepOutActExecute(Sender: TObject);
begin
  Engine.Debug.AddAction(TdbgpStepOut.Create);
  Engine.Debug.AddAction(TdbgpGetWatches.Create);
  Engine.Debug.AddAction(TdbgpGetCurrent.Create);
  Engine.Debug.Resume;
end;

function TMainForm.GetFolder: string;
begin
  Result := Engine.BrowseFolder;
end;

procedure TMainForm.RunTerminated(Sender: TObject);
begin
  FRunProject := nil;
end;

procedure TMainForm.ReceiveBuffer(const Buffer: string);
begin
  if Engine.Options.SendOutputToNewFile then
  begin
    if FRunProject.Console = nil then
      FRunProject.Console := Engine.Files.New('html', 'Result', Engine.Files.Current.Name, True, False).SynEdit;
  end
  else
  begin
    FRunProject.Console := OutputEdit;
    OutputAct.Checked := True;
    UpdateOutputPnl;
  end;
  (FRunProject.Console as TCustomSynEdit).Text := Buffer;
  (FRunProject.Console as TCustomSynEdit).CaretY := (FRunProject.Console as TCustomSynEdit).Lines.Count - 1;
end;

procedure TMainForm.SaveAsActExecute(Sender: TObject);
begin
  Engine.Files.SaveAs;
end;

procedure TMainForm.EngineDebug;
begin
  DebugPnl.Caption := Engine.Debug.SessionName;
  UpdateFileHeaderPanel;
  UpdateWatches;
  Engine.Files.Refresh; // not safe thread
end;

procedure TMainForm.UpdateFileHeaderPanel;
begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.SynEdit = Engine.Debug.ExecuteEdit) then
    FileHeaderPanel.Color := $00C6C6EC
  else
    FileHeaderPanel.Color := $00EEE0D7;
  FileHeaderPanel.Visible := Engine.Files.Count > 0;
  if FileHeaderPanel.Visible then
    FileHeaderPanel.Refresh;
end;

procedure TMainForm.PHPIniConfigActExecute(Sender: TObject);
begin
  ShowPHPIniForm;
end;

procedure TMainForm.MessagesTabChange(Sender: TObject; NewTab: integer; var AllowChange: boolean);
begin
end;

procedure TMainForm.UpdateWatches;
var
  i: integer;
  aItem: TListItem;
begin
  WatchList.Clear;
  DBGLock.Lock;
  try
    for i := 0 to Engine.Debug.Watches.Count - 1 do
    begin
      aItem := WatchList.Items.Add;
      aItem.ImageIndex := 41;
      aItem.Caption := Engine.Debug.Watches[i].VariableName;
      aItem.SubItems.Add(Engine.Debug.Watches[i].VariableType);
      aItem.SubItems.Add(Engine.Debug.Watches[i].Value);
    end;
  finally
    DBGLock.Unlock;
  end;
end;

procedure TMainForm.ShowValue1Click(Sender: TObject);
var
  s: string;
  aAction: TdbgpGetWatchInstance;
begin
  if Engine.Files.Current.SynEdit.SelEnd = 0 then
    s := Trim(Engine.Files.Current.SynEdit.GetWordAtRowCol(Engine.Files.Current.SynEdit.CaretXY))
  else
    s := Engine.Files.Current.SynEdit.SelText;
  aAction := TdbgpGetWatchInstance.Create;
  aAction.VariableName := s;
  Engine.Debug.AddAction(aAction);
  Engine.Debug.Resume;
end;

procedure TMainForm.ShowMessagesList;
begin
  MessagesTabs.ItemIndex := 0;
end;

procedure TMainForm.ShowWatchesList;
begin
  MessagesTabs.ItemIndex := 1;
end;

procedure TMainForm.Add1Click(Sender: TObject);
var
  s: string;
begin
  s := InputBox('Add Watch', 'Enter variable name', '');
  if s <> '' then
    AddWatch(s);
end;

procedure TMainForm.AddWatch(s: string);
begin
  s := Trim(s);
  if s <> '' then
  begin
    Engine.Debug.Watches.AddWatch(s);
    UpdateWatches;
  end;
end;

procedure TMainForm.Delete1Click(Sender: TObject);
begin
  DeleteCurrentWatch;
end;

procedure TMainForm.DeleteWatch(s: string);
begin
  Engine.Debug.Watches.RemoveWatch(s);
  UpdateWatches;
end;

procedure TMainForm.DBGToggleBreakpointExecute(Sender: TObject);
var
  aLine: integer;
begin
  if (Engine.Files.Current <> nil) and (GetFocus = Engine.Files.Current.SynEdit.Handle) and (fgkExecutable in Engine.Files.Current.Group.Kind) then
    with Engine.Files.Current do
    begin
      aLine := SynEdit.ScreenRowToRow(SynEdit.CaretY);
      DBGLock.Lock;
      try
        Engine.Debug.Breakpoints.Toggle(Name, aLine);
      finally
        DBGLock.Unlock;
      end;
      SynEdit.InvalidateLine(aLine);
    end;
end;

procedure TMainForm.OutputActExecute(Sender: TObject);
begin
  UpdateOutputPnl;
end;

procedure TMainForm.UpdateOutputPnl;
begin
  if OutputAct.Checked then
  begin
    OutputEdit.Visible := True;
    OutputSpl.Visible := True;
  end
  else
  begin
    OutputEdit.Visible := False;
    OutputSpl.Visible := False;
  end;
end;

procedure TMainForm.RunScript;
var
  aFile: string;
  aRoot: string;
  aUrlMode: TRunMode;
begin
  if (Engine.Files.Current <> nil) and (fgkExecutable in Engine.Files.Current.Group.Kind) then
  begin
    SaveAllAct.Execute;
    aFile := Engine.Files.Current.Name;
    if (Engine.Projects.IsOpened) then
    begin
      aFile := ExpandToPath(aFile, Engine.Projects.Current.RootDir);
      aUrlMode := Engine.Projects.Current.RunMode;
    end
    else
    begin
      aUrlMode := prunNone;
    end;

    case aUrlMode of
      prunUrl:
      begin
        if Engine.Projects.IsOpened then
        begin
          aRoot := IncludeTrailingPathDelimiter(Engine.Projects.Current.RootDir);
          if SameText((Copy(aFile, 1, Length(aRoot))), aRoot) then
          begin
            aFile := Copy(aFile, Length(aRoot) + 1, MaxInt);
            aFile := IncludeSlash(Engine.Projects.Current.RootUrl) + aFile;
            ShellExecute(0, 'open', PChar(aFile), '', PChar(ExtractFilePath(aFile)), SW_SHOWNOACTIVATE);
          end;
        end;
      end;
      prunConsole:
      begin
        if Engine.Options.CompilerFolder <> '' then
          aRoot := IncludeTrailingPathDelimiter(Engine.Options.CompilerFolder) + 'php.exe'
        else
          aRoot := 'php.exe';
        ShellExecute(0, '', PChar(aRoot), PChar(aFile), PChar(ExtractFilePath(aFile)), SW_SHOWNOACTIVATE);
      end;
    end;
  end;
end;

procedure TMainForm.DBGBreakpointsActExecute(Sender: TObject);
begin
  ShowBreakpointsForm;
end;

procedure TMainForm.CopyFileNameActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Clipboard.AsText := Engine.Files.Current.Name;
end;

procedure TMainForm.DBGAddWatchActExecute(Sender: TObject);
var
  s: string;
begin
  if Engine.Files.Current <> nil then
  begin
    if Engine.Files.Current.SynEdit.SelEnd = 0 then
      s := Trim(Engine.Files.Current.SynEdit.GetWordAtRowCol(Engine.Files.Current.SynEdit.CaretXY))
    else
      s := Engine.Files.Current.SynEdit.SelText;
    AddWatch(s);
    Engine.Debug.AddAction(TdbgpGetWatches.Create);
    Engine.Debug.AddAction(TdbgpGetCurrent.Create);
    Engine.Debug.Resume;
  end;
end;

procedure TMainForm.DeleteCurrentWatch;
begin
  if WatchList.Selected <> nil then
  begin
    DeleteWatch(WatchList.Selected.Caption);
  end;
end;

procedure TMainForm.WatchListKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_DELETE: DeleteCurrentWatch;
    end;
  end;
end;

procedure TMainForm.FolderHomeActExecute(Sender: TObject);
begin
  if Engine.Projects.Current <> nil then
    Folder := Engine.Projects.Current.RootDir;
end;

procedure TMainForm.StatusTimerTimer(Sender: TObject);
begin
  StatusTimer.Enabled := False;
  MessagePnl.Caption := '';
end;

procedure TMainForm.Clear1Click(Sender: TObject);
begin
  if MessagesPopup.PopupComponent is TListView then
    (MessagesPopup.PopupComponent as TListView).Clear;
end;

procedure TMainForm.SearchListDblClick(Sender: TObject);
var
  s: string;
  aLine, c, l: integer;
begin
  if SearchList.Selected <> nil then
  begin
    Engine.Files.OpenFile(SearchList.Selected.Caption);
    s := SearchList.Selected.SubItems[0];
    if s <> '' then
    begin
      aLine := StrToIntDef(s, 0);
      if aLine > 0 then
      begin
        if SearchList.Selected is TSearchListItem then
        begin
          c := (SearchList.Selected as TSearchListItem).Column;
          l := (SearchList.Selected as TSearchListItem).Length;
        end
        else
        begin
          c := 0;
          l := 0;
        end;
        Engine.Files.Current.SynEdit.CaretY := aLine;
        if l > 0 then
        begin
          Engine.Files.Current.SynEdit.CaretX := c;
          Engine.Files.Current.SynEdit.SelEnd := c + l;
        end
        else
          Engine.Files.Current.SynEdit.CaretX := 0;
        Engine.Files.Current.SynEdit.SetFocus;
      end;
    end;
  end;
end;

procedure TMainForm.MessageListDblClick(Sender: TObject);
var
  s: string;
  aLine: integer;
begin
  if MessageList.Selected <> nil then
  begin
    Engine.Files.OpenFile(MessageList.Selected.SubItems[1]);
    if MessageList.Selected.SubItems.Count > 2 then
    begin
      s := MessageList.Selected.SubItems[2];
      if s <> '' then
      begin
        aLine := StrToIntDef(s, 0);
        if aLine > 0 then
        begin
          Engine.Files.Current.SynEdit.CaretY := aLine;
          Engine.Files.Current.SynEdit.CaretX := 0;
          Engine.Files.Current.SynEdit.SetFocus;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.SearchListCustomDrawSubItem(Sender: TCustomListView; Item: TListItem; SubItem: integer; State: TCustomDrawState; var DefaultDraw: boolean);
var
  c, l: integer;
  w: integer;
  aRect: TRect;
  s, bf, md, af: string;
begin
  if (Sender.Items.Count > 0) and (SubItem = 2) and (Item.SubItems.Count > 0) then
  begin
    DefaultDraw := False;

    Sender.Canvas.Lock;
    try
      ListView_GetSubItemRect(Sender.Handle, Item.Index, SubItem, LVIR_BOUNDS, @aRect);
      c := (Item as TSearchListItem).Column;
      l := (Item as TSearchListItem).Length;
      s := Item.SubItems[1];
      bf := Copy(s, 1, c - 1);
      md := Copy(Item.SubItems[1], c, l);
      af := Copy(Item.SubItems[1], c + l, MaxInt);
      //TControlCanvas(Sender.Canvas).UpdateTextFlags;
      Sender.Canvas.Font.Color := clWindowText;
      Sender.Canvas.Brush.Color := clWindow;
      w := aRect.Left + 2;
      aRect.Bottom := aRect.Bottom - 1;
      aRect.Left := aRect.Left + 1;

      Sender.Canvas.FillRect(aRect);
      Sender.Canvas.Font.Style := [];
      Sender.Canvas.TextOut(w, aRect.Top, bf);
      w := w + Sender.Canvas.TextWidth(bf);
      Sender.Canvas.Refresh; //need to change color because canvas not change the font when style changed
      Sender.Canvas.Font.Style := [fsBold];
      Sender.Canvas.TextOut(w, aRect.Top, md);
      w := w + Sender.Canvas.TextWidth(md);
      Sender.Canvas.Font.Style := [];
      Sender.Canvas.Refresh; //need to change color because canvas not change the font when style changed
      Sender.Canvas.TextOut(w, aRect.Top, af);
    finally
      Sender.Canvas.Unlock;
    end;
  end
  else
    DefaultDraw := True;
end;

procedure TMainForm.FindInFilesActExecute(Sender: TObject);
var
  aText, aFolder: string;
begin
  if Engine.Files.Current <> nil then
  begin
    if Engine.Files.Current.SynEdit.SelAvail and (Engine.Files.Current.SynEdit.BlockBegin.y = Engine.Files.Current.SynEdit.BlockEnd.y) then
      aText := Engine.Files.Current.SynEdit.SelText
    else
      aText := Engine.Files.Current.SynEdit.GetWordAtRowCol(Engine.Files.Current.SynEdit.CaretXY);
  end;

  if Engine.Projects.Current <> nil then
    aFolder := Engine.Projects.Current.RootDir
  else
    aFolder := '';
  if aFolder = '' then
    aFolder := Folder;
  MessagesAct.Checked := True;
  UpdateMessagesPnl;
  MessagesTabs.ItemIndex := 2;
  ShowSearchInFilesForm(SearchList, aText, ExpandFileName(aFolder), Engine.Options.SearchFolderHistory, Engine.Options.SearchHistory, Engine.Options.ReplaceHistory);
end;

procedure TMainForm.NextMessageActExecute(Sender: TObject);
begin
  MoveListIndex(True);
end;

procedure TMainForm.MoveListIndex(vForward: boolean);

  procedure DblClick(List: TListView);
  begin
    if vForward then
    begin
      if List.ItemIndex < List.Items.Count - 1 then
        List.ItemIndex := List.ItemIndex + 1;
    end
    else
    begin
      if List.ItemIndex > 0 then
        List.ItemIndex := List.ItemIndex - 1;
    end;
    if List.Selected <> nil then
    begin
      List.Selected.Focused := True;
      List.Selected.MakeVisible(False);
    end;
    if Assigned(List.OnDblClick) then
      List.OnDblClick(MessageList);
  end;

begin
  case MessagesTabs.ItemIndex of
    0: DblClick(MessageList);
    1: DblClick(WatchList);
    2: DblClick(SearchList);
  end;
end;

procedure TMainForm.PriorMessageActExecute(Sender: TObject);
begin
  MoveListIndex(False);
end;

procedure TMainForm.OpenPHPiniActExecute(Sender: TObject);
var
  aFile: string;
begin
  if Engine.Options.ConfigFile <> '' then
    aFile := Engine.Options.ConfigFile
  else
    aFile := IncludeTrailingPathDelimiter(GetWinDir) + 'php.ini';
  Engine.Files.LoadFile(aFile, False);
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  OutputEdit.Lines.Clear;
end;

procedure TMainForm.EngineState;
var
  r: integer;
  s: string;
begin
{  if Engine.MacroRecorder.State = msRecording then
    StatePnl.Caption := 'R'
  else}
  StatePnl.Caption := '';
  if Engine.Files.Current <> nil then
  begin
    s := IntToStr(Engine.Files.Current.SynEdit.CaretY) + ':' + IntToStr(Engine.Files.Current.SynEdit.CaretX);
    if Engine.Files.Current.SynEdit.SelAvail then
    begin
      //  r := Engine.Files.Current.SynEdit.ScreenRowToRow(Engine.Files.Current.SynEdit.SelEnd).Line - Engine.Files.Current.SynEdit.CharIndexToRowCol(Engine.Files.Current.SynEdit.SelStart).Line + 1;
      s := s + ' [' + IntToStr(r) + ']';
    end;
    CursorPnl.Caption := s;
  end;
end;

procedure TMainForm.SVNCompareToActExecute(Sender: TObject);
var
  aDialog: TOpenDialog;
begin
  if Engine.Files.Current <> nil then
  begin
    aDialog := TOpenDialog.Create(nil);
    try
      aDialog.Title := 'Open file';
      aDialog.Options := aDialog.Options - [ofAllowMultiSelect];
      aDialog.Filter := Engine.Groups.CreateFilter(Engine.Files.Current.Group);
      //      aDialog.InitialDir := Engine.BrowseFolder;
      if aDialog.Execute then
      begin
        ShellExec(Engine.Options.TortoiseMerge, '/base:"' + aDialog.FileName + '" /mine:' + Engine.Files.Current.Name);
      end;
      Engine.Files.CheckChanged;
    finally
      aDialog.Free;
    end;
  end;
end;

procedure TMainForm.SwitchFocusActExecute(Sender: TObject);
begin
  if FoldersAct.Checked and (Engine.Files.Current <> nil) and (Engine.Files.Current.SynEdit.Focused) then
    FileList.SetFocus
  else if (Engine.Files.Current <> nil) then
    Engine.Files.Current.SynEdit.SetFocus;
end;

procedure TMainForm.QuickSearchNextBtnClick(Sender: TObject);
begin
  if QuickSearchEdit.Text <> '' then
    Engine.Files.Current.SynEdit.SearchReplace(QuickSearchEdit.Text, '', []);
end;

procedure TMainForm.QuickSearchPrevBtnClick(Sender: TObject);
begin
  if QuickSearchEdit.Text <> '' then
    Engine.Files.Current.SynEdit.SearchReplace(QuickSearchEdit.Text, '', [ssoBackwards]);
end;

procedure TMainForm.QuickSearchEditChange(Sender: TObject);
begin
  if QuickSearchEdit.Text <> '' then
    Engine.Files.Current.SynEdit.SearchReplace(QuickSearchEdit.Text, '', [ssoEntireScope]);
  SearchForms.SetTextSearch(QuickSearchEdit.Text);
end;

procedure TMainForm.QuickSearchEditKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #13) and (QuickSearchEdit.Text <> '') then
    Engine.Files.Current.SynEdit.SearchReplace(QuickSearchEdit.Text, '', []);
end;

procedure TMainForm.CloseQuickSearchBtnClick(Sender: TObject);
begin
  QuickFindAct.Execute;
end;

procedure TMainForm.QuickFindActExecute(Sender: TObject);
begin
  QuickFindPnl.Visible := QuickFindAct.Checked;
  if QuickFindPnl.Visible then
  begin
    QuickSearchEdit.Text := SearchForms.GetTextSearch;
    QuickSearchEdit.SetFocus;
    QuickSearchEdit.SelectAll;
  end;
end;

procedure TMainForm.QuickFindActUpdate(Sender: TObject);
begin
  QuickFindAct.Enabled := Engine.Files.Count > 0;
  QuickFindAct.Checked := QuickFindPnl.Visible;
end;

procedure TMainForm.OnSynEditReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var Action: TSynReplaceAction);
begin
  case MessageDlg(Format('Replace this ocurrence of "%s" with "%s"?', [ASearch, AReplace]), mtConfirmation, [mbYes, mbNo, mbAll, mbCancel], 0) of
    mrYes: Action := raReplace;
    mrNo: Action := raSkip;
    mrAll: Action := raReplaceAll;
    mrCancel: Action := raCancel;
  end;
end;

procedure TMainForm.PHPScriptError(Sender: TObject; AText: string; AType: TRunErrorType; AFileName: string; ALineNo: integer);
begin
  Log(0, RunErrorTypes[AType], AText, AFileName, ALineNo);
end;

procedure TMainForm.PHPLogMessage(Sender: TObject; AText: string);
begin
  Log('Message', AText);
end;

procedure TMainForm.Log(Caption, Msg: string);
begin
  Log(0, Caption, Msg, '', 0);
end;

end.

