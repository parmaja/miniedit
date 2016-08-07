unit MainUnit;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
{

SynEdit:
    - Make PaintTransient in TCustomerHighlighter as virtual and called from DoOnPaintTransientEx like calling OnPaintTransient
    - Need a retrieve the Range when call GetHighlighterAttriAtRowColEx
    - "Reset" method in TSynHighlighterAttributes to reassign property to default such as fBackground := fBackgroundDefault it is usfull to load and reload properties from file (XML one)

  MessageList TabStop must be False, can not Maximize window in startup the application (When it is take the focus)

  //i should move Macro Recorder to Engine not in Category
}
interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, MsgBox, contnrs,
  LCLVersion, LMessages, lCLType, LCLIntf, LCLProc, EditorDebugger, FileUtil,
  LazFileUtils, Dialogs, StdCtrls, Math, ComCtrls, ExtCtrls, ImgList, Menus,
  ToolWin, Buttons, FileCtrl, ShellCtrls, ActnList, EditorEngine, mneClasses,
  StdActns, Grids, SynEditHighlighter, SynEdit, IAddons, ntvSplitters,
  SynHighlighterSQL, EditorClasses, ntvImgBtns,
  {$ifdef WINDOWS}
  Windows, //TODO, i hate include it
  {$endif}

  {$ifdef WINDOWS}
  TSVN_SCM, TGIT_SCM,
  {$endif}
  ntvTabSets, EditorRun, Registry, SynEditPlugins,
  synhighlighterunixshellscript, SynHighlighterPas, SynHighlighterMulti,
  mnStreams,
  //Addons
  {$ifdef Windows}
  mneAssociateForm,
  {$endif}
  mnePHPIniForm, mneConsoleForms,
  //end of addons
  mneTendencyOptions, IniFiles, mnFields, simpleipc,
  mnUtils, ntvTabs, ntvPageControls;

type
  TTabSetDragObject = class(TDragObject)
  public
    TabIndex: integer;
  end;

  { TMainForm }

  TMainForm = class(TForm, INotifyEngine)
    Button1: TButton;
    FileModeBtn: TntvImgBtn;
    FileCloseBtn: TntvImgBtn;
    BugSignBtn: TntvImgBtn;
    FolderCloseBtn: TntvImgBtn;
    FolderCloseBtn1: TntvImgBtn;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    RecentFoldersMnu: TMenuItem;
    ShowSpecialCharsAct: TAction;
    FileList: TListView;
    FolderPathLbl: TLabel;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    OutputEdit: TSynEdit;
    FolderPanel: TPanel;
    WatchesGrid: TStringGrid;
    SearchGrid: TStringGrid;
    MessagesGrid: TStringGrid;
    TypeOptionsForMnu: TMenuItem;
    TypesOptionsAct: TAction;
    DBGCompileAct: TAction;
    BrowseTabs: TntvTabSet;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    TypeOptionsMnu: TMenuItem;
    TypeOptionsAct: TAction;
    DeleteAct: TAction;
    FileHeaderPanel: TPanel;
    FileNameLbl: TLabel;
    FileTabs: TntvTabSet;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    CallStackGrid: TStringGrid;
    WorkspaceMnu: TMenuItem;
    RenameAct: TAction;
    FindPreviousAct: TAction;
    MenuItem17: TMenuItem;
    SortByExtensionsAct: TAction;
    SortByNamesAct: TAction;
    FoldersSpl: TntvSplitter;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    NewAsMnu: TMenuItem;
    NewAsAct: TAction;
    MenuItem13: TMenuItem;
    MessagesSpl: TntvSplitter;
    SelectSCMTypeAct: TAction;
    TypePnl: TPanel;
    ProjectTypeMnu: TMenuItem;
    SelectProjectTypeAct: TAction;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    RefreshFilesAct: TAction;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    ShowAllAct: TAction;
    ShowKnownAct: TAction;
    ShowRelatedAct: TAction;
    ApplicationProperties: TApplicationProperties;
    MainMenu: TMainMenu;
    file1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MessagesTabs: TntvPageControl;
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
    GeneralOptionsAct: TAction;
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
    KeywordMnu: TMenuItem;
    About2: TMenuItem;
    DBGRunAct: TAction;
    DBGLintAct: TAction;
    ProjectMnu: TMenuItem;
    Run2: TMenuItem;
    Check1: TMenuItem;
    ToolsMnu: TMenuItem;
    FolderMenu: TPopupMenu;
    FolderOpenAllAct: TAction;
    OpenAll1: TMenuItem;
    FolderOpenAct: TAction;
    FolderOpenAct1: TMenuItem;
    FindAct: TAction;
    FindNextAct: TAction;
    EditMnu: TMenuItem;
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
    ToolButton2: TToolButton;
    MessagesAct: TAction;
    Messages1: TMenuItem;
    FileModeMenu: TPopupMenu;
    UnixMnu: TMenuItem;
    WindowsMnu: TMenuItem;
    MacMnu: TMenuItem;
    BrowserPnl: TPanel;
    BrowserHeaderPanel: TPanel;
    ManageAct: TAction;
    Manage1: TMenuItem;
    OpenFolder2: TMenuItem;
    ProjectOpenFolderAct: TAction;
    OpenIncludeAct: TAction;
    OpenInclude1: TMenuItem;
    CopyAct: TAction;
    CutAct: TAction;
    PasteAct: TAction;
    SelectAllAct: TAction;
    Copy1: TMenuItem;
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
    SCMMnu: TMenuItem;
    CommitMnu: TMenuItem;
    DiffMnu: TMenuItem;
    CommitFileMnu: TMenuItem;
    SCMCommitAct: TAction;
    SCMDiffFileAct: TAction;
    SCMCommitFileAct: TAction;
    SCMUpdateFileAct: TAction;
    SCMRevertAct: TAction;
    SCMUpdateAct: TAction;
    UpdateMnu: TMenuItem;
    UpdateFileMnu: TMenuItem;
    RevertMnu: TMenuItem;
    DBGStartServerAct: TAction;
    DBGStopServerAct: TAction;
    ExecuteMnu: TMenuItem;
    StartServer1: TMenuItem;
    DBGStopServerAct1: TMenuItem;
    DBGStepIntoAct: TAction;
    DBGStepOverAct: TAction;
    DBGResetAct: TAction;
    N12: TMenuItem;
    StepInto1: TMenuItem;
    StepOver1: TMenuItem;
    Reset2: TMenuItem;
    DBGExecuteAct: TAction;
    ResumeMnu: TMenuItem;
    DBGStepOutAct: TAction;
    DBGStepOutAct1: TMenuItem;
    ToolButton6: TToolButton;
    N5: TMenuItem;
    AddWatch1: TMenuItem;
    WatchesPopupMenu: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    N13: TMenuItem;
    Refresh1: TMenuItem;
    ShowValue1: TMenuItem;
    DBGToggleBreakpointAct: TAction;
    N14: TMenuItem;
    DBGToggleBreakpoint1: TMenuItem;
    oggleBreakpoint1: TMenuItem;
    ClientPnl: TPanel;
    EditorsPnl: TPanel;
    DBGRunToCursorAct: TAction;
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
    MessageLabel: TPanel;
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
    OutputPopup: TPopupMenu;
    MenuItem1: TMenuItem;
    StatePnl: TPanel;
    SCMCompareToAct: TAction;
    CompareToMnu: TMenuItem;
    SwitchFocusAct: TAction;
    SwitchFocus1: TMenuItem;
    N16: TMenuItem;
    QuickFindPnl: TPanel;
    Label2: TLabel;
    CloseQuickSearchBtn: TSpeedButton;
    QuickSearchPrevBtn: TBitBtn;
    QuickSearchNextBtn: TBitBtn;
    QuickSearchEdit: TEdit;
    QuickFindAct: TAction;
    QuickSearch: TMenuItem;
    procedure ApplicationPropertiesActivate(Sender: TObject);
    procedure ApplicationPropertiesShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
    procedure BrowseTabsTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
    procedure Button1Click(Sender: TObject);
    procedure CallStackListDblClick(Sender: TObject);
    procedure DBGCompileActExecute(Sender: TObject);
    procedure DeleteActExecute(Sender: TObject);
    procedure EditorPopupMenuPopup(Sender: TObject);
    procedure EditorsPnlClick(Sender: TObject);
    procedure FetchCallStackBtnClick(Sender: TObject);
    procedure FileCloseBtnClick(Sender: TObject);
    procedure FileTabsTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
    procedure FindPreviousActExecute(Sender: TObject);
    procedure FolderCloseBtnClick(Sender: TObject);
    procedure FoldersActExecute(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure IPCServerMessage(Sender: TObject);
    procedure MenuItem22Click(Sender: TObject);
    procedure MenuItem23Click(Sender: TObject);
    procedure MenuItem24Click(Sender: TObject);
    procedure MessagesGridDblClick(Sender: TObject);
    procedure NewAsActExecute(Sender: TObject);
    procedure OpenActExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FileClose1BtnClick(Sender: TObject);
    procedure FileTabsClick(Sender: TObject);
    procedure NextActExecute(Sender: TObject);
    procedure PriorActExecute(Sender: TObject);
    procedure CloseActExecute(Sender: TObject);
    procedure FileListDblClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure SearchGridDblClick(Sender: TObject);
    procedure SearchGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure SelectProjectTypeActExecute(Sender: TObject);
    procedure FileModeBtnClick(Sender: TObject);
    procedure RefreshFilesActExecute(Sender: TObject);
    procedure RenameActExecute(Sender: TObject);
    procedure SaveActExecute(Sender: TObject);
    procedure SaveAllActExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure NewActExecute(Sender: TObject);
    procedure FolderOpenAllActExecute(Sender: TObject);
    procedure FolderOpenActExecute(Sender: TObject);
    procedure FindActExecute(Sender: TObject);
    procedure FindNextActExecute(Sender: TObject);
    procedure FileListKeyPress(Sender: TObject; var Key: char);
    procedure KeywordActExecute(Sender: TObject);
    procedure HelpIndexActExecute(Sender: TObject);
    procedure EditorOptionsActExecute(Sender: TObject);
    procedure SelectSCMTypeActExecute(Sender: TObject);
    procedure GeneralOptionsActExecute(Sender: TObject);
    procedure DBGRunActExecute(Sender: TObject);
    procedure ProjectOptionsActExecute(Sender: TObject);
    procedure NewProjectActExecute(Sender: TObject);
    procedure OpenProjectActExecute(Sender: TObject);
    procedure SaveProjectActExecute(Sender: TObject);
    procedure SelectFileActExecute(Sender: TObject);
    procedure CloseProjectActExecute(Sender: TObject);
    procedure OpenFolderActExecute(Sender: TObject);
    procedure MessagesActExecute(Sender: TObject);
    procedure ShowAllActExecute(Sender: TObject);
    procedure ShowRelatedActExecute(Sender: TObject);
    procedure ShowKnownActExecute(Sender: TObject);
    procedure ShowSpecialCharsActExecute(Sender: TObject);
    procedure SortByExtensionsActExecute(Sender: TObject);
    procedure SortByNamesActExecute(Sender: TObject);
    procedure ToolButton4Click(Sender: TObject);
    procedure ToolsMnuClick(Sender: TObject);
    procedure TypeOptionsActExecute(Sender: TObject);
    procedure TypesOptionsActExecute(Sender: TObject);
    procedure UnixMnuClick(Sender: TObject);
    procedure WatchesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure WindowsMnuClick(Sender: TObject);
    procedure MacMnuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DBGLintActExecute(Sender: TObject);
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
    procedure SCMCommitActExecute(Sender: TObject);
    procedure SCMDiffFileActExecute(Sender: TObject);
    procedure SCMCommitFileActExecute(Sender: TObject);
    procedure SCMUpdateFileActExecute(Sender: TObject);
    procedure SCMUpdateActExecute(Sender: TObject);
    procedure SCMRevertActExecute(Sender: TObject);
    procedure DBGStopServerActUpdate(Sender: TObject);
    procedure DBGStartServerActUpdate(Sender: TObject);
    procedure DBGStartServerActExecute(Sender: TObject);
    procedure DBGStopServerActExecute(Sender: TObject);
    procedure DBGStepOverActExecute(Sender: TObject);
    procedure DBGStepIntoActExecute(Sender: TObject);
    procedure DBGResetActExecute(Sender: TObject);
    procedure DBGExecuteActExecute(Sender: TObject);
    procedure DBGStepOutActExecute(Sender: TObject);
    procedure SaveAsActExecute(Sender: TObject);
    procedure Add1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure DBGToggleBreakpointActExecute(Sender: TObject);
    procedure DBGBreakpointsActExecute(Sender: TObject);
    procedure CopyFileNameActExecute(Sender: TObject);
    procedure DBGAddWatchActExecute(Sender: TObject);
    procedure FolderHomeActExecute(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure Clear1Click(Sender: TObject);
    procedure FindInFilesActExecute(Sender: TObject);
    procedure NextMessageActExecute(Sender: TObject);
    procedure PriorMessageActExecute(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure SCMCompareToActExecute(Sender: TObject);
    procedure SwitchFocusActExecute(Sender: TObject);
    procedure QuickSearchNextBtnClick(Sender: TObject);
    procedure QuickSearchPrevBtnClick(Sender: TObject);
    procedure QuickSearchEditChange(Sender: TObject);
    procedure QuickSearchEditKeyPress(Sender: TObject; var Key: char);
    procedure CloseQuickSearchBtnClick(Sender: TObject);
    procedure QuickFindActExecute(Sender: TObject);
    procedure QuickFindActUpdate(Sender: TObject);
    procedure WorkspaceMnuClick(Sender: TObject);
  private
    FMessages: TEditorMessages;
    FShowFolderFiles: TShowFolderFiles;
    FSortFolderFiles: TSortFolderFiles;
    function CanOpenInclude: boolean;
    procedure CatchErr(Sender: TObject; e: exception);
    procedure ForceForegroundWindow;

    procedure SearchFoundEvent(Index: Integer; FileName: string; const Line: string; LineNo, Column, FoundLength: Integer);
    procedure SetShowFolderFiles(AValue: TShowFolderFiles);
    procedure SetSortFolderFiles(AValue: TSortFolderFiles);
    procedure UpdateFileHeaderPanel;
    procedure UpdateCallStack;
    procedure OptionsChanged;
    function ChooseTendency(var vTendency: TEditorTendency): Boolean;
    function ChooseSCM(var vSCM: TEditorSCM): Boolean;

    procedure EngineChanged;
    procedure UpdateWatches;
    procedure EngineDebug;
    procedure EngineRefresh;
    procedure EngineEdited;
    procedure EngineState;
    procedure ProjectLoaded;
    procedure UpdateFolder;
    procedure ProjectChanged;
    procedure AddMenuItem(AName, ACaption: string; AOnClickEvent: TNotifyEvent; AShortCut: TShortCut = 0);
    procedure UpdateMenu;
    procedure UpdatePanel;
    procedure SetFolder(const Value: string);
    procedure ReopenClick(Sender: TObject);
    procedure ReopenFolderClick(Sender: TObject);
    procedure ReopenProjectClick(Sender: TObject);
    procedure AddWatch(s: string);
    procedure DeleteWatch(s: string);
    procedure EnumRecents;
    procedure EnumRecentFiles;
    procedure EnumRecentFolders;
    procedure EnumRecentProjects;
    function GetCurrentColorText: string;
    function GetFolder: string;
    procedure DeleteCurrentWatch;
    procedure MoveListIndex(vForward: boolean);
  protected
    FProjectFrame: TFrame;
    FOutputBuffer: string; //TODO stupid idea
    FMenuItemsList: TObjectList;
    procedure RunFile;
    procedure CompileFile;
    //
    procedure Log(ACaption, AMsg: string); overload;
    procedure Log(AMsg: string);

    procedure EngineReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
    procedure EditorChangeState(State: TEditorChangeStates);
    procedure EngineMessage(S: string; Temporary: Boolean = False);
    procedure EngineOutput(S: string);
    procedure EngineAction(EngineAction: TEditorAction);
    procedure EngineError(Error: integer; ACaption, Msg, FileName: string; LineNo: integer); overload;

    procedure FollowFolder(vFolder: string; FocusIt: Boolean);
    procedure ShowMessagesList;
    procedure ShowWatchesList;
    procedure LoadAddons;
    property ShowFolderFiles: TShowFolderFiles read FShowFolderFiles write SetShowFolderFiles;
    property SortFolderFiles: TSortFolderFiles read FSortFolderFiles write SetSortFolderFiles;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Folder: string read GetFolder write SetFolder;
    procedure UpdateFoldersPnl;
    procedure UpdateMessagesPnl;
    procedure StopServer; deprecated;
  end;

var
  MainForm: TMainForm;

implementation

uses
  mnXMLUtils, StrUtils, SearchForms, mneProjectOptions, EditorOptions,
  EditorProfiles, mneResources, mneSetups, Clipbrd, ColorUtils,
  SelectFiles, mneSettings, mneConsts,
  SynEditTypes, AboutForms, mneManageRecentsForms, Types,
  mneBreakpoints, SynMacroRecorder,
  SearchInFilesForms, SelectList;

function SortByExt(List: TStringList; Index1, Index: Integer): Integer;
begin
  Result := CompareText(ExtractFileExt(List[Index1]), ExtractFileExt(List[Index]));
  if Result = 0 then
    Result := CompareText(List[Index1], List[Index]);
end;

{$R *.lfm}

{ TMainNotifyEngine }

constructor TMainForm.Create(AOwner: TComponent);
var
  aIniFile: TIniFile;
  lFilePath: string;
  aWorkspace: string;
begin
  inherited;
  FMenuItemsList := TObjectList.Create(True);
  aIniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'setting.ini');
  try
    aWorkspace := aIniFile.ReadString(SysPlatform, 'Workspace', '');
    aWorkspace := IncludeTrailingPathDelimiter(Engine.EnvReplace(aWorkspace));
  finally
    aIniFile.Free;
  end;
  Engine.Workspace := ExpandToPath(aWorkspace, Application.Location);
  Engine.Environment.Add('Workspace=' + aWorkspace);
  //ForceDirectories(Engine.Workspace);

  Engine.Container := EditorsPnl;

  Engine.SetNotifyEngine(Self);

  if (aWorkspace <> '') then
  begin
    Engine.Startup;
  end;
  ShowFolderFiles := Engine.Options.ShowFolderFiles;
  SortFolderFiles := Engine.Options.SortFolderFiles;
  FoldersAct.Checked := Engine.Options.ShowFolder;
  MessagesAct.Checked := Engine.Options.ShowMessages;
  BrowserPnl.Width := Engine.Options.FoldersWidth;
  //MessagesTabs.Height := Engine.Options.MessagesHeight;
  with MessagesTabs, BoundsRect do
    BoundsRect := Rect(Left, Bottom - Engine.Options.MessagesHeight, Right, Bottom);
  MessagesTabs.Visible := False;
  MessagesSpl.Visible := False;
  UpdateFoldersPnl;
  UpdateMessagesPnl;
  // Open any files passed in the command line
  if (ParamCount > 0) and not (SameText(ParamStr(1), '/dde')) then
  begin
    lFilePath := DequoteStr(ParamStr(1));
    Folder := ExtractFilePath(lFilePath);
    // The filename is expanded, if necessary, in EditorEngine.TEditorFiles.InternalOpenFile
    Engine.Files.OpenFile(lFilePath);
  end;
end;

procedure TMainForm.UpdateFoldersPnl;
begin
  FoldersSpl.Visible := FoldersAct.Checked;
  BrowserPnl.Visible := FoldersAct.Checked;
  if FoldersAct.Checked then
    UpdateFolder;
end;

procedure TMainForm.FolderCloseBtnClick(Sender: TObject);
begin
  FoldersAct.Execute;
end;

procedure TMainForm.ApplicationPropertiesShowHint(var HintStr: string; var CanShow: boolean; var HintInfo: THintInfo);
var
  s: string;
begin
  if (Engine.Files.Current <> nil) then
  begin
    CanShow := Engine.Files.Current.GetHint(HintInfo.HintControl, HintInfo.CursorPos, s);

    if CanShow then
    begin
      HintInfo.HintStr := s;
      HintInfo.HideTimeout := 10000;
      HintInfo.ReshowTimeout := 1;
    end;
  end;
end;

procedure TMainForm.BrowseTabsTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
begin
  if NewTab.Index = 0 then
  begin
    FolderPanel.Visible := True;
    if FProjectFrame <> nil then
      FProjectFrame.Visible := False;
  end
  else
  begin
    if FProjectFrame <> nil then
    begin
      FolderPanel.Visible := False;
      FProjectFrame.Visible := True;
    end
    else
      FolderPanel.Visible := True;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  with Engine.Tendencies.Find('Pascal') do
  begin
    Prepare;
    Debug.Start;
  end;
end;

procedure TMainForm.CallStackListDblClick(Sender: TObject);
var
  s: string;
  aLine: integer;
begin
  if CallStackGrid.Row > 0 then
  begin
    Engine.Files.OpenFile(CallStackGrid.Cells[1, CallStackGrid.Row]);
    s := CallStackGrid.Cells[2, CallStackGrid.Row];
    if s <> '' then
    begin
      aLine := StrToIntDef(s, 0);
      if aLine > 0 then
      begin
        with Engine.Files.Current do
        if Control is TCustomSynEdit then
        begin
          (Control as TCustomSynEdit).CaretY := aLine;
          (Control as TCustomSynEdit).CaretX := 0;
          (Control as TCustomSynEdit).SelectLine;
          (Control as TCustomSynEdit).SetFocus;
        end
      end;
    end;
  end;
end;

procedure TMainForm.DBGCompileActExecute(Sender: TObject);
begin
  CompileFile;
end;

procedure TMainForm.DeleteActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
  begin
    if not MsgBox.Msg.No('Are you sure want delete ' + Engine.Files.Current.NakeName) then
      Engine.Files.Current.Delete;
  end;
end;

procedure TMainForm.EditorPopupMenuPopup(Sender: TObject);
begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current is TSourceEditorFile) then
    ShowSpecialCharsAct.Checked := eoShowSpecialChars in (Engine.Files.Current as TSourceEditorFile).SynEdit.Options
  else
    ShowSpecialCharsAct.Checked := False;
end;

procedure TMainForm.EditorsPnlClick(Sender: TObject);
begin

end;

procedure TMainForm.FetchCallStackBtnClick(Sender: TObject);
begin
  if (Engine.Tendency.Debug <> nil) then
  begin
  end;
end;

procedure TMainForm.FileCloseBtnClick(Sender: TObject);
begin
  CloseAct.Execute;
end;

procedure TMainForm.ApplicationPropertiesActivate(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    Engine.Files.CheckChanged;
end;

procedure TMainForm.FileTabsTabSelected(Sender: TObject; OldTab, NewTab: TntvTabItem);
begin
  Engine.Files.SetCurrentIndex(FileTabs.ItemIndex, True);
end;

procedure TMainForm.FindPreviousActExecute(Sender: TObject);
begin
  Engine.Files.FindPrevious;
end;

procedure TMainForm.FoldersActExecute(Sender: TObject);
begin
  UpdateFoldersPnl;
end;

procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  i: integer;
  aFolder: string;
  A: string;
begin
  Engine.BeginUpdate;
  try
    aFolder := '';
    for i := 0 to Length(FileNames) - 1 do
    begin
      A := FileNames[i];
      if DirectoryExistsUTF8(A) and (i = 0) then
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

procedure TMainForm.FormShow(Sender: TObject);
begin
  if (ParamCount = 0) then
  begin
    if Engine.Options.AutoOpenProject then
    begin
      if FileExistsUTF8(Engine.Options.LastProject) then
        Engine.Session.Load(Engine.Options.LastProject)
      else
        Folder := Engine.Options.LastFolder;
    end
    else
        Folder := Engine.Options.LastFolder;
  end;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
end;

procedure TMainForm.ForceForegroundWindow;
{$ifdef windows}
var
  aForeThread, aAppThread: DWORD;
  aProcessID: DWORD;
  {$endif}
begin
  {$ifdef windows}
  aProcessID := 0;
  aForeThread := GetWindowThreadProcessId(GetForegroundWindow(), aProcessID);
  aAppThread := GetCurrentThreadId();

  if (aForeThread <> aAppThread) then
  begin
    AttachThreadInput(aForeThread, aAppThread, True);
    BringWindowToTop(Handle);
    AttachThreadInput(aForeThread, aAppThread, False);
  end
  else
    BringWindowToTop(Handle);
  {$endif}
  BringToFront;
end;

procedure TMainForm.IPCServerMessage(Sender: TObject);
var
  c: integer;
begin
  c := IPCServer.MsgType;
  case c of
    0: ForceForegroundWindow;
    1:
    begin
      if Engine.Files.OpenFile(IPCServer.StringMessage) <> nil then
      begin
        ForceForegroundWindow;
      end;
    end;
  end;
end;

procedure TMainForm.MenuItem22Click(Sender: TObject);
begin
  if Engine.Session.IsOpened then
  begin
    Folder := ExtractFilePath(Engine.Session.Project.FileName);
    Engine.ProcessRecentFolder(Folder);
  end;
end;

procedure TMainForm.MenuItem23Click(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
  begin
    Folder := ExtractFilePath(Engine.Files.Current.Name);
    Engine.ProcessRecentFolder(Folder);
  end;
end;

procedure TMainForm.MenuItem24Click(Sender: TObject);
begin
  if FileList.Selected <> nil then
    Clipbrd.Clipboard.AsText := Folder + FileList.Selected.Caption;
end;

procedure TMainForm.MessagesGridDblClick(Sender: TObject);
var
  s: string;
  aLine: integer;
begin
  if MessagesGrid.Row > 0 then
  begin
    Engine.Files.OpenFile(MessagesGrid.Cells[2, MessagesGrid.Row]);
    s := MessagesGrid.Cells[3, MessagesGrid.Row];
    if s <> '' then
    begin
      aLine := StrToIntDef(s, 0);
      if aLine > 0 then
      with Engine.Files do
      begin
        if (Current <> nil) and (Current.Control is TCustomSynEdit) then
        begin
          (Current.Control as TCustomSynEdit).CaretY := aLine;
          (Current.Control as TCustomSynEdit).CaretX := 0;
          (Current.Control as TCustomSynEdit).SelectLine;
          (Current.Control as TCustomSynEdit).SetFocus;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.NewAsActExecute(Sender: TObject);
var
  G: TFileGroups;
  E: Integer;
begin
  try
    if Engine.Tendency is TDefaultTendency then
      G := Engine.Groups
    else
      G := Engine.Tendency.Groups;
    if ShowSelectList('Select file type', G, [slfUseNameTitle], E) then
      Engine.Files.New(G[E]);
  finally
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
  FileTabs.Items.BeginUpdate;
  try
    FileTabs.Items.Clear;
    for i := 0 to Engine.Files.Count - 1 do
    begin
      if Engine.Files[i].Name = '' then
      begin
        if Engine.Files[i].Group <> nil then
          FileTabs.Items.AddItem(Engine.Files[i].Group.Name, '*' + Engine.Files[i].Group.Name + '*')
        else
          FileTabs.Items.AddItem('', '* No Name *');
      end
      else
        FileTabs.Items.AddItem(ExtractFileName(Engine.Files[i].Name), ExtractFileName(Engine.Files[i].Name));
    end;
  finally
    FileTabs.Items.EndUpdate;
  end;
  FileTabs.Visible := FileTabs.Items.Count > 0;
  if Engine.Files.Current = nil then
    QuickFindPnl.Visible := False;
  if (Engine.Session.IsOpened) then
  begin
    if (Engine.Session.Project.Title <> '') then
      Caption := Engine.Session.Project.Title + ' - ' + sApplicationTitle
    else if (Engine.Session.Project.Name <> '') then
      Caption := Engine.Session.Project.Name + ' - ' + sApplicationTitle
    else
      Caption := 'No Name - ' + sApplicationTitle;
  end
  else
    Caption := sApplicationTitle;
  Application.Title := Caption;
  EnumRecents;
end;

procedure TMainForm.EngineRefresh;
begin
  if Engine.Files.Current <> nil then
  begin
    if Engine.Files.Current.Control <> nil then
      Engine.Files.Current.Control.PopupMenu := EditorPopupMenu;
    FileNameLbl.Caption := Engine.Files.Current.Name;
    FileModeBtn.Caption := Engine.Files.Current.ModeAsText;
    FileModeBtn.Visible := Engine.Files.Current.IsText;
    FileTabs.ItemIndex := Engine.Files.Current.Index;
    if Engine.Files.Current.Name <> '' then
      FileTabs.Items[FileTabs.ItemIndex].Caption := ExtractFileName(Engine.Files.Current.Name);
    if Folder = '' then
      Folder := ExtractFilePath(Engine.Files.Current.Name);
    SaveAct.Enabled := Engine.Files.Current.IsEdited;
    SaveAllAct.Enabled := Engine.GetIsChanged;
  end
  else
  begin
    FileNameLbl.Caption := '';
    FileModeBtn.Caption := '';
    FileModeBtn.Visible := False;
    SaveAct.Enabled := False;
    SaveAllAct.Enabled := Engine.GetIsChanged;
  end;
  //  DebugPnl.Visible := DebugPnl.Caption <> '';
  UpdateMenu;
  UpdateFileHeaderPanel;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  IPCServer.StopServer;
end;

procedure TMainForm.FileClose1BtnClick(Sender: TObject);
begin

end;

procedure TMainForm.FileTabsClick(Sender: TObject);
begin
  Engine.Files.SetCurrentIndex(FileTabs.ItemIndex, True);
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
      Engine.BrowseFolder := ExtractFilePath(IncludeTrailingPathDelimiter(ExpandFileName(Value)))
    else
      Engine.BrowseFolder := IncludeTrailingPathDelimiter(ExpandFileName(Value));
    if FoldersAct.Checked then
      UpdateFolder;
  end;
end;

procedure TMainForm.FileListDblClick(Sender: TObject);
begin
  FolderOpenAct.Execute;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  mr: TmsgChoice;
begin
  if Engine.Files.GetEditedCount > 0 then
  begin
    mr := MsgBox.Msg.YesNoCancel('There a files changed but not saved'#13'Save it all?');
    if mr = msgcCancel then
      CanClose := False
    else if mr = msgcYes then
      Engine.Files.SaveAll;
  end;

  if CanClose and (Engine.Session.IsOpened) then
  begin
    if (Engine.Session.Project.FileName = '') then
    begin
      mr := MsgBox.Msg.YesNoCancel('Save project ' + Engine.Session.Project.Name + ' before close?');
      if mr = msgcCancel then
        CanClose := False
      else if mr = msgcYes then
        Engine.Session.Save;
    end
    else
      Engine.Session.Save;
  end;
end;

procedure TMainForm.SearchGridDblClick(Sender: TObject);
var
  s: string;
  aLine, c, l: integer;
begin
  if SearchGrid.Row > 0 then
  begin
    Engine.Files.OpenFile(SearchGrid.Cells[1, SearchGrid.Row]);
    s := SearchGrid.Cells[2, SearchGrid.Row];
    if s <> '' then
    begin
      aLine := StrToIntDef(s, 0);
      if aLine > 0 then
      begin
        with Engine.Files.Current do
        if Control is TCustomSynEdit then
        begin
          l := ptrint(SearchGrid.Rows[SearchGrid.Row].Objects[0]);
          c := ptrint(SearchGrid.Rows[SearchGrid.Row].Objects[1]);
          (Control as TCustomSynEdit).CaretY := aLine;
          if l > 0 then
          begin
            with (Control as TCustomSynEdit) do
            begin
              CaretX := c;
              BlockBegin := Point(c, aLine);
              BlockEnd := Point(c + l, aLine);
            end;
          end
          else
            (Control as TCustomSynEdit).CaretX := 0;
          (Control as TCustomSynEdit).SetFocus;
        end;
      end;
    end;
  end;
end;

procedure TMainForm.SearchGridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  c, l: integer;
  h, w: integer;
  s, bf, md, af: string;
  aCanvas: TCanvas;
begin
  if (aRow > 0) then
  begin
    aCanvas := SearchGrid.Canvas;
    if gdSelected in aState then
    begin
      aCanvas.Font.Color := clHighlightText;
      aCanvas.Brush.Color := clHighlight;
    end
    else
    begin
      aCanvas.Font.Color := clWindowText;
      aCanvas.Brush.Color := clWindow;
    end;
    s := SearchGrid.Cells[aCol, aRow];
    w := aRect.Left + 2;
    h := aCanvas.TextHeight(s);

    if (aCol < 3) then
    begin
      aCanvas.TextOut(w, aRect.Top, s);
    end
    else
    begin
      l := ptrint(SearchGrid.Rows[SearchGrid.Row].Objects[0]);
      c := ptrint(SearchGrid.Rows[SearchGrid.Row].Objects[1]);
      bf := Copy(s, 1, c - 1);
      md := Copy(s, c, l);
      af := Copy(s, c + l, MaxInt);
      aRect.Top := aRect.Top + ((aRect.Bottom - aRect.Top - h) div 2);
      //aCanvas.FillRect(aRect);
      aCanvas.Font.Style := [];
      aCanvas.TextOut(w, aRect.Top, bf);
      w := w + aCanvas.TextWidth(bf);
      aCanvas.Font.Style := [fsBold];
      aCanvas.TextOut(w, aRect.Top, md);
      w := w + aCanvas.TextWidth(md);
      aCanvas.Font.Style := [];
      aCanvas.TextOut(w, aRect.Top, af);
      aCanvas.Refresh;
    end;
  end;
end;

procedure TMainForm.SelectProjectTypeActExecute(Sender: TObject);
var
  lTendency: TEditorTendency;
begin
  if Engine.Session.IsOpened then
  begin
    if not MsgBox.Msg.No('You cannot change the type without losing project setting, are you sure?') then
    begin
      lTendency := Engine.Session.Project.Tendency;
      if ChooseTendency(lTendency) then
        Engine.Session.Project.TendencyName := lTendency.Name;
    end;
  end
end;

procedure TMainForm.FileModeBtnClick(Sender: TObject);
var
  Pt: TPoint;
begin
  Pt.X := FileModeBtn.BoundsRect.Left;
  Pt.Y := FileModeBtn.BoundsRect.Bottom;
  Pt := FileModeBtn.Parent.ClientToScreen(Pt);
  FileModeBtn.PopupMenu.Popup(Pt.X, Pt.Y);
end;

procedure TMainForm.RefreshFilesActExecute(Sender: TObject);
begin
  UpdateFolder;
end;

procedure TMainForm.RenameActExecute(Sender: TObject);
var
  s: string;
begin
  if Engine.Files.Current <> nil then
  begin
    s := Engine.Files.Current.NakeName;
    if MsgBox.Msg.Input(s, 'Please enter new name for ' + Engine.Files.Current.NakeName) then
      Engine.Files.Current.Rename(s);
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
    SaveAct.Enabled := Engine.Files.Current.IsEdited;
    if Engine.Files.Current.IsEdited then
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
  Engine.Session.Save;
  Engine.Files.SaveAll;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if WindowState <> wsMaximized then
    Engine.Options.BoundRect := BoundsRect;

  Engine.Options.WindowMaxmized := WindowState = wsMaximized;


  Engine.Options.ShowFolder := FoldersAct.Checked;
  Engine.Options.ShowFolderFiles := ShowFolderFiles;
  Engine.Options.SortFolderFiles := SortFolderFiles;
  Engine.Options.ShowMessages := MessagesAct.Checked;
  Engine.Options.MessagesHeight := MessagesTabs.Height;
  Engine.Options.FoldersWidth := BrowserPnl.Width;
  if Engine.Session.IsOpened then
    Engine.Options.LastProject := Engine.Session.Project.FileName
  else
      Engine.Options.LastProject := '';
  Engine.Options.LastFolder := Engine.BrowseFolder;
  Engine.Session.Close;
  Engine.RemoveNotifyEngine(Self);

  //StopServer; TODO: delete it
  Engine.Shutdown;
  //HtmlHelp(Application.Handle, nil, HH_CLOSE_ALL, 0);
end;

procedure TMainForm.NewActExecute(Sender: TObject);
begin
  Engine.Files.New;
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
    if FileList.Selected.Data = nil then
      FollowFolder(FileList.Selected.Caption, FileList.Focused)
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

procedure TMainForm.FileListKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    FolderOpenAct.Execute;
end;

procedure TMainForm.KeywordActExecute(Sender: TObject);
begin
  //  DoHtmlHelp;
end;

procedure TMainForm.HelpIndexActExecute(Sender: TObject);
begin
end;

procedure TMainForm.EditorOptionsActExecute(Sender: TObject);
begin
  Engine.Options.Show;
end;

procedure TMainForm.SelectSCMTypeActExecute(Sender: TObject);
var
  lSCM: TEditorSCM;
begin
  if Engine.Session.IsOpened then
  begin
    lSCM := Engine.Session.Project.SCM;
    if ChooseSCM(lSCM) then
      Engine.Session.Project.SetSCMClass(lSCM);
  end
  else
  begin
    lSCM := Engine.DefaultSCM;
    if ChooseSCM(lSCM) then
      Engine.DefaultSCM := lSCM;
  end;
end;

procedure TMainForm.GeneralOptionsActExecute(Sender: TObject);
begin
  ShowSettingForm(Engine);
end;

procedure TMainForm.DBGRunActExecute(Sender: TObject);
begin
  RunFile;
end;

procedure TMainForm.ProjectOptionsActExecute(Sender: TObject);
begin
  if Engine.Session.IsOpened then
  begin
    if ShowProjectForm(Engine.Session.Project) then
      Engine.Session.Changed;
  end;
end;

procedure TMainForm.NewProjectActExecute(Sender: TObject);
var
  aProject: TEditorProject;
  aTendency: TEditorTendency;
begin
  Engine.BeginUpdate;
  try
    if (not Engine.Session.IsOpened) or (Engine.Session.Save) then
    begin
      aTendency := nil;
      if ChooseTendency(aTendency) then
      begin
        aProject := Engine.Session.New(aTendency);
        if ShowProjectForm(aProject) then
        begin
          Engine.Session.Project := aProject;
        end
        else
          aProject.Free;
      end;
    end;
  finally
    Engine.EndUpdate;
  end;
end;

procedure TMainForm.OpenProjectActExecute(Sender: TObject);
begin
  Engine.Session.Open;
end;

procedure TMainForm.SaveProjectActExecute(Sender: TObject);
begin
  Engine.Session.Save;
end;

procedure TMainForm.SelectFileActExecute(Sender: TObject);
var
  aFileName: string;
begin
  if ShowSelectFile(Engine.Session.GetRoot, aFileName) then
    Engine.Files.OpenFile(aFileName);
end;

procedure TMainForm.EnumRecentFiles;
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
    aMenuItem.OnClick := @ReopenClick;
    ReopenMnu.Add(aMenuItem);
  end;
end;

procedure TMainForm.EnumRecentFolders;
var
  i, c: integer;
  aMenuItem: TMenuItem;
begin
  RecentFoldersMnu.Clear;
  c := Engine.Options.RecentFolders.Count;
  if c > 10 then
    c := 10;
  for i := 0 to c - 1 do
  begin
    aMenuItem := TMenuItem.Create(Self);
    aMenuItem.Caption := Engine.Options.RecentFolders[i];
    aMenuItem.Hint := aMenuItem.Caption;
    aMenuItem.OnClick := @ReopenFolderClick;
    RecentFoldersMnu.Add(aMenuItem);
  end;
end;

procedure TMainForm.ReopenClick(Sender: TObject);
var
  aFile: string;
begin
  if Sender is TMenuItem then
  begin
    aFile := (Sender as TMenuItem).Caption;
    if Engine.Session.IsOpened then
      aFile := ExpandToPath(aFile, Engine.Session.Project.RootDir);
    Engine.Files.OpenFile(aFile);
  end;
end;

procedure TMainForm.ReopenFolderClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    Folder := (Sender as TMenuItem).Caption;
end;

procedure TMainForm.ReopenProjectClick(Sender: TObject);
var
  aFile: string;
begin
  if Sender is TMenuItem then
  begin
    aFile := (Sender as TMenuItem).Caption;
    Engine.Session.Load(aFile);
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
    aMenuItem.OnClick := @ReopenProjectClick;
    ReopenProjectMnu.Add(aMenuItem);
  end;
end;

procedure TMainForm.CloseProjectActExecute(Sender: TObject);
begin
  Engine.Session.Close;
end;

procedure TMainForm.OpenFolderActExecute(Sender: TObject);
begin
(*
{$ifdef WINDOWS}
  if Engine.Files.Current <> nil then
    ShellExecute(0, 'open', 'explorer.exe', PChar('/select,"' + Engine.Files.Current.Name + '"'), nil, SW_SHOW);
{$endif}
*)
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

procedure TMainForm.ShowAllActExecute(Sender: TObject);
begin
  ShowFolderFiles := sffAll;
end;

procedure TMainForm.ShowRelatedActExecute(Sender: TObject);
begin
  ShowFolderFiles := sffRelated;
end;

procedure TMainForm.ShowKnownActExecute(Sender: TObject);
begin
  ShowFolderFiles := sffKnown;
end;

procedure TMainForm.ShowSpecialCharsActExecute(Sender: TObject);
begin
   if (Engine.Files.Current <> nil) and (Engine.Files.Current is TSourceEditorFile) then
   begin
     ShowSpecialCharsAct.Checked := not ShowSpecialCharsAct.Checked;
     if ShowSpecialCharsAct.Checked then
       (Engine.Files.Current as TSourceEditorFile).SynEdit.Options := (Engine.Files.Current as TSourceEditorFile).SynEdit.Options + [eoShowSpecialChars]
     else
       (Engine.Files.Current as TSourceEditorFile).SynEdit.Options := (Engine.Files.Current as TSourceEditorFile).SynEdit.Options - [eoShowSpecialChars];
   end;
end;

procedure TMainForm.SortByExtensionsActExecute(Sender: TObject);
begin
  SortFolderFiles := srtfByExt;
end;

procedure TMainForm.SortByNamesActExecute(Sender: TObject);
begin
  SortFolderFiles := srtfByNames;
end;

procedure TMainForm.ToolButton4Click(Sender: TObject);
begin
end;

procedure TMainForm.ToolsMnuClick(Sender: TObject);
begin
  //
end;

procedure TMainForm.TypeOptionsActExecute(Sender: TObject);
begin
  ShowTendencyForm(Engine.Tendency);
end;

procedure TMainForm.TypesOptionsActExecute(Sender: TObject);
var
  lTendency: TEditorTendency;
begin
  if Engine.Files.Current <> nil then
    lTendency := Engine.Files.Current.Tendency
  else
    lTendency := nil;
  if ChooseTendency(lTendency) then
    if (lTendency <> nil) then
      ShowTendencyForm(lTendency);
end;

procedure TMainForm.UnixMnuClick(Sender: TObject);
begin
  Engine.Files.Current.Mode := efmUnix;
end;

procedure TMainForm.WatchesGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      VK_DELETE: DeleteCurrentWatch;
    end;
  end;
end;

procedure TMainForm.WindowsMnuClick(Sender: TObject);
begin
  Engine.Files.Current.Mode := efmWindows;
end;

procedure TMainForm.MacMnuClick(Sender: TObject);
begin
  Engine.Files.Current.Mode := efmMac;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.OnException := @CatchErr;

  FMessages := Engine.MessagesList.GetMessages('Messages');
  EngineChanged;
  EngineRefresh;
  EngineDebug;
  FileTabs.ShowBorder := False;
  IPCServer.ServerID := sApplicationID;
  IPCServer.StartServer;
  LoadAddons;

  if Engine.Options.BoundRect.Right - Engine.Options.BoundRect.Left > 10 then   //Safe width
    BoundsRect := Engine.Options.BoundRect;

  if Engine.Options.WindowMaxmized then
    WindowState := wsMaximized;
  //Color := clSkyBlue; for test propose
end;

procedure TMainForm.DBGLintActExecute(Sender: TObject);
begin
  if (Engine.Files.Current <> nil) and (fgkExecutable in Engine.Files.Current.Group.Kind) then
  begin
    SaveAllAct.Execute;
    //    RunInternal(True);
  end;
end;

procedure TMainForm.EngineError(Error: integer; ACaption, Msg, FileName: string; LineNo: integer);
begin
  MessagesGrid.RowCount := MessagesGrid.RowCount + 1;
  MessagesGrid.Cells[1, MessagesGrid.RowCount - 1] := IntToStr(Error);
  MessagesGrid.Cells[2, MessagesGrid.RowCount - 1] := msg;
  MessagesGrid.Cells[3, MessagesGrid.RowCount - 1] := FileName;
  MessagesGrid.Cells[4, MessagesGrid.RowCount - 1] := IntToStr(LineNo);
end;

procedure TMainForm.AboutActExecute(Sender: TObject);
begin
  with TAboutForm.Create(Application) do
  begin
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.ProjectChanged;
var
  b: boolean;
begin
  b := Engine.Session.IsOpened;
  ProjectOptionsAct.Enabled := b;
  SaveProjectAct.Enabled := b;
  SaveAsProjectAct.Enabled := b;
  AddFileToProjectAct.Enabled := b;
  CloseProjectAct.Enabled := b;
  ProjectOpenFolderAct.Enabled := b;
  SCMMnu.Visible := Engine.SCM <> nil;
  if Engine.SCM <> nil then
    SCMMnu.Caption := Engine.SCM.Name;
  TypePnl.Caption := Engine.Tendency.Name;

  UpdateMenu;
  UpdatePanel;
end;

procedure TMainForm.AddMenuItem(AName, ACaption: string; AOnClickEvent: TNotifyEvent; AShortCut: TShortCut);
var
  MenuItem: TMenuItem;
begin
  if FMenuItemsList.Count = 0 then
  begin
    MenuItem := TMenuItem.Create(MainMenu);
    EditMnu.Add(MenuItem);
    MenuItem.Name := 'FILE_EDIT_MENUITEM_SEP';
    MenuItem.Caption := '-';
    FMenuItemsList.Add(MenuItem);

    MenuItem := TMenuItem.Create(EditorPopupMenu);
    EditorPopupMenu.Items.Add(MenuItem);
    MenuItem.Name := 'FILE_EDIT_POPUPMENUITEM_SEP';
    MenuItem.Caption := '-';
    FMenuItemsList.Add(MenuItem);
  end;
  MenuItem := TMenuItem.Create(MainMenu);
  EditMnu.Add(MenuItem);
  MenuItem.Name := 'FILE_EDIT_MENUITEM__' + AName;
  MenuItem.Caption := ACaption;
  MenuItem.OnClick := AOnClickEvent;
  MenuItem.ShortCut := AShortCut;
  FMenuItemsList.Add(MenuItem);

  MenuItem := TMenuItem.Create(EditorPopupMenu);
  EditorPopupMenu.Items.Add(MenuItem);
  MenuItem.Name := 'FILE_EDIT_POPUPMENUITEM__' + AName;
  MenuItem.Caption := ACaption;
  MenuItem.OnClick := AOnClickEvent;
  //MenuItem.ShortCut := AShortCut; //No ShortCut for popup, no dublicate
  FMenuItemsList.Add(MenuItem);
end;

procedure TMainForm.UpdateMenu;
var
  aTendency: TEditorTendency;
begin
  aTendency := Engine.Tendency;

  if Engine.Files.Current <> nil then
  begin
    FMenuItemsList.Clear;
    Engine.Files.Current.Group.Category.EnumMenuItems(@AddMenuItem);
  end;

  with aTendency do
  begin
    //ExecuteMnu.Visible := capRun in Engine.Tendency.Capabilities;

    DBGRunAct.Enabled := capRun in Capabilities;
    DBGCompileAct.Visible := capCompile in Capabilities;
    DBGExecuteAct.Enabled := capRun in Capabilities;
    DBGResetAct.Enabled := capRun in Capabilities;
    DBGLintAct.Enabled := capLint in Capabilities;

    DBGStartServerAct.Enabled := (capDebugServer in Capabilities) and (aTendency.Debug <> nil) and (not aTendency.Debug.Active);
    DBGStopServerAct.Enabled := (capDebugServer in Capabilities) and (aTendency.Debug <> nil) and (aTendency.Debug.Active);

    DBGAddWatchAct.Enabled := capTrace in Capabilities;
    DBGBreakpointsAct.Enabled := capTrace in Capabilities;
    DBGToggleBreakpointAct.Enabled := capTrace in Capabilities;

    MessagesTabs.PageItem[WatchesGrid].Visible := capDebug in Capabilities;
    MessagesTabs.PageItem[CallStackGrid].Visible := capTrace in Capabilities;
    MessagesTabs.PageItem[MessagesGrid].Visible := capErrors in Capabilities;

    DBGStepOverAct.Enabled := capTrace in Capabilities;
    DBGStepIntoAct.Enabled := capTrace in Capabilities;
    DBGStepOutAct.Enabled := capTrace in Capabilities;
    DBGRunToCursorAct.Enabled := capTrace in Capabilities;
  end;
end;

procedure TMainForm.UpdatePanel;
  procedure FreeFrame;
  begin
{    if (FProjectFrame <> nil) and Supports(FProjectFrame, IEditorOptions) then
      (FProjectFrame as IEditorOptions).Apply;} //Project is already freed we cant apply
    FreeAndNil(FProjectFrame);
  end;
begin
  if Engine.Session.IsOpened then
  begin
    if (FProjectFrame = nil) or ((FProjectFrame as IEditorProjectFrame).Project <> Engine.Session.Project) then
    begin
      if (FProjectFrame <> nil) then
        FreeFrame;
      Engine.Session.Project.Options.CreateProjectPanel(Self, Engine.Session.Project, FProjectFrame);
      if FProjectFrame <> nil then
      begin
        FProjectFrame.Parent := BrowserPnl;
        FProjectFrame.Align := alClient;
        FProjectFrame.Visible := False;
        if Supports(FProjectFrame, IEditorOptions) then
          (FProjectFrame as IEditorOptions).Retrieve;
      end;
    end;
  end
  else
    FreeFrame;
  BrowseTabs.Items[1].Visible := FProjectFrame <> nil;
end;

procedure TMainForm.SaveAsProjectActExecute(Sender: TObject);
begin
  if Engine.Session.IsOpened then
    Engine.Session.SaveAs;
end;

procedure TMainForm.ProjectOpenFolderActExecute(Sender: TObject);
begin
{  if Engine.Session.IsOpened then
    ShellExecute(0, 'open', 'explorer.exe', PChar('/select,"' + Engine.Session.Project.FileName + '"'), nil, SW_SHOW);}
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
begin
  if Engine.Files.Current <> nil then
    Engine.Files.Current.OpenInclude;
end;

procedure TMainForm.CopyActUpdate(Sender: TObject);
begin
  CopyAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.CanCopy;
end;

procedure TMainForm.PasteActUpdate(Sender: TObject);
begin
  PasteAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.CanPaste;
end;

procedure TMainForm.CutActUpdate(Sender: TObject);
begin
  CutAct.Enabled := (Engine.Files.Current <> nil) and Engine.Files.Current.CanCopy;
end;

procedure TMainForm.SelectAllActUpdate(Sender: TObject);
begin
  SelectAllAct.Enabled := (Engine.Files.Current <> nil);
end;

procedure TMainForm.PasteActExecute(Sender: TObject);
begin
  Engine.Files.Current.Paste;
end;

procedure TMainForm.CopyActExecute(Sender: TObject);
begin
  Engine.Files.Current.CanCopy;
end;

procedure TMainForm.CutActExecute(Sender: TObject);
begin
  Engine.Files.Current.Cut;
end;

procedure TMainForm.SelectAllActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.Files.Current.SelectAll;
end;

function TMainForm.CanOpenInclude: boolean;
begin
  Result := (Engine.Files.Current <> nil) and Engine.Files.Current.CanOpenInclude;
end;

procedure TMainForm.SetShowFolderFiles(AValue: TShowFolderFiles);
begin
  if FShowFolderFiles =AValue then exit;
  FShowFolderFiles :=AValue;
  if FoldersAct.Checked then
    UpdateFolder;
  case FShowFolderFiles of
    sffRelated: ShowRelatedAct.Checked := True;
    sffKnown: ShowKnownAct.Checked := True;
    sffAll: ShowAllAct.Checked := True;
  end;
end;

procedure TMainForm.SetSortFolderFiles(AValue: TSortFolderFiles);
begin
  if FSortFolderFiles =AValue then Exit;
  FSortFolderFiles :=AValue;
  if FoldersAct.Checked then
    UpdateFolder;
  case FSortFolderFiles of
    srtfByNames: SortByNamesAct.Checked := True;
    srtfByExt: SortByExtensionsAct.Checked := True;
  end;
end;

procedure TMainForm.OpenIncludeActUpdate(Sender: TObject);
begin
  OpenIncludeAct.Enabled := CanOpenInclude;
end;

procedure TMainForm.GotoLineActUpdate(Sender: TObject);
begin
  GotoLineAct.Enabled := (Engine.Files.Current <> nil) and (Engine.Files.Current is ITextEditor);
end;

procedure TMainForm.GotoLineActExecute(Sender: TObject);
begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current is ITextEditor) then
    Engine.Files.Current.GotoLine;
end;

procedure TMainForm.UpdateFolder;
var
  r: integer;
  All: Boolean;
  SearchRec: TSearchRec;
  aItem: TListItem;
  AExtensions: TStringList;

  function MatchExtension(vExtension: string): boolean;
  begin
    if not All then
    begin
      if LeftStr(vExtension, 1) = '.' then //that correct if some one added dot to the first char of extension
        vExtension := Copy(vExtension, 2, MaxInt);
      Result := AExtensions.IndexOf(vExtension) >= 0;
    end
    else
      Result := True;
  end;

var
  aFiles: TStringList;
begin
  FolderPathLbl.Caption := Folder;
  FolderPathLbl.Hint := Folder;
  FileList.Items.BeginUpdate;
  try
    FileList.Clear;

    All := False;
    AExtensions := TStringList.Create;
    try
      case ShowFolderFiles of
        sffRelated: Engine.Tendency.Groups.EnumExtensions(AExtensions);
        sffKnown: Engine.Groups.EnumExtensions(AExtensions);
        sffAll: All := True;
      end;
      AExtensions.Add('mne-project');

      aFiles := TStringList.Create;
      try
        //Folders
        if (Folder <> '') and DirectoryExistsUTF8(Folder) then
        begin
          aFiles.Clear;
          r := FindFirstUTF8(Folder + '*.*', faAnyFile or faDirectory, SearchRec);
          while r = 0 do
          begin
            if (SearchRec.Name <> '.') then
            begin
              if (SearchRec.Attr and faDirectory) <> 0 then
                aFiles.Add(SearchRec.Name);
            end;
            r := FindNextUTF8(SearchRec);
          end;
          FindCloseUTF8(SearchRec);

          aFiles.Sort;

          for r := 0 to aFiles.Count -1 do
          begin
            aItem := FileList.Items.Add;
            aItem.Caption := aFiles[r];
            aItem.Data := nil;
            aItem.ImageIndex := 0;
          end;

          //Files
          aFiles.Clear;
          r := FindFirstUTF8(Folder + '*.*', faAnyFile, SearchRec);
          while r = 0 do
          begin
            //if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
            if (SearchRec.Attr and faDirectory) = 0 then
            begin
              if MatchExtension(ExtractFileExt(SearchRec.Name)) then
                aFiles.Add(SearchRec.Name);
            end;
            r := FindNextUTF8(SearchRec);
          end;
          FindCloseUTF8(SearchRec);

          if SortFolderFiles  = srtfByNames then
            aFiles.Sort
          else
            aFiles.CustomSort(@SortByExt);

          for r := 0 to aFiles.Count -1 do
          begin
            aItem := FileList.Items.Add;
            aItem.Caption := aFiles[r];
            aItem.Data := Pointer(1);
            aItem.ImageIndex := EditorResource.GetFileImageIndex(aFiles[r]);
          end;
        end;
      finally
        aFiles.Free;
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
    Engine.ProcessRecentFolder(Folder);
  end;
end;

procedure TMainForm.EditorChangeState(State: TEditorChangeStates);
begin
  if ecsFolder in State then
    UpdateFolder;
  if ecsChanged in State then
    EngineChanged;
  if ecsRefresh in State then
    EngineRefresh;
  if ecsDebug in State then
    EngineDebug;
  if ecsShow in State then
    ForceForegroundWindow;
  if ecsEdit in State then
    EngineEdited;
  if ecsProject in State then
    ProjectChanged;
  if ecsProjectLoaded in State then
    ProjectLoaded;
  if ecsRecents in State then
    EnumRecents;
  if ecsState in State then
    EngineState;
  if ecsOptions in State then
    OptionsChanged;
end;

procedure TMainForm.EngineMessage(S: string; Temporary: Boolean);
begin
  MessageLabel.Caption := S;
  StatusTimer.Enabled := Temporary;
end;

function TMainForm.ChooseTendency(var vTendency: TEditorTendency): Boolean;
var
  aName: string;
begin
  if (vTendency <> nil) then
    aName := vTendency.Name
  else
    aName := '';
  Result := ShowSelectList('Select project type', Engine.Tendencies, [], aName); //slfIncludeNone
  if Result then
    vTendency := Engine.Tendencies.Find(aName);
end;

function TMainForm.ChooseSCM(var vSCM: TEditorSCM): Boolean;
var
  aName: string;
begin
  if (vSCM <> nil) then
    aName := vSCM.Name
  else
    aName := '';
  Result := ShowSelectList('Select SCM type', Engine.SourceManagements, [slfIncludeNone], aName);
  vSCM := Engine.SourceManagements.Find(aName);
end;

procedure TMainForm.ProjectLoaded;
begin
  Folder := Engine.GetRoot;
end;

procedure TMainForm.ReplaceActExecute(Sender: TObject);
begin
  Engine.Files.Replace;
end;

destructor TMainForm.Destroy;
begin
  FreeAndNil(FMenuItemsList);
  Application.OnException := nil;
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
        FollowFolder('..', FileList.Focused);
      end;
    end;
  end;
end;

procedure TMainForm.FollowFolder(vFolder: string; FocusIt: Boolean);
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
    if FocusIt then
      FileList.SetFocus;
  end;
end;

procedure TMainForm.FileFolder1Click(Sender: TObject);
begin
  if FileList.Selected <> nil then
  begin
    //    ShellExecute(0, 'open', 'explorer.exe', PChar('/select,"' + Folder + FileList.Selected.Caption + '"'), nil, SW_SHOW);
  end;
end;

procedure TMainForm.OpenColorActUpdate(Sender: TObject);
begin
  OpenColorAct.Enabled := GetCurrentColorText <> '';
end;

function TMainForm.GetCurrentColorText: string;
begin
  Result := '';
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.Control is TCustomSynEdit) then
  begin
    Result := Trim(GetWordAtRowColEx((Engine.Files.Current.Control as TCustomSynEdit), (Engine.Files.Current.Control as TCustomSynEdit).CaretXY, TSynWordBreakChars - ['#'], False));
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
        aIsUpper := UpperCase(aWord[i]) = aWord[i];//todo IsUpper
        break;
      end;
    end;
  end;

begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.Control is TCustomSynEdit) then
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
          GetWordAtRowColEx((Engine.Files.Current.Control as TCustomSynEdit), (Engine.Files.Current.Control as TCustomSynEdit).CaretXY, TSynWordBreakChars - ['#'], True);
          if aIsUpper then
            aWord := UpperCase(aWord)
          else
            aWord := LowerCase(aWord);
          (Engine.Files.Current.Control as TCustomSynEdit).SelText := aWord;
        end;
      finally
        aDialog.Free;
      end;
    end;
  end;
end;

procedure TMainForm.StopServer;
begin
  DBGStopServerAct.Execute;
end;

procedure TMainForm.EngineAction(EngineAction: TEditorAction);
begin
  case EngineAction of
    eaClearOutput :
    begin
      FOutputBuffer := '';
      OutputEdit.Lines.Clear;
    end;
  end;
end;

procedure TMainForm.SCMCommitActExecute(Sender: TObject);
begin
  Engine.SCM.CommitDirectory(Folder);
end;

procedure TMainForm.SCMDiffFileActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.SCM.DiffFile(Engine.Files.Current.Name);
end;

procedure TMainForm.SCMCommitFileActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.SCM.CommitFile(Engine.Files.Current.Name);
end;

procedure TMainForm.SCMUpdateFileActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.SCM.UpdateFile(Engine.Files.Current.Name);
end;

procedure TMainForm.SCMUpdateActExecute(Sender: TObject);
begin
  Engine.SCM.UpdateDirectory(Folder);
end;

procedure TMainForm.SCMRevertActExecute(Sender: TObject);
begin
  Engine.SCM.RevertDirectory(Folder);
end;

procedure TMainForm.DBGStopServerActUpdate(Sender: TObject);
begin
  DBGStopServerAct.Enabled := (Engine.Tendency.Debug <> nil) and Engine.Tendency.Debug.Active;
end;

procedure TMainForm.DBGStartServerActUpdate(Sender: TObject);
begin
  DBGStartServerAct.Enabled := (Engine.Tendency.Debug <> nil) and not Engine.Tendency.Debug.Active;
end;

procedure TMainForm.DBGStartServerActExecute(Sender: TObject);
begin
  if Engine.Tendency.Debug <> nil then
  begin
    DBGStartServerAct.Checked := not DBGStartServerAct.Checked;
    Engine.Tendency.Debug.Active := DBGStartServerAct.Checked;
  end
  else
    DBGStartServerAct.Checked := False;
end;

procedure TMainForm.DBGStopServerActExecute(Sender: TObject);
begin
  if Engine.Tendency.Debug <> nil then
    Engine.Tendency.Debug.Action(dbaStopServer);
end;

procedure TMainForm.DBGStepOverActExecute(Sender: TObject);
begin
  if Engine.Tendency.Debug <> nil then
    if Engine.Tendency.Debug.Running then
      Engine.Tendency.Debug.Action(dbaStepOver);
end;

procedure TMainForm.DBGStepIntoActExecute(Sender: TObject);
begin
  if Engine.Tendency.Debug <> nil then
    if Engine.Tendency.Debug.Running then
      Engine.Tendency.Debug.Action(dbaStepInto);
end;


procedure TMainForm.DBGResetActExecute(Sender: TObject);
begin
  if Engine.Tendency.Debug <> nil then
    Engine.Tendency.Debug.Action(dbaReset)
  else
    Engine.Session.Run.Stop;
end;

procedure TMainForm.DBGExecuteActExecute(Sender: TObject);
begin
  if Engine.Files.Current <> nil then
    Engine.Files.Current.Tendency.Run([rnaExecute])
  else
    Engine.Tendency.Run([rnaExecute]);
end;

procedure TMainForm.DBGStepOutActExecute(Sender: TObject);
begin
  if Engine.Tendency.Debug <> nil then
    Engine.Tendency.Debug.Action(dbaStepOut);
end;

function TMainForm.GetFolder: string;
begin
  Result := Engine.BrowseFolder;
end;

procedure TMainForm.SaveAsActExecute(Sender: TObject);
begin
  Engine.Files.SaveAs;
end;

procedure TMainForm.EngineDebug;
begin
  if Assigned(Engine) then
  begin
    DBGRunAct.Enabled := not Engine.Session.Run.Active;
    if (Engine.Tendency.Debug <> nil) then
    begin
      DebugPnl.Caption := Engine.Tendency.Debug.GetKey;
      UpdateFileHeaderPanel;
      UpdateCallStack;
      UpdateWatches;
      Engine.Files.Refresh; // not safe thread
    end;
  end;
end;

procedure TMainForm.UpdateFileHeaderPanel;
begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.Control = Engine.DebugLink.ExecutedControl) then
    BugSignBtn.Visible := True
  else
    BugSignBtn.Visible := False;
  //FileHeaderPanel.Color := $00EEE0D7;
  FileHeaderPanel.Visible := Engine.Files.Count > 0;
  if FileHeaderPanel.Visible then
    FileHeaderPanel.Refresh;
end;

procedure TMainForm.UpdateCallStack;
var
  i: integer;
  aIndex: integer;
begin
  if Engine.Tendency.Debug <> nil then
  begin
    aIndex := CallStackGrid.Row;
    CallStackGrid.BeginUpdate;
    try
      CallStackGrid.RowCount := Engine.DebugLink.CallStack.Count + 1;
      with Engine.DebugLink do
      try
        for i := 0 to CallStack.Count - 1 do
        begin
          CallStackGrid.Cells[1, i + 1] := Engine.DebugLink.CallStack[i].FileName;
          CallStackGrid.Cells[2, i + 1] := IntToStr(Engine.DebugLink.CallStack[i].Line);
        end;
      finally
      end;
    finally
      if (aIndex > 0) and (aIndex <= CallStackGrid.RowCount) then
        CallStackGrid.Row := aIndex;
      CallStackGrid.EndUpdate;
    end;
  end;
end;

procedure TMainForm.OptionsChanged;
begin
  {Color := Engine.Options.Profile.Attributes.Panel.Background;
  Font.Color := Engine.Options.Profile.Attributes.Panel.Foreground;

  FoldersSpl.RaisedColor := Engine.Options.Profile.Attributes.Separator.Foreground;
  FoldersSpl.LoweredColor := Engine.Options.Profile.Attributes.Separator.Background;}

  {BrowserPnl.Color := Engine.Options.Profile.Attributes.Panel.Background;
  ClientPnl.Color := Engine.Options.Profile.Attributes.Panel.Background;
  FoldersSpl.Color := Engine.Options.Profile.Attributes.Panel.Background;
  MessagesSpl.Color := Engine.Options.Profile.Attributes.Panel.Background;
  MessagesSpl.LoweredColor
  MessagesTabs.Color := Engine.Options.Profile.Attributes.Panel.Background;}

  FileTabs.Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
  FileTabs.ActiveColor := Engine.Options.Profile.Attributes.Default.Background;
  FileTabs.NormalColor := MixColors(FileTabs.ActiveColor, OppositeColor(FileTabs.ActiveColor), 200);

  FileList.Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
  FileList.Color := Engine.Options.Profile.Attributes.Default.Background;

  SearchGrid.Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
  SearchGrid.Font.Name := Engine.Options.Profile.Attributes.FontName;
  SearchGrid.Font.Size := Engine.Options.Profile.Attributes.FontSize;

  OutputEdit.Font.Color := Engine.Options.Profile.Attributes.Default.Foreground;
  OutputEdit.Color := Engine.Options.Profile.Attributes.Default.Background;
end;

procedure TMainForm.UpdateWatches;
var
  i: integer;
  aIndex: integer;
begin
  //todo not good idea, we should refresh without clear the grid
  if Engine.Tendency.Debug <> nil then
  begin
    aIndex := WatchesGrid.Row;
    WatchesGrid.BeginUpdate;
    try
      WatchesGrid.RowCount := Engine.Tendency.Debug.Watches.Count + 1;
      Engine.Tendency.Debug.Lock;
      try
        for i := 0 to Engine.Tendency.Debug.Watches.Count - 1 do
        begin
          WatchesGrid.Cells[1, i + 1] := Engine.Tendency.Debug.Watches[i].Name;
          WatchesGrid.Cells[2, i + 1] := Engine.Tendency.Debug.Watches[i].VarType;
          WatchesGrid.Cells[3, i + 1] := Engine.Tendency.Debug.Watches[i].Value;
        end;
      finally
        Engine.Tendency.Debug.Unlock;
      end;
    finally
      if (aIndex > 0) and (aIndex <= WatchesGrid.RowCount) then
        WatchesGrid.Row := aIndex;
      WatchesGrid.EndUpdate;
    end;
  end;
end;

procedure TMainForm.ShowMessagesList;
begin
  MessagesTabs.ActiveControl := MessagesGrid;
end;

procedure TMainForm.ShowWatchesList;
begin
  MessagesTabs.ActiveControl := WatchesGrid;
end;

procedure TMainForm.LoadAddons;
var
  i: integer;

  procedure AddMenu(vParentMenu, vName, vCaption: string; vOnClick: TNotifyEvent);
  var
    c: TComponent;

    function CreateMenuItem: TMenuItem;
    begin
      Result := TMenuItem.Create(Self);
      Result.Name := vName;
      Result.Caption := vCaption;
      Result.OnClick := vOnClick;
    end;

    function CreateToolButton: TToolButton;
    begin
      Result := TToolButton.Create(Self);
      Result.Name := vName;
      Result.Caption := vCaption;
      Result.OnClick := vOnClick;
    end;

  begin
    c := FindComponent(vParentMenu);
    if c <> nil then
    begin
      if c is TMenu then
        (c as TMenu).Items.Add(CreateMenuItem)
      else if c is TMenuItem then
        (c as TMenuItem).Add(CreateMenuItem);
      {else if c is TToolBar then
         CreateToolButton.Parent := c as TToolBar
      else if c is TToolButton then
        CreateToolButton.Parent := c as TbittToolBar}
    end
    else
      ToolsMnu.Add(CreateMenuItem);
    //m.Parent := ToolsMnu;
  end;

begin
  //MainMenu.Items.BeginUpdate;
  try
    for i := 0 to Addons.Count - 1 do
    begin
      if Supports(Addons[i].Addon, IMenuAddon) and (Supports(Addons[i].Addon, IClickAddon)) then
        AddMenu('', Addons[i].Name, (Addons[i].Addon as IMenuAddon).GetCaption, @((Addons[i].Addon as IClickAddon).Click));
    end;
  finally
    //MainMenu.Items.EndUpdate;
  end;
end;

procedure TMainForm.Add1Click(Sender: TObject);
var
  S: string;
begin
  if MsgBox.Msg.Input(S, 'Add Watch, Enter variable name') then
    if S <> '' then
      AddWatch(S);
end;

procedure TMainForm.AddWatch(s: string);
begin
  if Engine.Tendency.Debug <> nil then
  begin
    s := Trim(s);
    if s <> '' then
    begin
      Engine.Tendency.Debug.Watches.Add(s);
      UpdateWatches;
    end;
  end;
end;

procedure TMainForm.Delete1Click(Sender: TObject);
begin
  DeleteCurrentWatch;
end;

procedure TMainForm.DeleteWatch(s: string);
begin
  if Engine.Tendency.Debug <> nil then
  begin
    Engine.Tendency.Debug.Watches.Remove(s);
    //UpdateWatches;
  end;
end;

procedure TMainForm.EnumRecents;
begin
  EnumRecentFiles;
  EnumRecentFolders;
  EnumRecentProjects;
end;

procedure TMainForm.DBGToggleBreakpointActExecute(Sender: TObject);
var
  aLine: integer;
begin
  if Engine.Tendency.Debug <> nil then
  begin
    if (Engine.Files.Current <> nil) and (Engine.Files.Current.Control is TCustomSynEdit) and (ActiveControl = Engine.Files.Current.Control) and (fgkExecutable in Engine.Files.Current.Group.Kind) then
      with Engine.Files.Current do
      begin
        aLine := (Control as TCustomSynEdit).CaretY;
        Engine.Tendency.Debug.Lock;
        try
          Engine.Tendency.Debug.Breakpoints.Toggle(Name, aLine);
        finally
          Engine.Tendency.Debug.Unlock;
        end;
        (Control as TCustomSynEdit).InvalidateLine(aLine);
      end;
  end;
end;

procedure TMainForm.RunFile;
begin
  if Engine.Files.Current <> nil then
  begin
    SaveAllAct.Execute;
    Engine.Files.Current.Tendency.Run([rnaCompile, rnaExecute, rnaDebug]);
  end;
end;

procedure TMainForm.CompileFile;
begin
  if Engine.Files.Current <> nil then
  begin
    SaveAllAct.Execute;
    Engine.Files.Current.Tendency.Run([rnaCompile]);
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
  with Engine.Files do
    if (Current <> nil) and (Current.Control is TCustomSynEdit) then
    begin
      if not (Current.Control as TCustomSynEdit).SelAvail then
        s := Trim((Current.Control as TCustomSynEdit).GetWordAtRowCol((Current.Control as TCustomSynEdit).CaretXY))
      else
        s := (Current.Control as TCustomSynEdit).SelText;
      AddWatch(s);
      MessagesTabs.ActiveControl := WatchesGrid;
    end;
end;

procedure TMainForm.DeleteCurrentWatch;
begin
  if WatchesGrid.Row > 0 then
  begin
    DeleteWatch(WatchesGrid.Cells[1, WatchesGrid.Row]);
  end;
end;

procedure TMainForm.FolderHomeActExecute(Sender: TObject);
begin
  Folder := Engine.GetRoot;
end;

procedure TMainForm.StatusTimerTimer(Sender: TObject);
begin
  StatusTimer.Enabled := False;
  MessageLabel.Caption := '';
end;

procedure TMainForm.Clear1Click(Sender: TObject);
begin
  if MessagesPopup.PopupComponent is TListView then
    (MessagesPopup.PopupComponent as TListView).Clear
  else if MessagesPopup.PopupComponent is TStringGrid then
    (MessagesPopup.PopupComponent as TStringGrid).RowCount := 1;
end;

{type
  TSearchListItem = class(TListItem)
  private
    FColumn: Integer;
    FLength: Integer;
  public
    property Column: Integer read FColumn write FColumn;
    property Length: Integer read FLength write FLength;
  end; } //TODO delete it

procedure TMainForm.SearchFoundEvent(Index: Integer; FileName: string; const Line: string; LineNo, Column, FoundLength: Integer);
begin
  if Index = 0 then
  begin
    SearchGrid.RowCount := 2;
  end
  else
    SearchGrid.RowCount := SearchGrid.RowCount + 1;
  SearchGrid.Cells[1, SearchGrid.RowCount - 1] := FileName;
  SearchGrid.Cells[2, SearchGrid.RowCount - 1] := IntToStr(LineNo);

  SearchGrid.Cells[3, SearchGrid.RowCount - 1] := Line;
  SearchGrid.Rows[SearchGrid.RowCount - 1].Objects[0] := TObject(PtrInt(FoundLength));
  SearchGrid.Rows[SearchGrid.RowCount - 1].Objects[1] := TObject(PtrInt(Column));
end;

procedure TMainForm.FindInFilesActExecute(Sender: TObject);
var
  aText, aFolder: string;
begin
  with Engine.Files do
  if (Current <> nil) and (Current.Control is TCustomSynEdit) then
  begin
    if (Current.Control as TCustomSynEdit).SelAvail and ((Current.Control as TCustomSynEdit).BlockBegin.y = (Current.Control as TCustomSynEdit).BlockEnd.y) then
      aText := (Current.Control as TCustomSynEdit).SelText
    else
      aText := (Current.Control as TCustomSynEdit).GetWordAtRowCol((Current.Control as TCustomSynEdit).CaretXY);
  end;

  aFolder := Engine.GetRoot;

  if aFolder = '' then
    aFolder := Folder;
  MessagesAct.Checked := True;
  UpdateMessagesPnl;
  MessagesTabs.ActiveControl := SearchGrid;
  ShowSearchInFilesForm(@SearchFoundEvent, aText, ExpandFileName(aFolder), Engine.Options.SearchFolderHistory, Engine.Options.SearchHistory, Engine.Options.ReplaceHistory);
end;

procedure TMainForm.NextMessageActExecute(Sender: TObject);
begin
  MoveListIndex(True);
end;

procedure TMainForm.MoveListIndex(vForward: boolean);

  procedure DblClick(Grid: TStringGrid);
  begin
    if vForward then
    begin
      if Grid.Row < Grid.RowCount - 1 then
        Grid.Row := Grid.Row + 1;
    end
    else
    begin
      if Grid.Row > 0 then
        Grid.Row := Grid.Row - 1;
    end;
    if Grid.Row > 0 then
    begin
      //Grid.MakeVisible(False); TODO
    end;
    if Assigned(Grid.OnDblClick) then
      Grid.OnDblClick(Grid);
  end;

begin
  case MessagesTabs.ItemIndex of
    0: DblClick(MessagesGrid);
    1: DblClick(WatchesGrid);
    2: DblClick(SearchGrid);
    3: DblClick(CallStackGrid);
  end;
end;

procedure TMainForm.PriorMessageActExecute(Sender: TObject);
begin
  MoveListIndex(False);
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin
  FOutputBuffer := '';//TODO bad bad bad
  OutputEdit.Lines.Clear;
end;

procedure TMainForm.EngineState;
begin
  if Engine.MacroRecorder.State = msRecording then
    StatePnl.Caption := 'R'
  else if Engine.Files.Current <> nil then
  begin
    CursorPnl.Caption := Engine.Files.Current.GetGlance;
    if Engine.Files.Current.IsNew then
      StatePnl.Caption := 'N'
    else if Engine.Files.Current.IsReadOnly then
      StatePnl.Caption := 'R'
    else
      StatePnl.Caption := 'S'
  end
  else
    StatePnl.Caption := '';
end;

procedure TMainForm.SCMCompareToActExecute(Sender: TObject);
var
  aDialog: TOpenDialog;
begin
  if Engine.Files.Current <> nil then
  begin
    aDialog := TOpenDialog.Create(nil);
    try
      aDialog.Title := 'Open file';
      aDialog.Options := aDialog.Options - [ofAllowMultiSelect];
      aDialog.Filter := Engine.Groups.CreateFilter(True, '', Engine.Files.Current.Group);
      //      aDialog.InitialDir := Engine.BrowseFolder;
      if aDialog.Execute then
      begin
        Engine.SCM.DiffToFile(aDialog.FileName, Engine.Files.Current.Name);
      end;
      Engine.Files.CheckChanged;
    finally
      aDialog.Free;
    end;
  end;
end;

procedure TMainForm.SwitchFocusActExecute(Sender: TObject);
begin
  if FoldersAct.Checked and not FileList.Focused then
    FileList.SetFocus
  else if (Engine.Files.Current <> nil) then
    Engine.Files.Current.Activate;
end;

procedure TMainForm.QuickSearchNextBtnClick(Sender: TObject);
begin
  if (Engine.Files.Current.Control is TCustomSynEdit) then
    if (QuickSearchEdit.Text <> '') then
      (Engine.Files.Current.Control as TCustomSynEdit).SearchReplace(QuickSearchEdit.Text, '', []);
end;

procedure TMainForm.QuickSearchPrevBtnClick(Sender: TObject);
begin
  if (Engine.Files.Current.Control is TCustomSynEdit) then
    if QuickSearchEdit.Text <> '' then
      (Engine.Files.Current.Control as TCustomSynEdit).SearchReplace(QuickSearchEdit.Text, '', [ssoBackwards]);
end;

procedure TMainForm.QuickSearchEditChange(Sender: TObject);
begin
  if (Engine.Files.Current.Control is TCustomSynEdit) then
  begin
    if QuickSearchEdit.Text <> '' then
      (Engine.Files.Current.Control as TCustomSynEdit).SearchReplace(QuickSearchEdit.Text, '', [ssoEntireScope]);
    SearchForms.SetTextSearch(QuickSearchEdit.Text);
  end;
end;

procedure TMainForm.QuickSearchEditKeyPress(Sender: TObject; var Key: char);
begin
  if (Engine.Files.Current.Control is TCustomSynEdit) then
    if (Key = #13) and (QuickSearchEdit.Text <> '') then
      (Engine.Files.Current.Control as TCustomSynEdit).SearchReplace(QuickSearchEdit.Text, '', []);
end;

procedure TMainForm.CloseQuickSearchBtnClick(Sender: TObject);
begin
  QuickFindAct.Execute;
end;

procedure TMainForm.QuickFindActExecute(Sender: TObject);
begin
  if (Engine.Files.Current.Control is TCustomSynEdit) then
  begin
    QuickFindPnl.Visible := QuickFindAct.Checked;
    if QuickFindPnl.Visible then
    begin
      QuickSearchEdit.Text := SearchForms.GetTextSearch;
      QuickSearchEdit.SetFocus;
      QuickSearchEdit.SelectAll;
    end;
  end;
end;

procedure TMainForm.QuickFindActUpdate(Sender: TObject);
begin
  if (Engine.Files.Current <> nil) and (Engine.Files.Current.Control is TCustomSynEdit) then
  begin
    QuickFindAct.Enabled := Engine.Files.Count > 0;
    QuickFindAct.Checked := QuickFindPnl.Visible;
  end;
end;

procedure TMainForm.WorkspaceMnuClick(Sender: TObject);
begin
  with TEditorSetupForm.Create(Application) do
  begin
    NeedToRestartLbl.Visible := True;
    ShowModal;
    Free;
  end;
end;

procedure TMainForm.EngineReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: integer; var ReplaceAction: TSynReplaceAction);
begin
  case MsgBox.Msg.Ask(Format('Replace this ocurrence of "%s" with "%s"?', [ASearch, AReplace]), [msgcYes, msgcNo, msgcAll, msgcCancel], msgcYes, msgcCancel, msgkConfirmation) of
    msgcYes: ReplaceAction := raReplace;
    msgcNo: ReplaceAction := raSkip;
    msgcAll: ReplaceAction := raReplaceAll;
    else //msgcCancel
      ReplaceAction := raCancel;
  end;
end;

procedure TMainForm.Log(ACaption, AMsg: string);
begin
  EngineError(0, ACaption, AMsg, '', 0);
end;

procedure TMainForm.Log(AMsg: string);
begin
  Log('', AMsg);
end;

procedure TMainForm.EngineOutput(S: string);
begin
  FOutputBuffer := FOutputBuffer + S;//TODO baaad
  OutputEdit.Text := FOutputBuffer;
  OutputEdit.CaretY := OutputEdit.Lines.Count;
end;

procedure TMainForm.CatchErr(Sender: TObject; e: exception);
begin
  MsgBox.Msg.Error(e.Message);
end;

end.
